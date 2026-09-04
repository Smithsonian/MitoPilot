# Map-to-reference assembler, LEAN proposal (bowtie2 + samtools consensus loop inside assemble.nf)

Date: 2026-09-03
Branch: map-to-ref-assembly
Status: design proposal (planning only, no code)
Angle: ponytail. Smallest diff that still behaves like Geneious "Map to Reference" for a
full-length single reference. Zero new container dependencies.

All file:line references were re-checked against the working tree on 2026-09-03.

---------------------------------------------------------------------------------------

## 1. Summary

1. A third `elif` branch in `inst/nextflow/modules/assemble.nf` (after line 101) named
   `MapToRef`: a bash loop of `bowtie2 --very-sensitive-local` -> `samtools sort` ->
   `samtools consensus`, re-mapping to the previous consensus, up to N times, stopping
   early when the consensus stops changing.
2. Tools: bowtie2 2.5.4, samtools 1.21 (has `samtools consensus`), awk, sed. All already in
   `docker/Dockerfile:31-32`. No bioconda addition, no Dockerfile change, no image rebuild
   beyond the normal package bump.
3. Reference: one user file (.gb or FASTA), one record, given as a path or URL in a new
   `assemble_opts.maptoref_ref` column (same plumbing as `mitofinder_db`). Sequence and
   LOCUS topology are pulled out with awk inside the task; annotations are not used in v1.
4. Circular references use the elongate-and-clip trick this repo already uses for coverage
   (`R/coverage.R:250-259`): append the first 500 bp, map, call consensus, keep reference
   positions 1..L. Uncovered or sub-depth sites are N (Geneious "low coverage call").
5. Why: everything downstream (coverage, BLAST, ref fetch, join, annotate, export) keys only
   on the published FASTA layout and header convention (codebase-map section 10), so the
   whole feature is one shell branch, four DB columns, one `path()` input, four textInputs,
   and one changed condition in `coverage.nf`. Roughly 220 new lines across 9 files.

---------------------------------------------------------------------------------------

## 2. Algorithm

### 2.1 Pipeline for one sample (inside the `MapToRef` branch of assemble.nf)

Inputs already staged by Nextflow: `reads[0]`/`reads[1]` (fastp-trimmed pairs from
PREPROCESS, `preprocess.nf:25-26`), `ref` (the user file, `path(ref)`), `opts.cpus`,
`opts.memory`, and the new option values `opts.maptoref` (bowtie2 flags),
`opts.maptoref_consensus` (samtools consensus flags), `opts.maptoref_iter` (integer cap).

Step 0. Trim: none. Reads are already trimmed by fastp in PREPROCESS
(`R/init_db.R:248` default `--trim_poly_g --correction --detect_adapter_for_pe ...`).
This is Geneious "Do not trim (already trimmed)".

Step 1. Reference to plain sequence + topology (awk, ~10 lines):

```
if first non-blank line starts with LOCUS   -> GenBank
    records=$(grep -c '^//' ref); [ records -eq 1 ] || fail "reference must hold exactly one record"
    topology: LOCUS line tokens contain "circular" -> circular, else linear
    seq: awk '/^ORIGIN/{f=1;next} /^\/\//{f=0} f{gsub(/[^A-Za-z]/,""); printf "%s", toupper($0)}'
else                                         -> FASTA
    [ $(grep -c '^>') -eq 1 ] || fail "reference must hold exactly one sequence"
    topology: header contains the token "circular" -> circular, else linear
    seq: grep -v '^>' | tr -d ' \r\n' | tr a-z A-Z
fi
[ -n "$seq" ] || fail; grep -q '[^ACGTNRYSWKMBDHV]' && fail "non-IUPAC characters"
L=${#seq}; warn in log if L < 5000 or L > 50000
```

`fail` = write `${outDir}/${id}_assembly_0.fasta` with `>No assembly found`, append the
reason to `assembler.log.txt`, write the other mandatory files, `exit 0` (contract item K,
codebase-map section 10: the workflow records "failed assembly", status 3).

Step 2. Build the mapping reference for iteration 1 (`ref_iter0.fa`):
- circular: `seq + substr(seq, 1, F)` with `F = min(500, L/2)` (same numbers as
  `R/coverage.R:250-259` and `R/circularize_asmb.R:529`).
- linear: `seq` as is.
Header `>ref`.

Step 3. Iterate, k = 1 .. `maptoref_iter` (default 5):

```
bowtie2-build -q ref_iter$((k-1)).fa idx
bowtie2 !{opts.maptoref} --no-unal -x idx -1 R1 -2 R2 --threads !{opts.cpus} 2>> log \
  | samtools sort -@ !{opts.cpus} -o iter$k.bam -
samtools consensus !{opts.maptoref_consensus} -a --show-del yes --mark-ins \
  -@ !{opts.cpus} -o iter$k.raw.fa iter$k.bam
awk (clip + unmark, below) iter$k.raw.fa > iter$k.fa
mapped=$(samtools view -c -F 4 iter$k.bam); nN=$(grep -v '>' iter$k.fa | tr -cd N | wc -c)
echo "$k,$mapped,$(len iter$k.fa),$nN" >> summary
cmp -s iter$k.fa iter$((k-1)).fa && break          # converged
build ref_iter$k.fa from iter$k.fa (re-append first F bases if circular)
```

Reads for iteration 1 = the full preprocessed pair. Reads for iterations >= 2 = the pairs
where at least one mate aligned in iteration 1:
`samtools view -b -G 12 iter1.bam | samtools sort -n | samtools fastq -1 sub_R1.fq -2 sub_R2.fq -0 /dev/null -s /dev/null -n`.
This is the "normalize then extend, then one clean pass" shape a Geneious staff member
recommends (geneious-advisor 5.15) and what the GetOrganelle branch already does
(`extended_*_paired.fq`). The clean full-read pass is `coverage.nf`, which re-maps every
read to the final consensus anyway (section 2.4). Iteration 1 costs what the existing
coverage step costs per sample (`R/coverage.R:63-75` is the same bowtie2 call); later
iterations run on a subset that is usually < 1% of the reads and finish in seconds.

Step 4. Consensus construction (what `samtools consensus` does with the defaults chosen):
- Bayesian mode (default `-m bayesian`, "derived from the Gap5 consensus algorithm"), which
  weights bases by base quality: the analogue of Geneious "Highest Quality".
- `-a`: emit every reference position, N where nothing can be called. Geneious
  "If no coverage call: N".
- `-d 3`: N below depth 3 (after `--min-BQ` filtering). Geneious XML default
  `coverageThreshold=3` for reference assemblies (geneious-advisor 8.3).
- `--min-BQ 20`: Geneious "Call N if quality below 20" analogue (per base, not per column).
- `--no-use-MQ`: mapping quality is NOT used to weight bases. Two reasons. (a) With a
  17 kb mito-only reference a read's MAPQ says nothing about NUMT origin (there is no
  nuclear alternative in the index), so MQ carries no signal. (b) In the duplicated block
  of a circular reference bowtie2 places reads randomly between the two copies and gives
  them MAPQ 0-1; with MQ in use those 500 bp would come out as N. This is also why the
  branch does not need the read-level fold that `R/circularize_asmb.R:480` performs for
  depth.
- `-A`: IUPAC codes at mixed sites, like Geneious ambiguity calls. The rest of the pipeline
  already accepts them (NEWS.md 1.5.4; `R/assembly_path_scoring.R:56-60`).
- `--show-del yes --mark-ins` are fixed (not user-editable): deletions print as `*` and
  inserted bases are prefixed with `+`, so every unmarked character is one reference
  position. That is what lets the awk clip at reference position L (step 5).

Step 5. Clip and unmark (one awk, ~8 lines): walk the consensus string, count reference
positions (characters not preceded by `+`), stop after L positions, drop `*` and `+`.
For a linear reference L is the whole thing; the awk only strips marks. Then strip leading
and trailing N runs for linear references only (`sed`), because samtools 1.21 still pads
in some cases (alt-tools section 0) and a linear consensus should not carry reference
padding it never covered. Circular consensus keeps full length L; internal N runs stay.

Step 6. Extension beyond reference ends: DEFERRED (not implemented in v1). Reason: with a
full-length circular reference there are no ends (alt-tools section 5). With a linear or
partial reference the composed loop cannot grow the sequence; that needs a recruit-and-
micro-assemble step (SPAdes on the recruited subset, then re-align), which is the one
place MITObim wins. The UI text says "reference must be a complete mitogenome". If a user
asks for barcode-seed growth, add the SPAdes step then.

Step 7. Circularity: handled entirely by steps 2, 4 and 5 (elongate, consensus, clip).
The seam region L-F..L is called from reads that also span into the duplicate copy, and
positions 1..F are called from the half of the reads that landed on the first copy, with
MQ ignored. Nothing else to do; `coverage.nf` will then re-map with its own junction
construct and fold depth as it does for every circular assembly.

Step 8. Final pass: the last iteration IS the final consensus. Then outputs:

```
awk -v topo=$topology '/^>/{print ">'!{id}'.1.1 " topo} !/^>/{print}' iter$k.fa > outDir/id_assembly_1.fasta
cp ref outDir/reference.gb|reference.fasta        (provenance, 1 line)
tar -czvf outDir/id_reads.tar.gz *.fastq.gz          (MitoFinder layout, contract item D)
echo "iteration,reads_mapped,length,n_count" + rows > outDir/id_summary.txt
bowtie2/samtools stderr -> outDir/assembler.log.txt ; opts -> opts.txt ; NF_work_dir_assemble.txt
```

The loop's BAMs are not published (they are re-created by `coverage.nf`); per-iteration
FASTAs (`iter1.fa` .. `iterN.fa`, 17 kb each) are copied into `outDir/maptoref/` for
debugging. If iteration 1 maps zero reads: `fail "no reads mapped to the reference"`.

### 2.2 Stopping rule

Stop when `iter$k.fa` is byte-identical to `iter$((k-1)).fa` (consensus converged) or at
`maptoref_iter`. Geneious stops "once no additional sequence reads are aligned"
(geneious-advisor 5.15, 9.2); the aITE script stops on an unchanged mapped-read count
(alt-tools 3.3). Unchanged consensus is stricter and cheaper (one `cmp`). Iteration
counts are logged so a user can see whether the cap was hit.

### 2.3 Defaults (the three option strings)

| Column | Default | Why |
|---|---|---|
| `maptoref` | `--very-sensitive-local` | Already the repo's mapping preset (`R/coverage.R:69`); in the Geneious white paper this preset lifts bowtie2 from 44% to 85% mapped reads at 89% identity (geneious-advisor 7.2), i.e. it is the Medium-sensitivity analogue |
| `maptoref_consensus` | `-A -d 3 --min-BQ 20 --no-use-MQ` | Section 2.1 step 4 |
| `maptoref_iter` | `5` | Geneious default "Iterate up to 5 times" and the mode of 27 published mitogenome studies (geneious-advisor 9.8); Kemp saw saturation at 3-10 |

### 2.4 Geneious option mapping table

Fidelity: exact = same semantics; approximate = same intent, different arithmetic;
not replicable = nothing equivalent with the tools in the image (or deferred).

| Geneious option (XML key) | Geneious value | Our equivalent | Our default | Fidelity |
|---|---|---|---|---|
| Mapper | Geneious / Bowtie2 | bowtie2 2.5.4 (the advisor's own alternative for short reads, non-complex reference) | bowtie2 | approximate (bowtie2 is the advisor's second choice; no Geneious mapper) |
| Sensitivity: Low / Fastest | index 14, word 24, 10% mism, 10% gaps | `--fast-local` in `maptoref` | n/a | approximate |
| Sensitivity: Medium-Low / Fast | 20% mism, 10% gaps (second-hand) | `--sensitive-local` | n/a | approximate |
| Sensitivity: Medium / Fast (default) | index 12, word 14, 30% mism, 15% gaps | `--very-sensitive-local` | DEFAULT | approximate |
| Sensitivity: Medium-High | unknown | `--very-sensitive-local -N 1` (one seed mismatch, which the white paper names as a Geneious heuristic) | n/a | approximate |
| Sensitivity: Highest / Medium | index ~10 | `--very-sensitive-local -N 1 -L 15 --score-min G,10,6` | n/a | approximate |
| Custom sensitivity (8.1 keys), one row each: | | | | |
| `indexWordLength` (12) | seed word | bowtie2 `-L` (seed length; 20 in very-sensitive-local, min 4) | 20 | approximate (bowtie2 seeds are FM-index substrings, not hashed words) |
| `expansionWordLength` (14) | expansion word | none; bowtie2 extends with SIMD DP, no second word size | - | not replicable |
| `filterRepeatsReference` / size (true / 20) | ignore words repeated > n times | none; bowtie2 reports one random placement and lowers MAPQ | - | not replicable (MAPQ is ignored by design, see 2.1) |
| `allowGaps` (true) | allow indels | bowtie2 always allows gaps; `--rdg`/`--rfg` set penalties | on | exact |
| `maxGapsPerRead` (15%) | gap budget per read | `--rdg 5,3 --rfg 5,3` (defaults) within `--score-min` | defaults | approximate |
| `maxGapSize` (50) | longest gap | bowtie2 has no cap; long gaps become soft-clips in local mode; `--gbar` only guards read ends | - | not replicable |
| `maxMismatches` (30%) | mismatch budget per read | `--score-min G,20,8` (local default) plus `--mp 6,2`; lower the function for more divergence, e.g. `--score-min G,10,6` | default | approximate (score budget, not a percentage) |
| `maxAmbiguity` (4) | max N in a read | `--n-ceil L,0,0.15` | default | approximate |
| `applyMinOverlap` / `minOverlap` (off / 25) | min read overlap | none (local mode aligns whatever part matches) | - | not replicable |
| `minOverlapPercentageIdentical` (off / 80) | | none | - | not replicable |
| `doMoreThoroughSearching` (false) | | `-D 20 -R 3` are already inside `--very-sensitive-local` | on | approximate |
| `accuratelyMapReadsWithErrorsToRepeatRegions` (true) | | none | - | not replicable |
| `multipleBestMatches` (mapRandomly) | random / none / all | bowtie2 default = one random best placement | random | exact for "Randomly"; "map to none" would be `--min-MQ 2` in consensus flags (drops ties) = approximate; "map to all" = not replicable |
| `applyMinimumMappingQuality` / `minimumMappingQuality` (off / 30) | | `--min-MQ N` in `maptoref_consensus` | off (0) | exact |
| `trimPairedOverhangs` (true) | trim mate overhang past its partner | none in samtools 1.21 consensus (both mates count over the overlap) | - | not replicable |
| `onlyMapPairedHitsReference` (false, mapNearby) | drop pairs that do not map nearby | `--no-mixed --no-discordant` in `maptoref` | off | exact |
| Paired-read distance (Set Paired Reads, insert midpoint) | soft penalty | `-I 0 -X 500` (bowtie2 defaults; raise `-X` for long inserts) | 0-500 | approximate (hard window, not a penalty) |
| `findStructuralVariants` / `findDeletions` (false / false, 1000) | junction discovery | none | off | not replicable (deferred; de novo assemblers in the same project cover this) |
| Fine tuning `fineTune` (iterate_5; None/3/5/10/25/custom) | re-map to previous consensus | `maptoref_iter` (1 = None) plus convergence stop | 5 | approximate (Geneious re-aligns reads to each other around indels; we re-map to the consensus; stop rule differs, 2.2) |
| Trim before mapping | Do not trim | reads pre-trimmed by fastp in PREPROCESS | do not trim | exact |
| Consensus threshold: Highest Quality (60%) `weighted_60` | quality-summed call | Bayesian mode (default) | DEFAULT | approximate (Gap5 model, not a 60% quality fraction) |
| Consensus threshold: percentage (e.g. 65%, `thresholdPercentNoQuality`) | fraction of bases | `-m simple -c 0.65` (add `-q` to weight by quality) | n/a | approximate (samtools -c is "fraction agreeing with the top call" and emits N otherwise, not an ambiguity code) |
| Consensus threshold: 0% majority / plurality | most frequent base | `-m simple -c 0.5 -H 1` | n/a | approximate |
| Ambiguity codes | on when threshold not met | `-A` (Bayesian: het model; simple: `-H` fraction) | on | approximate |
| `mapQuality` / `mapQualityMethod` (true / mapSummed) | include MQ in call | `--use-MQ` (we default to `--no-use-MQ`, see 2.1) | off | exact toggle, different arithmetic |
| `applyLowCoverageOrQualityCall` + `coverageThreshold` (true / 3) | low coverage call | `-d 3` | 3 | exact (character is N, not `?`) |
| `lowCoverageOrQualityCharacter` (`?`) | | N only | N | approximate (`?` is not a FASTA base; N is what export and scoring already understand) |
| `qualityThreshold` (20, "Call N if quality below") | | `--min-BQ 20` (per base) or `-C 10` (Bayesian per-column cutoff) | 20 | approximate |
| `noCoverageCharacterReference` (`?`; options -, N, ?, Ref) | no coverage call | `-a` gives N; "Ref" needs `samtools consensus -T` (samtools >= 1.22) | N | exact for N; "Ref" not replicable in 1.21 (deferred, one version bump away) |
| `trimToReference` (false) | clip consensus to reference extent | circular: consensus is always exactly L; linear: leading/trailing N stripped, never extended | effectively true | approximate (no extension either way; see 2.1 step 6) |
| `noConsensusGaps` / `noConsensusEndGaps` (false / true) | gap calls | deletions applied (`--show-del yes` then `*` removed); insertions applied (`--show-ins yes`) | applied | approximate (no local realignment around indels) |
| `ignoreReadsMappedToMultipleLocations` (false) | | `--min-MQ 2` | off | approximate |
| `splitAroundQuestionMarks` (false) | | not offered | off | exact (never split) |
| Coverage graph export | CSV | `coverage.nf` produces `*_coverageStats.csv` and PDFs for every assembly | on | exact (already exists) |

---------------------------------------------------------------------------------------

## 3. Reference input

- Formats: GenBank flat file (`.gb`, `.gbk`, `.gbff`) or FASTA. Detected by content
  (`LOCUS` first token vs `>`), not extension. Exactly one record; a multi-record file
  fails the sample with a clear log line (ref-handling section 4 argues for reject over
  silent first-record pick).
- What we keep from the `.gb`: the ORIGIN sequence and the LOCUS topology token
  ("circular" / "linear", parsed token-wise as NCBI recommends, ref-handling 5.1). The
  file is also copied verbatim into the published directory as `reference.gb` for
  provenance. Features/annotations are NOT parsed in v1 (see YAGNI list; WF1 BLAST +
  `blast_ref_fetch` still fetch annotations for the closest GenBank record, which for an
  NCBI-derived reference is normally the reference itself, and the same `.gb` can already
  be typed into `annotate_opts.mitofinder_db` for MitoFinder gap-fill annotation with no
  new code).
- FASTA topology: `circular` token anywhere in the header, else linear. Documented; the
  vignette tells users to prefer `.gb` for that reason.
- Validation: in the task (section 2.1 step 1), not in the app. The app field is a plain
  `textInput` exactly like `mf_db` (`R/app_assemble_utils.R:323-332`), no `file.exists`,
  because the value may be a URL and the container is the only place the file is
  guaranteed to be visible. `new_db()` also does no validation beyond non-empty when
  `assembler == "MapToRef"` (one `stop()` if `maptoref_ref` is empty).
- Per project vs per sample: per parameter set, like `mitofinder_db`. Samples that need a
  different reference get their own `assemble_opts` set from the modal (the `create = TRUE`
  selectize at `R/app_assemble_utils.R:229-241`). A `Reference` mapping-file column with a
  `COALESCE` in the SQL is a five-line follow-up if anyone asks.
- Storage: wherever the user points. Recommended (docs) location is inside the project
  directory (next to `.sqlite`), because Nextflow stages `path()` inputs by symlink and a
  symlink target outside a bound path is invisible in a Singularity container
  (`prepare_ref_db.nf:15-19` comment). URLs work because `path()` already stages
  `mitofinder_db` from a GitHub raw URL (`R/init_db.R:80`).
- How it reaches the container: `path(ref)` input on the `assemble` process (same as
  `path(mf_db)`, `assemble.nf:16`); `${projectDir}/assets/NO_FILE` placeholder when the
  assembler is not MapToRef (`annotate_workflow.nf:92` pattern; `inst/nextflow/assets/NO_FILE`
  exists).

---------------------------------------------------------------------------------------

## 4. Integration (touch points, minimal diff)

### 4.1 R init and options

- `R/init_db.R`
  - :45 roxygen for `assembler`: add "MapToRef"; new `@param maptoref_ref`,
    `@param maptoref`, `@param maptoref_consensus`, `@param maptoref_iter`.
  - :70-83 args: `maptoref_ref = NA_character_`, `maptoref = "--very-sensitive-local"`,
    `maptoref_consensus = "-A -d 3 --min-BQ 20 --no-use-MQ"`, `maptoref_iter = 5`.
  - :131-133 validator: `c("GetOrganelle", "MitoFinder", "MapToRef")` and the message;
    plus `if (assembler == "MapToRef" && !nzchar(maptoref_ref %||% "")) stop(...)`.
  - :313-328 DDL: four columns `maptoref_ref TEXT, maptoref TEXT, maptoref_consensus TEXT,
    maptoref_iter INTEGER`; :330-350 default row gets the four values.
- `R/init_project.R`: nothing (`...` forwards to `new_db()`, :125-133). Vignette shows
  `new_project(assembler = "MapToRef", maptoref_ref = "ref/NC_002333.gb")`.
- `R/backwards_compatibility.R`
  - :176-178 add the four names to the "already current" predicate.
  - after :1313 one block per column, the two-statement shape used for `join_scaffolds`
    (:435-440): `ALTER TABLE assemble_opts ADD COLUMN ...` then `UPDATE ... SET x = default
    WHERE x IS NULL` (defaults as in init; `maptoref_ref` stays NULL).
  - `schema_gaps()` (:2130-2170): unchanged; missing assemble_opts columns are not hard
    stops today.

### 4.2 DB schema and migration

| Table | Column | Type | Default | Meaning |
|---|---|---|---|---|
| assemble_opts | maptoref_ref | TEXT | NULL | path or URL of the .gb / FASTA reference |
| assemble_opts | maptoref | TEXT | `--very-sensitive-local` | flags passed verbatim to bowtie2 |
| assemble_opts | maptoref_consensus | TEXT | `-A -d 3 --min-BQ 20 --no-use-MQ` | flags passed verbatim to samtools consensus |
| assemble_opts | maptoref_iter | INTEGER | 5 | iteration cap |

No change to `assemblies`, `assemble`, `annotate` (contract item H). Assembler string
value: `MapToRef`.

### 4.3 Nextflow

- `inst/nextflow/modules/assemble_workflow.nf`
  - :6-11 SQL: append `opts.maptoref_ref, opts.maptoref, opts.maptoref_consensus,
    opts.maptoref_iter` at the END of the select list (indices 19-22; the house rule is
    append-only because everything is positional).
  - :102-108 opts map: add `maptoref: it[20], maptoref_consensus: it[21],
    maptoref_iter: (it[22] == null ? 5 : (it[22] as Integer))`.
  - :99-117 opts tuple: add a 9th element
    `(it[19] != null && it[19].toString().trim()) ? it[19] : "${projectDir}/assets/NO_FILE"`.
  - :183-195 cross map: add `it[1][8]` as the 10th tuple element.
- `inst/nextflow/modules/assemble.nf`
  - :16 input: append `path(ref)`.
  - :19 output: unchanged (`opts.assembler` flows as it[7]).
  - after :101: `elif [ "!{opts.assembler}" = "MapToRef" ]; then ... fi`, the ~60-line
    branch of section 2.1. It writes exactly the files the other two branches write:
    `${outDir}/${id}_assembly_1.fasta` (or `_assembly_0.fasta` sentinel), `${id}_reads.tar.gz`
    (`*.fastq.gz`, MitoFinder layout), `${id}_summary.txt`, `assembler.log.txt`, `opts.txt`,
    `NF_work_dir_assemble.txt`, plus `reference.gb|fasta` and `maptoref/iter*.fa`.
  - cpus/memory: inherited via the config closure on `opts` (`inst/config.local:12-13`), so
    nothing to add.
- `inst/nextflow/modules/coverage.nf:40`: `elif [ !{assembler} == "MitoFinder" ]` becomes
  `elif [ "!{assembler}" = "MitoFinder" ] || [ "!{assembler}" = "MapToRef" ]`. The
  tarball layout and read names (`<ID>_preprocess_R1.fastq.gz`) match, so
  `MitoPilot::coverage()` runs unchanged and produces the contract CSV (item G).
- `coverage_workflow.nf`, `blast_genbank_workflow.nf`, `scaffold_join*`, WF2: unchanged.
  One path, one scaffold, header `>ID.1.1 circular|linear` satisfies every reader listed
  in codebase-map 2.6.
- `inst/config.*` (8 templates): unchanged (no new `params` block, no new process).
- `MITOPILOT_PROCESS_ORDER` (`R/app_run_pipline.R:8-15`): unchanged (no new process).

### 4.4 Container

`docker/Dockerfile`: no change. bowtie2 2.5.4 (:31), samtools 1.21 (:32) with
`consensus -a -m -A -d -c -H -q --min-MQ --min-BQ --show-del --show-ins --mark-ins -@`
(verified in the local `macguigand/mitopilot:1.5.4` image on 2026-09-03), awk/sed/tr from
the base image. The normal version bump and rebuild that ships any release applies.

### 4.5 App UI

- `R/app_assemble_utils.R` `assemble_opts_modal()`
  - :293 `choices = c("GetOrganelle", "MitoFinder", "MapToRef")`.
  - :301-308 help sentence: add "or map reads to a reference you supply (MapToRef)".
  - after :332 (end of the `mf_db` block) four inputs, same shape as `mf_db`:
    `textInput(ns("maptoref_ref"), "Reference (.gb or FASTA, one complete mitogenome):")`,
    `textInput(ns("maptoref"), "bowtie2 options")`,
    `textInput(ns("maptoref_consensus"), "samtools consensus options")`,
    `numericInput(ns("maptoref_iter"), "Iterations (max)", min = 1)`, each with a nested
    `opts_help(...)` (href to the bowtie2 manual and the samtools consensus man page). No
    `tool_help_icon`/`inst/tool_help` file: `bowtie2 --help` is not worth a captured
    text dump when the manual link does the job.
  - :438-447 initial hide: GetOrganelle and MitoFinder branches also hide the four new
    ids; new `MapToRef` branch hides `mitofinder`, `mf_db`, `getOrganelle`, `seeds_db`,
    `labels_db`.
- `R/app_assemble.R`
  - :847-870 `updateTextAreaInput` for the three text fields, `updateNumericInput` for
    the iteration cap.
  - :873-885 and :944-956 show/hide: add the `MapToRef` case (one hide/show block each).
  - :888-900 `toggleState` for the four ids.
  - :963-976 `rows_upsert` data.frame: add the four columns (`maptoref_iter` as integer;
    `maptoref_ref` via `input$maptoref_ref %||% ""`, not `req()`, so GetOrganelle sets
    with an empty field still save).
- No new table column in the Assemble tab, no upload widget, no validation alert. The
  post-save "no output directory for this set" warning (:1010-1052) already covers the
  new-set case.

### 4.6 Export, annotation, curation

Unchanged. `R/export.R` is assembler-agnostic; `find_sequence_gaps()` (:110) already
reports N runs "whatever put it there". WF2 keys on the header topology only.

### 4.7 Docs

- `NEWS.md`: "### Map-to-reference assembly" bullets under New Features, house style.
- `vignettes/Test-Project-Assemble.Rmd:142-176`: Assembler bullet gains MapToRef; a
  "Reference" bullet (.gb preferred, one record, complete mitogenome, put it in the
  project folder; FASTA needs `circular` in the header).
- `vignettes/Difficult-Assemblies.Rmd:16-23`: "MapToRef, like MitoFinder, always returns
  one path; N runs mark regions the reads did not cover."
- `vignettes/Your-Own-Project.Rmd:188-192`: the `new_project(assembler = "MapToRef",
  maptoref_ref = ...)` example.
- `man/` regenerated by roxygen.

### 4.8 Tests

- `tests/testthat/test-backwards-compatibility.R:431-433`: add the four columns to
  `expect_cols`; add them to `create_v1310_db()` expectations if that fixture asserts the
  assembler columns list.
- `tests/testthat/test-new-project*`: one case `new_db(assembler = "MapToRef",
  maptoref_ref = "x.gb")` stores the four values; one case that `assembler = "MapToRef"`
  without a reference stops.
- The shell loop is exercised end to end by the test project: add a `maptoref` parameter
  set to `new_test_project()` (`R/init_test_project.R`) pointing at
  `inst/test_data/fish_mito_sampler.gb`? No: that file has ten records and must fail.
  Ship `ref_dbs/MitoFinder/NC_002333_Danio_rerio.gb` (55 KB, one record, circular VRT) as
  `inst/test_data/NC_002333_Danio_rerio.gb` and use it. Two SRR samples in the test set
  are cyprinids close enough for a meaningful run; the rest exercise the divergent case.
- No R unit test for the awk/bash (there is no R function to test). If a reviewer wants
  one, move the branch body to `inst/nextflow/bin/map_to_ref.sh` (Nextflow adds
  `projectDir/bin` to PATH) and call it from a testthat `system2` with the Danio .gb and
  a 1000-read fixture. Deferred until asked.

---------------------------------------------------------------------------------------

## 5. Outputs and QC

Files in `out/<ID>/assemble/<opts>/`:
- `<ID>_assembly_1.fasta` (header `>ID.1.1 circular|linear`), or `<ID>_assembly_0.fasta`
  sentinel on failure.
- `<ID>_summary.txt`: CSV `iteration,reads_mapped,length,n_count` (one row per
  iteration) plus a final line `converged,<yes|no>`.
- `assembler.log.txt`: reference parse result (format, length, topology), bowtie2 stderr
  per iteration (alignment rate lines), any failure reason.
- `opts.txt`: the three option strings and the cap.
- `reference.gb` or `reference.fasta`: the input as given.
- `maptoref/iter1.fa .. iterN.fa`: consensus per iteration.
- `<ID>_reads.tar.gz`: the preprocessed pair (MitoFinder layout).
- From `coverage.nf` (unchanged): `<ID>_assembly_1.bam`, `_coverage.csv`,
  `_coverageStats.csv`, per-scaffold PDF, `NF_work_dir_coverage.txt`.

DB columns populated (all by existing workflow code): `assemblies` row (ID,1,1) with
`sequence`, `length`, `topology`, then `depth/gc/errors` from coverage; `assemble.paths=1`,
`scaffolds=1`, `length`, `topology`, `assemble_switch` 4 -> 2 after BLAST; `annotate` seed
row. Nothing new.

Metrics the user sees: the existing Assemble table (length, topology, BLAST hit), the
coverage details modal (depth curve, error rate, GC; ambiguity count in path scoring),
and `find_sequence_gaps()` at export for N runs. The per-iteration table lives in
`<ID>_summary.txt`, reachable from "Open output folder" (`R/app_assemble.R:1255-1262`).
An in-app iteration panel is deferred.

Failure modes and how they surface:
- Reference missing / unreadable / >1 record / non-IUPAC / empty: sentinel + log ->
  status 3 "failed assembly" in the Assemble table, reason in `assembler.log.txt`.
- Zero reads mapped in iteration 1 (wrong clade, wrong reference): same sentinel path.
- Low-coverage sample: consensus with long N runs; length still ~L so the
  `min_assembly_length` rule does not trip. Visible as N count in the summary, low depth in
  the coverage modal, gaps at export, poor ambiguity score. Acceptable for v1; a
  "> 50% N -> fail" rule is one `if` if it proves annoying.
- Iteration cap hit without convergence: `converged,no` in the summary; not an error.
- OOM: `errorStrategy` retry on 137-140 (`assemble.nf:10`), memory is `opts.memory`
  (24 GB default), far above what bowtie2 + samtools need for a 17 kb index.

---------------------------------------------------------------------------------------

## 6. Risks and mitigations

- Reference bias. The consensus is called from reads only (never `-T ref`), uncovered
  and sub-depth sites are N, and iteration re-maps to the sample's own sequence, which is
  the documented mechanism that let Geneious reach 100% at 89% identity (white paper,
  geneious-advisor 7.2). Residual bias: indels and rearrangements follow the reference.
  Mitigation: the Culicoides benchmark and Kemp both recommend a de novo cross-check; the
  same project can hold a GetOrganelle parameter set, and the app's path scoring shows
  ambiguity and depth. Documented in the vignette.
- NUMTs. MAPQ cannot separate NUMT reads from mito reads with a mito-only reference, so
  no MQ filter is claimed to help. Mitigation: `-A` marks mixed sites with IUPAC codes,
  coverage() masks depth outliers (`MeanDepth_mask`), and high `ErrorRate` windows show
  in the coverage modal. Deep NUMT contamination is a known limit of every
  map-to-reference method (alt-tools 9).
- Divergent reference. bowtie2 local mode plus iteration is the composed loop's weakest
  axis versus MIRA/MITObim (alt-tools 9). Mitigation: iteration 1 is the only pass against
  the foreign sequence; the default preset is the most permissive bowtie2 ships; users
  can lower `--score-min` or add `-N 1` in the free-form field; zero-mapped fails loudly.
  Iteration 1 read recruitment is a hard subset for later rounds: pairs with both mates
  unmapped on the reference never return. Same property as the GetOrganelle branch;
  documented.
- Rearrangements and duplications. Not detectable; the consensus inherits reference gene
  order. Documented; de novo cross-check as above.
- Low coverage. `-d 3` makes N explicit; nothing is guessed from the reference.
- Repeats / control region. Tandem repeat copy number collapses to the reference's;
  bowtie2 random placement smears reads across copies. Ambiguity codes and depth spikes
  are the visible signal. Same limit as Geneious (advisor sends tandem-repeat references
  to BBMap, which we do not have; not worth a dependency for v1).
- Circular seam. Handled by elongate + clip; correctness depends on `--no-use-MQ` (or
  `-m simple`). If a user removes `--no-use-MQ` from the consensus flags, the first 500 bp
  can turn to N; the help text says so. Insertions called exactly at the clip point are
  dropped (edge case, sub-read-length).
- Runtime. Iteration 1 = one full bowtie2 pass (the existing coverage step's cost);
  iterations 2-5 on the recruited subset are seconds to a minute; then `coverage.nf` does
  one more full pass as today. Net per sample: about 2x the current MitoFinder-branch
  coverage cost, less than a GetOrganelle run.
- Container size. Zero change.
- Nextflow `-resume`. The reference is a staged `path()` input hashed by content, so
  re-runs with the same file are cached; a URL reference is re-fetched per run like
  `mitofinder_db` is today.

---------------------------------------------------------------------------------------

## 7. Effort

Files touched (9) and approximate new lines (~220):

| File | Change | Lines |
|---|---|---|
| inst/nextflow/modules/assemble.nf | `path(ref)` input + MapToRef branch | ~65 |
| inst/nextflow/modules/assemble_workflow.nf | 4 SQL columns, opts map, tuple slots | ~10 |
| inst/nextflow/modules/coverage.nf | one condition | 1 |
| R/init_db.R | args, validator, DDL, default row, roxygen | ~25 |
| R/backwards_compatibility.R | predicate + 4 migration blocks | ~25 |
| R/app_assemble_utils.R | choice, 4 inputs, hide logic | ~45 |
| R/app_assemble.R | update/toggle/show-hide/upsert | ~30 |
| tests/testthat/test-backwards-compatibility.R (+ new-project test) | expectations | ~15 |
| NEWS.md, 3 vignettes, inst/test_data/NC_002333_Danio_rerio.gb | docs + fixture | ~40 + 55 KB |

Phases:
1. Pipeline only (assemble.nf, assemble_workflow.nf, coverage.nf, init_db.R,
   backwards_compatibility.R, fixture). Runnable via
   `new_project(assembler = "MapToRef", maptoref_ref = ...)` and the CLI updater before
   any UI exists. Half-day spike on 3 test samples first: verify `-G 12` recruitment with
   `--no-unal`, the clip awk on a real `--mark-ins` output, and convergence in <= 5 rounds.
2. App modal (app_assemble_utils.R, app_assemble.R) + tests.
3. Docs, NEWS, vignette screenshots, release.

YAGNI list (deferred until a user asks, each with the trigger):
- Sensitivity dropdown in the UI (free-form bowtie2 flags cover it; the vignette lists
  the five preset strings from section 2.4).
- Per-sample reference column in mapping.csv (one parameter set per reference works).
- Parsing `.gb` features into `blast_ref_annotations` / `blast_ref_sequences` (the
  existing BLAST + fetch path supplies reference annotations; needed only for a
  non-NCBI, hand-annotated reference).
- "Ref" no-coverage call (`samtools consensus -T`, needs samtools 1.22 bump) and `?`
  characters.
- Extension past reference ends / barcode-seed growth (SPAdes micro-assembly of the
  recruited subset, or MITObim).
- Local indel realignment (bcftools mpileup BAQ route, +0.97 MB).
- Structural variant / rearrangement discovery.
- App panel for per-iteration stats (summary.txt suffices).
- App-side reference validation and copy-into-project at save time.
- Multiple references per sample (Westbury's "combine baits" advice).
- MIA / MITObim as alternative engines.
- Extracting the branch into `inst/nextflow/bin/map_to_ref.sh` for a unit test.
