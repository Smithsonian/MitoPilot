# Map to Reference assembler: user-first design

Date: 2026-09-03
Branch: map-to-ref-assembly
Status: proposal (planning only, no code)
Angle: what the user sees and types, how the reference gets in, how results and failures are shown. Implementation stays on the seams named in codebase-map.md sections 9-10.

Line references are to the working tree on this branch (checked while writing): R/app_assemble_utils.R:288-300 (assembler picker), R/app_assemble.R:873-885 and :944-956 (show/hide), :963-976 (upsert), R/init_db.R:130-133 and :311-350, inst/nextflow/modules/assemble.nf:16-19 and :27/:56/:102, coverage.nf:35-42, assemble_workflow.nf:6-20 and :98-133, R/coverage.R:51-77, R/circularize_asmb.R:522-555, R/custom_assembly_db.R:487-649, R/blast_ref_utils.R:392-423, R/scaffold_join.R:357-380, R/export.R:110, R/app_assemble_coverage_details.R:135-190.

---

## 1. Summary

1. Add a third assembler, shown to the user as "Map to reference" (stored value `MapToRef`), chosen in the same Assembly Opts. modal as GetOrganelle and MitoFinder; the modal shows a Reference field plus four Geneious-named controls (Sensitivity, Iterate up to, Call N below depth, Consensus threshold) and hides everything GetOrganelle- or MitoFinder-specific.
2. Core tool choice: an iterate-to-consensus loop composed from what the image already ships, bowtie2 2.5.4 local mode -> samtools 1.21 sort -> `samtools consensus`, wrapped in one exported R function `map_to_ref()` called from a third `elif` in assemble.nf; zero new binaries, zero container rebuild for phase 1.
3. Why this and not MITObim/MIA: it is the only option whose knobs map one-to-one onto the Geneious dialog the user already knows (alt-tools.md section 7), it reuses the bowtie2 and circular elongate-and-fold code already in the repo, and the Geneious white paper plus every published skim protocol say the loop, not the mapper, is what makes map-to-reference work (geneious-advisor.md sections 7, 9.10).
4. The reference is one `.gb` (preferred) or FASTA per parameter set, typed as a path or URL exactly like the MitoFinder database; it is checked at save time with plain-English errors, and again inside the task. From a `.gb` we keep the sequence, topology, genetic code, organism, and gene table; phase 2 registers those in the existing `blast_ref_*` tables so the synteny view, start-gene rotation, and curation work offline against the user's own reference.
5. Results reach the user through the existing Assemble table (topology, length, Notes) and a new "Map to reference" panel in the assembly details window that reads a per-sample stats JSON from the published folder (no schema change): reads mapped, mean depth, reference covered, identity to reference, N and ambiguous-base counts, iterations run, and plain-language warnings (divergent reference, low coverage, unresolved regions, did not converge).

---

## 2. Algorithm

### 2.1 One sample, start to finish

Inputs: `<ID>_preprocess_R1.fastq.gz`, `_R2` (fastp-trimmed by PREPROCESS, so "Trim before mapping = Do not trim"), the staged reference file, the options string, cpus.

```
map_to_ref(id, ref_file, r1, r2, opts_string, cpus, out_dir)

 0. read_reference(ref_file)                         # section 3
      -> ref.fasta (single record, uppercase, IUPAC only)
      -> ref_info.json {name, accession, source, length, topology, genetic_code,
                        organism, n_count, features(if .gb)}
      on failure: write sentinel, stats json with the error, return

 1. cur <- ref.fasta ; prev_reads <- -1 ; prev_seq <- ""
    flank <- if circular min(500, len %/% 2) else 0

 2. for i in 1..iterations:                           # iterations = 0 skips the loop
      work  <- if circular  cur + substr(cur, 1, flank)  else cur      # R/coverage.R:250-259 pattern
      bowtie2-build -q work.fa idx
      bowtie2 <preset> -X 1000 --no-unal -p cpus -x idx -1 r1 -2 r2 \
        | samtools sort -@ cpus -o iter_i.bam ; samtools index iter_i.bam
      reads_i <- samtools view -c -F 0x904 iter_i.bam                  # primary mapped
      samtools consensus -a -A -d min_depth --min-MQ 20 --min-BQ 20 \
        --show-ins no --show-del no -r work:1-len iter_i.bam > iter_i.fa
      # substitutions only during iterations: coordinates stay reference-stable,
      # so the fold is a plain cut at len and iteration diffs are countable
      changed_i <- hamming(iter_i.fa, cur)
      log row: i, reads_i, mean depth, n_bases, changed_i
      if reads_i == prev_reads or iter_i.fa == prev_seq: converged <- TRUE; break
      prev_reads <- reads_i ; prev_seq <- cur ; cur <- iter_i.fa

 3. final pass (always runs, also when iterations = 0):
      work <- elongate(cur) as above
      bowtie2 ... -> final.bam
      samtools consensus -a -A -d min_depth --min-MQ 20 --min-BQ 20 \
        [threshold mode, section 2.2] -r work:1-len final.bam > consensus.fa
      # indels ON here (samtools defaults: --show-ins yes, --show-del no
      # so a deleted base is dropped, an inserted run is kept)
      strip nothing: leading/trailing N runs are KEPT so coordinates match the
      reference (linear refs); circular refs have no ends

 4. QC (section 5): samtools depth -a final.bam ; flagstat ; N/IUPAC counts;
    identity to reference via run_minimap2_paf(consensus, ref, cigar = TRUE)
    (R/scaffold_join.R:357-380, asm20 -k 13, matches/aln_len from PAF);
    gaps via find_sequence_gaps() (R/export.R:110)

 5. write outputs (section 5.1): <ID>_assembly_1.fasta with header
    ">ID.1.1 <topology>", <ID>_summary.txt, assembler.log.txt,
    <ID>_maptoref_stats.json, blast_ref_<acc>/ bundle (phase 2)
```

Stopping rule: the Geneious behaviour "iterate up to N, stop when nothing new maps" (geneious-advisor.md 5.15, 9.2) is reproduced with the aITE rule "mapped read count unchanged between rounds" OR "consensus unchanged", capped at N. Default cap 5 (the Geneious default and the mode of 27 published skim studies, section 9.8). The UI says "Iterate up to" so a high cap reads as safe, which matches Kemp's finding and the staff advice ("Geneious will only perform as many iterations as required").

Extension beyond reference ends: NOT done in phase 1. With a circular reference there are no ends (the elongate-and-fold construct lets origin-spanning reads map), which covers the stated use case (one full-length mitogenome per project). With a linear or partial reference the consensus is exactly the reference extent; the stats panel says "Extension beyond reference: 0 bp (not supported)". Growing a partial seed is the one thing MITObim does better and is deferred (section 7).

Circularity: reference topology decides the consensus header. `.gb` -> LOCUS token (`circular` / `linear`); FASTA -> the "Reference topology" control in the modal (default circular). The consensus is NOT rotated; it stays in the reference's coordinates and strand, which is what makes the `.gb` gene table reusable downstream (section 3.5). WF2 rotates to `start_gene` as it does for every other circular assembly (R/annotate.R:384).

Known approximation at the origin: reads fully inside the first `flank` bases can land on either copy in the elongated construct, so consensus depth in bases 1..flank is roughly halved during calling. Displayed depth is unaffected (coverage.nf recomputes and folds it, R/coverage.R:124). If this shows up as N calls near position 1 on low-depth samples, the phase-3 fix is to rotate the working sequence by len/2 on alternate iterations; not needed for the skims in the test set.

### 2.2 Geneious option map

Column "Ours" gives the token in the `maptoref` options string (section 4.2) and what it turns into. Fidelity: exact = same effect; approximate = same intent, different mechanism; not replicable = no equivalent, documented.

| Geneious option (dialog / XML key) | Geneious values, default | Ours (token -> flags), default | Fidelity |
|---|---|---|---|
| Mapper | Geneious, Bowtie2, BBMap, minimap2 | fixed: bowtie2 2.5.4 `--local` | approximate. Bowtie2 is the advisor's own alternative for short reads on a non-repetitive reference; the loop, not the mapper, drives the result (white paper Table 1) |
| Sensitivity (`sensitivity`) | Low/Fastest, Medium-Low/Fast, Medium/Fast, Medium-High, Highest/Medium, Custom; default Medium (Geneious 6 default and community mode: Medium-Low) | `--sensitivity medium-low` (default). low = `--fast-local`; medium-low = `--sensitive-local`; medium = `--very-sensitive-local`; high = `--very-sensitive-local -N 1 -L 16`. Custom = high preset plus `--bowtie2-args "..."` | approximate. Geneious presets vary word length and per-read mismatch/gap caps; bowtie2 presets vary seed effort and seed length; bowtie2 local mode has no per-read mismatch cap, its minimum local score (`--score-min`, default G,20,8) plays that role |
| Index Word Length (`indexWordLength`, Medium 12) | 10-15 | `-L` seed length: 20 (bowtie2 local default) for low/medium-low/medium, 16 for high | approximate (different indexing scheme) |
| Word Length (`expansionWordLength`, Medium 14) | 14-24 | none | not replicable |
| Ignore words repeated more than n times (`filterRepeats`, 20) | on/20 | none; bowtie2 places multi-hit reads at random (`multipleBestMatches`) | not replicable |
| Maximum Mismatches Per Read % (`maxMismatches`, Medium 30, Medium-Low 20, Low 10) | 2-30 | not a cap; local alignment score threshold. Custom users can set `--bowtie2-args "--score-min G,1,5"` for looser, `"--score-min G,20,10"` for stricter | approximate |
| Allow Gaps / Max Per Read % / Max Gap Size (`allowGaps` true, 15, 50) | | bowtie2 affine gap penalties (`--rdg 5,3 --rfg 5,3` defaults), no per-read cap; gaps > ~20 bp inside a read are soft-clipped instead | approximate; 50 bp gaps not replicable |
| Maximum Ambiguity (`maxAmbiguity`, 4) | | `--n-ceil L,0,0.15` (bowtie2 default, i.e. up to 15% N per read) | approximate |
| Minimum Overlap / Identity (`minOverlap` 25 / 80%, off) | off | none (off by default in Geneious too) | not replicable |
| Search more thoroughly for poor matching reads (`doMoreThoroughSearching`, false) | | folded into the preset ladder (`-D`/`-R` effort) | approximate |
| Accurately map reads with errors to repeat regions (true) | | none | not replicable |
| Map multiple best matches (`multipleBestMatches`, Randomly) | Randomly / None / All | bowtie2 default = random best; "None" approximated by `--min-mapq` (below) which drops MAPQ 0-1 reads from the consensus; "All" not offered | exact for Randomly, approximate for None |
| Minimum mapping quality (off / 30) | | `--min-mapq 20` -> `samtools consensus --min-MQ 20` | approximate (applied at consensus, not at mapping) |
| Trim paired read overhangs (true) | | none; overlapping mates are counted twice by samtools consensus | not replicable (small effect at 60% threshold) |
| Only map paired reads which map nearby (`onlyMapPairedHitsReference`, false) | off | off = bowtie2 default (mixed + discordant allowed); on = `--bowtie2-args "--no-mixed --no-discordant"` | exact |
| Paired read distance (Set Paired Reads, expected insert) | per library | `--maxins 1000` -> bowtie2 `-X 1000` (default 500 raised so long-insert skims are not called discordant); bowtie2 has no soft penalty around the mean | approximate |
| Find structural variants / Find short insertions and large deletions (false / 1000) | off | none; local mode soft-clips; rearrangements are not detected (same limitation as Geneious with these off) | not replicable, off by default anyway |
| Fine Tuning (`fineTune`): None / Iterate 3 / up to 5 / up to 10 / up to 25 / custom | Iterate up to 5 | `--iterations 5` (0 = None); stop early on convergence | approximate (Geneious re-maps to the consensus and converts back to reference coordinates; we re-map fully, keep reference coordinates by calling substitutions only until the final pass) |
| Trim Before Mapping | Do not trim (after BBDuk) | reads are fastp-trimmed in PREPROCESS; nothing here | exact |
| Consensus Threshold (`thresholdPercent`): Highest Quality 50/60/75%, 0-100%, Identical | Highest Quality 60% | `--threshold hq` (default) -> samtools Bayesian mode (base + mapping qualities, the "Highest Quality" analogue); `--threshold 60` -> `--mode simple -c 0.60 -H 0.4 -A` (below the fraction: ambiguity code, or N when no base clears it) | approximate |
| Assign Quality (Total / Highest) | Total | Bayesian mode sums qualities | approximate |
| Call N if quality below (`qualityThreshold`, 20) | 20 | `--min-bq 20` -> `--min-BQ 20` (the base is dropped from the column instead of forcing N) | approximate |
| Low coverage call: character + threshold (`coverageThreshold` 3, char ?) | ? below 3 | `--min-depth 3` -> `samtools consensus -d 3`, character N | exact for the depth rule; character is N (FASTA-legal, what annotation and export already handle) |
| If no coverage call (`noCoverageCharacterReference`): -, N, ?, Ref | ? | N, fixed. "Ref" deliberately not offered (reference bias); possible later via samtools >= 1.22 `-T ref.fa` | approximate (N instead of ?); Ref not replicable in samtools 1.21 |
| Trim to reference (`trimToReference`, false) | off | effectively ON: we never extend past the reference; consensus spans exactly the reference (indels aside) | not replicable (extension); coordinates exact |
| Ignore reads mapped to multiple locations (false) | | `--min-mapq` (above) | approximate |
| Split into separate sequences around ? (false) | | none; gaps reported as N runs in the stats panel and the Export gap finder | not applicable |
| Circular reference (document topology) | circular | elongate-and-fold, flank min(500, len/2) (R/circularize_asmb.R:529, R/coverage.R:250) | approximate, same effect as CircularMapper |
| Save assembly report / used reads / consensus | | `<ID>_maptoref_stats.json`, `<ID>_summary.txt`, consensus FASTA; BAM kept in the Nextflow work dir only (coverage.nf publishes its own BAM) | exact in spirit |

Defaults in one line (what a fresh set stores):
`--sensitivity medium-low --iterations 5 --min-depth 3 --threshold hq --topology circular`
with the internal constants `--min-mapq 20 --min-bq 20 --maxins 1000` applied unless overridden in the same string.

Why these defaults: Medium-Low was used in 19 of 36 published organelle map-to-reference studies and was Geneious's own default for years; 5 iterations is the Geneious default and the mode of published counts, and early stopping makes a higher cap free; depth 3 is the Geneious `coverageThreshold` default for reference assemblies; Highest Quality is what the Geneious tutorial recommends whenever reads carry qualities; topology circular because the target is a mitogenome and a `.gb` overrides it anyway (geneious-advisor.md 8.3, 9.8, 10.2, 10.4).

---

## 3. Reference input

### 3.1 What the user supplies

One reference per parameter set: a path or URL to a single-record GenBank flat file (`.gb`, `.gbk`, `.gbff`, `.genbank`) or a single-record FASTA. Typed into a text field, exactly like the MitoFinder database (no upload widget exists in the app and none is added). URLs are accepted because Nextflow stages `path()` inputs from http(s) (assemble_workflow.nf:113 -> assemble.nf:16 pattern for `mf_db`).

Per project vs per sample: per parameter set in phase 1. That already gives per-sample references, because each sample points at a set (`assemble.assemble_opts`, R/init_db.R:268) and sets are cheap to create in the modal (type a new name, tick Edit, Update). The vignette shows the recipe: "one set per reference; assign samples to the set whose reference is closest". Phase 2 adds an optional `Reference` column in mapping.csv (bare path), stored in `samples` for free (R/init_db.R:178-198) and read as `COALESCE(s.Reference, opts.maptoref_ref)` in the sqlRead, the same shape as userAsmb's `samples.assembly` (coverage_userAsmb_workflow.nf:5,:319).

### 3.2 Validation, in two places, one function

`read_reference(path)` (new, in R/map_to_ref.R, pure R, reuses the parsers below) returns a list or a classed error with a plain-English message. It is called:

- at save time in the modal (head-node, reads a 50 KB file, not "heavy work"), skipped when the value is a URL or the path is unreachable from the app host (HPC login node without the data mount): then it warns "could not check the reference from here; it will be checked when the pipeline runs";
- inside the assemble task (always), so an invalid file becomes a failed-assembly row with the same message in Notes instead of a Nextflow crash.

Checks and their messages (sweet alert title "Reference problem", body = the message):

| Check | Message |
|---|---|
| file missing (non-URL) | "Reference file not found: <path>" |
| first non-blank line neither `LOCUS` nor `>` | "Reference must be a GenBank flat file (.gb) or a FASTA file; the file starts with '<first 40 chars>'" |
| GenBank with 0 or >1 `//` records | "Reference must contain exactly one record; this file has <n>. For a multi-record file, extract the mitogenome you want (for example with seqkit or by copying the record) and try again" |
| FASTA with >1 sequence | "Reference FASTA must contain one sequence; this file has <n>" |
| non-IUPAC characters after stripping whitespace/digits | "Reference contains characters that are not nucleotides (<examples>); gaps '-' and 'X' or 'U' are not allowed" (regex from R/blast_ref_utils.R:397) |
| length < 5,000 or > 50,000 bp | "Reference is <n> bp; MitoPilot expects a full mitogenome (5-50 kb). Partial references are not supported yet" |
| length outside 10-25 kb | warning only: "Reference is <n> bp, unusual for a metazoan mitogenome; check that this is the sequence you meant" |
| N or IUPAC content > 1% | warning only: "Reference has <n> ambiguous bases (<pct>%); they lower mapping near those sites" |
| `.gb` `/transl_table` differs from `samples.genetic_code` of the selected samples | warning only: "Reference uses genetic code <a>, the selected samples use <b>" |
| `.gb` has no `/organelle="mitochondrion"` and no "mitochond" in DEFINITION | warning only: "Reference record does not say it is mitochondrial" |
| FASTA and topology not set | no message; the Reference topology control supplies it (default circular) |

Warnings do not block Update; they are shown once and written into the stats JSON so they reappear in the details panel.

### 3.3 Parsing, all pure R already in the package

- GenBank: split on `//` and read VERSION, DEFINITION, feature table, ORIGIN with the `.cadb_parse_gb` / `.cadb_record_cds` / `.cadb_parse_location` logic (R/custom_assembly_db.R:487-649). Two small additions: strip `\r` before the `//` test (CRLF files otherwise yield "no records"), and read the LOCUS line token-wise for `circular`/`linear` and the declared length (NCBI gbrel.txt 3.4.4.1 recommends tokens over columns). Features kept: CDS, rRNA, tRNA, D-loop/misc_feature named control region; `/gene` else `/product`, run through `normalize_mito_gene()` (R/blast_ref_utils.R:978) so names match MitoPilot's vocabulary; `/transl_table` from the first CDS, default 2; ORGANISM line for the organism string.
- FASTA: `Biostrings::readDNAStringSet` in `tryCatch` (pattern R/custom_curation_db.R:199-214), one record, uppercase, first header token as the name.
- Common: IUPAC regex check, N count, length window, write `ref.fasta` (single record named by accession or header token) with `writeXStringSet`.

### 3.4 Where the file lives and how it reaches the container

Stored as typed (path or URL) in `assemble_opts.maptoref_ref`, read by the sqlRead, staged into the task by a `path(ref)` input next to `path(mf_db)` (assemble.nf:16), with `${projectDir}/assets/NO_FILE` when the set is not MapToRef (annotate_workflow.nf:92 pattern). No copying into the project folder in phase 1: this is exactly how the MitoFinder `.gb` already travels, and the docs carry the same Singularity note (bind the directory holding the reference, or put it inside the project folder). Copying a validated reference into `<project>/references/` at save time is listed as a phase-2 convenience.

### 3.5 What we keep from the .gb, and what it buys the user later

Written by the task into the published set folder:

- `ref_info.json`: name, accession, source (gb/fasta), length, topology, genetic code, organism, N count, and the normalized feature table (gene, type, pos1, pos2, direction). Read by the details panel (section 5.3) for the "Reference" block.
- Phase 2: the same four files the NCBI fetch writes, `blast_ref_<accession>/{blast_ref_annotations.csv, blast_ref_sequence.txt, blast_ref_genetic_code.txt, remote_blast_ref.json}`, via `.write_ref_files()` (R/blast_ref_utils.R:401-423), so downstream code needs no new reader.

Downstream benefits, in the order they matter to a user:

1. Topology and coordinates. The consensus is in the reference's frame and strand, so a `.gb` reference means the user's own gene coordinates are approximately the sample's gene coordinates before annotation even runs; the synteny view shows a straight diagonal when nothing is rearranged.
2. Start gene and orientation. Once the reference is registered in `blast_ref_sequences` / `blast_ref_annotations` (phase 2, sqlInsert statements identical to blast_ref_fetch_workflow.nf:49-55) and a `blast_ref_override` row names it for each unit (R/blast_ref_utils.R:22-29), `unit_ref_rotation()` (R/blast_ref_utils.R:1268-1292) rotates the reference to `start_gene` and `ref_based_rc` in curation flips the sample to the reference strand, both with no code change.
3. Gene order check. The existing synteny alignment (compute_blast_ref_alignment, R/blast_ref_utils.R:1389) draws sample vs reference; a rearrangement or duplication missed by mapping (the known blind spot, geneious-advisor.md 10.3) shows as an off-diagonal block. The Difficult Assemblies vignette tells users to look there when the "identity to reference" warning fires.
4. Curation refHits. `prepend_blast_hit_to_refhits()` (R/blast_ref_utils.R:649) adds the reference's own PCG translations to the top of each gene's hit list, so a reference from an under-represented clade improves curation exactly where the BLAST default databases are thin.
5. MitoFinder gap-fill annotation. A single-record annotated `.gb` is already a valid MitoFinder `-r` database (name must end in `.gb`/`.genbank`); the docs suggest reusing the same file as `annotate_opts.mitofinder_db`.
6. Offline projects. With phase 2 in place, `run_blast = 0` plus a `.gb` reference gives a complete WF1 without any NCBI call; the reference tables are populated from the user's file instead of the fetch.

---

## 4. Integration

### 4.1 Touch points (minimal diff, third `elif`)

R package
1. R/init_db.R
   - :45 roxygen: `assembler` choice of "GetOrganelle" (default), "MitoFinder", or "MapToRef"; new `@param maptoref_ref` (path/URL to a single-record .gb or FASTA), `@param maptoref` (options string, default above).
   - :70 args: `maptoref_ref = NA_character_`, `maptoref = "--sensitivity medium-low --iterations 5 --min-depth 3 --threshold hq --topology circular"`.
   - :130-133 validator: allow "MapToRef"; if `assembler == "MapToRef"` and `maptoref_ref` is empty, stop("MapToRef needs a reference: pass maptoref_ref = '/path/to/reference.gb'").
   - :311-328 DDL: `maptoref_ref TEXT, maptoref TEXT`; :330-350 default row gets both.
2. R/backwards_compatibility.R: two guarded ALTER TABLE + backfill blocks after :1313 (copy the `mitofinder` block shape :1295-1313; defaults NA and the options string); add both names to the "already current" predicate at :176-178. Not added to `schema_gaps()` (assemble_opts columns are not gaps today).
3. R/map_to_ref.R (new): `read_reference()`, `map_to_ref()` (exported; the task calls `Rscript -e "MitoPilot::map_to_ref(...)"`), `parse_maptoref_opts()` (string -> list, defaults filled), `compose_maptoref_opts()` (list -> string, used by the modal), `maptoref_stats()`.
4. R/app_assemble_utils.R `assemble_opts_modal()`
   - :293 `choices = c("GetOrganelle", "MitoFinder", "Map to reference" = "MapToRef")` (named vector: label differs from stored value).
   - :301-308 help sentence gains the third tool.
   - after :365, the MapToRef block (wireframe 4.3): `textInput(ns("maptoref_ref"))`, `selectInput(ns("maptoref_sensitivity"))`, `numericInput(ns("maptoref_iterations"))`, `numericInput(ns("maptoref_min_depth"))`, `selectInput(ns("maptoref_threshold"))`, `selectInput(ns("maptoref_topology"))`, and a collapsed `textInput(ns("maptoref"))` labelled "Advanced options" showing the composed string; each with nested `opts_help()`.
   - :438-447 initial hide: MapToRef hides the five GetOrganelle/MitoFinder inputs; the other two hide the MapToRef block.
5. R/app_assemble.R
   - :125-126 `register_tool_help("maptoref", ...)` pointing at a hand-written inst/tool_help/maptoref.txt (there is no CLI `--help` to capture; the file documents the option tokens).
   - :847-870 populate the MapToRef inputs from `parse_maptoref_opts(cur$maptoref)` and `cur$maptoref_ref`; :873-885 and :944-956 show/hide branches.
   - :888-900 `toggleState` for the new ids.
   - new observer: when any structured MapToRef input changes, `updateTextInput("maptoref", compose_maptoref_opts(...))`; when the advanced string is edited by hand, re-parse and update the structured inputs (guard against loops with `ignoreInit` and a value comparison).
   - :963-976 upsert gains `maptoref_ref = input$maptoref_ref %||% NA`, `maptoref = input$maptoref %||% NA`.
   - before the upsert, when `input$assembler == "MapToRef"`: `req(nzchar(input$maptoref_ref))` with the sweet alert "Map to reference needs a reference file"; then `read_reference()` validation per section 3.2 (URL or unreachable path -> warning, continue).
6. R/app_assemble_coverage_details.R: new `uiOutput(ns("maptoref_div"))` after `reactableOutput(ns("table"))` (:171); server reads `<dir_out>/<ID>/assemble/<opts>/<ID>_maptoref_stats.json` if it exists (same directory resolution as :96-121) and renders the panel in section 5.3. Nothing rendered for other assemblers.
7. inst/tool_help/maptoref.txt (new, hand-written; `tools/capture_tool_help.sh` is not involved).
8. inst/test_data/NC_002333_Danio_rerio.gb: copy of ref_dbs/MitoFinder/NC_002333_Danio_rerio.gb (55 KB, single record, LOCUS circular) for tests and the test project.
9. R/init_test_project.R: optional second parameter set "maptoref" (assembler MapToRef, reference = the packaged Danio file) assigned to none by default; documented in the vignette as "switch a sample to it to try the method".

Nextflow
10. inst/nextflow/modules/assemble_workflow.nf
    - :6-20 sqlRead: append `opts.maptoref_ref, opts.maptoref` at the END (house rule: new columns last, positional indices).
    - :102-108 opts map gains `maptoref: it[20]`; the `opts:` tuple gains `it[19]` (reference) after `it[8]`; :183-195 cross map passes it; wrap as `file(x ?: "${projectDir}/assets/NO_FILE")`.
    - phase 2 only: read `note=` from `<ID>_summary.txt` in the pass branch (:272-296) and append `[maptoref] <text>` to `assemble_notes` using the `appendTaggedNoteSql` helper shape from blast_ref_fetch_workflow.nf:18-21.
11. inst/nextflow/modules/assemble.nf
    - :16 input gains `path(ref)`; output tuple unchanged (`opts.assembler` flows as it[7]).
    - after :101: `elif [ "!{opts.assembler}" = "MapToRef" ]; then mkdir -p !{outDir}; Rscript -e "MitoPilot::map_to_ref('!{id}', '!{ref}', '!{reads[0]}', '!{reads[1]}', '!{opts.maptoref}', !{opts.cpus}, '!{outDir}')"; tar -czvf !{outDir}/!{id}_reads.tar.gz *.fastq.gz; echo ... > NF_work_dir_assemble.txt; fi`. The R function writes `<ID>_assembly_1.fasta` (or the `_assembly_0.fasta` sentinel), `<ID>_summary.txt`, `assembler.log.txt`, `opts.txt`, `<ID>_maptoref_stats.json`, `ref_info.json`, `ref.fasta`. Reads archived in the MitoFinder layout (`<ID>_preprocess_R{1,2}.fastq.gz` at the archive root).
    - cpus/memory inherited through the `opts` input (config closure, inst/config.local:12-13), nothing to add.
12. inst/nextflow/modules/coverage.nf :40: `elif [ !{assembler} == "MitoFinder" ]` becomes `else` (any assembler that archives the preprocessed pair). One-line change; coverage() then produces the exact coverageStats.csv contract.
13. inst/config.* (8 templates), R/app_run_pipline.R `MITOPILOT_PROCESS_ORDER`: untouched (no new process).

Container
14. docker/Dockerfile: no change for phase 1 (bowtie2 2.5.4, samtools 1.21 with `consensus`, minimap2 2.28 present; probed 2026-09-02). Phase 3 optional: `samtools=1.24` to unlock `-T` (fill with reference) and threading.

Export
15. R/export.R: no change. The consensus is a normal `assemblies` row; `find_sequence_gaps()` already declares N runs as gaps "whatever put it there" (:101-103); ambiguity codes are already handled (NEWS 1.5.4).

Docs
16. NEWS.md "### Map to reference assembler" bullets; vignettes per section 6 of this document (new Map-to-Reference.Rmd; edits to Test-Project-Assemble.Rmd:150-162, Difficult-Assemblies.Rmd:16-23 and :45-47, Your-Own-Project.Rmd:188-192, FAQ.Rmd, Troubleshooting.Rmd); man/ regenerated.

Tests
17. tests/testthat/test-backwards-compatibility.R:431-433 expected columns; new test-map-to-ref.R: `read_reference()` on the packaged Danio .gb (topology circular, 16,596 bp, code 2, 13 PCG), on fish_mito_sampler.gb (rejects: 10 records), on a CRLF copy, on a FASTA with two records (rejects), on a `-`-containing sequence (rejects); `parse_maptoref_opts()`/`compose_maptoref_opts()` round trip; `map_to_ref()` end to end with stub `bowtie2`/`bowtie2-build`/`samtools` scripts on PATH (pattern tests/testthat/test-find-mito.R:248-255) that emit a canned BAM-free flow (the stubs write the expected consensus FASTA and counts), asserting the header `>ID.1.1 circular`, the stats JSON keys, early stop on unchanged read count, and the sentinel on an invalid reference; a stats-to-warnings test on synthetic numbers.

### 4.2 Data model

| Table.column | Type | Meaning | Default |
|---|---|---|---|
| assemble_opts.maptoref_ref | TEXT | path or URL to the single-record reference (.gb or FASTA) | NA (required when assembler = MapToRef) |
| assemble_opts.maptoref | TEXT | options string, tokens: `--sensitivity low|medium-low|medium|high`, `--iterations N`, `--min-depth N`, `--threshold hq|<pct>`, `--topology circular|linear`, plus advanced `--min-mapq N`, `--min-bq N`, `--maxins N`, `--bowtie2-args "..."` | `--sensitivity medium-low --iterations 5 --min-depth 3 --threshold hq --topology circular` |

One options string rather than six columns for the same reason GetOrganelle and MitoFinder have one: one migration block, one upsert entry, one tuple slot, and advanced users get a place to put a bowtie2 flag without a release. The modal's structured controls are a view on that string.

No change to `assemblies`, `assemble`, `annotate`, or the `blast_ref_*` tables. Per-sample stats live in `<ID>_maptoref_stats.json` in the published set folder, read on demand like coverageStats.csv.

### 4.3 Wireframes

Assembly Opts. modal, assembler = Map to reference (everything else in the modal unchanged):

```
 +--------------------------------------------------------------------------+
 | Assembly options                                                          |
 |  Parameter set name: [ maptoref_danio        v ]   [x] Edit               |
 |  CPUs: [ 6 ]        Memory (GB): [ 24 ]                                   |
 |  +--------------------------------------------------------------------+  |
 |  | Assembler                                                          |  |
 |  | [ Map to reference                                       v ]       |  |
 |  |  Tool used to assemble the mitogenome from reads: GetOrganelle,    |  |
 |  |  MitoFinder, or Map to reference (iterative mapping to a single    |  |
 |  |  reference, Geneious style); the relevant options appear below.    |  |
 |  |                                                                    |  |
 |  | Reference (.gb or FASTA): [ /data/refs/NC_002333_Danio_rerio.gb  ] |  |
 |  |  One complete mitogenome, GenBank format preferred (topology,      |  |
 |  |  genetic code, and gene positions are read from it). Path or URL.  |  |
 |  |  (learn more)                                                      |  |
 |  |                                                                    |  |
 |  | Sensitivity:            [ Medium-Low / Fast          v ]           |  |
 |  |  Low / Fastest, Medium-Low / Fast (default), Medium / Fast,        |  |
 |  |  High / Slow. Raise it if few reads map or the reference is from a |  |
 |  |  different genus.                                                  |  |
 |  | Iterate up to:          [ 5  ] times                               |  |
 |  |  Re-maps reads to the growing consensus; stops early when nothing  |  |
 |  |  new maps. 0 = single pass. Use 10-25 for a distant reference.     |  |
 |  | Call N below depth:     [ 3  ] reads                               |  |
 |  |  Positions with fewer reads become N; never filled from the        |  |
 |  |  reference.                                                        |  |
 |  | Consensus threshold:    [ Highest Quality (60%)      v ]           |  |
 |  |  Highest Quality (default), 50%, 60%, 75%, 90%. Percentages call   |  |
 |  |  an ambiguity code when no base reaches the threshold.             |  |
 |  | Reference topology:     [ circular                   v ]           |  |
 |  |  Used for FASTA references; a GenBank LOCUS line overrides it.     |  |
 |  |                                                                    |  |
 |  | > Advanced options (click to expand)                               |  |
 |  |   [ --sensitivity medium-low --iterations 5 --min-depth 3        ] |  |
 |  |   [ --threshold hq --topology circular                           ] |  |
 |  |   Mirrors the controls above; add --bowtie2-args "..." here. (?)   |  |
 |  +--------------------------------------------------------------------+  |
 |  Max assembly paths: [ 10 ]   Max scaffolds: [ 10 ]   (unchanged)         |
 |  Min assembly length (bp): [ 500 ]                                        |
 |  [ ] Automatically join multi-scaffold assemblies (Path 0)                |
 |                                             [ Update ]  [ Cancel ]        |
 +--------------------------------------------------------------------------+
```

On Update with a bad reference:

```
 +-----------------------------------------------+
 |  (!) Reference problem                         |
 |  Reference must contain exactly one record;    |
 |  this file has 10. For a multi-record file,    |
 |  extract the mitogenome you want and try again.|
 |                                    [ OK ]      |
 +-----------------------------------------------+
```

Assemble table: no new columns. What changes per row for a MapToRef sample: Topology (from the reference), Asmb. Length (consensus length), # Paths = 1, # Scaffolds = 1, BLAST columns as usual (BLAST still runs by default), Notes carries `[maptoref] ...` warnings in phase 2. The set name in the Assembly Opts. column is the only visible hint of the method, as it is for MitoFinder today; users are told in the vignette to name sets after the method and reference (`maptoref_danio`).

Assembly details window (existing modal, one new panel between the paths table and the consensus/join tools):

```
 +--------------------------------------------------------------------------+
 | Assembly details for ID: SRR21844202          Taxon: Danio sp.           |
 |  Multiple assembly paths? How do I choose?                               |
 |  [ paths table: Ignore | Path | Scaffold | Flags | Topology | Length ...]|
 |                                                                          |
 |  +---- Map to reference ---------------------------------------------+   |
 |  | Reference   NC_002333.2  Danio rerio  16,596 bp  circular  code 2 |   |
 |  |             13 PCG, 2 rRNA, 22 tRNA, 1 control region (from .gb)  |   |
 |  | Settings    Medium-Low / Fast, iterate up to 5, N below 3 reads,  |   |
 |  |             Highest Quality                                       |   |
 |  |                                                                   |   |
 |  | Reads mapped        41,812 of 12,304,556 (0.34%)                  |   |
 |  | Mean depth          312x  (median 298x, min 4x at 15,880-15,902)  |   |
 |  | Reference covered   99.8% of positions at >= 3 reads              |   |
 |  | Identity to ref     96.1%  (642 substitutions, 5 ins, 3 del)      |   |
 |  | Consensus length    16,594 bp  (reference 16,596)                 |   |
 |  | Unknown bases       27 N (1 run >= 10 bp: 15,880-15,902)          |   |
 |  | Ambiguous bases     4 IUPAC                                       |   |
 |  | Iterations          4 of up to 5, converged (no new reads mapped) |   |
 |  | Extension           0 bp beyond reference (not supported)         |   |
 |  |                                                                   |   |
 |  | Iteration  Reads mapped  Mean depth  N bases  Bases changed       |   |
 |  |     1        39,905        297x        188      611               |   |
 |  |     2        41,530        309x         41       48               |   |
 |  |     3        41,812        312x         27        3               |   |
 |  |     4        41,812        312x         27        0               |   |
 |  |                                                                   |   |
 |  | Warnings                                                          |   |
 |  |  (!) 1 region below 3 reads (15,880-15,902, control region);      |   |
 |  |      called as N. Inspect in the coverage plot below.             |   |
 |  +-------------------------------------------------------------------+   |
 |                                                                          |
 |  [ coverage / consensus / scaffold join / MSA panels as today ]          |
 |  Notes: [                                                     ]          |
 |                                                  [ Align ] [ Close ]     |
 +--------------------------------------------------------------------------+
```

The panel is a plain key/value table plus a small reactable and a warning list, rendered from the JSON; no plot of its own (the per-scaffold coverage plot that already exists sits below it).

### 4.4 Project init from R

```r
new_project(
  path = "~/proj_danio", mapping_fn = "mapping.csv", data_path = "~/reads",
  assembler = "MapToRef",
  maptoref_ref = "~/refs/NC_002333_Danio_rerio.gb"
)
# optional: maptoref = "--sensitivity medium --iterations 10 --min-depth 5 --threshold 60"
```

Both arguments reach `new_db()` through `...` (R/init_project.R:125-133), like `assembler`/`mitofinder_db` today. Error if `maptoref_ref` is missing: "MapToRef needs a reference: pass maptoref_ref = '/path/to/reference.gb'".

---

## 5. Outputs and QC

### 5.1 Files written (published set folder `out/<ID>/assemble/<set>/`)

| File | Content | Consumer |
|---|---|---|
| `<ID>_assembly_1.fasta` | one record `>ID.1.1 circular|linear`, uppercase, N and IUPAC allowed | everything downstream (contract codebase-map 10.B) |
| `<ID>_assembly_0.fasta` | `>No assembly found` sentinel when the reference is invalid, no reads map, or the consensus is all N | assemble_workflow.nf:356-367 -> status 3 |
| `<ID>_reads.tar.gz` | `<ID>_preprocess_R1.fastq.gz`, `_R2` (MitoFinder layout) | coverage.nf (now an `else`) |
| `<ID>_summary.txt` | human-readable: reference line, settings line, per-iteration table, final metrics, `note=` line (phase 2 Notes source) | user; Groovy note reader (phase 2) |
| `assembler.log.txt` | bowtie2 and samtools stderr per iteration, timings | user, troubleshooting |
| `opts.txt` | the options string | parity with other assemblers |
| `<ID>_maptoref_stats.json` | machine-readable stats (5.2) | details panel |
| `ref_info.json`, `ref.fasta` | parsed reference (3.5) | details panel; MitoFinder gap-fill (manual); phase 2 registration |
| `blast_ref_<acc>/` (phase 2) | the four fetch-shaped files | synteny view, rotation, curation, scaffold join |
| `NF_work_dir_assemble.txt` | work dir path | app work-dir browser |

Not published: iteration BAMs and the final BAM (work dir only; coverage.nf publishes `<ID>_assembly_1.bam` and the coverageStats CSV as for every assembler).

### 5.2 Stats JSON (keys the panel reads)

```
reference: {name, accession, source, length, topology, genetic_code, organism,
            n_count, n_features: {PCG, rRNA, tRNA, ctrl}}
settings:  {sensitivity, iterations_cap, min_depth, threshold, topology,
            min_mapq, min_bq, maxins, bowtie2_args}
iterations: [{i, reads_mapped, mean_depth, n_bases, changed}]
final: {reads_total, reads_mapped, pct_mapped, mean_depth, median_depth,
        min_depth_pos, positions_below_min_depth, pct_covered,
        consensus_length, identity_pct, substitutions, insertions, deletions,
        n_count, n_runs: [{start, end, length}], iupac_count,
        iterations_run, converged, extension_bp: 0, runtime_s}
warnings: [{code, text}]
error: null | text
```

DB columns populated: only the existing ones, by the existing workflow (`assemblies` row, `assemble` paths/scaffolds/length/topology/status, `annotate` seed; then `assemblies.depth/gc/errors` by COVERAGE). No new columns.

### 5.3 Metrics and warnings shown, with the rule and why

| Shown as | Rule | Why this threshold |
|---|---|---|
| Reads mapped | count and % of preprocessed pairs | Geneious assembly report shows it; skims usually give 0.1-1% |
| Mean / median depth | from `samtools depth -a` on the final BAM | tutorial: 10x minimum, 20-30x recommended |
| Reference covered | % positions with depth >= min_depth | the honest "did we recover it or inherit N" number (snippy `.aligned.fa` idea) |
| Identity to reference | matches / alignment length from minimap2 PAF (`-c`, asm20 -k 13) | reference-bias check advised by Kemp and the Culicoides benchmark |
| Consensus vs reference length | consensus bp, reference bp | Westbury: divergent baits under-recover length |
| Unknown / ambiguous bases | N count with runs >= 10 bp listed (find_sequence_gaps), IUPAC count | same numbers Export and the Ambig. Bases column use |
| Iterations | run vs cap, converged or not | so a user knows whether raising the cap can help |
| Extension | fixed 0 bp with "(not supported)" | keeps the Geneious "trim to reference" expectation explicit |

Warnings (code -> text; all also end up in Notes in phase 2, prefixed `[maptoref]`):

| Code | Fires when | Text |
|---|---|---|
| ref_divergent | identity < 90% | "Reference is more than 10% divergent; expect reference bias and missing regions. Use a closer reference, raise Sensitivity or Iterate up to, and compare with a de novo assembly (GetOrganelle set) before trusting rearrangements." (89% identity is the white paper's demonstrated floor with iterations; Medium-Low tolerates about 20% per read) |
| low_depth | mean depth < 20 | "Mean depth 14x is low; positions under 3 reads are N. More reads or a lower Call N threshold (not below 2) may help." |
| incomplete | positions below min_depth > 2% of reference | "3.4% of the reference could not be called (N); regions listed above." |
| short | consensus length < 95% of reference | "Consensus is 9% shorter than the reference (deletions or uncalled ends)." |
| not_converged | iterations_run == cap and last `changed` > 0 | "Still changing after 5 iterations; raise Iterate up to (10-25) and re-run." |
| few_reads | reads_mapped < 1,000 | "Only 612 reads mapped; check that the reference is a mitogenome from a related taxon." |
| ref_ambiguous | reference n_count > 1% | "Reference has 213 ambiguous bases; mapping is weaker there." |
| code_mismatch | .gb transl_table != sample genetic_code | "Reference genetic code 2 differs from sample code 5; annotation uses the sample's." |
| origin_lowdepth | any N within 500 bp of position 1 on a circular reference | "Uncalled bases near the origin; the junction region has reduced calling depth." |

### 5.4 Failure modes and how they surface

| Failure | Where caught | What the user sees |
|---|---|---|
| Reference path wrong / unreadable | modal save (if reachable) else task | sweet alert at save; else Assemble state 3 "failed assembly", Notes `[maptoref] Reference file not found: ...`, stats JSON `error` shown in details |
| Multi-record .gb, multi-record FASTA, non-nucleotide characters, length out of range | same | same, with the section 3.2 message |
| URL unreachable at run time | Nextflow staging error, task not started | run fails with the Nextflow error in .logs/nextflow.log; Troubleshooting vignette entry "reference URL could not be downloaded" |
| Zero reads map | task | sentinel + Notes `[maptoref] No reads mapped to the reference`; state 3 |
| Consensus all N or below min_assembly_length | existing rule (assemble_workflow.nf:288-292) | state 3 "All scaffolds below min assembly length" |
| bowtie2/samtools crash | `tryCatch` in `map_to_ref()` | sentinel + Notes `[maptoref] Mapping failed, see assembler.log.txt`; exit 0 so the batch continues (codebase-map 10.K) |
| Out of memory | Nextflow retry (exit 137) | as today: retry once with doubled memory |
| Partial assembly (N runs, low identity) | QC warnings | state 2/4 as usual; warnings in the details panel (and Notes in phase 2); the sequence is annotated like any other, N runs become INSDC gaps at export |

---

## 6. Documentation and vignette outline

New vignette `vignettes/Map-to-Reference.Rmd` ("Assembling by mapping to a reference"):

1. When to use it: you have a closely related complete mitogenome and either GetOrganelle/MitoFinder failed, coverage is low, or you want a Geneious-style consensus reproducibly on many samples. When not to: no reference within roughly 10% divergence, suspected rearrangements or duplications (map-to-reference cannot see them), partial references.
2. Getting a reference: NCBI nucleotide search tips (RefSeq NC_ accession, "complete genome", mitochondrion), download as GenBank (full) not FASTA, one record per file; or reuse a MitoPilot export of a finished sample.
3. Setting it up: (a) at project creation with `assembler = "MapToRef", maptoref_ref = ...`; (b) in the app, one parameter set per reference, screenshot of the modal; assigning samples to sets.
4. The options, in Geneious words: Sensitivity, Iterate up to, Call N below depth, Consensus threshold, Reference topology, Advanced string; the defaults and when to change each (table mirroring section 2.2 defaults, with the "distant reference" recipe: High, 25, depth 3).
5. Reading the results: the details panel walk-through with the wireframe screenshot; what each warning means and what to do.
6. Checking for reference bias: run the same samples with a GetOrganelle set and compare in the assembly details Align tool; look at the synteny view for rearrangements; try a second reference.
7. Limits: no extension past reference ends, N instead of ?, indels from read alignments only, NUMT caveat, control region.
8. HPC notes: Singularity bind for the reference directory; URL references.

Edits to existing pages:
- Test-Project-Assemble.Rmd:150-162: third bullet under Assembler; new bullet "Reference (Map to reference only)".
- Difficult-Assemblies.Rmd:16-23 and :45-47: "Map to reference always returns a single path and a single scaffold"; add a paragraph "Map-to-reference consensus with N runs" pointing at the gap list.
- Your-Own-Project.Rmd:188-192: the `new_project()` example with `maptoref_ref`.
- custom_dbs.Rmd: short section "A single reference for Map to reference is not a database; any single-record .gb works, including the ones `custom_assembly_db(db_type = "mitofinder")` writes if you keep one record".
- FAQ.Rmd: "Can I use MitoPilot instead of Geneious Map to Reference?" (yes, with the fidelity caveats in one paragraph).
- Troubleshooting.Rmd: the failure table from 5.4.
- NEWS.md: "### Map to reference assembler" under New Features, user-facing bullets.
- inst/tool_help/maptoref.txt: the option tokens and defaults, one line each.

---

## 7. Risks and mitigations

| Risk | Effect on the user | Mitigation in this design |
|---|---|---|
| Reference bias | consensus drifts toward the reference in divergent regions; Culicoides benchmark saw 2x the differences with high sensitivity | reads-only consensus, N never filled from the reference ("Ref" not offered); `ref_divergent` warning at < 90% identity; vignette section 6 tells users to cross-check with a de novo set; Medium-Low default, not High |
| NUMTs in skims | inflated ambiguity codes, wrong bases at NUMT sites | `--min-mapq 20` drops multi-mappers; Highest Quality threshold calls the majority; IUPAC count surfaced; documented as a limitation like every metazoan tool |
| Divergent reference (> 10-15%) | fewer reads map, N runs, shorter consensus, or convergence to a mosaic | High preset + 25 iterations recipe; `ref_divergent`, `incomplete`, `short`, `few_reads` warnings; per-set references so users can try a second reference cheaply |
| Rearrangements / duplications | invisible to mapping; consensus silently follows the reference gene order | stated up front in the vignette; synteny view against the .gb (phase 2) shows off-diagonal blocks; Difficult Assemblies advises a de novo comparison |
| Low coverage | N runs, `not_converged` | depth 3 default (Geneious default), `low_depth` warning, user can lower to 2; existing coverage plot shows where |
| Repeats / control region | low identity and N at the D-loop; origin sits in or near it | origin handled by elongate-and-fold; `origin_lowdepth` warning; N runs listed with coordinates |
| Indels | read-level indels from CIGAR only, no realignment (the one real fidelity gap vs Fine Tuning) | substitution-only iterations keep coordinates stable; indels applied once at the final pass; bcftools route deferred |
| Runtime | up to cap+1 bowtie2 runs on the full read set (each about the cost of the existing coverage step, minutes on 6 CPUs for a skim) | early stop; 5 default; `runtime_s` in the stats; vignette suggests fewer iterations on very deep libraries |
| Container size / rebuild | none in phase 1 | all tools present; samtools bump is phase 3 and optional |
| Head-node validation on HPC | reference path not visible from the app host | validation is skipped with a warning when unreachable and repeated in the task |
| Options string drift | user edits the advanced string by hand into something unparseable | parser tolerates unknown tokens (passed to nothing, warned in the log), structured controls re-sync from the string, defaults fill missing tokens |

---

## 8. Effort

Rough size (phase 1): 12 files touched, 2 new R files/fixtures, about 900-1,100 new lines including tests and docs.

| File | Change | Lines |
|---|---|---|
| R/map_to_ref.R (new) | read_reference, opts parse/compose, loop, stats, warnings, sentinel | 350-400 |
| R/init_db.R | args, validator, DDL, default row, roxygen | 20 |
| R/backwards_compatibility.R | two migration blocks, predicate | 40 |
| R/app_assemble_utils.R | picker choice, MapToRef inputs block, hide logic | 90 |
| R/app_assemble.R | populate/show/hide/toggle, sync observer, save validation, upsert | 70 |
| R/app_assemble_coverage_details.R | stats panel | 80 |
| inst/nextflow/modules/assemble_workflow.nf | sqlRead + two tuple slots + NO_FILE | 8 |
| inst/nextflow/modules/assemble.nf | `path(ref)` + elif branch | 25 |
| inst/nextflow/modules/coverage.nf | elif -> else | 1 |
| inst/tool_help/maptoref.txt, inst/test_data/NC_002333_Danio_rerio.gb | new | 30 + fixture |
| tests/testthat/test-map-to-ref.R, test-backwards-compatibility.R | new tests, column expectation | 150 |
| vignettes, NEWS.md, man/ | docs | 200 |

Phases

- Phase 1 (ship): everything in section 4.1 items 1-5, 7-8, 10-12, 14-17 except the phase-2 notes; details panel (item 6) with metrics and warnings from the JSON; new vignette. Outcome: a user can pick Map to reference, point at a .gb or FASTA, run WF1, and see the consensus plus QC.
- Phase 2 (make the .gb pay off): register the reference in `blast_ref_sequences`/`blast_ref_annotations` plus a `blast_ref_override` row per unit (two sqlInsert statements copied from blast_ref_fetch_workflow.nf:49-55, one from the override writer), so synteny, start-gene rotation, curation refHits, and scaffold-join reference all use the user's file; `[maptoref]` warnings into Notes via `<ID>_summary.txt`; optional per-sample `Reference` mapping column with COALESCE; copy validated references into `<project>/references/`.
- Phase 3 (fidelity, only if asked for): samtools >= 1.22 for `-T` ("If no coverage: Ref" option, off by default) and threading; bcftools consensus route for indels; origin rotation trick if `origin_lowdepth` fires on real data; MIA half-day comparator on divergent references.

YAGNI list (explicitly not built):
- upload widget (no fileInput anywhere in the app; typed paths and URLs are the house pattern);
- extension past reference ends / partial or single-gene seeds (MITObim territory; full-length reference is the stated scope);
- "?" or "-" no-coverage characters (N is what every downstream step understands);
- "Ref" fill (reference bias; needs samtools 1.22);
- structural-variant discovery, multiple references per set, mapping to several references at once (use several sets);
- per-base quality output, separate substitutions-only FASTA (the reference-frame product is only needed internally during iterations);
- a separate Nextflow process (the elif inherits publishDir, resources, retries, and all DB writes);
- new assemble table columns (the details panel reads the JSON; Notes carries the one-line warning in phase 2);
- read normalization or subsampling before iterating;
- a MapToRef-specific export path (export is assembler-agnostic and already handles N runs and IUPAC codes).
