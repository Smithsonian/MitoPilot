# Map-to-reference assembly (MapToRef)

Date: 2026-09-03
Branch: map-to-ref-assembly (off main)
Status: proposed, decisions taken 2026-09-03 (section 8.1)

This is the final planning document for a third assembler in MitoPilot,
presented next to GetOrganelle and MitoFinder, that mimics Geneious Prime
"Map to Reference" without needing Geneious. It synthesizes four lanes of
research in five files under `tools/map_to_ref_research/` (geneious-advisor.md,
alt-tools.md, ref-handling.md, and the codebase map in two versions,
codebase-map.md and codebase-map.v1.md), the three design proposals under
`tools/map_to_ref_research/` (lean.md, fidelity.md, ux.md), and the judge's assessment
(tools/map_to_ref_research/judge.md), which verified the load-bearing claims by simulation inside
the shipped `macguigand/mitopilot:1.5.4` image. Where the two codebase-map
versions disagree, the repository wins; every file:line reference below was
re-checked against the working tree on 2026-09-03.

A completeness review of the previous draft (tools/map_to_ref_research/critic.md) found six
correctness defects and a set of honesty and coverage gaps. All of them are
applied here. Section 11 records what changed and why, for anyone comparing
against the earlier draft, and section 12 records the three places where the
review offered a choice and which way it went.

No code is written here. Section 9 gives the phased outline.

---------------------------------------------------------------------------

## 1. Summary and recommendation

Build the feature as an iterate-to-consensus loop out of tools already in
the container: bowtie2 2.5.4 maps the reads, samtools sorts the BAM and
calls a reads-only consensus, the consensus becomes the next mapping
reference, the loop stops when the sequence stops changing or a cap is hit,
and one last pass maps every read to the converged reference so the published
sequence is called from all of the data. The loop is driven by one small
exported R function called from a third `elif` branch in the existing
`assemble` Nextflow process, exactly the way `coverage.nf` calls
`MitoPilot::coverage()` today. The user supplies one reference per parameter
set (a single-record GenBank file, or a FASTA with its topology stated
explicitly), typed into the options modal like the MitoFinder database is.

Why this shape:

- Zero new binaries. Every command it runs is in the image now
  (`docker/Dockerfile:31-32`); the only Dockerfile change is bumping the
  samtools pin from 1.21 to 1.24 for real consensus threading (5.4), and a full mapping pass costs what the
  existing coverage step already costs per sample (`R/coverage.R:66`).
- It is the only option whose knobs map onto the Geneious dialog a user knows
  (sensitivity ladder, iterate up to N, N below depth, consensus threshold,
  ambiguity codes), because samtools consensus exposes each of those directly
  (alt-tools.md sections 4 and 7).
- It touches only the seams the codebase map identified: one shell branch, one
  `path()` input, five option columns, five modal inputs, one changed condition
  in `coverage.nf`. Everything downstream (coverage, BLAST, reference fetch,
  scaffold join, annotation, curation, export) keys on the published FASTA
  layout and header convention and needs no change (codebase-map.md section
  10).
- The judge scored lean.md, which is this shape, highest on codebase fit and
  operations (9 and 9). Its science score (5) came from four specific defects
  that the judge verified and that this document fixes: the circular origin
  was called from the wrong copy, read recruitment silently dropped the pairs
  it meant to keep, N runs in an intermediate consensus repel reads on the next
  pass, and the insertion-marking format was misread.

What is grafted from the other two proposals: the R-side reference parser with
CRLF, LOCUS topology, and genetic-code checks (fidelity.md); the
per-iteration stats file, stop reason, and substitutions-versus-reference count
(fidelity.md); the "substitutions only during iterations, indels at the final
call" idea (ux.md); the warning thresholds, plain-English validation messages,
and the phase-2 registration of the .gb into the existing reference tables
(ux.md). A custom consensus caller with a true origin fold (fidelity.md) is
kept as the phase-3 upgrade and as open decision 1, not built in v1.

---------------------------------------------------------------------------

## 2. How Geneious Map to Reference works (what matters for cloning it)

Digest of geneious-advisor.md. Only the parts a re-implementation needs.

### 2.1 The mapper

- Seed-and-expand hash mapper with index word length "typically 10 to 15"
  depending on sensitivity; allows a single mismatch in the seed; circular
  references are indexed with words spanning the origin so expansion wraps
  (manual section 6.4, white paper 7.1).
- Reads mapping equally well to several places are placed randomly by default
  ("Map multiple best matches: Randomly"); alternatives are none or all.
- Paired distance is a soft penalty, not a hard window (white paper 7.1).
- Iteration ("Fine Tuning: Iterate up to N times"): reads are mapped to the
  consensus of the previous iteration, converted back to reference
  coordinates, and the process repeats. This is what lets reads reach variable
  regions and extend past the ends of a linear reference (white paper 7.1,
  manual 6.2). Iteration stops early "once no additional sequence reads are
  aligned" (Geneious staff, advisor 5.15; Kemp thesis, advisor 9.2).
- The white paper's own validation (E. coli reads on an 89% identical gene):
  bowtie2 default 44% mapped and 84% consensus accuracy; bowtie2
  `--very-sensitive-local` 85% mapped and 96.5% accuracy; Geneious single pass
  90% mapped and 97% accuracy; Geneious with 5 iterations 100% and 100%. The
  iteration, not the choice of mapper, closes most of that gap (advisor 7.2).
- Deterministic: same settings and input give the same result (manual 6.4).

### 2.2 Recovered Medium preset (advisor 8.1, identical in 17 workflow files from Geneious 9.1 to 2025.1)

| Dialog label | XML key | Medium | Low-derived custom (SARS-CoV-2 recipe) |
|---|---|---|---|
| Fine Tuning | fineTune | iterate_5 | iterate_3 |
| Index Word Length | indexWordLength | 12 | 14 |
| Word Length | expansionWordLength | 14 | 24 |
| Ignore words repeated more than n times | filterRepeatsReference / size | true / 20 | true / 8 |
| Allow Gaps, Maximum Per Read (%) | allowGaps / maxGapsPerRead | true / 15 | true / 10 |
| Maximum Gap Size | maxGapSize | 50 | 50 (raised from the Low default) |
| Maximum Mismatches Per Read (%) | maxMismatches | 30 | 10 |
| Maximum Ambiguity | maxAmbiguity | 4 | 4 |
| Minimum Overlap / Identity | applyMinOverlap 25 / 80% | off / off | off / off |
| Search more thoroughly | doMoreThoroughSearching | false | false |
| Accurately map reads with errors to repeats | accuratelyMapReadsWithErrorsToRepeatRegions | true | false |
| Map multiple best matches | multipleBestMatches | Randomly | Randomly |
| Minimum mapping quality | applyMinimumMappingQuality / value | off / 30 | off / 30 |
| Trim paired read overhangs | trimPairedOverhangs | true | true |
| Only map paired reads which map nearby | onlyMapPairedHitsReference | false | true |
| Find structural variants / deletions | findStructuralVariants / findDeletions | false / false (1000) | false / false |

Medium-Low, second hand (PMC10507443): 20% mismatches, 10% gaps. Low,
Medium-High, and Highest word lengths were never recovered; the white paper
says index length spans 10 to 15.

### 2.3 Consensus defaults (advisor 8.3, manual 6.5)

| Setting | Default | Meaning |
|---|---|---|
| Threshold | Highest Quality (60%), XML `weighted_60` | sum base qualities per candidate; call the base whose total exceeds 60% of the column total; "consensus of the reads only, never the reference" |
| Threshold without qualities | 65% | percentage rule: walk candidates by count, call the IUPAC code of the set needed to reach the threshold; ties are all-or-none (manual example: 6 A, 3 G, 1 T gives A at <= 60%, R at 60-90%, D above 90%) |
| Use mapping quality | true, summed | mapping quality combined with base quality |
| Low coverage call | on, `?` below depth 3 (reference assemblies) | XML `coverageThreshold=3` |
| Call N if quality below | 20 | when the threshold type is quality |
| If no coverage | `?` (options: `-`, N, `?`, Ref) | Ref pastes the reference base in (reference bias) |
| Trim to reference | false | consensus may extend past the reference ends |
| No consensus gaps / end gaps | false / true | gaps are called internally, never at the ends |
| Trim before mapping | Do not trim (BBDuk Q20 first) | tutorial recipe |

### 2.4 What practitioners set (advisor 9.8, 36 mitogenome papers)

Sensitivity: Medium-Low 19, Medium 9, Highest 4, Low 2. Iterations: 5 in
17 of 27 papers, 25 in 5 (all distant-reference or seed-and-grow), 10 in 2.
Kemp's fish-skim test saturated at 3 to 10 iterations and found no downside
to a high cap because iteration stops early. Every published recipe converges
on the same five steps: map, call a reads-only consensus with explicit unknowns
where coverage is missing, re-map to the consensus, stop when nothing changes,
and finish by mapping all reads once more to the converged reference and
calling the published sequence from that alignment (advisor 9.10). Section 4.6
step 8 implements that last step; it is not the same thing as MitoPilot's
existing coverage pass, which measures depth but never calls a sequence.

### 2.5 Known limits of the Geneious approach (advisor 10.3, 9.9)

Mapping cannot see rearrangements or duplications absent from the reference
(Winn 2025 missed a duplicated Cytb/D-loop block). Reference bias is real and
measured: on a different-species reference the Culicoides benchmark found the
Geneious consensus carried twice the differences of bowtie2's, and swapping
the reference changed the consensus; the authors fell back to de novo. A
de novo cross-check remains necessary, and MitoPilot already ships two.

---------------------------------------------------------------------------

## 3. Approaches considered

### 3.1 The three proposals and the judge's scores

| Proposal | Shape | Fit | Science | Ops | Overall |
|---|---|---|---|---|---|
| lean.md | bowtie2 + samtools consensus loop as a shell branch in assemble.nf; ~220 lines, 9 files, 4 option columns | 9 | 5 | 9 | 7.5 |
| fidelity.md | bowtie2 presets + samtools view -e per-read caps + custom R caller over bam-readcount counts with an origin fold, linear-end extension, mask table; ~1,300 lines | 7 | 8 | 6 | 7 |
| ux.md | Geneious-named modal controls, two-way option sync, details panel, samtools consensus loop; ~1,000 lines | 6 | 3 | 6 | 4.5 |

lean.md strengths: smallest diff, every touch point verified, `--no-use-MQ`
correctly identified as load-bearing for the circular seam (simulation: 38 N
versus 95 N without it), a two-stage read strategy that keeps later passes
cheap, the most honest option table. Weaknesses, all verified: (1) clipping the
elongated consensus at L leaves positions 1 to ~36 as N at 30x because depth
at the origin of the first copy is structurally zero; (2) `--no-unal` drops the
unmapped mate so `-G 12` recruits only fully mapped pairs; (3) `--mark-ins`
writes inserted bases with a leading underscore, not `+`, so the clip step
miscounts; (4) N runs longer than ~22 bp in an intermediate consensus block
re-mapping on the next pass (shared by all three proposals).

fidelity.md strengths: the only correct origin (count fold), the exact
Geneious threshold, IUPAC, and tie rule, Ref/N/gap no-coverage policy, a
per-position mask table, a soft-clip extension for linear ends, the best test
plan. Weaknesses: `[XS] == null` is invalid samtools filter syntax (use
`!exists([XS])`); full read set every pass with cap 10 and a `-N 1 -i C,1,0`
Highest preset is the slowest bowtie2 configuration, so "about an hour" is
optimistic; the stop rule can fire spuriously at pass 2; ~1,300 new lines and
a 14-key option grammar in a repo that wants narrow, boring code.

ux.md strengths: the best user-facing design (Geneious-named controls,
details panel metrics, sourced warning thresholds, plain-English validation
messages) and the correct phase-2 registration of a .gb into the existing
reference tables. Weaknesses: `--min-MQ 20` as a default turns the whole
duplicated block into N (299 N in positions 1 to 600); `--show-del no` removes
deleted bases so "coordinates stay reference-stable" is false; up to seven
full bowtie2 passes per sample; `--bowtie2-args "..."` breaks the shell
quoting of the Rscript call; a two-way sync observer between six inputs and a
string is unneeded complexity.

### 3.2 The hybrid chosen

Skeleton: lean.md. Mandatory fixes from judge.md section 5, all adopted:
splice the appended copy's calls in for the origin, recruit without dropping
unmapped mates, fill N from the previous reference during iterations, and
parse `--mark-ins` output correctly. Added on review: a final all-reads
mapping pass before the published call, so the product is not limited to the
reads that mapped to the user's original reference (section 4.6 step 8). The
shell branch body moves into a small R function so the reference parser,
splice, fill, and output writer are testable and CRLF-safe (judge.md section
7). Grafts are listed in section 1.

### 3.3 Wrap MITObim, and why it lost

MITObim 1.9.1 (MIRA 4.0.2) is the best-known published analogue and is
genuinely better at one thing: growing a partial seed (a COI barcode) into a
mitogenome, which is its documented Tutorial III (alt-tools.md 3.1). It lost
for the stated scope (one complete reference per project) on every other
axis (alt-tools.md section 7): no per-base consensus rules (`--min_cov` is a
whole-contig filter), no IUPAC codes, no N-below-depth, no circular reference
handling, one mismatch knob that cannot express a Low/Medium/High ladder,
FASTA only, single-threaded, needs uncompressed interleaved FASTQ, is NFS
hostile on HPC, needs MIRA in its own conda env plus Perl, proofreading has
been disabled since 1.8 with an eight-year-old note, and upstream was last
touched 2020-12-29. MIA (bioconda `mapping-iterative-assembler`, 0.09 MB) has
native circular support and convergence but no paired-end model and its
thresholds are compile-time constants; it is a possible comparator, not a
shipped path. If partial-seed growth is ever required, MITObim or a SPAdes
micro-assembly of the recruited subset is the fallback (section 9, deferred).

---------------------------------------------------------------------------

## 4. Recommended design

### 4.1 Problem

MitoPilot has two de novo assemblers. Neither can produce a consensus in the
coordinates of a user-chosen reference, honor a per-base consensus threshold
with explicit unknowns, or rescue a low-coverage skim that de novo assembly
fragments. Users who do this today do it by hand in Geneious, which is
closed, licensed, and not reproducible across a batch.

### 4.2 What is already in place

- A complete short-read mapping stack in the image: bowtie2 2.5.4, samtools
  1.21 (with `consensus`), minimap2 2.28, bam-readcount 1.0.1
  (`docker/Dockerfile:30-33`).
- The exact mapping command shape, `bowtie2 --very-sensitive-local --no-unal
  ... | samtools sort` (`R/coverage.R:66-77`).
- The circular elongate-and-fold pattern with `flank <- min(500L, len %/% 2L)`
  and the comment explaining why MAPQ must not be filtered in the duplicated
  block (`R/circularize_asmb.R:466-471`, `:529`).
- A junction-spanning read counter that builds the same elongated construct
  this design needs, `count_junction_reads()`
  (`R/circularize_asmb.R:522-599`).
- A pure-R GenBank flat-file reader: record split on `//`, VERSION,
  DEFINITION, ORIGIN extraction, feature locations
  (`R/custom_assembly_db.R:487-700`). See 4.5 for which parts are reusable.
- The IUPAC sequence regex (`R/blast_ref_utils.R:397`) and the writers for the
  four reference files the NCBI fetch produces (`R/blast_ref_utils.R:401`).
- The assembler seams: `assemble.nf:16` input tuple with `path(mf_db)`,
  `:56-102` MitoFinder branch and closing `fi`, `:80` tarball of the
  preprocessed pair, `coverage.nf:40` MitoFinder condition, the
  `${projectDir}/assets/NO_FILE` placeholder (`inst/nextflow/assets/NO_FILE`).
- A single-record circular test reference,
  `ref_dbs/MitoFinder/NC_002333_Danio_rerio.gb` (16,596 bp, `circular VRT`).

### 4.3 What is missing

| Piece | Status |
|---|---|
| Assembler value `MapToRef` in the validator, DDL, modal, and migration | absent |
| Reference file column and staging into the task | absent |
| Mapping loop, consensus, origin handling, output writer | absent |
| LOCUS topology parsing (only GFF3 `Is_circular` is read today) | absent |
| Coverage branch for a third assembler (`coverage.nf:40` has no `else`) | absent |
| Docs, tests, test fixture | absent |

### 4.4 Architecture

No new Nextflow process, no config change, no new DB tables. The `assemble`
process gains a `path(ref)` input and a third `elif` that runs:

```
Rscript -e "MitoPilot::map_to_ref(id, ref, R1, R2, bowtie2_opts, consensus_opts, iter_cap, topology, genetic_code, cpus, outDir)"
tar -czvf outDir/ID_reads.tar.gz *.fastq.gz        # MitoFinder layout
echo ... > outDir/NF_work_dir_assemble.txt
```

`map_to_ref()` lives in a new `R/map_to_ref.R` and shells out with
`system2()` exactly as `coverage()` does. It writes the contract files and
never exits non-zero for a per-sample problem: it writes the
`ID_assembly_0.fasta` sentinel with the reason in `assembler.log.txt` and
returns, so the workflow records "failed assembly" (status 3) and the batch
continues (`assemble_workflow.nf:356-367`; contract item K).

Resources come from the `opts` input as today (`inst/config.local:12-13`).

Two mapping passes over the full read set happen inside `map_to_ref()`: pass 1
against the user's reference, and the final pass against the converged
reference from which the published sequence is called (4.6 step 8). The
existing `coverage` process is a third full pass, but it only measures depth
and error rates: it runs bowtie2 and bam-readcount and never calls a consensus
(`R/coverage.R:58-82`), so it cannot stand in for the Geneious recipe's final
consensus pass.

### 4.5 Reference input: handling and validation

Accepted: a GenBank flat file (`.gb`, `.gbk`, `.gbff`, detected by content:
first non-blank line starts with `LOCUS`) or a FASTA (starts with `>`), one
record, given as a path or URL. Stored per parameter set in
`assemble_opts.maptoref_ref`, mirroring `mitofinder_db` (`R/init_db.R:321`).
Per-sample references are one parameter set per reference; the set picker
already creates sets (`R/app_assemble_utils.R:229-241`). Nextflow stages the
file with `path()` (URLs work the way the MitoFinder db URL works,
`R/init_db.R:80`); `NO_FILE` is passed when the set is not MapToRef.

`maptoref_prepare_ref(ref_file)` runs in R inside the task at the top of
`map_to_ref()`. It is not called at modal save time: save-time validation in
v1 is the topology rule only (5.5). It is a new reader, not a call into the
existing one:
`.cadb_parse_gb()` skips every record without `/organelle="mitochondrion"`
(`R/custom_assembly_db.R:490`) and every record with an empty ORIGIN, so a
perfectly good user reference without that qualifier parses to zero records.
Reusable as-is: `.cadb_grab_version()` (`:696`), `.cadb_grab_definition()`
(`:682`), and the ORIGIN extraction idiom at `:520-524`. Steps:

1. Strip `\r` from every line (a CRLF file otherwise yields "no records",
   ref-handling.md section 4).
2. GenBank: exactly one `//` record, else fail with "Reference must contain
   exactly one record; this file has N. The MitoFinder database format is not
   accepted here." Accession from VERSION, organism from DEFINITION, sequence
   from ORIGIN (uppercased, digits and spaces removed), topology from the
   LOCUS line by whitespace token (`circular` present, else `linear`), per
   NCBI gbrel.txt 3.4.4.1. First `/transl_table=` captured (default 2). No
   organelle qualifier is required.
3. FASTA: `Biostrings::readDNAStringSet` in `tryCatch`
   (`R/custom_curation_db.R:199-214` shape), one record, first header token
   as the name. Topology comes from the `maptoref_topology` option, which the
   user MUST set to `circular` or `linear` whenever the reference is a FASTA.
   There is no silent linear default and a `circular` header token is not
   enough on its own. An empty option with a FASTA reference is a per-sample
   failure: "Set the reference topology (circular or linear) for a FASTA
   reference." For a `.gb` reference the LOCUS line wins and the option is
   ignored.
4. Sequence must match the package IUPAC regex; fail otherwise with the
   offending characters listed. Length outside [5000, 50000] fails; outside
   [10000, 25000] warns. N or IUPAC content above 1% warns.
5. `/transl_table` different from `samples.genetic_code` warns (the sample's
   code is what annotation uses).
6. Writes `maptoref/ref.fasta` (single record `>ACC topology`) and copies the
   input verbatim as `maptoref/reference.gb` or `reference.fasta`.

Kept from the .gb in v1: sequence, topology, accession, organism, genetic
code. The feature table is preserved by the verbatim copy and registered in
phase 3 (section 9).

### 4.6 Per-sample algorithm

Notation: L = reference length, F = min(500, L %/% 2) for circular, 0 for
linear; R1/R2 = the fastp-trimmed pair from PREPROCESS
(`preprocess.nf:25-27`), so "Trim before mapping" is "Do not trim". Every
consensus FASTA is read with newlines stripped before indexing, because
samtools consensus line-wraps its output (`-l/--line-len`); after unwrapping,
an intermediate consensus is exactly L+F characters for a circular reference
and L for a linear one.

All intermediates (bowtie2 index, `sub_R?.fq`, `pass_*.bam`, `ref_*.fa`) are
written under `maptoref/` in the task directory, never at the task root, so
the `*.fastq.gz` tarball glob at `assemble.nf:80` cannot pick them up.

Step 1. Prepare the reference (4.5). Write `ref_0.fa` = the reference plus its
first F bases when circular (`.coverage_extend_circular` pattern,
`R/coverage.R:250-259`).

Step 2. Pass 1, all reads:

```
bowtie2-build -q ref_0.fa idx
bowtie2 <bowtie2_opts> -x idx -1 R1 -2 R2 --threads cpus 2>> assembler.log.txt \
  | samtools view -b -G 12 - | samtools sort -@ cpus -o pass_1.bam -
```

No `--no-unal` here: with it bowtie2 drops the unmapped mate of a half-mapped
pair, and recruitment then keeps only fully mapped pairs (judge.md 1.4).
`-G 12` discards records with both mates unmapped, so the BAM stays small.

Step 3. Recruit the subset for the iteration passes: pairs with at least one
mate mapped, mates kept:

```
samtools sort -n pass_1.bam | samtools fastq -1 sub_R1.fq -2 sub_R2.fq -0 /dev/null -s /dev/null -n
```

If pass 1 maps fewer than 100 primary reads, fail with "N reads mapped to the
reference; use a closer reference or a more sensitive preset".

Step 4. Call the intermediate consensus for pass k (substitutions only,
coordinates fixed):

```
samtools consensus -a -A --no-use-MQ --show-del yes --show-ins no <consensus_opts> -@ cpus pass_k.bam > raw_k.fa
```

`-a`, `-A`, `--no-use-MQ`, `--show-del yes`, and `--show-ins no` are fixed by
the code, not part of the user string. `--no-use-MQ` is load-bearing: reads
fully inside the duplicated block get MAPQ 1 and would otherwise be
discounted (judge.md 1.3). The unwrapped output has L+F characters (L for
linear) with N at uncovered or sub-depth sites and `*` at deleted bases.

Step 5. Fill and splice (R string operations on fixed-length strings):

- Fill: every N and `*` in `raw_k.fa` is replaced by the base at the same
  position of `ref_(k-1).fa`. This is "Ref" fill for the mapping reference
  only; the published product is never filled. Reason: an N run longer than
  ~15% of the read length (`--n-ceil L,0,0.15`, about 22 bp for 150 bp reads)
  blocks re-mapping and collapses flanking depth, so N runs are
  self-perpetuating unless filled (judge.md 1.5).
- Splice (circular only). Positions 1..L of the filled string are the
  reference frame; positions L+1..L+F are the appended copy of reference
  positions 1..F. The first F/2 positions of the first copy have structurally
  low depth, so take their calls from the appended copy instead:

  ```
  cons_k = filled[(L+1) .. (L+F/2)] followed by filled[(F/2+1) .. L]
  ```

  Invariant: `nchar(cons_k) == L`, and position 1 of `cons_k` is reference
  position 1. The judge verified that the appended copy's calls for those
  positions match the truth exactly (judge.md 1.3). Depth in the block is
  roughly half the genome mean instead of ramping to zero at the origin; full
  depth needs the count fold of open decision 1.
- Linear: nothing to splice; `cons_k` is the filled string.

Count bases changed versus the previous mapping reference (positions 1..L) and
count primary mapped reads (`samtools view -c -F 0x904 pass_k.bam`), then write
the pass row of `iterations.tsv`.

Step 6. Stop rule. Stop when bases changed is below 5 and the primary mapped
read count changed by less than 0.1% from the previous pass, or when k reaches
the cap. Record `stop_reason` as `converged`, `cap`, or `failed`. Cap 1 means
a single pass (Geneious "Fine Tuning: None"). The 5-base tolerance keeps an
ambiguity flip inside the duplicated block from consuming the whole cap; the
read-count term is the aITE convergence rule (alt-tools 3.3) and catches the
case where the sequence has settled but reads are still being recruited.

Note a limit of that counter: step 5 fills both N and `*` from the previous
mapping reference, so a real deletion is re-inserted into every iteration
reference and is only applied at the final call. That keeps coordinates fixed
and the reference mappable, but it also means "bases changed" is blind to
indels, so `stop_reason=converged` is compatible with an indel that never
settled.

Step 7. Next iteration: `ref_k.fa` = `cons_k.fa` plus its first F bases; map
the recruited subset (`sub_R1.fq`, `sub_R2.fq`) with the same bowtie2 flags,
`--no-unal` allowed now, and repeat from step 4. Iteration passes run on a
subset that is usually well under 1% of the reads and finish in seconds.

Step 8. Final all-reads pass. When the loop stops at pass k, map the full read
set once more against `ref_k.fa` (the converged reference, elongated the same
way):

```
bowtie2 <bowtie2_opts> --no-unal -x idx_final -1 R1 -2 R2 --threads cpus 2>> assembler.log.txt \
  | samtools sort -@ cpus -o final.bam -
```

This is the pass every published recipe ends with (advisor 9.10, aITE,
alt-tools 3.3). It matters because the recruited subset in steps 3 to 7
contains only pairs with a mate that mapped to the user's ORIGINAL reference;
reads that become mappable only after the reference has moved toward the
sample are exactly the ones the loop exists to reach, and without this pass
they would never contribute to the published sequence. Record the primary
mapped read count from this BAM as `reads_mapped_final`. Also record
`junction_depth`: the number of primary alignments in `final.bam` whose
reference span crosses position L (the seam), computed in R from POS and CIGAR
the way `count_junction_reads()` does (`R/circularize_asmb.R:522-599`).
Linear references skip the junction count.

Step 9. Final call, from `final.bam` (two cheap samtools invocations, no more
mapping):

```
samtools consensus -a -A --no-use-MQ --show-del yes --show-ins yes --mark-ins <consensus_opts> -@ cpus final.bam > final_raw.fa
samtools consensus -a -A --no-use-MQ --show-del yes --show-ins no   <consensus_opts> -@ cpus final.bam > final_subs.fa
```

Parse `final_raw.fa` into one token per reference position: the position's
character (base, IUPAC code, N, or `*`) followed by any inserted run.
`--mark-ins` marks an inserted base with a leading underscore
(`bam_consensus.c:2270`); tokenize strictly on that `_` prefix
(`_[ACGTacgt]`), never on case. Case means something else: with `-A` the
consensus character comes from the ambiguity matrix
`"AMRWa" "MCSYc" "RSGKg" "WYKTt" "acgt*"` (`bam_consensus.c:2090-2094`,
`:2238-2242`), whose lowercase entries are the base-versus-gap heterozygous
codes ("we use lower-case letter to symbolise a half-present base type",
1.21 man page). They can appear at any position, insertion or not, and an
inserted base is lowercase only when it is itself heterozygous.

Policy for those codes: call them N in the product, and report how many there
were as `half_deletions=` in the summary. A half-present code means the reads
disagree about whether any base is present at that site at all, so promoting it
to a solid base would overstate what the data say, and N is the character this
design already uses for "not called". Splice by token index exactly as in
step 5 (`tokens[(L+1)..(L+F/2)]` then `tokens[(F/2+1)..L]`), then drop `*`,
drop the `_` markers, and uppercase the rest. N is kept: this is the only place the N
policy applies to the product. Linear references: strip leading and trailing N
runs only (the consensus should not claim reference extent it never covered);
internal N runs stay.

`final_subs.fa` gets the same splice but no indel application and no fill, so
it is exactly L characters in the reference coordinate frame, with N where
nothing was called and `*` where a deletion was called. It is written as
`maptoref/subs_only.fasta`, the analogue of snippy's `.consensus.subs.fa`
(alt-tools "NEW FINDING 5"), and it answers the question "did we recover this
region from reads, or is the product inheriting reference structure?".

Step 10. Outputs (4.11). Header `>ID.1.1 circular|linear`, one path, one
scaffold. The published topology starts from the reference and is then
downgraded: a circular reference with `junction_depth` of 0 is published as
linear, with a `no_junction` note saying so. No read spanning the seam means
this sample's reads cannot support a circular claim, and publishing it as
circular anyway would send an unsupported molecule into rotation
(`R/annotate.R:128`, `:380-392`) and seed the `partial` flag
(`assemble_workflow.nf:418`) on evidence that does not exist. The reference's
own topology is still reported as `reference_topology=` next to
`published_topology=` in the summary, so the downgrade is visible and the user
can override it by editing the header.

Runtime per sample: two full bowtie2 passes plus cheap subset passes, then the
existing coverage pass. About three times the current MitoFinder-branch
coverage cost, still well under a GetOrganelle run.

### 4.7 Geneious option mapping

Fidelity: exact = same rule; approximate = same intent, different mechanism;
not replicable = no equivalent with the tools in the image, or deliberately
deferred. Our defaults are the shipped values.

| Geneious option (XML key) | Geneious value | Ours | Our default | Fidelity |
|---|---|---|---|---|
| Mapper | Geneious (Bowtie2 is the advisor's own alternative for short reads on a non-repetitive reference) | bowtie2 2.5.4 local mode | bowtie2 | approximate |
| Sensitivity Low / Fastest | index 14, word 24, 10% mismatch, 10% gaps | `--fast-local` | | approximate |
| Sensitivity Medium-Low / Fast | 20% mismatch, 10% gaps | `--sensitive-local` | | approximate |
| Sensitivity Medium / Fast (Geneious default) | index 12, word 14, 30% mismatch, 15% gaps | `--very-sensitive-local` | default | approximate (the white paper's best bowtie2 result at 89% identity; the repo's own preset, `R/coverage.R:66`) |
| Sensitivity Medium-High | unknown | `--very-sensitive-local -N 1` | | approximate |
| Sensitivity Highest / Medium | index ~10 | `--very-sensitive-local -N 1 -L 15 --score-min G,10,6` | | approximate; slow |
| Index Word Length (indexWordLength 12) | seed word | bowtie2 `-L` (20 in very-sensitive-local) | 20 | approximate (FM-index seed, not a hash word) |
| Word Length (expansionWordLength 14) | expansion word | none | | not replicable |
| Ignore words repeated > n (filterRepeats 20) | | none; bowtie2 places multi-hit reads randomly | | not replicable |
| Allow Gaps (true) | percent-of-read gap budget | always on, `--rdg 5,3 --rfg 5,3` (bowtie2 defaults) | on | approximate (affine score penalty inside `--score-min`, not a percentage) |
| Maximum gaps per read (15%) | | inside `--score-min` budget | | approximate |
| Maximum Gap Size (50) | | none; long gaps become soft clips in local mode | | not replicable in v1 (fidelity.md's `samtools view -e` CIGAR filter is the phase-3 route) |
| Maximum Mismatches Per Read (30%) | | `--score-min G,20,8` (bowtie2 default in local mode, stated for the record); loosen to `G,10,6` for more divergence | default | approximate (score budget, not a percentage) |
| Maximum Ambiguity (4) | | `--n-ceil L,0,0.15` (bowtie2 default, stated for the record) | default | approximate |
| Minimum Overlap / Identity (off) | | none | | not replicable (off in Geneious by default) |
| Search more thoroughly (false) | | `-D 20 -R 3` inside the preset | | approximate |
| Accurately map reads with errors to repeats (true) | | none; iteration provides the same effect (white paper) | | not replicable |
| Map multiple best matches (Randomly) | Randomly / None / All | bowtie2 default = random best; None = `samtools view -e '!exists([XS]) \|\| [AS] > [XS]'` (phase 3) | Randomly | exact for Randomly |
| Minimum mapping quality (off / 30) and "Ignore reads mapped to multiple locations" (false) | both are MAPQ filters in Geneious | `--min-MQ` in the consensus string | 0 (off) | exact toggle, but unsafe on a circular reference: reads inside the duplicated block carry MAPQ 1, so any `--min-MQ` above 0 blanks the seam (judge.md 1.2 measured 299 N in positions 1 to 600). Validated and warned about (4.7 note below) |
| Trim paired read overhangs (true) | trims mate overhang past the fragment end | nothing equivalent; the shipped fastp string sets `--detect_adapter_for_pe` (removes adapter read-through) and `--correction` (fixes mismatched bases in the overlap), neither of which trims overhangs (`R/init_db.R:248`; `preprocess.nf:30`) | upstream fastp | not replicable |
| Only map paired reads which map nearby (false) | soft distance penalty (white paper 7.1) | `--no-mixed --no-discordant` in the bowtie2 string | off | approximate (hard concordance requirement, not a penalty) |
| Paired distance (soft penalty) | | `-I 0 -X 500` (bowtie2 defaults; raise `-X` for long inserts) | 0-500 | approximate (hard window with concordance preference) |
| Find structural variants / deletions (false) | | none | off | not replicable (off in every public workflow; de novo assemblers cover it) |
| Fine Tuning (iterate_5; None/3/5/10/25/custom) | re-map to previous consensus, stop when nothing new maps | `maptoref_iter` cap plus the change-based stop rule; 1 = None | 5 | approximate (Geneious realigns reads to each other around indels; we re-map to the consensus) |
| Trim Before Mapping | Do not trim | fastp upstream in PREPROCESS | do not trim here | exact |
| Consensus threshold Highest Quality 60% (weighted_60) | quality-summed call | samtools Bayesian mode (default, Gap5 model, base qualities) | default | approximate (no 60% share rule; exact rule is open decision 1) |
| Consensus threshold percentage (thresholdPercentNoQuality 65) | cumulative IUPAC rule | `-m simple -c 0.65 -H <het>` in the consensus string; `-c`, `-H`, and `-q` are simple-mode only and do nothing without `-m simple` (1.21 man page), which the code validates | not set | approximate (samtools: top-call fraction else N) |
| Ambiguity codes | when threshold not met | `-A` (fixed on) | on | approximate |
| Use mapping quality (mapQuality true, summed) | | fixed `--no-use-MQ` (see 4.6 step 4) | off | different by design; MAPQ carries no signal against a mito-only reference and breaks the seam |
| Low coverage call (coverageThreshold 3, `?`) | `?` below 3 | `-d 3`, character N | 3 | exact rule; N instead of `?` (FASTA legal, understood by export and scoring) |
| Call N if quality below (qualityThreshold 20) | | `--min-BQ 20` (base dropped from the column) | 20 | approximate |
| If no coverage (noCoverageCharacterReference `?`; -, N, ?, Ref) | | N in the product; Ref only for the mapping reference during iterations | N | exact for N; Ref not offered for the product (reference bias) |
| Trim to reference (false) | consensus may extend | circular: the consensus covers exactly the reference extent, length L plus or minus called indels; linear: clipped to covered extent, never extended | effectively true | not replicable in v1 (extension deferred, 4.9) |
| No consensus gaps / end gaps (false / true) | | deletions and insertions applied at the final call; no indel calls at ends beyond N | applied | approximate (CIGAR only, no local realignment) |
| Split around `?` (false) | | never | off | exact |
| Circular reference (native origin wrap) | | elongate F, map, splice at F/2, restore the origin; published circular only if a read spans the junction (4.9) | .gb LOCUS line, or the `maptoref_topology` option for a FASTA | approximate (half depth in the block; full depth needs the fold, open decision 1) |
| Deterministic results | yes | bowtie2 `--seed 0` (the bowtie2 default, stated for the record; determinism comes from seeding the per-read generator with the read name, sequence, qualities, and this seed, and holds with `-p` above 1) | yes | exact |
| Coverage graph export | CSV | `coverage.nf` writes `*_coverageStats.csv` and PDFs | on | exact (already exists) |

Validation of the free-form consensus string. Because several samtools flags
are silently mode-specific or actively unsafe here, `map_to_ref()` scans
`maptoref_consensus` before running and:

- warns into `assembler.log.txt` and the summary if `-c`, `-H`, or `-q` appear
  without `-m simple` (they are ignored in the default Bayesian mode);
- warns, and refuses on a circular reference, if `--min-MQ` is greater than 0;
- refuses `-T`, `-a`, `-A`, `--show-del`, `--show-ins`, `--mark-ins`,
  `--no-use-MQ`, `-o`, `-f`, and `-r`, which the code sets itself.

A refusal is a per-sample failure with the reason in the log, not a crash.

### 4.8 Consensus rules, stated plainly

- The product is called from reads only, from an alignment of all reads to the
  converged reference. The reference base never enters the product. During
  iterations the mapping reference is patched with the previous reference's
  base where the call is N, so mapping does not collapse, and the final call is
  made without that patch.
- A site with fewer than 3 reads after base-quality filtering is N.
- A site with no reads is N (never the reference base, never a gap).
- Mixed sites get IUPAC codes (`-A`). The rest of the pipeline already
  accepts them (NEWS.md 1.5.4; `R/assembly_path_scoring.R:56-60`).
- Half-present base codes (lowercase output, meaning base versus gap) are
  called N in the product and counted in the summary as `half_deletions`
  (4.6 step 9).
- Deletions are applied (bases dropped) and insertions are applied (bases
  added) at the final call only, so the product length can differ from L. The
  reference-length, substitutions-only view is written separately as
  `maptoref/subs_only.fasta`.
- Mapping quality is not used in calling and not filtered.
- Duplicate removal is not done inside MapToRef. If duplicates need handling
  they are handled once, upstream, for every assembler: fastp 0.23.4 in the
  image has `--dedup`, and the shipped preprocess string currently sets
  `--dont_eval_duplication` (`R/init_db.R:248`), so the project has no
  duplicate metric today at all. Phase 1 spikes fastp deduplication on real
  samples; if it is worth having, it ships as a toggle on the PREPROCESS
  options (a `dedup` column on `preprocess_opts` that swaps the flag), not as
  a MapToRef option. Until then duplicates are not removed, which is the
  deliberate trade: mitochondrial depth in a genome skim is usually the
  limiting resource, and dropping duplicates costs recall more often than it
  prevents a locked-in error.
- The consensus stays in the reference's coordinate frame and strand; WF2
  rotates to `start_gene` as it does for every assembler (`R/annotate.R`).

### 4.9 Circular handling, topology, and extension past reference ends

Circular: elongate by F, map, call, splice at F/2, restore the origin (4.6).
Judge's simulation at 30x on a 3% divergent sample: clipping at L gave 36
consecutive N at the origin; the appended copy's calls for those positions were
exactly right. `coverage.nf` then re-maps with its own junction construct and
folds depth for display as it does for every circular assembly
(`R/coverage.R:124`).

Topology starts from the reference but must be earned by the reads. The
published topology gates rotation to `start_gene` (`R/annotate.R:128`,
`:380-392`) and seeds the `partial` flag (`assemble_workflow.nf:418`), so it
has to reflect this sample, not the user's reference choice. The rule:

- Linear reference: published linear.
- Circular reference and `junction_depth` above 0: published circular.
- Circular reference and `junction_depth` of 0: published LINEAR, with the
  `no_junction` note recording the downgrade.

`junction_depth` is counted from `final.bam` (4.6 step 8) and both topologies
are reported in the summary (`reference_topology=`, `published_topology=`),
so the downgrade is never silent. A user who is confident the molecule is
circular can change the header topology; nothing else in the pipeline depends
on which of the two produced it.

Extension: not in v1. With a complete circular reference there are no ends.
With a linear reference the loop can never grow past the reference extent, so
the product is clipped to the covered extent and the `ref_divergent` and
`incomplete` warnings are the only signal that a short or partial linear
reference truncated the result. The cheapest extension design on the table is
fidelity.md's soft-clip column consensus at the ends (one `samtools view -e`
scan per pass), deferred to phase 3 and triggered by a real linear or
partial-reference request.

### 4.10 Reads

Pass 1: all preprocessed reads. Iteration passes: the recruited subset (pairs
with at least one mate mapped in pass 1). Final pass: all preprocessed reads
again, against the converged reference (4.6 step 8). This is the two-stage
recipe a Geneious staff member recommends (advisor 5.15) plus the closing
all-reads pass every published recipe ends with. The subset bounds how far the
reference can move during iterations (a pair with neither mate mapping to the
original reference cannot influence the intermediate consensus), but it no
longer bounds the published sequence, because the final call sees every read.

### 4.11 QC outputs and warnings

Files in `out/<ID>/assemble/<opts>/`:

- `<ID>_assembly_1.fasta` (or the `_assembly_0.fasta` sentinel).
- `<ID>_reads.tar.gz`: the preprocessed pair, MitoFinder layout. The glob at
  `assemble.nf:80` picks up `<ID>_preprocess_R1.fastq.gz` and `_R2`
  (`preprocess.nf:26-27`), the names `coverage.nf:42` passes to `coverage()`.
  Those staged reads are symlinks and `tar` without `-h` stores symlinks, the
  same as the MitoFinder branch does today; this design inherits that
  behavior rather than verifying the archive.
- `<ID>_summary.txt`: key=value lines (reference accession, organism, length,
  reference_topology, published_topology, transl_table, passes run,
  stop_reason, reads mapped in pass 1,
  reads_mapped_final, junction_depth, final N count, IUPAC count,
  half_deletions, substitutions versus reference, consensus length) followed by
  `note=` lines for warnings.
- `assembler.log.txt`: reference parse result, option-string validation
  results, bowtie2 alignment-rate lines per pass, any failure reason.
- `opts.txt`: the option strings and the cap.
- `maptoref/`: `ref.fasta`, `reference.gb|fasta`, `subs_only.fasta`,
  `iterations.tsv` (pass, reads_mapped, bases_changed, n_count, stop_reason),
  `cons_1.fa .. cons_N.fa`. These are kept deliberately as the record of the
  loop: at roughly 16 kb per consensus the cost is negligible, and without them
  a "why did this converge there?" question has no answer. They are not
  temporary files to be cleaned up later.
- `NF_work_dir_assemble.txt`.
- From `coverage.nf`, unchanged: BAM, `_coverage.csv`, `_coverageStats.csv`,
  per-scaffold PDF.

DB rows: only the existing ones, written by the existing workflow code
(`assemblies`, `assemble`, the `annotate` seed, then `depth/gc/errors`).

Warnings (written as `note=` lines in v1; appended to `assemble_notes` with a
`[maptoref]` tag in phase 2, see 5.3; shown in a details panel in phase 3):

| Code | Fires when | Text (plain) |
|---|---|---|
| few_reads | pass 1 mapped < 1,000 reads | "Only N reads mapped; check that the reference is a mitogenome from a related taxon." |
| ref_divergent | substitutions versus reference > 10% | "Reference is more than 10% divergent; expect reference bias and missing regions. Use a closer reference, a more sensitive preset, or compare with a de novo set." (89% identity is the white paper's demonstrated floor with iteration) |
| incomplete | N > 2% of positions | "X% of the reference could not be called (N)." |
| not_converged | stop_reason = cap | "Still changing after N passes; raise the cap (10 to 25) and re-run." |
| no_junction | circular reference and junction_depth = 0 | "No reads span the start and end of the sequence, so this assembly is published as linear even though the reference is circular. Add reads or use a closer reference, or edit the topology if you are confident the molecule is circular." |
| code_mismatch | .gb transl_table != samples.genetic_code | "Reference genetic code A differs from the sample's B; annotation uses the sample's." |
| ref_ambiguous | reference N/IUPAC > 1% | "Reference has N ambiguous bases; mapping is weaker there." |
| opts_ignored | `-c`, `-H`, or `-q` given without `-m simple` | "Consensus options A were ignored; they only apply with -m simple." |

Failure modes: invalid reference, a FASTA reference with no topology set,
zero or too few reads mapped, a refused consensus flag, and a bowtie2/samtools
error all write the sentinel and a reason, giving status 3 "failed assembly" in the Assemble table with the reason
in `assembler.log.txt`. OOM retries through the existing `errorStrategy`
(`assemble.nf:10-11`). A mostly-N consensus is emitted and warned about, not
failed; `coverage_trim` and export's gap finder handle it as they do for any
assembler (`R/annotate_coverage_trim.R`, `R/export.R:110`).

---------------------------------------------------------------------------

## 5. Integration touch points

### 5.1 R package

| File | Change |
|---|---|
| `R/map_to_ref.R` (new, ~280-330 lines) | exported `map_to_ref(id, ref, reads_1, reads_2, bowtie2_opts, consensus_opts, iter_cap, topology, genetic_code, cpus, out_dir)`; exported `maptoref_prepare_ref()`; internal `.mtr_fill()`, `.mtr_splice()`, `.mtr_parse_marked()` (the `--mark-ins` tokenizer), `.mtr_check_consensus_opts()` (4.7 validation), `.mtr_junction_depth()`, `.mtr_stop()`, `.mtr_write_outputs()`, `.mtr_fail()` (sentinel). Shell calls via `system2()` as in `R/coverage.R:58-82`; Biostrings only |
| `R/init_db.R:45` | roxygen: `assembler` choices add "MapToRef"; new `@param maptoref_ref`, `@param maptoref`, `@param maptoref_consensus`, `@param maptoref_iter`, `@param maptoref_topology` |
| `R/init_db.R:70-83` | new args `maptoref_ref = NA_character_`, `maptoref = "--very-sensitive-local"`, `maptoref_consensus = "-d 3 --min-BQ 20"`, `maptoref_iter = 5L`, `maptoref_topology = NA_character_` |
| `R/init_db.R:131-132` | validator vector and message add `MapToRef`; `stop()` if `assembler == "MapToRef"` and `maptoref_ref` is empty; `stop()` if `maptoref_topology` is set to anything other than `circular` or `linear` |
| `R/init_db.R:311-329`, `:330-350` | DDL adds the five columns after `join_scaffolds`; the default row gets the five values |
| `R/init_project.R` | none (`...` forwards to `new_db()`, `:125-133`); roxygen example added |
| `R/backwards_compatibility.R:11-15` | roxygen list of migrated columns: add the five `assemble_opts` names to the `assemble_opts` bullet |
| `R/backwards_compatibility.R:176-178` | add the five names to the "already current" predicate |
| `R/backwards_compatibility.R` after `:1313` | five migration blocks copied from the `mitofinder` block (`:1296-1313`): `ALTER TABLE assemble_opts ADD COLUMN ...` then backfill (reference NA, the two strings, cap 5, topology NA). `schema_gaps()` untouched; assemble_opts columns are not hard stops today |
| `R/app_assemble_utils.R:293` | `choices = c("GetOrganelle", "MitoFinder", "MapToRef")` |
| `R/app_assemble_utils.R:303-309` | the `opts_help()` sentence "Tool used to assemble the mitogenome from reads: GetOrganelle or MitoFinder..." names two tools; it must name three and link MapToRef's help |
| `R/app_assemble_utils.R` after `:332` | five inputs shaped like `mf_db` (`:323-332`): `textInput(ns("maptoref_ref"), "Reference (.gb or FASTA, one complete mitogenome):")`, `selectInput(ns("maptoref_topology"), "Reference topology (required for FASTA references):", choices = c("", "circular", "linear"))`, `textInput(ns("maptoref"), "bowtie2 options")`, `textInput(ns("maptoref_consensus"), "samtools consensus options")`, `numericInput(ns("maptoref_iter"), "Iterate up to", min = 1)`, each with a nested `opts_help()` linking to the bowtie2 manual and the samtools consensus man page, plus one line of help listing the five preset strings of 4.7. The topology field's help says a `.gb` reference takes its topology from the LOCUS line and ignores this field |
| `R/app_assemble_utils.R:438-447` | initial hide: GetOrganelle and MitoFinder branches also hide the five new ids; a `MapToRef` branch hides `mitofinder`, `mf_db`, `getOrganelle`, `seeds_db`, `labels_db` |
| `R/app_assemble.R:847-870` | populate the five inputs on set change |
| `R/app_assemble.R:873-885`, `:944-958` | `MapToRef` show/hide branch in both observers |
| `R/app_assemble.R:888-900` | five `toggleState` lines |
| `R/app_assemble.R:960-986` | `rows_upsert` data.frame gains the five columns; use `input$maptoref_ref %||% ""` and defaults, not `req()`, so a GetOrganelle set with an empty reference field still saves; `maptoref_iter` as integer. Save-time check: a MapToRef set whose reference name ends in a FASTA extension, or whose local file starts with `>`, and whose topology is empty is refused with "Set the reference topology (circular or linear) for a FASTA reference." |
| `NAMESPACE`, `man/` | roxygen regenerate |

Tool help: the other three tools register a "?" icon through
`register_tool_help()` (`R/app_assemble.R:124-127`) backed by a captured help
dump in `inst/tool_help/` and an entry in `tools/capture_tool_help.sh`.
MapToRef deliberately does not follow that pattern, because there is no single
wrapped CLI whose `--help` output describes the feature: the branch composes
bowtie2 and samtools consensus, and each option field links to the relevant
upstream manual through `opts_help()` instead. This is a choice, not an
oversight; if the missing "?" icon looks inconsistent next to the other three,
the fallback is a hand-written `inst/tool_help/maptoref.txt` plus one
`register_tool_help("maptoref", ...)` line.

### 5.2 DB schema and migration

| Table | Column | Type | Default | Meaning |
|---|---|---|---|---|
| assemble_opts | maptoref_ref | TEXT | NULL | path or URL of the single-record .gb or FASTA |
| assemble_opts | maptoref | TEXT | `--very-sensitive-local` | flags passed verbatim to bowtie2 |
| assemble_opts | maptoref_consensus | TEXT | `-d 3 --min-BQ 20` | flags passed to samtools consensus after validation (fixed flags are added by the code) |
| assemble_opts | maptoref_iter | INTEGER | 5 | iteration cap ("Iterate up to"), user-editable |
| assemble_opts | maptoref_topology | TEXT | NULL | `circular` or `linear`; REQUIRED when the reference is a FASTA, ignored when it is a .gb |

Assembler string value: `MapToRef`. No change to `assemblies`, `assemble`,
`annotate`, `samples`, or `blast_ref_*`. Five passthrough columns rather than
one parsed option string: with a shell-adjacent R driver, direct arguments
are simpler and safer than a parser (judge.md section 7).

### 5.3 Nextflow

| File | Change |
|---|---|
| `inst/nextflow/modules/assemble_workflow.nf:6-11` | append `opts.maptoref_ref, opts.maptoref, opts.maptoref_consensus, opts.maptoref_iter, opts.maptoref_topology` at the END of the select list (house rule). sqlRead selects 19 columns today (`it[0]` to `it[18]`), so these land at `it[19]` to `it[23]` |
| `assemble_workflow.nf:102-108` | opts map gains `maptoref: it[20], maptoref_consensus: it[21], maptoref_iter: (it[22] == null ? 5 : (it[22] as Integer)), maptoref_topology: (it[23] ?: "")` |
| `assemble_workflow.nf:99-117` | the opts tuple has 8 elements today (ID, opts id, opts map, dbs list, mf_db, genetic code, max_paths, max_scaffolds). Append the reference as the 9th element, index 8: `file((it[19] != null && it[19].toString().trim()) ? it[19] : "${projectDir}/assets/NO_FILE")` |
| `assemble_workflow.nf:183-195` | the cross map emits 9 elements today; append `it[1][8]` (the 9th element of the opts tuple) as the 10th element, so no existing position moves |
| `inst/nextflow/modules/assemble.nf:16` | append `path(ref)` at the END of the input tuple, after `val(max_scaffolds)`, matching the cross map |
| `assemble.nf` between `:101` and the `fi` at `:102` | `elif [ "!{opts.assembler}" = "MapToRef" ]; then mkdir -p !{outDir}; Rscript -e "MitoPilot::map_to_ref('!{id}', '!{ref}', '!{reads[0]}', '!{reads[1]}', '!{opts.maptoref}', '!{opts.maptoref_consensus}', !{opts.maptoref_iter}, '!{opts.maptoref_topology}', !{genetic_code.intValue()}, !{opts.cpus}, '!{outDir}')"; echo "..." > !{outDir}/opts.txt; tar -czvf !{outDir}/!{id}_reads.tar.gz *.fastq.gz; echo "Nextflow assemble working directory:" > !{outDir}/NF_work_dir_assemble.txt; echo "$PWD" >> ...; fi`. Option strings must not contain quotes (same rule as the existing tools) |
| `assemble.nf:19` output | ADD a second output declaration after the tuple, `path("${id}/assemble/${opts_id}/maptoref", optional: true)`, so the loop record of 4.11 is actually published; only declared outputs reach `publishDir`. It is a standalone `path` output because Nextflow does not honour `optional: true` inside a tuple. Optional because the other two assemblers never create that directory. The tuple is untouched (`opts.assembler` still flows as `[7]`) |
| `inst/nextflow/modules/coverage.nf:40` | `elif [ !{assembler} == "MitoFinder" ]` becomes `elif [ "!{assembler}" = "MitoFinder" ] \|\| [ "!{assembler}" = "MapToRef" ]`; the tarball layout and read names match, so `MitoPilot::coverage()` runs unchanged |
| `assemble_workflow.nf:268-300` (phase 2) | fold the `note=` lines of `<ID>_summary.txt` (reachable as `raw[3]`) into the `notes` string the `branched.pass` classification map already builds, so the existing `params.sqlWriteAssemble` is the only write. Do NOT add a second tagged-note `sqlInsert`: paired sqlInsert operators commit in any order and `sqlWriteAssemble` SETs `assemble_notes` wholesale for the same row in the same run, so a racing write would sometimes be clobbered. Keep the `[maptoref]` tag: prefix each folded line with it as it is collected, so the tag survives the fold |
| `coverage_workflow.nf`, `blast_genbank_workflow.nf`, `scaffold_join*`, WF2 | unchanged; one path, one scaffold, header `>ID.1.1 circular\|linear` satisfies every reader listed in codebase-map.md 2.6 |
| `inst/config.*` (8 templates), `MITOPILOT_PROCESS_ORDER` (`R/app_run_pipline.R:8-15`) | unchanged (no new process, no new params block) |

Contract check (codebase-map.md section 10): A directory yes; B single path
1, header token, sentinel yes; C .gb to FASTA in R inside the task yes; D
MitoFinder tarball layout yes; E summary, log, workdir files yes; F tuple
positions untouched (both new elements are appended) yes; G coverage CSV from
the unchanged `coverage()` yes; H DB rows by existing workflow code yes; I
`opts` carries cpus and memory yes; J genetic code unused by this branch yes;
K sentinel and exit 0 yes.

### 5.4 Container and release chores

`docker/Dockerfile:32`: bump samtools from 1.21 to 1.24 as part of this
feature. bowtie2 2.5.4 (`:31`) is unchanged. Every consensus flag this design
uses (`-a -A -d -m -c -H --min-MQ --min-BQ --show-del --show-ins --mark-ins
--no-use-MQ -@`) was probed in the shipped 1.21 image (judge.md 1.2) and all of
them survive into 1.24. What the bump buys: real consensus multi-threading
(added at 1.22, release notes), so `-@ cpus` starts paying instead of being
future-proofing, and the leading/trailing-N fix. What it does NOT buy for us:
`-T` (reference fill) is available in 1.24 and is deliberately not used, since
filling the product from the reference is exactly the reference bias this
design refuses.

Because the bump changes a pinned tool for every process in the image, it is
documented in three places: the Dockerfile pin, the image notes in
`docker/README.md`, and a NEWS bullet stating the old and new versions. Any
project run against the older image keeps working; nothing in this design
requires a 1.24-only flag, so the branch degrades to "no consensus threading"
rather than failing on 1.21.

The normal release chores apply: bump `Version:` in `DESCRIPTION` (1.5.4
today), delete stale `MitoPilot_*.tar.gz` before the image build so the
Dockerfile glob cannot install an older package
(`docker/deploy-local.sh:21-24`), and update `docker/README.md` with the new
image tag.

### 5.5 App UI

Only the options modal changes (5.1). No new Assemble table column, no upload widget (none exists in the app; typed paths and URLs are the house
pattern). Save-time validation in v1 is limited to the topology rule (a FASTA
reference with no topology is refused, 5.1); the reference path itself is not
checked, because the value may be a URL and the container is the only place
the file is guaranteed visible. The post-save "no output
directory for this set" warning (`R/app_assemble.R:1010-1052`) already covers
the new-set case. The coverage/details modal is assembler-agnostic and works
on the published files.

### 5.6 Export, annotation, curation

Unchanged. `R/export.R` is assembler-agnostic; `find_sequence_gaps()`
(`:110`) reports N runs "whatever put it there"; IUPAC codes translate to X
with a warning in curation (NEWS.md 1.5.4). WF2 keys on the header topology.

### 5.7 Docs

- `NEWS.md`: "### Map-to-reference assembly" bullets under New Features,
  including one bullet for the samtools 1.21 to 1.24 image bump (5.4) and, if
  the spike adopts it, one for the fastp deduplication toggle on PREPROCESS.
- `README.md:27-31` and `:165-170`: both name the assembler set ("GetOrganelle
  (default) or MitoFinder for mitogenome assembly", "Assembly references for
  GetOrganelle or MitoFinder"); both gain MapToRef.
- `vignettes/Test-Project-Assemble.Rmd:142-176`: Assembler bullet gains
  MapToRef; a "Reference" bullet (.gb preferred, one record, complete
  mitogenome, keep it inside the project folder; a FASTA reference requires
  the topology option to be set explicitly); an "Iterate up to" bullet saying
  the default is 5 and to raise it to 10 to 25 for a distant reference; and a
  note that a circular reference with no reads across the junction is
  published as linear.
- `vignettes/Difficult-Assemblies.Rmd:16-23`: "MapToRef, like MitoFinder,
  returns one path; N runs mark regions the reads did not cover; mapping
  cannot see rearrangements, cross-check with a de novo set."
- `vignettes/Difficult-Assemblies.Rmd:45`: "Multiple paths come only from
  GetOrganelle; MitoFinder always returns a single path" must name MapToRef as
  a second single-path assembler.
- `vignettes/Your-Own-Project.Rmd:188-192`: `new_project(assembler =
  "MapToRef", maptoref_ref = "ref/NC_002333.gb")`, and the FASTA form
  `new_project(assembler = "MapToRef", maptoref_ref = "ref/mito.fasta",
  maptoref_topology = "circular")`.
- `vignettes/custom_dbs.Rmd`: how to fetch a single-record .gb from NCBI and
  why the MitoFinder sampler is not a MapToRef reference.
- Singularity note: put the reference under the project folder or bind its
  directory (`prepare_ref_db.nf:15-19` explains the symlink trap).
- `docker/README.md`: new image tag and the samtools version change (5.4).
- `man/` regenerated.

### 5.8 Tests

- `tests/testthat/test-map-to-ref.R` (new), pure functions, no mapper:
  `maptoref_prepare_ref()` on the Danio fixture (circular, 16,596 bp, code 2,
  accession NC_002333), on a single-record .gb with NO `/organelle` qualifier
  (must parse, the case `.cadb_parse_gb()` would drop), on
  `fish_mito_sampler.gb` (rejects, many records), on a CRLF copy, on a
  two-record FASTA (rejects), on a single-record FASTA with no topology
  option (rejects with the topology message), with `maptoref_topology =
  "circular"` (accepts, circular), with a `.gb` reference and a contradicting
  topology option (the LOCUS line wins), on a `-` containing sequence
  (rejects), length bounds; `.mtr_fill()` (N and `*` take the previous base); `.mtr_splice()` on
  a synthetic 200 bp reference with F = 50, asserting both that the output is
  200 characters and that output position 1 equals truth position 1 (a
  mis-rotation preserves length, so length alone proves nothing);
  `.mtr_parse_marked()` on a string containing `*`, an UPPERCASE inserted base
  (`_G`), a lowercase inserted base, and a lowercase half-present code at a
  non-insertion position; `.mtr_check_consensus_opts()` on `-c 0.65` without
  `-m simple`, on `--min-MQ 20` with a circular reference, and on a refused
  flag; the stop rule with both terms.
- Stub-binary loop test: fake `bowtie2`, `bowtie2-build`, `samtools` scripts
  on PATH (`tests/testthat/test-find-mito.R:247-259` pattern) emitting canned
  FASTA output, asserting the header `>ID.1.1 circular`, the summary keys,
  early stop, the presence of `maptoref/subs_only.fasta`, and the sentinel on
  an invalid reference. One more case: a circular reference whose canned BAM
  has no junction-spanning read must publish `>ID.1.1 linear`, report
  `reference_topology=circular` with `published_topology=linear`, and emit the
  `no_junction` note.
- `tests/testthat/test-backwards-compatibility.R:431`: add the five columns to
  `expect_cols`; migration from the v1.3.10 fixture.
- `new_db(assembler = "MapToRef", maptoref_ref = "x.gb")` stores the five
  values; without a reference it stops; with an invalid `maptoref_topology`
  it stops.
- Fixture: copy `ref_dbs/MitoFinder/NC_002333_Danio_rerio.gb` (55,541 bytes,
  `LOCUS NC_002333 16596 bp DNA circular VRT`) into `inst/test_data/`. It is
  not packaged today, so the copy is part of this work.
- End to end (manual, needs Docker): a `maptoref` parameter set in the test
  project pointing at the Danio file; the two cyprinid samples should reach
  identity > 99% to their GetOrganelle assemblies with differences confined
  to N or IUPAC sites; the divergent samples exercise the warnings; the
  Scyphozoa project exercises the linear path.

---------------------------------------------------------------------------

## 6. Defaults and their justification

| Default | Value | Why (source) |
|---|---|---|
| bowtie2 preset | `--very-sensitive-local` | the repo's own mapping preset (`R/coverage.R:66`); the white paper's best bowtie2 result at 89% identity (advisor 7.2); Geneious Medium is the shipped default and equally common in the literature (advisor 9.8) |
| Iteration cap | 5, user-editable | Geneious default "Iterate up to 5 times" (tutorial, XML `iterate_5`); 17 of 27 published counts; Kemp saturated at 3 to 10; early stop makes a higher cap cheap, so the field takes any value and the help text suggests 10 to 25 for a distant reference (advisor 10.2) |
| Stop rule | bases changed < 5 AND mapped reads changed < 0.1% | judge.md section 6; the base term keeps ambiguity flips inside the duplicated block from consuming the cap, the read term is the aITE rule ("repeat until no new reads map", alt-tools 3.3) |
| N below depth | 3 | XML `coverageThreshold=3` for reference assemblies (advisor 8.3); viral-ngs and aITE use 3 (alt-tools 8); 5 to 10 are practitioner choices for trimming or MSA-grade work, not the Geneious default |
| Base quality floor | 20 | XML `qualityThreshold=20`; ivar and nf-core defaults (alt-tools 8) |
| Mapping quality | not used, not filtered | MAPQ carries no NUMT signal against a mito-only reference and reads inside the duplicated block get MAPQ 1 (judge.md 1.3; `R/circularize_asmb.R:466-471`) |
| Consensus model | samtools Bayesian with `-A` | closest in-image analogue of Highest Quality; IUPAC at mixed sites as Geneious does (alt-tools 4) |
| Reference topology | .gb LOCUS line; FASTA requires the explicit `maptoref_topology` option | a FASTA header carries no topology field, and guessing linear silently changes rotation and the `partial` flag (4.5 step 3) |
| Published topology | reference topology, downgraded to linear when no read spans the junction | the topology gates rotation and `partial`, so it must describe this sample's reads and not the user's reference choice (4.9) |
| No-coverage call | N | manual lists `-`, N, `?`, Ref; Ref pastes reference bases into the sample (advisor 10.2); N is FASTA legal and already handled by export and scoring |
| Duplicates | not removed inside MapToRef; handled upstream in PREPROCESS if at all | mitochondrial depth in a skim is the limiting resource; aITE removes them, viral pipelines often do not; fastp `--dedup` is spiked in phase 1 and becomes a PREPROCESS toggle only if it earns its place (4.8) |
| Circular flank F | min(500, L/2) | `R/circularize_asmb.R:529`, `R/coverage.R:250` |
| Splice point | F/2 | judge.md section 5 |
| Minimum mapped reads | 100 | fidelity.md 5.4; below this the reference is the wrong clade |
| Reference length window | reject outside [5000, 50000], warn outside [10000, 25000] | MitoFinder's NCBI query range 12,000 to 20,000 and `nogene_min_length = 12000` (`R/custom_assembly_db.R:69`), widened for non-metazoan users |
| Paired distance | bowtie2 defaults `-I 0 -X 500` | parity with `coverage()`; Geneious uses a soft penalty around the insert midpoint (advisor 5.4) |
| Read strategy | pass 1 all reads, iteration passes on the recruited subset, final pass all reads | Geneious staff recipe (advisor 5.15) plus the closing all-reads pass every published recipe ends with (advisor 9.10, aITE) |

---------------------------------------------------------------------------

## 7. Risks and mitigations

| Risk | Mitigation |
|---|---|
| Reference bias | reads-only product; N never filled from the reference; iteration replaces reference bases with the sample's; `subs_only.fasta` and the `substitutions versus reference` count make the reference contribution visible, and the `ref_divergent` warning names it; BLAST and reference fetch still run so curation compares against an independent record; docs recommend a GetOrganelle or MitoFinder set on the same samples. The cheapest user-facing check available today needs no code: run the same samples under two parameter sets with two different references and compare, which is exactly the experiment that exposed the effect in the Culicoides benchmark |
| NUMTs | no MAPQ filter is claimed to help (no nuclear alternative in the index); `-A` marks mixed sites; `coverage()` masks depth outliers and shows error-rate windows; documented as a limit of every map-to-reference method |
| Divergent reference | bowtie2 local mode is the composed loop's weakest axis (alt-tools 9); pass 1 is the only pass against the foreign sequence and the final pass is against the converged one; the free-form field accepts `--score-min G,10,6` or `-N 1`; too few reads fails loudly. Published magnitude of the residual effect: in the Westbury palaeognath test, every bait reference except the conspecific one recovered about 14,886 bp against an expected 16,740 bp, regardless of mismatch setting (alt-tools 3.3), so expect under-recovery rather than wrong bases |
| Rearrangements and duplications | not detectable by mapping; the consensus inherits reference gene order; documented; de novo cross-check; phase-3 synteny view against the .gb shows off-diagonal blocks |
| Low coverage | N below 3 is explicit; nothing is guessed; the half-depth duplicated block matters only when the genome mean is below about 8x (open decision 1 removes it) |
| Duplicate reads inflating depth | not removed inside MapToRef (4.8); a duplicated fragment can carry a site past `-d 3` on its own, so treat depth in a heavily duplicated library with care. Phase 1 spikes fastp `--dedup` in PREPROCESS, which would fix this once for every assembler rather than only here; `samtools markdup` remains in the image as a manual pre-filter |
| Repeats and the control region | tandem-repeat copy number collapses to the reference's; random placement smears reads across copies; ambiguity codes and depth spikes are the visible signal; same limit as Geneious (the advisor sends tandem-repeat references to BBMap, which we do not ship) |
| N runs in intermediate references | filled from the previous reference during iterations (4.6 step 5) |
| Circular seam | splice from the appended copy; `--no-use-MQ` hard-coded; `--min-MQ` above 0 refused on circular references (4.7) |
| Declared circularity with no evidence | `junction_depth` in the summary, and the published topology is downgraded to linear with the `no_junction` note when it is 0 (4.9). The reference's own topology is still reported, so a user who disagrees can see exactly what happened and edit the header |
| Indels | CIGAR-only, no local realignment (the one real fidelity gap versus Fine Tuning); applied once at the final call; the change counter cannot see them (4.6 step 6); bcftools route deferred |
| Runtime | two full passes plus subset passes plus the existing coverage pass; about three times the current MitoFinder-branch coverage cost |
| Container size | unchanged; the only image change is the samtools pin, 1.21 to 1.24 (5.4) |
| samtools bump breaking another process | every consensus flag used here exists in both versions, and nothing else in the pipeline pins a samtools behavior that changed; the bump is documented in the Dockerfile, `docker/README.md`, and NEWS so a regression has an obvious first suspect |
| Shell quoting | option strings are interpolated into the `Rscript -e` call like the existing tools; no quotes allowed; documented |
| Nextflow `-resume` | the reference is a staged `path()` input hashed by content; a URL is re-fetched per run like `mitofinder_db` |
| Singularity visibility | a reference outside the project tree may be an unreadable symlink target; docs say keep it under the project folder or bind the directory; phase-2 copy-into-project removes the question |

---------------------------------------------------------------------------

## 8. Open decisions for the maintainer

1. Consensus engine: samtools consensus with the origin splice (v1 as
   written) or fidelity.md's custom R caller over bam-readcount counts with a
   true origin fold (~200 lines of R). The caller buys full depth in the
   duplicated block, the exact Geneious threshold, IUPAC, and tie rule, and a
   Ref/N/gap no-coverage policy without a samtools bump. Recommended default:
   ship v1 with samtools consensus and the splice; run the phase-1 spike on
   real low-coverage samples; adopt the caller in phase 3 only if the block
   shows N at realistic depths or a user asks for the percentage rule.
2. Iteration cap default: 5 (Geneious default, community mode) or 10 (Kemp's
   saturation for the worst reference, free when convergence is early).
   Recommended default: 5, with the help text saying 10 to 25 for a distant
   reference.
3. Column layout: four passthrough columns (this document), three columns with
   `maptoref_consensus` dropped and `-d`/`--min-BQ` exposed as validated
   numbers, or two columns with a parsed option string (fidelity.md, ux.md).
   The free-form consensus string is the weakest of the four columns: it is the
   only one whose contents can silently do nothing or break a documented
   invariant, which is why 4.7 validates it. Recommended default: four columns
   with the validation; drop to three if the validation turns out to be
   fighting users rather than helping them.
4. Deduplication (new): none in v1 (this document), or `samtools markdup -r`
   before the final call, or a per-set toggle. Recommended default: none in
   v1, stated as a limit; revisit if a user reports a duplicate-driven false
   call. Note that the shipped fastp string sets `--dont_eval_duplication`, so
   the project has no duplicate metric today either.
5. Junction evidence for a declared circular topology (new): report
   `junction_depth` and warn when it is 0 (this document), or additionally
   downgrade the published topology to linear when no read spans the seam.
   Recommended default: report and warn only. The reference's topology is the
   user's claim, `rotate_asmb()` and the `partial` flag depend on it, and
   silently rewriting it would surprise anyone comparing two assemblers on the
   same sample.
6. Confirm: linear reference ends are stripped of leading and trailing N runs
   (4.6 step 9) rather than keeping the full reference extent as N. Nothing is
   blocked on this; a consensus should not claim extent it never covered, and
   `partial` handling for linear units already exists.
7. Confirm: FASTA topology comes from a `circular` header token (4.5 step 3),
   with .gb documented as the preferred input. Add a per-set topology option
   only if FASTA users ask.
8. samtools bump 1.21 to 1.24: buys `-T` (reference fill for the product,
   which this design does not want), real consensus multi-threading (1.22
   release notes, so `-@` would start paying), and the leading/trailing-N fix.
   Recommended default: no bump in this feature; revisit when the image is
   rebuilt for another reason.

### 8.1 Decisions taken (maintainer, 2026-09-03)

The eight questions above were answered, and the answers have been propagated
into the doc body (2026-09-03). Section 8 above is kept as the record of what
was on the table; where it and the body disagree, the body and this list are
current. The "affects" notes on each item below name the sections that were
changed.

1. Consensus engine: A, samtools consensus with the origin splice (as written).
2. Iteration cap: default 5, and the value stays user-editable (the
   `maptoref_iter` column and numeric input already provide this).
3. Column layout: four passthrough columns with the validation in 4.7.
4. Deduplication: explore dedup with fastp first (fastp 0.23.4 in the image
   has `--dedup`; the shipped preprocess string uses `--dont_eval_duplication`,
   `R/init_db.R:248`). If the spike shows it is worthwhile, add it as a toggle
   in the PREPROCESS options (a `dedup` column on `preprocess_opts` that swaps
   the flag), not in the MapToRef options. Affects 4.8, 6, 7, and 9.
5. Topology with no junction reads: DOWNGRADE the published topology to
   linear when `junction_depth` is 0, and write an assembly note saying so.
   Replaces the report-only behavior in 4.6 step 10, 4.9, 4.11 (the
   `no_junction` row becomes a downgrade note), 7, and the phase-1 list in 9.
6. Linear reference ends: strip leading and trailing N runs (as written).
7. FASTA topology: when the reference is a FASTA, the user MUST set the
   topology explicitly; a `circular` header token is not enough and there is
   no silent linear default. Add a fifth option column `maptoref_topology`
   (circular or linear, required when the reference is FASTA, ignored for
   .gb where the LOCUS line wins) with a modal select input and a save-time
   check. Affects 4.5 step 3, 5.1, 5.2, 5.3 (sqlRead and opts map gain one
   more column), 5.8, and the migration list.
8. samtools: bump the image from 1.21 to 1.24 as part of this feature, and
   document it (Dockerfile pin, docker/README.md, NEWS). Consensus then has
   real multi-threading, so `-@` pays. Do NOT adopt `-T` (reference fill).
   Replaces the "no bump" text in 5.4 and 7.

---------------------------------------------------------------------------

## 9. Phased implementation outline

Phase 1, pipeline (one PR): `R/map_to_ref.R` with reference prep, the loop,
fill, splice, mark-ins parser, option validation, junction count, the
topology downgrade, the final all-reads pass, outputs, sentinel; unit tests
and the stub-binary test; `assemble.nf`, `assemble_workflow.nf`,
`coverage.nf`; `init_db.R` and migration for the five option columns; the
samtools 1.21 to 1.24 bump in `docker/Dockerfile` plus its `docker/README.md`
and NEWS lines; the Danio fixture. Runnable through
`new_project(assembler = "MapToRef", maptoref_ref = ...)` and the CLI updater
before any UI exists. Start with a half-day spike on three test samples that
re-verifies, on real data, the judge's four findings plus the review's: origin
calls after the splice (position 1 equals position 1), recruitment keeps
half-mapped pairs, N fill lets pass 2 map through, the mark-ins parser
round-trips, the final all-reads pass recovers measurably more than the
recruited subset alone, and `junction_depth` is non-zero on a sample that
really is circular (so the downgrade does not fire on good data). The spike
also answers the deduplication question: run the same samples with fastp
`--dedup` in place of `--dont_eval_duplication` and compare depth, N count,
and the final consensus. If it changes calls for the better, PREPROCESS gains
a `dedup` toggle (4.8); if not, nothing ships and the limit stays documented.

Phase 2, app and docs (one PR): the five modal inputs (including the topology
select), show/hide, upsert, the FASTA-topology save-time check, which is the
whole of save-time validation in v1 (5.5: the reference file itself is not
read at save time); `[maptoref]` notes into `assemble_notes` read from
`<ID>_summary.txt`, folded into the existing assemble write rather than a
second `sqlInsert` (5.3); NEWS, README, vignettes; the
PREPROCESS `dedup` toggle if the phase-1 spike earned it; optional one-way
sensitivity dropdown that writes a preset string.

Phase 3, only if asked: details panel reading `iterations.tsv` and the
summary (reads mapped, reference covered, identity via `run_minimap2_paf()`,
N runs via `find_sequence_gaps()`, passes and convergence); register the .gb
features into `blast_ref_sequences` and `blast_ref_annotations` through
`.write_ref_files()` plus a `blast_ref_override` row so synteny, start-gene
rotation, curation refHits, and offline projects use the user's file; the
custom caller with the count fold (open decision 1); soft-clip extension for
linear ends; `samtools view -e` per-read caps for "multi-best: None" and
maximum gap size.

Deferred (YAGNI), each with its trigger:

- "Re-map all reads every iteration" per-set toggle. The final all-reads pass
  (4.6 step 8) removes the reason this mattered for the product; add the toggle
  only if the spike shows the recruited subset stalling the loop itself on
  divergent samples.
- Sensitivity dropdown in the UI (free-form flags plus the documented preset
  strings cover it; add when users ask for labels).
- Per-sample `Reference` mapping column with `COALESCE` (one set per
  reference works; add when a project has dozens of references).
- Extension past reference ends and partial-seed growth (needs a linear or
  barcode-seed request; fidelity.md's soft-clip design or MITObim).
- "Ref" no-coverage call for the product and `?` characters (reference bias;
  no request).
- Local indel realignment via bcftools (+0.97 MB; needs an indel-quality
  complaint).
- Structural-variant discovery, maximum gap size, minimum overlap filters.
- Multiple references per sample ("combine baits"; the multi-candidate BLAST
  machinery could feed it).
- MIA or MITObim as alternative engines (comparator spike only if divergent
  references prove to be the binding problem).
- Upload widget (no `fileInput` anywhere in the app).
- A separate Nextflow process (the elif inherits publishDir, resources,
  retries, and all DB writes).
- New Assemble table columns (the summary file and notes carry the signal).

---------------------------------------------------------------------------

## 10. Sources

Research reports (all under `tools/map_to_ref_research/`), four lanes in five
files: geneious-advisor.md (sections 5.15, 6.2-6.6, 7, 8.1, 8.3, 9.1-9.10,
10, 11), alt-tools.md (sections 0, 3.1-3.4, 4, 5, 6, 7, 8, 9, 11, "NEW
FINDING 5"), ref-handling.md (sections 1.3, 2, 3, 4, 5.1), codebase-map.md
and codebase-map.v1.md (sections 1-6, 9, 10, 11 of each; where they disagree,
the working tree wins). Design inputs: tools/map_to_ref_research/lean.md, tools/map_to_ref_research/fidelity.md,
tools/map_to_ref_research/ux.md, tools/map_to_ref_research/judge.md (sections 1.2-1.5, 5, 6, 7), tools/map_to_ref_research/critic.md.

Geneious:
- Advisor article (Zendesk API): https://help.geneious.com/api/v2/help_center/en-us/articles/21749604628372.json
- Manual, assembly and mapping: https://manual.geneious.com/en/latest/AssemblyMapping.html
- Manual, consensus semantics: https://manual.geneious.com/en/latest/Alignments.html
- Read Mapper white paper: https://desktop-links.geneious.com/assets/documentation/geneious/GeneiousReadMapper.pdf
- Tutorial: https://www.geneious.com/tutorials/map-to-reference
- Staff answer on iterations: https://help.geneious.com/hc/en-us/community/posts/360068924391
- SARS-CoV-2 recipe and workflow XML (option keys): https://help.geneious.com/hc/article_attachments/7069270589204
- Winn et al. 2025 Bio-protocol: https://pmc.ncbi.nlm.nih.gov/articles/PMC11896769/
- Kemp thesis chapter (iteration saturation): https://bookdown.org/leahmhkemp/welly-trevally-html/mitogenome.html
- Culicoides mapper benchmark: https://pmc.ncbi.nlm.nih.gov/articles/PMC9375341/
- Westbury et al. 2022 (reference divergence, under-recovery, aITE loop): https://www.biorxiv.org/content/10.1101/2021.12.16.472923v1.full ; script https://raw.githubusercontent.com/Mvwestbury/Iterative_mapping/main/BWA/aITE_mapper.sh

Tools:
- samtools consensus 1.21 man page (mode-specific options, `-d` semantics, `-l` line wrapping, lower-case half-present bases): https://raw.githubusercontent.com/samtools/samtools/1.21/doc/samtools-consensus.1 ; current: https://www.htslib.org/doc/samtools-consensus.html
- samtools 1.21 `bam_consensus.c` (ambiguity matrix at :2090-2094 and :2238-2242, `--mark-ins` underscore at :2270, option table at :2415-2455): https://github.com/samtools/samtools/blob/1.21/bam_consensus.c
- samtools 1.22 release notes (`-T`, consensus multi-threading): https://github.com/samtools/samtools/releases/tag/1.22
- bowtie2 manual (presets, `--n-ceil`, `--score-min`, `--rdg/--rfg`, `-N`, `-L`, `--seed`): https://bowtie-bio.sourceforge.net/bowtie2/manual.shtml
- snippy README (`.consensus.subs.fa`, GenBank reference): https://raw.githubusercontent.com/tseemann/snippy/master/README.md
- MITObim: https://github.com/chrishah/MITObim ; MIA: https://github.com/mpieva/mapping-iterative-assembler ; CircularMapper: https://github.com/apeltzer/CircularMapper
- nf-core/viralmetagenome refinement loop: https://nf-co.re/viralmetagenome/1.1.3/docs/usage/workflow/5_variant_and_refinement
- NCBI GenBank release notes, LOCUS line tokens: https://ftp.ncbi.nih.gov/genbank/gbrel.txt (section 3.4.4)

Repository (file:line, working tree 2026-09-03):
- `inst/nextflow/modules/assemble.nf:16` (input tuple, 9 elements today), `:19` (output tuple), `:56` (MitoFinder branch), `:80` (reads tarball), `:102` (closing `fi`)
- `inst/nextflow/modules/assemble_workflow.nf:6-20` (sqlRead, 19 columns), `:99-117` (opts tuple, 8 elements, mf_db at `it[4]`), `:183-195` (cross map, 9 elements), `:356-367` (sentinel handling), `:418` (`partial` seed)
- `inst/nextflow/modules/coverage.nf:35-42` (assembler switch), `:42` (read names)
- `inst/nextflow/modules/preprocess.nf:26-30` (output read names, fastp string)
- `inst/nextflow/modules/blast_ref_fetch_workflow.nf:17`, `blast_genbank_workflow.nf:18`, `blast_ref_align_workflow.nf:15` (three local copies of `appendTaggedNoteSql`)
- `inst/nextflow/modules/prepare_ref_db.nf:15-19` (symlink staging note)
- `inst/nextflow/assets/NO_FILE`; `annotate_workflow.nf:92` (placeholder pattern)
- `inst/config.local:12-13` (resource closure on `opts`)
- `R/coverage.R:58-82` (bowtie2 + bam-readcount, no consensus), `:66-77` (mapping command), `:124` (seam fold), `:250-259` (elongation)
- `R/circularize_asmb.R:466-471` (MAPQ comment), `:522-599` (`count_junction_reads`), `:529` (flank)
- `R/custom_assembly_db.R:69`, `:487-552` (`.cadb_parse_gb`, organelle filter at `:490`, ORIGIN idiom at `:520-524`), `:682` (`.cadb_grab_definition`), `:696` (`.cadb_grab_version`)
- `R/blast_ref_utils.R:22` (`resolve_unit_blast_ref`), `:397` (IUPAC regex), `:401` (`.write_ref_files`), `:1268` (`unit_ref_rotation`)
- `R/custom_curation_db.R:199-214` (FASTA validation pattern)
- `R/init_db.R:45`, `:70-83`, `:131-132`, `:248` (fastp default string), `:311-350`
- `R/backwards_compatibility.R:11-15` (roxygen migration list), `:176-178`, `:1274-1313`
- `R/annotate.R:128`, `:380-392` (rotation gated on topology)
- `R/app_assemble_utils.R:229-241`, `:293`, `:303-309`, `:323-332`, `:438-447`
- `R/app_assemble.R:124-127` (`register_tool_help`), `:847-900`, `:944-958`, `:960-986`, `:1010-1052`
- `R/export.R:110` (`find_sequence_gaps`); `R/scaffold_join.R:357` (`run_minimap2_paf`)
- `R/assembly_path_scoring.R:56-60` (ambiguity scoring)
- `tests/testthat/test-find-mito.R:247-259` (stub-binary pattern); `tests/testthat/test-backwards-compatibility.R:431`
- `docker/Dockerfile:30-33`; `docker/deploy-local.sh:21-24`; `DESCRIPTION:3`
- `ref_dbs/MitoFinder/NC_002333_Danio_rerio.gb`; `inst/tool_help/` (no maptoref entry, by choice, 5.1)

---------------------------------------------------------------------------

## 11. What changed after the completeness review

Every finding in tools/map_to_ref_research/critic.md was checked against the research reports,
the samtools 1.21 sources, and the working tree. All of them held up, and all
are applied. The load-bearing ones:

- The published sequence is now called from a final pass over all reads
  against the converged reference (4.6 step 8), not from the pass-1 recruited
  subset. The earlier draft claimed the existing `coverage` process was that
  pass; it is not, because `coverage()` only measures depth
  (`R/coverage.R:58-82`).
- The circular splice is written as an explicit construction
  (`filled[(L+1)..(L+F/2)]` then `filled[(F/2+1)..L]`). The earlier "rotate
  left by F/2" wording produced a genome mis-rotated by F/2 with the correct
  length, which nothing downstream would have caught.
- Lowercase letters in samtools consensus output are base-versus-gap
  heterozygous codes, not insertion marks. Tokenizing now keys on the `_`
  prefix only, and the half-present codes have a stated policy and a count.
- Nextflow tuple positions are stated once and consistently: both new elements
  are appended at the end, so no existing index moves.
- The free-form consensus string is validated, because `-c`, `-H`, and `-q` do
  nothing outside `-m simple` and `--min-MQ` destroys the circular seam.
- Added because the research covered them and the draft did not: a
  substitutions-only reference-length FASTA (snippy precedent), the read-count
  term in the stop rule (aITE precedent), a junction-depth number and warning
  behind the declared circular topology, an explicit no-deduplication decision,
  and the Westbury under-recovery magnitude in the divergent-reference risk.
- Corrected for honesty: three option rows that were bowtie2 defaults dressed
  as choices, four rows labeled "exact" that are approximate, the fastp
  overhang row (fastp does not trim overhangs), the `-@` speed implication, and
  the claim that a consensus FASTA can be indexed without unwrapping it.
- Added touch points that were verified missing: the `opts_help()` sentence,
  README, `Difficult-Assemblies.Rmd:45`, the migration roxygen list, the
  DESCRIPTION bump and `docker/README.md`, the tool-help convention (declared
  as a choice), and the correct location of the Danio fixture.

---------------------------------------------------------------------------

## 12. Reviewer notes

Every item in tools/map_to_ref_research/critic.md (F1 to F40) was checked and applied. Nothing
was rejected: each finding either matched the working tree at the cited
file:line, matched a research report, or matched the primary tool source the
critic quoted. Re-verified here on 2026-09-03: the assemble sqlRead selects 19
columns (`assemble_workflow.nf:6-20`), the `assemble` input tuple has 9
elements (`assemble.nf:16`), `coverage.nf:40` is the only assembler branch
downstream, `appendTaggedNoteSql` exists as three separate local `def`s and
none of them in `assemble_workflow.nf`, and the Danio reference is
`ref_dbs/MitoFinder/NC_002333_Danio_rerio.gb` (55,541 bytes, `LOCUS NC_002333
16596 bp DNA circular VRT`), not a packaged fixture.

Three items offered a choice, so the choice is recorded here:

- F3 (half-present base codes). The critic allowed either calling them as the
  uppercase base or calling them N, as long as the policy is stated. This
  document calls them N and counts them as `half_deletions=`, because a
  half-present code is exactly the case where the reads do not agree that a
  base is there.
- F6 and F33 (the free-form consensus column). The critic offered validation
  or dropping the column. This document keeps the column and adds the
  validation, and open decision 3 keeps dropping it on the table if the
  validation turns out to fight users.
- F27 (tool help). The critic accepted either the "?" icon convention or the
  per-field manual links, provided the choice is stated. Section 5.1 states it
  and names the fallback.
