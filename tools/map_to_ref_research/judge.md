# Judge assessment: map-to-reference design proposals

Date: 2026-09-03
Branch: map-to-ref-assembly
Inputs: design/lean.md, design/fidelity.md, design/ux.md; research reports
(geneious-advisor.md, alt-tools.md, codebase-map.md, codebase-map.v1.md,
ref-handling.md, summary-*.md); the working tree; the local image
macguigand/mitopilot:1.5.4.

Scores are 0-10 per lens. (a) codebase fit and maintainability, (b) scientific
fidelity and correctness, (c) operational feasibility.

| Proposal | (a) fit | (b) science | (c) ops | Overall |
|---|---|---|---|---|
| lean.md | 9 | 5 | 9 | 7.5 |
| fidelity.md | 7 | 8 | 6 | 7 |
| ux.md | 6 | 3 | 6 | 4.5 |

Winner: lean.md, as the skeleton, but only with the four mandatory fixes in
section 5 (two of them are verified defects in the design as written). The
consensus-calling core of fidelity.md is the thing worth grafting if the
maintainers accept a ~200-line R helper; everything else in fidelity.md and
ux.md is deferrable.

---------------------------------------------------------------------------------------

## 1. What was verified, and how

### 1.1 Repo line references (all three proposals share the codebase-map seams)

Checked in the working tree on 2026-09-03. All of these hold for all three
proposals unless noted:

- `inst/nextflow/modules/assemble.nf:16` input tuple, `:19` output tuple with
  `opts.assembler` at [7], `:27` GetOrganelle branch, `:56` MitoFinder branch,
  `:80` `tar -czvf ... *.fastq.gz`, `:101-102` `fi` closing the elif chain. A
  third `elif` after line 101 is the right seam.
- `assemble_workflow.nf:6-20` sqlRead selects 19 positional columns (it[0]..it[18]);
  `:99-117` the opts tuple has 8 elements (indices 0-7, mf_db at it[8] of the row
  -> tuple index 4); `:183-195` cross map has 9 elements. A reference file added
  at the END of the opts tuple is its 9th element (index 8) and `it[1][8]` becomes
  the 10th element of the cross map. lean.md counts this correctly ("9th element",
  "10th tuple element"). fidelity.md says "8th element" (wrong count, harmless
  since it names `it[1][8]`). ux.md says "gains it[19] after it[8]" (fine).
- `coverage.nf:40` `elif [ !{assembler} == "MitoFinder" ]` with no else; the
  MitoFinder branch extracts `<ID>_preprocess_R{1,2}.fastq.gz` at archive root.
  All three reuse that layout; `elif -> else` (fidelity, ux) or an added `||`
  condition (lean) both work.
- `R/init_db.R:45` roxygen, `:70-83` args, `:130-132` validator (only two
  assemblers), `:310-349` DDL + default row. Correct.
- `R/backwards_compatibility.R:176-178` predicate, `:1293-1310` `mitofinder`
  migration block (ALTER TABLE + rows_upsert). The shape all three copy. Note
  lean.md says "two-statement shape used for join_scaffolds (:435-440), ALTER then
  UPDATE ... WHERE x IS NULL"; the `mitofinder` block at :1293-1310 uses
  ALTER + rows_upsert. Either works; the line refs differ, not the pattern.
- `R/app_assemble_utils.R:290-300` selectize choices, `:313-333` `mitofinder` /
  `mf_db` textInputs with nested `opts_help`, `:437-447` initial hide. Correct.
- `R/app_assemble.R:123-127` `register_tool_help`, `:847-885` update + show/hide,
  `:888-900` toggleState, `:944-958` assembler observer, `:960-986` upsert with
  `req()` on every column. Correct. lean.md's note that the upsert must use
  `%||% ""` rather than `req()` for `maptoref_ref` is right: `req(input$mf_db)`
  already means a MitoFinder set cannot be saved with an empty db field, and a
  GetOrganelle set with an empty MapToRef field would silently fail to save if
  `req()` were copied.
- `R/coverage.R:48-75` (elongate + `bowtie2 --very-sensitive-local --no-unal` +
  `samtools sort`), `:75` `conda run -n bam-readcount bam-readcount -w1 -f`,
  `:99-113` zero-depth rows are absent from bam-readcount output and are padded
  in R, `:249-257` `.coverage_extend_circular(flank = 500)`. Correct.
- `R/circularize_asmb.R:463-495` `contig_depth()` fold, `:520-555`
  `count_junction_reads()` with `flank <- min(500L, len %/% 2L)` and the comment
  that MAPQ must NOT be filtered in the duplicated block. Correct, and this
  comment is exactly the trap ux.md falls into (section 4.3).
- `R/scaffold_join.R:357-378` `run_minimap2_paf(query, ref, cigar = TRUE)` exists
  (ux.md identity metric). `R/blast_ref_utils.R:397` IUPAC regex, `:400-421`
  `.write_ref_files()`, `:22-41` `resolve_unit_blast_ref()`. Correct.
- `R/custom_assembly_db.R:487-517` `.cadb_parse_gb` record split on `//` and
  ORIGIN extraction, `:682-698` `.cadb_grab_definition` / `.cadb_grab_version`.
  Correct.
- `tests/testthat/test-find-mito.R:247-259` PATH-stub pattern. Correct.
- `R/app_assemble_coverage_details.R:96-116` directory resolution and `:171`
  `reactableOutput(ns("table"))` (ux.md panel insertion point). Correct.
- `inst/nextflow/assets/NO_FILE` exists; `annotate_workflow.nf:92` is the
  placeholder pattern. `inst/config.local:12-13` resource closure keyed on the
  `opts` input. `ref_dbs/MitoFinder/NC_002333_Danio_rerio.gb` is one record,
  `LOCUS ... 16596 bp DNA circular VRT`. `inst/tool_help/` has no maptoref file.
- `docker/Dockerfile:27-36`: bowtie2 2.5.4, samtools 1.21, minimap2 2.28,
  bam-readcount 1.0.1 in its own env. No bcftools/bwa/pysam. Correct.

### 1.2 Container probes (macguigand/mitopilot:1.5.4, run 2026-09-03)

`samtools consensus` 1.21 help: `-r`, `-a`, `-m simple|bayesian`, `--min-MQ`,
`--min-BQ`, `--show-del yes/no [no]`, `--show-ins yes/no [yes]`, `--mark-ins`,
`-A`, `-d`, simple-mode `-q/-c/-H`, Bayesian `-C`, `--(no-)use-MQ`, `-@`. No `-T`
(confirmed: "invalid option -- 'T'"). So every flag lean.md and ux.md name exists,
and the alt-tools finding that `-T` needs 1.22 is right.

`samtools view -e` 1.21: `[NM] + sclen <= 0.30*qlen`, the CIGAR gap-size regex,
`(qlen - sclen) >= 25`, `endpos`, `pos == 1 && cigar =~ "^[0-9]+S"` all evaluate.
`[XS] == null` FAILS ("Couldn't process filter expression"); the working form is
`!exists([XS]) || [AS] > [XS]`. fidelity.md section 2.2 has the broken form.

`--mark-ins` output: inserted bases are emitted as `_g_g` (underscore prefix,
LOWERCASE), not `+g+g`. The 1.21 help text says "Add '+' before every inserted
base" but the FASTA output uses `_`. lean.md section 2.1 steps 4-5 count
"characters not preceded by `+`" and "drop `*` and `+`"; as written the clip awk
would miscount every insertion and leave lowercase letters in the consensus.
Trivial to fix, but the "verified in the local image" claim did not cover it.

bam-readcount 1.0.1 indel reporting (synthetic 3-bp deletion and 2-bp insertion):
the deletion allele `-TTA:33` is reported on the FIRST deleted base and the two
following deleted positions have depth 0; the insertion `+GG:26` is reported on
the base PRECEDING the insertion (CIGAR `141M2I7M` at pos 7862 -> reported at
8002). Zero-depth positions are omitted (17058 rows for a 17096 bp reference).
fidelity.md section 2.3 describes exactly this. Verified.

### 1.3 Simulation: the circular origin (the load-bearing correctness check)

Setup: NC_002333 with 3% random substitutions plus one 3-bp deletion and one 2-bp
insertion as the "sample"; 1659 read pairs (150 bp, 350 bp insert, 30x, 0.5%
error, Q40); reference elongated by F = 500; `bowtie2 --very-sensitive-local
--no-unal`. Results:

- Reads fully inside the duplicated block get MAPQ 1 (20 of 36 on copy 1, 14 of
  30 on copy 2), the rest 11-44. Depth in the block is split roughly evenly
  between the two copies (18x on copy 1 at position 250, 12x on copy 2 at
  16846). This is the fold rationale in `circularize_asmb.R:466-471`.
- Depth at positions 1-25 of copy 1 is ZERO, then ramps (1x at 26, 3x at 37).
  Reason: a read covering base k < read length without spanning the origin must
  START inside 1..k, and origin-spanning reads land on copy 2 only. The mirror
  ramp is at the end of copy 2 (17083-17096 depth 0).
- lean.md flags (`-a -A -d 3 --min-BQ 20 --no-use-MQ --show-del yes --mark-ins`),
  clipped at L: 36 consecutive N at positions 1-36; 108 N total (the rest are
  sub-3x sites from 30x with 0.5% error, fine). The appended-block calls for
  positions L+1..L+40 match the sample truth at 1..40 exactly, so the
  information is there, on the other copy.
- Same flags WITHOUT `--no-use-MQ` (samtools default): 95 N in 1..600 and 218
  total. lean.md's claim that `--no-use-MQ` is load-bearing is right.
- ux.md flags (`--min-MQ 20 ... --show-ins no --show-del no -r ref:1-16596`):
  299 N, ALL inside positions 1..600, i.e. the whole duplicated block is N because
  every MAPQ-1 read is excluded. Output length 16592 = 16596 minus 4 deletions,
  so `--show-del no` does NOT keep reference coordinates (ux.md section 2.1 step 2
  says it does).

### 1.4 Simulation: read recruitment with `--no-unal` (lean.md step 3)

50 pairs with one mapping mate and one random mate were added. With `--no-unal`,
bowtie2 2.5.4 writes 0 records with flag 4: the unmapped mate of a half-mapped
pair is DROPPED, not written. `samtools view -G 12 | samtools sort -n | samtools
fastq -1 -2 -s /dev/null` then reports "discarded 50 singletons" and the
recruited subset contains 0 of the 50 half-mapped pairs. lean.md's "pairs where
at least one mate aligned" recruitment therefore recruits only fully mapped
pairs as written. Fix: drop `--no-unal` on pass 1 (or use `--al-conc`/`--un-conc`
files). Its phase-1 spike item names this exact check, so it would have been
caught, but the design text is wrong.

### 1.5 Simulation: N runs in the mapping reference (affects all three)

Reference with an internal N run of 10 / 30 / 60 bp, same reads:
- 10 bp: reads align through (depth 34 inside the run).
- 30 bp and 60 bp: depth 0 inside the run, and flanking depth collapses (14x at
  5990 vs 38x with no N) because reads that would cross the run are soft-clipped
  or dropped (`--n-ceil L,0,0.15` = 22 ambiguous positions per 150 bp read).
So any N run longer than ~15% of the read length in an intermediate consensus is
self-perpetuating: the next pass cannot map through it, and the region can only
get worse. All three proposals feed the N-bearing consensus back as the next
mapping reference (lean 2.1 step 3, fidelity 2.1 step 2, ux 2.1 step 2). Combined
with 1.3, the 36-N origin run in lean/ux would degrade the seam every pass.

---------------------------------------------------------------------------------------

## 2. lean.md

### 2.1 Strengths
- Smallest diff by far (~220 lines, 9 files, no new R function, no new process,
  no container change) and every touch point checks out against the tree
  (section 1.1). Correct tuple arithmetic, correct sentinel/exit-0 contract,
  correct `%||%` vs `req()` note for the upsert, correct MitoFinder tarball
  layout so `coverage()` runs unchanged.
- `--no-use-MQ` is the right call and the simulation shows why (38 vs 95 N).
  The reasoning that MAPQ carries no NUMT signal against a mito-only reference is
  correct, and ux.md's contrary NUMT claim is wrong.
- Recruit-then-iterate keeps passes 2..N cheap; this is also the Geneious staff
  recipe (advisor 5.15) and what the GetOrganelle branch effectively does.
- The option table (2.4) is the most honest of the three: "not replicable" is
  used where it should be, and the sensitivity ladder maps onto real bowtie2
  presets that the repo already uses.
- Failure handling (reference parse errors, zero mapped, OOM) all route through
  existing mechanisms. `-resume` and URL staging reasoning is right.
- The YAGNI list is well chosen; the Danio single-record fixture is the right
  test asset (55 KB, `circular VRT`).

### 2.2 Weaknesses (specific)
1. Section 2.1 step 7 / section 6 "Circular seam": the claim "positions 1..F are
   called from the half of the reads that landed on the first copy ... Nothing
   else to do" is false at the origin. Verified: 36 N at positions 1-36 at 30x
   (section 1.3). The depth at position 1 of copy 1 is structurally near zero;
   only a fold (fidelity.md) or a splice from the appended copy fixes it. This is
   a shipped defect in every circular assembly, in the first tRNA of every
   vertebrate mitogenome, and it gets worse with iteration (section 1.5).
2. Section 2.1 step 3, recruitment: `--no-unal` discards the unmapped mate, so
   `-G 12` recruits nothing beyond fully mapped pairs (section 1.4). As written
   the loop cannot grow into divergent regions at all; even fixed, growth is
   limited to one insert length from pass-1 islands because the subset is fixed
   after pass 1. The white paper's headline result (89.8% -> 100% mapped with
   iteration, advisor 7.2) depends on re-mapping ALL reads each pass. lean.md
   admits the hard-subset limit in section 6 but understates it.
3. Section 2.1 steps 4-5: `--mark-ins` emits `_x` lowercase, not `+x`; the clip
   awk as specified miscounts insertions. Small, but the doc claims image
   verification.
4. Section 2.1 step 3, stop rule: byte-identical consensus is a fine rule, but
   with random placement in the duplicated block and `-A` ambiguity calls the
   consensus can flip a base each pass and never converge; that only costs the
   cap (5), so it is a cost issue not a correctness one. Logged, not handled.
5. Section 2.1 step 4: Bayesian mode is labelled "the analogue of Highest
   Quality". It is quality-aware but has no 60% share semantics, no cumulative
   IUPAC rule, and its `-C 10` cutoff decides N vs call. Labelled approximate in
   the table, which is honest, but the user-facing "consensus threshold" knob is
   then a free-form samtools flag string, not a Geneious-shaped control.
6. No extension past linear ends (deferred with a reason). Acceptable for the
   stated scope, but the Scyphozoa test project is linear, and a linear reference
   consensus is clipped to reference extent while `trimToReference=false` is the
   Geneious default. Documented.
7. N-run stickiness across passes (section 1.5): not recognised.
8. Section 4.5: four free-form text fields (bowtie2 flags, samtools consensus
   flags, iteration cap) put the seam-critical `--no-use-MQ` into a user-editable
   string; the help text warns, but a user removing it silently breaks the origin.

### 2.3 Scores
(a) 9. (b) 5: the loop skeleton is right but two verified defects (origin N run,
recruitment) and one shared defect (N stickiness) mean the v1 as written ships a
wrong sequence at the origin. (c) 9: cheapest runtime of the three (one full
pass plus cheap subset passes plus the existing coverage pass), zero new
binaries, correct failure routing.

---------------------------------------------------------------------------------------

## 3. fidelity.md

### 3.1 Strengths
- The only proposal whose circular handling is actually correct: folding
  bam-readcount allele counts (section 2.3 step 1) recombines both copies, so
  the origin gets full depth. The bam-readcount format facts it relies on are
  verified (section 1.2).
- The consensus caller reproduces the manual's documented rule (advisor 6.5:
  cumulative threshold, all-or-none ties, the 6A/3G/1T example) and the XML
  defaults (advisor 8.3: weighted_60, thresholdPercentNoQuality 65,
  coverageThreshold 3, noCoverageCharacter, trimToReference false). It is the
  only design that can offer "If no coverage: Ref" without a samtools bump and
  the only one with a per-position mask table (`mask.tsv`), which is the snippy
  `.aligned.fa` idea the alt-tools report singled out.
- Linear-end extension from soft-clipped overhangs (2.4) is small, uses only
  `samtools view -e`, and is the one thing the other two defer.
- Per-read caps via `samtools view -e` give the Geneious "max mismatches %",
  "max gap size", "min overlap", "min overlap identity" and "multi-best: none"
  semantics as real filters, not approximations. The expressions were probed
  (one syntax error, below).
- Integration is still the minimal shape: third elif calling
  `Rscript -e "MitoPilot::map_to_ref(...)"` exactly like `coverage.nf` calls
  `MitoPilot::coverage()`; two columns; coverage `elif -> else`; `.cadb_*`
  helpers and the `test-find-mito.R` stub pattern reused. Unit tests on pure
  functions are the best test plan of the three.
- The reference-bias tripwire (substitutions vs reference in the summary) and
  the honest fidelity labels (EXACT/APPROX/NOT with the INFER marks on Medium-High
  and Highest presets) match the research exactly.

### 3.2 Weaknesses (specific)
1. Section 2.2, multi-best filter: `[XS] == null` is not valid samtools filter
   syntax in 1.21 (verified error). Use `!exists([XS])`. One-token fix.
2. Section 2.5 / 6 runtime: every pass re-maps the FULL read set, default cap 10,
   and the "Highest" preset is `-L 10 -N 1 -i C,1,0 -D 30 -R 4`. `-N 1` with a
   constant seed interval of 1 is the slowest configuration bowtie2 offers (the
   manual warns `-N 1` is much slower) and the distant-reference recipe the doc
   recommends is "Highest, 25 iterations". On a 30 M-pair skim that is many
   hours, not the "on the order of an hour" claimed. The Geneious staff recipe
   (normalize/recruit, then one full pass) is listed as deferred "for fidelity",
   which inverts the cost/benefit for an HPC pipeline whose coverage step is
   already the known slow part.
3. Section 2.5 stop rule (a) `n_k <= n_{k-1}`: after pass 1 the consensus carries
   N runs wherever depth < 3; section 1.5 shows those runs repel reads, so the
   pass-2 count can DROP even while other regions improve, and the loop stops at
   pass 2 with the pass-2 consensus. The rule needs either ref-fill during
   iteration (section 5 graft) or a tolerance.
4. Section 2.3 step 3, HQ weighting: `count * avg_baseq` is the manual's summed
   quality, fine; the BQ/MQ combination formula is invented (labelled APPROX).
   Also MQ is excluded only for positions 1..F, but junction-spanning reads that
   land on copy 2 have high MQ and fold into 1..F; the exclusion should be
   applied per read source, not per position. Minor.
5. Section 2.2 caps on a divergent reference: `[NM] + sclen <= 0.30*qlen`
   counts soft-clipped bases as mismatches. In local mode against a 10-15%
   divergent reference many reads are clipped at a divergent stretch; those
   reads are dropped rather than half-used, which is stricter than Geneious
   (which would align through at 30%). The doc says so (section 6), but the
   effect on pass-1 recruitment for divergent references is not quantified.
6. Section 2.1 step 4 / 4.1: the Rscript call interpolates `'!{opts.maptoref}'`
   inside double quotes; the default string has no quotes so it works, but any
   user-added value with `'` breaks it. Same class as the existing tools, fine.
7. Section 3.2 save-time validation in the app: `maptoref_prepare_ref()` on the
   head node is cheap (55 KB), but the "only when the value is a local path that
   exists" guard means HPC users typing a compute-node path get no check, which
   is correct but should be spelled out in the alert.
8. Size: ~650 lines of new R plus ~300 of tests in a package whose maintainers
   ask for boring, narrow code. The caller itself is ~200 lines; the rest
   (option parser with 14 keys, five presets, extension, mask table, stats) is
   where the bulk sits and most of it is deferrable.
9. Shares the N-run stickiness defect (section 1.5) through `--no-cov-call N`
   and `low_cov -> N` in every intermediate consensus.

### 3.3 Scores
(a) 7: right seams, good reuse, good tests, but 1,300 lines and a 14-key
option grammar. (b) 8: the most faithful design by a wide margin and the only
correct origin; docked for the stop-rule/N interaction and the invented MQ
formula. (c) 6: all tools present and probed, but full-read passes at cap 10
with an extreme "Highest" preset is a real HPC cost problem, and it explicitly
refuses the cheap recruitment recipe.

---------------------------------------------------------------------------------------

## 4. ux.md

### 4.1 Strengths
- Best user-facing thinking: Geneious-named controls (Sensitivity, Iterate up
  to, Call N below depth, Consensus threshold), a details panel with the
  metrics Kemp and the Culicoides benchmark say matter (reads mapped, reference
  covered at >= min depth, identity to reference, consensus vs reference length,
  N runs with coordinates, iterations run and convergence), and a warning table
  with concrete thresholds and plain-language text. The 90% identity
  `ref_divergent` tripwire is well sourced (white paper floor).
- Plain-English reference validation messages (3.2) are good and the
  "warn but continue when the path is unreachable from the app host" rule is the
  right HPC behaviour.
- The phase-2 idea of registering the `.gb` features through
  `.write_ref_files()` + `blast_ref_override` so synteny, start-gene rotation,
  refHits and offline projects all work is correct in every line ref it cites
  and is the best long-term payoff of a `.gb` reference in any of the three.
- Substitutions-only intermediate consensus with indels applied once at the end
  is a sound idea for keeping coordinates stable and diffs countable.
- Stats JSON in the published folder, no schema change, is consistent with how
  coverageStats.csv is consumed.

### 4.2 Weaknesses (specific)
1. Section 2.1 step 2 and 2.2 defaults, `--min-MQ 20` as an internal constant:
   verified to turn the entire duplicated block (positions 1..~300 at 30x) into N
   (299 N, all in 1..600). The repo's own comment at `circularize_asmb.R:466-471`
   says exactly why this must not be done. This is a shipped defect in every
   circular sample and the design also relies on it for NUMT mitigation (section
   7), which is wrong for a mito-only reference (MAPQ carries no NUMT signal).
2. Section 2.1 step 2 "substitutions only ... coordinates stay reference-stable"
   is implemented with `--show-del no`, which REMOVES deleted bases (verified:
   16592 bp output for a 16596 bp reference). The `hamming()` diff and the
   "plain cut at len" fold both assume equal length. The correct flags are
   `--show-del yes --show-ins no` and then `*` -> reference base (or N).
3. Section 2.1 step 2 `-r work:1-len` is a cut, not a fold: the appended copy's
   calls are discarded, so positions 1..~40 have the depth ramp of section 1.3
   even without the MQ filter. The doc acknowledges "roughly halved" and
   proposes a phase-3 rotation trick; the real number is zero depth at the
   first ~25 bases.
4. Section 2.1 step 3 "final pass always runs, also when iterations = 0" plus
   full-read re-mapping in every iteration: with the default cap 5 that is up
   to 6 full bowtie2 passes plus the coverage pass, 7 per sample. Runtime is
   listed as a risk but the design has no recruitment or subsetting at all
   ("read normalization or subsampling" is on the YAGNI list).
5. Section 4.2 `--bowtie2-args "..."` inside the options string, which section
   4.1 item 11 interpolates as `Rscript -e "MitoPilot::map_to_ref(... '!{opts.maptoref}' ...)"`:
   the inner double quotes terminate the shell string. The advanced escape hatch
   breaks the task as designed.
6. Section 2.2 default `--sensitivity medium-low = --sensitive-local`: weaker
   than the `--very-sensitive-local` the repo already uses for the same job
   (`coverage.R:61`), chosen for Geneious-label parity rather than results.
   The Medium-Low label in Geneious tolerates 20% mismatches; `--sensitive-local`
   is a seed-effort preset with no such meaning.
7. Section 4.1 item 5, the two-way sync observer between six structured inputs
   and the advanced string "guard against loops with ignoreInit and a value
   comparison": this is the kind of Shiny state machine that produces the
   reactiveValues bugs already in the project's memory. Six inputs, a parser,
   a composer, and a round-trip test for a v1 is not minimal.
8. Section 5.3 warnings and the details panel (~80 lines) are good but are
   phase-1 scope in a design whose core loop has three correctness defects.
9. Section 3.2 warning "reference record does not say it is mitochondrial" is
   fine; the length window and IUPAC checks duplicate fidelity.md's. No issue.
10. Shares the N-run stickiness defect (section 1.5).

### 4.3 Scores
(a) 6: touch points correct, but the sync observer, details panel, JSON stats
and quoting bug add complexity for no correctness gain. (b) 3: two verified
defaults (`--min-MQ 20`, `--show-del no`) produce a wrong origin and wrong
coordinates; the NUMT rationale is wrong; sensitivity default is a regression
from the repo's own preset. (c) 6: tools present, but the most bowtie2 passes
per sample of the three and a broken escape hatch.

---------------------------------------------------------------------------------------

## 5. Mandatory fixes for the winning skeleton (lean.md)

1. Origin: do not clip at L. Take the elongated consensus, keep reference
   positions F/2+1 .. L+F/2 (i.e. splice the appended copy's calls in for
   positions 1..F/2), then un-rotate by F/2. Verified in section 1.3 that the
   appended copy's calls at L+1..L+40 equal the true sequence at 1..40. This is
   one more awk step and gives ~half depth everywhere in the block instead of a
   ramp to zero. (fidelity.md's count fold gives full depth; the splice is the
   samtools-only approximation of it.)
2. Recruitment: run pass 1 WITHOUT `--no-unal` (or with `--al-conc`), so
   `-G 12` actually keeps the unmapped mate. Verified in section 1.4.
3. Iteration reference: during passes 2..N, fill every N (no-coverage and
   sub-depth) with the previous mapping reference's base at that position
   (`--show-del yes --show-ins no` keeps coordinates, then a positional merge in
   awk or R). Apply the N policy only in the final call. Verified in section 1.5
   that N runs > ~22 bp block re-mapping. This is "Ref" fill for the mapping
   reference only; the published consensus stays reads-only, so reference bias
   in the product is unchanged.
4. Clip awk: insertions are `_x` lowercase with `--mark-ins`; strip `_` and
   uppercase, or use `--show-ins no` during iterations (then insertions are
   only applied in the final call).

Optional but recommended: keep `--no-use-MQ` out of the user-editable string
(hard-code it, expose `--min-MQ` separately if at all).

---------------------------------------------------------------------------------------

## 6. Ideas worth grafting from the non-winning proposals

From fidelity.md:
- The bam-readcount count fold + custom column caller (section 2.3), IF the
  maintainers will take ~200 lines of R. It buys: full origin depth, the exact
  Geneious threshold/IUPAC/tie rule, "If no coverage: Ref/N/gap" without a
  samtools bump, and a `mask.tsv` per-position status table. bam-readcount is
  already the repo's pileup tool (`coverage.R:75`). If not taken, keep samtools
  consensus and the splice.
- `!exists([XS]) || [AS] > [XS]` as the "map multiple best matches: None"
  option, and `--no-mixed --no-discordant` for "only map paired reads which map
  nearby" (both one flag, both exact).
- `maptoref_prepare_ref()` reusing `.cadb_grab_version`/`_definition`, the
  CRLF strip, token-wise LOCUS topology, `/transl_table` capture with a warning
  against `samples.genetic_code`, and the length window [5000, 50000]. Do it in
  R inside the task (the awk parser in lean.md is CRLF-unsafe; ref-handling 4).
- The per-iteration `iterations.tsv` columns and the `stop_reason` field.
- The "substitutions vs reference" count in the summary as a bias tripwire.
- Pure-function unit tests for the caller and the reference parser; the
  stub-binary loop test.
- Linear-end extension from soft clips (2.4) as a later phase when a linear or
  partial reference is actually requested; it is the cheapest extension design
  on the table.

From ux.md:
- The details-panel metrics list (reads mapped, reference covered at >= depth,
  identity to reference via `run_minimap2_paf(cigar = TRUE)`, consensus vs
  reference length, N runs via `find_sequence_gaps()`, iterations/converged) as
  a v1.1 panel reading a JSON or the summary file. Nothing in it needs a schema
  change.
- The warning table thresholds and texts (`ref_divergent` < 90%, `few_reads`,
  `incomplete` > 2% below depth, `not_converged`, `code_mismatch`), and the
  `[maptoref]` tagged note via `appendTaggedNoteSql` (verified at
  `blast_ref_fetch_workflow.nf:16-20`).
- The phase-2 `.gb` registration into `blast_ref_sequences` /
  `blast_ref_annotations` + `blast_ref_override` (sections 3.5 and 8). All the
  cited helpers exist. This is the real payoff of asking for a `.gb`.
- The validation message texts (3.2) for the task log and a future save-time
  check.
- A Geneious-labelled sensitivity dropdown that writes a bowtie2 preset string,
  but as a one-way convenience (dropdown -> string), not a two-way sync.
- "Iterate up to" wording and the note that 0 = single pass.

From the research that none of the three used:
- Stop rule tolerance: treat "mapped reads changed by < 0.1% AND consensus
  changed by < 5 bases" as converged, so ambiguity flips in the duplicated block
  do not burn the cap.

---------------------------------------------------------------------------------------

## 7. Cross-cutting notes for the synthesis

- All three agree on the seams and they are all correct: third elif in
  `assemble.nf`, `path(ref)` appended to the input tuple with the NO_FILE
  placeholder, columns appended at the end of `sqlRead`, MitoFinder tarball
  layout so `coverage.nf` needs one line, sentinel + exit 0 on failure, no new
  process, no config change, no new DB tables. Take that as settled.
- Column count: lean.md's four columns (ref, bowtie2 flags, consensus flags,
  cap) vs the others' two (ref, one option string). Two columns with a parsed
  string is the house pattern only if the string is parsed by R; if the branch
  stays pure shell, separate columns are simpler and safer than a shell parser.
- Reference parsing: do it in R inside the task (all the pieces exist), not awk
  (CRLF, lowercase, IUPAC). Even a shell-only branch can call one small exported
  R helper for this.
- Runtime shape that satisfies both fidelity and ops: pass 1 all reads; passes
  2..N on the recruited pairs (mates kept); the existing `coverage.nf` pass IS
  the final all-reads pass. Offer a per-set toggle to re-map all reads every
  pass for distant references, off by default.
- Defaults to carry: `--very-sensitive-local` (repo preset, white-paper best
  bowtie2 result); depth 3 (Geneious XML); cap 5 (Geneious default, community
  mode), 10 is equally defensible; no-coverage N (never Ref in the product);
  MAPQ not used and not filtered in the duplicated block.
