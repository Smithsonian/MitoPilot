# Completeness critique of design/final-design.md

Date: 2026-09-03
Reviewer: completeness critic
Inputs: prompts/context.md, geneious-advisor.md, alt-tools.md, codebase-map.md,
codebase-map.v1.md, ref-handling.md, the four summary-*.md, design/lean.md,
design/fidelity.md, design/ux.md, design/judge.md, design/final-design.md.
Verification done for this review: repo working tree at file:line, the samtools
1.21 tagged man page and 1.21 bam_consensus.c source, the current htslib
consensus doc, samtools 1.22 and 1.23 release notes, the bowtie2 manual, the
samtools view man page, and the anaconda.org API. Container simulations from
judge.md were NOT re-run (no container here) and are taken as reported.

Verdict: the shape is right and the plumbing is very nearly correct. Six items
below are real defects that will produce wrong output or a broken patch if the
document is implemented as written. The rest are honesty, completeness, and
scope fixes.

---------------------------------------------------------------------------

## Priority 1: correctness defects, fix before any code is written

### F1. The product is called from the recruited subset, not from all reads;
### section 4.4 claims the opposite

Where: 4.4 (last paragraph), 4.6 step 8, 4.10, and the 2.4 digest.

What the document says: "The final all-reads pass that the Geneious staff
recipe recommends is the existing `coverage` process, which re-maps every read
to the published FASTA" (4.4), and 2.4 states every published recipe finishes
"with one clean pass of all reads".

Why it is wrong: `coverage()` maps reads and calls bam-readcount for depth and
error rates (R/coverage.R:58-82). It never calls a consensus and cannot change
the published sequence. Meanwhile 4.6 step 8 calls the final consensus from
`pass_k.bam`, and for any cap > 1 that BAM was built from `sub_R1.fq`/`sub_R2.fq`
only, i.e. pairs that had at least one mate mapping to the ORIGINAL user
reference (4.6 step 3). Reads that become mappable only after the reference has
moved toward the sample, which are exactly the reads the loop exists to
recruit, are absent from the BAM that produces the product. The design already
admits the bound in 4.10 ("pairs with neither mate mapping to the original
reference never return") but then contradicts it in 4.4.

Fix: pick one and state it in 4.4, 4.6 step 8, and section 7.
(a) Add a final full-reads mapping pass against `ref_(k)` before the final
call. Cost: one more full bowtie2 pass, so the runtime line in 4.6 becomes
about 3x the coverage step, not 2x. This is what the Geneious recipe actually
describes and it makes open decision 4 unnecessary.
(b) Keep the subset and delete the "final all-reads pass" claim, saying plainly
that the product is called from reads recruited in pass 1.
Recommended: (a). It is one more `bowtie2 | samtools sort` line and it removes
the single largest science gap in the proposal.

### F2. The circular splice formula is wrong as written

Where: 4.6 step 5 ("take positions F/2+1 .. L+F/2 of the filled string and
rotate it left by F/2") and the same wording in step 8.

Layout of the elongated consensus: positions 1..L are the reference frame,
positions L+1..L+F are the appended copy of reference positions 1..F.

Taking F/2+1 .. L+F/2 yields the string
`[ref F/2+1 .. L][copy 1 .. F/2]`. To put the origin back at position 1 you
must move the LAST F/2 characters to the front, which is a right rotation.
Rotating left by F/2, as written, starts the sequence at reference coordinate
F+1 and produces a scrambled genome. The bug is silent: length is still L, so
nothing downstream complains, and the whole assembly is mis-rotated by F/2.

Fix: replace the prose with the explicit construction, in both step 5 and step
8 (in step 8 it is token indices, not characters):

    cons_k = filled[(L+1) .. (L+F/2)] followed by filled[(F/2+1) .. L]

and state the invariant `nchar(cons_k) == L` plus the unit test
(".mtr_splice() on a synthetic 200 bp reference with F = 50" in 5.8 must assert
that position 1 of the output equals position 1 of the truth, not just that the
length is right).

### F3. Lowercase letters in samtools consensus output are not insertions

Where: 4.6 step 8 ("`--mark-ins` writes as `_x` with lowercase bases ... drop
`*`, drop `_`, uppercase") and 3.2/3.1 which repeat the judge's `_x lowercase`
observation.

Primary source (samtools 1.21 `bam_consensus.c`): with `-A`, the consensus
character is taken from the 5x5 ambiguity matrix

    "AMRWa" "MCSYc" "RSGKg" "WYKTt" "acgt*"

at lines 2090-2094 and 2238-2242. The lowercase letters in the last row and
column are the base-versus-gap heterozygous codes; the 1.21 man page says so
explicitly ("although IUPAC has ambiguity codes for A,C,G,T vs any other
A,C,G,T it does not have codes for A,C,G,T vs gap ... we use lower-case letter
to symbolise a half-present base type"). They can appear at ANY position, with
or without insertions. Separately, `--mark-ins` emits only an underscore
(`kputc('_', seq)` at :2270); an inserted base is lowercase only when it is
itself heterozygous, uppercase otherwise.

Two consequences for step 8 as written: a tokeniser that keys on case will
mis-tokenise homozygous insertions, and blanket uppercasing silently promotes a
half-present base (a heterozygous deletion) to a solid called base.

Fix: tokenise strictly on the `_` prefix (`_[ACGTacgt]`), never on case; and
state the policy for lowercase het-with-gap codes explicitly. Recommended: keep
them as the uppercase base and count them in a new `half_deletions=` summary
key, or call them N; either is defensible, silence is not. Add a
`.mtr_parse_marked()` test case with an uppercase inserted base and a lowercase
het code at a non-insertion position.

### F4. Nextflow tuple positions contradict each other

Where: 5.3 rows 3, 4, and 5.

The document says the opts tuple "gains a 9th element", that the cross map
"forwards it as `it[1][8]` (10th element)", and that `assemble.nf:16` gains
`path(ref)` "after `path(mf_db)`". Those cannot all be true. Verified counts:
the opts tuple at assemble_workflow.nf:99-117 has 8 elements today (ID,
opts_id, opts map, dbs list, mf_db, genetic_code, max_paths, max_scaffolds), so
the ref is index 8, the 9th element, and `it[1][8]` is right. The cross map at
:183-195 emits a 9-element tuple today, so the ref makes 10 elements. But if
`path(ref)` is inserted after `path(mf_db)` in the process input, the cross map
must emit it in that position (index 5), not last.

Fix: state the ordering once. Recommended: append `path(ref)` at the END of the
`assemble.nf:16` input tuple and at the end of the cross map output, so no
existing position moves; then correct "10th element" to "9th element of the
opts tuple, 10th of the process input tuple".

### F5. Product length: 4.7 and 4.6/4.8 disagree

Where: 4.7 row "Trim to reference" says "circular: consensus is exactly L";
4.6 step 8 and 4.8 say deletions and insertions are applied at the final call
"so the product length can differ from L".

Fix: the indel-applied statement is the intended behaviour; correct the 4.7 row
to "circular: consensus covers exactly the reference extent, length L plus or
minus called indels".

### F6. Free-form `maptoref_consensus` accepts flags that are silently ignored
### or that break the circular seam

Where: 4.7 (threshold percentage row), 5.2, 6.

Primary source (1.21 man page): `-q/--use-qual`, `-c/--call-fract` and
`-H/--het-fract` are introduced by "The following options apply only to the
simple consensus mode", and `-C/--cutoff` by "The following options apply only
to Bayesian consensus mode". The shipped default (`-A -d 3 --min-BQ 20`) runs
in Bayesian mode, so a user who follows the 4.7 advice and types `-c 0.65`
without `-m simple` gets no error, no warning, and no effect. Separately, the
design hard-codes `--no-use-MQ` because MAPQ 1 reads in the duplicated block
must not be discounted (4.6 step 4), yet a user can still type `--min-MQ 20`
into the same free-form string and destroy the seam; 4.7 flags this as a
"documented trap" and judge.md 1.2 measured it (299 N inside positions 1..600).

Fix, cheapest first: in `map_to_ref()`, scan the consensus string and (i) warn
into `assembler.log.txt` and the summary if `-c`/`-H`/`-q` appear without
`-m simple`, (ii) warn or refuse if `--min-MQ` is greater than 0 while the
reference is circular, (iii) refuse `-T`, `-a`, `-A`, `--show-del`,
`--show-ins`, `--mark-ins`, `--no-use-MQ`, `-o`, `-f`, `-r` because the code
sets them. Alternative worth considering (ponytail): drop the column from v1
and ship three option columns; `-d` and `--min-BQ` are the only two knobs the
defaults table actually justifies exposing.

---------------------------------------------------------------------------

## Priority 2: unverified or misstated claims about tools and flags

### F7. `-@` on samtools consensus 1.21 buys nothing

`-@` is accepted in 1.21 (it comes from `SAM_OPT_GLOBAL_OPTIONS` in the
`getopt_long` string `"@:qd:c:H:r:5f:C:aAl:o:m:pt:X:"`, bam_consensus.c:2416,
:2462), so the command in 4.6 will not error. But the samtools 1.22 release
notes say consensus "now supports proper multi-threading. Previously this was
restricted to decompression only". Fix: keep `-@` but remove any implied speed
benefit, and add this to open decision 7 as a second argument for the bump
(alongside `-T` and the leading/trailing-N fix, both correctly attributed).

### F8. "The output has exactly L+F characters" is true only after unwrapping

samtools consensus writes line-wrapped FASTA (`-l/--line-len`, man page). Fix:
say the reader strips newlines (or passes `-l` with a large value); it matters
because every downstream step in 4.6 indexes by character position.

### F9. Two 4.7 rows present bowtie2 defaults as design choices

`--n-ceil L,0,0.15` (Maximum Ambiguity row) and `--score-min G,20,8` (Maximum
Mismatches row) are the bowtie2 defaults in local mode, confirmed in the
manual. `--seed 0` is also the default, so "fixed in the code" is a no-op
(bowtie2's determinism comes from seeding the per-read PRNG with read name,
sequence, qualities and `--seed`, which holds with `-p > 1`). Fix: mark these
three rows "bowtie2 default, stated for the record" so nobody thinks a
deliberate choice is being made.

### F10. Verified-good list (no change needed, recorded so it is not re-checked)

- samtools consensus 1.21 has `-a`, `-aa`, `-A/--ambig`, `-d/--min-depth`,
  `-m/--mode simple|bayesian`, `-c`, `-H`, `-q`, `-C`, `--min-MQ`, `--min-BQ`,
  `--show-del yes|no` (default no), `--show-ins yes|no` (default yes),
  `--mark-ins`, `--use-MQ/--no-use-MQ`, `-l`, `-o`, `-r`, `-f`, `--ff/--rf`
  (long-option table at bam_consensus.c:2415-2455; man page at tag 1.21).
- `-d` semantics as claimed: "Failing this depth check will produce consensus
  N ... this check is performed after filtering by flags and mapping/base
  quality", so `-d 3 --min-BQ 20` behaves as 4.8 describes.
- `-A` does work in Bayesian mode (`opts->ambig` is read in the gap5 path,
  bam_consensus.c:2237-2242), so the default option string is coherent.
- `-T` is absent from 1.21 and was added in 1.22 ("Add samtools consensus -T
  ref.fa"). The design correctly does not use it.
- Default excluded flags are "UNMAP,SECONDARY,QCFAIL,DUP", so leaving unmapped
  mates in `pass_1.bam` (needed for recruitment) does not pollute the pileup.
- bowtie2: `--very-sensitive-local` = `-D 20 -R 3 -N 0 -L 20 -i S,1,0.50`;
  `--sensitive-local` = `-D 15 -R 2 -N 0 -L 20 -i S,1,0.75`; `--fast-local` =
  `-D 10 -R 2 -N 0 -L 22 -i S,1,1.75`; `-N` is 0 or 1 only; `--rdg`/`--rfg`
  default 5,3; `-I 0 -X 500`; `--no-unal`, `--no-mixed`, `--no-discordant`,
  `--seed`, `-p` all exist as used. The Highest ladder entry
  (`-N 1 -L 15 --score-min G,10,6`) is legal; later flags override the preset,
  so the preset must come first in the string, which the examples do.
- `samtools view -G 12`: "Do not output alignments with all bits set in INT
  present in the FLAG field", so it drops only pairs with both mates unmapped.
  Correct for step 2.
- `samtools fastq` default excludes secondary and supplementary records, so the
  recruited subset cannot contain duplicate read names.
- bioconda `mapping-iterative-assembler` exists (anaconda.org API); samtools
  latest on bioconda is 1.24. No new binary is proposed, so nothing else on
  bioconda needs checking.
- Container lines are right: bam-readcount Dockerfile:30, bowtie2 :31,
  samtools :32, minimap2 :33.

---------------------------------------------------------------------------

## Priority 3: the Geneious mapping table is not fully honest (item e)

### F11. Rows labelled "exact" that are approximate

- "Allow Gaps (true) | always on, `--rdg 5,3 --rfg 5,3` | exact": the Geneious
  knob is a percent-of-read gap budget; ours is an affine score penalty inside
  `--score-min`. Approximate.
- "Only map paired reads which map nearby (false) | `--no-mixed
  --no-discordant` | exact": Geneious applies a soft distance penalty (advisor
  white paper 7.1, and the design's own "Paired distance" row says so and calls
  it approximate). Two rows describing the same mechanism cannot be both exact
  and approximate. Approximate.
- "Trim Before Mapping | Do not trim | fastp upstream | exact | exact": the
  fidelity cell is duplicated into the default column (formatting bug), and see
  F12.

### F12. The "Trim paired read overhangs" row misstates what fastp does

The shipped fastp string is
`--trim_poly_g --correction --detect_adapter_for_pe --dont_eval_duplication`
(R/init_db.R:248; preprocess.nf:30). `--correction` fixes mismatched bases in
the overlapped region of a pair; it does not trim overhangs.
`--detect_adapter_for_pe` removes adapter read-through, which is the nearest
analogue of Geneious's overhang trim. Fix the mechanism text, or mark the row
not replicable.

### F13. `--min-MQ` appears in two rows with contradictory advice

"Minimum mapping quality (off / 30)" says it must stay 0 for circular
references; "Ignore reads mapped to multiple locations (false)" offers
`--min-MQ 2`. Merge into one row and say the second is unsafe on a circular
reference for the same reason as the first (judge.md 1.3: reads inside the
duplicated block carry MAPQ 1).

---------------------------------------------------------------------------

## Priority 4: behaviour that is asserted but not specified

### F14. Circular topology is declarative, with no read evidence and no warning

4.6 step 9 stamps the header topology from the reference. Every consensus from
a circular `.gb` will be published as circular even when no read spans the
junction. Downstream that gates `rotate_asmb()` rotation to `start_gene`
(R/annotate.R:128, :380-392) and the `partial` flag seeded at
assemble_workflow.nf:418. ref-handling.md open question 5 raised exactly this
and the design does not answer it, while the repo already has
`count_junction_reads()` (R/circularize_asmb.R:522-599) reading spanning depth
from the same kind of construct this design builds.

Fix: add a `junction_depth=` key to `<ID>_summary.txt` from the last pass's BAM
(reads whose alignment crosses position L of the elongated reference) and a
`no_junction` warning when it is 0. Do not change the topology automatically;
say in 4.9 that topology is inherited and declarative, and that the number is
the user's check.

### F15. Extension past ends: specified for circular, thin for linear

4.9 is honest that extension is deferred, and open decision 5 covers the
strip-versus-keep choice, so this is not hand-waving. Two gaps remain:
(i) with a linear reference the loop can never grow past the reference extent,
so the `ref_divergent`/`incomplete` warnings are the only signal a user gets
that a linear reference truncated the result. Say so in 4.9 in one sentence.
(ii) the Westbury result in alt-tools (about 14,886 bp recovered against 16,740
expected on divergent baits) is the published magnitude of this effect and is
not cited anywhere in section 7. Add it to the "Divergent reference" risk row.

### F16. Deduplication is never mentioned

The design's own cited analogue (aITE, alt-tools 3) runs `rmdup` before calling
the consensus, and the shipped fastp string sets `--dont_eval_duplication`, so
nothing upstream removes PCR duplicates either. On a skim with a duplicated
mito fraction, duplicates inflate depth past `-d 3` and can lock an error in.
Fix: one sentence deciding it. Recommended: no dedup in v1 (mito depth in skims
is usually the limiting resource and dropping duplicates costs recall), stated
as a known limit, with `samtools markdup` named as the in-image escape hatch.

### F17. The stop rule dropped half of the recommended rule

judge.md section 6 recommends "mapped reads changed by < 0.1% AND consensus
changed by < 5 bases". The design (4.6 step 6, section 6) keeps only the second
term, yet `iterations.tsv` (4.11) declares a `reads_mapped` column with no
command to fill it. Fix: either adopt both terms (the read count is one
`samtools view -c -F 0x904` per pass, which is already needed for the column
and for the `few_reads` warning) or say in section 6 why the read-count term
was dropped.

### F18. Deletions can never converge, and the change counter cannot see them

4.6 step 5 fills both N and `*` from the previous mapping reference, so a real
deletion is re-inserted into every iteration reference and is only applied at
the final call. That is probably the right trade (it keeps coordinates fixed
and keeps the reference mappable), but it means `bases changed` is blind to
indels, so `stop_reason=converged` is compatible with an indel that never
settled. State this in step 5-6 in one sentence.

### F19. The snippy two-file output contract was researched and then dropped

alt-tools "NEW FINDING 5" recommends publishing both a full consensus and a
substitutions-only, reference-length FASTA, which answers "did we recover this
region or inherit the reference?". The loop already computes exactly that
string (the last `cons_k` before indels are applied). Fix: write it as
`maptoref/subs_only.fasta` and say what it is for. One extra write, no new
logic.

### F20. `appendTaggedNoteSql` is not a shared helper

The design cites `blast_ref_fetch_workflow.nf:17` as if it were importable.
Verified: it is a local Groovy `def` duplicated in
blast_genbank_workflow.nf:18, blast_ref_fetch_workflow.nf:17 and
blast_ref_align_workflow.nf:15, and assemble_workflow.nf has no copy. Fix: in
5.3/phase 2 say the def is copied (or `include`d) into assemble_workflow.nf,
and note that the tag-strip helpers only strip their own tag, so a `[maptoref]`
note survives the later `[blast]` and `[ref]` writes.

### F21. `.cadb_parse_gb` cannot be reused wholesale

4.5 says the parser "reuses the `.cadb_*` pieces". Verified:
`.cadb_parse_gb()` skips every record that lacks `/organelle="mitochondrion"`
(R/custom_assembly_db.R:490) and drops records with an empty ORIGIN, so a
perfectly good user `.gb` without that qualifier parses to zero records. Only
`.cadb_grab_version()` (:696) and `.cadb_grab_definition()` (:682) are reusable
as-is, plus the ORIGIN idiom at :520-524. Say that explicitly in 4.5 so nobody
wires in the wrong entry point, and add a unit test for an organelle-qualifier-
free single-record `.gb`.

### F22. Where the R function runs, and what the tarball glob catches

The proposed branch tars `*.fastq.gz` at the task root, copying MitoFinder
(assemble.nf:80). That is correct because PREPROCESS emits
`<ID>_preprocess_R1.fastq.gz` / `_R2` (preprocess.nf:26-27) and those are the
names coverage.nf:42 passes to `coverage()`. Two unstated requirements: (i)
`map_to_ref()` must keep its intermediates (bowtie2 index, `sub_R?.fq`,
`pass_*.bam`) out of that glob, so say it writes them under `maptoref/`; (ii)
the staged reads are symlinks and `tar` without `-h` stores symlinks
(codebase-map v1 gotcha 3), inherited from the MitoFinder branch, so it works,
but the design should not claim it verified the archive contents.

---------------------------------------------------------------------------

## Priority 5: touch-point list versus codebase-map section 9

The list in 5.1-5.3 is otherwise accurate. Re-verified against the working
tree: sqlRead currently selects 19 columns (indices 0-18: ID, assemble_opts,
cpus, memory, seeds_db, labels_db, getOrganelle, assembler, mitofinder_db,
mitofinder, genetic_code, max_paths, max_scaffolds, min_assembly_length,
run_blast, join_scaffolds, join_switch, assemble_switch, blast_accession), so
appended columns land at 19-22 exactly as stated; init_db.R:131-132 is the only
assembler validator; app_assemble_utils.R:293 is the only `choices` vector;
coverage.nf:40 is the only assembler branch downstream; contract items A-K hold.

Missing touch points (all named in codebase-map.md section 9 or
codebase-map.v1.md section 9, all verified present in the tree):

### F23. `R/app_assemble_utils.R:303-309`
The `opts_help()` sentence reads "Tool used to assemble the mitogenome from
reads: GetOrganelle or MitoFinder; the relevant tool options appear below." It
must name three tools. Not in the 5.1 table.

### F24. `README.md`
Lines ~27-31 ("GetOrganelle (default) or MitoFinder for mitogenome assembly")
and ~165-170 ("Assembly references for GetOrganelle or MitoFinder") both name
the assembler set. Section 5.7 lists NEWS and four vignettes but not README.

### F25. `vignettes/Difficult-Assemblies.Rmd:45`
"Multiple paths come only from GetOrganelle; MitoFinder always returns a single
path" needs MapToRef. 5.7 cites only :16-23.

### F26. `R/backwards_compatibility.R:11-15`
The roxygen list of migrated columns (codebase-map.v1 item 5). 5.1 cites
:176-178 and the block after :1313 only.

### F27. Tool help
codebase-map section 9 item 8 and v1 item 15 call for
`inst/tool_help/maptoref.txt` plus a `register_tool_help()` line at
`R/app_assemble.R:124-127` and a `tools/capture_tool_help.sh` entry. The design
uses `opts_help()` links instead. That is a legitimate choice given there is no
single wrapped CLI tool, but say it is a choice and not an oversight, otherwise
the "?" icon convention looks broken next to the other three tools.

### F28. Release chores
5.4 mentions the rebuild and the stale-tarball trap but not the `DESCRIPTION`
Version bump that the tarball glob depends on, nor `docker/README.md`.

### F29. Test fixture wording
5.8 says "the packaged Danio .gb". It is not packaged: it lives at
`ref_dbs/MitoFinder/NC_002333_Danio_rerio.gb` (55,541 bytes, `LOCUS
NC_002333 16596 bp DNA circular VRT`), and the last bullet of 5.8 correctly
says to copy it into `inst/test_data/`. Make the two bullets agree.

---------------------------------------------------------------------------

## Priority 6: open decisions (item f)

Real decisions, keep as they are: 1 (consensus engine), 2 (iteration cap
default), 3 (four columns versus a parsed string), 7 (samtools bump).

### F30. Decision 4 is deferred work, not a decision
"Re-map all reads every pass" is recommended for deferral to phase 2 in its own
text. Either move it to the deferred list, or, if F1 is fixed by option (b),
promote it to a real decision because it becomes the only way a user can escape
the pass-1 recruitment bound.

### F31. Decisions 5 and 6 are confirmations, not open questions
Both recommend exactly what sections 4.6 step 8 and 4.5 step 3 already
specify. Fine to keep, but label them "confirm" so the maintainer knows nothing
is blocked on them.

### F32. Two decisions are missing
Deduplication (F16) and junction evidence for the declared topology (F14) are
choices with visible consequences and no default in the document. Add them, or
resolve them inline.

---------------------------------------------------------------------------

## Priority 7: over-engineering and scope (ponytail pass)

### F33. `maptoref_consensus` is the weakest of the four columns
See F6. It is the only column whose contents can silently do nothing (`-c`
without `-m simple`) or silently break a documented invariant (`--min-MQ` on a
circular reference). Three columns plus a validated `-d`/`--min-BQ` pair is the
narrower design. If it stays, the validation in F6 is the minimum price.

### F34. Diagnostic artifacts should be declared deliberate
`maptoref/` keeps `cons_1.fa .. cons_N.fa` plus `iterations.tsv` alongside
`<ID>_summary.txt`, `opts.txt`, and `assembler.log.txt`. At about 16 kb per
consensus this is harmless, but say it is for debugging so a later reader does
not "clean it up" and remove the only record of the loop.

### F35. Nothing else looks over-built
No new process, no config change, no new tables, no parser, no upload widget,
no new binaries. The deferred list in section 9 is well triaged and each entry
has a trigger, which is the right shape.

---------------------------------------------------------------------------

## Priority 8: research the design ignored (item g)

Already covered above: the snippy substitutions-only output (F19), the aITE
read-count convergence term (F17), deduplication (F16), the Westbury
under-recovery number (F15), the topology-evidence question from ref-handling
(F14). One more:

### F36. The Culicoides reference-swap result is cited but not acted on
2.5 reports that swapping the reference changed the Geneious consensus. The
repo now has multi-candidate BLAST reference machinery (memory:
"multi BLAST ref candidates"), and section 9's deferred list mentions
"Multiple references per sample" only in passing. Worth one line in section 7's
"Reference bias" row: the cheapest user-facing mitigation available today is to
run the same samples under two parameter sets with two references and compare,
which needs no code at all.

---------------------------------------------------------------------------

## Priority 9: house style (item h)

### F37. ASCII and dashes: clean
`grep -P '[^\x00-\x7F]'` over final-design.md returns nothing, and there are no
em or en dashes. No action.

### F38. Spelling register
"synthesises", "manoeuvre" are en-GB; the repo and NEWS are en-US. Minor, but
worth a pass since parts of this text will end up in vignettes.

### F39. Jargon that will not land
"seam" is house vocabulary (codebase-map uses it), so keep it. "Tripwire",
"recruit-then-iterate", "burn the cap", and "the loop, not the mapper, closes
the gap" are fine inside a planning doc but must not survive into 4.11's
warning strings or the vignettes. The warning texts themselves are already
plain English; keep them that way.

### F40. Section 1 undercounts its own inputs
It says "the four research reports" and section 10 lists five files
(codebase-map.md and codebase-map.v1.md are both cited). Say four lanes, five
files, and state the rule the context file gives: where the two codebase-map
versions disagree, the repo wins.

---------------------------------------------------------------------------

## Suggested order of work

1. F1, F2, F3 (wrong output if unfixed), then F5, F4, F6.
2. F14, F16, F17, F18, F19 (unspecified behaviour that the phase-1 spike should
   settle on real data).
3. F7 through F13 (documentation honesty; no code impact).
4. F20 through F29 (touch-point completeness; each is one or two lines).
5. F30 through F40 (decisions, scope, style).
