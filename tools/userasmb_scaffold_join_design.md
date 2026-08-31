# Scaffold join for the user-assembly workflow

Date: 2026-08-27
Branch: userasmb-find-mito

## Problem

`WF1_userAsmb` never runs the scaffold join. `inst/nextflow/main.nf:84` wires
`SCAFFOLD_JOIN` into the regular `WF1` only; the user-assembly workflow stops
after the reference fetch and says so in a comment.

The consequence is a real gap, not a cosmetic one. `select_mito_contigs()`
deliberately keeps a mitogenome that is broken across several contigs when they
all match the same reference. On the regular path those pieces go to
`SCAFFOLD_JOIN` and are ordered against the reference into one Path 0. On the
user-assembly path they stay as scaffolds 1..N of one sample, are annotated
separately, and there is no way to join them from the pipeline.

The manual join editor is already available on this path: `app_assemble_userAsmb.R:1144`
calls the shared `assembly_coverage_details_server()`, which carries the layout
editor. So the editor exists with no pipeline behind it, and there is no way to
queue a join redo.

## What is already in place

Most of what the join needs is present on the user-assembly path:

- Per-contig coverage statistics, written by `COVERAGE_userAsmb`.
- Per-scaffold BLAST hits and a fetched reference, from `BLAST_GENBANK` and
  `BLAST_REF_FETCH`.
- The `scaffold_mappings` table (`init_db_userAsmb.R:595`).
- `join_notes` and `join_switch` columns on `assemble` (`init_db_userAsmb.R:312-313`),
  created and never written.
- The manual layout editor in the app.

## What is missing

| Piece | Regular pipeline | User-assembly path |
|---|---|---|
| `join_scaffolds` toggle on `assemble_opts` | `init_db.R:326` | absent |
| Auto-join wired into the workflow | `main.nf:84` | absent |
| `join_eligible` / `join_expected` / `join_redo` channels | `assemble_workflow.nf:425-462` | absent |
| "Redo scaffold join" action (`join_switch = 1`) | `app_assemble.R:661-766` | absent |
| WF1 launch count includes queued redos | `app_run_pipline.R:95-99` | absent |
| `join_notes` shown in the Assemble table | yes | absent |

## Design decisions

### Eligibility: the plain rule

A sample is join-eligible when it has one path and more than one scaffold, the
same rule the regular pipeline uses (`assemble_workflow.nf:425-430`,
`scaffold_join_eligible()` at `R/scaffold_join.R:28`).

No user-assembly-specific contamination gate is added. `run_scaffold_join()`
already calls `scaffold_hits_disagree()` (`R/scaffold_join.R:1573`): when the
scaffolds' per-scaffold BLAST accessions do not all agree, it writes outcome
`declined` with a note naming the competing accessions, builds no Path 0, and
ignores nothing. Contaminated samples such as `UA_MULTI_TWO` and
`UA_MULTI_MIXED`, whose contigs carry different accessions, take that branch and
keep both mitogenomes as independent units.

### Excluded contigs: reuse the existing SQL

`sqlIgnoreOriginals` is used unchanged, so every original scaffold is ignored
once a Path 0 is built. The case this could have harmed, a confirmed second
mitogenome being ignored after being excluded from Path 0, is caught upstream by
the disagree gate. What remains is a contig that shares the winning accession
but maps below the `min_qcov` floor of 0.5, which is junk or a duplicate;
ignoring it matches the regular pipeline.

### `join_scaffolds` defaults off

New user-assembly projects get `join_scaffolds = 0`, the same default as the
regular pipeline. Existing projects already receive the column from the shared
migration at `backwards_compatibility.R:426-430`, also defaulting to 0, so
nothing changes for a project created before this work.

A user with a fragmented assembly and the toggle off is not stranded: the manual
editor is reachable, and the redo action added below queues a join on demand.

`new_db_userAsmb()` and `new_project_userAsmb()` gain a `join_scaffolds`
argument so a project can be created with it on.
`new_test_project_userAsmb()` passes `TRUE`, matching how it already passes
`find_mitogenome = TRUE` and `attempt_circularization = TRUE`.

## Changes

### 1. Schema

`R/init_db_userAsmb.R`

- Add `join_scaffolds INTEGER` to the `assemble_opts` table definition.
- Seed the `"user"` options row from a new `join_scaffolds` argument to
  `new_db_userAsmb()` (default `FALSE` -> `0`).

`R/init_project_userAsmb.R`

- Add a `join_scaffolds` argument to `new_project_userAsmb()`, default `FALSE`,
  passed through to `new_db_userAsmb()`.

No `backwards_compatibility.R` change: the existing migration already covers
`assemble_opts.join_scaffolds` for every project type, and the consistency check
at line 2123 already requires the `assemble.join_notes` / `join_switch` columns.

### 2. Channels

`inst/nextflow/modules/coverage_userAsmb_workflow.nf`

The shared writer already computes `max_paths` and `max_scaffolds` per sample
for its `assemble` update. Three emits are added alongside the existing
`blast_in`, mirroring `assemble_workflow.nf:432-462`:

- `cov_files` — `tuple(id, [coverageStats csvs])`.
- `join_eligible` — `tuple(id, assembly_fasta, opts_id, join_scaffolds)`, filtered
  to `status in ('4','2') && max_paths == 1 && max_scaffolds > 1`, joined to a
  `join_scaffolds` lookup read from `assemble_opts`.
- `join_expected` — the eligible IDs still at status 4 whose blast options have
  `run_blast = 1`, so `BLAST_REF_FETCH` withholds them from its 4 -> 2 promotion
  and `SCAFFOLD_JOIN` owns their final state. Samples with BLAST switched off are
  excluded: they never get a fetched reference, so withholding them would strand
  them at status 4 and reporting them would call a missing input a failure. The
  regular pipeline gets this for free from its status 2 distinction, which does
  not exist at the same point on this path, so `blast_opts.run_blast` is read
  into the sample query instead.
- `join_redo` — `tuple(id, assemble_opts, join_scaffolds, blast_accession)` from a
  `fromQuery` on `assemble` where `join_switch = 1`, excluding samples at
  `assemble_switch` 1 or 4 (those are being reprocessed by this same run and
  reach the join by the normal route) and clearing `join_switch` for the ones it
  declines to service, exactly as `assemble_workflow.nf:147-168` does.

Both `COVERAGE_userAsmb` and `COVERAGE_userAsmb_noReads` emit the same set, so
`main.nf` does not care which path produced them.

### 3. Wiring

`inst/nextflow/main.nf`, in `WF1_userAsmb`:

- Pass `join_expected` to `BLAST_REF_FETCH` instead of `channel.empty()`.
- Build `join_rows` by joining `join_eligible` against `cov_files`,
  `BLAST_REF_FETCH.out.ref_seq` and `BLAST_GENBANK.out.scaffold_hits` with
  `remainder: true`, branching complete from incomplete, and filtering
  incomplete rows against `join_expected` before reporting them as dropped.
- Call `SCAFFOLD_JOIN(join_rows.complete, join_dropped, join_redo)`.

This is the same block as `main.nf:41-84`, with the user-assembly channel names
substituted. `SCAFFOLD_JOIN` itself needs no change.

### 4. App plumbing

`R/app_assemble_userAsmb.R`

- Add the "Redo scaffold join" action, ported from `app_assemble.R:661-766`. It
  sets `join_switch = 1` for eligible unlocked samples and reports the ones it
  refused, reusing the existing `redo_join_eligible_ids()` and the request
  classifier in `R/scaffold_join.R`.
- Add `join_notes` to the metadata column group (line 11-12) and give it a
  `colDef`, so declines and failures are visible in the Assemble table.

`R/app_run_pipline_userAsmb.R`

- Include queued redos in the Assemble launch count: change the filter at line
  55 from `assemble_switch == 1` to also admit
  `join_switch == 1 & assemble_lock == 0`, matching `app_run_pipline.R:95-99`.

### 5. Test sample

A ninth sample, `UA_MULTI_FRAG`, covering a single mitogenome split across
three contigs.

- Donor reads: `SRR22396843` (Conger oceanicus), the same donor as
  `UA_MULTI_ONE`, so no new read data is fetched.
- Assembly: the 100 nuclear decoy contigs from `UA_MULTI_ONE.fasta`, plus the
  Conger mitogenome cut into three adjacent pieces of roughly 6 kb named
  `mito_contig_1`, `mito_contig_2`, `mito_contig_3`.
- The three pieces are written out of order and one of them is
  reverse-complemented. A clean in-order split would pass even if the join did
  nothing but concatenate; out of order with one flipped forces the join to use
  the reference to order and orient.
- `inst/test_data/mapping_test_userAsmb.csv` gains the row, with `Expected`
  reading "multi-contig assembly with one mitogenome split across 3 contigs ->
  found, then joined".
- `R/init_test_project_userAsmb.R` passes `join_scaffolds = TRUE` to
  `new_project_userAsmb()`, so the shipped test project auto-joins and the new
  sample demonstrates the feature without hand-editing the database. Its roxygen
  header is updated from eight samples to nine.

Expected pipeline behaviour: the search keeps all three contigs against one
accession; coverage records one path with three scaffolds; the join orders and
orients them into a Path 0 whose length is close to the original mitogenome;
annotation sees one unit.

## Testing

Unit tests, run without Nextflow or Docker:

- `select_mito_contigs()` keeps three same-reference fragments as candidates
  (extends the existing split-mitogenome test to the fixture's shape).
- `scaffold_join_eligible()` returns `TRUE` for a 1-path 3-scaffold sample and
  `FALSE` for the contaminated shape.
- `scaffold_hits_disagree()` returns `TRUE` for the two-accession case, so a
  contaminated sample declines rather than joining. This is the guard the
  eligibility decision rests on and it is currently untested for this shape.
- `new_db_userAsmb()` creates `assemble_opts.join_scaffolds` and honours the new
  argument.
- `new_test_project_userAsmb()` fixture checks extend to the ninth sample: the
  assembly file ships, the donor resolves, and the three mito contigs are
  present, out of order, with one reverse-complemented relative to the others.
- The backwards-compatibility consistency check still passes on a database
  created by the new `init_db_userAsmb()`.

End-to-end verification is a manual run of the test project, since it needs
Docker, BLAST and minimap2:

- `UA_MULTI_FRAG` reaches annotation as one joined unit.
- `UA_MULTI_TWO` and `UA_MULTI_MIXED` decline with the different-references note
  and keep two units each.
- `UA_MULTI_ONE`, `UA_LINEAR`, `UA_CIRCULAR`, `UA_UNCIRC`, `UA_MULTI_UNCIRC` and
  `UA_MULTI_NONE` are unchanged from their current behaviour.

## Out of scope

- Any change to `SCAFFOLD_JOIN`, `scaffold_join.nf` or `R/scaffold_join.R`
  beyond what the tests above read.
- Any change to the regular `WF1`.
- The manual layout editor, which already works on this path.
