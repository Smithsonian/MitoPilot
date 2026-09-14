# MapToRef reference decoupled from the parameter set

Branch: `feat/maptoref-ref-per-sample` (off `map-to-ref-assembly`).

## Goal

One MapToRef parameter set (mapper, mapper options, consensus options, iterations) can serve many samples, and every sample carries its own reference and, for a FASTA reference, its own topology. Today a mapping-file `Reference` clones a `<ID>_maptoref` parameter set per sample and stores the reference and topology on that set. This gives each of those two values a single home: the sample row.

## Decisions

### D1. `assemble.maptoref_ref` and `assemble.maptoref_topology` are the only homes

- New nullable column `assemble.maptoref_topology TEXT`, added to both `assemble` DDLs (`R/init_db.R`, `R/init_db_userAsmb.R`) directly after `maptoref_ref TEXT,`, and by `backwards_compatibility()` next to the `maptoref_ref` ALTER.
- Pipeline reads `a.maptoref_ref` and `a.maptoref_topology` only. In `inst/nextflow/modules/assemble_workflow.nf` the `COALESCE(a.maptoref_ref, opts.maptoref_ref)` expression becomes `NULLIF(TRIM(a.maptoref_ref), '')` at the same position, and `opts.maptoref_topology` becomes `a.maptoref_topology` at the same position. No tuple index moves. `assemble.nf` and `map_to_ref()` are unchanged: both already receive a topology string.
- `assemble_opts.maptoref_ref` and `assemble_opts.maptoref_topology` stay in both DDLs and in the migrations that add them, but nothing reads or writes them after migration.
- `new_db()` / `new_project()` lose the `maptoref_ref` and `maptoref_topology` arguments. Mapper, mapper options, consensus options, and iterations stay on the set.
- The Assemble options modal loses the "MapToRef reference" and "Reference topology" inputs and their help lines.

### D2. One validator, every entry point

`.mtr_validate_refs(x, ids, context)` keeps its current checks (shell metacharacters; accession existence at NCBI via esummary; file exists, is not a directory, is readable; file or URL content parses as exactly one GenBank or FASTA record with valid bases and mitogenome length; ftp refused) and its return (normalised values, NA for blank).

New `.mtr_validate_ref_topology(vals, topology, ids, context)`:
- `vals` are the values `.mtr_validate_refs()` returned; `topology` is the same length (NA or blank means unset).
- A non-blank topology must be `circular` or `linear` (case-insensitive, stored lowercase); anything else is an error naming the sample.
- A value for which `.mtr_needs_topology()` is TRUE (a file or URL not ending `.gb`, `.gbk`, or `.gbff`) with a blank topology is an error naming the sample: "FASTA reference needs a topology (circular or linear)".
- A blank value clears the topology too (returns NA), so a sample never keeps a stale topology.
- Errors are collected and reported together, in the same `ids [value]: reason` shape as `.mtr_validate_refs()`.

Every writer calls both, in that order, before any write: `new_db()`, `add_samples()`, `set_maptoref_refs()`, and the app modal.

### D3. Ingest writes the columns directly

- Mapping CSV: optional `Reference` column (as today) and optional `Reference_topology` column. Both are stripped by `.mtr_take_ref_col()` before the `samples` table is built, so neither becomes a `samples` column. `Reference_topology` without `Reference` is an error. Plain `Topology` keeps its existing meaning (declared sample topology, `R/sample_topology.R`).
- `new_db()` and `add_samples()` validate, then write `maptoref_ref` and `maptoref_topology` in the same `rows_insert` / `rows_upsert` that creates the row. No parameter set is created; samples stay on `default`.
- If the `default` set's assembler is not `MapToRef` and at least one `Reference` was given, warn once: the references are stored and ignored until those samples are put on a MapToRef set.
- Delete `.mtr_seed_per_sample_opts()`, `.mtr_dedicated_opts()`, `.mtr_route_refs()`, `.mtr_opts_name()`, `.mtr_fold_override_column()`.

### D4. `set_maptoref_refs()` is a plain column update

Input CSV or data frame: column 1 sample ID, column 2 reference, optional column 3 topology. Keeps duplicate-ID and unknown-ID refusal, validation before any write, unchanged-row skip (a row changes if its reference or its topology changes), locked-row refusal for rows that would change, `assemble_switch = 1` on changed rows, and the trailing `.mtr_warn_missing_refs()`. Writes both columns. Loses all set routing.

### D5. App: the reference cell is the entry point

- `fetch_assemble_data()` selects `assemble.maptoref_ref` and `assemble.maptoref_topology` and builds one display column: for a sample whose set has `assembler == "MapToRef"`, the reference, followed by ` (circular)` or ` (linear)` when a topology is stored, or the placeholder `set reference` when no reference is set; for any other sample, `NA`. `rt_link()` renders NA as plain empty text, so only MapToRef samples get a link.
- New `colDef` `maptoref_ref` in the Assemble table, `Options` column group, `cell = rt_link(ns("set_maptoref_ref"), title = "Set MapToRef reference", lock_col = "assemble_lock")`, extra class `mp-col-maptoref`.
- `output$col_css` adds one rule hiding `.mp-col-maptoref` when no row in `rv$data` has `assembler == "MapToRef"`.
- Clicking the link opens a modal for that one sample: title with the sample ID, a text input prefilled with the current reference, a `circular` / `linear` select prefilled with the current topology and shown only while the typed value is FASTA-class (`.mtr_needs_topology()` on the input, via `shinyjs::toggle` in an observer), help text, Cancel and Save. Save runs `.mtr_validate_refs()` then `.mtr_validate_ref_topology()`; an error shows in `mp_alert()` and nothing is written. Success writes `maptoref_ref`, `maptoref_topology`, `assemble_switch = 1`, then refreshes the table. Locked rows never reach the modal.
- The coverage viewer keeps calling `.mtr_ref_now()`, which reads the sample column only.

### D6. Migration in `backwards_compatibility()`

Replaces the fold-into-set step at the call site of `.mtr_fold_override_column()`:

1. Add `assemble.maptoref_topology` if missing.
2. For every sample where `assemble.maptoref_ref` is NULL or blank and its set has a non-blank `maptoref_ref`, copy the set reference onto the sample.
3. For every sample where `assemble.maptoref_topology` is NULL or blank and its set has a non-blank `maptoref_topology`, copy the set topology onto the sample.
4. NULL `maptoref_ref` and `maptoref_topology` on every set.
5. Message the counts copied.

Idempotent because step 4 empties the source. `<ID>_maptoref` sets are left in place as ordinary sets. The "already current" predicate gains `"maptoref_topology" %in% names(assemble_table)`.

### D7. `map_to_ref()` untouched

Only the "no reference" error message text changes (drops "or in the Assemble options").

## Out of scope

- Bulk reference editing in the app; `set_maptoref_refs()` covers it.
- Dropping `assemble_opts.maptoref_ref` / `maptoref_topology` from the DDL.
- Hard-failing when NCBI is unreachable; that stays a warning, and the accession resolves in the pipeline.

## Files

- `R/map_to_ref_refs.R`: delete routing helpers; add `.mtr_validate_ref_topology()`, `.mtr_copy_set_refs_down()`, `.mtr_warn_refs_ignored()`; simplify `set_maptoref_refs()`, `.mtr_ref_now()`, `.mtr_warn_missing_refs()`, `.mtr_take_ref_col()`.
- `R/init_db.R`, `R/init_db_userAsmb.R`, `R/init_project.R`: drop two arguments, new DDL column, write both columns at insert.
- `R/add_samples.R`: write both columns at insert, drop set seeding.
- `R/backwards_compatibility.R`: ALTER for the new column, copy-down migration, predicate.
- `R/app_assemble_utils.R`: remove two modal inputs; add columns to `fetch_assemble_data()`; add `maptoref_ref_modal()`.
- `R/app_assemble.R`: remove modal wiring for the two inputs; add column, CSS rule, click handler, topology toggle, save handler.
- `R/utils_maptoref_display.R`: `mtr_display_ref()`.
- `R/constants.R`: column name and tip.
- `R/map_to_ref.R`: error message text.
- `inst/nextflow/modules/assemble_workflow.nf`: two select fragments.
- `man/*.Rd`, `README.md`, `NEWS.md`.
- `tests/testthat/test-map-to-ref-refs.R`, `tests/testthat/test-map-to-ref.R`, `tests/testthat/test-backwards-compatibility.R`, new `tests/testthat/test-maptoref-display.R`.

## Testing

`Rscript -e 'devtools::test()'` from the repo root. Baseline on `map-to-ref-assembly` recorded before the first edit. Nextflow change verified by reading the select string; no pipeline run.
