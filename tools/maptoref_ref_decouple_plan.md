# MapToRef Reference Decouple Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** A MapToRef reference and its FASTA topology live only on the sample row (`assemble.maptoref_ref`, `assemble.maptoref_topology`), so one parameter set serves many samples that each map to a different reference.

**Architecture:** Delete the per-sample parameter-set cloning added in commits cbbe9e8, 9a2f960, and 5048cee. Ingest, `set_maptoref_refs()`, and a new Assemble-table modal all validate through the same two helpers and write the two sample columns; the pipeline, coverage viewer, and warning helper read the sample columns only; a one-time migration copies any set-level reference and topology down onto samples. The Assemble table gains a `maptoref_ref` column that is the app's entry point, shown only when some sample is on a MapToRef set and clickable only on MapToRef samples.

**Tech Stack:** R (DBI, RSQLite, dplyr, shiny, shinyjs, reactable, testthat, withr), Nextflow DSL2, SQLite.

**Spec:** `tools/maptoref_ref_decouple_spec.md`

## Global Constraints

- Branch `feat/maptoref-ref-per-sample`, off `map-to-ref-assembly`. Never push. Commit only if the maintainer asks; no task has a commit step.
- No Claude attribution anywhere.
- ASCII only in every touched file. Check: `grep -nP '[^\x00-\x7F]' <file>` (expected: no output). No em dashes.
- Minimal comments. Comment the why, never narrate the fix.
- Ponytail: smallest correct diff. Reuse `.mtr_validate_refs()`, `.mtr_needs_topology()`, `.mtr_ref_key()`, `.mtr_take_ref_col()`, `rt_link()`, `mp_alert()`, `mp_modal_title()`, `mp_footer()`, `opts_help()`, `%||%`, `%|NA|%`, `%nin%`.
- Nextflow tuple positions do not move. `it[19]` stays the raw reference string; `it[23]` stays the topology string.
- `assemble_opts.maptoref_ref` and `assemble_opts.maptoref_topology` stay in both DDLs and in the migrations that add them; nothing reads or writes them after Task 4's migration.
- Run tests with `Rscript -e 'devtools::test()'` from the repo root. One file: `Rscript -e 'devtools::test(filter = "map-to-ref-refs")'`. Docs: `Rscript -e 'devtools::document()'`.
- Anchor every edit on the quoted code text, never on a line number.

---

### Task 0: Baseline

- [ ] **Step 1: Run the full suite and record the numbers**

Run: `Rscript -e 'devtools::test()' 2>&1 | tail -5`
Expected: a `[ FAIL 0 | WARN 0 | SKIP n | PASS m ]` line. Write it under this task. Skips are missing external binaries.

---

### Task 1: Schema column, readers, and the topology validator

**Files:**
- Modify: `R/init_db.R`, `R/init_db_userAsmb.R` (assemble DDL), `R/backwards_compatibility.R` (ALTER and predicate)
- Modify: `inst/nextflow/modules/assemble_workflow.nf` (two select fragments)
- Modify: `R/map_to_ref_refs.R` (`.mtr_ref_now`, `.mtr_warn_missing_refs`, new `.mtr_validate_ref_topology`)
- Modify: `R/map_to_ref.R` (stop message)
- Test: `tests/testthat/test-map-to-ref-refs.R`

**Interfaces:**
- Produces: `.mtr_ref_now(con, id)` returns `assemble.maptoref_ref` or `NA_character_`. `.mtr_warn_missing_refs(con)` warns for MapToRef samples with a blank column. `.mtr_validate_ref_topology(vals, topology, ids, context = "reference")` returns a character vector of lowercase `circular`/`linear`/`NA`, or stops listing every bad row.

- [ ] **Step 1: Write the validator tests**

Append to `tests/testthat/test-map-to-ref-refs.R` after the `.mtr_validate_refs` tests:

```r
test_that(".mtr_validate_ref_topology demands one for a FASTA and normalises case", {
  d <- withr::local_tempdir()
  fa <- mtr_ref_fasta(d)
  expect_equal(
    .mtr_validate_ref_topology(c(fa, "NC_002333", NA), c("Circular", "", NA),
                           ids = c("S1", "S2", "S3")),
    c("circular", NA, NA)
  )
  expect_error(
    .mtr_validate_ref_topology(fa, "", ids = "S1"),
    "S1.*FASTA reference needs a topology"
  )
  expect_error(
    .mtr_validate_ref_topology(c(fa, fa), c("round", ""), ids = c("S1", "S2")),
    "S1.*circular or linear.*S2.*needs a topology"
  )
  # A GenBank name never needs one; a blank reference drops a stale topology.
  expect_equal(.mtr_validate_ref_topology("/x/ref.gb", "", ids = "S1"), NA_character_)
  expect_equal(.mtr_validate_ref_topology(NA, "linear", ids = "S1"), NA_character_)
})
```

- [ ] **Step 2: Rewrite the `.mtr_ref_now` and fallback tests**

Replace `".mtr_ref_now reads the set, and a leftover column value over it"` with:

```r
test_that(".mtr_ref_now reads the sample column only", {
  d <- withr::local_tempdir()
  fa <- mtr_ref_fasta(d)
  db <- mtr_refs_project(d)
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_true(is.na(.mtr_ref_now(con, "S1")))
  DBI::dbExecute(con, "UPDATE assemble_opts SET maptoref_ref = 'NC_002333'")
  expect_true(is.na(.mtr_ref_now(con, "S1")))
  DBI::dbExecute(con, "UPDATE assemble SET maptoref_ref = ? WHERE ID = 'S1'",
                 params = list(fa))
  expect_equal(.mtr_ref_now(con, "S1"), fa)
  expect_true(is.na(.mtr_ref_now(con, "S2")))
})
```

Replace `"the option-set reference covers samples that have none of their own"` with:

```r
test_that("a set-level reference no longer covers a sample", {
  d <- withr::local_tempdir()
  db <- mtr_refs_project(d)
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "UPDATE assemble_opts SET maptoref_ref = 'NC_002333', assembler = 'MapToRef'")
  expect_warning(ids <- .mtr_warn_missing_refs(con), "2 sample")
  expect_setequal(ids, c("S1", "S2"))
})
```

`mtr_refs_project()` passes `maptoref_topology` to `new_db()`; that argument goes away in Task 2, so these tests fail until then. That is expected.

- [ ] **Step 3: Add `.mtr_validate_ref_topology` to `R/map_to_ref_refs.R`**

Place it directly after `.mtr_validate_refs`:

```r
# Second pass over what .mtr_validate_refs() returned. Only a FASTA (a file or
# URL not named .gb/.gbk/.gbff) needs a topology; GenBank and accessions carry
# their own. Same report shape as .mtr_validate_refs().
#' @noRd
.mtr_validate_ref_topology <- function(vals, topology, ids, context = "reference") {
  n <- length(vals)
  topology <- tolower(trimws(as.character(topology)))
  topology[is.na(topology)] <- ""
  if (length(topology) != n) stop("topology must be the same length as vals", call. = FALSE)
  ids <- as.character(ids)
  out <- rep(NA_character_, n)
  bad <- character(0)
  for (i in seq_len(n)) {
    if (is.na(vals[i])) next
    needs <- .mtr_needs_topology(vals[i])
    if (nzchar(topology[i]) && topology[i] %nin% c("circular", "linear")) {
      bad <- c(bad, sprintf("  %s [%s]: topology must be circular or linear, not %s",
                            ids[i], vals[i], topology[i]))
    } else if (needs && !nzchar(topology[i])) {
      bad <- c(bad, sprintf("  %s [%s]: FASTA reference needs a topology (circular or linear)",
                            ids[i], vals[i]))
    } else if (needs) {
      out[i] <- topology[i]
    }
  }
  if (length(bad) > 0L) {
    stop(sprintf("MapToRef reference topology problems (%d) in %s:\n%s",
                 length(bad), context, paste(bad, collapse = "\n")),
         call. = FALSE)
  }
  out
}
```

Note the `else if (needs)` branch: a topology typed for a GenBank or accession reference is dropped, since the record's own topology wins in `maptoref_prepare_ref()`.

- [ ] **Step 4: Edit `.mtr_ref_now`**

```r
.mtr_ref_now <- function(con, id) {
  if ("maptoref_ref" %nin% DBI::dbListFields(con, "assemble")) {
    return(NA_character_)
  }
  v <- DBI::dbGetQuery(
    con, "SELECT NULLIF(TRIM(maptoref_ref), '') AS ref FROM assemble WHERE ID = ?",
    params = list(id)
  )$ref
  if (length(v) != 1L || is.na(v)) NA_character_ else v
}
```

- [ ] **Step 5: Edit `.mtr_warn_missing_refs`**

Replace the `ids <- DBI::dbGetQuery(con, paste(` block with:

```r
  ids <- DBI::dbGetQuery(con, paste(
    "SELECT a.ID FROM assemble a",
    "JOIN assemble_opts o ON a.assemble_opts = o.assemble_opts",
    "WHERE o.assembler = 'MapToRef'",
    "AND NULLIF(TRIM(a.maptoref_ref), '') IS NULL"
  ))$ID
```

Change the warning tail `"sample with MitoPilot::set_maptoref_refs(), or set one for the ", "parameter set in the Assemble options."` to `"sample with MitoPilot::set_maptoref_refs() or by clicking the sample's ", "MapToRef ref cell in the Assemble table."`.

- [ ] **Step 6: DDL and migration**

In `R/init_db.R` and `R/init_db_userAsmb.R`, in the `CREATE TABLE assemble` string, directly after `      maptoref_ref TEXT,` add `      maptoref_topology TEXT,`.

In `R/init_db.R`, in the assemble `rows_upsert` mutate, directly after `maptoref_ref = NA_character_,` add `maptoref_topology = NA_character_,` (Task 2 replaces both with real values).

In `R/backwards_compatibility.R`, directly after the block

```r
  if (!("maptoref_ref" %in% DBI::dbListFields(con, "assemble"))) {
    message("added 'maptoref_ref' column to assemble table")
    DBI::dbExecute(con, "ALTER TABLE assemble ADD COLUMN maptoref_ref TEXT")
  }
```

add:

```r
  if (!("maptoref_topology" %in% DBI::dbListFields(con, "assemble"))) {
    message("added 'maptoref_topology' column to assemble table")
    DBI::dbExecute(con, "ALTER TABLE assemble ADD COLUMN maptoref_topology TEXT")
  }
```

In the "already current" predicate, directly after `"maptoref_ref" %in% names(assemble_table) &&` add `"maptoref_topology" %in% names(assemble_table) &&`. In the gap-report block, change the line `gaps <- c(gaps, "the assemble table lacks the per-sample MapToRef reference column")` and its guard to:

```r
  if (!has(all(c("maptoref_ref", "maptoref_topology") %in% DBI::dbListFields(con, "assemble")))) {
    gaps <- c(gaps, "the assemble table lacks the per-sample MapToRef reference columns")
  }
```

In `tests/testthat/test-backwards-compatibility.R`, find every assertion that `"maptoref_ref" %in% DBI::dbListFields(con, "assemble")` is TRUE after migration and add the same assertion for `"maptoref_topology"` beside it.

- [ ] **Step 7: Nextflow select**

In `inst/nextflow/modules/assemble_workflow.nf`, replace the comment block above the COALESCE and the two COALESCE lines:

```groovy
                  "COALESCE(NULLIF(TRIM(a.maptoref_ref), ''), " +
                  "NULLIF(TRIM(opts.maptoref_ref), '')), " +
```

with:

```groovy
                  // Per-sample reference; a value on the set is not read.
                  "NULLIF(TRIM(a.maptoref_ref), ''), " +
```

and in the line `'opts.maptoref_iter, opts.maptoref_topology, opts.maptoref_mapper ' +` change `opts.maptoref_topology` to `a.maptoref_topology`. Nothing else in the select moves. Update the comment on `maptoref_topology: (it[23] ?: "")` to `// MapToRef reference topology (per sample)`.

- [ ] **Step 8: `map_to_ref()` message**

In `R/map_to_ref.R` change `"MitoPilot::set_maptoref_refs(), or in the Assemble options")` to `"MitoPilot::set_maptoref_refs(), or in the Assemble table")`.

- [ ] **Step 9: Run the validator test**

Run: `Rscript -e 'devtools::test(filter = "map-to-ref-refs")' 2>&1 | grep -E "demands one for a FASTA"`
Expected: no failure line for that test. Other failures in the file are expected until Task 3.

- [ ] **Step 10: ASCII check**

Run: `grep -nP '[^\x00-\x7F]' R/map_to_ref_refs.R R/init_db.R R/init_db_userAsmb.R R/backwards_compatibility.R R/map_to_ref.R inst/nextflow/modules/assemble_workflow.nf`
Expected: no output.

---

### Task 2: Ingest writes the columns

**Files:**
- Modify: `R/init_db.R` (roxygen, signature, validation, `checked` block, assemble insert, `assemble_opts` insert, seed call)
- Modify: `R/init_project.R` (roxygen)
- Modify: `R/add_samples.R` (roxygen, insert, seed call)
- Modify: `R/map_to_ref_refs.R` (`.mtr_take_ref_col`; delete `.mtr_opts_name`, `.mtr_seed_per_sample_opts`; add `.mtr_warn_refs_ignored`)
- Test: `tests/testthat/test-map-to-ref-refs.R`, `tests/testthat/test-map-to-ref.R`

**Interfaces:**
- Consumes: `.mtr_validate_refs`, `.mtr_validate_ref_topology` (Task 1).
- Produces: `.mtr_take_ref_col(mapping, mapping_id)` returns `list(mapping, refs, topology)`, `refs` and `topology` named character vectors or NULL. `new_db()` without `maptoref_ref` or `maptoref_topology`. `.mtr_warn_refs_ignored(con, ids)`.

- [ ] **Step 1: Fixture and `new_db` tests in `test-map-to-ref-refs.R`**

Change `mtr_refs_mapping` to accept a topology column:

```r
mtr_refs_mapping <- function(dir, refs = NULL, topology = NULL, ids = c("S1", "S2")) {
  m <- data.frame(
    ID = ids,
    Taxon = "Danio rerio",
    R1 = paste0(ids, "_R1.fastq.gz"),
    R2 = paste0(ids, "_R2.fastq.gz")
  )
  if (!is.null(refs)) m$Reference <- refs
  if (!is.null(topology)) m$Reference_topology <- topology
  fn <- file.path(dir, "mapping.csv")
  utils::write.csv(m, fn, row.names = FALSE)
  fn
}
```

Change `mtr_refs_project` to:

```r
mtr_refs_project <- function(dir, ids = c("S1", "S2"), ...) {
  new_db(db_path = file.path(dir, ".sqlite"),
         mapping_fn = mtr_refs_mapping(dir, ids = ids), ...)
  file.path(dir, ".sqlite")
}
```

Replace `"new_db puts a Reference on the sample's own MapToRef options set"` with:

```r
test_that("new_db writes a Reference and its topology onto the sample row", {
  d <- withr::local_tempdir()
  fa <- mtr_ref_fasta(d)
  mapping <- mtr_refs_mapping(d, refs = c(fa, ""), topology = c("circular", ""))
  db <- file.path(d, ".sqlite")
  expect_warning(
    new_db(db_path = db, mapping_fn = mapping, assembler = "MapToRef"),
    "S2"
  )
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_false(any(c("Reference", "Reference_topology") %in% DBI::dbListFields(con, "samples")))
  a <- DBI::dbGetQuery(
    con, "SELECT ID, assemble_opts, maptoref_ref, maptoref_topology FROM assemble ORDER BY ID")
  expect_equal(a$assemble_opts, c("default", "default"))
  expect_equal(a$maptoref_ref, c(normalizePath(fa, winslash = "/"), NA_character_))
  expect_equal(a$maptoref_topology, c("circular", NA_character_))
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM assemble_opts")$n, 1L)
})

test_that("new_db refuses a FASTA Reference without a Reference_topology", {
  d <- withr::local_tempdir()
  fa <- mtr_ref_fasta(d)
  mapping <- mtr_refs_mapping(d, refs = c(fa, ""))
  expect_error(
    new_db(db_path = file.path(d, ".sqlite"), mapping_fn = mapping, assembler = "MapToRef"),
    "S1.*needs a topology"
  )
  expect_false(file.exists(file.path(d, ".sqlite")))
})

test_that("Reference_topology without Reference is refused", {
  d <- withr::local_tempdir()
  mapping <- mtr_refs_mapping(d, topology = c("circular", ""))
  expect_error(
    new_db(db_path = file.path(d, ".sqlite"), mapping_fn = mapping),
    "Reference_topology"
  )
})

test_that("a Reference under a non-MapToRef default set is stored and warned about", {
  d <- withr::local_tempdir()
  mapping <- mtr_refs_mapping(d, refs = c("NC_002333", "NC_002333"))
  db <- file.path(d, ".sqlite")
  testthat::local_mocked_bindings(
    .mtr_ncbi_known = function(accs, timeout = 30L) list(ok = TRUE, found = "NC_002333")
  )
  expect_warning(
    new_db(db_path = db, mapping_fn = mapping, assembler = "GetOrganelle"),
    "ignored until"
  )
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_equal(DBI::dbGetQuery(con, "SELECT maptoref_ref FROM assemble")$maptoref_ref,
               rep("NC_002333", 2))
})
```

Delete `"a per-sample set inherits the base set and leaves other samples alone"`, `"no Reference column means no extra options sets"`, `"a Reference switches that sample to MapToRef whatever assembler says"`, `"new_db still demands a topology for a FASTA option-set reference"`, and `"new_db does not demand a topology for an accession"`.

Replace `"a bad Reference value and a bad option-set value are reported together"` with:

```r
test_that("every bad Reference value is reported at once", {
  d <- withr::local_tempdir()
  mapping <- mtr_refs_mapping(d, refs = c("/no/such/a.gb", "/no/such/b.gb"))
  expect_error(
    new_db(db_path = file.path(d, ".sqlite"), mapping_fn = mapping, assembler = "MapToRef"),
    "S1.*S2"
  )
  expect_false(file.exists(file.path(d, ".sqlite")))
})
```

Rewrite `"add_samples seeds the reference and never adds a samples column"` so the added mapping carries `Reference` and `Reference_topology` and it asserts: new sample's `assemble_opts` is `"default"`, `maptoref_ref` is the normalised path, `maptoref_topology` is `"circular"`, neither column is on `samples`, and `assemble_opts` still has one row. Delete `"add_samples refuses to clobber an options set that already exists"`. Keep `"add_samples refuses a project that predates the reference column"` and `"update_sample_metadata strips a Reference column with a message"`.

- [ ] **Step 2: `new_db` tests in `tests/testthat/test-map-to-ref.R`**

In the block from `"new_db stores the five MapToRef option columns"` through `"new_db applies the modal's reference-topology and quote rules"`:

- Remove every `maptoref_ref = ...` and `maptoref_topology = ...` argument from `mtr_test_db()` calls.
- Wrap each `mtr_test_db(d, assembler = "MapToRef", ...)` that must succeed in `suppressWarnings(...)` (it warns "no reference"). `expect_error(...)` calls need no wrapper.
- In `"new_db stores the five MapToRef option columns"`, drop `skip_if_not(file.exists(mtr_fixture()))`, change `expect_equal(opts$maptoref_ref, ...)` to `expect_true(is.na(opts$maptoref_ref))` and `expect_equal(opts$maptoref_topology, "circular")` to `expect_true(is.na(opts$maptoref_topology))`.
- In `"new_db warns for MapToRef without a reference and rejects a bad topology"`, delete the `d2` half (the `expect_error(..., "circular or linear")`) and rename the test to `"new_db warns for MapToRef without a reference"`.
- In `"new_db applies the modal's reference-topology and quote rules"`, delete the `fa <- mtr_write(...)` line and the first `expect_error(... "maptoref_topology")`, and change the final `expect_no_error(...)` to `expect_no_error(suppressWarnings(mtr_test_db(d, assembler = "MapToRef")))`. Rename to `"new_db applies the modal's quote rules"`.

- [ ] **Step 3: Run both files, expect the rewritten tests to fail**

Run: `Rscript -e 'devtools::test(filter = "map-to-ref")' 2>&1 | grep -E "^Failure|^Error" | head`

- [ ] **Step 4: `.mtr_take_ref_col` in `R/map_to_ref_refs.R`**

```r
.mtr_take_ref_col <- function(mapping, mapping_id = "ID") {
  has_ref <- "Reference" %in% colnames(mapping)
  has_topo <- "Reference_topology" %in% colnames(mapping)
  if (has_topo && !has_ref) {
    stop("The mapping file has a Reference_topology column but no Reference column",
         call. = FALSE)
  }
  if (!has_ref) {
    return(list(mapping = mapping, refs = NULL, topology = NULL))
  }
  ids <- as.character(mapping[[mapping_id]])
  refs <- stats::setNames(as.character(mapping[["Reference"]]), ids)
  topology <- if (has_topo) {
    stats::setNames(as.character(mapping[["Reference_topology"]]), ids)
  } else {
    stats::setNames(rep(NA_character_, length(ids)), ids)
  }
  keep <- setdiff(colnames(mapping), c("Reference", "Reference_topology"))
  list(mapping = mapping[, keep, drop = FALSE], refs = refs, topology = topology)
}
```

Update its comment to mention both columns.

- [ ] **Step 5: `R/init_db.R`**

a. Roxygen: delete the `@param maptoref_ref` block (from `#' @param maptoref_ref Default MapToRef reference mitogenome for the parameter` through `#'   MapToRef parameter set, or through \code{\link{set_maptoref_refs}}.`) and the `@param maptoref_topology` block (from `#' @param maptoref_topology Topology of a MapToRef reference, "circular" or` through the end of that paragraph; read it to find the last line).

b. Signature: delete `maptoref_ref = NA_character_,` and `maptoref_topology = NA_character_,`.

c. Validation: delete the `if (!is.na(maptoref_topology) && maptoref_topology %nin% c("circular", "linear"))` stop (three lines) and the `if (assembler == "MapToRef" && !is.na(maptoref_ref) && ...` stop (six lines).

d. Replace the `checked <-` block (from `checked <- .mtr_validate_refs(` through `names(refs) <- names(taken$refs)` and its closing `}`) with:

```r
  refs <- NULL
  topo <- NULL
  if (!is.null(taken$refs)) {
    refs <- .mtr_validate_refs(taken$refs, ids = names(taken$refs),
                               context = "the mapping file 'Reference' column")
    topo <- .mtr_validate_ref_topology(refs, taken$topology, ids = names(taken$refs),
                                   context = "the mapping file 'Reference_topology' column")
    names(refs) <- names(topo) <- names(taken$refs)
  }
```

e. Assemble `rows_upsert` mutate: replace `maptoref_ref = NA_character_,` and `maptoref_topology = NA_character_,` with:

```r
          maptoref_ref = if (is.null(refs)) NA_character_ else unname(refs[ID]),
          maptoref_topology = if (is.null(topo)) NA_character_ else unname(topo[ID]),
```

f. `assemble_opts` insert: `maptoref_ref = maptoref_ref,` becomes `maptoref_ref = NA_character_,` and `maptoref_topology = maptoref_topology` becomes `maptoref_topology = NA_character_`.

g. Replace `.mtr_seed_per_sample_opts(con, refs)` and its two comment lines with `.mtr_warn_refs_ignored(con, names(refs)[!is.na(refs)])`.

- [ ] **Step 6: `R/add_samples.R`**

a. Roxygen: replace the run from `A sample with a \code{Reference} is given its own` through `keep the default set.` with `A FASTA reference also needs a \code{Reference_topology} column (circular or linear). Both values are stored on the sample and used when its parameter set assembles with MapToRef.`

b. Replace the `refs <- if (is.null(taken$refs)) { NULL } else { ... }` block with:

```r
  refs <- NULL
  topo <- NULL
  if (!is.null(taken$refs)) {
    refs <- .mtr_validate_refs(taken$refs, ids = names(taken$refs),
                               context = "the mapping file 'Reference' column")
    topo <- .mtr_validate_ref_topology(refs, taken$topology, ids = names(taken$refs),
                                   context = "the mapping file 'Reference_topology' column")
    names(refs) <- names(topo) <- names(taken$refs)
  }
```

c. In the assemble `rows_insert`, replace `maptoref_ref = NA_character_,` with:

```r
          maptoref_ref = if (is.null(refs)) NA_character_ else unname(refs[mapping$ID]),
          maptoref_topology = if (is.null(topo)) NA_character_ else unname(topo[mapping$ID]),
```

d. Change the guard `if ("maptoref_ref" %nin% DBI::dbListFields(con, "assemble"))` to `if (!all(c("maptoref_ref", "maptoref_topology") %in% DBI::dbListFields(con, "assemble")))`.

e. Replace `.mtr_seed_per_sample_opts(con, refs)` and its comment with `.mtr_warn_refs_ignored(con, names(refs)[!is.na(refs)])`.

- [ ] **Step 7: `R/init_project.R` roxygen**

Apply the Step 6a sentence replacement to the `mapping_fn` param text.

- [ ] **Step 8: Helpers in `R/map_to_ref_refs.R`**

Delete `.mtr_opts_name` and `.mtr_seed_per_sample_opts`. Add above `.mtr_warn_missing_refs`:

```r
# A reference stored under a set that does not assemble with MapToRef is not
# an error: the user may switch the set later. Say so once.
#' @noRd
.mtr_warn_refs_ignored <- function(con, ids) {
  if (length(ids) == 0L ||
      "assembler" %nin% DBI::dbListFields(con, "assemble_opts")) {
    return(invisible(character(0)))
  }
  q <- DBI::dbGetQuery(con, paste(
    "SELECT a.ID FROM assemble a JOIN assemble_opts o",
    "ON a.assemble_opts = o.assemble_opts",
    "WHERE o.assembler <> 'MapToRef'"
  ))$ID
  hit <- intersect(ids, q)
  if (length(hit) > 0L) {
    warning(length(hit), " sample(s) have a MapToRef reference but are on a ",
            "parameter set that does not assemble with MapToRef; the reference ",
            "is stored and ignored until the set is switched to MapToRef.",
            call. = FALSE)
  }
  invisible(hit)
}
```

- [ ] **Step 9: Document and run both files**

Run: `Rscript -e 'devtools::document()' && Rscript -e 'devtools::test(filter = "map-to-ref")' 2>&1 | grep -E "^Failure|^Error" | head -20`
Expected: `test-map-to-ref.R` clean; in `test-map-to-ref-refs.R` only `set_maptoref_refs` and migration tests still fail.

- [ ] **Step 10: ASCII check**

Run: `grep -nP '[^\x00-\x7F]' R/init_db.R R/add_samples.R R/init_project.R R/map_to_ref_refs.R`

---

### Task 3: `set_maptoref_refs()` writes the columns

**Files:**
- Modify: `R/map_to_ref_refs.R` (`set_maptoref_refs` roxygen and body; delete `.mtr_dedicated_opts`, `.mtr_route_refs`)
- Test: `tests/testthat/test-map-to-ref-refs.R`

**Interfaces:**
- Produces: `set_maptoref_refs(path, refs)`; `refs` column 1 ID, column 2 reference, optional column 3 topology.

- [ ] **Step 1: Rewrite the tests**

Replace `"set_maptoref_refs gives the sample its own set and flips the switch"` with:

```r
test_that("set_maptoref_refs writes both columns and flips the switch", {
  d <- withr::local_tempdir()
  fa <- mtr_ref_fasta(d)
  db <- mtr_refs_project(d)
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "UPDATE assemble SET assemble_switch = 2")

  set_maptoref_refs(d, data.frame(a = "S1", b = fa, c = "Linear"))

  a <- DBI::dbGetQuery(con, paste(
    "SELECT ID, assemble_opts, maptoref_ref, maptoref_topology, assemble_switch",
    "FROM assemble ORDER BY ID"))
  expect_equal(a$assemble_opts, c("default", "default"))
  expect_equal(a$maptoref_ref, c(normalizePath(fa, winslash = "/"), NA_character_))
  expect_equal(a$maptoref_topology, c("linear", NA_character_))
  expect_equal(a$assemble_switch, c(1, 2))
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM assemble_opts")$n, 1L)
})

test_that("set_maptoref_refs refuses a FASTA without a topology", {
  d <- withr::local_tempdir()
  fa <- mtr_ref_fasta(d)
  mtr_refs_project(d)
  expect_error(set_maptoref_refs(d, data.frame(a = "S1", b = fa)), "needs a topology")
})

test_that("a topology change alone re-queues the row", {
  d <- withr::local_tempdir()
  fa <- mtr_ref_fasta(d)
  db <- mtr_refs_project(d)
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  set_maptoref_refs(d, data.frame(a = "S1", b = fa, c = "circular"))
  DBI::dbExecute(con, "UPDATE assemble SET assemble_switch = 2")
  expect_message(set_maptoref_refs(d, data.frame(a = "S1", b = fa, c = "linear")), "Updated 1")
  a <- DBI::dbGetQuery(con, "SELECT maptoref_topology, assemble_switch FROM assemble WHERE ID = 'S1'")
  expect_equal(a$maptoref_topology, "linear")
  expect_equal(a$assemble_switch, 1)
})
```

Delete `"set_maptoref_refs edits the sample's own set, not the column"`, `"set_maptoref_refs compares against the value on the sample's own set"`, `"a sample pointing at no existing set is refused before any write"`, and `"a FASTA reference on a set without a topology warns and names the set"`.

Rewrite `"a blank value clears the per-sample reference"`: set `fa` with topology `circular`, then set blank; expect `maptoref_ref` and `maptoref_topology` for S1 both NA and `assemble_switch` 1.

Read each remaining `set_maptoref_refs` test (`reads a CSV by position`, `does not re-queue an unchanged row`, `refuses unknown IDs, duplicates, and locked rows`, `a locked row the call would not change is left alone`, `validates values before writing anything`, `warns about samples still without a reference`) and `".mtr_ref_key compares files by their normalised path"`. Wherever one passes a FASTA path, add a third column `"circular"`. Any assertion on `S1_maptoref` or on `assemble_opts.maptoref_ref` becomes the equivalent on `assemble.maptoref_ref`.

- [ ] **Step 2: Run the file, expect the rewritten tests to fail**

Run: `Rscript -e 'devtools::test(filter = "map-to-ref-refs")' 2>&1 | grep -E "^Failure|^Error" | head`

- [ ] **Step 3: Rewrite `set_maptoref_refs` from the `refs` shape check onward**

```r
  if (!is.data.frame(refs) || ncol(refs) < 2L || nrow(refs) == 0L) {
    stop("refs must be a CSV path or a data frame with at least two columns ",
         "(sample ID, reference, optional topology) and at least one row")
  }

  ids <- trimws(as.character(refs[[1]]))
  vals <- as.character(refs[[2]])
  topo_in <- if (ncol(refs) >= 3L) as.character(refs[[3]]) else rep(NA_character_, length(ids))
  if (any(duplicated(ids))) {
    stop("Duplicate IDs in refs: ",
         paste(unique(ids[duplicated(ids)]), collapse = ", "))
  }

  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = db)
  on.exit(DBI::dbDisconnect(con))
  if (!all(c("maptoref_ref", "maptoref_topology") %in% DBI::dbListFields(con, "assemble"))) {
    stop("This project predates the per-sample MapToRef reference columns; run ",
         "MitoPilot::backwards_compatibility() first")
  }
  cur <- DBI::dbGetQuery(
    con, "SELECT ID, maptoref_ref, maptoref_topology, assemble_lock FROM assemble")

  unknown <- setdiff(ids, cur$ID)
  if (length(unknown) > 0L) {
    stop("sample(s) ", paste(shQuote(unknown), collapse = ", "),
         " absent in the existing database")
  }
  new_vals <- .mtr_validate_refs(vals, ids = ids, context = "the reference list")
  new_topo <- .mtr_validate_ref_topology(new_vals, topo_in, ids = ids,
                                     context = "the reference list")
  m <- match(ids, cur$ID)
  same_ref <- (is.na(new_vals) & is.na(cur$maptoref_ref[m])) |
    (!is.na(new_vals) & !is.na(cur$maptoref_ref[m]) & new_vals == cur$maptoref_ref[m])
  same_topo <- (is.na(new_topo) & is.na(cur$maptoref_topology[m])) |
    (!is.na(new_topo) & !is.na(cur$maptoref_topology[m]) & new_topo == cur$maptoref_topology[m])
  changed <- which(!(same_ref & same_topo))
  locked <- intersect(
    ids[changed],
    cur$ID[!is.na(cur$assemble_lock) & cur$assemble_lock == 1]
  )
  if (length(locked) > 0L) {
    stop("sample(s) ", paste(shQuote(locked), collapse = ", "),
         " are locked; unlock them in the Assemble module first")
  }
  if (length(changed) == 0L) {
    message("No changes: every sample already had that reference.")
    return(invisible(.mtr_warn_missing_refs(con)))
  }

  dplyr::tbl(con, "assemble") |>
    dplyr::rows_update(
      data.frame(ID = ids[changed], maptoref_ref = new_vals[changed],
                 maptoref_topology = new_topo[changed], assemble_switch = 1),
      unmatched = "ignore",
      in_place = TRUE,
      copy = TRUE,
      by = "ID"
    )
  message("Updated ", length(changed), " sample(s); ",
          length(ids) - length(changed), " already had that reference.")
  invisible(.mtr_warn_missing_refs(con))
}
```

- [ ] **Step 4: Delete `.mtr_dedicated_opts` and `.mtr_route_refs`**

Whole functions with their comments.

- [ ] **Step 5: Roxygen**

Replace the paragraph from `#' project. A reference has one home: the sample's own parameter set` through `#' will use.` with:

```r
#' project. A reference has one home, the sample row; the parameter set the
#' sample is on supplies the mapper and its options. A FASTA reference also
#' needs a topology (circular or linear) in the third column. A blank
#' reference clears both values.
```

In `@param refs`, after `Any further columns are ignored.` change to `An optional third column holds the topology, required for a FASTA reference; any further columns are ignored.`

- [ ] **Step 6: Document, run the file**

Run: `Rscript -e 'devtools::document()' && Rscript -e 'devtools::test(filter = "map-to-ref-refs")' 2>&1 | grep -E "^Failure|^Error" | head`
Expected: only the two migration tests still fail.

---

### Task 4: Migration copies set values down

**Files:**
- Modify: `R/map_to_ref_refs.R` (replace `.mtr_fold_override_column` with `.mtr_copy_set_refs_down`)
- Modify: `R/backwards_compatibility.R` (call site)
- Test: `tests/testthat/test-map-to-ref-refs.R`

**Interfaces:**
- Produces: `.mtr_copy_set_refs_down(con)` returns invisibly the IDs whose reference was written.

- [ ] **Step 1: Replace the two migration tests**

Replace `"the migration folds a column value into the sample's own set"` and `"a column value the migration cannot fold stays where the pipeline reads it"` with:

```r
test_that("the migration copies a set reference and topology down onto its samples", {
  d <- withr::local_tempdir()
  fa <- mtr_ref_fasta(d)
  db <- mtr_refs_project(d)
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "UPDATE assemble_opts SET maptoref_ref = ?, maptoref_topology = 'circular'",
                 params = list(fa))
  DBI::dbExecute(con, "UPDATE assemble SET maptoref_ref = 'NC_002333' WHERE ID = 'S2'")

  expect_message(.mtr_copy_set_refs_down(con), "1 sample")

  a <- DBI::dbGetQuery(
    con, "SELECT ID, maptoref_ref, maptoref_topology FROM assemble ORDER BY ID")
  expect_equal(a$maptoref_ref, c(fa, "NC_002333"))
  expect_equal(a$maptoref_topology, c("circular", "circular"))
  o <- DBI::dbGetQuery(con, "SELECT maptoref_ref, maptoref_topology FROM assemble_opts")
  expect_true(is.na(o$maptoref_ref) && is.na(o$maptoref_topology))
  expect_silent(.mtr_copy_set_refs_down(con))
})

test_that("the migration is a no-op on a userAsmb project", {
  d <- withr::local_tempdir()
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(d, ".sqlite"))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE assemble (ID TEXT, maptoref_ref TEXT, maptoref_topology TEXT)")
  DBI::dbExecute(con, "CREATE TABLE assemble_opts (assemble_opts TEXT)")
  expect_silent(.mtr_copy_set_refs_down(con))
})
```

- [ ] **Step 2: Run, expect failure**

Run: `Rscript -e 'devtools::test(filter = "map-to-ref-refs")' 2>&1 | grep -E "copies a set reference|no-op on a userAsmb"`

- [ ] **Step 3: Replace `.mtr_fold_override_column`**

```r
# One-time move of set-level values onto the samples that used them. The last
# statement empties the source, so running it again is silent.
#' @noRd
.mtr_copy_set_refs_down <- function(con) {
  opts_cols <- DBI::dbListFields(con, "assemble_opts")
  asm_cols <- DBI::dbListFields(con, "assemble")
  if (!all(c("maptoref_ref", "maptoref_topology") %in% opts_cols) ||
      !all(c("maptoref_ref", "maptoref_topology") %in% asm_cols)) {
    return(invisible(character(0)))
  }
  copy <- function(col) {
    ids <- DBI::dbGetQuery(con, sprintf(paste(
      "SELECT a.ID FROM assemble a JOIN assemble_opts o",
      "ON a.assemble_opts = o.assemble_opts",
      "WHERE NULLIF(TRIM(a.%1$s), '') IS NULL",
      "AND NULLIF(TRIM(o.%1$s), '') IS NOT NULL"), col))$ID
    if (length(ids) > 0L) {
      DBI::dbExecute(con, sprintf(paste(
        "UPDATE assemble SET %1$s = (SELECT TRIM(o.%1$s) FROM assemble_opts o",
        "WHERE o.assemble_opts = assemble.assemble_opts)",
        "WHERE NULLIF(TRIM(%1$s), '') IS NULL AND ID IN (%2$s)"),
        col, paste(sprintf("'%s'", ids), collapse = ",")))
    }
    ids
  }
  refs <- copy("maptoref_ref")
  copy("maptoref_topology")
  if (length(refs) > 0L) {
    message("moved the MapToRef reference of ", length(refs),
            " sample(s) from their parameter set onto the sample")
  }
  DBI::dbExecute(con, paste(
    "UPDATE assemble_opts SET maptoref_ref = NULL, maptoref_topology = NULL",
    "WHERE maptoref_ref IS NOT NULL OR maptoref_topology IS NOT NULL"))
  invisible(refs)
}
```

Sample IDs are validated to `^[a-zA-Z0-9_:-]+$` at ingest, so the quoted `IN` list is safe.

- [ ] **Step 4: Call site in `R/backwards_compatibility.R`**

Replace the two comment lines above the `maptoref_ref` ALTER with `# per-sample MapToRef reference and topology; set-level values move down onto samples` and `.mtr_fold_override_column(con)` with `.mtr_copy_set_refs_down(con)`. The call must sit after both ALTER blocks from Task 1.

- [ ] **Step 5: Run the whole file**

Run: `Rscript -e 'devtools::test(filter = "map-to-ref-refs")' 2>&1 | tail -3`
Expected: `FAIL 0`.

- [ ] **Step 6: Dead-name grep**

Run: `grep -rn "mtr_seed_per_sample_opts\|mtr_dedicated_opts\|mtr_route_refs\|mtr_opts_name\|mtr_fold_override_column\|mtr_warn_no_topology" R/ tests/ inst/ man/`
Expected: no output.

---

### Task 5: App column, CSS rule, and reference modal

**Files:**
- Create: `R/utils_maptoref_display.R`
- Modify: `R/constants.R` (`MP_COL_NAMES`, `MP_COL_TIPS`)
- Modify: `R/app_assemble_utils.R` (`fetch_assemble_data`; options modal; new `maptoref_ref_modal`)
- Modify: `R/app_assemble.R` (`ASSEMBLE_COL_GROUPS`, colDef, `col_css`, modal wiring, handlers)
- Test: `tests/testthat/test-maptoref-display.R`

**Interfaces:**
- Consumes: `rt_link(InputId, title, lock_col)`; `.mtr_validate_refs`, `.mtr_validate_ref_topology`, `.mtr_needs_topology`; `mp_alert(title, text, type)`; `mp_modal_title()`; `mp_footer(primary = )`; `opts_help(...)`; `need_unlocked(locked_ids, noun, session)`; `assemble_locked_ids(rv, rows)`; `trigger("refresh_assemble")`.
- Produces: `mtr_display_ref(assembler, ref, topology)`.

- [ ] **Step 1: Display helper test**

Create `tests/testthat/test-maptoref-display.R`:

```r
test_that("mtr_display_ref links only MapToRef samples and shows a topology", {
  out <- mtr_display_ref(
    assembler = c("MapToRef", "MapToRef", "MapToRef", "GetOrganelle", NA),
    ref = c("NC_002333", "/r/m.fasta", NA, "NC_002333", NA),
    topology = c(NA, "circular", NA, "linear", NA)
  )
  expect_equal(out, c("NC_002333", "/r/m.fasta (circular)", "set reference", NA, NA))
})
```

- [ ] **Step 2: Run, expect failure**

Run: `Rscript -e 'devtools::test(filter = "maptoref-display")' 2>&1 | tail -3`

- [ ] **Step 3: Create `R/utils_maptoref_display.R`**

```r
# The Assemble table cell for a MapToRef reference. rt_link() renders NA as
# plain empty text, so a sample on any other assembler gets no link.
#' @noRd
mtr_display_ref <- function(assembler, ref, topology) {
  is_mtr <- !is.na(assembler) & assembler == "MapToRef"
  blank <- is.na(ref) | !nzchar(trimws(ref))
  topo <- ifelse(is.na(topology) | !nzchar(trimws(topology)), "",
                 paste0(" (", topology, ")"))
  out <- rep(NA_character_, length(assembler))
  out[is_mtr] <- ifelse(blank[is_mtr], "set reference", paste0(ref, topo)[is_mtr])
  out
}
```

- [ ] **Step 4: Run, expect pass**

- [ ] **Step 5: `fetch_assemble_data()` in `R/app_assemble_utils.R`**

In the final `dplyr::mutate(blast_ref_status = poor_blast_ref, blast_hits = ...)`, add:

```r
      maptoref_ref = mtr_display_ref(assembler, maptoref_ref, maptoref_topology)
```

`assembler` is joined from `assemble_opts_tbl`; both `maptoref_*` columns come from `dplyr::tbl(db, "assemble")`. In the trailing `dplyr::relocate(...)`, add `maptoref_ref,` after `blast_opts,`.

- [ ] **Step 6: Options modal in `R/app_assemble_utils.R`**

Delete the `textInput(ns("maptoref_ref"), ...)` element (through `id = ns("help_maptoref_ref"), nested = TRUE)),`) and the `selectInput(ns("maptoref_topology"), ...)` element (through `id = ns("help_maptoref_topology"), nested = TRUE)),`). In the modal's `maptoref_ids <- c(...)` remove `"maptoref_ref", "maptoref_topology", `.

- [ ] **Step 7: `maptoref_ref_modal()` at the end of `R/app_assemble_utils.R`**

```r
#' Set one sample's MapToRef reference and, for a FASTA, its topology
#' @noRd
maptoref_ref_modal <- function(id, ref, topology, session = getDefaultReactiveDomain()) {
  ns <- session$ns
  showModal(
    modalDialog(
      title = mp_modal_title(paste("MapToRef reference for", id)),
      textInput(
        ns("maptoref_ref_value"),
        label = "Reference:",
        value = ref %|NA|% "",
        width = "100%"
      ) |> tagAppendChild(opts_help(
        "Absolute path on this machine, URL, or NCBI nucleotide accession ",
        "(for example NC_002333) of one complete mitogenome to map this ",
        "sample's reads against. Blank clears the reference. Saving ",
        "re-queues the sample.")),
      selectInput(
        ns("maptoref_ref_topology"),
        label = "Reference topology:",
        choices = c("", "circular", "linear"),
        selected = topology %|NA|% "",
        width = "100%"
      ) |> tagAppendChild(opts_help(
        "Required for a FASTA reference, whose header carries no topology. ",
        "A GenBank record or accession supplies its own.")),
      footer = mp_footer(primary = actionButton(ns("update_maptoref_ref"), "Save"))
    )
  )
}
```

- [ ] **Step 8: `R/constants.R`**

In `MP_COL_NAMES`, after the `assemble_opts` entry add `maptoref_ref        = "MapToRef ref",`. In `MP_COL_TIPS`, at the matching position add `maptoref_ref        = "Reference this sample maps its reads to. Click to change it or set a FASTA topology",`. Match each vector's alignment style.

- [ ] **Step 9: `R/app_assemble.R` column**

`ASSEMBLE_COL_GROUPS`: `Options  = c("pre_opts", "assemble_opts", "maptoref_ref", "blast_opts"),`.

After the `blast_opts = colDef(...)` entry add:

```r
            maptoref_ref = colDef(
              show = TRUE,
              class = paste(.grp("maptoref_ref"), "mp-col-maptoref"),
              headerClass = paste(.grp("maptoref_ref"), "mp-col-maptoref"),
              name = mp_col_name("maptoref_ref"),
              header = mp_col_header("maptoref_ref"),
              html = TRUE,
              width = 180,
              cell = rt_link(ns("set_maptoref_ref"), title = "Set MapToRef reference",
                             lock_col = "assemble_lock")
            ),
```

- [ ] **Step 10: CSS rule**

In `output$col_css`, after `hidden_state <- ...` add `no_mtr <- is.null(rv$data) || !any(rv$data$assembler %in% "MapToRef")`, and in `rules <- c(` add `if (no_mtr) paste0(sel, ".mp-col-maptoref { display: none !important; }")`.

- [ ] **Step 11: Options-modal wiring**

a. Delete the `updateTextInput(inputId = "maptoref_ref", ...)` call and the `updateSelectInput(inputId = "maptoref_topology", ...)` call.
b. In the three `maptoref_ids <- c("maptoref_ref", "maptoref_topology", ...)` / `for (i in c("maptoref_ref", "maptoref_topology", ...))` vectors remove those two names.
c. In the save handler delete `ref_value <- ...`, `topology_value <- ...`, and the whole `needs_topology` block with its alert. Change the quote check to `grepl(.mtr_bad_chars_re, paste(input$maptoref %||% "", input$maptoref_consensus %||% ""))` and its alert text to "The mapper and samtools consensus values".
d. In the `rows_upsert` data frame: `maptoref_ref = NA_character_,` and `maptoref_topology = NA_character_`.

- [ ] **Step 12: Handlers**

After the `observeEvent(input$set_assemble_opts, { ... })` block add:

```r
    observeEvent(input$set_maptoref_ref, {
      row <- as.numeric(input$set_maptoref_ref)
      if (!need_unlocked(assemble_locked_ids(rv, row))) return()
      id <- rv$data$ID[row]
      rv$mtr_ref_id <- id
      cur <- DBI::dbGetQuery(
        session$userData$con,
        "SELECT maptoref_ref, maptoref_topology FROM assemble WHERE ID = ?",
        params = list(id)
      )
      maptoref_ref_modal(id, cur$maptoref_ref, cur$maptoref_topology)
    })
    # Only a FASTA needs the topology select.
    observeEvent(input$maptoref_ref_value, {
      shinyjs::toggle("maptoref_ref_topology",
                      condition = .mtr_needs_topology(input$maptoref_ref_value %||% ""))
    }, ignoreNULL = FALSE)
    observeEvent(input$update_maptoref_ref, {
      id <- req(rv$mtr_ref_id)
      res <- tryCatch({
        val <- .mtr_validate_refs(input$maptoref_ref_value %||% "", ids = id,
                                  context = "the MapToRef reference")
        topo <- .mtr_validate_ref_topology(val, input$maptoref_ref_topology %||% "",
                                       ids = id, context = "the MapToRef reference")
        list(val = val, topo = topo)
      }, error = function(e) {
        mp_alert(title = "Invalid reference", text = conditionMessage(e), type = "error")
        NULL
      })
      if (is.null(res)) return()
      dplyr::tbl(session$userData$con, "assemble") |>
        dplyr::rows_update(
          data.frame(ID = id, maptoref_ref = res$val, maptoref_topology = res$topo,
                     assemble_switch = 1),
          unmatched = "ignore", in_place = TRUE, copy = TRUE, by = "ID"
        )
      rv$mtr_ref_id <- NULL
      removeModal()
      trigger("refresh_assemble")
    })
```

Check `need_unlocked()` in `R/app_guards.R`: signature `(locked_ids, noun = "sample", session = getDefaultReactiveDomain())`; the call above is correct.

- [ ] **Step 13: Smoke the data path without Shiny**

```bash
Rscript -e '
devtools::load_all(quiet = TRUE)
d <- tempfile(); dir.create(d)
m <- data.frame(ID = c("S1","S2"), Taxon = "x", R1 = "a", R2 = "b", Reference = c("NC_002333", ""))
fn <- file.path(d, "m.csv"); write.csv(m, fn, row.names = FALSE)
suppressWarnings(new_db(db_path = file.path(d, ".sqlite"), mapping_fn = fn, assembler = "MapToRef"))
con <- DBI::dbConnect(RSQLite::SQLite(), file.path(d, ".sqlite"))
a <- DBI::dbGetQuery(con, "SELECT a.ID, a.maptoref_ref, a.maptoref_topology, o.assembler FROM assemble a JOIN assemble_opts o USING (assemble_opts)")
print(mtr_display_ref(a$assembler, a$maptoref_ref, a$maptoref_topology))
'
```

Expected: `[1] "NC_002333"     "set reference"`. If NCBI is unreachable the call warns and still prints that.

- [ ] **Step 14: Full suite**

Run: `Rscript -e 'devtools::test()' 2>&1 | tail -3`
Expected: `FAIL 0`. If `test-ui-reactable-helpers.R` or `test-maptoref-help.R` asserts on `maptoref_ref`, `maptoref_topology`, `help_maptoref_ref`, or `help_maptoref_topology` in the options modal, remove that assertion.

- [ ] **Step 15: ASCII check**

Run: `grep -nP '[^\x00-\x7F]' R/app_assemble.R R/app_assemble_utils.R R/utils_maptoref_display.R R/constants.R`

---

### Task 6: Docs and final sweep

**Files:** `README.md`, `NEWS.md`, `man/*.Rd` (generated), `tools/map_to_ref_per_sample_refs_plan.md`.

- [ ] **Step 1: README**

In the Assembly bullet, replace from `Each sample can use a different one: add a` through `A reference may be a file path, a URL, or an NCBI accession.` with:

```
supply yourself. Each sample carries its own: add a `Reference`
column (and, for a FASTA, a `Reference_topology` column) to your
mapping CSV, call `MitoPilot::set_maptoref_refs()`, or click the
sample's MapToRef ref cell in the Assemble table. A reference may be a
file path, a URL, or an NCBI accession; the mapper and its options
come from the sample's parameter set.
```

- [ ] **Step 2: NEWS**

Replace the `**Per-sample references.**` bullet with:

```
- **Per-sample references.** A MapToRef reference and its FASTA topology belong to the sample, not to the parameter set, so one set of mapper options serves any number of samples that each map to a different mitogenome. Add an optional `Reference` column to the mapping CSV (an absolute file path, a URL, or an NCBI nucleotide accession such as `NC_002333`) plus a `Reference_topology` column for a FASTA, set them later with `set_maptoref_refs(path, refs)` from a two- or three-column CSV, or click a sample's MapToRef ref cell in the Assemble table. The column appears only when a sample is on a MapToRef set. Values stored on a parameter set by an earlier version are moved onto its samples by `backwards_compatibility()`.
```

Change the bullet `References can be GenBank (preferred, one record) or FASTA. A FASTA reference needs its topology set explicitly.` to end `... needs its topology set per sample.`

- [ ] **Step 3: Mark the old plan superseded**

Second line of `tools/map_to_ref_per_sample_refs_plan.md`: `> Superseded by tools/maptoref_ref_decouple_spec.md and tools/maptoref_ref_decouple_plan.md (2026-09-14).`

- [ ] **Step 4: Document and grep**

Run: `Rscript -e 'devtools::document()' && grep -rn "own parameter set\|S1_maptoref\|_maptoref\b" R/ man/ README.md NEWS.md tests/`
Expected: no output.

- [ ] **Step 5: Full suite and ASCII check**

Run: `Rscript -e 'devtools::test()' 2>&1 | tail -3 && git diff --name-only | xargs grep -nP '[^\x00-\x7F]'; git ls-files --others --exclude-standard | xargs grep -nP '[^\x00-\x7F]'`
Expected: `FAIL 0` and no ASCII hits.
