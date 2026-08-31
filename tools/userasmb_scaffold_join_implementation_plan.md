# User-Assembly Scaffold Join Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Run the scaffold join on the user-assembly workflow, so a mitogenome split across several contigs is ordered against a reference into one Path 0 instead of being annotated as separate units.

**Architecture:** `SCAFFOLD_JOIN` is reused unchanged. The work is producing the three channels it takes (`join_eligible`, `join_expected`, `join_redo`) from `coverage_userAsmb_workflow.nf`, calling it from `WF1_userAsmb` the same way `WF1` does, adding the `join_scaffolds` toggle to the user-assembly schema, and adding the app plumbing that lets a user queue a redo and see the result.

**Tech Stack:** R (testthat, dplyr, DBI/RSQLite, Biostrings, shiny/reactable), Nextflow DSL2 with the nf-sqldb plugin, SQLite.

**Spec:** `tools/userasmb_scaffold_join_design.md`

## Global Constraints

- ASCII only in R code. No non-ASCII characters, no em dashes.
- Minimal comments. Comment the why, never narrate a bugfix.
- Never push. Commit only as the plan's steps say.
- No Claude attribution anywhere in commit messages.
- Branch: work stays on `userasmb-find-mito`. Do not create a branch.
- Existing tests and public function signatures keep working. New arguments get defaults that preserve current behaviour.
- `join_scaffolds` defaults to `0` / `FALSE` everywhere except `new_test_project_userAsmb()`.
- Nextflow module files must pass `nextflow lint`. `inst/nextflow/main.nf` has one pre-existing lint error (`import groovy.transform.*` at line 3); that count must not increase.
- Run the R test suite with `Rscript -e 'devtools::test()'` from the repo root. Baseline before this work: `FAIL 0 | PASS 1665 | SKIP 23`.

---

## File Structure

**Modified:**

- `R/init_db_userAsmb.R` — adds the `join_scaffolds` column and argument.
- `R/init_project_userAsmb.R` — threads `join_scaffolds` through to the database.
- `R/init_test_project_userAsmb.R` — turns the toggle on for the test project, adds the ninth sample to the docs.
- `inst/nextflow/modules/coverage_userAsmb_workflow.nf` — produces the four new channels.
- `inst/nextflow/main.nf` — wires `SCAFFOLD_JOIN` into `WF1_userAsmb`.
- `R/app_assemble_userAsmb.R` — redo action, `join_notes` column.
- `R/app_ui_userAsmb.R` — the Redo Scaffold Join button.
- `R/app_server_userAsmb.R` — the button's trigger.
- `R/app_assemble_utils_userAsmb.R` — selects `join_notes` for the table.
- `R/app_run_pipline_userAsmb.R` — counts queued redos in the launch modal.
- `inst/test_data/mapping_test_userAsmb.csv` — the `UA_MULTI_FRAG` row.

**Created:**

- `inst/test_data/assemblies/UA_MULTI_FRAG.fasta` — the fixture assembly.
- `tools/make_ua_multi_frag.R` — the script that generates it, kept so the fixture is reproducible.
- `tests/testthat/test-userasmb-scaffold-join.R` — tests for the new schema and the guards the design relies on.

---

### Task 1: `join_scaffolds` option on the user-assembly schema

The user-assembly `assemble_opts` table has no `join_scaffolds` column, so nothing can gate the join. Old projects already get the column from the shared migration at `R/backwards_compatibility.R:426-430`; this is only for newly created ones.

**Files:**
- Modify: `R/init_db_userAsmb.R:354-370` (table definition and seed row), `R/init_db_userAsmb.R:75-127` (function arguments)
- Modify: `R/init_project_userAsmb.R:65-83` (arguments), `R/init_project_userAsmb.R:174-185` (call site)
- Test: `tests/testthat/test-userasmb-scaffold-join.R`

**Interfaces:**
- Consumes: nothing from earlier tasks.
- Produces: `new_db_userAsmb(join_scaffolds = FALSE)` and `new_project_userAsmb(join_scaffolds = FALSE)`. The `assemble_opts` table gains an integer `join_scaffolds` column, `0` or `1`. Task 2 reads it in SQL as `opts.join_scaffolds`; Task 4's redo action reads it via dplyr.

- [ ] **Step 1: Write the failing test**

Create `tests/testthat/test-userasmb-scaffold-join.R`:

```r
test_that("new_db_userAsmb creates the join_scaffolds toggle, off by default", {
  d <- withr::local_tempdir()
  mapping <- file.path(d, "mapping.csv")
  utils::write.csv(
    data.frame(ID = "S1", Taxon = "Conger oceanicus", Assembly = "S1.fasta",
               Topology = "linear", R1 = "S1_R1.fastq.gz", R2 = "S1_R2.fastq.gz"),
    mapping, row.names = FALSE
  )
  db <- file.path(d, ".sqlite")
  new_db_userAsmb(db_path = db, mapping_fn = mapping)

  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_true("join_scaffolds" %in% DBI::dbListFields(con, "assemble_opts"))
  expect_equal(
    DBI::dbGetQuery(con, "SELECT join_scaffolds FROM assemble_opts")$join_scaffolds,
    0L
  )
})

test_that("new_db_userAsmb honours join_scaffolds = TRUE", {
  d <- withr::local_tempdir()
  mapping <- file.path(d, "mapping.csv")
  utils::write.csv(
    data.frame(ID = "S1", Taxon = "Conger oceanicus", Assembly = "S1.fasta",
               Topology = "linear", R1 = "S1_R1.fastq.gz", R2 = "S1_R2.fastq.gz"),
    mapping, row.names = FALSE
  )
  db <- file.path(d, ".sqlite")
  new_db_userAsmb(db_path = db, mapping_fn = mapping, join_scaffolds = TRUE)

  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_equal(
    DBI::dbGetQuery(con, "SELECT join_scaffolds FROM assemble_opts")$join_scaffolds,
    1L
  )
})
```

- [ ] **Step 2: Run the test to verify it fails**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-userasmb-scaffold-join.R")'`

Expected: FAIL. The first test fails on `"join_scaffolds" %in% DBI::dbListFields(...)`; the second fails with an unused-argument error.

- [ ] **Step 3: Add the argument to `new_db_userAsmb()`**

In `R/init_db_userAsmb.R`, in the argument list, immediately after the `min_assembly_length = 500,` line and its two comment lines (currently line 106):

```r
    # Scaffold joining: order a fragmented single-path assembly against its BLAST
    # reference into one Path 0. Off by default, as in the regular pipeline.
    join_scaffolds = FALSE,
```

- [ ] **Step 4: Add the column and seed it**

In the same file, change the `assemble_opts` table definition (currently lines 354-358) to:

```r
    "CREATE TABLE assemble_opts (
      assemble_opts TEXT NOT NULL,
      min_assembly_length INTEGER,
      join_scaffolds INTEGER,
      PRIMARY KEY (assemble_opts)
    );"
```

and the seed row (currently lines 360-368) to:

```r
  dplyr::tbl(con, "assemble_opts") |>
    dplyr::rows_upsert(
      data.frame(
        assemble_opts = "user",
        min_assembly_length = min_assembly_length,
        join_scaffolds = as.integer(isTRUE(join_scaffolds))
      ),
      in_place = TRUE,
      copy = TRUE,
      by = "assemble_opts"
    )
```

- [ ] **Step 5: Thread it through `new_project_userAsmb()`**

In `R/init_project_userAsmb.R`, add to the argument list immediately after `attempt_circularization = FALSE,` (currently line 75):

```r
    join_scaffolds = FALSE,
```

Add the roxygen entry immediately after the `@param attempt_circularization` block (currently near line 32):

```r
#' @param join_scaffolds (logical) Order a fragmented single-path assembly
#'   against its BLAST reference into one joined sequence during WF1 (default =
#'   FALSE). Samples whose contigs match different reference mitogenomes are
#'   left alone.
```

In the `new_db_userAsmb()` call (currently lines 174-185), add after the `attempt_circularization = attempt_circularization,` line:

```r
    join_scaffolds = join_scaffolds,
```

- [ ] **Step 6: Run the test to verify it passes**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-userasmb-scaffold-join.R")'`

Expected: PASS, 2 tests.

- [ ] **Step 7: Confirm nothing else broke**

Run: `Rscript -e 'devtools::test()'`

Expected: `FAIL 0`. Pay particular attention to `test-init-db-userasmb.R`, `test-backwards-compatibility.R` and `test-project-consistency.R`, which read this schema.

- [ ] **Step 8: Regenerate docs and commit**

```bash
Rscript -e 'devtools::document()'
git add R/init_db_userAsmb.R R/init_project_userAsmb.R man/ tests/testthat/test-userasmb-scaffold-join.R
git commit -m "feat: add join_scaffolds option to user-assembly projects"
```

---

### Task 2: Join channels from the user-assembly coverage workflow

`SCAFFOLD_JOIN` takes three channels that nothing on this path produces. They are built in the coverage workflow because that is where per-sample path and scaffold counts are already computed for the `assemble` row, so eligibility cannot drift from what the table says.

**Files:**
- Modify: `inst/nextflow/modules/coverage_userAsmb_workflow.nf`

**Interfaces:**
- Consumes: `assemble_opts.join_scaffolds` from Task 1.
- Produces four new named emits on both `COVERAGE_userAsmb` and `COVERAGE_userAsmb_noReads`:
  - `cov_files` — `tuple(id, [coverageStats csv files])`
  - `join_eligible` — `tuple(id, assembly_fasta, opts_id, join_scaffolds)`
  - `join_expected` — `id` (a bare value, not a tuple)
  - `join_redo` — `tuple(id, assemble_opts, join_scaffolds, blast_accession)`

  These are exactly the shapes `main.nf` needs in Task 3 and `SCAFFOLD_JOIN` already takes.

- [ ] **Step 1: Extend the sample query with the two option columns**

In `inst/nextflow/modules/coverage_userAsmb_workflow.nf`, replace the `params.sqlRead` definition (lines 5-18) with:

```groovy
params.sqlRead =  'SELECT s.ID, s.assembly, s.topology, ' +
                  'a.assemble_opts, opts.min_assembly_length, ' +
                  'copts.attempt, copts.min_overlap, copts.min_identity, ' +
                  'copts.min_junction_reads, copts.min_overhang, copts.cpus, copts.memory, ' +
                  's.genetic_code, ' +
                  'fopts.attempt, fopts.mitofinder_db, fopts.min_contig_length, ' +
                  'fopts.min_identity, fopts.min_aligned_length, fopts.min_aligned_fraction, ' +
                  'fopts.max_candidates, fopts.min_genes, fopts.cpus, fopts.memory, ' +
                  'opts.join_scaffolds, bopts.run_blast ' +
                  'FROM samples s ' +
                  'JOIN assemble a ON s.ID = a.ID ' +
                  'JOIN assemble_opts opts ON a.assemble_opts = opts.assemble_opts ' +
                  'LEFT JOIN circularize_opts copts ON a.circularize_opts = copts.circularize_opts ' +
                  'LEFT JOIN find_mito_opts fopts ON a.find_mito_opts = fopts.find_mito_opts ' +
                  'LEFT JOIN blast_opts bopts ON a.blast_opts = bopts.blast_opts ' +
                  'WHERE a.assemble_switch IN (1, 4) AND a.assemble_lock = 0'
```

`run_blast` is needed because a fragmented sample with BLAST switched off never gets a fetched reference. Without it, that sample would be reported as a join failure for a missing input it was never going to have.

- [ ] **Step 2: Add the redo query and the switch-clearing statement**

In the same file, immediately after the `params.sqlWriteAssemble` definition (currently ends line 58), add:

```groovy
// Samples the app queued for a join-only redo (assemble.join_switch = 1). Read
// separately from params.sqlRead because a redo sample sits at state 2 or 3 and
// that query only admits 1 and 4.
params.sqlReadJoinRedoUserAsmb =
    'SELECT a.ID, a.assemble_opts, COALESCE(opts.join_scaffolds, 0), ' +
    'COALESCE(a.join_switch, 0), COALESCE(a.assemble_switch, 0), a.blast_accession ' +
    'FROM assemble a ' +
    'JOIN assemble_opts opts ON a.assemble_opts = opts.assemble_opts ' +
    'WHERE a.join_switch = 1 AND a.assemble_lock = 0'

// A redo this run will not service must not leave the request queued forever.
params.sqlClearJoinSwitchUserAsmb = 'UPDATE assemble SET join_switch = NULL WHERE ID = ?'
```

- [ ] **Step 3: Give the shared writer the new inputs and emits**

In `workflow COVERAGE_userAsmb_WRITE`, change the `take:` block (currently lines 66-69) to:

```groovy
    take:
        coverage_out
        min_len_lookup
        min_len_summary
        join_lookup
        run_blast_lookup
```

Then find the `assemblies_ch` summary block that ends in
`.sqlInsert(statement: params.sqlWriteAssemble , db: 'sqlite')`. Change that final line to capture the channel first:

```groovy
            .set { assemble_summary }

        assemble_summary.sqlInsert(statement: params.sqlWriteAssemble , db: 'sqlite')

        // Join eligibility comes off the same summary the assemble row is built
        // from, so the table and the join can never disagree about how many
        // paths and scaffolds a sample has.
        assemble_summary
            .map { paths, scaffolds, length_str, topo_str, status, notes, ts, id ->
                tuple(id, paths as Integer, scaffolds as Integer, status)
            }
            .filter { id, n_paths, n_scaffolds, status ->
                status == '4' && n_paths == 1 && n_scaffolds > 1
            }
            .join(coverage_out.map { files, wd, id, fasta, opts -> tuple(id, fasta, opts) })
            .join(join_lookup)
            .join(run_blast_lookup)
            .set { join_eligible_meta }
```

`join_eligible_meta` is `tuple(id, n_paths, n_scaffolds, status, fasta, opts, join_scaffolds, run_blast)`.

- [ ] **Step 4: Build the redo channel in the shared writer**

Immediately after the block added in Step 3, still inside `COVERAGE_userAsmb_WRITE`, add:

```groovy
        // Join-only redo requests. States 1 and 4 are being reprocessed by this
        // same run and reach the join by the normal route, so servicing them
        // here would feed the join twice for one sample.
        channel.fromQuery(params.sqlReadJoinRedoUserAsmb, db: 'sqlite')
            .filter { id, opts, join_scaffolds, join_switch, assemble_switch, blast_accession ->
                join_switch == 1
            }
            .branch { id, opts, join_scaffolds, join_switch, assemble_switch, blast_accession ->
                moot: assemble_switch == 1 || assemble_switch == 4
                redo: true
            }
            .set { redo_branch }

        redo_branch.moot
            .map { id, opts, join_scaffolds, join_switch, assemble_switch, blast_accession ->
                tuple(id)
            }
            .sqlInsert(statement: params.sqlClearJoinSwitchUserAsmb, db: 'sqlite')
```

- [ ] **Step 5: Add the emits to the shared writer**

Replace the `emit:` block of `COVERAGE_userAsmb_WRITE` (currently the two lines ending `blast_in = coverage_out.map{ it -> tuple(it[2], it[3], it[4]) }`) with:

```groovy
    emit:
        // tuple(id, assembly, opts_id) for single-contig BLAST search
        blast_in = coverage_out.map{ it -> tuple(it[2], it[3], it[4]) }

        // Per-ID coverageStats CSV files, for the scaffold join to stitch.
        cov_files = coverage_out
            .map { files, wd, id, fasta, opts ->
                def fl = (files instanceof List) ? files : [files]
                tuple(id, fl.findAll { it.name ==~ /.*coverageStats\.csv/ })
            }
            .filter { id, csvs -> csvs.size() > 0 }
            .groupTuple()
            .map { id, lists -> tuple(id, lists.flatten()) }

        // Single-path multi-scaffold samples eligible for the join, carrying the
        // per-sample join_scaffolds toggle. The mapping precompute runs for ALL
        // eligible samples; the toggle only gates the automatic Path 0 build.
        join_eligible = join_eligible_meta
            .map { id, np, ns, status, fasta, opts, join_scaffolds, run_blast ->
                tuple(id, fasta, opts, join_scaffolds)
            }

        // IDs expected to reach the join in THIS run, withheld from the
        // reference fetch's 4 -> 2 promotion so the join owns their final state.
        // Samples with BLAST switched off never get a fetched reference, so they
        // are excluded: withholding them would strand them, and reporting them
        // would call a missing input a failure.
        join_expected = join_eligible_meta
            .filter { id, np, ns, status, fasta, opts, join_scaffolds, run_blast ->
                (run_blast == null ? 1 : (run_blast as Integer)) == 1
            }
            .map { id, np, ns, status, fasta, opts, join_scaffolds, run_blast -> id }

        // tuple(id, assemble_opts, join_scaffolds, blast_accession)
        join_redo = redo_branch.redo
            .map { id, opts, join_scaffolds, join_switch, assemble_switch, blast_accession ->
                tuple(id, opts, join_scaffolds, blast_accession)
            }
```

- [ ] **Step 6: Feed and re-export from `COVERAGE_userAsmb`**

In `workflow COVERAGE_userAsmb`, add two entries to the `multiMap` block, immediately after the `min_len_summary:` line:

```groovy
                join_lookup:       tuple(it[0], it[23] == null ? 0 : (it[23] as Integer))
                run_blast_lookup:  tuple(it[0], it[24] == null ? 1 : (it[24] as Integer))
```

Then change the `COVERAGE_userAsmb_WRITE` call and the `emit:` block at the end of that workflow to:

```groovy
        COVERAGE_userAsmb_WRITE(coverage_out, min_len_lookup, min_len_summary,
                                query_ch.join_lookup, query_ch.run_blast_lookup)

    emit:
        blast_in      = COVERAGE_userAsmb_WRITE.out.blast_in
        cov_files     = COVERAGE_userAsmb_WRITE.out.cov_files
        join_eligible = COVERAGE_userAsmb_WRITE.out.join_eligible
        join_expected = COVERAGE_userAsmb_WRITE.out.join_expected
        join_redo     = COVERAGE_userAsmb_WRITE.out.join_redo
```

- [ ] **Step 7: Do the same for `COVERAGE_userAsmb_noReads`**

Make the identical two `multiMap` additions and the identical call and `emit:` replacement in `workflow COVERAGE_userAsmb_noReads`. Both variants must emit the same names so `main.nf` cannot tell them apart.

- [ ] **Step 8: Lint**

Run: `nextflow lint inst/nextflow/modules/coverage_userAsmb_workflow.nf`

Expected: no errors for this file. Warnings are acceptable; the repo's modules already carry them.

- [ ] **Step 9: Commit**

```bash
git add inst/nextflow/modules/coverage_userAsmb_workflow.nf
git commit -m "feat: emit scaffold join channels from user-assembly coverage"
```

---

### Task 3: Wire `SCAFFOLD_JOIN` into `WF1_userAsmb`

**Files:**
- Modify: `inst/nextflow/main.nf:87-104`

**Interfaces:**
- Consumes: `cov_files`, `join_eligible`, `join_expected`, `join_redo` from Task 2; `SCAFFOLD_JOIN(input, dropped, redo)` unchanged.
- Produces: nothing later tasks read.

- [ ] **Step 1: Replace the body of `WF1_userAsmb`**

In `inst/nextflow/main.nf`, replace the whole `workflow WF1_userAsmb { ... }` block with:

```groovy
// ASSEMBLY WORKFLOW - user provided assemblies
workflow WF1_userAsmb {

    // No-reads projects skip PREPROCESS entirely and pull samples straight from
    // the DB; read-based projects preprocess then map reads for coverage. Either
    // path emits the same channels, so everything below is invoked once.
    if (params.noRawData) {
        COVERAGE_userAsmb_noReads()
        cov = COVERAGE_userAsmb_noReads.out
    } else {
        PREPROCESS()
        COVERAGE_userAsmb(PREPROCESS.out[0])
        cov = COVERAGE_userAsmb.out
    }

    BLAST_GENBANK(cov.blast_in)
    // Join-eligible samples are withheld from the reference fetch's 4 -> 2
    // promotion: SCAFFOLD_JOIN owns their final state.
    BLAST_REF_FETCH(BLAST_GENBANK.out.ref_input, BLAST_GENBANK.out.scaffold_map,
                    BLAST_GENBANK.out.ref_batches, cov.join_expected)

    // remainder: true rather than plain inner joins, so a sample whose coverage
    // or reference fetch failed is reported instead of silently vanishing while
    // still counting as a success.
    cov.join_eligible
        .join(cov.cov_files, remainder: true)
        .join(BLAST_REF_FETCH.out.ref_seq, remainder: true)
        .join(BLAST_GENBANK.out.scaffold_hits, remainder: true)
        .branch { row ->
            complete:   !row.contains(null)
            incomplete: true
        }
        .set { join_rows }

    // Restrict to IDs actually expected to reach the join before reporting
    // anything as failed. The filter comes FIRST: a remainder row for an ID with
    // no left-hand entry is emitted shorter than the full tuple, so the
    // positions below are only safe once the row is known to be join_eligible.
    join_rows.incomplete
        .join(cov.join_expected.map { id -> tuple(id, true) })
        .map { row ->
            def missing = []
            if (row[4] == null) missing << 'coverage statistics'
            if (row[5] == null) missing << 'the BLAST reference sequence'
            if (row[6] == null) missing << 'scaffold BLAST hits'
            tuple(row[0], missing.join(', '))
        }
        .set { join_dropped }

    SCAFFOLD_JOIN(join_rows.complete, join_dropped, cov.join_redo)

}
```

- [ ] **Step 2: Lint**

Run: `nextflow lint inst/nextflow/main.nf`

Expected: exactly 1 error, the pre-existing `Unexpected input: '*'` at line 3 for `import groovy.transform.*`. Any second error is a regression introduced by this task.

- [ ] **Step 3: Commit**

```bash
git add inst/nextflow/main.nf
git commit -m "feat: run the scaffold join in the user-assembly workflow"
```

---

### Task 4: App plumbing for the join

Without this the redo channel has no producer and a declined or failed join writes `join_notes` that the user-assembly Assemble table never shows.

**Files:**
- Modify: `R/app_ui_userAsmb.R:31-48` (the `asmb_ctrls` div)
- Modify: `R/app_server_userAsmb.R:168-176` (trigger observers)
- Modify: `R/app_assemble_userAsmb.R:5-13` (column groups), `R/app_assemble_userAsmb.R` (colDef list, and a new `on("redo_join", ...)` observer)
- Modify: `R/app_assemble_utils_userAsmb.R:85-105` (the select list)
- Modify: `R/app_run_pipline_userAsmb.R:53-57` (launch count)

**Interfaces:**
- Consumes: `assemble_opts.join_scaffolds` from Task 1; `assemble.join_switch` and `assemble.join_notes`, which already exist in the schema.
- Produces: sets `assemble.join_switch = 1`, which Task 2's `join_redo` query reads.

- [ ] **Step 1: Add the button**

In `R/app_ui_userAsmb.R`, inside the `div(id = "asmb_ctrls", ...)` block, between the `"lock"` button and the `"run_modal"` button, add:

```r
            shinyWidgets::actionBttn(
              "redo_join",
              label = "Redo Scaffold Join",
              style = "material-flat",
              size = "sm"
            ),
```

- [ ] **Step 2: Add the trigger**

In `R/app_server_userAsmb.R`, immediately after the observer that calls `trigger("lock")`, add:

```r
  # Redo Scaffold Join
  observeEvent(input$redo_join, {
    trigger("redo_join")
  })
```

- [ ] **Step 3: Add `join_notes` to the table query**

In `R/app_assemble_utils_userAsmb.R`, in the `dplyr::select()` list, add `join_notes,` immediately after `find_mito_notes` (it is currently the last entry, so add a comma to that line first).

- [ ] **Step 4: Add `join_notes` to the column group and give it a colDef**

In `R/app_assemble_userAsmb.R`, change the `Metadata` entry of `ASSEMBLE_COL_GROUPS_USERASMB` to:

```r
  Metadata = c("time_stamp", "assemble_notes", "circularize_notes",
               "find_mito_notes", "join_notes")
```

In the `colDef` list, immediately after the `circularize_notes = colDef(...)` entry, add:

```r
            join_notes = colDef(
              show = TRUE, class = .grp("join_notes"), headerClass = .grp("join_notes"),
              name = "Scaffold Join Notes",
              html = TRUE,
              align = "left",
              minWidth = 150,
              cell = rt_longtext()
            ),
```

- [ ] **Step 5: Add the redo observer**

In `R/app_assemble_userAsmb.R`, inside the same `moduleServer` body that holds the other `on(...)` handlers, add the observer below. It is the regular pipeline's handler from `R/app_assemble.R:662-775`, unchanged except that it lives in this module.

```r
    # Redo Scaffold Join ----
    # Narrower than Set State: only queues the join (join_switch = 1),
    # assemble_switch is left alone so this never re-enters assembly.
    init("redo_join")
    on("redo_join", {
      req(session$userData$mode == "Assemble")
      req(selected())
      req(all(rv$data$assemble_lock[req(selected())] == 0))
      ids <- unique(rv$data$ID[selected()])

      asmb <- dplyr::tbl(session$userData$con, "assemblies") |>
        dplyr::filter(ID %in% ids, path > 0) |>
        dplyr::select(ID, path, scaffold) |>
        dplyr::collect()
      stale <- tryCatch(
        stale_assemble_dirs(
          session$userData$con,
          session$userData$dir_out,
          ids = ids,
          pending_only = FALSE
        ),
        error = function(e) NULL
      )
      missing_ids <- if (!is.null(stale)) stale$ID else character(0)
      toggles <- tryCatch(
        dplyr::tbl(session$userData$con, "assemble") |>
          dplyr::filter(ID %in% ids) |>
          dplyr::select(ID, assemble_opts, blast_accession) |>
          dplyr::inner_join(
            dplyr::tbl(session$userData$con, "assemble_opts") |>
              dplyr::select(assemble_opts, join_scaffolds),
            by = "assemble_opts"
          ) |>
          dplyr::collect(),
        error = function(e) NULL
      )
      join_off_ids <- if (is.null(toggles) || nrow(toggles) == 0L) {
        character(0)
      } else {
        off <- is.na(toggles$join_scaffolds) | toggles$join_scaffolds == 0
        unique(toggles$ID[off])
      }
      no_ref_ids <- redo_join_no_ref_ids(ids, toggles)
      plan <- redo_join_plan(ids, asmb, missing_ids, join_off_ids, no_ref_ids)

      if (length(plan$not_eligible) > 0 || length(plan$missing_output) > 0 ||
          length(plan$join_off) > 0 || length(plan$no_ref) > 0) {
        shinyWidgets::sendSweetAlert(
          title = "Redo scaffold join not queued for some samples",
          text = shiny::tags$div(
            if (length(plan$not_eligible) > 0) {
              shiny::tags$p(
                "Not join-eligible (need exactly one assembler path fragmented ",
                "into more than one scaffold): ",
                tags$code(paste(plan$not_eligible, collapse = ", "))
              )
            },
            if (length(plan$missing_output) > 0) {
              shiny::tags$div(
                shiny::tags$p(
                  "Published assembly output is not on disk for a redo to use:"
                ),
                shiny::tags$ul(stale_assemble_items(
                  stale[stale$ID %in% plan$missing_output, , drop = FALSE]
                ))
              )
            },
            if (length(plan$join_off) > 0) {
              shiny::tags$p(
                "Scaffold joining is switched off in the assembly parameter ",
                "set, so a redo would report the sample as skipped and mark it ",
                "done. Turn 'join_scaffolds' on first for: ",
                tags$code(paste(plan$join_off, collapse = ", "))
              )
            },
            if (length(plan$no_ref) > 0) {
              shiny::tags$p(
                "No BLAST reference was ever selected, so the join has nothing ",
                "to align the scaffolds against. Set a reference (or run BLAST) ",
                "first for: ",
                tags$code(paste(plan$no_ref, collapse = ", "))
              )
            },
            shiny::tags$p(
              if (length(plan$ready) > 0) {
                "The rest of the selected samples were queued for a join redo."
              } else {
                "No samples were queued."
              }
            )
          ),
          html = TRUE,
          type = if (length(plan$ready) > 0) "warning" else "error"
        )
      }
      req(length(plan$ready) > 0)

      upd <- data.frame(ID = plan$ready, join_switch = 1L, stringsAsFactors = FALSE)
      dplyr::tbl(session$userData$con, "assemble") |>
        dplyr::rows_update(
          upd,
          unmatched = "ignore",
          in_place = TRUE,
          copy = TRUE,
          by = "ID"
        )
      rv$data <- rv$data |> dplyr::rows_update(upd, by = "ID")
      trigger("update_assemble_table")
    })
```

Before writing it, confirm the names this module actually uses for the selection reactive and the data store. Run:

```bash
grep -n 'selected()\|rv\$data\|init("\|on("' R/app_assemble_userAsmb.R | head -30
```

If the module names them differently, adapt the handler to the local names rather than introducing new ones.

- [ ] **Step 6: Count queued redos in the launch modal**

In `R/app_run_pipline_userAsmb.R`, replace the Assemble branch filter (currently `dplyr::filter(assemble_switch == 1)`) with:

```r
          # join_switch = 1 is a join-only redo: WF1 admits it on its own
          # regardless of assemble_switch, so it counts as work to be done.
          dplyr::filter(assemble_switch == 1 |
                          (join_switch == 1 & assemble_lock == 0)) |>
```

- [ ] **Step 7: Verify the app still loads and the suite passes**

Run:

```bash
Rscript -e 'devtools::load_all("."); invisible(ASSEMBLE_COL_GROUPS_USERASMB)'
Rscript -e 'devtools::test()'
```

Expected: loads without error, `FAIL 0`. `test-userasmb-app-units.R` exercises this module, so watch it specifically.

- [ ] **Step 8: Commit**

```bash
git add R/app_ui_userAsmb.R R/app_server_userAsmb.R R/app_assemble_userAsmb.R R/app_assemble_utils_userAsmb.R R/app_run_pipline_userAsmb.R
git commit -m "feat: queue and report scaffold joins in the user-assembly app"
```

---

### Task 5: `UA_MULTI_FRAG` test sample

A ninth sample holding one mitogenome split across three contigs, which is the case the whole change exists to handle and which no existing sample covers.

**Files:**
- Create: `tools/make_ua_multi_frag.R`
- Create: `inst/test_data/assemblies/UA_MULTI_FRAG.fasta`
- Modify: `inst/test_data/mapping_test_userAsmb.csv`
- Modify: `R/init_test_project_userAsmb.R:1-8` (roxygen), `R/init_test_project_userAsmb.R:99-116` (the `new_project_userAsmb()` call)

**Interfaces:**
- Consumes: `new_project_userAsmb(join_scaffolds = )` from Task 1.
- Produces: a fixture read by Task 6's tests and by `test-new-test-project-userAsmb.R`.

- [ ] **Step 1: Write the fixture generator**

Create `tools/make_ua_multi_frag.R`:

```r
# Builds inst/test_data/assemblies/UA_MULTI_FRAG.fasta: the UA_MULTI_ONE decoy
# scaffolds plus one mitogenome cut into three pieces, written out of order with
# the middle piece reverse-complemented. Out of order and flipped on purpose: a
# clean in-order split would pass even if the join only concatenated.
# Run from the repo root: Rscript tools/make_ua_multi_frag.R

src <- Biostrings::readDNAStringSet("inst/test_data/assemblies/UA_MULTI_ONE.fasta")
mito <- src[names(src) == "mito_contig"]
decoys <- src[names(src) != "mito_contig"]
stopifnot(length(mito) == 1L)

n <- BiocGenerics::width(mito)[1]
cuts <- round(seq(0, n, length.out = 4))
pieces <- Biostrings::DNAStringSet(lapply(seq_len(3), function(i) {
  Biostrings::subseq(mito[[1]], start = cuts[i] + 1, end = cuts[i + 1])
}))
names(pieces) <- paste0("mito_contig_", seq_len(3))

# Middle piece flipped, so the join has to detect and correct the orientation.
pieces[[2]] <- Biostrings::reverseComplement(pieces[[2]])

# Written 3, 1, 2 so file order carries no information about assembly order.
out <- c(pieces[3], decoys[1:50], pieces[1], decoys[51:100], pieces[2])
Biostrings::writeXStringSet(out, "inst/test_data/assemblies/UA_MULTI_FRAG.fasta")

cat("wrote", length(out), "contigs; piece widths:",
    paste(BiocGenerics::width(pieces), collapse = ", "), "\n")
```

- [ ] **Step 2: Generate the fixture**

Run: `Rscript tools/make_ua_multi_frag.R`

Expected: `wrote 103 contigs; piece widths: 6008, 6008, 6008`

- [ ] **Step 3: Add the mapping row**

Append to `inst/test_data/mapping_test_userAsmb.csv`:

```
UA_MULTI_FRAG,Conger oceanicus,UA_MULTI_FRAG_R1.fastq.gz,UA_MULTI_FRAG_R2.fastq.gz,UA_MULTI_FRAG.fasta,linear,SRR22396843,multi-contig assembly with one mitogenome split across 3 contigs -> found then joined
```

The `Expected` column must not contain a comma; the file is plain unquoted CSV.

- [ ] **Step 4: Turn the toggle on for the test project**

In `R/init_test_project_userAsmb.R`, in the `new_project_userAsmb()` call, add after `attempt_circularization = TRUE,`:

```r
    join_scaffolds = TRUE,
```

In the same file's roxygen header, change "eight samples" to "nine samples" and extend the sentence describing the multi-contig samples so it mentions the fragmented one. The current text reads "and five multi-contig assemblies holding one, two, or no mitogenomes"; change it to "and six multi-contig assemblies holding one, two, none, or one split across three contigs".

- [ ] **Step 5: Verify the fixture is what the pipeline will see**

Run:

```bash
Rscript -e '
x <- Biostrings::readDNAStringSet("inst/test_data/assemblies/UA_MULTI_FRAG.fasta")
cat("contigs:", length(x), "\n")
cat("mito pieces:", paste(grep("^mito_contig", names(x), value = TRUE), collapse = ", "), "\n")
cat("total mito bp:", sum(BiocGenerics::width(x)[grepl("^mito_contig", names(x))]), "\n")'
```

Expected: 103 contigs; the three mito pieces present but not adjacent in file order; total mito bp 18024, matching the original `mito_contig`.

- [ ] **Step 6: Run the existing fixture test**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-new-test-project-userAsmb.R")'`

Expected: PASS. It checks every mapping row has a shipped assembly file and a resolvable donor, so it covers the new row automatically.

- [ ] **Step 7: Commit**

```bash
Rscript -e 'devtools::document()'
git add tools/make_ua_multi_frag.R inst/test_data/assemblies/UA_MULTI_FRAG.fasta inst/test_data/mapping_test_userAsmb.csv R/init_test_project_userAsmb.R man/
git commit -m "test: add UA_MULTI_FRAG sample with a mitogenome in three contigs"
```

---

### Task 6: Tests for the guards the design rests on

The decision not to add a contamination gate rests entirely on `scaffold_hits_disagree()` declining a sample whose contigs match different references. That behaviour is currently untested for this shape, so the design has an untested load-bearing assumption.

**Files:**
- Modify: `tests/testthat/test-userasmb-scaffold-join.R`

**Interfaces:**
- Consumes: `select_mito_contigs()`, `scaffold_join_eligible()`, `scaffold_hits_disagree()`, all already exported or internal in the package.
- Produces: nothing.

- [ ] **Step 1: Write the failing tests**

Append to `tests/testthat/test-userasmb-scaffold-join.R`:

```r
test_that("a mitogenome in three same-reference pieces is kept whole", {
  # The UA_MULTI_FRAG shape: three ~6 kb pieces, one reference.
  hits <- do.call(rbind, lapply(1:3, function(i) {
    data.frame(qseqid = paste0("mito_contig_", i), saccver = "NC_083079.1",
               pident = 99, length = 6000, bitscore = 11000, qlen = 6008)
  }))
  res <- select_mito_contigs(hits)
  expect_equal(res$accession, "NC_083079.1")
  expect_length(res$candidates, 3L)
})

test_that("three contigs of one path are join-eligible", {
  asmb <- data.frame(ID = "UA_MULTI_FRAG", path = c(1, 1, 1), scaffold = 1:3)
  expect_true(scaffold_join_eligible(asmb))
})

test_that("a single-contig sample is not join-eligible", {
  expect_false(scaffold_join_eligible(data.frame(ID = "S", path = 1, scaffold = 1)))
})

test_that("contigs matching different references cancel the automatic join", {
  # This is the guard the user-assembly path relies on instead of a
  # contamination gate of its own: UA_MULTI_TWO must decline, not join.
  contaminated <- data.frame(
    scaffold = 1:2,
    blast_accession = c("NC_083079.1", "NC_083028.1")
  )
  expect_true(scaffold_hits_disagree(contaminated))

  fragmented <- data.frame(
    scaffold = 1:3,
    blast_accession = rep("NC_083079.1", 3)
  )
  expect_false(scaffold_hits_disagree(fragmented))
})

test_that("a scaffold with no BLAST hit is not a second opinion", {
  df <- data.frame(scaffold = 1:2,
                   blast_accession = c("NC_083079.1", "NO HIT"))
  expect_false(scaffold_hits_disagree(df))
})
```

- [ ] **Step 2: Run them**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-userasmb-scaffold-join.R")'`

Expected: all PASS. These describe behaviour that already exists; they are here to pin it down, so a failure means the design's assumption is wrong and Task 3 must be reconsidered before going further. If `scaffold_hits_disagree` or `scaffold_join_eligible` are not visible, load them with `MitoPilot:::` rather than exporting them.

- [ ] **Step 3: Run the full suite**

Run: `Rscript -e 'devtools::test()'`

Expected: `FAIL 0`, total passes at least 1665 plus the new tests.

- [ ] **Step 4: Commit**

```bash
git add tests/testthat/test-userasmb-scaffold-join.R
git commit -m "test: pin the guards the user-assembly join relies on"
```

---

### Task 7: End-to-end verification

Needs Docker, BLAST and minimap2, so it is a manual run rather than a test.

**Files:** none modified.

- [ ] **Step 1: Rebuild the image**

Run: `bash docker/deploy-local.sh userAsmb`

Expected: `Local image build successfull!` and `macguigand/mitopilot:userAsmb` pointing at a fresh image ID.

- [ ] **Step 2: Create a clean test project**

```r
MitoPilot::new_test_project_userAsmb(
  path = "<a new empty directory>",
  container = "macguigand/mitopilot:userAsmb",
  Rproj = FALSE
)
```

A fresh directory matters: Nextflow's `find_mito_pick` and `scaffold_join` tasks hash on their inputs, so resuming an old run would replay cached results.

- [ ] **Step 3: Run WF1 and check the outcomes**

Launch the Assemble workflow, then read the `assemble` table:

```r
con <- DBI::dbConnect(RSQLite::SQLite(), ".sqlite")
DBI::dbGetQuery(con, "SELECT ID, paths, scaffolds, assemble_switch, find_mito_notes, join_notes FROM assemble ORDER BY ID")
DBI::dbGetQuery(con, "SELECT ID, path, scaffold, length, ignore FROM assemblies ORDER BY ID, path, scaffold")
```

Expected:

- `UA_MULTI_FRAG` — search keeps 3 contigs against one accession, join builds a Path 0 of roughly 18024 bp, the three original scaffolds carry `ignore = 1`, and annotation sees one unit.
- `UA_MULTI_TWO` and `UA_MULTI_MIXED` — `join_notes` reads "the scaffolds matched different reference mitogenomes (...), so they were left separate for review", no Path 0 row exists, and both mitogenome contigs stay at `ignore = 0` as two units.
- `UA_MULTI_ONE`, `UA_LINEAR`, `UA_CIRCULAR`, `UA_UNCIRC`, `UA_MULTI_UNCIRC` — one contig each, unchanged, `join_notes` empty.
- `UA_MULTI_NONE` — still fails the search, `assemble_switch = 3`, never reaches the join.

- [ ] **Step 4: Check the app**

Open the app on that project. Confirm the Assemble table shows a "Scaffold Join Notes" column carrying the decline note for the two contaminated samples, and that selecting `UA_MULTI_FRAG` and pressing "Redo Scaffold Join" queues it without a warning dialog.

- [ ] **Step 5: Report**

Summarise what each of the nine samples did against the `Expected` column of `mapping.csv`. Do not claim success for any sample whose row you did not read out of the database.

---

## Self-Review

**Spec coverage.** Every section of `tools/userasmb_scaffold_join_design.md` maps to a task: schema to Task 1; channels to Task 2; wiring to Task 3; app plumbing to Task 4; the test sample to Task 5; the unit tests to Task 6; the manual end-to-end checks to Task 7. The spec's "out of scope" items are touched by no task.

**One deviation from the spec, deliberate.** Task 2 also reads `blast_opts.run_blast` and uses it to filter `join_expected`. The spec did not mention it. Without it, a fragmented sample with BLAST switched off would be withheld from the reference fetch's 4 to 2 promotion, never receive a reference, and then be reported as a join failure for an input it was never going to have. The regular pipeline avoids this through its status 2 distinction, which does not exist at the same point on the user-assembly path. The spec should be read as including this.

**Placeholders.** None. Every code step carries the code to write; every run step carries the command and the expected output.

**Type consistency.** `join_eligible_meta` is `tuple(id, n_paths, n_scaffolds, status, fasta, opts, join_scaffolds, run_blast)` in Task 2 and is destructured with those eight names in both emits. `join_eligible` is `tuple(id, fasta, opts, join_scaffolds)`, which lands at positions 0 to 3 of the joined row in Task 3, so `row[4]`, `row[5]` and `row[6]` are coverage, reference and hits, matching both the `missing` messages and `SCAFFOLD_JOIN`'s declared input order. `join_redo` is `tuple(id, assemble_opts, join_scaffolds, blast_accession)`, matching `SCAFFOLD_JOIN`'s `redo` take. `join_scaffolds` is spelled identically in the schema, the SQL, the channels and the app.
