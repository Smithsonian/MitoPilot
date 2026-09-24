# Specimen Metadata (GEOME + GBIF) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Generalize the GEOME integration into one "specimen metadata" system with a `source` dimension, add GBIF occurrences as a second source, flag (never merge) disagreements between the user's CSV, GEOME, and GBIF, and replace the Export Data modal's flat column lists with grouped, clickable token chips.

**Architecture:** Storage moves from `geome_*` tables to source-keyed `meta_*` tables (migrated automatically), with one shared fetch core (`.meta_fetch_into()` and friends) driven by a small source registry (`META_SOURCES`). GBIF gets its own API client (`R/gbif_api.R`) and GenBank-ready combos (`R/gbif_export.R`) that plug into the same registry and export-key scheme. One reconciliation engine (`specimen_conflicts()` in `R/specimen_reconcile.R`) compares concepts across CSV, GEOME, and GBIF; the app's Specimen icon column, the viewer's Compare tab, and the export-time warning all read it. The Export Data modal renders `export_token_groups()` as chips that insert at the cursor.

**Tech Stack:** R, DBI/RSQLite, dplyr/dbplyr, httr2, jsonlite, htmltools, Shiny + reactable + shinyWidgets, gargoyle, testthat 3e + withr, jQuery (app JS in `inst/app/www/custom.js`).

**Spec:** `tools/specimen_metadata_gbif_spec.md` (binding; read it first). Prior spec: `tools/geome_metadata_spec.md`. GBIF API notes with real responses: `dev/gbif/gbif_api_research.md`.

## Global Constraints

- Work in the worktree `~/Documents/GitHub/MitoPilot-geome`, branch `geome-metadata`. Never push.
- ASCII only in R code and in `inst/extdata/countries.csv`. Tests that need accented input use `\uXXXX` escapes.
- Minimal comments; match surrounding style. No new package dependencies (httr2, jsonlite, htmltools are already in Imports; no rgbif).
- GBIF API base: `https://api.gbif.org/v1`. No auth. User agent `MitoPilot (https://github.com/Smithsonian/MitoPilot)`, 30 s timeout, retry on 429/5xx.
- GBIF error messages, verbatim: 404 -> `GBIF occurrence not found (IDs can change when a dataset is republished)`; other 4xx/5xx -> `GBIF returned HTTP <n>`; network -> `could not reach GBIF (...)`.
- Reserved samples columns: `GEOME_BCID`, `GBIF_ID`. Init args: `mapping_gbif = "GBIF_ID"`, `fetch_gbif = TRUE` (beside `mapping_geome`, `fetch_geome`) in `new_project()`, `new_project_userAsmb()`, `new_db()`, `new_db_userAsmb()`, `add_samples()`, `update_sample_metadata()`.
- Tables: `meta_records(ID, source, level, depth, ref, field, value)` PK (ID, source, depth, field); `meta_status(ID, source, ref, status, message, fetched_at)` PK (ID, source); `meta_export_fields(key)`; `meta_csv_map(concept, column)` PK concept.
- `source` values: `GEOME`, `GBIF`. GBIF levels: `Occurrence` (depth 0), `Dataset` (1), `Organization` (2). `issues` stored as one comma-joined field on the Occurrence level.
- Export keys: `geome:combo:<name>`, `geome:raw:<level>:<field>`, `gbif:combo:<name>`, `gbif:raw:<level>:<field>`. Columns: `geome_<name>`, `gbif_<name>`, `<prefix>_<Level>_<field>` (non `[A-Za-z0-9_]` replaced by `_`).
- Reconciliation flags only. MitoPilot never merges or picks a value, never ticks a field by default, never edits a template.
- App-only columns `specimen` and `specimen_message` are never exported, never offered as tokens, and never written to the summary CSV.
- All GEOME/GBIF calls run in the R session or app (driver side), never inside Nextflow.
- Module servers must not read reactives at module top level outside `isolate()`. The MockShinySession startup test (not `testServer`) must pass for all four panels after every app task.
- `shiny::testServer` here cannot drive `session$setInputValue` + `observeEvent`; click-throughs are verified in a real browser (Task 13).
- Vignette text: no em dashes, Oxford commas.
- Commit messages brief, no attribution lines.

## Review Focus

1. **GBIF IDs typed as numbers in the CSV:** `read.csv()` turns a GBIF column into doubles (blanks become `NA`); the stored value must be `"6186461308"`, never `"6.186461e+09"` or the string `"NA"` (Task 3 test).
2. **A project created on this branch before the change:** its `geome_*` tables hold fetched records and ticked fields; opening it in the app or exporting must migrate once, keep every ticked token resolving, and a second open must be a no-op (Task 1 test).
3. **GBIF records with bad coordinates or a URL/ARK catalog number:** `gbif_lat_lon` / `gbif_specimen_voucher` must be empty, and reconciliation must treat the blank as missing, never as a conflict (Tasks 4 and 6 tests).
4. **The same country written differently per source:** `USA: Florida` (CSV), `United States of America` (GEOME), `US` (GBIF), or an accented `Côte d'Ivoire` must agree; an unmappable name against a code is a note, not a conflict (Task 5 test).
5. **Templates that reach a concept through a raw token or a CSV column:** `{gbif_Occurrence_decimalLatitude}` or `{collection_date}` must still trigger the export warning on a conflict; a template with no specimen tokens never does (Task 10 test).

## File map

| File | Responsibility | Task |
|---|---|---|
| `R/meta_db.R` (new) | Source registry `META_SOURCES`, `meta_*` tables and migration, shared fetch/set/drop core, project-hook helpers | 1, 3 |
| `R/meta_export.R` (new) | Export keys -> columns, `meta_export_cols()`, `.meta_join()`, picker summary, `.meta_ymd()` | 1, 4 |
| `R/geome_api.R`, `R/geome_db.R`, `R/geome_export.R` | GEOME client, `fetch_geome()` wrapper, `GEOME_COMBOS` | 1 |
| `R/gbif_api.R` (new) | `gbif_normalize_id()`, `.gbif_get()`, `.gbif_fetch_chain()` | 2 |
| `R/gbif_db.R` (new) | `fetch_gbif()` wrapper | 3 |
| `R/gbif_export.R` (new) | `GBIF_COMBOS` | 4 |
| `R/specimen_reconcile.R` (new) | Country table lookup, per-concept rules, CSV detection, `specimen_conflicts()`, `set_metadata_columns()`, export-warning helpers | 5, 6, 10 |
| `inst/extdata/countries.csv`, `data-raw/build_countries.R` (new) | ISO2 lookup built from GBIF's country enumeration plus aliases | 5 |
| `R/app_specimen.R` (renamed from `R/app_geome.R`) | Specimen status/icon column, viewer (GEOME/GBIF/Compare), field picker modal | 7, 8, 9 |
| `R/app_export_tokens.R` (new) | `export_token_groups()`, `export_token_ui()` | 11 |
| Hooks: `R/init_checks.R`, `R/init_db*.R`, `R/init_project*.R`, `R/add_samples.R`, `R/update_sample_metadata.R` | `mapping_gbif` / `fetch_gbif`, reserved names, preflight | 1, 3 |
| Tables: `R/app_assemble*.R`, `R/app_annotate*.R`, `R/app_export*.R`, `R/export.R` | Specimen column, groups, export join, owned/drop lists, warning, chips | 1, 7-11 |
| `R/app_ui*.R`, `R/app_server*.R` | Specimen Fields toolbar button and forwarding | 9 |
| `inst/app/www/custom.js`, `custom.css` | Chip insert-at-cursor and filter; chip and Compare styles | 8, 11 |
| `vignettes/Specimen-Metadata.Rmd` (renamed) | User docs | 12, 13 |

---

### Task 1: Generalize GEOME storage to `meta_*` tables with migration

Moves every GEOME read and write onto source-keyed tables. No behavior change for GEOME users; old `geome_*` tables migrate on first touch.

**Files:**
- Create: `R/meta_db.R`, `R/meta_export.R`, `tests/testthat/test-meta-db.R`
- Modify: `R/geome_api.R` (drop `.geome_flatten`, call `.meta_flatten`)
- Modify: `R/geome_db.R` (reduce to `fetch_geome()`)
- Modify: `R/geome_export.R` (keep only `.geome_pick`, `.geome_coord`, `.geome_passthrough`, `GEOME_COMBOS`)
- Modify: `R/app_geome.R` (status join, viewer SQL, drop `.geome_save_fields`)
- Modify: `R/app_export.R:233-276` (GEOME col defs, picker)
- Modify: `R/app_export_utils.R:344`, `R/export.R:445,1504` (`.geome_join` -> `.meta_join`)
- Modify: `R/add_samples.R:105-108,242-246`, `R/init_db.R:222-225,868-872`, `R/init_db_userAsmb.R:206-209,960-964`, `R/update_sample_metadata.R:52-55,138-148`, `R/app_server.R:19`, `R/app_server_userAsmb.R:19`
- Test: `tests/testthat/test-geome-db.R`, `test-geome-export.R`, `test-geome-app.R`, `test-geome-init.R`, `test-geome-update.R`, `test-geome-api.R` (renames), new `test-meta-db.R`

**Interfaces:**
- Produces (used by every later task):
  - `META_SOURCES`: named list; element `GEOME` = `list(col = "GEOME_BCID", label = "GEOME", id_label = "BCID", arg = "bcids", normalize = function(x), invalid = function(x) -> message, chain = function(ref, cache) -> data.frame(level, depth, ref, field, value))`. Task 3 appends `GBIF`.
  - `.meta_chr(x)`: character vector; doubles formatted without scientific notation, `NA` kept as `NA`.
  - `.meta_flatten(x, level, depth, ref)`: data.frame `level, depth (int), ref, field, value` of the scalar, non-empty entries of list `x`, or `NULL`.
  - `.meta_ensure_tables(con)`: creates the four `meta_*` tables, adds each registered source's samples column, migrates and drops `geome_*` tables. Idempotent.
  - `.meta_store_value(source, x)`: normalized ref, trimmed raw text if non-blank but invalid, `NA` if blank.
  - `.meta_drop(con, source, ids)`, `.meta_set_ref(con, source, id, raw)` (returns stored value; drops that source's rows when the value changes).
  - `.meta_fetch_into(con, source, ids, refs, cache = new.env())`: invisible data.frame `ID, status, message`; warns once listing failures.
  - `.meta_fetch_project(path, source, ids = NULL, refs = NULL)`: engine behind `fetch_geome()` / `fetch_gbif()`.
  - `.meta_combos(prefix)`: combo list for `"geome"`/`"GEOME"` (Task 4 adds `gbif`).
  - `.meta_key_col(key)`: `"geome:combo:lat_lon"` -> `"geome_lat_lon"`; `"geome:raw:Event:country"` -> `"geome_Event_country"`.
  - `.meta_key_value(recs, key)`, `meta_export_cols(con, ids = NULL)` (NULL when nothing ticked), `.meta_join(dat, con)` (NA -> ""), `meta_field_summary(con, source)` (columns `key, kind, level, field, n_samples, example, col, selected`), `.meta_save_fields(con, keys)` (replace-all).

- [ ] **Step 1: Write the failing migration test**

`tests/testthat/test-meta-db.R`:

```r
old_geome_db <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(con, "samples", data.frame(ID = c("s1", "s2"), Taxon = "x",
                                               GEOME_BCID = c("ark:/1/A", NA)))
  DBI::dbExecute(con, "CREATE TABLE geome_records (
    ID TEXT NOT NULL, level TEXT NOT NULL, depth INTEGER NOT NULL, bcid TEXT,
    field TEXT NOT NULL, value TEXT, PRIMARY KEY (ID, depth, field))")
  DBI::dbExecute(con, "CREATE TABLE geome_status (
    ID TEXT NOT NULL, bcid TEXT, status TEXT NOT NULL, message TEXT,
    fetched_at INTEGER, PRIMARY KEY (ID))")
  DBI::dbExecute(con, "CREATE TABLE geome_export_fields (key TEXT NOT NULL, PRIMARY KEY (key))")
  DBI::dbExecute(con, "INSERT INTO geome_records VALUES ('s1', 'Event', 2, 'ark:/1/E', 'country', 'Peru')")
  DBI::dbExecute(con, "INSERT INTO geome_status VALUES ('s1', 'ark:/1/A', 'ok', NULL, 5)")
  DBI::dbExecute(con, "INSERT INTO geome_export_fields VALUES ('combo:geo_loc_name'), ('raw:Event:country')")
  con
}

test_that(".meta_ensure_tables migrates geome_* tables into meta_* once", {
  con <- old_geome_db()
  on.exit(DBI::dbDisconnect(con))
  .meta_ensure_tables(con)
  .meta_ensure_tables(con)
  tabs <- DBI::dbListTables(con)
  expect_false(any(c("geome_records", "geome_status", "geome_export_fields") %in% tabs))
  expect_true(all(c("meta_records", "meta_status", "meta_export_fields", "meta_csv_map") %in% tabs))
  r <- DBI::dbGetQuery(con, "SELECT ID, source, level, depth, ref, field, value FROM meta_records")
  expect_equal(r$source, "GEOME")
  expect_equal(r$ref, "ark:/1/E")
  expect_equal(r$value, "Peru")
  st <- DBI::dbGetQuery(con, "SELECT ID, source, ref, status, fetched_at FROM meta_status")
  expect_equal(st$source, "GEOME")
  expect_equal(st$ref, "ark:/1/A")
  expect_equal(st$fetched_at, 5L)
  expect_setequal(DBI::dbGetQuery(con, "SELECT key FROM meta_export_fields")$key,
                  c("geome:combo:geo_loc_name", "geome:raw:Event:country"))
})

test_that("ticked fields from a migrated project still resolve at export", {
  con <- old_geome_db()
  on.exit(DBI::dbDisconnect(con))
  out <- meta_export_cols(con, ids = c("s1", "s2"))
  expect_equal(out$geome_geo_loc_name, c("Peru", NA))
  expect_equal(out$geome_Event_country, c("Peru", NA))
  dat <- .meta_join(data.frame(ID = c("s1", "s2")), con)
  expect_equal(dat$geome_Event_country, c("Peru", ""))
})

test_that(".meta_chr keeps long IDs in plain digits and NA as NA", {
  expect_equal(.meta_chr(c(6186461308, NA, 12, -170.58225)), c("6186461308", NA, "12", "-170.58225"))
  expect_equal(.meta_chr(c("a", NA)), c("a", NA))
})

test_that(".meta_key_col builds column names from source-prefixed keys", {
  expect_equal(.meta_key_col("geome:combo:lat_lon"), "geome_lat_lon")
  expect_equal(.meta_key_col("geome:raw:Event:country"), "geome_Event_country")
  expect_equal(.meta_key_col("geome:raw:Event:odd field:x"), "geome_Event_odd_field_x")
})
```

- [ ] **Step 2: Run, verify fail**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-meta-db.R")'`
Expected: FAIL, `could not find function ".meta_ensure_tables"`.

- [ ] **Step 3: Create `R/meta_db.R`**

```r
META_SOURCES <- list(
  GEOME = list(
    col = "GEOME_BCID", label = "GEOME", id_label = "BCID", arg = "bcids",
    normalize = function(x) geome_normalize_bcid(x),
    invalid = function(x) paste0("'", x, "' is not a GEOME BCID (expected ark:/NNNNN/...)"),
    chain = function(ref, cache) .geome_fetch_chain(ref, cache)
  )
)

.meta_chr <- function(x) {
  if (!is.numeric(x)) return(as.character(x))
  vapply(x, function(v) if (is.na(v)) NA_character_ else format(v, scientific = FALSE, digits = 15),
         character(1), USE.NAMES = FALSE)
}

.meta_flatten <- function(x, level, depth, ref) {
  keep <- vapply(x, function(v) {
    length(v) == 1L && !is.list(v) && !is.na(v) && nzchar(as.character(v))
  }, logical(1))
  x <- x[keep]
  if (!length(x)) return(NULL)
  data.frame(level = level, depth = as.integer(depth), ref = ref,
             field = names(x), value = vapply(x, as.character, ""),
             row.names = NULL)
}

.meta_ensure_tables <- function(con) {
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS meta_records (
    ID TEXT NOT NULL, source TEXT NOT NULL, level TEXT NOT NULL, depth INTEGER NOT NULL,
    ref TEXT, field TEXT NOT NULL, value TEXT, PRIMARY KEY (ID, source, depth, field))")
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS meta_status (
    ID TEXT NOT NULL, source TEXT NOT NULL, ref TEXT, status TEXT NOT NULL, message TEXT,
    fetched_at INTEGER, PRIMARY KEY (ID, source))")
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS meta_export_fields (
    key TEXT NOT NULL, PRIMARY KEY (key))")
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS meta_csv_map (
    concept TEXT NOT NULL, column TEXT, PRIMARY KEY (concept))")
  if (DBI::dbExistsTable(con, "samples")) {
    have <- DBI::dbListFields(con, "samples")
    for (s in META_SOURCES) {
      if (!s$col %in% have) DBI::dbExecute(con, paste0("ALTER TABLE samples ADD COLUMN ", s$col, " TEXT"))
    }
  }
  old <- intersect(c("geome_records", "geome_status", "geome_export_fields"), DBI::dbListTables(con))
  if (length(old)) {
    DBI::dbWithTransaction(con, {
      if ("geome_records" %in% old) {
        DBI::dbExecute(con, "INSERT OR IGNORE INTO meta_records
          SELECT ID, 'GEOME', level, depth, bcid, field, value FROM geome_records")
      }
      if ("geome_status" %in% old) {
        DBI::dbExecute(con, "INSERT OR IGNORE INTO meta_status
          SELECT ID, 'GEOME', bcid, status, message, fetched_at FROM geome_status")
      }
      if ("geome_export_fields" %in% old) {
        DBI::dbExecute(con, "INSERT OR IGNORE INTO meta_export_fields
          SELECT 'geome:' || key FROM geome_export_fields")
      }
      for (t in old) DBI::dbExecute(con, paste("DROP TABLE", t))
    })
  }
  invisible(NULL)
}

.meta_store_value <- function(source, x) {
  raw <- trimws(.meta_chr(x))
  raw[is.na(raw)] <- ""
  norm <- META_SOURCES[[source]]$normalize(raw)
  ifelse(!is.na(norm), norm, ifelse(nzchar(raw), raw, NA_character_))
}

.meta_drop <- function(con, source, ids) {
  for (id in ids) {
    DBI::dbExecute(con, "DELETE FROM meta_records WHERE ID = ? AND source = ?", params = list(id, source))
    DBI::dbExecute(con, "DELETE FROM meta_status WHERE ID = ? AND source = ?", params = list(id, source))
  }
  invisible(NULL)
}

.meta_set_ref <- function(con, source, id, raw) {
  .meta_ensure_tables(con)
  col <- META_SOURCES[[source]]$col
  old <- DBI::dbGetQuery(con, paste0("SELECT ", col, " AS v FROM samples WHERE ID = ?"),
                         params = list(id))$v
  old <- if (length(old)) old[1] else NA_character_
  val <- .meta_store_value(source, raw)
  changed <- xor(is.na(old), is.na(val)) || (!is.na(old) && !is.na(val) && old != val)
  DBI::dbExecute(con, paste0("UPDATE samples SET ", col, " = ? WHERE ID = ?"), params = list(val, id))
  if (changed) .meta_drop(con, source, id)
  val
}

.meta_fetch_into <- function(con, source, ids, refs, cache = new.env()) {
  .meta_ensure_tables(con)
  src <- META_SOURCES[[source]]
  status <- character(length(ids))
  msg <- rep(NA_character_, length(ids))
  for (i in seq_along(ids)) {
    r <- refs[i]
    res <- if (is.na(src$normalize(r))) {
      simpleError(src$invalid(r))
    } else {
      tryCatch(src$chain(r, cache), error = function(e) e)
    }
    ok <- !inherits(res, "error")
    status[i] <- if (ok) "ok" else "failed"
    if (!ok) msg[i] <- conditionMessage(res)
    DBI::dbWithTransaction(con, {
      if (ok) {
        DBI::dbExecute(con, "DELETE FROM meta_records WHERE ID = ? AND source = ?",
                       params = list(ids[i], source))
        if (!is.null(res) && nrow(res)) {
          DBI::dbAppendTable(con, "meta_records", data.frame(
            ID = ids[i], source = source, level = res$level, depth = res$depth,
            ref = res$ref, field = res$field, value = res$value))
        }
      }
      DBI::dbExecute(con, "INSERT OR REPLACE INTO meta_status VALUES (?, ?, ?, ?, ?, ?)",
                     params = list(ids[i], source, r, status[i], msg[i], as.integer(Sys.time())))
    })
  }
  out <- data.frame(ID = ids, status = status, message = msg)
  bad <- out$status == "failed"
  if (any(bad)) {
    warning(src$label, " fetch failed for ",
            .lst(paste0(out$ID[bad], " (", out$message[bad], ")")), call. = FALSE)
  }
  invisible(out)
}

.meta_fetch_project <- function(path, source, ids = NULL, refs = NULL) {
  src <- META_SOURCES[[source]]
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = file.path(path, ".sqlite"))
  on.exit(DBI::dbDisconnect(con))
  .meta_ensure_tables(con)
  q <- paste0("SELECT ID, ", src$col, " AS ref FROM samples")
  samples <- DBI::dbGetQuery(con, q)
  if (!is.null(ids)) {
    unknown <- setdiff(ids, samples$ID)
    if (length(unknown)) stop("sample(s) not in this project: ", .lst(unknown), call. = FALSE)
  }
  if (!is.null(refs)) {
    if (length(refs) != length(ids)) stop("ids and ", src$arg, " must be the same length", call. = FALSE)
    for (i in seq_along(ids)) .meta_set_ref(con, source, ids[i], refs[i])
    samples <- DBI::dbGetQuery(con, q)
  }
  target <- samples[!is.na(samples$ref) & (is.null(ids) | samples$ID %in% ids), ]
  if (!nrow(target)) {
    message("No samples with a ", src$label, " ", src$id_label, " to fetch")
    return(invisible(data.frame(ID = character(), status = character(), message = character())))
  }
  .meta_fetch_into(con, source, target$ID, target$ref)
}
```

- [ ] **Step 4: Create `R/meta_export.R`**

```r
.meta_combos <- function(prefix) {
  switch(tolower(prefix), geome = GEOME_COMBOS, NULL)
}

.meta_key_col <- function(key) {
  p <- strsplit(key, ":", fixed = TRUE)[[1]]
  nm <- if (p[2] == "combo") p[3] else paste(p[3], paste(p[-(1:3)], collapse = ":"), sep = "_")
  paste0(tolower(p[1]), "_", gsub("[^A-Za-z0-9_]", "_", nm))
}

.meta_key_value <- function(recs, key) {
  p <- strsplit(key, ":", fixed = TRUE)[[1]]
  if (p[2] == "combo") {
    spec <- .meta_combos(p[1])[[p[3]]]
    return(if (is.null(spec)) NA_character_ else spec$fn(recs))
  }
  field <- paste(p[-(1:3)], collapse = ":")
  v <- recs$value[recs$level == p[3] & recs$field == field]
  if (length(v)) v[1] else NA_character_
}

meta_export_cols <- function(con, ids = NULL) {
  .meta_ensure_tables(con)
  keys <- DBI::dbGetQuery(con, "SELECT key FROM meta_export_fields")$key
  if (!length(keys)) return(NULL)
  recs <- DBI::dbGetQuery(con, "SELECT ID, source, level, depth, field, value FROM meta_records")
  ids <- ids %||% unique(recs$ID)
  out <- data.frame(ID = ids)
  for (k in keys) {
    r <- recs[recs$source == toupper(sub(":.*", "", k)), ]
    out[[.meta_key_col(k)]] <- vapply(ids, function(i) .meta_key_value(r[r$ID == i, ], k),
                                      character(1), USE.NAMES = FALSE)
  }
  out
}

.meta_join <- function(dat, con) {
  g <- meta_export_cols(con, ids = unique(dat$ID))
  if (is.null(g)) return(dat)
  out <- dplyr::left_join(dat, g, by = "ID")
  cols <- setdiff(names(g), "ID")
  out[cols] <- lapply(out[cols], function(x) ifelse(is.na(x), "", x))
  out
}

meta_field_summary <- function(con, source) {
  .meta_ensure_tables(con)
  prefix <- tolower(source)
  recs <- DBI::dbGetQuery(con, "SELECT ID, level, depth, field, value FROM meta_records WHERE source = ?",
                          params = list(source))
  sel <- DBI::dbGetQuery(con, "SELECT key FROM meta_export_fields")$key
  defs <- .meta_combos(prefix)
  ids <- unique(recs$ID)
  combos <- do.call(rbind, lapply(names(defs), function(nm) {
    vals <- vapply(ids, function(i) defs[[nm]]$fn(recs[recs$ID == i, ]), character(1))
    data.frame(key = paste0(prefix, ":combo:", nm), kind = "combo", level = NA_character_,
               field = paste(defs[[nm]]$sources, collapse = " + "),
               n_samples = sum(!is.na(vals)),
               example = if (any(!is.na(vals))) vals[!is.na(vals)][1] else NA_character_)
  }))
  raw <- if (nrow(recs)) {
    g <- unique(recs[, c("level", "depth", "field")])
    g <- g[order(-g$depth, g$level, g$field), ]
    data.frame(
      key = paste0(prefix, ":raw:", g$level, ":", g$field), kind = "raw", level = g$level,
      field = g$field,
      n_samples = vapply(seq_len(nrow(g)), function(j) {
        length(unique(recs$ID[recs$level == g$level[j] & recs$field == g$field[j]]))
      }, integer(1)),
      example = vapply(seq_len(nrow(g)), function(j) {
        recs$value[recs$level == g$level[j] & recs$field == g$field[j]][1]
      }, character(1))
    )
  }
  out <- rbind(combos, raw)
  out <- unique(out[, c("key", "kind", "level", "field", "n_samples", "example")])
  out$n_samples <- as.integer(out$n_samples)
  out$col <- vapply(out$key, .meta_key_col, character(1), USE.NAMES = FALSE)
  out$selected <- out$key %in% sel
  rownames(out) <- NULL
  out
}

.meta_save_fields <- function(con, keys) {
  DBI::dbWithTransaction(con, {
    DBI::dbExecute(con, "DELETE FROM meta_export_fields")
    if (length(keys)) DBI::dbAppendTable(con, "meta_export_fields", data.frame(key = unique(keys)))
  })
  invisible(keys)
}
```

- [ ] **Step 5: Slim the GEOME files**

`R/geome_api.R`: delete the whole `.geome_flatten <- function(...) {...}` definition. In `.geome_fetch_chain()` replace the three calls `.geome_flatten(` with `.meta_flatten(` (arguments unchanged; the fourth argument is now the `ref` column).

`R/geome_export.R`: delete everything from `.geome_key_col <- function(key) {` to the end of the file (`.geome_key_col`, `.geome_key_value`, `geome_export_cols`, `.geome_join`, `geome_field_summary`). Keep `.geome_pick`, `.geome_coord`, `.geome_passthrough`, and `GEOME_COMBOS`.

`R/geome_db.R`: replace the whole file with:

```r
#' Fetch GEOME metadata for project samples
#'
#' Looks up each sample's GEOME BCID, walks up its parent records (e.g.
#' Tissue, Sample, Event) and adds expedition and project details, then stores
#' everything in the project database for viewing in the app and use at export.
#'
#' @param path Path to the project directory (default = current working directory)
#' @param ids Sample IDs to fetch. Default: every sample with a BCID.
#' @param bcids Optional BCIDs to set for `ids` first (same length as `ids`).
#'   A blank value removes that sample's BCID and its GEOME data.
#' @return Invisibly, a data frame of `ID`, `status`, and `message`.
#' @export
fetch_geome <- function(path = ".", ids = NULL, bcids = NULL) {
  .meta_fetch_project(path, "GEOME", ids, bcids)
}
```

- [ ] **Step 6: Switch every caller**

Mechanical replacements (check each with `grep` afterwards):

```bash
cd ~/Documents/GitHub/MitoPilot-geome
sed -i 's/\.geome_ensure_tables(/.meta_ensure_tables(/g' R/add_samples.R R/init_db.R R/init_db_userAsmb.R R/update_sample_metadata.R R/app_server.R R/app_server_userAsmb.R R/app_geome.R
sed -i 's/\.geome_store_value(mapping\[\[mapping_geome\]\])/.meta_store_value("GEOME", mapping[[mapping_geome]])/' R/add_samples.R R/init_db.R R/init_db_userAsmb.R R/update_sample_metadata.R
sed -i 's/\.geome_fetch_into(con, /.meta_fetch_into(con, "GEOME", /' R/add_samples.R R/init_db.R R/init_db_userAsmb.R R/update_sample_metadata.R
sed -i 's/\.geome_drop(con, /.meta_drop(con, "GEOME", /' R/update_sample_metadata.R
sed -i 's/\.geome_join(/.meta_join(/' R/app_export_utils.R R/export.R
grep -rn "\.geome_ensure_tables\|\.geome_store_value\|\.geome_fetch_into\|\.geome_drop\|\.geome_join\|\.geome_set_bcid\|geome_export_cols\|geome_field_summary\|\.geome_key_col\|\.geome_save_fields\|\.geome_flatten" R/
```

Expected after Step 7 below: the final grep prints nothing.

- [ ] **Step 7: `R/app_geome.R` and `R/app_export.R`**

In `R/app_geome.R`:

`.geome_status_join()` body becomes:

```r
.geome_status_join <- function(tbl, db) {
  .meta_ensure_tables(db)
  tbl |>
    dplyr::left_join(
      dplyr::tbl(db, "meta_status") |>
        dplyr::filter(source == "GEOME") |>
        dplyr::select(ID, geome_status = status, geome_message = message),
      by = "ID"
    ) |>
    dplyr::mutate(geome = dplyr::case_when(
      geome_status == "ok" ~ "ok",
      geome_status == "failed" ~ "failed",
      TRUE ~ "none"
    )) |>
    dplyr::select(-geome_status)
}
```

In its roxygen, change "(source of `geome_status`)" to "(source of `meta_status`)".

Delete the `.geome_save_fields` definition and its roxygen block (it now lives in `R/meta_export.R` as `.meta_save_fields`).

In `geome_viewer_server()`:
- `samples()` SQL becomes
  ```r
      DBI::dbGetQuery(con, "SELECT s.ID, s.Taxon, s.GEOME_BCID, g.status, g.message, g.fetched_at
                            FROM samples s LEFT JOIN meta_status g ON s.ID = g.ID AND g.source = 'GEOME'
                            ORDER BY s.ID")
  ```
- the records query in `output$detail` becomes
  ```r
      recs <- DBI::dbGetQuery(con, "SELECT level, depth, ref AS bcid, field, value FROM meta_records
                                    WHERE ID = ? AND source = 'GEOME'", params = list(rv$id))
  ```
- `val <- .geome_set_bcid(con, rv$id, input$bcid)` -> `val <- .meta_set_ref(con, "GEOME", rv$id, input$bcid)`
- `res <- suppressWarnings(.geome_fetch_into(con, rv$id, val))` -> `res <- suppressWarnings(.meta_fetch_into(con, "GEOME", rv$id, val))`
- `suppressWarnings(.geome_fetch_into(con, s$ID[i], s$GEOME_BCID[i], cache))` -> `suppressWarnings(.meta_fetch_into(con, "GEOME", s$ID[i], s$GEOME_BCID[i], cache))`

In `R/app_export.R` (`EXPORT_COL_GROUPS` comment ~L10, `geome_col_defs()` and the picker, ~L237-276):
- the comment `# geome + the fields ticked in geome_export_fields, filled at render time` -> `# geome + the fields ticked in meta_export_fields, filled at render time`
- `"SELECT key FROM geome_export_fields"` -> `"SELECT key FROM meta_export_fields"`
- `vapply(keys, .geome_key_col, ...)` -> `vapply(keys, .meta_key_col, ...)`
- both `geome_field_summary(session$userData$con)` -> `meta_field_summary(session$userData$con, "GEOME")`
- `.geome_save_fields(session$userData$con, c(input$geome_combos, picked))` -> `.meta_save_fields(session$userData$con, c(input$geome_combos, picked))`

- [ ] **Step 8: Update the existing GEOME tests**

```bash
cd ~/Documents/GitHub/MitoPilot-geome/tests/testthat
sed -i -e 's/FROM geome_status/FROM meta_status/g' -e 's/FROM geome_records/FROM meta_records/g' \
       -e 's/"geome_export_fields" %in%/"meta_export_fields" %in%/g' test-geome-init.R test-geome-update.R
sed -i 's/out\$bcid\[out\$level == "Sample"\]/out$ref[out$level == "Sample"]/' test-geome-api.R
```

Replace `tests/testthat/test-geome-db.R` with:

```r
geome_test_db <- function() {
  f <- withr::local_tempfile(fileext = ".sqlite", .local_envir = parent.frame())
  con <- DBI::dbConnect(RSQLite::SQLite(), f)
  withr::defer(DBI::dbDisconnect(con), envir = parent.frame())
  DBI::dbWriteTable(con, "samples", data.frame(ID = c("s1", "s2", "s3"), Taxon = "x"))
  con
}

n_recs <- function(con, id) {
  DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM meta_records WHERE ID = ? AND source = 'GEOME'",
                  params = list(id))$n
}

test_that(".meta_ensure_tables is idempotent and adds GEOME_BCID", {
  con <- geome_test_db()
  .meta_ensure_tables(con)
  .meta_ensure_tables(con)
  expect_true(all(c("meta_records", "meta_status", "meta_export_fields") %in% DBI::dbListTables(con)))
  expect_true("GEOME_BCID" %in% DBI::dbListFields(con, "samples"))
})

test_that(".meta_store_value keeps bad GEOME input visible and blanks as NA", {
  expect_equal(.meta_store_value("GEOME", c(" https://n2t.net/ark:/21547/X ", "junk", "", NA)),
               c("ark:/21547/X", "junk", NA, NA))
})

test_that(".meta_set_ref drops old records on a changed BCID, keeps them on same BCID", {
  con <- geome_test_db()
  .meta_ensure_tables(con)
  .meta_set_ref(con, "GEOME", "s1", "ark:/21547/A")
  DBI::dbExecute(con, "INSERT INTO meta_records VALUES ('s1', 'GEOME', 'Tissue', 0, 'ark:/21547/A', 'f', 'v')")
  DBI::dbExecute(con, "INSERT INTO meta_status VALUES ('s1', 'GEOME', 'ark:/21547/A', 'ok', NULL, 1)")
  .meta_set_ref(con, "GEOME", "s1", "ark:/21547/A")
  expect_equal(n_recs(con, "s1"), 1L)
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM meta_status WHERE ID='s1'")$n, 1L)
  .meta_set_ref(con, "GEOME", "s1", "ark:/21547/B")
  expect_equal(n_recs(con, "s1"), 0L)
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM meta_status WHERE ID='s1'")$n, 0L)
})

test_that(".meta_set_ref then a failing fetch leaves no stale records", {
  local_mocked_bindings(.geome_get = geome_fixture_get)
  con <- geome_test_db()
  .meta_ensure_tables(con)
  DBI::dbExecute(con, "INSERT INTO meta_records VALUES ('s1', 'GEOME', 'Tissue', 0, 'ark:/21547/A', 'f', 'v')")
  DBI::dbExecute(con, "INSERT INTO meta_status VALUES ('s1', 'GEOME', 'ark:/21547/A', 'ok', NULL, 1)")
  val <- .meta_set_ref(con, "GEOME", "s1", "ark:/21547/NOPE")
  suppressWarnings(.meta_fetch_into(con, "GEOME", "s1", val))
  expect_equal(n_recs(con, "s1"), 0L)
  expect_equal(DBI::dbGetQuery(con, "SELECT status FROM meta_status WHERE ID='s1'")$status, "failed")
})

test_that(".meta_fetch_into stores GEOME records and ok status", {
  local_mocked_bindings(.geome_get = geome_fixture_get)
  con <- geome_test_db()
  .meta_fetch_into(con, "GEOME", "s1", "ark:/21547/CYC2CMPI38181.1")
  st <- DBI::dbGetQuery(con, "SELECT * FROM meta_status")
  expect_equal(st$status, "ok")
  expect_equal(st$source, "GEOME")
  expect_true(is.na(st$message))
  expect_gt(n_recs(con, "s1"), 10)
})

test_that("failures are per sample, warned once, and keep prior data", {
  local_mocked_bindings(.geome_get = geome_fixture_get)
  con <- geome_test_db()
  .meta_fetch_into(con, "GEOME", "s1", "ark:/21547/CYC2CMPI38181.1")
  before <- n_recs(con, "s1")
  expect_warning(
    res <- .meta_fetch_into(con, "GEOME", c("s1", "s2", "s3"),
                            c("ark:/21547/NOPE", "ark:/21547/CYB2REEDY", "junk")),
    "GEOME fetch failed for .*(s1.*s3|s3.*s1)"
  )
  expect_equal(res$status, c("failed", "ok", "failed"))
  expect_match(res$message[3], "not a GEOME BCID")
  expect_equal(n_recs(con, "s1"), before)
})

test_that("fetch_geome sets, fetches, and clears BCIDs", {
  local_mocked_bindings(.geome_get = geome_fixture_get)
  d <- withr::local_tempdir()
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(d, ".sqlite"))
  DBI::dbWriteTable(con, "samples", data.frame(ID = c("s1", "s2"), Taxon = "x"))
  DBI::dbDisconnect(con)
  fetch_geome(d, ids = c("s1", "s2"),
              bcids = c("https://n2t.net/ark:/21547/CYB2REEDY", "ark:/21547/CYA2Reedy01"))
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(d, ".sqlite"))
  on.exit(DBI::dbDisconnect(con))
  expect_equal(DBI::dbGetQuery(con, "SELECT GEOME_BCID FROM samples ORDER BY ID")$GEOME_BCID,
               c("ark:/21547/CYB2REEDY", "ark:/21547/CYA2Reedy01"))
  fetch_geome(d, ids = "s1", bcids = "")
  expect_true(is.na(DBI::dbGetQuery(con, "SELECT GEOME_BCID FROM samples WHERE ID='s1'")$GEOME_BCID))
  expect_equal(DBI::dbGetQuery(con, "SELECT ID FROM meta_status")$ID, "s2")
  expect_error(fetch_geome(d, ids = "nope", bcids = "ark:/1/A"), "not in this project")
  expect_error(fetch_geome(d, ids = c("s1", "s2"), bcids = "ark:/1/A"), "same length")
})

test_that("fetch_geome with no args refreshes every sample with a BCID", {
  local_mocked_bindings(.geome_get = geome_fixture_get)
  d <- withr::local_tempdir()
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(d, ".sqlite"))
  DBI::dbWriteTable(con, "samples", data.frame(ID = c("s1", "s2"), Taxon = "x",
                                               GEOME_BCID = c("ark:/21547/CYB2REEDY", NA)))
  DBI::dbDisconnect(con)
  res <- fetch_geome(d)
  expect_equal(res$ID, "s1")
})
```

In `tests/testthat/test-geome-export.R`, replace everything from `test_that("geome_export_cols returns only ticked keys, NULL when none", {` to the end of the file with:

```r
geome_mem_db <- function(ids = c("s1", "s2")) {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(con, "samples", data.frame(ID = ids, Taxon = "x"))
  .meta_ensure_tables(con)
  con
}

add_geome_recs <- function(con, id, field, value, level = "Event", depth = 2L) {
  DBI::dbAppendTable(con, "meta_records", data.frame(
    ID = id, source = "GEOME", level = level, depth = depth, ref = "ark:/1/E",
    field = field, value = value))
}

test_that("meta_export_cols returns only ticked keys, NULL when none", {
  con <- geome_mem_db()
  on.exit(DBI::dbDisconnect(con))
  expect_null(meta_export_cols(con))
  add_geome_recs(con, "s1", c("country", "locality"), c("Peru", "Lima"))
  .meta_save_fields(con, c("geome:combo:geo_loc_name", "geome:raw:Event:country"))
  out <- meta_export_cols(con, ids = c("s1", "s2"))
  expect_equal(names(out), c("ID", "geome_geo_loc_name", "geome_Event_country"))
  expect_equal(out$geome_geo_loc_name, c("Peru: Lima", NA))
  expect_equal(out$geome_Event_country, c("Peru", NA))
})

test_that("meta_export_cols is NULL on a project that never had GEOME tables", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  expect_null(meta_export_cols(con))
})

test_that("meta_field_summary lists GEOME combos and raw fields with counts", {
  con <- geome_mem_db()
  on.exit(DBI::dbDisconnect(con))
  add_geome_recs(con, c("s1", "s2"), "country", c("Peru", "Chile"))
  .meta_save_fields(con, "geome:raw:Event:country")
  s <- meta_field_summary(con, "GEOME")
  raw <- s[s$key == "geome:raw:Event:country", ]
  expect_equal(raw$n_samples, 2L)
  expect_true(raw$selected)
  expect_equal(raw$col, "geome_Event_country")
  expect_true(all(paste0("geome:combo:", names(GEOME_COMBOS)) %in% s$key))
})

test_that("export_metadata_cols treats GEOME_BCID as owned", {
  expect_equal(export_metadata_cols(c("ID", "Taxon", "GEOME_BCID", "site"), character()), "site")
})

test_that(".meta_join adds ticked columns and is a no-op otherwise", {
  con <- geome_mem_db("s1")
  on.exit(DBI::dbDisconnect(con))
  dat <- data.frame(ID = "s1", Taxon = "x")
  expect_identical(.meta_join(dat, con), dat)
  add_geome_recs(con, "s1", "country", "Peru")
  .meta_save_fields(con, "geome:raw:Event:country")
  expect_equal(.meta_join(dat, con)$geome_Event_country, "Peru")
})

test_that("a ticked GEOME column resolves in a header template", {
  dat <- data.frame(ID = "s1", geome_lat_lon = "17.5 S 149.8 W")
  expect_equal(as.character(stringr::str_glue_data(dat, "{ID} [lat_lon={geome_lat_lon}]")),
               "s1 [lat_lon=17.5 S 149.8 W]")
})

test_that("a ticked GEOME field with no value joins as empty, not NA", {
  con <- geome_mem_db()
  on.exit(DBI::dbDisconnect(con))
  add_geome_recs(con, "s1", "country", "Peru")
  .meta_save_fields(con, "geome:combo:lat_lon")
  dat <- .meta_join(data.frame(ID = c("s1", "s2"), Taxon = c("x", NA)), con)
  expect_equal(dat$geome_lat_lon, c("", ""))
  expect_true(is.na(dat$Taxon[2]))
  expect_equal(as.character(stringr::str_glue_data(dat[1, ], "[lat_lon={geome_lat_lon}]")), "[lat_lon=]")
})
```

In `tests/testthat/test-geome-app.R`:
- in `".geome_status_join labels ok, failed, and none"`, replace `.geome_ensure_tables(con)` and the `dbAppendTable(con, "geome_status", ...)` call with
  ```r
  .meta_ensure_tables(con)
  DBI::dbAppendTable(con, "meta_status", data.frame(
    ID = c("a", "b"), source = "GEOME", ref = "ark:/1/A", status = c("ok", "failed"),
    message = c(NA, "BCID not found in GEOME"), fetched_at = 1L))
  ```
- rename the test `".geome_save_fields replaces the selection"` to `".meta_save_fields replaces the selection"` and in it replace `.geome_ensure_tables` with `.meta_ensure_tables`, `.geome_save_fields(` with `.meta_save_fields(`, `"combo:lat_lon"` with `"geome:combo:lat_lon"`, `"raw:Event:country"` with `"geome:raw:Event:country"`, and `geome_export_fields` with `meta_export_fields` (both queries).
- in the two tests that append to `geome_records`, replace `.geome_ensure_tables(con)` with `.meta_ensure_tables(con)` and each
  ```r
  DBI::dbAppendTable(con, "geome_records", data.frame(
    ID = <id>, level = "Event", depth = 0L, bcid = NA_character_,
    field = "country", value = "Peru"))
  ```
  with
  ```r
  DBI::dbAppendTable(con, "meta_records", data.frame(
    ID = <id>, source = "GEOME", level = "Event", depth = 0L, ref = NA_character_,
    field = "country", value = "Peru"))
  ```
  keeping `<id>` as it was (`id1` or `"s1"`); replace `.geome_save_fields(con, c("combo:lat_lon", "raw:Event:country"))` with `.meta_save_fields(con, c("geome:combo:lat_lon", "geome:raw:Event:country"))`, `.geome_save_fields(con, "combo:lat_lon")` with `.meta_save_fields(con, "geome:combo:lat_lon")`, and `s <- geome_field_summary(con)` with `s <- meta_field_summary(con, "GEOME")`.
- in `".geome_project_has_bcids and .geome_default_groups follow the samples table"` nothing changes.

- [ ] **Step 9: Run the GEOME suites, verify pass**

```bash
cd ~/Documents/GitHub/MitoPilot-geome
Rscript -e 'devtools::load_all(); for (f in c("test-meta-db.R","test-geome-api.R","test-geome-db.R","test-geome-export.R","test-geome-init.R","test-geome-update.R","test-geome-app.R","test-export-summary-csv.R","test-export-metadata-cols.R","test-no-duplicate-definitions.R")) testthat::test_file(file.path("tests/testthat", f))'
grep -rn "FROM geome_\|INTO geome_\|\"geome_records\"\|\"geome_status\"\|geome_export_fields" R/ | grep -v "R/meta_db.R"
```

Expected: all PASS; the grep prints nothing (only `R/meta_db.R` names the old tables, for migration; the `geome_status` alias inside `.geome_status_join()` is a column name, not the table, and goes away in Task 7).

- [ ] **Step 10: Commit**

```bash
Rscript -e 'devtools::document()'
git add R/ tests/testthat/ man/ NAMESPACE
git commit -m "Move GEOME storage to source-keyed meta tables with migration"
```

---

### Task 2: GBIF API client

**Files:**
- Create: `R/gbif_api.R`
- Create: `tests/testthat/helper-gbif.R`
- Create: `tests/testthat/fixtures/gbif/*.json`
- Test: `tests/testthat/test-gbif-api.R`
- Modify: `_pkgdown.yml` (reference, after `geome_normalize_bcid`)

**Interfaces:**
- Consumes: `.meta_chr()`, `.meta_flatten()` (Task 1).
- Produces:
  - `gbif_normalize_id(x)` (exported): vectorized; digits-only string or `NA_character_`. Accepts numbers, whitespace, `https://www.gbif.org/occurrence/<n>`, `https://api.gbif.org/v1/occurrence/<n>`, trailing slash.
  - `.gbif_get(path)`: parsed JSON (`simplifyVector = FALSE`) or `stop()` with the Global Constraints messages.
  - `.gbif_fetch_chain(id, cache = new.env())`: data.frame `level, depth, ref, field, value`; Occurrence (0) always, Dataset (1) and Organization (2) when reachable. Occurrence failure stops; Dataset/Organization failures are dropped silently. `issues` is one comma-joined field; Dataset `citation` is the citation text; Organization `homepage` is comma-joined.
  - Test helper `gbif_fixture_get(path)`, counter env `gbif_calls`, `gbif_reset_calls()`.

- [ ] **Step 1: Record fixtures from the live API**

```bash
cd ~/Documents/GitHub/MitoPilot-geome
mkdir -p tests/testthat/fixtures/gbif
cd tests/testthat/fixtures/gbif
api=https://api.gbif.org/v1
for id in 6186461308 2336663130; do
  curl -s "$api/occurrence/$id" > "occurrence_$id.json"
  ds=$(python3 -c "import json;print(json.load(open('occurrence_$id.json'))['datasetKey'])")
  org=$(python3 -c "import json;print(json.load(open('occurrence_$id.json'))['publishingOrgKey'])")
  curl -s "$api/dataset/$ds" > "dataset_${ds//-/_}.json"
  curl -s "$api/organization/$org" > "organization_${org//-/_}.json"
done
ls -la
python3 -c "import json;d=json.load(open('occurrence_6186461308.json'));print({k:d.get(k) for k in ['decimalLatitude','decimalLongitude','year','month','day','eventDate','countryCode','country','stateProvince','locality','institutionCode','collectionCode','catalogNumber','recordedBy','issues']})"
```

Expected: six non-empty JSON files (two occurrences, datasets `eccf4b09-f0c8-462d-a48c-41a7ce36815a` and `89d78af0-ad71-4136-9812-fb1d415ec933`, organizations `8483a1f0-1032-11db-ae00-b8a03c50a862` and `9a23b798-ee7e-4593-9348-7016fdca9960`). On 2026-09-24 the fish occurrence printed `28.53783, -81.33322, 2026, 2, 23, '2026-02-23', 'US', 'United States of America', 'Florida', 'Lake Underhill', 'UF', 'Fish', '250399', 'Robins, Robert; Casteel, Jamie; Lange, Ted'` and three issues. GBIF reprocesses data; if a value printed here differs, use the fixture's value in every test of this plan that quotes it (tests pin fixtures, not live data).

- [ ] **Step 2: Write test helper**

`tests/testthat/helper-gbif.R`:

```r
gbif_calls <- new.env()

gbif_fixture_get <- function(path) {
  gbif_calls[[path]] <- (gbif_calls[[path]] %||% 0L) + 1L
  f <- testthat::test_path("fixtures", "gbif",
                           paste0(gsub("[^A-Za-z0-9.]", "_", path), ".json"))
  if (!file.exists(f)) {
    stop("GBIF occurrence not found (IDs can change when a dataset is republished)", call. = FALSE)
  }
  jsonlite::fromJSON(f, simplifyVector = FALSE)
}

gbif_reset_calls <- function() rm(list = ls(gbif_calls), envir = gbif_calls)
```

Check: `gsub("[^A-Za-z0-9.]", "_", "dataset/eccf4b09-f0c8-462d-a48c-41a7ce36815a")` is `dataset_eccf4b09_f0c8_462d_a48c_41a7ce36815a`, matching Step 1's `${ds//-/_}` names.

- [ ] **Step 3: Write failing tests**

`tests/testthat/test-gbif-api.R`:

```r
test_that("gbif_normalize_id accepts digits, numbers, and gbif.org URLs", {
  x <- c("6186461308", " 6186461308 ",
         "https://www.gbif.org/occurrence/6186461308",
         "https://gbif.org/occurrence/6186461308/",
         "https://api.gbif.org/v1/occurrence/6186461308",
         "", NA, "abc", "12.5", "https://www.gbif.org/dataset/6186461308")
  expect_equal(gbif_normalize_id(x), c(rep("6186461308", 5), rep(NA, 5)))
  expect_equal(gbif_normalize_id(c(6186461308, NA)), c("6186461308", NA))
})

test_that(".gbif_fetch_chain returns occurrence, dataset, and organization levels", {
  local_mocked_bindings(.gbif_get = gbif_fixture_get)
  out <- .gbif_fetch_chain("6186461308")
  lv <- unique(out[order(out$depth), c("level", "depth")])
  expect_equal(lv$level, c("Occurrence", "Dataset", "Organization"))
  expect_equal(lv$depth, 0:2)
  occ <- out[out$level == "Occurrence", ]
  expect_equal(occ$value[occ$field == "countryCode"], "US")
  expect_equal(occ$value[occ$field == "catalogNumber"], "250399")
  expect_equal(unique(occ$ref), "6186461308")
  expect_match(occ$value[occ$field == "issues"], "GEODETIC_DATUM_ASSUMED_WGS84", fixed = TRUE)
  expect_false(grepl("[", occ$value[occ$field == "issues"], fixed = TRUE))
  ds <- out[out$level == "Dataset", ]
  expect_equal(ds$value[ds$field == "title"], "UF FLMNH Ichthyology")
  expect_match(ds$value[ds$field == "citation"], "UF FLMNH Ichthyology", fixed = TRUE)
  expect_equal(unique(ds$ref), "eccf4b09-f0c8-462d-a48c-41a7ce36815a")
  org <- out[out$level == "Organization", ]
  expect_equal(org$value[org$field == "title"], "Florida Museum of Natural History")
  expect_equal(org$value[org$field == "homepage"], "http://www.flmnh.ufl.edu")
  expect_type(out$depth, "integer")
})

test_that("the GEOME-published record resolves to the GeOMe publisher", {
  local_mocked_bindings(.gbif_get = gbif_fixture_get)
  out <- .gbif_fetch_chain("2336663130")
  expect_equal(out$value[out$level == "Organization" & out$field == "title"],
               "The Genomic Observatories Metadatabase (GeOMe)")
  expect_equal(out$value[out$level == "Dataset" & out$field == "title"],
               "Diversity of the Indo-Pacific (DIPnet)")
})

test_that("dataset and organization are fetched once per cache", {
  local_mocked_bindings(.gbif_get = gbif_fixture_get)
  gbif_reset_calls()
  cache <- new.env()
  .gbif_fetch_chain("6186461308", cache)
  .gbif_fetch_chain("6186461308", cache)
  expect_equal(gbif_calls[["dataset/eccf4b09-f0c8-462d-a48c-41a7ce36815a"]], 1L)
  expect_equal(gbif_calls[["organization/8483a1f0-1032-11db-ae00-b8a03c50a862"]], 1L)
})

test_that("an unknown occurrence stops with the 404 message", {
  local_mocked_bindings(.gbif_get = gbif_fixture_get)
  expect_error(.gbif_fetch_chain("999999999999"), "GBIF occurrence not found")
})

test_that("a failing dataset lookup keeps the occurrence and organization", {
  fake <- function(path) {
    if (startsWith(path, "occurrence/")) {
      return(list(key = 1, datasetKey = "d1", publishingOrgKey = "o1", country = "Peru",
                  issues = list("ZERO_COORDINATE", "COORDINATE_INVALID")))
    }
    if (startsWith(path, "organization/")) return(list(title = "Org", homepage = list("http://a", "http://b")))
    stop("GBIF returned HTTP 503", call. = FALSE)
  }
  local_mocked_bindings(.gbif_get = fake)
  out <- .gbif_fetch_chain("1")
  expect_equal(unique(out$level), c("Occurrence", "Organization"))
  expect_equal(out$value[out$field == "issues"], "ZERO_COORDINATE,COORDINATE_INVALID")
  expect_equal(out$value[out$field == "homepage"], "http://a, http://b")
})

test_that("an occurrence without issues stores no issues field", {
  local_mocked_bindings(.gbif_get = function(path) list(key = 5, issues = list()))
  out <- .gbif_fetch_chain("5")
  expect_false("issues" %in% out$field)
})

test_that(".gbif_get maps HTTP status to readable errors", {
  msg <- function(st) {
    httr2::local_mocked_responses(function(req) httr2::response(status_code = st))
    tryCatch(.gbif_get("occurrence/1"), error = conditionMessage)
  }
  expect_equal(msg(404), "GBIF occurrence not found (IDs can change when a dataset is republished)")
  expect_equal(msg(400), "GBIF returned HTTP 400")
  expect_equal(msg(418), "GBIF returned HTTP 418")
})

test_that("live GBIF lookup works", {
  skip_on_cran()
  skip_if_offline("api.gbif.org")
  out <- .gbif_fetch_chain("6186461308")
  expect_true("Occurrence" %in% out$level)
})
```

- [ ] **Step 4: Run, verify fail**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-gbif-api.R")'`
Expected: FAIL, `could not find function "gbif_normalize_id"`.

- [ ] **Step 5: Implement `R/gbif_api.R`**

```r
GBIF_API <- "https://api.gbif.org/v1"

#' Normalize GBIF occurrence IDs
#'
#' @param x Vector of gbifIDs: digits, numbers, or a gbif.org / api.gbif.org
#'   occurrence URL.
#' @return Character vector of digit-only IDs, NA where the input is blank or
#'   not an occurrence ID.
#' @export
gbif_normalize_id <- function(x) {
  x <- trimws(.meta_chr(x))
  x <- sub("^https?://(www\\.)?gbif\\.org/occurrence/", "", x, ignore.case = TRUE)
  x <- sub("^https?://api\\.gbif\\.org/v1/occurrence/", "", x, ignore.case = TRUE)
  x <- sub("/+$", "", x)
  ok <- !is.na(x) & grepl("^[0-9]+$", x)
  ifelse(ok, x, NA_character_)
}

.gbif_get <- function(path) {
  req <- httr2::request(paste0(GBIF_API, "/", path)) |>
    httr2::req_user_agent("MitoPilot (https://github.com/Smithsonian/MitoPilot)") |>
    httr2::req_timeout(30) |>
    httr2::req_retry(max_tries = 3,
                     is_transient = function(r) httr2::resp_status(r) %in% c(429, 500, 502, 503, 504)) |>
    httr2::req_error(is_error = function(r) FALSE)
  resp <- tryCatch(httr2::req_perform(req), error = function(e) {
    stop("could not reach GBIF (", conditionMessage(e), ")", call. = FALSE)
  })
  st <- httr2::resp_status(resp)
  if (st == 404) {
    stop("GBIF occurrence not found (IDs can change when a dataset is republished)", call. = FALSE)
  }
  if (st >= 400) stop("GBIF returned HTTP ", st, call. = FALSE)
  jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector = FALSE)
}

.gbif_fetch_chain <- function(id, cache = new.env()) {
  get <- function(key, path) {
    if (is.null(cache[[key]])) cache[[key]] <- .gbif_get(path)
    cache[[key]]
  }
  occ <- get(paste0("occ:", id), paste0("occurrence/", id))
  occ$issues <- if (length(occ$issues)) paste(unlist(occ$issues), collapse = ",") else NULL
  out <- list(.meta_flatten(occ, "Occurrence", 0L, .meta_chr(occ$key %||% id)))
  org <- occ$publishingOrgKey
  if (!is.null(occ$datasetKey)) {
    tryCatch({
      d <- get(paste0("ds:", occ$datasetKey), paste0("dataset/", occ$datasetKey))
      d$citation <- d$citation$text
      out[[length(out) + 1L]] <- .meta_flatten(d, "Dataset", 1L, occ$datasetKey)
      org <- org %||% d$publishingOrganizationKey
    }, error = function(e) NULL)
  }
  if (!is.null(org)) {
    tryCatch({
      o <- get(paste0("org:", org), paste0("organization/", org))
      o$homepage <- if (length(o$homepage)) paste(unlist(o$homepage), collapse = ", ") else NULL
      out[[length(out) + 1L]] <- .meta_flatten(o, "Organization", 2L, org)
    }, error = function(e) NULL)
  }
  do.call(rbind, out)
}
```

Note: `.gbif_get()` retries 500 (GBIF 5xx is transient, unlike GEOME's unknown-ID 500); that is why the status test above does not use 500.

- [ ] **Step 6: Run tests, verify pass**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-gbif-api.R")'`
Expected: all PASS (live test passes or skips).

- [ ] **Step 7: pkgdown, document, commit**

In `_pkgdown.yml`, reference section "Project database", add `  - gbif_normalize_id` right after `  - geome_normalize_bcid`.

```bash
Rscript -e 'devtools::document()'
git add R/gbif_api.R tests/testthat/helper-gbif.R tests/testthat/fixtures/gbif tests/testthat/test-gbif-api.R NAMESPACE man/gbif_normalize_id.Rd _pkgdown.yml
git commit -m "GBIF API client and occurrence fetch"
```

---

### Task 3: GBIF storage, `fetch_gbif()`, and project hooks

**Files:**
- Modify: `R/meta_db.R` (register `GBIF`; add `.meta_take_cols`, `.meta_fetch_new`, `.meta_sync_changed`)
- Create: `R/gbif_db.R` (`fetch_gbif()`)
- Modify: `R/init_checks.R` (`check_mapping()` ~L190-260; `preflight_project()` ~L553-580)
- Modify: `R/init_db.R` (roxygen ~L17-20; signature ~L90-91; `check_mapping` call ~L154-155; rename block ~L222-225; fetch block ~L868-872)
- Modify: `R/init_db_userAsmb.R` (roxygen ~L12-15; signature ~L98-99; `check_mapping` call ~L165; rename block ~L206-209; fetch block ~L960-964)
- Modify: `R/init_project.R` (roxygen ~L18-23; signature ~L62-63; dots ~L82-83; `new_db()` call ~L130-131)
- Modify: `R/init_project_userAsmb.R` (roxygen ~L15-20; signature ~L89-90; dots ~L113-114; `new_db_userAsmb()` call ~L174-175)
- Modify: `R/add_samples.R` (roxygen ~L18-20; signature ~L29-30; rename block ~L105-108; fetch block ~L242-246)
- Modify: `R/update_sample_metadata.R` (roxygen ~L11-13; signature ~L22-23; rename block ~L52-55; `old_bcid` ~L87-91; sync block ~L138-148)
- Modify: `_pkgdown.yml` (reference, after `fetch_geome`)
- Test: `tests/testthat/test-gbif-db.R`

**Interfaces:**
- Consumes: `gbif_normalize_id()`, `.gbif_fetch_chain()` (Task 2); `META_SOURCES`, `.meta_store_value`, `.meta_fetch_into`, `.meta_drop`, `.meta_ensure_tables`, `.meta_fetch_project` (Task 1).
- Produces:
  - `META_SOURCES$GBIF` = `list(col = "GBIF_ID", label = "GBIF", id_label = "ID", arg = "gbifs", ...)`. `.meta_ensure_tables()` now also adds `samples.GBIF_ID`.
  - `.meta_take_cols(mapping, cols)`: `cols` is a named character vector `c(GEOME = <csv col>, GBIF = <csv col>)`; renames each present column to its reserved name with normalized values.
  - `.meta_fetch_new(con, mapping, fetch)`: `fetch` is a named logical list `list(GEOME = TRUE, GBIF = TRUE)`; fetches every non-NA ref of each enabled source.
  - `.meta_sync_changed(con, mapping, old, fetch)`: `old` is the samples table read before the update; drops rows of changed/cleared refs and refetches changed ones.
  - `fetch_gbif(path = ".", ids = NULL, gbifs = NULL)` (exported).
  - `new_db(..., mapping_gbif = "GBIF_ID", fetch_gbif = TRUE)`, same on `new_db_userAsmb`, `new_project`, `new_project_userAsmb`, `add_samples`, `update_sample_metadata`; `check_mapping(..., mapping_gbif = "GBIF_ID")`.

- [ ] **Step 1: Write failing tests**

`tests/testthat/test-gbif-db.R`:

```r
gbif_csv <- function(dir, col = "GBIF_ID", ids = c("6186461308", ""), extra = list()) {
  m <- data.frame(ID = c("s1", "s2"), Taxon = "Fish a", R1 = c("a_1.fq", "b_1.fq"),
                  R2 = c("a_2.fq", "b_2.fq"))
  m[[col]] <- ids
  for (nm in names(extra)) m[[nm]] <- extra[[nm]]
  f <- file.path(dir, "mapping.csv")
  utils::write.csv(m, f, row.names = FALSE)
  f
}

gbif_q <- function(d, sql) {
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(d, ".sqlite"))
  on.exit(DBI::dbDisconnect(con))
  DBI::dbGetQuery(con, sql)
}

gbif_mem_db <- function() {
  f <- withr::local_tempfile(fileext = ".sqlite", .local_envir = parent.frame())
  con <- DBI::dbConnect(RSQLite::SQLite(), f)
  withr::defer(DBI::dbDisconnect(con), envir = parent.frame())
  DBI::dbWriteTable(con, "samples", data.frame(ID = c("s1", "s2"), Taxon = "x"))
  con
}

test_that(".meta_ensure_tables adds GBIF_ID and GBIF values normalize", {
  con <- gbif_mem_db()
  .meta_ensure_tables(con)
  expect_true(all(c("GEOME_BCID", "GBIF_ID") %in% DBI::dbListFields(con, "samples")))
  expect_equal(.meta_store_value("GBIF", c(" https://www.gbif.org/occurrence/6186461308 ", "junk", "", NA)),
               c("6186461308", "junk", NA, NA))
})

test_that(".meta_fetch_into stores GBIF levels next to GEOME and a refetch failure keeps data", {
  local_mocked_bindings(.gbif_get = gbif_fixture_get, .geome_get = geome_fixture_get)
  con <- gbif_mem_db()
  .meta_fetch_into(con, "GEOME", "s1", "ark:/21547/CYB2REEDY")
  .meta_fetch_into(con, "GBIF", "s1", "6186461308")
  lv <- DBI::dbGetQuery(con, "SELECT DISTINCT level FROM meta_records WHERE source = 'GBIF'")$level
  expect_setequal(lv, c("Occurrence", "Dataset", "Organization"))
  expect_setequal(DBI::dbGetQuery(con, "SELECT source FROM meta_status")$source, c("GEOME", "GBIF"))
  n <- DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM meta_records WHERE source = 'GBIF'")$n
  local_mocked_bindings(.gbif_get = function(path) stop("could not reach GBIF (timeout)", call. = FALSE))
  expect_warning(.meta_fetch_into(con, "GBIF", "s1", "6186461308"), "GBIF fetch failed for s1")
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM meta_records WHERE source = 'GBIF'")$n, n)
  st <- DBI::dbGetQuery(con, "SELECT status, message FROM meta_status WHERE source = 'GBIF'")
  expect_equal(st$status, "failed")
  expect_match(st$message, "could not reach GBIF")
  expect_gt(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM meta_records WHERE source = 'GEOME'")$n, 0)
})

test_that("a non-numeric GBIF ID is marked failed without calling GBIF", {
  local_mocked_bindings(.gbif_get = function(...) stop("should not be called"))
  con <- gbif_mem_db()
  res <- suppressWarnings(.meta_fetch_into(con, "GBIF", "s1", "abc"))
  expect_equal(res$status, "failed")
  expect_match(res$message, "not a GBIF occurrence ID")
})

test_that("fetch_gbif sets, fetches, and clears IDs without touching GEOME", {
  local_mocked_bindings(.gbif_get = gbif_fixture_get)
  d <- withr::local_tempdir()
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(d, ".sqlite"))
  DBI::dbWriteTable(con, "samples", data.frame(ID = c("s1", "s2"), Taxon = "x", GEOME_BCID = "ark:/1/A"))
  .meta_ensure_tables(con)
  DBI::dbExecute(con, "INSERT INTO meta_status VALUES ('s1', 'GEOME', 'ark:/1/A', 'ok', NULL, 1)")
  DBI::dbDisconnect(con)
  fetch_gbif(d, ids = c("s1", "s2"),
             gbifs = c("https://www.gbif.org/occurrence/6186461308", "2336663130"))
  expect_equal(gbif_q(d, "SELECT GBIF_ID FROM samples ORDER BY ID")$GBIF_ID, c("6186461308", "2336663130"))
  fetch_gbif(d, ids = "s1", gbifs = "")
  expect_true(is.na(gbif_q(d, "SELECT GBIF_ID FROM samples WHERE ID = 's1'")$GBIF_ID))
  expect_equal(gbif_q(d, "SELECT ID FROM meta_status WHERE source = 'GBIF'")$ID, "s2")
  expect_equal(gbif_q(d, "SELECT ID FROM meta_status WHERE source = 'GEOME'")$ID, "s1")
  expect_error(fetch_gbif(d, ids = c("s1", "s2"), gbifs = "1"), "ids and gbifs must be the same length")
  expect_message(res <- fetch_gbif(d, ids = "s1"), "No samples with a GBIF ID to fetch")
})

test_that("new_db stores numeric GBIF IDs from the CSV as digit strings", {
  local_mocked_bindings(.gbif_get = function(...) stop("should not be called"))
  d <- withr::local_tempdir()
  f <- gbif_csv(d, ids = c("6186461308", ""))
  expect_true(is.numeric(utils::read.csv(f)$GBIF_ID))
  new_db(db_path = file.path(d, ".sqlite"), mapping_fn = f, fetch_gbif = FALSE)
  expect_equal(gbif_q(d, "SELECT GBIF_ID FROM samples ORDER BY ID")$GBIF_ID, c("6186461308", NA))
  expect_equal(gbif_q(d, "SELECT COUNT(*) n FROM meta_status")$n, 0L)
})

test_that("new_db renames a custom GBIF column and fetches it", {
  local_mocked_bindings(.gbif_get = gbif_fixture_get)
  d <- withr::local_tempdir()
  f <- gbif_csv(d, col = "Occ", ids = c("https://www.gbif.org/occurrence/6186461308", ""))
  new_db(db_path = file.path(d, ".sqlite"), mapping_fn = f, mapping_gbif = "Occ")
  s <- gbif_q(d, "SELECT * FROM samples ORDER BY ID")
  expect_false("Occ" %in% names(s))
  expect_equal(s$GBIF_ID, c("6186461308", NA))
  st <- gbif_q(d, "SELECT ID, source, status FROM meta_status")
  expect_equal(st$ID, "s1")
  expect_equal(st$source, "GBIF")
  expect_equal(st$status, "ok")
})

test_that("new_db_userAsmb stores GBIF IDs and respects fetch_gbif = FALSE", {
  local_mocked_bindings(.gbif_get = function(...) stop("should not be called"))
  d <- withr::local_tempdir()
  mapping_fn <- file.path(d, "mapping.csv")
  utils::write.csv(
    data.frame(ID = c("s1", "s2"), Taxon = "Danio rerio",
               Assembly = c("s1.fasta", "s2.fasta"), Topology = c("linear", "circular"),
               GBIF_ID = c("6186461308", "")),
    mapping_fn, row.names = FALSE
  )
  new_db_userAsmb(db_path = file.path(d, ".sqlite"), mapping_fn = mapping_fn,
                  no_raw_data = TRUE, fetch_gbif = FALSE)
  expect_equal(gbif_q(d, "SELECT GBIF_ID FROM samples ORDER BY ID")$GBIF_ID, c("6186461308", NA))
})

test_that("check_mapping warns on a non-numeric GBIF ID and guards the reserved name", {
  m <- data.frame(ID = "s1", Taxon = "x", R1 = "a", R2 = "b", Occ = "abc")
  iss <- check_mapping(m, mapping_gbif = "Occ")
  expect_match(paste(iss$warnings, collapse = " "), "GBIF")
  iss <- check_mapping(m, mapping_gbif = "Nope")
  expect_match(paste(iss$errors, collapse = " "), "Nope")
  m$GBIF_ID <- "1"
  iss <- check_mapping(m, mapping_gbif = "Occ")
  expect_match(paste(iss$errors, collapse = " "), "GBIF_ID")
  iss <- check_mapping(data.frame(ID = "s1", Taxon = "x", R1 = "a", R2 = "b", GBIF_ID = 6186461308))
  expect_false(any(grepl("GBIF", iss$warnings)))
})

test_that("add_samples stores and fetches GBIF IDs for new samples", {
  local_mocked_bindings(.gbif_get = gbif_fixture_get)
  d <- withr::local_tempdir()
  new_db(db_path = file.path(d, ".sqlite"), mapping_fn = gbif_csv(d, ids = c("", "")))
  up <- data.frame(ID = "s3", Taxon = "x", R1 = "c_1", R2 = "c_2",
                   Occ = "https://www.gbif.org/occurrence/2336663130")
  utils::write.csv(up, file.path(d, "up.csv"), row.names = FALSE)
  add_samples(d, file.path(d, "up.csv"), mapping_gbif = "Occ")
  expect_equal(gbif_q(d, "SELECT GBIF_ID FROM samples WHERE ID = 's3'")$GBIF_ID, "2336663130")
  expect_false("Occ" %in% names(gbif_q(d, "SELECT * FROM samples")))
  expect_equal(gbif_q(d, "SELECT ID FROM meta_status WHERE source = 'GBIF'")$ID, "s3")
})

test_that("update_sample_metadata refetches changed GBIF IDs, drops cleared ones, leaves GEOME alone", {
  local_mocked_bindings(.gbif_get = gbif_fixture_get, .geome_get = geome_fixture_get)
  d <- withr::local_tempdir()
  f <- gbif_csv(d, ids = c("6186461308", "2336663130"),
                extra = list(GEOME_BCID = c("ark:/21547/CYB2REEDY", "")))
  new_db(db_path = file.path(d, ".sqlite"), mapping_fn = f)
  gbif_reset_calls()
  up <- data.frame(ID = c("s1", "s2"), Taxon = "x", GBIF_ID = c("", "2336663130"))
  utils::write.csv(up, file.path(d, "up.csv"), row.names = FALSE)
  update_sample_metadata(d, file.path(d, "up.csv"))
  expect_equal(gbif_q(d, "SELECT ID FROM meta_status WHERE source = 'GBIF'")$ID, "s2")
  expect_equal(gbif_q(d, "SELECT COUNT(*) n FROM meta_records WHERE ID = 's1' AND source = 'GBIF'")$n, 0L)
  expect_equal(gbif_q(d, "SELECT ID FROM meta_status WHERE source = 'GEOME'")$ID, "s1")
  expect_equal(length(ls(gbif_calls)), 0L)
  up$GBIF_ID <- c("6186461308", "2336663130")
  utils::write.csv(up, file.path(d, "up.csv"), row.names = FALSE)
  update_sample_metadata(d, file.path(d, "up.csv"))
  expect_setequal(gbif_q(d, "SELECT ID FROM meta_status WHERE source = 'GBIF'")$ID, c("s1", "s2"))
})
```

- [ ] **Step 2: Run, verify fail**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-gbif-db.R")'`
Expected: FAIL (`GBIF_ID` missing from samples; `unused argument (fetch_gbif = FALSE)`).

- [ ] **Step 3: Register GBIF and add hook helpers in `R/meta_db.R`**

Add a `GBIF` element to `META_SOURCES` after `GEOME`:

```r
  GBIF = list(
    col = "GBIF_ID", label = "GBIF", id_label = "ID", arg = "gbifs",
    normalize = function(x) gbif_normalize_id(x),
    invalid = function(x) paste0("'", x, "' is not a GBIF occurrence ID (expected digits)"),
    chain = function(ref, cache) .gbif_fetch_chain(ref, cache)
  )
```

Append to `R/meta_db.R`:

```r
.meta_take_cols <- function(mapping, cols) {
  for (src in names(cols)) {
    col <- cols[[src]]
    std <- META_SOURCES[[src]]$col
    if (col %in% colnames(mapping)) {
      mapping[[std]] <- .meta_store_value(src, mapping[[col]])
      if (col != std) mapping[[col]] <- NULL
    }
  }
  mapping
}

.meta_fetch_new <- function(con, mapping, fetch) {
  .meta_ensure_tables(con)
  for (src in names(fetch)) {
    col <- META_SOURCES[[src]]$col
    if (!isTRUE(fetch[[src]]) || !col %in% colnames(mapping)) next
    has <- !is.na(mapping[[col]])
    if (any(has)) .meta_fetch_into(con, src, mapping$ID[has], mapping[[col]][has])
  }
  invisible(NULL)
}

.meta_sync_changed <- function(con, mapping, old, fetch) {
  for (src in names(fetch)) {
    col <- META_SOURCES[[src]]$col
    if (!col %in% colnames(mapping)) next
    .meta_ensure_tables(con)
    new <- mapping[[col]]
    prev <- if (col %in% colnames(old)) {
      unname(stats::setNames(old[[col]], old$ID)[mapping$ID])
    } else {
      rep(NA_character_, length(new))
    }
    changed <- xor(is.na(new), is.na(prev)) | (!is.na(new) & !is.na(prev) & new != prev)
    if (any(changed)) .meta_drop(con, src, mapping$ID[changed])
    refetch <- changed & !is.na(new)
    if (isTRUE(fetch[[src]]) && any(refetch)) {
      .meta_fetch_into(con, src, mapping$ID[refetch], new[refetch])
    }
  }
  invisible(NULL)
}
```

- [ ] **Step 4: `R/gbif_db.R`**

```r
#' Fetch GBIF occurrence metadata for project samples
#'
#' Looks up each sample's GBIF occurrence (gbifID), plus the dataset and
#' publisher it belongs to, and stores everything in the project database for
#' viewing in the app and use at export. GBIF IDs can change when a dataset is
#' republished; a sample whose ID no longer resolves keeps its previous data
#' and shows a failed fetch.
#'
#' @param path Path to the project directory (default = current working directory)
#' @param ids Sample IDs to fetch. Default: every sample with a GBIF ID.
#' @param gbifs Optional GBIF occurrence IDs (or gbif.org occurrence links) to
#'   set for `ids` first (same length as `ids`). A blank value removes that
#'   sample's GBIF ID and its GBIF data.
#' @return Invisibly, a data frame of `ID`, `status`, and `message`.
#' @export
fetch_gbif <- function(path = ".", ids = NULL, gbifs = NULL) {
  .meta_fetch_project(path, "GBIF", ids, gbifs)
}
```

- [ ] **Step 5: `check_mapping()` and `preflight_project()` in `R/init_checks.R`**

Signature: add `mapping_gbif = "GBIF_ID",` after `mapping_geome = "GEOME_BCID",`.

After the existing `if (mapping_geome != "GEOME_BCID") { ... }` block add:

```r
  if (mapping_gbif != "GBIF_ID") {
    if (mapping_gbif %nin% cols) {
      iss$err("mapping columns: GBIF ID column '", mapping_gbif, "' not found")
    }
    reserved <- c(reserved, "GBIF_ID")
  }
```

After the `# GEOME BCIDs ----` block add:

```r
  # GBIF IDs ----
  if (mapping_gbif %in% cols) {
    raw <- trimws(.meta_chr(mapping[[mapping_gbif]]))
    raw[is.na(raw)] <- ""
    bad <- nzchar(raw) & is.na(gbif_normalize_id(raw))
    if (any(bad)) {
      iss$warn("mapping GBIF ID: not a GBIF occurrence ID (digits) for ",
               .lst(lab[bad]), "; these samples will show a failed GBIF fetch")
    }
  }
```

In `preflight_project()`, in the `check_mapping(...)` call add `mapping_gbif = dots$mapping_gbif %||% "GBIF_ID",` after the `mapping_geome = ...` line. After the GEOME reachability block add:

```r
  gbif_col <- dots$mapping_gbif %||% "GBIF_ID"
  if (!is.null(mapping) && gbif_col %in% colnames(mapping) &&
      !isFALSE(dots$fetch_gbif) &&
      any(!is.na(gbif_normalize_id(mapping[[gbif_col]])))) {
    .check_resource("https://api.gbif.org/v1/enumeration/country", "GBIF", iss = iss)
  }
```

- [ ] **Step 6: `new_db()` and `new_db_userAsmb()`**

In both files:

Roxygen, after the `@param fetch_geome` lines:

```r
#' @param mapping_gbif Name of the mapping-file column holding GBIF occurrence
#'   IDs (optional). Stored as `GBIF_ID`. See `vignette("Specimen-Metadata")`.
#' @param fetch_gbif Fetch GBIF metadata for samples with a GBIF ID during setup
#'   (default TRUE). Set FALSE when offline and run [fetch_gbif()] later.
```

Signature, after `fetch_geome = TRUE,`:

```r
    mapping_gbif = "GBIF_ID",
    fetch_gbif = TRUE,
```

`check_mapping()` call: add `mapping_gbif = mapping_gbif` next to `mapping_geome = mapping_geome`.

Replace the rename block

```r
  if (mapping_geome %in% colnames(mapping)) {
    mapping$GEOME_BCID <- .meta_store_value("GEOME", mapping[[mapping_geome]])
    if (mapping_geome != "GEOME_BCID") mapping[[mapping_geome]] <- NULL
  }
```

with

```r
  mapping <- .meta_take_cols(mapping, c(GEOME = mapping_geome, GBIF = mapping_gbif))
```

Replace the end-of-function block

```r
  .meta_ensure_tables(con)
  if (fetch_geome && "GEOME_BCID" %in% colnames(mapping) && any(!is.na(mapping$GEOME_BCID))) {
    has <- !is.na(mapping$GEOME_BCID)
    .meta_fetch_into(con, "GEOME", mapping$ID[has], mapping$GEOME_BCID[has])
  }
```

with

```r
  .meta_fetch_new(con, mapping, list(GEOME = fetch_geome, GBIF = fetch_gbif))
```

- [ ] **Step 7: `new_project()` and `new_project_userAsmb()`**

Roxygen after the `@param fetch_geome` block:

```r
#' @param mapping_gbif Name of the mapping-file column holding GBIF occurrence
#'   IDs (optional). Stored as `GBIF_ID`. See `vignette("Specimen-Metadata")`.
#'   Passed to `new_db()`.
#' @param fetch_gbif Fetch GBIF metadata for samples with a GBIF ID during setup
#'   (default TRUE). Set FALSE when offline and run [fetch_gbif()] later.
#'   Passed to `new_db()`.
```

(`new_project_userAsmb()`: "Passed to `new_db_userAsmb()`.")

Signature after `fetch_geome = TRUE,`: `mapping_gbif = "GBIF_ID", fetch_gbif = TRUE,`. After `dots$fetch_geome <- fetch_geome` add:

```r
  dots$mapping_gbif <- mapping_gbif
  dots$fetch_gbif <- fetch_gbif
```

In the `new_db(...)` / `new_db_userAsmb(...)` call add `mapping_gbif = mapping_gbif, fetch_gbif = fetch_gbif,` after `fetch_geome = fetch_geome,`.

- [ ] **Step 8: `add_samples()` and `update_sample_metadata()`**

Both: roxygen after `@param fetch_geome`:

```r
#' @param mapping_gbif Name of the mapping-file column holding GBIF occurrence IDs
#' @param fetch_gbif Fetch GBIF metadata for samples with a GBIF ID
#'   (default TRUE). Set FALSE when offline and run [fetch_gbif()] later.
```

Signature: add `mapping_gbif = "GBIF_ID", fetch_gbif = TRUE` after `fetch_geome = TRUE` (mind the commas: `fetch_geome = TRUE,` then the two new args, last one without comma).

`add_samples()`: replace the rename block (same four lines as Step 6) with `mapping <- .meta_take_cols(mapping, c(GEOME = mapping_geome, GBIF = mapping_gbif))`, and replace

```r
  .meta_ensure_tables(con)
  if (fetch_geome && "GEOME_BCID" %in% colnames(mapping)) {
    has <- !is.na(mapping$GEOME_BCID)
    if (any(has)) .meta_fetch_into(con, "GEOME", mapping$ID[has], mapping$GEOME_BCID[has])
  }
```

with `.meta_fetch_new(con, mapping, list(GEOME = fetch_geome, GBIF = fetch_gbif))`.

`update_sample_metadata()`: replace the rename block with the same `.meta_take_cols(...)` line; delete the `old_bcid <- if (...) {...} else {...}` block; replace the final

```r
  if ("GEOME_BCID" %in% colnames(mapping)) {
    .meta_ensure_tables(con)
    new <- mapping$GEOME_BCID
    old <- unname(old_bcid[mapping$ID])
    changed <- xor(is.na(new), is.na(old)) | (!is.na(new) & !is.na(old) & new != old)
    if (any(changed)) .meta_drop(con, "GEOME", mapping$ID[changed])
    refetch <- changed & !is.na(new)
    if (fetch_geome && any(refetch)) {
      .meta_fetch_into(con, "GEOME", mapping$ID[refetch], new[refetch])
    }
  }
```

with

```r
  .meta_sync_changed(con, mapping, sample_table, list(GEOME = fetch_geome, GBIF = fetch_gbif))
```

(`sample_table` holds the pre-update values; the new-column loop only adds `NA` columns to it.)

- [ ] **Step 9: Run tests, verify pass, run the neighbour suites**

```bash
Rscript -e 'devtools::load_all(); for (f in c("test-gbif-db.R","test-geome-db.R","test-geome-init.R","test-geome-update.R","test-meta-db.R","test-init-checks.R","test-init-db-userasmb.R","test-map-to-ref-refs.R")) testthat::test_file(file.path("tests/testthat", f))'
```

Expected: all PASS.

- [ ] **Step 10: pkgdown, document, commit**

`_pkgdown.yml` reference: add `  - fetch_gbif` right after `  - fetch_geome`.

```bash
Rscript -e 'devtools::document()'
git add R/meta_db.R R/gbif_db.R R/init_checks.R R/init_db.R R/init_db_userAsmb.R R/init_project.R R/init_project_userAsmb.R R/add_samples.R R/update_sample_metadata.R tests/testthat/test-gbif-db.R NAMESPACE man/ _pkgdown.yml
git commit -m "GBIF IDs: storage, fetch_gbif(), and project setup hooks"
```

---

### Task 4: GBIF GenBank-ready combinations and export columns

**Files:**
- Create: `R/gbif_export.R`
- Modify: `R/meta_export.R` (`.meta_combos()` gains `gbif`; add `.meta_ymd()`)
- Modify: `R/geome_export.R` (`GEOME_COMBOS$collection_date` uses `.meta_ymd()`)
- Modify: `R/app_export_utils.R` (`export_metadata_cols()` owned list)
- Test: `tests/testthat/test-gbif-export.R`

**Interfaces:**
- Consumes: `.geome_pick`, `.geome_coord`, `GEOME_COMBOS` (existing); `.gbif_fetch_chain` (Task 2); `.meta_save_fields`, `meta_export_cols`, `meta_field_summary` (Task 1).
- Produces:
  - `.meta_ymd(y, m, d)`: `YYYY`, `YYYY-MM`, `YYYY-MM-DD`, or `NA` (y must be four digits; bad month drops month and day; bad day drops day).
  - `.gbif_occ(recs, field)`: first non-empty value of `field` on the Occurrence level, else `NA`.
  - `GBIF_COMBOS`: named list `lat_lon, collection_date, geo_loc_name, specimen_voucher, collected_by, identified_by, sex, dev_stage`; each `list(label, sources, fn)`, `fn(recs)` -> string or `NA`.
  - `.meta_combos("gbif")` returns `GBIF_COMBOS`; export keys `gbif:combo:<name>` and `gbif:raw:Occurrence:<field>` resolve through `meta_export_cols()`.

- [ ] **Step 1: Write failing tests**

`tests/testthat/test-gbif-export.R`:

```r
occ <- function(...) {
  x <- list(...)
  data.frame(level = "Occurrence", depth = 0L, field = names(x), value = unlist(x))
}

gbif_recs <- function(id) {
  local_mocked_bindings(.gbif_get = gbif_fixture_get)
  .gbif_fetch_chain(id)
}

test_that("GBIF combos build GenBank values from the fish occurrence", {
  r <- gbif_recs("6186461308")
  v <- function(nm) GBIF_COMBOS[[nm]]$fn(r)
  expect_equal(v("lat_lon"), "28.53783 N 81.33322 W")
  expect_equal(v("collection_date"), "2026-02-23")
  expect_equal(v("geo_loc_name"), "United States of America: Florida, Lake Underhill")
  expect_equal(v("specimen_voucher"), "UF:Fish:250399")
  expect_equal(v("collected_by"), "Robins, Robert; Casteel, Jamie; Lange, Ted")
  expect_true(is.na(v("sex")))
})

test_that("GBIF combos skip an ARK catalog number and lowercase life stage", {
  r <- gbif_recs("2336663130")
  expect_true(is.na(GBIF_COMBOS$specimen_voucher$fn(r)))
  expect_equal(GBIF_COMBOS$dev_stage$fn(r), "adult")
  expect_equal(GBIF_COMBOS$collection_date$fn(r), "2026")
  expect_equal(GBIF_COMBOS$lat_lon$fn(r), "14.27374 S 170.58225 W")
})

test_that("gbif lat_lon is empty when an issue flags the coordinates", {
  f <- GBIF_COMBOS$lat_lon$fn
  expect_equal(f(occ(decimalLatitude = "10.5", decimalLongitude = "-20", issues = "COORDINATE_ROUNDED")),
               "10.5 N 20 W")
  for (iss in c("ZERO_COORDINATE", "COORDINATE_INVALID", "COORDINATE_OUT_OF_RANGE")) {
    expect_true(is.na(f(occ(decimalLatitude = "0", decimalLongitude = "0",
                            issues = paste0("GEODETIC_DATUM_ASSUMED_WGS84,", iss)))))
  }
  expect_true(is.na(f(occ(decimalLatitude = "10"))))
})

test_that("gbif collection_date falls back to a single eventDate and ignores intervals", {
  f <- GBIF_COMBOS$collection_date$fn
  expect_equal(f(occ(eventDate = "2019-07-04T10:00:00")), "2019-07-04")
  expect_equal(f(occ(eventDate = "2019-07")), "2019-07")
  expect_true(is.na(f(occ(eventDate = "2019-07-01/2019-07-09"))))
  expect_equal(f(occ(year = "2019", month = "7", eventDate = "2019-07-01/2019-07-09")), "2019-07")
  expect_true(is.na(f(occ(eventDate = "July 2019"))))
})

test_that("gbif specimen_voucher builds the institution:collection:catalog triplet", {
  f <- GBIF_COMBOS$specimen_voucher$fn
  expect_equal(f(occ(institutionCode = "UF", collectionCode = "Fish", catalogNumber = "250399")), "UF:Fish:250399")
  expect_equal(f(occ(institutionCode = "UF", catalogNumber = "250399")), "UF:250399")
  expect_equal(f(occ(collectionCode = "Fish", catalogNumber = "250399")), "250399")
  expect_true(is.na(f(occ(institutionCode = "UF"))))
  expect_true(is.na(f(occ(catalogNumber = "https://n2t.net/ark:/21547/pm2269.04"))))
  expect_true(is.na(f(occ(catalogNumber = "ark:/21547/pm2269.04"))))
})

test_that("gbif combos read only the Occurrence level", {
  r <- rbind(occ(sex = "Female"),
             data.frame(level = "Dataset", depth = 1L, field = "country", value = "US"))
  expect_equal(GBIF_COMBOS$sex$fn(r), "female")
  expect_true(is.na(GBIF_COMBOS$geo_loc_name$fn(r)))
})

test_that("GEOME collection_date is unchanged by the shared date helper", {
  expect_equal(.meta_ymd("2009", "11", "5"), "2009-11-05")
  expect_equal(.meta_ymd("2009", "13", "5"), "2009")
  expect_equal(.meta_ymd("2009", "11", "40"), "2009-11")
  expect_true(is.na(.meta_ymd("09", NA, NA)))
})

test_that("ticked GBIF keys join as gbif_ columns and appear in the field summary", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbWriteTable(con, "samples", data.frame(ID = c("s1", "s2"), Taxon = "x"))
  .meta_ensure_tables(con)
  DBI::dbAppendTable(con, "meta_records", data.frame(
    ID = "s1", source = "GBIF", level = "Occurrence", depth = 0L, ref = "1",
    field = c("countryCode", "catalogNumber", "institutionCode"), value = c("US", "250399", "UF")))
  .meta_save_fields(con, c("gbif:combo:specimen_voucher", "gbif:raw:Occurrence:countryCode"))
  out <- meta_export_cols(con, c("s1", "s2"))
  expect_equal(out$gbif_specimen_voucher, c("UF:250399", NA))
  expect_equal(out$gbif_Occurrence_countryCode, c("US", NA))
  s <- meta_field_summary(con, "GBIF")
  expect_true(all(paste0("gbif:combo:", names(GBIF_COMBOS)) %in% s$key))
  expect_true(s$selected[s$key == "gbif:raw:Occurrence:countryCode"])
  expect_equal(s$col[s$key == "gbif:combo:lat_lon"], "gbif_lat_lon")
  expect_false(any(grepl("^geome:", s$key)))
})

test_that("export_metadata_cols treats GBIF_ID as owned", {
  expect_equal(export_metadata_cols(c("ID", "Taxon", "GBIF_ID", "GEOME_BCID", "site"), character()), "site")
})
```

- [ ] **Step 2: Run, verify fail**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-gbif-export.R")'`
Expected: FAIL, `object 'GBIF_COMBOS' not found`.

- [ ] **Step 3: Shared date helper and combos lookup in `R/meta_export.R`**

Replace `.meta_combos()` with:

```r
.meta_combos <- function(prefix) {
  switch(tolower(prefix), geome = GEOME_COMBOS, gbif = GBIF_COMBOS, NULL)
}
```

Add:

```r
.meta_ymd <- function(y, m, d) {
  if (is.na(y) || !grepl("^[0-9]{4}$", y)) return(NA_character_)
  m <- suppressWarnings(as.integer(m))
  d <- suppressWarnings(as.integer(d))
  if (is.na(m) || m < 1 || m > 12) return(y)
  if (is.na(d) || d < 1 || d > 31) return(sprintf("%s-%02d", y, m))
  sprintf("%s-%02d-%02d", y, m, d)
}
```

In `R/geome_export.R` replace the `collection_date` element of `GEOME_COMBOS` with:

```r
  collection_date = list(
    label = "collection_date", sources = c("yearCollected", "monthCollected", "dayCollected"),
    fn = function(recs) {
      .meta_ymd(.geome_pick(recs, "yearCollected"), .geome_pick(recs, "monthCollected"),
                .geome_pick(recs, "dayCollected"))
    }
  ),
```

- [ ] **Step 4: `R/gbif_export.R`**

```r
.gbif_occ <- function(recs, field) {
  .geome_pick(recs[recs$level == "Occurrence", , drop = FALSE], field)
}

.gbif_bad_coord_issues <- c("ZERO_COORDINATE", "COORDINATE_INVALID", "COORDINATE_OUT_OF_RANGE")

.gbif_passthrough <- function(field, label, lower = FALSE) {
  list(label = label, sources = field, fn = function(recs) {
    v <- .gbif_occ(recs, field)
    if (lower) tolower(v) else v
  })
}

GBIF_COMBOS <- list(
  lat_lon = list(
    label = "lat_lon", sources = c("decimalLatitude", "decimalLongitude"),
    fn = function(recs) {
      iss <- strsplit(.gbif_occ(recs, "issues") %|NA|% "", ",", fixed = TRUE)[[1]]
      if (any(iss %in% .gbif_bad_coord_issues)) return(NA_character_)
      la <- .geome_coord(.gbif_occ(recs, "decimalLatitude"), 90)
      lo <- .geome_coord(.gbif_occ(recs, "decimalLongitude"), 180)
      if (is.null(la) || is.null(lo)) return(NA_character_)
      paste(la$txt, if (la$neg) "S" else "N", lo$txt, if (lo$neg) "W" else "E")
    }
  ),
  collection_date = list(
    label = "collection_date", sources = c("year", "month", "day", "eventDate"),
    fn = function(recs) {
      out <- .meta_ymd(.gbif_occ(recs, "year"), .gbif_occ(recs, "month"), .gbif_occ(recs, "day"))
      if (!is.na(out)) return(out)
      ed <- .gbif_occ(recs, "eventDate")
      if (is.na(ed) || grepl("/", ed, fixed = TRUE)) return(NA_character_)
      hit <- regmatches(ed, regexec("^([0-9]{4})(-([0-9]{2}))?(-([0-9]{2}))?([T ].*)?$", ed))[[1]]
      if (!length(hit)) return(NA_character_)
      .meta_ymd(hit[2], hit[4], hit[6])
    }
  ),
  geo_loc_name = list(
    label = "geo_loc_name", sources = c("country", "stateProvince", "locality"),
    fn = function(recs) GEOME_COMBOS$geo_loc_name$fn(recs[recs$level == "Occurrence", , drop = FALSE])
  ),
  specimen_voucher = list(
    label = "specimen_voucher", sources = c("institutionCode", "collectionCode", "catalogNumber"),
    fn = function(recs) {
      cat <- .gbif_occ(recs, "catalogNumber")
      if (is.na(cat) || grepl("^[a-z]+://|ark:/", cat, ignore.case = TRUE)) return(NA_character_)
      inst <- .gbif_occ(recs, "institutionCode")
      coll <- .gbif_occ(recs, "collectionCode")
      if (is.na(inst)) return(cat)
      if (is.na(coll)) paste0(inst, ":", cat) else paste(inst, coll, cat, sep = ":")
    }
  ),
  collected_by = .gbif_passthrough("recordedBy", "collected_by"),
  identified_by = .gbif_passthrough("identifiedBy", "identified_by"),
  sex = .gbif_passthrough("sex", "sex", lower = TRUE),
  dev_stage = .gbif_passthrough("lifeStage", "dev_stage", lower = TRUE)
)
```

Note: `R/gbif_export.R` sorts after `R/geome_export.R`, so `GEOME_COMBOS` exists when `GBIF_COMBOS` is built; `geo_loc_name` only calls it at run time anyway.

- [ ] **Step 5: Owned columns**

In `export_metadata_cols()` (`R/app_export_utils.R`), add `"GBIF_ID"` to `owned` right after `"GEOME_BCID"`.

- [ ] **Step 6: Run tests, verify pass**

```bash
Rscript -e 'devtools::load_all(); for (f in c("test-gbif-export.R","test-geome-export.R","test-export-metadata-cols.R","test-meta-db.R")) testthat::test_file(file.path("tests/testthat", f))'
```

Expected: all PASS.

- [ ] **Step 7: Commit**

```bash
git add R/gbif_export.R R/meta_export.R R/geome_export.R R/app_export_utils.R tests/testthat/test-gbif-export.R
git commit -m "GBIF GenBank combos and export columns"
```

---

### Task 5: Country table and per-concept comparison rules

**Files:**
- Create: `R/specimen_reconcile.R` (normalizers and pairwise rules; Task 6 appends the engine)
- Create: `data-raw/build_countries.R`, `inst/extdata/countries.csv`
- Test: `tests/testthat/test-specimen-rules.R`

**Interfaces:**
- Consumes: `app_sys()` (existing, `R/app_config.R`).
- Produces:
  - `.spec_norm_name(x)`: lowercase ASCII, runs of non-alphanumerics collapsed to one space, trimmed (vectorized).
  - `.spec_country_iso2(x)`: scalar; ISO2 code or `NA`. Text after the first `:` is dropped first (so `geo_loc_name` values work). Names, aliases, ISO2, and ISO3 codes all map.
  - `.spec_parse_coords(x)`: numeric `c(lat, lon)` or `NULL`; accepts `d.dd N/S d.dd E/W` and decimal pairs separated by space, comma, or semicolon.
  - `.spec_parse_date(x)`: integer `c(y =, m =, d =)` (missing precision `NA`) or `NULL`; accepts `YYYY`, `YYYY-MM`, `YYYY-MM-DD` (optional time suffix), `Mmm-YYYY`, `DD-Mmm-YYYY`; intervals (`/`) are `NULL`.
  - `.spec_compare(concept, a, b)`: `"agree"`, `"note"`, or `"conflict"` for two non-blank values.
  - `.spec_status(concept, vals)`: `NA` (no values), `"single"`, or the worst pairwise result (`conflict` > `note` > `agree`).

- [ ] **Step 1: Write failing tests**

`tests/testthat/test-specimen-rules.R`:

```r
test_that(".spec_norm_name folds case, accents, and punctuation", {
  expect_equal(.spec_norm_name(c("Côte d’Ivoire", "  United  States of America ", "U.S.A.")),
               c("cote d ivoire", "united states of america", "u s a"))
})

test_that("country names, aliases, and codes map to ISO2", {
  x <- c("USA", "United States", "united states of america", "US", "USA: Florida",
         "Côte d’Ivoire", "Ivory Coast", "Namibia", "NA", "Atlantis", "", NA)
  expect_equal(vapply(x, .spec_country_iso2, "", USE.NAMES = FALSE),
               c(rep("US", 5), "CI", "CI", "NA", "NA", NA, NA, NA))
})

test_that("coordinates parse in decimal and hemisphere forms", {
  expect_equal(.spec_parse_coords("17.5 S 149.8 W"), c(-17.5, -149.8))
  expect_equal(.spec_parse_coords("28.5378, -81.3332"), c(28.5378, -81.3332))
  expect_equal(.spec_parse_coords("28.5 -81.3"), c(28.5, -81.3))
  expect_null(.spec_parse_coords("95 10"))
  expect_null(.spec_parse_coords("near the lake"))
  expect_null(.spec_parse_coords(NA))
})

test_that("dates parse in ISO and GenBank forms", {
  expect_equal(.spec_parse_date("2026-02-23"), c(y = 2026L, m = 2L, d = 23L))
  expect_equal(.spec_parse_date("2026-02-23T10:00:00"), c(y = 2026L, m = 2L, d = 23L))
  expect_equal(.spec_parse_date("23-Feb-2026"), c(y = 2026L, m = 2L, d = 23L))
  expect_equal(.spec_parse_date("Feb-2026"), c(y = 2026L, m = 2L, d = NA))
  expect_equal(.spec_parse_date("2026"), c(y = 2026L, m = NA, d = NA))
  expect_null(.spec_parse_date("2020-01-01/2020-12-31"))
  expect_null(.spec_parse_date("spring 2020"))
})

test_that(".spec_compare applies each concept's rule", {
  cmp <- .spec_compare
  expect_equal(cmp("coordinates", "28.5378, -81.3332", "28.53783 N 81.33322 W"), "agree")
  expect_equal(cmp("coordinates", "28.53, -81.33", "28.54 N 81.33 W"), "agree")
  expect_equal(cmp("coordinates", "28.52, -81.33", "28.54 N 81.33 W"), "conflict")
  expect_equal(cmp("coordinates", "near the lake", "28.5 N 81.3 W"), "note")
  expect_equal(cmp("collection_date", "spring 2020", "2020-04-01"), "note")
  expect_equal(cmp("collection_date", "2026", "2026-02-23"), "agree")
  expect_equal(cmp("collection_date", "23-Feb-2026", "2026-02-23"), "agree")
  expect_equal(cmp("collection_date", "Feb-2026", "2026-03-01"), "conflict")
  expect_equal(cmp("country", "USA: Florida", "US"), "agree")
  expect_equal(cmp("country", "United States of America", "USA"), "agree")
  expect_equal(cmp("country", "Canada", "US"), "conflict")
  expect_equal(cmp("country", "Atlantis", "US"), "note")
  expect_equal(cmp("country", "Atlantis: north", "atlantis"), "agree")
  expect_equal(cmp("voucher", "UF 250399", "UF:Fish:250399"), "agree")
  expect_equal(cmp("voucher", "uf fish 250399", "250399"), "agree")
  expect_equal(cmp("voucher", "250398", "UF:Fish:250399"), "conflict")
  expect_equal(cmp("taxon", "Notemigonus crysoleucas", "Notemigonus crysoleucas (Mitchill, 1814)"), "agree")
  expect_equal(cmp("taxon", "Notemigonus sp.", "Notemigonus crysoleucas"), "conflict")
  expect_equal(cmp("locality", " Lake  Underhill", "lake underhill"), "agree")
  expect_equal(cmp("locality", "Lake Underhill", "Lake Eola"), "note")
  expect_equal(cmp("collector", "R. Robins", "Robins, Robert"), "note")
  expect_equal(cmp("sex", "Female", "female"), "agree")
  expect_equal(cmp("dev_stage", "adult", "juvenile"), "note")
})

test_that(".spec_status reports single, the worst pair, or NA", {
  expect_true(is.na(.spec_status("sex", c(NA, "", NA))))
  expect_equal(.spec_status("sex", c("male", NA, NA)), "single")
  expect_equal(.spec_status("country", c("USA", NA, "US")), "agree")
  expect_equal(.spec_status("country", c("USA", "US", "Canada")), "conflict")
  expect_equal(.spec_status("collector", c(NA, "A. B", "Ann B")), "note")
})
```

- [ ] **Step 2: Run, verify fail**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-specimen-rules.R")'`
Expected: FAIL, `could not find function ".spec_norm_name"`.

- [ ] **Step 3: Implement the rules in `R/specimen_reconcile.R`**

```r
.spec_env <- new.env()

.spec_norm_name <- function(x) {
  x <- iconv(as.character(x), "UTF-8", "ASCII//TRANSLIT", sub = "")
  x <- gsub("[^a-z0-9]+", " ", tolower(x))
  trimws(x)
}

.spec_countries <- function() {
  if (is.null(.spec_env$countries)) {
    x <- utils::read.csv(app_sys("extdata", "countries.csv"), colClasses = "character",
                         na.strings = character())
    .spec_env$countries <- stats::setNames(x$iso2, x$name)
  }
  .spec_env$countries
}

.spec_country_iso2 <- function(x) {
  if (length(x) != 1L || is.na(x)) return(NA_character_)
  key <- .spec_norm_name(sub(":.*", "", x))
  if (!nzchar(key)) return(NA_character_)
  unname(.spec_countries()[key])
}

.spec_parse_coords <- function(x) {
  if (length(x) != 1L || is.na(x) || !nzchar(trimws(x))) return(NULL)
  x <- trimws(x)
  h <- regmatches(x, regexec("^([0-9.]+)\\s*([NSns])[ ,;]+([0-9.]+)\\s*([EWew])$", x))[[1]]
  if (length(h)) {
    la <- suppressWarnings(as.numeric(h[2])) * if (toupper(h[3]) == "S") -1 else 1
    lo <- suppressWarnings(as.numeric(h[4])) * if (toupper(h[5]) == "W") -1 else 1
  } else {
    d <- regmatches(x, regexec("^([+-]?[0-9.]+)\\s*[ ,;]\\s*([+-]?[0-9.]+)$", x))[[1]]
    if (!length(d)) return(NULL)
    la <- suppressWarnings(as.numeric(d[2]))
    lo <- suppressWarnings(as.numeric(d[3]))
  }
  if (is.na(la) || is.na(lo) || abs(la) > 90 || abs(lo) > 180) return(NULL)
  c(la, lo)
}

.spec_ymd <- function(y, m, d) {
  m <- suppressWarnings(as.integer(m))
  d <- suppressWarnings(as.integer(d))
  if (is.na(m) || m < 1 || m > 12) m <- NA_integer_
  if (is.na(m) || is.na(d) || d < 1 || d > 31) d <- NA_integer_
  c(y = as.integer(y), m = m, d = d)
}

.spec_parse_date <- function(x) {
  if (length(x) != 1L || is.na(x) || !nzchar(trimws(x))) return(NULL)
  x <- trimws(x)
  if (grepl("/", x, fixed = TRUE)) return(NULL)
  iso <- regmatches(x, regexec("^([0-9]{4})(-([0-9]{1,2}))?(-([0-9]{1,2}))?([T ].*)?$", x))[[1]]
  if (length(iso)) return(.spec_ymd(iso[2], iso[4], iso[6]))
  gb <- regmatches(x, regexec("^(([0-9]{1,2})-)?([A-Za-z]{3})-([0-9]{4})$", x))[[1]]
  if (length(gb)) {
    m <- match(tolower(gb[4]), tolower(month.abb))
    if (is.na(m)) return(NULL)
    return(.spec_ymd(gb[5], m, gb[3]))
  }
  NULL
}

.spec_same_date <- function(a, b) {
  if (a[["y"]] != b[["y"]]) return(FALSE)
  if (is.na(a[["m"]]) || is.na(b[["m"]])) return(TRUE)
  if (a[["m"]] != b[["m"]]) return(FALSE)
  if (is.na(a[["d"]]) || is.na(b[["d"]])) return(TRUE)
  a[["d"]] == b[["d"]]
}

.spec_norm_voucher <- function(x) {
  x <- sub(".*:", "", trimws(x))
  x <- sub("^([A-Za-z]+\\s+)+", "", x)
  tolower(gsub("\\s+", "", x))
}

.spec_binomial <- function(x) {
  paste(utils::head(strsplit(tolower(trimws(x)), "\\s+")[[1]], 2), collapse = " ")
}

.spec_squash <- function(x) gsub("\\s+", " ", tolower(trimws(x)))

.spec_compare <- function(concept, a, b) {
  switch(concept,
    coordinates = {
      pa <- .spec_parse_coords(a)
      pb <- .spec_parse_coords(b)
      if (is.null(pa) || is.null(pb)) "note"
      else if (all(abs(pa - pb) <= 0.01 + 1e-9)) "agree" else "conflict"
    },
    collection_date = {
      pa <- .spec_parse_date(a)
      pb <- .spec_parse_date(b)
      if (is.null(pa) || is.null(pb)) "note"
      else if (.spec_same_date(pa, pb)) "agree" else "conflict"
    },
    country = {
      ca <- .spec_country_iso2(a)
      cb <- .spec_country_iso2(b)
      if (!is.na(ca) && !is.na(cb)) {
        if (ca == cb) "agree" else "conflict"
      } else if (.spec_norm_name(sub(":.*", "", a)) == .spec_norm_name(sub(":.*", "", b))) {
        "agree"
      } else {
        "note"
      }
    },
    voucher = if (.spec_norm_voucher(a) == .spec_norm_voucher(b)) "agree" else "conflict",
    taxon = if (.spec_binomial(a) == .spec_binomial(b)) "agree" else "conflict",
    locality = ,
    collector = if (.spec_squash(a) == .spec_squash(b)) "agree" else "note",
    sex = ,
    dev_stage = if (tolower(trimws(a)) == tolower(trimws(b))) "agree" else "note"
  )
}

.spec_status <- function(concept, vals) {
  vals <- vals[!is.na(vals) & nzchar(trimws(vals))]
  if (!length(vals)) return(NA_character_)
  if (length(vals) == 1L) return("single")
  pairs <- utils::combn(length(vals), 2)
  st <- apply(pairs, 2, function(p) .spec_compare(concept, vals[p[1]], vals[p[2]]))
  if ("conflict" %in% st) "conflict" else if ("note" %in% st) "note" else "agree"
}
```

- [ ] **Step 4: Build `inst/extdata/countries.csv`**

`data-raw/build_countries.R` (`data-raw/` is already in `.Rbuildignore`):

```r
# Builds inst/extdata/countries.csv from GBIF's country enumeration plus common
# aliases. Run from the package root: Rscript data-raw/build_countries.R
devtools::load_all(quiet = TRUE)
x <- jsonlite::fromJSON("https://api.gbif.org/v1/enumeration/country")
aliases <- c(
  "USA" = "US", "United States" = "US", "U.S.A." = "US",
  "UK" = "GB", "United Kingdom" = "GB", "Great Britain" = "GB", "England" = "GB",
  "Scotland" = "GB", "Wales" = "GB",
  "Russia" = "RU", "South Korea" = "KR", "North Korea" = "KP", "Laos" = "LA",
  "Ivory Coast" = "CI", "Cote d'Ivoire" = "CI", "Czech Republic" = "CZ",
  "Cape Verde" = "CV", "Swaziland" = "SZ", "Macedonia" = "MK", "Turkey" = "TR",
  "Taiwan" = "TW", "Vietnam" = "VN", "Bolivia" = "BO", "Iran" = "IR", "Syria" = "SY",
  "Tanzania" = "TZ", "Venezuela" = "VE", "Moldova" = "MD", "Micronesia" = "FM",
  "Brunei" = "BN", "Burma" = "MM", "Democratic Republic of the Congo" = "CD",
  "DR Congo" = "CD", "Republic of the Congo" = "CG", "East Timor" = "TL",
  "Palestine" = "PS", "Vatican City" = "VA", "Falkland Islands" = "FK",
  "US Virgin Islands" = "VI", "British Virgin Islands" = "VG"
)
out <- data.frame(
  iso2 = c(x$iso2, x$iso2, x$iso2, x$iso2, unname(aliases)),
  name = .spec_norm_name(c(x$title, gsub("_", " ", x$enumName), x$iso2, x$iso3, names(aliases)))
)
out <- unique(out[nzchar(out$name), ])
dup <- out$name[duplicated(out$name)]
if (length(dup)) stop("names map to more than one country: ", paste(dup, collapse = ", "))
out <- out[order(out$iso2, out$name), ]
dir.create("inst/extdata", showWarnings = FALSE)
utils::write.csv(out, "inst/extdata/countries.csv", row.names = FALSE, quote = FALSE)
```

Run it and check the output:

```bash
cd ~/Documents/GitHub/MitoPilot-geome
Rscript data-raw/build_countries.R
wc -l inst/extdata/countries.csv
grep -P '[^\x00-\x7F]' inst/extdata/countries.csv || echo "ascii ok"
grep -E '^(US|CI|NA),' inst/extdata/countries.csv | head -20
```

Expected: roughly 1000-1100 lines, `ascii ok`, and rows such as `US,usa`, `US,united states of america`, `CI,cote d ivoire`, `NA,namibia`, `NA,na`. If the script stops on a duplicate, drop the offending alias from `aliases` and rerun.

- [ ] **Step 5: Run tests, verify pass**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-specimen-rules.R")'`
Expected: all PASS.

- [ ] **Step 6: Commit**

```bash
git add R/specimen_reconcile.R data-raw/build_countries.R inst/extdata/countries.csv tests/testthat/test-specimen-rules.R
git commit -m "Country table and comparison rules for specimen metadata"
```

---

### Task 6: CSV column detection, overrides, and `specimen_conflicts()`

**Files:**
- Modify: `R/specimen_reconcile.R` (append)
- Modify: `_pkgdown.yml` (reference, after `fetch_gbif`)
- Test: `tests/testthat/test-specimen-conflicts.R`

**Interfaces:**
- Consumes: Task 5 rules; `GEOME_COMBOS`, `.geome_pick` (existing); `GBIF_COMBOS`, `.gbif_occ` (Task 4); `.meta_combos`, `.meta_ensure_tables`, `.meta_chr` (Task 1).
- Produces:
  - `SPECIMEN_CONCEPTS`: `c("coordinates", "collection_date", "country", "locality", "voucher", "collector", "sex", "dev_stage", "taxon")`.
  - `.spec_detect_csv(cols, overrides = character())`: named list concept -> character vector of CSV columns (0, 1, or 2 for coordinates). `overrides` is a named character vector from `meta_csv_map` (`""` = none; `"A,B"` = two columns).
  - `specimen_csv_columns(con)`: `.spec_detect_csv()` on the samples columns plus stored overrides.
  - `specimen_conflicts(con, ids = NULL)`: data.frame `ID, concept, csv_column, csv_value, geome_value, gbif_value, status`, one row per sample x concept; `status` is `agree`, `note`, `conflict`, `single`, or `NA` (no source has a value). `csv_column` joins two columns with `" + "`.
  - `.spec_set_csv_map(con, map)`: `map` is a named list concept -> `NA` (back to auto), `""` (none), or column name(s). Validates everything before writing.
  - `set_metadata_columns(path = ".", ...)` (exported): invisible data.frame `concept, column` of what is now used.

- [ ] **Step 1: Write failing tests**

`tests/testthat/test-specimen-conflicts.R`:

```r
spec_db <- function() {
  f <- withr::local_tempfile(fileext = ".sqlite", .local_envir = parent.frame())
  con <- DBI::dbConnect(RSQLite::SQLite(), f)
  withr::defer(DBI::dbDisconnect(con), envir = parent.frame())
  DBI::dbWriteTable(con, "samples", data.frame(
    ID = c("s1", "s2", "s3"),
    Taxon = c("Notemigonus crysoleucas", "Nerita albicilla", "Fish b"),
    Latitude = c("28.5378", "-14.2", NA), Longitude = c("-81.3332", "-170.6", NA),
    Date = c("23-Feb-2026", "2025", NA), geo_loc_name = c("USA: Florida", "American Samoa", NA),
    Voucher = c("UF 250399", NA, NA)))
  .meta_ensure_tables(con)
  add <- function(id, source, level, field, value) {
    DBI::dbAppendTable(con, "meta_records", data.frame(
      ID = id, source = source, level = level, depth = 0L, ref = "r", field = field, value = value))
  }
  add("s1", "GBIF", "Occurrence",
      c("decimalLatitude", "decimalLongitude", "year", "month", "day", "countryCode",
        "catalogNumber", "institutionCode", "collectionCode", "scientificName", "locality"),
      c("28.53783", "-81.33322", "2026", "2", "23", "US", "250399", "UF", "Fish",
        "Notemigonus crysoleucas (Mitchill, 1814)", "Lake Underhill"))
  add("s2", "GEOME", "Event",
      c("yearCollected", "country", "locality", "collectorList", "scientificName"),
      c("2026", "Samoa", "Amouli beach", "E. Crandall", "Nerita albicilla"))
  add("s2", "GBIF", "Occurrence",
      c("countryCode", "locality", "recordedBy", "scientificName", "catalogNumber"),
      c("AS", "Amouli Beach", "Eric Crandall", "Nerita albicilla Linnaeus, 1758",
        "https://n2t.net/ark:/21547/pm2269.04"))
  con
}

st <- function(cf, id, k) cf$status[cf$ID == id & cf$concept == k]

test_that(".spec_detect_csv finds columns case-insensitively, first candidate wins", {
  d <- .spec_detect_csv(c("ID", "Taxon", "LAT", "long", "eventDate", "date", "collection_date"))
  expect_equal(d$coordinates, c("LAT", "long"))
  expect_equal(d$collection_date, "collection_date")
  expect_equal(d$taxon, "Taxon")
  expect_equal(d$sex, character())
  expect_equal(.spec_detect_csv(c("lat_lon", "lat", "lon"))$coordinates, "lat_lon")
  o <- .spec_detect_csv(c("lat", "lon", "Where", "x", "y"),
                        c(country = "Where", coordinates = "x,y", sex = ""))
  expect_equal(o$country, "Where")
  expect_equal(o$coordinates, c("x", "y"))
  expect_equal(o$sex, character())
  expect_equal(names(o), SPECIMEN_CONCEPTS)
})

test_that("specimen_conflicts flags agree, note, conflict, single, and empty", {
  con <- spec_db()
  cf <- specimen_conflicts(con)
  expect_equal(nrow(cf), 3L * length(SPECIMEN_CONCEPTS))
  expect_equal(names(cf), c("ID", "concept", "csv_column", "csv_value", "geome_value", "gbif_value", "status"))
  expect_equal(st(cf, "s1", "coordinates"), "agree")
  expect_equal(cf$csv_column[cf$ID == "s1" & cf$concept == "coordinates"], "Latitude + Longitude")
  expect_equal(cf$csv_value[cf$ID == "s1" & cf$concept == "coordinates"], "28.5378, -81.3332")
  expect_equal(st(cf, "s1", "collection_date"), "agree")
  expect_equal(st(cf, "s1", "country"), "agree")
  expect_equal(st(cf, "s1", "voucher"), "agree")
  expect_equal(st(cf, "s1", "taxon"), "agree")
  expect_equal(st(cf, "s1", "locality"), "single")
  expect_equal(st(cf, "s2", "collection_date"), "conflict")
  expect_equal(st(cf, "s2", "country"), "conflict")
  expect_equal(st(cf, "s2", "locality"), "agree")
  expect_equal(st(cf, "s2", "collector"), "note")
  expect_equal(st(cf, "s2", "voucher"), NA_character_)
  expect_equal(st(cf, "s2", "coordinates"), "single")
  expect_equal(cf$gbif_value[cf$ID == "s2" & cf$concept == "country"], "AS")
  expect_equal(st(cf, "s3", "taxon"), "single")
  expect_true(all(is.na(cf$status[cf$ID == "s3" & cf$concept != "taxon"])))
  expect_equal(unique(specimen_conflicts(con, ids = "s2")$ID), "s2")
})

test_that("stored CSV overrides change what is compared, NA restores detection", {
  con <- spec_db()
  .spec_set_csv_map(con, list(collection_date = ""))
  cf <- specimen_conflicts(con)
  expect_equal(st(cf, "s2", "collection_date"), "single")
  expect_true(is.na(cf$csv_column[cf$concept == "collection_date"][1]))
  .spec_set_csv_map(con, list(collection_date = NA))
  expect_equal(st(specimen_conflicts(con), "s2", "collection_date"), "conflict")
  .spec_set_csv_map(con, list(coordinates = c("Longitude", "Latitude")))
  expect_equal(st(specimen_conflicts(con), "s1", "coordinates"), "conflict")
})

test_that(".spec_set_csv_map validates before writing anything", {
  con <- spec_db()
  expect_error(.spec_set_csv_map(con, list(country = "geo_loc_name", colour = "x")), "unknown concept")
  expect_error(.spec_set_csv_map(con, list(taxon = "Taxon")), "unknown concept")
  expect_error(.spec_set_csv_map(con, list(country = "Nope")), "not in the samples table")
  expect_error(.spec_set_csv_map(con, list(sex = c("Date", "Voucher"))), "too many columns")
  expect_error(.spec_set_csv_map(con, list(coordinates = c("Date", "Voucher", "Taxon"))), "too many columns")
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM meta_csv_map")$n, 0L)
})

test_that("set_metadata_columns writes overrides and reports the result", {
  d <- withr::local_tempdir()
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(d, ".sqlite"))
  DBI::dbWriteTable(con, "samples", data.frame(ID = "s1", Taxon = "x", Where = "Peru", country = "Chile"))
  DBI::dbDisconnect(con)
  res <- set_metadata_columns(d, country = "Where")
  expect_equal(res$column[res$concept == "country"], "Where")
  res <- set_metadata_columns(d, country = NA)
  expect_equal(res$column[res$concept == "country"], "country")
  res <- set_metadata_columns(d)
  expect_equal(res$column[res$concept == "taxon"], "Taxon")
})
```

Note on s2: CSV `American Samoa` = AS, GEOME `Samoa` = WS, GBIF `AS` -> conflict (CSV vs GEOME). The GBIF catalog number is an ARK, so `gbif_specimen_voucher` is empty and voucher has no values at all (Review Focus 3).

- [ ] **Step 2: Run, verify fail**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-specimen-conflicts.R")'`
Expected: FAIL, `could not find function ".spec_detect_csv"`.

- [ ] **Step 3: Append the engine to `R/specimen_reconcile.R`**

```r
SPECIMEN_CONCEPTS <- c("coordinates", "collection_date", "country", "locality", "voucher",
                       "collector", "sex", "dev_stage", "taxon")

.SPEC_CSV_NAMES <- list(
  coordinates = "lat_lon",
  collection_date = c("collection_date", "date", "eventDate"),
  country = c("country", "geo_loc_name"),
  locality = "locality",
  voucher = c("specimen_voucher", "voucher", "catalogNumber"),
  collector = c("collected_by", "collector", "recordedBy"),
  sex = "sex",
  dev_stage = c("dev_stage", "life_stage", "lifeStage"),
  taxon = "Taxon"
)
.SPEC_LAT <- c("lat", "latitude", "decimalLatitude")
.SPEC_LON <- c("lon", "long", "longitude", "decimalLongitude")

.spec_detect_csv <- function(cols, overrides = character()) {
  pick <- function(cands) {
    hit <- cols[match(tolower(cands), tolower(cols))]
    hit <- hit[!is.na(hit)]
    if (length(hit)) hit[1] else character()
  }
  out <- lapply(.SPEC_CSV_NAMES, pick)
  if (!length(out$coordinates)) {
    la <- pick(.SPEC_LAT)
    lo <- pick(.SPEC_LON)
    if (length(la) && length(lo)) out$coordinates <- c(la, lo)
  }
  out$taxon <- if ("Taxon" %in% cols) "Taxon" else character()
  for (k in intersect(names(overrides), setdiff(names(out), "taxon"))) {
    v <- overrides[[k]]
    out[[k]] <- if (is.na(v) || !nzchar(v)) character() else strsplit(v, ",", fixed = TRUE)[[1]]
  }
  out
}

specimen_csv_columns <- function(con) {
  .meta_ensure_tables(con)
  m <- DBI::dbGetQuery(con, "SELECT concept, column FROM meta_csv_map")
  .spec_detect_csv(DBI::dbListFields(con, "samples"), stats::setNames(m$column, m$concept))
}

.spec_csv_value <- function(row, cols) {
  if (!length(cols) || !all(cols %in% names(row))) return(NA_character_)
  v <- trimws(vapply(cols, function(cc) .meta_chr(row[[cc]])[1], character(1)))
  if (any(is.na(v) | !nzchar(v))) return(NA_character_)
  paste(v, collapse = ", ")
}

.spec_source_value <- function(concept, source, recs) {
  if (!nrow(recs)) return(NA_character_)
  combos <- .meta_combos(source)
  pick <- if (source == "GBIF") .gbif_occ else .geome_pick
  switch(concept,
    coordinates = combos$lat_lon$fn(recs),
    collection_date = combos$collection_date$fn(recs),
    country = pick(recs, if (source == "GBIF") "countryCode" else "country"),
    locality = pick(recs, "locality"),
    voucher = combos$specimen_voucher$fn(recs),
    collector = combos$collected_by$fn(recs),
    sex = combos$sex$fn(recs),
    dev_stage = combos$dev_stage$fn(recs),
    taxon = pick(recs, "scientificName")
  )
}

specimen_conflicts <- function(con, ids = NULL) {
  .meta_ensure_tables(con)
  s <- DBI::dbReadTable(con, "samples")
  if (!is.null(ids)) s <- s[s$ID %in% ids, , drop = FALSE]
  cols <- specimen_csv_columns(con)
  recs <- DBI::dbGetQuery(con, "SELECT ID, source, level, depth, field, value FROM meta_records")
  by_id <- split(recs, recs$ID)
  empty <- recs[0, ]
  nk <- length(SPECIMEN_CONCEPTS)
  n <- nrow(s) * nk
  csv_v <- geome_v <- gbif_v <- status <- rep(NA_character_, n)
  j <- 0L
  for (i in seq_len(nrow(s))) {
    row <- s[i, , drop = FALSE]
    r <- by_id[[s$ID[i]]] %||% empty
    g <- r[r$source == "GEOME", , drop = FALSE]
    b <- r[r$source == "GBIF", , drop = FALSE]
    for (k in SPECIMEN_CONCEPTS) {
      j <- j + 1L
      v <- c(.spec_csv_value(row, cols[[k]]), .spec_source_value(k, "GEOME", g),
             .spec_source_value(k, "GBIF", b))
      csv_v[j] <- v[1]
      geome_v[j] <- v[2]
      gbif_v[j] <- v[3]
      status[j] <- .spec_status(k, v)
    }
  }
  csv_col <- vapply(SPECIMEN_CONCEPTS, function(k) {
    if (length(cols[[k]])) paste(cols[[k]], collapse = " + ") else NA_character_
  }, character(1), USE.NAMES = FALSE)
  data.frame(ID = rep(s$ID, each = nk), concept = rep(SPECIMEN_CONCEPTS, nrow(s)),
             csv_column = rep(csv_col, nrow(s)), csv_value = csv_v, geome_value = geome_v,
             gbif_value = gbif_v, status = status)
}

.spec_set_csv_map <- function(con, map) {
  if (!length(map)) return(invisible(NULL))
  .meta_ensure_tables(con)
  ok <- setdiff(SPECIMEN_CONCEPTS, "taxon")
  nm <- names(map) %||% rep("", length(map))
  bad <- nm[!nm %in% ok]
  if (length(bad)) {
    stop("unknown concept(s): ", .lst(ifelse(nzchar(bad), bad, "<unnamed>")),
         "; use ", .lst(ok), call. = FALSE)
  }
  cols <- DBI::dbListFields(con, "samples")
  for (k in nm) {
    v <- map[[k]]
    if ((length(v) == 1L && is.na(v)) || (length(v) == 1L && !nzchar(v))) next
    if (length(v) > (if (k == "coordinates") 2L else 1L)) stop(k, ": too many columns", call. = FALSE)
    miss <- setdiff(v, cols)
    if (length(miss)) stop(k, ": column(s) not in the samples table: ", .lst(miss), call. = FALSE)
  }
  for (k in nm) {
    v <- map[[k]]
    if (length(v) == 1L && is.na(v)) {
      DBI::dbExecute(con, "DELETE FROM meta_csv_map WHERE concept = ?", params = list(k))
    } else {
      DBI::dbExecute(con, "INSERT OR REPLACE INTO meta_csv_map VALUES (?, ?)",
                     params = list(k, paste(v, collapse = ",")))
    }
  }
  invisible(NULL)
}

#' Choose which mapping-file columns are compared with GEOME and GBIF
#'
#' MitoPilot compares specimen details (coordinates, collection date, country,
#' locality, voucher, collector, sex, and life stage) between your mapping file,
#' GEOME, and GBIF, and flags disagreements. It finds the mapping-file columns
#' by name; use this function when a column has a name it does not recognize,
#' or to stop comparing one. The Taxon column is always compared.
#'
#' @param path Path to the project directory (default = current working directory)
#' @param ... Named `concept = column` pairs. Concepts: `coordinates`,
#'   `collection_date`, `country`, `locality`, `voucher`, `collector`, `sex`,
#'   `dev_stage`. `coordinates` takes one combined column or two columns
#'   (latitude, then longitude). `NA` returns a concept to automatic detection;
#'   `""` stops comparing it.
#' @return Invisibly, a data frame of each concept and the column(s) now used
#'   (`NA` when none).
#' @export
set_metadata_columns <- function(path = ".", ...) {
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = file.path(path, ".sqlite"))
  on.exit(DBI::dbDisconnect(con))
  .spec_set_csv_map(con, list(...))
  cols <- specimen_csv_columns(con)
  invisible(data.frame(
    concept = names(cols),
    column = vapply(cols, function(x) if (length(x)) paste(x, collapse = " + ") else NA_character_,
                    character(1), USE.NAMES = FALSE)
  ))
}
```

- [ ] **Step 4: Run tests, verify pass**

```bash
Rscript -e 'devtools::load_all(); for (f in c("test-specimen-conflicts.R","test-specimen-rules.R")) testthat::test_file(file.path("tests/testthat", f))'
```

Expected: all PASS.

- [ ] **Step 5: pkgdown, document, commit**

`_pkgdown.yml` reference: add `  - set_metadata_columns` right after `  - fetch_gbif`.

```bash
Rscript -e 'devtools::document()'
git add R/specimen_reconcile.R tests/testthat/test-specimen-conflicts.R NAMESPACE man/set_metadata_columns.Rd _pkgdown.yml
git commit -m "specimen_conflicts() engine, CSV column detection, set_metadata_columns()"
```

---

### Task 7: Specimen status column in every samples table

Replaces the GEOME icon column with one Specimen column (worst state wins) and renames the Columns-picker group to `Specimen`.

**Files:**
- Rename: `R/app_geome.R` -> `R/app_specimen.R` (`git mv`)
- Rename: `tests/testthat/test-geome-app.R` -> `tests/testthat/test-specimen-app.R` (`git mv`)
- Modify: `R/app_specimen.R` (status, cell renderer, colDef, default groups)
- Modify: `R/app_assemble_utils.R` (~L15-17 `taxa`; ~L71-74 collect; ~L132 relocate)
- Modify: `R/app_assemble_utils_userAsmb.R` (~L43-45; ~L67-69; ~L129)
- Modify: `R/app_annotate_utils.R` (~L20-22; ~L62-65; ~L78; ~L162)
- Modify: `R/app_export_utils.R` (`fetch_export_data()` ~L273-340; `export_metadata_cols()` owned)
- Modify: `R/export.R` (~L1310 summary `drop`)
- Modify: `R/app_assemble.R` (~L10, ~L75, ~L135-137, ~L272), `R/app_assemble_userAsmb.R` (~L13, ~L66, ~L119-121, ~L251), `R/app_annotate.R` (~L12, ~L90, ~L184-186, ~L362), `R/app_export.R` (~L10-11, ~L116, ~L158-160, ~L244, ~L342, ~L434 `.export_cols_drop`)
- Modify: `tests/testthat/test-export-summary-csv.R` (last test)
- Test: `tests/testthat/test-specimen-app.R`

**Interfaces:**
- Consumes: `specimen_conflicts(con, ids = NULL)` (Task 6); `META_SOURCES`, `.meta_ensure_tables` (Task 1).
- Produces:
  - `specimen_status(con)`: data.frame `ID, specimen ("ok"|"failed"|"conflict"|"none"), specimen_message` (multi-line tooltip text: one line per source with a ref or status, then `Conflicts: ...`, `Notes: ...`).
  - `.specimen_status_join(df, con)`: left-joins `specimen_status()` onto a collected data frame by `ID`; missing -> `"none"`.
  - `rt_specimen(inputId)`: reactable JS cell; click sends the row ID to `inputId`.
  - `specimen_col_def(inputId, sticky = NULL, class = NULL)`.
  - `.specimen_project_has_ids(con)`, `.specimen_default_groups(groups, con)` (drops `"Specimen"` when no sample has a GEOME BCID or GBIF ID).
  - Each panel's column is `specimen`; each module input is `specimen_open`; group name is `Specimen`, CSS class `mp-grp-Specimen`.

- [ ] **Step 1: Move the files**

```bash
cd ~/Documents/GitHub/MitoPilot-geome
git mv R/app_geome.R R/app_specimen.R
git mv tests/testthat/test-geome-app.R tests/testthat/test-specimen-app.R
```

- [ ] **Step 2: Write failing tests**

In `tests/testthat/test-specimen-app.R` delete these tests (their subjects are removed in Step 4): `".geome_status_join labels ok, failed, and none"`, `".geome_status_join ensures GEOME tables on a pre-branch DB with no geome tables"`, `"rt_geome sends the row ID to the given input"`, `".geome_project_has_bcids and .geome_default_groups follow the samples table"`, `"geome_col_def applies the group class to cell and header"`, `"every table's column groups include GEOME holding the geome column"`, and `"panel module servers start outside a reactive context"`. In `"the Export column picker offers a GEOME group, ..."` change `expect_true("GEOME" %in% names(EXPORT_COL_GROUPS))` to `expect_true("Specimen" %in% names(EXPORT_COL_GROUPS))` and the last line to

```r
    expect_setequal(id[grepl("mp-grp-Specimen", cls)], c("specimen", "geome_lat_lon", "geome_Event_country"))
```

Append:

```r
status_db <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(con, "samples", data.frame(
    ID = c("a", "b", "c", "d"), Taxon = "x",
    GEOME_BCID = c("ark:/1/A", NA, "ark:/1/C", NA), GBIF_ID = c(NA, "2", "3", NA)))
  .meta_ensure_tables(con)
  DBI::dbAppendTable(con, "meta_status", data.frame(
    ID = c("a", "b", "c", "c"), source = c("GEOME", "GBIF", "GEOME", "GBIF"),
    ref = c("ark:/1/A", "2", "ark:/1/C", "3"), status = c("ok", "failed", "ok", "ok"),
    message = c(NA, "GBIF returned HTTP 503", NA, NA), fetched_at = 1L))
  DBI::dbAppendTable(con, "meta_records", data.frame(
    ID = "c", source = c("GEOME", "GBIF", "GEOME", "GBIF"),
    level = c("Event", "Occurrence", "Event", "Occurrence"), depth = 0L, ref = "r",
    field = c("country", "countryCode", "collectorList", "recordedBy"),
    value = c("Peru", "CL", "A. B", "Ann B")))
  con
}

test_that("specimen_status: failed beats conflict beats ok beats none", {
  con <- status_db()
  on.exit(DBI::dbDisconnect(con))
  s <- specimen_status(con)
  s <- s[order(s$ID), ]
  expect_equal(s$specimen, c("ok", "failed", "conflict", "none"))
  expect_equal(s$specimen_message[1], "GEOME: fetched")
  expect_equal(s$specimen_message[2], "GBIF: failed (GBIF returned HTTP 503)")
  expect_equal(s$specimen_message[3],
               "GEOME: fetched\nGBIF: fetched\nConflicts: country\nNotes: collector")
  expect_equal(s$specimen_message[4], "No GEOME BCID or GBIF ID")
})

test_that("a set but never fetched ID says so", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbWriteTable(con, "samples", data.frame(ID = "a", Taxon = "x", GBIF_ID = "5"))
  s <- specimen_status(con)
  expect_equal(s$specimen, "none")
  expect_equal(s$specimen_message, "GBIF: not fetched yet")
})

test_that(".specimen_status_join adds specimen columns to a collected table", {
  con <- status_db()
  on.exit(DBI::dbDisconnect(con))
  out <- .specimen_status_join(data.frame(ID = c("c", "a", "zz")), con)
  expect_equal(out$specimen, c("conflict", "ok", "none"))
  expect_true(is.na(out$specimen_message[3]))
})

test_that("rt_specimen sends the row ID and knows all four states", {
  js <- as.character(rt_specimen("assemble-specimen_open"))
  expect_match(js, "assemble-specimen_open", fixed = TRUE)
  expect_match(js, "setInputValue", fixed = TRUE)
  expect_match(js, "dataset.id", fixed = TRUE)
  expect_match(js, "specimen_message", fixed = TRUE)
  for (cls in c("fa-earth-americas", "fa-triangle-exclamation", "fa-flag", "fa-square-plus")) {
    expect_match(js, cls, fixed = TRUE)
  }
})

test_that("specimen_col_def applies the group class to cell and header", {
  cd <- specimen_col_def("x-specimen_open", class = "mp-grp-Specimen")
  expect_equal(cd$class, "mp-grp-Specimen")
  expect_equal(cd$headerClass, "mp-grp-Specimen")
  expect_equal(cd$name, "Specimen")
})

test_that(".specimen_default_groups drops Specimen only when no sample has any ID", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbWriteTable(con, "samples", data.frame(ID = c("a", "b"), Taxon = "x"))
  grp <- c("Options", "Specimen", "Metadata")
  expect_false(.specimen_project_has_ids(con))
  expect_equal(.specimen_default_groups(grp, con), c("Options", "Metadata"))
  DBI::dbExecute(con, "UPDATE samples SET GEOME_BCID = '' WHERE ID = 'a'")
  expect_false(.specimen_project_has_ids(con))
  DBI::dbExecute(con, "UPDATE samples SET GBIF_ID = '6186461308' WHERE ID = 'b'")
  expect_true(.specimen_project_has_ids(con))
  expect_equal(.specimen_default_groups(grp, con), grp)
})

test_that("every table's column groups include Specimen holding the specimen column", {
  for (g in list(ASSEMBLE_COL_GROUPS, ASSEMBLE_COL_GROUPS_USERASMB, ANNOTATE_COL_GROUPS, EXPORT_COL_GROUPS)) {
    expect_true("specimen" %in% g$Specimen)
    expect_false("GEOME" %in% names(g))
  }
})

test_that("export_metadata_cols treats the specimen status columns as owned", {
  expect_equal(export_metadata_cols(c("ID", "specimen", "specimen_message", "site"), character()), "site")
})

test_that("panel module servers start outside a reactive context, with specimen data", {
  start <- function(maker, servers) {
    proj <- withr::local_tempdir()
    suppressMessages(maker(path = proj, executor = "local", Rproj = FALSE))
    con <- DBI::dbConnect(RSQLite::SQLite(), file.path(proj, ".sqlite"))
    withr::defer(DBI::dbDisconnect(con))
    withr::local_options(MitoPilot.db = file.path(proj, ".sqlite"))
    .meta_ensure_tables(con)
    id1 <- DBI::dbGetQuery(con, "SELECT ID FROM samples LIMIT 1")$ID
    DBI::dbExecute(con, "UPDATE samples SET GEOME_BCID = 'ark:/1/A', GBIF_ID = '1' WHERE ID = ?",
                   params = list(id1))
    DBI::dbAppendTable(con, "meta_status", data.frame(
      ID = id1, source = c("GEOME", "GBIF"), ref = c("ark:/1/A", "1"), status = "ok",
      message = NA_character_, fetched_at = 1L))
    DBI::dbAppendTable(con, "meta_records", data.frame(
      ID = id1, source = c("GEOME", "GBIF"), level = c("Event", "Occurrence"), depth = 0L,
      ref = c("ark:/1/A", "1"), field = c("country", "countryCode"), value = c("Peru", "CL")))
    for (srv in servers) {
      ms <- shiny::MockShinySession$new()
      ms$userData$con <- con
      ms$userData$mode <- "annotate"
      for (f in c("goto_annotate", "reopen_outlier_review", "run_modal")) gargoyle::init(f, session = ms)
      # not testServer: it runs the module inside isolate(), hiding reads that crash the real app
      expect_no_error(shiny::withReactiveDomain(ms, get(srv)("m")), message = srv)
    }
  }
  start(function(...) new_test_project(n = 2, ...), c("assemble_server", "annotate_server", "export_server"))
  start(new_test_project_userAsmb, "assemble_server_userAsmb")
})
```

In `tests/testthat/test-export-summary-csv.R`, replace the last test with:

```r
test_that("the summary CSV leaves out the app-only specimen status columns", {
  res <- run_summary_export(annotate_lock = 1L, assemble_lock = 1L)
  expect_false(any(c("specimen", "specimen_message", "geome", "geome_message") %in% names(res$summary)))
})
```

- [ ] **Step 3: Run, verify fail**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-specimen-app.R")'`
Expected: FAIL, `could not find function "specimen_status"`.

- [ ] **Step 4: Status helpers in `R/app_specimen.R`**

Delete `.geome_status_join`, `rt_geome`, `geome_col_def`, `.geome_project_has_bcids`, and `.geome_default_groups` (with their roxygen blocks). Add at the top of the file:

```r
#' Specimen metadata state per sample: worst of fetch status and conflicts
#'
#' @param con database connection
#' @return data.frame `ID`, `specimen` ("ok" | "failed" | "conflict" | "none"),
#'   `specimen_message` (tooltip text, one line per source then conflicts and notes)
#' @noRd
specimen_status <- function(con) {
  .meta_ensure_tables(con)
  cols <- vapply(META_SOURCES, function(s) s$col, character(1))
  s <- DBI::dbGetQuery(con, paste0("SELECT ID, ", paste(cols, collapse = ", "), " FROM samples"))
  st <- DBI::dbGetQuery(con, "SELECT ID, source, status, message FROM meta_status")
  cf <- specimen_conflicts(con)
  state <- msg <- character(nrow(s))
  for (i in seq_len(nrow(s))) {
    id <- s$ID[i]
    lines <- states <- character()
    for (src in names(META_SOURCES)) {
      ref <- s[[META_SOURCES[[src]]$col]][i]
      r <- st[st$ID == id & st$source == src, , drop = FALSE]
      if (nrow(r)) {
        states <- c(states, r$status[1])
        lines <- c(lines, if (r$status[1] == "ok") paste0(src, ": fetched") else
          paste0(src, ": failed (", r$message[1] %|NA|% "unknown error", ")"))
      } else if (!is.na(ref) && nzchar(ref)) {
        lines <- c(lines, paste0(src, ": not fetched yet"))
      }
    }
    k <- cf[cf$ID == id, , drop = FALSE]
    conf <- k$concept[k$status %in% "conflict"]
    note <- k$concept[k$status %in% "note"]
    if (length(conf)) lines <- c(lines, paste("Conflicts:", paste(conf, collapse = ", ")))
    if (length(note)) lines <- c(lines, paste("Notes:", paste(note, collapse = ", ")))
    state[i] <- if ("failed" %in% states) "failed" else if (length(conf)) "conflict" else
      if ("ok" %in% states) "ok" else "none"
    msg[i] <- if (length(lines)) paste(lines, collapse = "\n") else "No GEOME BCID or GBIF ID"
  }
  data.frame(ID = s$ID, specimen = state, specimen_message = msg)
}

#' Left-join specimen status onto a collected, ID-keyed data frame
#'
#' @param df data frame with an `ID` column
#' @param con database connection
#' @noRd
.specimen_status_join <- function(df, con) {
  out <- dplyr::left_join(df, specimen_status(con), by = "ID")
  out$specimen[is.na(out$specimen)] <- "none"
  out
}

#' reactable cell renderer for the Specimen column
#'
#' @param inputId namespaced Shiny input id to receive the clicked row's ID
#' @noRd
rt_specimen <- function(inputId) {
  sprintf(
    "function(cellInfo) {
      var st = cellInfo.value || 'none';
      var row = cellInfo.row || {};
      var esc = function(s) { return String(s).replace(/&/g, '&amp;').replace(/'/g, '&#39;')
        .replace(/\"/g, '&quot;').replace(/</g, '&lt;').replace(/>/g, '&gt;'); };
      var cls = {
        ok: 'fa-solid fa-earth-americas',
        failed: 'fa-solid fa-triangle-exclamation mp-fg-warning',
        conflict: 'fa-solid fa-flag mp-fg-warning',
        none: 'fa-regular fa-square-plus text-muted'
      }[st] || 'fa-regular fa-square-plus text-muted';
      var tip = (row['specimen_message'] || 'No GEOME BCID or GBIF ID') +
        (st === 'none' ? '. Click to add one.' : '\\nClick to view.');
      return `<a href='#' class='mp-specimen-cell' data-id='${esc(row['ID'])}' title='${esc(tip)}' aria-label='${esc(tip)}' ` +
        `onclick=\"event.preventDefault(); event.stopPropagation(); Shiny.setInputValue('%s', this.dataset.id, {priority: 'event'})\">` +
        `<i class='${cls}' aria-hidden='true'></i></a>`;
    }",
    inputId
  ) |>
    htmlwidgets::JS()
}

#' Shared colDef for the Specimen column
#'
#' @param inputId namespaced Shiny input id to receive the clicked row's ID
#' @noRd
specimen_col_def <- function(inputId, sticky = NULL, class = NULL) {
  reactable::colDef(
    show = TRUE, name = "Specimen", sticky = sticky, width = 80, align = "center",
    html = TRUE, filterable = FALSE, sortable = TRUE,
    class = class, headerClass = class,
    header = rt_header("Specimen", paste(
      "GEOME and GBIF metadata for this sample. Click an icon to view, add,",
      "compare, or refresh.")),
    cell = rt_specimen(inputId)
  )
}

#' TRUE when any sample has a non-blank GEOME BCID or GBIF ID
#'
#' @param con database connection
#' @noRd
.specimen_project_has_ids <- function(con) {
  .meta_ensure_tables(con)
  DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM samples
                        WHERE (GEOME_BCID IS NOT NULL AND TRIM(GEOME_BCID) != '')
                           OR (GBIF_ID IS NOT NULL AND TRIM(GBIF_ID) != '')")$n > 0
}

#' Drop the Specimen group from a default column-group selection when no
#' sample has a GEOME BCID or GBIF ID
#'
#' @param groups character vector of group names
#' @param con database connection
#' @noRd
.specimen_default_groups <- function(groups, con) {
  if (.specimen_project_has_ids(con)) groups else setdiff(groups, "Specimen")
}
```

- [ ] **Step 5: Data fetches**

`R/app_assemble_utils.R` (`fetch_assemble_data()`):
- `taxa <- dplyr::tbl(db, "samples") |> dplyr::select(ID, Taxon) |> .geome_status_join(db)` -> `taxa <- dplyr::tbl(db, "samples") |> dplyr::select(ID, Taxon)`
- in the `out <- ...` chain, `dplyr::collect() |>` (right after `dplyr::left_join(assemble_opts_tbl, by = "assemble_opts") |>`) -> `dplyr::collect() |>` followed by a new line `.specimen_status_join(db) |>`
- the final `dplyr::relocate(dplyr::any_of(c("geome", "geome_message")), .after = Taxon)` -> `dplyr::relocate(dplyr::any_of(c("specimen", "specimen_message")), .after = Taxon)`

`R/app_assemble_utils_userAsmb.R`: same three edits (`taxa` keeps `dplyr::select(ID, Taxon, topology, assembly)`; the `collect()` is the one right after `dplyr::left_join(taxa, by = "ID") |>`).

`R/app_annotate_utils.R` (`fetch_annotate_units()`):
- drop `|> .geome_status_join(db)` from `taxa`
- after the `dplyr::collect() |>` that follows `dplyr::left_join(export_state, by = c("ID", "path", "scaffold")) |>`, add `.specimen_status_join(db) |>`
- both `"geome", "geome_message"` lists (~L78 and ~L162) -> `"specimen", "specimen_message"`

`R/app_export_utils.R` (`fetch_export_data()`):
- `samples <- dplyr::tbl(db, "samples") |> dplyr::select(-dplyr::any_of("topology")) |> .geome_status_join(db)` -> drop the last pipe step
- replace

  ```r
      dplyr::relocate(Taxon, .after = ID) |>
      dplyr::relocate(geome, .after = Taxon) |>
      dplyr::collect() |>
  ```

  with

  ```r
      dplyr::relocate(Taxon, .after = ID) |>
      dplyr::collect() |>
      .specimen_status_join(db) |>
      dplyr::relocate(specimen, .after = Taxon) |>
  ```
- `blast_lineage, .after = geome) |>` -> `blast_lineage, .after = specimen) |>`
- `export_metadata_cols()` owned: replace `"geome", "geome_message"` with `"specimen", "specimen_message"`

`R/export.R` summary CSV: `drop <- c("poor_blast_ref", "blast_ref_status", "curate_opts", "annotate_switch", "geome", "geome_message")` -> replace the last two with `"specimen", "specimen_message"`.

- [ ] **Step 6: Table modules**

In each of `R/app_assemble.R`, `R/app_assemble_userAsmb.R`, `R/app_annotate.R`:
- `GEOME    = c("geome")` -> `Specimen = c("specimen")`
- `geome_viewer_server("geome", open = reactive(input$geome_open),` -> `geome_viewer_server("geome", open = reactive(input$specimen_open),` (Task 8 renames the module)
- `col_groups_rv <- reactiveVal(.geome_default_groups(names(<GROUPS>), session$userData$con))` -> `.specimen_default_groups(...)`; the next line `if (!"GEOME" %in% isolate(col_groups_rv())) {` -> `if (!"Specimen" %in% isolate(col_groups_rv())) {` (keep `isolate()`)
- `geome = geome_col_def(ns("geome_open"), <args>, class = "mp-grp-GEOME"),` -> `specimen = specimen_col_def(ns("specimen_open"), <same args>, class = "mp-grp-Specimen"),`

In `R/app_export.R`:
- `EXPORT_COL_GROUPS`: replace the comment and `GEOME = c("geome")` with

  ```r
    # specimen + the fields ticked in meta_export_fields, filled at render time
    Specimen = c("specimen")
  ```
- the viewer, `col_groups_rv`, and `geome =` colDef lines as above
- in `geome_col_defs()`: `class = "mp-grp-GEOME", headerClass = "mp-grp-GEOME",` -> `class = "mp-grp-Specimen", headerClass = "mp-grp-Specimen",`
- `.export_cols_drop <- c("poor_blast_ref", "blast_accession_auto", "annotate_switch")` -> add `"specimen", "specimen_message"`

Then:

```bash
grep -rn "geome_open\|mp-grp-GEOME\|\"GEOME\" %in%\|geome_message\|\bgeome = \|geome_col_def\|\.geome_default_groups\|\.geome_status_join\|rt_geome" R/
```

Expected: no output. Confirm the Annotate table hides `specimen_message` (its `defaultColDef` is `colDef(show = FALSE)`, as it was for `geome_message`); if not, add `specimen_message = colDef(show = FALSE)`.

- [ ] **Step 7: Run tests, verify pass**

```bash
Rscript -e 'devtools::load_all(); for (f in c("test-specimen-app.R","test-export-summary-csv.R","test-export-metadata-cols.R","test-userasmb-app-units.R","test-ui-reactable-helpers.R","test-no-duplicate-definitions.R")) testthat::test_file(file.path("tests/testthat", f))'
```

Expected: all PASS, including the MockShinySession startup test for all four panels.

- [ ] **Step 8: Commit**

```bash
git add -A R/ tests/testthat/
git commit -m "Specimen status column replaces the GEOME column"
```

---

### Task 8: Specimen metadata viewer (GEOME, GBIF, and Compare tabs)

**Files:**
- Modify: `R/app_specimen.R` (replace `geome_record_view` and `geome_viewer_server`)
- Modify: `R/app_assemble.R`, `R/app_assemble_userAsmb.R`, `R/app_annotate.R`, `R/app_export.R` (the one viewer call each)
- Modify: `inst/app/www/custom.css` (append two rules)
- Test: `tests/testthat/test-specimen-app.R` (replace the two `geome_record_view` tests, append)

**Interfaces:**
- Consumes: `META_SOURCES`, `.meta_set_ref`, `.meta_fetch_into` (Tasks 1, 3); `specimen_status` (Task 7); `specimen_conflicts`, `specimen_csv_columns`, `.spec_set_csv_map`, `SPECIMEN_CONCEPTS` (Task 6); `export_metadata_cols` (existing).
- Produces:
  - `meta_record_view(recs, source, box_id)`: recs columns `level, depth, ref, field, value`; GEOME cards root first (highest depth first), GBIF cards Occurrence first; links to geome-db.org or gbif.org; `issues` rendered as `mp-pill mp-pill-warning` badges; a Dataset `citation` shown above its table; Expand/Collapse all; scroll box `id = box_id`.
  - `specimen_compare_view(cf)`: table `Item | CSV (column) | GEOME | GBIF | Status`; conflict rows class `mp-spec-conflict`, note rows `text-muted`.
  - `specimen_csv_map_ui(ns, current, overrides, choices)`: `<details>` "CSV columns..." with one `selectizeInput(ns("map_<concept>"), multiple = TRUE)` per concept except taxon (`maxItems` 2 for coordinates, else 1; `"__none__"` = (none); empty = automatic) and a `Save columns` button `ns("map_save")`.
  - `specimen_viewer_server(id, open, on_change)`: panels call `specimen_viewer_server("specimen", open = reactive(input$specimen_open), on_change = ...)`.

- [ ] **Step 1: Write failing tests**

In `tests/testthat/test-specimen-app.R` delete the tests `"geome_record_view orders levels root first and links BCIDs"` and `"geome_record_view puts cards in a scroll box with expand/collapse all"`. Append:

```r
test_that("meta_record_view orders GEOME root first and links BCIDs", {
  recs <- data.frame(
    level = c("Tissue", "Event", "Project"), depth = c(0L, 2L, 3L),
    ref = c("ark:/1/T", "ark:/1/E", NA), field = c("tissueID", "country", "projectTitle"),
    value = c("T1", "Peru", "My proj"))
  html <- as.character(meta_record_view(recs, "GEOME", "m-geome_records"))
  expect_lt(regexpr("Project", html), regexpr("Event", html))
  expect_lt(regexpr("Event", html), regexpr("Tissue", html))
  expect_match(html, "https://geome-db.org/record/ark:/1/E", fixed = TRUE)
  expect_match(html, "id=\"m-geome_records\"[^>]*overflow-y: auto")
  expect_match(html, "#m-geome_records details", fixed = TRUE)
  expect_match(html, "Expand all", fixed = TRUE)
  expect_false(grepl("Expand all", as.character(meta_record_view(recs[0, ], "GEOME", "x"))))
})

test_that("meta_record_view shows GBIF occurrence first, issues as badges, and the citation", {
  recs <- data.frame(
    level = c("Occurrence", "Occurrence", "Dataset", "Dataset", "Organization"),
    depth = c(0L, 0L, 1L, 1L, 2L), ref = c("61", "61", "ds1", "ds1", "org1"),
    field = c("issues", "country", "title", "citation", "title"),
    value = c("ZERO_COORDINATE,COORDINATE_ROUNDED", "Peru", "UF Fish",
              "Robins R (2026). UF Fish.", "Florida Museum"))
  html <- as.character(meta_record_view(recs, "GBIF", "b2"))
  expect_lt(regexpr(">Occurrence<", html), regexpr(">Dataset<", html))
  expect_lt(regexpr(">Dataset<", html), regexpr(">Organization<", html))
  expect_match(html, "https://www.gbif.org/occurrence/61", fixed = TRUE)
  expect_match(html, "https://www.gbif.org/dataset/ds1", fixed = TRUE)
  expect_match(html, "https://www.gbif.org/publisher/org1", fixed = TRUE)
  expect_match(html, "mp-pill mp-pill-warning\">ZERO_COORDINATE<", fixed = TRUE)
  expect_match(html, "mp-pill mp-pill-warning\">COORDINATE_ROUNDED<", fixed = TRUE)
  expect_match(html, "class=\"mp-meta-citation\"", fixed = TRUE)
})

test_that("specimen_compare_view marks conflicts and notes and names the CSV column", {
  cf <- data.frame(ID = "s1", concept = c("country", "collector", "sex", "voucher"),
                   csv_column = c("geo_loc_name", NA, NA, NA),
                   csv_value = c("USA: Florida", NA, NA, NA),
                   geome_value = c("Canada", "A. B", NA, NA),
                   gbif_value = c(NA, "Ann B", "male", NA),
                   status = c("conflict", "note", "single", NA))
  html <- as.character(specimen_compare_view(cf))
  expect_match(html, "<tr class=\"mp-spec-conflict\">", fixed = TRUE)
  expect_match(html, "<tr class=\"text-muted\">", fixed = TRUE)
  expect_match(html, "(geo_loc_name)", fixed = TRUE)
  expect_match(html, "Canada", fixed = TRUE)
  expect_match(html, "CSV (column)", fixed = TRUE)
})

test_that("specimen_csv_map_ui offers every concept but taxon, with auto and none", {
  current <- list(coordinates = c("Latitude", "Longitude"), collection_date = character(),
                  country = "Where", locality = character(), voucher = character(),
                  collector = character(), sex = character(), dev_stage = character(),
                  taxon = "Taxon")
  html <- as.character(specimen_csv_map_ui(NS("v"), current, c(country = "Where", sex = ""),
                                           c("Latitude", "Longitude", "Where")))
  expect_match(html, "CSV columns...", fixed = TRUE)
  expect_match(html, "v-map_coordinates", fixed = TRUE)
  expect_false(grepl("v-map_taxon", html, fixed = TRUE))
  expect_match(html, "auto: Latitude + Longitude", fixed = TRUE)
  expect_match(html, "__none__", fixed = TRUE)
  expect_match(html, "\"maxItems\":2", fixed = TRUE)
  expect_match(html, "v-map_save", fixed = TRUE)
})
```

- [ ] **Step 2: Run, verify fail**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-specimen-app.R")'`
Expected: FAIL, `could not find function "meta_record_view"`.

- [ ] **Step 3: Replace the viewer in `R/app_specimen.R`**

Delete `geome_record_view` and `geome_viewer_server` (and their roxygen). Add:

```r
#' Render one sample's records from one source as level cards
#'
#' GEOME levels run root first; GBIF runs Occurrence, Dataset, Organization.
#'
#' @param recs `meta_records` rows for one sample and source (level, depth, ref, field, value)
#' @param source "GEOME" or "GBIF"
#' @param box_id DOM id of the scroll box holding the cards
#' @noRd
meta_record_view <- function(recs, source, box_id) {
  if (!nrow(recs)) return(p(class = "text-muted", paste("No", source, "data stored for this sample yet.")))
  toggle <- function(label, open) {
    tags$button(
      type = "button", class = "btn btn-default btn-sm", label,
      onclick = sprintf("document.querySelectorAll('#%s details').forEach(function(d) { d.open = %s; });",
                        box_id, tolower(open))
    )
  }
  link <- function(level, ref) {
    if (is.na(ref)) return(NULL)
    if (source == "GEOME") return(paste0("https://geome-db.org/record/", ref))
    switch(level,
      Occurrence = paste0("https://www.gbif.org/occurrence/", ref),
      Dataset = paste0("https://www.gbif.org/dataset/", ref),
      Organization = paste0("https://www.gbif.org/publisher/", ref),
      NULL)
  }
  cell <- function(field, value) {
    if (field != "issues") return(value)
    lapply(strsplit(value, ",", fixed = TRUE)[[1]], function(x) {
      tagList(span(class = "mp-pill mp-pill-warning", x), " ")
    })
  }
  ord <- if (source == "GEOME") -recs$depth else recs$depth
  lv <- unique(recs[order(ord), c("level", "depth", "ref")])
  lv <- lv[!duplicated(lv$depth), ]
  cards <- lapply(seq_len(nrow(lv)), function(i) {
    r <- recs[recs$depth == lv$depth[i], ]
    url <- link(lv$level[i], lv$ref[i])
    cit <- r$value[r$field == "citation"]
    tags$details(
      open = NA, class = "mp-meta-level",
      tags$summary(
        strong(lv$level[i]),
        if (!is.null(url)) tagList(" ", tags$a(href = url, target = "_blank", rel = "noopener", lv$ref[i]))
      ),
      if (length(cit)) p(class = "mp-meta-citation", em(cit[1])),
      tags$table(class = "table table-sm",
        tags$tbody(lapply(seq_len(nrow(r)), function(j) {
          tags$tr(tags$th(r$field[j]), tags$td(cell(r$field[j], r$value[j])))
        }))
      )
    )
  })
  tagList(
    div(style = "margin-bottom: 6px;", toggle("Expand all", TRUE), " ", toggle("Collapse all", FALSE)),
    div(id = box_id, style = "max-height: 50vh; overflow-y: auto;", cards)
  )
}

#' Compare tab table: one row per concept across CSV, GEOME, and GBIF
#'
#' @param cf `specimen_conflicts()` rows for one sample
#' @noRd
specimen_compare_view <- function(cf) {
  if (!nrow(cf)) return(p(class = "text-muted", "No sample selected."))
  dash <- function(x) if (is.na(x)) "-" else x
  tags$table(
    class = "table table-sm mp-spec-compare",
    tags$thead(tags$tr(tags$th("Item"), tags$th("CSV (column)"), tags$th("GEOME"),
                       tags$th("GBIF"), tags$th("Status"))),
    tags$tbody(lapply(seq_len(nrow(cf)), function(i) {
      r <- cf[i, ]
      cls <- if (identical(r$status, "conflict")) "mp-spec-conflict" else
        if (identical(r$status, "note")) "text-muted" else NULL
      tags$tr(
        class = cls,
        tags$td(r$concept),
        tags$td(dash(r$csv_value),
                if (!is.na(r$csv_column)) span(class = "text-muted", paste0(" (", r$csv_column, ")"))),
        tags$td(dash(r$geome_value)),
        tags$td(dash(r$gbif_value)),
        tags$td(dash(r$status))
      )
    }))
  )
}

#' "CSV columns..." control: pick the mapping-file column per concept
#'
#' @param ns module namespace function
#' @param current `specimen_csv_columns()` result
#' @param overrides named character vector from `meta_csv_map` (concept -> column(s))
#' @param choices mapping-file column names
#' @noRd
specimen_csv_map_ui <- function(ns, current, overrides, choices) {
  concepts <- setdiff(SPECIMEN_CONCEPTS, "taxon")
  tags$details(
    class = "mp-spec-map",
    tags$summary("CSV columns..."),
    opts_help(
      "Pick the mapping-file column MitoPilot compares for each item. Leave a ",
      "box empty to detect the column automatically, or pick (none) to skip ",
      "that item. Coordinates take one combined column, or latitude then longitude.",
      nested = TRUE
    ),
    lapply(concepts, function(k) {
      set <- k %in% names(overrides)
      sel <- if (!set) character() else if (nzchar(overrides[[k]])) {
        strsplit(overrides[[k]], ",", fixed = TRUE)[[1]]
      } else {
        "__none__"
      }
      auto <- if (length(current[[k]])) paste(current[[k]], collapse = " + ") else "none"
      selectizeInput(
        ns(paste0("map_", k)),
        label = if (set) k else paste0(k, " (auto: ", auto, ")"),
        choices = c("(none)" = "__none__", choices), selected = sel,
        multiple = TRUE, width = "100%",
        options = list(maxItems = if (k == "coordinates") 2 else 1,
                       placeholder = "detect automatically")
      )
    }),
    actionButton(ns("map_save"), "Save columns")
  )
}

#' Specimen metadata viewer: GEOME, GBIF, and Compare tabs
#'
#' @param id module id
#' @param open reactive yielding the sample ID to open (from a `specimen_open` input)
#' @param on_change function called after any DB write, so the caller can refresh its table
#' @noRd
specimen_viewer_server <- function(id, open, on_change = function() NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    con <- session$userData$con
    rv <- reactiveValues(id = NULL, ver = 0L)
    bump <- function() { rv$ver <- rv$ver + 1L; on_change() }
    empty_msg <- list(
      GEOME = paste("This sample has no GEOME BCID. Paste one above and click Fetch, or add a",
                    "GEOME_BCID column to your mapping file (see the Specimen Metadata article)."),
      GBIF = paste("This sample has no GBIF ID. Paste a gbifID or a gbif.org/occurrence link above",
                   "and click Fetch, or add a GBIF_ID column to your mapping file.")
    )

    samples <- function() {
      DBI::dbGetQuery(con, "SELECT ID, Taxon, GEOME_BCID, GBIF_ID FROM samples ORDER BY ID")
    }

    observeEvent(open(), {
      rv$id <- open()
      s <- samples()
      st <- specimen_status(con)
      mark <- st$specimen[match(s$ID, st$ID)]
      lab <- paste0(s$ID, ifelse(mark %in% "failed", " (failed)",
                                 ifelse(mark %in% "conflict", " (conflict)", "")))
      modalDialog(
        title = mp_modal_title(
          tagList("Specimen metadata: ", textOutput(ns("hdr_id"), inline = TRUE)),
          subtitle = tagList("Taxon: ", textOutput(ns("hdr_taxon"), inline = TRUE))
        ),
        size = "l", easyClose = TRUE,
        fluidRow(
          column(3,
            selectInput(ns("sample"), "Sample", choices = stats::setNames(s$ID, lab),
                        selected = rv$id, width = "100%", selectize = FALSE, size = 15),
            uiOutput(ns("failed"))
          ),
          column(9,
            tabsetPanel(
              id = ns("tab"),
              tabPanel("GEOME", uiOutput(ns("geome_detail"))),
              tabPanel("GBIF", uiOutput(ns("gbif_detail"))),
              tabPanel("Compare", uiOutput(ns("compare")))
            )
          )
        ),
        footer = mp_footer(
          extra = actionButton(ns("refresh_all"), "Refresh all",
                               title = "Fetch every sample's GEOME and GBIF records again"),
          dismiss = "Close"
        )
      ) |> showModal()
    })

    observeEvent(input$sample, rv$id <- input$sample, ignoreInit = TRUE)

    output$hdr_id <- renderText(rv$id)
    output$hdr_taxon <- renderText({
      req(rv$id)
      s <- samples()
      s$Taxon[s$ID == rv$id] %|NA|% "NA"
    })

    output$failed <- renderUI({
      rv$ver
      st <- specimen_status(con)
      bad <- st$ID[st$specimen == "failed"]
      if (!length(bad)) return(NULL)
      div(class = "mp-fg-warning", icon("triangle-exclamation"), " Failed: ",
          paste(bad, collapse = ", "))
    })

    source_detail <- function(source) {
      rv$ver
      req(rv$id)
      src <- META_SOURCES[[source]]
      key <- tolower(source)
      ref <- DBI::dbGetQuery(con, paste0("SELECT ", src$col, " AS v FROM samples WHERE ID = ?"),
                             params = list(rv$id))$v
      ref <- if (length(ref)) ref[1] else NA_character_
      st <- DBI::dbGetQuery(con, "SELECT status, message, fetched_at FROM meta_status
                                  WHERE ID = ? AND source = ?", params = list(rv$id, source))
      recs <- DBI::dbGetQuery(con, "SELECT level, depth, ref, field, value FROM meta_records
                                    WHERE ID = ? AND source = ?", params = list(rv$id, source))
      tagList(
        div(class = "mp-meta-ref",
          textInput(ns(paste0(key, "_ref")), paste(source, src$id_label), value = ref %|NA|% "",
                    placeholder = if (source == "GEOME") "ark:/21547/..." else "6186461308",
                    width = "420px"),
          actionButton(ns(paste0(key, "_fetch")), "Fetch", icon = icon("arrows-rotate"))
        ),
        if (nrow(st)) p(class = if (st$status == "failed") "mp-fg-warning" else "text-muted",
          if (st$status == "failed") paste("Last fetch failed:", st$message) else "Fetched",
          " ", format(as.POSIXct(st$fetched_at, origin = "1970-01-01"), "%Y-%m-%d %H:%M")),
        if (is.na(ref) && !nrow(recs)) p(class = "text-muted", empty_msg[[source]]),
        meta_record_view(recs, source, box_id = ns(paste0(key, "_records")))
      )
    }
    output$geome_detail <- renderUI(source_detail("GEOME"))
    output$gbif_detail <- renderUI(source_detail("GBIF"))

    output$compare <- renderUI({
      rv$ver
      req(rv$id)
      m <- DBI::dbGetQuery(con, "SELECT concept, column FROM meta_csv_map")
      tagList(
        specimen_compare_view(specimen_conflicts(con, rv$id)),
        specimen_csv_map_ui(ns, specimen_csv_columns(con), stats::setNames(m$column, m$concept),
                            export_metadata_cols(DBI::dbListFields(con, "samples"), character()))
      )
    })

    fetch_one <- function(source) {
      req(rv$id)
      tryCatch({
        val <- .meta_set_ref(con, source, rv$id, input[[paste0(tolower(source), "_ref")]])
        if (!is.na(val)) {
          withProgress(message = paste("Fetching from", source), {
            res <- suppressWarnings(.meta_fetch_into(con, source, rv$id, val))
          })
          if (res$status == "failed") showNotification(res$message, type = "warning")
        }
        bump()
      }, error = function(e) showNotification(conditionMessage(e), type = "error"))
    }
    observeEvent(input$geome_fetch, fetch_one("GEOME"))
    observeEvent(input$gbif_fetch, fetch_one("GBIF"))

    observeEvent(input$map_save, {
      concepts <- setdiff(SPECIMEN_CONCEPTS, "taxon")
      map <- lapply(stats::setNames(nm = concepts), function(k) {
        v <- input[[paste0("map_", k)]]
        if (!length(v)) NA_character_ else if ("__none__" %in% v) "" else v
      })
      tryCatch({
        .spec_set_csv_map(con, map)
        bump()
        showNotification("CSV columns saved", type = "message")
      }, error = function(e) showNotification(conditionMessage(e), type = "error"))
    })

    observeEvent(input$refresh_all, {
      s <- samples()
      jobs <- do.call(rbind, lapply(names(META_SOURCES), function(src) {
        ref <- s[[META_SOURCES[[src]]$col]]
        keep <- !is.na(ref) & nzchar(ref)
        data.frame(ID = s$ID[keep], source = rep(src, sum(keep)), ref = ref[keep])
      }))
      if (!nrow(jobs)) {
        return(showNotification("No samples have a GEOME BCID or GBIF ID", type = "message"))
      }
      caches <- lapply(META_SOURCES, function(x) new.env())
      tryCatch({
        withProgress(message = "Fetching specimen records", value = 0, {
          for (i in seq_len(nrow(jobs))) {
            suppressWarnings(.meta_fetch_into(con, jobs$source[i], jobs$ID[i], jobs$ref[i],
                                              caches[[jobs$source[i]]]))
            incProgress(1 / nrow(jobs), detail = paste(jobs$ID[i], jobs$source[i]))
          }
        })
        bump()
      }, error = function(e) showNotification(conditionMessage(e), type = "error"))
    })
  })
}
```

- [ ] **Step 4: Panel calls and CSS**

In the four panel modules replace `geome_viewer_server("geome", open = reactive(input$specimen_open),` with `specimen_viewer_server("specimen", open = reactive(input$specimen_open),` (the `on_change = ...` argument stays).

Append to `inst/app/www/custom.css`:

```css
/* Specimen viewer Compare tab */
.mp-spec-compare tr.mp-spec-conflict > td { background: var(--mp-warning-soft); }
.mp-meta-citation { margin: 4px 0 6px; font-size: var(--mp-fs-meta); }
```

```bash
grep -rn "geome_viewer_server\|geome_record_view" R/ tests/
```

Expected: no output.

- [ ] **Step 5: Run tests, verify pass**

```bash
Rscript -e 'devtools::load_all(); for (f in c("test-specimen-app.R","test-no-duplicate-definitions.R")) testthat::test_file(file.path("tests/testthat", f))'
```

Expected: all PASS (the startup test covers `specimen_viewer_server` creation in all four panels).

- [ ] **Step 6: Commit**

```bash
git add R/app_specimen.R R/app_assemble.R R/app_assemble_userAsmb.R R/app_annotate.R R/app_export.R inst/app/www/custom.css tests/testthat/test-specimen-app.R
git commit -m "Specimen viewer with GEOME, GBIF, and Compare tabs"
```

---

### Task 9: Specimen Fields picker with GEOME and GBIF sections

**Files:**
- Modify: `R/app_specimen.R` (replace `geome_fields_modal` with `specimen_fields_modal`)
- Modify: `R/app_export.R` (`geome_col_defs()` -> `meta_col_defs()`; picker `on()`/save observer; table render)
- Modify: `R/app_ui.R` (~L107-110) and `R/app_ui_userAsmb.R` (~L107-110): toolbar button
- Modify: `R/app_server.R` (~L239-241) and `R/app_server_userAsmb.R` (~L209-211): forwarder
- Test: `tests/testthat/test-specimen-app.R`

**Interfaces:**
- Consumes: `meta_field_summary(con, source)`, `.meta_save_fields(con, keys)`, `.meta_key_col(key)` (Task 1); `GBIF_COMBOS` via `.meta_combos` (Task 4).
- Produces:
  - `specimen_fields_modal(ns, geome, gbif)`: `geome`/`gbif` are `meta_field_summary()` results. Inputs: `ns("geome_combos")`, `ns("gbif_combos")` (checkbox groups), reactable outputs `ns("geome_raw")`, `ns("gbif_raw")`, Save button `ns("specimen_fields_save")`.
  - Toolbar input `specimen_fields`, gargoyle flag `"specimen_fields"` (Task 11 triggers it from the Export Data modal).
  - Export table: every ticked key (both sources) renders as a column in the `Specimen` group, header tip `From GEOME` or `From GBIF`.

- [ ] **Step 1: Write failing tests**

In `tests/testthat/test-specimen-app.R` replace the test `"geome_fields_modal builds a checkbox list and a raw-fields reactable"` with:

```r
test_that("specimen_fields_modal has a GEOME and a GBIF section", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbWriteTable(con, "samples", data.frame(ID = "s1", Taxon = "x"))
  .meta_ensure_tables(con)
  DBI::dbAppendTable(con, "meta_records", data.frame(
    ID = "s1", source = c("GEOME", "GBIF"), level = c("Event", "Occurrence"), depth = 0L,
    ref = "r", field = c("country", "countryCode"), value = c("Peru", "PE")))
  .meta_save_fields(con, c("geome:combo:lat_lon", "gbif:combo:sex"))
  html <- as.character(specimen_fields_modal(NS("exp"), meta_field_summary(con, "GEOME"),
                                              meta_field_summary(con, "GBIF")))
  expect_match(html, "Specimen fields for export", fixed = TRUE)
  expect_match(html, "exp-geome_combos", fixed = TRUE)
  expect_match(html, "exp-gbif_combos", fixed = TRUE)
  expect_match(html, "exp-geome_raw", fixed = TRUE)
  expect_match(html, "exp-gbif_raw", fixed = TRUE)
  expect_match(html, "{gbif_specimen_voucher}", fixed = TRUE)
  expect_match(html, "value=\"gbif:combo:sex\" checked", fixed = TRUE)
  expect_lt(regexpr("exp-geome_combos", html), regexpr("exp-gbif_combos", html))
})
```

In the test `"the Export column picker offers a GEOME group, ..."`: after the existing `DBI::dbAppendTable(con, "meta_records", ...)` add

```r
  DBI::dbAppendTable(con, "meta_records", data.frame(
    ID = id1, source = "GBIF", level = "Occurrence", depth = 0L, ref = "1",
    field = "sex", value = "Female"))
```

change the save line to `.meta_save_fields(con, c("geome:combo:lat_lon", "geome:raw:Event:country", "gbif:combo:sex"))`, the shown-columns check to `expect_true(all(c("geome_lat_lon", "geome_Event_country", "gbif_sex") %in% id[shown]))`, and the group check to

```r
    expect_setequal(id[grepl("mp-grp-Specimen", cls)],
                    c("specimen", "geome_lat_lon", "geome_Event_country", "gbif_sex"))
```

- [ ] **Step 2: Run, verify fail**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-specimen-app.R")'`
Expected: FAIL, `could not find function "specimen_fields_modal"`.

- [ ] **Step 3: Modal in `R/app_specimen.R`**

Replace `geome_fields_modal` (and its roxygen) with:

```r
#' Modal listing GEOME and GBIF fields available at export
#'
#' @param ns module namespace function
#' @param geome,gbif `meta_field_summary()` output for each source
#' @noRd
specimen_fields_modal <- function(ns, geome, gbif) {
  section <- function(source, s) {
    key <- tolower(source)
    combos <- s[s$kind == "combo", ]
    tagList(
      h4(source),
      h5("GenBank-ready combinations"),
      checkboxGroupInput(
        ns(paste0(key, "_combos")), NULL, width = "100%",
        choiceValues = combos$key, selected = combos$key[combos$selected],
        choiceNames = lapply(seq_len(nrow(combos)), function(i) tagList(
          code(paste0("{", combos$col[i], "}")), " from ", combos$field[i], ": ",
          if (is.na(combos$example[i])) em("no samples") else
            tagList(tags$samp(combos$example[i]), sprintf(" (%d samples)", combos$n_samples[i]))
        ))
      ),
      h5(paste("All", source, "fields")),
      reactable::reactableOutput(ns(paste0(key, "_raw")))
    )
  }
  modalDialog(
    title = mp_modal_title("Specimen fields for export",
                           "Ticked fields become columns you can use in header templates"),
    size = "l", easyClose = TRUE,
    section("GEOME", geome),
    tags$hr(),
    section("GBIF", gbif),
    footer = mp_footer(primary = actionButton(ns("specimen_fields_save"), "Save"), dismiss = "Cancel")
  )
}
```

- [ ] **Step 4: Export module**

In `R/app_export.R`, replace the block from `geome_fields_ver <- reactiveVal(0L)` through the end of `observeEvent(input$geome_fields_save, {...})` with:

```r
    meta_fields_ver <- reactiveVal(0L)
    meta_col_defs <- function() {
      keys <- tryCatch(
        DBI::dbGetQuery(session$userData$con, "SELECT key FROM meta_export_fields")$key,
        error = function(e) character(0))
      cols <- vapply(keys, .meta_key_col, character(1), USE.NAMES = FALSE)
      tips <- paste("From", toupper(sub(":.*", "", keys)))
      stats::setNames(lapply(seq_along(cols), function(i) {
        colDef(show = TRUE, name = cols[i], header = rt_header(cols[i], tips[i]),
               class = "mp-grp-Specimen", headerClass = "mp-grp-Specimen",
               html = TRUE, cell = rt_longtext(), minWidth = 120)
      }), cols)
    }

    # Specimen field picker ----
    raw_fields_table <- function(raw) {
      reactable::reactable(
        raw[, c("level", "field", "n_samples", "example", "col")],
        selection = "multiple", onClick = "select", compact = TRUE, searchable = TRUE,
        defaultSelected = which(raw$selected), defaultPageSize = 10,
        columns = list(
          level = colDef(name = "Level"), field = colDef(name = "Field"),
          n_samples = colDef(name = "Samples", width = 80),
          example = colDef(name = "Example", cell = rt_longtext(), html = TRUE),
          col = colDef(name = "Template token", cell = function(v) paste0("{", v, "}"))
        )
      )
    }
    init("specimen_fields")
    on("specimen_fields", {
      con <- session$userData$con
      g <- meta_field_summary(con, "GEOME")
      b <- meta_field_summary(con, "GBIF")
      showModal(specimen_fields_modal(ns, g, b))
      output$geome_raw <- reactable::renderReactable(raw_fields_table(g[g$kind == "raw", ]))
      output$gbif_raw <- reactable::renderReactable(raw_fields_table(b[b$kind == "raw", ]))
    })

    observeEvent(input$specimen_fields_save, {
      con <- session$userData$con
      picked <- unlist(lapply(c("GEOME", "GBIF"), function(src) {
        s <- meta_field_summary(con, src)
        raw <- s[s$kind == "raw", ]
        raw$key[reactable::getReactableState(paste0(tolower(src), "_raw"), "selected") %||% integer(0)]
      }))
      .meta_save_fields(con, c(input$geome_combos, input$gbif_combos, picked))
      removeModal()
      meta_fields_ver(meta_fields_ver() + 1L)
      rv$data <- fetch_export_data()
    })
```

In `output$table <- reactable::renderReactable({`: `geome_fields_ver()` -> `meta_fields_ver()`, `geome_cols <- geome_col_defs()` -> `meta_cols <- meta_col_defs()`, and `columns = c(declared_cols, metadata_cols, geome_cols)` -> `columns = c(declared_cols, metadata_cols, meta_cols)`. Also delete the old comment above `geome_fields_ver` if it still names GEOME only, or reword it to "colDefs for the GEOME and GBIF fields ticked in the Specimen Fields picker, toggled as one group."

- [ ] **Step 5: Toolbar and forwarding**

In `R/app_ui.R` and `R/app_ui_userAsmb.R` replace

```r
              mp_toolbar_button(
                "geome_fields", "GEOME Fields",
                title = "Choose which GEOME fields are available at export"
              ),
```

with

```r
              mp_toolbar_button(
                "specimen_fields", "Specimen Fields",
                title = "Choose which GEOME and GBIF fields are available at export"
              ),
```

In `R/app_server.R` and `R/app_server_userAsmb.R` replace

```r
  observeEvent(input$geome_fields, {
    trigger("geome_fields")
  })
```

with

```r
  observeEvent(input$specimen_fields, {
    trigger("specimen_fields")
  })
```

```bash
grep -rn "geome_fields\|geome_col_defs\|geome_combos\b" R/ | grep -v "input\$geome_combos"
```

Expected: no output.

- [ ] **Step 6: Run tests, verify pass**

```bash
Rscript -e 'devtools::load_all(); for (f in c("test-specimen-app.R","test-export-metadata-cols.R")) testthat::test_file(file.path("tests/testthat", f))'
```

Expected: all PASS.

- [ ] **Step 7: Commit**

```bash
git add R/app_specimen.R R/app_export.R R/app_ui.R R/app_ui_userAsmb.R R/app_server.R R/app_server_userAsmb.R tests/testthat/test-specimen-app.R
git commit -m "Specimen Fields picker with GEOME and GBIF sections"
```

---

### Task 10: Export-time conflict warning

**Files:**
- Modify: `R/specimen_reconcile.R` (append pure functions)
- Modify: `R/app_export.R` (`observeEvent(input$export_data, ...)` ~L1467-1520; new observer)
- Test: `tests/testthat/test-specimen-conflicts.R` (append)

**Interfaces:**
- Consumes: `specimen_conflicts`, `specimen_csv_columns` (Task 6); `mp_confirm(id, title, text, action_label, danger, html)` and `mp_n()` (existing).
- Produces:
  - `specimen_template_concepts(templates, csv_cols)`: character vector of concepts the templates reach through `{geome_*}`/`{gbif_*}` combo or raw tokens, or through CSV columns mapped in `csv_cols` (a `specimen_csv_columns()` result; `{Taxon}` reaches `taxon`). `NA` templates are ignored.
  - `specimen_export_warnings(conflicts, concepts, ids)`: rows of `conflicts` with `status == "conflict"`, `concept %in% concepts`, `ID %in% ids`; columns `ID, concept, csv_value, geome_value, gbif_value`.
  - `specimen_warning_html(rows)`: `htmltools::HTML` body for the confirm dialog.
  - App: Export shows a confirm titled "Specimen metadata disagrees" with buttons Cancel / "Export anyway" before the fragmented-sample and overwrite checks. Notes never trigger it.

- [ ] **Step 1: Write failing tests (append to `test-specimen-conflicts.R`)**

```r
csv_map <- function() {
  list(coordinates = c("Latitude", "Longitude"), collection_date = "Date",
       country = "geo_loc_name", locality = character(), voucher = character(),
       collector = character(), sex = character(), dev_stage = character(), taxon = "Taxon")
}

test_that("specimen_template_concepts maps combo, raw, and CSV tokens to concepts", {
  f <- function(t) specimen_template_concepts(t, csv_map())
  expect_setequal(f("{seqid} [lat_lon={geome_lat_lon}] [geo_loc_name={gbif_geo_loc_name}]"),
                  c("coordinates", "country", "locality"))
  expect_setequal(f("{gbif_Occurrence_decimalLatitude} {geome_Event_yearCollected}"),
                  c("coordinates", "collection_date"))
  expect_setequal(f("[collection_date={Date}] {Taxon}"), c("collection_date", "taxon"))
  expect_equal(f("{seqid} [mgcode={genetic_code}] {completeness}"), character())
  expect_equal(f(c("{seqid}", NA, "[sex={geome_sex}]")), "sex")
  expect_equal(f("{geome_tissue_type} {gbif_identified_by} {gbif_Dataset_title}"), character())
})

test_that("specimen_export_warnings keeps only conflicts on used concepts for exported samples", {
  cf <- data.frame(ID = c("s1", "s1", "s2", "s3"), concept = c("country", "sex", "country", "country"),
                   csv_column = NA, csv_value = c("USA", "m", "Peru", "Chile"),
                   geome_value = c("Canada", "f", "Peru", "Peru"), gbif_value = NA,
                   status = c("conflict", "note", "agree", "conflict"))
  w <- specimen_export_warnings(cf, c("country", "sex"), c("s1", "s2"))
  expect_equal(w$ID, "s1")
  expect_equal(w$concept, "country")
  expect_equal(names(w), c("ID", "concept", "csv_value", "geome_value", "gbif_value"))
  expect_equal(nrow(specimen_export_warnings(cf, "sex", c("s1", "s2", "s3"))), 0L)
  html <- as.character(specimen_warning_html(w))
  expect_match(html, "<td>s1</td><td>country</td><td>USA</td><td>Canada</td><td>-</td>", fixed = TRUE)
  expect_match(html, "1 sample", fixed = TRUE)
})

test_that("a raw-token template triggers the warning end to end", {
  con <- spec_db()
  cf <- specimen_conflicts(con)
  concepts <- specimen_template_concepts("{seqid} [country={geome_Event_country}]", specimen_csv_columns(con))
  w <- specimen_export_warnings(cf, concepts, c("s1", "s2"))
  expect_equal(unique(w$ID), "s2")
  expect_equal(w$concept, "country")
  expect_equal(nrow(specimen_export_warnings(
    cf, specimen_template_concepts("{seqid} {completeness}", specimen_csv_columns(con)), "s2")), 0L)
})
```

- [ ] **Step 2: Run, verify fail**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-specimen-conflicts.R")'`
Expected: FAIL, `could not find function "specimen_template_concepts"`.

- [ ] **Step 3: Append to `R/specimen_reconcile.R`**

```r
.SPEC_COMBO_CONCEPTS <- list(
  lat_lon = "coordinates", collection_date = "collection_date",
  geo_loc_name = c("country", "locality"), specimen_voucher = "voucher",
  collected_by = "collector", sex = "sex", dev_stage = "dev_stage"
)
.SPEC_FIELD_CONCEPTS <- c(
  decimalLatitude = "coordinates", decimalLongitude = "coordinates",
  yearCollected = "collection_date", monthCollected = "collection_date",
  dayCollected = "collection_date", eventDate = "collection_date", year = "collection_date",
  month = "collection_date", day = "collection_date",
  country = "country", countryCode = "country", locality = "locality",
  catalogNumber = "voucher", institutionCode = "voucher", collectionCode = "voucher",
  collectorList = "collector", recordedBy = "collector", sex = "sex", lifeStage = "dev_stage",
  scientificName = "taxon"
)

specimen_template_concepts <- function(templates, csv_cols) {
  templates <- templates[!is.na(templates)]
  toks <- unlist(regmatches(templates, gregexpr("\\{[^{}]+\\}", templates)))
  toks <- unique(trimws(gsub("^\\{|\\}$", "", toks)))
  out <- character()
  for (t in toks) {
    m <- regmatches(t, regexec("^(geome|gbif)_(.+)$", t))[[1]]
    if (length(m)) {
      if (m[3] %in% names(.SPEC_COMBO_CONCEPTS)) {
        out <- c(out, .SPEC_COMBO_CONCEPTS[[m[3]]])
      } else {
        f <- sub("^[A-Za-z0-9]+_", "", m[3])
        if (f %in% names(.SPEC_FIELD_CONCEPTS)) out <- c(out, .SPEC_FIELD_CONCEPTS[[f]])
      }
      next
    }
    out <- c(out, names(csv_cols)[vapply(csv_cols, function(cc) t %in% cc, logical(1))])
  }
  unique(out)
}

specimen_export_warnings <- function(conflicts, concepts, ids) {
  keep <- conflicts$status %in% "conflict" & conflicts$concept %in% concepts & conflicts$ID %in% ids
  out <- conflicts[keep, c("ID", "concept", "csv_value", "geome_value", "gbif_value"), drop = FALSE]
  rownames(out) <- NULL
  out
}

specimen_warning_html <- function(rows) {
  cell <- function(x) ifelse(is.na(x), "-", htmltools::htmlEscape(as.character(x)))
  body <- paste0("<tr><td>", cell(rows$ID), "</td><td>", cell(rows$concept), "</td><td>",
                 cell(rows$csv_value), "</td><td>", cell(rows$geome_value), "</td><td>",
                 cell(rows$gbif_value), "</td></tr>", collapse = "")
  n <- length(unique(rows$ID))
  htmltools::HTML(paste0(
    "<p>", mp_n(n, "sample"), " in this group ", if (n == 1) "has" else "have",
    " specimen details that disagree between sources, on items your header template uses. ",
    "MitoPilot does not pick a value; check which one is right before submitting.</p>",
    "<div style=\"max-height: 260px; overflow-y: auto;\">",
    "<table class=\"table table-sm\" style=\"text-align: left; font-size: var(--mp-fs-meta);\">",
    "<thead><tr><th>Sample</th><th>Item</th><th>CSV</th><th>GEOME</th><th>GBIF</th></tr></thead>",
    "<tbody>", body, "</tbody></table></div>"
  ))
}
```

Note on the end-to-end test: `{geome_Event_country}` is a raw token, so it reaches the `country` concept through the field map; in the Task 6 fixture only s2 has a country conflict (Review Focus 5).

- [ ] **Step 4: Wire the warning into `R/app_export.R`**

In `observeEvent(input$export_data, ignoreInit = T, {...})`, replace everything from `frag <- fragmented_samples(input$export_group)` to the closing `check_overwrite_then_export()` of that observer with:

```r
      sp <- tryCatch(specimen_conflict_rows(input$export_group), error = function(e) NULL)
      if (!is.null(sp) && nrow(sp) > 0) {
        mp_confirm(
          ns("specimen_confirm"),
          title = "Specimen metadata disagrees",
          text = specimen_warning_html(sp),
          action_label = "Export anyway",
          danger = TRUE,
          html = TRUE
        )
        return()
      }
      fragmented_then_export()
```

Just above that observer (next to `check_overwrite_then_export`) add:

```r
    # Conflicts on the specimen items the active header templates use. Notes
    # never count, and a template without specimen tokens never warns.
    specimen_conflict_rows <- function(group) {
      ids <- unique(rv$data$ID[!is.na(rv$data$export_group) & rv$data$export_group == group])
      con <- session$userData$con
      tmpl <- c(input$fasta_header, if (isTRUE(input$export_genes)) input$fasta_header_gene)
      concepts <- specimen_template_concepts(tmpl, specimen_csv_columns(con))
      if (!length(concepts)) return(NULL)
      specimen_export_warnings(specimen_conflicts(con, ids), concepts, ids)
    }

    fragmented_then_export <- function() {
      frag <- fragmented_samples(input$export_group)
      if (length(frag) > 0) {
        shown <- paste(utils::head(frag, 5), collapse = ", ")
        if (length(frag) > 5) shown <- paste0(shown, ", and ", length(frag) - 5, " more")
        mp_confirm(
          ns("fragmented_confirm"),
          title = "Some samples export as multiple records",
          text = stringr::str_glue(
            "{mp_n(length(frag), 'sample')} have more than one assembly and will each ",
            "produce a SEPARATE GenBank record: {shown}.\n\n",
            "That is correct when the scaffolds really are different genomes. If a ",
            "sample is instead ONE genome broken into fragments, each record will ",
            "be submitted as an incomplete genome. Cancel and use consensus ",
            "trimming / scaffold joining to combine them, or 'ignore' all but one ",
            "scaffold."
          ),
          action_label = "Export anyway",
          danger = TRUE
        )
        return()
      }
      check_overwrite_then_export()
    }
```

(This is the existing fragmented-sample block, moved verbatim into a function.) Below the observer, next to `observeEvent(input$fragmented_confirm, ...)`, add:

```r
    observeEvent(input$specimen_confirm, ignoreInit = TRUE, {
      req(input$specimen_confirm)
      fragmented_then_export()
    })
```

- [ ] **Step 5: Run tests, verify pass**

```bash
Rscript -e 'devtools::load_all(); for (f in c("test-specimen-conflicts.R","test-specimen-app.R")) testthat::test_file(file.path("tests/testthat", f))'
```

Expected: all PASS (the startup test still creates `export_server` cleanly). The dialog itself is checked in the browser in Task 13.

- [ ] **Step 6: Commit**

```bash
git add R/specimen_reconcile.R R/app_export.R tests/testthat/test-specimen-conflicts.R
git commit -m "Warn at export when template specimen items conflict"
```

---

### Task 11: Export Data token chips

**Files:**
- Create: `R/app_export_tokens.R`
- Modify: `R/app_export.R` (Export Data modal "Available columns" block ~L685-720; two link observers)
- Modify: `inst/app/www/custom.js` (append), `inst/app/www/custom.css` (append)
- Test: `tests/testthat/test-export-tokens.R`

**Interfaces:**
- Consumes: `export_metadata_cols()` (Tasks 4, 7), `.meta_key_col()` (Task 1), gargoyle flag `"specimen_fields"` (Task 9).
- Produces:
  - `export_token_groups(data, sample_cols, ticked_keys)`: named list in order `Basics`, `Your CSV columns`, `GEOME`, `GBIF`, `Reference (BLAST)`, `Assembly and annotation`; each element `list(open = <lgl>, tokens = data.frame(token, insert, example), hint = <chr or NULL>)`. Tokens only for columns present in `data`; examples from `data[1, ]` (`""` when missing).
  - `export_token_ui(groups, target_id, ns)`: `div.mp-token-list[data-target=target_id]` with a filter box and one `<details>` per group; chips are `button.mp-token-chip[data-insert]`; GEOME and GBIF summaries hold an `actionLink(ns("token_fields_geome"|"token_fields_gbif"), "Choose fields")`.

- [ ] **Step 1: Write failing tests**

`tests/testthat/test-export-tokens.R`:

```r
tok_data <- function() {
  data.frame(
    ID = c("s1", "s2"), seqid = c("s1", "s2"), path = 1L, scaffold = 1L,
    Taxon = c("Fish a", "Fish b"), genetic_code = 2L, topology = "circular",
    completeness = "complete genome", site = c("Pond", "Lake"),
    GEOME_BCID = c("ark:/1/A", NA), GBIF_ID = c(NA, "6186461308"),
    geome_lat_lon = c("17.5 S 149.8 W", ""), gbif_Occurrence_countryCode = c("", "US"),
    blast_accession = "NC_1", length = 16500L, annotate_switch = 1L,
    blast_accession_auto = 0L, poor_blast_ref = "ok", export_time_stamp = NA,
    export_group = "g1", specimen = "ok", specimen_message = "GEOME: fetched"
  )
}
tok_cols <- c("ID", "Taxon", "genetic_code", "R1", "R2", "site", "GEOME_BCID", "GBIF_ID")

test_that("export_token_groups sorts columns into the six groups", {
  g <- export_token_groups(tok_data(), tok_cols,
                           c("geome:combo:lat_lon", "gbif:raw:Occurrence:countryCode"))
  expect_equal(names(g), c("Basics", "Your CSV columns", "GEOME", "GBIF",
                           "Reference (BLAST)", "Assembly and annotation"))
  expect_equal(g$Basics$tokens$token,
               c("seqid", "ID", "Taxon", "genetic_code", "topology", "completeness", "path", "scaffold"))
  expect_equal(g$Basics$tokens$insert[3], "{Taxon}")
  expect_equal(g$`Your CSV columns`$tokens$token, "site")
  expect_equal(g$GEOME$tokens$token, c("GEOME_BCID", "geome_lat_lon"))
  expect_equal(g$GEOME$tokens$insert, c("{GEOME_BCID}", "[lat_lon={geome_lat_lon}]"))
  expect_equal(g$GEOME$tokens$example, c("ark:/1/A", "17.5 S 149.8 W"))
  expect_equal(g$GBIF$tokens$insert, c("{GBIF_ID}", "{gbif_Occurrence_countryCode}"))
  expect_equal(g$GBIF$tokens$example, c("", ""))
  expect_equal(g$`Reference (BLAST)`$tokens$token, "blast_accession")
  expect_equal(g$`Assembly and annotation`$tokens$token, "length")
  expect_equal(unname(vapply(g, function(x) x$open, logical(1))),
               c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE))
  listed <- unlist(lapply(g, function(x) x$tokens$token))
  expect_false(any(c("annotate_switch", "blast_accession_auto", "poor_blast_ref", "export_time_stamp",
                     "export_group", "specimen", "specimen_message", "R1", "R2") %in% listed))
  expect_null(g$Basics$hint)
})

test_that("groups with nothing ticked or no extra columns say how to add some", {
  d <- tok_data()[, c("ID", "seqid", "Taxon", "GEOME_BCID", "GBIF_ID")]
  g <- export_token_groups(d, c("ID", "Taxon", "GEOME_BCID", "GBIF_ID"), character())
  expect_false(g$GEOME$open)
  expect_false(g$GBIF$open)
  expect_match(g$GEOME$hint, "Specimen Fields", fixed = TRUE)
  expect_match(g$GBIF$hint, "Specimen Fields", fixed = TRUE)
  expect_true(g$`Your CSV columns`$open)
  expect_match(g$`Your CSV columns`$hint, "no extra columns", fixed = TRUE)
  expect_equal(nrow(g$`Your CSV columns`$tokens), 0L)
  expect_equal(g$GEOME$tokens$token, "GEOME_BCID")
})

test_that("export_token_groups copes with no rows", {
  g <- export_token_groups(tok_data()[0, ], tok_cols, "geome:combo:lat_lon")
  expect_equal(g$GEOME$tokens$example, c("", ""))
})

test_that("export_token_ui renders chips with insert text, a filter box, and picker links", {
  g <- export_token_groups(tok_data(), tok_cols, "geome:combo:lat_lon")
  html <- as.character(export_token_ui(g, target_id = "export-fasta_header", ns = NS("export")))
  expect_match(html, "data-target=\"export-fasta_header\"", fixed = TRUE)
  expect_match(html, "data-insert=\"[lat_lon={geome_lat_lon}]\"", fixed = TRUE)
  expect_match(html, "data-insert=\"{Taxon}\"", fixed = TRUE)
  expect_match(html, "mp-token-filter", fixed = TRUE)
  expect_match(html, "export-token_fields_geome", fixed = TRUE)
  expect_match(html, "export-token_fields_gbif", fixed = TRUE)
  expect_match(html, "First row: Pond", fixed = TRUE)
  expect_equal(lengths(regmatches(html, gregexpr("<details open", html))), 3L)
})
```

- [ ] **Step 2: Run, verify fail**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-export-tokens.R")'`
Expected: FAIL, `could not find function "export_token_groups"`.

- [ ] **Step 3: `R/app_export_tokens.R`**

```r
EXPORT_TOKEN_BASICS <- c("seqid", "ID", "Taxon", "genetic_code", "topology", "completeness",
                         "path", "scaffold")
EXPORT_TOKEN_REFERENCE <- c("blast_accession", "blast_ref_status", "blast_species", "blast_lineage")
EXPORT_TOKEN_ASSEMBLY <- c("length", "structure", "PCGCount", "tRNACount", "rRNACount", "ORFCount",
                           "missing", "extra", "warnings", "partial", "curate_opts")

#' Group the columns usable in a header template for the Export Data modal
#'
#' @param data rows about to be exported (first row supplies the hover example)
#' @param sample_cols column names of the `samples` table
#' @param ticked_keys keys from `meta_export_fields`
#' @return named list of groups, each `list(open, tokens, hint)`
#' @noRd
export_token_groups <- function(data, sample_cols, ticked_keys) {
  ex <- function(col) {
    if (!nrow(data) || !col %in% names(data)) return("")
    v <- data[[col]][1]
    if (is.na(v)) "" else as.character(v)
  }
  tok <- function(cols, insert) {
    data.frame(token = cols, insert = insert,
               example = vapply(cols, ex, character(1), USE.NAMES = FALSE))
  }
  plain <- function(cols) {
    cols <- cols[cols %in% names(data)]
    if (!length(cols)) return(tok(character(), character()))
    tok(cols, paste0("{", cols, "}"))
  }
  meta <- function(prefix, id_col) {
    keys <- ticked_keys[startsWith(ticked_keys, paste0(prefix, ":"))]
    cols <- vapply(keys, .meta_key_col, character(1), USE.NAMES = FALSE)
    keep <- cols %in% names(data)
    keys <- keys[keep]
    cols <- cols[keep]
    ticked <- if (length(cols)) {
      combo <- grepl(":combo:", keys, fixed = TRUE)
      label <- sub("^[^:]+:combo:", "", keys)
      tok(cols, ifelse(combo, paste0("[", label, "={", cols, "}]"), paste0("{", cols, "}")))
    } else {
      tok(character(), character())
    }
    list(tokens = rbind(plain(id_col), ticked), n = nrow(ticked))
  }
  pick_hint <- "Nothing ticked yet. Use Specimen Fields in the Export toolbar to add fields."
  csv <- setdiff(export_metadata_cols(sample_cols, character()), EXPORT_TOKEN_BASICS)
  geome <- meta("geome", "GEOME_BCID")
  gbif <- meta("gbif", "GBIF_ID")
  csv_tokens <- plain(csv)
  list(
    Basics = list(open = TRUE, tokens = plain(EXPORT_TOKEN_BASICS), hint = NULL),
    `Your CSV columns` = list(
      open = TRUE, tokens = csv_tokens,
      hint = if (!nrow(csv_tokens)) "Your mapping file has no extra columns." else NULL),
    GEOME = list(open = geome$n > 0, tokens = geome$tokens,
                 hint = if (!geome$n) pick_hint else NULL),
    GBIF = list(open = gbif$n > 0, tokens = gbif$tokens,
                hint = if (!gbif$n) pick_hint else NULL),
    `Reference (BLAST)` = list(open = FALSE, tokens = plain(EXPORT_TOKEN_REFERENCE), hint = NULL),
    `Assembly and annotation` = list(open = FALSE, tokens = plain(EXPORT_TOKEN_ASSEMBLY), hint = NULL)
  )
}

#' Clickable token chips for the Export Data modal
#'
#' @param groups `export_token_groups()` output
#' @param target_id DOM id of the header box chips insert into by default
#' @param ns module namespace function (for the picker links)
#' @noRd
export_token_ui <- function(groups, target_id, ns) {
  div(
    class = "mp-token-list", `data-target` = target_id,
    tags$input(type = "search", class = "form-control input-sm mp-token-filter",
               placeholder = "Filter columns", `aria-label` = "Filter columns"),
    lapply(names(groups), function(g) {
      x <- groups[[g]]
      link <- if (g %in% c("GEOME", "GBIF")) {
        tagList(" ", actionLink(ns(paste0("token_fields_", tolower(g))), "Choose fields"))
      }
      tags$details(
        open = if (isTRUE(x$open)) NA else NULL, class = "mp-token-group",
        tags$summary(g, link),
        if (!is.null(x$hint)) p(class = "text-muted mp-token-hint", x$hint),
        div(class = "mp-token-chips", lapply(seq_len(nrow(x$tokens)), function(i) {
          tags$button(
            type = "button", class = "mp-token-chip", `data-insert` = x$tokens$insert[i],
            title = paste0("Inserts ", x$tokens$insert[i],
                           if (nzchar(x$tokens$example[i])) paste0("\nFirst row: ", x$tokens$example[i])),
            x$tokens$token[i]
          )
        }))
      )
    })
  )
}
```

- [ ] **Step 4: Chip behavior (JS) and style (CSS)**

Append to `inst/app/www/custom.js`:

```js
// Export Data token chips: a chip inserts its text at the cursor of the header
// box last focused in the same modal (the FASTA header box by default).
$(document).on('focusin', 'textarea', function() {
  var list = $(this).closest('.modal').find('.mp-token-list');
  if (list.length) list.attr('data-target', this.id);
});
$(document).on('click', '.mp-token-chip', function(e) {
  e.preventDefault();
  var box = document.getElementById($(this).closest('.mp-token-list').attr('data-target'));
  if (!box) return;
  var ins = this.getAttribute('data-insert');
  var s = box.selectionStart, t = box.selectionEnd, v = box.value;
  box.value = v.slice(0, s) + ins + v.slice(t);
  box.selectionStart = box.selectionEnd = s + ins.length;
  box.focus();
  $(box).trigger('input').trigger('change');
});
$(document).on('input', '.mp-token-filter', function() {
  var q = this.value.toLowerCase();
  var list = $(this).closest('.mp-token-list');
  list.find('.mp-token-chip').each(function() {
    $(this).toggle($(this).text().toLowerCase().indexOf(q) !== -1);
  });
  if (q) list.find('details').attr('open', '');
});
```

Append to `inst/app/www/custom.css`:

```css
/* Export Data token chips */
.mp-token-list { margin: 6px 0; }
.mp-token-filter { max-width: 260px; margin-bottom: 6px; }
.mp-token-group > summary { cursor: pointer; font-weight: 600; }
.mp-token-hint { margin: 2px 0 4px; font-size: var(--mp-fs-meta); }
.mp-token-chips { display: flex; flex-wrap: wrap; gap: 4px; margin: 4px 0 8px; }
.mp-token-chip {
  height: var(--mp-chip-h); padding: 0 8px; border: 1px solid var(--mp-border);
  border-radius: 12px; background: var(--mp-surface-alt); font-family: monospace;
  font-size: var(--mp-fs-meta); cursor: pointer;
}
.mp-token-chip:hover, .mp-token-chip:focus { border-color: var(--mp-primary); }
```

Check the variables exist: `grep -n "\-\-mp-chip-h\|\-\-mp-border:\|\-\-mp-surface-alt\|\-\-mp-primary:\|\-\-mp-fs-meta" inst/app/www/custom.css` should list all five; if one is missing use the nearest existing token from the `:root` block.

- [ ] **Step 5: Use the chips in the Export Data modal**

In `R/app_export.R` inside `on("export", {...})`, replace

```r
      # One collapsed list of usable tokens, split by where the column came
      # from. Bookkeeping fields are not offered (T23).
      bookkeeping <- c("annotate_switch", "blast_accession_auto",
                       "poor_blast_ref", "export_time_stamp")
      sample_cols <- tryCatch(
        colnames(dplyr::tbl(con, "samples")),
        error = function(e) character(0)
      )
      avail <- setdiff(names(rv$data), bookkeeping)
      yours <- sort(intersect(avail, sample_cols))
      ours <- sort(setdiff(avail, yours))
      cols_help <- tags$details(
        tags$summary("Available columns"),
```

with

```r
      # Usable tokens as grouped chips; bookkeeping fields are never offered (T23).
      sample_cols <- tryCatch(
        colnames(dplyr::tbl(con, "samples")),
        error = function(e) character(0)
      )
      ticked <- tryCatch(DBI::dbGetQuery(con, "SELECT key FROM meta_export_fields")$key,
                         error = function(e) character(0))
      token_groups <- export_token_groups(
        rv$data[!is.na(rv$data$export_group), , drop = FALSE], sample_cols, ticked
      )
      cols_help <- tags$details(
        open = NA,
        tags$summary("Available columns"),
```

In the same `cols_help`, add as the first words of the first `opts_help(...)`: `"Click a column to insert it at the cursor of the header box you last clicked. ", ` and replace

```r
        p(tags$b("Your columns: "), paste(yours, collapse = ", ")),
        p(tags$b("MitoPilot columns: "), paste(ours, collapse = ", ")),
```

with

```r
        export_token_ui(token_groups, target_id = ns("fasta_header"), ns = ns),
```

Next to the `init("specimen_fields")` block from Task 9 add:

```r
    observeEvent(input$token_fields_geome, trigger("specimen_fields"))
    observeEvent(input$token_fields_gbif, trigger("specimen_fields"))
```

- [ ] **Step 6: Run tests, verify pass**

```bash
Rscript -e 'devtools::load_all(); for (f in c("test-export-tokens.R","test-specimen-app.R","test-no-duplicate-definitions.R")) testthat::test_file(file.path("tests/testthat", f))'
```

Expected: all PASS. Insert-at-cursor, the filter box, and the picker links are checked in the browser in Task 13.

- [ ] **Step 7: Commit**

```bash
git add R/app_export_tokens.R R/app_export.R inst/app/www/custom.js inst/app/www/custom.css tests/testthat/test-export-tokens.R
git commit -m "Export Data: grouped, clickable column tokens"
```

---

### Task 12: Specimen Metadata vignette, pkgdown, cross-links, and NEWS

Text only; screenshots are taken and the article is built in Task 13.

**Files:**
- Rename: `vignettes/GEOME-Metadata.Rmd` -> `vignettes/Specimen-Metadata.Rmd` (`git mv`), then rewrite
- Delete: `vignettes/figures/geome_column.png`, `geome_viewer.png`, `geome_fields.png` (`git rm`; Task 13 adds the new figures)
- Modify: `_pkgdown.yml` (Usage articles), `vignettes/Your-Own-Project.Rmd:133`, `vignettes/Test-Project-Export.Rmd:185-186`, `NEWS.md:5`
- Modify: roxygen `vignette("GEOME-Metadata")` references in `R/init_db.R`, `R/init_db_userAsmb.R`, `R/init_project.R`, `R/init_project_userAsmb.R`

**Interfaces:**
- Consumes: every user-facing name from Tasks 1-11 (`mapping_gbif`, `fetch_gbif()`, `gbif_normalize_id()`, `set_metadata_columns()`, the Specimen column, the viewer tabs, the Specimen Fields button, the token chips, the export warning).
- Produces: figure file names Task 13 must create: `vignettes/figures/specimen_column.png`, `specimen_viewer_gbif.png`, `specimen_compare.png`, `specimen_fields.png`, `export_tokens.png`, `specimen_export_warning.png`.

- [ ] **Step 1: Move and delete**

```bash
cd ~/Documents/GitHub/MitoPilot-geome
git mv vignettes/GEOME-Metadata.Rmd vignettes/Specimen-Metadata.Rmd
git rm vignettes/figures/geome_column.png vignettes/figures/geome_viewer.png vignettes/figures/geome_fields.png
grep -rn "GEOME-Metadata\|geome_column.png\|geome_viewer.png\|geome_fields.png" R/ vignettes/ _pkgdown.yml NEWS.md
```

Every hit of that grep is fixed in Steps 2-4.

- [ ] **Step 2: Write `vignettes/Specimen-Metadata.Rmd`**

Replace the whole file with:

````markdown
---
title: "Specimen metadata: GEOME and GBIF"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Specimen metadata: GEOME and GBIF}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

<style>
.alert {
  border-left: 5px solid;
  padding: 10px;
  margin: 10px 0;
  border-radius: 5px;
}
.alert-tip { border-color: #28A745; background-color: #E9F7EF; }
.alert-note { border-color: #007BFF; background-color: #EBF5FF; }
.alert-warning { border-color: #FFC107; background-color: #FFF9E6; }
.alert-danger { border-color: #DC3545; background-color: #F8D7DA; }
strong { font-weight: bold; }
</style>

```{r, include = FALSE}
knitr::opts_chunk$set(eval = FALSE)
```

## What MitoPilot pulls, and from where

MitoPilot can link each sample to specimen records in two public databases:

- [GEOME](https://geome-db.org) (the Genomic Observatories Metadatabase) stores
  specimen, tissue, and collecting-event metadata. Every record has a BCID, a
  persistent identifier such as `ark:/21547/CXu2MBIO1000.1`. Given a BCID,
  MitoPilot fetches that record, walks up its parent records (for example
  Tissue, then Sample, then collecting Event), and adds the GEOME expedition
  and project.
- [GBIF](https://www.gbif.org) (the Global Biodiversity Information Facility)
  publishes occurrence records from museums and other collections. Every
  occurrence has a gbifID, a number such as `6186461308`. Given a gbifID,
  MitoPilot fetches the occurrence, the dataset it belongs to, and the
  organization that published it.

Everything is stored in the project database, so you can:

- browse the full records for each sample in the app,
- see where your mapping file, GEOME, and GBIF disagree, and
- pick fields, including GenBank-ready source modifiers such as `lat_lon` and
  `collection_date`, to use in FASTA headers at export.

MitoPilot never merges values or picks one source over another. Each source
keeps its own fields, and you decide which ones go into your headers.

Both links are optional. Projects without BCIDs or gbifIDs work exactly as
before.

<div class="alert alert-warning">
<strong>Public records only.</strong> MitoPilot reads public GEOME records and
public GBIF occurrences. Private GEOME projects are not supported yet, because
logging in to GEOME from another program needs credentials issued by the GEOME
team.
</div>

## Finding the identifiers

**GEOME BCID.** Open a record on [geome-db.org](https://geome-db.org) (a tissue,
sample, or event) and copy its identifier, which starts with `ark:/`. Use the
BCID of the most specific record that matches your sequenced material, usually
the tissue: MitoPilot fetches everything above it. Full URLs are trimmed to the
bare ARK, so all of these work:

```
ark:/21547/CXu2MBIO1000.1
https://n2t.net/ark:/21547/CXu2MBIO1000.1
https://geome-db.org/record/ark:/21547/CXu2MBIO1000.1
```

**GBIF ID.** Open the occurrence on [gbif.org](https://www.gbif.org) and copy
the number at the end of its address. The number and both link forms work:

```
6186461308
https://www.gbif.org/occurrence/6186461308
https://api.gbif.org/v1/occurrence/6186461308
```

<div class="alert alert-note">
<strong>GBIF IDs can change.</strong> GBIF may give an occurrence a new gbifID
when its dataset is republished. If an ID that used to work stops resolving,
MitoPilot keeps the data it fetched before and shows the fetch as failed. Look
the specimen up again on gbif.org (the stored <code>occurrenceID</code> and
catalog number help) and paste the new ID.
</div>

Some GEOME projects also publish to GBIF. Their GBIF occurrences carry the
GEOME ARK in `occurrenceID` or `catalogNumber`, and their publisher is "The
Genomic Observatories Metadatabase (GeOMe)". MitoPilot does not link the two
automatically; add both IDs if you want both.

## Adding identifiers at project setup

Add a column of BCIDs, a column of gbifIDs, or both to your mapping file (see
[Running Your Own Project](Your-Own-Project.html#the-mapping-file)), and name
them with `mapping_geome` and `mapping_gbif`:

```
ID,Taxon,R1,R2,Tissue_BCID,Occurrence
FISH01,Psenes pellucidus,FISH01_R1.fastq.gz,FISH01_R2.fastq.gz,ark:/21547/CXu2MBIO1000.1,
FISH02,Notemigonus crysoleucas,FISH02_R1.fastq.gz,FISH02_R2.fastq.gz,,6186461308
FISH03,Conger oceanicus,FISH03_R1.fastq.gz,FISH03_R2.fastq.gz,,
```

```r
new_project(path = "my_project", mapping_fn = "mapping.csv",
            mapping_geome = "Tissue_BCID", mapping_gbif = "Occurrence",
            data_path = "reads/")
```

- The columns are stored in the project as `GEOME_BCID` and `GBIF_ID`. If your
  columns already have those names, leave out `mapping_geome` and
  `mapping_gbif`.
- Blank cells are fine: those samples simply have no link.
- MitoPilot fetches the records during setup, which needs an internet
  connection. When setting up offline, pass `fetch_geome = FALSE` and
  `fetch_gbif = FALSE`, then run `fetch_geome()` and `fetch_gbif()` later.

`new_project_userAsmb()` takes the same arguments.

## Adding or changing identifiers later

`add_samples()` and `update_sample_metadata()` accept `mapping_geome`,
`mapping_gbif`, `fetch_geome`, and `fetch_gbif` too. With
`update_sample_metadata()`, samples whose identifier changed (or was added) are
fetched again, and a blank cell removes that sample's link and its data for
that source only.

To fetch again without touching the mapping file:

```r
fetch_geome("my_project")                     # refresh every BCID
fetch_gbif("my_project")                      # refresh every gbifID
fetch_gbif("my_project", ids = "FISH02")      # refresh one sample
fetch_gbif("my_project", ids = c("FISH01", "FISH02"),
           gbifs = c("2336663130", "6186461308"))
```

The last form sets (or replaces) the gbifIDs of those samples before fetching;
`fetch_geome()` does the same with `bcids`. A blank value removes the link.

When some fetches fail, the functions finish the rest and then give one warning
listing each failed sample with the reason, for example:

```
Warning message:
GBIF fetch failed for FISH02 (GBIF occurrence not found (IDs can change when a dataset is republished))
```

Records are fetched when you run these functions, not when the pipeline runs.

## Viewing records in the app

### The Specimen column

The Assemble, Annotate, and Export tables have a **Specimen** column right after
**Taxon**. Each sample gets one icon, and the most serious state wins:

| Icon | Meaning |
|---|---|
| Warning triangle | A GEOME or GBIF fetch failed. Hover to see why. |
| Orange flag | The sources disagree on at least one item (see [Comparing sources](#comparing-sources)). |
| Globe | Records fetched, no conflicts. |
| Faint plus | No GEOME BCID or GBIF ID. |

Hovering over an icon lists each source with its status, then any items in
conflict and any notes.

![Assemble table with the Specimen column showing fetched, failed, conflict, and empty samples](figures/specimen_column.png)

The column belongs to the **Specimen** entry in each table's **Columns**
picker. It is on when at least one sample has a BCID or gbifID, and off
otherwise. To add identifiers from the app in a project that has none yet, tick
**Specimen** in the Columns picker, then click a sample's plus icon.

### The specimen metadata viewer

Click any icon in the Specimen column to open the viewer for that sample.
Clicking the icon does not select or deselect the row. The title shows the
sample ID and its Taxon; **Sample** on the left lists every sample, with
"(failed)" or "(conflict)" after samples that need attention.

- **GEOME** shows the GEOME BCID box with **Fetch**, the time of the last fetch
  or the reason it failed, and the record level by level from Project down to
  the sample's own record. Each level links to its page on geome-db.org.
- **GBIF** shows the gbifID box with **Fetch**, then three cards: the
  **Occurrence**, its **Dataset** (with the citation GBIF asks you to use), and
  the publishing **Organization**. Each card links to gbif.org. GBIF's data
  quality flags for the occurrence are shown as badges in the `issues` row.
- **Compare** puts the sources side by side (see below).

**Expand all** and **Collapse all** open or close every card. Clearing an
identifier box and clicking **Fetch** removes that link. **Refresh all**
fetches every sample's GEOME and GBIF records again.

![Specimen viewer, GBIF tab, with occurrence issues and the dataset citation](figures/specimen_viewer_gbif.png)

## Comparing sources

The **Compare** tab lists each item MitoPilot checks, with the value from your
mapping file (and the column it came from), from GEOME, and from GBIF, and a
status:

| Status | Meaning |
|---|---|
| agree | Every source that has a value agrees. |
| note | A minor difference worth a look (shown in grey). |
| conflict | A real disagreement (shown in orange). |
| single | Only one source has a value. |

Blanks never conflict: only sources that both have a value are compared.

| Item | Agree when | If not |
|---|---|---|
| coordinates | Both parse (decimal, or `17.5 S 149.8 W`) and differ by at most 0.01 degree in each axis | conflict; unparseable value = note |
| collection_date | Equal at the coarser of the two precisions (`2026` agrees with `2026-02-23`) | conflict; unparseable value = note |
| country | Same ISO country code; names such as `USA`, `United States of America`, and `US` all map to the same code | conflict; a name MitoPilot cannot map, compared with a code, is a note |
| voucher | Same catalog number after dropping institution and collection prefixes, spaces, and case | conflict |
| taxon | First two words equal, ignoring case | conflict |
| locality, collector | Equal ignoring case and extra spaces | note |
| sex, dev_stage | Equal ignoring case | note |

![Compare tab with a country conflict and a collector note](figures/specimen_compare.png)

### Which mapping-file columns are compared

MitoPilot finds your mapping-file columns by name (ignoring case):

| Item | Columns tried, first match wins |
|---|---|
| coordinates | `lat_lon`; or a latitude column (`lat`, `latitude`, `decimalLatitude`) plus a longitude column (`lon`, `long`, `longitude`, `decimalLongitude`) |
| collection_date | `collection_date`, `date`, `eventDate` |
| country | `country`, `geo_loc_name` (the part before `:`) |
| locality | `locality` |
| voucher | `specimen_voucher`, `voucher`, `catalogNumber` |
| collector | `collected_by`, `collector`, `recordedBy` |
| sex | `sex` |
| dev_stage | `dev_stage`, `life_stage`, `lifeStage` |
| taxon | `Taxon` (always) |

If a column has another name, or a matching name holds something else, open
**CSV columns...** at the bottom of the Compare tab, pick the column for each
item (or **(none)** to skip it), and click **Save columns**. Leaving a box
empty goes back to automatic detection. The same works from R:

```r
set_metadata_columns("my_project", country = "Country_Name",
                     coordinates = c("Lat_DD", "Long_DD"))
set_metadata_columns("my_project", country = NA)   # back to automatic
set_metadata_columns("my_project", sex = "")       # do not compare sex
```

## Using specimen fields at export

Fetched data does not reach your exported files until you choose which fields
to use. In the Export module, click **Specimen Fields** in the toolbar.

![Specimen Fields window with GEOME and GBIF sections](figures/specimen_fields.png)

The window has a **GEOME** section and a **GBIF** section. Each starts with
**GenBank-ready combinations**: values built from one or more fields in the
format GenBank expects for a FASTA source modifier, each with an example and the
number of samples that have one. Below that, a table lists every raw field found
in the project's records; use its search box to find a field, and click rows to
tick them. Nothing is ticked by default. Click **Save** and each ticked field
becomes a column in the Export table, in the **Specimen** entry of the Columns
picker.

### GEOME combinations

Source fields are taken from the nearest level that has them (a tissue value
wins over a sample value, which wins over an event value).

| Column | Built from | Example |
|---|---|---|
| `geome_lat_lon` | `decimalLatitude`, `decimalLongitude` | `17.48260 S 149.89990 W` |
| `geome_collection_date` | `yearCollected`, `monthCollected`, `dayCollected` | `2006-03-18`, `2009-11`, or `2009` |
| `geome_geo_loc_name` | `country`, then `stateProvince` and `locality` | `French Polynesia: Moorea` |
| `geome_specimen_voucher` | `catalogNumber` (required), `institutionCode` (optional prefix) | `UF:12345` |
| `geome_collected_by` | `collectorList` | as in GEOME |
| `geome_tissue_type` | `tissueType` | as in GEOME |
| `geome_sex` | `sex` | as in GEOME |
| `geome_dev_stage` | `lifeStage` | as in GEOME |

### GBIF combinations

Built from the occurrence only (never from the dataset or organization).

| Column | Built from | Example |
|---|---|---|
| `gbif_lat_lon` | `decimalLatitude`, `decimalLongitude` | `28.53783 N 81.33322 W` |
| `gbif_collection_date` | `year`, `month`, `day`; else `eventDate` when it is a single date | `2026-02-23`, `2026-02`, or `2026` |
| `gbif_geo_loc_name` | `country`, then `stateProvince` and `locality` | `United States of America: Florida, Lake Underhill` |
| `gbif_specimen_voucher` | `institutionCode`, `collectionCode`, `catalogNumber` (required) | `UF:Fish:250399` |
| `gbif_collected_by` | `recordedBy` | as in GBIF |
| `gbif_identified_by` | `identifiedBy` | as in GBIF |
| `gbif_sex` | `sex` | lowercased |
| `gbif_dev_stage` | `lifeStage` | lowercased |

A combination is empty for a sample unless its required parts exist. Two GBIF
rules keep questionable values out of your headers:

- `gbif_lat_lon` is empty when GBIF flags the coordinates with
  `ZERO_COORDINATE`, `COORDINATE_INVALID`, or `COORDINATE_OUT_OF_RANGE`. Other
  flags, such as `COORDINATE_ROUNDED` or `COUNTRY_COORDINATE_MISMATCH`, are
  shown in the viewer but do not empty the value, so check them yourself.
- `gbif_specimen_voucher` is empty when the catalog number is a web address or
  an ARK rather than a real catalog number.

GBIF's `country` is its own English name (`United States of America`), which
is not always the name GenBank expects (`USA`). Check `geo_loc_name` values
against GenBank's country list before submitting.

Raw fields become columns named `geome_<Level>_<field>` or
`gbif_<Level>_<field>`, for example `geome_Tissue_tissueType` or
`gbif_Occurrence_occurrenceID`.

### Adding columns to a header template

The **Available columns** list in the Export Data window groups every column you
can use: **Basics**, **Your CSV columns**, **GEOME**, **GBIF**, **Reference
(BLAST)**, and **Assembly and annotation**. Click a column to insert it at the
cursor of the header box you last clicked (the mitogenome FASTA header box if
you have not clicked one). A GenBank-ready combination inserts the whole source
modifier, for example `[lat_lon={geome_lat_lon}]`. Hover over a column to see its
value for the first record in the export group, and type in the filter box to
find one. **Choose fields** next to GEOME or GBIF opens the Specimen Fields
window (this closes the Export Data window, so save your template first).

![Export Data window with grouped column chips](figures/export_tokens.png)

A template using GEOME and GBIF fields:

```
{seqid} [organism={Taxon}] [mgcode={genetic_code}] [location=mitochondrion] [lat_lon={gbif_lat_lon}] [collection_date={gbif_collection_date}] [geo_loc_name={geome_geo_loc_name}] [specimen_voucher={gbif_specimen_voucher}] {Taxon} mitochondrion, {completeness}
```

<div class="alert alert-warning">
<strong>Empty values stay in the header.</strong> If a sample has no value for
a field, the header still gets the modifier with nothing after it, such as
<code>[lat_lon=]</code>, which GenBank rejects. Only add a modifier to a template
when every sample in the export group has a value for it. MitoPilot never
changes your templates on its own.
</div>

### The conflict warning at export

When you click **Export**, MitoPilot checks which items your header templates
use: GEOME and GBIF combinations and raw fields, and mapping-file columns
matched to an item (including `{Taxon}`). If any sample in the export group has
a **conflict** on one of those items, a warning lists each sample, item, and the
differing values. Click **Export anyway** to continue or **Cancel** to go back
and fix the data or the template. Notes never trigger the warning.

![Export warning listing a country conflict](figures/specimen_export_warning.png)

The ticked columns are also written to the group's `sample_info` CSV.

## Limits and troubleshooting

- **Public records only** (see above).
- **Internet access is needed** by the machine running R or the app, since
  that is where GEOME and GBIF are contacted. Compute nodes running the
  pipeline never contact them.
- **A failed refresh of the same identifier keeps the previous data.** Only the
  status changes to failed. Changing a sample to a different identifier clears
  its old record for that source right away, even if the new fetch then fails.

Messages you may see in the viewer, on a warning icon, or in a fetch warning:

| Message | Meaning |
|---|---|
| `BCID not found in GEOME` | GEOME has no record with that identifier. GEOME answers unknown IDs with a server error, so a typo in a well-formed BCID shows up this way. |
| `record is private or needs a GEOME login` | The record belongs to a private GEOME project. |
| `could not reach GEOME (...)` | No connection to `api.geome-db.org`. Try again later with `fetch_geome()` or **Refresh all**. |
| `'x' is not a GEOME BCID (expected ark:/NNNNN/...)` | The value does not look like a BCID. |
| `BCID not recognized by GEOME` | GEOME rejected the identifier as malformed. |
| `GEOME returned HTTP ...` | Any other error from GEOME. Try again later. |
| `GBIF occurrence not found (IDs can change when a dataset is republished)` | GBIF has no occurrence with that gbifID. It may have been replaced when its dataset was republished; look the specimen up again on gbif.org. |
| `could not reach GBIF (...)` | No connection to `api.gbif.org`. Try again later with `fetch_gbif()` or **Refresh all**. |
| `'x' is not a GBIF occurrence ID (expected digits)` | The value is not a gbifID or a gbif.org occurrence link. |
| `GBIF returned HTTP ...` | Any other error from GBIF, for example when it is busy. Try again later. |
````

Check the text: `grep -nP "\x{2014}" vignettes/Specimen-Metadata.Rmd` prints nothing (no em dashes), and every list of three or more items has an Oxford comma.

- [ ] **Step 3: pkgdown, cross-links, roxygen, NEWS**

`_pkgdown.yml`, Usage articles: `  - 'GEOME-Metadata'` -> `  - 'Specimen-Metadata'`.

`vignettes/Your-Own-Project.Rmd:133`:

```
To link samples to GEOME records or GBIF occurrences, add a column of GEOME BCIDs or gbifIDs; see [Specimen metadata](Specimen-Metadata.html).
```

`vignettes/Test-Project-Export.Rmd:185-186`:

```
Samples linked to GEOME or GBIF records can also use `{geome_...}` and `{gbif_...}` tokens,
such as `{geome_lat_lon}` and `{gbif_collection_date}`; see [Specimen metadata](Specimen-Metadata.html).
```

In `R/init_db.R`, `R/init_db_userAsmb.R`, `R/init_project.R`, `R/init_project_userAsmb.R`: `vignette("GEOME-Metadata")` -> `vignette("Specimen-Metadata")`.

`NEWS.md`, replace the GEOME bullet under the development version's New Features with:

```
- Specimen metadata from GEOME and GBIF: link samples to GEOME BCIDs and GBIF occurrences (`mapping_geome`, `mapping_gbif`, `fetch_geome()`, `fetch_gbif()`), browse the records in the app, and choose fields, including GenBank-ready `lat_lon`, `collection_date`, `geo_loc_name`, and `specimen_voucher`, for export header templates. MitoPilot compares your mapping file, GEOME, and GBIF and flags disagreements (Compare tab, Specimen column, and a warning at export) without ever merging values; `set_metadata_columns()` picks which mapping-file columns are compared. The Export Data window now lists usable columns as grouped chips that insert at the cursor.
```

- [ ] **Step 4: Check and commit**

```bash
Rscript -e 'devtools::document()'
grep -rn "GEOME-Metadata\|geome_column.png\|geome_viewer.png\|geome_fields.png" R/ vignettes/ _pkgdown.yml NEWS.md man/
Rscript -e 'pkgdown::check_pkgdown()'
git add -A vignettes/ _pkgdown.yml NEWS.md R/init_db.R R/init_db_userAsmb.R R/init_project.R R/init_project_userAsmb.R man/
git commit -m "Specimen Metadata article, pkgdown, cross-links, and NEWS"
```

Expected: the grep prints nothing; `check_pkgdown()` reports no problems (all exported functions, including `fetch_gbif`, `gbif_normalize_id`, and `set_metadata_columns`, are in the reference index).

---

### Task 13: Real-browser verification, screenshots, and full checks

`testServer` cannot click through these flows here, so this task drives the real app in headless Chrome with the UI review harness and records what it saw. Fix any bug found in the task that owns the code, with a test, before finishing.

**Files:**
- Create: `vignettes/figures/specimen_column.png`, `specimen_viewer_gbif.png`, `specimen_compare.png`, `specimen_fields.png`, `export_tokens.png`, `specimen_export_warning.png`
- Create (not committed; harness lives outside the repo): `$H/steps/specimen_*.R`, where `H=/home/dmacguig/Documents/GitHub/MitoPilot/dev/ui_review`
- Scratch project: `~/MitoPilot_scratch/specimen_demo`

**Interfaces:**
- Consumes: everything from Tasks 1-12. Harness helpers from `$H/capture_lib.R` (`nav`, `goto`, `wait_for`, `click`, `click_text`, `click_contains`, `mbtn`, `swal`, `modal_text`, `swal_text`, `shot`, `shot_modal`, `resize`, `js`, `ROWS_ASM`, `ROWS_EXP`); see `$H/HARNESS_README.md`.
- Produces: the six figures, plus a pass/fail list of the checks in Steps 3-6 for the task report.

- [ ] **Step 1: Build the demo project (migrated GEOME data + GBIF + CSV, one deliberate conflict)**

```bash
rm -rf ~/MitoPilot_scratch/specimen_demo
cp -a ~/MitoPilot_scratch/geome_demo ~/MitoPilot_scratch/specimen_demo
cd ~/MitoPilot_scratch/specimen_demo
sqlite3 .sqlite "select name from sqlite_master where name like 'geome%'"
```

Expected: the three old `geome_*` tables are listed (this project predates Task 1).

```bash
cd ~/MitoPilot_scratch/specimen_demo
Rscript -e '
devtools::load_all("~/Documents/GitHub/MitoPilot-geome", quiet = TRUE)
con <- DBI::dbConnect(RSQLite::SQLite(), ".sqlite")
.meta_ensure_tables(con)
print(DBI::dbListTables(con)[grepl("^(geome|meta)_", DBI::dbListTables(con))])
s <- DBI::dbGetQuery(con, "SELECT ID, GEOME_BCID FROM samples ORDER BY ID")
print(s)
DBI::dbDisconnect(con)
'
```

Expected: only `meta_*` tables remain; the GEOME BCIDs are still there. Pick three sample IDs from the printout: `A` = one with a GEOME BCID of the French Polynesia records (for example `SRR22396940`, `ark:/21547/CXu2MBIO1000.1`), `B` = one without a BCID, `C` = another without a BCID. Then (replace `A`, `B`, `C`):

```bash
cd ~/MitoPilot_scratch/specimen_demo
Rscript -e '
devtools::load_all("~/Documents/GitHub/MitoPilot-geome", quiet = TRUE)
fetch_gbif(".", ids = c("A", "B", "C"), gbifs = c("6186461308", "2336663130", "999999999999"))
con <- DBI::dbConnect(RSQLite::SQLite(), ".sqlite")
tx <- DBI::dbGetQuery(con, "SELECT ID, Taxon FROM samples")
up <- data.frame(ID = c("A", "B"), Taxon = tx$Taxon[match(c("A", "B"), tx$ID)],
                 country = c("French Polynesia", "American Samoa"),
                 collection_date = c("2006", "2026"))
utils::write.csv(up, "specimen_up.csv", row.names = FALSE)
update_sample_metadata(".", "specimen_up.csv")
cf <- specimen_conflicts(con, c("A", "B"))
print(cf[!is.na(cf$status), ])
DBI::dbDisconnect(con)
'
```

Expected: one warning naming `C` (GBIF occurrence not found). For `A`, `country` is a conflict (French Polynesia from CSV and GEOME vs `US` from GBIF), which is the deliberate conflict; `B` has `country` agree. Make sure `A` is locked for export and in an export group (`sqlite3 .sqlite "select ID, export_group from export"`); if not, pick an `A` that is.

- [ ] **Step 2: Start the app**

```bash
H=/home/dmacguig/Documents/GitHub/MitoPilot/dev/ui_review
MP_REPO=/home/dmacguig/Documents/GitHub/MitoPilot-geome $H/app.sh start ~/MitoPilot_scratch/specimen_demo 3871
```

Expected: `app up on 3871`. Check `$H/app_logs/app_3871.log` has no error.

- [ ] **Step 3: Drive the Specimen column and viewer**

`$H/steps/specimen_01_viewer.R` (replace `A` with the real ID):

```r
id <- "A"
nav(wait = 5); wait_for(ROWS_ASM); Sys.sleep(3)
cat("icons:", js("[...document.querySelectorAll('#assemble-table .mp-specimen-cell i')].map(e => e.className.split(' ')[1]).join(' ')"), "\n")
cat("tips:", js("[...document.querySelectorAll('#assemble-table .mp-specimen-cell')].slice(0,4).map(e => e.title).join(' || ')"), "\n")
shot("assemble-specimen-column")
js(sprintf("document.querySelector('#assemble-table .mp-specimen-cell[data-id=\"%s\"]').click()", id)); Sys.sleep(6)
cat("title:", js("document.querySelector('.modal .mp-modal-title').innerText"), "\n")
cat("tabs:", js("[...document.querySelectorAll('.modal .nav-tabs a')].map(e => e.innerText).join(' | ')"), "\n")
cat("selected rows after icon click:", js("document.querySelectorAll('#assemble-table input[type=checkbox]:checked').length"), "\n")
click_text("GBIF", tag = "a"); Sys.sleep(3)
cat("gbif cards:", js("[...document.querySelectorAll('.modal details.mp-meta-level summary strong')].map(e => e.innerText).join(' | ')"), "\n")
cat("badges:", js("document.querySelectorAll('.modal .mp-pill-warning').length"), "\n")
cat("footer on screen:", js("document.querySelector('.modal .modal-footer').getBoundingClientRect().bottom <= window.innerHeight"), "\n")
shot_modal("specimen-viewer-gbif")
click_text("Compare", tag = "a"); Sys.sleep(3)
cat("compare rows:", js("[...document.querySelectorAll('.modal .mp-spec-compare tbody tr')].map(r => r.className + ':' + r.cells[0].innerText + '=' + r.cells[4].innerText).join(' | ')"), "\n")
shot_modal("specimen-compare")
click_text("CSV columns...", tag = "summary"); Sys.sleep(2)
cat("map inputs:", js("[...document.querySelectorAll('.modal select[id*=map_]')].map(e => e.id).join(' ')"), "\n")
mbtn("Close")
```

Run:

```bash
cd $H && APP_PORT=3871 SHOT_DIR=$H/shots/specimen CHROMOTE_CHROME=$H/chrome-wrap.sh Rscript capture_lib.R steps/specimen_01_viewer.R
```

Check the printed output against:
- icons include `fa-earth-americas`, `fa-flag` (sample `A`), `fa-triangle-exclamation` (sample `C`), and `fa-square-plus`; the tips show one line per source plus `Conflicts: country` for `A`;
- title `Specimen metadata: A`; tabs `GEOME | GBIF | Compare`; zero selected rows after the icon click;
- GBIF cards `Occurrence | Dataset | Organization`; badges > 0; footer on screen `TRUE` (cards scroll inside their box; header, ID box, tabs, and footer stay put);
- a compare row `mp-spec-conflict:country=conflict`;
- the map inputs include `...map_coordinates` and no `...map_taxon`.

Then, in the same way (a second step script), set a CSV mapping in the Compare tab: open it for `A`, pick `(none)` for country through the selectize (`js("$('#assemble-specimen-map_country')[0].selectize.setValue('__none__')")`), click **Save columns**, reopen Compare and confirm the `country` row now shows `single` or `conflict` from GEOME vs GBIF only and the CSV cell is `-`; then set it back to automatic (clear the selectize, Save) and confirm the conflict returns. Note: the module id of the viewer inside the Assemble panel is `assemble-specimen`, so its inputs are `assemble-specimen-<name>`; confirm with `modal_inputs()` if a selector misses.

- [ ] **Step 4: Drive the picker, token chips, and export warning**

`$H/steps/specimen_02_export.R`:

```r
nav(wait = 5); wait_for(ROWS_ASM); Sys.sleep(3)
goto("Export"); wait_for(ROWS_EXP); Sys.sleep(3)
click("#specimen_fields", wait = 8)
cat("sections:", js("[...document.querySelectorAll('.modal h4')].map(e => e.innerText).join(' | ')"), "\n")
js("document.querySelector('.modal input[value=\"gbif:combo:lat_lon\"]').click()")
js("document.querySelector('.modal input[value=\"geome:combo:geo_loc_name\"]').click()")
Sys.sleep(1)
shot_modal("specimen-fields")
mbtn("Save", wait = 8)
cat("export cols:", js("[...document.querySelectorAll('#export-table .rt-th')].map(e => e.innerText).filter(t => /gbif_|geome_/.test(t)).join(' | ')"), "\n")
click("#export", wait = 8)
cat("groups:", js("[...document.querySelectorAll('.modal .mp-token-group > summary')].map(e => e.innerText).join(' | ')"), "\n")
js("var b = document.getElementById('export-fasta_header'); b.focus(); b.setSelectionRange(b.value.length, b.value.length);")
js("[...document.querySelectorAll('.modal .mp-token-chip')].find(e => e.innerText === 'geome_geo_loc_name').click()")
Sys.sleep(2)
cat("header now:", js("document.getElementById('export-fasta_header').value"), "\n")
cat("status line:", js("document.getElementById('export-fasta_header_status').innerText"), "\n")
js("var f = document.querySelector('.modal .mp-token-filter'); f.value = 'gbif'; f.dispatchEvent(new Event('input', {bubbles: true}));")
Sys.sleep(1)
cat("visible chips:", js("[...document.querySelectorAll('.modal .mp-token-chip')].filter(e => e.offsetParent !== null).map(e => e.innerText).join(' ')"), "\n")
js("var f = document.querySelector('.modal .mp-token-filter'); f.value = ''; f.dispatchEvent(new Event('input', {bubbles: true}));")
resize(1600, 1600); Sys.sleep(1)
shot_modal("export-tokens")
resize(1600, 1000)
mbtn("Export", wait = 10)
cat("swal:", swal_text(), "\n")
shot("export-specimen-warning")
swal("Cancel")
cat("export modal still open:", js("document.querySelectorAll('.modal.in, .modal.show').length"), "\n")
```

Run it like Step 3 (`steps/specimen_02_export.R`). Check:
- sections `GEOME | GBIF`; after Save the Export table shows `gbif_lat_lon` and `geome_geo_loc_name` headers;
- groups `Basics | Your CSV columns | GEOME ... | GBIF ... | Reference (BLAST) | Assembly and annotation`;
- `header now` ends with `[geo_loc_name={geome_geo_loc_name}]` (inserted at the cursor, full snippet), and the status line is green or amber, not red;
- the filter leaves only `gbif...` chips visible;
- the export warning text names sample `A` and `country`; Cancel leaves the Export Data modal open.

Also check by hand in the same session (short extra script or interactively via `js()`): click into the gene header box (tick "Export individual ... genes" first), click a chip, and confirm the text went into `export-fasta_header_gene`, not the FASTA box; hover title on a chip shows `First row: ...`; the **Choose fields** link opens the Specimen Fields window.

- [ ] **Step 5: Real export and defline**

In the Export Data modal, pick the export group that contains `A` and set the FASTA header to

```
{seqid} [organism={Taxon}] [mgcode={genetic_code}] [location=mitochondrion] [lat_lon={gbif_lat_lon}] [geo_loc_name={geome_geo_loc_name}] {Taxon} mitochondrion, {completeness}
```

click **Export**, confirm the warning appears (`geo_loc_name` reaches the country conflict, `gbif_lat_lon` the coordinates), click **Export anyway**, let it finish (untick the outlier review if the harness gets stuck on it), then:

```bash
grep ">" ~/MitoPilot_scratch/specimen_demo/out/export/<group>/<group>.fasta | head
```

Expected: the defline of `A` carries `[lat_lon=28.53783 N 81.33322 W]` and `[geo_loc_name=French Polynesia...]` (values from their own sources, never merged).

- [ ] **Step 6: Startup regression on a project with no identifiers**

```bash
$H/app.sh stop 3871
MP_REPO=/home/dmacguig/Documents/GitHub/MitoPilot-geome $H/app.sh start ~/MitoPilot_scratch/geome_nobcid 3872
```

Load the app (`nav()` in a two-line step script), check the app log for `Operation not allowed without an active reactive context` or any error, confirm the Specimen column is hidden and **Specimen** is unticked in the Assemble Columns picker, tick it, and confirm the plus icons appear. Then `$H/app.sh stop 3872`.

- [ ] **Step 7: Figures**

Copy the screenshots into the vignette, cropping only if the harness shot includes empty page space:

```bash
S=$H/shots/specimen
cp $S/assemble-specimen-column.png ~/Documents/GitHub/MitoPilot-geome/vignettes/figures/specimen_column.png
cp $S/specimen-viewer-gbif.png ~/Documents/GitHub/MitoPilot-geome/vignettes/figures/specimen_viewer_gbif.png
cp $S/specimen-compare.png ~/Documents/GitHub/MitoPilot-geome/vignettes/figures/specimen_compare.png
cp $S/specimen-fields.png ~/Documents/GitHub/MitoPilot-geome/vignettes/figures/specimen_fields.png
cp $S/export-tokens.png ~/Documents/GitHub/MitoPilot-geome/vignettes/figures/export_tokens.png
cp $S/export-specimen-warning.png ~/Documents/GitHub/MitoPilot-geome/vignettes/figures/specimen_export_warning.png
```

Open each PNG (Read tool) and confirm it shows what its caption in `Specimen-Metadata.Rmd` says.

- [ ] **Step 8: Full checks**

```bash
cd ~/Documents/GitHub/MitoPilot-geome
Rscript -e 'devtools::document(); devtools::build_vignettes()' 2>&1 | tail -20
Rscript -e 'devtools::test()' 2>&1 | tail -30
Rscript -e 'pkgdown::build_article("Specimen-Metadata")' 2>&1 | tail -5
grep -rnP "[^\x00-\x7F]" R/ inst/extdata/ || echo "ascii ok"
```

Expected: vignette builds, the full test suite has no new failures (if a failure looks unrelated to this work, run that test file on `main` to confirm it predates the branch), the article renders with all six figures, and the last line is `ascii ok`.

- [ ] **Step 9: Commit**

```bash
git add vignettes/figures/specimen_*.png vignettes/figures/export_tokens.png
git commit -m "Specimen Metadata screenshots"
```

Report the check results from Steps 3-6 (each bullet pass/fail) in the task report.

