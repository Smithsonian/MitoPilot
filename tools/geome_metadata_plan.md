# GEOME Metadata Integration Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Let users attach a GEOME BCID to each sample, fetch that record plus its whole parent chain into the project DB, browse it in the app, and pick which fields (raw or GenBank-ready combinations) become export columns / header-template tokens.

**Architecture:** A small API layer (`R/geome_api.R`) walks GEOME's one-level `parent` links up to the root, then adds expedition and project metadata. A storage layer (`R/geome_db.R`) keeps raw fields long-format in `geome_records`, fetch status in `geome_status`, and the user's export picks in `geome_export_fields`; `samples.GEOME_BCID` holds the BCID. An export layer (`R/geome_export.R`) turns ticked keys into wide `geome_*` columns joined into export data. App pieces (`R/app_geome.R`) add an icon column after Taxon in every samples table, a shared viewer modal, and an Export-only field picker.

**Tech Stack:** R, DBI/RSQLite, dplyr/dbplyr, httr2, jsonlite, Shiny + reactable, gargoyle, testthat 3e + withr.

**Spec:** `tools/geome_metadata_spec.md` (read it first). API notes with real responses: `dev/geome/geome_api_research.md`.

## Global Constraints

- Work in the worktree `~/Documents/GitHub/MitoPilot-geome`, branch `geome-metadata`. Never push.
- ASCII only in R code (no non-ASCII characters, no em dashes in comments or strings).
- Minimal comments; match surrounding style.
- No new package dependencies: httr2 and jsonlite are already in Imports.
- API base: `https://api.geome-db.org`. Public records only, no auth.
- Reserved samples column: `GEOME_BCID`. Init arg: `mapping_geome = "GEOME_BCID"`. Skip-fetch arg: `fetch_geome = TRUE`.
- Tables: `geome_records(ID, level, depth, bcid, field, value)`, `geome_status(ID, bcid, status, message, fetched_at)`, `geome_export_fields(key)`.
- Export keys: `combo:<name>` and `raw:<level>:<field>`. Column names: `geome_<name>` and `geome_<level>_<field>` (non `[A-Za-z0-9_]` chars replaced by `_`).
- Nothing is ticked for export by default. No template is ever edited automatically.
- All GEOME calls run in the R session or app (driver side), never inside Nextflow.
- Commit messages brief, no Claude attribution lines.
- Heads-up: branch `genbank-accession-col` has uncommitted edits to `R/add_samples.R`, `R/init_db*.R`, `R/init_project*.R`, `R/init_checks.R`, `R/update_sample_metadata.R`, `R/app_export*.R`, `R/export.R`. Keep edits here small and localized to ease a later merge.

## Verified API facts (live, 2026-09-24)

- `GET /records/{bcid}?includeParent=true` returns `{projectId, record: {...flat..., entity, bcid}, parent: {...flat..., entity, bcid}}`. `parent` is absent at the root. It is one level only.
- Unknown BCID returns HTTP **500** (`{"usrMessage":"Server Error", ...}`), not 404.
- `GET /projects/{projectId}/expeditions/{expeditionCode}` returns expedition fields (`expeditionTitle`, `identifier`, nested `project`, `user`, `entityIdentifiers`).
- `GET /projects/{projectId}` returns **405**. Project metadata comes from `GET /projects?includePublic=true` (a JSON array, ~171 entries); filter on `projectId`.
- Example chain: `ark:/21547/CYC2CMPI38181.1` (Tissue) -> `ark:/21547/CYA2Reedy01` (Sample) -> `ark:/21547/CYB2REEDY` (Event), project 75, expedition `PLANT_LEGACY`.

## Review Focus

1. **Shared parents across samples:** ten tissues from one Event must fetch that Event once per run; a test counts calls (Task 1).
2. **Refetch failure keeps old data:** a sample that fetched fine, then fails on refresh, must still show its previous record plus a failed status (Task 2).
3. **BCID pasted as URL or with whitespace:** `https://n2t.net/ark:/21547/X`, `https://geome-db.org/record/ark:/21547/X`, and `" ark:/21547/X "` all normalize to `ark:/21547/X`; garbage is stored as typed and marked failed, not silently dropped (Tasks 1, 2).
4. **Old projects opened in the app:** projects created before this branch have no `geome_*` tables or `GEOME_BCID` column; the app and export must still work (Tasks 2, 6, 7).
5. **Coordinates with odd formatting:** `"+17.5"`, `"-149.830"`, out-of-range, or non-numeric latitude must give a valid GenBank `lat_lon` or empty, never a malformed string (Task 3).

---

### Task 1: GEOME API client and parent-chain walk

**Files:**
- Create: `R/geome_api.R`
- Create: `tests/testthat/helper-geome.R`
- Create: `tests/testthat/fixtures/geome/*.json`
- Test: `tests/testthat/test-geome-api.R`

**Interfaces:**
- Produces:
  - `geome_normalize_bcid(x)`: character vector in, bare `ark:/...` or `NA_character_` out (vectorized).
  - `.geome_get(path, query = list())`: parsed JSON (`simplifyVector = FALSE`) or `stop()` with a user-facing message.
  - `.geome_fetch_chain(bcid, cache = new.env())`: data.frame `level, depth, bcid, field, value` (all character except `depth` integer). Stops on failure of the sample's own record or any record in the chain.
  - Test helper `geome_fixture_get(path, query = list())` and counter env `geome_calls`.

- [ ] **Step 1: Record fixtures from the live API**

```bash
cd ~/Documents/GitHub/MitoPilot-geome
mkdir -p tests/testthat/fixtures/geome
cd tests/testthat/fixtures/geome
api=https://api.geome-db.org
for b in CYC2CMPI38181.1 CYA2Reedy01 CYB2REEDY; do
  curl -s --compressed "$api/records/ark:/21547/$b?includeParent=true" > "records_ark__21547_$b.json"
done
curl -s --compressed "$api/projects/75/expeditions/PLANT_LEGACY" \
  | python3 -c "import json,sys;d=json.load(sys.stdin);d.pop('user',None);print(json.dumps(d))" \
  > projects_75_expeditions_PLANT_LEGACY.json
curl -s --compressed "$api/projects?includePublic=true" \
  | python3 -c "import json,sys;d=json.load(sys.stdin);p=[x for x in d if x['projectId']==75];[x.pop('user',None) for x in p];print(json.dumps(p))" \
  > projects.json
ls -la; head -c 300 records_ark__21547_CYB2REEDY.json
```

Expected: five non-empty JSON files; the Event file has no `"parent"` key.

- [ ] **Step 2: Write test helper**

`tests/testthat/helper-geome.R`:

```r
geome_calls <- new.env()

geome_fixture_get <- function(path, query = list()) {
  geome_calls[[path]] <- (geome_calls[[path]] %||% 0L) + 1L
  f <- testthat::test_path("fixtures", "geome",
                           paste0(gsub("[^A-Za-z0-9.]", "_", path), ".json"))
  if (!file.exists(f)) stop("BCID not found in GEOME", call. = FALSE)
  jsonlite::fromJSON(f, simplifyVector = FALSE)
}

geome_reset_calls <- function() rm(list = ls(geome_calls), envir = geome_calls)
```

Check: `gsub("[^A-Za-z0-9.]", "_", "records/ark:/21547/CYB2REEDY")` gives `records_ark__21547_CYB2REEDY`, and `projects/75/expeditions/PLANT_LEGACY` gives `projects_75_expeditions_PLANT_LEGACY`, matching Step 1 file names.

- [ ] **Step 3: Write failing tests**

`tests/testthat/test-geome-api.R`:

```r
test_that("geome_normalize_bcid strips resolver prefixes and whitespace", {
  x <- c("ark:/21547/CYB2REEDY", " ark:/21547/CYB2REEDY ",
         "https://n2t.net/ark:/21547/CYB2REEDY",
         "http://n2t.net/ark:/21547/CYB2REEDY",
         "https://geome-db.org/record/ark:/21547/CYB2REEDY",
         "", NA, "not a bcid", "ark:/abc/X")
  expect_equal(
    geome_normalize_bcid(x),
    c(rep("ark:/21547/CYB2REEDY", 5), NA, NA, NA, NA)
  )
})

test_that(".geome_fetch_chain walks Tissue -> Sample -> Event -> Expedition -> Project", {
  local_mocked_bindings(.geome_get = geome_fixture_get)
  out <- .geome_fetch_chain("ark:/21547/CYC2CMPI38181.1")
  lv <- unique(out[order(out$depth), c("level", "depth")])
  expect_equal(lv$level, c("Tissue", "Sample", "Event", "Expedition", "Project"))
  expect_equal(lv$depth, 0:4)
  expect_equal(out$value[out$level == "Event" & out$field == "country"], "French Polynesia")
  expect_equal(out$value[out$level == "Project" & out$field == "projectTitle"], "Moorea Biocode")
  expect_equal(unique(out$bcid[out$level == "Sample"]), "ark:/21547/CYA2Reedy01")
  expect_false(any(out$field %in% c("user", "project", "entityIdentifiers")))
  expect_type(out$depth, "integer")
})

test_that("shared ancestors are fetched once per cache", {
  local_mocked_bindings(.geome_get = geome_fixture_get)
  geome_reset_calls()
  cache <- new.env()
  .geome_fetch_chain("ark:/21547/CYC2CMPI38181.1", cache)
  .geome_fetch_chain("ark:/21547/CYA2Reedy01", cache)
  expect_equal(geome_calls[["records/ark:/21547/CYB2REEDY"]], 1L)
  expect_equal(geome_calls[["projects"]], 1L)
})

test_that("a missing own record stops with the API message", {
  local_mocked_bindings(.geome_get = geome_fixture_get)
  expect_error(.geome_fetch_chain("ark:/21547/NOPE"), "not found")
})

test_that("a parent loop is refused", {
  fake <- function(path, query = list()) {
    list(record = list(entity = "Sample", bcid = "ark:/1/A"),
         parent = list(entity = "Sample", bcid = "ark:/1/A"))
  }
  local_mocked_bindings(.geome_get = fake)
  expect_error(.geome_fetch_chain("ark:/1/A"), "loops")
})

test_that("a failing expedition/project lookup keeps the record chain", {
  fake <- function(path, query = list()) {
    if (startsWith(path, "records/")) {
      return(list(record = list(entity = "Event", bcid = "ark:/1/E",
                                projectId = "9", expeditionCode = "X", country = "Peru")))
    }
    stop("record is private or needs a GEOME login", call. = FALSE)
  }
  local_mocked_bindings(.geome_get = fake)
  out <- .geome_fetch_chain("ark:/1/E")
  expect_equal(unique(out$level), "Event")
})

test_that("live GEOME walk works", {
  skip_on_cran()
  skip_if_offline("api.geome-db.org")
  out <- .geome_fetch_chain("ark:/21547/CYC2CMPI38181.1")
  expect_true(all(c("Tissue", "Sample", "Event") %in% out$level))
})
```

- [ ] **Step 4: Run tests, verify they fail**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-geome-api.R")'`
Expected: FAIL, `could not find function "geome_normalize_bcid"`.

- [ ] **Step 5: Implement `R/geome_api.R`**

```r
GEOME_API <- "https://api.geome-db.org"

#' Normalize GEOME BCIDs to bare ARKs
#'
#' @param x Character vector of BCIDs, optionally with an n2t.net or
#'   geome-db.org URL prefix.
#' @return Character vector of `ark:/NNNNN/...` identifiers, NA where the
#'   input is blank or not an ARK.
#' @export
geome_normalize_bcid <- function(x) {
  x <- trimws(as.character(x))
  x <- sub("^https?://[^/]+/(record/)?", "", x)
  ok <- !is.na(x) & grepl("^ark:/[0-9]+/[A-Za-z0-9._~-]+$", x)
  ifelse(ok, x, NA_character_)
}

.geome_get <- function(path, query = list()) {
  req <- httr2::request(paste0(GEOME_API, "/", path)) |>
    httr2::req_url_query(!!!query) |>
    httr2::req_user_agent("MitoPilot (https://github.com/Smithsonian/MitoPilot)") |>
    httr2::req_timeout(30) |>
    httr2::req_retry(max_tries = 3,
                     is_transient = function(r) httr2::resp_status(r) %in% c(429, 502, 503, 504)) |>
    httr2::req_error(is_error = function(r) FALSE)
  resp <- tryCatch(httr2::req_perform(req), error = function(e) {
    stop("could not reach GEOME (", conditionMessage(e), ")", call. = FALSE)
  })
  st <- httr2::resp_status(resp)
  if (st %in% c(401, 403)) stop("record is private or needs a GEOME login", call. = FALSE)
  if (st %in% c(404, 500)) stop("BCID not found in GEOME", call. = FALSE)
  if (st >= 400) stop("GEOME returned HTTP ", st, call. = FALSE)
  jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector = FALSE)
}

.geome_flatten <- function(x, level, depth, bcid) {
  keep <- vapply(x, function(v) {
    length(v) == 1L && !is.list(v) && !is.na(v) && nzchar(as.character(v))
  }, logical(1))
  x <- x[keep]
  if (!length(x)) return(NULL)
  data.frame(level = level, depth = as.integer(depth), bcid = bcid,
             field = names(x), value = vapply(x, as.character, ""),
             row.names = NULL)
}

.geome_fetch_chain <- function(bcid, cache = new.env()) {
  get <- function(key, path, query = list()) {
    if (is.null(cache[[key]])) cache[[key]] <- .geome_get(path, query)
    cache[[key]]
  }
  out <- list()
  seen <- character()
  cur <- bcid
  depth <- 0L
  top <- NULL
  while (!is.null(cur)) {
    if (cur %in% seen || depth >= 10L) {
      stop("GEOME parent chain loops or is too deep", call. = FALSE)
    }
    seen <- c(seen, cur)
    resp <- get(cur, paste0("records/", cur), list(includeParent = "true"))
    top <- resp$record
    out[[length(out) + 1L]] <- .geome_flatten(top, top$entity %||% "Record", depth, cur)
    cur <- resp$parent$bcid
    depth <- depth + 1L
  }
  pid <- top$projectId
  exp <- top$expeditionCode
  if (!is.null(pid) && !is.null(exp)) {
    tryCatch({
      e <- get(paste0("exp:", pid, ":", exp), paste0("projects/", pid, "/expeditions/", exp))
      out[[length(out) + 1L]] <- .geome_flatten(e, "Expedition", depth, e$identifier %||% NA_character_)
      projects <- get("projects", "projects", list(includePublic = "true"))
      p <- Filter(function(x) identical(as.character(x$projectId), as.character(pid)), projects)
      if (length(p)) {
        out[[length(out) + 1L]] <- .geome_flatten(p[[1]], "Project", depth + 1L, NA_character_)
      }
    }, error = function(e) NULL)
  }
  do.call(rbind, out)
}
```

Note: if the Expedition lookup fails but Project would not, Project is skipped too (both in one `tryCatch`). Acceptable; matches the test.

- [ ] **Step 6: Run tests, verify pass**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-geome-api.R")'`
Expected: all PASS (live test passes or skips).

If the live test fails on the URL (e.g. httr2 encodes `:`), inspect with `httr2::req_dry_run()` and build the URL so `ark:/21547/...` is sent literally.

- [ ] **Step 7: Commit**

```bash
git add R/geome_api.R tests/testthat/helper-geome.R tests/testthat/fixtures/geome tests/testthat/test-geome-api.R
git commit -m "GEOME API client and parent-chain walk"
```

---

### Task 2: Storage layer and `fetch_geome()`

**Files:**
- Create: `R/geome_db.R`
- Test: `tests/testthat/test-geome-db.R`

**Interfaces:**
- Consumes: `geome_normalize_bcid()`, `.geome_fetch_chain()` (Task 1).
- Produces:
  - `.geome_ensure_tables(con)`: creates the three tables if missing and adds `samples.GEOME_BCID` if missing. Idempotent.
  - `.geome_store_value(x)`: normalized ARK if valid; trimmed raw text if non-blank but invalid; `NA` if blank.
  - `.geome_fetch_into(con, ids, bcids, cache = new.env())`: fetches each sample, writes per-sample in one transaction, returns invisible data.frame `ID, status, message`, warns once listing failures.
  - `.geome_drop(con, ids)`: deletes `geome_records` and `geome_status` rows.
  - `.geome_set_bcid(con, id, raw)`: writes `samples.GEOME_BCID` via `.geome_store_value`; drops GEOME rows when blank. Returns stored value.
  - `fetch_geome(path = ".", ids = NULL, bcids = NULL)` (exported).

- [ ] **Step 1: Write failing tests**

`tests/testthat/test-geome-db.R`:

```r
geome_test_db <- function() {
  f <- withr::local_tempfile(fileext = ".sqlite", .local_envir = parent.frame())
  con <- DBI::dbConnect(RSQLite::SQLite(), f)
  withr::defer(DBI::dbDisconnect(con), envir = parent.frame())
  DBI::dbWriteTable(con, "samples", data.frame(ID = c("s1", "s2", "s3"), Taxon = "x"))
  con
}

test_that(".geome_ensure_tables is idempotent and adds GEOME_BCID", {
  con <- geome_test_db()
  .geome_ensure_tables(con)
  .geome_ensure_tables(con)
  expect_true(all(c("geome_records", "geome_status", "geome_export_fields") %in% DBI::dbListTables(con)))
  expect_true("GEOME_BCID" %in% DBI::dbListFields(con, "samples"))
})

test_that(".geome_store_value keeps bad input visible and blanks as NA", {
  expect_equal(.geome_store_value(c(" https://n2t.net/ark:/21547/X ", "junk", "", NA)),
               c("ark:/21547/X", "junk", NA, NA))
})

test_that(".geome_fetch_into stores records and ok status", {
  local_mocked_bindings(.geome_get = geome_fixture_get)
  con <- geome_test_db()
  .geome_fetch_into(con, "s1", "ark:/21547/CYC2CMPI38181.1")
  st <- DBI::dbGetQuery(con, "SELECT * FROM geome_status")
  expect_equal(st$status, "ok")
  expect_true(is.na(st$message))
  n <- DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM geome_records WHERE ID = 's1'")$n
  expect_gt(n, 10)
})

test_that("failures are per sample, warned once, and keep prior data", {
  local_mocked_bindings(.geome_get = geome_fixture_get)
  con <- geome_test_db()
  .geome_fetch_into(con, "s1", "ark:/21547/CYC2CMPI38181.1")
  before <- DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM geome_records WHERE ID = 's1'")$n
  expect_warning(
    res <- .geome_fetch_into(con, c("s1", "s2", "s3"),
                             c("ark:/21547/NOPE", "ark:/21547/CYB2REEDY", "junk")),
    "s1.*s3|s3.*s1"
  )
  expect_equal(res$status, c("failed", "ok", "failed"))
  expect_match(res$message[3], "not a GEOME BCID")
  after <- DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM geome_records WHERE ID = 's1'")$n
  expect_equal(after, before)
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
  expect_equal(DBI::dbGetQuery(con, "SELECT ID FROM geome_status")$ID, "s2")
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

- [ ] **Step 2: Run, verify fail**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-geome-db.R")'`
Expected: FAIL, `could not find function ".geome_ensure_tables"`.

- [ ] **Step 3: Implement `R/geome_db.R`**

```r
.geome_ensure_tables <- function(con) {
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS geome_records (
    ID TEXT NOT NULL, level TEXT NOT NULL, depth INTEGER NOT NULL, bcid TEXT,
    field TEXT NOT NULL, value TEXT, PRIMARY KEY (ID, depth, field))")
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS geome_status (
    ID TEXT NOT NULL, bcid TEXT, status TEXT NOT NULL, message TEXT,
    fetched_at INTEGER, PRIMARY KEY (ID))")
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS geome_export_fields (
    key TEXT NOT NULL, PRIMARY KEY (key))")
  if (DBI::dbExistsTable(con, "samples") &&
      !"GEOME_BCID" %in% DBI::dbListFields(con, "samples")) {
    DBI::dbExecute(con, "ALTER TABLE samples ADD COLUMN GEOME_BCID TEXT")
  }
  invisible(NULL)
}

.geome_store_value <- function(x) {
  raw <- trimws(as.character(x))
  raw[is.na(raw)] <- ""
  norm <- geome_normalize_bcid(raw)
  ifelse(!is.na(norm), norm, ifelse(nzchar(raw), raw, NA_character_))
}

.geome_drop <- function(con, ids) {
  for (id in ids) {
    DBI::dbExecute(con, "DELETE FROM geome_records WHERE ID = ?", params = list(id))
    DBI::dbExecute(con, "DELETE FROM geome_status WHERE ID = ?", params = list(id))
  }
  invisible(NULL)
}

.geome_set_bcid <- function(con, id, raw) {
  .geome_ensure_tables(con)
  val <- .geome_store_value(raw)
  DBI::dbExecute(con, "UPDATE samples SET GEOME_BCID = ? WHERE ID = ?", params = list(val, id))
  if (is.na(val)) .geome_drop(con, id)
  val
}

.geome_fetch_into <- function(con, ids, bcids, cache = new.env()) {
  .geome_ensure_tables(con)
  status <- character(length(ids))
  msg <- rep(NA_character_, length(ids))
  for (i in seq_along(ids)) {
    b <- bcids[i]
    res <- if (is.na(geome_normalize_bcid(b))) {
      simpleError(paste0("'", b, "' is not a GEOME BCID (expected ark:/NNNNN/...)"))
    } else {
      tryCatch(.geome_fetch_chain(b, cache), error = function(e) e)
    }
    ok <- !inherits(res, "error")
    status[i] <- if (ok) "ok" else "failed"
    if (!ok) msg[i] <- conditionMessage(res)
    DBI::dbWithTransaction(con, {
      if (ok) {
        DBI::dbExecute(con, "DELETE FROM geome_records WHERE ID = ?", params = list(ids[i]))
        DBI::dbAppendTable(con, "geome_records", cbind(ID = ids[i], res))
      }
      DBI::dbExecute(con, "INSERT OR REPLACE INTO geome_status VALUES (?, ?, ?, ?, ?)",
                     params = list(ids[i], b, status[i], msg[i], as.integer(Sys.time())))
    })
  }
  out <- data.frame(ID = ids, status = status, message = msg)
  bad <- out$status == "failed"
  if (any(bad)) {
    warning("GEOME fetch failed for ", .lst(paste0(out$ID[bad], " (", out$message[bad], ")")),
            call. = FALSE)
  }
  invisible(out)
}

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
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = file.path(path, ".sqlite"))
  on.exit(DBI::dbDisconnect(con))
  .geome_ensure_tables(con)
  samples <- DBI::dbGetQuery(con, "SELECT ID, GEOME_BCID FROM samples")
  if (!is.null(ids)) {
    unknown <- setdiff(ids, samples$ID)
    if (length(unknown)) stop("sample(s) not in this project: ", .lst(unknown), call. = FALSE)
  }
  if (!is.null(bcids)) {
    if (length(bcids) != length(ids)) stop("ids and bcids must be the same length", call. = FALSE)
    for (i in seq_along(ids)) .geome_set_bcid(con, ids[i], bcids[i])
    samples <- DBI::dbGetQuery(con, "SELECT ID, GEOME_BCID FROM samples")
  }
  target <- samples[!is.na(samples$GEOME_BCID) & (is.null(ids) | samples$ID %in% ids), ]
  if (!nrow(target)) {
    message("No samples with a GEOME BCID to fetch")
    return(invisible(data.frame(ID = character(), status = character(), message = character())))
  }
  .geome_fetch_into(con, target$ID, target$GEOME_BCID)
}
```

Note: `is.null(ids) | ...` with a scalar `TRUE` recycles correctly; keep it.

- [ ] **Step 4: Run tests, verify pass**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-geome-db.R")'`
Expected: PASS.

- [ ] **Step 5: Document and commit**

```bash
Rscript -e 'devtools::document()'
git add R/geome_db.R tests/testthat/test-geome-db.R NAMESPACE man/fetch_geome.Rd man/geome_normalize_bcid.Rd
git commit -m "GEOME storage tables and fetch_geome()"
```

---

### Task 3: GenBank-ready combinations and export columns

**Files:**
- Create: `R/geome_export.R`
- Test: `tests/testthat/test-geome-export.R`

**Interfaces:**
- Consumes: `geome_records`, `geome_export_fields` tables (Task 2).
- Produces:
  - `GEOME_COMBOS`: named list; each element `list(label, sources, fn)` where `fn(recs)` takes one sample's `geome_records` rows and returns a string or `NA`.
  - `.geome_key_col(key)`: `"combo:lat_lon"` -> `"geome_lat_lon"`; `"raw:Event:country"` -> `"geome_Event_country"`.
  - `.geome_key_value(recs, key)`: value for one sample.
  - `geome_export_cols(con, ids = NULL)`: data.frame `ID` + one column per ticked key, or `NULL` when nothing is ticked or tables are missing.
  - `geome_field_summary(con)`: data.frame `key, kind ("combo"|"raw"), level, field, col, n_samples, example, selected` for the picker (Task 8).

- [ ] **Step 1: Confirm source field names against a real record with coordinates**

```bash
curl -s --compressed "https://api.geome-db.org/records/Event/json?q=_projects_:75+decimalLatitude:*&limit=1" | head -c 1500
```

Expected: an Event with `decimalLatitude`, `decimalLongitude`, `yearCollected`, and likely `collectorList`, `locality`, `stateProvince`. If a name differs (e.g. `tissueType` vs `tissue_type`), update `GEOME_COMBOS` sources below and the spec table before continuing.

- [ ] **Step 2: Write failing tests**

`tests/testthat/test-geome-export.R`:

```r
recs <- function(...) {
  x <- list(...)
  data.frame(level = "Event", depth = 2L, field = names(x), value = unlist(x))
}

test_that("lat_lon formats hemispheres and keeps given precision", {
  f <- GEOME_COMBOS$lat_lon$fn
  expect_equal(f(recs(decimalLatitude = "-17.530", decimalLongitude = "-149.83")),
               "17.530 S 149.83 W")
  expect_equal(f(recs(decimalLatitude = "+38.9", decimalLongitude = "77.03")),
               "38.9 N 77.03 E")
  expect_true(is.na(f(recs(decimalLatitude = "95", decimalLongitude = "10"))))
  expect_true(is.na(f(recs(decimalLatitude = "abc", decimalLongitude = "10"))))
  expect_true(is.na(f(recs(decimalLatitude = "10"))))
})

test_that("collection_date handles partial dates", {
  f <- GEOME_COMBOS$collection_date$fn
  expect_equal(f(recs(yearCollected = "2009", monthCollected = "11", dayCollected = "5")), "2009-11-05")
  expect_equal(f(recs(yearCollected = "2009", monthCollected = "11")), "2009-11")
  expect_equal(f(recs(yearCollected = "2009", dayCollected = "5")), "2009")
  expect_true(is.na(f(recs(yearCollected = "09"))))
  expect_equal(f(recs(yearCollected = "2009", monthCollected = "13")), "2009")
})

test_that("geo_loc_name joins country with region and locality", {
  f <- GEOME_COMBOS$geo_loc_name$fn
  expect_equal(f(recs(country = "French Polynesia", locality = "Moorea")), "French Polynesia: Moorea")
  expect_equal(f(recs(country = "USA", stateProvince = "Maryland", locality = "Solomons")),
               "USA: Maryland, Solomons")
  expect_equal(f(recs(country = "French Polynesia", locality = "French Polynesia")), "French Polynesia")
  expect_true(is.na(f(recs(locality = "Moorea"))))
})

test_that("specimen_voucher needs a catalog number", {
  f <- GEOME_COMBOS$specimen_voucher$fn
  expect_equal(f(recs(institutionCode = "USNM", catalogNumber = "123")), "USNM:123")
  expect_equal(f(recs(catalogNumber = "123")), "123")
  expect_true(is.na(f(recs(institutionCode = "USNM"))))
})

test_that("nearest level wins when a field appears twice", {
  r <- rbind(
    data.frame(level = "Sample", depth = 1L, field = "country", value = "Near"),
    data.frame(level = "Event", depth = 2L, field = "country", value = "Far")
  )
  expect_equal(GEOME_COMBOS$geo_loc_name$fn(r), "Near")
})

test_that("geome_export_cols returns only ticked keys, NULL when none", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbWriteTable(con, "samples", data.frame(ID = c("s1", "s2"), Taxon = "x"))
  .geome_ensure_tables(con)
  expect_null(geome_export_cols(con))
  DBI::dbAppendTable(con, "geome_records", data.frame(
    ID = "s1", level = "Event", depth = 2L, bcid = "ark:/1/E",
    field = c("country", "locality"), value = c("Peru", "Lima")))
  DBI::dbAppendTable(con, "geome_export_fields",
                     data.frame(key = c("combo:geo_loc_name", "raw:Event:country")))
  out <- geome_export_cols(con, ids = c("s1", "s2"))
  expect_equal(names(out), c("ID", "geome_geo_loc_name", "geome_Event_country"))
  expect_equal(out$geome_geo_loc_name, c("Peru: Lima", NA))
  expect_equal(out$geome_Event_country, c("Peru", NA))
})

test_that("geome_export_cols is NULL on a project without GEOME tables", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  expect_null(geome_export_cols(con))
})

test_that("geome_field_summary lists combos and raw fields with counts", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbWriteTable(con, "samples", data.frame(ID = c("s1", "s2"), Taxon = "x"))
  .geome_ensure_tables(con)
  DBI::dbAppendTable(con, "geome_records", data.frame(
    ID = c("s1", "s2"), level = "Event", depth = 2L, bcid = "ark:/1/E",
    field = "country", value = c("Peru", "Chile")))
  DBI::dbAppendTable(con, "geome_export_fields", data.frame(key = "raw:Event:country"))
  s <- geome_field_summary(con)
  raw <- s[s$key == "raw:Event:country", ]
  expect_equal(raw$n_samples, 2L)
  expect_true(raw$selected)
  expect_equal(raw$col, "geome_Event_country")
  expect_true(all(paste0("combo:", names(GEOME_COMBOS)) %in% s$key))
})
```

- [ ] **Step 3: Run, verify fail**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-geome-export.R")'`
Expected: FAIL, `object 'GEOME_COMBOS' not found`.

- [ ] **Step 4: Implement `R/geome_export.R`**

```r
.geome_pick <- function(recs, field) {
  hit <- recs[recs$field == field & !is.na(recs$value) & nzchar(recs$value), ]
  if (!nrow(hit)) NA_character_ else hit$value[which.min(hit$depth)]
}

.geome_coord <- function(x, max) {
  if (is.na(x)) return(NULL)
  s <- sub("^\\+", "", trimws(x))
  n <- suppressWarnings(as.numeric(s))
  if (is.na(n) || abs(n) > max) return(NULL)
  list(neg = n < 0, txt = sub("^-", "", s))
}

.geome_passthrough <- function(field, label) {
  list(label = label, sources = field, fn = function(recs) .geome_pick(recs, field))
}

GEOME_COMBOS <- list(
  lat_lon = list(
    label = "lat_lon", sources = c("decimalLatitude", "decimalLongitude"),
    fn = function(recs) {
      la <- .geome_coord(.geome_pick(recs, "decimalLatitude"), 90)
      lo <- .geome_coord(.geome_pick(recs, "decimalLongitude"), 180)
      if (is.null(la) || is.null(lo)) return(NA_character_)
      paste(la$txt, if (la$neg) "S" else "N", lo$txt, if (lo$neg) "W" else "E")
    }
  ),
  collection_date = list(
    label = "collection_date", sources = c("yearCollected", "monthCollected", "dayCollected"),
    fn = function(recs) {
      y <- .geome_pick(recs, "yearCollected")
      if (is.na(y) || !grepl("^[0-9]{4}$", y)) return(NA_character_)
      m <- suppressWarnings(as.integer(.geome_pick(recs, "monthCollected")))
      d <- suppressWarnings(as.integer(.geome_pick(recs, "dayCollected")))
      if (is.na(m) || m < 1 || m > 12) return(y)
      if (is.na(d) || d < 1 || d > 31) return(sprintf("%s-%02d", y, m))
      sprintf("%s-%02d-%02d", y, m, d)
    }
  ),
  geo_loc_name = list(
    label = "geo_loc_name", sources = c("country", "stateProvince", "locality"),
    fn = function(recs) {
      co <- .geome_pick(recs, "country")
      if (is.na(co)) return(NA_character_)
      parts <- c(.geome_pick(recs, "stateProvince"), .geome_pick(recs, "locality"))
      parts <- unique(parts[!is.na(parts) & parts != co])
      if (length(parts)) paste0(co, ": ", paste(parts, collapse = ", ")) else co
    }
  ),
  specimen_voucher = list(
    label = "specimen_voucher", sources = c("institutionCode", "catalogNumber"),
    fn = function(recs) {
      cat <- .geome_pick(recs, "catalogNumber")
      if (is.na(cat)) return(NA_character_)
      inst <- .geome_pick(recs, "institutionCode")
      if (is.na(inst)) cat else paste0(inst, ":", cat)
    }
  ),
  collected_by = .geome_passthrough("collectorList", "collected_by"),
  tissue_type = .geome_passthrough("tissueType", "tissue_type"),
  sex = .geome_passthrough("sex", "sex"),
  dev_stage = .geome_passthrough("lifeStage", "dev_stage")
)

.geome_key_col <- function(key) {
  p <- strsplit(key, ":", fixed = TRUE)[[1]]
  nm <- if (p[1] == "combo") p[2] else paste(p[2], paste(p[-(1:2)], collapse = ":"), sep = "_")
  paste0("geome_", gsub("[^A-Za-z0-9_]", "_", nm))
}

.geome_key_value <- function(recs, key) {
  p <- strsplit(key, ":", fixed = TRUE)[[1]]
  if (p[1] == "combo") {
    spec <- GEOME_COMBOS[[p[2]]]
    return(if (is.null(spec)) NA_character_ else spec$fn(recs))
  }
  field <- paste(p[-(1:2)], collapse = ":")
  v <- recs$value[recs$level == p[2] & recs$field == field]
  if (length(v)) v[1] else NA_character_
}

geome_export_cols <- function(con, ids = NULL) {
  if (!DBI::dbExistsTable(con, "geome_export_fields")) return(NULL)
  keys <- DBI::dbGetQuery(con, "SELECT key FROM geome_export_fields")$key
  if (!length(keys)) return(NULL)
  recs <- DBI::dbGetQuery(con, "SELECT ID, level, depth, field, value FROM geome_records")
  ids <- ids %||% unique(recs$ID)
  out <- data.frame(ID = ids)
  for (k in keys) {
    out[[.geome_key_col(k)]] <- vapply(ids, function(i) {
      .geome_key_value(recs[recs$ID == i, ], k)
    }, character(1), USE.NAMES = FALSE)
  }
  out
}

geome_field_summary <- function(con) {
  .geome_ensure_tables(con)
  recs <- DBI::dbGetQuery(con, "SELECT ID, level, depth, field, value FROM geome_records")
  sel <- DBI::dbGetQuery(con, "SELECT key FROM geome_export_fields")$key
  ids <- unique(recs$ID)
  combos <- do.call(rbind, lapply(names(GEOME_COMBOS), function(nm) {
    vals <- vapply(ids, function(i) GEOME_COMBOS[[nm]]$fn(recs[recs$ID == i, ]), character(1))
    data.frame(key = paste0("combo:", nm), kind = "combo", level = NA_character_,
               field = paste(GEOME_COMBOS[[nm]]$sources, collapse = " + "),
               n_samples = sum(!is.na(vals)),
               example = if (any(!is.na(vals))) vals[!is.na(vals)][1] else NA_character_)
  }))
  raw <- if (nrow(recs)) {
    g <- unique(recs[, c("level", "depth", "field")])
    g <- g[order(-g$depth, g$level, g$field), ]
    data.frame(
      key = paste0("raw:", g$level, ":", g$field), kind = "raw", level = g$level,
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
  out$col <- vapply(out$key, .geome_key_col, character(1), USE.NAMES = FALSE)
  out$selected <- out$key %in% sel
  rownames(out) <- NULL
  out
}
```

Note: raw rows are unique per (level, field). If the same level name appears at two depths (unusual), `unique()` on key collapses them; that's intentional.

- [ ] **Step 5: Run tests, verify pass**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-geome-export.R")'`
Expected: PASS.

- [ ] **Step 6: Commit**

```bash
git add R/geome_export.R tests/testthat/test-geome-export.R
git commit -m "GEOME GenBank combos and export columns"
```

---

### Task 4: Project init: `mapping_geome`, `fetch_geome`, and checks

**Files:**
- Modify: `R/init_checks.R` (`check_mapping()` ~L190-230; `preflight_project()` ~L486-603)
- Modify: `R/init_db.R` (signature ~L80-86; mapping mutate ~L196-200; before `.mtr_warn_missing_refs(con)` near end)
- Modify: `R/init_db_userAsmb.R` (signature ~L89-95; mutate ~L192; end of function ~L950)
- Modify: `R/init_project.R` (signature ~L53-69; `preflight_project()` call ~L74; `new_db()` call ~L115)
- Modify: `R/init_project_userAsmb.R` (same three spots, ~L110 and ~L163)
- Test: `tests/testthat/test-geome-init.R`

**Interfaces:**
- Consumes: `geome_normalize_bcid`, `.geome_store_value`, `.geome_ensure_tables`, `.geome_fetch_into`.
- Produces: `new_db(..., mapping_geome = "GEOME_BCID", fetch_geome = TRUE)`, same on `new_db_userAsmb`, `new_project`, `new_project_userAsmb`. `check_mapping(..., mapping_geome = "GEOME_BCID")`.

- [ ] **Step 1: Write failing tests**

`tests/testthat/test-geome-init.R`:

```r
geome_mapping <- function(dir, bcid_col = "GEOME_BCID", bcids = c("ark:/21547/CYB2REEDY", "")) {
  m <- data.frame(ID = c("s1", "s2"), Taxon = "Fish a", R1 = c("a_1.fq", "b_1.fq"),
                  R2 = c("a_2.fq", "b_2.fq"))
  m[[bcid_col]] <- bcids
  f <- file.path(dir, "mapping.csv")
  utils::write.csv(m, f, row.names = FALSE)
  f
}

test_that("check_mapping warns on a non-ARK BCID and errors on a missing custom column", {
  m <- data.frame(ID = "s1", Taxon = "x", R1 = "a", R2 = "b", Bcid = "junk")
  iss <- check_mapping(m, mapping_geome = "Bcid")
  expect_match(paste(iss$warnings, collapse = " "), "GEOME")
  iss <- check_mapping(m, mapping_geome = "Nope")
  expect_match(paste(iss$errors, collapse = " "), "Nope")
  m$GEOME_BCID <- "ark:/1/A"
  iss <- check_mapping(m, mapping_geome = "Bcid")
  expect_match(paste(iss$errors, collapse = " "), "GEOME_BCID")
})

test_that("new_db stores a renamed BCID column and fetches it", {
  local_mocked_bindings(.geome_get = geome_fixture_get)
  d <- withr::local_tempdir()
  new_db(db_path = file.path(d, ".sqlite"), mapping_fn = geome_mapping(d, "Bcid"),
         mapping_geome = "Bcid")
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(d, ".sqlite"))
  on.exit(DBI::dbDisconnect(con))
  s <- DBI::dbGetQuery(con, "SELECT * FROM samples ORDER BY ID")
  expect_false("Bcid" %in% names(s))
  expect_equal(s$GEOME_BCID, c("ark:/21547/CYB2REEDY", NA))
  expect_equal(DBI::dbGetQuery(con, "SELECT ID, status FROM geome_status")$ID, "s1")
})

test_that("new_db fetch_geome = FALSE stores BCIDs without calling GEOME", {
  local_mocked_bindings(.geome_get = function(...) stop("should not be called"))
  d <- withr::local_tempdir()
  new_db(db_path = file.path(d, ".sqlite"), mapping_fn = geome_mapping(d), fetch_geome = FALSE)
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(d, ".sqlite"))
  on.exit(DBI::dbDisconnect(con))
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM geome_status")$n, 0L)
  expect_true("geome_export_fields" %in% DBI::dbListTables(con))
})

test_that("new_db without any BCID column still creates GEOME tables", {
  d <- withr::local_tempdir()
  m <- data.frame(ID = "s1", Taxon = "x", R1 = "a_1.fq", R2 = "a_2.fq")
  utils::write.csv(m, file.path(d, "mapping.csv"), row.names = FALSE)
  new_db(db_path = file.path(d, ".sqlite"), mapping_fn = file.path(d, "mapping.csv"))
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(d, ".sqlite"))
  on.exit(DBI::dbDisconnect(con))
  expect_true("GEOME_BCID" %in% DBI::dbListFields(con, "samples"))
})
```

Also add one `new_db_userAsmb` case mirroring the second test. Build its mapping the way `tests/testthat/test-init-db-userasmb.R` does (copy its fixture/mapping helper; add a `GEOME_BCID` column).

- [ ] **Step 2: Run, verify fail**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-geome-init.R")'`
Expected: FAIL, `unused argument (mapping_geome = "Bcid")`.

- [ ] **Step 3: `check_mapping()`**

Add `mapping_geome = "GEOME_BCID"` after `mapping_taxon` in the signature. After the `if (mapping_taxon != "Taxon") ...` line, add:

```r
  if (mapping_geome != "GEOME_BCID") {
    if (mapping_geome %nin% cols) {
      iss$err("mapping columns: GEOME BCID column '", mapping_geome, "' not found")
    }
    reserved <- c(reserved, "GEOME_BCID")
  }
```

After the Taxon block (where `lab` exists), add:

```r
  # GEOME BCIDs ----
  if (mapping_geome %in% cols) {
    raw <- trimws(as.character(mapping[[mapping_geome]]))
    raw[is.na(raw)] <- ""
    bad <- nzchar(raw) & is.na(geome_normalize_bcid(raw))
    if (any(bad)) {
      iss$warn("mapping GEOME BCID: not a GEOME ARK (ark:/NNNNN/...) for ",
               .lst(lab[bad]), "; these samples will show a failed GEOME fetch")
    }
  }
```

In `preflight_project()`, pass `mapping_geome = dots$mapping_geome %||% "GEOME_BCID"` to `check_mapping()`. After the mapping block, add:

```r
  geome_col <- dots$mapping_geome %||% "GEOME_BCID"
  if (!is.null(mapping) && geome_col %in% colnames(mapping) &&
      !isFALSE(dots$fetch_geome) &&
      any(!is.na(geome_normalize_bcid(mapping[[geome_col]])))) {
    .check_resource("https://api.geome-db.org/docs/geomeAPI.json", "GEOME", iss = iss)
  }
```

- [ ] **Step 4: `new_db()` and `new_db_userAsmb()`**

Add to both signatures after `mapping_taxon`:

```r
    mapping_geome = "GEOME_BCID",
    fetch_geome = TRUE,
```

Roxygen for both:

```r
#' @param mapping_geome Name of the mapping-file column holding GEOME BCIDs
#'   (optional). Stored as `GEOME_BCID`. See `vignette("GEOME-Metadata")`.
#' @param fetch_geome Fetch GEOME metadata for samples with a BCID during setup
#'   (default TRUE). Set FALSE when offline and run [fetch_geome()] later.
```

Pass `mapping_geome = mapping_geome` into the existing `check_mapping()` call. Right after the `dplyr::mutate(ID = ..., Taxon = ..., ...)` that precedes `CREATE TABLE samples`, add:

```r
  if (mapping_geome %in% colnames(mapping)) {
    mapping$GEOME_BCID <- .geome_store_value(mapping[[mapping_geome]])
    if (mapping_geome != "GEOME_BCID") mapping[[mapping_geome]] <- NULL
  }
```

At the end of the function, before `.mtr_warn_missing_refs(con)` (in `new_db_userAsmb`, before its final `invisible(return())`), add:

```r
  .geome_ensure_tables(con)
  if (fetch_geome && "GEOME_BCID" %in% colnames(mapping) && any(!is.na(mapping$GEOME_BCID))) {
    has <- !is.na(mapping$GEOME_BCID)
    .geome_fetch_into(con, mapping$ID[has], mapping$GEOME_BCID[has])
  }
```

- [ ] **Step 5: `new_project()` and `new_project_userAsmb()`**

Add `mapping_geome = "GEOME_BCID", fetch_geome = TRUE,` after `mapping_id` in both signatures, with `@param mapping_geome` and `@param fetch_geome` roxygen matching the lines above (plus "Passed to `new_db()`"). Before the `preflight_project()` call add:

```r
  dots$mapping_geome <- mapping_geome
  dots$fetch_geome <- fetch_geome
```

and add `mapping_geome = mapping_geome, fetch_geome = fetch_geome,` to the `new_db(...)` / `new_db_userAsmb(...)` call.

- [ ] **Step 6: Run tests, verify pass, run the init suites**

Run:
```bash
Rscript -e 'devtools::load_all(); for (f in c("test-geome-init.R","test-init-checks.R","test-init-db-userasmb.R","test-map-to-ref-refs.R")) testthat::test_file(file.path("tests/testthat", f))'
```
Expected: all PASS.

- [ ] **Step 7: Commit**

```bash
Rscript -e 'devtools::document()'
git add R/init_checks.R R/init_db.R R/init_db_userAsmb.R R/init_project.R R/init_project_userAsmb.R man/ tests/testthat/test-geome-init.R
git commit -m "GEOME BCID column and fetch at project setup"
```

---

### Task 5: `add_samples()` and `update_sample_metadata()`

**Files:**
- Modify: `R/add_samples.R`
- Modify: `R/update_sample_metadata.R`
- Test: `tests/testthat/test-geome-update.R`

**Interfaces:**
- Consumes: `.geome_store_value`, `.geome_ensure_tables`, `.geome_fetch_into`, `.geome_drop`.
- Produces: `add_samples(..., mapping_geome = "GEOME_BCID", fetch_geome = TRUE)`, `update_sample_metadata(..., mapping_geome = "GEOME_BCID", fetch_geome = TRUE)`.

- [ ] **Step 1: Write failing tests**

`tests/testthat/test-geome-update.R`:

```r
geome_project <- function(bcids = c("ark:/21547/CYB2REEDY", "")) {
  d <- withr::local_tempdir(.local_envir = parent.frame())
  m <- data.frame(ID = c("s1", "s2"), Taxon = "x", R1 = c("a_1", "b_1"),
                  R2 = c("a_2", "b_2"), GEOME_BCID = bcids)
  utils::write.csv(m, file.path(d, "mapping.csv"), row.names = FALSE)
  new_db(db_path = file.path(d, ".sqlite"), mapping_fn = file.path(d, "mapping.csv"))
  d
}

geome_q <- function(d, sql) {
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(d, ".sqlite"))
  on.exit(DBI::dbDisconnect(con))
  DBI::dbGetQuery(con, sql)
}

test_that("add_samples stores and fetches BCIDs for new samples only", {
  local_mocked_bindings(.geome_get = geome_fixture_get)
  d <- geome_project()
  up <- data.frame(ID = "s3", Taxon = "x", R1 = "c_1", R2 = "c_2",
                   MyBcid = "https://n2t.net/ark:/21547/CYA2Reedy01")
  utils::write.csv(up, file.path(d, "up.csv"), row.names = FALSE)
  add_samples(d, file.path(d, "up.csv"), mapping_geome = "MyBcid")
  expect_equal(geome_q(d, "SELECT GEOME_BCID FROM samples WHERE ID='s3'")$GEOME_BCID,
               "ark:/21547/CYA2Reedy01")
  expect_false("MyBcid" %in% names(geome_q(d, "SELECT * FROM samples")))
  expect_setequal(geome_q(d, "SELECT ID FROM geome_status")$ID, c("s1", "s3"))
})

test_that("update_sample_metadata refetches changed, drops cleared, skips unchanged", {
  local_mocked_bindings(.geome_get = geome_fixture_get)
  d <- geome_project(bcids = c("ark:/21547/CYB2REEDY", "ark:/21547/CYA2Reedy01"))
  t0 <- geome_q(d, "SELECT fetched_at FROM geome_status WHERE ID='s1'")$fetched_at
  geome_reset_calls()
  up <- data.frame(ID = c("s1", "s2"), Taxon = "x",
                   GEOME_BCID = c("ark:/21547/CYB2REEDY", ""))
  utils::write.csv(up, file.path(d, "up.csv"), row.names = FALSE)
  update_sample_metadata(d, file.path(d, "up.csv"))
  expect_equal(geome_q(d, "SELECT ID FROM geome_status")$ID, "s1")
  expect_equal(geome_q(d, "SELECT COUNT(*) n FROM geome_records WHERE ID='s2'")$n, 0L)
  expect_equal(length(ls(geome_calls)), 0L)
  up$GEOME_BCID <- c("ark:/21547/CYC2CMPI38181.1", "")
  utils::write.csv(up, file.path(d, "up.csv"), row.names = FALSE)
  update_sample_metadata(d, file.path(d, "up.csv"))
  lv <- geome_q(d, "SELECT DISTINCT level FROM geome_records WHERE ID='s1'")$level
  expect_true("Tissue" %in% lv)
})

test_that("update CSV without a BCID column leaves GEOME data alone", {
  local_mocked_bindings(.geome_get = geome_fixture_get)
  d <- geome_project()
  up <- data.frame(ID = "s1", Taxon = "y")
  utils::write.csv(up, file.path(d, "up.csv"), row.names = FALSE)
  update_sample_metadata(d, file.path(d, "up.csv"))
  expect_equal(geome_q(d, "SELECT ID FROM geome_status")$ID, "s1")
})
```

- [ ] **Step 2: Run, verify fail**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-geome-update.R")'`
Expected: FAIL, `unused argument (mapping_geome = "MyBcid")`.

- [ ] **Step 3: Implement in `add_samples()`**

Signature: add `mapping_geome = "GEOME_BCID", fetch_geome = TRUE` after `mapping_taxon`. Roxygen `@param` lines as in Task 4. Right before `# convert everything to characters` (after both `mutate` branches), add:

```r
  if (mapping_geome %in% colnames(mapping)) {
    mapping$GEOME_BCID <- .geome_store_value(mapping[[mapping_geome]])
    if (mapping_geome != "GEOME_BCID") mapping[[mapping_geome]] <- NULL
  }
```

At the end, after `.sync_sample_genetic_codes(...)` and before `.mtr_warn_missing_refs(con)`:

```r
  .geome_ensure_tables(con)
  if (fetch_geome && "GEOME_BCID" %in% colnames(mapping)) {
    has <- !is.na(mapping$GEOME_BCID)
    if (any(has)) .geome_fetch_into(con, mapping$ID[has], mapping$GEOME_BCID[has])
  }
```

- [ ] **Step 4: Implement in `update_sample_metadata()`**

Signature and roxygen as above. Add the same rename block right after the `mutate(ID = ..., Taxon = ...)` and before `# convert everything to characters`. After `sample_table` is read and converted to character, add:

```r
  old_bcid <- if ("GEOME_BCID" %in% colnames(sample_table)) {
    stats::setNames(sample_table$GEOME_BCID, sample_table$ID)
  } else {
    character()
  }
```

At the end, after the final `rows_upsert(...)`, add:

```r
  if ("GEOME_BCID" %in% colnames(mapping)) {
    .geome_ensure_tables(con)
    new <- mapping$GEOME_BCID
    old <- unname(old_bcid[mapping$ID])
    changed <- xor(is.na(new), is.na(old)) | (!is.na(new) & !is.na(old) & new != old)
    if (any(changed & is.na(new))) .geome_drop(con, mapping$ID[changed & is.na(new)])
    refetch <- changed & !is.na(new)
    if (fetch_geome && any(refetch)) {
      .geome_fetch_into(con, mapping$ID[refetch], new[refetch])
    }
  }
```

Note: `old_bcid[mapping$ID]` on an empty named vector yields `NA`s, which is the right "no old value" signal.

- [ ] **Step 5: Run tests, verify pass**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-geome-update.R")'`
Expected: PASS.

- [ ] **Step 6: Commit**

```bash
Rscript -e 'devtools::document()'
git add R/add_samples.R R/update_sample_metadata.R man/add_samples.Rd man/update_sample_metadata.Rd tests/testthat/test-geome-update.R
git commit -m "GEOME BCIDs in add_samples and update_sample_metadata"
```

---

### Task 6: Export integration

**Files:**
- Modify: `R/app_export_utils.R` (`fetch_export_data()` ~L269-395; `export_metadata_cols()` ~L412)
- Modify: `R/export.R` (sample `dat` builds at ~L429 and ~L1498)
- Test: `tests/testthat/test-geome-export.R` (append)

**Interfaces:**
- Consumes: `geome_export_cols(con, ids)`.
- Produces: export rows carry `geome_*` columns for ticked keys; `GEOME_BCID` is MitoPilot-owned (not in the Metadata group).

- [ ] **Step 1: Write failing tests (append to `test-geome-export.R`)**

```r
test_that("export_metadata_cols treats GEOME_BCID as owned", {
  expect_equal(export_metadata_cols(c("ID", "Taxon", "GEOME_BCID", "site"), character()), "site")
})

test_that(".geome_join adds ticked columns and is a no-op otherwise", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbWriteTable(con, "samples", data.frame(ID = "s1", Taxon = "x"))
  dat <- data.frame(ID = "s1", Taxon = "x")
  expect_identical(.geome_join(dat, con), dat)
  .geome_ensure_tables(con)
  DBI::dbAppendTable(con, "geome_records", data.frame(
    ID = "s1", level = "Event", depth = 2L, bcid = "ark:/1/E", field = "country", value = "Peru"))
  DBI::dbAppendTable(con, "geome_export_fields", data.frame(key = "raw:Event:country"))
  expect_equal(.geome_join(dat, con)$geome_Event_country, "Peru")
})

test_that("a ticked GEOME column resolves in a header template", {
  dat <- data.frame(ID = "s1", geome_lat_lon = "17.5 S 149.8 W")
  expect_equal(as.character(stringr::str_glue_data(dat, "{ID} [lat_lon={geome_lat_lon}]")),
               "s1 [lat_lon=17.5 S 149.8 W]")
})
```

- [ ] **Step 2: Run, verify fail**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-geome-export.R")'`
Expected: FAIL on `export_metadata_cols` and `.geome_join`.

- [ ] **Step 3: Implement**

Append to `R/geome_export.R`:

```r
.geome_join <- function(dat, con) {
  g <- geome_export_cols(con, ids = unique(dat$ID))
  if (is.null(g)) return(dat)
  dplyr::left_join(dat, g, by = "ID")
}
```

In `export_metadata_cols()` add `"GEOME_BCID"` to `owned`.

In `fetch_export_data()`: right after `dplyr::left_join(unit_topology, by = unit_key)` (end of the `out <- ...` chain) add `out <- .geome_join(out, db)`. Place the `geome_*` columns before `export_group`: the final `dplyr::relocate(export_group, .after = dplyr::last_col())` already keeps Export Group last. No other change needed.

In `R/export.R`, at both sample `dat` builds (the `dat <- dplyr::tbl(con, "samples") |> ... |> dplyr::collect()` chains near L429 and L1498), append `|> .geome_join(con)` after `dplyr::collect()`.

- [ ] **Step 4: Run tests, verify pass, run export suites**

```bash
Rscript -e 'devtools::load_all(); for (f in c("test-geome-export.R","test-export-metadata-cols.R","test-export-header.R","test-export-summary-csv.R")) testthat::test_file(file.path("tests/testthat", f))'
```
Expected: all PASS.

- [ ] **Step 5: Commit**

```bash
git add R/geome_export.R R/app_export_utils.R R/export.R tests/testthat/test-geome-export.R
git commit -m "Join ticked GEOME fields into export data"
```

---

### Task 7: GEOME status column in every samples table

**Files:**
- Create: `R/app_geome.R` (cell helper and status join here; viewer added in Task 8)
- Modify: `R/app_server.R` (~L18) and `R/app_server_userAsmb.R` (~L18): ensure tables on connect
- Modify: `R/app_assemble_utils.R` (`fetch_assemble_data()` `taxa` ~L15)
- Modify: `R/app_assemble_utils_userAsmb.R` (`taxa` ~L44)
- Modify: `R/app_annotate_utils.R` (`taxa` ~L20 and the `dplyr::select(dplyr::any_of(c(...)))` order list ~L159)
- Modify: `R/app_export_utils.R` (`fetch_export_data()`)
- Modify: `R/app_assemble.R` (after `Taxon = colDef(` ~L257), `R/app_assemble_userAsmb.R` (~L236), `R/app_annotate.R` (~L348), `R/app_export.R` (~L286)
- Test: `tests/testthat/test-geome-app.R`

**Interfaces:**
- Produces:
  - `.geome_status_join(tbl, db)`: left-joins lazy `geome_status` onto a lazy table keyed by `ID`, adding `geome` (`"ok"|"failed"|"none"`) and `geome_message`.
  - `rt_geome(inputId)`: `htmlwidgets::JS` cell renderer; click sends the row's `ID` to `inputId`.
  - `geome_col_def(inputId)`: the shared `colDef` for column `geome`.
  - Each panel module has input `geome_open` (namespaced) carrying a sample ID.

- [ ] **Step 1: Write failing tests**

`tests/testthat/test-geome-app.R`:

```r
test_that(".geome_status_join labels ok, failed, and none", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbWriteTable(con, "samples", data.frame(ID = c("a", "b", "c"), Taxon = "x"))
  .geome_ensure_tables(con)
  DBI::dbAppendTable(con, "geome_status", data.frame(
    ID = c("a", "b"), bcid = "ark:/1/A", status = c("ok", "failed"),
    message = c(NA, "BCID not found in GEOME"), fetched_at = 1L))
  out <- dplyr::tbl(con, "samples") |> .geome_status_join(con) |> dplyr::collect()
  out <- out[order(out$ID), ]
  expect_equal(out$geome, c("ok", "failed", "none"))
  expect_equal(out$geome_message[2], "BCID not found in GEOME")
})

test_that("rt_geome sends the row ID to the given input", {
  js <- as.character(rt_geome("assemble-geome_open"))
  expect_match(js, "assemble-geome_open", fixed = TRUE)
  expect_match(js, "setInputValue", fixed = TRUE)
  expect_match(js, "dataset.id", fixed = TRUE)
})
```

- [ ] **Step 2: Run, verify fail**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-geome-app.R")'`
Expected: FAIL, `could not find function ".geome_status_join"`.

- [ ] **Step 3: Implement helpers in `R/app_geome.R`**

```r
.geome_status_join <- function(tbl, db) {
  tbl |>
    dplyr::left_join(
      dplyr::tbl(db, "geome_status") |>
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

rt_geome <- function(inputId) {
  sprintf(
    "function(cellInfo) {
      var st = cellInfo.value || 'none';
      var row = cellInfo.row || {};
      var esc = function(s) { return String(s).replace(/&/g, '&amp;').replace(/'/g, '&#39;')
        .replace(/\"/g, '&quot;').replace(/</g, '&lt;').replace(/>/g, '&gt;'); };
      var cls = st === 'ok' ? 'fa-solid fa-earth-americas' :
        (st === 'failed' ? 'fa-solid fa-triangle-exclamation mp-fg-warning' : 'fa-regular fa-square-plus mp-muted');
      var tip = st === 'ok' ? 'GEOME record fetched. Click to view.' :
        (st === 'failed' ? 'GEOME fetch failed: ' + (row['geome_message'] || 'unknown error') + '. Click to fix or retry.' :
        'No GEOME BCID. Click to add one.');
      return `<a href='#' class='mp-geome-cell' data-id='${esc(row['ID'])}' title='${esc(tip)}' aria-label='${esc(tip)}' ` +
        `onclick=\"event.preventDefault(); event.stopPropagation(); Shiny.setInputValue('%s', this.dataset.id, {priority: 'event'})\">` +
        `<i class='${cls}' aria-hidden='true'></i></a>`;
    }",
    inputId
  ) |>
    htmlwidgets::JS()
}

geome_col_def <- function(inputId) {
  reactable::colDef(
    show = TRUE, name = "GEOME", sticky = "left", width = 70, align = "center",
    html = TRUE, filterable = FALSE, sortable = TRUE,
    header = rt_header("GEOME", "GEOME metadata for this sample. Click an icon to view, add, or refresh."),
    cell = rt_geome(inputId)
  )
}
```

Check that `mp-muted` exists in the app CSS (`grep -rn "mp-muted" inst/`); if not, use whatever muted-text class the CSS already defines (for example `text-muted`).

- [ ] **Step 4: Wire the status into each data fetch**

- `R/app_server.R` and `R/app_server_userAsmb.R`, right after `session$userData$con <- DBI::dbConnect(...)`:
  ```r
  .geome_ensure_tables(session$userData$con)
  ```
- `fetch_assemble_data()`, `fetch_annotate_units()`: change `taxa <- dplyr::tbl(db, "samples") |> dplyr::select(ID, Taxon)` to append `|> .geome_status_join(db)`. userAsmb: same on its `select(ID, Taxon, topology, assembly)`.
- `R/app_annotate_utils.R` select-order list (~L159): insert `"geome", "geome_message"` right after `"Taxon"`.
- `fetch_export_data()`: change `samples <- dplyr::tbl(db, "samples") |> dplyr::select(-dplyr::any_of("topology"))` to append `|> .geome_status_join(db)`, and add `dplyr::relocate(geome, .after = Taxon)` next to the existing `dplyr::relocate(Taxon, .after = ID)`.
- Make sure `geome` lands right after `Taxon` in the Assemble data frames too: add `dplyr::relocate(dplyr::any_of(c("geome", "geome_message")), .after = Taxon)` at the end of `fetch_assemble_data()` and the userAsmb fetch (reactable renders in data-frame order).
- `export_metadata_cols()`: add `"geome", "geome_message"` to `owned`.

- [ ] **Step 5: Add the colDef after Taxon in each table**

In `R/app_assemble.R`, `R/app_assemble_userAsmb.R`, `R/app_annotate.R` (right after the `Taxon = colDef(...)` entry) and `R/app_export.R` (right after `Taxon = .cd(...)`), add:

```r
            geome = geome_col_def(ns("geome_open")),
```

`geome_message` stays hidden via each table's `defaultColDef = colDef(show = FALSE)`; confirm the Annotate table also uses a hidden default, otherwise add `geome_message = colDef(show = FALSE)`.

- [ ] **Step 6: Run tests, verify pass, run app-unit suites**

```bash
Rscript -e 'devtools::load_all(); for (f in c("test-geome-app.R","test-userasmb-app-units.R","test-ui-reactable-helpers.R","test-export-metadata-cols.R")) testthat::test_file(file.path("tests/testthat", f))'
```
Expected: all PASS.

- [ ] **Step 7: Commit**

```bash
git add R/app_geome.R R/app_server.R R/app_server_userAsmb.R R/app_assemble_utils.R R/app_assemble_utils_userAsmb.R R/app_annotate_utils.R R/app_export_utils.R R/app_assemble.R R/app_assemble_userAsmb.R R/app_annotate.R R/app_export.R tests/testthat/test-geome-app.R
git commit -m "GEOME status column after Taxon in sample tables"
```

---

### Task 8: GEOME viewer modal

**Files:**
- Modify: `R/app_geome.R` (add viewer)
- Modify: `R/app_assemble.R`, `R/app_assemble_userAsmb.R`, `R/app_annotate.R`, `R/app_export.R` (one call each inside the module server)
- Test: `tests/testthat/test-geome-app.R` (append)

**Interfaces:**
- Consumes: `.geome_set_bcid`, `.geome_fetch_into`, `geome_normalize_bcid`.
- Produces:
  - `geome_record_view(recs)`: pure function; takes one sample's `geome_records` rows and returns a `tagList` of `<details>` cards ordered root first (highest depth first). Testable.
  - `geome_viewer_server(id, open, on_change)`: nested module. `open` is a reactive yielding a sample ID; `on_change` is a function called after any DB write (the panel passes its table refresh).

- [ ] **Step 1: Write failing test (append)**

```r
test_that("geome_record_view orders levels root first and links BCIDs", {
  recs <- data.frame(
    level = c("Tissue", "Event", "Project"), depth = c(0L, 2L, 3L),
    bcid = c("ark:/1/T", "ark:/1/E", NA), field = c("tissueID", "country", "projectTitle"),
    value = c("T1", "Peru", "My proj"))
  html <- as.character(geome_record_view(recs))
  expect_lt(regexpr("Project", html), regexpr("Event", html))
  expect_lt(regexpr("Event", html), regexpr("Tissue", html))
  expect_match(html, "https://geome-db.org/record/ark:/1/E", fixed = TRUE)
  expect_match(html, "Peru", fixed = TRUE)
})
```

- [ ] **Step 2: Run, verify fail**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-geome-app.R")'`
Expected: FAIL, `could not find function "geome_record_view"`.

- [ ] **Step 3: Implement in `R/app_geome.R`**

```r
geome_record_view <- function(recs) {
  if (!nrow(recs)) return(p(class = "mp-muted", "No GEOME data stored for this sample yet."))
  lv <- unique(recs[order(-recs$depth), c("level", "depth", "bcid")])
  tagList(lapply(seq_len(nrow(lv)), function(i) {
    r <- recs[recs$depth == lv$depth[i], ]
    tags$details(
      open = NA, class = "mp-geome-level",
      tags$summary(
        strong(lv$level[i]),
        if (!is.na(lv$bcid[i])) tagList(" ", tags$a(
          href = paste0("https://geome-db.org/record/", lv$bcid[i]),
          target = "_blank", rel = "noopener", lv$bcid[i]))
      ),
      tags$table(class = "table table-sm",
        tags$tbody(lapply(seq_len(nrow(r)), function(j) {
          tags$tr(tags$th(r$field[j]), tags$td(r$value[j]))
        }))
      )
    )
  }))
}

geome_viewer_server <- function(id, open, on_change = function() NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    con <- session$userData$con
    rv <- reactiveValues(id = NULL, ver = 0L)
    bump <- function() { rv$ver <- rv$ver + 1L; on_change() }

    samples <- function() {
      DBI::dbGetQuery(con, "SELECT s.ID, s.GEOME_BCID, g.status, g.message, g.fetched_at
                            FROM samples s LEFT JOIN geome_status g ON s.ID = g.ID ORDER BY s.ID")
    }

    observeEvent(open(), {
      rv$id <- open()
      s <- samples()
      lab <- paste0(s$ID, ifelse(is.na(s$status), "", ifelse(s$status == "failed", " (failed)", "")))
      modalDialog(
        title = mp_modal_title("GEOME metadata", "Records fetched from geome-db.org"),
        size = "xl", easyClose = TRUE,
        fluidRow(
          column(3,
            selectInput(ns("sample"), "Sample", choices = stats::setNames(s$ID, lab),
                        selected = rv$id, width = "100%", selectize = FALSE, size = 15),
            uiOutput(ns("failed"))
          ),
          column(9, uiOutput(ns("detail")))
        ),
        footer = mp_footer(
          extra = actionButton(ns("refresh_all"), "Refresh all",
                               title = "Fetch every sample with a BCID again"),
          dismiss = "Close"
        )
      ) |> showModal()
    })

    observeEvent(input$sample, rv$id <- input$sample, ignoreInit = TRUE)

    output$failed <- renderUI({
      rv$ver
      s <- samples()
      bad <- s$ID[!is.na(s$status) & s$status == "failed"]
      if (!length(bad)) return(NULL)
      div(class = "mp-fg-warning", icon("triangle-exclamation"), " Failed: ",
          paste(bad, collapse = ", "))
    })

    output$detail <- renderUI({
      rv$ver
      req(rv$id)
      s <- samples()
      s <- s[s$ID == rv$id, ]
      recs <- DBI::dbGetQuery(con, "SELECT level, depth, bcid, field, value FROM geome_records WHERE ID = ?",
                              params = list(rv$id))
      tagList(
        div(class = "mp-geome-bcid",
          textInput(ns("bcid"), "GEOME BCID", value = s$GEOME_BCID %|NA|% "",
                    placeholder = "ark:/21547/...", width = "420px"),
          actionButton(ns("fetch"), "Fetch", icon = icon("arrows-rotate"))
        ),
        if (!is.na(s$status)) p(class = if (s$status == "failed") "mp-fg-warning" else "mp-muted",
          if (s$status == "failed") paste("Last fetch failed:", s$message) else "Fetched",
          " ", format(as.POSIXct(s$fetched_at, origin = "1970-01-01"), "%Y-%m-%d %H:%M")),
        if (is.na(s$GEOME_BCID) && !nrow(recs)) p(class = "mp-muted",
          "This sample has no GEOME BCID. Paste one above and click Fetch, or add a GEOME_BCID ",
          "column to your mapping file (see the GEOME Metadata article)."),
        geome_record_view(recs)
      )
    })

    observeEvent(input$fetch, {
      req(rv$id)
      val <- .geome_set_bcid(con, rv$id, input$bcid)
      if (!is.na(val)) {
        withProgress(message = "Fetching from GEOME", {
          res <- suppressWarnings(.geome_fetch_into(con, rv$id, val))
        })
        if (res$status == "failed") showNotification(res$message, type = "warning")
      }
      bump()
    })

    observeEvent(input$refresh_all, {
      s <- samples()
      s <- s[!is.na(s$GEOME_BCID), ]
      if (!nrow(s)) return(showNotification("No samples have a GEOME BCID", type = "message"))
      cache <- new.env()
      withProgress(message = "Fetching from GEOME", value = 0, {
        for (i in seq_len(nrow(s))) {
          suppressWarnings(.geome_fetch_into(con, s$ID[i], s$GEOME_BCID[i], cache))
          incProgress(1 / nrow(s), detail = s$ID[i])
        }
      })
      bump()
    })
  })
}
```

Check `%|NA|%` exists in the package (it is used in `R/export.R`); `mp_modal_title`, `mp_footer` are in `R/app_ui_helpers.R`.

- [ ] **Step 4: Wire into each panel server**

Inside `assemble_server`, `assemble_server_userAsmb`, `annotate_server`, `export_server` (inside their `moduleServer` body), add one line, using the refresh trigger each panel already uses to reload its table (look for the `trigger("...")` / `gargoyle::watch("...")` that re-fetches the table in that file; Export uses `trigger("update_export_table")`):

```r
    geome_viewer_server("geome", open = reactive(input$geome_open),
                        on_change = function() trigger("<that panel's table refresh flag>"))
```

- [ ] **Step 5: Run tests, verify pass**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-geome-app.R")'`
Expected: PASS.

- [ ] **Step 6: Manual smoke check**

Create a scratch project (see Task 10, Step 1), run `MitoPilot::MitoPilot("<dir>")`, click a GEOME icon in Assemble: the modal opens on that sample, levels show root first, Fetch on a blank sample with `ark:/21547/CYB2REEDY` turns its icon into the globe after close. Check that clicking the icon does not also toggle row selection.

- [ ] **Step 7: Commit**

```bash
git add R/app_geome.R R/app_assemble.R R/app_assemble_userAsmb.R R/app_annotate.R R/app_export.R tests/testthat/test-geome-app.R
git commit -m "GEOME viewer modal"
```

---

### Task 9: Export field picker

Deviation from spec, flagged for the user: Shiny shows one modal at a time and the header template editor lives inside the Export modal, so the picker gets its own **"GEOME Fields"** toolbar button in the Export toolbar instead of sitting next to the template controls. The spec's optional "insert into template" helper is dropped (YAGNI); the vignette shows the syntax.

**Files:**
- Modify: `R/app_ui.R` (Export `conditionalPanel` toolbar ~L90-111), `R/app_ui_userAsmb.R` (same toolbar)
- Modify: `R/app_server.R` (~L232) and `R/app_server_userAsmb.R` (~L203): forward the button
- Modify: `R/app_export.R` (`EXPORT_COL_GROUPS` ~L3-10, table render ~L214-266, new picker observer)
- Modify: `R/app_geome.R` (picker modal)
- Test: `tests/testthat/test-geome-app.R` (append)

**Interfaces:**
- Consumes: `geome_field_summary(con)`, `geome_export_cols`, `.geome_key_col`.
- Produces: `.geome_save_fields(con, keys)` (replace-all write), a `GEOME` column group in the Export table.

- [ ] **Step 1: Write failing test (append)**

```r
test_that(".geome_save_fields replaces the selection", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbWriteTable(con, "samples", data.frame(ID = "s1", Taxon = "x"))
  .geome_ensure_tables(con)
  .geome_save_fields(con, c("combo:lat_lon", "raw:Event:country"))
  .geome_save_fields(con, "combo:lat_lon")
  expect_equal(DBI::dbGetQuery(con, "SELECT key FROM geome_export_fields")$key, "combo:lat_lon")
  .geome_save_fields(con, character())
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM geome_export_fields")$n, 0L)
})
```

- [ ] **Step 2: Run, verify fail**

Expected: FAIL, `could not find function ".geome_save_fields"`.

- [ ] **Step 3: Implement save helper and picker in `R/app_geome.R`**

```r
.geome_save_fields <- function(con, keys) {
  DBI::dbWithTransaction(con, {
    DBI::dbExecute(con, "DELETE FROM geome_export_fields")
    if (length(keys)) DBI::dbAppendTable(con, "geome_export_fields", data.frame(key = unique(keys)))
  })
  invisible(keys)
}

geome_fields_modal <- function(ns, s) {
  combos <- s[s$kind == "combo", ]
  raw <- s[s$kind == "raw", ]
  modalDialog(
    title = mp_modal_title("GEOME fields for export",
                           "Ticked fields become columns you can use in header templates"),
    size = "xl", easyClose = TRUE,
    h5("GenBank-ready combinations"),
    checkboxGroupInput(
      ns("geome_combos"), NULL, width = "100%",
      choiceValues = combos$key, selected = combos$key[combos$selected],
      choiceNames = lapply(seq_len(nrow(combos)), function(i) tagList(
        code(paste0("{", combos$col[i], "}")), " from ", combos$field[i], ": ",
        if (is.na(combos$example[i])) em("no samples") else
          tagList(tags$samp(combos$example[i]), sprintf(" (%d samples)", combos$n_samples[i]))
      ))
    ),
    h5("All GEOME fields"),
    reactable::reactableOutput(ns("geome_raw")),
    footer = mp_footer(primary = actionButton(ns("geome_fields_save"), "Save"), dismiss = "Cancel")
  )
}
```

- [ ] **Step 4: Toolbar button and forwarding**

In both `R/app_ui.R` and `R/app_ui_userAsmb.R` Export toolbars, before the `"export"` button:

```r
              mp_toolbar_button(
                "geome_fields", "GEOME Fields",
                title = "Choose which GEOME fields are available at export"
              ),
```

In both `app_server*.R`, next to `observeEvent(input$group, ...)`:

```r
  observeEvent(input$geome_fields, {
    trigger("geome_fields")
  })
```

Register the flag the same way `"group"` is registered (find `init("group")` in `R/app_export.R` ~L407 and add `init("geome_fields")` beside it).

- [ ] **Step 5: Picker observer and GEOME column group in `R/app_export.R`**

Add to `EXPORT_COL_GROUPS` after `Metadata`:

```r
  # filled at render time from geome_export_fields
  GEOME = character(0)
```

Inside `export_server`, near the `metadata_col_defs` helper:

```r
    geome_fields_ver <- reactiveVal(0L)
    geome_col_defs <- function() {
      keys <- tryCatch(
        DBI::dbGetQuery(session$userData$con, "SELECT key FROM geome_export_fields")$key,
        error = function(e) character(0))
      cols <- vapply(keys, .geome_key_col, character(1), USE.NAMES = FALSE)
      stats::setNames(lapply(cols, function(col) {
        colDef(show = TRUE, name = col, header = rt_header(col, "From GEOME"),
               class = "mp-grp-GEOME", headerClass = "mp-grp-GEOME",
               html = TRUE, cell = rt_longtext(), minWidth = 120)
      }), cols)
    }

    on("geome_fields", {
      s <- geome_field_summary(session$userData$con)
      showModal(geome_fields_modal(ns, s))
      raw <- s[s$kind == "raw", ]
      output$geome_raw <- reactable::renderReactable(reactable::reactable(
        raw[, c("level", "field", "n_samples", "example", "col")],
        selection = "multiple", onClick = "select", compact = TRUE, searchable = TRUE,
        defaultSelected = which(raw$selected), defaultPageSize = 15,
        columns = list(
          level = colDef(name = "Level"), field = colDef(name = "Field"),
          n_samples = colDef(name = "Samples", width = 80),
          example = colDef(name = "Example", cell = rt_longtext(), html = TRUE),
          col = colDef(name = "Template token", cell = function(v) paste0("{", v, "}"))
        )
      ))
    })

    observeEvent(input$geome_fields_save, {
      s <- geome_field_summary(session$userData$con)
      raw <- s[s$kind == "raw", ]
      picked <- raw$key[reactable::getReactableState("geome_raw", "selected") %||% integer(0)]
      .geome_save_fields(session$userData$con, c(input$geome_combos, picked))
      removeModal()
      geome_fields_ver(geome_fields_ver() + 1L)
      rv$data <- fetch_export_data()
    })
```

In `output$table <- reactable::renderReactable({ ... })`, add `geome_fields_ver()` as the first line (so a save re-renders with new columns), build `geome_cols <- geome_col_defs()` and change `columns = c(declared_cols, metadata_cols)` to `columns = c(declared_cols, metadata_cols, geome_cols)`. Confirm the `col_groups` picker hides the group through the existing `.mp-grp-<name>` CSS rule (it iterates `names(EXPORT_COL_GROUPS)`, so `GEOME` is picked up automatically).

Match whichever pattern `R/app_export.R` already uses for `on(...)` vs `observeEvent(watch(...))`; the code above assumes `on()` as used at ~L408.

- [ ] **Step 6: Run tests, verify pass**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-geome-app.R")'`
Expected: PASS.

- [ ] **Step 7: Manual check**

In the scratch project (Task 10), lock one sample through Export or use an existing test project with locked annotations: click "GEOME Fields", tick `geo_loc_name` and one raw field, Save. The Export table gains a GEOME column group; the Columns picker can hide it. In Export Data, a header template containing `[geo_loc_name={geome_geo_loc_name}]` validates and previews.

- [ ] **Step 8: Commit**

```bash
git add R/app_ui.R R/app_ui_userAsmb.R R/app_server.R R/app_server_userAsmb.R R/app_export.R R/app_geome.R tests/testthat/test-geome-app.R
git commit -m "GEOME export field picker"
```

---

### Task 10: Vignette, docs, NEWS, and end-to-end check

**Files:**
- Create: `vignettes/GEOME-Metadata.Rmd`
- Create: `vignettes/figures/geome_*.png` (3 screenshots)
- Modify: `_pkgdown.yml` (Usage group, after `custom_dbs`)
- Modify: `vignettes/Your-Own-Project.Rmd` (mapping file section), `vignettes/Test-Project-Export.Rmd` (header templates section)
- Modify: `NEWS.md`

- [ ] **Step 1: Build a scratch project with real BCIDs**

Find a few more public BCIDs (ideally with coordinates and dates) via:
```bash
curl -s --compressed "https://api.geome-db.org/records/Tissue/json?q=_projects_:75+decimalLatitude:*&limit=3" | head -c 2000
```
Copy the shipped test project mapping (see `R/new_test_project*.R` or the Test-Project vignettes for how it is created), add a `GEOME_BCID` column with those BCIDs plus one deliberately bad value, create the project under `~/MitoPilot_scratch/geome_demo`, and confirm `fetch_geome("~/MitoPilot_scratch/geome_demo")` prints one warning naming the bad sample.

- [ ] **Step 2: Write `vignettes/GEOME-Metadata.Rmd`**

Header (match other vignettes):

```
---
title: "GEOME Metadata"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{GEOME Metadata}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---
```

Sections, each with real prose and `eval = FALSE` code chunks:

1. **What GEOME is and what MitoPilot pulls:** GEOME stores specimen, tissue, and collecting-event metadata. Given a BCID, MitoPilot fetches that record, walks up its parents (for example Tissue, Sample, Event) and adds expedition and project details. Public records only.
2. **Finding a BCID:** the ARK on a GEOME record page (`ark:/21547/...`); n2t.net and geome-db.org URLs are accepted and trimmed.
3. **At project setup:**
   ```r
   new_project(path = "my_project", mapping_fn = "mapping.csv",
               mapping_geome = "Tissue_BCID", data_path = "reads/")
   ```
   Explain `mapping_geome` (column is stored as `GEOME_BCID`), blank cells are fine, `fetch_geome = FALSE` for offline setup.
4. **Later:** `add_samples(..., mapping_geome = )`, `update_sample_metadata()` (changed BCIDs refetch, blank clears), and
   ```r
   fetch_geome("my_project")                       # refresh all
   fetch_geome("my_project", ids = "S01")          # refresh one
   fetch_geome("my_project", ids = c("S01", "S02"),
               bcids = c("ark:/21547/CYB2REEDY", "ark:/21547/CYA2Reedy01"))
   ```
5. **Viewing in the app:** the GEOME column after Taxon (globe = fetched, warning = failed with reason on hover, plus = no BCID), the viewer (levels root first, links to geome-db.org, BCID box + Fetch, Refresh all, failed list). Screenshot `figures/geome_viewer.png` and `figures/geome_column.png`.
6. **Using GEOME fields at export:** "GEOME Fields" button; nothing exported unless ticked; table of combinations (copy from the spec, with the final field names confirmed in Task 3); raw field naming `geome_<Level>_<field>`; worked template:
   ```
   {seqid} [organism={Taxon}] [mgcode={genetic_code}] [location=mitochondrion] [lat_lon={geome_lat_lon}] [collection_date={geome_collection_date}] [geo_loc_name={geome_geo_loc_name}] [specimen_voucher={geome_specimen_voucher}] {Taxon} mitochondrion, {completeness}
   ```
   Warn that an empty value leaves `[lat_lon=]` in the header, so only add modifiers every sample in the group has. Screenshot `figures/geome_fields.png`.
7. **Limits and troubleshooting:** public records only; "BCID not found in GEOME" (GEOME answers unknown IDs with a server error); "record is private or needs a GEOME login"; "could not reach GEOME"; a failed refresh keeps the previous data; GEOME calls need internet from the machine running R/the app, not the cluster.

- [ ] **Step 3: Screenshots**

Use the docs screenshot harness under `dev/docs_shots` (see its README / step files) against `~/MitoPilot_scratch/geome_demo`; save three PNGs to `vignettes/figures/` with the names above. If the harness cannot drive modals, take them manually at the same window size as existing figures.

- [ ] **Step 4: pkgdown, cross-links, NEWS**

`_pkgdown.yml` Usage contents: add `- 'GEOME-Metadata'` after `- 'custom_dbs'`.
`Your-Own-Project.Rmd`, mapping file section: one sentence, "To link samples to GEOME records, add a column of GEOME BCIDs; see [GEOME Metadata](GEOME-Metadata.html)."
`Test-Project-Export.Rmd`, header template section: one sentence pointing to GEOME Metadata for `{geome_...}` tokens.
`NEWS.md`, top development section: "GEOME integration: link samples to GEOME BCIDs (`mapping_geome`, `fetch_geome()`), browse the full record in the app, and choose GEOME fields, including GenBank-ready `lat_lon`, `collection_date`, `geo_loc_name`, and `specimen_voucher`, for export header templates."

- [ ] **Step 5: Build checks**

```bash
Rscript -e 'devtools::document(); devtools::build_vignettes()' 2>&1 | tail -20
Rscript -e 'devtools::test()' 2>&1 | tail -30
Rscript -e 'pkgdown::build_article("GEOME-Metadata")' 2>&1 | tail -5
```
Expected: vignette builds, full test suite has no new failures (compare against `main` if unsure), article renders.

- [ ] **Step 6: End-to-end export check**

In the scratch project: tick `geo_loc_name`, `lat_lon`, `collection_date`; export one locked sample with the template from Step 2; open the FASTA and confirm the defline carries the expected `[lat_lon=...]` etc.

- [ ] **Step 7: Commit**

```bash
git add vignettes/GEOME-Metadata.Rmd vignettes/figures/geome_*.png _pkgdown.yml vignettes/Your-Own-Project.Rmd vignettes/Test-Project-Export.Rmd NEWS.md man/
git commit -m "GEOME Metadata vignette and docs"
```

---

### Task 11: GEOME column toggle in each table's Columns picker (runs after Task 9, before Task 10)

User decision (2026-09-24): the GEOME icon column must be hideable through each table's existing "Columns:" picker, and default to OFF for projects where no sample has a BCID. Supersedes "always shown" for the icon column.

**Files:**
- Modify: `R/app_assemble.R` (`ASSEMBLE_COL_GROUPS` ~L3; `col_groups_rv` ~L134; `geome` colDef)
- Modify: `R/app_assemble_userAsmb.R` (`ASSEMBLE_COL_GROUPS_USERASMB` ~L5; `col_groups_rv` ~L118; `geome` colDef)
- Modify: `R/app_annotate.R` (`ANNOTATE_COL_GROUPS` ~L3; `col_groups_rv` ~L183; `geome` colDef)
- Modify: `R/app_export.R` (`EXPORT_COL_GROUPS`; `col_groups_rv`; `geome` colDef)
- Modify: `R/app_geome.R` (`geome_col_def()` gains a `class` arg; new `.geome_project_has_bcids(con)`)
- Test: `tests/testthat/test-geome-app.R` (append)

**Interfaces:**
- Consumes: `geome_col_def(inputId, sticky = NULL)` (Task 7), the Export `GEOME` column group added in Task 9, `.geome_ensure_tables(con)`.
- Produces:
  - `.geome_project_has_bcids(con)`: TRUE when any `samples.GEOME_BCID` is non-NA and non-empty (ensures tables first, so it works on pre-branch DBs).
  - `.geome_default_groups(groups, con)`: `groups` minus `"GEOME"` when `.geome_project_has_bcids(con)` is FALSE, else `groups` unchanged.
  - `geome_col_def(inputId, sticky = NULL, class = NULL)`: `class` applied to both `class` and `headerClass`.

- [ ] **Step 1: Write failing tests (append to `test-geome-app.R`)**

```r
test_that(".geome_project_has_bcids and .geome_default_groups follow the samples table", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbWriteTable(con, "samples", data.frame(ID = c("a", "b"), Taxon = "x"))
  grp <- c("Options", "GEOME", "Metadata")
  expect_false(.geome_project_has_bcids(con))
  expect_equal(.geome_default_groups(grp, con), c("Options", "Metadata"))
  DBI::dbExecute(con, "UPDATE samples SET GEOME_BCID = '' WHERE ID = 'a'")
  expect_false(.geome_project_has_bcids(con))
  DBI::dbExecute(con, "UPDATE samples SET GEOME_BCID = 'ark:/1/A' WHERE ID = 'b'")
  expect_true(.geome_project_has_bcids(con))
  expect_equal(.geome_default_groups(grp, con), grp)
})

test_that("geome_col_def applies the group class to cell and header", {
  cd <- geome_col_def("x-geome_open", class = "mp-grp-GEOME")
  expect_equal(cd$class, "mp-grp-GEOME")
  expect_equal(cd$headerClass, "mp-grp-GEOME")
})

test_that("every table's column groups include GEOME holding the geome column", {
  for (g in list(ASSEMBLE_COL_GROUPS, ASSEMBLE_COL_GROUPS_USERASMB, ANNOTATE_COL_GROUPS)) {
    expect_true("geome" %in% g$GEOME)
  }
  expect_true("GEOME" %in% names(EXPORT_COL_GROUPS))
})
```

- [ ] **Step 2: Run, verify fail**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-geome-app.R")'`
Expected: FAIL, `could not find function ".geome_project_has_bcids"`.

- [ ] **Step 3: Helpers in `R/app_geome.R`**

```r
.geome_project_has_bcids <- function(con) {
  .geome_ensure_tables(con)
  DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM samples
                        WHERE GEOME_BCID IS NOT NULL AND TRIM(GEOME_BCID) != ''")$n > 0
}

.geome_default_groups <- function(groups, con) {
  if (.geome_project_has_bcids(con)) groups else setdiff(groups, "GEOME")
}
```

Add `class = NULL` to `geome_col_def()` and pass `class = class, headerClass = class` to `reactable::colDef()`.

- [ ] **Step 4: Add the group and default in each table**

In `ASSEMBLE_COL_GROUPS`, `ASSEMBLE_COL_GROUPS_USERASMB`, and `ANNOTATE_COL_GROUPS` add `GEOME = c("geome")` as the last entry. In `R/app_export.R`, put `"geome"` into the `GEOME` group Task 9 created (so it becomes `GEOME = c("geome")` with the ticked `geome_*` columns still added at render time); check that Task 9's `geome_col_defs()` still tags the field columns `mp-grp-GEOME`.

In each module server, replace the `col_groups_rv <- reactiveVal(names(<GROUPS>))` initial value with:

```r
    col_groups_rv <- reactiveVal(.geome_default_groups(names(<GROUPS>), session$userData$con))
```

and right after it:

```r
    if (!"GEOME" %in% col_groups_rv()) {
      shinyWidgets::updatePickerInput(session, "col_groups", selected = col_groups_rv())
    }
```

(The picker UI is built without DB access and starts with every group selected; this syncs it to the server-side default. The existing `observeEvent(input$col_groups, ...)` keeps working because the user can still turn GEOME back on.)

Pass the group class to the column: `geome = geome_col_def(ns("geome_open"), sticky = ..., class = "mp-grp-GEOME")` in all four tables (keep the Task 7 sticky arguments unchanged).

- [ ] **Step 5: Run tests, verify pass**

```bash
Rscript -e 'devtools::load_all(); for (f in c("test-geome-app.R","test-userasmb-app-units.R","test-ui-reactable-helpers.R")) testthat::test_file(file.path("tests/testthat", f))'
```
Expected: all PASS.

- [ ] **Step 6: Verify the default in a running module**

Using `shiny::testServer()` on `assemble_server` (or the lightest module that accepts a `session$userData$con`), confirm `col_groups_rv()` excludes `"GEOME"` for a DB with no BCIDs and includes it once a BCID exists. If `testServer` setup is impractical for these modules, say so in the report and rely on the Step 1 tests; the real-app check in Task 10 covers the rendered picker.

- [ ] **Step 7: Commit**

```bash
git add R/app_geome.R R/app_assemble.R R/app_assemble_userAsmb.R R/app_annotate.R R/app_export.R tests/testthat/test-geome-app.R
git commit -m "GEOME column toggle, off by default without BCIDs"
```

Task 10 addendum: the vignette's app section says the GEOME column can be turned on or off with each table's Columns picker and starts off for projects without BCIDs; the real-app check confirms both states.
