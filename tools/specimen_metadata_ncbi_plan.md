# NCBI BioSample Metadata Source Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add NCBI (BioSample, reached directly or through an SRA accession, plus linked BioProjects) as a third sample-metadata source next to GEOME and GBIF.

**Architecture:** One new `META_SOURCES$NCBI` registry entry reuses the shared `meta_*` storage, fetch, refresh, export-field, and viewer code. New files `R/ncbi_api.R` (IDs + E-utilities client + fetch chain), `R/ncbi_db.R` (`fetch_biosample()`), `R/ncbi_export.R` (GenBank-ready combos + Compare values). Hard-coded GEOME/GBIF pairs in reconcile and app code are generalized to loop over `names(META_SOURCES)`.

**Tech Stack:** R, httr2, xml2 (new Imports), jsonlite, RSQLite, shiny, reactable, testthat 3.

**Spec:** `tools/specimen_metadata_ncbi_spec.md`

## Global Constraints

- Source name `NCBI`; samples column `BioSample`; export key prefix `ncbi:`; token prefix `ncbi_`.
- Mapping args `mapping_biosample = "BioSample"`, `fetch_biosample = TRUE`; function `fetch_biosample(path = ".", ids = NULL, biosamples = NULL, from_id = FALSE)`.
- Valid IDs: `^SAM(N|EA|D)[0-9]+$`, `^[0-9]+$`, `^[SED]R[RXS][0-9]+$` (after trim + uppercase + URL strip).
- Levels/depths: `SRA` 0 (only when resolved from SRA), `BioSample` 1, `BioProject` 2, 3, ...
- Request spacing >= 0.34 s (0.11 s when env `ENTREZ_KEY` is set); `tool=MitoPilot`.
- Missing set (case-insensitive, text before any `:`): `missing`, `not collected`, `not applicable`, `not provided`, `restricted access`, `unknown`, `na`, `n/a`, `none`, `-`.
- R code ASCII only. Minimal comments. Never merge values across sources (flag only).
- Run tests with: `Rscript -e 'devtools::load_all(quiet=TRUE); testthat::test_file("tests/testthat/<file>")'`.
- Commit messages brief, no attribution trailer.

## Review Focus

1. `mapping_biosample` naming the same column as `mapping_id` (including the literal `ID` column): the ID column must survive and BioSample must be filled. Test in Task 3.
2. SRA accession whose run has no linked BioSample, and an SRA accession NCBI does not know: clear failed-fetch message, no crash, prior data kept. Test in Task 2.
3. BioSample attributes holding "missing"-type text (`missing: control sample`, `not collected`): never exported as a value and never raised as a conflict. Test in Tasks 4 and 5.
4. BioSample with two BioProject links, one of which fails to fetch: BioSample and the good project kept. Test in Task 2.
5. Old projects (no `BioSample` column) opened in the app: column added silently, NCBI tab shows the empty message. Test in Task 3 (ensure) and Task 6 (compat flag).

---

### Task 1: IDs, xml2 dependency, and fixtures

**Files:**
- Create: `R/ncbi_api.R`
- Create: `tests/testthat/test-ncbi-api.R`
- Create: `tests/testthat/fixtures/ncbi/*` (captured live)
- Modify: `DESCRIPTION` (Imports: add `xml2`)

**Interfaces:**
- Produces: `ncbi_normalize_id(x)` -> character (normalized or NA); `.ncbi_is_sra(x)` -> logical.

- [ ] **Step 1: Capture fixtures (live, once)**

```bash
cd tests/testthat/fixtures && mkdir -p ncbi && cd ncbi
E=https://eutils.ncbi.nlm.nih.gov/entrez/eutils
g() { curl -s "$E/$1" -o "$2"; sleep 0.4; }
g "efetch.fcgi?db=biosample&id=SAMN63236902&retmode=xml" efetch_biosample_SAMN63236902.xml
g "efetch.fcgi?db=biosample&id=SAMN29555051&retmode=xml" efetch_biosample_SAMN29555051.xml
g "efetch.fcgi?db=biosample&id=SAMN99999999999&retmode=xml" efetch_biosample_SAMN99999999999.xml
g "efetch.fcgi?db=bioproject&id=1422759&retmode=xml" efetch_bioproject_1422759.xml
g "efetch.fcgi?db=bioproject&id=720393&retmode=xml" efetch_bioproject_720393.xml
g "esearch.fcgi?db=sra&term=SRR21844202%5Baccn%5D" esearch_sra_SRR21844202.xml
g "esummary.fcgi?db=sra&id=24760195&retmode=json" esummary_sra_24760195.json
g "esearch.fcgi?db=sra&term=SRR99999999999%5Baccn%5D" esearch_sra_SRR99999999999.xml
ls -la
```
Check `efetch_bioproject_720393.xml` is non-empty; if SAMN29555051 links a different project uid, fetch that uid instead (read `Links/Link@target="bioproject"` text).

- [ ] **Step 2: Write failing test**

```r
test_that("ncbi_normalize_id accepts BioSample, uid, SRA, and NCBI links", {
  x <- c("SAMN29555051", " samn29555051 ", "https://www.ncbi.nlm.nih.gov/biosample/SAMN29555051/",
         "SAMEA1234567", "SAMD00012345", "29555051", "SRR21844202", "err000001",
         "SRX17832658", "SRS14384543", "https://www.ncbi.nlm.nih.gov/sra/SRR21844202",
         "", NA, "PRJNA720393", "SAMX1", "SRR", "GCA_000001.1")
  expect_equal(ncbi_normalize_id(x), c(
    "SAMN29555051", "SAMN29555051", "SAMN29555051", "SAMEA1234567", "SAMD00012345",
    "29555051", "SRR21844202", "ERR000001", "SRX17832658", "SRS14384543", "SRR21844202",
    rep(NA, 6)))
  expect_equal(.ncbi_is_sra(c("SRR1", "SAMN1", "DRS9", NA)), c(TRUE, FALSE, TRUE, FALSE))
})
```

- [ ] **Step 3: Run, expect FAIL** (`could not find function "ncbi_normalize_id"`).

- [ ] **Step 4: Implement in `R/ncbi_api.R`**

```r
NCBI_EUTILS <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils"
.ncbi_env <- new.env()

#' Normalize NCBI BioSample or SRA accessions
#'
#' @param x Vector of BioSample accessions (SAMN, SAMEA, SAMD), BioSample
#'   numbers, SRA accessions (SRR/ERR/DRR runs, SRX experiments, SRS samples),
#'   or ncbi.nlm.nih.gov biosample / sra links.
#' @return Character vector of upper-case IDs, NA where the input is blank or
#'   not a BioSample or SRA ID.
#' @export
ncbi_normalize_id <- function(x) {
  x <- toupper(trimws(.meta_chr(x)))
  x <- sub("^HTTPS?://(WWW\\.)?NCBI\\.NLM\\.NIH\\.GOV/(BIOSAMPLE|SRA)/", "", x)
  x <- sub("/+$", "", x)
  ok <- !is.na(x) & grepl("^(SAM(N|EA|D)[0-9]+|[0-9]+|[SED]R[RXS][0-9]+)$", x)
  ifelse(ok, x, NA_character_)
}

.ncbi_is_sra <- function(x) !is.na(x) & grepl("^[SED]R[RXS][0-9]+$", x)
```
Add `xml2` to DESCRIPTION Imports (alphabetical position, after `waiter`/before `zoo` as the list allows). Run `Rscript -e 'devtools::document()'` so NAMESPACE exports `ncbi_normalize_id`.

- [ ] **Step 5: Run, expect PASS.**

- [ ] **Step 6: Commit** `git add DESCRIPTION NAMESPACE man R/ncbi_api.R tests/testthat/test-ncbi-api.R tests/testthat/fixtures/ncbi && git commit -m "NCBI ID normalizer, xml2 import, fixtures"`

---

### Task 2: E-utilities client and fetch chain

**Files:**
- Modify: `R/ncbi_api.R`
- Create: `tests/testthat/helper-ncbi.R`
- Modify: `tests/testthat/test-ncbi-api.R`

**Interfaces:**
- Consumes: `ncbi_normalize_id`, `.ncbi_is_sra` (Task 1); `.meta_flatten(x, level, depth, ref)`, `.meta_chr`, `%|NA|%` (existing).
- Produces: `.ncbi_get(endpoint, query)` -> response body string; `.ncbi_fetch_chain(ref, cache = new.env())` -> data.frame(level, depth, ref, field, value) (same shape as `.gbif_fetch_chain`).

- [ ] **Step 1: Test helper `tests/testthat/helper-ncbi.R`**

```r
ncbi_calls <- new.env()

ncbi_fixture_get <- function(endpoint, query) {
  key <- query$id %||% sub("\\[accn\\]$", "", query$term)
  f <- testthat::test_path("fixtures", "ncbi", paste0(endpoint, "_", query$db, "_", key,
                           if (identical(query$retmode, "json")) ".json" else ".xml"))
  ncbi_calls[[basename(f)]] <- (ncbi_calls[[basename(f)]] %||% 0L) + 1L
  if (!file.exists(f)) stop("NCBI returned HTTP 500", call. = FALSE)
  paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

ncbi_reset_calls <- function() rm(list = ls(ncbi_calls), envir = ncbi_calls)
```

- [ ] **Step 2: Failing tests (append to `test-ncbi-api.R`)**

```r
test_that("a BioSample accession yields BioSample and BioProject levels", {
  local_mocked_bindings(.ncbi_get = ncbi_fixture_get)
  out <- .ncbi_fetch_chain("SAMN63236902")
  lv <- unique(out[order(out$depth), c("level", "depth", "ref")])
  expect_equal(lv$level, c("BioSample", "BioProject"))
  expect_equal(lv$depth, 1:2)
  expect_equal(lv$ref, c("SAMN63236902", "PRJNA1422759"))
  bs <- out[out$level == "BioSample", ]
  v <- function(f) bs$value[bs$field == f]
  expect_equal(v("organism"), "Zoarces americanus")
  expect_equal(v("taxonomy_id"), "8199")
  expect_equal(v("collection_date"), "2024-06-27")
  expect_equal(v("geo_loc_name"), "Canada: Scotian Shelf, Nova Scotia, Atlantic Ocean")
  expect_equal(v("identified_by"), bs$value[bs$field == "identified_by"][1])
  expect_equal(v("sex"), "missing")
  expect_equal(v("ToLID"), "fZoaAme1")
  expect_equal(v("sample_name"), "OceanPout01-tissueD")
  expect_false(any(duplicated(bs$field)))
  bp <- out[out$level == "BioProject", ]
  expect_equal(bp$value[bp$field == "umbrella"], "PRJNA1422710")
  expect_match(bp$value[bp$field == "title"], "ocean pout", fixed = TRUE)
  expect_type(out$depth, "integer")
})

test_that("an SRA run resolves through its BioSample", {
  local_mocked_bindings(.ncbi_get = ncbi_fixture_get)
  out <- .ncbi_fetch_chain("SRR21844202")
  expect_equal(unique(out$level[order(out$depth)]), c("SRA", "BioSample", "BioProject"))
  sra <- out[out$level == "SRA", ]
  expect_equal(unique(sra$ref), "SRR21844202")
  expect_equal(sra$value[sra$field == "biosample"], "SAMN29555051")
  expect_equal(sra$value[sra$field == "library_layout"], "PAIRED")
  expect_equal(sra$value[sra$field == "runs"], "SRR21844202")
  expect_equal(unique(out$ref[out$level == "BioSample"]), "SAMN29555051")
})

test_that("unknown IDs stop with a clear message", {
  local_mocked_bindings(.ncbi_get = ncbi_fixture_get)
  expect_error(.ncbi_fetch_chain("SAMN99999999999"), "BioSample SAMN99999999999 not found")
  expect_error(.ncbi_fetch_chain("SRR99999999999"), "SRA accession SRR99999999999 not found")
})

test_that("an SRA record without a BioSample stops", {
  local_mocked_bindings(.ncbi_get = function(endpoint, query) {
    if (endpoint == "esummary") {
      return('{"result":{"uids":["1"],"1":{"expxml":"<Summary><Title>x</Title></Summary>","runs":""}}}')
    }
    "<eSearchResult><Count>1</Count><IdList><Id>1</Id></IdList></eSearchResult>"
  })
  expect_error(.ncbi_fetch_chain("SRR1"), "SRA accession SRR1 has no linked BioSample")
})

test_that("a failing BioProject keeps the BioSample and the other projects", {
  local_mocked_bindings(.ncbi_get = function(endpoint, query) {
    if (query$db == "biosample") {
      x <- ncbi_fixture_get(endpoint, query)
      return(sub("</Links>", '<Link type="entrez" target="bioproject" label="PRJNA1">1</Link></Links>', x,
                 fixed = TRUE))
    }
    ncbi_fixture_get(endpoint, query)
  })
  out <- .ncbi_fetch_chain("SAMN63236902")
  expect_equal(sum(out$level == "BioSample" & out$field == "accession"), 1L)
  expect_equal(out$value[out$level == "BioProject" & out$field == "accession"], "PRJNA1422759")
})

test_that("BioProjects are fetched once per cache", {
  local_mocked_bindings(.ncbi_get = ncbi_fixture_get)
  ncbi_reset_calls()
  cache <- new.env()
  .ncbi_fetch_chain("SAMN63236902", cache)
  .ncbi_fetch_chain("SAMN63236902", cache)
  expect_equal(ncbi_calls[["efetch_bioproject_1422759.xml"]], 1L)
})

test_that(".ncbi_get sends tool and api_key and spaces requests", {
  seen <- list()
  local_mocked_bindings(req_perform = function(req, ...) {
    seen[[length(seen) + 1L]] <<- list(url = req$url, t = Sys.time())
    httr2::response(status_code = 200, body = charToRaw("<x/>"))
  }, .package = "httr2")
  withr::local_envvar(ENTREZ_KEY = "abc")
  .ncbi_get("efetch", list(db = "biosample", id = "1"))
  .ncbi_get("efetch", list(db = "biosample", id = "2"))
  expect_match(seen[[1]]$url, "tool=MitoPilot", fixed = TRUE)
  expect_match(seen[[1]]$url, "api_key=abc", fixed = TRUE)
  expect_gte(as.numeric(difftime(seen[[2]]$t, seen[[1]]$t, units = "secs")), 0.1)
})
```

- [ ] **Step 3: Run, expect FAIL.**

- [ ] **Step 4: Implement (append to `R/ncbi_api.R`)**

```r
.ncbi_get <- function(endpoint, query) {
  key <- Sys.getenv("ENTREZ_KEY")
  gap <- if (nzchar(key)) 0.11 else 0.34
  wait <- gap - (as.numeric(Sys.time()) - (.ncbi_env$last %||% 0))
  if (wait > 0) Sys.sleep(wait)
  q <- c(query, list(tool = "MitoPilot"), if (nzchar(key)) list(api_key = key))
  req <- httr2::request(paste0(NCBI_EUTILS, "/", endpoint, ".fcgi")) |>
    httr2::req_url_query(!!!q) |>
    httr2::req_user_agent("MitoPilot (https://github.com/Smithsonian/MitoPilot)") |>
    httr2::req_timeout(30) |>
    httr2::req_retry(max_tries = 3,
                     is_transient = function(r) httr2::resp_status(r) %in% c(429, 500, 502, 503, 504)) |>
    httr2::req_error(is_error = function(r) FALSE)
  resp <- tryCatch(httr2::req_perform(req), error = function(e) {
    stop("could not reach NCBI (", conditionMessage(e), ")", call. = FALSE)
  })
  .ncbi_env$last <- as.numeric(Sys.time())
  st <- httr2::resp_status(resp)
  if (st >= 400) stop("NCBI returned HTTP ", st, call. = FALSE)
  httr2::resp_body_string(resp)
}

.ncbi_txt <- function(node, xpath) {
  n <- xml2::xml_find_all(node, xpath)
  v <- trimws(xml2::xml_text(n))
  v <- v[nzchar(v)]
  if (!length(v)) NA_character_ else paste(unique(v), collapse = ", ")
}

.ncbi_att <- function(node, xpath, attr) {
  n <- xml2::xml_find_first(node, xpath)
  if (inherits(n, "xml_missing")) NA_character_ else xml2::xml_attr(n, attr)
}

.ncbi_enum <- function(x) sub("^e(?=[A-Z])", "", x, perl = TRUE)

.ncbi_sra <- function(acc) {
  s <- xml2::read_xml(.ncbi_get("esearch", list(db = "sra", term = paste0(acc, "[accn]"))))
  uid <- .ncbi_txt(s, "//IdList/Id")
  if (is.na(uid)) stop("SRA accession ", acc, " not found", call. = FALSE)
  uid <- strsplit(uid, ", ", fixed = TRUE)[[1]][1]
  j <- jsonlite::fromJSON(.ncbi_get("esummary", list(db = "sra", id = uid, retmode = "json")),
                          simplifyVector = FALSE)
  r <- j$result[[uid]]
  x <- xml2::read_xml(paste0("<r>", r$expxml %||% "", "</r>"))
  runs <- xml2::read_xml(paste0("<r>", r$runs %||% "", "</r>"))
  lay <- xml2::xml_find_first(x, ".//LIBRARY_LAYOUT/*")
  f <- list(
    accession = acc,
    experiment = .ncbi_att(x, ".//Experiment", "acc"),
    study = .ncbi_att(x, ".//Study", "acc"),
    study_title = .ncbi_att(x, ".//Study", "name"),
    title = .ncbi_txt(x, ".//Summary/Title"),
    runs = .ncbi_txt(runs, ".//Run/@acc"),
    platform = .ncbi_txt(x, ".//Summary/Platform"),
    instrument = .ncbi_att(x, ".//Summary/Platform", "instrument_model"),
    library_strategy = .ncbi_txt(x, ".//LIBRARY_STRATEGY"),
    library_source = .ncbi_txt(x, ".//LIBRARY_SOURCE"),
    library_selection = .ncbi_txt(x, ".//LIBRARY_SELECTION"),
    library_layout = if (inherits(lay, "xml_missing")) NA_character_ else xml2::xml_name(lay),
    center = .ncbi_att(x, ".//Submitter", "center_name"),
    biosample = .ncbi_txt(x, ".//Biosample"),
    bioproject = .ncbi_txt(x, ".//Bioproject")
  )
  if (is.na(f$biosample)) stop("SRA accession ", acc, " has no linked BioSample", call. = FALSE)
  f
}

.ncbi_biosample <- function(id) {
  x <- xml2::read_xml(.ncbi_get("efetch", list(db = "biosample", id = id, retmode = "xml")))
  b <- xml2::xml_find_first(x, "/BioSampleSet/BioSample")
  if (inherits(b, "xml_missing")) stop("BioSample ", id, " not found", call. = FALSE)
  acc <- xml2::xml_attr(b, "accession")
  f <- list(
    accession = acc,
    title = .ncbi_txt(b, "Description/Title"),
    organism = .ncbi_txt(b, "Description/Organism/OrganismName") %|NA|%
      .ncbi_att(b, "Description/Organism", "taxonomy_name"),
    taxonomy_id = .ncbi_att(b, "Description/Organism", "taxonomy_id"),
    sample_name = .ncbi_txt(b, "Ids/Id[@db_label='Sample name']"),
    sra_sample = .ncbi_txt(b, "Ids/Id[@db='SRA']"),
    owner = .ncbi_txt(b, "Owner/Name"),
    package = .ncbi_txt(b, "Package"),
    publication_date = xml2::xml_attr(b, "publication_date"),
    last_update = xml2::xml_attr(b, "last_update")
  )
  at <- xml2::xml_find_all(b, "Attributes/Attribute")
  nm <- xml2::xml_attr(at, "harmonized_name")
  nm <- ifelse(is.na(nm), xml2::xml_attr(at, "attribute_name"), nm)
  val <- trimws(xml2::xml_text(at))
  keep <- !duplicated(nm) & !nm %in% names(f)
  f <- c(f, stats::setNames(as.list(val[keep]), nm[keep]))
  ln <- xml2::xml_find_all(b, "Links/Link[@target='bioproject']")
  list(fields = f, projects = data.frame(uid = trimws(xml2::xml_text(ln)),
                                         acc = xml2::xml_attr(ln, "label")))
}

.ncbi_bioproject <- function(uid) {
  x <- xml2::read_xml(.ncbi_get("efetch", list(db = "bioproject", id = uid, retmode = "xml")))
  p <- xml2::xml_find_first(x, "//DocumentSummary")
  if (inherits(p, "xml_missing")) stop("BioProject ", uid, " not found", call. = FALSE)
  rel <- xml2::xml_find_all(p, "Project/ProjectDescr/Relevance/*")
  list(
    accession = .ncbi_att(p, "Project/ProjectID/ArchiveID", "accession"),
    name = .ncbi_txt(p, "Project/ProjectDescr/Name"),
    title = .ncbi_txt(p, "Project/ProjectDescr/Title"),
    description = .ncbi_txt(p, "Project/ProjectDescr/Description"),
    relevance = if (length(rel)) paste(xml2::xml_name(rel), collapse = ", ") else NA_character_,
    material = .ncbi_enum(.ncbi_att(p, "Project/ProjectType//Target", "material")),
    capture = .ncbi_enum(.ncbi_att(p, "Project/ProjectType//Target", "capture")),
    sample_scope = .ncbi_enum(.ncbi_att(p, "Project/ProjectType//Target", "sample_scope")),
    data_type = .ncbi_txt(p, "Project/ProjectType//ProjectDataTypeSet/DataType"),
    organization = .ncbi_txt(p, "Submission//Organization/Name"),
    submitted = .ncbi_att(p, "Submission", "submitted"),
    last_update = .ncbi_att(p, "Submission", "last_update"),
    umbrella = .ncbi_att(p, "ProjectLinks//Hierarchical[@type='TopAdmin']/MemberID", "accession")
  )
}

.ncbi_fetch_chain <- function(ref, cache = new.env()) {
  ref <- ncbi_normalize_id(ref)
  out <- list()
  bs <- ref
  if (.ncbi_is_sra(ref)) {
    sra <- .ncbi_sra(ref)
    out[[1]] <- .meta_flatten(sra, "SRA", 0L, ref)
    bs <- sra$biosample
  }
  b <- .ncbi_biosample(bs)
  out[[length(out) + 1L]] <- .meta_flatten(b$fields, "BioSample", 1L, b$fields$accession)
  for (i in seq_len(nrow(b$projects))) {
    key <- paste0("bp:", b$projects$uid[i])
    p <- tryCatch({
      if (is.null(cache[[key]])) cache[[key]] <- .ncbi_bioproject(b$projects$uid[i])
      cache[[key]]
    }, error = function(e) NULL)
    if (!is.null(p)) {
      out[[length(out) + 1L]] <- .meta_flatten(p, "BioProject", 1L + i,
                                               p$accession %|NA|% b$projects$acc[i])
    }
  }
  do.call(rbind, out)
}
```
Note: the successful-project depth is `1L + i` where `i` is the link index, so a failed first link leaves depth 2 unused. That is fine (depths only need to be unique).

- [ ] **Step 5: Run, expect PASS.** Fix xpath details against the real fixture if any assertion on a field value fails (the fixture is ground truth; do not loosen the test's intent).

- [ ] **Step 6: Commit** `git commit -am "NCBI E-utilities client and BioSample fetch chain"` (add helper file first).

---

### Task 3: Registry entry, fetch_biosample(), mapping-column guard

**Files:**
- Modify: `R/meta_db.R` (registry + `.meta_take_cols`)
- Create: `R/ncbi_db.R`
- Create: `tests/testthat/test-ncbi-db.R`

**Interfaces:**
- Consumes: `ncbi_normalize_id`, `.ncbi_fetch_chain` (Tasks 1-2); `.meta_fetch_project(path, source, ids, refs)`, `.meta_set_ref`, `.meta_ensure_tables` (existing).
- Produces: `META_SOURCES$NCBI`; exported `fetch_biosample(path, ids, biosamples, from_id)`; samples column `BioSample`.

- [ ] **Step 1: Failing tests `tests/testthat/test-ncbi-db.R`**

```r
ncbi_proj <- function(ids = c("SRR21844202", "s2")) {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(dir, ".sqlite"))
  DBI::dbWriteTable(con, "samples", data.frame(ID = ids, Taxon = "Fundulus majalis"))
  DBI::dbDisconnect(con)
  dir
}
q <- function(dir, sql) {
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(dir, ".sqlite"))
  on.exit(DBI::dbDisconnect(con))
  DBI::dbGetQuery(con, sql)
}

test_that(".meta_ensure_tables adds a BioSample column", {
  dir <- ncbi_proj()
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(dir, ".sqlite"))
  on.exit(DBI::dbDisconnect(con))
  .meta_ensure_tables(con)
  expect_true("BioSample" %in% DBI::dbListFields(con, "samples"))
})

test_that("fetch_biosample sets IDs and stores records", {
  local_mocked_bindings(.ncbi_get = ncbi_fixture_get)
  dir <- ncbi_proj()
  res <- fetch_biosample(dir, ids = "s2", biosamples = "samn63236902")
  expect_equal(res$status, "ok")
  expect_equal(q(dir, "SELECT BioSample FROM samples WHERE ID = 's2'")$BioSample, "SAMN63236902")
  r <- q(dir, "SELECT DISTINCT level FROM meta_records WHERE ID = 's2' AND source = 'NCBI'")
  expect_setequal(r$level, c("BioSample", "BioProject"))
})

test_that("from_id copies valid sample IDs only", {
  local_mocked_bindings(.ncbi_get = ncbi_fixture_get)
  dir <- ncbi_proj()
  expect_message(res <- fetch_biosample(dir, from_id = TRUE), "s2")
  expect_equal(q(dir, "SELECT ID, BioSample FROM samples ORDER BY ID")$BioSample,
               c("SRR21844202", NA))
  expect_equal(res$ID, "SRR21844202")
  expect_equal(res$status, "ok")
})

test_that("a failed refresh keeps earlier NCBI data", {
  local_mocked_bindings(.ncbi_get = ncbi_fixture_get)
  dir <- ncbi_proj()
  fetch_biosample(dir, ids = "s2", biosamples = "SAMN63236902")
  n <- q(dir, "SELECT COUNT(*) n FROM meta_records WHERE ID = 's2'")$n
  local_mocked_bindings(.ncbi_get = function(...) stop("could not reach NCBI (offline)", call. = FALSE))
  expect_warning(fetch_biosample(dir, ids = "s2"), "offline")
  expect_equal(q(dir, "SELECT COUNT(*) n FROM meta_records WHERE ID = 's2'")$n, n)
})

test_that(".meta_take_cols keeps the ID column when it doubles as BioSample", {
  m <- data.frame(ID = c("SRR1", "SRR2"), Taxon = "x")
  out <- .meta_take_cols(m, c(NCBI = "ID"))
  expect_equal(out$ID, c("SRR1", "SRR2"))
  expect_equal(out$BioSample, c("SRR1", "SRR2"))
  m2 <- data.frame(run = c("SRR1", "SRR2"), ID = c("SRR1", "SRR2"), Taxon = "x")
  out2 <- .meta_take_cols(m2, c(NCBI = "run"))
  expect_equal(out2$BioSample, c("SRR1", "SRR2"))
})
```

- [ ] **Step 2: Run, expect FAIL.**

- [ ] **Step 3: Implement**

`R/meta_db.R`, add to `META_SOURCES` after GBIF:

```r
  NCBI = list(
    col = "BioSample", label = "NCBI", id_label = "BioSample or SRA accession", arg = "biosamples",
    normalize = function(x) ncbi_normalize_id(x),
    invalid = function(x) paste0("'", x, "' is not a BioSample or SRA accession (expected SAMN..., SRR..., or digits)"),
    chain = function(ref, cache) .ncbi_fetch_chain(ref, cache)
  )
```

`.meta_take_cols` guard (replace the `if (col != std)` line):

```r
      if (col != std && !col %in% c("ID", "Taxon")) mapping[[col]] <- NULL
```

`R/ncbi_db.R`:

```r
#' Fetch NCBI BioSample and BioProject metadata for project samples
#'
#' Looks up each sample's NCBI BioSample, directly or through an SRA accession
#' (SRR/ERR/DRR run, SRX experiment, or SRS sample), plus the BioProject(s) it
#' belongs to, and stores everything in the project database for viewing in
#' the app and use at export. Set the environment variable `ENTREZ_KEY` to an
#' NCBI API key for faster lookups.
#'
#' @param path Path to the project directory (default = current working directory)
#' @param ids Sample IDs to fetch. Default: every sample with a BioSample value.
#' @param biosamples Optional BioSample or SRA accessions to set for `ids` first
#'   (same length as `ids`). A blank value removes that sample's BioSample and
#'   its NCBI data.
#' @param from_id Use each sample's own ID as its BioSample value when that ID
#'   is a BioSample or SRA accession (for example an SRR run used as the sample
#'   ID). Samples whose ID is not one are left unchanged.
#' @return Invisibly, a data frame of `ID`, `status`, and `message`.
#' @export
fetch_biosample <- function(path = ".", ids = NULL, biosamples = NULL, from_id = FALSE) {
  if (isTRUE(from_id)) {
    con <- DBI::dbConnect(RSQLite::SQLite(), dbname = file.path(path, ".sqlite"))
    on.exit(DBI::dbDisconnect(con))
    .meta_ensure_tables(con)
    s <- DBI::dbGetQuery(con, "SELECT ID FROM samples")$ID
    s <- if (is.null(ids)) s else intersect(s, ids)
    ok <- !is.na(ncbi_normalize_id(s))
    for (id in s[ok]) .meta_set_ref(con, "NCBI", id, id)
    if (any(!ok)) message("Sample ID is not a BioSample or SRA accession, skipped: ", .lst(s[!ok]))
    ids <- s[ok]
    if (!length(ids)) return(invisible(data.frame(ID = character(), status = character(),
                                                  message = character())))
  }
  .meta_fetch_project(path, "NCBI", ids, biosamples)
}
```
Run `devtools::document()`.

- [ ] **Step 4: Run test-ncbi-db.R, test-meta-db.R, test-gbif-db.R, test-geome-db.R; expect PASS.** If a GEOME/GBIF test asserted the exact `samples` column list, add `BioSample`.

- [ ] **Step 5: Commit** `git add -A R/meta_db.R R/ncbi_db.R NAMESPACE man tests/testthat/test-ncbi-db.R && git commit -m "NCBI source registry entry and fetch_biosample()"`

---

### Task 4: NCBI GenBank-ready combos

**Files:**
- Create: `R/ncbi_export.R`
- Modify: `R/meta_export.R` (`.meta_combos`)
- Create: `tests/testthat/test-ncbi-export.R`

**Interfaces:**
- Consumes: `.geome_pick`, `.geome_coord` (existing).
- Produces: `NCBI_COMBOS` (named list of `list(label, sources, fn(recs))`); `.ncbi_val(recs, field)`; `.ncbi_concept_value(concept, recs)`.

- [ ] **Step 1: Failing tests**

```r
bs_recs <- function(...) {
  f <- list(...)
  data.frame(level = "BioSample", depth = 1L, ref = "SAMN1", field = names(f),
             value = unlist(f, use.names = FALSE))
}

test_that("NCBI combos pass GenBank values through and drop missing text", {
  r <- bs_recs(collection_date = "2024-06-27", geo_loc_name = "Canada: Nova Scotia",
               specimen_voucher = "ROM:12345", collected_by = "R. Martin",
               identified_by = "not collected", sex = "Female", dev_stage = "missing: control sample",
               lat_lon = "44.5 N 63.1 W", accession = "SAMN1", organism = "Zoarces americanus")
  f <- function(k) NCBI_COMBOS[[k]]$fn(r)
  expect_equal(f("collection_date"), "2024-06-27")
  expect_equal(f("geo_loc_name"), "Canada: Nova Scotia")
  expect_equal(f("specimen_voucher"), "ROM:12345")
  expect_true(is.na(f("identified_by")))
  expect_equal(f("sex"), "female")
  expect_true(is.na(f("dev_stage")))
  expect_equal(f("lat_lon"), "44.5 N 63.1 W")
  expect_equal(f("biosample"), "SAMN1")
  expect_true(is.na(f("bioproject")))
})

test_that("ncbi lat_lon converts decimal pairs and rejects junk", {
  f <- function(x) NCBI_COMBOS$lat_lon$fn(bs_recs(lat_lon = x))
  expect_equal(f("44.5, -63.1"), "44.5 N 63.1 W")
  expect_equal(f("-12.25 130.5"), "12.25 S 130.5 E")
  expect_equal(f("44.5 N, 63.1 W"), "44.5 N 63.1 W")
  expect_true(is.na(f("not collected")))
  expect_true(is.na(f("95, 10")))
})

test_that("bioproject combo takes the first project", {
  r <- rbind(bs_recs(accession = "SAMN1"),
             data.frame(level = "BioProject", depth = 2:3, ref = c("PRJNA2", "PRJNA3"),
                        field = "accession", value = c("PRJNA2", "PRJNA3")))
  expect_equal(NCBI_COMBOS$bioproject$fn(r), "PRJNA2")
  expect_identical(.meta_combos("NCBI"), NCBI_COMBOS)
})

test_that("NCBI concept values split geo_loc_name", {
  r <- bs_recs(geo_loc_name = "Canada: Scotian Shelf, Nova Scotia", organism = "Zoarces americanus")
  expect_equal(.ncbi_concept_value("country", r), "Canada")
  expect_equal(.ncbi_concept_value("locality", r), "Scotian Shelf, Nova Scotia")
  expect_equal(.ncbi_concept_value("taxon", r), "Zoarces americanus")
  expect_true(is.na(.ncbi_concept_value("locality", bs_recs(geo_loc_name = "Canada"))))
})
```

- [ ] **Step 2: Run, expect FAIL.**

- [ ] **Step 3: Implement `R/ncbi_export.R`**

```r
.NCBI_MISSING <- c("missing", "not collected", "not applicable", "not provided",
                   "restricted access", "unknown", "na", "n/a", "none", "-")

.ncbi_val <- function(recs, field, level = "BioSample") {
  v <- .geome_pick(recs[recs$level == level, , drop = FALSE], field)
  if (is.na(v)) return(v)
  if (tolower(trimws(sub(":.*", "", v))) %in% .NCBI_MISSING) NA_character_ else trimws(v)
}

.ncbi_passthrough <- function(field, lower = FALSE) {
  list(label = field, sources = field, fn = function(recs) {
    v <- .ncbi_val(recs, field)
    if (lower) tolower(v) else v
  })
}

.ncbi_lat_lon <- function(recs) {
  v <- .ncbi_val(recs, "lat_lon")
  if (is.na(v)) return(v)
  h <- regmatches(v, regexec("^([0-9.]+)\\s*([NSns])[ ,;]+([0-9.]+)\\s*([EWew])$", v))[[1]]
  if (length(h)) {
    if (is.null(.geome_coord(h[2], 90)) || is.null(.geome_coord(h[4], 180))) return(NA_character_)
    return(paste(h[2], toupper(h[3]), h[4], toupper(h[5])))
  }
  d <- regmatches(v, regexec("^([+-]?[0-9.]+)\\s*[ ,;]\\s*([+-]?[0-9.]+)$", v))[[1]]
  if (!length(d)) return(NA_character_)
  la <- .geome_coord(d[2], 90)
  lo <- .geome_coord(d[3], 180)
  if (is.null(la) || is.null(lo)) return(NA_character_)
  paste(la$txt, if (la$neg) "S" else "N", lo$txt, if (lo$neg) "W" else "E")
}

NCBI_COMBOS <- list(
  lat_lon = list(label = "lat_lon", sources = "lat_lon", fn = .ncbi_lat_lon),
  collection_date = .ncbi_passthrough("collection_date"),
  geo_loc_name = .ncbi_passthrough("geo_loc_name"),
  specimen_voucher = .ncbi_passthrough("specimen_voucher"),
  collected_by = .ncbi_passthrough("collected_by"),
  identified_by = .ncbi_passthrough("identified_by"),
  sex = .ncbi_passthrough("sex", lower = TRUE),
  dev_stage = .ncbi_passthrough("dev_stage", lower = TRUE),
  biosample = list(label = "BioSample", sources = "accession",
                   fn = function(recs) .ncbi_val(recs, "accession")),
  bioproject = list(label = "BioProject", sources = "accession",
                    fn = function(recs) .ncbi_val(recs, "accession", level = "BioProject"))
)

.ncbi_concept_value <- function(concept, recs) {
  geo <- NCBI_COMBOS$geo_loc_name$fn(recs)
  switch(concept,
    coordinates = NCBI_COMBOS$lat_lon$fn(recs),
    collection_date = NCBI_COMBOS$collection_date$fn(recs),
    country = if (is.na(geo)) NA_character_ else trimws(sub(":.*", "", geo)),
    locality = if (is.na(geo) || !grepl(":", geo, fixed = TRUE)) NA_character_ else
      trimws(sub("^[^:]*:", "", geo)) %|NA|% NA_character_,
    voucher = NCBI_COMBOS$specimen_voucher$fn(recs),
    collector = NCBI_COMBOS$collected_by$fn(recs),
    sex = NCBI_COMBOS$sex$fn(recs),
    dev_stage = NCBI_COMBOS$dev_stage$fn(recs),
    taxon = .ncbi_val(recs, "organism")
  )
}
```
`R/meta_export.R`: `switch(tolower(prefix), geome = GEOME_COMBOS, gbif = GBIF_COMBOS, ncbi = NCBI_COMBOS, NULL)`.

Note: combo `label` is used by the Export Data chips as the defline modifier name (`[label={col}]`). Before committing, check NCBI's table2asn / source-modifier docs (WebFetch `https://www.ncbi.nlm.nih.gov/genbank/mods_fastadefline/`) that `[BioSample=...]` and `[BioProject=...]` are accepted in FASTA deflines. If they are not, give those two combos a label that makes the chip insert a plain `{ncbi_biosample}` token: in `export_token_groups` the combo insert is built from the label; add a field `plain = TRUE` to these two specs and in `R/app_export_tokens.R` use `paste0("{", col, "}")` when the combo spec has `plain` (look it up via `.meta_combos(prefix)[[label_key]]$plain`).

- [ ] **Step 4: Run test-ncbi-export.R, test-gbif-export.R, test-geome-export.R; expect PASS.**

- [ ] **Step 5: Commit** `git commit -m "NCBI GenBank-ready export fields"` (add new files).

---

### Task 5: Compare and export warning across all sources

**Files:**
- Modify: `R/specimen_reconcile.R`
- Modify: `tests/testthat/test-specimen-conflicts.R`

**Interfaces:**
- Consumes: `.ncbi_concept_value` (Task 4), `META_SOURCES`.
- Produces: `specimen_conflicts()` columns `ID, concept, csv_column, csv_value, geome_value, gbif_value, ncbi_value, status`; `specimen_export_warnings()` returns `ID, concept, csv_value, <src>_value...`; `specimen_warning_html()` renders one column per source.

- [ ] **Step 1: Failing tests (append)**

```r
test_that("NCBI values join the comparison and missing text never conflicts", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbWriteTable(con, "samples", data.frame(ID = "s1", Taxon = "Zoarces americanus",
                                               country = "USA", sex = "male"))
  .meta_ensure_tables(con)
  DBI::dbAppendTable(con, "meta_records", data.frame(
    ID = "s1", source = "NCBI", level = "BioSample", depth = 1L, ref = "SAMN1",
    field = c("geo_loc_name", "organism", "sex"),
    value = c("Canada: Nova Scotia", "Zoarces americanus", "not collected")))
  cf <- specimen_conflicts(con)
  expect_true("ncbi_value" %in% names(cf))
  row <- function(k) cf[cf$concept == k, ]
  expect_equal(row("country")$ncbi_value, "Canada")
  expect_equal(row("country")$status, "conflict")
  expect_equal(row("taxon")$status, "agree")
  expect_true(is.na(row("sex")$ncbi_value))
  expect_equal(row("sex")$status, "single")
})

test_that("ncbi tokens map to concepts and warnings carry an NCBI column", {
  expect_setequal(specimen_template_concepts("[lat_lon={ncbi_lat_lon}] {ncbi_BioSample_organism}", list()),
                  c("coordinates", "taxon"))
  cf <- data.frame(ID = "s1", concept = "country", csv_value = "USA", geome_value = NA,
                   gbif_value = NA, ncbi_value = "Canada", status = "conflict")
  w <- specimen_export_warnings(cf, "country", "s1")
  expect_equal(w$ncbi_value, "Canada")
  expect_match(as.character(specimen_warning_html(w)), "<th>NCBI</th>", fixed = TRUE)
})
```

- [ ] **Step 2: Run, expect FAIL.**

- [ ] **Step 3: Implement**

In `.spec_source_value`, first line after the `nrow` guard:
```r
  if (source == "NCBI") return(.ncbi_concept_value(concept, recs))
```

Replace the body of `specimen_conflicts` from `csv_v <- geome_v <- ...` to the final `data.frame(...)`:

```r
  srcs <- names(META_SOURCES)
  csv_v <- status <- rep(NA_character_, n)
  sv <- matrix(NA_character_, n, length(srcs), dimnames = list(NULL, srcs))
  j <- 0L
  for (i in seq_len(nrow(s))) {
    row <- s[i, , drop = FALSE]
    r <- by_id[[s$ID[i]]] %||% empty
    no_meta <- !nrow(r)
    for (k in SPECIMEN_CONCEPTS) {
      j <- j + 1L
      cv <- .spec_csv_value(row, cols[[k]])
      csv_v[j] <- cv
      if (no_meta) {
        status[j] <- if (!is.na(cv) && nzchar(cv)) "single" else NA_character_
        next
      }
      for (src in srcs) sv[j, src] <- .spec_source_value(k, src, r[r$source == src, , drop = FALSE])
      status[j] <- .spec_status(k, c(cv, sv[j, ]))
    }
  }
  csv_col <- vapply(SPECIMEN_CONCEPTS, function(k) {
    if (length(cols[[k]])) paste(cols[[k]], collapse = " + ") else NA_character_
  }, character(1), USE.NAMES = FALSE)
  out <- data.frame(ID = rep(s$ID, each = nk), concept = rep(SPECIMEN_CONCEPTS, nrow(s)),
                    csv_column = rep(csv_col, nrow(s)), csv_value = csv_v)
  for (src in srcs) out[[paste0(tolower(src), "_value")]] <- sv[, src]
  out$status <- status
  out
```
(Drop the now-unused `g`/`b` locals.)

`.SPEC_FIELD_CONCEPTS`: append
```r
  lat_lon = "coordinates", collection_date = "collection_date", geo_loc_name = "country",
  specimen_voucher = "voucher", collected_by = "collector", dev_stage = "dev_stage",
  organism = "taxon"
```
(`sex` already present.)

`specimen_template_concepts`: replace the regex line with
```r
    m <- regmatches(t, regexec(paste0("^(", paste(tolower(names(META_SOURCES)), collapse = "|"),
                                      ")_(.+)$"), t))[[1]]
```

`specimen_export_warnings`:
```r
  vcols <- paste0(tolower(names(META_SOURCES)), "_value")
  out <- conflicts[keep, c("ID", "concept", "csv_value", intersect(vcols, names(conflicts))), drop = FALSE]
```

`specimen_warning_html`: build source cells and headers from `names(META_SOURCES)`:
```r
  srcs <- names(META_SOURCES)
  srcs <- srcs[paste0(tolower(srcs), "_value") %in% names(rows)]
  src_cells <- if (length(srcs)) {
    do.call(paste0, lapply(srcs, function(s) paste0("<td>", cell(rows[[paste0(tolower(s), "_value")]]), "</td>")))
  } else ""
  body <- paste0("<tr><td>", cell(rows$ID), "</td><td>", cell(rows$concept), "</td><td>",
                 cell(rows$csv_value), "</td>", src_cells, "</tr>", collapse = "")
```
and the header row `paste0("<thead><tr><th>Sample</th><th>Item</th><th>CSV</th>", paste0("<th>", srcs, "</th>", collapse = ""), "</tr></thead>")`.

Update roxygen of `set_metadata_columns` ("GEOME, GBIF, and NCBI").

- [ ] **Step 4: Run test-specimen-conflicts.R, test-specimen-rules.R; expect PASS** (update any existing assertion that compared the whole column set to include `ncbi_value`).

- [ ] **Step 5: Commit** `git commit -am "Compare and export warnings cover NCBI"`

---

### Task 6: Setup hooks, mapping checks, preflight, compat

**Files:**
- Modify: `R/init_project.R`, `R/init_project_userAsmb.R`, `R/init_db.R`, `R/init_db_userAsmb.R`, `R/add_samples.R`, `R/update_sample_metadata.R`, `R/init_checks.R`, `R/backwards_compatibility.R`
- Test: `tests/testthat/test-ncbi-db.R` (append)

**Interfaces:**
- Consumes: `META_SOURCES$NCBI`, `.meta_take_cols`, `.meta_fetch_new`, `.meta_sync_changed`.
- Produces: `mapping_biosample`, `fetch_biosample` args everywhere `mapping_gbif`/`fetch_gbif` exist.

- [ ] **Step 1: Failing tests (append to test-ncbi-db.R)**

```r
test_that("check_mapping validates BioSample values and allows reusing the ID column", {
  m <- data.frame(ID = c("SRR21844202", "s2"), Taxon = "x", R1 = "a", R2 = "b")
  iss <- check_mapping(m, mapping_biosample = "ID", need_reads = FALSE)
  msgs <- paste(c(iss$errors(), iss$warnings()), collapse = " ")
  expect_match(msgs, "not a BioSample or SRA accession for s2", fixed = TRUE)
  expect_false(grepl("reserved", msgs))
  iss2 <- check_mapping(cbind(m, BioSample = "SAMN1"), mapping_biosample = "ID", need_reads = FALSE)
  expect_match(paste(iss2$errors(), collapse = " "), "reserved")
})

test_that("new_db stores BioSample from the ID column without losing IDs", {
  local_mocked_bindings(.ncbi_get = ncbi_fixture_get)
  proj <- withr::local_tempdir()
  suppressMessages(new_test_project(path = proj, executor = "local", Rproj = FALSE,
                                    mapping_biosample = "ID", fetch_biosample = FALSE))
  s <- q(proj, "SELECT ID, BioSample FROM samples")
  expect_equal(s$BioSample, ncbi_normalize_id(s$ID))
})
```
Before writing the second test, open `tests/testthat/helper-*.R` to confirm `new_test_project()` forwards `...` to `new_project()`; if it does not, build the project with `new_project()` directly the way `test-geome-init.R` does and mirror that file.
Also check how `iss` exposes messages (`R/init_checks.R` `.issues()`); use its real accessor names.

- [ ] **Step 2: Run, expect FAIL.**

- [ ] **Step 3: Implement.** For each file, next to every `mapping_gbif`/`fetch_gbif` occurrence add the NCBI twin (roxygen, formals, forwarding in `dots`/calls):

```r
#' @param mapping_biosample Name of the mapping-file column holding NCBI
#'   BioSample or SRA accessions (optional). May be the same column as
#'   `mapping_id`. Stored as `BioSample`. See `vignette("Specimen-Metadata")`.
#' @param fetch_biosample Fetch NCBI metadata for samples with a BioSample
#'   value during setup (default TRUE). Set FALSE when offline and run
#'   [fetch_biosample()] later.
    mapping_biosample = "BioSample",
    fetch_biosample = TRUE,
```
- `.meta_take_cols(mapping, c(GEOME = mapping_geome, GBIF = mapping_gbif, NCBI = mapping_biosample))` in init_db.R:229, init_db_userAsmb.R:213, add_samples.R:110, update_sample_metadata.R:57.
- `.meta_fetch_new(con, mapping, list(GEOME = fetch_geome, GBIF = fetch_gbif, NCBI = fetch_biosample))` in init_db.R:872, init_db_userAsmb.R:964, add_samples.R:244; `.meta_sync_changed(..., NCBI = fetch_biosample)` in update_sample_metadata.R:138.
- `check_mapping()` gets `mapping_biosample = "BioSample"` formal and:

```r
  if (mapping_biosample != "BioSample") {
    if (mapping_biosample %nin% cols) {
      iss$err("mapping columns: BioSample column '", mapping_biosample, "' not found")
    }
    reserved <- c(reserved, "BioSample")
  }
```
and after the GBIF block:
```r
  # NCBI BioSample / SRA ----
  if (mapping_biosample %in% cols) {
    raw <- trimws(.meta_chr(mapping[[mapping_biosample]]))
    raw[is.na(raw)] <- ""
    bad <- nzchar(raw) & is.na(ncbi_normalize_id(raw))
    if (any(bad)) {
      iss$warn("mapping BioSample: not a BioSample or SRA accession for ",
               .lst(lab[bad]), "; these samples will show a failed NCBI fetch")
    }
  }
```
- Pass `mapping_biosample` through every `check_mapping(` call (init_checks.R:572 via `dots$mapping_biosample %||% "BioSample"`, init_db.R:160, init_db_userAsmb.R:171).
- Preflight after the GBIF block in init_checks.R:
```r
  ncbi_col <- dots$mapping_biosample %||% "BioSample"
  if (!is.null(mapping) && ncbi_col %in% colnames(mapping) &&
      !isFALSE(dots$fetch_biosample) &&
      any(!is.na(ncbi_normalize_id(mapping[[ncbi_col]])))) {
    .check_resource("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/einfo.fcgi", "NCBI", iss = iss)
  }
```
- backwards_compatibility.R:101 `all(c("GEOME_BCID", "GBIF_ID", "BioSample") %in% names(samples_table))`; message text "(GEOME/GBIF/NCBI)".
- `R/app_export_utils.R` `export_metadata_cols` owned list: add `"BioSample"`.

Run `devtools::document()`.

- [ ] **Step 4: Run test-ncbi-db.R, test-geome-init.R, test-geome-update.R, and `grep -l "check_mapping\|new_db" tests/testthat/*.R` files; expect PASS.**

- [ ] **Step 5: Commit** `git commit -am "Setup hooks and checks for NCBI BioSample column"`

---

### Task 7: App: icon, Metadata column, viewer NCBI tab, pickers, token chips

**Files:**
- Create: `inst/app/www/specimen/ncbi_helix.png`
- Modify: `inst/app/www/custom.css`, `R/app_specimen.R`, `R/meta_view.R`, `R/app_meta_view.R`, `R/app_export.R`, `R/app_export_tokens.R`, `R/app_ui.R`, `R/app_ui_userAsmb.R`
- Test: `tests/testthat/test-specimen-app.R` (append)

**Interfaces:**
- Consumes: everything above.
- Produces: `specimen_fields_modal(ns, summaries, closed_input = NULL)` where `summaries` is a named list (`GEOME`, `GBIF`, `NCBI`) of `meta_field_summary()` results (signature change; update the one caller in `R/app_export.R`).

- [ ] **Step 1: Icon**

```bash
S=$(mktemp -d)
curl -s -A "MitoPilot" -o $S/ncbi.svg "https://upload.wikimedia.org/wikipedia/commons/0/07/US-NLM-NCBI-Logo.svg"
rsvg-convert -w 392 $S/ncbi.svg -o $S/full.png
# helix sits roughly in the top 60% of the 392x484 canvas; crop a centered square around it
python3 - "$S/full.png" inst/app/www/specimen/ncbi_helix.png <<'EOF'
import sys
from PIL import Image
im = Image.open(sys.argv[1]).convert("RGBA")
w, h = im.size
side = int(w * 0.62)
cx, cy = w // 2, int(h * 0.33)
box = (cx - side // 2, cy - side // 2, cx + side // 2, cy + side // 2)
im.crop(box).resize((64, 64), Image.LANCZOS).save(sys.argv[2])
EOF
```
Open the PNG with the image viewer (Read tool) and confirm the whole helix is visible with no text; adjust `cy`/`side` and re-run if not. If PIL is missing use `magick` (`convert full.png -crop ...`).

CSS after `.mp-meta-logo-gbif`:
```css
.mp-meta-logo-ncbi { background-image: url("specimen/ncbi_helix.png"); }
```

- [ ] **Step 2: Failing app tests (append to test-specimen-app.R)**

```r
test_that("specimen_status and the viewer know the NCBI source", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbWriteTable(con, "samples", data.frame(ID = c("s1", "s2"), Taxon = "x"))
  .meta_ensure_tables(con)
  DBI::dbExecute(con, "UPDATE samples SET BioSample = 'SAMN1' WHERE ID = 's1'")
  st <- specimen_status(con)
  expect_equal(st$specimen_icons[st$ID == "s1"], "NCBI:pending")
  expect_match(st$specimen_message[st$ID == "s2"], "NCBI")
  expect_match(as.character(rt_specimen("x")), "ncbi_helix.png", fixed = TRUE)
  expect_equal(meta_view_logo("NCBI")$attribs$class, "mp-meta-logo mp-meta-logo-ncbi")
})

test_that("specimen_fields_modal has a section per source", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbWriteTable(con, "samples", data.frame(ID = "s1", Taxon = "x"))
  .meta_ensure_tables(con)
  sm <- lapply(stats::setNames(nm = names(META_SOURCES)), function(s) meta_field_summary(con, s))
  html <- as.character(specimen_fields_modal(shiny::NS("x"), sm))
  for (s in c("GEOME", "GBIF", "NCBI")) expect_match(html, paste0("<h4>", s, "</h4>"), fixed = TRUE)
  expect_match(html, "x-ncbi_combos", fixed = TRUE)
})

test_that("export token groups include NCBI", {
  g <- export_token_groups(data.frame(ID = "s1", BioSample = "SAMN1", ncbi_lat_lon = "1 N 2 E"),
                           c("ID", "Taxon", "BioSample"), "ncbi:combo:lat_lon")
  expect_true("NCBI" %in% names(g))
  expect_true("[lat_lon={ncbi_lat_lon}]" %in% g$NCBI$tokens$insert)
})

test_that("the viewer starts with an NCBI tab", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbWriteTable(con, "samples", data.frame(ID = "SRR21844202", Taxon = "x"))
  .meta_ensure_tables(con)
  ms <- shiny::MockShinySession$new()
  ms$userData$con <- con
  open <- shiny::reactiveVal(NULL)
  shiny::withReactiveDomain(ms, specimen_viewer_server("v", open))
  shiny::isolate(open("SRR21844202"))
  expect_silent(ms$flushReact())
})
```
Keep the existing test "specimen_fields_modal has a GEOME and a GBIF section" working by updating its call to the new list signature.

- [ ] **Step 3: Run, expect FAIL.**

- [ ] **Step 4: Implement**

`R/app_specimen.R`:
- Both "No GEOME BCID or GBIF ID" strings -> "No GEOME, GBIF, or NCBI ID".
- `rt_specimen` logos: `{GEOME: 'www/specimen/geome_g.png', GBIF: 'www/specimen/gbif_leaf.png', NCBI: 'www/specimen/ncbi_helix.png'}`.
- `specimen_col_def` header text: "GEOME, GBIF, and NCBI metadata for this sample. Click an icon to view, add, compare, or refresh."
- `meta_record_view` `link()`: before the GBIF `switch`, add
```r
    if (source == "NCBI") {
      return(switch(level,
        SRA = paste0("https://www.ncbi.nlm.nih.gov/sra/", ref),
        BioSample = paste0("https://www.ncbi.nlm.nih.gov/biosample/", ref),
        BioProject = paste0("https://www.ncbi.nlm.nih.gov/bioproject/", ref),
        NULL))
    }
```
  and update its roxygen line ("NCBI runs SRA, BioSample, BioProject").
- `specimen_compare_view`: header and cells per source:
```r
    tags$thead(tags$tr(tags$th("Item"), tags$th("Mapfile (column)"),
                       lapply(names(META_SOURCES), tags$th), tags$th("Status"))),
    ...
        lapply(names(META_SOURCES), function(s) tags$td(dash(r[[paste0(tolower(s), "_value")]]))),
```
- `specimen_fields_modal(ns, summaries, closed_input = NULL)`: body becomes
```r
    lapply(seq_along(summaries), function(i) tagList(
      if (i > 1) tags$hr(), section(names(summaries)[i], summaries[[i]]))),
```
- Viewer:
  - `empty_msg$NCBI <- paste("This sample has no BioSample. Paste a BioSample (SAMN...) or SRA accession",
    "(SRR..., SRX..., SRS...) above and click Fetch, click Use sample ID if the sample ID is one,",
    "or add a BioSample column to your mapping file and load it into the project with",
    "update_sample_metadata() (see the Sample Metadata article).")`
  - `samples()` SQL: `paste0("SELECT ID, Taxon, ", paste(vapply(META_SOURCES, function(s) s$col, ""), collapse = ", "), " FROM samples ORDER BY ID")`.
  - Tabs: add `tabPanel("NCBI", uiOutput(ns("ncbi_detail")))` after GBIF.
  - Refresh-all title: "Fetch every sample's GEOME, GBIF, and NCBI records again".
  - `source_detail`: placeholder `switch(source, GEOME = "ark:/21547/...", GBIF = "6186461308", NCBI = "SAMN29555051 or SRR21844202")`; after the Fetch button, when `source == "NCBI"` add
    `actionButton(ns("ncbi_use_id"), "Use sample ID", title = "Use this sample's ID as its BioSample or SRA accession and fetch")`.
  - `output$ncbi_detail <- renderUI(source_detail("NCBI"))`; `observeEvent(input$ncbi_fetch, fetch_one("NCBI"))`.
  - Use-ID handler:
```r
    observeEvent(input$ncbi_use_id, {
      req(rv$id)
      if (is.na(ncbi_normalize_id(rv$id))) {
        return(showNotification(paste0("'", rv$id, "' is not a BioSample or SRA accession"), type = "error"))
      }
      updateTextInput(session, "ncbi_ref", value = rv$id)
      fetch_one("NCBI", rv$id)
    })
```
    and give `fetch_one` a second arg `ref = input[[paste0(tolower(source), "_ref")]]`, used in `.meta_set_ref(con, source, rv$id, ref)`.
  - "No samples have a GEOME BCID or GBIF ID" -> "No samples have a GEOME, GBIF, or NCBI ID".
- `R/meta_view.R`: `META_VIEW_SOURCES <- c("Map file", names(META_SOURCES))` must be evaluated after META_SOURCES exists; since collation order is alphabetical (`meta_db.R` before `meta_view.R`) this is fine. Replace `lapply(c("GEOME", "GBIF"), ...)` with `lapply(names(META_SOURCES), ...)`; `meta_view_logo`: `if (!src %in% names(META_SOURCES)) return(NULL)`; roxygen mentions NCBI.
- `R/app_meta_view.R`: add `src_btn("NCBI", "NCBI")`; empty text "fetch GEOME, GBIF, or NCBI records".
- `R/app_export_tokens.R`: `ncbi <- meta("ncbi", "BioSample")`; add group after GBIF:
```r
    NCBI = list(open = ncbi$n > 0, tokens = ncbi$tokens,
                hint = if (!ncbi$n) pick_hint else NULL),
```
  and `if (g %in% names(META_SOURCES))` for the link.
- `R/app_export.R`:
```r
    observeEvent(input$token_fields_ncbi, snap_export())
    ...
    on("specimen_fields", {
      con <- session$userData$con
      sm <- lapply(stats::setNames(nm = names(META_SOURCES)), function(s) meta_field_summary(con, s))
      closed <- if (!is.null(rv$export_snap)) ns("specimen_fields_closed")
      showModal(specimen_fields_modal(ns, sm, closed_input = closed))
      for (s in names(sm)) local({
        src <- s
        output[[paste0(tolower(src), "_raw")]] <- reactable::renderReactable(
          raw_fields_table(sm[[src]][sm[[src]]$kind == "raw", ]))
      })
    })
```
  and in the save handler loop over `names(META_SOURCES)` and collect combos with
  `unlist(lapply(names(META_SOURCES), function(s) input[[paste0(tolower(s), "_combos")]]))`.
- `R/app_ui.R:109`, `R/app_ui_userAsmb.R:109` titles: "Choose which GEOME, GBIF, and NCBI fields are available at export".

- [ ] **Step 5: Run test-specimen-app.R, test-meta-view.R, test-export-metadata-cols.R; expect PASS.**

- [ ] **Step 6: Commit** `git add -A inst/app/www R && git commit -m "App: NCBI icon, viewer tab, pickers, and token chips"` (plus test file).

---

### Task 8: Docs

**Files:**
- Modify: `vignettes/Specimen-Metadata.Rmd`, `NEWS.md`, `_pkgdown.yml`

- [ ] **Step 1:** Vignette title -> "Sample metadata: GEOME, GBIF, and NCBI". Add a section "NCBI BioSample and SRA" covering: accepted IDs; `mapping_biosample` (can be the ID column); `fetch_biosample()` incl. `from_id = TRUE`; the NCBI tab and "Use sample ID"; what is stored (SRA, BioSample attributes, BioProjects); `ncbi_*` tokens table (copy from spec); missing-value handling; `ENTREZ_KEY`. Mention NCBI in the intro, Compare, and export-warning sections. Keep the article's existing tone and headings.
- [ ] **Step 2:** NEWS.md: under a new top heading `# MitoPilot (development version)` (create if absent), a bullet "**NCBI BioSample metadata.** ..." one or two sentences in the style of the 1.5.7 GEOME/GBIF bullets.
- [ ] **Step 3:** `_pkgdown.yml`: add `fetch_biosample` and `ncbi_normalize_id` next to `fetch_gbif`/`gbif_normalize_id`. Run `Rscript -e 'pkgdown::check_pkgdown()'`; expect no missing-topic errors.
- [ ] **Step 4: Commit** `git commit -am "Docs: NCBI BioSample metadata"`

---

### Task 9: Full test run and real-app pass

- [ ] **Step 1:** `Rscript -e 'devtools::test()' 2>&1 | tail -40`. Expected: no new failures versus main (record any pre-existing failures by running the same command on main first if anything fails).
- [ ] **Step 2:** `Rscript -e 'devtools::check(document = FALSE, args = "--no-tests", error_on = "error")'`; fix NOTE/WARNINGs this branch introduced (undocumented args, non-ASCII).
- [ ] **Step 3:** Demo project: copy an existing specimen demo from `~/MitoPilot_scratch/specimen_*` or build one with the shipped fish SRR IDs as sample IDs; add a mapping column `country` with one deliberately wrong value. In the app (use the harness in `dev/ui_review` / `dev/gbif/steps` as a template): open the Metadata viewer, NCBI tab, click "Use sample ID", confirm SRA/BioSample/BioProject cards and links; Refresh all; confirm icons in the Metadata column; Compare tab shows the NCBI column and the country conflict; Set Export Metadata shows an NCBI section; tick `lat_lon` and `collection_date`; Export Data chips show the NCBI group; export one group with a template using `{ncbi_lat_lon}` and confirm the conflict warning for country only if the template uses country, and that the defline carries the value. Save screenshots under `dev/ncbi/shots/`.
- [ ] **Step 4:** Commit any fixes found. Do not push.
