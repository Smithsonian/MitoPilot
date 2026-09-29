# Cross-Database Linking and Metadata Removal Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** (1) Optionally follow links between GEOME, GBIF, and NCBI records so one ID fills in the others; (2) let users remove fetched metadata per source or all, never touching mapping-file data; (3) document both, with flowcharts.

**Architecture:** Two small tables added by `.meta_ensure_tables`: `meta_options(key, value)` (project switch `link_sources`) and `meta_links(ID, source, ref, via, note)` (provenance of auto-found IDs, and notes about links not taken). Pure "link reader" functions read a sample's fetched records of one source and return candidate IDs for the other sources; `.meta_link_sample()` applies them (fill only empty IDs, fetch each new one once, two passes). Removal deletes `meta_records`/`meta_status` rows and only auto-linked IDs.

**Tech Stack:** R, httr2, RSQLite, shiny, testthat 3.

**Spec:** Approved in-chat designs (2026-09-28), recorded here as the binding brief:
- Linking is opt-in (`link_sources = FALSE` default), project-level, set at setup or by a viewer checkbox; `fetch_*()` take `link_sources = NULL` (NULL = project setting).
- Only fill sources the sample has no ID for; never overwrite a user/CSV ID; record provenance ("via"); a record pointing to a different ID than the user's leaves the user's and stores a note.
- Links: NCBI->GEOME `bcid`; NCBI->GBIF NMNH EZID in `voucherURI`/`catalogNumber`, else voucher search on `specimen_voucher`/`genbankSpecimenVoucher`; GEOME->NCBI fastqMetadata child `bioSample.accession` (extra request); GEOME->GBIF EZID in `voucherURI`/`catalogNumber`, else voucher search on `genbankSpecimenVoucher`/`materialSampleID`; GBIF->NCBI `associatedSequences` (BioSample first, else SRA run); GBIF->GEOME `ark:/21547/` in `occurrenceID`/`materialSampleID`/`catalogNumber`.
- GBIF voucher search: parse `inst:[coll:]cat`; try `occurrenceID=cat`, `catalogNumber="inst cat"`, `catalogNumber=cat&institutionCode=inst`; keep hits whose species matches the sample Taxon, whose collectionCode matches coll (if given, case-insensitive), prefer PRESERVED_SPECIMEN; link only if exactly one remains, else note "N possible GBIF matches for <v>; not linked".
- Removal: viewer "Remove fetched data" panel (sources checkboxes, This sample / All samples) and `remove_metadata(path, sources, ids)`. Deletes fetched records/status and auto-linked IDs only; mapping-file columns and user IDs untouched; export/view field picks kept.
- Docs: NCBI mentions wherever GEOME/GBIF are mentioned; Specimen-Metadata subsection "Resolving records across databases" with flowchart(s); removal section; NEWS.

## Global Constraints

- R code ASCII only; minimal comments; narrow changes; commit per task, brief messages, no attribution, no push.
- Tests: `Rscript -e "devtools::load_all(quiet=TRUE); testthat::test_file('tests/testthat/<f>')"`.
- Figures: hand-written SVG in `vignettes/figures/` (no graphviz on this machine), text readable in light and dark pkgdown themes (solid fills with dark text).

## Review Focus

1. User/CSV IDs are never overwritten or removed, including the ID column reused as BioSample. (Tasks 3, 4)
2. Mapping-file metadata columns survive every removal path byte-for-byte. (Task 4)
3. Voucher search never links when ambiguous (SIO lot 09-320 has 27 species). (Task 2)
4. Linking cannot loop or refetch a source twice for a sample. (Task 3)
5. Linking failures (network, bad candidate) never fail the originating fetch. (Task 3)

---

### Task 1: Storage and project switch

**Files:** Modify `R/meta_db.R`; Test `tests/testthat/test-meta-links.R` (new)

**Produces:** tables `meta_options`, `meta_links`; `.meta_link_enabled(con) -> logical`; `.meta_set_link_enabled(con, on)`; `.meta_set_ref()` deletes the sample's `meta_links` row for that source (the ID is now the user's).

- [ ] Test:
```r
test_that("link switch and provenance tables", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbWriteTable(con, "samples", data.frame(ID = "s1", Taxon = "x"))
  .meta_ensure_tables(con)
  expect_false(.meta_link_enabled(con))
  .meta_set_link_enabled(con, TRUE)
  expect_true(.meta_link_enabled(con))
  DBI::dbExecute(con, "INSERT INTO meta_links VALUES ('s1', 'GBIF', '123', 'NCBI BioSample voucherURI', NULL)")
  .meta_set_ref(con, "GBIF", "s1", "456")
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM meta_links")$n, 0L)
})
```
- [ ] Implement in `.meta_ensure_tables`:
```r
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS meta_options (key TEXT NOT NULL, value TEXT, PRIMARY KEY (key))")
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS meta_links (
    ID TEXT NOT NULL, source TEXT NOT NULL, ref TEXT, via TEXT, note TEXT, PRIMARY KEY (ID, source))")
```
```r
.meta_link_enabled <- function(con) {
  .meta_ensure_tables(con)
  isTRUE(DBI::dbGetQuery(con, "SELECT value FROM meta_options WHERE key = 'link_sources'")$value[1] == "1")
}
.meta_set_link_enabled <- function(con, on) {
  .meta_ensure_tables(con)
  DBI::dbExecute(con, "INSERT OR REPLACE INTO meta_options VALUES ('link_sources', ?)",
                 params = list(if (isTRUE(on)) "1" else "0"))
  invisible(isTRUE(on))
}
```
In `.meta_set_ref`, after the UPDATE: `DBI::dbExecute(con, "DELETE FROM meta_links WHERE ID = ? AND source = ?", params = list(id, source))`.
Backwards compat check (`R/backwards_compatibility.R` meta_current) add `"meta_options", "meta_links"` to the required table list.
- [ ] Run test-meta-links.R, test-meta-db.R, test-backwards-compatibility.R; commit "Link switch and provenance tables".

### Task 2: Link readers and GBIF voucher search

**Files:** Create `R/meta_link.R`; Test `tests/testthat/test-meta-links.R`

**Produces:** `.meta_link_candidates(source, recs, taxon, cache) -> named list` of target source -> `list(ref=, via=)` or `list(note=)`; `.gbif_find_voucher(voucher, taxon) -> list(ref, via) | list(note) | NULL`; `.geome_fastq_biosample(bcid) -> character(1) | NA`.

- [ ] Tests (inline `recs` frames like `data.frame(level, depth, ref, field, value)`; mock `.gbif_get`, `.geome_get`):
  - NCBI recs with `bcid = "https://n2t.net/ark:/21547/FDZ2UW:157636.1"` and `voucherURI = "http://n2t.net/ark:/65665/3dd003c5a-d734-480a-9670-918173b14bd3"` -> GEOME ref `ark:/21547/FDZ2UW:157636.1` via "NCBI BioSample bcid"; GBIF ref `ark:/65665/3dd003c5a-d734-480a-9670-918173b14bd3` via "NCBI BioSample voucherURI".
  - NCBI recs with only `specimen_voucher = "UW:157636"`; mocked search returning for `catalogNumber=UW%20157636` one Psychrolutes paradoxus PRESERVED_SPECIMEN key 2013211250 -> GBIF ref "2013211250", via "NCBI BioSample specimen_voucher UW:157636".
  - Voucher `SIO:09-320`, mocked `catalogNumber=09-320&institutionCode=SIO` returns 3 hits of other species -> NULL (no taxon match), and two hits of the right species (both PRESERVED_SPECIMEN) -> `note` matching "2 possible GBIF matches".
  - `USNM:FISH:419933`, hits: FISH preserved (right taxon), FISH material sample (right taxon), HERP preserved (other taxon) -> the FISH preserved key.
  - GBIF recs `associatedSequences = "PV245865|SRR34274583|SAMN49487040|PRJNA1057917"` -> NCBI "SAMN49487040"; with only `"PV1|SRR9"` -> "SRR9"; `occurrenceID = "ark:/21547/ABC1"` -> GEOME.
  - GEOME recs (Tissue level bcid `ark:/21547/T1`, Sample level `voucherURI` EZID) with mocked `.geome_get` for `records/ark:/21547/T1` + includeChildren returning a fastqMetadata child with `bioSample$accession = "SAMN1"` -> NCBI "SAMN1", GBIF EZID.
  - A reader error (mock throwing) returns no candidate for that target, not an error.
- [ ] Implement `R/meta_link.R`:
```r
.link_first <- function(recs, fields, pattern, level = NULL) {
  r <- recs[recs$field %in% fields & (is.null(level) | recs$level %in% level), , drop = FALSE]
  r <- r[order(match(r$field, fields), r$depth), , drop = FALSE]
  for (i in seq_len(nrow(r))) {
    m <- regmatches(r$value[i], regexpr(pattern, r$value[i]))
    if (length(m)) return(list(value = m, field = r$field[i], level = r$level[i]))
  }
  NULL
}

.link_species <- function(x) {
  w <- strsplit(tolower(trimws(x %|NA|% "")), "\\s+")[[1]]
  paste(utils::head(w, 2), collapse = " ")
}

.gbif_find_voucher <- function(voucher, taxon) {
  p <- trimws(strsplit(voucher, ":", fixed = TRUE)[[1]])
  if (length(p) < 2 || !nzchar(p[1]) || !nzchar(p[length(p)])) return(NULL)
  inst <- p[1]; cat <- p[length(p)]; coll <- if (length(p) >= 3) p[2] else NA_character_
  want <- .link_species(taxon)
  enc <- function(x) utils::URLencode(x, reserved = TRUE)
  queries <- c(paste0("occurrenceID=", enc(cat)),
               paste0("catalogNumber=", enc(paste(inst, cat))),
               paste0("catalogNumber=", enc(cat), "&institutionCode=", enc(inst)))
  for (q in queries) {
    res <- tryCatch(.gbif_get(paste0("occurrence/search?limit=50&", q))$results, error = function(e) NULL)
    if (!length(res)) next
    sp <- vapply(res, function(r) .link_species(r$scientificName), "")
    keep <- if (length(strsplit(want, " ")[[1]]) < 2) sub(" .*", "", sp) == want else sp == want
    if (!is.na(coll)) keep <- keep & tolower(vapply(res, function(r) r$collectionCode %||% "", "")) == tolower(coll)
    res <- res[keep]
    if (!length(res)) next
    spec <- vapply(res, function(r) identical(r$basisOfRecord, "PRESERVED_SPECIMEN"), logical(1))
    if (any(spec)) res <- res[spec]
    if (length(res) == 1L) return(list(ref = .meta_chr(res[[1]]$key), via = voucher))
    return(list(note = paste0(length(res), " possible GBIF matches for ", voucher, "; not linked")))
  }
  NULL
}

.geome_fastq_biosample <- function(bcid) {
  kids <- .geome_get(paste0("records/", bcid), list(includeChildren = "true"))$children
  for (k in kids) {
    acc <- k$bioSample$accession
    if (!is.null(acc) && nzchar(acc)) return(acc)
  }
  NA_character_
}

.meta_link_candidates <- function(source, recs, taxon, cache = new.env()) {
  out <- list()
  lab <- function(hit) paste(source, hit$level, hit$field)
  ezid <- "ark:/65665/3[0-9a-fA-F-]+"
  geome <- "ark:/21547/[A-Za-z0-9._~:-]+"
  voucher_link <- function(fields) {
    h <- .link_first(recs, fields, "^[^:]+:.+$")
    if (is.null(h)) return(NULL)
    v <- tryCatch(.gbif_find_voucher(h$value, taxon), error = function(e) NULL)
    if (!is.null(v$ref)) v$via <- paste(lab(h), v$via)
    v
  }
  if (source == "NCBI") {
    h <- .link_first(recs, "bcid", geome, "BioSample")
    if (!is.null(h)) out$GEOME <- list(ref = h$value, via = lab(h))
    h <- .link_first(recs, c("voucherURI", "catalogNumber"), ezid, "BioSample")
    out$GBIF <- if (!is.null(h)) list(ref = h$value, via = lab(h)) else
      voucher_link(c("specimen_voucher", "genbankSpecimenVoucher"))
  }
  if (source == "GEOME") {
    t <- recs[recs$depth == min(recs$depth), , drop = FALSE]
    start <- t$ref[1]
    bs <- tryCatch(.geome_fastq_biosample(start), error = function(e) NA_character_)
    if (!is.na(bs)) out$NCBI <- list(ref = bs, via = paste("GEOME", t$level[1], "sequencing record"))
    h <- .link_first(recs, c("voucherURI", "catalogNumber"), ezid)
    out$GBIF <- if (!is.null(h)) list(ref = h$value, via = lab(h)) else
      voucher_link(c("genbankSpecimenVoucher", "materialSampleID"))
  }
  if (source == "GBIF") {
    h <- .link_first(recs, "associatedSequences", "SAM(N|EA|D)[0-9]+", "Occurrence") %||%
      .link_first(recs, "associatedSequences", "[SED]RR[0-9]+", "Occurrence")
    if (!is.null(h)) out$NCBI <- list(ref = h$value, via = lab(h))
    h <- .link_first(recs, c("occurrenceID", "materialSampleID", "catalogNumber"), geome, "Occurrence")
    if (!is.null(h)) out$GEOME <- list(ref = h$value, via = lab(h))
  }
  Filter(Negate(is.null), out)
}
```
Note `.link_first` with `level = NULL`: use `if (is.null(level)) TRUE else recs$level %in% level` (the inline `|` form above is illustrative; write it correctly).
- [ ] Run tests; commit "Link readers and GBIF voucher search".

### Task 3: Link engine and wiring

**Files:** Modify `R/meta_link.R`, `R/meta_db.R` (`.meta_fetch_into` gains `link = FALSE`; `.meta_fetch_project`, `.meta_fetch_new`, `.meta_sync_changed` pass it), `R/geome_db.R`/`R/gbif_db.R`/`R/ncbi_db.R` (`link_sources = NULL` arg), setup functions (`link_sources = FALSE`, stored via `.meta_set_link_enabled` when TRUE); Test `tests/testthat/test-meta-links.R`.

**Produces:** `.meta_link_sample(con, id, caches) -> invisible`; `fetch_geome/fetch_gbif/fetch_biosample(..., link_sources = NULL)`.

- [ ] Tests (mock `.meta_link_candidates` and the three `*_fetch_chain` via `META_SOURCES` chain functions by mocking `.ncbi_fetch_chain`, `.geome_fetch_chain`, `.gbif_fetch_chain`):
  - Sample with BioSample only, NCBI fetch with `link = TRUE`: candidates NCBI->GEOME, then GEOME->GBIF (second pass) -> samples GEOME_BCID and GBIF_ID filled, meta_links has 2 rows with via, each chain called once.
  - Sample with a user GBIF_ID "999": candidate GBIF "123" -> GBIF_ID stays "999", meta_links row for GBIF has `ref` NULL and `note` mentioning "123".
  - Candidate chain for a source that errors -> originating fetch status still "ok", target status "failed", link row kept (ID was found; fetch failed).
  - Candidates that cycle (GBIF->NCBI when NCBI already fetched) do not refetch NCBI.
  - `link = FALSE` (default) -> no candidates read.
  - `fetch_biosample(dir, link_sources = NULL)` uses the project switch; `TRUE`/`FALSE` override without changing the switch.
  - `new_db(..., link_sources = TRUE)` stores the switch.
- [ ] Implement:
```r
.meta_link_sample <- function(con, id, caches = lapply(META_SOURCES, function(x) new.env())) {
  done <- character()
  for (pass in 1:2) {
    st <- DBI::dbGetQuery(con, "SELECT source FROM meta_status WHERE ID = ? AND status = 'ok'", params = list(id))$source
    todo <- setdiff(st, done)
    if (!length(todo)) break
    for (src in todo) {
      done <- c(done, src)
      recs <- DBI::dbGetQuery(con, "SELECT level, depth, ref, field, value FROM meta_records WHERE ID = ? AND source = ?",
                              params = list(id, src))
      taxon <- DBI::dbGetQuery(con, "SELECT Taxon FROM samples WHERE ID = ?", params = list(id))$Taxon[1]
      cands <- tryCatch(.meta_link_candidates(src, recs, taxon, caches[[src]]), error = function(e) list())
      for (tgt in names(cands)) {
        c <- cands[[tgt]]
        col <- META_SOURCES[[tgt]]$col
        cur <- DBI::dbGetQuery(con, paste0("SELECT ", col, " AS v FROM samples WHERE ID = ?"), params = list(id))$v[1]
        mine <- DBI::dbGetQuery(con, "SELECT ref FROM meta_links WHERE ID = ? AND source = ?", params = list(id, tgt))$ref
        if (!is.null(c$note)) {
          if (is.na(cur)) DBI::dbExecute(con, "INSERT OR REPLACE INTO meta_links VALUES (?, ?, NULL, NULL, ?)",
                                         params = list(id, tgt, c$note))
          next
        }
        ref <- META_SOURCES[[tgt]]$normalize(c$ref)
        if (is.na(ref)) next
        if (is.na(cur)) {
          DBI::dbExecute(con, paste0("UPDATE samples SET ", col, " = ? WHERE ID = ?"), params = list(ref, id))
          DBI::dbExecute(con, "INSERT OR REPLACE INTO meta_links VALUES (?, ?, ?, ?, NULL)", params = list(id, tgt, ref, c$via))
          if (!tgt %in% done) suppressWarnings(.meta_fetch_into(con, tgt, id, ref, caches[[tgt]]))
        } else if (cur != ref && !length(mine[!is.na(mine)])) {
          DBI::dbExecute(con, "INSERT OR REPLACE INTO meta_links VALUES (?, ?, NULL, NULL, ?)", params = list(
            id, tgt, paste0(src, " record links to ", ref, " (", c$via, "); kept your ID ", cur)))
        }
      }
    }
  }
  invisible(NULL)
}
```
`.meta_fetch_into(con, source, ids, refs, cache = new.env(), link = FALSE)`: after the per-sample loop, `if (isTRUE(link)) for (id in ids[status == "ok"]) tryCatch(.meta_link_sample(con, id), error = function(e) NULL)`.
`.meta_fetch_project(path, source, ids, refs, link = NULL)`: `link <- link %||% .meta_link_enabled(con)`; pass to `.meta_fetch_into`. Note: this package's `%||%` treats length-0 as missing; `NULL` here is fine.
`.meta_fetch_new(con, mapping, fetch, link = FALSE)` and `.meta_sync_changed(..., link = FALSE)` pass `link`.
Setup (`new_project`, `new_project_userAsmb`, `new_db`, `new_db_userAsmb`, `add_samples`, `update_sample_metadata`): add `link_sources = FALSE` next to `fetch_biosample` (roxygen: "Follow links between GEOME, GBIF, and NCBI records to fill in IDs a sample does not have yet (default FALSE). Saved as the project setting; see `vignette("Specimen-Metadata")`."). In `new_db`/`new_db_userAsmb`: `if (isTRUE(link_sources)) .meta_set_link_enabled(con, TRUE)` before `.meta_fetch_new(..., link = isTRUE(link_sources))`. In `add_samples`/`update_sample_metadata`: `link = isTRUE(link_sources) || .meta_link_enabled(con)`; do not change the stored switch there.
`fetch_geome`, `fetch_gbif`, `fetch_biosample`: add `link_sources = NULL` (roxygen: "Follow links to other databases after fetching. NULL (default) uses the project setting.") and pass `link = link_sources` to `.meta_fetch_project`.
- [ ] Run test-meta-links.R plus ncbi/gbif/geome db + init tests; commit "Follow links between GEOME, GBIF, and NCBI records".

### Task 4: Removal

**Files:** Modify `R/meta_link.R` (or new `R/meta_remove.R`); Test `tests/testthat/test-meta-remove.R`.

**Produces:** `.meta_remove(con, sources, ids = NULL)`; exported `remove_metadata(path = ".", sources = names(META_SOURCES), ids = NULL)`.

- [ ] Test: samples with CSV columns `country`, `voucher`; s1 has user GEOME_BCID (no link row) and linked GBIF_ID (link row), s2 has BioSample = its own ID (CSV reuse, no link row); records+status for all three sources on both; `meta_export_fields` has a ncbi key.
  - `remove_metadata(dir, "GBIF", "s1")`: only s1 GBIF records/status gone; s1 GBIF_ID NULL and link row gone; GEOME records untouched; CSV columns identical (`expect_identical` on the full samples frame minus GBIF_ID).
  - `remove_metadata(dir)`: all records/status gone; GEOME_BCID for s1 and BioSample for s2 unchanged; `meta_export_fields` unchanged; `meta_links` notes for removed sources cleared.
  - Unknown source -> error listing valid sources; unknown ID -> error.
- [ ] Implement:
```r
.meta_remove <- function(con, sources = names(META_SOURCES), ids = NULL) {
  .meta_ensure_tables(con)
  bad <- setdiff(sources, names(META_SOURCES))
  if (length(bad)) stop("unknown source(s): ", .lst(bad), "; use ", .lst(names(META_SOURCES)), call. = FALSE)
  all_ids <- DBI::dbGetQuery(con, "SELECT ID FROM samples")$ID
  if (!is.null(ids)) {
    unknown <- setdiff(ids, all_ids)
    if (length(unknown)) stop("sample(s) not in this project: ", .lst(unknown), call. = FALSE)
  } else ids <- all_ids
  DBI::dbWithTransaction(con, {
    for (src in sources) {
      col <- META_SOURCES[[src]]$col
      for (id in ids) {
        ln <- DBI::dbGetQuery(con, "SELECT ref FROM meta_links WHERE ID = ? AND source = ?", params = list(id, src))$ref
        if (length(ln) && !is.na(ln[1])) {
          DBI::dbExecute(con, paste0("UPDATE samples SET ", col, " = NULL WHERE ID = ? AND ", col, " = ?"),
                         params = list(id, ln[1]))
        }
        DBI::dbExecute(con, "DELETE FROM meta_links WHERE ID = ? AND source = ?", params = list(id, src))
      }
      .meta_drop(con, src, ids)
    }
  })
  invisible(NULL)
}
```
Roxygen for `remove_metadata`: removes records fetched from GEOME, GBIF, and NCBI and IDs found by following links; keeps mapping-file columns, IDs you supplied, and export/view field choices; fetch again any time.
- [ ] Run; `devtools::document()`; commit "remove_metadata(): delete fetched data, keep mapping-file data".

### Task 5: App

**Files:** Modify `R/app_specimen.R`; Test `tests/testthat/test-specimen-app.R`.

- [ ] Tests (testServer pattern already used for the NCBI tab test):
  - `output$ncbi_detail` for a sample whose NCBI ID is linked shows "Found through GEOME Tissue sequencing record"; a note row shows its note text.
  - Footer contains a checkbox `v-link_sources` whose value reflects the project switch; setting it updates `meta_options`.
  - `remove_panel` UI has checkboxes GEOME/GBIF/NCBI and scope radio; clicking `remove_go` with scope "sample" and GBIF only removes that sample's GBIF records (verify via DB).
  - `specimen_status` tooltip line reads "GBIF: fetched (linked from NCBI BioSample voucherURI)" for a linked source.
- [ ] Implement:
  - `source_detail()`: query `meta_links` row for (ID, source); under the status line, if `ref` present: `p(class = "text-muted", "Found through ", via)`; if `note` present: `p(class = "mp-fg-warning", note)`.
  - Left column under the sample list: `tags$details(class = "mp-spec-remove", tags$summary("Remove fetched data..."), checkboxGroupInput(ns("remove_sources"), NULL, choices = names(META_SOURCES), selected = names(META_SOURCES), inline = TRUE), radioButtons(ns("remove_scope"), NULL, c("This sample" = "sample", "All samples" = "all"), inline = TRUE), p(class = "text-muted", "Mapping-file columns and the IDs you entered are kept. You can fetch again at any time."), actionButton(ns("remove_go"), "Remove", class = "btn-danger btn-sm"))`.
  - Handler: `.meta_remove(con, input$remove_sources, if (input$remove_scope == "sample") rv$id)`; `bump()`; notification "Removed <sources> data for <this sample|all samples>". No-op + message when no source ticked.
  - Footer: `checkboxInput(ns("link_sources"), "Follow links between databases", value = .meta_link_enabled(con))` next to Refresh all, with title "When a fetched record names a record in another database, add and fetch it too (only for samples without an ID there)". Observer: `.meta_set_link_enabled(con, input$link_sources)` (ignoreInit).
  - `fetch_one` and Refresh all pass `link = .meta_link_enabled(con)` to `.meta_fetch_into`.
  - `specimen_status`: when a `meta_links` row with `ref` exists for (ID, src), append " (linked from <via>)" to that source's line.
- [ ] Run test-specimen-app.R, test-meta-view.R; commit "App: link switch, link provenance, remove fetched data".

### Task 6: Docs

**Files:** `vignettes/Specimen-Metadata.Rmd`, `vignettes/Your-Own-Project.Rmd`, `vignettes/Test-Project-Export.Rmd`, `vignettes/figures/link_overview.svg`, `vignettes/figures/link_gbif_voucher.svg`, `NEWS.md`, `_pkgdown.yml`.

- [ ] `Your-Own-Project.Rmd` ~line 134: add NCBI BioSample/SRA to the linking paragraph (`mapping_biosample`, can be the ID column) and mention `link_sources = TRUE`.
- [ ] `Test-Project-Export.Rmd` tip ~line 186: "GEOME, GBIF, or NCBI" and `{ncbi_...}` example `{ncbi_collection_date}`.
- [ ] `Specimen-Metadata.Rmd`: new section after "Adding or changing identifiers later": `## Resolving records across databases` with: what it does (one ID fills the rest), how to turn it on (`link_sources = TRUE` at setup, `fetch_*(link_sources = TRUE)`, viewer checkbox), the link table (from/to/field), figure 1 `link_overview.svg` (three database boxes with labelled arrows for the six links; note "each database fetched once per sample; your IDs never replaced"), subsection `### Matching a voucher to a GBIF record` with figure 2 `link_gbif_voucher.svg` (flowchart: voucher `inst:coll:cat` -> try occurrenceID -> try "inst cat" -> try cat + institutionCode -> filter species = Taxon -> filter collection -> prefer preserved specimen -> exactly one? yes: link / no: note, not linked), provenance ("Found through ..." in the viewer and tooltip) and notes, limits (non-Smithsonian vouchers need institution code and a GBIF-published catalog; GEOME sequencing records may name a different SRA run for the same BioSample; linking uses extra requests). New section `## Removing fetched data` (viewer panel, `remove_metadata()`, what is kept). Update "Limits and troubleshooting" table with the "possible GBIF matches ... not linked" note.
- [ ] SVGs: `viewBox` sized, `font-family` system sans, boxes with fills `#e4f0f4` (strokes `#1f6f8b`), text `#243137`, arrows with `marker-end`; background rect white so it reads in dark mode. Check rendering by converting with `rsvg-convert` to PNG and viewing.
- [ ] NEWS: bullets for linking and removal (plain language). `_pkgdown.yml`: add `remove_metadata`.
- [ ] `pkgdown::check_pkgdown()`; render the vignette once with `rmarkdown::render()` to a scratch dir to confirm figures resolve; commit "Docs: cross-database linking and removing fetched data".

### Task 7: Verification

- [ ] Full `devtools::test()`; only the pre-existing translate-guard failure allowed.
- [ ] Live: fresh demo `~/MitoPilot_scratch/ncbi_link_demo` via `new_test_project(n = 6, mapping_biosample = "ID", link_sources = TRUE)`; expect GEOME 6/6, GBIF 6/6 (4 by EZID, UW and SIO by voucher search), all `meta_links` with via; `remove_metadata(sources = "GBIF")` leaves BioSample column (from ID) and CSV intact; app click-through of checkbox, provenance lines, remove panel; screenshots `dev/ncbi/shots/`.
- [ ] Final whole-branch review (fresh reviewer), fix Critical/Important with tests.
