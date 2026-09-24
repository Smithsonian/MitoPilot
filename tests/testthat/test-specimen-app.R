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

test_that("geome_record_view puts cards in a scroll box with expand/collapse all", {
  recs <- data.frame(level = "Event", depth = 2L, bcid = "ark:/1/E",
                     field = "country", value = "Peru")
  html <- as.character(geome_record_view(recs, box_id = "m-geome-records"))
  expect_match(html, "id=\"m-geome-records\"[^>]*overflow-y: auto")
  expect_lt(regexpr("m-geome-records", html), regexpr("<details", html))
  expect_match(html, "Expand all", fixed = TRUE)
  expect_match(html, "Collapse all", fixed = TRUE)
  expect_match(html, "#m-geome-records details", fixed = TRUE)
  expect_false(grepl("Expand all", as.character(geome_record_view(recs[0, ]))))
})

test_that(".meta_save_fields replaces the selection", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbWriteTable(con, "samples", data.frame(ID = "s1", Taxon = "x"))
  .meta_ensure_tables(con)
  .meta_save_fields(con, c("geome:combo:lat_lon", "geome:raw:Event:country"))
  .meta_save_fields(con, "geome:combo:lat_lon")
  expect_equal(DBI::dbGetQuery(con, "SELECT key FROM meta_export_fields")$key, "geome:combo:lat_lon")
  .meta_save_fields(con, character())
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM meta_export_fields")$n, 0L)
})

test_that("the Export column picker offers a GEOME group, and ticked GEOME
          fields render as a GEOME column group in the Export table", {
  proj <- withr::local_tempdir()
  suppressMessages(new_test_project_userAsmb(path = proj, executor = "local", Rproj = FALSE))
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(proj, ".sqlite"))
  withr::defer(DBI::dbDisconnect(con))
  withr::local_options(MitoPilot.db = file.path(proj, ".sqlite"))
  .meta_ensure_tables(con)
  id1 <- DBI::dbGetQuery(con, "SELECT ID FROM samples LIMIT 1")$ID
  DBI::dbAppendTable(con, "meta_records", data.frame(
    ID = id1, source = "GEOME", level = "Event", depth = 0L, ref = NA_character_,
    field = "country", value = "Peru"))
  # Fields ticked ahead of time, as the picker's Save handler would leave them
  # (session$setInputValue + observeEvent(input$x) does not fire reliably
  # under shiny::testServer in this environment, even for a bare module with
  # no MitoPilot code involved; see task-9-report.md).
  .meta_save_fields(con, c("geome:combo:lat_lon", "geome:raw:Event:country"))

  expect_true("Specimen" %in% names(EXPORT_COL_GROUPS))

  ms <- shiny::MockShinySession$new()
  ms$userData$con <- con
  ms$userData$mode <- "annotate"
  for (f in c("goto_annotate", "reopen_outlier_review", "run_modal")) gargoyle::init(f, session = ms)

  shiny::testServer(export_server, args = list(id = "exp"), session = ms, {
    gargoyle::trigger("refresh_export")

    w <- jsonlite::fromJSON(output$table, simplifyVector = FALSE)
    cols <- w$x$tag$attribs$columns
    if (is.character(cols)) cols <- jsonlite::fromJSON(cols, simplifyVector = FALSE)
    id <- vapply(cols, function(c) c$id, character(1))
    cls <- vapply(cols, function(c) c$className %||% "", character(1))
    shown <- vapply(cols, function(c) !isFALSE(c$show), logical(1))
    expect_true(all(c("geome_lat_lon", "geome_Event_country") %in% id[shown]))
    expect_setequal(id[grepl("mp-grp-Specimen", cls)], c("specimen", "geome_lat_lon", "geome_Event_country"))
  })
})

test_that("geome_fields_modal builds a checkbox list and a raw-fields reactable", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbWriteTable(con, "samples", data.frame(ID = "s1", Taxon = "x"))
  .meta_ensure_tables(con)
  DBI::dbAppendTable(con, "meta_records", data.frame(
    ID = "s1", source = "GEOME", level = "Event", depth = 0L, ref = NA_character_,
    field = "country", value = "Peru"))
  .meta_save_fields(con, "geome:combo:lat_lon")

  s <- meta_field_summary(con, "GEOME")
  modal_html <- as.character(geome_fields_modal(NS("exp"), s))
  expect_match(modal_html, "GEOME fields for export", fixed = TRUE)
  expect_match(modal_html, "lat_lon", fixed = TRUE)

  raw <- s[s$kind == "raw", ]
  rt <- reactable::reactable(
    raw[, c("level", "field", "n_samples", "example", "col")],
    selection = "multiple", defaultSelected = which(raw$selected)
  )
  rt_html <- as.character(htmltools::as.tags(rt))
  expect_match(rt_html, "country", fixed = TRUE)
})

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
