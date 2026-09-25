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
  DBI::dbAppendTable(con, "meta_records", data.frame(
    ID = id1, source = "GBIF", level = "Occurrence", depth = 0L, ref = "1",
    field = "sex", value = "Female"))
  # Fields ticked ahead of time, as the picker's Save handler would leave them
  # (session$setInputValue + observeEvent(input$x) does not fire reliably
  # under shiny::testServer in this environment, even for a bare module with
  # no MitoPilot code involved; see task-9-report.md).
  .meta_save_fields(con, c("geome:combo:lat_lon", "geome:raw:Event:country", "gbif:combo:sex"))

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
    expect_true(all(c("geome_lat_lon", "geome_Event_country", "gbif_sex") %in% id[shown]))
    expect_setequal(id[grepl("mp-grp-Specimen", cls)],
                    c("specimen", "geome_lat_lon", "geome_Event_country", "gbif_sex"))
  })
})

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
               "GEOME: fetched\nGBIF: fetched\nConflicts: country\nNot checked: collector")
  expect_equal(s$specimen_message[4], "No GEOME BCID or GBIF ID")
  expect_equal(s$specimen_icons, c("GEOME:ok", "GBIF:failed", "GEOME:ok GBIF:ok conflict", ""))
})

test_that("a set but never fetched ID says so", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbWriteTable(con, "samples", data.frame(ID = "a", Taxon = "x", GBIF_ID = "5"))
  s <- specimen_status(con)
  expect_equal(s$specimen, "none")
  expect_equal(s$specimen_message, "GBIF: not fetched yet")
  expect_equal(s$specimen_icons, "GBIF:pending")
})

test_that(".specimen_status_join adds specimen columns to a collected table", {
  con <- status_db()
  on.exit(DBI::dbDisconnect(con))
  out <- .specimen_status_join(data.frame(ID = c("c", "a", "zz")), con)
  expect_equal(out$specimen, c("conflict", "ok", "none"))
  expect_true(is.na(out$specimen_message[3]))
})

test_that("rt_specimen draws one logo per source, a conflict flag, or a plus", {
  js <- as.character(rt_specimen("assemble-specimen_open"))
  expect_match(js, "assemble-specimen_open", fixed = TRUE)
  expect_match(js, "setInputValue", fixed = TRUE)
  expect_match(js, "dataset.id", fixed = TRUE)
  expect_match(js, "specimen_message", fixed = TRUE)
  expect_match(js, "specimen_icons", fixed = TRUE)
  for (x in c("www/specimen/geome_g.png", "www/specimen/gbif_leaf.png", "fa-flag",
              "fa-square-plus", "fa-triangle-exclamation", "mp-spec-faded")) {
    expect_match(js, x, fixed = TRUE)
  }
  expect_false(grepl("fa-earth-americas", js, fixed = TRUE))
})

test_that("specimen logo files ship with the app", {
  for (f in c("geome_g.png", "gbif_leaf.png")) {
    expect_true(file.exists(app_sys("app/www/specimen", f)))
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
                   status = c("conflict", "not checked", "single", NA))
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
