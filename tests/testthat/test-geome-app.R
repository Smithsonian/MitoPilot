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

test_that(".geome_status_join ensures GEOME tables on a pre-branch DB with no geome tables", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbWriteTable(con, "samples", data.frame(ID = c("a", "b"), Taxon = "x"))
  out <- dplyr::tbl(con, "samples") |> .geome_status_join(con) |> dplyr::collect()
  expect_equal(out$geome, c("none", "none"))
})

test_that("rt_geome sends the row ID to the given input", {
  js <- as.character(rt_geome("assemble-geome_open"))
  expect_match(js, "assemble-geome_open", fixed = TRUE)
  expect_match(js, "setInputValue", fixed = TRUE)
  expect_match(js, "dataset.id", fixed = TRUE)
})

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

test_that("the Export column picker offers a GEOME group, and ticked GEOME
          fields render as a GEOME column group in the Export table", {
  proj <- withr::local_tempdir()
  suppressMessages(new_test_project_userAsmb(path = proj, executor = "local", Rproj = FALSE))
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(proj, ".sqlite"))
  withr::defer(DBI::dbDisconnect(con))
  withr::local_options(MitoPilot.db = file.path(proj, ".sqlite"))
  .geome_ensure_tables(con)
  id1 <- DBI::dbGetQuery(con, "SELECT ID FROM samples LIMIT 1")$ID
  DBI::dbAppendTable(con, "geome_records", data.frame(
    ID = id1, level = "Event", depth = 0L, bcid = NA_character_,
    field = "country", value = "Peru"))
  # Fields ticked ahead of time, as the picker's Save handler would leave them
  # (session$setInputValue + observeEvent(input$x) does not fire reliably
  # under shiny::testServer in this environment, even for a bare module with
  # no MitoPilot code involved; see task-9-report.md).
  .geome_save_fields(con, c("combo:lat_lon", "raw:Event:country"))

  expect_true("GEOME" %in% names(EXPORT_COL_GROUPS))

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
    expect_setequal(id[grepl("mp-grp-GEOME", cls)], c("geome", "geome_lat_lon", "geome_Event_country"))
  })
})

test_that("geome_fields_modal builds a checkbox list and a raw-fields reactable", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbWriteTable(con, "samples", data.frame(ID = "s1", Taxon = "x"))
  .geome_ensure_tables(con)
  DBI::dbAppendTable(con, "geome_records", data.frame(
    ID = "s1", level = "Event", depth = 0L, bcid = NA_character_,
    field = "country", value = "Peru"))
  .geome_save_fields(con, "combo:lat_lon")

  s <- geome_field_summary(con)
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

test_that("panel module servers start outside a reactive context", {
  start <- function(maker, servers) {
    proj <- withr::local_tempdir()
    suppressMessages(maker(path = proj, executor = "local", Rproj = FALSE))
    con <- DBI::dbConnect(RSQLite::SQLite(), file.path(proj, ".sqlite"))
    withr::defer(DBI::dbDisconnect(con))
    withr::local_options(MitoPilot.db = file.path(proj, ".sqlite"))
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
