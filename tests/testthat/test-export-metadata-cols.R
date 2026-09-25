test_that("export_metadata_cols keeps only the user's mapping-file columns", {
  sample_cols <- c("ID", "Taxon", "genetic_code", "topology", "R1", "R2",
                   "Donors", "Expected", "Assembly", "Topology", "assembly")
  declared <- c(".selection", "ID", "path", "Taxon", "genetic_code", "curate_opts",
                "export_group")
  expect_equal(export_metadata_cols(sample_cols, declared), c("Donors", "Expected"))
})

test_that("export_metadata_cols is empty when the mapping file had no extras", {
  expect_equal(
    export_metadata_cols(c("ID", "Taxon", "R1", "R2"), c("ID", "Taxon")),
    character(0)
  )
})

test_that("the Export column picker offers a Metadata group, on by default", {
  expect_true("Metadata" %in% names(EXPORT_COL_GROUPS))
})

test_that("the rendered Export table shows mapping-file columns in the Metadata group by default", {
  proj <- withr::local_tempdir()
  suppressMessages(new_test_project_userAsmb(path = proj, executor = "local", Rproj = FALSE))
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(proj, ".sqlite"))
  withr::defer(DBI::dbDisconnect(con))
  withr::local_options(MitoPilot.db = file.path(proj, ".sqlite"))

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
    expect_true("mv_map_Expected" %in% id[shown])
    expect_equal(id[grepl("mp-grp-Metadata", cls)], "mv_map_Expected")
    expect_false("Expected" %in% id[shown])
    expect_false(any(id[shown] %in% c("assembly", "R1", "R2")))
  })
})
