meta_view_db <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(con, "samples", data.frame(
    ID = c("s1", "s2"), Taxon = "x", R1 = "a", R2 = "b",
    country = c("Peru", NA), `lat lon` = c("", ""), check.names = FALSE))
  .meta_ensure_tables(con)
  DBI::dbAppendTable(con, "meta_records", data.frame(
    ID = c("s1", "s2", "s1"), source = c("GBIF", "GBIF", "GEOME"),
    level = c("Occurrence", "Occurrence", "Event"), depth = c(0L, 0L, 2L), ref = "r",
    field = c("sex", "sex", "locality"), value = c("Female", "Male", "Cusco")))
  con
}

test_that("meta_view_fields lists fields with data, map file first, map file on by default", {
  con <- meta_view_db()
  on.exit(DBI::dbDisconnect(con))
  f <- meta_view_fields(con)
  expect_equal(unique(f$source), c("Map file", "GEOME", "GBIF"))
  expect_false("map:lat lon" %in% f$key)
  expect_false(any(c("map:R1", "map:Taxon") %in% f$key))
  expect_equal(f$shown, f$source == "Map file")
  expect_true(all(startsWith(f$col, "mv_")))
  expect_false(anyDuplicated(f$col) > 0)
  expect_equal(f$n_samples[f$key == "gbif:raw:Occurrence:sex"], 2L)
})

test_that("meta_view_save keeps hidden map fields and shown specimen fields, plus wrap", {
  con <- meta_view_db()
  on.exit(DBI::dbDisconnect(con))
  f <- meta_view_fields(con)
  meta_view_save(con, f, "gbif:raw:Occurrence:sex", wrap = TRUE)
  f2 <- meta_view_fields(con)
  expect_equal(f2$key[f2$shown], "gbif:raw:Occurrence:sex")
  expect_true(meta_view_wrap(con))
  DBI::dbExecute(con, "ALTER TABLE samples ADD COLUMN habitat TEXT")
  DBI::dbExecute(con, "UPDATE samples SET habitat = 'reef'")
  f3 <- meta_view_fields(con)
  expect_true(f3$shown[f3$key == "map:habitat"])
  meta_view_save(con, f3, character(0), wrap = FALSE)
  expect_false(meta_view_wrap(con))
  expect_false(any(meta_view_fields(con)$shown & meta_view_fields(con)$source != "Map file"))
})

test_that("meta_view_join adds shown columns before the notes columns", {
  con <- meta_view_db()
  on.exit(DBI::dbDisconnect(con))
  f <- meta_view_fields(con)
  meta_view_save(con, f, c("map:country", "gbif:raw:Occurrence:sex", "geome:raw:Event:locality"), FALSE)
  dat <- data.frame(ID = c("s2", "s1", "s1"), Taxon = "x", time_stamp = 1, output = "o")
  out <- meta_view_join(dat, con)
  f <- meta_view_fields(con)
  cols <- f$col[f$shown]
  expect_equal(names(out), c("ID", "Taxon", cols, "time_stamp", "output"))
  expect_equal(out[[f$col[f$key == "map:country"]]], c("", "Peru", "Peru"))
  expect_equal(out[[f$col[f$key == "gbif:raw:Occurrence:sex"]]], c("Male", "Female", "Female"))
  expect_equal(out[[f$col[f$key == "geome:raw:Event:locality"]]], c("", "Cusco", "Cusco"))
  expect_equal(names(meta_view_join(out, con)), names(out))
})

test_that("meta_view_col_defs puts logos in GEOME/GBIF headers and tags the Metadata group", {
  con <- meta_view_db()
  on.exit(DBI::dbDisconnect(con))
  f <- meta_view_fields(con)
  f$shown <- TRUE
  defs <- meta_view_col_defs(f, wrap = TRUE)
  expect_equal(names(defs), f$col)
  gb <- defs[[f$col[f$key == "gbif:raw:Occurrence:sex"]]]
  expect_match(as.character(gb$header), "mp-meta-logo-gbif", fixed = TRUE)
  expect_match(as.character(gb$header), "GBIF &gt; Occurrence &gt; sex", fixed = TRUE)
  expect_match(gb$className, "mp-grp-Metadata")
  expect_match(gb$className, "mp-meta-wrap")
  expect_true(gb$resizable)
  mp <- defs[[f$col[f$key == "map:country"]]]
  expect_false(grepl("mp-meta-logo", as.character(mp$header)))
  expect_false(grepl("mp-meta-wrap", meta_view_col_defs(f)[[1]]$className))
})

test_that("every table offers a Metadata column group", {
  for (g in list(ASSEMBLE_COL_GROUPS, ASSEMBLE_COL_GROUPS_USERASMB, ANNOTATE_COL_GROUPS, EXPORT_COL_GROUPS)) {
    expect_true("Metadata" %in% names(g))
  }
})
