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

test_that("showing source fields on an empty table does not crash", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbWriteTable(con, "samples", data.frame(ID = c("s1", "s2"), Taxon = "x"))
  .meta_ensure_tables(con)
  DBI::dbAppendTable(con, "meta_records", data.frame(
    ID = c("s1", "s2"), source = "NCBI", level = "BioSample", depth = 1L, ref = "SAMN1",
    field = "sex", value = "female"))
  f <- meta_view_fields(con)
  f$shown <- TRUE
  out <- meta_view_join(data.frame(ID = character(), x = character()), con, f)
  expect_equal(nrow(out), 0L)
  expect_equal(nrow(meta_export_cols(con, ids = character(), keys = "ncbi:combo:sex")), 0L)
})

test_that("meta_view_fields carries export state, sample totals, and template text", {
  con <- meta_view_db()
  on.exit(DBI::dbDisconnect(con))
  .meta_save_fields(con, "gbif:raw:Occurrence:sex")
  f <- meta_view_fields(con)
  expect_true(all(f$n_total == 2L))
  expect_true(is.na(f$export[f$key == "map:country"]))
  expect_true(f$export[f$key == "gbif:raw:Occurrence:sex"])
  expect_equal(f$token[f$key == "map:country"], "{country}")
  expect_equal(f$token[f$key == "gbif:raw:Occurrence:sex"], "{gbif_Occurrence_sex}")
  expect_true(meta_view_link(con))
})

test_that("meta_view_save writes export keys and the link option, keeping unlisted keys", {
  con <- meta_view_db()
  on.exit(DBI::dbDisconnect(con))
  .meta_save_fields(con, c("ncbi:combo:lat_lon", "gbif:raw:Occurrence:sex"))
  f <- meta_view_fields(con)
  meta_view_save(con, f, "map:country", wrap = FALSE,
                 export_keys = c("geome:raw:Event:locality", "map:country"), link = FALSE)
  ex <- DBI::dbGetQuery(con, "SELECT key FROM meta_export_fields")$key
  expect_setequal(ex, c("ncbi:combo:lat_lon", "geome:raw:Event:locality"))
  expect_false(meta_view_link(con))
  expect_false(meta_view_wrap(con))
  meta_view_save(con, f, character(0), wrap = TRUE)
  expect_false(meta_view_link(con))
  expect_setequal(DBI::dbGetQuery(con, "SELECT key FROM meta_export_fields")$key,
                  c("ncbi:combo:lat_lon", "geome:raw:Event:locality"))
})

test_that("meta_view_modal has Show/Export controls and the picker table renders", {
  con <- meta_view_db()
  on.exit(DBI::dbDisconnect(con))
  f <- meta_view_fields(con)
  html <- as.character(meta_view_modal(shiny::NS("x"), f, wrap = FALSE, source = "GBIF",
                                       closed_input = "x-meta_view_closed"))
  expect_match(html, "Tick both together", fixed = TRUE)
  expect_match(html, "GenBank-ready", fixed = TRUE)
  expect_match(html, "\"source\":\"GBIF\"", fixed = TRUE)
  expect_match(html, "x-meta_view_closed", fixed = TRUE)
  expect_match(html, "mpMV.save(&#39;x-meta_view_tbl&#39;, &#39;x-meta_view_state&#39;)", fixed = TRUE)
  expect_s3_class(meta_view_picker_table(f, "x-meta_view_tbl"), "reactable")
})

test_that("meta_view_fields is empty when no field has data", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbWriteTable(con, "samples", data.frame(ID = c("s1", "s2"), Taxon = "x", R1 = "a", R2 = "b"))
  .meta_ensure_tables(con)
  f <- meta_view_fields(con)
  expect_equal(nrow(f), 0L)
  dat <- data.frame(ID = c("s1", "s2"), Taxon = "x")
  expect_equal(meta_view_join(dat, con), dat)
})
