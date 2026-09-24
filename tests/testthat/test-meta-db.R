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
