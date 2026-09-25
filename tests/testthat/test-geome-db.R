geome_test_db <- function() {
  f <- withr::local_tempfile(fileext = ".sqlite", .local_envir = parent.frame())
  con <- DBI::dbConnect(RSQLite::SQLite(), f)
  withr::defer(DBI::dbDisconnect(con), envir = parent.frame())
  DBI::dbWriteTable(con, "samples", data.frame(ID = c("s1", "s2", "s3"), Taxon = "x"))
  con
}

n_recs <- function(con, id) {
  DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM meta_records WHERE ID = ? AND source = 'GEOME'",
                  params = list(id))$n
}

test_that(".meta_ensure_tables is idempotent and adds GEOME_BCID", {
  con <- geome_test_db()
  .meta_ensure_tables(con)
  .meta_ensure_tables(con)
  expect_true(all(c("meta_records", "meta_status", "meta_export_fields") %in% DBI::dbListTables(con)))
  expect_true("GEOME_BCID" %in% DBI::dbListFields(con, "samples"))
})

test_that(".meta_store_value keeps bad GEOME input visible and blanks as NA", {
  expect_equal(.meta_store_value("GEOME", c(" https://n2t.net/ark:/21547/X ", "junk", "", NA)),
               c("ark:/21547/X", "junk", NA, NA))
})

test_that(".meta_set_ref drops old records on a changed BCID, keeps them on same BCID", {
  con <- geome_test_db()
  .meta_ensure_tables(con)
  .meta_set_ref(con, "GEOME", "s1", "ark:/21547/A")
  DBI::dbExecute(con, "INSERT INTO meta_records VALUES ('s1', 'GEOME', 'Tissue', 0, 'ark:/21547/A', 'f', 'v')")
  DBI::dbExecute(con, "INSERT INTO meta_status VALUES ('s1', 'GEOME', 'ark:/21547/A', 'ok', NULL, 1)")
  .meta_set_ref(con, "GEOME", "s1", "ark:/21547/A")
  expect_equal(n_recs(con, "s1"), 1L)
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM meta_status WHERE ID='s1'")$n, 1L)
  .meta_set_ref(con, "GEOME", "s1", "ark:/21547/B")
  expect_equal(n_recs(con, "s1"), 0L)
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM meta_status WHERE ID='s1'")$n, 0L)
})

test_that(".meta_set_ref then a failing fetch leaves no stale records", {
  local_mocked_bindings(.geome_get = geome_fixture_get)
  con <- geome_test_db()
  .meta_ensure_tables(con)
  DBI::dbExecute(con, "INSERT INTO meta_records VALUES ('s1', 'GEOME', 'Tissue', 0, 'ark:/21547/A', 'f', 'v')")
  DBI::dbExecute(con, "INSERT INTO meta_status VALUES ('s1', 'GEOME', 'ark:/21547/A', 'ok', NULL, 1)")
  val <- .meta_set_ref(con, "GEOME", "s1", "ark:/21547/NOPE")
  suppressWarnings(.meta_fetch_into(con, "GEOME", "s1", val))
  expect_equal(n_recs(con, "s1"), 0L)
  expect_equal(DBI::dbGetQuery(con, "SELECT status FROM meta_status WHERE ID='s1'")$status, "failed")
})

test_that(".meta_fetch_into stores GEOME records and ok status", {
  local_mocked_bindings(.geome_get = geome_fixture_get)
  con <- geome_test_db()
  .meta_fetch_into(con, "GEOME", "s1", "ark:/21547/CYC2CMPI38181.1")
  st <- DBI::dbGetQuery(con, "SELECT * FROM meta_status")
  expect_equal(st$status, "ok")
  expect_equal(st$source, "GEOME")
  expect_true(is.na(st$message))
  expect_gt(n_recs(con, "s1"), 10)
})

test_that("failures are per sample, warned once, and keep prior data", {
  local_mocked_bindings(.geome_get = geome_fixture_get)
  con <- geome_test_db()
  .meta_fetch_into(con, "GEOME", "s1", "ark:/21547/CYC2CMPI38181.1")
  before <- n_recs(con, "s1")
  expect_warning(
    res <- .meta_fetch_into(con, "GEOME", c("s1", "s2", "s3"),
                            c("ark:/21547/NOPE", "ark:/21547/CYB2REEDY", "junk")),
    "GEOME fetch failed for .*(s1.*s3|s3.*s1)"
  )
  expect_equal(res$status, c("failed", "ok", "failed"))
  expect_match(res$message[3], "not a GEOME BCID")
  expect_equal(n_recs(con, "s1"), before)
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
  expect_equal(DBI::dbGetQuery(con, "SELECT ID FROM meta_status")$ID, "s2")
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
