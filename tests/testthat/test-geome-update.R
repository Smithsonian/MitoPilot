geome_project <- function(bcids = c("ark:/21547/CYB2REEDY", "")) {
  d <- withr::local_tempdir(.local_envir = parent.frame())
  m <- data.frame(ID = c("s1", "s2"), Taxon = "x", R1 = c("a_1", "b_1"),
                  R2 = c("a_2", "b_2"), GEOME_BCID = bcids)
  utils::write.csv(m, file.path(d, "mapping.csv"), row.names = FALSE)
  new_db(db_path = file.path(d, ".sqlite"), mapping_fn = file.path(d, "mapping.csv"))
  d
}

geome_q <- function(d, sql) {
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(d, ".sqlite"))
  on.exit(DBI::dbDisconnect(con))
  DBI::dbGetQuery(con, sql)
}

test_that("add_samples stores and fetches BCIDs for new samples only", {
  local_mocked_bindings(.geome_get = geome_fixture_get)
  d <- geome_project()
  up <- data.frame(ID = "s3", Taxon = "x", R1 = "c_1", R2 = "c_2",
                   MyBcid = "https://n2t.net/ark:/21547/CYA2Reedy01")
  utils::write.csv(up, file.path(d, "up.csv"), row.names = FALSE)
  add_samples(d, file.path(d, "up.csv"), mapping_geome = "MyBcid")
  expect_equal(geome_q(d, "SELECT GEOME_BCID FROM samples WHERE ID='s3'")$GEOME_BCID,
               "ark:/21547/CYA2Reedy01")
  expect_false("MyBcid" %in% names(geome_q(d, "SELECT * FROM samples")))
  expect_setequal(geome_q(d, "SELECT ID FROM geome_status")$ID, c("s1", "s3"))
})

test_that("update_sample_metadata refetches changed, drops cleared, skips unchanged", {
  local_mocked_bindings(.geome_get = geome_fixture_get)
  d <- geome_project(bcids = c("ark:/21547/CYB2REEDY", "ark:/21547/CYA2Reedy01"))
  t0 <- geome_q(d, "SELECT fetched_at FROM geome_status WHERE ID='s1'")$fetched_at
  geome_reset_calls()
  up <- data.frame(ID = c("s1", "s2"), Taxon = "x",
                   GEOME_BCID = c("ark:/21547/CYB2REEDY", ""))
  utils::write.csv(up, file.path(d, "up.csv"), row.names = FALSE)
  update_sample_metadata(d, file.path(d, "up.csv"))
  expect_equal(geome_q(d, "SELECT ID FROM geome_status")$ID, "s1")
  expect_equal(geome_q(d, "SELECT COUNT(*) n FROM geome_records WHERE ID='s2'")$n, 0L)
  expect_equal(length(ls(geome_calls)), 0L)
  up$GEOME_BCID <- c("ark:/21547/CYC2CMPI38181.1", "")
  utils::write.csv(up, file.path(d, "up.csv"), row.names = FALSE)
  update_sample_metadata(d, file.path(d, "up.csv"))
  lv <- geome_q(d, "SELECT DISTINCT level FROM geome_records WHERE ID='s1'")$level
  expect_true("Tissue" %in% lv)
})

test_that("update_sample_metadata with fetch_geome = FALSE drops stale records for a changed BCID", {
  local_mocked_bindings(.geome_get = geome_fixture_get)
  d <- geome_project(bcids = c("ark:/21547/CYB2REEDY", ""))
  up <- data.frame(ID = c("s1", "s2"), Taxon = "x",
                   GEOME_BCID = c("ark:/21547/CYA2Reedy01", ""))
  utils::write.csv(up, file.path(d, "up.csv"), row.names = FALSE)
  update_sample_metadata(d, file.path(d, "up.csv"), fetch_geome = FALSE)
  expect_equal(geome_q(d, "SELECT COUNT(*) n FROM geome_records WHERE ID='s1'")$n, 0L)
  expect_equal(geome_q(d, "SELECT COUNT(*) n FROM geome_status WHERE ID='s1'")$n, 0L)
})

test_that("update CSV without a BCID column leaves GEOME data alone", {
  local_mocked_bindings(.geome_get = geome_fixture_get)
  d <- geome_project()
  up <- data.frame(ID = "s1", Taxon = "y")
  utils::write.csv(up, file.path(d, "up.csv"), row.names = FALSE)
  update_sample_metadata(d, file.path(d, "up.csv"))
  expect_equal(geome_q(d, "SELECT ID FROM geome_status")$ID, "s1")
})
