remove_proj <- function(envir = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = envir)
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(dir, ".sqlite"))
  on.exit(DBI::dbDisconnect(con))
  DBI::dbWriteTable(con, "samples", data.frame(ID = c("s1", "SRR2"), Taxon = "x",
                                               country = c("USA", "Peru"), voucher = c("UW:1", "UW:2")))
  .meta_ensure_tables(con)
  DBI::dbExecute(con, "UPDATE samples SET GEOME_BCID = 'ark:/21547/MINE', GBIF_ID = '77' WHERE ID = 's1'")
  DBI::dbExecute(con, "UPDATE samples SET BioSample = 'SRR2' WHERE ID = 'SRR2'")
  DBI::dbExecute(con, "INSERT INTO meta_links VALUES ('s1', 'GBIF', '77', 'NCBI BioSample voucherURI', NULL)")
  DBI::dbExecute(con, "INSERT INTO meta_links VALUES ('SRR2', 'GEOME', NULL, NULL, '2 possible matches')")
  for (id in c("s1", "SRR2")) for (src in c("GEOME", "GBIF", "NCBI")) {
    DBI::dbAppendTable(con, "meta_records", data.frame(ID = id, source = src, level = "L", depth = 0L,
                                                       ref = "r", field = "f", value = "v"))
    DBI::dbAppendTable(con, "meta_status", data.frame(ID = id, source = src, ref = "r", status = "ok",
                                                      message = NA_character_, fetched_at = 1L))
  }
  .meta_save_fields(con, "ncbi:combo:sex")
  dir
}
rq <- function(dir, sql) {
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(dir, ".sqlite"))
  on.exit(DBI::dbDisconnect(con))
  DBI::dbGetQuery(con, sql)
}

test_that("removing one source for one sample keeps mapping data and user IDs", {
  dir <- remove_proj()
  before <- rq(dir, "SELECT * FROM samples ORDER BY ID")
  remove_metadata(dir, sources = "GBIF", ids = "s1")
  after <- rq(dir, "SELECT * FROM samples ORDER BY ID")
  expect_true(is.na(after$GBIF_ID[after$ID == "s1"]))
  expect_identical(after[setdiff(names(after), "GBIF_ID")], before[setdiff(names(before), "GBIF_ID")])
  left <- rq(dir, "SELECT ID, source FROM meta_records ORDER BY ID, source")
  expect_equal(nrow(left), 5L)
  expect_false(any(left$ID == "s1" & left$source == "GBIF"))
  expect_equal(rq(dir, "SELECT COUNT(*) n FROM meta_links WHERE source = 'GBIF'")$n, 0L)
})

test_that("removing everything keeps CSV columns, user IDs, and field picks", {
  dir <- remove_proj()
  before <- rq(dir, "SELECT * FROM samples ORDER BY ID")
  remove_metadata(dir)
  after <- rq(dir, "SELECT * FROM samples ORDER BY ID")
  keep <- setdiff(names(after), "GBIF_ID")
  expect_identical(after[keep], before[keep])
  expect_equal(after$GEOME_BCID[after$ID == "s1"], "ark:/21547/MINE")
  expect_equal(after$BioSample[after$ID == "SRR2"], "SRR2")
  expect_equal(rq(dir, "SELECT COUNT(*) n FROM meta_records")$n, 0L)
  expect_equal(rq(dir, "SELECT COUNT(*) n FROM meta_status")$n, 0L)
  expect_equal(rq(dir, "SELECT COUNT(*) n FROM meta_links")$n, 0L)
  expect_equal(rq(dir, "SELECT key FROM meta_export_fields")$key, "ncbi:combo:sex")
})

test_that("remove_metadata rejects unknown sources and samples", {
  dir <- remove_proj()
  expect_error(remove_metadata(dir, sources = "BOLD"), "unknown source")
  expect_error(remove_metadata(dir, ids = "nope"), "not in this project")
})
