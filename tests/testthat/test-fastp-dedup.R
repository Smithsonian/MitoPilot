test_that(".fastp_set_dedup swaps the duplicate handling flag both ways", {
  base <- "--trim_poly_g --correction --detect_adapter_for_pe --dont_eval_duplication"
  on <- .fastp_set_dedup(base, TRUE)
  expect_true(grepl("--dedup", on, fixed = TRUE))
  expect_false(grepl("--dont_eval_duplication", on, fixed = TRUE))

  off <- .fastp_set_dedup(on, FALSE)
  expect_equal(off, base)
})

test_that(".fastp_set_dedup adds a flag to a string that has neither", {
  expect_equal(.fastp_set_dedup("--correction", TRUE), "--correction --dedup")
  expect_equal(
    .fastp_set_dedup("--correction", FALSE),
    "--correction --dont_eval_duplication"
  )
})

test_that(".fastp_set_dedup is idempotent and normalizes whitespace", {
  expect_equal(
    .fastp_set_dedup("  --correction   --dedup  --dedup ", TRUE),
    "--correction --dedup"
  )
  once <- .fastp_set_dedup("--correction --dedup", FALSE)
  expect_equal(.fastp_set_dedup(once, FALSE), once)
  expect_equal(.fastp_set_dedup("", TRUE), "--dedup")
})

fastp_test_db <- function(dir, ...) {
  mapping <- file.path(dir, "mapping.csv")
  utils::write.csv(
    data.frame(ID = "S1", Taxon = "Danio rerio",
               R1 = "S1_R1.fastq.gz", R2 = "S1_R2.fastq.gz"),
    mapping, row.names = FALSE
  )
  db <- file.path(dir, ".sqlite")
  new_db(db_path = db, mapping_fn = mapping, ...)
  db
}

fastp_test_opts <- function(db) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbGetQuery(con, "SELECT fastp FROM pre_opts")$fastp
}

test_that("new_db keeps deduplication off by default", {
  d <- withr::local_tempdir()
  fastp <- fastp_test_opts(fastp_test_db(d))
  expect_true(grepl("--dont_eval_duplication", fastp, fixed = TRUE))
  expect_false(grepl("--dedup", fastp, fixed = TRUE))
})

test_that("new_db(dedup = TRUE) stores --dedup", {
  d <- withr::local_tempdir()
  fastp <- fastp_test_opts(fastp_test_db(d, dedup = TRUE))
  expect_true(grepl("--dedup", fastp, fixed = TRUE))
  expect_false(grepl("--dont_eval_duplication", fastp, fixed = TRUE))
})
