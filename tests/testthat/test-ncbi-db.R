ncbi_proj <- function(ids = c("SRR21844202", "s2"), envir = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = envir)
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(dir, ".sqlite"))
  DBI::dbWriteTable(con, "samples", data.frame(ID = ids, Taxon = "Fundulus majalis"))
  DBI::dbDisconnect(con)
  dir
}
q <- function(dir, sql) {
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(dir, ".sqlite"))
  on.exit(DBI::dbDisconnect(con))
  DBI::dbGetQuery(con, sql)
}

test_that(".meta_ensure_tables adds a BioSample column", {
  dir <- ncbi_proj()
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(dir, ".sqlite"))
  on.exit(DBI::dbDisconnect(con))
  .meta_ensure_tables(con)
  expect_true("BioSample" %in% DBI::dbListFields(con, "samples"))
})

test_that("fetch_biosample sets IDs and stores records", {
  local_mocked_bindings(.ncbi_get = ncbi_fixture_get)
  dir <- ncbi_proj()
  res <- fetch_biosample(dir, ids = "s2", biosamples = "samn63236902")
  expect_equal(res$status, "ok")
  expect_equal(q(dir, "SELECT BioSample FROM samples WHERE ID = 's2'")$BioSample, "SAMN63236902")
  r <- q(dir, "SELECT DISTINCT level FROM meta_records WHERE ID = 's2' AND source = 'NCBI'")
  expect_setequal(r$level, c("BioSample", "BioProject"))
})

test_that("from_id copies valid sample IDs only", {
  local_mocked_bindings(.ncbi_get = ncbi_fixture_get)
  dir <- ncbi_proj()
  expect_message(res <- fetch_biosample(dir, from_id = TRUE), "s2")
  expect_equal(q(dir, "SELECT ID, BioSample FROM samples ORDER BY ID")$BioSample,
               c("SRR21844202", NA))
  expect_equal(res$ID, "SRR21844202")
  expect_equal(res$status, "ok")
})

test_that("a failed refresh keeps earlier NCBI data", {
  local_mocked_bindings(.ncbi_get = ncbi_fixture_get)
  dir <- ncbi_proj()
  fetch_biosample(dir, ids = "s2", biosamples = "SAMN63236902")
  n <- q(dir, "SELECT COUNT(*) n FROM meta_records WHERE ID = 's2'")$n
  local_mocked_bindings(.ncbi_get = function(...) stop("could not reach NCBI (offline)", call. = FALSE))
  expect_warning(fetch_biosample(dir, ids = "s2"), "offline")
  expect_equal(q(dir, "SELECT COUNT(*) n FROM meta_records WHERE ID = 's2'")$n, n)
})

test_that(".meta_take_cols keeps the ID column when it doubles as BioSample", {
  m <- data.frame(ID = c("SRR1", "SRR2"), Taxon = "x")
  out <- .meta_take_cols(m, c(NCBI = "ID"))
  expect_equal(out$ID, c("SRR1", "SRR2"))
  expect_equal(out$BioSample, c("SRR1", "SRR2"))
  m2 <- data.frame(run = c("SRR1", "SRR2"), ID = c("SRR1", "SRR2"), Taxon = "x")
  out2 <- .meta_take_cols(m2, c(NCBI = "run"))
  expect_equal(out2$BioSample, c("SRR1", "SRR2"))
})
