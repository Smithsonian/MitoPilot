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
  m3 <- data.frame(ID = c("SRR1", "s2"), Taxon = "x")
  expect_equal(.meta_take_cols(m3, c(NCBI = "ID"))$BioSample, c("SRR1", NA))
  m4 <- data.frame(ID = c("s1", "s2"), BioSample = c("SRR1", "junk"), Taxon = "x")
  expect_equal(.meta_take_cols(m4, c(NCBI = "BioSample"))$BioSample, c("SRR1", "junk"))
})

ncbi_mapping <- function(dir) {
  m <- data.frame(ID = c("SRR21844202", "s2"), Taxon = "Fundulus majalis",
                  R1 = c("a_1.fq", "b_1.fq"), R2 = c("a_2.fq", "b_2.fq"))
  f <- file.path(dir, "mapping.csv")
  utils::write.csv(m, f, row.names = FALSE)
  f
}

test_that("check_mapping validates BioSample values and allows reusing the ID column", {
  m <- data.frame(ID = c("SRR21844202", "s2"), Taxon = "x", R1 = "a", R2 = "b")
  iss <- check_mapping(m, mapping_biosample = "ID")
  expect_match(paste(iss$warnings, collapse = " "), "not BioSample or SRA accessions for s2; no NCBI lookup", fixed = TRUE)
  expect_false(any(grepl("reserved", iss$errors)))
  iss2 <- check_mapping(cbind(m, BioSample = "SAMN1"), mapping_biosample = "ID")
  expect_match(paste(iss2$errors, collapse = " "), "reserved")
  iss3 <- check_mapping(m, mapping_biosample = "Nope")
  expect_match(paste(iss3$errors, collapse = " "), "Nope")
})

test_that("new_db stores BioSample from the ID column and fetches it", {
  local_mocked_bindings(.ncbi_get = ncbi_fixture_get)
  d <- withr::local_tempdir()
  new_db(db_path = file.path(d, ".sqlite"), mapping_fn = ncbi_mapping(d), mapping_biosample = "ID")
  s <- q(d, "SELECT ID, BioSample FROM samples ORDER BY ID")
  expect_equal(s$ID, c("SRR21844202", "s2"))
  expect_equal(s$BioSample, c("SRR21844202", NA))
  expect_equal(q(d, "SELECT ID, status FROM meta_status WHERE source = 'NCBI'")$status, "ok")
})

test_that("new_db fetch_biosample = FALSE stores IDs without calling NCBI", {
  local_mocked_bindings(.ncbi_get = function(...) stop("should not be called"))
  d <- withr::local_tempdir()
  new_db(db_path = file.path(d, ".sqlite"), mapping_fn = ncbi_mapping(d), mapping_biosample = "ID",
         fetch_biosample = FALSE)
  expect_equal(q(d, "SELECT COUNT(*) n FROM meta_status")$n, 0L)
})

test_that("the NCBI reachability check uses a real request, not HEAD", {
  seen <- NULL
  local_mocked_bindings(.ncbi_get = function(endpoint, query) { seen <<- endpoint; "<eInfoResult/>" })
  iss <- .issues()
  .check_ncbi_reachable(iss)
  expect_equal(seen, "einfo")
  expect_length(iss$warnings, 0)
  local_mocked_bindings(.ncbi_get = function(...) stop("could not reach NCBI (timeout)", call. = FALSE))
  .check_ncbi_reachable(iss)
  expect_match(iss$warnings, "NCBI: not reachable right now", fixed = TRUE)
})

test_that("plain-number sample IDs are never used as BioSample numbers", {
  expect_equal(ncbi_normalize_id(c("12345", "SRR1", "SAMN1"), strict = TRUE), c(NA, "SRR1", "SAMN1"))
  m <- data.frame(ID = c("12345", "SRR1"), Taxon = "x")
  expect_equal(.meta_take_cols(m, c(NCBI = "ID"))$BioSample, c(NA, "SRR1"))
  iss <- check_mapping(data.frame(ID = c("12345", "SRR1"), Taxon = "x", R1 = "a", R2 = "b"),
                       mapping_biosample = "ID")
  expect_match(paste(iss$warnings, collapse = " "), "accessions for 12345", fixed = TRUE)
  local_mocked_bindings(.ncbi_get = function(...) stop("should not be called"))
  dir <- ncbi_proj(ids = c("12345", "s2"))
  expect_message(res <- fetch_biosample(dir, from_id = TRUE), "12345")
  expect_equal(nrow(res), 0L)
})

test_that(".meta_take_cols keeps a renamed ID column and shared source columns", {
  m <- data.frame(SampleID = c("SRR1", "SRR2"), Taxon = "x")
  m$ID <- m$SampleID
  out <- .meta_take_cols(m, c(NCBI = "SampleID"), keep = "SampleID")
  expect_true("SampleID" %in% names(out))
  expect_equal(out$BioSample, c("SRR1", "SRR2"))
  m2 <- data.frame(ID = "s1", Taxon = "x", Acc = "123")
  out2 <- .meta_take_cols(m2, c(GBIF = "Acc", NCBI = "Acc"))
  expect_equal(out2$GBIF_ID, "123")
  expect_equal(out2$BioSample, "123")
  expect_false("Acc" %in% names(out2))
})
