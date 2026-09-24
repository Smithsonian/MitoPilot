gbif_csv <- function(dir, col = "GBIF_ID", ids = c("6186461308", ""), extra = list()) {
  m <- data.frame(ID = c("s1", "s2"), Taxon = "Fish a", R1 = c("a_1.fq", "b_1.fq"),
                  R2 = c("a_2.fq", "b_2.fq"))
  m[[col]] <- ids
  for (nm in names(extra)) m[[nm]] <- extra[[nm]]
  f <- file.path(dir, "mapping.csv")
  utils::write.csv(m, f, row.names = FALSE)
  f
}

gbif_q <- function(d, sql) {
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(d, ".sqlite"))
  on.exit(DBI::dbDisconnect(con))
  DBI::dbGetQuery(con, sql)
}

gbif_mem_db <- function() {
  f <- withr::local_tempfile(fileext = ".sqlite", .local_envir = parent.frame())
  con <- DBI::dbConnect(RSQLite::SQLite(), f)
  withr::defer(DBI::dbDisconnect(con), envir = parent.frame())
  DBI::dbWriteTable(con, "samples", data.frame(ID = c("s1", "s2"), Taxon = "x"))
  con
}

test_that(".meta_ensure_tables adds GBIF_ID and GBIF values normalize", {
  con <- gbif_mem_db()
  .meta_ensure_tables(con)
  expect_true(all(c("GEOME_BCID", "GBIF_ID") %in% DBI::dbListFields(con, "samples")))
  expect_equal(.meta_store_value("GBIF", c(" https://www.gbif.org/occurrence/6186461308 ", "junk", "", NA)),
               c("6186461308", "junk", NA, NA))
})

test_that(".meta_fetch_into stores GBIF levels next to GEOME and a refetch failure keeps data", {
  local_mocked_bindings(.gbif_get = gbif_fixture_get, .geome_get = geome_fixture_get)
  con <- gbif_mem_db()
  .meta_fetch_into(con, "GEOME", "s1", "ark:/21547/CYB2REEDY")
  .meta_fetch_into(con, "GBIF", "s1", "6186461308")
  lv <- DBI::dbGetQuery(con, "SELECT DISTINCT level FROM meta_records WHERE source = 'GBIF'")$level
  expect_setequal(lv, c("Occurrence", "Dataset", "Organization"))
  expect_setequal(DBI::dbGetQuery(con, "SELECT source FROM meta_status")$source, c("GEOME", "GBIF"))
  n <- DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM meta_records WHERE source = 'GBIF'")$n
  local_mocked_bindings(.gbif_get = function(path) stop("could not reach GBIF (timeout)", call. = FALSE))
  expect_warning(.meta_fetch_into(con, "GBIF", "s1", "6186461308"), "GBIF fetch failed for s1")
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM meta_records WHERE source = 'GBIF'")$n, n)
  st <- DBI::dbGetQuery(con, "SELECT status, message FROM meta_status WHERE source = 'GBIF'")
  expect_equal(st$status, "failed")
  expect_match(st$message, "could not reach GBIF")
  expect_gt(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM meta_records WHERE source = 'GEOME'")$n, 0)
})

test_that("a non-numeric GBIF ID is marked failed without calling GBIF", {
  local_mocked_bindings(.gbif_get = function(...) stop("should not be called"))
  con <- gbif_mem_db()
  res <- suppressWarnings(.meta_fetch_into(con, "GBIF", "s1", "abc"))
  expect_equal(res$status, "failed")
  expect_match(res$message, "not a GBIF occurrence ID")
})

test_that("fetch_gbif sets, fetches, and clears IDs without touching GEOME", {
  local_mocked_bindings(.gbif_get = gbif_fixture_get)
  d <- withr::local_tempdir()
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(d, ".sqlite"))
  DBI::dbWriteTable(con, "samples", data.frame(ID = c("s1", "s2"), Taxon = "x", GEOME_BCID = "ark:/1/A"))
  .meta_ensure_tables(con)
  DBI::dbExecute(con, "INSERT INTO meta_status VALUES ('s1', 'GEOME', 'ark:/1/A', 'ok', NULL, 1)")
  DBI::dbDisconnect(con)
  fetch_gbif(d, ids = c("s1", "s2"),
             gbifs = c("https://www.gbif.org/occurrence/6186461308", "2336663130"))
  expect_equal(gbif_q(d, "SELECT GBIF_ID FROM samples ORDER BY ID")$GBIF_ID, c("6186461308", "2336663130"))
  fetch_gbif(d, ids = "s1", gbifs = "")
  expect_true(is.na(gbif_q(d, "SELECT GBIF_ID FROM samples WHERE ID = 's1'")$GBIF_ID))
  expect_equal(gbif_q(d, "SELECT ID FROM meta_status WHERE source = 'GBIF'")$ID, "s2")
  expect_equal(gbif_q(d, "SELECT ID FROM meta_status WHERE source = 'GEOME'")$ID, "s1")
  expect_error(fetch_gbif(d, ids = c("s1", "s2"), gbifs = "1"), "ids and gbifs must be the same length")
  expect_message(res <- fetch_gbif(d, ids = "s1"), "No samples with a GBIF ID to fetch")
})

test_that("new_db stores numeric GBIF IDs from the CSV as digit strings", {
  local_mocked_bindings(.gbif_get = function(...) stop("should not be called"))
  d <- withr::local_tempdir()
  f <- gbif_csv(d, ids = c("6186461308", ""))
  expect_true(is.numeric(utils::read.csv(f)$GBIF_ID))
  new_db(db_path = file.path(d, ".sqlite"), mapping_fn = f, fetch_gbif = FALSE)
  expect_equal(gbif_q(d, "SELECT GBIF_ID FROM samples ORDER BY ID")$GBIF_ID, c("6186461308", NA))
  expect_equal(gbif_q(d, "SELECT COUNT(*) n FROM meta_status")$n, 0L)
})

test_that("new_db renames a custom GBIF column and fetches it", {
  local_mocked_bindings(.gbif_get = gbif_fixture_get)
  d <- withr::local_tempdir()
  f <- gbif_csv(d, col = "Occ", ids = c("https://www.gbif.org/occurrence/6186461308", ""))
  new_db(db_path = file.path(d, ".sqlite"), mapping_fn = f, mapping_gbif = "Occ")
  s <- gbif_q(d, "SELECT * FROM samples ORDER BY ID")
  expect_false("Occ" %in% names(s))
  expect_equal(s$GBIF_ID, c("6186461308", NA))
  st <- gbif_q(d, "SELECT ID, source, status FROM meta_status")
  expect_equal(st$ID, "s1")
  expect_equal(st$source, "GBIF")
  expect_equal(st$status, "ok")
})

test_that("new_db_userAsmb stores GBIF IDs and respects fetch_gbif = FALSE", {
  local_mocked_bindings(.gbif_get = function(...) stop("should not be called"))
  d <- withr::local_tempdir()
  mapping_fn <- file.path(d, "mapping.csv")
  utils::write.csv(
    data.frame(ID = c("s1", "s2"), Taxon = "Danio rerio",
               Assembly = c("s1.fasta", "s2.fasta"), Topology = c("linear", "circular"),
               GBIF_ID = c("6186461308", "")),
    mapping_fn, row.names = FALSE
  )
  new_db_userAsmb(db_path = file.path(d, ".sqlite"), mapping_fn = mapping_fn,
                  no_raw_data = TRUE, fetch_gbif = FALSE)
  expect_equal(gbif_q(d, "SELECT GBIF_ID FROM samples ORDER BY ID")$GBIF_ID, c("6186461308", NA))
})

test_that("check_mapping warns on a non-numeric GBIF ID and guards the reserved name", {
  m <- data.frame(ID = "s1", Taxon = "x", R1 = "a", R2 = "b", Occ = "abc")
  iss <- check_mapping(m, mapping_gbif = "Occ")
  expect_match(paste(iss$warnings, collapse = " "), "GBIF")
  iss <- check_mapping(m, mapping_gbif = "Nope")
  expect_match(paste(iss$errors, collapse = " "), "Nope")
  m$GBIF_ID <- "1"
  iss <- check_mapping(m, mapping_gbif = "Occ")
  expect_match(paste(iss$errors, collapse = " "), "GBIF_ID")
  iss <- check_mapping(data.frame(ID = "s1", Taxon = "x", R1 = "a", R2 = "b", GBIF_ID = 6186461308))
  expect_false(any(grepl("GBIF", iss$warnings)))
})

test_that("add_samples stores and fetches GBIF IDs for new samples", {
  local_mocked_bindings(.gbif_get = gbif_fixture_get)
  d <- withr::local_tempdir()
  new_db(db_path = file.path(d, ".sqlite"), mapping_fn = gbif_csv(d, ids = c("", "")))
  up <- data.frame(ID = "s3", Taxon = "x", R1 = "c_1", R2 = "c_2",
                   Occ = "https://www.gbif.org/occurrence/2336663130")
  utils::write.csv(up, file.path(d, "up.csv"), row.names = FALSE)
  add_samples(d, file.path(d, "up.csv"), mapping_gbif = "Occ")
  expect_equal(gbif_q(d, "SELECT GBIF_ID FROM samples WHERE ID = 's3'")$GBIF_ID, "2336663130")
  expect_false("Occ" %in% names(gbif_q(d, "SELECT * FROM samples")))
  expect_equal(gbif_q(d, "SELECT ID FROM meta_status WHERE source = 'GBIF'")$ID, "s3")
})

test_that("update_sample_metadata refetches changed GBIF IDs, drops cleared ones, leaves GEOME alone", {
  local_mocked_bindings(.gbif_get = gbif_fixture_get, .geome_get = geome_fixture_get)
  d <- withr::local_tempdir()
  f <- gbif_csv(d, ids = c("6186461308", "2336663130"),
                extra = list(GEOME_BCID = c("ark:/21547/CYB2REEDY", "")))
  new_db(db_path = file.path(d, ".sqlite"), mapping_fn = f)
  gbif_reset_calls()
  up <- data.frame(ID = c("s1", "s2"), Taxon = "x", GBIF_ID = c("", "2336663130"))
  utils::write.csv(up, file.path(d, "up.csv"), row.names = FALSE)
  update_sample_metadata(d, file.path(d, "up.csv"))
  expect_equal(gbif_q(d, "SELECT ID FROM meta_status WHERE source = 'GBIF'")$ID, "s2")
  expect_equal(gbif_q(d, "SELECT COUNT(*) n FROM meta_records WHERE ID = 's1' AND source = 'GBIF'")$n, 0L)
  expect_equal(gbif_q(d, "SELECT ID FROM meta_status WHERE source = 'GEOME'")$ID, "s1")
  expect_equal(length(ls(gbif_calls)), 0L)
  up$GBIF_ID <- c("6186461308", "2336663130")
  utils::write.csv(up, file.path(d, "up.csv"), row.names = FALSE)
  update_sample_metadata(d, file.path(d, "up.csv"))
  expect_setequal(gbif_q(d, "SELECT ID FROM meta_status WHERE source = 'GBIF'")$ID, c("s1", "s2"))
})
