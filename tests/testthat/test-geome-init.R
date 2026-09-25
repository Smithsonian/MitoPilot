geome_mapping <- function(dir, bcid_col = "GEOME_BCID", bcids = c("ark:/21547/CYB2REEDY", "")) {
  m <- data.frame(ID = c("s1", "s2"), Taxon = "Fish a", R1 = c("a_1.fq", "b_1.fq"),
                  R2 = c("a_2.fq", "b_2.fq"))
  m[[bcid_col]] <- bcids
  f <- file.path(dir, "mapping.csv")
  utils::write.csv(m, f, row.names = FALSE)
  f
}

test_that("check_mapping warns on a non-ARK BCID and errors on a missing custom column", {
  m <- data.frame(ID = "s1", Taxon = "x", R1 = "a", R2 = "b", Bcid = "junk")
  iss <- check_mapping(m, mapping_geome = "Bcid")
  expect_match(paste(iss$warnings, collapse = " "), "GEOME")
  iss <- check_mapping(m, mapping_geome = "Nope")
  expect_match(paste(iss$errors, collapse = " "), "Nope")
  m$GEOME_BCID <- "ark:/1/A"
  iss <- check_mapping(m, mapping_geome = "Bcid")
  expect_match(paste(iss$errors, collapse = " "), "GEOME_BCID")
})

test_that("new_db stores a renamed BCID column and fetches it", {
  local_mocked_bindings(.geome_get = geome_fixture_get)
  d <- withr::local_tempdir()
  new_db(db_path = file.path(d, ".sqlite"), mapping_fn = geome_mapping(d, "Bcid"),
         mapping_geome = "Bcid")
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(d, ".sqlite"))
  on.exit(DBI::dbDisconnect(con))
  s <- DBI::dbGetQuery(con, "SELECT * FROM samples ORDER BY ID")
  expect_false("Bcid" %in% names(s))
  expect_equal(s$GEOME_BCID, c("ark:/21547/CYB2REEDY", NA))
  expect_equal(DBI::dbGetQuery(con, "SELECT ID, status FROM meta_status")$ID, "s1")
})

test_that("new_db fetch_geome = FALSE stores BCIDs without calling GEOME", {
  local_mocked_bindings(.geome_get = function(...) stop("should not be called"))
  d <- withr::local_tempdir()
  new_db(db_path = file.path(d, ".sqlite"), mapping_fn = geome_mapping(d), fetch_geome = FALSE)
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(d, ".sqlite"))
  on.exit(DBI::dbDisconnect(con))
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM meta_status")$n, 0L)
  expect_true("meta_export_fields" %in% DBI::dbListTables(con))
})

test_that("new_db_userAsmb stores a renamed BCID column and fetches it", {
  local_mocked_bindings(.geome_get = geome_fixture_get)
  d <- withr::local_tempdir()
  mapping_fn <- file.path(d, "mapping.csv")
  utils::write.csv(
    data.frame(ID = c("s1", "s2"), Taxon = c("Danio rerio", "Danio rerio"),
               Assembly = c("s1.fasta", "s2.fasta"), Topology = c("linear", "circular"),
               Bcid = c("ark:/21547/CYB2REEDY", "")),
    mapping_fn, row.names = FALSE
  )
  db_path <- file.path(d, ".sqlite")
  new_db_userAsmb(db_path = db_path, mapping_fn = mapping_fn, no_raw_data = TRUE,
                  mapping_geome = "Bcid")
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))
  s <- DBI::dbGetQuery(con, "SELECT * FROM samples ORDER BY ID")
  expect_false("Bcid" %in% names(s))
  expect_equal(s$GEOME_BCID, c("ark:/21547/CYB2REEDY", NA))
  expect_equal(DBI::dbGetQuery(con, "SELECT ID, status FROM meta_status")$ID, "s1")
})

test_that("new_db_userAsmb fetch_geome = FALSE stores BCIDs without calling GEOME", {
  local_mocked_bindings(.geome_get = function(...) stop("should not be called"))
  d <- withr::local_tempdir()
  mapping_fn <- file.path(d, "mapping.csv")
  utils::write.csv(
    data.frame(ID = c("s1", "s2"), Taxon = c("Danio rerio", "Danio rerio"),
               Assembly = c("s1.fasta", "s2.fasta"), Topology = c("linear", "circular"),
               GEOME_BCID = c("ark:/21547/CYB2REEDY", "")),
    mapping_fn, row.names = FALSE
  )
  db_path <- file.path(d, ".sqlite")
  new_db_userAsmb(db_path = db_path, mapping_fn = mapping_fn, no_raw_data = TRUE,
                  fetch_geome = FALSE)
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM meta_status")$n, 0L)
  expect_true("meta_export_fields" %in% DBI::dbListTables(con))
  expect_equal(DBI::dbGetQuery(con, "SELECT GEOME_BCID FROM samples ORDER BY ID")$GEOME_BCID,
               c("ark:/21547/CYB2REEDY", NA))
})

test_that("new_db without any BCID column still creates GEOME tables", {
  d <- withr::local_tempdir()
  m <- data.frame(ID = "s1", Taxon = "x", R1 = "a_1.fq", R2 = "a_2.fq")
  utils::write.csv(m, file.path(d, "mapping.csv"), row.names = FALSE)
  new_db(db_path = file.path(d, ".sqlite"), mapping_fn = file.path(d, "mapping.csv"))
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(d, ".sqlite"))
  on.exit(DBI::dbDisconnect(con))
  expect_true("GEOME_BCID" %in% DBI::dbListFields(con, "samples"))
})
