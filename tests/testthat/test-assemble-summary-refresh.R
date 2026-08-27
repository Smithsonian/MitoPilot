# assemble.length / assemble.scaffolds describe the contigs that are ACTIVE.
# Anything that changes which contigs are ignored has to refresh them, or the
# Assemble table keeps describing a state that no longer exists.

summary_db <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbExecute(con, "CREATE TABLE assemble (ID TEXT PRIMARY KEY, length TEXT,
                       paths INTEGER, scaffolds INTEGER)")
  DBI::dbExecute(con, "INSERT INTO assemble VALUES ('S1', 'stale', 1, 99)")
  DBI::dbExecute(con, "CREATE TABLE assemblies (ID TEXT, path INTEGER,
                       scaffold INTEGER, length INTEGER, ignore INTEGER)")
  con
}

test_that("the summary lists every active contig, longest first", {
  con <- summary_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "INSERT INTO assemblies VALUES
    ('S1', 1, 1, 6008, 0), ('S1', 1, 2, 6008, 0), ('S1', 1, 3, 5990, 0)")

  out <- refresh_assemble_summary(con, "S1")

  # Equal fragments are NOT collapsed: three contigs read as three contigs.
  expect_equal(out$length, "6008;6008;5990")
  expect_equal(out$scaffolds, 3L)
  row <- DBI::dbGetQuery(con, "SELECT * FROM assemble")
  expect_equal(row$length, "6008;6008;5990")
  expect_equal(row$scaffolds, 3L)
})

test_that("a joined sample reports one contig", {
  con <- summary_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "INSERT INTO assemblies VALUES
    ('S1', 0, 0, 18024, 0), ('S1', 1, 1, 6008, 1), ('S1', 1, 2, 6008, 1),
    ('S1', 1, 3, 6008, 1)")

  out <- refresh_assemble_summary(con, "S1")
  expect_equal(out$length, "18024")
  expect_equal(out$scaffolds, 1L)
})

test_that("un-ignoring a merged fragment brings its length back", {
  con <- summary_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "INSERT INTO assemblies VALUES
    ('S1', 0, 0, 18024, 0), ('S1', 1, 1, 6008, 0), ('S1', 1, 2, 6008, 1),
    ('S1', 1, 3, 6008, 1)")

  out <- refresh_assemble_summary(con, "S1")
  expect_equal(out$length, "18024;6008")
  expect_equal(out$scaffolds, 2L)
})

test_that("a sample with nothing active reports no length", {
  con <- summary_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "INSERT INTO assemblies VALUES ('S1', 1, 1, 6008, 1)")

  out <- refresh_assemble_summary(con, "S1")
  expect_true(is.na(out$length))
  expect_equal(out$scaffolds, 0L)
  expect_true(is.na(DBI::dbGetQuery(con, "SELECT length FROM assemble")$length))
})
