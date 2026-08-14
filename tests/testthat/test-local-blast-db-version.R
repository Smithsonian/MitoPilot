test_that("local_blast_db_info returns NULL when there is nothing to read", {
  expect_null(local_blast_db_info(NULL))
  expect_null(local_blast_db_info(file.path(tempdir(), "does-not-exist")))

  empty <- withr::local_tempdir()
  expect_null(local_blast_db_info(empty))
})

test_that("local_blast_db_info parses a published VERSION file", {
  out <- withr::local_tempdir()
  d <- file.path(out, "SAMPLE1", "assemble", "default")
  dir.create(d, recursive = TRUE)
  writeLines(
    c(
      "name\tmito_metazoa",
      "built\t2026-08-14T02:37:22.267146+00:00",
      "sequences\t134560",
      "bases\t2201289510",
      "deduplicated_identical\tTrue"
    ),
    file.path(d, "blast_db_VERSION.txt")
  )

  info <- local_blast_db_info(out)
  expect_type(info, "list")
  expect_equal(info$name, "mito_metazoa")
  expect_equal(info$sequences, "134560")
  expect_equal(info$built_date, "2026-08-14")
})

test_that("local_blast_db_info picks the most recently written file", {
  out <- withr::local_tempdir()
  older <- file.path(out, "OLD", "assemble", "default")
  newer <- file.path(out, "NEW", "assemble", "default")
  dir.create(older, recursive = TRUE)
  dir.create(newer, recursive = TRUE)
  writeLines(c("built\t2025-01-01T00:00:00+00:00", "sequences\t1"),
             file.path(older, "blast_db_VERSION.txt"))
  writeLines(c("built\t2026-08-14T02:37:22+00:00", "sequences\t134560"),
             file.path(newer, "blast_db_VERSION.txt"))
  # make the mtimes unambiguous regardless of write speed
  Sys.setFileTime(file.path(older, "blast_db_VERSION.txt"), Sys.time() - 600)

  info <- local_blast_db_info(out)
  expect_equal(info$built_date, "2026-08-14")
  expect_equal(info$sequences, "134560")
})

test_that("local_blast_db_info survives a malformed file", {
  out <- withr::local_tempdir()
  d <- file.path(out, "S", "assemble", "default")
  dir.create(d, recursive = TRUE)
  writeLines(c("this is not tab separated", ""), file.path(d, "blast_db_VERSION.txt"))

  info <- local_blast_db_info(out)
  # no usable build date is the same as nothing to report
  expect_null(info)
})
