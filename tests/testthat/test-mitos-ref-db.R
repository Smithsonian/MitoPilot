test_that("check_mitos_ref_db accepts a real MITOS2 reference directory", {
  refdir <- tempfile()
  db <- file.path(refdir, "Chordata")
  dir.create(file.path(db, "ncRNA"), recursive = TRUE)
  dir.create(file.path(db, "featureProt"))
  writeLines("{}", file.path(db, "auxinfo.json"))

  expect_true(check_mitos_ref_db("Chordata", refdir))
})

test_that("check_mitos_ref_db rejects a curation-only BLAST database", {
  refdir <- tempfile()
  db <- file.path(refdir, "Metazoa_RefSeq235")
  dir.create(file.path(db, "featureProt"), recursive = TRUE)
  dir.create(file.path(db, "featureNuc"))

  expect_error(
    check_mitos_ref_db("Metazoa_RefSeq235", refdir),
    "auxinfo\\.json"
  )
  expect_error(
    check_mitos_ref_db("Metazoa_RefSeq235", refdir),
    "Metazoa_RefSeq89 or Chordata"
  )
})

test_that("check_mitos_ref_db reports a partial reference directory", {
  refdir <- tempfile()
  db <- file.path(refdir, "Partial")
  dir.create(db, recursive = TRUE)
  writeLines("{}", file.path(db, "auxinfo.json"))

  expect_error(check_mitos_ref_db("Partial", refdir), "ncRNA/")
})

test_that("check_mitos_ref_db reports a missing database directory", {
  refdir <- tempfile()
  dir.create(refdir)
  expect_error(check_mitos_ref_db("Nope", refdir), "not found")
})
