# tests/testthat/test-export-ref-note.R
test_that("the reference accession is blank when unusable or flagged", {
  expect_equal(usable_ref_accession("NC_002333.2"), "NC_002333.2")
  expect_equal(usable_ref_accession("NC_002333.2", poor = "poor"), "")
  expect_equal(usable_ref_accession("NO HIT"), "")
  expect_equal(usable_ref_accession(NA), "")
  expect_equal(usable_ref_accession(character(0)), "")
})
