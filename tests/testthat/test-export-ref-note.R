# tests/testthat/test-export-ref-note.R
test_that("the reference comparison sentence names sample and accession", {
  expect_equal(ref_comparison_note("S1", "NC_002333.2"),
               "compared sample S1 to GenBank accession NC_002333.2")
  expect_equal(ref_comparison_note("S1", "NC_002333.2", poor = "poor"), "")
  expect_equal(ref_comparison_note("S1", "NO HIT"), "")
  expect_equal(ref_comparison_note("S1", NA), "")
  expect_equal(ref_comparison_note("S1", character(0)), "")
})
