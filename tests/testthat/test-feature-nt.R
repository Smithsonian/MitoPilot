test_that("feature_nt reads minus-strand and wrapped features in gene direction", {
  s <- Biostrings::DNAString("AAACCCGGGTTT")
  expect_equal(as.character(feature_nt(s, 4, 6, "+")), "CCC")
  expect_equal(as.character(feature_nt(s, 4, 9, "-")), "CCCGGG")
  expect_equal(as.character(feature_nt(s, 7, 12, "-")), "AAACCC")
  expect_equal(as.character(feature_nt(s, 11, 2, "+")), "TTAA")
  expect_equal(as.character(feature_nt(s, 11, 2, "-")), "TTAA")
})
