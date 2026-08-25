test_that("aligned strings become one row per column", {
  df <- circularize_aln_df("ACGT", "ACGT")
  expect_equal(nrow(df), 4L)
  expect_equal(df$col, 1:4)
  expect_true(all(df$match))
})

test_that("mismatches and gaps are marked", {
  df <- circularize_aln_df("ACGT", "AGGT")
  expect_equal(df$match, c(TRUE, FALSE, TRUE, TRUE))
  gap <- circularize_aln_df("A-GT", "ACGT")
  expect_false(gap$match[2])
  expect_equal(gap$base_q[2], "-")
})

test_that("a window selects a slice", {
  df <- circularize_aln_df(strrep("A", 100), strrep("A", 100), from = 10L, to = 19L)
  expect_equal(nrow(df), 10L)
  expect_equal(df$col, 10:19)
})

test_that("a window past the end is clamped, not an error", {
  df <- circularize_aln_df("ACGT", "ACGT", from = 3L, to = 999L)
  expect_equal(df$col, 3:4)
  expect_equal(nrow(circularize_aln_df("ACGT", "ACGT", from = 99L, to = 200L)), 0L)
})

test_that("empty input yields no rows", {
  expect_equal(nrow(circularize_aln_df(NA_character_, NA_character_)), 0L)
  expect_equal(nrow(circularize_aln_df("", "")), 0L)
})
