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

test_that("NA bounds are treated as absent, not an error", {
  df <- circularize_aln_df("ACGT", "ACGT", from = NA_integer_)
  expect_equal(df$col, 1:4)
  df <- circularize_aln_df("ACGT", "ACGT", to = NA_integer_)
  expect_equal(df$col, 1:4)
  df <- circularize_aln_df("ACGT", "ACGT", from = NA_integer_, to = NA_integer_)
  expect_equal(df$col, 1:4)
})

test_that("a negative from clamps to 1", {
  df <- circularize_aln_df("ACGT", "ACGT", from = -5L)
  expect_equal(df$col, 1:4)
})

test_that("from > to within range yields zero rows with correct column types", {
  df <- circularize_aln_df("ACGT", "ACGT", from = 3L, to = 2L)
  expect_equal(nrow(df), 0L)
  expect_type(df$col, "integer")
  expect_type(df$base_q, "character")
  expect_type(df$base_s, "character")
  expect_type(df$match, "logical")
})


test_that("contig_depth folds the duplicated block onto the contig start", {
  # Reference is contig[1..10] plus a copy of contig[1..5], so an alignment at
  # reference 11-13 is really contig 1-3.
  d <- contig_depth(starts = 11L, ends = 13L, len = 10L)
  expect_equal(d, c(1L, 1L, 1L, rep(0L, 7)))
})

test_that("contig_depth splits an alignment that crosses the fold point", {
  # Reference 9-12 covers contig 9, 10 and then wraps to contig 1, 2.
  d <- contig_depth(starts = 9L, ends = 12L, len = 10L)
  expect_equal(d, c(1L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 1L))
})

test_that("contig_depth sums both copies rather than losing one", {
  # One read at the contig start, one at the appended copy of it: folding must
  # recombine them, which is the whole point of not filtering on mapping quality.
  d <- contig_depth(starts = c(1L, 11L), ends = c(3L, 13L), len = 10L)
  expect_equal(d[1:3], c(2L, 2L, 2L))
})

test_that("contig_depth handles no alignments", {
  expect_equal(contig_depth(integer(0), integer(0), len = 5L), integer(5))
})


test_that("circ_length_label shows before and after when something was trimmed", {
  expect_equal(circ_length_label(16996L, 400L), "16,996 -> 16,596")
})

test_that("circ_length_label shows one number when nothing was trimmed", {
  expect_equal(circ_length_label(16596L, 0L), "16,596")
})

test_that("circ_length_label survives missing values", {
  expect_equal(circ_length_label(NA_integer_, 400L), "unknown")
  expect_equal(circ_length_label(16596L, NA_integer_), "16,596")
})
