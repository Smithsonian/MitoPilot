# Runs of unknown bases must be declared as assembly_gap features, and a coding
# feature sitting across one has to say so. Nothing here assumes what produced
# the Ns.

test_that("gaps are found, ordered, and filtered by length", {
  seq <- paste0(strrep("A", 10), strrep("N", 12), strrep("C", 10),
                strrep("N", 3), strrep("G", 10))
  g <- find_sequence_gaps(seq, min_len = 10)
  expect_equal(nrow(g), 1L)
  expect_equal(g$start, 11L)
  expect_equal(g$end, 22L)
  expect_equal(g$length, 12L)

  # A lower floor picks up the short run too, still in ascending order.
  g2 <- find_sequence_gaps(seq, min_len = 3)
  expect_equal(g2$start, c(11L, 33L))
  expect_equal(g2$length, c(12L, 3L))
})

test_that("a sequence with no unknown bases reports no gaps", {
  g <- find_sequence_gaps(strrep("ACGT", 25))
  expect_s3_class(g, "data.frame")
  expect_equal(nrow(g), 0L)
  expect_equal(nrow(find_sequence_gaps("")), 0L)
  expect_equal(nrow(find_sequence_gaps(NA_character_)), 0L)
})

test_that("unknown bases are counted within a feature, wrap included", {
  seq <- paste0("NN", strrep("A", 8), "NNN", strrep("C", 7))
  # 1..10 holds the two leading Ns only.
  expect_equal(count_unknown_bases(seq, 1, 10, FALSE, nchar(seq)), 2L)
  # 11..13 is the interior run.
  expect_equal(count_unknown_bases(seq, 11, 13, FALSE, nchar(seq)), 3L)
  expect_equal(count_unknown_bases(seq, 14, 20, FALSE, nchar(seq)), 0L)
  # A feature spanning the origin counts both of its intervals.
  expect_equal(count_unknown_bases(seq, 15, 3, TRUE, nchar(seq)), 2L)
})

test_that("the gap block carries the qualifiers INSDC requires", {
  fn <- withr::local_tempfile()
  write_tbl_gap(
    data.frame(start = 11L, end = 22L, length = 12L), fn,
    gap_qualifiers(data.frame(start = 11, end = 22, length = 12),
                   data.frame(start = 11, end = 22, size_known = 1L),
                   genus_match = "same")
  )
  out <- readLines(fn)

  expect_equal(out[1], "11\t22\tassembly_gap")
  expect_equal(out[2], "\t\t\testimated_length\t12")
  expect_equal(out[3], "\t\t\tgap_type\twithin scaffold")
  expect_equal(out[4], "\t\t\tlinkage_evidence\talign-genus")
})
