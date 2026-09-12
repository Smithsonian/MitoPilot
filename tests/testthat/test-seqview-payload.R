# tests/testthat/test-seqview-payload.R
sv_ann <- function(...) {
  base <- data.frame(
    type = "PCG", gene = "nad1", pos1 = 10L, pos2 = 39L, direction = "+",
    partial_start = "no", partial_stop = "no", translation = "MKLIVLLKN",
    notes = "", stringsAsFactors = FALSE
  )
  rows <- list(...)
  if (length(rows) == 0) return(base)
  do.call(rbind, lapply(rows, function(r) { b <- base; b[names(r)] <- r; b }))
}

test_that("payload carries the unit, sequence, and every live feature", {
  a <- sv_ann(list(), list(gene = "trnF", type = "tRNA", pos1 = 40L, pos2 = 108L, translation = NA))
  p <- seqview_payload(a, "acgtacgt", "circular", "S1.1.1", selected = 2L, version = 3L)
  expect_equal(p$unit, "S1.1.1")
  expect_equal(p$len, 8L)
  expect_equal(p$seq, "ACGTACGT")
  expect_equal(p$topology, "circular")
  expect_equal(p$version, 3L)
  expect_equal(p$selected, 2L)
  expect_length(p$features, 2)
  f <- p$features[[1]]
  expect_equal(f[c("row", "gene", "pos1", "pos2", "dir")],
               list(row = 1L, gene = "nad1", pos1 = 10L, pos2 = 39L, dir = "+"))
  expect_equal(f$translation, "MKLIVLLKN")
  expect_false(f$partial5); expect_false(f$partial3)
  expect_null(p$features[[2]]$translation)
})

test_that("soft-deleted rows are dropped and the selection follows the row index", {
  a <- sv_ann(list(), list(gene = "nad2_DELETED_1", pos1 = 0L, pos2 = 0L),
              list(gene = "cox1", pos1 = 100L, pos2 = 200L))
  p <- seqview_payload(a, "ACGT", "linear", "S1.1.1", selected = 3L)
  expect_equal(vapply(p$features, `[[`, integer(1), "row"), c(1L, 3L))
  expect_equal(p$selected, 3L)
  expect_null(seqview_payload(a, "ACGT", "linear", "S1.1.1", selected = 2L)$selected)
})

test_that("partial flags, join markers, and notes are carried", {
  a <- sv_ann(list(partial_start = "yes", notes = "JOIN: mode=exon group=2 extra words that go on and on and on and on and on and on and on"))
  f <- seqview_payload(a, "ACGT", "linear", "S1.1.1")$features[[1]]
  expect_true(f$partial5); expect_false(f$partial3)
  expect_equal(f$joined, "JOIN: mode=exon group=2")
  expect_equal(nchar(f$notes), 80L)
})

test_that("a wrap-around feature is passed through untouched", {
  a <- sv_ann(list(pos1 = 16500L, pos2 = 120L))
  f <- seqview_payload(a, strrep("A", 16600), "circular", "S1.1.1")$features[[1]]
  expect_equal(c(f$pos1, f$pos2), c(16500L, 120L))
})
