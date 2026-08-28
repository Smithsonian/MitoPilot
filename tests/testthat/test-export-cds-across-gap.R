test_that("a CDS clear of the gaps is not split", {
  gaps <- data.frame(start = 5000L, end = 5100L)
  p <- split_cds_at_gaps(100, 400, "+", FALSE, 10000, gaps)
  expect_length(p, 1L)
  expect_identical(p[[1]]$loc, list(c("100", "400")))
  expect_false(p[[1]]$cut5)
  expect_false(p[[1]]$cut3)
})

test_that("no gaps at all leaves the feature whole", {
  empty <- data.frame(start = integer(0), end = integer(0))
  expect_length(split_cds_at_gaps(100, 400, "+", FALSE, 10000, empty), 1L)
  expect_length(split_cds_at_gaps(100, 400, "+", FALSE, 10000, NULL), 1L)
})

test_that("plus strand splits into a 5' and a 3' piece abutting the gap", {
  gaps <- data.frame(start = 200L, end = 249L)
  p <- split_cds_at_gaps(100, 400, "+", FALSE, 10000, gaps)
  expect_length(p, 2L)
  expect_identical(p[[1]]$loc, list(c("100", "199")))
  expect_identical(p[[2]]$loc, list(c("250", "400")))
  # the ends made by the gap are the 3' end of the first piece and the 5' end
  # of the second
  expect_false(p[[1]]$cut5)
  expect_true(p[[1]]$cut3)
  expect_true(p[[2]]$cut5)
  expect_false(p[[2]]$cut3)
})

test_that("minus strand pieces come back 5'->3' with the ends swapped", {
  gaps <- data.frame(start = 200L, end = 249L)
  p <- split_cds_at_gaps(100, 400, "-", FALSE, 10000, gaps)
  expect_length(p, 2L)
  # 5' end of a minus-strand feature is the higher coordinate
  expect_identical(p[[1]]$loc, list(c("400", "250")))
  expect_identical(p[[2]]$loc, list(c("199", "100")))
  expect_true(p[[1]]$cut3)
  expect_false(p[[1]]$cut5)
  expect_true(p[[2]]$cut5)
  expect_false(p[[2]]$cut3)
})

test_that("a gap at the very start of the CDS trims rather than splits", {
  gaps <- data.frame(start = 90L, end = 120L)
  p <- split_cds_at_gaps(100, 400, "+", FALSE, 10000, gaps)
  expect_length(p, 1L)
  expect_identical(p[[1]]$loc, list(c("121", "400")))
  expect_true(p[[1]]$cut5)
  expect_false(p[[1]]$cut3)
})

test_that("a gap at the very end of the CDS trims rather than splits", {
  gaps <- data.frame(start = 380L, end = 500L)
  p <- split_cds_at_gaps(100, 400, "+", FALSE, 10000, gaps)
  expect_length(p, 1L)
  expect_identical(p[[1]]$loc, list(c("100", "379")))
  expect_false(p[[1]]$cut5)
  expect_true(p[[1]]$cut3)
})

test_that("a gap swallowing the whole CDS leaves nothing", {
  gaps <- data.frame(start = 50L, end = 500L)
  expect_length(split_cds_at_gaps(100, 400, "+", FALSE, 10000, gaps), 0L)
})

test_that("two gaps give three pieces", {
  gaps <- data.frame(start = c(200L, 300L), end = c(249L, 319L))
  p <- split_cds_at_gaps(100, 400, "+", FALSE, 10000, gaps)
  expect_length(p, 3L)
  expect_identical(p[[1]]$loc, list(c("100", "199")))
  expect_identical(p[[2]]$loc, list(c("250", "299")))
  expect_identical(p[[3]]$loc, list(c("320", "400")))
  expect_identical(vapply(p, function(x) x$cut5, logical(1)),
                   c(FALSE, TRUE, TRUE))
  expect_identical(vapply(p, function(x) x$cut3, logical(1)),
                   c(TRUE, TRUE, FALSE))
})

test_that("two gaps on the minus strand come back in 5'->3' order", {
  gaps <- data.frame(start = c(200L, 300L), end = c(249L, 319L))
  p <- split_cds_at_gaps(100, 400, "-", FALSE, 10000, gaps)
  expect_length(p, 3L)
  expect_identical(p[[1]]$loc, list(c("400", "320")))
  expect_identical(p[[2]]$loc, list(c("299", "250")))
  expect_identical(p[[3]]$loc, list(c("199", "100")))
})

test_that("a feature spanning the origin splits on its own reading order", {
  # plus strand, 9900..10000 then 1..200 on a 10000 bp circle
  gaps <- data.frame(start = 50L, end = 79L)
  p <- split_cds_at_gaps(9900, 200, "+", TRUE, 10000, gaps)
  expect_length(p, 2L)
  expect_identical(p[[1]]$loc, list(c("9900", "10000"), c("1", "49")))
  expect_identical(p[[2]]$loc, list(c("80", "200")))
  expect_true(p[[1]]$cut3)
  expect_true(p[[2]]$cut5)
})

test_that("origin-spanning minus strand reverses both pieces and intervals", {
  gaps <- data.frame(start = 50L, end = 79L)
  p <- split_cds_at_gaps(9900, 200, "-", TRUE, 10000, gaps)
  expect_length(p, 2L)
  expect_identical(p[[1]]$loc, list(c("200", "80")))
  expect_identical(p[[2]]$loc, list(c("49", "1"), c("10000", "9900")))
  expect_true(p[[1]]$cut3)
  expect_true(p[[2]]$cut5)
})

test_that("a gap crossing the origin inside the feature still splits once", {
  gaps <- data.frame(start = 9990L, end = 10000L)
  p <- split_cds_at_gaps(9900, 200, "+", TRUE, 10000, gaps)
  expect_length(p, 2L)
  expect_identical(p[[1]]$loc, list(c("9900", "9989")))
  expect_identical(p[[2]]$loc, list(c("1", "200")))
})

test_that("only gaps of unknown size reach the splitter", {
  # gap_qualifiers is what decides; a sized spacer is not passed on
  run <- data.frame(start = 200L, end = 249L, length = 50L)
  sized <- data.frame(start = 200L, end = 249L, size_known = 1L)
  unsized <- data.frame(start = 200L, end = 249L, size_known = 0L)
  expect_false(identical(
    gap_qualifiers(run, sized, "same")$estimated_length, "unknown"
  ))
  expect_identical(
    gap_qualifiers(run, unsized, "same")$estimated_length, "unknown"
  )
  # a known-size gap is never handed to split_cds_at_gaps, so the CDS is whole
  expect_length(
    split_cds_at_gaps(100, 400, "+", FALSE, 10000,
                      data.frame(start = integer(0), end = integer(0))),
    1L
  )
})

test_that("the written pieces are partial, keep their qualifiers, and carry the NCBI notes", {
  fn <- withr::local_tempfile()
  file.create(fn)
  pieces <- split_cds_at_gaps(100, 400, "+", FALSE, 10000,
                              data.frame(start = 200L, end = 249L))
  write_tbl_cds_pieces(pieces, "COX1", "cytochrome c oxidase subunit I", 2, fn,
                       note = "start codon not determined", codon_start = TRUE,
                       partial5 = TRUE)
  out <- readLines(fn)

  expect_true("<100\t>199\tCDS" %in% out)
  expect_true("<250\t400\tCDS" %in% out)
  # both pieces keep product and transl_table
  expect_equal(sum(grepl("product", out)), 2L)
  expect_equal(sum(grepl("transl_table", out)), 2L)
  # NCBI's note, verbatim, on both pieces
  expect_equal(sum(out == "\t\t\tnote\tgap found within coding sequence"), 2L)
  # each piece points at the other
  expect_true(any(grepl("part 1 of 2.*250\\.\\.400", out)))
  expect_true(any(grepl("part 2 of 2.*100\\.\\.199", out)))
  # the note the feature already had survives on both
  expect_equal(sum(grepl("start codon not determined", out)), 2L)
  # codon_start belongs to the 5' piece only: the gap length is unknown, so the
  # frame on the far side is not knowable and is not invented
  expect_equal(sum(grepl("codon_start", out)), 1L)
  expect_lt(grep("codon_start", out), grep("<250", out))
})

test_that("a poly-A stop is written on the 3' piece only", {
  fn <- withr::local_tempfile()
  file.create(fn)
  pieces <- split_cds_at_gaps(100, 400, "-", FALSE, 10000,
                              data.frame(start = 200L, end = 249L))
  write_tbl_cds_pieces(pieces, "ND5", "NADH dehydrogenase subunit 5", 2, fn,
                       transl_except = "(pos:100,aa:TERM)", partial3 = TRUE)
  out <- readLines(fn)
  expect_true("400\t>250\tCDS" %in% out)
  expect_true("<199\t>100\tCDS" %in% out)
  expect_equal(sum(grepl("transl_except", out)), 1L)
  expect_gt(grep("transl_except", out), grep("<199", out))
})
