# Export of features that span the origin of a circular assembly.
#
# In the GenBank 5-column feature table a single "10623<TAB>468" line means
# "minus strand, 468..10623" - a 10 kb reverse-strand feature - not "wraps the
# origin". An origin-spanning feature has to be written as two intervals.

L <- 10946L

test_that("a feature that does not wrap keeps its single strand-oriented interval", {
  expect_equal(tbl_locations(100, 200, "+", FALSE, L), list(c("100", "200")))
  expect_equal(tbl_locations(100, 200, "-", FALSE, L), list(c("200", "100")))
})

test_that("an origin-spanning feature becomes two 5'->3' intervals", {
  # nad1 10623..468: 10623..10946 then 1..468
  expect_equal(
    tbl_locations(10623, 468, "+", TRUE, L),
    list(c("10623", "10946"), c("1", "468"))
  )
  # minus strand reads the same arc backwards: 468..1 then 10946..10623
  expect_equal(
    tbl_locations(10623, 468, "-", TRUE, L),
    list(c("468", "1"), c("10946", "10623"))
  )
})

test_that("only the first interval carries the feature key", {
  fn <- withr::local_tempfile()
  write_tbl_loc(tbl_locations(10623, 468, "+", TRUE, L), "CDS", fn)
  expect_equal(readLines(fn), c("10623\t10946\tCDS", "1\t468"))
})

test_that("the 3' partial marker lands on the last interval", {
  pos <- mark_tbl_3p(tbl_locations(10623, 468, "+", TRUE, L))
  expect_equal(pos, list(c("10623", "10946"), c("1", ">468")))

  pos <- mark_tbl_3p(tbl_locations(100, 200, "-", FALSE, L))
  expect_equal(pos, list(c("200", ">100")))
})

test_that("GFF3 writes an origin-spanning feature past the end of the sequence", {
  expect_equal(gff_end(468, TRUE, L), L + 468)
  expect_equal(gff_end(468, FALSE, L), 468)
})

test_that("transl_except points at the 3' end, not the numerically larger end", {
  # + strand: 3' end is pos2 even when pos2 < pos1
  expect_equal(
    .transl_except_pos(10623, 468, "+", 2, TRUE, L),
    "(pos:467..468,aa:TERM)"
  )
  # - strand: 3' end is pos1
  expect_equal(
    .transl_except_pos(10623, 468, "-", 2, TRUE, L),
    "(pos:10624..10623,aa:TERM)"
  )
  # a partial stop that itself straddles the origin wraps onto the contig
  expect_equal(
    .transl_except_pos(9000, 1, "+", 2, TRUE, L),
    paste0("(pos:", L, "..1,aa:TERM)")
  )
  # ordinary non-wrapping feature is unchanged
  expect_equal(
    .transl_except_pos(100, 200, "+", 1, FALSE, L),
    "(pos:200,aa:TERM)"
  )
})
