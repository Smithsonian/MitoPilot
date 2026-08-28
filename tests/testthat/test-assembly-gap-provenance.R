# Where a run of Ns came from decides how it must be described on export. A gap
# the reference could size is a measurement; a gap it could not is a placeholder
# and must not be submitted as one. Ns in a sequence we did not join carry no
# evidence of ours at all.

# --- what the join records ---------------------------------------------------

rand_seq <- function(n, seed) {
  set.seed(seed)
  paste(sample(c("A", "C", "G", "T"), n, replace = TRUE), collapse = "")
}

two_scaffold_layout <- function(gap_before, ref_start = c(0L, 30L),
                                ref_end = c(8L, 38L)) {
  data.frame(
    scaffold = c("a", "b"), order = 1:2, rc = c(FALSE, FALSE),
    gap_before = c(NA, gap_before), mapped = c(TRUE, TRUE),
    ref_start = ref_start, ref_end = ref_end, stringsAsFactors = FALSE
  )
}

test_that("a reference-sized gap is recorded as a measurement", {
  seqs <- c(a = "AAAACCCC", b = "GGGGTTTT")
  res <- join_scaffolds(seqs, two_scaffold_layout(12), gap_len_default = 100L)

  ji <- res$junction_info
  expect_equal(nrow(ji), 1L)
  expect_equal(ji$type, "gap")
  expect_equal(ji$gap_bases, 12L)
  expect_equal(ji$size_known, 1L)
  expect_equal(sum(is.na(res$src_scaffold)), 12L)
})

test_that("an unmappable junction is recorded as a placeholder", {
  # gap_before NA: the reference cannot place these two relative to each other,
  # so the join falls back to its fixed spacer.
  seqs <- c(a = "AAAACCCC", b = "GGGGTTTT")
  lay <- two_scaffold_layout(NA_real_)
  lay$mapped[2] <- FALSE
  lay$ref_start[2] <- NA_integer_
  lay$ref_end[2] <- NA_integer_
  res <- join_scaffolds(seqs, lay, gap_len_default = 100L)

  ji <- res$junction_info
  expect_equal(ji$gap_bases, 100L)
  expect_equal(ji$size_known, 0L)
})

test_that("a butt join records no gap at all", {
  seqs <- c(a = rand_seq(300, 1), b = rand_seq(300, 2))
  res <- join_scaffolds(seqs, two_scaffold_layout(0), gap_len_default = 100L)

  ji <- res$junction_info
  expect_equal(ji$gap_bases, 0L)
  expect_equal(ji$size_known, 0L)
  expect_equal(sum(is.na(res$src_scaffold)), 0L)
})

test_that("the junction CSV is written even when nothing was joined", {
  d <- withr::local_tempdir()
  out <- write_scaffold_junctions(d, "S1", NULL)

  expect_equal(nrow(out), 0L)
  fn <- file.path(d, "S1_scaffold_junctions.csv")
  expect_true(file.exists(fn))
  expect_equal(
    strsplit(readLines(fn)[1], ",")[[1]],
    c("ID", "junction", "from_scaffold", "to_scaffold", "type", "gap_bases",
      "size_known")
  )
})

test_that("the junction CSV numbers junctions in order", {
  seqs <- c(a = "AAAACCCC", b = "GGGGTTTT")
  res <- join_scaffolds(seqs, two_scaffold_layout(12), gap_len_default = 100L)
  d <- withr::local_tempdir()
  out <- write_scaffold_junctions(d, "S1", res$junction_info)

  expect_equal(out$ID, "S1")
  expect_equal(out$junction, 1L)
  expect_equal(out$gap_bases, 12L)
  expect_equal(out$size_known, 1L)
})

# --- how export describes what it finds --------------------------------------

test_that("a measured gap in a joined unit reports its length", {
  q <- gap_qualifiers(12, joined = TRUE, placeholder_lengths = 100L,
                      genus_match = "same")
  expect_equal(q$estimated_length, "12")
  expect_equal(q$linkage_evidence, "align_genus")
})

test_that("a placeholder gap reports an unknown length", {
  q <- gap_qualifiers(100, joined = TRUE, placeholder_lengths = 100L,
                      genus_match = "different")
  expect_equal(q$estimated_length, "unknown")
  expect_equal(q$linkage_evidence, "align_xgenus")
})

test_that("genus answer picks the evidence, and no answer claims nothing", {
  expect_equal(gap_qualifiers(12, TRUE, genus_match = "same")$linkage_evidence,
               "align_genus")
  expect_equal(gap_qualifiers(12, TRUE, genus_match = "different")$linkage_evidence,
               "align_xgenus")
  expect_equal(gap_qualifiers(12, TRUE, genus_match = NA_character_)$linkage_evidence,
               "unspecified")
})

test_that("Ns we did not put there never borrow our alignment as evidence", {
  # Not joined: even with a genus answer on file, we cannot vouch for the run.
  q <- gap_qualifiers(100, joined = FALSE, placeholder_lengths = 100L,
                      genus_match = "same")
  expect_equal(q$linkage_evidence, "unspecified")
  # And its length is what it is, not "unknown": we are not claiming it is ours.
  expect_equal(q$estimated_length, "100")
})

test_that("the written block carries the chosen qualifiers", {
  fn <- withr::local_tempfile()
  write_tbl_gap(
    data.frame(start = 11L, end = 110L, length = 100L), fn,
    gap_qualifiers(100, joined = TRUE, placeholder_lengths = 100L,
                   genus_match = "different")
  )
  out <- readLines(fn)

  expect_equal(out[1], "11\t110\tassembly_gap")
  expect_equal(out[2], "\t\t\testimated_length\tunknown")
  expect_equal(out[3], "\t\t\tgap_type\twithin scaffold")
  expect_equal(out[4], "\t\t\tlinkage_evidence\talign_xgenus")
})

test_that("write_tbl_gap without qualifiers stays conservative", {
  fn <- withr::local_tempfile()
  write_tbl_gap(data.frame(start = 1L, end = 12L, length = 12L), fn)
  out <- readLines(fn)
  expect_equal(out[2], "\t\t\testimated_length\t12")
  expect_equal(out[4], "\t\t\tlinkage_evidence\tunspecified")
})
