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
    c("ID", "junction", "gap_index", "start", "end", "gap_bases", "size_known")
  )
})

test_that("the junction CSV records intervals in final coordinates", {
  seqs <- c(a = "AAAACCCC", b = "GGGGTTTT")
  joined <- join_scaffolds(seqs, two_scaffold_layout(12), gap_len_default = 100L)
  iv <- spacer_intervals(spacer_track(joined), joined$junction_info)
  d <- withr::local_tempdir()
  out <- write_scaffold_junctions(d, "S1", iv)

  expect_equal(out$ID, "S1")
  expect_equal(out$gap_index, 1L)
  expect_equal(c(out$start, out$end), c(9L, 20L))
  expect_equal(out$gap_bases, 12L)
  expect_equal(out$size_known, 1L)
})

# --- spacers survive reindexing, and are told apart from a scaffold's own Ns ---

test_that("a scaffold's own Ns are never mistaken for a spacer", {
  # Finding from review: b starts with Ns, which fuse with the inserted spacer
  # into one long run. Length-based matching called the placeholder a
  # measurement; position tracking does not.
  seqs <- c(a = rand_seq(300, 3), b = paste0(strrep("N", 40), rand_seq(300, 4)))
  lay <- two_scaffold_layout(NA_real_)
  lay$mapped[2] <- FALSE
  lay$ref_start[2] <- NA_integer_
  lay$ref_end[2] <- NA_integer_
  joined <- join_scaffolds(seqs, lay, gap_len_default = 100L)

  iv <- spacer_intervals(spacer_track(joined), joined$junction_info)
  expect_equal(nrow(iv), 1L)
  expect_equal(iv$start, 301L)
  expect_equal(iv$length, 100L)   # the spacer, NOT the fused 140 bp run
  expect_equal(iv$size_known, 0L)

  # The run of Ns really is 140 long, and it overlaps the spacer, so it is ours
  # and unsized.
  run <- find_sequence_gaps(joined$seq, min_len = 10)
  expect_equal(run$length, 140L)
  q <- gap_qualifiers(run, iv, genus_match = "same")
  expect_equal(q$estimated_length, "unknown")
  expect_equal(q$linkage_evidence, "align-genus")
})

test_that("a scaffold's own 100 bp gap does not borrow our evidence", {
  # Second review finding: 100 is what many scaffolders pad with, so an internal
  # assembler gap used to collide with our placeholder length.
  seqs <- c(a = paste0(rand_seq(200, 5), strrep("N", 100), rand_seq(200, 6)),
            b = rand_seq(300, 7))
  lay <- two_scaffold_layout(NA_real_)
  lay$mapped[2] <- FALSE
  lay$ref_start[2] <- NA_integer_
  lay$ref_end[2] <- NA_integer_
  joined <- join_scaffolds(seqs, lay, gap_len_default = 100L)
  iv <- spacer_intervals(spacer_track(joined), joined$junction_info)

  runs <- find_sequence_gaps(joined$seq, min_len = 10)
  expect_equal(nrow(runs), 2L)

  own <- gap_qualifiers(runs[1, ], iv, genus_match = "same")
  expect_false(own$ours)
  expect_equal(own$estimated_length, "100")
  expect_true(is.na(own$linkage_evidence))
  expect_equal(own$gap_type, "unknown")

  ours <- gap_qualifiers(runs[2, ], iv, genus_match = "same")
  expect_true(ours$ours)
  expect_equal(ours$estimated_length, "unknown")
  expect_equal(ours$linkage_evidence, "align-genus")
})

test_that("spacer positions survive a trim and a rotation", {
  spacer <- c(rep(0L, 10), rep(1L, 5), rep(0L, 10))
  # circularization trim drops the tail
  trimmed <- utils::head(spacer, length(spacer) - 4L)
  iv <- spacer_intervals(trimmed)
  expect_equal(c(iv$start, iv$end), c(11L, 15L))

  # rotation moves the origin; the interval moves with it
  rp <- 12L
  rotated <- spacer[c(seq.int(rp + 1L, length(spacer)), seq_len(rp))]
  iv2 <- spacer_intervals(rotated)
  expect_equal(sum(iv2$length), 5L)
})

# --- how export describes what it finds --------------------------------------

spacer_df <- function(start, end, size_known) {
  data.frame(start = start, end = end, size_known = size_known)
}

test_that("a measured gap in a joined unit reports its length", {
  q <- gap_qualifiers(data.frame(start = 11, end = 22, length = 12),
                      spacer_df(11, 22, 1L), genus_match = "same")
  expect_equal(q$estimated_length, "12")
  expect_equal(q$linkage_evidence, "align-genus")
  expect_equal(q$gap_type, "within scaffold")
})

test_that("a placeholder gap reports an unknown length", {
  q <- gap_qualifiers(data.frame(start = 11, end = 110, length = 100),
                      spacer_df(11, 110, 0L), genus_match = "different")
  expect_equal(q$estimated_length, "unknown")
  expect_equal(q$linkage_evidence, "align-xgenus")
})

test_that("linkage evidence uses the hyphenated feature-table spelling", {
  # The underscore form belongs to AGP, a different file format.
  ev <- function(g) gap_qualifiers(data.frame(start = 1, end = 12, length = 12),
                                   spacer_df(1, 12, 1L), genus_match = g)$linkage_evidence
  expect_equal(ev("same"), "align-genus")
  expect_equal(ev("different"), "align-xgenus")
})

test_that("with no genus answer we claim nothing at all", {
  q <- gap_qualifiers(data.frame(start = 1, end = 12, length = 12),
                      spacer_df(1, 12, 1L), genus_match = NA_character_)
  expect_true(is.na(q$linkage_evidence))
  # linkage_evidence is mandatory for "within scaffold", so the scaffold claim
  # goes too rather than emitting an invalid feature.
  expect_equal(q$gap_type, "unknown")
})

test_that("Ns we did not put there never borrow our alignment as evidence", {
  q <- gap_qualifiers(data.frame(start = 500, end = 599, length = 100),
                      spacer_df(11, 110, 0L), genus_match = "same")
  expect_false(q$ours)
  expect_true(is.na(q$linkage_evidence))
  expect_equal(q$gap_type, "unknown")
  expect_equal(q$estimated_length, "100")
})

test_that("the written block carries the chosen qualifiers", {
  fn <- withr::local_tempfile()
  write_tbl_gap(
    data.frame(start = 11L, end = 110L, length = 100L), fn,
    gap_qualifiers(data.frame(start = 11, end = 110, length = 100),
                   spacer_df(11, 110, 0L), genus_match = "different")
  )
  out <- readLines(fn)

  expect_equal(out[1], "11\t110\tassembly_gap")
  expect_equal(out[2], "\t\t\testimated_length\tunknown")
  expect_equal(out[3], "\t\t\tgap_type\twithin scaffold")
  expect_equal(out[4], "\t\t\tlinkage_evidence\talign-xgenus")
})

test_that("no linkage_evidence line is written when none may be claimed", {
  fn <- withr::local_tempfile()
  write_tbl_gap(data.frame(start = 1L, end = 12L, length = 12L), fn)
  out <- readLines(fn)
  expect_equal(out[2], "\t\t\testimated_length\t12")
  expect_equal(out[3], "\t\t\tgap_type\tunknown")
  expect_length(out, 3L)
})
