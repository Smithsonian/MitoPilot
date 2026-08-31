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
  expect_true(gap_is_ours(run, iv))
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

  expect_false(gap_is_ours(runs[1, ], iv))   # the scaffold's own gap
  expect_true(gap_is_ours(runs[2, ], iv))    # the spacer we inserted
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

# NCBI (S. Storz, 2026-08-28): a plain `gap` feature is enough, the number of Ns
# must BE the estimated length, and a feature spanning an estimated gap stays
# continuous. So the only question left is whether a run is one we inserted.

spacer_df <- function(start, end, size_known) {
  data.frame(start = start, end = end, size_known = size_known)
}

test_that("a run overlapping a spacer is ours", {
  expect_true(gap_is_ours(data.frame(start = 11, end = 22, length = 12),
                          spacer_df(11, 22, 1L)))
  # partial overlap still counts: a scaffold's own Ns can fuse with a spacer
  expect_true(gap_is_ours(data.frame(start = 1, end = 30, length = 30),
                          spacer_df(11, 22, 1L)))
})

test_that("a run we did not insert is not ours", {
  expect_false(gap_is_ours(data.frame(start = 500, end = 599, length = 100),
                           spacer_df(11, 110, 1L)))
  expect_false(gap_is_ours(data.frame(start = 1, end = 12, length = 12), NULL))
  expect_false(gap_is_ours(data.frame(start = 1, end = 12, length = 12),
                           spacer_df(integer(0), integer(0), integer(0))))
})

test_that("the gap feature carries only an estimated length", {
  fn <- withr::local_tempfile()
  write_tbl_gap(data.frame(start = 11L, end = 22L, length = 12L), fn)
  out <- readLines(fn)

  expect_equal(out[1], "11\t22\tgap")
  expect_equal(out[2], "\t\t\testimated_length\t12")
  # no gap_type, no linkage_evidence: NCBI says the plain feature is enough
  expect_length(out, 2L)
})

# --- a junction we cannot size is refused, not padded ------------------------

test_that("unsized gaps are picked out of a join", {
  iv <- data.frame(junction = c(1L, 2L), start = c(11L, 50L), end = c(22L, 149L),
                   length = c(12L, 100L), size_known = c(1L, 0L))
  expect_equal(nrow(unsized_gaps(iv)), 1L)
  expect_equal(unsized_gaps(iv)$start, 50L)

  sized_only <- iv[iv$size_known == 1L, , drop = FALSE]
  expect_equal(nrow(unsized_gaps(sized_only)), 0L)
  expect_equal(nrow(unsized_gaps(NULL) %||% data.frame()), 0L)
})

test_that("the refusal note says why and what to do instead", {
  note <- unsized_join_note(data.frame(start = 50L, end = 149L))
  expect_match(note, "could not be sized")
  expect_match(note, "estimated gap length")
  expect_match(note, "multiple sequences")
})

# --- the app writes its own provenance ---------------------------------------

junction_db <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbExecute(con, "CREATE TABLE scaffold_junctions (ID TEXT NOT NULL,
    junction INTEGER NOT NULL, gap_index INTEGER NOT NULL, start INTEGER,
    end INTEGER, gap_bases INTEGER, size_known INTEGER, time_stamp INTEGER,
    PRIMARY KEY (ID, gap_index))")
  con
}

test_that("a hand-built join records its gaps", {
  con <- junction_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  iv <- data.frame(junction = c(1L, 2L), start = c(11L, 50L), end = c(22L, 149L),
                   length = c(12L, 100L), size_known = c(1L, 0L))

  expect_equal(store_scaffold_junctions(con, "S1", iv), 2L)
  got <- DBI::dbGetQuery(con, "SELECT * FROM scaffold_junctions ORDER BY gap_index")
  expect_equal(got$start, c(11L, 50L))
  expect_equal(got$size_known, c(1L, 0L))
  expect_equal(got$gap_index, c(1L, 2L))
})

test_that("rebuilding replaces rather than accumulates", {
  con <- junction_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  iv1 <- data.frame(junction = 1L, start = 11L, end = 22L, length = 12L,
                    size_known = 1L)
  iv2 <- data.frame(junction = 1L, start = 90L, end = 99L, length = 10L,
                    size_known = 0L)
  store_scaffold_junctions(con, "S1", iv1)
  store_scaffold_junctions(con, "S1", iv2)

  got <- DBI::dbGetQuery(con, "SELECT * FROM scaffold_junctions")
  expect_equal(nrow(got), 1L)
  expect_equal(got$start, 90L)
})

test_that("deleting the consensus clears its intervals", {
  con <- junction_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  store_scaffold_junctions(con, "S1", data.frame(
    junction = 1L, start = 11L, end = 22L, length = 12L, size_known = 1L))
  store_scaffold_junctions(con, "S2", data.frame(
    junction = 1L, start = 5L, end = 14L, length = 10L, size_known = 1L))

  store_scaffold_junctions(con, "S1", NULL)
  got <- DBI::dbGetQuery(con, "SELECT ID FROM scaffold_junctions")
  expect_equal(got$ID, "S2")   # only the sample asked for is cleared
})

test_that("a project without the table is tolerated", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_equal(store_scaffold_junctions(con, "S1", NULL), 0L)
})
