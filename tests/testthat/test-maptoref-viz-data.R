mtr_viz_project <- function(id = "S1", opts = "default", len = 20L,
                            with_features = TRUE) {
  d <- withr::local_tempdir(.local_envir = parent.frame())
  work <- file.path(d, id, "assemble", opts, "maptoref")
  dir.create(work, recursive = TRUE)
  utils::write.csv(
    data.frame(Position = seq_len(len), Depth = seq_len(len) * 2),
    file.path(work, "maptoref_depth.csv"), row.names = FALSE, quote = FALSE
  )
  if (with_features) {
    utils::write.csv(
      data.frame(
        type = c("CDS", "tRNA"), gene = c("ND1", "trnQ"),
        start = c(2L, 12L), end = c(9L, 16L), strand = c("+", "-")
      ),
      file.path(work, "maptoref_features.csv"), row.names = FALSE, quote = TRUE
    )
  }
  writeLines(c(">TESTREF circular", strrep("ACGTA", len %/% 5L)),
             file.path(work, "ref.fasta"))
  writeLines(c(">S1.1.1 subs_only", strrep("ACGTT", len %/% 5L)),
             file.path(work, "subs_only.fasta"))
  writeLines(c(
    "assembler=MapToRef",
    "accession=NC_000001.1",
    "organism=Testus testus",
    "reference_length=20",
    "reads_mapped_final=1234",
    "n_count=2",
    "note=first note",
    "note=second note"
  ), file.path(d, id, "assemble", opts, paste0(id, "_summary.txt")))
  list(dir_out = d, id = id, opts = opts, work = work)
}

test_that("maptoref_paths builds every path from the project convention", {
  p <- maptoref_paths("/out", "S1", "default")
  expect_equal(
    sort(names(p)),
    sort(c("dir", "work", "ref_fasta", "consensus", "bam", "bai", "gb",
           "depth", "features", "summary"))
  )
  expect_equal(p$dir, file.path("/out", "S1", "assemble", "default"))
  expect_equal(p$work, file.path("/out", "S1", "assemble", "default", "maptoref"))
  expect_equal(p$bam, file.path(p$work, "final.bam"))
  expect_equal(p$bai, file.path(p$work, "final.bam.bai"))
  expect_equal(p$gb, file.path(p$work, "reference.gb"))
  expect_equal(p$ref_fasta, file.path(p$work, "ref.fasta"))
  expect_equal(p$consensus, file.path(p$work, "subs_only.fasta"))
  expect_equal(p$depth, file.path(p$work, "maptoref_depth.csv"))
  expect_equal(p$features, file.path(p$work, "maptoref_features.csv"))
  expect_equal(p$summary, file.path(p$dir, "S1_summary.txt"))
})

test_that("maptoref_read_depth reads the CSV", {
  pr <- mtr_viz_project()
  p <- maptoref_paths(pr$dir_out, pr$id, pr$opts)
  out <- maptoref_read_depth(p$depth)
  expect_equal(nrow(out), 20L)
  expect_equal(out$Position, 1:20)
  expect_equal(out$Depth[3], 6)
})

test_that("maptoref_read_depth returns zero rows when absent", {
  out <- maptoref_read_depth(file.path(tempdir(), "nope.csv"))
  expect_equal(nrow(out), 0L)
  expect_equal(names(out), c("Position", "Depth"))
})

test_that("maptoref_read_features reads the CSV", {
  pr <- mtr_viz_project()
  p <- maptoref_paths(pr$dir_out, pr$id, pr$opts)
  out <- maptoref_read_features(p$features)
  expect_equal(nrow(out), 2L)
  expect_equal(out$gene, c("ND1", "trnQ"))
  expect_equal(out$strand, c("+", "-"))
})

test_that("maptoref_read_features returns zero rows when absent", {
  pr <- mtr_viz_project(with_features = FALSE)
  p <- maptoref_paths(pr$dir_out, pr$id, pr$opts)
  out <- maptoref_read_features(p$features)
  expect_equal(nrow(out), 0L)
  expect_equal(names(out), c("type", "gene", "start", "end", "strand"))
})

test_that("maptoref_read_summary parses key=value and collapses notes", {
  pr <- mtr_viz_project()
  p <- maptoref_paths(pr$dir_out, pr$id, pr$opts)
  s <- maptoref_read_summary(p$summary)
  expect_equal(unname(s["organism"]), "Testus testus")
  expect_equal(unname(s["reference_length"]), "20")
  expect_equal(unname(s["note"]), "first note | second note")
})

test_that("maptoref_read_summary returns nothing when absent", {
  expect_length(maptoref_read_summary(file.path(tempdir(), "nope.txt")), 0L)
})

test_that("maptoref_bin_depth leaves a short series alone", {
  d <- data.frame(Position = 1:10, Depth = as.numeric(1:10))
  expect_identical(maptoref_bin_depth(d, n = 2000L), d)
})

test_that("maptoref_bin_depth keeps spikes and respects the point cap", {
  d <- data.frame(Position = 1:1000, Depth = rep(1, 1000))
  d$Depth[c(137, 851)] <- 999
  out <- maptoref_bin_depth(d, n = 100L)
  expect_lte(nrow(out), 100L)
  expect_equal(max(out$Depth), 999)
  expect_equal(sum(out$Depth == 999), 2L)
  expect_false(is.unsorted(out$Position))
})

test_that("maptoref_bin_depth returns zero rows for zero rows", {
  d <- data.frame(Position = integer(0), Depth = numeric(0))
  expect_equal(nrow(maptoref_bin_depth(d, n = 100L)), 0L)
})

test_that("maptoref_read_seq reads and uppercases the first record", {
  pr <- mtr_viz_project()
  p <- maptoref_paths(pr$dir_out, pr$id, pr$opts)
  s <- maptoref_read_seq(p$ref_fasta)
  expect_equal(nchar(s), 20L)
  expect_equal(substr(s, 1, 5), "ACGTA")
  expect_equal(substr(maptoref_read_seq(p$consensus), 1, 5), "ACGTT")
})

test_that("maptoref_read_seq returns NA when absent", {
  expect_true(is.na(maptoref_read_seq(file.path(tempdir(), "nope.fasta"))))
})

mtr_viz_ref <- function() paste(rep("ACGTACGTAC", 6), collapse = "")

mtr_viz_bam <- function(envir = parent.frame()) {
  d <- withr::local_tempdir(.local_envir = envir)
  sam <- file.path(d, "t.sam")
  writeLines(c(
    "@HD\tVN:1.6\tSO:coordinate",
    "@SQ\tSN:mapping_ref\tLN:60",
    # perfect match
    "r1\t0\tmapping_ref\t1\t60\t10M\t*\t0\t0\tACGTACGTAC\tIIIIIIIIII",
    # 2 bp insertion after reference position 4
    "r3\t0\tmapping_ref\t1\t60\t4M2I4M\t*\t0\t0\tACGTTTACGT\tIIIIIIIIII",
    # 2 bp soft clip, then a perfect 8 bp match at 11
    "r5\t0\tmapping_ref\t11\t60\t2S8M\t*\t0\t0\tGGACGTACGT\tIIIIIIIIII",
    # reverse strand, single mismatch at reference position 25
    "r2\t16\tmapping_ref\t21\t60\t10M\t*\t0\t0\tACGTTCGTAC\tIIIIIIIIII",
    # 2 bp deletion of reference positions 35 and 36
    "r4\t0\tmapping_ref\t31\t60\t4M2D4M\t*\t0\t0\tACGTGTAC\tIIIIIIII"
  ), sam)
  Rsamtools::asBam(sam, file.path(d, "t"), overwrite = TRUE,
                   indexDestination = TRUE)
}

test_that(".mtr_cigar_walk reports a perfect match as clean", {
  w <- .mtr_cigar_walk(1L, "10M", "ACGTACGTAC", mtr_viz_ref())
  expect_equal(w$start, 1L)
  expect_equal(w$end, 10L)
  expect_null(w$mm)
  expect_null(w$del)
  expect_null(w$ins)
})

test_that(".mtr_cigar_walk finds a single mismatch and its base", {
  w <- .mtr_cigar_walk(21L, "10M", "ACGTTCGTAC", mtr_viz_ref())
  expect_equal(nrow(w$mm), 1L)
  expect_equal(w$mm$pos, 25L)
  expect_equal(w$mm$base, "T")
})

test_that(".mtr_cigar_walk records an insertion without consuming reference", {
  w <- .mtr_cigar_walk(1L, "4M2I4M", "ACGTTTACGT", mtr_viz_ref())
  expect_equal(nrow(w$ins), 1L)
  expect_equal(w$ins$pos, 4L)
  expect_equal(w$ins$len, 2L)
  expect_null(w$mm)
  expect_equal(w$end, 8L)
})

test_that(".mtr_cigar_walk records a deletion and skips the reference", {
  w <- .mtr_cigar_walk(31L, "4M2D4M", "ACGTGTAC", mtr_viz_ref())
  expect_equal(nrow(w$del), 1L)
  expect_equal(w$del$start, 35L)
  expect_equal(w$del$end, 36L)
  expect_null(w$mm)
  expect_equal(w$end, 40L)
})

test_that(".mtr_cigar_walk ignores soft-clipped bases", {
  w <- .mtr_cigar_walk(11L, "2S8M", "GGACGTACGT", mtr_viz_ref())
  expect_null(w$mm)
  expect_equal(w$start, 11L)
  expect_equal(w$end, 18L)
})

test_that(".mtr_stack_rows puts disjoint reads on one row", {
  rows <- .mtr_stack_rows(c(1L, 20L, 40L), c(10L, 30L, 50L))
  expect_equal(rows, c(1L, 1L, 1L))
})

test_that(".mtr_stack_rows pushes an overlapping read to the next row", {
  rows <- .mtr_stack_rows(c(1L, 5L, 9L), c(10L, 14L, 18L))
  expect_equal(rows, c(1L, 2L, 3L))
})

test_that(".mtr_stack_rows reuses a row once the previous read has ended", {
  rows <- .mtr_stack_rows(c(1L, 5L, 20L), c(10L, 14L, 30L))
  expect_equal(rows, c(1L, 2L, 1L))
})

test_that("maptoref_window_reads returns every read overlapping the window", {
  bam <- mtr_viz_bam()
  out <- maptoref_window_reads(bam, 1L, 60L, mtr_viz_ref())
  expect_equal(out$n_total, 5L)
  expect_equal(out$n_shown, 5L)
  expect_equal(nrow(out$reads), 5L)
  expect_true(all(out$reads$row >= 1L))
  expect_setequal(as.character(out$reads$strand), c("+", "-"))
})

test_that("maptoref_window_reads carries mismatches, indels, and strand", {
  bam <- mtr_viz_bam()
  out <- maptoref_window_reads(bam, 1L, 60L, mtr_viz_ref())
  expect_equal(nrow(out$mm), 1L)
  expect_equal(out$mm$pos, 25L)
  expect_equal(out$mm$base, "T")
  expect_equal(nrow(out$del), 1L)
  expect_equal(out$del$start, 35L)
  expect_equal(nrow(out$ins), 1L)
  expect_equal(out$ins$pos, 4L)
  minus <- out$reads[as.character(out$reads$strand) == "-", ]
  expect_equal(minus$start, 21L)
})

test_that("maptoref_window_reads restricts to the requested window", {
  bam <- mtr_viz_bam()
  out <- maptoref_window_reads(bam, 31L, 45L, mtr_viz_ref())
  expect_equal(out$n_total, 1L)
  expect_equal(out$reads$start, 31L)
})

test_that("maptoref_window_reads includes a read ending on the window's first base", {
  bam <- mtr_viz_bam()
  # r2 spans 21-30, so a window starting at 30 overlaps it by one base.
  out <- maptoref_window_reads(bam, 30L, 45L, mtr_viz_ref())
  expect_equal(out$n_total, 2L)
  expect_true(all(c(21L, 31L) %in% out$reads$start))
})

test_that("maptoref_window_reads caps the rows it draws and reports the total", {
  bam <- mtr_viz_bam()
  # order() breaks the (start=1,start=1) tie between r1 and r3 by end
  # ascending, so r3 (end 8) sorts and packs before r1 (end 10): row 1
  # ends up holding r3, r5, r2 (3 reads); row 2 holds r1, r4.
  out <- maptoref_window_reads(bam, 1L, 60L, mtr_viz_ref(), max_reads = 1L)
  expect_equal(out$n_total, 5L)
  expect_equal(out$n_shown, 3L)
  expect_equal(nrow(out$reads), 3L)
  expect_true(all(out$reads$row == 1L))
})

test_that("maptoref_window_reads returns an empty result for an empty window", {
  bam <- mtr_viz_bam()
  out <- maptoref_window_reads(bam, 55L, 60L, mtr_viz_ref())
  expect_equal(out$n_total, 0L)
  expect_equal(nrow(out$reads), 0L)
  expect_equal(nrow(out$mm), 0L)
})

mtr_viz_bam_circular <- function(envir = parent.frame()) {
  d <- withr::local_tempdir(.local_envir = envir)
  sam <- file.path(d, "c.sam")
  writeLines(c(
    "@HD\tVN:1.6\tSO:coordinate",
    "@SQ\tSN:mapping_ref\tLN:70",
    "r1\t0\tmapping_ref\t1\t60\t10M\t*\t0\t0\tACGTACGTAC\tIIIIIIIIII",
    # tail read: bam positions 65-70 fold back to reference positions 5-10
    "rF\t0\tmapping_ref\t65\t60\t6M\t*\t0\t0\tACGTAC\tIIIIII"
  ), sam)
  Rsamtools::asBam(sam, file.path(d, "c"), overwrite = TRUE,
                   indexDestination = TRUE)
}

test_that("maptoref_window_reads folds back reads mapped into the circular flank", {
  bam <- mtr_viz_bam_circular()
  out <- maptoref_window_reads(bam, 1L, 10L, mtr_viz_ref(), ref_len = 60L)
  expect_true("rF" %in% out$reads$read)
  folded <- out$reads[out$reads$read == "rF", ]
  expect_equal(folded$start, 5L)
  expect_equal(folded$end, 10L)
})

test_that("maptoref_window_reads leaves a linear reference's behaviour unchanged", {
  bam <- mtr_viz_bam()
  out <- maptoref_window_reads(bam, 1L, 60L, mtr_viz_ref())
  expect_equal(out$n_total, 5L)
  expect_false(any(grepl("^rF$", out$reads$read)))
})

test_that("maptoref_window_reads returns an empty result for a missing BAM", {
  out <- maptoref_window_reads(file.path(tempdir(), "nope.bam"), 1L, 60L,
                               mtr_viz_ref())
  expect_equal(out$n_total, 0L)
  expect_equal(nrow(out$reads), 0L)
})
