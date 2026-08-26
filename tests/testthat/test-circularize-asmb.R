skip_if_no_blastn <- function() {
  testthat::skip_if(
    !nzchar(Sys.which(getOption("MitoPilot.blastn", "blastn"))),
    "blastn not installed"
  )
}

# Reproducible pseudo-random contig with no long internal repeats.
random_seq <- function(n, seed = 1) {
  set.seed(seed)
  paste(sample(c("A", "C", "G", "T"), n, replace = TRUE), collapse = "")
}

test_that("a duplicated contig end is trimmed back to the original length", {
  skip_if_no_blastn()
  core <- random_seq(6000)
  overlap <- substr(core, 1, 250)
  res <- trim_end_overlap(paste0(core, overlap))
  expect_equal(nchar(res$sequence), nchar(core))
  expect_equal(res$sequence, core)
  expect_equal(res$trimmed, 250L)
})

test_that("a clean linear contig is left alone", {
  skip_if_no_blastn()
  core <- random_seq(6000, seed = 2)
  res <- trim_end_overlap(core)
  expect_equal(res$trimmed, 0L)
  expect_equal(res$sequence, core)
})

test_that("an overlap shorter than min_overlap is rejected", {
  skip_if_no_blastn()
  core <- random_seq(6000, seed = 3)
  seq <- paste0(core, substr(core, 1, 100))
  expect_equal(trim_end_overlap(seq, min_overlap = 220)$trimmed, 0L)
  expect_equal(trim_end_overlap(seq, min_overlap = 80)$trimmed, 100L)
})

test_that("a rejected overlap is reported but not trimmed", {
  skip_if_no_blastn()
  core <- random_seq(6000, seed = 24)
  res <- trim_end_overlap(paste0(core, substr(core, 1, 100)), min_overlap = 220)
  expect_equal(res$trimmed, 0L)
  expect_equal(res$sequence, paste0(core, substr(core, 1, 100)))
  expect_false(res$hit$accepted)
  expect_match(res$hit$reason, "below the 220 bp minimum")
})

test_that("no end-anchored hit leaves no evidence", {
  skip_if_no_blastn()
  res <- trim_end_overlap(random_seq(6000, seed = 25))
  expect_equal(res$trimmed, 0L)
  expect_null(res$hit)
})

test_that("the trivial full-length self hit is not mistaken for an overlap", {
  skip_if_no_blastn()
  # Every sequence aligns to itself end to end; only the 90% rule keeps that hit
  # from being read as a redundant overlap.
  expect_null(find_end_overlap(random_seq(6000, seed = 4)))
})

test_that("a qualifying overlap comes back accepted with aligned strings", {
  skip_if_no_blastn()
  core <- random_seq(6000, seed = 21)
  hit <- find_end_overlap(paste0(core, substr(core, 1, 300)))
  expect_true(hit$accepted)
  expect_true(is.na(hit$reason))
  expect_equal(hit$trimmed, 300L)
  expect_equal(nchar(hit$qseq), nchar(hit$sseq))
  expect_equal(hit$mismatches, 0L)
})

test_that("an overlap below the length floor comes back rejected, not dropped", {
  skip_if_no_blastn()
  core <- random_seq(6000, seed = 22)
  hit <- find_end_overlap(paste0(core, substr(core, 1, 100)), min_overlap = 220)
  expect_false(hit$accepted)
  expect_match(hit$reason, "100 bp below the 220 bp minimum")
})

test_that("an overlap below the identity floor comes back rejected", {
  skip_if_no_blastn()
  core <- random_seq(6000, seed = 23)
  hit <- find_end_overlap(paste0(core, substr(core, 1, 400)), min_identity = 100.5)
  expect_false(hit$accepted)
  expect_match(hit$reason, "identical, below")
})

test_that("a tandem duplication collapses to a single copy", {
  skip_if_no_blastn()
  unit <- random_seq(3000, seed = 10)
  res <- trim_end_overlap(paste0(unit, unit))
  expect_equal(res$sequence, unit)
})

test_that("cigar_ref_length ignores soft clips and insertions", {
  expect_equal(cigar_ref_length("100M"), 100L)
  expect_equal(cigar_ref_length("10S90M"), 90L)
  expect_equal(cigar_ref_length("50M2D48M"), 100L)
  expect_equal(cigar_ref_length("50M2I48M"), 98L)
  expect_equal(cigar_ref_length("*"), NA_integer_)
})

test_that("circularize_asmb leaves a multi-contig assembly with no overlaps alone", {
  skip_if_no_blastn()
  fa <- withr::local_tempfile(fileext = ".fasta")
  writeLines(
    c(">a", random_seq(1000, seed = 5), ">b", random_seq(1000, seed = 6)),
    fa
  )
  res <- circularize_asmb(fa)
  expect_false(res$circular)
  expect_equal(res$trimmed, 0L)
  expect_equal(vapply(res$contigs, function(x) x$contig, character(1)), c("a", "b"))
  expect_match(res$note, "a: linear")
})

test_that("circularize_asmb attempts every contig and maps reads only for accepted overlaps", {
  skip_if_no_blastn()
  circ <- random_seq(6000, seed = 51)
  short <- random_seq(6000, seed = 52)
  plain <- random_seq(6000, seed = 53)
  fa <- withr::local_tempfile(fileext = ".fasta")
  writeLines(c(
    ">c_circular some description", paste0(circ, substr(circ, 1, 300)),
    ">c_short", paste0(short, substr(short, 1, 100)),
    ">c_plain", plain
  ), fa)

  mapped <- character(0)
  local_mocked_bindings(count_junction_reads = function(seq, ...) {
    mapped <<- c(mapped, substr(seq, 1, 20))
    list(count = 12L, window_bp = 500L,
         depth = data.frame(position = integer(0), rel_position = integer(0),
                            depth = integer(0), depth_spanning = integer(0)))
  })
  res <- circularize_asmb(fa, paired_reads_1 = "r1.fq", paired_reads_2 = "r2.fq")

  expect_equal(vapply(res$contigs, function(x) x$contig, character(1)),
               c("c_circular", "c_short", "c_plain"))
  expect_equal(vapply(res$contigs, function(x) x$circular, logical(1)),
               c(TRUE, FALSE, FALSE))
  expect_equal(vapply(res$contigs, function(x) as.integer(x$trimmed), integer(1)),
               c(300L, 0L, 0L))
  expect_match(res$contigs[[2]]$note, "below the 220 bp minimum")
  expect_equal(res$contigs[[3]]$note, "linear: no self-overlap found")
  expect_false(res$circular)
  expect_equal(res$trimmed, 300L)

  # Reads mapped for the accepted contig only, never for the other two.
  expect_equal(mapped, substr(circ, 1, 20))
})

test_that("circularize_asmb writes the trimmed multi-contig assembly and per-contig evidence", {
  skip_if_no_blastn()
  circ <- random_seq(6000, seed = 54)
  plain <- random_seq(6000, seed = 55)
  fa <- withr::local_tempfile(fileext = ".fasta")
  out <- withr::local_tempfile(fileext = ".fasta")
  ev <- withr::local_tempdir()
  writeLines(c(">c1", paste0(circ, substr(circ, 1, 300)), ">c2", plain), fa)

  circularize_asmb(fa, id = "s5", out_fn = out, evidence_dir = ev)

  got <- Biostrings::readDNAStringSet(out)
  expect_equal(names(got), c("c1", "c2"))
  expect_equal(as.character(got[[1]]), circ)
  expect_equal(as.character(got[[2]]), plain)
  ov <- utils::read.csv(file.path(ev, "circularize_overlap.csv"))
  expect_equal(nrow(ov), 1L)
  expect_equal(ov$trimmed, 300L)
})

test_that("circularize_asmb without reads calls a trimmed contig circular", {
  skip_if_no_blastn()
  core <- random_seq(6000, seed = 7)
  fa <- withr::local_tempfile(fileext = ".fasta")
  out <- withr::local_tempfile(fileext = ".fasta")
  writeLines(c(">contig", paste0(core, substr(core, 1, 300))), fa)
  res <- circularize_asmb(fa, out_fn = out)
  expect_true(res$circular)
  expect_equal(res$trimmed, 300L)
  expect_equal(as.character(Biostrings::readDNAStringSet(out)[[1]]), core)
})

test_that("circularize_asmb keeps a contig linear when reads do not span the junction", {
  skip_if_no_blastn()
  core <- random_seq(6000, seed = 8)
  fa <- withr::local_tempfile(fileext = ".fasta")
  writeLines(c(">contig", paste0(core, substr(core, 1, 300))), fa)

  # Stub the read check rather than shipping a fastq: the mapping itself is
  # bowtie2's business, the veto logic is ours.
  local_mocked_bindings(count_junction_reads = function(...) {
    list(count = 1L, window_bp = 500L,
         depth = data.frame(position = integer(0), rel_position = integer(0),
                            depth = integer(0), depth_spanning = integer(0)))
  })
  res <- circularize_asmb(fa, paired_reads_1 = "r1.fq", paired_reads_2 = "r2.fq")
  expect_false(res$circular)
  expect_equal(res$trimmed, 0L)
  expect_match(res$note, "only 1 junction read")
})

test_that("circularize_asmb calls a contig circular when reads span the junction", {
  skip_if_no_blastn()
  core <- random_seq(6000, seed = 9)
  fa <- withr::local_tempfile(fileext = ".fasta")
  writeLines(c(">contig", paste0(core, substr(core, 1, 300))), fa)

  local_mocked_bindings(count_junction_reads = function(...) {
    list(count = 12L, window_bp = 500L,
         depth = data.frame(position = integer(0), rel_position = integer(0),
                            depth = integer(0), depth_spanning = integer(0)))
  })
  res <- circularize_asmb(fa, paired_reads_1 = "r1.fq", paired_reads_2 = "r2.fq")
  expect_true(res$circular)
  expect_equal(res$trimmed, 300L)
  expect_match(res$note, "12 junction reads")
})

test_that("window_depth counts overlapping intervals per position", {
  # Two reads over a 10 bp contig, junction at position 10, window +/- 3.
  d <- window_depth(starts = c(8L, 9L), ends = c(12L, 11L),
                    win_start = 8L, win_end = 13L)
  expect_length(d, 6L)
  expect_equal(d, c(1L, 2L, 2L, 2L, 1L, 0L))
})

test_that("window_depth clips intervals to the window", {
  d <- window_depth(starts = 1L, ends = 100L, win_start = 8L, win_end = 13L)
  expect_equal(d, rep(1L, 6L))
})

test_that("window_depth handles no intervals", {
  d <- window_depth(starts = integer(0), ends = integer(0),
                    win_start = 8L, win_end = 13L)
  expect_equal(d, rep(0L, 6L))
})

test_that("count_junction_reads window_bp is integer on the empty-return path", {
  # Contig too short for flank to clear min_overhang, forces the empty return.
  res <- count_junction_reads("A", "NA", "NA", min_overhang = 30)
  expect_type(res$window_bp, "integer")
  expect_type(res$depth$position, "integer")
})

test_that("count_junction_reads window_bp and position are integer on a populated window", {
  local_mocked_bindings(
    system = function(...) invisible(0L),
    system2 = function(command, args, ...) {
      if (identical(command, "samtools")) {
        return(paste("read1", "99", "junction", "1900", "60", "200M",
                     "=", "1900", "200", "SEQ", "QUAL", sep = "\t"))
      }
      character(0)
    },
    .package = "base"
  )
  res <- count_junction_reads(random_seq(2000, seed = 42), "r1.fq", "r2.fq",
                              min_overhang = 30)
  expect_type(res$window_bp, "integer")
  expect_type(res$depth$position, "integer")
})

test_that("evidence is written for an accepted overlap with no reads", {
  skip_if_no_blastn()
  core <- random_seq(6000, seed = 26)
  fa <- withr::local_tempfile(fileext = ".fasta")
  ev <- withr::local_tempdir()
  writeLines(c(">contig", paste0(core, substr(core, 1, 300))), fa)

  circularize_asmb(fa, id = "s1", evidence_dir = ev)

  ov <- utils::read.csv(file.path(ev, "circularize_overlap.csv"))
  expect_equal(nrow(ov), 1L)
  expect_equal(ov$ID, "s1")
  expect_equal(ov$accepted, 1L)
  expect_equal(ov$trimmed, 300L)
  expect_equal(nchar(ov$aln_query), nchar(ov$aln_subject))
  # No reads, so the depth file is header only
  dp <- utils::read.csv(file.path(ev, "circularize_depth.csv"))
  expect_equal(nrow(dp), 0L)
})

test_that("evidence is written for a rejected overlap", {
  skip_if_no_blastn()
  core <- random_seq(6000, seed = 27)
  fa <- withr::local_tempfile(fileext = ".fasta")
  ev <- withr::local_tempdir()
  writeLines(c(">contig", paste0(core, substr(core, 1, 100))), fa)

  circularize_asmb(fa, id = "s2", evidence_dir = ev, min_overlap = 220)

  ov <- utils::read.csv(file.path(ev, "circularize_overlap.csv"))
  expect_equal(nrow(ov), 1L)
  expect_equal(ov$accepted, 0L)
  expect_equal(ov$trimmed, 0L)
  expect_match(ov$reason, "below the 220 bp minimum")
})

test_that("no end-anchored hit writes header-only evidence", {
  skip_if_no_blastn()
  fa <- withr::local_tempfile(fileext = ".fasta")
  ev <- withr::local_tempdir()
  writeLines(c(">contig", random_seq(6000, seed = 28)), fa)

  circularize_asmb(fa, id = "s3", evidence_dir = ev)

  expect_equal(nrow(utils::read.csv(file.path(ev, "circularize_overlap.csv"))), 0L)
  expect_equal(nrow(utils::read.csv(file.path(ev, "circularize_depth.csv"))), 0L)
})

test_that("a multi-contig assembly still writes both files", {
  skip_if_no_blastn()
  fa <- withr::local_tempfile(fileext = ".fasta")
  ev <- withr::local_tempdir()
  writeLines(c(">a", random_seq(1000, seed = 29), ">b", random_seq(1000, seed = 30)), fa)

  circularize_asmb(fa, id = "s4", evidence_dir = ev)

  expect_true(file.exists(file.path(ev, "circularize_overlap.csv")))
  expect_true(file.exists(file.path(ev, "circularize_depth.csv")))
  expect_equal(nrow(utils::read.csv(file.path(ev, "circularize_overlap.csv"))), 0L)
})


test_that("the overlap hit carries flanking contig context", {
  skip_if_no_blastn()
  core <- random_seq(6000, seed = 41)
  hit <- find_end_overlap(paste0(core, substr(core, 1, 400)))
  expect_true(hit$accepted)
  # The 5' copy sits at the contig start, so it has no room on its left.
  expect_equal(nchar(hit$q_ctx_left), 0L)
  expect_equal(nchar(hit$q_ctx_right), 50L)
  # The 3' copy runs to the contig end, so no room on its right.
  expect_equal(nchar(hit$s_ctx_left), 50L)
  expect_equal(nchar(hit$s_ctx_right), 0L)
  # Context is real contig sequence, taken from just past the aligned block.
  expect_equal(hit$q_ctx_right, substr(core, hit$qend + 1L, hit$qend + 50L))
})

test_that("the evidence CSV carries the context columns", {
  skip_if_no_blastn()
  core <- random_seq(6000, seed = 42)
  fa <- withr::local_tempfile(fileext = ".fasta")
  ev <- withr::local_tempdir()
  writeLines(c(">contig", paste0(core, substr(core, 1, 400))), fa)

  circularize_asmb(fa, id = "s1", evidence_dir = ev)

  ov <- utils::read.csv(file.path(ev, "circularize_overlap.csv"),
                        colClasses = "character")
  expect_true(all(c("q_ctx_left", "q_ctx_right", "s_ctx_left", "s_ctx_right")
                  %in% names(ov)))
  expect_equal(nchar(ov$q_ctx_right), 50L)
  expect_equal(nchar(ov$s_ctx_left), 50L)
})
