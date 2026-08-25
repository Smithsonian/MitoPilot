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

test_that("circularize_asmb leaves a multi-contig assembly untouched", {
  fa <- withr::local_tempfile(fileext = ".fasta")
  writeLines(
    c(">a", random_seq(1000, seed = 5), ">b", random_seq(1000, seed = 6)),
    fa
  )
  res <- circularize_asmb(fa)
  expect_false(res$circular)
  expect_equal(res$trimmed, 0L)
  expect_match(res$note, "more than one contig")
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
  local_mocked_bindings(count_junction_reads = function(...) 1L)
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

  local_mocked_bindings(count_junction_reads = function(...) 12L)
  res <- circularize_asmb(fa, paired_reads_1 = "r1.fq", paired_reads_2 = "r2.fq")
  expect_true(res$circular)
  expect_equal(res$trimmed, 300L)
  expect_match(res$note, "12 junction reads")
})
