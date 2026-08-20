# The shared circular-coordinate helpers. Curation, validation, export and the
# annotate editor all route their wrap-around handling through these, so a
# regression here is a regression everywhere.

L <- 900L
dna <- Biostrings::DNAString(paste(rep("ACGTTGCA", 200L), collapse = ""))
dna <- Biostrings::subseq(dna, 1L, L)

test_that("wrap_pos maps any coordinate onto [1, L]", {
  expect_equal(wrap_pos(1L, L), 1L)
  expect_equal(wrap_pos(L, L), L)
  expect_equal(wrap_pos(0L, L), L)
  expect_equal(wrap_pos(-1L, L), L - 1L)
  expect_equal(wrap_pos(L + 1L, L), 1L)
  expect_equal(wrap_pos(2L * L + 5L, L), 5L)
  expect_equal(wrap_pos(c(0L, 1L, L + 1L), L), c(L, 1L, 1L))
})

test_that("circ_len measures the arc, not the gap", {
  expect_equal(circ_len(100L, 200L, L), 101L)
  expect_equal(circ_len(850L, 150L, L), 201L) # 51 before the origin + 150 after
  expect_equal(circ_len(1L, L, L), L)
})

test_that("extract_circ_region reads across the origin", {
  expect_equal(
    as.character(extract_circ_region(dna, 850L, 150L)),
    paste0(
      as.character(Biostrings::subseq(dna, 850L, L)),
      as.character(Biostrings::subseq(dna, 1L, 150L))
    )
  )
  # non-wrapping input is plain subseq
  expect_equal(
    as.character(extract_circ_region(dna, 10L, 20L)),
    as.character(Biostrings::subseq(dna, 10L, 20L))
  )
  # width matches circ_len
  expect_equal(length(extract_circ_region(dna, 850L, 150L)), circ_len(850L, 150L, L))
})

test_that("circ_overlap sees both arms of a wrapping interval", {
  # focal wraps, compared intervals do not
  expect_equal(
    circ_overlap(850L, 150L, c(1L, 500L, 880L), c(67L, 600L, 890L)),
    c(TRUE, FALSE, TRUE)
  )
  # neither wraps
  expect_equal(
    circ_overlap(100L, 200L, c(150L, 300L), c(250L, 400L)),
    c(TRUE, FALSE)
  )
  # compared interval wraps, focal does not
  expect_equal(
    circ_overlap(10L, 20L, 850L, 150L),
    TRUE
  )
  # a single point inside a wrapping interval
  expect_true(circ_overlap(5L, 5L, 850L, 150L))
  expect_false(circ_overlap(500L, 500L, 850L, 150L))
})

test_that("circ_overlap_len sums both arms", {
  # trnF 1..67 sits entirely in the post-origin arm of 850..150
  expect_equal(circ_overlap_len(850L, 150L, 1L, 67L, L), 67L)
  # a gene in the numeric middle does not touch it at all
  expect_equal(circ_overlap_len(850L, 150L, 400L, 600L, L), 0L)
  # partial overlap of the pre-origin arm
  expect_equal(circ_overlap_len(850L, 150L, 800L, 860L, L), 11L) # 850..860
  # both wrap
  expect_equal(circ_overlap_len(850L, 150L, 880L, 50L, L), circ_len(880L, 50L, L))
})

test_that("splice_join_cds spans the origin instead of dropping the wrapping exon", {
  gc <- Biostrings::getGeneticCode("2")
  # exon1 850..30 crosses the origin (81 bp), exon2 100..150 (51 bp)
  members <- data.frame(
    pos1 = c(850L, 100L), pos2 = c(30L, 150L), direction = "+",
    start_codon = c("ATG", NA), stop_codon = c(NA, "TAA"),
    partial_start = 0L, partial_stop = 0L, stringsAsFactors = FALSE
  )
  # the function already warns that exon ORDER is approximate across the origin
  expect_warning(res <- splice_join_cds(members, dna, gc), "crosses the circular origin")

  # the shortest arc covering both exons starts at the wrapping exon
  expect_equal(res$pos1, 850L)
  expect_equal(res$pos2, 150L)
  expect_equal(res$length, 81 + 51)
  # min/max would have reported 100..150 and lost the 81 bp wrapping exon
  expect_gt(circ_len(res$pos1, res$pos2, L), res$length)
})

test_that("splice_join_cds keeps min/max span when no exon wraps", {
  gc <- Biostrings::getGeneticCode("2")
  members <- data.frame(
    pos1 = c(100L, 300L), pos2 = c(150L, 350L), direction = "+",
    start_codon = c("ATG", NA), stop_codon = c(NA, "TAA"),
    partial_start = 0L, partial_stop = 0L, stringsAsFactors = FALSE
  )
  res <- splice_join_cds(members, dna, gc)
  expect_equal(res$pos1, 100L)
  expect_equal(res$pos2, 350L)
})
