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

# extend_oh_to_ctrl(): the control region fills the gap between the OH's
# neighbours. A feature spanning the origin sorts last by pos1 while also sitting
# at [1, pos2], so it - not the contig edge - bounds an OH at either end.
oh_tbl <- function(...) {
  rows <- list(...)
  data.frame(
    contig = "ctg1",
    gene = vapply(rows, `[[`, character(1), 1),
    pos1 = vapply(rows, function(r) as.integer(r[[2]]), integer(1)),
    pos2 = vapply(rows, function(r) as.integer(r[[3]]), integer(1)),
    length = 0L, stringsAsFactors = FALSE
  )
}
lens <- c(ctg1 = 900L)

test_that("an interior OH is extended to its neighbours whether or not anything wraps", {
  # OH between trnP (300-370) and trnF (600-670)
  base <- oh_tbl(list("trnP", 300, 370), list("OH", 400, 450), list("trnF", 600, 670))
  res <- extend_oh_to_ctrl(base, lens)
  expect_equal(res$gene[2], "ctrl")
  expect_equal(c(res$pos1[2], res$pos2[2]), c(371L, 599L))
  expect_equal(res$length[2], 229L)

  # same layout, plus a gene crossing the origin (sorts last by pos1)
  with_wrap <- rbind(base, oh_tbl(list("atp8", 850, 100)))
  res2 <- extend_oh_to_ctrl(with_wrap, lens)
  expect_equal(c(res2$pos1[2], res2$pos2[2]), c(371L, 599L))
})

test_that("an OH at the table edges is bounded by the contig when nothing wraps", {
  res <- extend_oh_to_ctrl(
    oh_tbl(list("OH", 50, 80), list("trnF", 600, 670)), lens
  )
  expect_equal(c(res$pos1[1], res$pos2[1]), c(1L, 599L))

  res <- extend_oh_to_ctrl(
    oh_tbl(list("trnF", 100, 170), list("OH", 800, 850)), lens
  )
  expect_equal(c(res$pos1[2], res$pos2[2]), c(171L, 900L))
})

test_that("an OH at the table edges is bounded by the origin-spanning feature", {
  # atp8 850..100 wraps, so it occupies 1..100 and 850..900
  res <- extend_oh_to_ctrl(
    oh_tbl(list("OH", 200, 250), list("trnF", 600, 670), list("atp8", 850, 100)),
    lens
  )
  # first row: bounded below by the wrapping feature's far end, not by 1
  expect_equal(res$pos1[1], 101L)
  expect_equal(res$pos2[1], 599L)

  res <- extend_oh_to_ctrl(
    oh_tbl(list("trnF", 100, 170), list("atp8", 300, 200), list("OH", 700, 750)),
    lens
  )
  # last row: bounded above by the wrapping feature's start, not by the contig end
  expect_equal(res$pos2[3], 299L)
})

# extend_oh_to_ctrl ----
# The OH call is grown into the control region using its neighbours. A feature
# spanning the origin sorts last by pos1 while occupying [1, pos2], so it, not
# the contig edge, bounds an OH at either end of the table.

oh_lens <- c(ctg1 = 10000L)

oh_row <- function(gene, pos1, pos2, contig = "ctg1") {
  data.frame(contig = contig, gene = gene, pos1 = as.integer(pos1),
             pos2 = as.integer(pos2), length = 0L, stringsAsFactors = FALSE)
}

test_that("an OH at the head of the table is bounded by the wrapping feature", {
  ann <- rbind(oh_row("OH", 300, 320), oh_row("nad2", 500, 700),
               oh_row("nad1", 9800, 100))
  res <- extend_oh_to_ctrl(ann, oh_lens)
  expect_equal(res$gene[1], "ctrl")
  expect_equal(c(res$pos1[1], res$pos2[1]), c(101L, 499L))
  expect_equal(res$length[1], 399L)
})

test_that("the ctrl region clears EVERY feature spanning the origin", {
  # nad1 9700..50 and trnP 9900..120 both cover the origin; the first free base
  # after the origin is 121, not 51.
  ann <- rbind(oh_row("OH", 300, 320), oh_row("nad2", 500, 700),
               oh_row("nad1", 9700, 50), oh_row("trnP", 9900, 120))
  res <- extend_oh_to_ctrl(ann, oh_lens)
  expect_equal(c(res$pos1[1], res$pos2[1]), c(121L, 499L))
})

test_that("crossed bounds keep the called coordinates instead of inverting", {
  # OH 130..140 sits inside the post-origin arm of nad1, so its neighbours give
  # pos1 = 201 and pos2 = 149. An inverted region reads as a wrap downstream.
  ann <- rbind(oh_row("OH", 130, 140), oh_row("nad2", 150, 400),
               oh_row("nad1", 9800, 200))
  res <- extend_oh_to_ctrl(ann, oh_lens)
  expect_equal(c(res$pos1[1], res$pos2[1]), c(130L, 140L))
  expect_equal(res$gene[1], "ctrl")
})

test_that("without a wrapping feature the contig edges still bound the ctrl region", {
  res <- extend_oh_to_ctrl(rbind(oh_row("OH", 50, 60), oh_row("nad2", 500, 700)), oh_lens)
  expect_equal(c(res$pos1[1], res$pos2[1]), c(1L, 499L))
  res2 <- extend_oh_to_ctrl(rbind(oh_row("nad2", 500, 700), oh_row("OH", 5000, 5010)), oh_lens)
  expect_equal(c(res2$pos1[2], res2$pos2[2]), c(701L, 10000L))
})

test_that("a wrapping feature on another contig does not bound this OH", {
  l2 <- c(ctg1 = 10000L, ctg2 = 8000L)
  ann <- rbind(
    oh_row("OH", 50, 60), oh_row("nad2", 500, 700),
    oh_row("nad5", 100, 300, contig = "ctg2"), oh_row("nad6", 7900, 40, contig = "ctg2")
  )
  res <- extend_oh_to_ctrl(ann, l2)
  expect_equal(c(res$pos1[1], res$pos2[1]), c(1L, 499L))
})

test_that("a ctrl region spanning the origin gets a circular length", {
  ann <- rbind(oh_row("nad2", 100, 400), oh_row("OH", 9500, 9600))
  res <- extend_oh_to_ctrl(ann, oh_lens)
  expect_equal(c(res$pos1[2], res$pos2[2]), c(401L, 10000L))
  expect_equal(res$length[2], 9600L)
})

test_that("circ_overlap is symmetric and agrees with circ_overlap_len", {
  set.seed(1)
  for (i in 1:200) {
    p <- sample.int(L, 2); q <- sample.int(L, 2)
    expect_equal(
      as.logical(circ_overlap(p[1], p[2], q[1], q[2])),
      as.logical(circ_overlap(q[1], q[2], p[1], p[2])),
      info = paste(c(p, q), collapse = " ")
    )
    expect_equal(
      as.logical(circ_overlap(p[1], p[2], q[1], q[2])),
      circ_overlap_len(p[1], p[2], q[1], q[2], L) > 0L,
      info = paste(c(p, q), collapse = " ")
    )
  }
})

test_that("circ_overlap_len measures two intervals that both span the origin", {
  expect_equal(circ_overlap_len(850, 100, 800, 50, L), circ_len(850, 50, L))
  expect_equal(circ_overlap_len(850, 100, 850, 100, L), circ_len(850, 100, L))
})
