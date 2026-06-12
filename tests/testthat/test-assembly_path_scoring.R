# Tests for the multi-path scoring, conflict classification, and resolution
# helpers in R/assembly_path_scoring.R. All functions are pure (no Shiny).

# Helper: a clean single circular path.
clean_path <- function(p = 0, len = 16500, topo = "circular",
                       species = "Genus species",
                       lineage = "Eukaryota;Metazoa;Genus") {
  data.frame(
    path = p, scaffold = 1, topology = topo, length = len,
    sequence = strrep("ACGT", 10),
    blast_species = species, blast_lineage = lineage,
    blast_pident = 99, blast_qcovs = 98, stringsAsFactors = FALSE
  )
}

test_that("count_ambiguities counts N and IUPAC, ignores ACGT and gaps", {
  expect_equal(count_ambiguities("ACGT"), 0L)
  expect_equal(count_ambiguities("AC-GT"), 0L)
  expect_equal(count_ambiguities("ACGTN"), 1L)
  expect_equal(count_ambiguities("ACGTRYN"), 3L)
  expect_equal(count_ambiguities(NA), 0L)
})

test_that("iupac_code maps base sets correctly and drops gaps", {
  expect_equal(iupac_code(c("A", "A")), "A")
  expect_equal(iupac_code(c("A", "G")), "R")
  expect_equal(iupac_code(c("C", "T")), "Y")
  expect_equal(iupac_code(c("A", "-", "T")), "W")
  expect_equal(iupac_code(c("A", "C", "G", "T")), "N")
  expect_equal(iupac_code(c("-", "-")), "N")
})

test_that("score_assembly_paths ranks a clean circular path above a poor one", {
  pdf <- rbind(
    clean_path(0),
    # poor: linear, divergent BLAST, will get low concordance only with a 3rd
    data.frame(path = 1, scaffold = 1, topology = "linear", length = 14000,
               sequence = paste0(strrep("ACGT", 9), "NNNN"),
               blast_species = "Other org", blast_lineage = "Bacteria;Other",
               blast_pident = 82, blast_qcovs = 70, stringsAsFactors = FALSE)
  )
  cov <- list(
    "0" = data.frame(Depth = rep(120, 20), ErrorRate = rep(0.001, 20)),
    "1" = data.frame(Depth = c(rep(5, 10), rep(200, 10)), ErrorRate = rep(0.05, 20))
  )
  sc <- score_assembly_paths(pdf, cov)
  expect_equal(nrow(sc), 2L)
  expect_equal(sc$path[sc$rank == 1L], 0)
  expect_true(sc$score[sc$path == 0] > sc$score[sc$path == 1])
})

test_that("score_assembly_paths flags the taxonomic minority in 3+ paths", {
  pdf <- rbind(clean_path(0), clean_path(1),
               clean_path(2, species = "Other", lineage = "Bacteria;Other"))
  sc <- score_assembly_paths(pdf)
  expect_lt(sc$s_blast_conc[sc$path == 2], 0.5)
  expect_match(sc$flags[sc$path == 2], "NUMT|contaminant")
  expect_false(grepl("NUMT", sc$flags[sc$path == 0]))
})

test_that("two differing paths are not over-flagged for concordance", {
  pdf <- rbind(clean_path(0),
               clean_path(1, species = "Other", lineage = "Bacteria;Other"))
  sc <- score_assembly_paths(pdf)
  expect_equal(unname(sc$s_blast_conc), c(0.5, 0.5))
  expect_false(any(grepl("NUMT", sc$flags)))
})

test_that("classify_conflict_block distinguishes the main causes", {
  expect_equal(
    classify_conflict_block(list(len = 1, n_snps = 1, n_indels = 0,
                                 min_depth = 80))$cause,
    "heteroplasmy"
  )
  expect_equal(
    classify_conflict_block(list(len = 3, n_snps = 0, n_indels = 3,
                                 min_depth = 80))$cause,
    "repeat_indel"
  )
  expect_equal(
    classify_conflict_block(list(len = 80, n_snps = 80, n_indels = 0,
                                 min_depth = 80), blast_divergent = TRUE)$cause,
    "numt"
  )
  expect_equal(
    classify_conflict_block(list(len = 5, n_snps = 5, n_indels = 0,
                                 min_depth = 80), at_junction = TRUE)$cause,
    "circularization"
  )
  expect_equal(
    classify_conflict_block(list(len = 5, n_snps = 0, n_indels = 0,
                                 min_depth = 3))$cause,
    "low_confidence"
  )
})

test_that("heteroplasmy recommends IUPAC only when depth is adequate", {
  hi <- classify_conflict_block(list(len = 1, n_snps = 1, n_indels = 0,
                                     min_depth = 80))
  lo <- classify_conflict_block(list(len = 1, n_snps = 1, n_indels = 0,
                                     min_depth = 4))
  expect_true("iupac" %in% hi$tools)
  expect_false("iupac" %in% lo$tools)
})

# Resolution builder fixtures: 3 paths, one conflict column (col 2).
make_aln <- function() {
  m <- matrix(c(
    "A", "C", "G", "T",
    "A", "G", "G", "T",
    "A", "C", "G", "T"
  ), nrow = 3, byrow = TRUE)
  rownames(m) <- c("P0", "P1", "P2")
  m
}

test_that("build_resolved_sequence splices a chosen path per block", {
  m <- make_aln()
  blk <- data.frame(aln_start = 2L, aln_end = 2L)
  # base = P0 (C at col2); choose P1 (G) for the block
  res <- build_resolved_sequence(m, blk, list(list(mode = "path", row = 2L)),
                                 base_row = 1L)
  expect_equal(res$seq, "AGGT")
  expect_equal(nrow(res$map), 4L)
  expect_equal(res$map$src_row[2], 2L)
})

test_that("build_resolved_sequence falls back to base path when no decisions", {
  m <- make_aln()
  blk <- data.frame(aln_start = 2L, aln_end = 2L)
  res <- build_resolved_sequence(m, blk, list(list(mode = "base")), base_row = 1L)
  expect_equal(res$seq, "ACGT")
})

test_that("build_resolved_sequence encodes IUPAC for a SNP block", {
  m <- make_aln()
  blk <- data.frame(aln_start = 2L, aln_end = 2L)
  res <- build_resolved_sequence(m, blk, list(list(mode = "iupac")), base_row = 1L)
  # col2 bases C,G,C -> {C,G} -> S; src is NA (synthesized)
  expect_equal(res$seq, "ASGT")
  expect_true(is.na(res$map$src_row[2]))
})

test_that("build_resolved_sequence N-masks a block", {
  m <- make_aln()
  blk <- data.frame(aln_start = 2L, aln_end = 2L)
  res <- build_resolved_sequence(m, blk, list(list(mode = "nmask")), base_row = 1L)
  expect_equal(res$seq, "ANGT")
})

test_that("majority fill picks the highest-support base", {
  m <- make_aln()
  blk <- data.frame(aln_start = 2L, aln_end = 2L)
  # support favors P1 (the G allele) at col 2
  support <- list(P0 = c(0, 10, 0, 0), P1 = c(0, 99, 0, 0), P2 = c(0, 10, 0, 0))
  res <- build_resolved_sequence(m, blk, list(list(mode = "majority")),
                                 base_row = 1L, support = support)
  expect_equal(substr(res$seq, 2, 2), "G")
})

test_that("gap columns are dropped from the resolved sequence", {
  m <- matrix(c(
    "A", "C", "G",
    "A", "-", "G",
    "A", "C", "G"
  ), nrow = 3, byrow = TRUE)
  rownames(m) <- c("P0", "P1", "P2")
  blk <- data.frame(aln_start = 2L, aln_end = 2L)
  # choose P1 (gap) for the block -> base drops to length 2
  res <- build_resolved_sequence(m, blk, list(list(mode = "path", row = 2L)),
                                 base_row = 1L)
  expect_equal(res$seq, "AG")
})
