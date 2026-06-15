test_that("scaffold_join_eligible: single path, multi scaffold", {
  df <- data.frame(path = c(1, 1, 1), scaffold = c(1, 2, 3))
  expect_true(scaffold_join_eligible(df))
})

test_that("scaffold_join_eligible: multi path is not eligible", {
  df <- data.frame(path = c(1, 1, 2, 2), scaffold = c(1, 2, 1, 2))
  expect_false(scaffold_join_eligible(df))
})

test_that("scaffold_join_eligible: single scaffold is not eligible", {
  df <- data.frame(path = c(1), scaffold = c(1))
  expect_false(scaffold_join_eligible(df))
})

test_that("choose_reference weights by length * pident", {
  df <- data.frame(
    blast_accession = c("A", "B", "B"),
    length = c(1000, 400, 400),
    blast_pident = c(99, 99, 99),
    stringsAsFactors = FALSE
  )
  # B total weight (~792) > A (~990)? A=990, B=792 -> A wins
  expect_equal(choose_reference(df), "A")
  df2 <- data.frame(
    blast_accession = c("A", "B", "B"),
    length = c(500, 400, 400),
    blast_pident = c(99, 99, 99),
    stringsAsFactors = FALSE
  )
  expect_equal(choose_reference(df2), "B")
})

test_that("choose_reference returns NA when no hits", {
  df <- data.frame(blast_accession = c(NA, ""), length = c(1, 1),
                   blast_pident = c(1, 1), stringsAsFactors = FALSE)
  expect_true(is.na(choose_reference(df)))
})

test_that("parse_paf extracts coords, strand, nmatch, query span", {
  paf <- c(
    "scaf1\t100\t0\t100\t+\tref\t16000\t0\t100\t95\t100\t60",
    "scaf2\t80\t0\t80\t-\tref\t16000\t500\t580\t70\t80\t60\tcs:Z:foo"
  )
  rows <- parse_paf(paf)
  expect_equal(nrow(rows), 2)
  expect_equal(rows$scaffold, c("scaf1", "scaf2"))
  expect_equal(rows$ref_start, c(0L, 500L))
  expect_equal(rows$ref_end, c(100L, 580L))
  expect_equal(rows$strand, c("+", "-"))
  expect_equal(rows$nmatch, c(95L, 70L))
  expect_equal(rows$qlen, c(100L, 80L))
  expect_equal(rows$qstart, c(0L, 0L))
  expect_equal(rows$qend, c(100L, 80L))
})

test_that("union_len merges overlapping intervals", {
  expect_equal(union_len(c(0, 50), c(100, 150)), 150)   # overlap 50-100
  expect_equal(union_len(c(0, 200), c(100, 300)), 200)  # disjoint
  expect_equal(union_len(0, 100), 100)
})

test_that("derive_scaffold_layout excludes poorly-mapped scaffolds", {
  mappings <- data.frame(
    scaffold = c("a", "b", "c"),
    ref_start = c(0L, 500L, 10L),
    ref_end = c(100L, 600L, 30L),
    strand = c("+", "+", "+"),
    nmatch = c(95L, 90L, 5L),
    qcov = c(0.95, 0.90, 0.05),   # c maps poorly
    mapped = c(TRUE, TRUE, TRUE),
    stringsAsFactors = FALSE
  )
  lay <- derive_scaffold_layout(mappings, 16000L, min_qcov = 0.5)
  expect_true(all(lay$include[lay$scaffold %in% c("a", "b")]))
  expect_false(lay$include[lay$scaffold == "c"])
  # excluded scaffold is not joined
  seqs <- c(a = "AAAA", b = "CCCC", c = "GGGG")
  res <- join_scaffolds(seqs, lay)
  expect_false(grepl("GGGG", res$seq))
})

test_that("derive_scaffold_layout fallback keeps best when none clear the bar", {
  mappings <- data.frame(
    scaffold = c("a", "b"),
    ref_start = c(0L, 500L), ref_end = c(100L, 600L),
    strand = c("+", "+"), nmatch = c(95L, 90L),
    qcov = c(0.2, 0.1), mapped = c(TRUE, TRUE), stringsAsFactors = FALSE
  )
  lay <- derive_scaffold_layout(mappings, 16000L, min_qcov = 0.5)
  expect_true(any(lay$include))   # fallback: not empty
})

test_that("parse_paf drops malformed lines", {
  expect_equal(nrow(parse_paf(c("too\tshort"))), 0)
})

test_that("derive_scaffold_layout orders by ref_start and flags rc", {
  mappings <- data.frame(
    scaffold = c("b", "a"),
    ref_start = c(500L, 0L),
    ref_end = c(600L, 100L),
    strand = c("-", "+"),
    nmatch = c(90L, 95L),
    mapped = c(TRUE, TRUE),
    stringsAsFactors = FALSE
  )
  lay <- derive_scaffold_layout(mappings, 16000L)
  expect_equal(lay$scaffold, c("a", "b"))      # ordered by ref_start
  expect_equal(lay$rc, c(FALSE, TRUE))
  expect_equal(lay$gap_before[2], 400)         # 500 - 100
})

test_that("derive_scaffold_layout appends unmapped scaffolds", {
  mappings <- data.frame(
    scaffold = c("a", "u"),
    ref_start = c(0L, NA_integer_),
    ref_end = c(100L, NA_integer_),
    strand = c("+", NA_character_),
    nmatch = c(95L, NA_integer_),
    mapped = c(TRUE, FALSE),
    stringsAsFactors = FALSE
  )
  lay <- derive_scaffold_layout(mappings, 16000L)
  expect_equal(lay$scaffold, c("a", "u"))
  expect_false(lay$mapped[2])
  expect_true(is.na(lay$gap_before[2]))
})

test_that("join_scaffolds inserts N gaps and applies RC", {
  seqs <- c(a = "AAAACCCC", b = "GGGGTTTT")
  lay <- data.frame(
    scaffold = c("a", "b"), order = 1:2, rc = c(FALSE, TRUE),
    gap_before = c(NA, 10), mapped = c(TRUE, TRUE),
    ref_start = c(0L, 30L), ref_end = c(8L, 38L), stringsAsFactors = FALSE
  )
  res <- join_scaffolds(seqs, lay, gap_len_default = 100L)
  # a (8) + 10 N + rc(b) (8) = 26
  expect_equal(nchar(res$seq), 26)
  expect_equal(substring(res$seq, 1, 8), "AAAACCCC")
  expect_equal(substring(res$seq, 9, 18), strrep("N", 10))
  expect_equal(substring(res$seq, 19, 26), "AAAACCCC")  # rc of GGGGTTTT
  expect_equal(sum(is.na(res$src_scaffold)), 10)
})

test_that("refine_overlap confirms a real (matching) overlap", {
  # a ends with the same 20 bp that b starts with
  ov_seq <- paste(sample(c("A","C","G","T"), 20, replace = TRUE), collapse = "")
  a <- paste0(strrep("A", 40), ov_seq)
  b <- paste0(ov_seq, strrep("C", 40))
  r <- refine_overlap(a, b, est_overlap = 20)
  expect_true(r$reliable)
  expect_equal(r$trim_b, 20L)
  expect_gt(r$identity, 0.95)
})

test_that("refine_overlap rejects a divergent (non-matching) overlap", {
  a <- paste0(strrep("A", 40), strrep("A", 20))   # a end = poly-A
  b <- paste0(strrep("T", 20), strrep("C", 40))   # b start = poly-T
  r <- refine_overlap(a, b, est_overlap = 20)
  expect_false(r$reliable)
})

test_that("join_scaffolds merges a confirmed overlap without duplication", {
  ov_seq <- paste(sample(c("A","C","G","T"), 30, replace = TRUE), collapse = "")
  a <- paste0(strrep("A", 50), ov_seq)
  b <- paste0(ov_seq, strrep("C", 50))
  seqs <- c(a = a, b = b)
  lay <- data.frame(
    scaffold = c("a", "b"), order = 1:2, rc = c(FALSE, FALSE),
    gap_before = c(NA, -30), mapped = c(TRUE, TRUE),
    ref_start = c(0L, 50L), ref_end = c(80L, 130L), stringsAsFactors = FALSE
  )
  res <- join_scaffolds(seqs, lay)
  # a(80) + b trimmed by the 30bp overlap (50) = 130, overlap not duplicated
  expect_equal(nchar(res$seq), 130)
  expect_equal(res$seq, paste0(strrep("A", 50), ov_seq, strrep("C", 50)))
  expect_match(res$junctions[1], "confirmed")
})

test_that("join_scaffolds flags an unconfirmed (divergent) overlap", {
  a <- paste0(strrep("A", 50), strrep("A", 20))
  b <- paste0(strrep("T", 20), strrep("C", 50))
  seqs <- c(a = a, b = b)
  lay <- data.frame(
    scaffold = c("a", "b"), order = 1:2, rc = c(FALSE, FALSE),
    gap_before = c(NA, -20), mapped = c(TRUE, TRUE),
    ref_start = c(0L, 50L), ref_end = c(70L, 120L), stringsAsFactors = FALSE
  )
  res <- join_scaffolds(seqs, lay)
  expect_match(res$junctions[1], "NOT confirmed")
  # still trimmed by the estimate (no duplication)
  expect_equal(nchar(res$seq), 70 + (70 - 20))
})

test_that("stitch_coverage matches joined length and reverses RC", {
  seqs <- c(a = "AACC", b = "GGTT")
  lay <- data.frame(
    scaffold = c("a", "b"), order = 1:2, rc = c(FALSE, TRUE),
    gap_before = c(NA, 2), mapped = c(TRUE, TRUE),
    ref_start = c(0L, 6L), ref_end = c(4L, 10L), stringsAsFactors = FALSE
  )
  joined <- join_scaffolds(seqs, lay)
  cov <- list(
    a = list(depth = c(1, 2, 3, 4), gc = c(0, 0, 1, 1), err = c(0, 0, 0, 0)),
    b = list(depth = c(10, 20, 30, 40), gc = c(1, 1, 0, 0), err = c(0, 0, 0, 0))
  )
  st <- stitch_coverage(cov, lay, joined)
  expect_equal(length(st$depth), nchar(joined$seq))
  # a depth first 4
  expect_equal(st$depth[1:4], c(1, 2, 3, 4))
  # 2 gap bases -> 0
  expect_equal(st$depth[5:6], c(0, 0))
  # b is RC so depth reversed: 40,30,20,10
  expect_equal(st$depth[7:10], c(40, 30, 20, 10))
})

test_that("overlap_consensus picks higher-depth base, IUPAC at ties, N when low", {
  a <- c("A", "A", "A", "A")
  b <- c("A", "G", "C", "T")
  # pos2: B much deeper -> B; pos3: tie -> IUPAC; pos4: both low -> N
  res <- overlap_consensus(a, b, a_dep = c(30, 5, 20, 2), b_dep = c(30, 40, 22, 1))
  expect_equal(res$base[1], "A")          # agree
  expect_equal(res$base[2], "G")          # B deeper
  expect_true(res$use_b[2])
  expect_equal(res$base[3], iupac_code(c("A", "C")))  # tie -> IUPAC (M)
  expect_equal(res$base[4], "N")          # both low depth
  expect_equal(res$n_mismatch, 3)
})

test_that("join_scaffolds applies coverage consensus across a confirmed overlap", {
  ov <- paste(sample(c("A","C","G","T"), 40, replace = TRUE), collapse = "")
  ov_chars <- strsplit(ov, "")[[1]]
  # B's overlap copy differs at position 10 (a SNP)
  ovb <- ov_chars; ovb[10] <- if (ovb[10] == "A") "G" else "A"
  a <- paste0(strrep("A", 30), ov)
  b <- paste0(paste(ovb, collapse = ""), strrep("C", 30))
  seqs <- c(a = a, b = b)
  lay <- data.frame(scaffold = c("a","b"), order = 1:2, rc = c(FALSE, FALSE),
                    gap_before = c(NA, -40), mapped = c(TRUE, TRUE),
                    stringsAsFactors = FALSE)
  # B much deeper over the overlap -> consensus should take B's SNP base at pos 10
  depth <- list(a = c(rep(10, 30), rep(10, 40)),
                b = c(rep(50, 40), rep(50, 30)))
  res <- join_scaffolds(seqs, lay, scaffold_depth = depth)
  # overlap region is a's tail (positions 31..70 of the joined seq)
  joined_chars <- strsplit(res$seq, "")[[1]]
  expect_equal(joined_chars[30 + 10], ovb[10])   # took B's deeper SNP base
  expect_match(res$junctions[1], "resolved by coverage")
})

test_that("circularize_sequence detects + trims a redundant end overlap", {
  core <- paste(sample(c("A","C","G","T"), 400, replace = TRUE), collapse = "")
  wrap <- substring(core, 1, 60)              # redundant copy of the 5' start
  seq <- paste0(core, wrap)                    # circular molecule, linearized
  n <- nchar(seq)
  cz <- circularize_sequence(seq, depth = rep(10, n), gc = rep(0.4, n),
                             errors = rep(0, n))
  expect_true(cz$circular)
  expect_equal(nchar(cz$seq), 400)            # redundant 60 bp trimmed
  expect_equal(length(cz$depth), 400)
  expect_gte(cz$overlap_len, 50)
})

test_that("circularize_sequence leaves a linear sequence unchanged", {
  seq <- paste(sample(c("A","C","G","T"), 400, replace = TRUE), collapse = "")
  n <- nchar(seq)
  cz <- circularize_sequence(seq, rep(10, n), rep(0.4, n), rep(0, n))
  expect_false(cz$circular)
  expect_equal(cz$seq, seq)
})

test_that("parse_cov_string handles empty + space-separated", {
  expect_equal(parse_cov_string("1 2 3"), c(1, 2, 3))
  expect_equal(length(parse_cov_string(NA)), 0)
  expect_equal(length(parse_cov_string("")), 0)
})

test_that("parse_scaffold_hits parses workflow string + maps NO HIT to NA", {
  s <- "1|PZ291823.1|91.52;2|PZ285099.1|99.75;3|NO HIT|"
  sl <- c("1" = 723, "2" = 17444, "3" = 1200)
  df <- parse_scaffold_hits(s, sl)
  expect_equal(nrow(df), 3)
  expect_equal(df$blast_accession, c("PZ291823.1", "PZ285099.1", NA))
  expect_equal(df$length, c(723, 17444, 1200))
  expect_equal(df$blast_pident[2], 99.75)
  expect_true(scaffold_hits_disagree(df))      # two distinct real accessions
  expect_null(parse_scaffold_hits("", sl))
  expect_null(parse_scaffold_hits(NA, sl))
})

test_that("scaffold_hits_disagree flags conflicting BLAST accessions", {
  same <- data.frame(blast_accession = c("NC_1", "NC_1", NA), stringsAsFactors = FALSE)
  diff <- data.frame(blast_accession = c("NC_1", "NC_2"), stringsAsFactors = FALSE)
  none <- data.frame(blast_accession = c(NA, ""), stringsAsFactors = FALSE)
  expect_false(scaffold_hits_disagree(same))
  expect_true(scaffold_hits_disagree(diff))
  expect_false(scaffold_hits_disagree(none))
})

test_that("load_scaffold_mappings round-trips into derive_scaffold_layout shape", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "CREATE TABLE scaffold_mappings (
    ID TEXT, ref_accession TEXT, scaffold INTEGER, ref_start INTEGER, ref_end INTEGER,
    strand TEXT, nmatch INTEGER, qcov REAL, qstart INTEGER, mapped INTEGER)")
  DBI::dbExecute(con, "INSERT INTO scaffold_mappings VALUES
    ('s1','NC_1',1,1,500,'+',480,0.96,0,1),
    ('s1','NC_1',2,600,900,'-',280,0.9,0,1),
    ('s1','NC_2',1,1,500,'+',100,0.2,0,1)")

  m <- load_scaffold_mappings(con, "s1", "NC_1")
  expect_equal(nrow(m), 2)
  expect_true(all(c("scaffold", "ref_start", "ref_end", "strand", "nmatch",
                    "qcov", "qstart", "mapped") %in% names(m)))
  expect_type(m$mapped, "logical")

  lay <- derive_scaffold_layout(m, ref_len = 1000, circular = FALSE)
  expect_equal(lay$scaffold[1], "1")          # ordered by ref_start
  expect_true(lay$rc[lay$scaffold == "2"])    # '-' strand -> rc

  expect_null(load_scaffold_mappings(con, "s1", "NC_missing"))
})
