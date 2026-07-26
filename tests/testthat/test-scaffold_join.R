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

test_that("collapse_scaffold_blocks unions colinear blocks per scaffold", {
  # scaffold 'a' maps in two + blocks (e.g. across a duplication); placement must
  # span both, not just the higher-nmatch block. 'b' is single-block.
  rows <- data.frame(
    scaffold  = c("a", "a", "b"),
    qlen      = c(1000L, 1000L, 500L),
    qstart    = c(0L, 600L, 0L),
    qend      = c(400L, 1000L, 500L),
    strand    = c("+", "+", "-"),
    ref_start = c(100L, 800L, 50L),
    ref_end   = c(500L, 1200L, 550L),
    nmatch    = c(380L, 360L, 480L),
    cigar     = NA_character_,
    stringsAsFactors = FALSE
  )
  out <- collapse_scaffold_blocks(rows)
  a <- out[out$scaffold == "a", ]
  expect_equal(a$ref_start, 100L)        # union min
  expect_equal(a$ref_end, 1200L)         # union max (not 500 from best block)
  expect_equal(a$nmatch, 740L)           # summed over blocks
  expect_equal(a$qstart, 0L)             # anchored at min ref_start block
  expect_equal(out[out$scaffold == "b", ]$strand, "-")
})

test_that("collapse_scaffold_blocks picks dominant strand by matched residues", {
  rows <- data.frame(
    scaffold = c("a", "a"), qlen = c(1000L, 1000L),
    qstart = c(0L, 500L), qend = c(450L, 600L),
    strand = c("+", "-"), ref_start = c(0L, 700L), ref_end = c(450L, 800L),
    nmatch = c(440L, 90L), cigar = NA_character_, stringsAsFactors = FALSE)
  out <- collapse_scaffold_blocks(rows)
  expect_equal(out$strand, "+")          # + carries more matches
  expect_equal(out$ref_end, 450L)        # minor - block excluded from extent
})

test_that("join_scaffolds discovers an overlap the reference did not predict", {
  # Reference shows a butt join (gb == 0) but the scaffolds truly share 40 bp;
  # without an end probe this would duplicate the shared region.
  ov_seq <- paste(sample(c("A","C","G","T"), 40, replace = TRUE), collapse = "")
  a <- paste0(strrep("A", 60), ov_seq)
  b <- paste0(ov_seq, strrep("C", 60))
  seqs <- c(a = a, b = b)
  lay <- data.frame(scaffold = c("a", "b"), order = 1:2, rc = c(FALSE, FALSE),
                    gap_before = c(NA, 0), mapped = TRUE, include = TRUE,
                    stringsAsFactors = FALSE)
  res <- join_scaffolds(seqs, lay)
  expect_equal(nchar(res$seq), 160)                 # 60 + 40 + 60, not 200
  expect_equal(res$junction_info$type[1], "overlap_confirmed")
  expect_match(res$junctions[1], "confirmed")
})

test_that("join_scaffolds discovers an overlap under a positively-biased gap", {
  # minimap inset bias: reference says a 30 bp gap, but the ends really overlap.
  ov_seq <- paste(sample(c("A","C","G","T"), 50, replace = TRUE), collapse = "")
  a <- paste0(strrep("A", 60), ov_seq)
  b <- paste0(ov_seq, strrep("C", 60))
  seqs <- c(a = a, b = b)
  lay <- data.frame(scaffold = c("a", "b"), order = 1:2, rc = c(FALSE, FALSE),
                    gap_before = c(NA, 30), mapped = TRUE, include = TRUE,
                    stringsAsFactors = FALSE)
  res <- join_scaffolds(seqs, lay)
  expect_equal(res$junction_info$type[1], "overlap_confirmed")
  expect_false(grepl("N", res$seq))                 # no spurious N spacer
})

test_that("join_scaffolds keeps a true large gap (no spurious merge)", {
  # Large predicted gap beyond the probe window: trust the reference, insert Ns,
  # even if the ends share a short motif by chance.
  a <- paste0(strrep("A", 80), "ACGTACGTAC")
  b <- paste0("ACGTACGTAC", strrep("T", 80))
  seqs <- c(a = a, b = b)
  lay <- data.frame(scaffold = c("a", "b"), order = 1:2, rc = c(FALSE, FALSE),
                    gap_before = c(NA, 2000), mapped = TRUE, include = TRUE,
                    stringsAsFactors = FALSE)
  res <- join_scaffolds(seqs, lay)
  expect_equal(res$junction_info$type[1], "gap")
  expect_equal(res$junction_info$gap_bases[1], 2000L)
})

test_that("join_scaffolds emits one junction_info row per junction", {
  seqs <- c(a = "AAAACCCC", b = "GGGGTTTT", c = "ACGTACGT")
  lay <- data.frame(
    scaffold = c("a", "b", "c"), order = 1:3, rc = c(FALSE, FALSE, FALSE),
    gap_before = c(NA, 10, 0), mapped = TRUE, include = TRUE,
    stringsAsFactors = FALSE)
  res <- join_scaffolds(seqs, lay)
  expect_equal(nrow(res$junction_info), 2)            # n_scaffolds - 1
  expect_equal(res$junction_info$type, c("gap", "butt"))
  expect_equal(res$junction_info$gap_bases, c(10L, 0L))
})

test_that("summarize_join_quality flags a gappy join + counts spacer N only", {
  # scaffold b carries an internal N (assembly gap) that must NOT inflate gap_bases
  seqs <- c(a = "AAAACCCC", b = "GGNGTTTT")
  lay <- data.frame(
    scaffold = c("a", "b"), order = 1:2, rc = c(FALSE, FALSE),
    gap_before = c(NA, 12), mapped = TRUE, include = TRUE,
    stringsAsFactors = FALSE)
  res <- join_scaffolds(seqs, lay)
  q <- summarize_join_quality(res, lay)
  expect_equal(q$continuity, "gappy")
  expect_equal(q$gap_bases, 12L)                       # spacer only, not the internal N
  expect_equal(q$n_gap_junctions, 1L)
  expect_true(is.na(q$min_overlap_identity))
})

test_that("summarize_join_quality flags a continuous join with overlap identity", {
  ov_seq <- paste(sample(c("A","C","G","T"), 30, replace = TRUE), collapse = "")
  a <- paste0(strrep("A", 50), ov_seq)
  b <- paste0(ov_seq, strrep("C", 50))
  seqs <- c(a = a, b = b)
  lay <- data.frame(
    scaffold = c("a", "b"), order = 1:2, rc = c(FALSE, FALSE),
    gap_before = c(NA, -30), mapped = TRUE, include = TRUE,
    stringsAsFactors = FALSE)
  res <- join_scaffolds(seqs, lay)
  q <- summarize_join_quality(res, lay)
  expect_equal(q$continuity, "continuous")
  expect_equal(q$gap_bases, 0L)
  expect_equal(q$n_overlap_junctions, 1L)
  expect_gt(q$min_overlap_identity, 0.95)
})

test_that("summarize_join_quality reports a single scaffold", {
  seqs <- c(a = "AAAACCCC")
  lay <- data.frame(scaffold = "a", order = 1L, rc = FALSE, gap_before = NA_real_,
                    mapped = TRUE, include = TRUE, stringsAsFactors = FALSE)
  res <- join_scaffolds(seqs, lay)
  q <- summarize_join_quality(res, lay)
  expect_equal(q$continuity, "single")
  expect_equal(q$n_junctions, 0L)
})

test_that("compose_join_note uses spacer gap_bases, not raw N count", {
  seqs <- c(a = "AANACCCC", b = "GGGGTTNT")   # one internal N each
  lay <- data.frame(scaffold = c("a", "b"), order = 1:2, rc = FALSE,
                    gap_before = c(NA, 5), mapped = TRUE, include = TRUE,
                    stringsAsFactors = FALSE)
  res <- join_scaffolds(seqs, lay)
  note <- compose_join_note(lay, res$seq, 100L, res$junctions,
                            gap_bases = sum(is.na(res$src_scaffold)))
  expect_match(note, "5 N gap bases")          # spacer only; internal Ns excluded
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

test_that("detect_subsumed flags a contained scaffold, keeps the container", {
  inc <- data.frame(
    scaffold = c("A", "B"),
    ref_start = c(0L, 500L), ref_end = c(2000L, 800L),
    nmatch = c(1800L, 290L), stringsAsFactors = FALSE
  )
  r <- detect_subsumed(inc)
  expect_true(is.na(r[1]))
  expect_match(r[2], "subsumed by scaffold A")
})

test_that("detect_subsumed keeps disjoint and partially-overlapping scaffolds", {
  inc <- data.frame(
    scaffold = c("A", "B", "C"),
    ref_start = c(0L, 1800L, 4000L), ref_end = c(2000L, 3000L, 6000L),
    nmatch = c(1900L, 1100L, 1900L), stringsAsFactors = FALSE
  )
  expect_true(all(is.na(detect_subsumed(inc))))
})

test_that("detect_subsumed density guard: a sparse ballooned extent cannot subsume", {
  # A spans the whole reference but matches almost nothing (origin-spanning /
  # repeat artifact); it must not swallow the well-matched B.
  inc <- data.frame(
    scaffold = c("A", "B"),
    ref_start = c(0L, 500L), ref_end = c(10000L, 800L),
    nmatch = c(200L, 290L), stringsAsFactors = FALSE
  )
  expect_true(all(is.na(detect_subsumed(inc))))
})

test_that("derive_scaffold_layout excludes subsumed scaffold and recomputes gap across it", {
  mappings <- data.frame(
    scaffold = c("A", "B", "C"),
    ref_start = c(0L, 500L, 2100L), ref_end = c(2000L, 800L, 4000L),
    strand = c("+", "+", "+"), nmatch = c(1800L, 290L, 1850L),
    qcov = c(0.95, 0.95, 0.95), qstart = c(0L, 0L, 0L),
    mapped = c(TRUE, TRUE, TRUE), stringsAsFactors = FALSE
  )
  lay <- derive_scaffold_layout(mappings, min_qcov = 0.5)
  expect_false(lay$include[lay$scaffold == "B"])
  expect_match(lay$exclude_reason[lay$scaffold == "B"], "subsumed by scaffold A")
  # gap A -> C computed across the removed B (2100 - 2000), not a huge negative
  expect_equal(lay$gap_before[lay$scaffold == "C"], 100)
})

test_that("join_scaffolds drops a fully-overlapped scaffold without src_pos corruption", {
  a <- paste(rep("ACGT", 50), collapse = "")   # 200 bp
  b <- substring(a, 101)                        # last 100 bp of a (contained)
  seqs <- list(A = a, B = b)
  lay <- data.frame(
    scaffold = c("A", "B"), order = 1:2, rc = c(FALSE, FALSE),
    gap_before = c(NA, -100), include = c(TRUE, TRUE), stringsAsFactors = FALSE
  )
  res <- join_scaffolds(seqs, lay)
  expect_equal(nchar(res$seq), 200L)                    # B added no new bases
  expect_equal(length(res$src_pos), nchar(res$seq))     # src_pos stays in lockstep
  expect_false(any(is.na(res$src_pos)))
  expect_false(any(res$src_pos < 1))
  expect_true(any(grepl("dropped as redundant", res$junctions)))
})

test_that("join_scaffolds keeps src_pos aligned after a partial overlap trim", {
  # b overlaps a's tail by 40 bp; the trim path must not desync src_pos.
  a <- paste(rep("ACGTACGTAC", 10), collapse = "")   # 100 bp
  overlap <- substring(a, 61)                         # a's last 40 bp
  b <- paste0(overlap, paste(rep("TTTTGGGGCC", 6), collapse = ""))  # 40 + 60
  seqs <- list(A = a, B = b)
  lay <- data.frame(
    scaffold = c("A", "B"), order = 1:2, rc = c(FALSE, FALSE),
    gap_before = c(NA, -40), include = c(TRUE, TRUE), stringsAsFactors = FALSE
  )
  res <- join_scaffolds(seqs, lay)
  expect_equal(length(res$src_pos), nchar(res$seq))
  expect_false(any(is.na(res$src_pos)))
  expect_true(all(res$src_pos >= 1))
  # Assert the MAPPING, not its shape. A src_pos uniformly offset by the trim
  # amount satisfies every check above, and that offset is exactly the desync
  # class the trim + seq_len(n) fix exists to prevent.
  expect_equal(nchar(res$seq), 160L)                       # 100 + (100 - 40)
  expect_equal(res$src_pos[res$src_scaffold == "A"], 1:100)
  expect_equal(res$src_pos[res$src_scaffold == "B"], 41:100)

  # Round-trip through stitch_coverage with per-scaffold distinguishable depth:
  # B's contribution must land on B's positions 41..100, not 1..60.
  cov <- list(
    A = list(depth = 1:100,        gc = rep(0, 100), err = rep(0, 100)),
    B = list(depth = 101:200,      gc = rep(0, 100), err = rep(0, 100))
  )
  st <- stitch_coverage(cov, lay, res)
  expect_equal(length(st$depth), nchar(res$seq))
  expect_equal(utils::tail(st$depth, 60), 141:200)
})


# =============================================================================
# Adversarial-audit batch fixes (F3, F11, F12, F13, F15, F16, F17, F18, F24)
# =============================================================================

# --- F12 ---
test_that("collapse_scaffold_blocks does not balloon an origin-wrap extent", {
  rows <- data.frame(
    scaffold = c("s", "s"),
    qlen = c(8000L, 8000L),
    qstart = c(0L, 4000L),
    qend = c(4000L, 8000L),
    strand = c("+", "+"),
    ref_start = c(12000L, 0L),
    ref_end = c(16000L, 4000L),
    nmatch = c(3900L, 3800L),
    cigar = NA_character_,
    stringsAsFactors = FALSE
  )
  out <- collapse_scaffold_blocks(rows)
  # Anchor block [12000,16000] kept; off-diagonal wrap block [0,4000] dropped.
  expect_equal(out$ref_start, 12000)
  expect_equal(out$ref_end, 16000)
  expect_true(out$ref_end - out$ref_start <= 4000)
})

# --- F13 ---
test_that("collapse_scaffold_blocks qcov uses dominant-strand blocks only", {
  rows <- data.frame(
    scaffold = c("s", "s"),
    qlen = c(9000L, 9000L),
    qstart = c(0L, 5000L),
    qend = c(5000L, 9000L),
    strand = c("+", "-"),
    ref_start = c(0L, 8000L),
    ref_end = c(5000L, 12000L),
    nmatch = c(4000L, 3000L),
    cigar = NA_character_,
    stringsAsFactors = FALSE
  )
  out <- collapse_scaffold_blocks(rows)
  expect_equal(out$strand, "+")
  # dominant-only coverage 5000/9000, not the both-strand union 1.0
  expect_lt(out$qcov, 0.99)
  expect_equal(out$qcov, 5000 / 9000, tolerance = 1e-6)
})

# --- F11 ---
test_that("refine_overlap confirms a large overlap beyond the est window", {
  skip_if_not_installed("pwalign")
  set.seed(1)
  bp <- function(n) paste0(sample(c("A", "C", "G", "T"), n, replace = TRUE), collapse = "")
  x <- bp(800)
  a <- paste0(bp(1200), x)
  b <- paste0(x, bp(1500))
  ov <- refine_overlap(a, b, est_overlap = 300, min_identity = 0.9, min_overlap = 20L)
  expect_true(ov$reliable)
  expect_equal(ov$trim_b, 800)
})

# --- F11 ---
test_that("refine_overlap rejects a low-complexity (homopolymer) seam", {
  skip_if_not_installed("pwalign")
  set.seed(2)
  bp <- function(n) paste0(sample(c("A", "C", "G", "T"), n, replace = TRUE), collapse = "")
  a <- paste0(bp(1000), strrep("A", 30))
  b <- paste0(strrep("A", 40), bp(1000))
  ov <- refine_overlap(a, b, est_overlap = 300, min_identity = 0.9, min_overlap = 20L)
  expect_false(ov$reliable)
})

# --- F16 ---
test_that("parse_cov_string strips '#' mask prefix and preserves position count", {
  # F16: masked positions keep their value instead of becoming NA
  expect_equal(parse_cov_string("250 #300 275 #280 260"), c(250, 300, 275, 280, 260))
  # F15: empty cells map to NA and the count is preserved when n is supplied
  raw <- "  0.4 0.5 0.6  "   # 7 positions: 2 leading NA, 3 values, 2 trailing NA
  expect_equal(parse_cov_string(raw, n = 7), c(NA, NA, 0.4, 0.5, 0.6, NA, NA))
  # strsplit drops the single final trailing empty when n is absent (documented)
  expect_length(parse_cov_string(raw), 6)
  # existing behavior preserved
  expect_equal(parse_cov_string("1 2 3"), c(1, 2, 3))
  expect_length(parse_cov_string(NA), 0)
  expect_length(parse_cov_string(""), 0)
})

# --- F15 ---
test_that("assemble_from_layout pads short GC/error tracks to sequence length", {
  seq <- "ACGTACGTAC"                       # 10 bp; too short to self-circularize
  df <- data.frame(
    scaffold = "1", sequence = seq,
    depth = paste(rep(5, 10), collapse = " "),
    # GC with 2 leading + 2 trailing empty cells (rollapply NA ends), 10 positions
    gc = "  0.4 0.5 0.6 0.5 0.4 0.5  ",
    errors = paste(rep(0, 10), collapse = " "),
    stringsAsFactors = FALSE)
  lay <- data.frame(scaffold = "1", order = 1L, rc = FALSE,
                    gap_before = NA_real_, mapped = TRUE, include = TRUE,
                    stringsAsFactors = FALSE)
  res <- assemble_from_layout(df, lay, gap_len = 100L, circular = FALSE, ref_seq = NULL)
  expect_equal(length(res$gc), nchar(res$seq))
  expect_equal(length(res$errors), nchar(res$seq))
  expect_equal(length(res$depth), nchar(res$seq))
})

# --- F24 ---
test_that("load_scaffold_mappings coerces numeric columns despite empty-string unmapped rows", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "CREATE TABLE scaffold_mappings (
    ID TEXT, ref_accession TEXT, scaffold INTEGER, ref_start INTEGER, ref_end INTEGER,
    strand TEXT, nmatch INTEGER, qcov REAL, qstart INTEGER, mapped INTEGER)")
  # Unmapped scaffold stored FIRST with empty strings: SQLite then returns the
  # ref_start/ref_end columns as character without coercion.
  DBI::dbExecute(con, "INSERT INTO scaffold_mappings VALUES
    ('s1','NC_1',3,'','','','','','',0),
    ('s1','NC_1',1,1,500,'+',480,0.96,0,1),
    ('s1','NC_1',2,600,900,'-',280,0.9,0,1)")
  m <- load_scaffold_mappings(con, "s1", "NC_1")
  expect_type(m$ref_start, "integer")
  expect_type(m$ref_end, "integer")
  expect_type(m$qcov, "double")
  # derive_scaffold_layout must not throw on the (previously character) columns
  expect_silent(derive_scaffold_layout(m, ref_len = 1000, circular = FALSE))
})

# --- F3 ---
test_that("derive_scaffold_layout does not derive a negative gap from an origin-wrapping predecessor", {
  ref_len <- 16000L
  mappings <- data.frame(
    scaffold = c("wrap", "b"),
    ref_start = c(0L, 4000L),
    ref_end = c(16000L, 9000L),
    strand = c("+", "+"),
    nmatch = c(3000L, 4500L),   # wrap density 3000/16000 < 0.5 -> not a container
    qcov = c(1, 1),
    qstart = c(0L, 0L),
    mapped = c(TRUE, TRUE),
    stringsAsFactors = FALSE
  )
  lay <- derive_scaffold_layout(mappings, ref_len = ref_len, circular = TRUE,
                                min_qcov = 0.5)
  inc <- lay[lay$include, , drop = FALSE]
  inc <- inc[order(inc$order), , drop = FALSE]
  expect_equal(nrow(inc), 2L)                 # neither dropped/subsumed
  gb_b <- inc$gap_before[inc$scaffold == "b"]
  # Ballooned predecessor: no genome-scale negative gap, but 0 rather than NA so
  # join_scaffolds still probes the junction instead of blindly N-padding it.
  expect_equal(gb_b, 0)
})

# --- F3 ---
test_that("derive_scaffold_layout keeps a real gap after a dense full-length predecessor", {
  ref_len <- 16000L
  mappings <- data.frame(
    scaffold = c("a", "b"),
    ref_start = c(0L, 15000L),
    ref_end = c(14500L, 15800L),   # a spans >= 0.9*ref_len but is disjoint from b
    strand = c("+", "+"),
    nmatch = c(10000L, 700L),      # a density 10000/14500 = 0.69 >= 0.5 -> not a wrap
    qcov = c(1, 1),
    qstart = c(0L, 0L),
    mapped = c(TRUE, TRUE),
    stringsAsFactors = FALSE
  )
  lay <- derive_scaffold_layout(mappings, ref_len = ref_len, circular = FALSE,
                                min_qcov = 0.5)
  inc <- lay[lay$include, , drop = FALSE]
  inc <- inc[order(inc$order), , drop = FALSE]
  expect_equal(nrow(inc), 2L)                 # dense predecessor does not subsume b
  gb_b <- inc$gap_before[inc$scaffold == "b"]
  expect_equal(gb_b, 500)                     # 15000 - 14500, density guard not tripped
})

# --- F11 ---
test_that("refine_overlap rejects a repeat buried inside A's tail (not at its 3' end)", {
  skip_if_not_installed("pwalign")
  set.seed(11)
  bp <- function(n) paste0(sample(c("A", "C", "G", "T"), n, replace = TRUE), collapse = "")
  p <- bp(100)                       # b's prefix; a carries an internal copy of it
  a <- paste0(bp(500), p, bp(200))   # p is buried, then 200 bp of distinct 3' end
  b <- paste0(p, bp(800))
  ov <- refine_overlap(a, b, est_overlap = 50, min_identity = 0.9, min_overlap = 20L)
  expect_false(ov$reliable)          # internal repeat must not confirm a junction
  expect_equal(ov$trim_b, 0L)
})

# --- F17 ---
test_that("assemble_from_layout circularizes on the app path only when circular is asserted", {
  skip_if_not_installed("pwalign")
  set.seed(170)
  bp <- function(n) paste0(sample(c("A", "C", "G", "T"), n, replace = TRUE), collapse = "")
  ov <- bp(60); core <- bp(300)
  seq <- paste0(ov, core, ov)        # 420 bp with a 60 bp redundant end overlap
  n <- nchar(seq)
  df <- data.frame(scaffold = "1", sequence = seq,
    depth = paste(rep(5, n), collapse = " "),
    gc = paste(rep(0.4, n), collapse = " "),
    errors = paste(rep(0, n), collapse = " "),
    stringsAsFactors = FALSE)
  lay <- data.frame(scaffold = "1", order = 1L, rc = FALSE, gap_before = NA_real_,
                    mapped = TRUE, include = TRUE, stringsAsFactors = FALSE)
  # App path: no reference, circular unchecked -> left linear, no end trim.
  lin <- assemble_from_layout(df, lay, gap_len = 100L, circular = FALSE, ref_seq = NULL)
  expect_equal(lin$topology, "linear")
  expect_equal(nchar(lin$seq), n)
  # ...but the duplication must never be SILENT: the detection still runs and the
  # note has to say the origin region is present twice.
  expect_match(lin$note, "redundant end overlap detected .* NOT")
  # Same input, Circular checked -> redundant overlap trimmed, marked circular.
  cir <- assemble_from_layout(df, lay, gap_len = 100L, circular = TRUE, ref_seq = NULL)
  expect_equal(cir$topology, "circular")
  expect_equal(nchar(cir$seq), n - 60L)
  expect_match(cir$note, "circular: trimmed 60 bp")
})

# --- F17 ---
test_that("circularize_sequence with raised min_overlap ignores a short spurious end match", {
  skip_if_not_installed("pwalign")
  set.seed(17)
  bp <- function(n) paste0(sample(c("A", "C", "G", "T"), n, replace = TRUE), collapse = "")
  # A RANDOM 35 bp terminal repeat, not an AT motif: low_complexity_overlap would
  # reject an AT motif on its own, which made this test pass identically at
  # min_overlap 20 and 50 and so proved nothing about min_overlap.
  motif <- bp(35L)
  core <- bp(400L)
  seq <- paste0(motif, core, motif)
  n <- nchar(seq); cov <- rep(1, n)
  expect_false(low_complexity_overlap(motif))     # the guard under test is min_overlap
  # 35 < 50 -> not circular at the raised threshold...
  cz <- circularize_sequence(seq, cov, cov, cov, min_overlap = 50L)
  expect_false(cz$circular)
  expect_equal(nchar(cz$seq), n)                  # no real 3' bases trimmed
  # ...but the same 35 bp repeat IS found when the threshold is below it, which is
  # what makes this a test of min_overlap rather than of the complexity guard.
  cz20 <- circularize_sequence(seq, cov, cov, cov, min_overlap = 20L)
  expect_true(cz20$circular)
  expect_equal(nchar(cz20$seq), n - 35L)
})

# --- F17 ---
test_that("assemble_from_layout passes min_overlap through at its call site", {
  skip_if_not_installed("pwalign")
  set.seed(171)
  bp <- function(n) paste0(sample(c("A", "C", "G", "T"), n, replace = TRUE), collapse = "")
  # 35 bp terminal repeat: below assemble_from_layout's hardcoded min_overlap = 50,
  # so the call site must NOT circularize it. Testing circularize_sequence directly
  # cannot catch a wrong (or missing) value at the call site.
  motif <- bp(35L); seq <- paste0(motif, bp(400L), motif)
  n <- nchar(seq)
  df <- data.frame(scaffold = "1", sequence = seq,
    depth = paste(rep(5, n), collapse = " "),
    gc = paste(rep(0.4, n), collapse = " "),
    errors = paste(rep(0, n), collapse = " "), stringsAsFactors = FALSE)
  lay <- data.frame(scaffold = "1", order = 1L, rc = FALSE, gap_before = NA_real_,
                    mapped = TRUE, include = TRUE, stringsAsFactors = FALSE)
  res <- assemble_from_layout(df, lay, gap_len = 100L, circular = TRUE, ref_seq = NULL)
  expect_equal(nchar(res$seq), n)
  expect_false(grepl("trimmed 35 bp", res$note))
})

# --- F17/F18 ---
test_that("assemble_from_layout circularizes and rotates on the reference path", {
  skip_if_not_installed("pwalign")
  set.seed(172)
  bp <- function(n) paste0(sample(c("A", "C", "G", "T"), n, replace = TRUE), collapse = "")
  ov <- bp(60L); core <- bp(400L)
  seq <- paste0(ov, core, ov)                     # 520 bp, 60 bp redundant end overlap
  n <- nchar(seq)
  df <- data.frame(scaffold = "1", sequence = seq,
    depth = paste(rep(5, n), collapse = " "),
    gc = paste(rep(0.4, n), collapse = " "),
    errors = paste(rep(0, n), collapse = " "), stringsAsFactors = FALSE)
  lay <- data.frame(scaffold = "1", order = 1L, rc = FALSE, gap_before = NA_real_,
                    mapped = TRUE, include = TRUE, stringsAsFactors = FALSE)
  # has_ref TRUE: circularizes without the user asserting circular, then rotates.
  # Every other assemble_from_layout test in this file passes ref_seq = NULL, so
  # this wiring was previously uncovered.
  origin_block <- data.frame(scaffold = "joined", qlen = n - 60L, qstart = 100L,
    qend = 300L, strand = "+", ref_start = 0L, ref_end = 200L, nmatch = 200L,
    cigar = NA_character_, stringsAsFactors = FALSE)
  testthat::local_mocked_bindings(run_minimap2_paf = function(...) origin_block)
  res <- assemble_from_layout(df, lay, gap_len = 100L, circular = FALSE,
                              ref_seq = paste0(core, ov))
  expect_equal(res$topology, "circular")
  expect_equal(nchar(res$seq), n - 60L)
  expect_match(res$note, "rotated 100 bp to reference origin")
})

# --- F18 ---
test_that("rotate_to_reference skips rotation when no block covers the reference origin", {
  # APERIODIC fixture. A "ACGT" tandem repeat rotated by a multiple of 4 is
  # indistinguishable from the original, so the sequence assertion below was
  # vacuous against a period-4 fixture.
  set.seed(180)
  seq <- paste(sample(c("A", "C", "G", "T"), 2000L, replace = TRUE), collapse = "")
  n <- nchar(seq); cov <- seq_len(n)
  far_block <- data.frame(scaffold = "joined", qlen = n, qstart = 800L, qend = 1200L,
    strand = "+", ref_start = 3000L, ref_end = 3400L, nmatch = 400L,
    cigar = NA_character_, stringsAsFactors = FALSE)
  testthat::local_mocked_bindings(run_minimap2_paf = function(...) far_block)
  rt <- rotate_to_reference(seq, cov, cov, cov, ref_seq = seq)
  expect_equal(rt$rotated, 0L)                     # min ref_start 3000 > tol -> no rotation
  expect_identical(rt$seq, seq)
  expect_identical(rt$depth, cov)                  # coverage untouched too
})

test_that("rotate_to_reference rotates to the block covering reference position 0", {
  set.seed(181)
  seq <- paste(sample(c("A", "C", "G", "T"), 2000L, replace = TRUE), collapse = "")
  n <- nchar(seq); cov <- seq_len(n)
  origin_block <- data.frame(scaffold = "joined", qlen = n, qstart = 120L, qend = 500L,
    strand = "+", ref_start = 0L, ref_end = 380L, nmatch = 380L,
    cigar = NA_character_, stringsAsFactors = FALSE)
  testthat::local_mocked_bindings(run_minimap2_paf = function(...) origin_block)
  rt <- rotate_to_reference(seq, cov, cov, cov, ref_seq = seq)
  expect_equal(rt$rotated, 120L)                   # qstart of the origin-covering block
  # Assert the actual rotation, and that the coverage vectors rotated with it.
  expect_equal(rt$seq, paste0(substring(seq, 121L), substring(seq, 1L, 120L)))
  expect_equal(rt$depth[1], 121L)
  expect_equal(utils::tail(rt$depth, 1L), 120L)
  expect_equal(length(rt$depth), n)
})



# =============================================================================
# Post-review fixes (18-finding adversarial review of the batches above)
# =============================================================================

# --- review #5 ---
test_that("colinear_chain keeps blocks across a large real indel", {
  # Two colinear blocks separated by a 3 kb reference-side deletion. The old
  # fixed diagonal band (max(1000, 0.2*qlen) = 1600 here) discarded the far side,
  # which then shrank ref_end and fabricated a gap at the next junction.
  rows <- data.frame(
    scaffold = c("s", "s"), qlen = c(8000L, 8000L),
    qstart = c(0L, 4000L), qend = c(4000L, 8000L),
    strand = c("+", "+"),
    ref_start = c(0L, 7000L), ref_end = c(4000L, 11000L),  # 3 kb ref-side indel
    nmatch = c(3900L, 3800L), cigar = NA_character_, stringsAsFactors = FALSE
  )
  cc <- collapse_scaffold_blocks(rows)
  expect_equal(cc$ref_start, 0L)
  expect_equal(cc$ref_end, 11000L)      # both blocks kept; extent spans the indel
  expect_equal(cc$nmatch, 7700L)
})

test_that("colinear_chain still prunes an origin wrap", {
  # Same two blocks, but the query advances while the reference goes BACKWARDS:
  # not colinear, so the monotone walk must still drop one side.
  rows <- data.frame(
    scaffold = c("s", "s"), qlen = c(8000L, 8000L),
    qstart = c(0L, 4000L), qend = c(4000L, 8000L),
    strand = c("+", "+"),
    ref_start = c(12000L, 0L), ref_end = c(16000L, 4000L),
    nmatch = c(3900L, 3800L), cigar = NA_character_, stringsAsFactors = FALSE
  )
  cc <- collapse_scaffold_blocks(rows)
  expect_lt(cc$ref_end - cc$ref_start, 8000L)   # not ballooned to the whole reference
})

# --- review #6 ---
test_that("qcov is measured over all blocks, not the pruned chain", {
  # An origin-wrapping scaffold covers its whole query, just not colinearly.
  # Scoring qcov on the pruned chain halved it and let min_qcov drop a real
  # scaffold; the placement extent still comes from the pruned chain.
  rows <- data.frame(
    scaffold = c("s", "s"), qlen = c(8000L, 8000L),
    qstart = c(0L, 4000L), qend = c(4000L, 8000L),
    strand = c("+", "+"),
    ref_start = c(12000L, 0L), ref_end = c(16000L, 4000L),
    nmatch = c(3900L, 3800L), cigar = NA_character_, stringsAsFactors = FALSE
  )
  cc <- collapse_scaffold_blocks(rows)
  expect_equal(cc$qcov, 1)
})

# --- review #7 ---
test_that("an excluded scaffold keeps its orientation and reference coordinates", {
  mappings <- data.frame(
    scaffold = c("A", "B"),
    ref_start = c(0L, 500L), ref_end = c(3000L, 1000L),
    strand = c("+", "-"),                     # B is minus-strand and subsumed by A
    nmatch = c(2900L, 480L),
    qcov = c(0.95, 0.95), qstart = c(0L, 0L),
    mapped = c(TRUE, TRUE), stringsAsFactors = FALSE
  )
  lay <- derive_scaffold_layout(mappings, min_qcov = 0.5)
  b <- lay[lay$scaffold == "B", ]
  expect_false(b$include)
  expect_true(b$rc)                            # orientation survives exclusion
  expect_equal(b$ref_start, 500L)              # ...and so do the coordinates
  expect_equal(b$ref_end, 1000L)
  # Order is seeded from the reference position, so re-including B in the editor
  # puts it where it belongs rather than at the end.
  expect_lt(lay$order[lay$scaffold == "A"], b$order)
})

# --- review #9 ---
test_that("detect_subsumed keeps a contained scaffold that carries unaligned sequence", {
  inc <- data.frame(
    scaffold = c("A", "B"),
    ref_start = c(0L, 500L), ref_end = c(3000L, 1000L),
    nmatch = c(2900L, 480L),
    qcov = c(0.95, 0.40),      # B's aligned part nests in A, but 60% never aligned
    stringsAsFactors = FALSE
  )
  expect_true(is.na(detect_subsumed(inc)[2]))
  # ...and it IS dropped once essentially all of it is accounted for.
  inc$qcov[2] <- 0.98
  expect_match(detect_subsumed(inc)[2], "subsumed by scaffold A")
})

# --- review #18 ---
test_that("detect_subsumed names the largest container", {
  inc <- data.frame(
    scaffold = c("small", "mid", "big"),
    ref_start = c(500L, 400L, 0L), ref_end = c(1000L, 2000L, 6000L),
    nmatch = c(480L, 1500L, 5500L),
    qcov = c(0.98, 0.98, 0.98), stringsAsFactors = FALSE
  )
  expect_match(detect_subsumed(inc)[1], "subsumed by scaffold big")
})

# --- review #10 ---
test_that("collapse_scaffold_blocks flips qstart onto the reverse-complemented sequence", {
  # The zoom view indexes the ORIENTED (rc'd) scaffold, so a '-' block's
  # forward-query PAF coordinate has to be converted, or every '-' scaffold with
  # asymmetric soft-clipping gets a fabricated base map.
  rows <- data.frame(
    scaffold = "s", qlen = 1000L, qstart = 100L, qend = 900L, strand = "-",
    ref_start = 0L, ref_end = 800L, nmatch = 780L,
    cigar = NA_character_, stringsAsFactors = FALSE
  )
  cc <- collapse_scaffold_blocks(rows)
  expect_equal(cc$qstart, 100L)          # qlen - qend = 1000 - 900
  # '+' is unchanged
  rows$strand <- "+"
  expect_equal(collapse_scaffold_blocks(rows)$qstart, 100L)
  rows$qend <- 950L
  rows$strand <- "-"
  expect_equal(collapse_scaffold_blocks(rows)$qstart, 50L)
})

# --- review #13 ---
test_that("refine_overlap rejects an alignment dragged through a divergent tail", {
  skip_if_not_installed("pwalign")
  set.seed(131)
  bp <- function(n) paste0(sample(c("A", "C", "G", "T"), n, replace = TRUE), collapse = "")
  shared <- bp(120)
  # A ends with the shared block plus 25 bp that B does not have: a genuine
  # junction would end in exact identities, this one cannot.
  a <- paste0(bp(400), shared, bp(25))
  b <- paste0(shared, bp(600))
  ov <- refine_overlap(a, b, est_overlap = 120, min_identity = 0.7, min_overlap = 10L)
  expect_false(ov$reliable)
  expect_equal(ov$trim_b, 0L)
  # ...while the same pair with A actually ending at the shared block confirms.
  a2 <- paste0(bp(400), shared)
  ov2 <- refine_overlap(a2, b, est_overlap = 120, min_identity = 0.7, min_overlap = 10L)
  expect_true(ov2$reliable)
  expect_equal(ov2$trim_b, 120L)
})

# --- review #3 ---
test_that("refine_overlap reports indels so the consensus can be skipped", {
  skip_if_not_installed("pwalign")
  set.seed(132)
  bp <- function(n) paste0(sample(c("A", "C", "G", "T"), n, replace = TRUE), collapse = "")
  shared <- bp(200)
  a <- paste0(bp(300), shared)
  # B's copy of the overlap carries a 1 bp insertion: pairing A and B by index
  # across this junction would shift every downstream base.
  b_ov <- paste0(substring(shared, 1L, 100L), "G", substring(shared, 101L))
  b <- paste0(b_ov, bp(400))
  ov <- refine_overlap(a, b, est_overlap = 200, min_identity = 0.7, min_overlap = 10L)
  expect_gt(ov$n_indel, 0L)
  expect_true(ov$pat_len > 0L)
})

test_that("join_scaffolds skips the consensus when the overlap is not base-alignable", {
  skip_if_not_installed("pwalign")
  set.seed(133)
  bp <- function(n) paste0(sample(c("A", "C", "G", "T"), n, replace = TRUE), collapse = "")
  shared <- bp(200)
  a <- paste0(bp(300), shared)
  b <- paste0(substring(shared, 1L, 100L), "G", substring(shared, 101L), bp(400))
  lay <- data.frame(
    scaffold = c("A", "B"), order = 1:2, rc = c(FALSE, FALSE),
    gap_before = c(NA, -200), include = c(TRUE, TRUE), stringsAsFactors = FALSE
  )
  res <- join_scaffolds(list(A = a, B = b), lay,
                        scaffold_depth = list(A = rep(10, nchar(a)), B = rep(50, nchar(b))))
  # No IUPAC codes manufactured across the junction.
  expect_false(grepl("[RYSWKMBDHVN]", res$seq))
  expect_true(any(grepl("not base-alignable", res$junctions)))
})

# --- review #12 ---
test_that("plot_scaffold_mapping still renders an included but unplaced scaffold", {
  lay <- data.frame(
    scaffold = c("A", "B"), order = 1:2, rc = c(FALSE, FALSE),
    gap_before = c(NA_real_, NA_real_), mapped = c(TRUE, FALSE),
    include = c(TRUE, TRUE), exclude_reason = c(NA_character_, NA_character_),
    qcov = c(0.9, 0), ref_start = c(0L, NA_integer_), ref_end = c(1000L, NA_integer_),
    qstart = c(0L, NA_integer_), stringsAsFactors = FALSE
  )
  pf <- tempfile(fileext = ".png")
  grDevices::png(pf); on.exit(unlink(pf), add = TRUE)
  expect_silent(plot_scaffold_mapping(lay, ref_len = 1000))
  grDevices::dev.off()
  expect_gt(file.info(pf)$size, 0)
})
