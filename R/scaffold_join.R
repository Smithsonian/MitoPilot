#' Multi-scaffold assembly joining
#'
#' Reference-guided ordering of the scaffolds of a single fragmented assembly
#' path into one joined sequence: map each scaffold to a reference mitogenome
#' with minimap2, order + orient by reference coordinate, insert N-gap spacers
#' where scaffolds are disjoint, and stitch per-position coverage to match.
#'
#' Pure-logic functions here back both the automatic Nextflow join (WF1) and the
#' in-app manual override editor, so they take/return plain vectors and
#' data.frames and never touch Shiny or the database directly.
#'
#' @name scaffold_join
NULL

#' Is a sample eligible for automatic scaffold joining?
#'
#' Eligible when exactly one assembler path is present and that path is
#' fragmented into more than one scaffold. Samples with multiple competing paths
#' that are also fragmented are NOT eligible (ambiguous which path's scaffolds to
#' join) and are left blocked.
#'
#' @param assemblies_df data.frame with integer `path` and `scaffold` columns
#'   (the per-scaffold rows for one sample, path 0 / ignored rows excluded by
#'   the caller as appropriate).
#' @return TRUE if single-path multi-scaffold.
#' @noRd
scaffold_join_eligible <- function(assemblies_df) {
  raw <- assemblies_df[!is.na(assemblies_df$path) & assemblies_df$path > 0, , drop = FALSE]
  if (nrow(raw) == 0) return(FALSE)
  n_paths <- length(unique(raw$path))
  max_scaf <- max(table(raw$path))
  n_paths == 1 && max_scaf > 1
}

#' Choose a single reference accession for a multi-scaffold sample
#'
#' Scaffolds can carry different BLAST hits, but all must map to ONE reference or
#' their coordinates are not comparable. Weight each distinct accession by
#' `length * (blast_pident / 100)` and pick the max; ties broken toward a
#' complete reference (when `ref_length` supplied) then the longest scaffold's
#' hit.
#'
#' @param scaffolds_df data.frame with `blast_accession`, `length`,
#'   `blast_pident`, and optionally `ref_length` columns.
#' @return single accession string, or NA if none available.
#' @noRd
choose_reference <- function(scaffolds_df) {
  df <- scaffolds_df[!is.na(scaffolds_df$blast_accession) &
                       nzchar(scaffolds_df$blast_accession), , drop = FALSE]
  if (nrow(df) == 0) return(NA_character_)
  pid <- suppressWarnings(as.numeric(df$blast_pident))
  pid[is.na(pid)] <- 0
  len <- suppressWarnings(as.numeric(df$length))
  len[is.na(len)] <- 0
  df$weight <- len * (pid / 100)
  agg <- stats::aggregate(weight ~ blast_accession, data = df, FUN = sum)
  ord <- order(-agg$weight)
  agg <- agg[ord, , drop = FALSE]
  agg$blast_accession[1]
}

#' Parse the workflow's per-scaffold BLAST-hit string
#'
#' @param s ';'-separated records of "scaffold|accession|pident" (as emitted by
#'   BLAST_GENBANK). "NO HIT" / empty accessions become NA.
#' @param scaf_len named numeric of scaffold lengths (names = scaffold ids), used
#'   to fill the `length` column that [choose_reference()] weights by.
#' @return data.frame(scaffold, length, blast_accession, blast_pident), or NULL
#'   when `s` is empty (caller falls back to the DB).
#' @noRd
parse_scaffold_hits <- function(s, scaf_len = NULL) {
  if (is.null(s) || length(s) != 1 || is.na(s) || !nzchar(s)) return(NULL)
  recs <- strsplit(s, ";", fixed = TRUE)[[1]]
  recs <- recs[nzchar(recs)]
  if (length(recs) == 0) return(NULL)
  parts <- strsplit(recs, "|", fixed = TRUE)
  scaffold <- vapply(parts, function(p) if (length(p) >= 1) p[1] else NA_character_, character(1))
  acc      <- vapply(parts, function(p) if (length(p) >= 2) p[2] else NA_character_, character(1))
  pid      <- vapply(parts, function(p) if (length(p) >= 3) p[3] else NA_character_, character(1))
  acc[acc %in% c("", "NO HIT", "NA", "null")] <- NA_character_
  len <- if (!is.null(scaf_len)) unname(scaf_len[scaffold]) else rep(NA_real_, length(scaffold))
  data.frame(
    scaffold = scaffold,
    length = as.numeric(len),
    blast_accession = acc,
    blast_pident = suppressWarnings(as.numeric(pid)),
    stringsAsFactors = FALSE
  )
}

#' Do a sample's scaffolds carry conflicting BLAST hits?
#'
#' Joining scaffolds that hit different references is risky (poor overlap, bad
#' joins), so the automatic join is skipped and the app forces an explicit user
#' override in this case.
#'
#' @param scaffolds_df data.frame with a `blast_accession` column.
#' @return TRUE when more than one distinct non-empty accession is present.
#' @noRd
scaffold_hits_disagree <- function(scaffolds_df) {
  if (is.null(scaffolds_df) || is.null(scaffolds_df$blast_accession)) return(FALSE)
  acc <- scaffolds_df$blast_accession
  acc <- acc[!is.na(acc) & nzchar(acc)]
  length(unique(acc)) > 1
}

#' Locate the minimap2 binary
#'
#' Honors `options(MitoPilot.minimap2=)`, else `minimap2` on PATH. Returns "" if
#' not found (callers disable reference-guided layout in that case).
#' @noRd
find_minimap2 <- function() {
  p <- getOption("MitoPilot.minimap2", "")
  if (nzchar(p)) return(p)
  unname(Sys.which("minimap2"))
}

#' Map scaffolds to a reference mitogenome with minimap2
#'
#' @param scaffold_seqs named character vector of scaffold sequences (names are
#'   the scaffold identifiers used downstream).
#' @param ref_seq reference mitogenome sequence (single string).
#' @param minimap2 path to the minimap2 binary.
#' @return data.frame with one best row per scaffold:
#'   `scaffold, ref_start, ref_end, strand, nmatch, qcov, mapped`. `qcov` is the
#'   fraction of the scaffold covered by its best alignment (a "maps reasonably
#'   well" signal). Scaffolds with no alignment get `mapped = FALSE`, NA coords,
#'   and `qcov = 0`.
#' @noRd
map_scaffolds_to_reference <- function(scaffold_seqs, ref_seq,
                                       minimap2 = getOption("MitoPilot.minimap2", "minimap2")) {
  scaffolds <- names(scaffold_seqs)
  empty <- data.frame(
    scaffold = scaffolds, ref_start = NA_integer_, ref_end = NA_integer_,
    strand = NA_character_, nmatch = NA_integer_, qcov = 0, qstart = NA_integer_,
    mapped = FALSE, stringsAsFactors = FALSE
  )
  if (length(scaffold_seqs) == 0 || is.na(ref_seq) || !nzchar(ref_seq)) return(empty)

  rows <- run_minimap2_paf(scaffold_seqs, ref_seq, minimap2)
  if (nrow(rows) == 0) return(empty)

  best <- collapse_scaffold_blocks(rows)
  out <- merge(empty[, "scaffold", drop = FALSE], best, by = "scaffold", all.x = TRUE)
  out$qcov[is.na(out$qcov)] <- 0
  out$mapped <- !is.na(out$ref_start)
  out[match(scaffolds, out$scaffold), , drop = FALSE]
}

#' Map scaffolds against every candidate reference (long-format, for precompute)
#'
#' Runs [map_scaffolds_to_reference()] once per candidate reference and stacks the
#' results with a `ref_accession` column. Written to the DB by the Nextflow join
#' so the in-app manual editor can build layouts WITHOUT minimap2 installed.
#'
#' @param scaffold_seqs named character vector of scaffold sequences.
#' @param ref_seqs named character vector of reference sequences (names are the
#'   accessions; one mapping block is produced per non-empty entry).
#' @param minimap2 path to the minimap2 binary.
#' @return long data.frame: `ref_accession` plus the columns from
#'   [map_scaffolds_to_reference()] (`scaffold, ref_start, ref_end, strand,
#'   nmatch, qcov, qstart, mapped`); empty data.frame when nothing to map.
#' @noRd
compute_scaffold_mappings <- function(scaffold_seqs, ref_seqs,
                                      minimap2 = getOption("MitoPilot.minimap2", "minimap2")) {
  empty <- data.frame(
    ref_accession = character(0), scaffold = character(0),
    ref_start = integer(0), ref_end = integer(0), strand = character(0),
    nmatch = integer(0), qcov = numeric(0), qstart = integer(0),
    mapped = logical(0), stringsAsFactors = FALSE)
  if (length(scaffold_seqs) == 0 || length(ref_seqs) == 0) return(empty)
  accs <- names(ref_seqs)
  blocks <- lapply(seq_along(ref_seqs), function(i) {
    rs <- ref_seqs[[i]]
    if (is.na(rs) || !nzchar(rs)) return(NULL)
    m <- map_scaffolds_to_reference(scaffold_seqs, rs, minimap2)
    cbind(ref_accession = accs[i], m, stringsAsFactors = FALSE)
  })
  blocks <- blocks[!vapply(blocks, is.null, logical(1))]
  if (length(blocks) == 0) return(empty)
  do.call(rbind, blocks)
}

#' Load precomputed scaffold->reference mappings from the DB
#'
#' Reads the `scaffold_mappings` rows for one sample + accession and returns them
#' in the same shape [map_scaffolds_to_reference()] produces, so
#' [derive_scaffold_layout()] consumes them unchanged (no minimap2 needed).
#'
#' @param con a DBI connection.
#' @param ID sample id.
#' @param accession chosen reference accession.
#' @return data.frame with `scaffold, ref_start, ref_end, strand, nmatch, qcov,
#'   qstart, mapped`, or NULL when no rows exist.
#' @noRd
load_scaffold_mappings <- function(con, ID, accession) {
  if (is.null(accession) || is.na(accession) || !nzchar(accession)) return(NULL)
  rows <- tryCatch(
    DBI::dbGetQuery(con, paste0(
      "SELECT scaffold, ref_start, ref_end, strand, nmatch, qcov, qstart, mapped ",
      "FROM scaffold_mappings WHERE ID = ? AND ref_accession = ?"),
      params = list(ID, accession)),
    error = function(e) NULL)
  if (is.null(rows) || nrow(rows) == 0) return(NULL)
  rows$scaffold <- as.character(rows$scaffold)
  rows$mapped <- as.logical(rows$mapped)
  rows$qcov[is.na(rows$qcov)] <- 0
  rows
}

#' Total length covered by a set of (possibly overlapping) intervals
#' @noRd
union_len <- function(starts, ends) {
  if (length(starts) == 0) return(0)
  o <- order(starts)
  s <- starts[o]; e <- ends[o]
  total <- 0; cur_s <- s[1]; cur_e <- e[1]
  for (i in seq_along(s)[-1]) {
    if (s[i] <= cur_e) {
      cur_e <- max(cur_e, e[i])
    } else {
      total <- total + (cur_e - cur_s); cur_s <- s[i]; cur_e <- e[i]
    }
  }
  total + (cur_e - cur_s)
}

#' Run minimap2 (query seqs vs one reference) and parse the PAF
#' @param query_seqs named character vector of query sequences.
#' @param ref_seq reference sequence (single string).
#' @return parsed PAF data.frame (see [parse_paf()]); empty on failure.
#' @noRd
run_minimap2_paf <- function(query_seqs, ref_seq,
                             minimap2 = getOption("MitoPilot.minimap2", "minimap2"),
                             cigar = FALSE) {
  empty <- parse_paf(character(0))
  if (length(query_seqs) == 0 || is.na(ref_seq) || !nzchar(ref_seq)) return(empty)
  qfile <- tempfile(fileext = ".fa")
  tfile <- tempfile(fileext = ".fa")
  on.exit(unlink(c(qfile, tfile)), add = TRUE)
  nm <- names(query_seqs)
  writeLines(unlist(lapply(nm, function(s) c(paste0(">", s), query_seqs[[s]]))), qfile)
  writeLines(c(">ref", ref_seq), tfile)
  # Mitogenome references are often cross-species (~25-30% divergent); the asm20
  # preset alone (k=19) misses them, so use a smaller k-mer for sensitivity.
  mm_args <- getOption("MitoPilot.minimap2_args", c("-x", "asm20", "-k", "13"))
  if (isTRUE(cigar)) mm_args <- c(mm_args, "-c")
  paf <- tryCatch(
    system2(minimap2, c(mm_args, shQuote(tfile), shQuote(qfile)),
            stdout = TRUE, stderr = FALSE),
    error = function(e) character(0)
  )
  parse_paf(paf)
}

#' Parse minimap2 PAF lines (tab-separated, no extra dependency)
#' @noRd
parse_paf <- function(paf_lines) {
  parts <- strsplit(paf_lines, "\t", fixed = TRUE)
  parts <- parts[vapply(parts, length, integer(1)) >= 12]
  if (length(parts) == 0) {
    return(data.frame(scaffold = character(0), qlen = integer(0),
                      qstart = integer(0), qend = integer(0), strand = character(0),
                      ref_start = integer(0), ref_end = integer(0),
                      nmatch = integer(0), cigar = character(0),
                      stringsAsFactors = FALSE))
  }
  # CIGAR (cg:Z:) tag if present (only with minimap2 -c), captured before truncating.
  cig <- vapply(parts, function(p) {
    hit <- grep("^cg:Z:", p, value = TRUE)
    if (length(hit)) sub("^cg:Z:", "", hit[1]) else NA_character_
  }, character(1))
  m <- do.call(rbind, lapply(parts, `[`, 1:12))
  data.frame(
    scaffold  = m[, 1],
    qlen      = as.integer(m[, 2]),
    qstart    = as.integer(m[, 3]),
    qend      = as.integer(m[, 4]),
    strand    = m[, 5],
    ref_start = as.integer(m[, 8]),
    ref_end   = as.integer(m[, 9]),
    nmatch    = as.integer(m[, 10]),
    cigar     = cig,
    stringsAsFactors = FALSE
  )
}

#' Collapse a scaffold's PAF alignment blocks to one placement row
#'
#' A scaffold crossing a duplication/rearrangement maps to the reference in
#' several colinear blocks; a single best block underplaces its order and gaps.
#' Orientation = the strand carrying the most matched residues; reference
#' placement = the UNION extent (`min(ref_start)`..`max(ref_end)`) of that
#' strand's blocks. `qstart`/`ref_start` are kept as a colinear pair (the block
#' at min `ref_start`) so the zoom view can still map reference positions back to
#' scaffold bases. `qcov` is the union of query intervals across ALL blocks.
#'
#' @param rows parsed PAF rows (see [parse_paf()]) for one or more scaffolds.
#' @return one row per scaffold: `scaffold, ref_start, ref_end, strand, nmatch,
#'   qcov, qstart`.
#' @noRd
collapse_scaffold_blocks <- function(rows) {
  do.call(rbind, lapply(split(rows, rows$scaffold), function(g) {
    by_strand <- tapply(g$nmatch, g$strand, sum)
    dom <- names(by_strand)[which.max(by_strand)]
    gs <- g[g$strand == dom, , drop = FALSE]
    anchor <- gs[which.min(gs$ref_start), , drop = FALSE]
    data.frame(
      scaffold  = g$scaffold[1],
      ref_start = min(gs$ref_start),
      ref_end   = max(gs$ref_end),
      strand    = dom,
      nmatch    = sum(gs$nmatch),
      qcov      = union_len(g$qstart, g$qend) / g$qlen[1],
      qstart    = anchor$qstart,
      stringsAsFactors = FALSE
    )
  }))
}

#' Per-reference-position scaffold bases for ONE reference window (lazy)
#'
#' For the base-pair zoom: aligns just the small scaffold sub-region that maps to
#' `[win_start, win_end]` (estimated from the minimap2 colinear offset, padded)
#' against the reference window. This keeps each alignment tiny and works for
#' divergent references (where minimap2 `-c` CIGAR is unreliable).
#'
#' @param ref_seq reference sequence.
#' @param layout_inc included layout rows (need `scaffold, rc, ref_start, ref_end,
#'   qstart`); `ref_start`/`qstart` are 0-based as emitted by minimap2.
#' @param oriented_seqs named character vector of ORIENTED scaffold sequences.
#' @param win_start,win_end 1-based reference window.
#' @param pad bp of slack around the estimated scaffold sub-region.
#' @return named list (by scaffold id) of reference-position -> base vectors,
#'   restricted to the window.
#' @noRd
zoom_window_base_maps <- function(ref_seq, layout_inc, oriented_seqs,
                                  win_start, win_end, pad = 100L) {
  ws <- max(1L, as.integer(win_start)); we <- min(nchar(ref_seq), as.integer(win_end))
  rsub_s <- max(1L, ws - pad); rsub_e <- min(nchar(ref_seq), we + pad)
  ref_sub <- substring(ref_seq, rsub_s, rsub_e)
  mx <- pwalign::nucleotideSubstitutionMatrix(match = 1, mismatch = -1)
  out <- list()
  for (i in seq_len(nrow(layout_inc))) {
    s <- as.character(layout_inc$scaffold[i])
    rstart <- layout_inc$ref_start[i]; rend <- layout_inc$ref_end[i]
    qstart <- layout_inc$qstart[i]
    if (is.na(rstart) || is.na(qstart) || rend < ws || rstart > we) next
    oseq <- oriented_seqs[[s]]
    if (is.null(oseq)) next
    # colinear estimate of scaffold coords for [ws, we] (PAF coords are 0-based)
    qs_est <- qstart + ((ws - 1L) - rstart)
    qe_est <- qstart + ((we - 1L) - rstart)
    sub_s <- max(1L, qs_est + 1L - pad); sub_e <- min(nchar(oseq), qe_est + 1L + pad)
    if (sub_e <= sub_s) next
    scaf_sub <- substring(oseq, sub_s, sub_e)
    aln <- tryCatch(
      pwalign::pairwiseAlignment(
        pattern = Biostrings::DNAString(scaf_sub),
        subject = Biostrings::DNAString(ref_sub),
        substitutionMatrix = mx, gapOpening = 5, gapExtension = 2,
        type = "global-local"),
      error = function(e) NULL)
    if (is.null(aln)) next
    ref_off <- rsub_s + BiocGenerics::start(pwalign::subject(aln)@range) - 1L
    bm <- build_ref_base_map(as.character(pwalign::alignedPattern(aln)),
                             as.character(pwalign::alignedSubject(aln)), ref_off)
    out[[s]] <- bm[as.character(ws:we)]
  }
  out
}

#' Derive scaffold layout (order + orientation + gaps) from reference mappings
#'
#' Only scaffolds that map reasonably well to the reference (query coverage
#' >= `min_qcov`) are INCLUDED in the joined Path 0. Scaffolds with no alignment
#' or poor coverage are kept in the layout (so the editor/plot can show them) but
#' flagged `include = FALSE` and left out of the join. If nothing clears the bar,
#' a fallback keeps the best-available scaffolds so Path 0 is never empty.
#'
#' @param mappings data.frame from [map_scaffolds_to_reference()].
#' @param ref_len reference length (used only when `circular`).
#' @param circular whether to treat the joined molecule as circular.
#' @param min_qcov minimum fraction of a scaffold that must align to the
#'   reference for it to be used (default from option `MitoPilot.scaffold_min_qcov`).
#' @return data.frame ordered for joining:
#'   `scaffold, order, rc, gap_before, mapped, include, qcov, ref_start, ref_end`.
#'   `gap_before` (for included neighbors) is positive = estimated N-gap
#'   (disjoint on the reference), negative = overlap to trim from the current
#'   scaffold.
#' @noRd
derive_scaffold_layout <- function(mappings, ref_len = NA_integer_, circular = FALSE,
                                   min_qcov = getOption("MitoPilot.scaffold_min_qcov", 0.5)) {
  qcov <- if (!is.null(mappings$qcov)) mappings$qcov else rep(NA_real_, nrow(mappings))
  qcov[is.na(qcov)] <- 0
  mappings$qcov <- qcov
  well <- mappings$mapped & qcov >= min_qcov
  # Fallbacks so a Path 0 is always produced: if nothing maps well, take whatever
  # mapped at all; if nothing mapped, take everything (degenerate, flagged).
  if (!any(well)) well <- mappings$mapped
  if (!any(well)) well <- rep(TRUE, nrow(mappings))

  inc <- mappings[well, , drop = FALSE]
  exc <- mappings[!well, , drop = FALSE]
  ord <- order(inc$ref_start, inc$ref_end)
  inc <- inc[ord, , drop = FALSE]

  gap_before <- rep(NA_real_, nrow(inc))
  if (nrow(inc) >= 2) {
    for (i in 2:nrow(inc)) {
      gap_before[i] <- inc$ref_start[i] - inc$ref_end[i - 1]
    }
  }

  qstart_col <- if (!is.null(mappings$qstart)) {
    list(inc = inc$qstart, exc = exc$qstart)
  } else {
    list(inc = rep(NA_integer_, nrow(inc)), exc = rep(NA_integer_, nrow(exc)))
  }
  lay <- data.frame(
    scaffold = c(inc$scaffold, exc$scaffold),
    rc = c(inc$strand == "-", rep(FALSE, nrow(exc))),
    gap_before = c(gap_before, rep(NA_real_, nrow(exc))),
    mapped = c(inc$mapped, exc$mapped),
    include = c(rep(TRUE, nrow(inc)), rep(FALSE, nrow(exc))),
    qcov = c(inc$qcov, exc$qcov),
    ref_start = c(inc$ref_start, rep(NA_integer_, nrow(exc))),
    ref_end = c(inc$ref_end, rep(NA_integer_, nrow(exc))),
    qstart = c(qstart_col$inc, qstart_col$exc),
    stringsAsFactors = FALSE
  )
  lay$rc[is.na(lay$rc)] <- FALSE
  lay$order <- seq_len(nrow(lay))
  lay[, c("scaffold", "order", "rc", "gap_before", "mapped", "include",
          "qcov", "ref_start", "ref_end", "qstart")]
}

#' Reverse-complement a DNA string
#' @noRd
rc_seq <- function(seq) {
  as.character(Biostrings::reverseComplement(Biostrings::DNAString(seq)))
}

#' Verify a scaffold-junction overlap by aligning the actual scaffold ends
#'
#' The reference coordinates only *suggest* an overlap of `est_overlap` bp; the
#' two scaffolds may be divergent (not the same bases) there. This aligns the 3'
#' end of `a_seq` to the 5' end of `b_seq` (free end gaps, `pwalign` overlap
#' alignment) to find the real overlap length + identity, so the join can trim at
#' the true boundary rather than a coordinate estimate.
#'
#' @param a_seq,b_seq oriented (RC-applied) sequences of the two scaffolds.
#' @param est_overlap reference-estimated overlap (bp, > 0). Also sizes the probe
#'   window, so pass a discovery window when the reference does NOT predict an
#'   overlap (see [join_scaffolds()]).
#' @param min_identity minimum overlap identity to "confirm" the overlap.
#' @param min_overlap minimum aligned length to "confirm" the overlap; raise it
#'   for overlaps not predicted by the reference to suppress spurious short hits.
#' @return list(reliable, trim_b, identity, overlap_len): `trim_b` = bases to drop
#'   from the start of `b_seq` (the confirmed overlap length when reliable).
#' @noRd
refine_overlap <- function(a_seq, b_seq, est_overlap, min_identity = 0.7,
                           min_overlap = 10L) {
  est <- max(as.integer(round(est_overlap)), 1L)
  slack <- max(50L, as.integer(round(est)))
  L <- min(nchar(a_seq), nchar(b_seq), est + slack)
  out <- list(reliable = FALSE, trim_b = 0L, identity = NA_real_, overlap_len = 0L)
  if (L < 10L) return(out)
  a_tail <- substring(a_seq, nchar(a_seq) - L + 1L)
  b_head <- substring(b_seq, 1L, L)
  mx <- pwalign::nucleotideSubstitutionMatrix(match = 1, mismatch = -1)
  aln <- tryCatch(
    pwalign::pairwiseAlignment(
      pattern = Biostrings::DNAString(a_tail),
      subject = Biostrings::DNAString(b_head),
      substitutionMatrix = mx, type = "overlap",
      gapOpening = 5, gapExtension = 2),
    error = function(e) NULL)
  if (is.null(aln)) return(out)
  alen <- pwalign::nchar(aln)
  if (alen < max(min_overlap, 1L)) return(out)
  ident <- pwalign::nmatch(aln) / alen
  # Overlap = a's suffix vs b's prefix, so the aligned subject (b_head) region
  # should start at base 1; trim b by where that aligned region ends.
  sub_rng <- pwalign::subject(aln)@range
  sub_start <- BiocGenerics::start(sub_rng)
  sub_end <- BiocGenerics::end(sub_rng)
  if (ident >= min_identity && sub_start <= 2L) {
    out$reliable <- TRUE
    out$trim_b <- as.integer(sub_end)
    out$identity <- ident
    out$overlap_len <- as.integer(alen)
  } else {
    out$identity <- ident
  }
  out
}

#' Coverage-majority consensus across a confirmed scaffold overlap
#'
#' For the `k`-bp overlap (scaffold A's suffix vs scaffold B's prefix), pick the
#' base from the higher read-depth scaffold at each disagreement; IUPAC code at a
#' near-tie, N where both are below `min_depth`. Per-position `src_*` point at the
#' chosen scaffold's coverage so depth stitches correctly.
#'
#' @param a_ch,b_ch length-k char vectors (A suffix, B prefix; oriented).
#' @param a_dep,b_dep length-k depth vectors.
#' @param min_depth,tie_ratio thresholds.
#' @return list(base, use_b, n_mismatch, n_iupac, n_n). `use_b[j]` = TRUE when the
#'   consensus should draw its coverage from scaffold B (position j); otherwise A's
#'   existing coverage is kept.
#' @noRd
overlap_consensus <- function(a_ch, b_ch, a_dep, b_dep,
                              min_depth = 5, tie_ratio = 1.5) {
  k <- length(a_ch)
  base <- character(k); use_b <- logical(k)
  n_mm <- 0L; n_iu <- 0L; n_n <- 0L
  for (j in seq_len(k)) {
    ab <- a_ch[j]; bb <- b_ch[j]
    ad <- if (length(a_dep) >= j && !is.na(a_dep[j])) a_dep[j] else 0
    bd <- if (length(b_dep) >= j && !is.na(b_dep[j])) b_dep[j] else 0
    if (toupper(ab) == toupper(bb)) {
      base[j] <- ab
    } else {
      n_mm <- n_mm + 1L
      if (max(ad, bd) < min_depth) {
        base[j] <- "N"; n_n <- n_n + 1L; use_b[j] <- bd > ad
      } else if (ad >= bd * tie_ratio) {
        base[j] <- ab
      } else if (bd >= ad * tie_ratio) {
        base[j] <- bb; use_b[j] <- TRUE
      } else {
        base[j] <- iupac_code(c(ab, bb)); n_iu <- n_iu + 1L; use_b[j] <- bd > ad
      }
    }
  }
  list(base = base, use_b = use_b, n_mismatch = n_mm, n_iupac = n_iu, n_n = n_n)
}

#' Join scaffolds into one sequence following a layout
#'
#' @param scaffold_seqs named character vector of scaffold sequences.
#' @param layout data.frame from [derive_scaffold_layout()] (assumed pre-ordered).
#' @param gap_len_default N-gap length used for unknown junctions (unmapped or
#'   missing gap estimate).
#' @param circular reserved for topology bookkeeping (sequence stays linear).
#' @param scaffold_depth optional named list of per-scaffold depth vectors
#'   (ORIGINAL orientation); enables coverage-majority consensus across confirmed
#'   overlaps instead of keeping the first scaffold wholesale.
#' @return list(seq, src_scaffold, src_pos, junctions, junction_info) where the
#'   two `src_*` vectors are per joined base: the contributing scaffold id and the
#'   1-based index into that scaffold's ORIENTED (RC-applied) sequence, or NA over
#'   gap spacers; `junctions` is a character vector describing overlap resolutions;
#'   `junction_info` is a data.frame (one row per junction: from, to, type,
#'   gap_bases, overlap_len, identity) backing [summarize_join_quality()].
#' @noRd
join_scaffolds <- function(scaffold_seqs, layout, gap_len_default = 100L,
                           circular = FALSE, scaffold_depth = NULL) {
  # Only included scaffolds contribute to the joined sequence.
  if ("include" %in% names(layout)) {
    layout <- layout[layout$include, , drop = FALSE]
  }
  oriented_depth <- function(s, rc) {
    if (is.null(scaffold_depth) || is.null(scaffold_depth[[s]])) return(NULL)
    d <- scaffold_depth[[s]]; if (isTRUE(rc)) rev(d) else d
  }
  pieces <- character(0)
  src_scaffold <- character(0)
  src_pos <- integer(0)
  junctions <- character(0)
  junc_rec <- list()                         # one structured record per junction
  prev_oriented <- NULL
  prev_scaf <- NULL
  prev_depth <- NULL

  for (i in seq_len(nrow(layout))) {
    s <- layout$scaffold[i]
    seq <- scaffold_seqs[[s]]
    dep <- oriented_depth(s, layout$rc[i])
    if (isTRUE(layout$rc[i])) seq <- rc_seq(seq)

    trim <- 0L
    consensus <- NULL
    if (i > 1) {
      gb <- layout$gap_before[i]
      # Structured junction descriptor (type + measured overlap quality), kept
      # alongside the prose `junctions` for the join-quality summary.
      jtype <- NA_character_; j_ov_len <- 0L; j_ident <- NA_real_
      ngap <- 0L

      # Probe the ACTUAL scaffold ends for an overlap at every adjacency, not just
      # when the reference predicts one. On divergent cross-species references
      # minimap2 alignments are usually inset from the true scaffold ends, so the
      # reference gap estimate is biased positive: scaffolds that truly abut or
      # overlap show gb >= 0 and would otherwise be N-padded or butt-joined
      # (duplicating the shared bases). Stringency: lenient when the reference
      # predicts the overlap (gb < 0), strict when discovering one it did not
      # (gb >= 0), and skip entirely for a large predicted gap (trust it) or an
      # unmapped/unknown junction (gb is NA).
      probe_win <- as.integer(getOption("MitoPilot.scaffold_overlap_probe", 300L))
      ref_overlap <- !is.na(gb) && gb < 0
      do_probe <- !is.na(gb) && gb <= probe_win
      est <- if (ref_overlap) abs(as.integer(round(gb))) else probe_win
      ov <- if (do_probe) refine_overlap(
        prev_oriented, seq, est,
        min_identity = if (ref_overlap) 0.7 else 0.9,
        min_overlap  = if (ref_overlap) 10L else 20L) else NULL

      if (!is.null(ov) && isTRUE(ov$reliable)) {
        trim <- ov$trim_b
        jtype <- "overlap_confirmed"; j_ov_len <- ov$overlap_len; j_ident <- ov$identity
        jmsg <- sprintf("%s|%s overlap %d bp confirmed (%.0f%% identity), merged",
                        prev_scaf, s, ov$overlap_len, 100 * ov$identity)
        # Coverage-majority consensus across the overlap (replaces A's tail).
        if (!is.null(prev_depth) && !is.null(dep) && trim >= 1L) {
          k <- min(trim, nchar(prev_oriented))
          pl <- nchar(prev_oriented)
          a_ch <- strsplit(substring(prev_oriented, pl - k + 1L), "", fixed = TRUE)[[1]]
          b_ch <- strsplit(substring(seq, 1L, k), "", fixed = TRUE)[[1]]
          cc <- overlap_consensus(
            a_ch, b_ch, utils::tail(prev_depth, k), utils::head(dep, k))
          consensus <- cc
          if (cc$n_mismatch > 0) jmsg <- sprintf(
            "%s; %d mismatches resolved by coverage (%d IUPAC, %d N)",
            jmsg, cc$n_mismatch, cc$n_iupac, cc$n_n)
        }
        junctions <- c(junctions, jmsg)
      } else if (ref_overlap) {
        # Reference predicted an overlap but the ends did not confirm it: trim by
        # the estimate to avoid duplicating the region, flag for review.
        est <- abs(as.integer(round(gb)))
        trim <- min(est, nchar(seq))
        jtype <- "overlap_unconfirmed"; j_ident <- if (!is.null(ov)) ov$identity else NA_real_
        junctions <- c(junctions, sprintf(
          "%s|%s reference overlap %d bp NOT confirmed by alignment%s; trimmed by estimate (review)",
          prev_scaf, s, est,
          if (!is.null(ov) && !is.na(ov$identity)) sprintf(" (best %.0f%% identity)", 100 * ov$identity) else ""))
      } else if (is.na(gb)) {
        ngap <- gap_len_default; jtype <- "gap"   # unmapped/unknown junction
      } else if (gb > 0) {
        ngap <- as.integer(round(gb)); jtype <- "gap"   # disjoint on reference
      } else {
        jtype <- "butt"                            # gb == 0, no overlap found
      }
      junc_rec[[length(junc_rec) + 1L]] <- data.frame(
        from = prev_scaf, to = s, type = jtype,
        gap_bases = if (ngap > 0) as.integer(ngap) else 0L,
        overlap_len = as.integer(j_ov_len), identity = as.numeric(j_ident),
        stringsAsFactors = FALSE)
      if (ngap > 0) {
        pieces <- c(pieces, strrep("N", ngap))
        src_scaffold <- c(src_scaffold, rep(NA_character_, ngap))
        src_pos <- c(src_pos, rep(NA_integer_, ngap))
      }
    }

    # Replace the previous scaffold's overlap tail bases with the consensus; for
    # positions the consensus took from B, repoint coverage to B (oriented pos j),
    # leaving A-sourced positions on their existing (correct) coverage index.
    if (!is.null(consensus)) {
      k <- length(consensus$base)
      lastp <- pieces[length(pieces)]
      pieces[length(pieces)] <- paste0(substring(lastp, 1L, nchar(lastp) - k),
                                       paste0(consensus$base, collapse = ""))
      nn <- length(src_scaffold)
      idx <- (nn - k + 1L):nn
      ub <- which(consensus$use_b)
      if (length(ub) > 0) {
        src_scaffold[idx[ub]] <- s
        src_pos[idx[ub]] <- ub
      }
    }

    if (trim > 0) seq <- substring(seq, trim + 1L)
    n <- nchar(seq)
    pieces <- c(pieces, seq)
    src_scaffold <- c(src_scaffold, rep(s, n))
    src_pos <- c(src_pos, (trim + 1L):(trim + n))
    prev_oriented <- seq
    prev_scaf <- s
    prev_depth <- if (is.null(dep)) NULL else if (trim > 0) dep[-(seq_len(trim))] else dep
  }

  junction_info <- if (length(junc_rec) > 0) do.call(rbind, junc_rec) else
    data.frame(from = character(0), to = character(0), type = character(0),
               gap_bases = integer(0), overlap_len = integer(0),
               identity = numeric(0), stringsAsFactors = FALSE)
  list(seq = paste0(pieces, collapse = ""),
       src_scaffold = src_scaffold, src_pos = src_pos, junctions = junctions,
       junction_info = junction_info)
}

#' Stitch per-scaffold coverage vectors to match a joined sequence
#'
#' @param scaffold_cov named list; each element a list(depth, gc, err) of numeric
#'   vectors in the scaffold's ORIGINAL orientation.
#' @param layout layout data.frame (for `rc` flags).
#' @param joined list from [join_scaffolds()] (`src_scaffold`, `src_pos`).
#' @return list(depth, gc, errors) numeric vectors of length nchar(joined seq);
#'   gap bases are 0.
#' @noRd
stitch_coverage <- function(scaffold_cov, layout, joined) {
  oriented <- lapply(names(scaffold_cov), function(s) {
    cov <- scaffold_cov[[s]]
    if (isTRUE(layout$rc[match(s, layout$scaffold)])) {
      cov <- list(depth = rev(cov$depth), gc = rev(cov$gc), err = rev(cov$err))
    }
    cov
  })
  names(oriented) <- names(scaffold_cov)

  na0 <- function(x) if (length(x) == 0 || is.na(x)) 0 else x
  n <- length(joined$src_scaffold)
  depth <- numeric(n); gc <- numeric(n); err <- numeric(n)
  for (i in seq_len(n)) {
    s <- joined$src_scaffold[i]
    if (is.na(s)) next
    p <- joined$src_pos[i]
    cov <- oriented[[s]]
    depth[i] <- na0(cov$depth[p])
    gc[i] <- na0(cov$gc[p])
    err[i] <- na0(cov$err[p])
  }
  list(depth = depth, gc = gc, errors = err)
}

#' Parse a space-separated numeric coverage string to a vector
#' @noRd
parse_cov_string <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x) || !nzchar(x)) return(numeric(0))
  suppressWarnings(as.numeric(strsplit(trimws(x), "\\s+")[[1]]))
}

#' Build a joined assembly from per-scaffold sequence + coverage
#'
#' Orchestrates map -> layout -> join -> coverage-stitch and composes an edit
#' note describing the layout.
#'
#' @param scaffolds_df data.frame with columns `scaffold, sequence` and the
#'   space-separated coverage strings `depth, gc, errors`.
#' @param ref_seq reference mitogenome sequence.
#' @param gap_len default N-gap length for unknown junctions.
#' @param circular whether the joined molecule is circular.
#' @param minimap2 minimap2 binary path.
#' @return list(seq, depth, gc, errors, layout, note, topology, join_quality)
#'   (`join_quality` from [summarize_join_quality()]).
#' @noRd
build_joined_assembly <- function(scaffolds_df, ref_seq, gap_len = 100L,
                                   circular = FALSE,
                                   minimap2 = getOption("MitoPilot.minimap2", "minimap2")) {
  seqs <- stats::setNames(as.character(scaffolds_df$sequence),
                          as.character(scaffolds_df$scaffold))
  ref_len <- if (length(ref_seq) == 0 || is.na(ref_seq)) 0L else nchar(ref_seq)
  mappings <- map_scaffolds_to_reference(seqs, ref_seq, minimap2)
  layout <- derive_scaffold_layout(mappings, ref_len, circular)
  assemble_from_layout(scaffolds_df, layout, gap_len, circular,
                       ref_seq = ref_seq, minimap2 = minimap2)
}

#' Detect + trim a circular self-overlap on a joined sequence
#'
#' A circular mitogenome assembled (or joined) as a linear sequence often carries
#' the same bases at its 5' start and 3' end (a scaffold that maps to BOTH ends of
#' the linear reference wraps the origin). This aligns the sequence's 3' end to its
#' 5' start; if they overlap at high identity, the molecule is circular and the
#' redundant 3' copy is trimmed (with its coverage) so the sequence represents the
#' circle once.
#'
#' @param seq joined sequence.
#' @param depth,gc,errors per-position coverage vectors (same length as seq).
#' @param min_overlap minimum redundant-end length to call circular.
#' @param min_identity minimum identity of the end overlap.
#' @return list(circular, seq, depth, gc, errors, overlap_len, identity).
#' @noRd
circularize_sequence <- function(seq, depth, gc, errors,
                                 min_overlap = 20L, min_identity = 0.9) {
  n <- nchar(seq)
  out <- list(circular = FALSE, seq = seq, depth = depth, gc = gc,
              errors = errors, overlap_len = 0L, identity = NA_real_)
  if (n < 2L * min_overlap) return(out)
  # Compare the last W bp vs the first W bp (disjoint windows, so the only match
  # is a genuine origin wrap, not the trivial full self-diagonal).
  W <- min(n %/% 2L, 5000L)
  a_end <- substring(seq, n - W + 1L)
  b_start <- substring(seq, 1L, W)
  ov <- refine_overlap(a_end, b_start, est_overlap = W, min_identity = min_identity)
  if (!isTRUE(ov$reliable) || ov$overlap_len < min_overlap) return(out)
  k <- ov$trim_b
  if (k < min_overlap || k >= n) return(out)
  keep <- seq_len(n - k)
  out$circular <- TRUE
  out$seq <- substring(seq, 1L, n - k)
  out$depth <- depth[keep]; out$gc <- gc[keep]; out$errors <- errors[keep]
  out$overlap_len <- k; out$identity <- ov$identity
  out
}

#' Rotate a (circular) sequence so it starts at reference position 1
#'
#' Maps the sequence to the reference and rotates it (with its coverage) so the
#' base aligning to the lowest reference coordinate comes first - a canonical
#' origin for the circular molecule.
#'
#' @param seq,depth,gc,errors sequence + coverage vectors.
#' @param ref_seq reference sequence.
#' @param minimap2 minimap2 binary.
#' @return list(seq, depth, gc, errors, rotated) (unchanged if mapping fails).
#' @noRd
rotate_to_reference <- function(seq, depth, gc, errors, ref_seq,
                                minimap2 = getOption("MitoPilot.minimap2", "minimap2")) {
  out <- list(seq = seq, depth = depth, gc = gc, errors = errors, rotated = 0L)
  n <- nchar(seq)
  if (is.null(ref_seq) || is.na(ref_seq) || !nzchar(ref_seq) || n < 2L) return(out)
  rows <- run_minimap2_paf(c(joined = seq), ref_seq, minimap2)
  rows <- rows[rows$strand == "+" & !is.na(rows$ref_start), , drop = FALSE]
  if (nrow(rows) == 0) return(out)
  best <- rows[which.min(rows$ref_start), , drop = FALSE]
  # Query base aligning to reference position 0 (mod n).
  rp <- ((best$qstart - best$ref_start) %% n + n) %% n
  if (rp == 0L) return(out)
  idx <- c((rp + 1L):n, seq_len(rp))
  out$seq <- paste0(substring(seq, rp + 1L), substring(seq, 1L, rp))
  out$depth <- depth[idx]; out$gc <- gc[idx]; out$errors <- errors[idx]
  out$rotated <- as.integer(rp)
  out
}

#' Join + stitch from an explicit (possibly hand-edited) layout
#'
#' The part of [build_joined_assembly()] after reference mapping: used by the
#' in-app manual override editor with a layout the user reordered/flipped.
#'
#' @param scaffolds_df see [build_joined_assembly()].
#' @param layout layout data.frame (pre-ordered) as from
#'   [derive_scaffold_layout()] or edited by the user.
#' @param gap_len default N-gap length.
#' @param circular whether the joined molecule is circular.
#' @return list(seq, depth, gc, errors, layout, note, topology, join_quality)
#'   (`join_quality` from [summarize_join_quality()]).
#' @noRd
assemble_from_layout <- function(scaffolds_df, layout, gap_len = 100L,
                                 circular = FALSE, ref_seq = NULL,
                                 minimap2 = getOption("MitoPilot.minimap2", "minimap2")) {
  seqs <- stats::setNames(as.character(scaffolds_df$sequence),
                          as.character(scaffolds_df$scaffold))
  cov <- stats::setNames(lapply(seq_len(nrow(scaffolds_df)), function(i) {
    list(depth = parse_cov_string(scaffolds_df$depth[i]),
         gc = parse_cov_string(scaffolds_df$gc[i]),
         err = parse_cov_string(scaffolds_df$errors[i]))
  }), as.character(scaffolds_df$scaffold))
  depth_only <- lapply(cov, `[[`, "depth")
  joined <- join_scaffolds(seqs, layout, as.integer(gap_len), circular,
                           scaffold_depth = depth_only)
  stitched <- stitch_coverage(cov, layout, joined)

  seq <- joined$seq
  depth <- stitched$depth; gc <- stitched$gc; errors <- stitched$errors
  circ_note <- character(0)

  # Circularize: trim a redundant end if the molecule wraps the origin (a
  # scaffold mapping to both ends of the reference), then rotate to the
  # reference origin. Auto-detected; the `circular` arg forces rotation even
  # when no redundant overlap is present.
  cz <- circularize_sequence(seq, depth, gc, errors)
  is_circular <- isTRUE(cz$circular) || isTRUE(circular)
  if (isTRUE(cz$circular)) {
    seq <- cz$seq; depth <- cz$depth; gc <- cz$gc; errors <- cz$errors
    circ_note <- c(circ_note, sprintf(
      "circular: trimmed %d bp redundant end overlap (%.0f%% identity)",
      cz$overlap_len, 100 * cz$identity))
  }
  if (is_circular && !is.null(ref_seq) && !is.na(ref_seq) && nzchar(ref_seq)) {
    rt <- rotate_to_reference(seq, depth, gc, errors, ref_seq, minimap2)
    if (rt$rotated > 0) {
      seq <- rt$seq; depth <- rt$depth; gc <- rt$gc; errors <- rt$errors
      circ_note <- c(circ_note, sprintf("rotated %d bp to reference origin", rt$rotated))
    }
  }

  topology <- if (is_circular) "circular" else "linear"
  join_quality <- summarize_join_quality(joined, layout)
  note <- compose_join_note(layout, seq, gap_len, joined$junctions,
                            gap_bases = join_quality$gap_bases)
  note <- paste(note, format_join_quality(join_quality))
  if (length(circ_note) > 0) note <- paste0(note, " (", paste(circ_note, collapse = "; "), ").")

  list(seq = seq, depth = depth, gc = gc, errors = errors,
       layout = layout, note = note, topology = topology,
       join_quality = join_quality)
}

#' Write the joined Path 0 FASTA + coverageStats CSV
#'
#' Shared by the app's Path 0 writer and the Nextflow join so both emit an
#' identical `{ID}_assembly_0.fasta` / `{ID}_assembly_0_coverageStats.csv` pair
#' that `annotate()` consumes. Coverage layout mirrors [persist_path0()].
#'
#' @param out_dir directory to write into (the sample's assemble opts dir).
#' @param ID sample id.
#' @param seq joined sequence string.
#' @param depth_vec,gc_vec,err_vec per-position coverage vectors.
#' @param topology "linear" or "circular".
#' @noRd
write_joined_files <- function(out_dir, ID, seq, depth_vec, gc_vec, err_vec,
                               topology = "linear") {
  dna <- Biostrings::DNAStringSet(seq)
  names(dna) <- paste(paste(ID, 0, 0, sep = "."), topology)
  Biostrings::writeXStringSet(dna, file.path(out_dir, paste0(ID, "_assembly_0.fasta")))

  depth_i <- round(depth_vec); depth_i[is.na(depth_i)] <- 0
  err_i <- err_vec; err_i[is.na(err_i)] <- 0
  cov_in <- data.frame(
    SeqId     = paste(ID, 0, 0, sep = "."),
    Position  = seq_along(depth_vec),
    Call      = strsplit(seq, "", fixed = TRUE)[[1]],
    Depth     = depth_i,
    Correct   = depth_i,
    ErrorRate = err_i,
    stringsAsFactors = FALSE
  )
  cov <- .coverage_stats_to_output(.coverage_rolling_stats(cov_in))
  readr::write_csv(
    cov, file.path(out_dir, paste0(ID, "_assembly_0_coverageStats.csv")),
    quote = "none", na = ""
  )
  invisible(NULL)
}

#' Build the joined Path 0 row for the `assemblies` table
#'
#' @return one-row data.frame matching the joined-assembly columns; the caller
#'   adds blast columns and writes it.
#' @noRd
joined_assemblies_row <- function(ID, seq, depth_vec, gc_vec, err_vec,
                                  topology = "linear") {
  data.frame(
    ID = ID, path = 0, scaffold = 0, topology = topology,
    length = nchar(seq), length_raw = nchar(seq), sequence = seq,
    depth  = paste(round(depth_vec), collapse = " "),
    gc     = paste(round(gc_vec, 4), collapse = " "),
    errors = paste(err_vec, collapse = " "),
    ignore = 0, edited = 1, time_stamp = as.numeric(Sys.time()),
    stringsAsFactors = FALSE
  )
}

#' Nextflow entry point: build + persist a joined assembly (file outputs only)
#'
#' Reads the per-scaffold assembly FASTA + coverage CSVs + the reference FASTA,
#' joins, and writes the joined `{ID}_assembly_0.fasta`,
#' `{ID}_assembly_0_coverageStats.csv`, and a `{ID}_joined_row.csv` (one row of
#' `assemblies` columns) for the workflow to `sqlInsert`. No DB connection is
#' opened here; DB writes are done in Groovy (see scaffold_join_workflow.nf).
#'
#' @param assembly_fasta path to the multi-scaffold assembly FASTA (headers
#'   `ID.path.scaffold [topology]`).
#' @param coverage_csvs character vector of per-scaffold coverageStats CSV paths.
#' @param ref_fasta path to the reference mitogenome FASTA.
#' @param ID sample id.
#' @param out_dir directory to write outputs into.
#' @param gap_len default N-gap length for unknown junctions.
#' @param circular whether to mark the joined molecule circular.
#' @param db optional DBI connection; unused for file-only outputs (DB writes
#'   are done in Groovy) and kept for call-site compatibility.
#' @param auto_join when TRUE (and the scaffolds' BLAST hits agree) build + write
#'   the joined Path 0. When FALSE (toggle off) or the hits disagree, only the
#'   precomputed scaffold->reference mappings are written, leaving the sample
#'   fragmented for manual review in the app.
#' @param scaffold_hits optional ';'-separated string of per-scaffold BLAST hits
#'   ("scaffold|accession|pident") passed from the workflow. Preferred over a DB
#'   read of `assemblies.blast_accession`, which is written by an async UPDATE
#'   with no happens-before relative to this step (so it can still be NULL here).
#' @export
run_scaffold_join <- function(assembly_fasta, coverage_csvs, ref_fasta, ID,
                              out_dir, gap_len = 100, circular = FALSE, db = NULL,
                              auto_join = TRUE, scaffold_hits = NULL) {
  asm <- Biostrings::readDNAStringSet(assembly_fasta)
  ids <- sub("\\s.*$", "", names(asm))
  scaffolds <- vapply(strsplit(ids, ".", fixed = TRUE),
                      function(p) p[length(p)], character(1))
  seqs <- as.character(asm)

  # Coverage: gather per-scaffold MeanDepth/GC/ErrorRate from the CSVs, keyed by
  # the scaffold field of SeqId (ID.path.scaffold).
  cov_tab <- do.call(rbind, lapply(coverage_csvs, function(f) {
    if (!file.exists(f)) return(NULL)
    utils::read.csv(f, stringsAsFactors = FALSE)
  }))
  cov_scaf <- if (!is.null(cov_tab)) {
    vapply(strsplit(cov_tab$SeqId, ".", fixed = TRUE),
           function(p) p[length(p)], character(1))
  } else character(0)

  cov_string <- function(scaf, col) {
    if (is.null(cov_tab) || !col %in% names(cov_tab)) return("")
    v <- cov_tab[[col]][cov_scaf == scaf]
    # strip any '#' outlier-mask prefix before numeric use
    v <- suppressWarnings(as.numeric(sub("^#", "", as.character(v))))
    paste(v, collapse = " ")
  }

  scaffolds_df <- data.frame(
    scaffold = scaffolds,
    sequence = seqs,
    depth  = vapply(scaffolds, cov_string, character(1), col = "MeanDepth"),
    gc     = vapply(scaffolds, cov_string, character(1), col = "GC"),
    errors = vapply(scaffolds, cov_string, character(1), col = "ErrorRate"),
    stringsAsFactors = FALSE
  )

  # Per-scaffold BLAST hits drive (a) the conflicting-hit check and (b) the set of
  # candidate references to map against. Prefer the workflow-supplied scaffold_hits
  # string (reliable) over a DB read of assemblies.blast_accession (racy).
  named_seqs <- stats::setNames(scaffolds_df$sequence, as.character(scaffolds_df$scaffold))
  ref_fasta_seq <- paste(as.character(Biostrings::readDNAStringSet(ref_fasta)), collapse = "")
  scaf_len <- stats::setNames(nchar(named_seqs), names(named_seqs))
  sc <- parse_scaffold_hits(scaffold_hits, scaf_len)
  if (is.null(sc) && !is.null(db) && nzchar(db) && file.exists(db)) {
    sc <- tryCatch({
      con <- DBI::dbConnect(RSQLite::SQLite(), db, flags = RSQLite::SQLITE_RO)
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      DBI::dbGetQuery(con, sprintf(
        "SELECT scaffold, length, blast_accession, blast_pident FROM assemblies WHERE ID='%s' AND path>0", ID))
    }, error = function(e) NULL)
  }

  # Gather a candidate reference sequence per distinct accession from the cache.
  ref_seqs <- character(0)        # named accession -> sequence (candidate refs)
  accs <- if (!is.null(sc)) unique(sc$blast_accession[!is.na(sc$blast_accession) & nzchar(sc$blast_accession)]) else character(0)
  if (length(accs) && !is.null(db) && nzchar(db) && file.exists(db)) {
    tryCatch({
      con <- DBI::dbConnect(RSQLite::SQLite(), db, flags = RSQLite::SQLITE_RO)
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      for (acc in accs) {
        rs <- DBI::dbGetQuery(con, sprintf(
          "SELECT sequence FROM blast_ref_sequences WHERE accession='%s'", acc))$sequence
        if (length(rs) && !is.na(rs[1]) && nzchar(rs[1])) ref_seqs[[acc]] <- rs[1]
      }
    }, error = function(e) NULL)
  }

  # Reference selection for the auto-build: prefer the length x %identity weighted
  # choice across the scaffolds' BLAST hits (choose_reference) over the per-ID top
  # hit the ref-fetch passes in. Seed the chosen accession's sequence from the
  # fetched ref FASTA if it wasn't in the cache, so the mapping precompute always
  # covers at least the reference the join will use.
  acc <- if (!is.null(sc)) choose_reference(sc) else NA_character_
  # `ref_seqs` is an atomic named vector, so index by name membership (x[["miss"]]
  # errors on atomics, unlike lists).
  if (!is.na(acc) && !(acc %in% names(ref_seqs)) && nzchar(ref_fasta_seq)) {
    ref_seqs[[acc]] <- ref_fasta_seq
  }
  ref_seq <- if (!is.na(acc) && acc %in% names(ref_seqs)) ref_seqs[[acc]] else ref_fasta_seq

  # Precompute scaffold->reference mappings for ALL candidate references so the
  # in-app manual editor can build layouts with no minimap2 dependency.
  mappings <- compute_scaffold_mappings(named_seqs, ref_seqs)
  mappings$mapped <- as.integer(mappings$mapped)   # INTEGER column (0/1)
  if (nrow(mappings) > 0) mappings <- cbind(ID = ID, mappings, stringsAsFactors = FALSE)
  readr::write_csv(mappings, file.path(out_dir, paste0(ID, "_scaffold_mappings.csv")),
                   quote = "none", na = "")

  # Gate the automatic Path 0: only when enabled AND the scaffolds' BLAST hits
  # agree. Otherwise emit mappings only and leave the sample fragmented.
  if (!isTRUE(auto_join) || (!is.null(sc) && scaffold_hits_disagree(sc))) {
    return(invisible(list(skipped = TRUE, mappings = mappings)))
  }

  res <- build_joined_assembly(scaffolds_df, ref_seq, gap_len = gap_len,
                               circular = circular)

  write_joined_files(out_dir, ID, res$seq, res$depth, res$gc, res$errors,
                     res$topology)
  row <- joined_assemblies_row(ID, res$seq, res$depth, res$gc, res$errors,
                               res$topology)
  # Unquoted so the workflow's splitCsv(header:true) reads clean column names
  # (write.csv would quote the header -> '"ID"' and break the sqlInsert keys).
  readr::write_csv(row, file.path(out_dir, paste0(ID, "_joined_row.csv")),
                   quote = "none", na = "")
  invisible(res)
}

#' Plot scaffold placement against the reference mitogenome
#'
#' One horizontal track per INCLUDED scaffold positioned by its mapped reference
#' coordinates; arrow direction = strand (blue forward, red reverse-complement).
#' Excluded (poor/no mapping) scaffolds are listed under the title.
#'
#' @param layout layout data.frame from [derive_scaffold_layout()] (carries
#'   `ref_start`, `ref_end`, `rc`, `include`, `qcov`).
#' @param ref_len reference length (bp).
#' @param scaffold_len optional named numeric of scaffold lengths (for labels).
#' @noRd
plot_scaffold_mapping <- function(layout, ref_len, scaffold_len = NULL) {
  inc <- if ("include" %in% names(layout)) layout$include else !is.na(layout$ref_start)
  shown <- layout[inc & !is.na(layout$ref_start), , drop = FALSE]
  excluded <- layout[!inc, , drop = FALSE]
  ref_len <- max(ref_len, 1, shown$ref_end, na.rm = TRUE)

  # Extra top margin reserves a line for the excluded-scaffold note so it never
  # collides with the x-axis title at the bottom.
  top_mar <- if (nrow(excluded) > 0) 4 else 2.5
  op <- graphics::par(mar = c(4.5, 1, top_mar, 1))
  on.exit(graphics::par(op), add = TRUE)
  n <- max(nrow(shown), 1)
  graphics::plot(NA, xlim = c(0, ref_len), ylim = c(0.3, n + 0.7),
                 xlab = "Reference position (bp)", ylab = "", yaxt = "n",
                 main = "Scaffold mapping to reference")
  graphics::rect(0, 0.35, ref_len, 0.55, col = "grey85", border = NA)

  if (nrow(shown) > 0) for (i in seq_len(nrow(shown))) {
    y <- nrow(shown) - i + 1            # first (leftmost) scaffold on top
    rc <- isTRUE(shown$rc[i])
    col <- if (rc) "#d9534f" else "#4a90d9"
    x0 <- shown$ref_start[i]; x1 <- shown$ref_end[i]
    graphics::rect(x0, y - 0.25, x1, y + 0.25, col = col, border = "black")
    # orientation arrow along the block
    ax <- if (rc) c(x1, x0) else c(x0, x1)
    graphics::arrows(ax[1], y, ax[2], y, length = 0.08, col = "white", lwd = 2)
    lab <- paste0("scaffold ", shown$scaffold[i], if (rc) " (rc)" else "")
    if (!is.null(scaffold_len)) {
      sl <- scaffold_len[[as.character(shown$scaffold[i])]]
      if (!is.null(sl) && !is.na(sl)) lab <- paste0(lab, " - ", sl, " bp")
    }
    graphics::text(mean(c(x0, x1)), y + 0.38, labels = lab, cex = 0.8)
  }
  if (nrow(excluded) > 0) {
    # Place under the title (top), away from the x-axis labels.
    graphics::mtext(paste("Excluded from Path 0 (poor/no mapping):",
                          paste(excluded$scaffold, collapse = ", ")),
                    side = 3, line = 0.3, col = "#d9534f", cex = 0.85)
  }
  invisible(NULL)
}

#' Base-level alignment of one scaffold to its reference region
#'
#' Aligns the oriented scaffold to the reference subregion it maps to (padded),
#' for the base-pair zoom view. Returns the gapped aligned strings plus the
#' reference coordinate of the first aligned reference base.
#'
#' @param scaf_oriented scaffold sequence (already RC-applied if needed).
#' @param ref_seq full reference sequence.
#' @param ref_start,ref_end reference span (from minimap2) the scaffold maps to.
#' @param pad bp of reference context to include each side.
#' @return list(aln_scaf, aln_ref, ref_offset) or NULL on failure.
#' @noRd
align_scaffold_to_ref <- function(scaf_oriented, ref_seq, ref_start, ref_end, pad = 50L) {
  if (is.na(ref_start) || is.na(ref_end)) return(NULL)
  rs <- max(1L, as.integer(ref_start) - pad)
  re <- min(nchar(ref_seq), as.integer(ref_end) + pad)
  if (re <= rs) return(NULL)
  ref_sub <- substring(ref_seq, rs, re)
  mx <- pwalign::nucleotideSubstitutionMatrix(match = 1, mismatch = -1)
  aln <- tryCatch(
    pwalign::pairwiseAlignment(
      pattern = Biostrings::DNAString(scaf_oriented),
      subject = Biostrings::DNAString(ref_sub),
      substitutionMatrix = mx, gapOpening = 5, gapExtension = 2,
      type = "global-local"),
    error = function(e) NULL)
  if (is.null(aln)) return(NULL)
  list(
    aln_scaf = as.character(pwalign::alignedPattern(aln)),
    aln_ref  = as.character(pwalign::alignedSubject(aln)),
    ref_offset = rs + BiocGenerics::start(pwalign::subject(aln)@range) - 1L
  )
}

#' Map a pairwise alignment to per-reference-position scaffold bases
#'
#' @param aln_scaf,aln_ref gapped aligned strings (same length).
#' @param ref_offset reference coordinate of the first aligned reference base.
#' @return named character vector: names = reference positions, values = the
#'   scaffold base aligned there ("-" for a deletion). Insertions (scaffold bases
#'   between reference positions) are omitted.
#' @noRd
build_ref_base_map <- function(aln_scaf, aln_ref, ref_offset) {
  sc <- strsplit(aln_scaf, "", fixed = TRUE)[[1]]
  rf <- strsplit(aln_ref, "", fixed = TRUE)[[1]]
  consumes <- rf != "-"
  base_at_ref <- sc[consumes]
  stats::setNames(base_at_ref, ref_offset + seq_len(sum(consumes)) - 1L)
}

#' Per-base colour for a DNA character vector
#' @noRd
dna_base_color <- function(b) {
  u <- toupper(b)
  out <- rep("#666666", length(b))
  out[u == "A"] <- "#D9342B"; out[u == "C"] <- "#3878C5"
  out[u == "G"] <- "#E6A500"; out[u == "T"] <- "#2D9E3F"
  out[b == "-"] <- "#BBBBBB"
  out
}

#' Light per-base background tint (cell fill behind a base letter)
#' @noRd
dna_base_bg <- function(b) {
  u <- toupper(b)
  out <- rep("#EEEEEE", length(b))
  out[u == "A"] <- "#F8D5D2"; out[u == "C"] <- "#D3E0F2"
  out[u == "G"] <- "#FBEBC4"; out[u == "T"] <- "#CFE9D5"
  out[b == "-"] <- "#E4E4E4"
  out
}

#' Base-pair zoom view of scaffolds aligned to a reference window
#'
#' One row per included scaffold plus a reference row; each column is a reference
#' position in `[win_start, win_end]`. Every covered base sits in a cell tinted by
#' nucleotide with the (dark) base letter on top; mismatches to the reference get
#' a red cell outline, deletions show "-", and positions the scaffold does not
#' cover are left blank. Designed to render inside a horizontally scrollable
#' container (width ~ window x px_per_col).
#'
#' @param ref_seq reference sequence.
#' @param base_maps named list (by scaffold id) of [build_ref_base_map()] output.
#' @param scaffold_ids included scaffold ids, in layout order.
#' @param win_start,win_end reference window (bp).
#' @noRd
plot_scaffold_zoom <- function(ref_seq, base_maps, scaffold_ids, win_start, win_end) {
  win_start <- max(1L, as.integer(win_start))
  win_end <- min(nchar(ref_seq), as.integer(win_end))
  pos <- win_start:win_end
  W <- length(pos)
  ref_chars <- strsplit(substring(ref_seq, win_start, win_end), "", fixed = TRUE)[[1]]
  ntr <- length(scaffold_ids)
  rows <- ntr + 1L
  x <- seq_len(W)
  hh <- 0.46                          # cell half-height
  cell <- function(xi, y, fill, border = NA, lwd = 1)
    graphics::rect(xi - 0.5, y - hh, xi + 0.5, y + hh, col = fill,
                   border = border, lwd = lwd)

  op <- graphics::par(mar = c(2.4, 6, 1.2, 1))
  on.exit(graphics::par(op), add = TRUE)
  graphics::plot(NA, xlim = c(0.5, W + 0.5), ylim = c(0.5, rows + 0.5),
                 xlab = "", ylab = "", axes = FALSE)

  # Reference row: tinted cells + dark letters.
  yref <- rows
  cell(x, yref, dna_base_bg(ref_chars))
  graphics::text(x, yref, ref_chars, col = "#222222", cex = 1.15,
                 font = 2, family = "mono")
  graphics::mtext("reference", side = 2, at = yref, las = 1, line = 0.4, cex = 0.8)

  for (j in seq_len(ntr)) {
    s <- as.character(scaffold_ids[j])
    y <- rows - j
    bm <- base_maps[[s]]
    sc <- if (is.null(bm)) rep(NA_character_, W) else unname(bm[as.character(pos)])
    cover <- !is.na(sc)
    if (any(cover)) {
      idx <- which(cover)
      cell(idx, y, dna_base_bg(sc[idx]))
      # Outline mismatches (covered, not a deletion, differs from reference).
      mm <- cover & sc != "-" & toupper(sc) != toupper(ref_chars)
      if (any(mm)) cell(which(mm), y, NA, border = "#C0392B", lwd = 1.6)
      graphics::text(idx, y, sc[idx], col = "#222222", cex = 1.15,
                     font = 2, family = "mono")
    }
    graphics::mtext(paste("scaffold", s), side = 2, at = y, las = 1,
                    line = 0.4, cex = 0.8)
  }

  # Clean x axis: ~8-12 evenly spaced labels at real reference coordinates, short
  # ticks, no clutter.
  step <- max(1L, as.integer(ceiling(W / 10) ))
  at <- seq(1L, W, by = step)
  graphics::axis(1, at = at, labels = pos[at], cex.axis = 0.75,
                 tcl = -0.25, mgp = c(1, 0.25, 0), lwd = 0, lwd.ticks = 1)
  invisible(NULL)
}

#' Summarise the continuity + overlap quality of a scaffold join
#'
#' A compact "report value" for the user: is the joined Path 0 gappy (N-spacers
#' between disjoint scaffolds) or continuous (scaffolds abut or overlap), and for
#' the continuous parts, how good the confirmed overlaps are (alignment identity).
#'
#' @param joined list from [join_scaffolds()] (needs `src_scaffold` and
#'   `junction_info`).
#' @param layout layout data.frame (for the included-scaffold count).
#' @return list:
#'   `continuity` - "single" (one scaffold, no joins), "continuous" (no N-gap
#'     junctions), or "gappy" (>= 1 N-gap junction);
#'   `n_scaffolds`, `n_junctions`;
#'   `n_gap_junctions`, `n_butt_junctions`, `n_overlap_junctions`,
#'     `n_unconfirmed_overlaps` (junction-type counts);
#'   `gap_bases` (inserted N-spacer bases), `gap_fraction` (of joined length);
#'   `min_overlap_identity`, `mean_overlap_identity` (over confirmed overlaps; NA
#'     when none).
#' @noRd
summarize_join_quality <- function(joined, layout) {
  inc <- if ("include" %in% names(layout)) layout[layout$include, , drop = FALSE] else layout
  ji <- joined$junction_info
  if (is.null(ji)) ji <- data.frame(type = character(0), identity = numeric(0),
                                    stringsAsFactors = FALSE)
  n_inc <- nrow(inc)
  seq_n <- length(joined$src_scaffold)
  gap_bases <- sum(is.na(joined$src_scaffold))
  n_gap_j   <- sum(ji$type == "gap")
  n_butt    <- sum(ji$type == "butt")
  n_ov_conf <- sum(ji$type == "overlap_confirmed")
  n_ov_unc  <- sum(ji$type == "overlap_unconfirmed")
  ov_id <- ji$identity[ji$type == "overlap_confirmed" & !is.na(ji$identity)]
  continuity <- if (n_inc <= 1) "single" else if (n_gap_j > 0) "gappy" else "continuous"
  list(
    continuity = continuity,
    n_scaffolds = n_inc,
    n_junctions = max(n_inc - 1L, 0L),
    n_gap_junctions = n_gap_j,
    n_butt_junctions = n_butt,
    n_overlap_junctions = n_ov_conf,
    n_unconfirmed_overlaps = n_ov_unc,
    gap_bases = as.integer(gap_bases),
    gap_fraction = if (seq_n > 0) gap_bases / seq_n else 0,
    min_overlap_identity = if (length(ov_id)) min(ov_id) else NA_real_,
    mean_overlap_identity = if (length(ov_id)) mean(ov_id) else NA_real_
  )
}

#' One-line user-facing summary string from [summarize_join_quality()]
#' @noRd
format_join_quality <- function(q) {
  if (q$continuity == "single") return("Single scaffold; no junctions.")
  base <- if (q$continuity == "continuous") {
    sprintf("Continuous join: %d junction(s), no N gaps", q$n_junctions)
  } else {
    sprintf("Gappy join: %d of %d junction(s) are N gaps (%d N bases, %.1f%% of length)",
            q$n_gap_junctions, q$n_junctions, q$gap_bases, 100 * q$gap_fraction)
  }
  if (q$n_overlap_junctions > 0 && !is.na(q$min_overlap_identity)) {
    base <- sprintf("%s; %d confirmed overlap(s), identity min %.0f%% / mean %.0f%%",
                    base, q$n_overlap_junctions,
                    100 * q$min_overlap_identity, 100 * q$mean_overlap_identity)
  }
  if (q$n_unconfirmed_overlaps > 0) {
    base <- sprintf("%s; %d unconfirmed overlap(s) (review)", base, q$n_unconfirmed_overlaps)
  }
  paste0(base, ".")
}

#' Human-readable note describing a join layout
#'
#' @param gap_bases number of inserted junction N-spacer bases (NOT the raw N
#'   count of `seq`, which also includes assembly gaps internal to scaffolds).
#'   Supplied by the caller from `sum(is.na(joined$src_scaffold))`.
#' @noRd
compose_join_note <- function(layout, seq, gap_len, junctions = character(0),
                              gap_bases = NULL) {
  inc <- if ("include" %in% names(layout)) layout[layout$include, , drop = FALSE] else layout
  exc <- if ("include" %in% names(layout)) layout[!layout$include, , drop = FALSE] else layout[0, ]
  order_str <- paste(
    vapply(seq_len(nrow(inc)), function(i) {
      paste0(inc$scaffold[i], if (isTRUE(inc$rc[i])) "(rc)" else "")
    }, character(1)),
    collapse = " -> "
  )
  n_gaps <- if (!is.null(gap_bases)) as.integer(gap_bases) else
    sum(strsplit(seq, "", fixed = TRUE)[[1]] == "N")
  note <- sprintf("Joined %d scaffolds: %s; %d N gap bases.",
                  nrow(inc), order_str, n_gaps)
  if (nrow(exc) > 0) {
    note <- paste0(note, sprintf(" Excluded (poor/no reference mapping): %s.",
                                 paste(exc$scaffold, collapse = ", ")))
  }
  if (length(junctions) > 0) {
    note <- paste0(note, " Junctions: ", paste(junctions, collapse = "; "), ".")
  }
  note
}
