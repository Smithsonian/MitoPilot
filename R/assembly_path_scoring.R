#' Multi-path assembly scoring, conflict classification, and resolution helpers
#'
#' Pure functions (no Shiny dependencies) used by the assembly coverage modal to
#' (1) score and rank alternative GetOrganelle/MitoFinder assembly paths,
#' (2) classify conflict blocks between aligned paths by likely cause, and
#' (3) build a resolved consensus sequence from per-block user decisions.
#'
#' All functions are defensive about missing data (NA depths, absent BLAST,
#' empty coverage stats) so the modal never errors on incomplete inputs.
#'
#' @name assembly_path_scoring
#' @noRd
NULL

# ---- Scoring -----------------------------------------------------------------

#' Relative weights for the per-path score sub-components.
#'
#' Exposed as a named constant so the ranking is transparent and easy to tune.
#' Sub-scores are each normalized to [0, 1] (1 = best) before weighting.
#' @noRd
ASSEMBLY_PATH_SCORE_WEIGHTS <- c(
  topology   = 2.0,  # circular preferred over linear
  scaffolds  = 1.5,  # single contig preferred over fragmented
  length     = 1.5,  # close to expected / consensus length
  depth_even = 1.5,  # even coverage (low CV)
  depth_mean = 1.0,  # adequate mean depth
  error      = 1.0,  # low error rate
  blast      = 1.0,  # high BLAST identity / coverage
  blast_conc = 1.5,  # agrees taxonomically with sibling paths
  ambiguity  = 0.5   # few N / IUPAC bases
)

#' Numeric thresholds used by scoring and classification.
#' @noRd
ASSEMBLY_PATH_THRESHOLDS <- list(
  low_depth      = 10,    # below this mean/min depth is "low"
  good_depth     = 50,    # at/above this mean depth scores full marks
  high_error     = 0.02,  # error rate considered elevated
  large_block    = 50L,   # conflict block length considered "large"
  length_tol_pct = 0.02   # length within +/- 2% of expected = full marks
)

#' Coefficient of variation, NA-safe.
#' @noRd
.cv <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 2L) return(NA_real_)
  m <- mean(x)
  if (m == 0) return(NA_real_)
  stats::sd(x) / m
}

#' Count non-ACGT/non-gap characters (N and IUPAC ambiguity codes) in a sequence.
#' @noRd
count_ambiguities <- function(seq) {
  if (is.null(seq) || is.na(seq) || !nzchar(seq)) return(0L)
  chars <- strsplit(toupper(seq), "")[[1]]
  sum(!chars %in% c("A", "C", "G", "T", "-"))
}

#' Score and rank alternative assembly paths.
#'
#' @param paths_df data.frame, one row per path/scaffold, with at least columns
#'   `path`, `scaffold`, `topology`, `length`, `sequence`, and optionally
#'   `blast_species`, `blast_lineage`, `blast_pident`, `blast_qcovs`.
#' @param cov_by_path optional named list keyed by path number; each element a
#'   data.frame with at least a `Depth` column (and optionally `ErrorRate`).
#' @param expected_len optional expected genome length (bp). If NULL, the median
#'   path length is used as the reference.
#' @return data.frame, one row per path, with `score`, `rank`, the normalized
#'   sub-scores (prefixed `s_`), and a character `flags` column (human readable,
#'   `;`-separated). Higher score = better. `rank` 1 = suggested path.
#' @noRd
score_assembly_paths <- function(paths_df, cov_by_path = NULL, expected_len = NULL) {
  th <- ASSEMBLY_PATH_THRESHOLDS
  if (is.null(paths_df) || nrow(paths_df) == 0L) {
    return(data.frame())
  }
  paths <- sort(unique(paths_df$path))

  # Per-path aggregation -------------------------------------------------------
  per_path <- purrr::map_dfr(paths, function(p) {
    rows <- paths_df[paths_df$path == p, , drop = FALSE]
    topo <- if (any(tolower(rows$topology) == "circular", na.rm = TRUE)) {
      "circular"
    } else {
      rows$topology[1]
    }
    cov <- if (!is.null(cov_by_path)) cov_by_path[[as.character(p)]] else NULL
    depth <- if (!is.null(cov) && "Depth" %in% names(cov)) {
      suppressWarnings(as.numeric(cov$Depth))
    } else NA_real_
    err <- if (!is.null(cov) && "ErrorRate" %in% names(cov)) {
      suppressWarnings(as.numeric(sub("^#", "", as.character(cov$ErrorRate))))
    } else NA_real_
    data.frame(
      path        = p,
      topology    = topo,
      n_scaffolds = nrow(rows),
      length      = sum(rows$length, na.rm = TRUE),
      mean_depth  = if (all(is.na(depth))) NA_real_ else mean(depth, na.rm = TRUE),
      min_depth   = if (all(is.na(depth))) NA_real_ else min(depth, na.rm = TRUE),
      depth_cv    = .cv(depth),
      mean_error  = if (all(is.na(err))) NA_real_ else mean(err, na.rm = TRUE),
      blast_species = .first_chr(rows[["blast_species"]]),
      blast_lineage = .first_chr(rows[["blast_lineage"]]),
      # Needed to tell "BLASTed, matched nothing" ('NO HIT') from "never
      # BLASTed" (NA). Absent when the caller does not supply the column, in
      # which case every path scores exactly as before.
      blast_accession = .first_chr(rows[["blast_accession"]]),
      blast_pident  = .max_num(rows[["blast_pident"]]),
      blast_qcovs   = .max_num(rows[["blast_qcovs"]]),
      n_ambig       = sum(vapply(rows$sequence, count_ambiguities, integer(1))),
      stringsAsFactors = FALSE
    )
  })

  ref_len <- expected_len %||% stats::median(per_path$length, na.rm = TRUE)

  # Normalized sub-scores (1 = best) ------------------------------------------
  per_path$s_topology  <- ifelse(tolower(per_path$topology) == "circular", 1, 0.4)
  per_path$s_scaffolds <- 1 / per_path$n_scaffolds
  per_path$s_length    <- .score_length(per_path$length, ref_len, th$length_tol_pct)
  per_path$s_depth_even <- .score_inverse(per_path$depth_cv, lo = 0, hi = 0.5)
  per_path$s_depth_mean <- .score_linear(per_path$mean_depth, lo = th$low_depth, hi = th$good_depth)
  per_path$s_error      <- .score_inverse(per_path$mean_error, lo = 0, hi = th$high_error)
  per_path$s_blast      <- .score_blast(per_path$blast_pident, per_path$blast_qcovs)
  per_path$s_blast_conc <- .score_concordance(per_path$blast_species, per_path$blast_lineage)
  # A path that was searched and matched nothing is evidence, not absence of it.
  # Left as NA, both sub-scores drop out of the weighted mean, whose denominator
  # renormalizes over whatever is present, so a no-hit path is not penalized at
  # all and can outrank a path with a genuine but distant hit. Since the search
  # space became mitogenome-only, "no hit" also became the signature of a
  # non-target contaminant, so it must score worst, not opt out.
  searched_no_hit <- !is.na(per_path$blast_accession) &
    toupper(trimws(per_path$blast_accession)) == "NO HIT"
  per_path$s_blast[searched_no_hit]      <- 0
  per_path$s_blast_conc[searched_no_hit] <- 0
  per_path$s_ambiguity  <- .score_inverse(per_path$n_ambig, lo = 0, hi = 20)

  w <- ASSEMBLY_PATH_SCORE_WEIGHTS
  sub <- data.frame(
    topology = per_path$s_topology, scaffolds = per_path$s_scaffolds,
    length = per_path$s_length, depth_even = per_path$s_depth_even,
    depth_mean = per_path$s_depth_mean, error = per_path$s_error,
    blast = per_path$s_blast, blast_conc = per_path$s_blast_conc,
    ambiguity = per_path$s_ambiguity
  )
  # Weighted mean over the sub-scores that are available (non-NA) for each path.
  per_path$score <- vapply(seq_len(nrow(sub)), function(i) {
    vals <- as.numeric(sub[i, names(w)])
    ok <- is.finite(vals)
    if (!any(ok)) return(NA_real_)
    round(100 * sum(vals[ok] * w[ok]) / sum(w[ok]), 1)
  }, numeric(1))

  per_path$rank <- rank(-per_path$score, ties.method = "min", na.last = "keep")
  per_path$flags <- vapply(seq_len(nrow(per_path)),
                           function(i) .path_flags(per_path[i, ], ref_len, th),
                           character(1))
  per_path[order(per_path$rank), , drop = FALSE]
}

#' Build the human-readable flag string for one scored path.
#' @noRd
.path_flags <- function(r, ref_len, th) {
  f <- character(0)
  if (!identical(tolower(r$topology), "circular")) f <- c(f, "not circular")
  if (isTRUE(r$n_scaffolds > 1)) f <- c(f, sprintf("%d scaffolds (fragmented)", r$n_scaffolds))
  if (is.finite(r$mean_depth) && r$mean_depth < th$low_depth) {
    f <- c(f, sprintf("low mean depth (%.0fx)", r$mean_depth))
  }
  if (is.finite(r$depth_cv) && r$depth_cv > 0.5) f <- c(f, "uneven coverage")
  if (is.finite(r$mean_error) && r$mean_error > th$high_error) {
    f <- c(f, sprintf("elevated error rate (%.1f%%)", 100 * r$mean_error))
  }
  if (is.finite(r$length) && is.finite(ref_len) && ref_len > 0) {
    diff <- r$length - ref_len
    if (abs(diff) / ref_len > 0.05) {
      f <- c(f, sprintf("length %+0.0f bp vs other paths", diff))
    }
  }
  if (is.finite(r$s_blast_conc) && r$s_blast_conc < 0.5) {
    f <- c(f, "BLAST hit differs from sibling paths (possible NUMT/contaminant)")
  }
  if (isTRUE(r$n_ambig > 0)) f <- c(f, sprintf("%d ambiguous bases", r$n_ambig))
  if (length(f) == 0) "" else paste(f, collapse = "; ")
}

# ---- Scoring sub-helpers -----------------------------------------------------

.first_chr <- function(x) if (is.null(x)) NA_character_ else as.character(x[1])
.max_num <- function(x) {
  if (is.null(x)) return(NA_real_)
  x <- suppressWarnings(as.numeric(x))
  if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
}

#' Linear 0->1 ramp between lo and hi (clamped).
#' @noRd
.score_linear <- function(x, lo, hi) {
  if (length(x) == 0) return(numeric(0))
  pmin(1, pmax(0, (x - lo) / (hi - lo)))
}

#' Inverse ramp: 1 at lo, 0 at hi (clamped). For metrics where lower is better.
#' @noRd
.score_inverse <- function(x, lo, hi) {
  if (length(x) == 0) return(numeric(0))
  pmin(1, pmax(0, 1 - (x - lo) / (hi - lo)))
}

#' Length closeness score: full marks within tol of ref, decaying to 0 at 25% off.
#' @noRd
.score_length <- function(len, ref, tol) {
  if (!is.finite(ref) || ref <= 0) return(rep(1, length(len)))
  rel <- abs(len - ref) / ref
  pmin(1, pmax(0, 1 - pmax(0, rel - tol) / (0.25 - tol)))
}

#' BLAST identity/coverage score (mean of pident and qcovs scaled to [0,1]).
#' @noRd
.score_blast <- function(pident, qcovs) {
  pi <- .score_linear(pident, lo = 80, hi = 100)
  qc <- .score_linear(qcovs, lo = 80, hi = 100)
  out <- rowMeans(cbind(pi, qc), na.rm = TRUE)
  out[is.nan(out)] <- NA_real_
  out
}

#' Cross-path BLAST concordance: 1 if a path's species+lineage agree with the
#' majority of sibling paths, lower if it is the odd one out.
#' @noRd
.score_concordance <- function(species, lineage) {
  n <- length(species)
  if (n <= 1L) return(rep(NA_real_, n))
  # Compare lineage first (broader, more robust), fall back to species string.
  # All-or-nothing, NOT per element: .top_taxon reduces a lineage to its last
  # token (family) but a species string to its first word (genus), so a mixed
  # key set compares family against genus and never matches. One path whose
  # taxonomy fetch failed would then look like the odd one out and get flagged
  # as a possible contaminant.
  has_lineage <- !is.na(lineage) & nzchar(lineage)
  key <- if (all(has_lineage)) lineage else species
  if (all(is.na(key))) return(rep(NA_real_, n))
  tops <- .top_taxon(key)
  vapply(seq_len(n), function(i) {
    if (is.na(key[i])) return(NA_real_)
    known <- tops[!is.na(key)]
    if (length(known) == 0) return(NA_real_)
    # Fraction of all paths (incl. self) sharing this path's taxon. A 2-way
    # tie gives 0.5 (ambiguous, not flagged); a true minority in 3+ paths < 0.5.
    mean(known == tops[i])
  }, numeric(1))
}

#' Extract a coarse taxon token (genus or highest informative rank) for comparison.
#' @noRd
.top_taxon <- function(x) {
  x <- as.character(x)
  # species strings: "Genus species ..." -> "Genus"; lineage: take last token.
  toupper(vapply(x, function(s) {
    s <- trimws(s)
    if (grepl(";", s)) {
      toks <- trimws(strsplit(s, ";")[[1]])
      toks <- toks[nzchar(toks)]
      if (length(toks)) tail(toks, 1) else s
    } else {
      strsplit(s, "\\s+")[[1]][1]
    }
  }, character(1)))
}

# ---- Conflict block classification ------------------------------------------

#' Classify a single conflict block by its most likely cause.
#'
#' @param block one-row data.frame/list with `len`, `n_snps`, `n_indels`, and
#'   optionally `min_depth`, `mean_depth`, `max_error`.
#' @param at_junction logical; TRUE if the block sits at the circular-genome
#'   linear-representation junction (first/last alignment columns).
#' @param blast_divergent logical; TRUE if the paths involved disagree
#'   taxonomically (suggests one is a NUMT/contaminant).
#' @return list(cause, label, advice, tools) where `tools` is a character vector
#'   of recommended resolution-tool keys ("splice","iupac","majority","nmask").
#' @noRd
classify_conflict_block <- function(block, at_junction = FALSE,
                                    blast_divergent = FALSE) {
  th <- ASSEMBLY_PATH_THRESHOLDS
  len      <- block$len %||% 0L
  n_snps   <- block$n_snps %||% 0L
  n_indels <- block$n_indels %||% 0L
  min_d    <- block$min_depth %||% NA_real_
  max_e    <- block$max_error %||% NA_real_

  low_depth <- is.finite(min_d) && min_d < th$low_depth

  if (isTRUE(blast_divergent) && len >= th$large_block) {
    return(list(
      cause = "numt",
      label = "Possible NUMT / contamination",
      advice = paste(
        "A large divergent region where paths disagree taxonomically.",
        "Favor the path matching the expected taxon with even depth, and",
        "ignore the divergent path. Inspect BLAST hits before deciding."
      ),
      tools = c("splice")
    ))
  }
  if (isTRUE(at_junction)) {
    return(list(
      cause = "circularization",
      label = "Circularization artifact",
      advice = paste(
        "Differences clustered at the circular junction are usually cosmetic",
        "(where the circle was cut into a linear sequence). Trimming to the",
        "consensus or splicing either path is generally safe."
      ),
      tools = c("splice", "majority")
    ))
  }
  if (n_indels == 0L && n_snps >= 1L) {
    return(list(
      cause = "heteroplasmy",
      label = if (low_depth) "SNP difference (low depth)" else "Likely heteroplasmy / SNP",
      advice = paste(
        "Substitution-only difference with no length change.",
        if (low_depth) "Depth is low, so evidence is weak; lean on the reference-consistent base."
        else "If both alleles have real read support this may be true heteroplasmy.",
        "Use the IUPAC option to record both alleles, or splice/majority to pick one."
      ),
      tools = if (low_depth) c("majority", "splice") else c("iupac", "majority", "splice")
    ))
  }
  if (n_indels > 0L) {
    return(list(
      cause = "repeat_indel",
      label = "Repeat / homopolymer length ambiguity",
      advice = paste(
        "An insertion/deletion difference, typically in a repeat or homopolymer.",
        "Length is uncertain; prefer the path whose length matches close relatives",
        "(reference). Splice that path's bases, or N-mask if truly unresolvable."
      ),
      tools = c("splice", "majority", "nmask")
    ))
  }
  if (low_depth) {
    return(list(
      cause = "low_confidence",
      label = "Low-confidence region",
      advice = paste(
        "Coverage is low across paths here, so the evidence is weak either way.",
        "Consider N-masking, or splice the reference-consistent path."
      ),
      tools = c("nmask", "splice")
    ))
  }
  list(
    cause = "other",
    label = "Conflict",
    advice = "Mixed difference. Inspect depth, error rate, and read support, then splice or mask.",
    tools = c("splice", "majority", "nmask")
  )
}

# ---- Resolution builders -----------------------------------------------------

#' Map a set of bases to an IUPAC ambiguity code.
#'
#' @param bases character vector of bases (A/C/G/T, case-insensitive). Gaps and
#'   non-ACGT are dropped; if nothing valid remains, returns "N".
#' @return single IUPAC character.
#' @noRd
iupac_code <- function(bases) {
  b <- sort(unique(toupper(bases[bases %in% c("a", "c", "g", "t", "A", "C", "G", "T")])))
  key <- paste(b, collapse = "")
  if (!nzchar(key)) return("N")
  codes <- c(
    A = "A", C = "C", G = "G", T = "T",
    AC = "M", AG = "R", AT = "W", CG = "S", CT = "Y", GT = "K",
    ACG = "V", ACT = "H", AGT = "D", CGT = "B", ACGT = "N"
  )
  out <- codes[[key]]
  if (is.null(out)) "N" else out
}

#' Build a resolved consensus sequence from per-block decisions.
#'
#' Walks the alignment column by column. Non-conflict columns take the chosen
#' base path's base. Within each conflict block the per-block decision controls
#' the output. Gap characters ("-") are dropped from the final sequence.
#'
#' @param aln_mat character matrix, rows = paths, cols = alignment columns
#'   (e.g. `as.matrix(DECIPHER::AlignSeqs(...))`). `rownames` are path labels.
#' @param blocks data.frame of conflict blocks with `aln_start`, `aln_end`
#'   (alignment-column coordinates). May be empty.
#' @param decisions list, length `nrow(blocks)`; each element a list with `mode`
#'   in c("path","iupac","majority","nmask","base") and, for "path", `row` = the
#'   integer row index of the chosen path.
#' @param base_row integer row index used for non-conflict columns (the "base"
#'   path the consensus is built on).
#' @param support optional list keyed by rowname; per-column numeric read support
#'   used to break ties for "majority" mode (e.g. aligned `Correct` counts).
#' @return list(seq = character scalar, map = data.frame(aln_col, src_row, base))
#'   where `map` has one row per emitted (non-gap) base; `src_row` is the source
#'   alignment row, or NA for synthesized (IUPAC/N) bases.
#' @noRd
build_resolved_sequence <- function(aln_mat, blocks, decisions, base_row = 1L,
                                    support = NULL) {
  n_col <- ncol(aln_mat)
  # Per-column resolution mode + source row.
  col_mode <- rep("base", n_col)
  col_dec  <- vector("list", n_col)
  if (!is.null(blocks) && nrow(blocks) > 0L) {
    for (i in seq_len(nrow(blocks))) {
      cols <- seq.int(blocks$aln_start[i], blocks$aln_end[i])
      d <- decisions[[i]] %||% list(mode = "base")
      col_mode[cols] <- d$mode %||% "base"
      for (cc in cols) col_dec[[cc]] <- d
    }
  }

  bases <- character(0)
  src   <- integer(0)
  cols  <- integer(0)
  for (c in seq_len(n_col)) {
    mode <- col_mode[c]
    if (mode == "path") {
      r <- col_dec[[c]]$row %||% base_row
      ch <- aln_mat[r, c]; sr <- r
    } else if (mode == "majority") {
      r <- .majority_row(aln_mat, c, support)
      ch <- aln_mat[r, c]; sr <- r
    } else if (mode == "iupac") {
      ch <- iupac_code(aln_mat[, c]); sr <- NA_integer_
    } else if (mode == "nmask") {
      ch <- "N"; sr <- NA_integer_
    } else { # "base"
      ch <- aln_mat[base_row, c]; sr <- base_row
    }
    if (is.na(ch) || ch == "-") next  # gap: emits no base
    bases <- c(bases, toupper(ch))
    src   <- c(src, sr)
    cols  <- c(cols, c)
  }
  list(
    seq = paste(bases, collapse = ""),
    map = data.frame(aln_col = cols, src_row = src, base = bases,
                     stringsAsFactors = FALSE)
  )
}

#' Pick the alignment row with the highest read support at a column.
#' Falls back to the first non-gap row when support is unavailable.
#' @noRd
.majority_row <- function(aln_mat, col, support) {
  rn <- rownames(aln_mat)
  if (!is.null(support)) {
    vals <- vapply(rn, function(nm) {
      v <- support[[nm]]
      if (is.null(v) || col > length(v)) NA_real_ else v[col]
    }, numeric(1))
    if (any(is.finite(vals))) return(which.max(replace(vals, !is.finite(vals), -Inf)))
  }
  nongap <- which(aln_mat[, col] != "-")
  if (length(nongap)) nongap[1] else 1L
}
