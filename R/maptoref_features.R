#' Parse mitogenome annotations from a GenBank record
#'
#' Reads the FEATURES table of a single-record GenBank file into the flat frame
#' the MapToRef viewer's annotation track draws. Returns a zero-row frame with
#' the same columns whenever there is nothing to draw, so callers never branch
#' on NULL.
#'
#' @param gb_path path to a single-record GenBank file
#' @return data.frame with columns type, gene, start, end, strand
#'
#' @noRd
maptoref_parse_features <- function(gb_path) {
  empty <- data.frame(
    type = character(0), gene = character(0),
    start = integer(0), end = integer(0), strand = character(0),
    stringsAsFactors = FALSE
  )
  if (length(gb_path) != 1L || is.na(gb_path) || !file.exists(gb_path)) {
    return(empty)
  }
  rec <- NULL
  # read.gb writes a progress line; capture.output alone does not silence it.
  # A malformed file also makes read.gb warn internally, not just error.
  suppressWarnings(suppressMessages(invisible(utils::capture.output(
    rec <- try(
      read.gb::read.gb(gb_path, DNA = FALSE, Type = "full", Source = "File"),
      silent = TRUE
    )
  ))))
  if (inherits(rec, "try-error") || length(rec) == 0L) {
    return(empty)
  }
  feats <- rec[[1]]$FEATURES
  if (length(feats) == 0L) {
    return(empty)
  }

  keep <- c("CDS", "tRNA", "rRNA", "D_loop", "gene")
  types <- names(feats)
  out <- list()
  for (i in seq_along(feats)) {
    if (types[i] %nin% keep) {
      next
    }
    d <- feats[[i]]
    spans <- .mtr_parse_location(d$Qualifier[1])
    if (nrow(spans) == 0L) {
      next
    }
    out[[length(out) + 1L]] <- data.frame(
      type = sub("_", "-", types[i]),
      gene = .mtr_feature_name(d, types[i]),
      start = spans$start,
      end = spans$end,
      strand = spans$strand,
      stringsAsFactors = FALSE
    )
  }
  if (length(out) == 0L) {
    return(empty)
  }
  res <- do.call(rbind, out)

  # A mitogenome record annotates nearly every gene twice, once as `gene` and
  # once as its type. Keeping both draws every arrow twice.
  typed <- res[res$type != "gene", , drop = FALSE]
  bare <- res[res$type == "gene", , drop = FALSE]
  bare <- bare[paste(bare$start, bare$end) %nin%
                 paste(typed$start, typed$end), , drop = FALSE]
  res <- rbind(typed, bare)
  res <- res[order(res$start, res$end), , drop = FALSE]
  rownames(res) <- NULL
  res
}

#' Expand a GenBank location string into one row per span
#'
#' @param loc a location string such as "3803..4777" or "complement(join(...))"
#' @return data.frame with columns start, end, strand
#'
#' @noRd
.mtr_parse_location <- function(loc) {
  empty <- data.frame(
    start = integer(0), end = integer(0), strand = character(0),
    stringsAsFactors = FALSE
  )
  if (length(loc) != 1L || is.na(loc) || !nzchar(loc)) {
    return(empty)
  }
  strand <- if (grepl("complement", loc, fixed = TRUE)) "-" else "+"
  # Strips every operator name and its parentheses, leaving comma-separated
  # spans; a partial marker (< or >) survives and is dropped by the digit match.
  txt <- gsub("[a-zA-Z_.]+\\(|\\)", "", loc)
  parts <- trimws(strsplit(txt, ",", fixed = TRUE)[[1]])
  parts <- parts[nzchar(parts)]
  spans <- lapply(parts, function(p) {
    n <- as.integer(regmatches(p, gregexpr("[0-9]+", p))[[1]])
    if (length(n) == 0L) {
      return(NULL)
    }
    data.frame(
      start = n[1], end = n[length(n)], strand = strand,
      stringsAsFactors = FALSE
    )
  })
  spans <- spans[!vapply(spans, is.null, logical(1))]
  if (length(spans) == 0L) {
    return(empty)
  }
  do.call(rbind, spans)
}

#' Best available display name for one GenBank feature
#'
#' Row 1 of a feature frame holds the location, not a qualifier, so the lookup
#' starts at row 2; a `gene` feature would otherwise name itself "3803..4777".
#'
#' @param d two-column feature data.frame from read.gb
#' @param type feature type, used as the last-resort name
#' @return single character
#'
#' @noRd
.mtr_feature_name <- function(d, type) {
  pick <- function(key) {
    hit <- which(d$Location == key)
    hit <- hit[hit > 1L]
    if (length(hit) == 0L) {
      return(NA_character_)
    }
    v <- trimws(d$Qualifier[hit[1]])
    if (nzchar(v)) v else NA_character_
  }
  for (key in c("gene", "product", "locus_tag")) {
    v <- pick(key)
    if (!is.na(v)) {
      return(v)
    }
  }
  sub("_", "-", type)
}
