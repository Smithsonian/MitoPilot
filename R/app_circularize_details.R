#' Turn a stored gapped alignment into per-column plot data
#'
#' The two copies are stored as gapped strings rather than one row per base, so
#' the split happens here, once, for the window actually being drawn.
#'
#' @param aln_query,aln_subject gapped alignment strings of equal length
#' @param from,to 1-based inclusive column bounds, clamped to the alignment
#'
#' @return data frame with `col`, `base_q`, `base_s`, `match`
#'
#' @noRd
circularize_aln_df <- function(aln_query, aln_subject, from = 1L, to = NULL) {
  empty <- data.frame(
    col = integer(0), base_q = character(0),
    base_s = character(0), match = logical(0)
  )
  if (length(aln_query) != 1L || is.na(aln_query) || !nzchar(aln_query)) {
    return(empty)
  }
  q <- strsplit(aln_query, "")[[1]]
  s <- strsplit(aln_subject, "")[[1]]
  n <- min(length(q), length(s))
  from <- as.integer(from)
  from <- if (length(from) != 1L || is.na(from)) 1L else max(1L, from)
  to <- as.integer(to)
  to <- if (length(to) != 1L || is.na(to)) n else min(n, to)
  if (from > to) {
    return(empty)
  }
  idx <- from:to
  data.frame(
    col = idx,
    base_q = q[idx],
    base_s = s[idx],
    match = q[idx] == s[idx]
  )
}
