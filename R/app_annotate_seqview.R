# R/app_annotate_seqview.R

#' Sequence viewer payload (tools/nt_viewer_spec.md, section 4)
#'
#' Pure: no Shiny, no database. Soft-deleted rows (pos1 == 0) are dropped;
#' the browser owns all coordinate arithmetic, so positions pass through as
#' stored (1-based, inclusive, pos1 > pos2 for an origin-crossing feature).
#' @noRd
seqview_payload <- function(annotations, seq, topology, unit,
                            selected = NULL, version = 0L) {
  seq <- toupper(as.character(seq)[1])
  a <- annotations
  yes <- function(x) {
    x <- tolower(as.character(x))
    length(x) == 1L && !is.na(x) && x %in% c("yes", "true", "1")
  }
  rows <- which(!is.na(a$pos1) & a$pos1 > 0)
  feats <- lapply(rows, function(i) {
    notes <- as.character(a$notes[i])
    if (is.na(notes)) notes <- ""
    joined <- regmatches(notes, regexpr("JOIN: mode=[A-Za-z]+ group=[0-9]+", notes))
    tr <- as.character(a$translation[i])
    f <- list(
      row = i,
      type = as.character(a$type[i]),
      gene = as.character(a$gene[i]),
      pos1 = as.integer(a$pos1[i]),
      pos2 = as.integer(a$pos2[i]),
      dir = as.character(a$direction[i]),
      partial5 = yes(a$partial_start[i]),
      partial3 = yes(a$partial_stop[i]),
      notes = substr(notes, 1L, 80L)
    )
    if (identical(f$type, "PCG") && !is.na(tr) && nzchar(tr)) f$translation <- tr
    if (length(joined) == 1L) f$joined <- joined
    f
  })
  sel <- if (length(selected) == 1L && !is.na(selected) && selected %in% rows) {
    as.integer(selected)
  }
  list(
    unit = unit, len = nchar(seq), topology = topology, seq = seq,
    version = as.integer(version), selected = sel, features = feats
  )
}
