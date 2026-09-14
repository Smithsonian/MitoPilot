# The Assemble table cell for a MapToRef reference. rt_link() renders NA as
# plain empty text, so a sample on any other assembler gets no link.
#' @noRd
mtr_display_ref <- function(assembler, ref, topology) {
  is_mtr <- !is.na(assembler) & assembler == "MapToRef"
  blank <- is.na(ref) | !nzchar(trimws(ref))
  topo <- ifelse(is.na(topology) | !nzchar(trimws(topology)), "",
                 paste0(" (", topology, ")"))
  out <- rep(NA_character_, length(assembler))
  out[is_mtr] <- ifelse(blank[is_mtr], "set reference", paste0(ref, topo)[is_mtr])
  out
}
