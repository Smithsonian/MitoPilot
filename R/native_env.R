#' Normalise a conda env name; NULL means "tool is on PATH"
#'
#' Native (no-container) installs export `MITOPILOT_NO_CONDA=1` from
#' `activate.sh`, which turns every conda env name into NULL so callers fall
#' through to a plain `system2()` on PATH.
#' @noRd
.mp_condaenv <- function(x) {
  if (identical(Sys.getenv("MITOPILOT_NO_CONDA"), "1")) return(NULL)
  if (is.null(x) || !nzchar(x) || tolower(x) %in% c("none", "null")) return(NULL)
  x
}

#' @noRd
bam_readcount_cmd <- function(assembly, bam, out) {
  runner <- if (is.null(.mp_condaenv("bam-readcount"))) "" else "conda run -n bam-readcount "
  paste0(runner, "bam-readcount -w1 -f ", assembly, " ", bam, " > ", out)
}
