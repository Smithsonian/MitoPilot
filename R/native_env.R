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

#' Environment variables produced by sourcing a native activate.sh
#'
#' Runs the script in a bash subshell and returns the variables the Nextflow
#' launcher needs, as a named character vector suitable for `processx` `env`.
#' @noRd
native_env <- function(activate) {
  if (!file.exists(activate)) {
    stop("Native activate script not found: ", activate, call. = FALSE)
  }
  keys <- c("PATH", "JAVA_HOME", "NXF_HOME", "MITOPILOT_NO_CONDA", "MITOPILOT_NATIVE_PREFIX")
  script <- paste0("source ", shQuote(activate), " >/dev/null 2>&1; ",
                   "for k in ", paste(keys, collapse = " "),
                   "; do [ -n \"${!k-}\" ] && printf '%s=%s\\n' \"$k\" \"${!k}\"; done; true")
  out <- system2("bash", c("-c", shQuote(script)), stdout = TRUE, stderr = FALSE)
  out <- out[nzchar(out)]
  vals <- sub("^[A-Z_]+=", "", out)
  stats::setNames(vals, sub("=.*$", "", out))
}
