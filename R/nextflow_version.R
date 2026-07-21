# Nextflow version compatibility for the MitoPilot pipeline.
#
# Supported range was determined empirically (see README "Nextflow
# compatibility"):
#   * Lower bound 24.10.0: first release that honors `workflow.failOnIgnore`.
#     Below it, an ignored task failure (e.g. the driver-side curated-result DB
#     writer, which runs errorStrategy 'ignore') silently passes as success.
#   * Upper bound: Nextflow 26.x removed `nextflow.NF.isDsl2()`, which every
#     released nf-sqldb plugin calls at load, hard-breaking the pipeline with
#     `NoSuchMethodError`. Verified good through the 25.10.x line.

#' Minimum supported Nextflow version.
#' @noRd
NF_MIN_SUPPORTED <- "24.10.0"

#' Latest verified-good Nextflow release (pin target when the installed version
#' is outside the supported range).
#' @noRd
NF_MAX_SUPPORTED <- "25.10.6"

#' First Nextflow release line that breaks the pipeline (nf-sqldb / NF.isDsl2()).
#' @noRd
NF_BROKEN_FROM <- "26.0.0"

#' Human-readable supported range, for messages.
#' @noRd
nf_supported_label <- function() {
  paste0(NF_MIN_SUPPORTED, " - 25.10.x")
}

#' Parse the installed Nextflow version
#'
#' Runs `nextflow -version` and extracts the `X.Y.Z` version, dropping any
#' `-edge` / build suffix.
#'
#' @return A version string like "24.10.9", or `NA_character_` if Nextflow is
#'   not installed / not parseable.
#' @noRd
nf_installed_version <- function() {
  # Merge stderr and ignore the exit status: the first run of a freshly
  # installed launcher prints "Downloading nextflow dependencies..." (and may
  # exit non-zero) before the version banner, and some builds emit the banner on
  # stderr. Parse the version from whatever the command produced.
  out <- suppressWarnings(tryCatch(
    system2("nextflow", args = "-version", stdout = TRUE, stderr = TRUE),
    error = function(e) character(0)
  ))
  if (length(out) == 0) {
    return(NA_character_)
  }
  m <- stringr::str_match(out, "version\\s+([0-9]+\\.[0-9]+\\.[0-9]+)")
  v <- stats::na.omit(m[, 2])
  if (length(v) == 0) NA_character_ else v[1]
}

#' Is a `nextflow` executable on PATH?
#'
#' Lets callers tell "not installed" apart from "installed but `nextflow
#' -version` failed" (e.g. Java missing from the session, or a failed first-run
#' download), so the guidance can point at the real problem.
#' @return `TRUE` if a `nextflow` binary is found on PATH.
#' @noRd
nf_on_path <- function() {
  nzchar(Sys.which("nextflow"))
}

#' Classify a Nextflow version against the supported range
#'
#' @param version Version string (defaults to the installed Nextflow).
#' @return One of "missing", "too_old", "supported", "too_new".
#' @noRd
nf_version_status <- function(version = nf_installed_version()) {
  if (is.na(version)) {
    return("missing")
  }
  v <- numeric_version(version)
  if (v < numeric_version(NF_MIN_SUPPORTED)) {
    return("too_old")
  }
  if (v >= numeric_version(NF_BROKEN_FROM)) {
    return("too_new")
  }
  "supported"
}

#' Choose the NXF_VER value to pin for a run
#'
#' In-range installs pin to themselves so cluster nodes without internet do not
#' try to download a different build. Out-of-range installs (too old, or too new
#' / broken) pin to the latest verified-good release so the run uses a compatible
#' engine.
#'
#' @param version Installed version string (defaults to the installed Nextflow).
#' @return A version string suitable for `NXF_VER`, or `NA_character_` if
#'   Nextflow is missing.
#' @noRd
nf_pin_version <- function(version = nf_installed_version()) {
  if (is.na(version)) {
    return(NA_character_)
  }
  if (nf_version_status(version) == "supported") {
    # Return the original string (e.g. "25.04.8"); a numeric_version round-trip
    # would drop the leading zero ("25.4.8"), which NXF_VER does not accept.
    return(version)
  }
  NF_MAX_SUPPORTED
}

#' Update hint shown when the installed Nextflow is out of range.
#' @noRd
nf_update_hint <- function() {
  "Update Nextflow, e.g. mamba install -n MitoPilot_deps 'bioconda::nextflow>=24.10,<26'"
}

#' Are we inside R's install / build / check / dev-load machinery?
#'
#' `R CMD INSTALL` (and `R CMD check`) attach the namespace in a final load-test,
#' so an unconditional `stop()` in `.onAttach` would break installation on any
#' machine without a compatible Nextflow (including CI). `devtools::load_all()`
#' likewise runs `.onAttach` during development. These env vars are set only by
#' that machinery, never by a real `library()` call.
#' @noRd
in_r_build_context <- function() {
  nzchar(Sys.getenv("R_INSTALL_PKG")) ||
    nzchar(Sys.getenv("R_CMD")) ||
    nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_")) ||
    nzchar(Sys.getenv("DEVTOOLS_LOAD"))
}

#' Check the installed Nextflow version at a pipeline entry point
#'
#' Warns (or stops) when the installed Nextflow is outside the supported range.
#' Called from `new_project*()`, the app launcher, and `hydra_setup()`.
#'
#' @param context Short label for where the check runs (e.g. "new_project"),
#'   included in messages.
#' @param on_too_old "stop" (default) errors out; "warn" only warns.
#' @return Invisibly, the version status string.
#' @noRd
check_nextflow_version <- function(context = NULL, on_too_old = c("stop", "warn")) {
  on_too_old <- match.arg(on_too_old)
  version <- nf_installed_version()
  status <- nf_version_status(version)
  ctx <- if (is.null(context)) "" else paste0(" (", context, ")")

  switch(status,
    missing = warning(
      if (nf_on_path()) {
        glue::glue(
          "A 'nextflow' executable was found but 'nextflow -version' did not ",
          "report a version{ctx}. If you just installed Nextflow, run `nextflow ",
          "-version` once in a terminal to let it finish downloading, and make ",
          "sure Java is on PATH in this session. MitoPilot needs Nextflow ",
          "{nf_supported_label()}."
        )
      } else {
        glue::glue(
          "Nextflow not found{ctx}. MitoPilot needs Nextflow ",
          "{nf_supported_label()}. Install from https://www.nextflow.io/"
        )
      },
      call. = FALSE
    ),
    too_old = {
      msg <- glue::glue(
        "Nextflow {version} is below the minimum supported version ",
        "({NF_MIN_SUPPORTED}){ctx}. {nf_update_hint()}"
      )
      if (on_too_old == "stop") stop(msg, call. = FALSE) else warning(msg, call. = FALSE)
    },
    too_new = warning(
      glue::glue(
        "Nextflow {version} is newer than MitoPilot supports{ctx}; the nf-sqldb ",
        "plugin breaks on Nextflow 26+. Runs will be pinned to Nextflow ",
        "{NF_MAX_SUPPORTED} via NXF_VER. To avoid re-downloading an engine, ",
        "install a supported version: {nf_update_hint()}"
      ),
      call. = FALSE
    ),
    supported = invisible(NULL)
  )
  invisible(status)
}
