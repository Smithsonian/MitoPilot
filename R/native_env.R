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

#' Run a pipeline tool, through conda when an env name is given
#' @noRd
run_tool <- function(cmd, args, condaenv = NULL, runner = system2) {
  condaenv <- .mp_condaenv(condaenv)
  if (is.null(condaenv)) {
    return(runner(cmd, args))
  }
  reticulate::conda_run2(cmd = cmd, args = args, envname = condaenv, echo = FALSE)
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
  keys <- c("PATH", "JAVA_HOME", "NXF_HOME", "MITOPILOT_NO_CONDA", "MITOPILOT_NATIVE_PREFIX",
            "SGE_ROOT", "SGE_CELL", "SGE_ARCH", "SGE_EXECD_PORT", "SGE_QMASTER_PORT",
            "LSF_ENVDIR", "LSF_SERVERDIR", "LSF_LIBDIR", "LSF_BINDIR", "SLURM_CONF", "PBS_HOME")
  script <- paste0("source ", shQuote(activate), " >/dev/null 2>&1; ",
                   "for k in ", paste(keys, collapse = " "),
                   "; do [ -n \"${!k-}\" ] && printf '%s=%s\\n' \"$k\" \"${!k}\"; done; true")
  out <- suppressWarnings(system2("bash", c("-c", shQuote(script)), stdout = TRUE, stderr = FALSE))
  out <- out[nzchar(out)]
  vals <- sub("^[A-Z_]+=", "", out)
  result <- stats::setNames(vals, sub("=.*$", "", out))
  if (!"PATH" %in% names(result)) {
    stop("Sourcing ", activate, " produced no environment; run it in a shell to see the error.", call. = FALSE)
  }
  result
}

#' Nextflow pin computed with the native env's launcher on PATH
#' @noRd
native_nf_pin <- function(nat_env) {
  old <- Sys.getenv(c("PATH", "NXF_VER"), unset = NA)
  on.exit({
    Sys.setenv(PATH = old[["PATH"]])
    if (is.na(old[["NXF_VER"]])) Sys.unsetenv("NXF_VER") else Sys.setenv(NXF_VER = old[["NXF_VER"]])
  }, add = TRUE)
  Sys.setenv(PATH = nat_env[["PATH"]])
  Sys.unsetenv("NXF_VER")
  nf_pin_version()
}

#' Use a native (no-container) MitoPilot environment in this R session
#'
#' Applies the PATH and variables from `<prefix>/activate.sh` (written by
#' `inst/native/install_mitopilot_native.sh`) to the current session, so `nextflow`,
#' `java`, and every pipeline tool resolve here exactly as they do inside
#' pipeline tasks. Call it once after `library(MitoPilot)` when running the app
#' from RStudio Server or any R session that was not started from a shell where
#' `activate.sh` was already sourced. The app itself reads the same file from the
#' project `.config`, so this is only needed for console use.
#'
#' @param prefix Directory given to `install_mitopilot_native.sh --prefix`. Defaults to
#'   `MITOPILOT_NATIVE_PREFIX` if set.
#' @return (invisibly) `TRUE`.
#' @export
native_setup <- function(prefix = Sys.getenv("MITOPILOT_NATIVE_PREFIX")) {
  if (!nzchar(prefix)) stop("Give `prefix`, or set MITOPILOT_NATIVE_PREFIX.", call. = FALSE)
  env <- native_env(file.path(prefix, "activate.sh"))
  env[["MITOPILOT_NATIVE_PREFIX"]] <- prefix
  do.call(Sys.setenv, as.list(env))
  check_nextflow_version("native_setup", on_too_old = "warn")
  pin <- nf_pin_version()
  if (!is.na(pin)) {
    Sys.setenv(NXF_VER = pin)
    message("Pinned NXF_VER=", pin, " for this session.")
  }
  message("Native MitoPilot environment active: ", prefix)
  invisible(TRUE)
}

#' Check a native (no-container) MitoPilot environment
#'
#' Sources `<prefix>/activate.sh` and looks up every pipeline tool. Core tools
#' are needed for the default workflow; optional tools back the MitoFinder,
#' ARWEN, and ORFfinder options and are only present after
#' `install_mitopilot_native.sh --with-optional`.
#'
#' @inheritParams native_setup
#' @param strict Error if any core tool is missing (default `TRUE`).
#' @return (invisibly) a data.frame with columns `tool`, `required`, `found`,
#'   `path`, `version`.
#' @export
native_check <- function(prefix = Sys.getenv("MITOPILOT_NATIVE_PREFIX"), strict = TRUE) {
  if (!nzchar(prefix)) stop("Give `prefix`, or set MITOPILOT_NATIVE_PREFIX.", call. = FALSE)
  env <- native_env(file.path(prefix, "activate.sh"))
  old_path <- Sys.getenv("PATH")
  on.exit(Sys.setenv(PATH = old_path), add = TRUE)
  Sys.setenv(PATH = env[["PATH"]])

  core <- c("R", "Rscript", "nextflow", "java", "fastp", "get_organelle_from_reads.py",
            "bowtie2", "bwa", "samtools", "minimap2", "blastn", "blastdbcmd",
            "makeblastdb", "runmitos", "tRNAscan-SE", "aragorn", "bam-readcount",
            "parallel", "file")
  optional <- c("mitofinder", "arwen", "ORFfinder")
  version_flag <- c(R = "--version", Rscript = "--version", nextflow = "-version",
                    java = "-version", fastp = "--version",
                    "get_organelle_from_reads.py" = "--version", bowtie2 = "--version",
                    bwa = "", samtools = "--version", minimap2 = "--version",
                    blastn = "-version", blastdbcmd = "-version", makeblastdb = "-version",
                    runmitos = "--version", "tRNAscan-SE" = "-h", aragorn = "-h",
                    "bam-readcount" = "--version", parallel = "--version", file = "--version",
                    mitofinder = "--version", arwen = "-h")
  tools <- c(core, optional)
  path <- vapply(tools, function(t) unname(Sys.which(t)), character(1))
  version <- vapply(tools, function(t) {
    if (!nzchar(path[[t]])) return(NA_character_)
    flag <- version_flag[t]
    if (is.na(flag)) return("")
    out <- tryCatch(suppressWarnings(system2(t, flag, stdout = TRUE, stderr = TRUE)),
                    error = function(e) character())
    out <- trimws(out[grepl("[0-9]", out)])
    hit <- out[grepl(paste0("version|", sub("[.].*$", "", t)), out, ignore.case = TRUE)]
    out <- if (length(hit)) hit[1] else out[1]
    if (!length(out) || is.na(out)) return("")
    out <- sub("^.*?(version)", "\\1", out, ignore.case = TRUE)
    substr(out, 1, 60)
  }, character(1))
  res <- data.frame(tool = tools, required = tools %in% core, found = nzchar(path),
                    path = unname(path), version = unname(version),
                    stringsAsFactors = FALSE, row.names = NULL)
  db <- file.path(prefix, "ref_dbs", "mito_metazoa", "taxonomy4blast.sqlite3")
  res <- rbind(res, data.frame(tool = "mito_metazoa BLAST DB", required = FALSE,
                               found = file.exists(db), path = if (file.exists(db)) dirname(db) else "",
                               version = "", stringsAsFactors = FALSE))
  print(res[, c("tool", "required", "found", "version")], row.names = FALSE)
  missing <- res$tool[res$required & !res$found]
  if (strict && length(missing)) {
    stop("Missing core tools: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  invisible(res)
}
