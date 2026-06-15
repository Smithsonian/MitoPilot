#' Generate Nextflow command to run pipline
#'
#' @param path MitoPilot project directory
#' @param workflow Which module to update (default = c("assemble",
#'   "annotate"))
#' @param source Nextflow script source. By default, this will be in the
#'   `nextflow/` subdirectory of the package installation.
#' @param userAsmbs User supplied assemblies, TRUE/FALSE? (default = FALSE)
#'
#' @export
nextflow_cmd <- function(
    workflow = c("assemble", "annotate"),
    path = NULL,
    source = app_sys("nextflow"),
    userAsmbs = FALSE) {
  path <- path %||% dirname(getOption("MitoPilot.db") %||% normalizePath(".sqlite"))
  workflow <- tolower(workflow[1])

  if (workflow %nin% c("assemble", "annotate")) {
    stop("Invalid workflow.")
  }

  if(userAsmbs){
    cmd <- c(
      "-log", "{file.path(path, '.logs', 'nextflow.log')}",
      "run", "{source}",
      #"-ansi-log", "false",
      "-c", "{file.path(path, '.config')}",
      "-entry", "{ifelse(workflow == 'assemble', 'WF1_userAsmb', 'WF2')}",
      "{ifelse(file.exists(file.path(path, '.logs', 'nextflow.log')), '-resume', '')}"
    ) |> purrr::map_chr(~ stringr::str_glue(.x))

    return(invisible(cmd))
  } else {
    cmd <- c(
      "-log", "{file.path(path, '.logs', 'nextflow.log')}",
      "run", "{source}",
      #"-ansi-log", "false",
      "-c", "{file.path(path, '.config')}",
      "-entry", "{ifelse(workflow == 'assemble', 'WF1', 'WF2')}",
      "{ifelse(file.exists(file.path(path, '.logs', 'nextflow.log')), '-resume', '')}"
    ) |> purrr::map_chr(~ stringr::str_glue(.x))

    return(invisible(cmd))
  }

}

#' Read the scheduler executor (and queue) from a project .config
#'
#' Parses the Nextflow `process` block for the literal `executor = '...'` and
#' `queue = '...'` assignments. Used to tailor a cluster submission script.
#'
#' @param path Path to the project `.config` file.
#'
#' @return A list with `executor` (e.g. "slurm", "sge", "pbs", "lsf", "local",
#'   "awsbatch"; defaults to "local" if not found) and `queue` (the queue name,
#'   or `NULL` if absent / still an unfilled `<<QUEUE>>` placeholder).
#' @noRd
read_config_executor <- function(path) {
  lines <- tryCatch(readLines(path), error = function(e) character(0))

  grab <- function(key) {
    m <- stringr::str_match(lines, paste0("^\\s*", key, "\\s*=\\s*['\"]([^'\"]+)['\"]"))
    vals <- stats::na.omit(m[, 2])
    if (length(vals) == 0) NULL else vals[1]
  }

  executor <- grab("executor") %||% "local"
  queue <- grab("queue")
  if (!is.null(queue) && grepl("<<", queue, fixed = TRUE)) {
    queue <- NULL
  }

  list(executor = executor, queue = queue)
}

#' Build a cluster submission script for a headless Nextflow run
#'
#' Emits a shell script that wraps the Nextflow command in a scheduler resource
#' block derived from the project executor. The head job is lightweight (it only
#' orchestrates Nextflow), so resource requests are modest. Cluster-specific
#' environment setup is left as commented placeholders for the user to edit.
#' This builder never submits the job.
#'
#' @param executor Scheduler name ("slurm", "sge", "pbs", "lsf", or other).
#' @param queue Queue / partition name, or `NULL` to omit the queue directive.
#' @param full_nf_cmd The full `nextflow ...` command string to run.
#' @param job_name Job name for the scheduler.
#' @param log_file Path to the combined stdout/stderr log file.
#'
#' @return Character vector of script lines, ready for `writeLines()`.
#' @noRd
submission_script <- function(executor, queue, full_nf_cmd, job_name, log_file) {
  executor <- tolower(executor %||% "local")
  if (executor == "pbspro") executor <- "pbs"

  directives <- switch(
    executor,
    slurm = c(
      paste0("#SBATCH -J ", job_name),
      paste0("#SBATCH -o ", log_file),
      if (!is.null(queue)) paste0("#SBATCH -p ", queue),
      "#SBATCH -c 1",
      "#SBATCH --mem=16G",
      "#SBATCH -t 168:00:00"
    ),
    sge = c(
      paste0("#$ -N ", job_name),
      paste0("#$ -o ", log_file),
      "#$ -cwd -j y",
      if (!is.null(queue)) paste0("#$ -q ", queue),
      "#$ -pe mthread 1",
      "#$ -S /bin/sh"
    ),
    pbs = c(
      paste0("#PBS -N ", job_name),
      paste0("#PBS -o ", log_file),
      "#PBS -j oe",
      if (!is.null(queue)) paste0("#PBS -q ", queue),
      "#PBS -l select=1:ncpus=1:mem=16gb",
      "#PBS -l walltime=168:00:00"
    ),
    lsf = c(
      paste0("#BSUB -J ", job_name),
      paste0("#BSUB -o ", log_file),
      if (!is.null(queue)) paste0("#BSUB -q ", queue),
      "#BSUB -n 1",
      "#BSUB -M 16000",
      "#BSUB -W 168:00"
    ),
    paste0("# No HPC scheduler resource block (executor: ", executor,
           "). Run this script directly or edit for your scheduler.")
  )

  c(
    "#!/bin/sh",
    directives,
    "",
    'echo "--- MitoPilot job started: `date` ---"',
    "",
    "# EDIT: load your cluster environment below (uncomment / adjust as needed)",
    "# source ~/.bashrc",
    "# module load java",
    "# mamba activate MitoPilot_deps",
    "",
    full_nf_cmd,
    "",
    'echo "--- MitoPilot job done: `date` ---"'
  )
}
