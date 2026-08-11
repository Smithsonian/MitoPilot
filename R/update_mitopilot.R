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
      # userAsmb assemblies are a single user-provided genome: WF2 validates all
      # their contigs together as one unit (no per-scaffold split).
      "--userAsmb", "true",
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
submission_script <- function(executor, queue, full_nf_cmd, job_name, log_file,
                              nxf_ver = nf_pin_version()) {
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
    # Pin the Nextflow engine to a MitoPilot-compatible version.
    if (!is.na(nxf_ver)) paste0("export NXF_VER=", nxf_ver),
    full_nf_cmd,
    "",
    'echo "--- MitoPilot job done: `date` ---"'
  )
}

#' Detect whether we are running on the NMNH Hydra cluster
#'
#' Mirrors the check used by the non-headless "Submit as Job" handler: greps
#' `/etc/hosts` for "hydra".
#'
#' @return `TRUE` if running on Hydra, otherwise `FALSE`.
#' @noRd
is_hydra_cluster <- function() {
  motd_output <- try(readLines("/etc/hosts", warn = FALSE), silent = TRUE)
  !inherits(motd_output, "try-error") &&
    any(grepl("hydra", motd_output, ignore.case = TRUE))
}

#' Configure the R session environment for the NMNH Hydra cluster
#'
#' RStudio Server sessions on Hydra start with a stripped `PATH` that omits the
#' Univa Grid Engine, Java, and user `~/bin` directories, so Nextflow cannot find
#' `qsub` and job submission fails. Call this once per session, before launching
#' the MitoPilot app, to prepend those directories to `PATH`. Has no effect (and
#' warns) when not running on Hydra.
#'
#' @return Invisibly, `TRUE` if the Hydra environment was applied, otherwise
#'   `FALSE`.
#' @export
hydra_setup <- function() {
  if (!is_hydra_cluster()) {
    warning("Not running on the NMNH Hydra cluster; environment unchanged.")
    return(invisible(FALSE))
  }

  hydra_bins <- c(
    file.path(Sys.getenv("HOME"), "bin"),
    "/share/apps/tools/java/21.0.2/bin",
    "/cm/shared/apps/uge/8.8.1/bin/lx-amd64"
  )
  cur <- strsplit(Sys.getenv("PATH"), .Platform$path.sep, fixed = TRUE)[[1]]
  # Force hydra bins to the front (drop any existing low-priority copies) so the
  # UGE qsub and Java 21 always win, matching the legacy PATH setup.
  cur <- cur[!cur %in% hydra_bins]
  Sys.setenv(PATH = paste(c(hydra_bins, cur), collapse = .Platform$path.sep))

  # Now that Nextflow + Java resolve on PATH, warn on an unsupported version and
  # pin NXF_VER for this session so app / submitted runs use a compatible engine.
  check_nextflow_version("hydra_setup", on_too_old = "warn")
  pin <- nf_pin_version()
  if (!is.na(pin)) {
    Sys.setenv(NXF_VER = pin)
    message("Pinned NXF_VER=", pin, " for this session.")
  }

  invisible(TRUE)
}

#' Hydra-specific submission script
#'
#' Reproduces the script format used by the non-headless "Submit as Job" handler
#' on NMNH Hydra (SGE directives, java module load, NXF_OPTS).
#'
#' @param full_nf_cmd The full `nextflow ...` command string to run.
#' @param job_name Job name for the scheduler.
#' @param log_file Path to the combined stdout/stderr log file.
#' @return Character vector of script lines.
#' @noRd
hydra_submission_script <- function(full_nf_cmd, job_name, log_file,
                                    nxf_ver = nf_pin_version()) {
  c(
    "#!/bin/sh",
    paste0("#$ -N ", job_name),
    paste0("#$ -o ", log_file),
    "#$ -cwd -j y",
    "#$ -q lTWFM.sq",
    "#$ -l wfmq",
    "#$ -l mres=24G,h_data=24G,h_vmem=64G",
    "#$ -pe mthread 1",
    "#$ -S /bin/sh",
    "",
    'echo "---"',
    'echo "+ `date` job $JOB_NAME started in $QUEUE with jobID=$JOB_ID on $HOSTNAME"',
    'echo "---"',
    "",
    "source ~/.bashrc",
    "module load tools/java/21.0.2",
    "",
    "export NXF_OPTS=\"-Xms512m -Xmx20g -XX:MaxMetaspaceSize=512m -Xss256k\" # Java memory limits for 16G RSS constraint",
    # Pin the Nextflow engine to a MitoPilot-compatible version.
    if (!is.na(nxf_ver)) paste0("export NXF_VER=", nxf_ver),
    full_nf_cmd,
    "",
    'echo "---"',
    'echo "= `date` job $JOB_NAME done"',
    'echo "---"'
  )
}

#' Cluster submit command for an executor
#'
#' @param executor Scheduler name.
#' @return The submit binary ("sbatch", "qsub", "bsub") or `NULL` if unknown.
#' @noRd
submit_command <- function(executor) {
  switch(tolower(executor %||% ""),
    slurm = "sbatch",
    sge = "qsub",
    pbs = "qsub",
    pbspro = "qsub",
    lsf = "bsub",
    NULL
  )
}

#' Path to a project's saved submission-script template
#'
#' @param work_dir Project directory.
#' @noRd
submit_template_path <- function(work_dir) {
  file.path(work_dir, ".mitopilot_submit.template")
}

#' Replace run-specific values in a script with reusable tokens (and back)
#'
#' Lets a user's edited submission script (resource directives, module loads,
#' etc.) be saved and reused while the job name, log path, and Nextflow command
#' are regenerated each run. `log_file` is substituted before `job_name` because
#' the log path contains the job name as a substring.
#'
#' @param lines Character vector of script lines.
#' @param full_nf_cmd,job_name,log_file Run-specific values.
#' @noRd
tokenize_submit_script <- function(lines, full_nf_cmd, job_name, log_file) {
  lines <- gsub(log_file, "<<LOG_FILE>>", lines, fixed = TRUE)
  lines <- gsub(job_name, "<<JOB_NAME>>", lines, fixed = TRUE)
  gsub(full_nf_cmd, "<<NEXTFLOW_CMD>>", lines, fixed = TRUE)
}

#' @rdname tokenize_submit_script
#' @noRd
fill_submit_template <- function(lines, full_nf_cmd, job_name, log_file) {
  lines <- gsub("<<LOG_FILE>>", log_file, lines, fixed = TRUE)
  lines <- gsub("<<JOB_NAME>>", job_name, lines, fixed = TRUE)
  gsub("<<NEXTFLOW_CMD>>", full_nf_cmd, lines, fixed = TRUE)
}

#' Build a submission script, reusing a saved per-project template if present
#'
#' @param work_dir Project directory (also where the template lives).
#' @param executor,queue Scheduler info (from [read_config_executor()]).
#' @param full_nf_cmd,job_name,log_file Run-specific values.
#' @return Character vector of script lines.
#' @noRd
build_submit_script <- function(work_dir, executor, queue, full_nf_cmd, job_name, log_file) {
  tmpl <- submit_template_path(work_dir)
  if (file.exists(tmpl)) {
    return(fill_submit_template(readLines(tmpl), full_nf_cmd, job_name, log_file))
  }
  if (is_hydra_cluster()) {
    return(hydra_submission_script(full_nf_cmd, job_name, log_file))
  }
  submission_script(executor, queue, full_nf_cmd, job_name, log_file)
}

#' Save a user-edited submission script as a reusable project template
#'
#' @param text The (possibly edited) script, as a single string or a vector.
#' @param work_dir Project directory.
#' @param full_nf_cmd,job_name,log_file Run-specific values to tokenize out.
#' @noRd
save_submit_template <- function(text, work_dir, full_nf_cmd, job_name, log_file) {
  lines <- if (length(text) == 1) strsplit(text, "\n", fixed = TRUE)[[1]] else text
  lines <- tokenize_submit_script(lines, full_nf_cmd, job_name, log_file)
  writeLines(lines, submit_template_path(work_dir))
}

#' Submit a script to the cluster scheduler
#'
#' Runs the executor's submit command from `work_dir` so the job's working
#' directory matches the project.
#'
#' @param script_path Path to the script to submit.
#' @param executor Scheduler name (selects the submit command).
#' @param work_dir Directory to submit from.
#' @return List with `success` (logical), `command` (the binary, or `NULL`), and
#'   `output` (combined stdout/stderr).
#' @noRd
run_submit <- function(script_path, executor, work_dir) {
  cmd <- submit_command(executor)
  if (is.null(cmd)) {
    return(list(
      success = FALSE,
      command = NULL,
      output = paste0("No known submit command for executor '", executor, "'.")
    ))
  }
  old <- getwd()
  on.exit(setwd(old), add = TRUE)
  setwd(work_dir)
  out <- tryCatch(
    system2(cmd, args = shQuote(script_path), stdout = TRUE, stderr = TRUE),
    error = function(e) structure(conditionMessage(e), status = 1L)
  )
  status <- attr(out, "status")
  list(
    success = is.null(status) || status == 0,
    command = cmd,
    output = paste(out, collapse = "\n")
  )
}
