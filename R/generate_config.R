#' MitoPilot user config directory
#'
#' Persistent, per-user directory where saved cluster profiles
#' (`config.<name>` files) are stored. Created on demand by
#' [generate_config()]. Uses [tools::R_user_dir()] so it follows the platform
#' convention (e.g. `~/.local/share/MitoPilot` or `~/.config/MitoPilot`).
#'
#' @return Path to the MitoPilot config directory (not guaranteed to exist).
#' @export
mitopilot_config_dir <- function() {
  tools::R_user_dir("MitoPilot", which = "config")
}

#' Build a Nextflow container-engine block
#'
#' @param engine One of "docker", "singularity", "apptainer".
#' @param cache Optional cacheDir (singularity/apptainer only).
#' @param run_options Optional runOptions string (singularity/apptainer only).
#'
#' @return A single string (may contain newlines) for the `<<CONTAINER_ENGINE>>`
#'   placeholder.
#' @noRd
container_engine_block <- function(engine, cache = NULL, run_options = NULL) {
  if (engine == "docker") {
    return("docker {\n  enabled = true\n}")
  }
  lines <- c(paste0(engine, " {"), "  enabled = true")
  if (!is.null(cache) && nzchar(cache)) {
    lines <- c(lines, paste0("  cacheDir = '", cache, "'"))
  }
  if (!is.null(run_options) && nzchar(run_options)) {
    lines <- c(lines, paste0("  runOptions = '", run_options, "'"))
  }
  lines <- c(lines, "}")
  paste(lines, collapse = "\n")
}

#' Substitute `<<PLACEHOLDER>>` tokens in config template lines
#'
#' Shared by [generate_config()] and the project-init functions so the
#' substitution logic lives in one place. `NULL` values are skipped, leaving the
#' token intact for a later pass (or for the user to hand-edit).
#'
#' @param lines Character vector of config file lines.
#' @param replacements Named list; name `FOO` replaces token `<<FOO>>` with its
#'   (coerced-to-character) value. `NULL` entries are skipped.
#'
#' @return The modified character vector.
#' @noRd
fill_config <- function(lines, replacements) {
  for (nm in names(replacements)) {
    val <- replacements[[nm]]
    if (is.null(val)) next
    token <- paste0("<<", nm, ">>")
    lines <- stringr::str_replace_all(lines, stringr::fixed(token), as.character(val))
  }
  lines
}

#' Resolve an executor name to a config template path
#'
#' Resolution order: a saved user profile (`<profile_dir>/config.<executor>`)
#' first, then a package built-in (`inst/config.<executor>`).
#'
#' @param executor Executor / profile name.
#' @param profile_dir User profile directory (see [mitopilot_config_dir()]).
#'
#' @return Path to a config template file.
#' @noRd
resolve_config <- function(executor, profile_dir = mitopilot_config_dir()) {
  user_path <- file.path(profile_dir, paste0("config.", executor))
  if (file.exists(user_path)) {
    return(user_path)
  }
  pkg_path <- app_sys(paste0("config.", executor))
  if (nzchar(pkg_path) && file.exists(pkg_path)) {
    return(pkg_path)
  }
  available <- list_configs(profile_dir = profile_dir)
  stop(
    "No config found for executor '", executor, "'.\nAvailable: ",
    paste(available$name, collapse = ", "),
    call. = FALSE
  )
}

#' List available executor configs
#'
#' Reports the package built-in templates plus any saved cluster profiles. The
#' `name` column is what you pass to `new_project(executor = ...)`.
#'
#' @param profile_dir User profile directory (see [mitopilot_config_dir()]).
#'
#' @return A data.frame with columns `name`, `type` ("builtin" or "saved"), and
#'   `path`.
#' @export
list_configs <- function(profile_dir = mitopilot_config_dir()) {
  builtin_files <- Sys.glob(file.path(app_sys(), "config.*"))
  saved_files <- if (dir.exists(profile_dir)) {
    Sys.glob(file.path(profile_dir, "config.*"))
  } else {
    character(0)
  }
  to_df <- function(files, type) {
    if (length(files) == 0) {
      return(NULL)
    }
    data.frame(
      name = sub("^config\\.", "", basename(files)),
      type = type,
      path = files,
      stringsAsFactors = FALSE
    )
  }
  out <- rbind(to_df(builtin_files, "builtin"), to_df(saved_files, "saved"))
  if (is.null(out)) {
    out <- data.frame(
      name = character(0), type = character(0), path = character(0),
      stringsAsFactors = FALSE
    )
  }
  out
}

#' Generate and save a reusable cluster config profile
#'
#' Configure your HPC cluster once and reuse it for every project. This builds a
#' Nextflow config from a generic scheduler template, fills in the cluster-level
#' settings you provide (queue, account / clusterOptions, container engine),
#' and saves it as a named profile in [mitopilot_config_dir()]. Afterwards,
#' `new_project(executor = "<name>")` (or `new_project_userAsmb()`) will find and
#' use it automatically, filling in the remaining per-project values.
#'
#' Per-project placeholders (`<<CONTAINER_ID>>`, `<<RAW_DIR>>`, `<<ASMB_DIR>>`,
#' `<<MIN_DEPTH>>`, `<<GENETIC_CODE>>`, `<<NCBI_API_KEY>>`) are intentionally
#' left in the saved profile for the project-init step to complete.
#'
#' @param name Profile name. Saved as `config.<name>`; pass this as the
#'   `executor` argument to `new_project()`.
#' @param scheduler Base template to build on. One of "slurm", "sge", "pbs",
#'   "lsf", "local", or "awsbatch".
#' @param container_engine Container runtime. "auto" picks docker for
#'   local/awsbatch and singularity for HPC schedulers; or set explicitly to
#'   "singularity", "apptainer", or "docker".
#' @param container_cache Optional cacheDir for singularity/apptainer.
#' @param container_run_options Optional runOptions for singularity/apptainer
#'   (e.g. bind mounts).
#' @param queue Partition / queue name. If `NULL`, the queue directive is
#'   omitted (cluster default is used).
#' @param account Optional accounting / project string. Folded into
#'   clusterOptions per scheduler (SLURM `--account=`, PBS `-A`, LSF `-P`,
#'   SGE `-P`).
#' @param cluster_options Optional raw clusterOptions string appended verbatim.
#' @param penv SGE parallel environment (default "mthread"); ignored for other
#'   schedulers.
#' @param profile_dir Directory to save the profile in (default
#'   [mitopilot_config_dir()]).
#' @param interactive If `TRUE` and running interactively, prompt for any unset
#'   queue / account / cluster_options / container values.
#' @param overwrite Overwrite an existing profile of the same name
#'   (default `FALSE`).
#'
#' @return (invisibly) the path to the saved profile.
#' @export
generate_config <- function(
    name,
    scheduler = c("slurm", "sge", "pbs", "lsf", "local", "awsbatch"),
    container_engine = c("auto", "singularity", "apptainer", "docker"),
    container_cache = NULL,
    container_run_options = NULL,
    queue = NULL,
    account = NULL,
    cluster_options = NULL,
    penv = "mthread",
    profile_dir = mitopilot_config_dir(),
    interactive = FALSE,
    overwrite = FALSE) {
  if (missing(name) || !is.character(name) || length(name) != 1 || !nzchar(name)) {
    stop("`name` must be a single non-empty string.", call. = FALSE)
  }
  scheduler <- match.arg(scheduler)
  container_engine <- match.arg(container_engine)

  if (container_engine == "auto") {
    container_engine <- if (scheduler %in% c("local", "awsbatch")) "docker" else "singularity"
  }

  # Optional interactive prompts for unset values ----
  if (isTRUE(interactive) && base::interactive()) {
    prompt_if_null <- function(val, msg) {
      if (!is.null(val)) {
        return(val)
      }
      ans <- trimws(readline(msg))
      if (nzchar(ans)) ans else NULL
    }
    if (scheduler %nin% c("local", "awsbatch")) {
      queue <- prompt_if_null(queue, "Queue / partition (blank = cluster default): ")
      account <- prompt_if_null(account, "Account / project (blank = none): ")
      cluster_options <- prompt_if_null(cluster_options, "Extra clusterOptions (blank = none): ")
    }
  }

  # Fold account into clusterOptions per scheduler ----
  acct_flag <- ""
  if (!is.null(account) && nzchar(account)) {
    acct_flag <- switch(scheduler,
      slurm = paste0("--account=", account),
      pbs = paste0("-A ", account),
      lsf = paste0("-P ", account),
      sge = paste0("-P ", account),
      ""
    )
  }
  cluster_options <- trimws(paste(acct_flag, cluster_options %||% ""))

  # Read base template ----
  template <- app_sys(paste0("config.", scheduler))
  if (!nzchar(template) || !file.exists(template)) {
    stop("Base template not found for scheduler '", scheduler, "'.", call. = FALSE)
  }
  lines <- readLines(template)

  # Drop the queue directive if no queue requested ----
  if (is.null(queue) || !nzchar(queue)) {
    lines <- lines[!grepl("<<QUEUE>>", lines, fixed = TRUE)]
  }

  # Substitute cluster-level placeholders (per-project tokens left intact) ----
  lines <- fill_config(lines, list(
    CONTAINER_ENGINE = container_engine_block(container_engine, container_cache, container_run_options),
    QUEUE = queue,
    CLUSTER_OPTIONS = cluster_options,
    PENV = if (scheduler == "sge") penv else NULL
  ))

  # Write the profile ----
  if (!dir.exists(profile_dir)) {
    dir.create(profile_dir, recursive = TRUE)
  }
  out_path <- file.path(profile_dir, paste0("config.", name))
  if (file.exists(out_path) && !overwrite) {
    stop(
      "Profile '", name, "' already exists at ", out_path,
      ". Use overwrite = TRUE to replace it.",
      call. = FALSE
    )
  }
  writeLines(lines, out_path)

  message("Saved cluster profile '", name, "' to ", out_path)
  message("Use it with: new_project(..., executor = '", name, "')")
  invisible(out_path)
}
