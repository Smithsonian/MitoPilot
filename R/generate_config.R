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

#' Pull a single `key = value` setting out of an existing .config
#'
#' Matches the first `key = value` line (value may be quoted or bare),
#' strips a trailing `// comment` and surrounding quotes.
#'
#' @return The value as a string, or `NULL` if absent / empty.
#' @noRd
config_get_param <- function(lines, key) {
  pat <- paste0("^\\s*", key, "\\s*=\\s*(.*)$")
  hit <- grep(pat, lines, value = TRUE)
  if (length(hit) == 0) return(NULL)
  val <- sub(pat, "\\1", hit[1])
  val <- sub("//.*$", "", val)            # strip trailing comment
  val <- trimws(val)
  val <- gsub("^['\"]|['\"]$", "", val)   # strip surrounding quotes
  if (nzchar(val)) val else NULL
}

#' Reconstruct the container-engine block from an existing .config
#'
#' Finds the first enabled `docker`/`singularity`/`apptainer` block and rebuilds
#' it (preserving cacheDir / runOptions) via [container_engine_block()]. Falls
#' back to a plain `singularity { enabled = true }` block (the HPC default).
#'
#' @param lines Character vector of the old .config.
#' @return A single string for the `<<CONTAINER_ENGINE>>` placeholder.
#' @noRd
extract_container_engine <- function(lines) {
  for (eng in c("singularity", "apptainer", "docker")) {
    start <- grep(paste0("^\\s*", eng, "\\s*\\{"), lines)
    if (length(start) == 0) next
    start <- start[1]
    end <- length(lines)
    for (j in (start + 1):length(lines)) {
      if (grepl("^\\s*\\}", lines[j])) { end <- j; break }
    }
    block <- lines[start:end]
    if (!any(grepl("enabled\\s*=\\s*true", block))) next
    cache <- config_get_param(block, "cacheDir")
    run_options <- config_get_param(block, "runOptions")
    return(container_engine_block(eng, cache, run_options))
  }
  container_engine_block("singularity")
}

#' Regenerate a project's .config from the current built-in template
#'
#' Backwards-compatibility helper. Rather than patch an old `.config` line by
#' line, this detects the executor from the existing config, regenerates a fresh
#' config from the matching built-in template (so every current section is
#' present), then ports the project-specific values across (raw/asmb dirs, min
#' depth, genetic code, NCBI key, queue, clusterOptions, container engine). The
#' container is bumped to the current package version. The old config is backed
#' up to a timestamped `.config.bak.<ts>` before the new one is written.
#'
#' If the executor cannot be detected, or regeneration would leave unfilled
#' placeholders, the old config is left untouched and a warning is issued.
#'
#' @param path Project directory containing `.config`.
#' @param con Optional open DB connection, used to default `genetic_code` from
#'   the samples table when the old config does not set it.
#'
#' @return (invisibly) `TRUE` if the config was regenerated, `FALSE` otherwise.
#' @noRd
migrate_config <- function(path, con = NULL) {
  conf_path <- file.path(path, ".config")
  old <- readLines(conf_path)

  # Detect executor: a quoted literal (ignores `executor = process.executor`).
  hits <- regmatches(
    old, regexpr("executor\\s*=\\s*['\"][A-Za-z0-9_]+['\"]", old)
  )
  exec <- if (length(hits) > 0) {
    sub(".*['\"]([A-Za-z0-9_]+)['\"].*", "\\1", hits[1])
  } else {
    NA_character_
  }
  builtin <- c("local", "slurm", "sge", "pbs", "lsf", "awsbatch")
  if (is.na(exec) || !(exec %in% builtin)) {
    warning(
      "Could not detect a known executor in .config; leaving .config unchanged. ",
      "Regenerate it manually with new_project(..., force = TRUE) if needed.",
      call. = FALSE
    )
    return(invisible(FALSE))
  }

  template <- app_sys(paste0("config.", exec))
  if (!nzchar(template) || !file.exists(template)) {
    warning("No built-in template for executor '", exec,
            "'; leaving .config unchanged.", call. = FALSE)
    return(invisible(FALSE))
  }
  lines <- readLines(template)

  # Port project-specific values from the old config ----
  raw_dir   <- config_get_param(old, "rawDir")
  asmb_dir  <- config_get_param(old, "asmbDir") %||% "NA"
  min_depth <- config_get_param(old, "minDepth")
  gcode     <- config_get_param(old, "genetic_code")
  api_key   <- config_get_param(old, "ncbi_api_key")
  queue     <- config_get_param(old, "queue")
  cluster_options <- config_get_param(old, "clusterOptions")

  # Default genetic_code from the (already-migrated) samples table if absent.
  if (is.null(gcode) && !is.null(con)) {
    gcode <- tryCatch({
      v <- DBI::dbGetQuery(
        con,
        "SELECT genetic_code FROM samples WHERE genetic_code IS NOT NULL LIMIT 1"
      )$genetic_code
      if (length(v) == 1) as.character(as.integer(v)) else NULL
    }, error = function(e) NULL)
  }

  engine_repl <- NULL
  if (any(grepl("<<CONTAINER_ENGINE>>", lines, fixed = TRUE))) {
    engine_repl <- extract_container_engine(old)
  }

  new_container <- paste0("macguigand/mitopilot:", utils::packageVersion("MitoPilot"))

  # Drop the queue directive if the old config had no queue (mirror generate_config).
  if (is.null(queue)) {
    lines <- lines[!grepl("<<QUEUE>>", lines, fixed = TRUE)]
  }

  lines <- fill_config(lines, list(
    CONTAINER_ID     = new_container,
    RAW_DIR          = raw_dir %||% "NA",
    ASMB_DIR         = asmb_dir,
    MIN_DEPTH        = min_depth %||% "2000000",
    GENETIC_CODE     = gcode %||% "2",
    NCBI_API_KEY     = api_key %||% "",
    QUEUE            = queue,
    CLUSTER_OPTIONS  = cluster_options %||% "",
    CONTAINER_ENGINE = engine_repl
  ))

  # Fail safe: never write a config that still has unfilled placeholders.
  if (any(grepl("<<[A-Z_]+>>", lines))) {
    warning("Config regeneration left unfilled placeholders; leaving .config unchanged.",
            call. = FALSE)
    return(invisible(FALSE))
  }

  ts <- format(Sys.time(), "%Y%m%d-%H%M%S")
  backup <- file.path(path, paste0(".config.bak.", ts))
  file.copy(conf_path, backup, overwrite = FALSE)
  writeLines(lines, conf_path)
  message("regenerated .config from built-in '", exec,
          "' template (old config backed up to ", basename(backup), ")")
  invisible(TRUE)
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
