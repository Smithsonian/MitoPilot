#' Initialize new MitoPilot Project
#'
#' @param path Path to the project directory (default = current working
#'   directory)
#' @param mapping_fn Path to a mapping file. Should be a csv that minimally
#'   includes an `ID` column with a unique identifier for each sample, a `Taxon`
#'   column containing taxonomic information for each sample, and columns
#'   `R1` and `R2` specifying the names of the raw paired read inputs.
#'   May include additional columns with other sample metadata, and an optional
#'   \code{Reference} column naming a per-sample MapToRef reference (file path,
#'   URL, or NCBI accession). A FASTA reference also needs a
#'   \code{Reference_topology} column (circular or linear). Both values are
#'   stored on the sample and used when its parameter set assembles with
#'   MapToRef. \code{Reference} is a reserved column name: it is never stored
#'   as sample metadata, so rename the column if you use it for something else.
#' @param mapping_id The name of the column in the mapping file that contains
#'   the unique sample identifiers (default = "ID").
#' @param data_path Path to the directory where the raw data is located. Can be
#'   a AWS s3 bucket even if not using AWS for pipeline execution..
#' @param min_depth Minimum number of paired sequences after pre-processing to proceed
#'   with assembly (default: 2000000 reads)
#' @param genetic_code Optional NCBI translation table override. Default `NULL`
#'   auto-selects the genetic code from each sample's curation ruleset. Supplying
#'   a number sets a project-wide override on the default curation options. See
#'   https://www.ncbi.nlm.nih.gov/Taxonomy/Utils/wprintgc.cgi
#' @param executor The executor to use for running the nextflow pipeline. May be
#'   a built-in template ("local" (default), "awsbatch", "slurm", "sge", "pbs",
#'   "lsf", "NMNH_Hydra", "NOAA_SEDNA") or the name of a saved cluster profile
#'   created with [generate_config()]. See [list_configs()] for available names.
#' @param Rproj (logical) Initialize and open an RStudio project in the project
#'   directory (default = TRUE). This option has no effect if not running
#'   interactively in RStudio.
#' @param custom_seeds_db Full path to custom seeds database for GetOrganelle
#' @param custom_labels_db Full path to custom labels database for GetOrganelle
#' @param force (logical) Force recreating of existing project database and
#'   config files (default = FALSE).
#' @param config (optional) provide a path to an existing custom nextflow config
#'   file. If not provided a config file template will be created based on the
#'   specified executor.
#' @param profile_dir Directory searched for saved cluster profiles when
#'   resolving `executor` (default [mitopilot_config_dir()]).
#' @param container The docker container to use for pipeline execution.
#' @param ncbi_api_key Optional NCBI API key string. Used to raise NCBI request
#'   rate limits for the remote BLAST + GenBank fetch steps. See
#'   <https://www.ncbi.nlm.nih.gov/datasets/docs/v2/api/api-keys/>. May be left
#'   empty and edited later in `.config` (`params.ncbi_api_key`).
#' @param ... Additional arguments passed as default processing parameters to
#'   `new_db()`
#'
#' @export
#'
new_project <- function(
    path = ".",
    mapping_fn = NULL,
    mapping_id = "ID",
    data_path = NULL,
    min_depth = 2000000,
    genetic_code = NULL,
    executor = c("local", "awsbatch", "slurm", "sge", "pbs", "lsf", "NMNH_Hydra", "NOAA_SEDNA"),
    container = paste0("macguigand/mitopilot:", utils::packageVersion("MitoPilot")),
    custom_seeds_db = NULL,
    custom_labels_db = NULL,
    config = NULL,
    profile_dir = mitopilot_config_dir(),
    ncbi_api_key = NULL,
    Rproj = TRUE,
    force = FALSE,
    ...) {
  # Fail early on an unsupported Nextflow (see README "Nextflow compatibility").
  check_nextflow_version("new_project")

  executor <- executor[1]
  dots <- list(...)
  preflight_project(
    path = path, mapping_fn = mapping_fn, mapping_id = mapping_id,
    data_path = data_path, executor = executor, config = config,
    profile_dir = profile_dir, container = container, min_depth = min_depth,
    genetic_code = genetic_code, ncbi_api_key = ncbi_api_key,
    custom_seeds_db = custom_seeds_db, custom_labels_db = custom_labels_db,
    force = force, db_fun = new_db, dots = dots
  )

  # Create directory if it doesn't exist ----
  if (!dir.exists(path)) {
    message("Creating project directory: ", path)
    dir.create(path, recursive = TRUE)
  }
  path <- normalizePath(path)
  if (length(data_path) == 1) {
    data_path <- normalizePath(data_path, mustWork = FALSE)
  }
  mapping_out <- file.path(path, "mapping.csv")
  if (!identical(normalizePath(mapping_fn), normalizePath(mapping_out, mustWork = FALSE))) {
    file.copy(mapping_fn, mapping_out, overwrite = TRUE)
  }

  # Initialize RStudio Project ----
  # (optional & only if running form RStudio)
  if (Rproj && !isFALSE(Sys.getenv("RSTUDIO", FALSE))) {
    if (isFALSE(requireNamespace("rstudioapi", quietly = TRUE))) {
      message("package 'rstudioapi' not available. Skipping RStudio project initialization.")
    } else {
      rstudioapi::initializeProject(path)
      on.exit(rstudioapi::openProject(path, newSession = TRUE))
    }
  }

  # Initialize sqlite db ----
  db <- file.path(path, ".sqlite")
  if (file.exists(db)) {
    message("Overwriting existing database")
    file.remove(db)
  }

  new_db(
    db_path = file.path(path, ".sqlite"),
    genetic_code = genetic_code,
    mapping_fn = mapping_out,
    mapping_id = mapping_id,
    seeds_db = custom_seeds_db,
    labels_db = custom_labels_db,
    ...
  )


  # Config file ----
  # Resolve a saved profile / built-in template (or use an explicit path),
  # then fill in the per-project placeholders.
  config <- config %||% resolve_config(executor, profile_dir = profile_dir)
  readLines(config) |>
    fill_config(list(
      CONTAINER_ENGINE = container_engine_block(
        if (executor %in% c("local", "awsbatch")) "docker" else "singularity"),
      CONTAINER_ID = container,
      RAW_DIR = data_path,
      ASMB_DIR = "NA",
      MIN_DEPTH = format(min_depth, scientific = FALSE),
      NCBI_API_KEY = ncbi_api_key %||% ""
    )) |>
    writeLines(file.path(path, ".config"))

  message("Project initialized: ", path)
  message(open_app_hint(path))
}

#' How to open the app, worded for the session at hand
#'
#' No browser can open on a Linux box with no display or reached over SSH, so
#' point those users at the tunnel form.
#' @noRd
open_app_hint <- function(path) {
  headless <- Sys.info()[["sysname"]] == "Linux" &&
    (nzchar(Sys.getenv("SSH_CONNECTION")) || !nzchar(Sys.getenv("DISPLAY"))) &&
    !nzchar(Sys.getenv("RSTUDIO"))
  if (headless) {
    paste0("To open the app from this cluster session, run:\n",
           "  setwd(\"", path, "\")\n",
           "  MitoPilot(host = \"0.0.0.0\", port = 7591, launch.browser = FALSE)\n",
           "then open the SSH tunnel it prints and visit http://localhost:7591 ",
           "(see vignette(\"Custom-HPC\")).")
  } else {
    paste0("To open the app, run:\n  setwd(\"", path, "\")\n  MitoPilot()")
  }
}
