#' Initialize new MitoPilot Project
#'
#' @param path Path to the project directory (default = current working
#'   directory)
#' @param mapping_fn Path to a mapping file. Should be a csv that minimally
#'   includes an `ID` column with a unique identifier for each sample and
#'   columns `fwd` and `rev` specifying the names of the raw paired read inputs.
#' @param mapping_id The name of the column in the mapping file that contains
#'   the unique sample identifiers (default = "ID").
#' @param data_path Path to the directory where the raw data is located. Can be
#'   a AWS s3 bucket even if not using AWS for pipeline execution..
#' @param min_depth Minimum sequencing depth after pre-processing to proceed
#'   with assembly (default: 2000000)
#' @param executor The executor to use for running the nextflow pipeline. Must
#'   be one of "local" (default) or "awsbatch", "NMNH_Hydra", or "NOAA_SEDNA".
#' @param Rproj (logical) Initialize and open an RStudio project in the project
#'   directory (default = TRUE). This option has no effect if not running
#'   interactively in RStudio.
#' @param force (logical) Force recreating of existing project database and
#'   config files (default = FALSE).
#' @param custom_seeds_db (optional) full path to a custom GetOrganelle seeds
#'   database. See https://github.com/Kinggerm/GetOrganelle/wiki/FAQ#general
#'   for information about custom databases.
#' @param custom_labels_db (optional) full path to a custom GetOrganelle labels
#'   database. See https://github.com/Kinggerm/GetOrganelle/wiki/FAQ#general
#'   for information about custom databases.
#' @param config (optional) provide a path to an existing custom nextflow config
#'   file. If not provided a config file template will be created based on the
#'   specified executor.
#' @param container The docker container to use for pipeline execution.
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
    executor = c("local", "awsbatch", "NMNH_Hydra", "NOAA_SEDNA"),
    container = "drleopold/mitopilot",
    custom_seeds_db = NULL,
    custom_labels_db = NULL,
    config = NULL,
    Rproj = TRUE,
    force = FALSE,
    ...) {

  # Read mapping file ----
  if (is.null(mapping_fn) || !file.exists(mapping_fn)) {
    stop("A mapping file is required to initialize a new project")
  }
  mapping_out <- file.path(path, "mapping.csv")
  if (!identical(mapping_fn, mapping_out)) {
    file.copy(mapping_fn, mapping_out)
  }

  # Validate executor ----
  executor <- executor[1]
  if (is.null(executor) || executor %nin% c("local", "awsbatch", "NMNH_Hydra", "NOAA_SEDNA")) {
    stop("Invalid executor.")
  }

  # Create directory if it doesn't exist ----
  if (!dir.exists(path)) {
    message("Creating project directory: ", path)
    dir.create(path, recursive = TRUE)
  }

  # Copy custom databases into project directory, if needed
  if (!file.exists(custom_seeds_db)) {
    stop("Custom seed database does not exist")
  }
  if (!file.exists(custom_labels_db)) {
    stop("Custom labels database does not exist")
  }
  if(!is.null(custom_seeds_db) | !is.null(custom_labels_db)){
    if (!dir.exists(paste0(path,"/work/ref_dbs"))) {
      message("Creating ref database directory: ", paste0(path, "/ref_dbs"))
      dir.create(paste0(path, "/work/ref_dbs/getOrganelle/seeds"), recursive = TRUE)
      dir.create(paste0(path, "/work/ref_dbs/getOrganelle/labels"), recursive = TRUE)
      if(!is.null(custom_seeds_db)){
        file.copy(custom_seeds_db, paste0(path, "/work/ref_dbs/getOrganelle/seeds"))
      }
      if(!is.null(custom_labels_db)){
        file.copy(custom_labels_db, paste0(path, "/work/ref_dbs/getOrganelle/labels"))
      }
    }
  }
  path <- normalizePath(path)

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
  if (file.exists(db) && !force) {
    message("Database already exists. Use force = TRUE to overwrite (old data will be lost).")
    return()
  }
  if (file.exists(db) && force) {
    message("Overwriting existing database")
    file.remove(db)
  }
  if(!is.null(custom_seeds_db) | !is.null(custom_labels_db)){
    # if using custom GetOrganelle databases
    new_db(
      db_path = file.path(path, ".sqlite"),
      mapping_fn = mapping_out,
      mapping_id = mapping_id,
      seeds_db = paste0(path, "/work/ref_dbs/getOrganelle/seeds/", basename(custom_seeds_db)),
      labels_db = paste0(path, "/work/ref_dbs/getOrganelle/labels/", basename(custom_labels_db)),
      ...
    )
  } else {
    new_db(
      db_path = file.path(path, ".sqlite"),
      mapping_fn = mapping_out,
      mapping_id = mapping_id,
      ...
    )
  }

  # Config file ----
  config <- config %||% app_sys(paste0("config.", executor))
  if (!file.exists(config)) {
    stop("Config file not found.")
    return()
  }
  readLines(config) |>
    stringr::str_replace("<<CONTAINER_ID>>", container %||% "<<CONTAINER_ID>>") |>
    stringr::str_replace("<<RAW_DIR>>", data_path %||% "<<RAW_DIR>>") |>
    stringr::str_replace("<<MIN_DEPTH>>", format(min_depth %||% "<<MIN_DEPTH>>", scientific = F)) |>
    writeLines(file.path(path, ".config"))

  message("Project initialized successfully.")
  message("Please open and review the .config file to ensure all required options are specified.")
}
