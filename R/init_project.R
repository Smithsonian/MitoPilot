#' Initialize new MitoPilot Project
#'
#' @param path Path to the project directory (default = current working
#'   directory)
#' @param mapping_fn Path to a mapping file. Should be a csv that minimally
#'   includes an `ID` column with a unique identifier for each sample and
#'   columns `fwd` and `rev` specifying the names of the raw paired read inputs.
#' @param mapping_id The name of the column in the mapping file that contains
#'   the unique sample identifiers (default = "ID").
#' @param executor The executor to use for running the nextflow pipeline. Must
#'   be one of "local" or "awsbatch".
#' @param Rproj (logical) Initialize and open an RStudio project in the project
#'   directory (default = TRUE). This option has no effect if not running
#'   interactively in RStudio.
#' @param force (logical) Force recreating of existing project database and
#'   config files (default = FALSE).
#' @param config (optional) provide a path to an existing custom nextflow config
#'   file. If not provided a config file template will be created based on the
#'   specified executor.
#'
#' @export
#'
new_project <- function(
  path = here::here(),
  mapping_fn = NULL,
  mapping_id = "ID",
  executor = NULL,
  container = 'drleopold/mitopilot:dev',
  config = NULL,
  Rproj = TRUE,
  force = FALSE
  ){

  # Validate executor ----
  if(executor %nin% c("local", "awsbatch")){
    stop("Invalid executor.")
  }

  # Create directory if it doesn't exist ----
  if (!dir.exists(path)) {
    message("Creating project directory: ", path)
    dir.create(path, recursive = TRUE)
  }
  path <- normalizePath(path)

  # Initialize RStudio Project ----
  # (optional & only if running form RStudio)
  if(Rproj && !isFALSE(Sys.getenv("RSTUDIO", FALSE))){
    if(isFALSE(requireNamespace('rstudioapi', quietly = TRUE))){
      message("package 'rstudioapi' not available. Skipping RStudio project initialization.")
    }else{
      rstudioapi::initializeProject(path)
      on.exit(rstudioapi::openProject(path, newSession = TRUE))
    }
  }

  # Read mapping file ----
  if(is.null(mapping_fn) || !file.exists(mapping_fn)){
    stop("A mapping file is required to initialize a new project")
  }
  mapping_out <- file.path(path, "mapping.csv")
  if(!identical(mapping_fn, mapping_out)){
    file.copy(mapping_fn, mapping_out)
  }

  # Initialize sqlite db ----
  db <- file.path(path, ".sqlite")
  if(file.exists(db) && !force){
    message("Database already exists. Use force = TRUE to overwrite (old data will be lost).")
    return()
  }
  if(file.exists(db) && force){
    message("Overwriting existing database")
    file.remove(db)
  }
  new_db(
    db_path = file.path(path, ".sqlite"),
    mapping_fn = mapping_out,
    mapping_id = mapping_id
  )

  # Config file ----
  config <- config %||% app_sys(paste0("config.", executor))
  if(!file.exists(config)){
    stop("Config file not found.")
    return()
  }
  readLines(config) |>
    stringr::str_replace("<<CONTAINER_ID>>", container %||% "<<CONTAINER_ID>>") |>
    writeLines(file.path(path, ".config"))

  message("Project initialized successfully.")
  message("Please open and review the .config file to ensure all required options are specified.")

}
