#' Initialize new MitoPilot Project with user-provided mitogenome assemblies
#'
#' @param path Path to the project directory (default = current working
#'   directory)
#' @param mapping_fn Path to a mapping file. Should be a csv that minimally
#'   includes an `ID` column with a unique identifier for each sample, a `Taxon`
#'   column containing taxonomic information for each sample, and columns
#'   `R1` and `R2` specifying the names of the raw paired read inputs, an `Assembly` column
#'   containing names of mitogenome assembly fasta files (one contig/scaffold sequence per sample),
#'   and a `Topology` column containing information about the assembly topology
#'   ("circular" or "linear") May include additional columns with other sample metadata.
#' @param mapping_id The name of the column in the mapping file that contains
#'   the unique sample identifiers (default = "ID").
#' @param data_path Path to the directory where the raw data is located. Can be
#'   a AWS s3 bucket even if not using AWS for pipeline execution.
#' @param assembly_path Path to the directory where the mitogenome assemblies are located. Can be
#'   a AWS s3 bucket even if not using AWS for pipeline execution.
#' @param genetic_code Translation table for your organisms. See NCBI website
#'   for more info https://www.ncbi.nlm.nih.gov/Taxonomy/Utils/wprintgc.cgi
#' @param executor The executor to use for running the nextflow pipeline. Must
#'   be one of "local" (default) or "awsbatch", "NMNH_Hydra", or "NOAA_SEDNA".
#' @param Rproj (logical) Initialize and open an RStudio project in the project
#'   directory (default = TRUE). This option has no effect if not running
#'   interactively in RStudio.
#' @param force (logical) Force recreating of existing project database and
#'   config files (default = FALSE).
#' @param config (optional) provide a path to an existing custom nextflow config
#'   file. If not provided a config file template will be created based on the
#'   specified executor.
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
new_project_userAsmb <- function(
    path = ".",
    mapping_fn = NULL,
    mapping_id = "ID",
    data_path = NULL,
    assembly_path = "NA",
    genetic_code = 2,
    executor = c("local", "awsbatch", "NMNH_Hydra", "NOAA_SEDNA"),
    container = paste0("macguigand/mitopilot:", utils::packageVersion("MitoPilot")),
    config = NULL,
    ncbi_api_key = NULL,
    Rproj = TRUE,
    force = FALSE,
    ...) {

  # Create directory if it doesn't exist ----
  if (!dir.exists(path)) {
    message("Creating project directory: ", path)
    dir.create(path, recursive = TRUE)
  }
  path <- normalizePath(path)

  # Normalize data path (if provided)----
  if(length(data_path)==1){
    data_path <- normalizePath(data_path)
  }

  # Normalize assembly path (if provided)----
  if(length(assembly_path)==1){
    assembly_path <- normalizePath(assembly_path)
  }

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

  new_db_userAsmb(
    db_path = file.path(path, ".sqlite"),
    genetic_code = genetic_code,
    mapping_fn = mapping_out,
    mapping_id = mapping_id,
    ...
  )


  # Config file ----
  config <- config %||% app_sys(paste0("config.", executor))
  if (!file.exists(config)) {
    stop("Config file not found.")
    return()
  }
  readLines(config) |>
    stringr::str_replace("<<CONTAINER_ID>>", container %||% "<<CONTAINER_ID>>") |>
    stringr::str_replace("<<RAW_DIR>>", data_path %||% "<<RAW_DIR>>") |>
    stringr::str_replace("<<ASMB_DIR>>", assembly_path %||% "<<ASMB_DIR>>") |>
    stringr::str_replace("<<MIN_DEPTH>>", format(2000000 %||% "<<MIN_DEPTH>>", scientific = F)) |>
    stringr::str_replace("<<GENETIC_CODE>>", format(genetic_code %||% "<<GENETIC_CODE>>", scientific = F)) |>
    stringr::str_replace("<<NCBI_API_KEY>>", ncbi_api_key %||% "") |>
    writeLines(file.path(path, ".config"))

  message("Project initialized successfully.")
  message("Please open and review the .config file to ensure all required options are specified.")
}
