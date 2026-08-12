#' Initialize new MitoPilot Project
#'
#' @param path Path to the project directory (default = current working
#'   directory)
#' @param mapping_fn Path to a mapping file. Should be a csv that minimally
#'   includes an `ID` column with a unique identifier for each sample, a `Taxon`
#'   column containing taxonomic information for each sample, and columns
#'   `R1` and `R2` specifying the names of the raw paired read inputs. May include
#'   additional columns with other sample metadata.
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
  init_project_core(
    path = path,
    mapping_fn = mapping_fn,
    mapping_id = mapping_id,
    data_path = data_path,
    genetic_code = genetic_code,
    executor = executor,
    container = container,
    config = config,
    profile_dir = profile_dir,
    ncbi_api_key = ncbi_api_key,
    Rproj = Rproj,
    force = force,
    context = "new_project",
    min_depth = min_depth,
    userAsmb = FALSE,
    # list() keeps NULL entries, so an unset custom db still overrides new_db's default
    db_args = c(list(seeds_db = custom_seeds_db, labels_db = custom_labels_db), list(...))
  )
}

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
#'   a AWS s3 bucket even if not using AWS for pipeline execution. Not required
#'   when `no_raw_data = TRUE`.
#' @param no_raw_data (logical) Run without raw sequence data (default = FALSE).
#'   When TRUE, the read-mapping coverage step is skipped: `data_path` is
#'   ignored, coverage/depth statistics are left empty, and annotation coverage
#'   trimming is disabled. Use this to annotate an assembly you already have.
#' @param assembly_path Path to the directory where the mitogenome assemblies are located. Can be
#'   a AWS s3 bucket even if not using AWS for pipeline execution.
#' @param genetic_code Optional NCBI translation table override. Default `NULL`
#'   auto-selects from each sample's curation ruleset; a number sets a
#'   project-wide override. https://www.ncbi.nlm.nih.gov/Taxonomy/Utils/wprintgc.cgi
#' @param executor The executor to use for running the nextflow pipeline. May be
#'   a built-in template ("local" (default), "awsbatch", "slurm", "sge", "pbs",
#'   "lsf", "NMNH_Hydra", "NOAA_SEDNA") or the name of a saved cluster profile
#'   created with [generate_config()]. See [list_configs()] for available names.
#' @param Rproj (logical) Initialize and open an RStudio project in the project
#'   directory (default = TRUE). This option has no effect if not running
#'   interactively in RStudio.
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
new_project_userAsmb <- function(
    path = ".",
    mapping_fn = NULL,
    mapping_id = "ID",
    data_path = NULL,
    no_raw_data = FALSE,
    assembly_path = "NA",
    genetic_code = NULL,
    executor = c("local", "awsbatch", "slurm", "sge", "pbs", "lsf", "NMNH_Hydra", "NOAA_SEDNA"),
    container = paste0("macguigand/mitopilot:", utils::packageVersion("MitoPilot")),
    config = NULL,
    profile_dir = mitopilot_config_dir(),
    ncbi_api_key = NULL,
    Rproj = TRUE,
    force = FALSE,
    ...) {
  init_project_core(
    path = path,
    mapping_fn = mapping_fn,
    mapping_id = mapping_id,
    data_path = data_path,
    genetic_code = genetic_code,
    executor = executor,
    container = container,
    config = config,
    profile_dir = profile_dir,
    ncbi_api_key = ncbi_api_key,
    Rproj = Rproj,
    force = force,
    context = "new_project_userAsmb",
    assembly_path = assembly_path,
    no_raw_data = no_raw_data,
    userAsmb = TRUE,
    db_args = list(...)
  )
}

#' Shared implementation for new_project() and new_project_userAsmb()
#'
#' @param context Name of the calling function, used in version warnings.
#' @param userAsmb (logical) Build a user-assembly project.
#' @param db_args Extra arguments forwarded to the database initializer.
#' @noRd
init_project_core <- function(
    path,
    mapping_fn,
    mapping_id,
    data_path,
    genetic_code,
    executor,
    container,
    config,
    profile_dir,
    ncbi_api_key,
    Rproj,
    force,
    context,
    min_depth = 2000000,
    assembly_path = "NA",
    no_raw_data = FALSE,
    userAsmb = FALSE,
    db_args = list()) {
  # Fail early on an unsupported Nextflow (see README "Nextflow compatibility").
  check_nextflow_version(context)

  # Create directory if it doesn't exist ----
  if (!dir.exists(path)) {
    message("Creating project directory: ", path)
    dir.create(path, recursive = TRUE)
  }
  path <- normalizePath(path)

  # No raw data mode: skip reads/coverage. data_path is not needed; RAW_DIR is
  # pinned to the "NA" sentinel so the pipeline runs the no-reads coverage path.
  if (no_raw_data) {
    data_path <- "NA"
    message("no_raw_data = TRUE: skipping read mapping and coverage calculation.")
  }

  # Normalize data path (if provided)----
  if (!no_raw_data && length(data_path) == 1) {
    data_path <- normalizePath(data_path)
  }

  # Normalize assembly path (if provided)----
  # Standard projects keep the literal "NA" sentinel.
  if (userAsmb && length(assembly_path) == 1) {
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
  # Accepts a built-in template, a saved cluster profile (see generate_config),
  # or an explicit `config` path. Resolution is deferred to resolve_config().
  executor <- executor[1]
  if (is.null(config) && (is.null(executor) || !nzchar(executor))) {
    stop("Invalid executor.")
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
  if (file.exists(db) && !force) {
    message("Database already exists. Use force = TRUE to overwrite (old data will be lost).")
    return()
  }
  if (file.exists(db) && force) {
    message("Overwriting existing database")
    file.remove(db)
  }

  do.call(
    if (userAsmb) new_db_userAsmb else new_db,
    c(
      list(
        db_path = file.path(path, ".sqlite"),
        genetic_code = genetic_code,
        mapping_fn = mapping_out,
        mapping_id = mapping_id
      ),
      if (userAsmb) list(no_raw_data = no_raw_data),
      db_args
    )
  )


  # Config file ----
  # Resolve a saved profile / built-in template (or use an explicit path),
  # then fill in the per-project placeholders.
  config <- config %||% resolve_config(executor, profile_dir = profile_dir)
  if (!file.exists(config)) {
    stop("Config file not found.")
    return()
  }
  readLines(config) |>
    fill_config(list(
      CONTAINER_ID = container,
      RAW_DIR = data_path,
      ASMB_DIR = assembly_path,
      MIN_DEPTH = format(min_depth, scientific = FALSE),
      NCBI_API_KEY = ncbi_api_key %||% ""
    )) |>
    writeLines(file.path(path, ".config"))

  message("Project initialized successfully.")
  message("Please open and review the .config file to ensure all required options are specified.")
}
