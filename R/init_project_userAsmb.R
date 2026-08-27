#' Initialize new MitoPilot Project with user-provided mitogenome assemblies
#'
#' @param path Path to the project directory (default = current working
#'   directory)
#' @param mapping_fn Path to a mapping file. Should be a csv that minimally
#'   includes an `ID` column with a unique identifier for each sample, a `Taxon`
#'   column containing taxonomic information for each sample, and columns
#'   `R1` and `R2` specifying the names of the raw paired read inputs, and an `Assembly`
#'   column containing names of assembly fasta files. An optional `Topology` column
#'   ("circular" or "linear") declares the topology of a single-contig assembly; an
#'   assembly holding more than one contig is recorded as "multi" and any declaration
#'   is ignored. May include additional columns with other sample metadata.
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
#' @param find_mitogenome (logical) Search each supplied assembly for its
#'   mitochondrial contigs before the rest of WF1 runs (default = FALSE). Use
#'   this when your FASTA files hold whole assemblies rather than a mitogenome:
#'   contigs are BLASTed against the bundled metazoan mitogenome database, the
#'   survivors confirmed with MitoFinder, and only those carried forward. See
#'   [find_mito()].
#' @param mitofinder_db Path to a MitoFinder GenBank database, built with
#'   [custom_assembly_db()] (`db_type = "mitofinder"`). Required when
#'   `find_mitogenome = TRUE`.
#' @param attempt_circularization (logical) Attempt to circularize linear,
#'   single-contig user assemblies during WF1 (default = FALSE). Redundant
#'   overlap between the contig ends is trimmed, and when raw reads are
#'   available the new junction must be supported by reads before the assembly
#'   is called circular. Settings are editable later in the app's
#'   circularization options modal. See [circularize_asmb()].
#' @param join_scaffolds (logical) Order a fragmented single-path assembly
#'   against its BLAST reference into one joined sequence during WF1 (default =
#'   FALSE). Samples whose contigs match different reference mitogenomes are
#'   left alone. Because eligibility here is any multi-contig assembly rather
#'   than just mitogenome scaffolds, use this alongside `find_mitogenome = TRUE`
#'   so the join sees only confirmed mitochondrial contigs.
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
    find_mitogenome = FALSE,
    mitofinder_db = NULL,
    attempt_circularization = FALSE,
    join_scaffolds = FALSE,
    executor = c("local", "awsbatch", "slurm", "sge", "pbs", "lsf", "NMNH_Hydra", "NOAA_SEDNA"),
    container = paste0("macguigand/mitopilot:", utils::packageVersion("MitoPilot")),
    config = NULL,
    profile_dir = mitopilot_config_dir(),
    ncbi_api_key = NULL,
    Rproj = TRUE,
    force = FALSE,
    ...) {

  # Fail early on an unsupported Nextflow (see README "Nextflow compatibility").
  check_nextflow_version("new_project_userAsmb")

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
  if(!no_raw_data && length(data_path)==1){
    data_path <- normalizePath(data_path)
  }

  # Normalize assembly path (if provided)----
  if(length(assembly_path)==1){
    assembly_path <- normalizePath(assembly_path)
  }

  # The mitogenome search cannot confirm anything without a MitoFinder
  # reference, so refuse at project creation rather than at the end of a run.
  if (isTRUE(find_mitogenome)) {
    if (is.null(mitofinder_db) || !nzchar(mitofinder_db) || !file.exists(mitofinder_db)) {
      stop(
        "find_mitogenome = TRUE requires a MitoFinder reference database.\n",
        "Build one for your clade with:\n",
        "  custom_assembly_db(clade = \"<your clade>\", db_type = \"mitofinder\")\n",
        "then pass its .gb file as mitofinder_db.",
        call. = FALSE
      )
    }
    mitofinder_db <- normalizePath(mitofinder_db)
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
    assembly_path = assembly_path,
    no_raw_data = no_raw_data,
    attempt_circularization = attempt_circularization,
    join_scaffolds = join_scaffolds,
    find_mitogenome = find_mitogenome,
    mitofinder_db = mitofinder_db,
    ...
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
      MIN_DEPTH = format(2000000, scientific = FALSE),
      NCBI_API_KEY = ncbi_api_key %||% ""
    )) |>
    writeLines(file.path(path, ".config"))

  message("Project initialized successfully.")
  message("Please open and review the .config file to ensure all required options are specified.")
}
