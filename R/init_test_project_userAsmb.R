#' Initialize a test project for user-supplied assemblies
#'
#' Sets up a test project for the user-assembly workflow, with nine samples
#' that between them cover the shapes an assembly can arrive in: a linear
#' mitogenome, a circular one, one that is circular but reported as linear, and
#' six multi-contig assemblies: one holding a single mitogenome, one holding
#' none, one holding two from different species, one holding a mitogenome that
#' needs circularizing, one holding two mitogenomes of which one needs
#' circularizing, and one holding a mitogenome split across three contigs. The
#' assemblies ship with the package; the raw reads are the same fish data used
#' by [new_test_project()].
#'
#' The project is created with the mitogenome search, the circularization
#' attempt, and scaffold joining all switched on, since that is what these
#' samples are for.
#'
#' @param path path to the directory for the test project (default = current
#'   working directory). Will be created if it does not already exist.
#' @param n how many samples to include in the test project (Default = Inf,
#'   include all)
#' @param full_size (logical) Use the full size raw reads (default = FALSE).
#'   Setting to TRUE will download the reads from ENA, which will require
#'   several GB and will take some time to complete. By default the smaller
#'   pre-filtered read files packaged with MitoPilot are used.
#' @param executor The executor to use for running the nextflow pipeline. A
#'   built-in template ("local" (default), "awsbatch", "slurm", "sge", "pbs",
#'   "lsf", "NMNH_Hydra", "NOAA_SEDNA") or a saved profile from
#'   [generate_config()]. See [list_configs()].
#' @param container The container to use for running the pipeline.
#' @param Rproj (logical) Initialize and open an RStudio project in the project
#'   directory (default = TRUE). This has no effect if not running
#'   interactively in RStudio.
#' @param force (logical) Force recreating of existing project database and
#'   config files (default = FALSE).
#' @param ... Additional arguments passed to [new_project_userAsmb()]
#'
#' @export
new_test_project_userAsmb <- function(
    path = ".",
    n = Inf,
    full_size = FALSE,
    executor = "local",
    container = paste0("macguigand/mitopilot:", utils::packageVersion("MitoPilot")),
    Rproj = TRUE,
    force = FALSE,
    ...) {

  # Fail early on an unsupported Nextflow (see README "Nextflow compatibility").
  check_nextflow_version("new_test_project_userAsmb")

  if (!dir.exists(path)) {
    message("Creating project directory: ", path)
    dir.create(path, recursive = TRUE)
  }
  path <- normalizePath(path)

  mapping <- app_sys(file.path("test_data", "mapping_test_userAsmb.csv")) |>
    utils::read.csv() |>
    dplyr::slice_head(n = n)

  dir.create(file.path(path, "data"), showWarnings = FALSE)
  dir.create(file.path(path, "assemblies"), showWarnings = FALSE)

  # Assemblies ----
  purrr::walk(mapping$Assembly, function(fn) {
    file.copy(
      app_sys(file.path("test_data", "assemblies", fn)),
      file.path(path, "assemblies", fn),
      overwrite = TRUE
    )
  })

  # Reads ----
  # Every sample borrows the reads of the fish it was built from. The two
  # contaminated samples hold two mitogenomes, so they get both donors' reads.
  message("Fetching test data...")
  purrr::pwalk(mapping, function(...) {
    cur <- list(...)
    message(glue::glue("{cur$ID} - {cur$Taxon}"))
    donors <- strsplit(cur$Donors, ";")[[1]]
    purrr::walk(c("R1", "R2"), function(mate) {
      srcs <- purrr::map_chr(donors, function(acc) {
        fetch_test_reads(acc, mate, file.path(path, "data"), full_size)
      })
      dest <- file.path(path, "data", glue::glue("{cur$ID}_{mate}.fastq.gz"))
      # Concatenated gzip members are a valid gzip stream, so a contaminated
      # sample's reads are just its donors' files end to end.
      if (file.exists(dest)) file.remove(dest)
      file.create(dest)
      purrr::walk(srcs, function(src) {
        file.append(dest, src)
      })
    })
  })

  # MitoFinder reference ----
  # The mitogenome search needs a local database file, so pull the packaged
  # fish sampler into the project.
  mf_db <- file.path(path, "fish_mito_sampler.gb")
  file.copy(app_sys(file.path("test_data", "fish_mito_sampler.gb")), mf_db, overwrite = TRUE)

  mapping$Donors <- NULL
  readr::write_csv(mapping, file.path(path, "mapping.csv"), quote = "none", na = "")

  new_project_userAsmb(
    path = path,
    mapping_fn = file.path(path, "mapping.csv"),
    mapping_id = "ID",
    data_path = file.path(path, "data", ""),
    assembly_path = file.path(path, "assemblies", ""),
    genetic_code = NULL,
    container = container,
    find_mitogenome = TRUE,
    mitofinder_db = mf_db,
    attempt_circularization = TRUE,
    join_scaffolds = TRUE,
    executor = executor,
    Rproj = Rproj,
    force = force,
    ...
  )
}

# Put one donor's reads in the project data directory, fetching from ENA when
# the full size data was asked for, and hand back the path. Cached: several
# test samples share a donor.
fetch_test_reads <- function(acc, mate, data_dir, full_size) {
  if (!full_size) {
    src <- app_sys(file.path("test_data", glue::glue("{acc}_{mate}.fastq.gz")))
    return(src)
  }
  fn <- file.path(data_dir, glue::glue("{acc}_{mate}.fastq.gz"))
  pre <- stringr::str_sub(acc, 1, 6)
  suf <- stringr::str_extract(acc, "..$") |> stringr::str_pad(3, "left", "0")
  status <- glue::glue(
    "-t {fn} >/dev/null 2>&1 && echo 'complete' || echo 'incomplete' "
  ) |> system2("gzip", args = _, stdout = TRUE)
  while (status == "incomplete") {
    glue::glue(
      "curl",
      "http://ftp.sra.ebi.ac.uk/vol1/fastq/{pre}/{suf}/{acc}/{acc}_{substr(mate, 2, 2)}.fastq.gz",
      "--silent -o {fn}",
      .sep = " "
    ) |> system()
    status <- glue::glue(
      "-t {fn} >/dev/null 2>&1 && echo 'complete' || echo 'incomplete' "
    ) |> system2("gzip", args = _, stdout = TRUE)
  }
  fn
}
