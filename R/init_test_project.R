#' Initialize a test project
#'
#' This function will set up a test project and fectch associated data from ENA.
#' The `n` parameter can be used to limit the number of species used in the test
#' project for faster set up.
#'
#' @param path path the the directory for the test project (default = currect
#'   working directory). Will be created if it does not already exists.
#' @param n how many samples to include in the test project (Default = Inf,
#'   include all)
#' @param full_size (logical) Use the full size test data set (default = FALSE).
#'   Setting to TRUE will download the raw data from ENA, which will require
#'   10GB and will take some time to complete. By default a set of smaller
#'   pre-filtered input files will be fetched from the MitoPilot github repo.
#' @param executor The executor to use for running the nextflow pipeline. Must
#'   be one of "local" (default) or "awsbatch", "NMNH_Hydra", or "NOAA_SEDNA".
#' @param container The container to use for running the pipeline.
#' @param Rproj (logical) Initialize and open an RStudio project in the project
#'   directory (default = TRUE). This has now effect if not running
#'   interactively in RStudio.
#' @param force (logical) Force recreating of existing project database and
#'   config files (default = FALSE).
#' @param ... Additional arguments passed `init_db()`
#'
#' @export
new_test_project <- function(
    path = here::here(),
    n = Inf,
    full_size = FALSE,
    executor = "local",
    container = "drleopold/mitopilot",
    Rproj = TRUE,
    force = FALSE,
    ...) {
  # TODO add check for curl available

  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
  path <- normalizePath(path)

  mapping <- app_sys(file.path("test_data", "mapping_test.csv")) |>
    utils::read.csv() |>
    dplyr::slice_head(n = n)
  readr::write_csv(mapping, file.path(path, "mapping.csv"), quote = "none", na = "")

  # Get Data ----

  dir.create(file.path(path, "data"))

  if (full_size) {
    # TODO - make parallel
    message("Fetching test data. Go grab a coffee, this may take a while...")
    purrr::pwalk(mapping, function(...) {
      cur <- list(...)
      message(glue::glue("{cur$ID} - {cur$Taxon}"))
      acc <- cur$ID
      pre <- stringr::str_sub(acc, 1, 6)
      suf <- stringr::str_extract(acc, "..$") |> stringr::str_pad(3, "left", "0")
      # R1
      fn_R1 <- file.path(path, "data", glue::glue("{acc}_R1.fastq.gz"))
      status <- glue::glue(
        "-t {fn_R1} >/dev/null 2>&1 && echo 'complete' || echo 'incomplete' "
      ) |> system2("gzip", args = _, stdout = T)
      while (status == "incomplete") {
        glue::glue(
          "curl",
          "http://ftp.sra.ebi.ac.uk/vol1/fastq/{pre}/{suf}/{acc}/{acc}_1.fastq.gz",
          "--silent -o {fn_R1}",
          .sep = " "
        ) |> system()
        status <- glue::glue(
          "-t {fn_R1} >/dev/null 2>&1 && echo 'complete' || echo 'incomplete' "
        ) |> system2("gzip", args = _, stdout = T)
      }
      # R2
      fn_R2 <- file.path(path, "data", glue::glue("{acc}_R2.fastq.gz"))
      status <- glue::glue(
        "-t {fn_R2} >/dev/null 2>&1 && echo 'complete' || echo 'incomplete' "
      ) |> system2("gzip", args = _, stdout = T)
      while (status == "incomplete") {
        glue::glue(
          "curl",
          "http://ftp.sra.ebi.ac.uk/vol1/fastq/{pre}/{suf}/{acc}/{acc}_2.fastq.gz",
          "--silent -o {fn_R2}",
          .sep = " "
        ) |> system()
        status <- glue::glue(
          "-t {fn_R2} >/dev/null 2>&1 && echo 'complete' || echo 'incomplete' "
        ) |> system2("gzip", args = _, stdout = T)
      }
    })
  }
  if (!full_size) {
    purrr::pwalk(mapping, function(...) {
      cur <- list(...)
      message(glue::glue("{cur$ID} - {cur$Taxon}"))
      file.copy(app_sys(file.path("test_data", cur$R1)), file.path(path, "data"))
      file.copy(app_sys(file.path("test_data", cur$R2)), file.path(path, "data"))
    })
  }

  # Initialize project ----
  new_project(
    path = path,
    mapping_fn = file.path(path, "mapping.csv"),
    mapping_id = "ID",
    data_path = file.path(path, "data", ""),
    min_depth = 500,
    executor = executor,
    Rproj = Rproj,
    ...
  )
}
