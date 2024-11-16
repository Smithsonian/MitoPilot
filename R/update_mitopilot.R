#' Update Pipeline Helper
#'
#' @param path MitoPilot project directory (defualt = here::here())
#' @param workflow Which module to update (default = c("both", "assemble",
#'   "annotate"))
#' @param source Nextflow script source. By default, this will be in the
#'   `nextflow/` subdirectory of the package installation.
#'
#' @export
update_mitopilot <- function(
    path = here::here(),
    workflow = c("both", "assemble", "annotate"),
    source = app_sys("nextflow")) {
  if (any(c("assemble", "both") %in% workflow)) {
    message("To update the assembly module, run in terminal:")
    glue::glue(
      "nextflow",
      "-log .logs/nextflow.log",
      "run {source}",
      "-c {file.path(path, '.config')}",
      "-entry WF1",
      "-resume",
      .sep = " "
    ) |>
      print()
  }

  if (any(c("annotate", "both") %in% workflow)) {
    message("Annotation module coming soon...")
  }
}
