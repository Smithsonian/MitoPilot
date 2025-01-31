#' Generate Nextflow command to run pipline
#'
#' @param path MitoPilot project directory
#' @param workflow Which module to update (default = c("assemble",
#'   "annotate"))
#' @param source Nextflow script source. By default, this will be in the
#'   `nextflow/` subdirectory of the package installation.
#'
#' @export
nextflow_cmd <- function(
    workflow = c("assemble", "annotate"),
    path = NULL,
    source = app_sys("nextflow")) {
  path <- path %||% dirname(getOption("MitoPilot.db") %||% normalizePath(".sqlite"))
  workflow <- tolower(workflow[1])

  if (workflow %nin% c("assemble", "annotate")) {
    stop("Invalid workflow.")
  }

  cmd <- c(
    "-log", "{file.path(path, '.logs', 'nextflow.log')}",
    "run", "{source}",
    # "-ansi-log", "false",
    "-c", "{file.path(path, '.config')}",
    "-entry", "{ifelse(workflow == 'assemble', 'WF1', 'WF2')}",
    "{ifelse(file.exists(file.path(path, '.logs', 'nextflow.log')), '-resume', '')}"
  ) |> purrr::map_chr(~ stringr::str_glue(.x))

  return(invisible(cmd))
}
