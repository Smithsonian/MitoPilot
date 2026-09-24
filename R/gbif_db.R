#' Fetch GBIF occurrence metadata for project samples
#'
#' Looks up each sample's GBIF occurrence (gbifID), plus the dataset and
#' publisher it belongs to, and stores everything in the project database for
#' viewing in the app and use at export. GBIF IDs can change when a dataset is
#' republished; a sample whose ID no longer resolves keeps its previous data
#' and shows a failed fetch.
#'
#' @param path Path to the project directory (default = current working directory)
#' @param ids Sample IDs to fetch. Default: every sample with a GBIF ID.
#' @param gbifs Optional GBIF occurrence IDs (or gbif.org occurrence links) to
#'   set for `ids` first (same length as `ids`). A blank value removes that
#'   sample's GBIF ID and its GBIF data.
#' @return Invisibly, a data frame of `ID`, `status`, and `message`.
#' @export
fetch_gbif <- function(path = ".", ids = NULL, gbifs = NULL) {
  .meta_fetch_project(path, "GBIF", ids, gbifs)
}
