#' Fetch GEOME metadata for project samples
#'
#' Looks up each sample's GEOME BCID, walks up its parent records (e.g.
#' Tissue, Sample, Event) and adds expedition and project details, then stores
#' everything in the project database for viewing in the app and use at export.
#'
#' @param path Path to the project directory (default = current working directory)
#' @param ids Sample IDs to fetch. Default: every sample with a BCID.
#' @param bcids Optional BCIDs to set for `ids` first (same length as `ids`).
#'   A blank value removes that sample's BCID and its GEOME data.
#' @return Invisibly, a data frame of `ID`, `status`, and `message`.
#' @export
fetch_geome <- function(path = ".", ids = NULL, bcids = NULL) {
  .meta_fetch_project(path, "GEOME", ids, bcids)
}
