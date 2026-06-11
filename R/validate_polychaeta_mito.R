#' Annotation validation for polychaete mitogenomes
#'
#' Thin wrapper around the shared validation core. See `validate_mito_core()`
#' for details.
#'
#' @param annotations_fn path to annotations file (csv)
#' @param coverage_fn path to coverage file (csv)
#' @param params nested list of curation/validation parameters. Can also be
#'   provided as a base64 encoded JSON string.
#' @param out_dir output directory
#'
#' @export
#'
validate_polychaeta_mito <- function(
    annotations_fn = NULL,
    coverage_fn = NULL,
    params = list(),
    out_dir = NULL) {
  validate_mito_core(
    annotations_fn = annotations_fn,
    coverage_fn = coverage_fn,
    params = params,
    out_dir = out_dir
  )
}

