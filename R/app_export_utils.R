#' Populate export table
#'
#' @param db database connection
#' @param session reactive session
#'
#' @noRd
fetch_export_data <- function(session = getDefaultReactiveDomain()) {
  db <- session$userData$con

  dplyr::tbl(db, "assemble") |>
    dplyr::filter(assemble_lock == 1) |>
    dplyr::select(ID) |>
    dplyr::left_join(dplyr::tbl(db, "annotate"), by = "ID") |>
    dplyr::filter(annotate_lock == 1) |>
    dplyr::select(ID, topology, structure) |>
    dplyr::left_join(dplyr::tbl(db, "samples"), by = "ID") |>
    dplyr::relocate(Taxon, .after = ID) |>
    dplyr::collect()

}
