#' Populate annotate table
#'
#' @param db database connection
#' @param session reactive session
#'
#' @noRd
fetch_annotate_data <- function(session = getDefaultReactiveDomain()) {
  db <- session$userData$con

  annotate <- dplyr::tbl(db, "annotate")

  assemble <- dplyr::tbl(db, "assemble") |>
    dplyr::filter(assemble_lock == 1) |>
    dplyr::select(ID)

  taxa <- dplyr::tbl(db, "samples") |>
    dplyr::select(ID, Taxon)

  dplyr::left_join(assemble, annotate, by = "ID") |>
    dplyr::left_join(taxa, by = "ID") |>
    dplyr::select(
      annotate_lock,
      annotate_switch,
      ID,
      Taxon,
      annotate_opts,
      curate_opts,
      length,
      topology,
      scaffolds,
      structure,
      PCGCount,
      tRNACount,
      rRNACount,
      missing,
      extra,
      warnings,
      time_stamp,
      annotate_notes
    ) |>
    dplyr::arrange(dplyr::desc(time_stamp)) |>
    dplyr::collect() |>
    dplyr::mutate(view = NA_character_)
}
