fetch_assemble_data <- function(
    path=here::here()
  ){

  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = file.path(path,".sqlite"))
  on.exit(DBI::dbDisconnect(con))

  assemble <- dplyr::tbl(con, "assemble")
  preprocess <- dplyr::tbl(con, "preprocess") |>
    dplyr::select(ID, pre_opts, reads, trimmed_reads, mean_length) |>
    dplyr::left_join(dplyr::tbl(con, "pre_opts"), by="pre_opts")

  dplyr::left_join(assemble, preprocess, by="ID") |>
    dplyr::collect() |>
    dplyr::arrange(dplyr::desc(time_stamp)) |>
    dplyr::relocate(
      lock,
      assemble_switch,
      ID,
      pre_opts,
      reads, trimmed_reads, mean_length,
      assemble_opts,
      topology,
      length,
      paths,
      scaffolds,
      assembly_notes,
      time_stamp
    ) |>
    dplyr::mutate(bttn_details=NA_character_)

}
