#' Update old project database for backwards compatibility
#'
#' Update old project database for backwards compatibility.
#' Adds "reviewed" column to the annotate SQL table.
#'
#' @param path Path to the project directory (default = current working directory)
#'
#' @export
#'
backwards_compatibility <- function(
    path = "."
){
  # update SQL database with "reviewed" column for annotations table
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = file.path(path, ".sqlite")) # open connection
  on.exit(DBI::dbDisconnect(con))

  annotate_table <- DBI::dbReadTable(con, "annotate") # read in annotations table

  # if reviewed column doesn't exist, add it
  if(!("reviewed" %in% names(annotate_table))){
    annotate_table$reviewed <- rep("no", nrow(annotate_table)) # add reviewed column
    # add new columns to database

    glue::glue_sql(
      "ALTER TABLE annotate
       ADD COLUMN reviewed",
      col = col,
      .con = con
    ) |> DBI::dbExecute(con, statement = _)

    dplyr::tbl(con, "annotate") |> # update SQL database
      dplyr::rows_upsert(
        annotate_table,
        in_place = TRUE,
        copy = TRUE,
        by = "ID"
      )
  } else {
      message("nothing to update")
    }
}
