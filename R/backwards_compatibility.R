#' Update old project database for backwards compatibility
#'
#' Update old project database for backwards compatibility.
#' Adds "reviewed", "ID_verified", "genetic_code", and "problematic" columns to the annotate table,
#' "start_gene" column to the annotate_opts table,
#' and "max_blast_hits" to the curate_opts table.
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

  samples_table <- DBI::dbReadTable(con, "samples") # read in annotations table
  annotate_table <- DBI::dbReadTable(con, "annotate") # read in annotations table
  annotate_opts_table <- DBI::dbReadTable(con, "annotate_opts") # read in annotations opts table
  curate_opts_table <- DBI::dbReadTable(con, "curate_opts") # read in curate opts table

  if("start_gene" %in% names(annotate_opts_table) &&
     "max_blast_hits" %in% names(curate_opts_table) &&
     "problematic" %in% names(annotate_table) &&
     "genetic_code" %in% names() &&
     "ID_verified" %in% names(annotate_table) &&
     "reviewed" %in% names(annotate_table)) {
    message("nothing to update")
    return(invisible(NULL))
  }

  # if genetic_code column doesn't exist, add it
  if(!("genetic_code" %in% names(samples_table))){
    message("added 'genetic_code' column to samples table")
    samples_table$genetic_code <- rep("2", nrow(samples_table)) # add genetic_code column
    # add new columns to database
    glue::glue_sql(
      "ALTER TABLE samples
       ADD COLUMN genetic_code TEXT",
      col = col,
      .con = con
    ) |> DBI::dbExecute(con, statement = _)

    dplyr::tbl(con, "samples") |> # update SQL database
      dplyr::rows_upsert(
        samples_table,
        in_place = TRUE,
        copy = TRUE,
        by = "ID"
      )
  }

  # if reviewed column doesn't exist, add it
  if(!("reviewed" %in% names(annotate_table))){
    message("added 'reviewed' column to annotate table")
    annotate_table$reviewed <- rep("no", nrow(annotate_table)) # add reviewed column
    # add new columns to database
    glue::glue_sql(
      "ALTER TABLE annotate
       ADD COLUMN reviewed TEXT",
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
  }
  # if ID_verified column doesn't exist, add it
  if(!("ID_verified" %in% names(annotate_table))){
    message("added 'ID_verified' column to annotate table")
    annotate_table$ID_verified <- rep("no", nrow(annotate_table)) # add ID_verified column
    # add new columns to database
    glue::glue_sql(
      "ALTER TABLE annotate
       ADD COLUMN ID_verified TEXT",
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
  }
  # if problematic column doesn't exist, add it
  if(!("problematic" %in% names(annotate_table))){
    message("added 'problematic' column to annotate table")
    annotate_table$problematic <- rep(NA_character_, nrow(annotate_table)) # add ID_verified column
    # add new columns to database
    glue::glue_sql(
      "ALTER TABLE annotate
       ADD COLUMN problematic TEXT",
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
  }
  # if start_gene column doesn't exist, add it
  if(!("start_gene" %in% names(annotate_opts_table))){
    message("added 'start_gene' column to annotate_opts table")
    annotate_opts_table$start_gene <- rep("trnF", nrow(annotate_opts_table)) # add ID_verified column
    # add new columns to database
    glue::glue_sql(
      "ALTER TABLE annotate_opts
       ADD COLUMN start_gene TEXT",
      col = col,
      .con = con
    ) |> DBI::dbExecute(con, statement = _)

    dplyr::tbl(con, "annotate_opts") |> # update SQL database
      dplyr::rows_upsert(
        annotate_opts_table,
        in_place = TRUE,
        copy = TRUE,
        by = "annotate_opts"
      )
  }

  # if max_blast_hits column doesn't exist, add it
  if(!("max_blast_hits" %in% names(curate_opts_table))){
    message("added 'max_blast_hits' column to annotate_opts table")
    curate_opts_table$max_blast_hits <- rep(100, nrow(curate_opts_table)) # add ID_verified column
    # add new columns to database
    glue::glue_sql(
      "ALTER TABLE curate_opts
       ADD COLUMN max_blast_hits INTEGER",
       col = col,
      .con = con
    ) |> DBI::dbExecute(con, statement = _)

    dplyr::tbl(con, "curate_opts") |> # update SQL database
      dplyr::rows_upsert(
        curate_opts_table,
        in_place = TRUE,
        copy = TRUE,
        by = "curate_opts"
      )
  }
}
