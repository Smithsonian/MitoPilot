#' Export project database to CSV
#'
#' Exports the MitoPilot SQLite database as a CSV.
#' File name will include the date and time of the export.
#'
#' @param path Path to the project directory (default = current working directory)
#'
#' @export
#'
export_db_to_csv <- function(
    path = "."
){
  # Create sqlite connection
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = file.path(path, ".sqlite"))
  on.exit(DBI::dbDisconnect(con))

  # read tables
  # samples
  sample_table <- DBI::dbReadTable(con, "samples")
  # preprocess
  pre_table <- DBI::dbReadTable(con, "preprocess")
  # assembly
  asmb_table <- DBI::dbReadTable(con, "assemble")
  # annotation
  annot_table <- DBI::dbReadTable(con, "annotate")

  # drop some unnecessary columns
  pre_table <- pre_table[,!names(pre_table) %in% c("R1", "R2", "hide_switch")]
  asmb_table <- asmb_table[,!names(asmb_table) %in% c("assemble_switch", "assemble_lock", "hide_switch")]
  annot_table <- annot_table[,!names(annot_table) %in% c("annotate_lock", "annotate_switch", "path", "topology", "scaffolds", "length")]

  # change time stamp col names
  names(pre_table)[names(pre_table) == 'time_stamp'] <- 'time_stamp.pre'
  names(asmb_table)[names(asmb_table) == 'time_stamp'] <- 'time_stamp.asmb'
  names(annot_table)[names(annot_table) == 'time_stamp'] <- 'time_stamp.annot'

  # join tables on ID
  tbl <- sample_table %>%
    dplyr::left_join(pre_table, by='ID') %>%
    dplyr::left_join(asmb_table, by='ID') %>%
    dplyr::left_join(annot_table, by='ID')

  # make new directory
  db_dir = file.path(path, "db_csv_export")
  if (!dir.exists(db_dir)) {   # Create backup directory if it doesn't exist
    dir.create(db_dir, recursive = TRUE)
    num = 1
  }

  # file name for backup dir, use date and time
  csv <- paste0("db_", format(Sys.time(), "%Y-%m-%d_%H%M%S_%Z"), ".csv")
  write.csv(tbl, file.path(db_dir, csv), quote = FALSE, row.names = FALSE)

  message("exported MitoPilot database to:")
  message(file.path(db_dir, csv))
}
