#' Update sequence files for existing project
#'
#' Update the sequence data files for samples in an existing project database.
#' After updating, user should rerun these samples, starting with the ASSEMBLE module.
#' Creates a backup of the existing database prior to updating.
#'
#' @param path Path to the project directory (default = current working directory)
#' @param update_mapping_fn Path to the update mapping CSV file. Must contain columns "ID", "R1", and "R2"
#' @param mapping_id Column name of the update mapping file to use as the primary key
#'
#' @export
#'
update_sample_seqdata <- function(
    path = ".",
    update_mapping_fn = NULL,
    mapping_id = "ID"
){

  # Check if project directory exists ----
  if (!dir.exists(path)) {
    stop("Project directory does not exist")
  }
  path <- normalizePath(path)

  # Read mapping file
  if (is.null(update_mapping_fn)) {
    stop("Must provide update mapping file")
  }else if(!file.exists(update_mapping_fn)){
    stop("Update mapping file does not exist")
  }
  mapping <- read_and_validate_mapping(update_mapping_fn, mapping_id)

  # Create sqlite connection
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = file.path(path, ".sqlite"))
  on.exit(DBI::dbDisconnect(con))

  # Metadata table ----
  mapping <- mapping |>
    dplyr::mutate(
      ID = .data[[mapping_id]],
    )

  # convert everything to characters
  mapping <- mapping |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

  # check that mapping file contains sequence data columns
  if(!("R1" %in% colnames(mapping)) | !("R2" %in% colnames(mapping))){
    stop("Update mapping file must contain columns `R1` and `R2`")
  }

  # remove other columns from mapping file, if present
  if(any(!(colnames(mapping) %in% c("ID", "R1", "R2")))){
    message("Update mapping file contains columns other than `ID`, `R1`, and `R2`")
    message("These columns will not be updated in the database")
    mapping = mapping[,-which(!(colnames(mapping) %in% c("ID", "R1", "R2")))]
  }

  # read existing sample table
  sample_table <- DBI::dbReadTable(con, "samples")
  # convert everything to characters
  sample_table <- sample_table |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

  # check to make sure there are no new samples in the update database
  new_samples <- mapping$ID[which(!(mapping$ID %in% sample_table$ID))]
  if(length(new_samples) > 0){
    stop(paste0("sample(s) ", paste(shQuote(new_samples), collapse=", "), " absent in the existing database"))
  }

  # join tables, using updated values from new table
  updated_table <- dplyr::rows_update(sample_table, mapping, by="ID")

  # which samples have updated sequence data?
  updated_samples <- updated_table$ID[which(!(paste0(updated_table$R1,updated_table$R2) %in% paste0(sample_table$R1,sample_table$R2)))]

  # print warning message to user
  if(length(updated_samples) > 0){
    message(paste0("Warning, sample(s): ", paste(shQuote(updated_samples), collapse=", "), " have been updated with new sequence data"))
    message("Please re-run these samples through MitoPilot, starting with the Assemble module")
  }else {
    stop("No samples in the update mapping file with new sequence data")
  }

  # make backup of SQL database
  backup_project_db(path)

  # update SQL database
  dplyr::tbl(con, "samples") |>
    dplyr::rows_upsert(
      updated_table,
      in_place = TRUE,
      copy = TRUE,
      by = "ID"
    )

  # also update the preprocess table: R1/R2 there are the copy the PREPROCESS
  # module actually reads (samples.R1/R2 is not consumed by the pipeline).
  # Targeted update so pipeline-computed columns (reads, etc.) are untouched.
  dplyr::tbl(con, "preprocess") |>
    dplyr::rows_update(
      dplyr::select(mapping, "ID", "R1", "R2"),
      by = "ID",
      in_place = TRUE,
      copy = TRUE,
      unmatched = "ignore"
    )
}
