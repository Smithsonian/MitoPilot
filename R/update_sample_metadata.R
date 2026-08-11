#' Update project database metadata.
#'
#' Update the metadata for an existing project database.
#' Cannot update ID, R1, or R2, but can add new metadata columns.
#' Creates a backup of the existing database prior to updating.
#'
#' @param path Path to the project directory (default = current working directory)
#' @param update_mapping_fn Path to the update mapping CSV file. Must contain columns "ID" and "Taxon"
#' @param mapping_id Column name of the update mapping file to use as the primary key
#' @param mapping_taxon Column name of the update mapping file containing a Taxonomic identifier (eg, species name)
#'
#' @export
#'
update_sample_metadata <- function(
    path = ".",
    update_mapping_fn = NULL,
    mapping_id = "ID",
    mapping_taxon = "Taxon"
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
  mapping <- utils::read.csv(update_mapping_fn)

  # Validate ID col
  if (any(duplicated(mapping[[mapping_id]]))) {
    stop("Duplicate IDs found in mapping file")
  }

  # Validate ID length
  if (any(nchar(mapping[[mapping_id]]) > 18)) {
    stop("IDs must be no more than 18 characters")
  }

  # Create sqlite connection
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = file.path(path, ".sqlite"))
  on.exit(DBI::dbDisconnect(con))

  # Metadata table ----
  mapping <- mapping |>
    dplyr::mutate(
      ID = .data[[mapping_id]],
      Taxon = .data[[mapping_taxon]]
    )
  # convert everything to characters
  mapping <- mapping |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

  # remove R1 and R2 columns from updated mapping
  if("R1" %in% colnames(mapping) | "R2" %in% colnames(mapping)){
    mapping = mapping[,-which(colnames(mapping) %in% c("R1", "R2"))]
    message("Update mapping file contains sequence data columns (R1 and/or R2)")
    message("These columns will not be updated in the database")
  }

  # remove Assembly and Topology columns from updated mapping
  if("Assembly" %in% colnames(mapping) | "Topology" %in% colnames(mapping)){
    mapping = mapping[,-which(colnames(mapping) %in% c("Assembly", "Topology"))]
    message("Update mapping file contains user assembly information (Assembly and/or Topology)")
    message("These columns will not be updated in the database")
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

  # make backup of SQL database
  backup_project_db(path)

  # add new columns to database if needed
  sample_table <- add_missing_sample_cols(con, mapping, sample_table)

  # join tables, using updated values from new table
  updated_table <- dplyr::rows_update(sample_table, mapping, by="ID")

  # update SQL database
  dplyr::tbl(con, "samples") |>
    dplyr::rows_upsert(
      updated_table,
      in_place = TRUE,
      copy = TRUE,
      by = "ID"
    )
}
