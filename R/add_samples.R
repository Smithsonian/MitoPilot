#' Add samples to project database
#'
#' Add new samples to an existing project database.
#' All samples will inherit the default processing options.
#' Creates a backup of the existing database prior to updating.
#'
#' @param path Path to the project directory (default = current working directory)
#' @param update_mapping_fn Path to the update mapping CSV file. Must contain columns "ID", "Taxon, "R1", and "R2"
#' @param mapping_id Column name of the update mapping file to use as the primary key
#' @param mapping_taxon Column name of the update mapping file containing a Taxonomic identifier (eg, species name)
#'
#' @export
#'
add_samples <- function(
    path = ".",
    update_mapping_fn = NULL,
    mapping_id = "ID",
    mapping_taxon = "Taxon")
{

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
  ##############################################################################################################
  mapping <- mapping |>
    dplyr::mutate(
      ID = .data[[mapping_id]],
      Taxon = .data[[mapping_taxon]]
    )
  # convert everything to characters
  mapping <- mapping %>%
    dplyr::mutate(across(everything(), as.character))

  # read existing sample table
  sample_table <- DBI::dbReadTable(con, "samples")
  # convert everything to characters
  sample_table <- sample_table %>%
    dplyr::mutate(across(everything(), as.character))

  # check to make sure there are no existing samples in the update database
  new_samples <- mapping$ID[which(mapping$ID %in% sample_table$ID)]
  if(length(new_samples) > 0){
    stop(paste0("sample(s) ", paste(shQuote(new_samples), collapse=", "), " present in the existing database"))
  }

  # add new columns to database if needed
  new_cols <- subset(colnames(mapping), !(colnames(mapping) %in% colnames(sample_table)))
  if(length(new_cols) > 0){
    for (col in new_cols) { # need to loop because SQLite doesn't allow multiple columns to be added in same statement
      glue::glue_sql(
        "ALTER TABLE samples
        ADD COLUMN {col}",
        col = col,
        .con = con
      ) |> DBI::dbExecute(con, statement = _)
      sample_table[,col] <- as.character(rep(NA, nrow(sample_table))) # add new column with NA values to dataframe object
    }
  }

  # join tables, using updated values from new table
  updated_table <- dplyr::rows_insert(sample_table, mapping, by="ID")

  # update SQL database
  dplyr::tbl(con, "samples") |>
    dplyr::rows_insert(
      updated_table,
      in_place = TRUE,
      copy = TRUE,
      by = "ID",
      conflict = "ignore"
    )

  # make backup of SQL database
  backup_dir = file.path(path, ".old_sqlite_dbs")
  if (!dir.exists(backup_dir)) {   # Create backup directory if it doesn't exist
    dir.create(backup_dir, recursive = TRUE)
    num = 1
  } else { # if backup dir exists, find any existing backups and increment backup number by 1
    backups <- list.files(backup_dir, pattern = ".sqlite.*", full.names=FALSE, all.files = TRUE)
    num <- max(as.numeric(sapply(strsplit(backups, "[.]"), "[", 3))) + 1
  }
  backup = file.path(backup_dir, paste0(".sqlite.", num))
  file.copy(file.path(path, ".sqlite"), backup)
  message("Backed up old SQLite database to: ", backup)

  # Preprocessing table ----
  ##############################################################################################################
  dplyr::tbl(con, "preprocess") |>
    dplyr::rows_insert(
      mapping |>
        dplyr::select(ID, R1, R2) |>
        dplyr::mutate(
          pre_opts = "default",
          reads = NA_real_,
          trimmed_reads = NA_real_,
          mean_length = NA_real_,
          time_stamp = NA_integer_
        ),
      in_place = TRUE,
      copy = TRUE,
      by = "ID",
      conflict = "ignore"
    )

  # Assemble table ----
  ##############################################################################################################
  dplyr::tbl(con, "assemble") |>
    dplyr::rows_insert(
      mapping |>
        dplyr::select(ID) |>
        dplyr::mutate(
          length = NA_character_,
          topology = NA_character_,
          paths = NA_integer_,
          scaffolds = NA_integer_,
          assemble_notes = NA_character_,
          assemble_switch = 1,
          assemble_lock = 0,
          hide_switch = 0,
          assemble_opts = "default",
          time_stamp = NA_integer_
        ),
      in_place = TRUE,
      copy = TRUE,
      by = "ID",
      conflict = "ignore"
    )

  # Annotate table ----
  ##############################################################################################################
  dplyr::tbl(con, "annotate") |>
    dplyr::rows_insert(
      data.frame(
        ID = mapping$ID,
        annotate_opts = "default",
        curate_opts = "default",
        annotate_switch = 1,
        annotate_lock = 0
      ),
      in_place = TRUE,
      copy = TRUE,
      by = "ID",
      conflict = "ignore"
    )

}
