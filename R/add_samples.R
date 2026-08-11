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
  mapping <- read_and_validate_mapping(update_mapping_fn, mapping_id)

  if ("Topology" %in% colnames(mapping)) {
    # Confirm topology field contains only lowercase "linear" or "circular"
    if (any(mapping$Topology %nin% c("circular", "linear"))) {
      bad_IDs <- mapping[[mapping_id]][mapping$Topology %nin% c("circular", "linear")]
      message("problematic samples:")
      message(paste(bad_IDs, collapse=", "))
      stop("Values in the Topology column must be either lowercase \"circular\" or \"linear\"")
    }
  }

  # genetic_code auto-selects from each sample's curation ruleset; it is filled
  # in below by .sync_sample_genetic_codes() after the annotate rows (which carry
  # the curate_opts assignment) are inserted. Use a placeholder for now.
  genetic_code <- NA_integer_

  # Create sqlite connection
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = file.path(path, ".sqlite"))
  on.exit(DBI::dbDisconnect(con))

  # Metadata table ----
  ##############################################################################################################
  if("Assembly" %in% colnames(mapping) & "Topology" %in% colnames(mapping)){
    mapping <- mapping |>
      dplyr::mutate(
        ID = .data[[mapping_id]],
        Taxon = .data[[mapping_taxon]],
        genetic_code = genetic_code,
        topology = .data[["Topology"]],
        assembly = .data[["Assembly"]],
      ) |>
      dplyr::select(-Topology, -Assembly)
  } else {
    mapping <- mapping |>
      dplyr::mutate(
        ID = .data[[mapping_id]],
        Taxon = .data[[mapping_taxon]],
        genetic_code = genetic_code
      )
  }

  # convert everything to characters
  mapping <- mapping |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

  # read existing sample table
  sample_table <- DBI::dbReadTable(con, "samples")
  # convert everything to characters
  sample_table <- sample_table |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

  # check to make sure there are no existing samples in the update database
  new_samples <- mapping$ID[which(mapping$ID %in% sample_table$ID)]
  if(length(new_samples) > 0){
    stop(paste0("sample(s) ", paste(shQuote(new_samples), collapse=", "), " present in the existing database"))
  }

  # add new columns to database if needed
  sample_table <- add_missing_sample_cols(con, mapping, sample_table)

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
  backup_project_db(path)

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
          blast_opts = "default",
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
        path = 1L,
        scaffold = 1L,
        annotate_opts = "default",
        curate_opts = "default",
        orf_opts = "default",
        reviewed = "no",
        problematic = "no",
        partial = "no",
        ID_verified = "no",
        annotate_switch = 1,
        annotate_lock = 0
      ),
      in_place = TRUE,
      copy = TRUE,
      by = c("ID", "path", "scaffold"),
      conflict = "ignore"
    )

  # Fill samples.genetic_code for the new samples from their curation ruleset
  # (default curate_opts target + optional override).
  .sync_sample_genetic_codes(con, ids = mapping$ID)

}
