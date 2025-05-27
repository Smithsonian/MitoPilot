#' Update old project database for backwards compatibility
#'
#' Update old project database for backwards compatibility.
#' Adds "reviewed", "ID_verified", "genetic_code", and "problematic" columns to the annotate table,
#' "start_gene" column to the annotate_opts table. Adds
#' "assembler", "mitofinder_db", and "mitofinder" columns to the assemble_opts table.
#' Adds "max_blast_hits" to the curate_opts table.
#' Also adds "asmbDir = 'NA'" to the .config params block
#' and updates the container to the current MitoPilot version
#' in the .config file.
#'
#
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
  assemble_opts_table <- DBI::dbReadTable(con, "assemble_opts") # read in assemble opts table
  annotate_opts_table <- DBI::dbReadTable(con, "annotate_opts") # read in annotations opts table
  curate_opts_table <- DBI::dbReadTable(con, "curate_opts") # read in curate opts table

  # check if .config file contains "asmbDir" parameter
  conf <- tryCatch({
    readLines(file.path(path, ".config"))
  }, error = function(e) {
    stop("Error reading .config file: ", e$message)
  })
  asmbDir <- any(grep("asmbDir = ", conf))

  # check if .config file contains latest container version
  new_container = paste0("macguigand/mitopilot:", utils::packageVersion("MitoPilot"))
  containerVer <- any(grep(new_container, conf))

  if(asmbDir &&
     containerVer &&
    "start_gene" %in% names(annotate_opts_table) &&
     "max_blast_hits" %in% names(curate_opts_table) &&
     "ref_db" %in% names(curate_opts_table) &&
     "ref_dir" %in% names(curate_opts_table) &&
     "assembler" %in% names(assemble_opts_table) &&
     "mitofinder_db" %in% names(assemble_opts_table) &&
     "mitofinder" %in% names(assemble_opts_table) &&
     "problematic" %in% names(annotate_table) &&
     "genetic_code" %in% names(samples_table) &&
     "ID_verified" %in% names(annotate_table) &&
     "reviewed" %in% names(annotate_table)) {
    message("nothing to update")
    return(invisible(NULL))
  }

  if(!(containerVer)){
    conf <- readLines(file.path(path, ".config"))
    # update the container version in the .config
    container_index <- grep("container = .*mitopilot.*", conf)
    if (length(container_index) == 1) {
      conf[container_index] <- paste0("  container = \'", new_container, "\'")
    } else {
      stop("Container not found or multiple containers specificed in Nextflow .config")
    }
    message("updated container version in nextflow .config file")
    writeLines(conf, file.path(path, ".config"))
  }

  # if .config does not contain "asmbDir" param, add it
  if(!(asmbDir)){
    conf <- readLines(file.path(path, ".config"))
    message("added \"asmbDir = 'NA'\" to nextflow .config file")
    rawDir_line <- grep("rawDir", conf) # find line containing "rawDir"
    conf <- append(conf, "    asmbDir = 'NA'", after = rawDir_line) # add new line to conf after "rawDir" line
    writeLines(conf, file.path(path, ".config"))
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

  # if ref_dir column doesn't exist, add it
  if(!("ref_dir" %in% names(curate_opts_table))){
    message("added 'ref_dir' column to annotate_opts table")
    curate_opts_table$ref_dir <- rep("/ref_dbs/Mitos2", nrow(curate_opts_table))
    # add new columns to database
    glue::glue_sql(
      "ALTER TABLE curate_opts
       ADD COLUMN ref_dir TEXT",
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

  # if ref_db column doesn't exist, add it
  if(!("ref_db" %in% names(curate_opts_table))){
    message("added 'ref_db' column to annotate_opts table")
    curate_opts_table$ref_db <- rep("Chordata", nrow(curate_opts_table))
    # add new columns to database
    glue::glue_sql(
      "ALTER TABLE curate_opts
       ADD COLUMN ref_db TEXT",
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

  # if assembler column doesn't exist, add it
  if(!("assembler" %in% names(assemble_opts_table))){
    message("added 'assembler' column to annotate_opts table")
    assemble_opts_table$assembler <- rep("GetOrganelle", nrow(assemble_opts_table)) # add assembler column
    # add new columns to database
    glue::glue_sql(
      "ALTER TABLE assemble_opts
       ADD COLUMN assembler TEXT",
      col = col,
      .con = con
    ) |> DBI::dbExecute(con, statement = _)

    dplyr::tbl(con, "assemble_opts") |> # update SQL database
      dplyr::rows_upsert(
        assemble_opts_table,
        in_place = TRUE,
        copy = TRUE,
        by = "assemble_opts"
      )
  }

  # if mitofinder_db column doesn't exist, add it
  if(!("mitofinder_db" %in% names(assemble_opts_table))){
    message("added 'mitofinder_db' column to annotate_opts table")
    assemble_opts_table$mitofinder_db <- rep("https://raw.githubusercontent.com/Smithsonian/MitoPilot/refs/heads/devel-DJM/ref_dbs/MitoFinder/NC_002333_Danio_rerio.gb",
                                             nrow(assemble_opts_table)) # add mitofinder_db column
    # add new columns to database
    glue::glue_sql(
      "ALTER TABLE assemble_opts
       ADD COLUMN mitofinder_db TEXT",
      col = col,
      .con = con
    ) |> DBI::dbExecute(con, statement = _)

    dplyr::tbl(con, "assemble_opts") |> # update SQL database
      dplyr::rows_upsert(
        assemble_opts_table,
        in_place = TRUE,
        copy = TRUE,
        by = "assemble_opts"
      )
  }


  # if mitofinder column doesn't exist, add it
  if(!("mitofinder" %in% names(assemble_opts_table))){
    message("added 'mitofinder' column to annotate_opts table")
    assemble_opts_table$mitofinder <- rep("--megahit", nrow(assemble_opts_table)) # add mitofinder column
    # add new columns to database
    glue::glue_sql(
      "ALTER TABLE assemble_opts
       ADD COLUMN mitofinder TEXT",
      col = col,
      .con = con
    ) |> DBI::dbExecute(con, statement = _)

    dplyr::tbl(con, "assemble_opts") |> # update SQL database
      dplyr::rows_upsert(
        assemble_opts_table,
        in_place = TRUE,
        copy = TRUE,
        by = "assemble_opts"
      )
  }

}
