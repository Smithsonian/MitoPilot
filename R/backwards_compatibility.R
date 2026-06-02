#' Update old project database for backwards compatibility
#'
#' Update old project database for backwards compatibility.
#' Adds "reviewed", "ID_verified", "genetic_code", and "problematic" columns to the annotate table,
#' "start_gene" column to the annotate_opts table. Adds
#' "assembler", "mitofinder_db", and "mitofinder" columns to the assemble_opts table.
#' Adds "max_blast_hits" to the curate_opts table.
#' Adds "asmbDir = 'NA'" to the .config params block
#' and updates the container to the current MitoPilot version
#' in the .config file.
#' Updates the "ref_dir" and "ref_db" fields in the annotate_opts table
#' adds these fields to the curate_opts table,
#' and updates the curation rules in the curate_opts table.
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

  assemble_table <- DBI::dbReadTable(con, "assemble")

  # check if .config file contains "asmbDir" parameter
  conf <- tryCatch({
    readLines(file.path(path, ".config"))
  }, error = function(e) {
    stop("Error reading .config file: ", e$message)
  })
  asmbDir <- any(grep("asmbDir = ", conf))

  # check if .config file contains "failOnIgnore = true"
  conf <- tryCatch({
    readLines(file.path(path, ".config"))
  }, error = function(e) {
    stop("Error reading .config file: ", e$message)
  })
  failOnIgnore <- any(grep("failOnIgnore = true", conf))
  blast_gb_conf <- any(grepl("blast_gb", conf))
  orffinder_conf <- any(grepl("orffinder_condaenv", conf))
  orf_block_conf <- any(grepl("^\\s*orf \\{", conf))

  # check if .config file contains latest container version
  new_container = paste0("macguigand/mitopilot:", utils::packageVersion("MitoPilot"))
  containerVer <- any(grep(new_container, conf))

  # check if annotate_opts or curate_params contains the ref_dir path
  old_ref_str = ("/ref_dbs/Mitos2" %in% annotate_opts_table || any(grep("/ref_dbs/Mitos2", curate_opts_table$params)))

  if (asmbDir &&
      failOnIgnore &&
      blast_gb_conf &&
      containerVer &&
      !old_ref_str &&
      "arwen_opts" %in% names(annotate_opts_table) &&
      "use_arwen" %in% names(annotate_opts_table) &&
      "start_gene" %in% names(annotate_opts_table) &&
      "max_blast_hits" %in% names(curate_opts_table) &&
      "ref_db" %in% names(curate_opts_table) &&
      "ref_dir" %in% names(curate_opts_table) &&
      "assembler" %in% names(assemble_opts_table) &&
      "mitofinder_db" %in% names(assemble_opts_table) &&
      "mitofinder" %in% names(assemble_opts_table) &&
      "problematic" %in% names(annotate_table) &&
      "genetic_code" %in% names(samples_table) &&
      "poor_blast_ref" %in% names(assemble_table) &&
      "ID_verified" %in% names(annotate_table) &&
      "reviewed" %in% names(annotate_table) &&
      "blast_accession" %in% names(assemble_table) &&
      "blast_opts" %in% names(assemble_table) &&
      "blast_opts" %in% DBI::dbListTables(con) &&
      "use_mitos_best" %in% names(annotate_opts_table) &&
      "use_aragorn" %in% names(annotate_opts_table) &&
      "aragorn_opts" %in% names(annotate_opts_table) &&
      "max_paths" %in% names(assemble_opts_table) &&
      "max_scaffolds" %in% names(assemble_opts_table) &&
      "tool" %in% DBI::dbListFields(con, "annotations") &&
      "blast_ref_annotations" %in% DBI::dbListTables(con) &&
      "blast_ref_alignment" %in% DBI::dbListTables(con) &&
      "use_orffinder" %in% names(annotate_opts_table) &&
      "orf_opts" %in% names(annotate_table) &&
      "orf_opts" %in% DBI::dbListTables(con) &&
      orffinder_conf &&
      orf_block_conf &&
      isTRUE(tryCatch(
        "genetic_code" %in% names(DBI::dbReadTable(con, "blast_ref_sequences")),
        error = function(e) FALSE
      )) &&
      isTRUE(tryCatch(
        "assemblies" %in% DBI::dbListTables(con) && "length_raw" %in% DBI::dbListFields(con, "assemblies"),
        error = function(e) FALSE
      )))
  {
    message("nothing to update")
    return(invisible(NULL))
  }

  # update annotation and curation reference databases
  if(old_ref_str){
    message("updated the annotate_opts table with new ref_dir and ref_db values")
    # update annotate ref_dir path
    annotate_opts_table$ref_dir <- rep("https://raw.githubusercontent.com/Smithsonian/MitoPilot/refs/heads/main/ref_dbs/Mitos2",
                                       nrow(annotate_opts_table))
    # update annotate ref_db name
    if("Metazoa" %in% annotate_opts_table){
      annotate_opts_table[which(annotate_opts_table$ref_db == "Metazoa"),]$ref_db <- "Metazoa_RefSeq89"
    }
    # update the annotate_opts table
    dplyr::tbl(con, "annotate_opts") |> # update SQL database
      dplyr::rows_update(
        annotate_opts_table,
        in_place = TRUE,
        copy = TRUE,
        by = "annotate_opts",
        unmatched = "ignore"
      )

    # make new fields in the curate_opts database
    message("added 'ref_dir' column to curate_opts table")
    curate_opts_table$ref_dir <- rep("https://raw.githubusercontent.com/Smithsonian/MitoPilot/refs/heads/main/ref_dbs/Mitos2",
                                     nrow(curate_opts_table))

    message("added 'ref_db' column to curate_opts table")
    curate_opts_table$ref_db <- rep("temp",
                                     nrow(curate_opts_table))

    for(i in 1:nrow(curate_opts_table)){
      if(any(grep("Metazoa", curate_opts_table[i,]$params))){
        curate_opts_table[i,]$ref_db <- "Metazoa_RefSeq89"
        curate_opts_table[i,]$params <- stringr::str_remove(curate_opts_table[i,]$params,
                                                            stringr::fixed("\"ref_dbs\":{\"default\":[\"/ref_dbs/Mitos2/Metazoa/featureProt/{gene}.fas\"]},"))
      } else {
        curate_opts_table[i,]$ref_db <- "Chordata"
        curate_opts_table[i,]$params <- stringr::str_remove(curate_opts_table[i,]$params,
                                                            stringr::fixed("\"ref_dbs\":{\"default\":[\"/ref_dbs/Mitos2/Chordata/featureProt/{gene}.fas\"]},"))
      }
    }

    sql_add_ref_dir <- "ALTER TABLE curate_opts ADD COLUMN ref_dir TEXT;"
    sql_add_ref_db <- "ALTER TABLE curate_opts ADD COLUMN ref_db TEXT;"

    DBI::dbExecute(con, sql_add_ref_dir)
    DBI::dbExecute(con, sql_add_ref_db)

    dplyr::tbl(con, "curate_opts") |> # update SQL database
      dplyr::rows_upsert(
        curate_opts_table,
        in_place = TRUE,
        copy = TRUE,
        by = "curate_opts"
      )
  }

  # update the Docker/Singularity container version to match the current package version
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

  # if .config does not contain "failOnIgnore = true" param, add it
  if(!(failOnIgnore)){
    conf <- readLines(file.path(path, ".config"))
    message("added \"failOnIgnore = true\" to nextflow .config file")
    lines <- c("// pipeline will exit with a non-zero exit code if any failed tasks are ignored using the ignore error strategy",
               "workflow {",
               "  failOnIgnore = true",
               "}")
    conf <- append(conf, lines)
    writeLines(conf, file.path(path, ".config"))
  }

  # if .config does not contain "orffinder_condaenv" param, add it. Anchor after
  # the last *_condaenv line if present, otherwise just inside the params block.
  if(!(orffinder_conf)){
    conf <- readLines(file.path(path, ".config"))
    anchor <- grep("_condaenv", conf)
    anchor <- if (length(anchor) > 0) max(anchor) else grep("^\\s*params \\{", conf)[1]
    if (length(anchor) == 1 && !is.na(anchor)) {
      conf <- append(conf, "    orffinder_condaenv = 'orffinder'", after = anchor)
      message("added \"orffinder_condaenv = 'orffinder'\" to nextflow .config file")
      writeLines(conf, file.path(path, ".config"))
    }
  }

  # if .config does not contain an "orf { }" process block, add one. Mirror the
  # curate block when present (to inherit clusterOptions); otherwise insert a
  # minimal block inside the params section.
  if(!(orf_block_conf)){
    conf <- readLines(file.path(path, ".config"))
    cur_start <- grep("^\\s*curate \\{", conf)
    if (length(cur_start) >= 1) {
      cur_start <- cur_start[1]
      cur_end <- cur_start
      for (j in (cur_start + 1):length(conf)) {
        if (grepl("^\\s*\\}", conf[j])) {
          cur_end <- j
          break
        }
      }
      block <- conf[cur_start:cur_end]
      block[1] <- sub("curate", "orf", block[1])
      conf <- append(conf, block, after = cur_end)
      message("added \"orf { }\" process block to nextflow .config file")
      writeLines(conf, file.path(path, ".config"))
    } else {
      params_line <- grep("^\\s*params \\{", conf)[1]
      if (!is.na(params_line)) {
        block <- c(
          "    orf {",
          "        container = process.container",
          "        executor = process.executor",
          "    }"
        )
        conf <- append(conf, block, after = params_line)
        message("added \"orf { }\" process block to nextflow .config file")
        writeLines(conf, file.path(path, ".config"))
      }
    }
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

  # if poor_blast_ref column doesn't exist on assemble, add it (TEXT: good/poor/failed/NULL)
  if (!("poor_blast_ref" %in% names(assemble_table))) {
    message("added 'poor_blast_ref' column to assemble table")
    DBI::dbExecute(con, "ALTER TABLE assemble ADD COLUMN poor_blast_ref TEXT")
    # migrate from samples if the column lived there in older projects
    if ("poor_blast_ref" %in% names(samples_table)) {
      message("migrating 'poor_blast_ref' values from samples to assemble")
      DBI::dbExecute(
        con,
        "UPDATE assemble SET poor_blast_ref = (
           SELECT CASE samples.poor_blast_ref WHEN 1 THEN 'poor' WHEN 0 THEN 'good' END
             FROM samples WHERE samples.ID = assemble.ID
         ) WHERE EXISTS (
           SELECT 1 FROM samples WHERE samples.ID = assemble.ID
             AND samples.poor_blast_ref IS NOT NULL
         )"
      )
      DBI::dbExecute(con, "ALTER TABLE samples DROP COLUMN poor_blast_ref")
    }
  }
  # convert any legacy integer values left in poor_blast_ref to TEXT (idempotent)
  DBI::dbExecute(
    con,
    "UPDATE assemble SET poor_blast_ref = CASE
       WHEN typeof(poor_blast_ref) = 'integer' AND poor_blast_ref = 1 THEN 'poor'
       WHEN typeof(poor_blast_ref) = 'integer' AND poor_blast_ref = 0 THEN 'good'
       ELSE poor_blast_ref
     END"
  )

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
  # if use_arwen column doesn't exist, add it (default off)
  if(!("use_arwen" %in% names(annotate_opts_table))){
    message("added 'use_arwen' column to annotate_opts table")
    annotate_opts_table$use_arwen <- rep(0L, nrow(annotate_opts_table))
    glue::glue_sql(
      "ALTER TABLE annotate_opts
       ADD COLUMN use_arwen INTEGER",
      col = col,
      .con = con
    ) |> DBI::dbExecute(con, statement = _)

    dplyr::tbl(con, "annotate_opts") |>
      dplyr::rows_upsert(
        annotate_opts_table,
        in_place = TRUE,
        copy = TRUE,
        by = "annotate_opts"
      )
  }

  # if arwen_opts column doesn't exist, add it
  if(!("arwen_opts" %in% names(annotate_opts_table))){
    message("added 'arwen_opts' column to annotate_opts table")
    annotate_opts_table$arwen_opts <- rep("-mtx", nrow(annotate_opts_table))
    glue::glue_sql(
      "ALTER TABLE annotate_opts
       ADD COLUMN arwen_opts TEXT",
      col = col,
      .con = con
    ) |> DBI::dbExecute(con, statement = _)

    dplyr::tbl(con, "annotate_opts") |>
      dplyr::rows_upsert(
        annotate_opts_table,
        in_place = TRUE,
        copy = TRUE,
        by = "annotate_opts"
      )
  }

  # if use_mitos_best column doesn't exist, add it (default on, matching prior behaviour)
  if(!("use_mitos_best" %in% names(annotate_opts_table))){
    message("added 'use_mitos_best' column to annotate_opts table")
    annotate_opts_table$use_mitos_best <- rep(1L, nrow(annotate_opts_table))
    glue::glue_sql(
      "ALTER TABLE annotate_opts
       ADD COLUMN use_mitos_best INTEGER",
      col = col,
      .con = con
    ) |> DBI::dbExecute(con, statement = _)

    dplyr::tbl(con, "annotate_opts") |>
      dplyr::rows_upsert(
        annotate_opts_table,
        in_place = TRUE,
        copy = TRUE,
        by = "annotate_opts"
      )
  }

  # if use_aragorn column doesn't exist, add it (default off)
  if(!("use_aragorn" %in% names(annotate_opts_table))){
    message("added 'use_aragorn' column to annotate_opts table")
    annotate_opts_table$use_aragorn <- rep(0L, nrow(annotate_opts_table))
    glue::glue_sql(
      "ALTER TABLE annotate_opts
       ADD COLUMN use_aragorn INTEGER",
      col = col,
      .con = con
    ) |> DBI::dbExecute(con, statement = _)

    dplyr::tbl(con, "annotate_opts") |>
      dplyr::rows_upsert(
        annotate_opts_table,
        in_place = TRUE,
        copy = TRUE,
        by = "annotate_opts"
      )
  }

  # if aragorn_opts column doesn't exist, add it
  if(!("aragorn_opts" %in% names(annotate_opts_table))){
    message("added 'aragorn_opts' column to annotate_opts table")
    annotate_opts_table$aragorn_opts <- rep("-m -gcstd", nrow(annotate_opts_table))
    glue::glue_sql(
      "ALTER TABLE annotate_opts
       ADD COLUMN aragorn_opts TEXT",
      col = col,
      .con = con
    ) |> DBI::dbExecute(con, statement = _)

    dplyr::tbl(con, "annotate_opts") |>
      dplyr::rows_upsert(
        annotate_opts_table,
        in_place = TRUE,
        copy = TRUE,
        by = "annotate_opts"
      )
  }

  # if coverage_trim column doesn't exist, add it (default on)
  if (!("coverage_trim" %in% names(annotate_opts_table))) {
    message("added 'coverage_trim' column to annotate_opts table")
    annotate_opts_table$coverage_trim <- rep(1L, nrow(annotate_opts_table))
    glue::glue_sql(
      "ALTER TABLE annotate_opts
       ADD COLUMN coverage_trim INTEGER",
      col = col,
      .con = con
    ) |> DBI::dbExecute(con, statement = _)
    dplyr::tbl(con, "annotate_opts") |>
      dplyr::rows_upsert(
        annotate_opts_table,
        in_place = TRUE,
        copy = TRUE,
        by = "annotate_opts"
      )
  }

  # if feature_trim column doesn't exist, add it (default on)
  if (!("feature_trim" %in% names(annotate_opts_table))) {
    message("added 'feature_trim' column to annotate_opts table")
    annotate_opts_table$feature_trim <- rep(1L, nrow(annotate_opts_table))
    glue::glue_sql(
      "ALTER TABLE annotate_opts
       ADD COLUMN feature_trim INTEGER",
      col = col,
      .con = con
    ) |> DBI::dbExecute(con, statement = _)
    dplyr::tbl(con, "annotate_opts") |>
      dplyr::rows_upsert(
        annotate_opts_table,
        in_place = TRUE,
        copy = TRUE,
        by = "annotate_opts"
      )
  }

  # if retain_low_conf_trna column doesn't exist, add it (default off = drop NNN)
  if (!("retain_low_conf_trna" %in% names(annotate_opts_table))) {
    message("added 'retain_low_conf_trna' column to annotate_opts table")
    annotate_opts_table$retain_low_conf_trna <- rep(0L, nrow(annotate_opts_table))
    glue::glue_sql(
      "ALTER TABLE annotate_opts
       ADD COLUMN retain_low_conf_trna INTEGER",
      col = col,
      .con = con
    ) |> DBI::dbExecute(con, statement = _)
    dplyr::tbl(con, "annotate_opts") |>
      dplyr::rows_upsert(
        annotate_opts_table,
        in_place = TRUE,
        copy = TRUE,
        by = "annotate_opts"
      )
  }

  # if use_orffinder column doesn't exist, add it (default off)
  if (!("use_orffinder" %in% names(annotate_opts_table))) {
    message("added 'use_orffinder' column to annotate_opts table")
    annotate_opts_table$use_orffinder <- rep(0L, nrow(annotate_opts_table))
    DBI::dbExecute(con, "ALTER TABLE annotate_opts ADD COLUMN use_orffinder INTEGER")
    dplyr::tbl(con, "annotate_opts") |>
      dplyr::rows_upsert(
        annotate_opts_table,
        in_place = TRUE,
        copy = TRUE,
        by = "annotate_opts"
      )
  }

  # if orf_opts column doesn't exist in the annotate table, add it (default set)
  if (!("orf_opts" %in% names(annotate_table))) {
    message("added 'orf_opts' column to annotate table")
    annotate_table$orf_opts <- rep("default", nrow(annotate_table))
    DBI::dbExecute(con, "ALTER TABLE annotate ADD COLUMN orf_opts TEXT")
    dplyr::tbl(con, "annotate") |>
      dplyr::rows_upsert(
        annotate_table,
        in_place = TRUE,
        copy = TRUE,
        by = "ID"
      )
  }

  # if orf_opts table doesn't exist, create it and seed a default row
  if (!("orf_opts" %in% DBI::dbListTables(con))) {
    message("created 'orf_opts' table")
    DBI::dbExecute(
      con,
      "CREATE TABLE orf_opts (
        orf_opts TEXT NOT NULL,
        cpus INTEGER,
        memory INTEGER,
        orffinder_opts TEXT,
        orf_min_len INTEGER,
        orf_max_overlap REAL,
        max_blast_hits INTEGER,
        ref_db TEXT,
        ref_dir TEXT,
        PRIMARY KEY (orf_opts)
      );"
    )
    # reuse the curation reference db/dir (the featureProt source) as the default,
    # falling back to the annotate ref and finally to the package defaults
    pick1 <- function(x, fallback) {
      x <- x[!is.na(x)]
      if (length(x) == 0) fallback else x[1]
    }
    ref_db_default <- pick1(
      c(curate_opts_table$ref_db[curate_opts_table$curate_opts == "default"],
        annotate_opts_table$ref_db),
      "Chordata"
    )
    ref_dir_default <- pick1(
      c(curate_opts_table$ref_dir[curate_opts_table$curate_opts == "default"],
        annotate_opts_table$ref_dir),
      "https://raw.githubusercontent.com/Smithsonian/MitoPilot/refs/heads/main/ref_dbs/Mitos2"
    )
    dplyr::tbl(con, "orf_opts") |>
      dplyr::rows_upsert(
        data.frame(
          orf_opts = "default",
          cpus = 4L,
          memory = 8L,
          orffinder_opts = "-s 1 -n true",
          orf_min_len = 300L,
          orf_max_overlap = 0.1,
          max_blast_hits = 100L,
          ref_db = ref_db_default,
          ref_dir = ref_dir_default
        ),
        in_place = TRUE,
        copy = TRUE,
        by = "orf_opts"
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

  # if max_paths column doesn't exist, add it
  if(!("max_paths" %in% names(assemble_opts_table))){
    message("added 'max_paths' column to assemble_opts table")
    assemble_opts_table$max_paths <- rep(10L, nrow(assemble_opts_table))
    glue::glue_sql(
      "ALTER TABLE assemble_opts
       ADD COLUMN max_paths INTEGER",
      col = col,
      .con = con
    ) |> DBI::dbExecute(con, statement = _)

    dplyr::tbl(con, "assemble_opts") |>
      dplyr::rows_upsert(
        assemble_opts_table,
        in_place = TRUE,
        copy = TRUE,
        by = "assemble_opts"
      )
  }

  # if max_scaffolds column doesn't exist, add it
  if(!("max_scaffolds" %in% names(assemble_opts_table))){
    message("added 'max_scaffolds' column to assemble_opts table")
    assemble_opts_table$max_scaffolds <- rep(10L, nrow(assemble_opts_table))
    glue::glue_sql(
      "ALTER TABLE assemble_opts
       ADD COLUMN max_scaffolds INTEGER",
      col = col,
      .con = con
    ) |> DBI::dbExecute(con, statement = _)

    dplyr::tbl(con, "assemble_opts") |>
      dplyr::rows_upsert(
        assemble_opts_table,
        in_place = TRUE,
        copy = TRUE,
        by = "assemble_opts"
      )
  }

  # if min_assembly_length column doesn't exist, add it
  if(!("min_assembly_length" %in% names(assemble_opts_table))){
    message("added 'min_assembly_length' column to assemble_opts table")
    assemble_opts_table$min_assembly_length <- rep(500L, nrow(assemble_opts_table))
    glue::glue_sql(
      "ALTER TABLE assemble_opts
       ADD COLUMN min_assembly_length INTEGER",
      col = col,
      .con = con
    ) |> DBI::dbExecute(con, statement = _)

    dplyr::tbl(con, "assemble_opts") |>
      dplyr::rows_upsert(
        assemble_opts_table,
        in_place = TRUE,
        copy = TRUE,
        by = "assemble_opts"
      )
  }

  # if blast_accession column doesn't exist, add BLAST result columns
  if (!("blast_accession" %in% names(assemble_table))) {
    message("added BLAST result columns to assemble table")
    DBI::dbExecute(con, "ALTER TABLE assemble ADD COLUMN blast_accession TEXT")
    DBI::dbExecute(con, "ALTER TABLE assemble ADD COLUMN blast_species TEXT")
    DBI::dbExecute(con, "ALTER TABLE assemble ADD COLUMN blast_pident REAL")
    DBI::dbExecute(con, "ALTER TABLE assemble ADD COLUMN blast_qcovs REAL")
    DBI::dbExecute(con, "ALTER TABLE assemble ADD COLUMN blast_evalue REAL")
    DBI::dbExecute(con, "ALTER TABLE assemble ADD COLUMN blast_lineage TEXT")
  }

  # if tool column doesn't exist in annotations table, add it
  annotations_cols <- DBI::dbListFields(con, "annotations")
  if (!("tool" %in% annotations_cols)) {
    message("added 'tool' column to annotations table")
    DBI::dbExecute(con, "ALTER TABLE annotations ADD COLUMN tool TEXT")
  }

  existing_tables <- DBI::dbListTables(con)

  if (!("blast_ref_annotations" %in% existing_tables)) {
    message("created blast_ref_annotations table")
    DBI::dbExecute(con,
      "CREATE TABLE blast_ref_annotations (
        ID TEXT NOT NULL,
        gene TEXT NOT NULL,
        type TEXT,
        pos1 INTEGER,
        pos2 INTEGER,
        direction TEXT,
        ref_length INTEGER,
        time_stamp INTEGER,
        PRIMARY KEY (ID, gene, pos1)
      );"
    )
  }

  if (!("blast_ref_sequences" %in% existing_tables)) {
    message("created blast_ref_sequences table")
    DBI::dbExecute(con,
      "CREATE TABLE blast_ref_sequences (
        accession TEXT NOT NULL,
        sequence TEXT NOT NULL,
        ref_length INTEGER,
        genetic_code INTEGER,
        time_stamp INTEGER,
        PRIMARY KEY (accession)
      );"
    )
  } else {
    ref_seq_cols <- names(DBI::dbReadTable(con, "blast_ref_sequences"))
    if (!("genetic_code" %in% ref_seq_cols)) {
      message("added 'genetic_code' column to blast_ref_sequences table")
      DBI::dbExecute(con, "ALTER TABLE blast_ref_sequences ADD COLUMN genetic_code INTEGER")
    }
  }

  if (!("blast_ref_alignment" %in% existing_tables)) {
    message("created blast_ref_alignment table")
    DBI::dbExecute(con,
      "CREATE TABLE blast_ref_alignment (
        ID TEXT NOT NULL,
        aligned_sample TEXT NOT NULL,
        aligned_ref TEXT NOT NULL,
        rotation INTEGER NOT NULL DEFAULT 0,
        ref_length INTEGER NOT NULL,
        time_stamp INTEGER,
        PRIMARY KEY (ID)
      );"
    )
  }

  # if blast_opts column doesn't exist in assemble table, add it
  if (!("blast_opts" %in% names(assemble_table))) {
    message("added 'blast_opts' column to assemble table")
    DBI::dbExecute(con, "ALTER TABLE assemble ADD COLUMN blast_opts TEXT")
    assemble_table$blast_opts <- rep("default", nrow(assemble_table))
    dplyr::tbl(con, "assemble") |>
      dplyr::rows_update(
        assemble_table[, c("ID", "blast_opts")],
        unmatched = "ignore",
        in_place = TRUE,
        copy = TRUE,
        by = "ID"
      )
  }

  # if assemblies table doesn't exist, create it; otherwise add length_raw if missing
  if (!("assemblies" %in% existing_tables)) {
    message("created assemblies table")
    DBI::dbExecute(con,
      "CREATE TABLE assemblies (
        ID TEXT NOT NULL,
        path INTEGER NOT NULL,
        scaffold INTEGER NOT NULL,
        topology TEXT,
        length INTEGER,
        length_raw INTEGER,
        sequence TEXT,
        depth TEXT,
        gc TEXT,
        errors TEXT,
        ignore INTEGER,
        edited INTEGER,
        blast_accession TEXT,
        blast_species TEXT,
        blast_pident REAL,
        blast_qcovs REAL,
        blast_evalue REAL,
        blast_lineage TEXT,
        time_stamp INTEGER,
        PRIMARY KEY (ID, path, scaffold)
      );"
    )
  } else if (!("length_raw" %in% DBI::dbListFields(con, "assemblies"))) {
    message("added 'length_raw' column to assemblies table")
    DBI::dbExecute(con, "ALTER TABLE assemblies ADD COLUMN length_raw INTEGER")
    DBI::dbExecute(con, "UPDATE assemblies SET length_raw = length WHERE length_raw IS NULL")
  }

  # if blast_opts table doesn't exist, create it with a default entry
  if (!("blast_opts" %in% DBI::dbListTables(con))) {
    message("created blast_opts table")
    DBI::dbExecute(con,
      "CREATE TABLE blast_opts (
        blast_opts TEXT NOT NULL,
        run_blast INTEGER,
        entrez_query TEXT,
        extra_opts TEXT,
        PRIMARY KEY (blast_opts)
      );"
    )
    dplyr::tbl(con, "blast_opts") |>
      dplyr::rows_upsert(
        data.frame(
          blast_opts   = "default",
          run_blast    = 1L,
          entrez_query = "mitochondrion[Location]",
          extra_opts   = ""
        ),
        in_place = TRUE,
        copy = TRUE,
        by = "blast_opts"
      )
  }

  # if .config does not contain "blast_gb" params section, add it
  if (!blast_gb_conf) {
    conf <- readLines(file.path(path, ".config"))
    message("added 'blast_gb' section to nextflow .config file")
    blast_gb_lines <- c(
      "    blast_gb {",
      "        cpus = 1",
      "        container = process.container",
      "        executor = process.executor",
      "    }"
    )
    # Insert after the last nested closing brace (end of the validate block)
    last_nested_close <- max(grep("^    }$", conf))
    conf <- append(conf, blast_gb_lines, after = last_nested_close)
    writeLines(conf, file.path(path, ".config"))
  }

}
