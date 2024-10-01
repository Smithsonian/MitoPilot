init_db <- function(
    db_path=file.path(here::here(),".sqlite"),
    mapping_fn=NULL,
    mapping_id="ID",
    target="fish_mito"){

  # Read mapping file
  if(is.null(mapping_fn)){
    mapping_fn <- here::here("mapping.csv")
    if(!file.exists(mapping_fn)){
      stop("Mapping file not found")
    }
  }
  mapping <- read.csv(mapping_fn)

  # Validate ID col
  if(any(duplicated(mapping[[mapping_id]]))){
    stop("Duplicate IDs found in mapping file")
  }

  # Create sqlite connection
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_path)
  on.exit(DBI::dbDisconnect(con))

  # Metadata table ----
  if(mapping_id != "ID"){
    mapping <- mapping |>
      dplyr::mutate(
        ID = .data[[ID_col]]
      )
  }
  glue::glue_sql(
    "CREATE TABLE samples (
     {cols*},
     PRIMARY KEY (ID)
   )",
    cols = colnames(mapping),
    .con = con
  ) |> DBI::dbExecute(con, statement = _)
  dplyr::tbl(con, "samples") |>
    dplyr::rows_upsert(
      mapping,
      in_place = TRUE,
      copy = TRUE,
      by = "ID"
    )

  # Preprocessing table ----
  DBI::dbExecute(
    con,
    "CREATE TABLE preprocess (
      ID TEXT NOT NULL,
      R1 TEXT,
      R2 TEXT,
      pre_opts TEXT NOT NULL,
      reads INTEGER,
      trimmed_reads INTEGER,
      mean_length INTEGER,
      time_stamp INTEGER,
      PRIMARY KEY (ID)
    );"
  )
  dplyr::tbl(con, "preprocess") |>
    dplyr::rows_upsert(
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
      by = "ID"
    )

  # Preprocessing options table ----
  DBI::dbExecute(
    con,
    "CREATE TABLE pre_opts (
      pre_opts TEXT NOT NULL,
      cpus INTEGER,
      memory INTEGER,
      fastp TEXT,
      PRIMARY KEY (pre_opts)
    );"
  )
  dplyr::tbl(con, "pre_opts") |>
    dplyr::rows_upsert(
      data.frame(
        pre_opts = "default",
        cpus = 8,
        memory = 4,
        fastp = "--trim_poly_g --correction"
      ),
      in_place = TRUE,
      copy = TRUE,
      by = "pre_opts"
    )

  # Assemble table ----
  DBI::dbExecute(
    con,
    "CREATE TABLE assemble (
      ID TEXT NOT NULL,
      length TEXT,
      topology TEXT,
      paths INTEGER,
      scaffolds INTEGER,
      assembly_notes TEXT,
      assemble_switch INTEGER,
      lock INTEGER,
      hide_switch INTEGER,
      assemble_opts TEXT,
      time_stamp INTEGER,
      PRIMARY KEY (ID)
    );"
  )
  dplyr::tbl(con, "assemble") |>
    dplyr::rows_upsert(
      mapping |>
        dplyr::select(ID) |>
        dplyr::mutate(
          length = NA_character_,
          topology = NA_character_,
          paths = NA_integer_,
          scaffolds = NA_integer_,
          assembly_notes = NA_character_,
          assemble_switch = 1,
          lock = 0,
          hide_switch = 0,
          assemble_opts = "default",
          time_stamp = NA_integer_
        ),
      in_place = TRUE,
      copy = TRUE,
      by = "ID"
    )

  # Assemble options table ----
  DBI::dbExecute(
    con,
    "CREATE TABLE assemble_opts (
      assemble_opts TEXT NOT NULL,
      cpus INTEGER,
      memory INTEGER,
      getOrganelle TEXT,
      seeds_db TEXT,
      labels_db TEXT,
      PRIMARY KEY (assemble_opts)
    );"
  )
  dplyr::tbl(con, "assemble_opts") |>
    dplyr::rows_upsert(
      data.frame(
        assemble_opts = "default",
        cpus = 6,
        memory = 16,
        seeds_db = glue::glue("/ref_dbs/getOrganelle/seeds/{target}.fasta"),
        labels_db = glue::glue("/ref_dbs/getOrganelle/labels/{target}.fasta"),
        getOrganelle = "-F 'anonym' -R 10 -k '21,45,65,85,105,115' --larger-auto-ws --expected-max-size 20000 --target-genome-size 16500"
      ),
      in_place = TRUE,
      copy = TRUE,
      by = "assemble_opts"
    )

  # Add assemblies table
  DBI::dbExecute(
    con,
    "CREATE TABLE assemblies (
      ID TEXT NOT NULL,
      path INTEGER NOT NULL,
      scaffold INTEGER NOT NULL,
      topology TEXT,
      length INTEGER,
      sequence TEXT,
      depth TEXT,
      d_depth TEXT,
      gc TEXT,
      errors TEXT,
      ignore INTEGER,
      edited INTEGER,
      time_stamp INTEGER,
      PRIMARY KEY (ID, path, scaffold)
    );"
  )

  invisible(return())

}
