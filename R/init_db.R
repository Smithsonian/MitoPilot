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

  # Add sample metadata table
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

  # Add pre-processing table
  DBI::dbExecute(
    con,
    "CREATE TABLE preprocess (
      ID TEXT NOT NULL,
      opts TEXT NOT NULL,
      reads INTEGER,
      trimmed_reads INTEGER,
      mean_length INTEGER
      PRIMARY KEY (ID)
    );"
  )
  dplyr::tbl(con, "preprocess") |>
    dplyr::rows_upsert(
      dat |>
        dplyr::select(ID, fwd, rev) |>
        dplyr::mutate(
          opts = "default",
          reads = NA_real_,
          trimmed_reads = NA_real_,
          mean_length = NA_real_
        ),
      in_place = TRUE,
      copy = TRUE,
      by = "ID"
    )

  # Add pre-processing options table
  DBI::dbExecute(
    con,
    "CREATE TABLE preprocess_opts (
      opts TEXT NOT NULL,
      cpus INTEGER,
      memory INTEGER,
      fastp TEXT,
      PRIMARY KEY (opts)
    );"
  )
  dplyr::tbl(con, "preprocess_opts") |>
    dplyr::rows_upsert(
      data.frame(
        opts = "default",
        cpus = 8,
        memory = '8.GB',
        fastp = "--trim_poly_g --correction"
      ),
      in_place = TRUE,
      copy = TRUE,
      by = "opts"
    )

  return()

}
