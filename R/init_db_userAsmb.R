#' Initialize a new project database
#'
#' @param db_path Path to the new database file
#' @param mapping_fn Path to the mapping CSV file. Must contain columns "ID", "Taxon, "R1", "R2", "Assembly", and "Topology"
#' @param mapping_id Column name of the mapping file to use as the primary key
#' @param mapping_taxon Column name of the mapping file containing a Taxonomic
#'   identifier (eg, species name)
#' @param genetic_code Translation table for your organisms. See NCBI website
#'   for more info https://www.ncbi.nlm.nih.gov/Taxonomy/Utils/wprintgc.cgi
#' @param annotate_cpus Default # cpus for annotation
#' @param annotate_memory Default memory (GB) for annotation
#' @param annotate_ref_db Default Mitos2 reference database
#' @param annotate_ref_dir Default Mitos2 reference database directory
#' @param mitos_opts Default MITOS2 command line options
#' @param trnaScan_opts Default tRNAscan-SE command line options
#' @param curate_cpus Default # cpus for curation
#' @param curate_memory Default memory (GB) for curation
#' @param curate_target Default target database for curation
#' @param max_blast_hits Maximum number of top BLAST hits to retain (default = 100)
#' @param curate_params Default curation parameters
#' @export
#'
new_db_userAsmb <- function(
    db_path = "./.sqlite",
    mapping_fn = NULL,
    mapping_id = "ID",
    mapping_taxon = "Taxon",
    genetic_code = 2,
    # Default annotation options
    annotate_cpus = 6,
    annotate_memory = 36,
    annotate_ref_db = "Chordata",
    annotate_ref_dir = "https://raw.githubusercontent.com/Smithsonian/MitoPilot/refs/heads/main/ref_dbs/Mitos2",
    mitos_opts = "--intron 0 --oril 0",
    trnaScan_opts = "-M vert",
    # Default curation options
    curate_cpus = 4,
    curate_memory = 8,
    curate_target = "fish_mito",
    max_blast_hits = 100,
    curate_params = NULL) {
  # Read mapping file
  if (is.null(mapping_fn)) {
    mapping_fn <- "./mapping.csv"
    if (!file.exists(mapping_fn)) {
      stop("Mapping file not found")
    }
  }
  mapping <- utils::read.csv(mapping_fn)

  # convert ID column to characters
  mapping[[mapping_id]] <- as.character(mapping[[mapping_id]])

  # Validate ID col
  if (any(duplicated(mapping[[mapping_id]]))) {
    bad_IDs <- unique(mapping[[mapping_id]][duplicated(mapping[[mapping_id]])])
    message("problematic IDs:")
    message(paste(bad_IDs, collapse=", "))
    stop("Duplicate IDs found in mapping file")
  }

  # Validate ID length
  if (any(nchar(mapping[[mapping_id]]) > 18)) {
    bad_IDs <- mapping[[mapping_id]][nchar(mapping[[mapping_id]]) > 18]
    message("problematic IDs:")
    message(paste(bad_IDs, collapse=", "))
    stop("IDs must be no more than 18 characters")
  }

  # Validate IDs contain only alphanumeric characters
  if (any(!(grepl("^[a-zA-Z0-9_:-]+$", mapping[[mapping_id]])))) {
    bad_IDs <- mapping[[mapping_id]][!(grepl("^[a-zA-Z0-9_:-]+$", mapping[[mapping_id]]))]
    message("problematic IDs:")
    message(paste(bad_IDs, collapse=", "))
    stop("IDs must contain only alphanumeric characters, dashes, underscores, and colons")
  }


  # check for assembly and topology columns
  if ("Assembly" %nin% colnames(mapping)) {
    stop("Mapping file missing Assembly column")
  }
  if ("Topology" %nin% colnames(mapping)) {
    stop("Mapping file missing Topology column")
  }

  # Load default curation parameters
  if (is.null(curate_params)) {
    curate_params <- do.call(paste0("params_", curate_target), list())
  }

  # Create sqlite connection
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_path)
  on.exit(DBI::dbDisconnect(con))

  # Metadata table ----
  mapping <- mapping |>
    dplyr::mutate(
      ID = .data[[mapping_id]],
      Taxon = .data[[mapping_taxon]],
      genetic_code = genetic_code,
      topology = .data[["Topology"]],
      assembly = .data[["Assembly"]],
      export_group = NA_character_
    ) |>
    dplyr::select(-Topology, -Assembly)
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

  ## Preprocessing options ----
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
        cpus = 4,
        memory = 16,
        fastp = "--trim_poly_g --correction --detect_adapter_for_pe --dont_eval_duplication"
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
      assemble_notes TEXT,
      assemble_switch INTEGER,
      assemble_lock INTEGER,
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
          assemble_notes = NA_character_,
          assemble_switch = 1,
          assemble_lock = 0,
          hide_switch = 0,
          assemble_opts = "user",
          time_stamp = NA_integer_
        ),
      in_place = TRUE,
      copy = TRUE,
      by = "ID"
    )

  ## Add assemblies output ----
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
      gc TEXT,
      errors TEXT,
      ignore INTEGER,
      edited INTEGER,
      time_stamp INTEGER,
      PRIMARY KEY (ID, path, scaffold)
    );"
  )

  # Add Annotate table ----
  DBI::dbExecute(
    con,
    "CREATE TABLE annotate (
      ID TEXT NOT NULL,
      ID_verified TEXT,
      path TEXT,
      scaffolds INTEGER,
      annotate_opts TEXT,
      curate_opts TEXT,
      annotate_switch INTEGER,
      annotate_lock INTEGER,
      annotate_notes TEXT,
      PCGCount INTEGER,
      tRNACount INTEGER,
      rRNACount INTEGER,
      missing TEXT,
      extra TEXT,
      warnings INTEGER,
      reviewed TEXT,
      problematic TEXT,
      structure TEXT,
      length INTEGER,
      topology TEXT,
      time_stamp INTEGER,
      PRIMARY KEY (ID)
    );"
  )
  dplyr::tbl(con, "annotate") |>
    dplyr::rows_upsert(
      data.frame(
        ID = mapping$ID,
        annotate_opts = "default",
        curate_opts = "default",
        reviewed = "no",
        problematic = "no",
        ID_verified = "no",
        annotate_switch = 1,
        annotate_lock = 0
      ),
      in_place = TRUE,
      copy = TRUE,
      by = "ID"
    )

  ## Annotate options ----
  DBI::dbExecute(
    con,
    "CREATE TABLE annotate_opts (
      annotate_opts TEXT NOT NULL,
      cpus INTEGER,
      memory INTEGER,
      ref_db TEXT,
      ref_dir TEXT,
      mitos_opts TEXT,
      trnaScan_opts TEXT,
      start_gene TEXT,
      PRIMARY KEY (annotate_opts)
    );"
  )
  dplyr::tbl(con, "annotate_opts") |>
    dplyr::rows_upsert(
      data.frame(
        annotate_opts = "default",
        cpus = annotate_cpus,
        memory = annotate_memory,
        ref_db = annotate_ref_db,
        ref_dir = annotate_ref_dir,
        mitos_opts = "--intron 0 --oril 0",
        trnaScan_opts = "-M vert",
        start_gene = "trnF"
      ),
      in_place = TRUE,
      copy = TRUE,
      by = "annotate_opts"
    )

  ## Curate options ----
  DBI::dbExecute(
    con,
    "CREATE TABLE curate_opts (
      curate_opts TEXT NOT NULL,
      cpus INTEGER,
      memory INTEGER,
      target TEXT,
      max_blast_hits INTEGER,
      ref_db TEXT,
      ref_dir TEXT,
      params JSON,
      PRIMARY KEY (curate_opts)
    );"
  )
  dplyr::tbl(con, "curate_opts") |>
    dplyr::rows_upsert(
      data.frame(
        curate_opts = "default",
        cpus = curate_cpus,
        memory = curate_memory,
        target = curate_target,
        max_blast_hits = 100,
        ref_db = annotate_ref_db,
        ref_dir = annotate_ref_dir,
        params = jsonlite::toJSON(curate_params)
      ),
      in_place = TRUE,
      copy = TRUE,
      by = "curate_opts"
    )

  # Annotations table
  DBI::dbExecute(
    con,
    "CREATE TABLE annotations (
      ID TEXT NOT NULL,
      path INTEGER NOT NULL,
      scaffold INTEGER NOT NULL,
      type TEXT,
      gene TEXT,
      product TEXT,
      pos1 INTEGER,
      pos2 INTEGER,
      length INTEGER,
      direction TEXT,
      anticodon TEXT,
      start_codon TEXT,
      stop_codon TEXT,
      translation TEXT,
      notes TEXT,
      warnings TEXT,
      refHits JSON,
      edited INTEGER,
      time_stamp INTEGER,
      PRIMARY KEY (ID, path, scaffold, gene, pos1)
    );"
  )

  invisible(return())
}
