#' Initialize a new project database
#'
#' @param db_path Path to the new database file
#' @param mapping_fn Path to the mapping file
#' @param mapping_id Column name of the mapping file to use as the primary key
#' @param assemble_cpus Default # cpus for assembly
#' @param assemble_memory default memory (GB) for assembly
#' @param seeds_db Path to the gotOrganelle seeds database
#' @param labels_db Path to the gotOrganelle labels database
#' @param getOrganelle Default getOrganelle command line options
#' @param annotate_cpus Default # cpus for annotation
#' @param annotate_memory Default memory (GB) for annotation
#' @param mitos_refDb Default mitos2 reference database
#'
#' @export
#'
new_db <- function(
    db_path = file.path(here::here(), ".sqlite"),
    mapping_fn = NULL,
    mapping_id = "ID",
    # Default assembly options
    assemble_cpus = 6,
    assemble_memory = 8,
    seeds_db = glue::glue("/ref_dbs/getOrganelle/seeds/fish_mito.fasta"),
    labels_db = glue::glue("/ref_dbs/getOrganelle/labels/fish_mito.fasta"),
    getOrganelle = "-F 'anonym' -R 10 -k '21,45,65,85,105,115' --larger-auto-ws --expected-max-size 20000 --target-genome-size 16500",
    # Default annotation options
    annotate_cpus = 6,
    annotate_memory = 8,
    annotate_ref_db = "Chordata",
    annotate_ref_dir = "/ref_dbs/Mitos2",
    mitos_opts = "--intron 0 --oril 0 --trna 0",
    trnaScan_opts = "-M vert",
    # Default curation options
    curate_cpus = 4,
    curate_memory = 8,
    curate_target = "fish_mito",
    curate_params = list(
      ref_dbs = list(
        default = "/ref_dbs/Mitos2/Chordata/featureProt/{gene}.fas"
      ),
      hit_threshold = 90,
      max_overlap = 0.25,
      default_rules = list(
        rRNA = list(
          count = 1,
          max_len = NA,
          min_len = NA,
          overlap = list(start = 0, stop = F)
        ),
        PCG = list(
          count = 1,
          max_len = NA,
          min_len = NA,
          overlap = list(start = 2, stop = F),
          stop_codons = c("TAA", "TAG", "AGA", "AGG", "AG", "TA", "T"),
          start_codons = c("ATG", "GTG", "ATA", "ATT", "TTA", "ATC")
        ),
        tRNA = list(
          count = 1,
          max_len = NA,
          min_len = NA
        )
      ),
      rules = list(
        ctrl = list(
          count = 1,
          type = "ctrl",
          min_len = 350
        ),
        rrnL = list(
          type = "rRNA",
          max_len = 1850
        ),
        rrnS = list(
          type = "rRNA",
          max_len = 1000
        ),
        nad1 = list(
          type = "PCG",
          start_codons = c("ATG", "GTG", "ATA", "ATT", "TTA", "ATC", "TTG")
        ),
        nad2 = list(
          type = "PCG"
        ),
        cox1 = list(
          type = "PCG",
          overlap = list(start = 2, stop = T)
        ),
        cox2 = list(
          type = "PCG",
          start_codons = c("ATG", "GTG", "ATA", "ATT", "TTA", "ATC", "TTG")
        ),
        atp8 = list(
          type = "PCG",
          overlap = list(start = 2, stop = T)
        ),
        atp6 = list(
          type = "PCG",
          overlap = list(start = 20, stop = F),
          start_codons = c("ATG", "GTG", "ATA", "ATT", "TTA", "ATC", "CTG")
        ),
        cox3 = list(
          type = "PCG"
        ),
        nad3 = list(
          type = "PCG"
        ),
        nad4l = list(
          type = "PCG",
          overlap = list(start = 2, stop = T)
        ),
        nad4 = list(
          type = "PCG",
          overlap = list(start = 20, stop = F)
        ),
        nad5 = list(
          type = "PCG",
          overlap = list(start = 2, stop = T)
        ),
        nad6 = list(
          type = "PCG",
          overlap = list(start = 2, stop = T)
        ),
        cob = list(
          type = "PCG"
        ),
        trnA = list(type = "tRNA"),
        trnC = list(type = "tRNA"),
        trnD = list(type = "tRNA"),
        trnE = list(type = "tRNA"),
        trnF = list(type = "tRNA"),
        trnG = list(type = "tRNA"),
        trnH = list(type = "tRNA"),
        trnI = list(type = "tRNA"),
        trnK = list(type = "tRNA"),
        trnL = list(
          type = "tRNA",
          count = 2
        ),
        trnM = list(type = "tRNA"),
        trnN = list(type = "tRNA"),
        trnP = list(type = "tRNA"),
        trnQ = list(type = "tRNA"),
        trnR = list(type = "tRNA"),
        trnS = list(
          type = "tRNA",
          count = 2
        ),
        trnT = list(type = "tRNA"),
        trnV = list(type = "tRNA"),
        trnW = list(type = "tRNA"),
        trnY = list(type = "tRNA")
      )
    )) {
  ){
  # Read mapping file
  if (is.null(mapping_fn)) {
    mapping_fn <- here::here("mapping.csv")
    if (!file.exists(mapping_fn)) {
      stop("Mapping file not found")
    }
  }
  mapping <- utils::read.csv(mapping_fn)

  # Validate ID col
  if (any(duplicated(mapping[[mapping_id]]))) {
    stop("Duplicate IDs found in mapping file")
  }

  # Create sqlite connection
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_path)
  on.exit(DBI::dbDisconnect(con))

  # Metadata table ----
  if (mapping_id != "ID") {
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
        cpus = 8,
        memory = 4,
        fastp = "--trim_poly_g --correction --detect_adapter_for_pe"
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
          assemble_opts = "default",
          time_stamp = NA_integer_
        ),
      in_place = TRUE,
      copy = TRUE,
      by = "ID"
    )

  ## Assemble options ----
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
        cpus = assemble_cpus,
        memory = assemble_memory,
        seeds_db = seeds_db,
        labels_db = labels_db,
        getOrganelle = getOrganelle
      ),
      in_place = TRUE,
      copy = TRUE,
      by = "assemble_opts"
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
      annotate_opts TEXT,
      curate_opts TEXT,
      annotate_switch INTEGER,
      annotate_lock INTEGER,
      annotate_notes TEXT,
      scaffolds INTEGER,
      genes INTEGER,
      tRNA INTEGER,
      rRNA INTEGER,
      missing INTEGER,
      duplicated INTEGER,
      warnings INTEGER,
      structure TEXT,
      length TEXT,
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
        mitos_opts = "--intron 0 --oril 0 --trna 0",
        trnaScan_opts = "-M vert"
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
        params = jsonlite::toJSON(curate_params)
      ),
      in_place = TRUE,
      copy = TRUE,
      by = "curate_opts"
    )

  invisible(return())
}
