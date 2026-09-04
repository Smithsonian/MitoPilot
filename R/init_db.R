# MapToRef option defaults, shared by new_db() and the assemble-options modal.
.mtr_default_bowtie2 <- "--very-sensitive-local"
.mtr_default_consensus <- "-d 3 --min-BQ 20"

#' Initialize a new project database
#'
#' @param db_path Path to the new database file
#' @param mapping_fn Path to the mapping CSV file. Must contain columns "ID", "Taxon, "R1", and "R2"
#' @param mapping_id Column name of the mapping file to use as the primary key
#' @param mapping_taxon Column name of the mapping file containing a Taxonomic
#'   identifier (eg, species name)
#' @param genetic_code Optional NCBI translation table override. Default `NULL`
#'   auto-selects from the curation ruleset (`curate_target`); a number sets an
#'   override on the default curate_opts set.
#'   https://www.ncbi.nlm.nih.gov/Taxonomy/Utils/wprintgc.cgi
#' @param dedup Run fastp with `--dedup` to drop PCR and optical duplicates
#'   before assembly? Default FALSE keeps `--dont_eval_duplication`, the shipped
#'   fastp default. Editable later in the preprocess-options modal.
#' @param assemble_cpus Default # cpus for assembly
#' @param assemble_memory default memory (GB) for assembly
#' @param seeds_db Path to the gotOrganelle seeds database, can be a URL, cannot have same file name as labels_db.
#'   Default is a fish database built from RefSeq. https://raw.githubusercontent.com/smithsonian/MitoPilot/main/ref_dbs/getOrganelle/seeds/fish_mito_seeds.fasta
#' @param labels_db Path to the gotOrganelle labels database, can be a URL, cannot have same file name as seeds_db.
#'   Default is a fish database built from RefSeq. https://raw.githubusercontent.com/smithsonian/MitoPilot/main/ref_dbs/getOrganelle/seeds/fish_mito_labels.fasta
#' @param getOrganelle Default getOrganelle command line options
#' @param annotate_cpus Default # cpus for annotation
#' @param annotate_memory Default memory (GB) for annotation
#' @param annotate_ref_db Default Mitos2 reference database
#' @param annotate_ref_dir Default Mitos2 reference database directory
#' @param mitos_opts Default MITOS2 command line options
#' @param trnaScan_opts Default tRNAscan-SE command line options
#' @param arwen_opts Default ARWEN command line options
#' @param aragorn_opts Default ARAGORN command line options
#' @param curate_cpus Default # cpus for curation
#' @param curate_memory Default memory (GB) for curation
#' @param curate_target Default target database for curation
#' @param curate_ref_db Default curation reference database (default =
#'   "Metazoa_RefSeq235", the only bundled DB with rRNA BLAST references)
#' @param max_blast_hits Maximum number of top BLAST hits to retain (default = 10)
#' @param linear_complete Treat linear assemblies as complete genomes for the
#'   export "completeness" field? By default only circular assemblies are
#'   labeled "complete genome" and linear assemblies "partial genome". Set TRUE
#'   for taxa whose complete mitogenome is genuinely linear (default = FALSE).
#'   Editable later in the curation-options modal.
#' @param curate_params Default curation parameters
#' @param orf_cpus CPUs for the optional ORF-finder step (default = 4)
#' @param orf_memory Memory (GB) for the optional ORF-finder step (default = 8)
#' @param orffinder_opts Default NCBI ORFfinder options (default = "-s 1")
#' @param orf_min_len Minimal ORF length in nucleotides (default = 300)
#' @param orf_max_overlap Maximum overlap with existing annotations, as a fraction
#'   of the ORF length, before an ORF is discarded (default = 0.1)
#' @param assembler Assembler, choice of "GetOrganelle" (default), "MitoFinder",
#'   or "MapToRef"
#' @param mitofinder_db Path to MitoFinder reference db, must be GenBank format (.gb), can be a URL.
#'   Default is a ten-species fish mitogenome sampler (https://raw.githubusercontent.com/Smithsonian/MitoPilot/refs/heads/main/ref_dbs/MitoFinder/fish_mito_sampler.gb)
#' @param mitofinder Default MitoFinder command line options
#' @param maptoref_ref Default MapToRef reference mitogenome for the parameter
#'   set: an absolute file path, a URL, or an NCBI nucleotide accession (for
#'   example NC_002333). A single-record GenBank file (.gb) is preferred; a
#'   FASTA is accepted but then \code{maptoref_topology} must be set. Optional:
#'   samples may instead name their own reference in the mapping file's
#'   \code{Reference} column, which gives each of those samples its own
#'   MapToRef parameter set, or through \code{\link{set_maptoref_refs}}.
#' @param maptoref Default bowtie2 options for MapToRef
#'   (default = "--very-sensitive-local")
#' @param maptoref_consensus Default samtools consensus options for MapToRef
#'   (default = "-d 3 --min-BQ 20")
#' @param maptoref_iter Maximum MapToRef iteration passes (default = 5)
#' @param maptoref_topology Topology of a MapToRef reference, "circular" or
#'   "linear". Ignored when the GenBank LOCUS line names a topology, and used
#'   when the LOCUS line names neither. Required for a FASTA reference.
#' @param max_paths Maximum number of assembly paths allowed for a sample to
#'   continue past the Assemble step (default = 10). Samples exceeding this are
#'   flagged as failed and skipped by downstream steps in WF1.
#' @param max_scaffolds Maximum number of scaffolds (within a single path)
#'   allowed for a sample to continue past the Assemble step (default = 10).
#'   Samples exceeding this are flagged as failed and skipped by downstream
#'   steps in WF1.
#' @param min_assembly_length Minimum contig length (bp) to retain for
#'   coverage calculations, BLAST, and annotation. Scaffolds shorter than this threshold are stored
#'   but flagged as ignored (default = 500).
#' @export
#'
new_db <- function(
    db_path = "./.sqlite",
    mapping_fn = NULL,
    mapping_id = "ID",
    mapping_taxon = "Taxon",
    genetic_code = NULL,
    # Default preprocessing options
    dedup = FALSE,
    # Default assembly options
    assemble_cpus = 6,
    assemble_memory = 24,
    assembler = "GetOrganelle",
    seeds_db = "https://raw.githubusercontent.com/smithsonian/MitoPilot/main/ref_dbs/getOrganelle/seeds/fish_mito_seeds.fasta",
    labels_db = "https://raw.githubusercontent.com/smithsonian/MitoPilot/main/ref_dbs/getOrganelle/labels/fish_mito_labels.fasta",
    getOrganelle = paste(
      "-F 'anonym'",
      "-R 10 -k '21,45,65,85,105,115'",
      "--larger-auto-ws",
      "--expected-max-size 20000",
      "--target-genome-size 16500"
    ),
    mitofinder_db = "https://raw.githubusercontent.com/Smithsonian/MitoPilot/refs/heads/main/ref_dbs/MitoFinder/fish_mito_sampler.gb",
    mitofinder = paste(
      "--megahit"
    ),
    maptoref_ref = NA_character_,
    maptoref = .mtr_default_bowtie2,
    maptoref_consensus = .mtr_default_consensus,
    maptoref_iter = 5L,
    maptoref_topology = NA_character_,
    max_paths = 10,
    max_scaffolds = 10,
    min_assembly_length = 500,
    # Default annotation options
    annotate_cpus = 6,
    annotate_memory = 36,
    annotate_ref_db = "Chordata",
    annotate_ref_dir = "https://raw.githubusercontent.com/Smithsonian/MitoPilot/refs/heads/main/ref_dbs/Mitos2",
    mitos_opts = "--intron 0 --oril 0",
    trnaScan_opts = "-M vert -X 20",
    arwen_opts = "-mtx",
    aragorn_opts = "-m -gcstd",
    # Default curation options
    curate_cpus = 4,
    curate_memory = 8,
    curate_target = "fish_mito",
    curate_ref_db = "Metazoa_RefSeq235",
    max_blast_hits = 10,
    linear_complete = FALSE,
    curate_params = NULL,
    # Default ORF-finder options
    orf_cpus = 4,
    orf_memory = 8,
    orffinder_opts = "-s 1",
    orf_min_len = 300,
    orf_max_overlap = 0.1) {
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
    message(paste(bad_IDs, collapse = ", "))
    stop("Duplicate IDs found in mapping file")
  }

  # Validate assembler choice
  if (assembler %nin% c("GetOrganelle", "MitoFinder", "MapToRef")) {
    stop("Assembler not supported, valid options: [GetOrganelle, MitoFinder, MapToRef]")
  }
  if (!is.na(maptoref_topology) &&
      maptoref_topology %nin% c("circular", "linear")) {
    stop("maptoref_topology must be circular or linear")
  }
  if (assembler == "MapToRef" &&
      !is.na(maptoref_ref) && nzchar(trimws(maptoref_ref)) &&
      !identical(.mtr_ref_class(maptoref_ref), "accession") &&
      !grepl("\\.(gb|gbk|gbff)$", trimws(maptoref_ref), ignore.case = TRUE) &&
      (is.na(maptoref_topology) || !nzchar(trimws(maptoref_topology)))) {
    stop("Set maptoref_topology (circular or linear) for a FASTA reference; ",
         "a GenBank (.gb) reference takes its topology from the file")
  }
  if (grepl("['\"]", paste0(maptoref, maptoref_consensus))) {
    stop("maptoref and maptoref_consensus must not contain quote characters")
  }

  # Validate ID length
  if (any(nchar(mapping[[mapping_id]]) > 18)) {
    bad_IDs <- mapping[[mapping_id]][nchar(mapping[[mapping_id]]) > 18]
    message("problematic IDs:")
    message(paste(bad_IDs, collapse = ", "))
    stop("IDs must be no more than 18 characters")
  }

  # Validate IDs contain only alphanumeric characters
  if (any(!(grepl("^[a-zA-Z0-9_:-]+$", mapping[[mapping_id]])))) {
    bad_IDs <- mapping[[mapping_id]][!(grepl("^[a-zA-Z0-9_:-]+$", mapping[[mapping_id]]))]
    message("problematic IDs:")
    message(paste(bad_IDs, collapse = ", "))
    stop("IDs must contain only alphanumeric characters, dashes, underscores, and colons")
  }

  # The optional Reference column seeds assemble.maptoref_ref. It must be taken
  # out here, before CREATE TABLE samples ({cols*}) below, and before the DB
  # connection is opened, so a bad reference leaves no half-built .sqlite.
  # Reserved for every assembler: a value stored unchecked would go unguarded
  # the moment the user switches a project to MapToRef in the app.
  taken <- .mtr_take_ref_col(mapping, mapping_id = mapping_id)
  mapping <- taken$mapping
  # One validation pass over both sources, so every bad value is listed at once.
  checked <- .mtr_validate_refs(
    c(maptoref_ref %||% NA_character_, taken$refs),
    ids = c("assemble options", names(taken$refs)),
    context = "the assemble options and the mapping file 'Reference' column"
  )
  maptoref_ref <- checked[1]
  refs <- NULL
  if (!is.null(taken$refs)) {
    refs <- checked[-1]
    names(refs) <- names(taken$refs)
  }

  # Set GetOrganelle databases if user did not supply them with MitoPilot::new_project()
  # using default fish databases
  if (is.null(seeds_db) & is.null(labels_db)) {
    seeds_db <- "https://raw.githubusercontent.com/smithsonian/MitoPilot/main/ref_dbs/getOrganelle/seeds/fish_mito_seeds.fasta"
    labels_db <- "https://raw.githubusercontent.com/smithsonian/MitoPilot/main/ref_dbs/getOrganelle/labels/fish_mito_labels.fasta"
  } else if (is.null(seeds_db)) {
    seeds_db <- "https://raw.githubusercontent.com/smithsonian/MitoPilot/main/ref_dbs/getOrganelle/seeds/fish_mito_seeds.fasta"
  } else if (is.null(labels_db)) {
    labels_db <- "https://raw.githubusercontent.com/smithsonian/MitoPilot/main/ref_dbs/getOrganelle/labels/fish_mito_labels.fasta"
  }

  # Load default curation parameters
  if (is.null(curate_params)) {
    curate_params <- do.call(paste0("params_", curate_target), list())
  }

  # Genetic code auto-selects from the curation ruleset (curate_target). A
  # non-NULL genetic_code arg is stored as an explicit override on the default
  # curate_opts set; NULL means auto-from-ruleset.
  gc_override <- if (is.null(genetic_code)) NA_integer_ else as.integer(genetic_code)
  resolved_genetic_code <- resolve_genetic_code(curate_target, gc_override)

  # Create sqlite connection
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_path)
  on.exit(DBI::dbDisconnect(con))

  # Metadata table ----
  mapping <- mapping |>
    dplyr::mutate(
      ID = .data[[mapping_id]],
      Taxon = .data[[mapping_taxon]],
      genetic_code = resolved_genetic_code
    )
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
        fastp = .fastp_set_dedup(
          "--trim_poly_g --correction --detect_adapter_for_pe",
          dedup
        )
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
      blast_opts TEXT,
      blast_accession TEXT,
      blast_accession_auto TEXT,
      blast_species TEXT,
      blast_pident REAL,
      blast_qcovs REAL,
      blast_evalue REAL,
      blast_lineage TEXT,
      synteny_accession TEXT,
      maptoref_ref TEXT,
      poor_blast_ref TEXT,
      join_notes TEXT,
      join_switch INTEGER,
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
          blast_opts = "default",
          poor_blast_ref = NA_character_,
          join_notes = NA_character_,
          join_switch = NA_integer_,
          maptoref_ref = NA_character_,
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
      assembler TEXT,
      mitofinder_db TEXT,
      mitofinder TEXT,
      max_paths INTEGER,
      max_scaffolds INTEGER,
      min_assembly_length INTEGER,
      join_scaffolds INTEGER,
      maptoref_ref TEXT,
      maptoref TEXT,
      maptoref_consensus TEXT,
      maptoref_iter INTEGER,
      maptoref_topology TEXT,
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
        assembler = assembler,
        getOrganelle = getOrganelle,
        mitofinder_db = mitofinder_db,
        mitofinder = mitofinder,
        max_paths = max_paths,
        max_scaffolds = max_scaffolds,
        min_assembly_length = min_assembly_length,
        join_scaffolds = 0L,
        maptoref_ref = maptoref_ref,
        maptoref = maptoref,
        maptoref_consensus = maptoref_consensus,
        maptoref_iter = as.integer(maptoref_iter),
        maptoref_topology = maptoref_topology
      ),
      in_place = TRUE,
      copy = TRUE,
      by = "assemble_opts"
    )

  # A sample that brought its own reference gets its own MapToRef parameter set,
  # so the app shows the assembler and the reference that sample will actually
  # use. Must follow the assemble_opts insert above: the set is cloned from it.
  .mtr_seed_per_sample_opts(con, refs)

  ## BLAST options ----
  DBI::dbExecute(
    con,
    "CREATE TABLE blast_opts (
      blast_opts TEXT NOT NULL,
      run_blast INTEGER,
      entrez_query TEXT,
      taxids TEXT,
      remote_blast INTEGER,
      remote_fallback INTEGER,
      extra_opts TEXT,
      max_target_seqs INTEGER,
      PRIMARY KEY (blast_opts)
    );"
  )
  dplyr::tbl(con, "blast_opts") |>
    dplyr::rows_upsert(
      data.frame(
        blast_opts      = "default",
        run_blast       = 1L,
        entrez_query    = "mitochondrion[Location]",
        taxids          = "",
        remote_blast    = 0L,
        remote_fallback = 1L,
        extra_opts      = "",
        max_target_seqs = 5L
      ),
      in_place = TRUE,
      copy = TRUE,
      by = "blast_opts"
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
      edit_positions TEXT,
      time_stamp INTEGER,
      PRIMARY KEY (ID, path, scaffold)
    );"
  )

  ## Per-path BLAST hits ----
  DBI::dbExecute(
    con,
    "CREATE TABLE assembly_blast (
      ID TEXT NOT NULL,
      path INTEGER NOT NULL,
      blast_opts TEXT,
      blast_accession TEXT,
      blast_species TEXT,
      blast_pident REAL,
      blast_qcovs REAL,
      blast_evalue REAL,
      blast_lineage TEXT,
      time_stamp INTEGER,
      PRIMARY KEY (ID, path)
    );"
  )

  ## Precomputed scaffold->reference mappings (one row per ID/scaffold/ref) ----
  ## Written by the Nextflow scaffold-join so the in-app manual join editor can
  ## build layouts with no minimap2 dependency.
  DBI::dbExecute(
    con,
    "CREATE TABLE scaffold_mappings (
      ID TEXT NOT NULL,
      ref_accession TEXT NOT NULL,
      scaffold INTEGER NOT NULL,
      ref_start INTEGER,
      ref_end INTEGER,
      strand TEXT,
      nmatch INTEGER,
      qcov REAL,
      qstart INTEGER,
      mapped INTEGER,
      PRIMARY KEY (ID, ref_accession, scaffold)
    );"
  )

  DBI::dbExecute(
    con,
    "CREATE TABLE scaffold_junctions (
      ID TEXT NOT NULL,
      junction INTEGER NOT NULL,
      gap_index INTEGER NOT NULL,
      start INTEGER,
      end INTEGER,
      gap_bases INTEGER,
      size_known INTEGER,
      time_stamp INTEGER,
      PRIMARY KEY (ID, gap_index)
    );"
  )

  # Add Annotate table ----
  DBI::dbExecute(
    con,
    "CREATE TABLE annotate (
      ID TEXT NOT NULL,
      ID_verified TEXT,
      path INTEGER NOT NULL DEFAULT 1,
      scaffold INTEGER NOT NULL DEFAULT 1,
      scaffolds INTEGER,
      annotate_opts TEXT,
      curate_opts TEXT,
      orf_opts TEXT,
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
      partial TEXT,
      structure TEXT,
      length INTEGER,
      topology TEXT,
      time_stamp INTEGER,
      PRIMARY KEY (ID, path, scaffold)
    );"
  )
  dplyr::tbl(con, "annotate") |>
    dplyr::rows_upsert(
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
      by = c("ID", "path", "scaffold")
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
      use_mitos INTEGER,
      mitos_opts TEXT,
      use_mitos_best INTEGER,
      rescue_no_trna INTEGER,
      use_trnaScan INTEGER,
      trnaScan_opts TEXT,
      arwen_opts TEXT,
      use_arwen INTEGER,
      aragorn_opts TEXT,
      use_aragorn INTEGER,
      use_mitofinder INTEGER,
      mitofinder_db TEXT,
      mitofinder_new_genes INTEGER,
      mitofinder_allow_introns INTEGER,
      mitofinder_opts TEXT,
      start_gene TEXT,
      coverage_trim INTEGER,
      feature_trim INTEGER,
      ref_based_rc INTEGER,
      retain_low_conf_trna INTEGER,
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
        use_mitos = 1L,
        mitos_opts = mitos_opts,
        use_mitos_best = 1L,
        rescue_no_trna = 1L,
        use_trnaScan = 1L,
        trnaScan_opts = trnaScan_opts,
        arwen_opts = arwen_opts,
        use_arwen = 0L,
        aragorn_opts = aragorn_opts,
        use_aragorn = 0L,
        use_mitofinder = 0L,
        mitofinder_db = NA_character_,
        mitofinder_new_genes = 0L,
        mitofinder_allow_introns = 0L,
        mitofinder_opts = "",
        start_gene = "trnF",
        coverage_trim = 1L,
        feature_trim = 1L,
        ref_based_rc = 0L,
        retain_low_conf_trna = 0L
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
      linear_complete INTEGER,
      genetic_code INTEGER,
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
        max_blast_hits = 10,
        ref_db = curate_ref_db,
        ref_dir = annotate_ref_dir,
        linear_complete = as.integer(isTRUE(linear_complete)),
        genetic_code = gc_override,
        params = jsonlite::toJSON(curate_params)
      ),
      in_place = TRUE,
      copy = TRUE,
      by = "curate_opts"
    )

  ## ORF-finder options ----
  DBI::dbExecute(
    con,
    "CREATE TABLE orf_opts (
      orf_opts TEXT NOT NULL,
      use_orffinder INTEGER,
      cpus INTEGER,
      memory INTEGER,
      orffinder_opts TEXT,
      orf_min_len INTEGER,
      orf_max_overlap REAL,
      orf_nested INTEGER,
      PRIMARY KEY (orf_opts)
    );"
  )
  dplyr::tbl(con, "orf_opts") |>
    dplyr::rows_upsert(
      data.frame(
        orf_opts = "default",
        use_orffinder = 0L,
        cpus = orf_cpus,
        memory = orf_memory,
        orffinder_opts = orffinder_opts,
        orf_min_len = orf_min_len,
        orf_max_overlap = orf_max_overlap,
        orf_nested = 0L
      ),
      in_place = TRUE,
      copy = TRUE,
      by = "orf_opts"
    )

  ## Export options ----
  DBI::dbExecute(
    con,
    "CREATE TABLE export_opts (
      export_opts TEXT NOT NULL,
      fasta_header TEXT,
      fasta_header_gene TEXT,
      PRIMARY KEY (export_opts)
    );"
  )
  dplyr::tbl(con, "export_opts") |>
    dplyr::rows_upsert(
      data.frame(
        export_opts = "default",
        fasta_header = DEFAULT_FASTA_HEADER,
        fasta_header_gene = DEFAULT_FASTA_HEADER_GENE
      ),
      in_place = TRUE,
      copy = TRUE,
      by = "export_opts"
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
      tool TEXT,
      start_codon TEXT,
      stop_codon TEXT,
      partial_start INTEGER,
      partial_stop INTEGER,
      translation TEXT,
      notes TEXT,
      warnings TEXT,
      refHits JSON,
      edited INTEGER,
      time_stamp INTEGER,
      PRIMARY KEY (ID, path, scaffold, gene, pos1)
    );"
  )

  # Pre-edit snapshots for the annotate modal's "Restore assembly"
  DBI::dbExecute(con, ASSEMBLY_BACKUP_DDL)

  DBI::dbExecute(
    con,
    "CREATE TABLE blast_ref_annotations (
      accession TEXT NOT NULL,
      gene TEXT NOT NULL,
      type TEXT,
      pos1 INTEGER,
      pos2 INTEGER,
      direction TEXT,
      ref_length INTEGER,
      time_stamp INTEGER,
      PRIMARY KEY (accession, gene, pos1)
    );"
  )

  # One row per sample per retained BLAST candidate reference (rank 1 = top hit).
  # Drives the reference picker in the annotate-details synteny plot.
  DBI::dbExecute(
    con,
    "CREATE TABLE blast_ref_candidates (
      ID TEXT NOT NULL,
      path INTEGER NOT NULL,
      scaffold INTEGER NOT NULL,
      rank INTEGER,
      accession TEXT NOT NULL,
      species TEXT,
      pident REAL,
      qcovs REAL,
      evalue REAL,
      time_stamp INTEGER,
      PRIMARY KEY (ID, path, scaffold, accession)
    );"
  )

  # One row per NCBI accession; shared across samples with the same BLAST hit
  DBI::dbExecute(
    con,
    "CREATE TABLE blast_ref_sequences (
      accession TEXT NOT NULL,
      sequence TEXT NOT NULL,
      ref_length INTEGER,
      genetic_code INTEGER,
      lineage TEXT,
      topology TEXT,
      time_stamp INTEGER,
      PRIMARY KEY (accession)
    );"
  )

  # One row per sample per candidate reference; pre-computed whole-genome
  # pairwise alignment. aligned_sample / aligned_ref are gap-character strings from pwalign
  DBI::dbExecute(
    con,
    "CREATE TABLE blast_ref_alignment (
      ID TEXT NOT NULL,
      path INTEGER NOT NULL DEFAULT 1,
      scaffold INTEGER NOT NULL DEFAULT 1,
      accession TEXT NOT NULL,
      aligned_sample TEXT NOT NULL,
      aligned_ref TEXT NOT NULL,
      rotation INTEGER NOT NULL DEFAULT 0,
      ref_length INTEGER NOT NULL,
      ref_start INTEGER NOT NULL DEFAULT 0,
      strand TEXT NOT NULL DEFAULT '+',
      time_stamp INTEGER,
      PRIMARY KEY (ID, path, scaffold, accession)
    );"
  )

  # User's chosen reference per assembly unit ("Set as best reference"), overriding
  # that scaffold's own BLAST top hit. Its own table for the same reason as `export`:
  # the pipeline rewrites assemblies/annotate rows wholesale and would drop it.
  DBI::dbExecute(
    con,
    "CREATE TABLE blast_ref_override (
      ID TEXT NOT NULL,
      path INTEGER NOT NULL DEFAULT 1,
      scaffold INTEGER NOT NULL DEFAULT 1,
      accession TEXT NOT NULL,
      time_stamp INTEGER,
      PRIMARY KEY (ID, path, scaffold)
    );"
  )

  # Export state, one row per assembly unit. Deliberately its own table rather than
  # columns on annotate/assemblies: the pipeline writes those with INSERT OR REPLACE
  # and an explicit column list, which would null anything it does not name.
  DBI::dbExecute(
    con,
    "CREATE TABLE export (
      ID TEXT NOT NULL,
      path INTEGER NOT NULL DEFAULT 1,
      scaffold INTEGER NOT NULL DEFAULT 1,
      export_group TEXT,
      export_time_stamp INTEGER,
      PRIMARY KEY (ID, path, scaffold)
    );"
  )

  .mtr_warn_missing_refs(con)

  invisible(return())
}
