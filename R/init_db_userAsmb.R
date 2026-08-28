#' Initialize a new project database
#'
#' @param db_path Path to the new database file
#' @param mapping_fn Path to the mapping CSV file. Must contain columns "ID",
#'   "Taxon, "R1", "R2", and "Assembly". An optional "Topology" column may
#'   declare "circular" or "linear" for a single-contig assembly.
#' @param mapping_id Column name of the mapping file to use as the primary key
#' @param mapping_taxon Column name of the mapping file containing a Taxonomic
#'   identifier (eg, species name)
#' @param assembly_path Directory holding the user-supplied assembly files. Used
#'   to count each assembly's contigs so a multi-contig assembly is recorded with
#'   topology "multi".
#' @param genetic_code Optional NCBI translation table override. Default `NULL`
#'   auto-selects from the curation ruleset; a number sets an override on the
#'   default curate_opts set. https://www.ncbi.nlm.nih.gov/Taxonomy/Utils/wprintgc.cgi
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
#' @param min_assembly_length Minimum scaffold length to include in analysis (default = 500)
#' @param join_scaffolds (logical) Order a fragmented single-path assembly
#'   against its BLAST reference into one joined sequence during WF1 (default =
#'   FALSE). Samples whose contigs match different reference mitogenomes are
#'   left alone.
#' @param find_mitogenome Search each user-supplied assembly for its
#'   mitochondrial contigs before the rest of WF1 runs (default = FALSE). See
#'   [find_mito()].
#' @param mitofinder_db Path to a MitoFinder GenBank database, built with
#'   [custom_assembly_db()] (`db_type = "mitofinder"`). Required when
#'   `find_mitogenome = TRUE`.
#' @param find_min_contig_length Contigs shorter than this are never searched,
#'   bp (default = 500)
#' @param find_min_identity Percent identity required against the reference
#'   (default = 70)
#' @param find_min_aligned_length Aligned bases required (default = 300)
#' @param find_min_aligned_fraction Fraction of the contig the alignment must
#'   cover (default = 0.5). The NUMT filter.
#' @param find_max_candidates Most contigs carried into MitoFinder confirmation
#'   (default = 20)
#' @param find_min_genes Mitochondrial genes a contig must carry to be confirmed
#'   (default = 3)
#' @param find_cpus Default # cpus for the search steps (default = 4)
#' @param find_memory Default memory (GB) for the search steps (default = 8)
#' @param attempt_circularization Try to circularize linear single-contig
#'   assemblies in WF1 (default = FALSE). See [circularize_asmb()].
#' @param circularize_min_overlap Shortest accepted self-overlap, bp (default = 220)
#' @param circularize_min_identity Percent identity required for the self-overlap
#'   (default = 99)
#' @param circularize_min_junction_reads Reads that must span the new junction
#'   before an assembly is called circular (default = 5). Ignored when the
#'   project has no raw data.
#' @param circularize_min_overhang Bases a read must extend past the junction on
#'   each side to count (default = 30)
#' @param circularize_cpus Default # cpus for the circularization step (default = 4)
#' @param circularize_memory Default memory (GB) for the circularization step
#'   (default = 8)
#' @param no_raw_data (logical) Initialize a project with no raw reads (default =
#'   FALSE). When TRUE, annotation coverage trimming (`coverage_trim`) is disabled
#'   since no read-depth information is available.
#' @export
#'
new_db_userAsmb <- function(
    db_path = "./.sqlite",
    mapping_fn = NULL,
    mapping_id = "ID",
    mapping_taxon = "Taxon",
    assembly_path = NULL,
    genetic_code = NULL,
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
    orf_max_overlap = 0.1,
    # Default assembly QC threshold (used by COVERAGE_userAsmb + BLAST_GENBANK to
    # set per-scaffold ignore flags; matches the regular pipeline default)
    min_assembly_length = 500,
    # Scaffold joining: order a fragmented single-path assembly against its BLAST
    # reference into one Path 0. Off by default, as in the regular pipeline.
    join_scaffolds = FALSE,
    # Default mitogenome-search options (see find_mito())
    find_mitogenome = FALSE,
    mitofinder_db = NULL,
    find_min_contig_length = 500,
    find_min_identity = 70,
    find_min_aligned_length = 300,
    find_min_aligned_fraction = 0.5,
    find_max_candidates = 20,
    find_min_genes = 3,
    find_cpus = 4,
    find_memory = 8,
    # Default circularization options (see circularize_asmb())
    attempt_circularization = FALSE,
    circularize_min_overlap = 220,
    circularize_min_identity = 99,
    circularize_min_junction_reads = 5,
    circularize_min_overhang = 30,
    circularize_cpus = 4,
    circularize_memory = 8,
    # Skip read-mapping coverage (no raw data). Disables annotate coverage trim.
    no_raw_data = FALSE) {
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

  # check for the assembly column; Topology is optional
  if ("Assembly" %nin% colnames(mapping)) {
    stop("Mapping file missing Assembly column")
  }
  validate_declared_topology(mapping, mapping_id = mapping_id)

  # No raw reads: R1/R2 are not needed, so tolerate their absence in the mapping.
  # Added here (before samples/preprocess are built) so both tables carry the
  # columns as NA, matching the read-based schema the app/export code expects.
  if (no_raw_data) {
    if (!"R1" %in% colnames(mapping)) mapping$R1 <- NA_character_
    if (!"R2" %in% colnames(mapping)) mapping$R2 <- NA_character_
  }

  # Load default curation parameters
  if (is.null(curate_params)) {
    curate_params <- do.call(paste0("params_", curate_target), list())
  }

  # Genetic code auto-selects from the curation ruleset; a non-NULL arg is
  # stored as an explicit override on the default curate_opts set.
  gc_override <- if (is.null(genetic_code)) NA_integer_ else as.integer(genetic_code)
  resolved_genetic_code <- resolve_genetic_code(curate_target, gc_override)

  # Create sqlite connection
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_path)
  on.exit(DBI::dbDisconnect(con))

  # Metadata table ----
  # Resolved outside the mutate so a warning reaches the user as itself, not
  # wrapped in dplyr's "there was 1 warning in mutate()" report.
  sample_topology <- resolve_sample_topology(mapping, assembly_path, mapping_id)
  mapping <- mapping |>
    dplyr::mutate(
      ID = .data[[mapping_id]],
      Taxon = .data[[mapping_taxon]],
      genetic_code = resolved_genetic_code,
      topology = sample_topology,
      assembly = .data[["Assembly"]]
    ) |>
    dplyr::select(-dplyr::any_of("Topology"), -Assembly)
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
      circularize_opts TEXT,
      circularize_notes TEXT,
      find_mito_opts TEXT,
      find_mito_notes TEXT,
      blast_opts TEXT,
      blast_accession TEXT,
      blast_accession_auto TEXT,
      blast_species TEXT,
      blast_pident REAL,
      blast_qcovs REAL,
      blast_evalue REAL,
      blast_lineage TEXT,
      synteny_accession TEXT,
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
          assemble_opts = "user",
          circularize_opts = "default",
          circularize_notes = NA_character_,
          find_mito_opts = "default",
          find_mito_notes = NA_character_,
          blast_opts = "default",
          poor_blast_ref = NA_character_,
          join_notes = NA_character_,
          join_switch = NA_integer_,
          time_stamp = NA_integer_
        ),
      in_place = TRUE,
      copy = TRUE,
      by = "ID"
    )

  ## Assemble options ----
  # Minimal table for userAsmb: only the columns the Nextflow workflows and the
  # app actually query (assemble_opts FK + min_assembly_length). The regular
  # pipeline schema carries assembler/getOrganelle/etc. fields that don't apply
  # when assemblies are user-provided.
  DBI::dbExecute(
    con,
    "CREATE TABLE assemble_opts (
      assemble_opts TEXT NOT NULL,
      min_assembly_length INTEGER,
      join_scaffolds INTEGER,
      PRIMARY KEY (assemble_opts)
    );"
  )
  dplyr::tbl(con, "assemble_opts") |>
    dplyr::rows_upsert(
      data.frame(
        assemble_opts = "user",
        min_assembly_length = min_assembly_length,
        join_scaffolds = as.integer(isTRUE(join_scaffolds))
      ),
      in_place = TRUE,
      copy = TRUE,
      by = "assemble_opts"
    )

  ## Circularization options ----
  # Named settings profile for the optional WF1 circularization step
  # (see circularize_asmb()). `attempt` is the master on/off switch.
  DBI::dbExecute(
    con,
    "CREATE TABLE circularize_opts (
      circularize_opts TEXT NOT NULL,
      attempt INTEGER,
      min_overlap INTEGER,
      min_identity REAL,
      min_junction_reads INTEGER,
      min_overhang INTEGER,
      cpus INTEGER,
      memory INTEGER,
      PRIMARY KEY (circularize_opts)
    );"
  )
  dplyr::tbl(con, "circularize_opts") |>
    dplyr::rows_upsert(
      data.frame(
        circularize_opts = "default",
        attempt = as.integer(attempt_circularization),
        min_overlap = circularize_min_overlap,
        min_identity = circularize_min_identity,
        min_junction_reads = circularize_min_junction_reads,
        min_overhang = circularize_min_overhang,
        cpus = circularize_cpus,
        memory = circularize_memory
      ),
      in_place = TRUE,
      copy = TRUE,
      by = "circularize_opts"
    )

  ## Circularization evidence ----
  # What the overlap search found for each sample, and the read depth across
  # the junction it produced. Read by the circularization details modal.
  DBI::dbExecute(
    con,
    "CREATE TABLE circularize_overlap (
      ID TEXT NOT NULL,
      contig TEXT NOT NULL,
      qstart INTEGER,
      qend INTEGER,
      sstart INTEGER,
      send INTEGER,
      length INTEGER,
      pident REAL,
      mismatches INTEGER,
      aln_query TEXT,
      aln_subject TEXT,
      q_ctx_left TEXT,
      q_ctx_right TEXT,
      s_ctx_left TEXT,
      s_ctx_right TEXT,
      accepted INTEGER,
      reason TEXT,
      contig_length INTEGER,
      trimmed INTEGER,
      junction_reads INTEGER,
      min_junction_reads INTEGER,
      window_bp INTEGER,
      min_overhang INTEGER,
      time_stamp INTEGER,
      PRIMARY KEY (ID, contig)
    );"
  )
  DBI::dbExecute(
    con,
    "CREATE TABLE circularize_depth (
      ID TEXT NOT NULL,
      contig TEXT NOT NULL,
      position INTEGER NOT NULL,
      rel_position INTEGER,
      depth INTEGER,
      depth_spanning INTEGER,
      time_stamp INTEGER,
      PRIMARY KEY (ID, contig, position)
    );"
  )

  ## Mitogenome search options ----
  # Settings for the optional WF1 step that locates mitochondrial contigs in a
  # large user-supplied assembly (see find_mito()).
  DBI::dbExecute(
    con,
    "CREATE TABLE find_mito_opts (
      find_mito_opts TEXT NOT NULL,
      attempt INTEGER,
      mitofinder_db TEXT,
      min_contig_length INTEGER,
      min_identity REAL,
      min_aligned_length INTEGER,
      min_aligned_fraction REAL,
      max_candidates INTEGER,
      min_genes INTEGER,
      cpus INTEGER,
      memory INTEGER,
      PRIMARY KEY (find_mito_opts)
    );"
  )
  dplyr::tbl(con, "find_mito_opts") |>
    dplyr::rows_upsert(
      data.frame(
        find_mito_opts = "default",
        attempt = as.integer(find_mitogenome),
        mitofinder_db = mitofinder_db %||% NA_character_,
        min_contig_length = find_min_contig_length,
        min_identity = find_min_identity,
        min_aligned_length = find_min_aligned_length,
        min_aligned_fraction = find_min_aligned_fraction,
        max_candidates = find_max_candidates,
        min_genes = find_min_genes,
        cpus = find_cpus,
        memory = find_memory
      ),
      in_place = TRUE,
      copy = TRUE,
      by = "find_mito_opts"
    )

  ## Mitogenome search evidence ----
  # One row per screened candidate contig: what the search saw and why each
  # contig was kept or dropped.
  DBI::dbExecute(
    con,
    "CREATE TABLE mito_candidates (
      ID TEXT NOT NULL,
      contig TEXT NOT NULL,
      length INTEGER,
      accession TEXT,
      pident REAL,
      aligned_length INTEGER,
      aligned_fraction REAL,
      genes INTEGER,
      rank INTEGER,
      selected INTEGER,
      reason TEXT,
      time_stamp INTEGER,
      PRIMARY KEY (ID, contig)
    );"
  )

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
      edit_positions TEXT,
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

  ## Precomputed scaffold->reference mappings (see init_db) ----
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
        coverage_trim = if (no_raw_data) 0L else 1L,
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

  # User's chosen reference per assembly unit. See init_db() for why this is its own
  # table and not columns on annotate/assemblies.
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

  # Export state, one row per assembly unit. See init_db() for why this is its own
  # table and not columns on annotate/assemblies.
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

  invisible(return())
}
