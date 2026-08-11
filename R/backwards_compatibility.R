#' Update old project database for backwards compatibility
#'
#' Migrates an old project's SQLite database to the current schema and refreshes
#' the curation rules. Each migration step is guarded by an existence check, so
#' the function is idempotent and safe to re-run; already-current projects report
#' "nothing to update" and exit without changes. Migrations include, among
#' others:
#' \itemize{
#'   \item \code{annotate}: "reviewed", "ID_verified", "problematic", "partial",
#'     "orf_opts" columns.
#'   \item \code{annotate_opts}: "start_gene", ARWEN / Aragorn / MitoFinder /
#'     tRNAscan / MITOS toggles and options, coverage/feature trimming, and
#'     reference db columns.
#'   \item \code{assemble_opts}: "assembler", "mitofinder_db"/"mitofinder",
#'     "max_paths", "max_scaffolds", "min_assembly_length", "join_scaffolds".
#'   \item \code{curate_opts}: "max_blast_hits", "ref_dir", "ref_db",
#'     "linear_complete" (and rewriting the legacy in-container Mitos2 ref path
#'     to the GitHub-hosted reference db).
#'   \item \code{assemble}: "poor_blast_ref" (migrated from \code{samples} and
#'     normalized to TEXT), BLAST result columns, "blast_opts".
#'   \item \code{samples}: numeric "genetic_code" (rebuilding any legacy TEXT
#'     column as INTEGER so assembly does not crash).
#'   \item New tables: "orf_opts", "blast_opts", "export_opts",
#'     "scaffold_mappings", "assemblies", "assembly_blast", and the
#'     "blast_ref_annotations"/"blast_ref_sequences"/"blast_ref_alignment" set.
#' }
#'
#' When `update_config = TRUE` the Nextflow `.config` is regenerated wholesale
#' from the template for the supplied `executor` (rather than patched line by
#' line): the project-specific values (raw/asmb dirs, min depth, genetic code,
#' NCBI key, and, for generic HPC templates, queue / penv / clusterOptions /
#' container engine) are carried over, the container is bumped to the current
#' MitoPilot version, and the previous `.config` is saved to a timestamped
#' `.config.bak.<timestamp>` backup. Because an existing config may carry hand
#' edits that cannot be parsed reliably (e.g. a multi-line `clusterOptions`
#' closure), the executor must be given explicitly and you should review the
#' regenerated config against the backup.
#'
#' @param path Path to the project directory (default = current working directory)
#' @param executor The executor whose config template to regenerate from. Same
#'   options as [new_project()]: a built-in template ("local", "awsbatch",
#'   "slurm", "sge", "pbs", "lsf", "NMNH_Hydra", "NOAA_SEDNA") or the name of a
#'   saved cluster profile (see [list_configs()]). Required when
#'   `update_config = TRUE`.
#' @param update_config Regenerate the Nextflow `.config` from the `executor`
#'   template (default `TRUE`). Set to `FALSE` to migrate only the SQLite
#'   database and leave the existing `.config` untouched.
#'
#' @export
#'
backwards_compatibility <- function(
    path = ".",
    executor = NULL,
    update_config = TRUE
){
  # Validate the executor up front (before any DB writes) so we fail fast rather
  # than half-migrate and then error on config regeneration.
  if (isTRUE(update_config)) {
    if (is.null(executor) || !nzchar(executor)) {
      stop(
        "`executor` is required when update_config = TRUE. Choose one of: ",
        paste(list_configs()$name, collapse = ", "),
        ".\nOr call backwards_compatibility(..., update_config = FALSE) to ",
        "migrate only the database.",
        call. = FALSE
      )
    }
    resolve_config(executor)  # errors early if the executor is unknown
  }
  # update SQL database with "reviewed" column for annotations table
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = file.path(path, ".sqlite")) # open connection
  on.exit(DBI::dbDisconnect(con))

  # Back up the database before the first migration write, and only then: several
  # steps below rebuild tables (DROP + RENAME) and drop columns, none of which
  # SQLite can undo, and a migrated database cannot be reopened by an older
  # MitoPilot. Mirrors the backup add_samples()/update_sample_metadata() already
  # make. `changed` doubles as the "was there anything to do" flag.
  changed <- FALSE
  ensure_backup <- function() {
    if (changed) {
      return(invisible(NULL))
    }
    changed <<- TRUE
    backup_dir <- file.path(path, ".old_sqlite_dbs")
    if (!dir.exists(backup_dir)) {
      dir.create(backup_dir, recursive = TRUE)
      num <- 1
    } else {
      backups <- list.files(backup_dir, pattern = "^\\.sqlite\\.[0-9]+$", all.files = TRUE)
      num <- if (length(backups) == 0) {
        1
      } else {
        max(as.numeric(sapply(strsplit(backups, "[.]"), "[", 3)), na.rm = TRUE) + 1
      }
    }
    backup <- file.path(backup_dir, paste0(".sqlite.", num))
    if (!file.copy(file.path(path, ".sqlite"), backup)) {
      stop("Could not back up the project database to ", backup,
           ". Refusing to migrate without a backup.", call. = FALSE)
    }
    message("Backed up project database to: ", backup)
  }

  add_col <- function(table, column, type, default = NULL) {
    add_opt_col(con, table, column, type, default, before = ensure_backup)
  }

  samples_table <- DBI::dbReadTable(con, "samples") # read in annotations table
  annotate_opts_table <- DBI::dbReadTable(con, "annotate_opts") # read in annotations opts table
  curate_opts_table <- DBI::dbReadTable(con, "curate_opts") # read in curate opts table

  assemble_table <- DBI::dbReadTable(con, "assemble")

  # read .config; the .config is regenerated wholesale from the current built-in
  # template (see migrate_config()) rather than patched line by line. Container
  # version currency is the proxy for "config is already up to date".
  conf <- tryCatch({
    readLines(file.path(path, ".config"))
  }, error = function(e) {
    stop("Error reading .config file: ", e$message)
  })
  new_container = paste0("macguigand/mitopilot:", utils::packageVersion("MitoPilot"))
  containerVer <- any(grep(new_container, conf, fixed = TRUE))

  # genetic_code must be a plain INTEGER: assemble.nf calls genetic_code.intValue()
  # and MITOS2's -c argparse rejects a float like '2.0'. Older projects stored it as
  # TEXT ('2') or REAL (2.0); either must be rebuilt to INTEGER affinity. Only a
  # genuine integer column is treated as current.
  genetic_code_numeric <- isTRUE(tryCatch(
    !any(c("text", "real") %in% DBI::dbGetQuery(
      con, "SELECT DISTINCT typeof(genetic_code) AS t FROM samples"
    )$t),
    error = function(e) TRUE
  ))

  # detect the old in-container Mitos2 ref path in the curate_opts params JSON.
  # (Only the params JSON is a safe trigger: migration strips this exact path
  # from params, and the replacement github-raw URL still contains the substring
  # "/ref_dbs/Mitos2" so matching annotate_opts$ref_dir would re-fire the block
  # on already-migrated projects.)
  old_ref_str <- any(grepl("/ref_dbs/Mitos2", curate_opts_table$params))

  # Superseded default export headers, for detecting unmodified templates. Derived
  # from the frozen {ID}-era template, NOT from DEFAULT_FASTA_HEADER: the default has
  # since changed to {seqid}, so deriving from the current value would no longer match
  # what is actually stored in older projects.
  old_export_defaults <- c(
    # pre-{completeness}
    sub("{completeness}", "complete genome", LEGACY_FASTA_HEADER_ID, fixed = TRUE),
    # pre-{seqid}
    LEGACY_FASTA_HEADER_ID
  )
  export_default_current <- tryCatch(
    !("export_opts" %in% DBI::dbListTables(con)) ||
      !any(DBI::dbReadTable(con, "export_opts")$fasta_header %in% old_export_defaults,
           na.rm = TRUE),
    error = function(e) TRUE
  )

  # The .config is stale and the caller opted into regenerating it, so this run
  # changes something even if the database itself is current.
  if (update_config && !containerVer) ensure_backup()

  # update annotation and curation reference databases
  if(old_ref_str){
    ensure_backup()
    message("updated the annotate_opts table with new ref_dir and ref_db values")
    # update annotate ref_dir path
    annotate_opts_table$ref_dir <- rep("https://raw.githubusercontent.com/Smithsonian/MitoPilot/refs/heads/main/ref_dbs/Mitos2",
                                       nrow(annotate_opts_table))
    # update annotate ref_db name (exact match on the ref_db column; the prior
    # `%in% annotate_opts_table` compared against deparsed columns and never hit)
    if("ref_db" %in% names(annotate_opts_table) &&
       "Metazoa" %in% annotate_opts_table$ref_db){
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

    curate_opts_fields <- DBI::dbListFields(con, "curate_opts")
    if (!("ref_dir" %in% curate_opts_fields)) {
      DBI::dbExecute(con, "ALTER TABLE curate_opts ADD COLUMN ref_dir TEXT;")
    }
    if (!("ref_db" %in% curate_opts_fields)) {
      DBI::dbExecute(con, "ALTER TABLE curate_opts ADD COLUMN ref_db TEXT;")
    }

    dplyr::tbl(con, "curate_opts") |> # update SQL database
      dplyr::rows_upsert(
        curate_opts_table,
        in_place = TRUE,
        copy = TRUE,
        by = "curate_opts"
      )
  }

  # NOTE: the .config is regenerated from the current built-in template at the
  # end of this function (see migrate_config()), which adds asmbDir, failOnIgnore,
  # the orffinder_condaenv param, the orf { } process block, blast_gb, and bumps
  # the container version in one pass.

  # store genetic_code as INTEGER so assemble.nf's genetic_code.intValue() works
  # (fresh projects store a number here; a TEXT value crashes assembly).
  add_col("samples", "genetic_code", "INTEGER", 2L)

  # normalize any legacy TEXT/REAL genetic_code to INTEGER (idempotent). Older
  # projects stored it as TEXT ('2', crashes assemble.nf's genetic_code.intValue())
  # or REAL (2.0, which MITOS2's -c argparse rejects as an invalid int). A plain
  # UPDATE/CAST is not enough: the column's affinity coerces the result straight
  # back, so the column itself must be rebuilt with INTEGER affinity.
  if (!genetic_code_numeric) {
    ensure_backup()
    message("rebuilt 'genetic_code' column in samples table as integer")
    # Wrap the multi-step rebuild so a mid-sequence failure rolls back cleanly
    # instead of leaving a half-migrated table.
    DBI::dbBegin(con)
    tryCatch({
      DBI::dbExecute(con, "ALTER TABLE samples RENAME COLUMN genetic_code TO genetic_code_old")
      DBI::dbExecute(con, "ALTER TABLE samples ADD COLUMN genetic_code INTEGER")
      DBI::dbExecute(con, "UPDATE samples SET genetic_code = CAST(genetic_code_old AS INTEGER)")
      DBI::dbExecute(con, "ALTER TABLE samples DROP COLUMN genetic_code_old")
      DBI::dbCommit(con)
    }, error = function(e) {
      DBI::dbRollback(con)
      stop(e)
    })
  }

  # if poor_blast_ref column doesn't exist on assemble, add it (TEXT: good/poor/failed/NULL)
  if (!("poor_blast_ref" %in% names(assemble_table))) {
    ensure_backup()
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
  if (DBI::dbGetQuery(
        con,
        "SELECT COUNT(*) AS n FROM assemble WHERE typeof(poor_blast_ref) = 'integer'"
      )$n > 0) {
    ensure_backup()
    DBI::dbExecute(
      con,
      "UPDATE assemble SET poor_blast_ref = CASE
         WHEN poor_blast_ref = 1 THEN 'poor'
         WHEN poor_blast_ref = 0 THEN 'good'
         ELSE poor_blast_ref
       END
       WHERE typeof(poor_blast_ref) = 'integer'"
    )
  }

  add_col("assemble_opts", "join_scaffolds", "INTEGER", 0L)

  # precomputed scaffold->reference mappings table (for the in-app join editor)
  if (!DBI::dbExistsTable(con, "scaffold_mappings")) {
    ensure_backup()
    message("added 'scaffold_mappings' table")
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
  }

  add_col("annotate", "reviewed", "TEXT", "no")
  add_col("annotate", "ID_verified", "TEXT", "no")
  add_col("annotate", "problematic", "TEXT")
  add_col("annotate", "partial", "TEXT")
  add_col("annotations", "partial_start", "INTEGER")
  add_col("annotations", "partial_stop", "INTEGER")
  add_col("curate_opts", "linear_complete", "INTEGER", 0L)
  # genetic_code override column on curate_opts. Genetic code now auto-selects
  # from the curation ruleset, but PRESERVE existing projects: freeze the
  # project's current code (uniform in the old single-code model) as an explicit
  # override on every curate_opts set so re-running curate/annotate does not
  # change translations. Read it from .config, falling back to the samples table.
  if(!("genetic_code" %in% DBI::dbListFields(con, "curate_opts"))){
    ensure_backup()
    message("added 'genetic_code' override column to curate_opts table")
    DBI::dbExecute(con, "ALTER TABLE curate_opts ADD COLUMN genetic_code INTEGER")
    config_gc <- suppressWarnings(as.integer(stringr::str_trim(
      stringr::str_split(
        grep("genetic_code =", conf, value = TRUE)[1],
        "="
      )[[1]][2]
    )))
    if (length(config_gc) != 1 || is.na(config_gc)) {
      config_gc <- suppressWarnings(as.integer(
        DBI::dbGetQuery(
          con,
          "SELECT genetic_code FROM samples WHERE genetic_code IS NOT NULL LIMIT 1"
        )$genetic_code[1]
      ))
    }
    if (length(config_gc) != 1 || is.na(config_gc)) config_gc <- 2L
    DBI::dbExecute(
      con,
      glue::glue_sql("UPDATE curate_opts SET genetic_code = {config_gc}", .con = con)
    )
  }
  # optional annotate_opts columns, in the order they were introduced:
  # column, SQLite type, back-filled default (NULL leaves existing rows NULL)
  annotate_opt_cols <- list(
    list("use_arwen", "INTEGER", 0L),
    list("arwen_opts", "TEXT", "-mtx"),
    list("use_mitos_best", "INTEGER", 1L),
    list("rescue_no_trna", "INTEGER", 1L),
    list("use_mitos", "INTEGER", 1L),
    list("use_trnaScan", "INTEGER", 1L),
    list("use_aragorn", "INTEGER", 0L),
    list("aragorn_opts", "TEXT", "-m -gcstd"),
    list("use_mitofinder", "INTEGER", 0L),
    list("mitofinder_db", "TEXT", NULL),
    list("mitofinder_new_genes", "INTEGER", 0L),
    list("mitofinder_allow_introns", "INTEGER", 0L),
    list("mitofinder_opts", "TEXT", ""),
    list("coverage_trim", "INTEGER", 1L),
    list("feature_trim", "INTEGER", 1L),
    list("ref_based_rc", "INTEGER", 0L),
    list("retain_low_conf_trna", "INTEGER", 0L)
  )
  for (s in annotate_opt_cols) add_col("annotate_opts", s[[1]], s[[2]], s[[3]])

  # use_orffinder now lives in orf_opts (was annotate_opts). Add it to an existing
  # orf_opts table if missing (default off). A stale annotate_opts.use_orffinder
  # column on older databases is harmless and left in place. The orf_opts table is
  # created with use_orffinder below for databases that lack the table entirely.
  add_col("orf_opts", "use_orffinder", "INTEGER", 0L)
  add_col("orf_opts", "orf_nested", "INTEGER", 0L)
  add_col("annotate", "orf_opts", "TEXT", "default")

  # if orf_opts table doesn't exist, create it and seed a default row.
  # max_blast_hits, ref_db and ref_dir are shared with curation (curate_opts),
  # not stored here.
  if (!("orf_opts" %in% DBI::dbListTables(con))) {
    ensure_backup()
    message("created 'orf_opts' table")
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
          cpus = 4L,
          memory = 8L,
          orffinder_opts = "-s 1 -n true",
          orf_min_len = 300L,
          orf_max_overlap = 0.1,
          orf_nested = 0L
        ),
        in_place = TRUE,
        copy = TRUE,
        by = "orf_opts"
      )
  } else {
    # drop max_blast_hits/ref_db/ref_dir from older orf_opts tables; now sourced
    # from curate_opts
    orf_cols <- DBI::dbListFields(con, "orf_opts")
    for (col_drop in intersect(c("max_blast_hits", "ref_db", "ref_dir"), orf_cols)) {
      ensure_backup()
      message(paste0("removed '", col_drop, "' column from orf_opts table (now shared with curation)"))
      DBI::dbExecute(con, paste0("ALTER TABLE orf_opts DROP COLUMN ", col_drop))
    }
  }

  # optional opts columns, in the order they were introduced:
  # table, column, SQLite type, back-filled default
  opts_cols <- list(
    list("annotate_opts", "start_gene", "TEXT", "trnF"),
    list("curate_opts", "max_blast_hits", "INTEGER", 10L),
    list("curate_opts", "ref_dir", "TEXT", "/ref_dbs/Mitos2"),
    list("curate_opts", "ref_db", "TEXT", "Chordata"),
    list("assemble_opts", "assembler", "TEXT", "GetOrganelle"),
    list("assemble_opts", "mitofinder_db", "TEXT",
         paste0("https://raw.githubusercontent.com/Smithsonian/MitoPilot/",
                "refs/heads/devel-DJM/ref_dbs/MitoFinder/NC_002333_Danio_rerio.gb")),
    list("assemble_opts", "mitofinder", "TEXT", "--megahit"),
    list("assemble_opts", "max_paths", "INTEGER", 10L),
    list("assemble_opts", "max_scaffolds", "INTEGER", 10L),
    list("assemble_opts", "min_assembly_length", "INTEGER", 500L)
  )
  for (s in opts_cols) add_col(s[[1]], s[[2]], s[[3]], s[[4]])

  # if blast_accession column doesn't exist, add BLAST result columns
  if (!("blast_accession" %in% names(assemble_table))) {
    ensure_backup()
    message("added BLAST result columns to assemble table")
    DBI::dbExecute(con, "ALTER TABLE assemble ADD COLUMN blast_accession TEXT")
    DBI::dbExecute(con, "ALTER TABLE assemble ADD COLUMN blast_species TEXT")
    DBI::dbExecute(con, "ALTER TABLE assemble ADD COLUMN blast_pident REAL")
    DBI::dbExecute(con, "ALTER TABLE assemble ADD COLUMN blast_qcovs REAL")
    DBI::dbExecute(con, "ALTER TABLE assemble ADD COLUMN blast_evalue REAL")
    DBI::dbExecute(con, "ALTER TABLE assemble ADD COLUMN blast_lineage TEXT")
  }

  # Remember the automatic (rank-1) BLAST top hit so a user-set reference override
  # (which overwrites blast_accession) can be flagged in the sample tables.
  # Initialize to the current blast_accession (the auto value at migration time).
  # Placed after the blast_accession add above so the UPDATE column exists.
  if (!("blast_accession_auto" %in% DBI::dbListFields(con, "assemble"))) {
    ensure_backup()
    message("added 'blast_accession_auto' column to assemble table")
    DBI::dbExecute(con, "ALTER TABLE assemble ADD COLUMN blast_accession_auto TEXT")
    DBI::dbExecute(
      con,
      "UPDATE assemble SET blast_accession_auto = blast_accession
         WHERE blast_accession_auto IS NULL"
    )
  }

  # user-selected synteny-plot reference accession (NULL falls back to top hit)
  add_col("assemble", "synteny_accession", "TEXT")
  add_col("annotations", "tool", "TEXT")

  existing_tables <- DBI::dbListTables(con)

  if (!("blast_ref_annotations" %in% existing_tables)) {
    ensure_backup()
    message("created blast_ref_annotations table")
    DBI::dbExecute(con,
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
  } else if (!("accession" %in% DBI::dbListFields(con, "blast_ref_annotations"))) {
    ensure_backup()
    # Migrate ID-keyed table to accession-keyed: map each sample's rows to its
    # blast_accession (annotations are a property of the reference genome).
    message("re-keyed blast_ref_annotations table by accession")
    DBI::dbExecute(con,
      "CREATE TABLE blast_ref_annotations_new (
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
    DBI::dbExecute(con,
      "INSERT OR IGNORE INTO blast_ref_annotations_new
         (accession, gene, type, pos1, pos2, direction, ref_length, time_stamp)
       SELECT a.blast_accession, r.gene, r.type, r.pos1, r.pos2, r.direction, r.ref_length, r.time_stamp
       FROM blast_ref_annotations r
       JOIN assemble a ON r.ID = a.ID
       WHERE a.blast_accession IS NOT NULL"
    )
    DBI::dbExecute(con, "DROP TABLE blast_ref_annotations")
    DBI::dbExecute(con, "ALTER TABLE blast_ref_annotations_new RENAME TO blast_ref_annotations")
  }

  # max_target_seqs moved out of the nextflow .config into the blast_opts table
  # so it can be edited per parameter-set in the app. Add the column + backfill
  # the previous default (5) for existing projects.
  if ("blast_opts" %in% DBI::dbListTables(con) &&
      !("max_target_seqs" %in% DBI::dbListFields(con, "blast_opts"))) {
    ensure_backup()
    message("added max_target_seqs column to blast_opts table")
    DBI::dbExecute(con, "ALTER TABLE blast_opts ADD COLUMN max_target_seqs INTEGER")
    DBI::dbExecute(con, "UPDATE blast_opts SET max_target_seqs = 5 WHERE max_target_seqs IS NULL")
  }

  # Per-SCAFFOLD candidate references (rank 1 = that scaffold's top hit). Kept
  # separate per (ID, path, scaffold) so divergent scaffolds/paths never merge
  # into one ranked list. Legacy tables (per-sample, PK (ID, accession)) are
  # rebuilt; seed each scaffold's existing best hit as rank 1 from assemblies so
  # the in-app picker shows something before a re-run.
  .create_scaffold_candidates <- function() {
    DBI::dbExecute(con,
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
    # Seed the per-scaffold best hit from assemblies, but only if that table and
    # its per-scaffold BLAST columns already exist (an old project may not have an
    # assemblies table yet - it is created later in this migration - in which case
    # the candidates table is simply left empty for the pipeline to populate).
    asm_fields <- tryCatch(DBI::dbListFields(con, "assemblies"), error = function(e) character(0))
    if (all(c("path", "scaffold", "blast_accession", "blast_species",
              "blast_pident", "blast_qcovs", "blast_evalue") %in% asm_fields)) {
      DBI::dbExecute(con,
        "INSERT OR IGNORE INTO blast_ref_candidates
           (ID, path, scaffold, rank, accession, species, pident, qcovs, evalue, time_stamp)
         SELECT ID, path, scaffold, 1, blast_accession, blast_species, blast_pident, blast_qcovs, blast_evalue, time_stamp
         FROM assemblies
         WHERE blast_accession IS NOT NULL AND blast_accession != 'NO HIT'"
      )
    }
  }
  if (!("blast_ref_candidates" %in% DBI::dbListTables(con))) {
    ensure_backup()
    message("created blast_ref_candidates table (per-scaffold)")
    .create_scaffold_candidates()
  } else if (!all(c("path", "scaffold") %in% DBI::dbListFields(con, "blast_ref_candidates"))) {
    ensure_backup()
    message("migrated blast_ref_candidates to per-scaffold schema")
    DBI::dbExecute(con, "DROP TABLE blast_ref_candidates")
    .create_scaffold_candidates()
  }

  if (!("blast_ref_sequences" %in% existing_tables)) {
    ensure_backup()
    message("created blast_ref_sequences table")
    DBI::dbExecute(con,
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
  } else {
    add_col("blast_ref_sequences", "genetic_code", "INTEGER")
    add_col("blast_ref_sequences", "lineage", "TEXT")
    add_col("blast_ref_sequences", "topology", "TEXT")
  }

  if (!("blast_ref_alignment" %in% existing_tables)) {
    ensure_backup()
    message("created blast_ref_alignment table")
    DBI::dbExecute(con,
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
  } else {
    bra_fields <- DBI::dbListFields(con, "blast_ref_alignment")
    if (!("accession" %in% bra_fields)) {
      ensure_backup()
      # Migrate single-ref (ID-keyed) alignment to (ID, path, accession): the
      # existing alignment is against the sample's current top hit, path 1.
      message("re-keyed blast_ref_alignment table by (ID, path, accession)")
      DBI::dbExecute(con,
        "CREATE TABLE blast_ref_alignment_new (
          ID TEXT NOT NULL,
          path INTEGER NOT NULL DEFAULT 1,
          accession TEXT NOT NULL,
          aligned_sample TEXT NOT NULL,
          aligned_ref TEXT NOT NULL,
          rotation INTEGER NOT NULL DEFAULT 0,
          ref_length INTEGER NOT NULL,
          time_stamp INTEGER,
          PRIMARY KEY (ID, path, accession)
        );"
      )
      DBI::dbExecute(con,
        "INSERT OR IGNORE INTO blast_ref_alignment_new
           (ID, path, accession, aligned_sample, aligned_ref, rotation, ref_length, time_stamp)
         SELECT al.ID, 1, a.blast_accession, al.aligned_sample, al.aligned_ref, al.rotation, al.ref_length, al.time_stamp
         FROM blast_ref_alignment al
         JOIN assemble a ON al.ID = a.ID
         WHERE a.blast_accession IS NOT NULL"
      )
      DBI::dbExecute(con, "DROP TABLE blast_ref_alignment")
      DBI::dbExecute(con, "ALTER TABLE blast_ref_alignment_new RENAME TO blast_ref_alignment")
    } else if (!("path" %in% bra_fields)) {
      ensure_backup()
      # Add path to the key: (ID, accession) -> (ID, path, accession). Legacy
      # single-path alignments map to path 1.
      message("re-keyed blast_ref_alignment table by (ID, path, accession)")
      DBI::dbExecute(con,
        "CREATE TABLE blast_ref_alignment_new (
          ID TEXT NOT NULL,
          path INTEGER NOT NULL DEFAULT 1,
          accession TEXT NOT NULL,
          aligned_sample TEXT NOT NULL,
          aligned_ref TEXT NOT NULL,
          rotation INTEGER NOT NULL DEFAULT 0,
          ref_length INTEGER NOT NULL,
          time_stamp INTEGER,
          PRIMARY KEY (ID, path, accession)
        );"
      )
      DBI::dbExecute(con,
        "INSERT OR IGNORE INTO blast_ref_alignment_new
           (ID, path, accession, aligned_sample, aligned_ref, rotation, ref_length, time_stamp)
         SELECT ID, 1, accession, aligned_sample, aligned_ref, rotation, ref_length, time_stamp
         FROM blast_ref_alignment"
      )
      DBI::dbExecute(con, "DROP TABLE blast_ref_alignment")
      DBI::dbExecute(con, "ALTER TABLE blast_ref_alignment_new RENAME TO blast_ref_alignment")
    }
    # Add scaffold to the key + ref_start/strand columns:
    # (ID, path, accession) -> (ID, path, scaffold, accession). Legacy rows are
    # per-path whole-genome alignments computed the OLD way (global, no scaffold,
    # a representative sequence) and cannot be reliably attributed to one scaffold,
    # so drop them; blast_ref_align's backfill recomputes one correct per-scaffold
    # alignment on the next WF2 run (a cheap LOCAL pairwise, no remote BLAST).
    # Re-read fields: the branches above may have rebuilt the table in this pass.
    bra_fields <- DBI::dbListFields(con, "blast_ref_alignment")
    if (!("scaffold" %in% bra_fields)) {
      ensure_backup()
      message("re-keyed blast_ref_alignment table by (ID, path, scaffold, accession); cleared legacy alignments for per-scaffold recompute")
      DBI::dbExecute(con, "DROP TABLE blast_ref_alignment")
      DBI::dbExecute(con,
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
    }
  }

  add_col("assemble", "blast_opts", "TEXT", "default")

  # if assemblies table doesn't exist, create it; otherwise add length_raw if missing
  if (!("assemblies" %in% existing_tables)) {
    ensure_backup()
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
    ensure_backup()
    message("added 'length_raw' column to assemblies table")
    DBI::dbExecute(con, "ALTER TABLE assemblies ADD COLUMN length_raw INTEGER")
    DBI::dbExecute(con, "UPDATE assemblies SET length_raw = length WHERE length_raw IS NULL")
  }

  # if blast_opts table doesn't exist, create it with a default entry
  if (!("blast_opts" %in% DBI::dbListTables(con))) {
    ensure_backup()
    message("created blast_opts table")
    DBI::dbExecute(con,
      "CREATE TABLE blast_opts (
        blast_opts TEXT NOT NULL,
        run_blast INTEGER,
        entrez_query TEXT,
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
          extra_opts      = "",
          max_target_seqs = 5L
        ),
        in_place = TRUE,
        copy = TRUE,
        by = "blast_opts"
      )
  }

  # if the pre-trim snapshot table doesn't exist, create it
  if (!("assembly_backup" %in% DBI::dbListTables(con))) {
    ensure_backup()
    message("created assembly_backup table")
    DBI::dbExecute(con, ASSEMBLY_BACKUP_DDL)
  }

  add_col("assemblies", "edit_positions", "TEXT")

  # A pre-existing assemblies table (created before these columns) never gets them
  # from the create branch above, yet the app (unit_ref_facts, the annotate/export
  # tables) and WF2 (CURATE/ORF select a.blast_accession) require them. NULLs are
  # fine; the pipeline repopulates them on the next run.
  asmb_blast_cols <- c(blast_accession = "TEXT", blast_species = "TEXT",
                       blast_pident = "REAL", blast_qcovs = "REAL",
                       blast_evalue = "REAL", blast_lineage = "TEXT")
  for (col in names(asmb_blast_cols)) add_col("assemblies", col, asmb_blast_cols[[col]])

  # if assembly_blast table doesn't exist, create it (per-path BLAST hits)
  if (!("assembly_blast" %in% DBI::dbListTables(con))) {
    ensure_backup()
    message("created assembly_blast table")
    DBI::dbExecute(con,
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
  }

  # if export_opts table doesn't exist, create it and seed the default templates
  if (!("export_opts" %in% DBI::dbListTables(con))) {
    ensure_backup()
    message("created 'export_opts' table")
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
  } else if (!export_default_current) {
    ensure_backup()
    # Migrate unmodified default export templates to the current default. Only exact
    # matches to a superseded default are touched; a customised template is left as
    # the user wrote it, even though it may need {seqid} for fragmented samples.
    message("updated default export template to use {seqid} / {completeness}")
    for (old in old_export_defaults) {
      DBI::dbExecute(
        con,
        "UPDATE export_opts SET fasta_header = ? WHERE fasta_header = ?",
        params = list(DEFAULT_FASTA_HEADER, old)
      )
    }
    DBI::dbExecute(
      con,
      "UPDATE export_opts SET fasta_header_gene = ? WHERE fasta_header_gene = ?",
      params = list(DEFAULT_FASTA_HEADER_GENE, LEGACY_FASTA_HEADER_GENE_ID)
    )
  }

  # Re-key annotate from (ID) to (ID, path, scaffold): each retained scaffold is
  # its own annotation unit. Legacy rows had exactly one advancing unit (the old
  # lock guard enforced it), so map each to the first non-ignored scaffold of its
  # stored path (fallback path/scaffold = 1). Placed after assemblies is ensured.
  annotate_fields <- DBI::dbListFields(con, "annotate")
  if (!("scaffold" %in% annotate_fields)) {
    ensure_backup()
    message("re-keyed annotate table by (ID, path, scaffold)")
    path_expr <- if ("path" %in% annotate_fields) "COALESCE(CAST(an.path AS INTEGER), 1)" else "1"
    # Carry over whatever the old table actually has. Legacy annotate tables vary in
    # shape (columns arrived across several releases, and `scaffolds` never had an
    # ALTER migration at all), so select NULL for anything absent rather than
    # assuming a fixed column set and failing the whole migration.
    carry <- c("ID_verified", "scaffolds", "annotate_opts", "curate_opts",
               "orf_opts", "annotate_switch", "annotate_lock", "annotate_notes",
               "PCGCount", "tRNACount", "rRNACount", "missing", "extra",
               "warnings", "reviewed", "problematic", "partial", "structure",
               "length", "topology", "time_stamp")
    carry_expr <- paste(
      vapply(
        carry,
        function(cn) if (cn %in% annotate_fields) paste0("an.", cn) else "NULL",
        character(1)
      ),
      collapse = ", "
    )
    DBI::dbExecute(con,
      "CREATE TABLE annotate_new (
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
    DBI::dbExecute(con, sprintf(
      "INSERT OR IGNORE INTO annotate_new
         (ID, path, scaffold, %s)
       SELECT an.ID, %s,
         COALESCE((SELECT MIN(s.scaffold) FROM assemblies s
                   WHERE s.ID = an.ID AND s.path = %s AND s.ignore = 0), 1),
         %s
       FROM annotate an",
      paste(carry, collapse = ", "), path_expr, path_expr, carry_expr
    ))
    DBI::dbExecute(con, "DROP TABLE annotate")
    DBI::dbExecute(con, "ALTER TABLE annotate_new RENAME TO annotate")
  }

  # Seed an annotate row for every remaining non-ignored unit. The re-key above
  # maps each legacy row to a single unit, which is right for a legacy LOCKED
  # sample (the old lock guard refused to lock a sample with >1 non-ignored
  # assembly row), but leaves the other units of an unlocked multi-scaffold
  # sample with no annotate row at all. Those units are then silently dropped by
  # WF2, whose unit list inner-joins assemblies to annotate. Only WF1 seeds units,
  # and an upgrading user has no reason to re-assemble. INSERT OR IGNORE keeps the
  # re-keyed legacy row untouched and seeds the rest ready to annotate, mirroring
  # assemble_workflow.nf's seed.
  if (DBI::dbGetQuery(con,
        "SELECT COUNT(*) AS n FROM assemblies asm
         LEFT JOIN annotate an
           ON an.ID = asm.ID AND an.path = asm.path AND an.scaffold = asm.scaffold
         WHERE asm.ignore = 0 AND an.ID IS NULL")$n > 0) {
    ensure_backup()
  }
  n_seeded <- DBI::dbExecute(con,
    "INSERT OR IGNORE INTO annotate
       (ID, path, scaffold, topology, partial, annotate_opts, curate_opts, orf_opts,
        annotate_switch, annotate_lock, reviewed)
     SELECT asm.ID, asm.path, asm.scaffold, asm.topology,
            CASE WHEN asm.topology = 'circular' OR co.linear_complete = 1
                 THEN 'no' ELSE 'yes' END,
            an.annotate_opts, an.curate_opts, an.orf_opts,
            1, 0, 'no'
     FROM assemblies asm
     LEFT JOIN (SELECT ID, annotate_opts, curate_opts, orf_opts, MIN(path)
                FROM annotate GROUP BY ID) an ON an.ID = asm.ID
     LEFT JOIN curate_opts co ON co.curate_opts = an.curate_opts
     WHERE asm.ignore = 0"
  )
  if (n_seeded > 0) {
    message("seeded ", n_seeded, " additional annotation unit(s) from existing assemblies; ",
            "re-run Annotate to populate them")
  }

  # Per-unit reference override. "Set as best reference" is chosen from ONE
  # scaffold's candidate list, so it belongs to that unit; it used to overwrite the
  # sample-level assemble.blast_accession (stashing the original in
  # blast_accession_auto), which relabelled sibling scaffolds that never hit that
  # accession. Carry any existing override onto that sample's units - the old model
  # applied it sample-wide, so that is what it meant at the time.
  if (!DBI::dbExistsTable(con, "blast_ref_override")) {
    ensure_backup()
    message("created 'blast_ref_override' table (per-unit reference selection)")
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
    asm_fields <- DBI::dbListFields(con, "assemble")
    if (all(c("blast_accession", "blast_accession_auto") %in% asm_fields)) {
      n <- DBI::dbExecute(con, sprintf(
        "INSERT OR IGNORE INTO blast_ref_override (ID, path, scaffold, accession, time_stamp)
         SELECT a.ID, a.path, a.scaffold, b.blast_accession, %d
         FROM assemblies a
         JOIN assemble b ON b.ID = a.ID
         WHERE a.ignore = 0
           AND b.blast_accession IS NOT NULL AND b.blast_accession != ''
           AND b.blast_accession != 'NO HIT'
           AND b.blast_accession_auto IS NOT NULL
           AND b.blast_accession != b.blast_accession_auto",
        as.integer(Sys.time())
      ))
      if (n > 0) message("carried ", n, " reference override(s) onto assembly units")
    }
  }

  # Move export state off samples (PK ID) onto a per-unit table: with multi-assembly
  # each (ID, path, scaffold) is its own GenBank record and carries its own group.
  # Must run after the annotate re-key above, which is what gives annotate path and
  # scaffold to join on.
  if (!DBI::dbExistsTable(con, "export")) {
    ensure_backup()
    message("created 'export' table (per-unit export state)")
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
    legacy <- DBI::dbListFields(con, "samples")
    grp <- if ("export_group" %in% legacy) "s.export_group" else "NULL"
    ts  <- if ("export_time_stamp" %in% legacy) "s.export_time_stamp" else "NULL"
    if (any(c("export_group", "export_time_stamp") %in% legacy)) {
      # Legacy groups were sample-level, so every unit of a sample inherits the
      # sample's group. annotate alone is not a safe unit source: superseded
      # per-path rows survive a Path-0 join, so gate on assemblies.ignore = 0 the
      # way fetch_annotate_units() does.
      n <- DBI::dbExecute(con, sprintf(
        "INSERT OR IGNORE INTO export (ID, path, scaffold, export_group, export_time_stamp)
         SELECT an.ID, an.path, an.scaffold, %s, %s
         FROM annotate an
         JOIN assemblies a
           ON a.ID = an.ID AND a.path = an.path AND a.scaffold = an.scaffold
          AND a.ignore = 0
         JOIN samples s ON s.ID = an.ID
         WHERE %s IS NOT NULL OR %s IS NOT NULL",
        grp, ts, grp, ts
      ))
      if (n > 0) message("carried ", n, " legacy export assignment(s) onto assembly units")
    }
  }
  # Drop the legacy sample-level columns: two sources of truth for the same state is
  # exactly what moving to the export table removes. Needs SQLite >= 3.35.
  for (col in c("export_group", "export_time_stamp")) {
    if (col %in% DBI::dbListFields(con, "samples")) {
      ensure_backup()
      message("dropped '", col, "' from samples table (now per-unit in 'export')")
      DBI::dbExecute(con, paste0("ALTER TABLE samples DROP COLUMN ", col))
    }
  }

  # Repoint any Smithsonian/MitoPilot MITOS2 ref_dir on a stale/non-main branch to
  # main (idempotent). Guards against projects built off a feature branch whose ref
  # URL 404s once that branch is deleted. Only our own repo URLs are rewritten;
  # custom hosts and local paths are left untouched. Done last, as a direct UPDATE
  # on the final table state, so earlier in-memory opts upserts cannot clobber it.
  canonical_mitos_ref <- "https://raw.githubusercontent.com/Smithsonian/MitoPilot/refs/heads/main/ref_dbs/Mitos2"
  stale_ref_where <- paste0(
    "WHERE ref_dir LIKE 'https://raw.githubusercontent.com/%mithsonian/MitoPilot/%/ref_dbs/Mitos2' ",
    "AND ref_dir <> ?"
  )
  for (ref_tbl in c("annotate_opts", "curate_opts")) {
    fields <- tryCatch(DBI::dbListFields(con, ref_tbl), error = function(e) character(0))
    if ("ref_dir" %in% fields) {
      n <- DBI::dbGetQuery(
        con,
        paste0("SELECT COUNT(*) AS n FROM ", ref_tbl, " ", stale_ref_where),
        params = list(canonical_mitos_ref)
      )$n
      if (n > 0) {
        ensure_backup()
        DBI::dbExecute(
          con,
          paste0("UPDATE ", ref_tbl, " SET ref_dir = ? ", stale_ref_where),
          params = list(canonical_mitos_ref, canonical_mitos_ref)
        )
        message(paste0("repointed ", n, " stale ", ref_tbl, ".ref_dir value(s) to main"))
      }
    }
  }

  if (!changed) {
    message("nothing to update")
    return(invisible(NULL))
  }

  # Regenerate the .config from the chosen executor's template (port project
  # values, bump container, back up the old config). Gated on the container
  # being stale so an already-current config is left alone, and on the caller
  # opting in (default on).
  if (update_config && !containerVer) {
    migrate_config(path, executor)
  }

}

#' Add an optional column to an existing table, back-filling a constant default
#'
#' No-op when the table is absent or already carries the column. The column is
#' added without a SQL DEFAULT clause, so a migrated database keeps the schema a
#' fresh project gets from [init_db()]; `default` is written to the existing rows
#' with a plain UPDATE instead. `default = NULL` leaves them NULL.
#'
#' @param con project database connection
#' @param table table to alter
#' @param column new column name
#' @param type SQLite column type
#' @param default constant back-fill value, or NULL for none
#' @param before function called once before the first write (e.g. to back up)
#'
#' @noRd
add_opt_col <- function(con, table, column, type, default = NULL, before = NULL) {
  fields <- tryCatch(DBI::dbListFields(con, table), error = function(e) character(0))
  if (length(fields) == 0L || column %in% fields) {
    return(invisible(FALSE))
  }
  if (is.function(before)) before()
  message("added '", column, "' column to ", table, " table")
  DBI::dbExecute(con, paste0("ALTER TABLE ", table, " ADD COLUMN ", column, " ", type))
  if (!is.null(default)) {
    DBI::dbExecute(con, paste0("UPDATE ", table, " SET ", column, " = ?"),
                   params = list(default))
  }
  invisible(TRUE)
}

#' Landmarks of the current project schema that an older database will lack.
#'
#' Returns a character vector of plain-language gaps, empty when the database is
#' current. Lives beside the migration so the two stay in sync. Used by the app to
#' refuse to open a stale project with a readable message rather than failing deep
#' inside dbplyr with "no such column: path".
#'
#' @param con An open connection to a project database.
#' @noRd
schema_gaps <- function(con) {
  has <- function(expr) isTRUE(tryCatch(expr, error = function(e) FALSE))
  gaps <- character(0)
  if (!has("scaffold" %in% DBI::dbListFields(con, "annotate"))) {
    gaps <- c(gaps, "the annotate table is not keyed by (ID, path, scaffold)")
  }
  if (!has(DBI::dbExistsTable(con, "export"))) {
    gaps <- c(gaps, "the per-unit 'export' table is missing")
  }
  if (!has(DBI::dbExistsTable(con, "blast_ref_override"))) {
    gaps <- c(gaps, "the 'blast_ref_override' table is missing")
  }
  if (!has("scaffold" %in% DBI::dbListFields(con, "blast_ref_alignment"))) {
    gaps <- c(gaps, "the blast_ref_alignment table is not keyed per scaffold")
  }
  if (!has("blast_accession" %in% DBI::dbListFields(con, "assemblies"))) {
    gaps <- c(gaps, "the assemblies table lacks per-scaffold BLAST columns")
  }
  if (!has("orf_nested" %in% DBI::dbListFields(con, "orf_opts"))) {
    gaps <- c(gaps, "the orf_opts table lacks the 'orf_nested' column")
  }
  if (has(any(c("text", "real") %in%
              DBI::dbGetQuery(con, "SELECT DISTINCT typeof(genetic_code) AS t FROM samples")$t))) {
    gaps <- c(gaps, "the samples.genetic_code column is not stored as an integer")
  }
  gaps
}
