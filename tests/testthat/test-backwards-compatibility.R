# Helper: build a minimal .config for a given version string.
# Includes a nested `withName:getOrganelle { }` block so that the
# blast_gb insertion code (which greps for "^    }$") has a target line.
make_config <- function(path, version, has_asmb_dir = FALSE,
                        has_fail_on_ignore = FALSE, has_blast_gb = FALSE) {
  lines <- c(
    "params {",
    "    rawDir = 'raw'"
  )
  if (has_asmb_dir) lines <- c(lines, "    asmbDir = 'NA'")
  lines <- c(lines, "}")

  if (has_fail_on_ignore) {
    lines <- c(lines, "", "workflow {", "  failOnIgnore = true", "}")
  }

  process_block <- c(
    "",
    "process {",
    "    executor = 'local'",
    paste0("    container = 'macguigand/mitopilot:", version, "'"),
    "    withName:getOrganelle {",
    "        cpus = 4",
    "    }"
  )
  if (has_blast_gb) {
    process_block <- c(process_block,
                       "    blast_gb {",
                       "        cpus = 1",
                       "        container = process.container",
                       "        executor = process.executor",
                       "    }")
  }
  process_block <- c(process_block, "}")
  lines <- c(lines, process_block)
  writeLines(lines, file.path(path, ".config"))
}

# Helper: assert every item in `cols` is present in the given table's fields
expect_cols <- function(con, table, cols) {
  fields <- DBI::dbListFields(con, table)
  for (col in cols) {
    expect_true(col %in% fields,
                label = paste0(table, "$", col))
  }
}

# Helper: create a v1.0.0-era database (none of the new columns/tables)
create_v100_db <- function(path) {
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(path, ".sqlite"))
  on.exit(DBI::dbDisconnect(con))

  DBI::dbExecute(con, "CREATE TABLE samples (
    ID TEXT NOT NULL PRIMARY KEY,
    sample TEXT
  )")
  DBI::dbExecute(con, "INSERT INTO samples VALUES ('s1', 'Sample1')")

  DBI::dbExecute(con, "CREATE TABLE annotate (
    ID TEXT NOT NULL PRIMARY KEY,
    gene TEXT
  )")
  DBI::dbExecute(con, "INSERT INTO annotate VALUES ('s1', 'cox1')")

  DBI::dbExecute(con, "CREATE TABLE assemble (
    ID TEXT NOT NULL PRIMARY KEY,
    assembly TEXT
  )")
  DBI::dbExecute(con, "INSERT INTO assemble VALUES ('s1', 'ATCG')")

  DBI::dbExecute(con, "CREATE TABLE assemble_opts (
    assemble_opts TEXT NOT NULL PRIMARY KEY,
    params TEXT
  )")
  DBI::dbExecute(con, "INSERT INTO assemble_opts VALUES ('default', '{}')")

  DBI::dbExecute(con, "CREATE TABLE annotate_opts (
    annotate_opts TEXT NOT NULL PRIMARY KEY,
    params TEXT
  )")
  DBI::dbExecute(con, "INSERT INTO annotate_opts VALUES ('default', '{}')")

  DBI::dbExecute(con, "CREATE TABLE curate_opts (
    curate_opts TEXT NOT NULL PRIMARY KEY,
    params TEXT
  )")
  DBI::dbExecute(con, "INSERT INTO curate_opts VALUES ('default', '{\"max_blast_hits\":100}')")

  DBI::dbExecute(con, "CREATE TABLE annotations (
    ID TEXT NOT NULL,
    gene TEXT NOT NULL,
    PRIMARY KEY (ID, gene)
  )")
  DBI::dbExecute(con, "INSERT INTO annotations VALUES ('s1', 'cox1')")
}

# Helper: create a v1.3.10-era database (has use_arwen/arwen_opts/start_gene and
# assembler/mitofinder* and blast_accession columns, but missing use_mitos_best,
# use_aragorn, aragorn_opts, tool, and blast_opts/blast_ref_* tables)
create_v1310_db <- function(path) {
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(path, ".sqlite"))
  on.exit(DBI::dbDisconnect(con))

  DBI::dbExecute(con, "CREATE TABLE samples (
    ID TEXT NOT NULL PRIMARY KEY,
    sample TEXT,
    genetic_code TEXT
  )")
  DBI::dbExecute(con, "INSERT INTO samples VALUES ('s1', 'Sample1', '2')")

  DBI::dbExecute(con, "CREATE TABLE annotate (
    ID TEXT NOT NULL PRIMARY KEY,
    gene TEXT,
    reviewed TEXT,
    ID_verified TEXT,
    problematic TEXT
  )")
  DBI::dbExecute(con, "INSERT INTO annotate VALUES ('s1', 'cox1', 'no', 'no', NULL)")

  DBI::dbExecute(con, "CREATE TABLE assemble (
    ID TEXT NOT NULL PRIMARY KEY,
    assembly TEXT
  )")
  DBI::dbExecute(con, "INSERT INTO assemble VALUES ('s1', 'ATCG')")

  DBI::dbExecute(con, "CREATE TABLE assemble_opts (
    assemble_opts TEXT NOT NULL PRIMARY KEY,
    params TEXT,
    assembler TEXT,
    mitofinder_db TEXT,
    mitofinder TEXT
  )")
  DBI::dbExecute(con, "INSERT INTO assemble_opts VALUES (
    'default', '{}', 'GetOrganelle',
    'https://raw.githubusercontent.com/Smithsonian/MitoPilot/refs/heads/main/ref_dbs/MitoFinder/fish_mito_sampler.gb',
    '--megahit'
  )")

  DBI::dbExecute(con, "CREATE TABLE annotate_opts (
    annotate_opts TEXT NOT NULL PRIMARY KEY,
    params TEXT,
    use_arwen INTEGER,
    arwen_opts TEXT,
    start_gene TEXT,
    ref_db TEXT,
    ref_dir TEXT
  )")
  DBI::dbExecute(con, "INSERT INTO annotate_opts VALUES (
    'default', '{}', 0, '-mtx', 'trnF', 'Chordata',
    'https://raw.githubusercontent.com/Smithsonian/MitoPilot/refs/heads/main/ref_dbs/Mitos2'
  )")

  DBI::dbExecute(con, "CREATE TABLE curate_opts (
    curate_opts TEXT NOT NULL PRIMARY KEY,
    params TEXT,
    max_blast_hits INTEGER,
    ref_db TEXT,
    ref_dir TEXT
  )")
  DBI::dbExecute(con, "INSERT INTO curate_opts VALUES (
    'default', '{\"max_blast_hits\":100}', 100, 'Chordata',
    'https://raw.githubusercontent.com/Smithsonian/MitoPilot/refs/heads/main/ref_dbs/Mitos2'
  )")

  DBI::dbExecute(con, "CREATE TABLE annotations (
    ID TEXT NOT NULL,
    gene TEXT NOT NULL,
    PRIMARY KEY (ID, gene)
  )")
  DBI::dbExecute(con, "INSERT INTO annotations VALUES ('s1', 'cox1')")
}

# Helper: a legacy project whose sample was never locked and kept several
# non-ignored scaffolds. The old lock guard refused to lock a sample with more
# than one retained assembly row, so these unlocked fragmented projects are
# exactly the ones users were waiting on multi-assembly for -- and the ones whose
# extra units nothing else will ever seed (only WF1 seeds units, and an upgrading
# user has no reason to re-assemble).
create_multi_scaffold_db <- function(path) {
  create_v1310_db(path)
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(path, ".sqlite"))
  on.exit(DBI::dbDisconnect(con))

  DBI::dbExecute(con, "CREATE TABLE assemblies (
    ID TEXT NOT NULL,
    path INTEGER NOT NULL,
    scaffold INTEGER NOT NULL,
    topology TEXT,
    length INTEGER,
    sequence TEXT,
    ignore INTEGER,
    time_stamp INTEGER,
    PRIMARY KEY (ID, path, scaffold)
  )")
  # three retained scaffolds, plus one already dropped as too short
  DBI::dbExecute(con, "INSERT INTO assemblies VALUES
    ('s1', 1, 1, 'linear', 5000, 'ACGT', 0, 1),
    ('s1', 1, 2, 'linear', 4000, 'ACGT', 0, 1),
    ('s1', 1, 3, 'linear', 3000, 'ACGT', 0, 1),
    ('s1', 1, 4, 'linear',   80, 'ACGT', 1, 1)")
}

test_that("backwards_compatibility seeds an annotate unit for every retained scaffold", {
  td <- tempfile()
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE))

  create_multi_scaffold_db(td)
  make_config(td, version = "1.3.10")

  MitoPilot::backwards_compatibility(path = td, update_config = FALSE)

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(td, ".sqlite"))
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # one unit per retained scaffold; the ignored scaffold gets none
  units <- DBI::dbGetQuery(
    con, "SELECT path, scaffold FROM annotate ORDER BY path, scaffold"
  )
  expect_equal(units$path, c(1, 1, 1))
  expect_equal(units$scaffold, c(1, 2, 3))

  # and every retained scaffold is reachable by the join WF2 uses to pick units,
  # which is an inner join and would otherwise drop them silently
  reachable <- DBI::dbGetQuery(con, "
    SELECT COUNT(*) AS n
    FROM assemblies a
    JOIN annotate an ON an.ID = a.ID AND an.path = a.path AND an.scaffold = a.scaffold
    WHERE a.ignore = 0")$n
  expect_equal(reachable, 3)

  # seeded units are ready to annotate, not silently marked done
  seeded <- DBI::dbGetQuery(
    con, "SELECT annotate_switch, annotate_lock, reviewed FROM annotate WHERE scaffold > 1"
  )
  expect_true(all(seeded$annotate_switch == 1))
  expect_true(all(seeded$annotate_lock == 0))
  expect_true(all(seeded$reviewed == "no"))
})

test_that("backwards_compatibility re-keys annotate by (ID, path, scaffold)", {
  td <- tempfile()
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE))

  create_multi_scaffold_db(td)
  make_config(td, version = "1.3.10")
  MitoPilot::backwards_compatibility(path = td, update_config = FALSE)

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(td, ".sqlite"))
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  pk <- DBI::dbGetQuery(con, "PRAGMA table_info(annotate)")
  expect_equal(pk$name[pk$pk > 0][order(pk$pk[pk$pk > 0])], c("ID", "path", "scaffold"))
})

test_that("backwards_compatibility backs up the database before migrating", {
  td <- tempfile()
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE))

  create_multi_scaffold_db(td)
  make_config(td, version = "1.3.10")

  expect_message(
    MitoPilot::backwards_compatibility(path = td, update_config = FALSE),
    regexp = "Backed up project database"
  )
  backups <- list.files(file.path(td, ".old_sqlite_dbs"), all.files = TRUE, no.. = TRUE)
  expect_equal(backups, ".sqlite.1")

  # the backup is the pre-migration database, i.e. still on the old schema
  bak <- DBI::dbConnect(RSQLite::SQLite(), file.path(td, ".old_sqlite_dbs", ".sqlite.1"))
  on.exit(DBI::dbDisconnect(bak), add = TRUE)
  expect_false("scaffold" %in% DBI::dbListFields(bak, "annotate"))

  # a second, no-op migration must not litter another backup
  MitoPilot::backwards_compatibility(path = td, update_config = FALSE)
  expect_equal(
    list.files(file.path(td, ".old_sqlite_dbs"), all.files = TRUE, no.. = TRUE),
    ".sqlite.1"
  )
})

test_that("backwards_compatibility is idempotent on a multi-scaffold project", {
  td <- tempfile()
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE))

  create_multi_scaffold_db(td)
  make_config(td, version = "1.3.10")
  MitoPilot::backwards_compatibility(path = td, update_config = FALSE)

  expect_message(
    MitoPilot::backwards_compatibility(path = td, update_config = FALSE),
    regexp = "nothing to update"
  )
})

test_that("schema_gaps flags a legacy database and passes a migrated one", {
  td <- tempfile()
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE))

  create_multi_scaffold_db(td)
  make_config(td, version = "1.3.10")

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(td, ".sqlite"))
  expect_true(length(schema_gaps(con)) > 0)
  DBI::dbDisconnect(con)

  MitoPilot::backwards_compatibility(path = td, update_config = FALSE)

  con2 <- DBI::dbConnect(RSQLite::SQLite(), file.path(td, ".sqlite"))
  on.exit(DBI::dbDisconnect(con2), add = TRUE)
  expect_equal(schema_gaps(con2), character(0))
})

test_that("schema_gaps flags assemble_opts without the MapToRef columns", {
  td <- tempfile()
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE))

  create_multi_scaffold_db(td)
  make_config(td, version = "1.5.4")

  con0 <- DBI::dbConnect(RSQLite::SQLite(), file.path(td, ".sqlite"))
  expect_false("maptoref_ref" %in% DBI::dbListFields(con0, "assemble_opts"))
  expect_true("the assemble_opts table lacks the MapToRef option columns" %in%
                schema_gaps(con0))
  DBI::dbDisconnect(con0)

  MitoPilot::backwards_compatibility(path = td, update_config = FALSE)

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(td, ".sqlite"))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_false("the assemble_opts table lacks the MapToRef option columns" %in%
                 schema_gaps(con))
})

test_that("schema_gaps passes a freshly created database", {
  td <- withr::local_tempdir()
  mapping <- file.path(td, "mapping.csv")
  utils::write.csv(
    data.frame(ID = "S1", Taxon = "Danio rerio",
               R1 = "S1_R1.fastq.gz", R2 = "S1_R2.fastq.gz"),
    mapping, row.names = FALSE
  )
  db <- file.path(td, ".sqlite")
  new_db(db_path = db, mapping_fn = mapping)

  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_false("the assemble_opts table lacks the MapToRef option columns" %in%
                 schema_gaps(con))
})

test_that("backwards_compatibility adds per-scaffold BLAST columns to a pre-existing assemblies table", {
  td <- tempfile()
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE))

  create_multi_scaffold_db(td)   # assemblies exists WITHOUT the BLAST columns
  make_config(td, version = "1.3.10")

  con0 <- DBI::dbConnect(RSQLite::SQLite(), file.path(td, ".sqlite"))
  expect_false("blast_accession" %in% DBI::dbListFields(con0, "assemblies"))
  # schema_gaps must catch it (the app/WF2 both SELECT these columns)
  expect_true("the assemblies table lacks per-scaffold BLAST columns" %in% schema_gaps(con0))
  DBI::dbDisconnect(con0)

  MitoPilot::backwards_compatibility(path = td, update_config = FALSE)

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(td, ".sqlite"))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_true(all(
    c("blast_accession", "blast_species", "blast_pident",
      "blast_qcovs", "blast_evalue", "blast_lineage") %in%
      DBI::dbListFields(con, "assemblies")
  ))
})

test_that("backwards_compatibility provisions orf_opts.orf_nested (created and altered paths)", {
  td <- tempfile()
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE))

  create_multi_scaffold_db(td)   # no orf_opts table -> migration CREATEs it
  make_config(td, version = "1.3.10")

  con0 <- DBI::dbConnect(RSQLite::SQLite(), file.path(td, ".sqlite"))
  expect_true("the orf_opts table lacks the 'orf_nested' column" %in% schema_gaps(con0))
  DBI::dbDisconnect(con0)

  MitoPilot::backwards_compatibility(path = td, update_config = FALSE)

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(td, ".sqlite"))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_true("orf_nested" %in% DBI::dbListFields(con, "orf_opts"))
  # seeded default row carries a concrete value (WF2 ORF reads it)
  expect_equal(
    DBI::dbGetQuery(con, "SELECT orf_nested FROM orf_opts WHERE orf_opts = 'default'")$orf_nested,
    0L
  )
})

test_that("backwards_compatibility repoints a stale non-main MITOS2 ref_dir to main (and leaves custom refs)", {
  stale <- "https://raw.githubusercontent.com/Smithsonian/MitoPilot/refs/heads/scyphozoa-ruleset/ref_dbs/Mitos2"
  main  <- "https://raw.githubusercontent.com/Smithsonian/MitoPilot/refs/heads/main/ref_dbs/Mitos2"

  td <- tempfile()
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE))

  create_multi_scaffold_db(td)
  make_config(td, version = "1.3.10")
  con0 <- DBI::dbConnect(RSQLite::SQLite(), file.path(td, ".sqlite"))
  # stale branch on our own repo -> should be rewritten
  DBI::dbExecute(con0, "UPDATE annotate_opts SET ref_dir = ?", params = list(stale))
  # a custom (non-Smithsonian) ref_dir -> must be left untouched
  DBI::dbExecute(con0, "UPDATE curate_opts SET ref_dir = ?",
                 params = list("/data/local_refs/Mitos2"))
  DBI::dbDisconnect(con0)

  MitoPilot::backwards_compatibility(path = td, update_config = FALSE)

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(td, ".sqlite"))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_equal(DBI::dbGetQuery(con, "SELECT ref_dir FROM annotate_opts")$ref_dir, main)
  expect_equal(DBI::dbGetQuery(con, "SELECT ref_dir FROM curate_opts")$ref_dir,
               "/data/local_refs/Mitos2")

  # idempotent: a re-run reports nothing to update
  expect_message(
    MitoPilot::backwards_compatibility(path = td, update_config = FALSE),
    regexp = "nothing to update"
  )
})


test_that("backwards_compatibility migrates a v1.0.0 database to current schema", {
  td <- tempfile()
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE))

  create_v100_db(td)
  make_config(td, version = "1.0.0")

  expect_message(
    MitoPilot::backwards_compatibility(path = td, executor = "local"),
    regexp = "added|updated|created",
    all = FALSE
  )

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(td, ".sqlite"))
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # samples
  expect_cols(con, "samples", "genetic_code")

  # annotate
  expect_cols(con, "annotate", c("reviewed", "ID_verified", "problematic", "partial"))

  # assemble
  expect_cols(con, "assemble",
              c("blast_accession", "blast_species", "blast_pident",
                "blast_qcovs", "blast_evalue", "blast_lineage", "blast_opts"))

  # assemble_opts
  expect_cols(con, "assemble_opts",
              c("assembler", "mitofinder_db", "mitofinder",
                "max_paths", "max_scaffolds",
                "maptoref_ref", "maptoref", "maptoref_consensus",
                "maptoref_iter", "maptoref_topology"))

  # annotate_opts
  expect_cols(con, "annotate_opts",
              c("use_arwen", "arwen_opts", "use_mitos_best",
                "use_aragorn", "aragorn_opts", "start_gene"))

  # curate_opts
  expect_cols(con, "curate_opts", c("max_blast_hits", "ref_db", "ref_dir", "linear_complete"))

  # annotations
  expect_cols(con, "annotations", c("tool", "partial_start", "partial_stop"))

  # new tables
  tables <- DBI::dbListTables(con)
  expect_true("blast_opts"            %in% tables)
  expect_true("blast_ref_annotations" %in% tables)
  expect_true("blast_ref_sequences"   %in% tables)
  expect_true("blast_ref_alignment"   %in% tables)
  expect_true("assemblies"            %in% tables)
  expect_cols(con, "assemblies", "length_raw")

  # .config updates
  conf <- readLines(file.path(td, ".config"))
  expect_true(any(grepl("asmbDir",       conf)))
  expect_true(any(grepl("failOnIgnore",  conf)))
  expect_true(any(grepl("blast_gb",      conf)))
  current_ver <- as.character(utils::packageVersion("MitoPilot"))
  expect_true(any(grepl(current_ver, conf, fixed = TRUE)))
})


test_that("backwards_compatibility migrates a v1.3.10 database to current schema", {
  td <- tempfile()
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE))

  create_v1310_db(td)
  make_config(td, version = "1.3.10",
              has_asmb_dir = TRUE, has_fail_on_ignore = TRUE)

  expect_message(
    MitoPilot::backwards_compatibility(path = td, executor = "local"),
    regexp = "added|updated|created",
    all = FALSE
  )

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(td, ".sqlite"))
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # assemble - still missing blast cols in 1.3.10
  expect_cols(con, "assemble",
              c("blast_accession", "blast_species", "blast_pident",
                "blast_qcovs", "blast_evalue", "blast_lineage", "blast_opts"))

  # annotate_opts - missing mitos_best / aragorn trio in 1.3.10
  expect_cols(con, "annotate_opts",
              c("use_mitos_best", "use_aragorn", "aragorn_opts"))

  # assemble_opts - max_paths / max_scaffolds added in this release
  expect_cols(con, "assemble_opts",
              c("max_paths", "max_scaffolds",
                "maptoref_ref", "maptoref", "maptoref_consensus",
                "maptoref_iter", "maptoref_topology"))

  # annotations
  expect_cols(con, "annotations", c("tool", "partial_start", "partial_stop"))

  # new tables
  tables <- DBI::dbListTables(con)
  expect_true("blast_opts"            %in% tables)
  expect_true("blast_ref_annotations" %in% tables)
  expect_true("blast_ref_sequences"   %in% tables)
  expect_true("blast_ref_alignment"   %in% tables)
  expect_true("assemblies"            %in% tables)
  expect_cols(con, "assemblies", "length_raw")

  # .config - only blast_gb and container missing in 1.3.10
  conf <- readLines(file.path(td, ".config"))
  expect_true(any(grepl("blast_gb", conf)))
  current_ver <- as.character(utils::packageVersion("MitoPilot"))
  expect_true(any(grepl(current_ver, conf, fixed = TRUE)))
})


test_that("backwards_compatibility adds join_notes/join_switch to assemble and is idempotent", {
  td <- tempfile()
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE))

  create_v1310_db(td)
  make_config(td, version = "1.3.10")

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(td, ".sqlite"))
  expect_false(all(c("join_notes", "join_switch") %in% DBI::dbListFields(con, "assemble")))
  DBI::dbDisconnect(con)

  suppressMessages(MitoPilot::backwards_compatibility(path = td, update_config = FALSE))

  con2 <- DBI::dbConnect(RSQLite::SQLite(), file.path(td, ".sqlite"))
  on.exit(DBI::dbDisconnect(con2), add = TRUE)
  expect_cols(con2, "assemble", c("join_notes", "join_switch"))
  expect_equal(schema_gaps(con2), character(0))

  expect_message(
    MitoPilot::backwards_compatibility(path = td, update_config = FALSE),
    regexp = "nothing to update"
  )
})

test_that("backwards_compatibility migrates an unmodified default export template to {completeness}", {
  td <- tempfile()
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE))

  # Bring a v1.0.0 DB up to current (this seeds export_opts with the new default)
  create_v100_db(td)
  make_config(td, version = "1.0.0")
  suppressMessages(MitoPilot::backwards_compatibility(path = td, executor = "local"))

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(td, ".sqlite"))
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # Fresh seed should already use {completeness}
  expect_match(
    DBI::dbReadTable(con, "export_opts")$fasta_header,
    "{completeness}", fixed = TRUE, all = FALSE
  )

  # Simulate a pre-change project: stomp the default back to the old hardcoded
  # string, plus add a user-customized template that must be left untouched.
  old_default <- "{ID} [organism={Taxon}] [topology={topology}] [mgcode={genetic_code}] [location=mitochondrion] {Taxon} mitochondrion, complete genome"
  custom <- "{ID} {Taxon} my custom complete genome layout"
  DBI::dbExecute(con, "UPDATE export_opts SET fasta_header = ? WHERE export_opts = 'default'",
                 params = list(old_default))
  DBI::dbExecute(con, "INSERT INTO export_opts (export_opts, fasta_header, fasta_header_gene) VALUES ('mine', ?, 'x')",
                 params = list(custom))

  expect_message(
    suppressWarnings(MitoPilot::backwards_compatibility(path = td, executor = "local")),
    regexp = "export template"
  )

  eo <- DBI::dbReadTable(con, "export_opts")
  # default migrated to {completeness}
  expect_match(eo$fasta_header[eo$export_opts == "default"], "{completeness}", fixed = TRUE)
  # customized template untouched
  expect_equal(eo$fasta_header[eo$export_opts == "mine"], custom)
})


test_that("backwards_compatibility is idempotent (early-exit on already-current DB)", {
  td <- tempfile()
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE))

  # First pass: migrate from v1.0.0
  create_v100_db(td)
  make_config(td, version = "1.0.0")
  suppressMessages(MitoPilot::backwards_compatibility(path = td, executor = "local"))

  # Second pass: should early-exit with "nothing to update"
  expect_message(
    MitoPilot::backwards_compatibility(path = td, executor = "local"),
    regexp = "nothing to update"
  )
})


test_that("backwards_compatibility stores genetic_code as a number (not TEXT)", {
  td <- tempfile()
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE))

  # v1.0.0 has no genetic_code column; migration must add it as a number so
  # assemble.nf's genetic_code.intValue() works.
  create_v100_db(td)
  make_config(td, version = "1.0.0")
  suppressMessages(MitoPilot::backwards_compatibility(path = td, executor = "local"))

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(td, ".sqlite"))
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  types <- DBI::dbGetQuery(con, "SELECT DISTINCT typeof(genetic_code) AS t FROM samples")$t
  expect_false("text" %in% types)
})


test_that("backwards_compatibility normalizes legacy TEXT genetic_code on re-run", {
  td <- tempfile()
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE))

  # Bring a v1.3.10 DB (genetic_code stored as TEXT) up to current.
  create_v1310_db(td)
  make_config(td, version = "1.3.10",
              has_asmb_dir = TRUE, has_fail_on_ignore = TRUE)

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(td, ".sqlite"))
  # confirm the fixture really stores it as TEXT
  expect_true("text" %in%
    DBI::dbGetQuery(con, "SELECT DISTINCT typeof(genetic_code) AS t FROM samples")$t)
  DBI::dbDisconnect(con)

  suppressMessages(MitoPilot::backwards_compatibility(path = td, executor = "local"))

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(td, ".sqlite"))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  types <- DBI::dbGetQuery(con, "SELECT DISTINCT typeof(genetic_code) AS t FROM samples")$t
  expect_false("text" %in% types)
  # value preserved
  expect_equal(DBI::dbGetQuery(con, "SELECT genetic_code FROM samples")$genetic_code, 2L)
})


test_that("backwards_compatibility normalizes REAL genetic_code to INTEGER (MITOS2 rejects '2.0')", {
  td <- tempfile()
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE))

  create_v1310_db(td)
  make_config(td, version = "1.3.10")

  # Rebuild samples so genetic_code is stored as REAL 2.0, as some old projects did
  # (a numeric inserted into an affinity-less column). WF2 then passes '2.0' to
  # runmitos.py -c, which argparse rejects.
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(td, ".sqlite"))
  DBI::dbExecute(con, "ALTER TABLE samples RENAME TO samples_old")
  DBI::dbExecute(con, "CREATE TABLE samples (ID TEXT NOT NULL PRIMARY KEY, sample TEXT, genetic_code)")
  DBI::dbExecute(con, "INSERT INTO samples (ID, sample, genetic_code) SELECT ID, sample, 2.0 FROM samples_old")
  DBI::dbExecute(con, "DROP TABLE samples_old")
  expect_equal(
    DBI::dbGetQuery(con, "SELECT DISTINCT typeof(genetic_code) AS t FROM samples")$t, "real")
  # schema_gaps must catch it, so the app forces migration before WF2 runs
  expect_true("the samples.genetic_code column is not stored as an integer" %in% schema_gaps(con))
  DBI::dbDisconnect(con)

  suppressMessages(MitoPilot::backwards_compatibility(path = td, update_config = FALSE))

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(td, ".sqlite"))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_equal(
    DBI::dbGetQuery(con, "SELECT DISTINCT typeof(genetic_code) AS t FROM samples")$t, "integer")
  expect_equal(DBI::dbGetQuery(con, "SELECT genetic_code FROM samples")$genetic_code, 2L)
  expect_false("the samples.genetic_code column is not stored as an integer" %in% schema_gaps(con))
})


test_that("backwards_compatibility regenerates .config and backs up the old one", {
  td <- tempfile()
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE))

  create_v100_db(td)
  make_config(td, version = "1.0.0")  # process executor = 'local', rawDir = 'raw'

  suppressMessages(MitoPilot::backwards_compatibility(path = td, executor = "local"))

  conf <- readLines(file.path(td, ".config"))
  # regenerated from the built-in local template: current sections present
  expect_true(any(grepl("orffinder_condaenv", conf)))
  expect_true(any(grepl("^\\s*orf \\{", conf)))
  expect_true(any(grepl("blast_gb", conf)))
  expect_true(any(grepl("failOnIgnore", conf)))
  # project value carried over from the old config
  expect_true(any(grepl("rawDir = 'raw'", conf, fixed = TRUE)))
  # container bumped to current version
  current_ver <- as.character(utils::packageVersion("MitoPilot"))
  expect_true(any(grepl(current_ver, conf, fixed = TRUE)))

  # timestamped backup of the old config was written
  backups <- list.files(td, pattern = "^\\.config\\.bak\\.", all.files = TRUE)
  expect_true(length(backups) >= 1)
})


# Helper: a DB carrying the old in-container Mitos2 ref path (triggers the
# old_ref_str branch). annotate_opts has ref_db="Metazoa"/ref_dir=old path;
# curate_opts params embed the old path and the table lacks ref_dir/ref_db.
create_old_ref_db <- function(path) {
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(path, ".sqlite"))
  on.exit(DBI::dbDisconnect(con))

  DBI::dbExecute(con, "CREATE TABLE samples (ID TEXT NOT NULL PRIMARY KEY, sample TEXT, genetic_code INTEGER)")
  DBI::dbExecute(con, "INSERT INTO samples VALUES ('s1', 'Sample1', 2)")
  DBI::dbExecute(con, "CREATE TABLE annotate (ID TEXT NOT NULL PRIMARY KEY, gene TEXT)")
  DBI::dbExecute(con, "INSERT INTO annotate VALUES ('s1', 'cox1')")
  DBI::dbExecute(con, "CREATE TABLE assemble (ID TEXT NOT NULL PRIMARY KEY, assembly TEXT)")
  DBI::dbExecute(con, "INSERT INTO assemble VALUES ('s1', 'ATCG')")
  DBI::dbExecute(con, "CREATE TABLE assemble_opts (assemble_opts TEXT NOT NULL PRIMARY KEY, params TEXT)")
  DBI::dbExecute(con, "INSERT INTO assemble_opts VALUES ('default', '{}')")
  DBI::dbExecute(con, "CREATE TABLE annotate_opts (annotate_opts TEXT NOT NULL PRIMARY KEY, params TEXT, ref_db TEXT, ref_dir TEXT)")
  DBI::dbExecute(con, "INSERT INTO annotate_opts VALUES ('default', '{}', 'Metazoa', '/ref_dbs/Mitos2')")
  DBI::dbExecute(con, "CREATE TABLE curate_opts (curate_opts TEXT NOT NULL PRIMARY KEY, params TEXT)")
  DBI::dbExecute(con, "INSERT INTO curate_opts VALUES ('default', '{\"ref_dbs\":{\"default\":[\"/ref_dbs/Mitos2/Metazoa/featureProt/{gene}.fas\"]},\"max_blast_hits\":100}')")
  DBI::dbExecute(con, "CREATE TABLE annotations (ID TEXT NOT NULL, gene TEXT NOT NULL, PRIMARY KEY (ID, gene))")
  DBI::dbExecute(con, "INSERT INTO annotations VALUES ('s1', 'cox1')")
}


test_that("backwards_compatibility migrates the old Mitos2 ref path (and is idempotent)", {
  td <- tempfile()
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE))

  create_old_ref_db(td)
  make_config(td, version = "1.0.0")

  suppressMessages(MitoPilot::backwards_compatibility(path = td, executor = "local"))

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(td, ".sqlite"))
  ao <- DBI::dbReadTable(con, "annotate_opts")
  co <- DBI::dbReadTable(con, "curate_opts")
  # annotate_opts ref_db renamed (the previously-dead %in% check) and ref_dir bumped
  expect_equal(ao$ref_db, "Metazoa_RefSeq89")
  expect_match(ao$ref_dir, "githubusercontent")
  # curate_opts gained ref_dir/ref_db and the old path was stripped from params
  expect_true(all(c("ref_dir", "ref_db") %in% names(co)))
  expect_equal(co$ref_db, "Metazoa_RefSeq89")
  expect_false(grepl("/ref_dbs/Mitos2/Metazoa", co$params))
  DBI::dbDisconnect(con)

  # Re-run must not re-fire the block (no duplicate-column ALTER crash)
  expect_message(
    MitoPilot::backwards_compatibility(path = td, executor = "local"),
    regexp = "nothing to update"
  )
})


test_that("backwards_compatibility(update_config = FALSE) migrates DB but not .config", {
  td <- tempfile()
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE))

  create_v100_db(td)
  make_config(td, version = "1.0.0")
  before <- readLines(file.path(td, ".config"))

  suppressMessages(MitoPilot::backwards_compatibility(path = td, update_config = FALSE))

  # .config untouched, no backup written
  expect_identical(readLines(file.path(td, ".config")), before)
  expect_equal(length(list.files(td, pattern = "^\\.config\\.bak\\.", all.files = TRUE)), 0L)

  # DB still migrated
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(td, ".sqlite"))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_cols(con, "annotate", c("reviewed", "ID_verified", "problematic"))
  expect_false("text" %in%
    DBI::dbGetQuery(con, "SELECT DISTINCT typeof(genetic_code) AS t FROM samples")$t)
})


test_that("backwards_compatibility errors when executor missing and update_config = TRUE", {
  td <- tempfile()
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE))

  create_v100_db(td)
  make_config(td, version = "1.0.0")

  expect_error(
    MitoPilot::backwards_compatibility(path = td),
    regexp = "executor.*required"
  )
})


test_that("migrate_config fills PENV and does not inject a clusterOptions closure (SGE)", {
  td <- tempfile()
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE))

  # SGE config with penv and a multi-line clusterOptions Groovy closure
  writeLines(c(
    "process {",
    "  executor = 'sge'",
    "  container = 'macguigand/mitopilot:1.4.8'",
    "  penv = 'mthread'",
    "  clusterOptions = {",
    "    if (x > 8) { '-l himem' } else { '-l normal' }",
    "  }",
    "}",
    "params {",
    "    rawDir = '/scratch/foo'",
    "    asmbDir = 'NA'",
    "    minDepth = 2000000",
    "    genetic_code = 2",
    "    ncbi_api_key = 'KEY'",
    "}"
  ), file.path(td, ".config"))

  expect_warning(
    res <- suppressMessages(MitoPilot:::migrate_config(td, executor = "sge")),
    regexp = "clusterOptions"
  )
  expect_true(res)

  conf <- readLines(file.path(td, ".config"))
  # PENV was filled (no leftover placeholder), rawDir carried over
  expect_false(any(grepl("<<", conf, fixed = TRUE)))
  expect_true(any(grepl("penv = 'mthread'", conf, fixed = TRUE)))
  expect_true(any(grepl("rawDir = '/scratch/foo'", conf, fixed = TRUE)))
  # the closure brace was NOT injected as a clusterOptions value
  expect_false(any(grepl("clusterOptions = '-S /bin/bash {", conf, fixed = TRUE)))
  # backup written
  expect_true(length(list.files(td, pattern = "^\\.config\\.bak\\.", all.files = TRUE)) >= 1)
})


test_that("backwards_compatibility moves export state onto per-unit table", {
  td <- tempfile()
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE))

  create_v1310_db(td)
  make_config(td, version = "1.3.10")

  # Legacy sample-level export state, plus a second sample that was never grouped.
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(td, ".sqlite"))
  DBI::dbExecute(con, "ALTER TABLE samples ADD COLUMN export_group TEXT")
  DBI::dbExecute(con, "ALTER TABLE samples ADD COLUMN export_time_stamp INTEGER")
  DBI::dbExecute(con, "INSERT INTO samples (ID, sample, genetic_code) VALUES ('s2', 'Sample2', '2')")
  DBI::dbExecute(con, "INSERT INTO assemble (ID, assembly) VALUES ('s2', 'ATCG')")
  DBI::dbExecute(con, "INSERT INTO annotate (ID, gene) VALUES ('s2', 'cox1')")
  DBI::dbExecute(con, "UPDATE samples SET export_group = 'grpA', export_time_stamp = 99 WHERE ID = 's1'")
  DBI::dbDisconnect(con)

  suppressMessages(MitoPilot::backwards_compatibility(path = td, update_config = FALSE))

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(td, ".sqlite"))
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # Table exists with the unit key, and the legacy columns are gone from samples
  expect_true("export" %in% DBI::dbListTables(con))
  expect_cols(con, "export", c("ID", "path", "scaffold", "export_group", "export_time_stamp"))
  expect_false(any(c("export_group", "export_time_stamp") %in%
                     DBI::dbListFields(con, "samples")))

  # The grouped sample's units carry the legacy group; the ungrouped one has no row
  got <- DBI::dbGetQuery(con, "SELECT ID, export_group, export_time_stamp FROM export")
  expect_true(all(got$ID == "s1"))
  expect_true(all(got$export_group == "grpA"))
  expect_true(all(got$export_time_stamp == 99))
  expect_false("s2" %in% got$ID)

  # Re-running is a no-op rather than re-migrating
  expect_message(
    MitoPilot::backwards_compatibility(path = td, update_config = FALSE),
    "nothing to update"
  )
})


test_that("export_seqid suffixes only samples with more than one exported unit", {
  # Single exported unit -> plain ID, so single-scaffold projects keep their names
  expect_identical(export_seqid("S1", 1, 1, 1), "S1")
  # More than one -> disambiguated by path and scaffold
  expect_identical(
    export_seqid(rep("S1", 3), c(1, 1, 1), c(1, 2, 3), rep(3, 3)),
    c("S1_p1_s1", "S1_p1_s2", "S1_p1_s3")
  )
  # Vectorised across a mixed set
  expect_identical(
    export_seqid(c("A", "B", "B"), c(1, 1, 1), c(1, 1, 2), c(1, 2, 2)),
    c("A", "B_p1_s1", "B_p1_s2")
  )
  # Scalar n_units must recycle, not collapse every unit onto the first SeqID
  # (ifelse() returns a result the length of its test).
  expect_identical(
    export_seqid(rep("S1", 3), 1, c(1, 2, 3), 3L),
    c("S1_p1_s1", "S1_p1_s2", "S1_p1_s3")
  )
  expect_identical(export_seqid("S1", 1, 1, 1L), "S1")
})


test_that("a user-assembly project gains the circularization and search schema", {
  # Regression: the new tables sat behind the "nothing to update" early exit, so
  # a project current on everything else never received them and the app then
  # failed reading columns that were never added.
  td <- tempfile()
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE))

  create_v1310_db(td)
  make_config(td, version = "1.3.10", has_asmb_dir = TRUE)

  # Mark it as a user-assembly project: its mapping carries an assembly column
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(td, ".sqlite"))
  DBI::dbExecute(con, "ALTER TABLE samples ADD COLUMN assembly TEXT")
  DBI::dbExecute(con, "UPDATE samples SET assembly = 's1.fasta'")
  DBI::dbDisconnect(con)

  MitoPilot::backwards_compatibility(path = td, update_config = FALSE)

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(td, ".sqlite"))
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  expect_true(all(c("circularize_opts", "find_mito_opts", "mito_candidates") %in%
                    DBI::dbListTables(con)))
  expect_cols(con, "assemble", c("circularize_opts", "circularize_notes",
                                 "find_mito_opts", "find_mito_notes"))
  # every sample points at the default parameter sets
  got <- DBI::dbGetQuery(con, "SELECT circularize_opts, find_mito_opts FROM assemble")
  expect_true(all(got$circularize_opts == "default"))
  expect_true(all(got$find_mito_opts == "default"))
  # both steps are off by default
  expect_equal(DBI::dbGetQuery(con, "SELECT attempt FROM circularize_opts")$attempt, 0L)
  expect_equal(DBI::dbGetQuery(con, "SELECT attempt FROM find_mito_opts")$attempt, 0L)

  # the app gate is satisfied, and a re-run is a no-op
  expect_false(any(grepl("mitogenome-search", schema_gaps(con))))
  expect_message(
    MitoPilot::backwards_compatibility(path = td, update_config = FALSE),
    "nothing to update"
  )
})

test_that("a read-based project is not given the user-assembly schema", {
  td <- tempfile()
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE))

  create_v1310_db(td)
  make_config(td, version = "1.3.10")
  MitoPilot::backwards_compatibility(path = td, update_config = FALSE)

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(td, ".sqlite"))
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  expect_false(any(c("circularize_opts", "find_mito_opts", "mito_candidates") %in%
                     DBI::dbListTables(con)))
  expect_false(any(grepl("mitogenome-search", schema_gaps(con))))
})

test_that("a user-assembly project gains the circularization evidence tables", {
  td <- tempfile()
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE))

  create_v1310_db(td)
  make_config(td, version = "1.3.10", has_asmb_dir = TRUE)

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(td, ".sqlite"))
  DBI::dbExecute(con, "ALTER TABLE samples ADD COLUMN assembly TEXT")
  DBI::dbExecute(con, "UPDATE samples SET assembly = 's1.fasta'")
  DBI::dbDisconnect(con)

  MitoPilot::backwards_compatibility(path = td, update_config = FALSE)

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(td, ".sqlite"))
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  expect_true(all(c("circularize_overlap", "circularize_depth") %in%
                    DBI::dbListTables(con)))
  expect_false(any(grepl("circularization", schema_gaps(con))))
  expect_message(
    MitoPilot::backwards_compatibility(path = td, update_config = FALSE),
    "nothing to update"
  )
})


test_that("single-key circularization evidence tables are rebuilt per contig", {
  # The primary key widens to include the contig, which SQLite cannot alter in
  # place, so the tables are dropped and recreated.
  td <- tempfile()
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE))

  create_v1310_db(td)
  make_config(td, version = "1.3.10", has_asmb_dir = TRUE)

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(td, ".sqlite"))
  DBI::dbExecute(con, "ALTER TABLE samples ADD COLUMN assembly TEXT")
  DBI::dbExecute(con, "UPDATE samples SET assembly = 's1.fasta'")
  # The per-sample version of both tables
  DBI::dbExecute(con, "CREATE TABLE circularize_overlap (
    ID TEXT NOT NULL, qstart INTEGER, qend INTEGER, sstart INTEGER,
    send INTEGER, length INTEGER, pident REAL, mismatches INTEGER,
    aln_query TEXT, aln_subject TEXT, q_ctx_left TEXT, q_ctx_right TEXT,
    s_ctx_left TEXT, s_ctx_right TEXT, accepted INTEGER, reason TEXT,
    contig_length INTEGER, trimmed INTEGER, junction_reads INTEGER,
    min_junction_reads INTEGER, window_bp INTEGER, min_overhang INTEGER,
    time_stamp INTEGER, PRIMARY KEY (ID))")
  DBI::dbExecute(con, "INSERT INTO circularize_overlap (ID, accepted) VALUES ('s1', 1)")
  DBI::dbExecute(con, "CREATE TABLE circularize_depth (
    ID TEXT NOT NULL, position INTEGER NOT NULL, rel_position INTEGER,
    depth INTEGER, depth_spanning INTEGER, time_stamp INTEGER,
    PRIMARY KEY (ID, position))")
  DBI::dbExecute(con, "INSERT INTO circularize_depth (ID, position) VALUES ('s1', 1)")
  DBI::dbDisconnect(con)

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(td, ".sqlite"))
  expect_true(any(grepl("circularization", schema_gaps(con))))
  DBI::dbDisconnect(con)

  expect_message(
    MitoPilot::backwards_compatibility(path = td, update_config = FALSE),
    "discarded and will be rebuilt"
  )

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(td, ".sqlite"))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_cols(con, "circularize_overlap", c("contig", "q_ctx_left"))
  expect_cols(con, "circularize_depth", c("contig", "position"))
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM circularize_overlap")$n, 0L)
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM circularize_depth")$n, 0L)
  # The widened key is not visible in dbListFields, so check it directly
  ov_pk <- DBI::dbGetQuery(con, "PRAGMA table_info(circularize_overlap)")
  expect_equal(ov_pk$name[ov_pk$pk > 0], c("ID", "contig"))
  dp_pk <- DBI::dbGetQuery(con, "PRAGMA table_info(circularize_depth)")
  expect_equal(dp_pk$name[dp_pk$pk > 0], c("ID", "contig", "position"))
  expect_false(any(grepl("circularization", schema_gaps(con))))
  expect_message(
    MitoPilot::backwards_compatibility(path = td, update_config = FALSE),
    "nothing to update"
  )
})

test_that("a pre-context circularize_overlap table is brought up to date", {
  # Regression: the modal's contig-context view was added after the table
  # shipped, so a project migrated at the earlier version has the table but not
  # the columns, and must not be told it is up to date.
  td <- tempfile()
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE))

  create_v1310_db(td)
  make_config(td, version = "1.3.10", has_asmb_dir = TRUE)

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(td, ".sqlite"))
  DBI::dbExecute(con, "ALTER TABLE samples ADD COLUMN assembly TEXT")
  DBI::dbExecute(con, "UPDATE samples SET assembly = 's1.fasta'")
  # The pre-context version of the table
  DBI::dbExecute(con, "CREATE TABLE circularize_overlap (
    ID TEXT NOT NULL, aln_query TEXT, aln_subject TEXT, PRIMARY KEY (ID))")
  DBI::dbDisconnect(con)

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(td, ".sqlite"))
  expect_true(any(grepl("circularization", schema_gaps(con))))
  DBI::dbDisconnect(con)

  MitoPilot::backwards_compatibility(path = td, update_config = FALSE)

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(td, ".sqlite"))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_cols(con, "circularize_overlap",
              c("q_ctx_left", "q_ctx_right", "s_ctx_left", "s_ctx_right"))
  expect_false(any(grepl("circularization", schema_gaps(con))))
  expect_message(
    MitoPilot::backwards_compatibility(path = td, update_config = FALSE),
    "nothing to update"
  )
})
