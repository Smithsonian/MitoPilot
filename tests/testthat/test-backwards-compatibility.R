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
    'https://raw.githubusercontent.com/Smithsonian/MitoPilot/refs/heads/devel-DJM/ref_dbs/MitoFinder/NC_002333_Danio_rerio.gb',
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


test_that("backwards_compatibility migrates a v1.0.0 database to current schema", {
  td <- tempfile()
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE))

  create_v100_db(td)
  make_config(td, version = "1.0.0")

  expect_message(
    MitoPilot::backwards_compatibility(path = td),
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
                "max_paths", "max_scaffolds"))

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
    MitoPilot::backwards_compatibility(path = td),
    regexp = "added|updated|created",
    all = FALSE
  )

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(td, ".sqlite"))
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # assemble — still missing blast cols in 1.3.10
  expect_cols(con, "assemble",
              c("blast_accession", "blast_species", "blast_pident",
                "blast_qcovs", "blast_evalue", "blast_lineage", "blast_opts"))

  # annotate_opts — missing mitos_best / aragorn trio in 1.3.10
  expect_cols(con, "annotate_opts",
              c("use_mitos_best", "use_aragorn", "aragorn_opts"))

  # assemble_opts — max_paths / max_scaffolds added in this release
  expect_cols(con, "assemble_opts", c("max_paths", "max_scaffolds"))

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

  # .config — only blast_gb and container missing in 1.3.10
  conf <- readLines(file.path(td, ".config"))
  expect_true(any(grepl("blast_gb", conf)))
  current_ver <- as.character(utils::packageVersion("MitoPilot"))
  expect_true(any(grepl(current_ver, conf, fixed = TRUE)))
})


test_that("backwards_compatibility migrates an unmodified default export template to {completeness}", {
  td <- tempfile()
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE))

  # Bring a v1.0.0 DB up to current (this seeds export_opts with the new default)
  create_v100_db(td)
  make_config(td, version = "1.0.0")
  suppressMessages(MitoPilot::backwards_compatibility(path = td))

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
    suppressWarnings(MitoPilot::backwards_compatibility(path = td)),
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
  suppressMessages(MitoPilot::backwards_compatibility(path = td))

  # Second pass: should early-exit with "nothing to update"
  expect_message(
    MitoPilot::backwards_compatibility(path = td),
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
  suppressMessages(MitoPilot::backwards_compatibility(path = td))

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

  suppressMessages(MitoPilot::backwards_compatibility(path = td))

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(td, ".sqlite"))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  types <- DBI::dbGetQuery(con, "SELECT DISTINCT typeof(genetic_code) AS t FROM samples")$t
  expect_false("text" %in% types)
  # value preserved
  expect_equal(DBI::dbGetQuery(con, "SELECT genetic_code FROM samples")$genetic_code, 2L)
})


test_that("backwards_compatibility regenerates .config and backs up the old one", {
  td <- tempfile()
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE))

  create_v100_db(td)
  make_config(td, version = "1.0.0")  # process executor = 'local', rawDir = 'raw'

  suppressMessages(MitoPilot::backwards_compatibility(path = td))

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

  suppressMessages(MitoPilot::backwards_compatibility(path = td))

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
    MitoPilot::backwards_compatibility(path = td),
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


test_that("migrate_config leaves .config untouched when executor is undetectable", {
  td <- tempfile()
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE))

  # config with no recognizable `executor = '<name>'` literal
  writeLines(c("params {", "    rawDir = 'raw'", "}"), file.path(td, ".config"))
  before <- readLines(file.path(td, ".config"))

  expect_warning(
    res <- MitoPilot:::migrate_config(td),
    regexp = "executor"
  )
  expect_false(res)
  expect_identical(readLines(file.path(td, ".config")), before)
  expect_equal(length(list.files(td, pattern = "^\\.config\\.bak\\.", all.files = TRUE)), 0L)
})
