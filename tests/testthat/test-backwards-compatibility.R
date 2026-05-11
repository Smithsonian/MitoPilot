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
  expect_cols(con, "annotate", c("reviewed", "ID_verified", "problematic"))

  # assemble
  expect_cols(con, "assemble",
              c("blast_accession", "blast_species", "blast_pident",
                "blast_qcovs", "blast_evalue", "blast_lineage", "blast_opts"))

  # assemble_opts
  expect_cols(con, "assemble_opts", c("assembler", "mitofinder_db", "mitofinder"))

  # annotate_opts
  expect_cols(con, "annotate_opts",
              c("use_arwen", "arwen_opts", "use_mitos_best",
                "use_aragorn", "aragorn_opts", "start_gene"))

  # curate_opts
  expect_cols(con, "curate_opts", c("max_blast_hits", "ref_db", "ref_dir"))

  # annotations
  expect_cols(con, "annotations", "tool")

  # new tables
  tables <- DBI::dbListTables(con)
  expect_true("blast_opts"            %in% tables)
  expect_true("blast_ref_annotations" %in% tables)
  expect_true("blast_ref_sequences"   %in% tables)
  expect_true("blast_ref_alignment"   %in% tables)

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

  # annotations
  expect_cols(con, "annotations", "tool")

  # new tables
  tables <- DBI::dbListTables(con)
  expect_true("blast_opts"            %in% tables)
  expect_true("blast_ref_annotations" %in% tables)
  expect_true("blast_ref_sequences"   %in% tables)
  expect_true("blast_ref_alignment"   %in% tables)

  # .config — only blast_gb and container missing in 1.3.10
  conf <- readLines(file.path(td, ".config"))
  expect_true(any(grepl("blast_gb", conf)))
  current_ver <- as.character(utils::packageVersion("MitoPilot"))
  expect_true(any(grepl(current_ver, conf, fixed = TRUE)))
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
