# Column order in the assemble table is set by the dplyr::relocate() in
# fetch_assemble_data() and differs between the two project flavours. These
# fixtures build real project databases (R/init_db.R) so a drift in either
# vector fails here instead of in the app.
make_fetch_db <- function(userAsmb) {
  dir <- tempfile()
  dir.create(dir, recursive = TRUE)
  mapping <- data.frame(
    ID = c("SAMP_01", "SAMP_02"),
    Taxon = c("Danio rerio", "Gadus morhua"),
    R1 = c("r/1_R1.fq.gz", "r/2_R1.fq.gz"),
    R2 = c("r/1_R2.fq.gz", "r/2_R2.fq.gz"),
    stringsAsFactors = FALSE
  )
  if (userAsmb) {
    mapping$Assembly <- c("a/1.fasta", "a/2.fasta")
    mapping$Topology <- c("circular", "linear")
  }
  mapping_fn <- file.path(dir, "mapping.csv")
  utils::write.csv(mapping, mapping_fn, row.names = FALSE)
  db_path <- file.path(dir, ".sqlite")
  suppressMessages(
    if (userAsmb) {
      new_db_userAsmb(db_path = db_path, mapping_fn = mapping_fn)
    } else {
      new_db(db_path = db_path, mapping_fn = mapping_fn)
    }
  )
  db_path
}

# fetch_assemble_data() only reads session$userData$con.
fake_session <- function(con) {
  list(userData = list(con = con, no_raw_data = FALSE))
}

test_that("standard projects get the standard column order", {
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = make_fetch_db(FALSE))
  on.exit(DBI::dbDisconnect(con))

  out <- fetch_assemble_data(session = fake_session(con))

  expect_identical(names(out), c(
    "assemble_lock", "assemble_switch", "ID", "Taxon", "pre_opts",
    "assemble_opts", "blast_opts", "reads", "trimmed_reads", "mean_length",
    "topology", "length", "paths", "scaffolds", "blast_accession",
    "blast_ref_status", "blast_species", "blast_pident", "blast_qcovs",
    "blast_evalue", "blast_lineage", "blast_hits", "time_stamp",
    "assemble_notes", "hide_switch", "blast_accession_auto",
    "synteny_accession", "poor_blast_ref", "R1", "R2", "min_assembly_length",
    "ignore_flags", "output", "view"
  ))
  expect_equal(nrow(out), 2)
})

test_that("user-assembly projects get the userAsmb column order", {
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = make_fetch_db(TRUE))
  on.exit(DBI::dbDisconnect(con))

  out <- fetch_assemble_data(userAsmb = TRUE, session = fake_session(con))

  expect_identical(names(out), c(
    "assemble_lock", "assemble_switch", "ID", "Taxon", "assembly", "topology",
    "pre_opts", "blast_opts", "reads", "trimmed_reads", "mean_length",
    "length", "paths", "scaffolds", "blast_accession", "blast_ref_status",
    "blast_species", "blast_pident", "blast_qcovs", "blast_evalue",
    "blast_lineage", "blast_hits", "time_stamp", "assemble_notes",
    "hide_switch", "assemble_opts", "blast_accession_auto",
    "synteny_accession", "poor_blast_ref", "R1", "R2", "output", "view"
  ))
  expect_equal(nrow(out), 2)
  # topology comes from the samples table, not the workflow-computed column
  expect_identical(out$topology[order(out$ID)], c("circular", "linear"))
  expect_identical(out$assembly[order(out$ID)], c("a/1.fasta", "a/2.fasta"))
})

test_that("the standard per-scaffold columns stay off for user assemblies", {
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = make_fetch_db(TRUE))
  on.exit(DBI::dbDisconnect(con))

  out <- fetch_assemble_data(userAsmb = TRUE, session = fake_session(con))

  expect_false("ignore_flags" %in% names(out))
  expect_false("min_assembly_length" %in% names(out))
})
