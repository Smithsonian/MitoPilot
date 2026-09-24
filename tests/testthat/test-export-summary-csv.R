# The summary CSV must describe the units export_files() actually wrote.
#
# export_files() picks units from `assemblies` (ignore == 0) and never looks at
# the lock columns, so a caller that passes IDs of samples that were never
# annotate-locked still gets .tbl/.fasta/.gff files. The summary used to come
# from fetch_export_data() with its Export-tab gate (assemble_lock == 1 AND
# annotate_lock == 1), so the same call wrote a header-only CSV.

L <- 1200L
withr::with_seed(11, {
  seq_chr <- paste(sample(c("A", "C", "G", "T"), L, replace = TRUE), collapse = "")
})

summary_fixture <- function(d, annotate_lock, assemble_lock) {
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(d, ".sqlite"))
  DBI::dbWriteTable(con, "annotations", data.frame(
    ID = "s1", path = 1L, scaffold = 1L, contig = "s1.1.1", type = "PCG",
    gene = "cox1", product = "cox1", pos1 = 100L, pos2 = 700L, length = 601L,
    direction = "+", start_codon = "ATG", stop_codon = "TAA",
    translation = strrep("M", 30L), anticodon = NA_character_,
    partial_start = 0L, partial_stop = 0L, notes = NA_character_, refHits = "{}",
    warnings = NA_character_, stringsAsFactors = FALSE
  ))
  DBI::dbWriteTable(con, "assemblies", data.frame(
    ID = "s1", path = 1L, scaffold = 1L, ignore = 0L, topology = "circular",
    sequence = seq_chr, blast_accession = "NC_000001",
    blast_species = "Testus testus", blast_lineage = "Metazoa",
    blast_pident = 99, blast_qcovs = 99, stringsAsFactors = FALSE
  ))
  DBI::dbWriteTable(con, "export", data.frame(
    ID = character(0), path = integer(0), scaffold = integer(0),
    export_group = character(0), export_time_stamp = integer(0),
    stringsAsFactors = FALSE
  ))
  DBI::dbWriteTable(con, "annotate", data.frame(
    ID = "s1", path = 1L, scaffold = 1L, annotate_lock = annotate_lock,
    topology = "circular", partial = "no", curate_opts = "default",
    orf_opts = "default", length = L, structure = "", PCGCount = 1L,
    tRNACount = 0L, rRNACount = 0L, missing = NA_character_,
    extra = NA_character_, warnings = NA_character_, stringsAsFactors = FALSE
  ))
  DBI::dbWriteTable(con, "curate_opts", data.frame(
    curate_opts = "default", params = "{}", linear_complete = 0L,
    stringsAsFactors = FALSE
  ))
  DBI::dbWriteTable(con, "orf_opts", data.frame(
    orf_opts = "default", use_orffinder = 0L, stringsAsFactors = FALSE
  ))
  DBI::dbWriteTable(con, "samples", data.frame(
    ID = "s1", Taxon = "Testus testus", genetic_code = 2L, topology = "circular",
    R1 = NA_character_, R2 = NA_character_, stringsAsFactors = FALSE
  ))
  DBI::dbWriteTable(con, "assemble", data.frame(
    ID = "s1", assemble_lock = assemble_lock, blast_accession = "NC_000001",
    blast_accession_auto = 0L, poor_blast_ref = "ok", stringsAsFactors = FALSE
  ))
  .geome_ensure_tables(con)
  DBI::dbDisconnect(con)
}

run_summary_export <- function(annotate_lock, assemble_lock) {
  d <- withr::local_tempdir(.local_envir = parent.frame())
  out_dir <- file.path(d, "out")
  dir.create(out_dir)
  summary_fixture(d, annotate_lock, assemble_lock)
  suppressMessages(export_files(
    IDs = "s1", out_dir = out_dir, generateAAalignments = FALSE,
    gene_export = FALSE, review = FALSE, summary_csv = TRUE
  ))
  list(
    tbl = file.exists(file.path(out_dir, "s1", "export", "s1.tbl")),
    summary = utils::read.csv(list.files(out_dir, "^sample_info_.*[.]csv$",
                                         full.names = TRUE)[1])
  )
}

test_that("the summary CSV lists every exported unit even when nothing is locked", {
  res <- run_summary_export(annotate_lock = 0L, assemble_lock = 0L)
  expect_true(res$tbl)
  expect_equal(nrow(res$summary), 1L)
  expect_equal(res$summary$ID, "s1")
  expect_equal(res$summary$seqid, "s1")
})

test_that("the summary CSV is unchanged for a locked unit", {
  res <- run_summary_export(annotate_lock = 1L, assemble_lock = 1L)
  expect_true(res$tbl)
  expect_equal(nrow(res$summary), 1L)
  expect_equal(res$summary$completeness, "complete genome")
})
