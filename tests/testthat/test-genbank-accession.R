# GenBank accessions come from a user-named mapping column, are stored as
# samples.GenBankAccession, and are flagged at export. Empty values (including
# an all-empty column, which SQLite hands back as NA) must not break export.

test_that(".clean_accession treats blanks, whitespace, and 'NA' as missing", {
  expect_equal(
    .clean_accession(c(" MN123456.1 ", "", NA, "NA", "  ")),
    c("MN123456.1", NA, NA, NA, NA)
  )
  expect_equal(.clean_accession(NA), NA_character_)
})

test_that(".is_accession accepts GenBank and RefSeq shapes", {
  expect_true(all(.is_accession(c("MN123456", "MN123456.1", "NC_012345.1", "U12345"))))
  expect_false(any(.is_accession(c("pending", "12345", "SAMN0001"))))
})

test_that(".take_genbank_col copies a named column into GenBankAccession", {
  m <- data.frame(ID = c("a", "b"), Acc = c("MN123456.1", ""))
  out <- .take_genbank_col(m, "Acc")
  expect_equal(out$GenBankAccession, c("MN123456.1", NA))
  expect_equal(out$Acc, m$Acc)
})

test_that(".take_genbank_col uses GenBankAccession by default and stops on a missing named column", {
  m <- data.frame(ID = "a", GenBankAccession = NA)
  expect_equal(.take_genbank_col(m)$GenBankAccession, NA_character_)
  expect_identical(.take_genbank_col(data.frame(ID = "a")), data.frame(ID = "a"))
  expect_error(.take_genbank_col(m, "Acc"), "'Acc' not found")
})

test_that("check_mapping validates the GenBank accession column", {
  m <- data.frame(ID = c("a", "b"), Taxon = "T", R1 = c("a1", "b1"), R2 = c("a2", "b2"),
                  Acc = c("MN123456", "pending"))
  iss <- check_mapping(m, mapping_genbank = "Acc")
  expect_length(iss$errors, 0)
  expect_true(any(grepl("b \\[pending\\]", iss$warnings)))

  iss <- check_mapping(m, mapping_genbank = "Nope")
  expect_true(any(grepl("'Nope' not found", iss$errors)))

  m$GenBankAccession <- "MN000001"
  iss <- check_mapping(m, mapping_genbank = "Acc")
  expect_true(any(grepl("reserved names.*GenBankAccession", iss$errors)))
})

L <- 1200L
withr::with_seed(7, {
  gb_seq <- paste(sample(c("A", "C", "G", "T"), L, replace = TRUE), collapse = "")
})

gb_fixture <- function(d, accession) {
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(d, ".sqlite"))
  on.exit(DBI::dbDisconnect(con))
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
    sequence = gb_seq, blast_accession = "NC_000001",
    blast_species = "Testus testus", blast_lineage = "Metazoa",
    blast_pident = 99, blast_qcovs = 99, stringsAsFactors = FALSE
  ))
  DBI::dbWriteTable(con, "export", data.frame(
    ID = character(0), path = integer(0), scaffold = integer(0),
    export_group = character(0), export_time_stamp = integer(0),
    stringsAsFactors = FALSE
  ))
  DBI::dbWriteTable(con, "annotate", data.frame(
    ID = "s1", path = 1L, scaffold = 1L, annotate_lock = 1L,
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
  # Created without column types, like new_db() does, so an all-empty
  # metadata column comes back as NA.
  DBI::dbExecute(con, "CREATE TABLE samples (ID, Taxon, genetic_code, R1, R2, GenBankAccession, Voucher)")
  DBI::dbExecute(con, "INSERT INTO samples VALUES ('s1', 'Testus testus', 2, NULL, NULL, ?, NULL)",
                 params = list(accession))
  DBI::dbWriteTable(con, "assemble", data.frame(
    ID = "s1", assemble_lock = 1L, blast_accession = "NC_000001",
    blast_accession_auto = 0L, poor_blast_ref = "ok", stringsAsFactors = FALSE
  ))
}

run_gb_export <- function(accession, header = NULL) {
  d <- withr::local_tempdir(.local_envir = parent.frame())
  out_dir <- file.path(d, "out")
  dir.create(out_dir)
  gb_fixture(d, accession)
  args <- list(IDs = "s1", out_dir = out_dir, generateAAalignments = FALSE,
               gene_export = FALSE, review = FALSE, summary_csv = FALSE)
  if (!is.null(header)) args$fasta_header <- header
  msgs <- testthat::capture_messages(do.call(export_files, args))
  list(msgs = msgs, tbl = readLines(file.path(out_dir, "s1", "export", "s1.tbl")))
}

test_that("export succeeds when GenBankAccession is missing", {
  res <- run_gb_export(NA_character_)
  expect_equal(res$tbl[1], ">Feature s1")
  expect_false(any(grepl("GenBank accession", res$msgs)))
})

test_that("export references and warns about an existing accession", {
  res <- run_gb_export(" MN123456.1 ")
  expect_equal(res$tbl[1], ">Feature gb|MN123456.1|")
  expect_true(any(grepl("s1: WARNING already has GenBank accession MN123456.1", res$msgs)))
})

test_that("export warns when a header field is empty", {
  res <- run_gb_export(NA_character_, header = "{seqid} [organism={Taxon}] [voucher={Voucher}]")
  expect_true(any(grepl("empty FASTA header field\\(s\\).*Voucher", res$msgs)))
})

test_that("header_missing_fields reports only empty template columns", {
  row <- data.frame(seqid = "s1", Taxon = "T", Voucher = NA, Blank = " ")
  expect_equal(header_missing_fields("{seqid} {Taxon} {Voucher} {Blank} {nope}", row),
               c("Voucher", "Blank"))
})

test_that("export ignores and warns about a value that is not an accession", {
  res <- run_gb_export("pending")
  expect_equal(res$tbl[1], ">Feature s1")
  expect_true(any(grepl("'pending' is not a GenBank accession and was ignored", res$msgs)))
})
