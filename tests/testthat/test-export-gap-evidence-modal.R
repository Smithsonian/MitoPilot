# Helper behind the export gap-evidence pre-flight gate (gap_evidence_prompts).

gap_db <- function(units, samples, asmb, evidence = NULL,
                   junction_ids = c("S1", "S2")) {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbExecute(con, "CREATE TABLE export (ID TEXT, path INTEGER, scaffold INTEGER,
    export_group TEXT)")
  DBI::dbExecute(con, "CREATE TABLE assemblies (ID TEXT, path INTEGER, scaffold INTEGER,
    sequence TEXT, ignore INTEGER)")
  DBI::dbExecute(con, "CREATE TABLE samples (ID TEXT, Taxon TEXT)")
  DBI::dbExecute(con, "CREATE TABLE assemble (ID TEXT, blast_accession TEXT,
    blast_species TEXT)")
  DBI::dbExecute(con, "CREATE TABLE gap_evidence (ID TEXT PRIMARY KEY,
    genus_match TEXT, time_stamp INTEGER)")
  DBI::dbExecute(con, "CREATE TABLE scaffold_junctions (ID TEXT, junction INTEGER,
    from_scaffold TEXT, to_scaffold TEXT, type TEXT, gap_bases INTEGER,
    size_known INTEGER, time_stamp INTEGER)")
  if (length(junction_ids) > 0) {
    DBI::dbAppendTable(con, "scaffold_junctions", data.frame(
      ID = junction_ids, junction = 1L, from_scaffold = "c1", to_scaffold = "c2",
      type = "join", gap_bases = 10L, size_known = 0L, time_stamp = 1L,
      stringsAsFactors = FALSE
    ))
  }
  DBI::dbAppendTable(con, "export", units[, c("ID", "path", "scaffold", "export_group")])
  DBI::dbAppendTable(con, "assemblies", units[, c("ID", "path", "scaffold",
                                                  "sequence", "ignore")])
  DBI::dbAppendTable(con, "samples", samples)
  DBI::dbAppendTable(con, "assemble", asmb)
  if (!is.null(evidence)) DBI::dbAppendTable(con, "gap_evidence", evidence)
  con
}

# One gapped sample (2 runs of 10 and 15 Ns) and one clean sample.
basic_units <- data.frame(
  ID = c("S1", "S2"),
  path = 0L, scaffold = 1L, export_group = "grp",
  sequence = c(
    paste0("AAAA", strrep("N", 10), "CCCC", strrep("N", 15), "GG"),
    strrep("A", 40)
  ),
  ignore = 0L,
  stringsAsFactors = FALSE
)
basic_samples <- data.frame(
  ID = c("S1", "S2"),
  Taxon = c("Conger oceanicus", "Anguilla rostrata"),
  stringsAsFactors = FALSE
)
basic_asmb <- data.frame(
  ID = c("S1", "S2"),
  blast_accession = c("NC_000001", "NC_000002"),
  blast_species = c(
    "Conger oceanicus voucher USNM:FISH:454713 mitochondrion, complete genome",
    "Anguilla rostrata mitochondrion, complete genome"
  ),
  stringsAsFactors = FALSE
)

test_that("only samples with gaps are listed, with counts and trimmed species", {
  con <- gap_db(basic_units, basic_samples, basic_asmb)
  on.exit(DBI::dbDisconnect(con))
  res <- gap_evidence_prompts(con, "grp")
  expect_equal(res$ID, "S1")
  expect_equal(res$n_gaps, 2L)
  expect_equal(res$gap_bp, 25L)
  expect_equal(res$Taxon, "Conger oceanicus")
  expect_equal(res$blast_accession, "NC_000001")
  expect_equal(res$blast_species, "Conger oceanicus")
})

test_that("runs shorter than min_len do not count", {
  units <- basic_units
  units$sequence[1] <- paste0("AAAA", strrep("N", 5), "CCCC")
  con <- gap_db(units, basic_samples, basic_asmb)
  on.exit(DBI::dbDisconnect(con))
  expect_equal(nrow(gap_evidence_prompts(con, "grp")), 0)
})

test_that("ignored units are excluded", {
  units <- basic_units
  units$ignore[1] <- 1L
  con <- gap_db(units, basic_samples, basic_asmb)
  on.exit(DBI::dbDisconnect(con))
  expect_equal(nrow(gap_evidence_prompts(con, "grp")), 0)
})

test_that("matching genera suggest 'same', case-insensitively", {
  samples <- basic_samples
  samples$Taxon[1] <- "conger oceanicus"
  con <- gap_db(basic_units, samples, basic_asmb)
  on.exit(DBI::dbDisconnect(con))
  expect_equal(gap_evidence_prompts(con, "grp")$genus_match, "same")
})

test_that("differing genera suggest 'different'", {
  asmb <- basic_asmb
  asmb$blast_species[1] <- "Anguilla rostrata mitochondrion, complete genome"
  con <- gap_db(basic_units, basic_samples, asmb)
  on.exit(DBI::dbDisconnect(con))
  expect_equal(gap_evidence_prompts(con, "grp")$genus_match, "different")
})

test_that("an unusable Taxon suggests 'different'", {
  samples <- basic_samples
  samples$Taxon[1] <- "sp. 12"
  con <- gap_db(basic_units, samples, basic_asmb)
  on.exit(DBI::dbDisconnect(con))
  expect_equal(gap_evidence_prompts(con, "grp")$genus_match, "different")

  samples$Taxon[1] <- NA_character_
  con2 <- gap_db(basic_units, samples, basic_asmb)
  on.exit(DBI::dbDisconnect(con2), add = TRUE)
  expect_equal(gap_evidence_prompts(con2, "grp")$genus_match, "different")
})

test_that("a stored gap_evidence value wins over the suggestion", {
  con <- gap_db(
    basic_units, basic_samples, basic_asmb,
    evidence = data.frame(ID = "S1", genus_match = "different",
                          time_stamp = 1L, stringsAsFactors = FALSE)
  )
  on.exit(DBI::dbDisconnect(con))
  expect_equal(gap_evidence_prompts(con, "grp")$genus_match, "different")
})

test_that("gaps in several units of one sample are summed into one row", {
  units <- rbind(
    basic_units,
    data.frame(ID = "S1", path = 0L, scaffold = 2L, export_group = "grp",
               sequence = paste0("AA", strrep("N", 20), "TT"), ignore = 0L,
               stringsAsFactors = FALSE)
  )
  con <- gap_db(units, basic_samples, basic_asmb)
  on.exit(DBI::dbDisconnect(con))
  res <- gap_evidence_prompts(con, "grp")
  expect_equal(nrow(res), 1)
  expect_equal(res$n_gaps, 3L)
  expect_equal(res$gap_bp, 45L)
})

test_that("other export groups are ignored", {
  con <- gap_db(basic_units, basic_samples, basic_asmb)
  on.exit(DBI::dbDisconnect(con))
  expect_equal(nrow(gap_evidence_prompts(con, "other")), 0)
})

test_that("a gapped sample with no scaffold_junctions rows is not prompted", {
  con <- gap_db(basic_units, basic_samples, basic_asmb, junction_ids = character(0))
  on.exit(DBI::dbDisconnect(con))
  expect_equal(nrow(gap_evidence_prompts(con, "grp")), 0)

  # Junctions for a different sample do not pull S1 in
  con2 <- gap_db(basic_units, basic_samples, basic_asmb, junction_ids = "S2")
  on.exit(DBI::dbDisconnect(con2), add = TRUE)
  expect_equal(nrow(gap_evidence_prompts(con2, "grp")), 0)
})

test_that("only path 0 units are prompted", {
  units <- basic_units
  units$path[1] <- 1L
  con <- gap_db(units, basic_samples, basic_asmb)
  on.exit(DBI::dbDisconnect(con))
  expect_equal(nrow(gap_evidence_prompts(con, "grp")), 0)
})

test_that("a NULL ignore value is excluded, as in export_files()", {
  units <- basic_units
  units$ignore <- NA_integer_
  con <- gap_db(units, basic_samples, basic_asmb)
  on.exit(DBI::dbDisconnect(con))
  expect_equal(nrow(gap_evidence_prompts(con, "grp")), 0)
})

test_that("a missing scaffold_junctions table prompts nobody", {
  con <- gap_db(basic_units, basic_samples, basic_asmb)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "DROP TABLE scaffold_junctions")
  expect_equal(nrow(gap_evidence_prompts(con, "grp")), 0)
})

test_that("a prefixed defline keeps its binomial", {
  expect_equal(
    species_binomial("UNVERIFIED: Conger oceanicus mitochondrion"),
    "Conger oceanicus"
  )
})
