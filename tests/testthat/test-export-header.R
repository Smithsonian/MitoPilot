# A sample with more than one assembly unit emits one GenBank record per unit, and
# each record's FASTA defline must match its .tbl >Feature line exactly or table2asn
# rejects the submission. That only holds if the header template carries {seqid};
# an {ID} template gives every unit of a sample the same defline.

single_unit <- data.frame(
  ID = c("s1", "s2"),
  seqid = c("s1", "s2"),
  Taxon = "Gadus morhua",
  topology = "circular",
  genetic_code = 2L,
  completeness = "complete genome",
  stringsAsFactors = FALSE
)

multi_unit <- data.frame(
  ID = c("s1", "s1", "s2"),
  seqid = c("s1_p1_s1", "s1_p1_s2", "s2"),
  Taxon = "Gadus morhua",
  topology = "circular",
  genetic_code = 2L,
  completeness = "complete genome",
  stringsAsFactors = FALSE
)

test_that("the default header templates validate cleanly", {
  expect_equal(
    validate_fasta_header(DEFAULT_FASTA_HEADER, single_unit, require_completeness = TRUE)$level,
    "ok"
  )
  expect_equal(
    validate_fasta_header(DEFAULT_FASTA_HEADER, multi_unit, require_completeness = TRUE)$level,
    "ok"
  )
  expect_equal(
    validate_fasta_header(DEFAULT_FASTA_HEADER_GENE, multi_unit)$level,
    "ok"
  )
})

test_that("an {ID} template is rejected once a sample has multiple units", {
  res <- validate_fasta_header(LEGACY_FASTA_HEADER_ID, multi_unit, require_completeness = TRUE)
  expect_false(res$ok)
  expect_match(res$message, "seqid", fixed = TRUE)
})

test_that("an {ID} template only warns while every sample is single-unit", {
  res <- validate_fasta_header(LEGACY_FASTA_HEADER_ID, single_unit, require_completeness = TRUE)
  expect_true(res$ok)
  expect_equal(res$level, "warn")
})

test_that("export_seqid suffixes only multi-unit samples", {
  expect_equal(export_seqid("s1", 1L, 1L, n_units = 1L), "s1")
  expect_equal(
    export_seqid(c("s1", "s1"), c(1L, 1L), c(1L, 2L), n_units = 2L),
    c("s1_p1_s1", "s1_p1_s2")
  )
})

test_that("check_single_path blocks multi-path samples but allows fragmented ones", {
  # one unit
  expect_true(check_single_path(data.frame(ID = "s1", path = 1L, scaffold = 1L)))

  # several scaffolds of ONE path: may be genuinely separate genomes, allowed here
  # (the app warns) rather than blocked
  expect_true(
    check_single_path(data.frame(ID = rep("s1", 3), path = 1L, scaffold = 1:3))
  )

  # two paths for one sample: competing resolutions of the same genome
  expect_error(
    check_single_path(data.frame(ID = rep("s1", 2), path = c(1L, 2L), scaffold = 1L)),
    "more than one assembly path"
  )

  # only the offending sample is named
  expect_error(
    check_single_path(
      data.frame(ID = c("s1", "s2", "s2"), path = c(1L, 1L, 2L), scaffold = 1L)
    ),
    "s2"
  )
})
