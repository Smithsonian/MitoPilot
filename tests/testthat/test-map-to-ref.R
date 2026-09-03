mtr_fixture <- function() {
  p <- system.file("test_data/NC_002333_Danio_rerio.gb", package = "MitoPilot")
  if (!nzchar(p)) p <- testthat::test_path("../..", "inst/test_data/NC_002333_Danio_rerio.gb")
  p
}

mtr_sampler <- function() {
  p <- system.file("test_data/fish_mito_sampler.gb", package = "MitoPilot")
  if (!nzchar(p)) p <- testthat::test_path("../..", "inst/test_data/fish_mito_sampler.gb")
  p
}

mtr_write <- function(dir, name, lines, eol = "\n") {
  fn <- file.path(dir, name)
  con <- file(fn, open = "wb")
  writeLines(lines, con, sep = eol)
  close(con)
  fn
}

test_that("maptoref_prepare_ref reads a single-record circular GenBank reference", {
  skip_if_not(file.exists(mtr_fixture()))
  d <- withr::local_tempdir()
  ref <- maptoref_prepare_ref(mtr_fixture(), out_dir = d)

  expect_equal(ref$topology, "circular")
  expect_equal(ref$length, 16596L)
  expect_equal(nchar(ref$seq), 16596L)
  expect_equal(ref$accession, "NC_002333.2")
  expect_equal(ref$transl_table, 2L)
  expect_true(grepl("Danio rerio", ref$organism))
  expect_true(file.exists(file.path(d, "maptoref", "ref.fasta")))
  expect_true(file.exists(file.path(d, "maptoref", "reference.gb")))
  expect_equal(
    readLines(file.path(d, "maptoref", "ref.fasta"))[1],
    ">NC_002333.2 circular"
  )
})

test_that("maptoref_prepare_ref keeps a record with no organelle qualifier", {
  d <- withr::local_tempdir()
  gb <- mtr_write(d, "plain.gb", c(
    "LOCUS       TEST0001               12000 bp    DNA     linear   INV",
    "DEFINITION  Testus testus mitochondrion, complete genome.",
    "VERSION     TEST0001.1",
    "FEATURES             Location/Qualifiers",
    "     source          1..12000",
    '                     /organism="Testus testus"',
    "     CDS             1..30",
    "                     /transl_table=5",
    "ORIGIN",
    rep(paste("        1", paste(rep("acgt", 15), collapse = " ")), 200),
    "//"
  ))
  ref <- maptoref_prepare_ref(gb, out_dir = d)
  expect_equal(ref$topology, "linear")
  expect_equal(ref$transl_table, 5L)
  expect_equal(ref$accession, "TEST0001.1")
})

test_that("maptoref_prepare_ref handles CRLF line endings", {
  d <- withr::local_tempdir()
  gb <- mtr_write(d, "crlf.gb", c(
    "LOCUS       TEST0002               12000 bp    DNA     circular INV",
    "DEFINITION  Testus testus mitochondrion, complete genome.",
    "VERSION     TEST0002.1",
    "ORIGIN",
    rep(paste("        1", paste(rep("acgt", 15), collapse = " ")), 200),
    "//"
  ), eol = "\r\n")
  ref <- maptoref_prepare_ref(gb, out_dir = d)
  expect_equal(ref$topology, "circular")
  expect_equal(nchar(ref$seq), 12000L)
})

test_that("maptoref_prepare_ref rejects a multi-record GenBank file", {
  skip_if_not(file.exists(mtr_sampler()))
  d <- withr::local_tempdir()
  expect_error(
    maptoref_prepare_ref(mtr_sampler(), out_dir = d),
    "exactly one record"
  )
})

test_that("maptoref_prepare_ref requires an explicit topology for FASTA references", {
  d <- withr::local_tempdir()
  fa <- mtr_write(d, "ref.fasta", c(">circular_ref some description",
                                    paste(rep("ACGT", 3000), collapse = "")))
  expect_error(maptoref_prepare_ref(fa, out_dir = d), "topology")

  ref <- maptoref_prepare_ref(fa, topology = "circular", out_dir = d)
  expect_equal(ref$topology, "circular")
  expect_equal(ref$accession, "circular_ref")
  expect_equal(
    readLines(file.path(d, "maptoref", "ref.fasta"))[1],
    ">circular_ref circular"
  )
})

test_that("a GenBank LOCUS line beats the topology option", {
  skip_if_not(file.exists(mtr_fixture()))
  d <- withr::local_tempdir()
  ref <- maptoref_prepare_ref(mtr_fixture(), topology = "linear", out_dir = d)
  expect_equal(ref$topology, "circular")
})

test_that("maptoref_prepare_ref falls back to the topology option when LOCUS has neither token", {
  d <- withr::local_tempdir()
  gb <- mtr_write(d, "notoken.gb", c(
    "LOCUS       TEST0004               12000 bp    DNA     INV",
    "DEFINITION  Testus testus mitochondrion, complete genome.",
    "VERSION     TEST0004.1",
    "ORIGIN",
    rep(paste("        1", paste(rep("acgt", 15), collapse = " ")), 200),
    "//"
  ))
  ref <- maptoref_prepare_ref(gb, topology = "circular", out_dir = d)
  expect_equal(ref$topology, "circular")
})

test_that("maptoref_prepare_ref requires a topology when LOCUS has neither token and none is given", {
  d <- withr::local_tempdir()
  gb <- mtr_write(d, "notoken2.gb", c(
    "LOCUS       TEST0005               12000 bp    DNA     INV",
    "DEFINITION  Testus testus mitochondrion, complete genome.",
    "VERSION     TEST0005.1",
    "ORIGIN",
    rep(paste("        1", paste(rep("acgt", 15), collapse = " ")), 200),
    "//"
  ))
  expect_error(maptoref_prepare_ref(gb, out_dir = d), "topology")
})

test_that("maptoref_prepare_ref rejects a GenBank record with an empty ORIGIN block", {
  d <- withr::local_tempdir()
  gb <- mtr_write(d, "emptyorigin.gb", c(
    "LOCUS       TEST0006               12000 bp    DNA     linear   INV",
    "DEFINITION  Testus testus mitochondrion, complete genome.",
    "VERSION     TEST0006.1",
    "ORIGIN",
    "//"
  ))
  expect_error(maptoref_prepare_ref(gb, out_dir = d), "ORIGIN sequence")
})

test_that("maptoref_prepare_ref rejects a header-only FASTA", {
  d <- withr::local_tempdir()
  fa <- mtr_write(d, "headeronly.fasta", c(">a"))
  expect_error(maptoref_prepare_ref(fa, topology = "linear", out_dir = d), "no sequence")
})

test_that("maptoref_prepare_ref rejects gap characters in a GenBank reference", {
  d <- withr::local_tempdir()
  gb <- mtr_write(d, "gapgb.gb", c(
    "LOCUS       TEST0007               12000 bp    DNA     linear   INV",
    "DEFINITION  Testus testus mitochondrion, complete genome.",
    "VERSION     TEST0007.1",
    "ORIGIN",
    rep(paste("        1", paste(rep("acg-", 15), collapse = " ")), 200),
    "//"
  ))
  expect_error(maptoref_prepare_ref(gb, out_dir = d), "invalid character")
})

test_that("maptoref_prepare_ref rejects bad sequences, counts, and lengths", {
  d <- withr::local_tempdir()

  two <- mtr_write(d, "two.fasta", c(">a", paste(rep("ACGT", 3000), collapse = ""),
                                     ">b", paste(rep("ACGT", 3000), collapse = "")))
  expect_error(maptoref_prepare_ref(two, topology = "linear", out_dir = d), "exactly one record")

  gappy <- mtr_write(d, "gap.fasta", c(">a", paste(rep("ACG-", 3000), collapse = "")))
  expect_error(maptoref_prepare_ref(gappy, topology = "linear", out_dir = d), "invalid character")

  short <- mtr_write(d, "short.fasta", c(">a", paste(rep("ACGT", 100), collapse = "")))
  expect_error(maptoref_prepare_ref(short, topology = "linear", out_dir = d), "length")
})

test_that("maptoref_prepare_ref warns instead of failing on soft problems", {
  d <- withr::local_tempdir()
  odd <- mtr_write(d, "odd.fasta", c(">a", paste0(paste(rep("ACGT", 1500), collapse = ""),
                                                  paste(rep("N", 400), collapse = ""))))
  ref <- maptoref_prepare_ref(odd, topology = "linear", out_dir = d)
  expect_true(any(grepl("ambiguous", ref$notes)))
  expect_true(any(grepl("outside the usual", ref$notes)))
})

test_that("maptoref_prepare_ref warns when the genetic codes disagree", {
  skip_if_not(file.exists(mtr_fixture()))
  d <- withr::local_tempdir()
  ref <- maptoref_prepare_ref(mtr_fixture(), genetic_code = 5L, out_dir = d)
  expect_true(any(grepl("genetic code", ref$notes)))
})

test_that("maptoref_prepare_ref tolerates a non-numeric genetic_code", {
  skip_if_not(file.exists(mtr_fixture()))
  d <- withr::local_tempdir()
  ref <- maptoref_prepare_ref(mtr_fixture(), genetic_code = "not-a-code", out_dir = d)
  expect_false(any(grepl("genetic code", ref$notes)))
})
