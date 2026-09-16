gb_fixture <- function() {
  testthat::skip_if_not(
    file.exists(system.file("test_data", "NC_002333_Danio_rerio.gb",
                            package = "MitoPilot")),
    "packaged GenBank fixture not available"
  )
  system.file("test_data", "NC_002333_Danio_rerio.gb", package = "MitoPilot")
}

test_that(".mtr_parse_location reads a plain span", {
  out <- .mtr_parse_location("3803..4777")
  expect_equal(nrow(out), 1L)
  expect_equal(out$start, 3803L)
  expect_equal(out$end, 4777L)
  expect_equal(out$strand, "+")
})

test_that(".mtr_parse_location marks complement as minus strand", {
  out <- .mtr_parse_location("complement(4852..4922)")
  expect_equal(out$start, 4852L)
  expect_equal(out$end, 4922L)
  expect_equal(out$strand, "-")
})

test_that(".mtr_parse_location emits one row per join segment", {
  out <- .mtr_parse_location("join(1..10,20..30)")
  expect_equal(nrow(out), 2L)
  expect_equal(out$start, c(1L, 20L))
  expect_equal(out$end, c(10L, 30L))
  expect_equal(out$strand, c("+", "+"))
})

test_that(".mtr_parse_location carries complement into every join segment", {
  out <- .mtr_parse_location("complement(join(1..10,20..30))")
  expect_equal(nrow(out), 2L)
  expect_equal(out$strand, c("-", "-"))
})

test_that(".mtr_parse_location tolerates a partial-span marker", {
  out <- .mtr_parse_location("<1..100")
  expect_equal(out$start, 1L)
  expect_equal(out$end, 100L)
})

test_that(".mtr_parse_location returns no rows for empty input", {
  expect_equal(nrow(.mtr_parse_location(NA_character_)), 0L)
  expect_equal(nrow(.mtr_parse_location("")), 0L)
})

test_that("maptoref_parse_features returns the canonical mitogenome features", {
  out <- maptoref_parse_features(gb_fixture())
  expect_equal(nrow(out), 38L)
  expect_equal(
    as.integer(table(out$type)[c("CDS", "D-loop", "rRNA", "tRNA")]),
    c(13L, 1L, 2L, 22L)
  )
  expect_equal(names(out), c("type", "gene", "start", "end", "strand"))
})

test_that("maptoref_parse_features drops gene rows duplicating typed features", {
  out <- maptoref_parse_features(gb_fixture())
  expect_false("gene" %in% out$type)
  expect_false(any(duplicated(paste(out$start, out$end))))
})

test_that("maptoref_parse_features reads coordinates and strand correctly", {
  out <- maptoref_parse_features(gb_fixture())
  nd1 <- out[out$type == "CDS" & out$gene == "ND1", ]
  expect_equal(nrow(nd1), 1L)
  expect_equal(nd1$start, 3803L)
  expect_equal(nd1$end, 4777L)
  expect_equal(nd1$strand, "+")
  trnq <- out[out$gene == "trnQ", ]
  expect_equal(trnq$strand, "-")
  expect_equal(trnq$start, 4852L)
})

test_that("maptoref_parse_features sorts by position", {
  out <- maptoref_parse_features(gb_fixture())
  expect_false(is.unsorted(out$start))
  expect_equal(out$type[1], "D-loop")
  expect_equal(out$start[1], 1L)
})

test_that("maptoref_parse_features returns an empty frame for a missing file", {
  out <- maptoref_parse_features(file.path(tempdir(), "does_not_exist.gb"))
  expect_equal(nrow(out), 0L)
  expect_equal(names(out), c("type", "gene", "start", "end", "strand"))
})

test_that("maptoref_parse_features returns an empty frame for a non-GenBank file", {
  d <- withr::local_tempdir()
  fn <- file.path(d, "not_a_record.gb")
  writeLines(c(">seq", "ACGTACGTAC"), fn)
  out <- maptoref_parse_features(fn)
  expect_equal(nrow(out), 0L)
})

test_that("maptoref_parse_features prints nothing", {
  expect_silent(maptoref_parse_features(gb_fixture()))
})

test_that("maptoref_parse_features resets rownames after sorting", {
  # Features listed out of positional order (LATE before EARLY) so the
  # sort actually permutes rows; a fixture already in order can't catch
  # a rownames-reset-before-sort bug.
  d <- withr::local_tempdir()
  fn <- file.path(d, "unordered.gb")
  writeLines(c(
    "LOCUS       TESTSEQ                  300 bp    DNA     linear   VRT 01-JAN-2024",
    "DEFINITION  Test mitochondrion, partial genome.",
    "ACCESSION   TESTSEQ",
    "VERSION     TESTSEQ.1",
    "KEYWORDS    .",
    "SOURCE      mitochondrion Test testus",
    "  ORGANISM  Test testus",
    "            Eukaryota; Metazoa; Chordata.",
    "REFERENCE   1  (bases 1 to 300)",
    "  AUTHORS   Test,A.",
    "  TITLE     Direct Submission",
    "  JOURNAL   Unpublished",
    "FEATURES             Location/Qualifiers",
    "     source          1..300",
    "                     /organism=\"Test testus\"",
    "                     /mol_type=\"genomic DNA\"",
    "     CDS             201..300",
    "                     /gene=\"LATE\"",
    "                     /product=\"late protein\"",
    "     CDS             1..100",
    "                     /gene=\"EARLY\"",
    "                     /product=\"early protein\"",
    "ORIGIN",
    "        1 acggccggcg acaatttata tgtcaatgtt ttagtaattt acaattaaga cagacatgca",
    "       61 ctgtattgat acattaatca tacataaaat gcatgctcta atttttacat atgcatgtgc",
    "      121 aagcatatac atatgtgtgg atacacacgt atgtgctttg tcacatgtat gtactggtta",
    "      181 catattatgc atgtattagg acatactatg tattatcacc atatcattat tttaaccata",
    "      241 aagcaggtac ataatgttta tattattcac catatcaagt gagagaccac caataattta",
    "//"
  ), fn)
  out <- maptoref_parse_features(fn)
  expect_equal(out$gene, c("EARLY", "LATE"))
  expect_equal(rownames(out), as.character(seq_len(nrow(out))))
})
