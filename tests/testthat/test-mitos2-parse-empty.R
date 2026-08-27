# A contig MITOS2 finds no genes on gets an EMPTY result.fas. That reaches the
# parser whenever a non-mitochondrial contig becomes an annotation unit, which
# per-contig processing made routine. The parser used to die there: first
# indexing a column of a zero-row split, then evaluating a rowwise `if` on a
# zero-length value.

mitos_fixture <- function(name) testthat::test_path("fixtures", name)

fake_assembly <- function(len, nm) {
  Biostrings::DNAStringSet(stats::setNames(strrep("ACGT", ceiling(len / 4)) |>
                                             substr(1, len), nm))
}

parse_fixture <- function(name, len, nm) {
  asm <- fake_assembly(len, nm)
  parse_mitos_dir(
    mitos_fixture(name), asm,
    stats::setNames(Biostrings::width(asm), names(asm)), "2"
  )
}

test_that("an empty MITOS2 result parses to zero rows, not an error", {
  out <- parse_fixture("mitos2_empty", 5000, "nuc")
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 0L)
})

test_that("the empty result keeps the full column shape", {
  empty <- parse_fixture("mitos2_empty", 5000, "nuc")
  real  <- parse_fixture("mitos2_genes", 16602, "circ_dup1200.1.1")
  # the rescue path filters the parse result on `type` and `gene`, so an empty
  # parse missing columns would error there instead
  expect_setequal(names(empty), names(real))
  expect_no_error(dplyr::filter(empty, type %in% c("PCG", "rRNA") & !(gene %in% "cox1")))
})

test_that("a real MITOS2 result still parses", {
  out <- parse_fixture("mitos2_genes", 16602, "circ_dup1200.1.1")
  expect_gt(nrow(out), 30L)
  expect_true(all(c("PCG", "tRNA", "rRNA") %in% out$type))
  expect_true(all(out$pos1 > 0, na.rm = TRUE))
  # PCGs carry a translation, tRNAs carry an anticodon
  expect_true(any(!is.na(out$translation[out$type == "PCG"])))
  expect_true(any(!is.na(out$anticodon[out$type == "tRNA"])))
})
