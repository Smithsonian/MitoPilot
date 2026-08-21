# Curation of features that span the origin of a circular assembly.
#
# A feature crossing the origin is stored pos1 > pos2 (e.g. 850..150 on a 900 bp
# contig). Before the fix, every boundary adjustment in curate_mito_core() fed
# those coordinates straight to Biostrings::subseq(), which aborts with
# "the supplied start/end lead to a negative width" and killed the whole sample.

# 900 bp of AAC (Asn in every genetic code, never a start or stop codon) so any
# start/stop codon in a fixture is one the test deliberately planted.
base_seq <- function(len = 900L) paste(rep("AAC", len / 3L), collapse = "")

set_codon <- function(s, at, codon) {
  substr(s, at, at + 2L) <- codon
  s
}

# annotations csv + assembly fasta + a stub featureProt db, as curate expects
write_fixture <- function(seq_chr, ann, circular = TRUE) {
  d <- withr::local_tempdir(.local_envir = parent.frame())
  dir.create(file.path(d, "featureProt"), recursive = TRUE)
  writeLines(">ref\nMKKK", file.path(d, "featureProt", paste0(ann$gene[1], ".fas")))
  asm <- Biostrings::DNAStringSet(seq_chr)
  names(asm) <- if (circular) "ctg1 topology=circular" else "ctg1 topology=linear"
  Biostrings::writeXStringSet(asm, file.path(d, "assembly.fasta"))
  write.csv(ann, file.path(d, "annotations.csv"), row.names = FALSE)
  dir.create(file.path(d, "out"))
  d
}

ann_row <- function(pos1, pos2, direction = "+", gene = "cox1", type = "PCG",
                    start_codon = "ATG", stop_codon = "TAA") {
  data.frame(
    contig = "ctg1", type = type, gene = gene, product = gene,
    pos1 = as.integer(pos1), pos2 = as.integer(pos2),
    length = 0L, direction = direction,
    start_codon = start_codon, stop_codon = stop_codon,
    translation = strrep("M", 60L),
    stringsAsFactors = FALSE
  )
}

hits <- function(gap_leading = 0L, gap_trailing = 0L, n = 3L) {
  data.frame(
    acc = paste0("A", seq_len(n)), Taxon = "t", eval = 0, target = "cox1",
    pctid = 90, similarity = 90,
    gap_leading = as.integer(gap_leading), gap_trailing = as.integer(gap_trailing),
    stringsAsFactors = FALSE
  )
}

run_curate <- function(d, hits_fn, feature_trim = FALSE) {
  testthat::with_mocked_bindings(
    .package = "MitoPilot",
    get_top_hits = hits_fn,
    code = curate_ctenophore_mito(
      annotations_fn = file.path(d, "annotations.csv"),
      assembly_fn = file.path(d, "assembly.fasta"),
      out_dir = file.path(d, "out"),
      params = params_ctenophore_mito(),
      ref_dir = d,
      genetic_code = 4,
      feature_trim = feature_trim
    )
  ) |> as.data.frame()
}

test_that("curation of an origin-spanning PCG on the plus strand does not crash", {
  # cox1 850..150 on a circular 900 bp contig: 51 bp before the origin + 150 after.
  s <- base_seq() |>
    set_codon(850L, "ATG") |> # current start
    set_codon(844L, "ATG") |> # start 2 codons upstream, what curation should find
    set_codon(148L, "TAA") # stop, in frame from 850
  d <- write_fixture(s, ann_row(850, 150))

  res <- run_curate(d, function(...) hits(gap_leading = 2L))

  expect_equal(res$pos1, 844L)
  expect_equal(res$pos2, 150L)
  # circular length, not the 900 - 207 = 693 bp complementary arc
  expect_equal(res$length, 207L)
  expect_match(res$notes, "extending start 6 bp")
})

test_that("curation of an origin-spanning PCG on the minus strand does not crash", {
  # revcomp("ATG") == "CAT": the minus-strand start codon 2 codons "upstream"
  # of pos2 sits at 154..156.
  s <- base_seq() |>
    set_codon(148L, "CAT") |>
    set_codon(154L, "CAT") |>
    set_codon(850L, "TTA") # revcomp("TTA") == "TAA", the minus-strand stop
  d <- write_fixture(s, ann_row(850, 150, direction = "-"))

  res <- run_curate(d, function(...) hits(gap_leading = 2L))

  expect_equal(res$pos1, 850L)
  expect_equal(res$pos2, 156L)
  expect_equal(res$length, 207L)
})

test_that("start extension still stops at the 5' end of a linear contig", {
  # cox1 at 4..300 on a LINEAR contig. Extending 2 codons would land at -2, which
  # must stay refused; extending 1 codon lands at 1, where there is no start codon.
  s <- base_seq() |>
    set_codon(4L, "ATG") |>
    set_codon(298L, "TAA")
  d <- write_fixture(s, ann_row(4, 300), circular = FALSE)

  res <- run_curate(d, function(...) hits(gap_leading = 2L))

  expect_equal(res$pos1, 4L)
  expect_equal(res$pos2, 300L)
  expect_true(is.na(res$notes) || !grepl("extending start", res$notes))
})

test_that("start extension crosses the origin on a circular contig", {
  # Same gene at 4..300, now circular: extending 2 codons wraps to 898 and the
  # feature becomes origin-spanning.
  s <- base_seq() |>
    set_codon(4L, "ATG") |>
    set_codon(898L, "ATG") |>
    set_codon(298L, "TAA")
  d <- write_fixture(s, ann_row(4, 300))

  res <- run_curate(d, function(...) hits(gap_leading = 2L))

  expect_equal(res$pos1, 898L)
  expect_equal(res$pos2, 300L)
  expect_equal(res$length, 303L)
})

test_that("start trimming crosses the origin on a circular contig", {
  # cox1 898..300 is over-extended by 2 codons; the corrected start at 4 is past
  # the origin, so trimming must remove the wrap rather than produce pos1 = 904.
  s <- base_seq() |>
    set_codon(898L, "ATG") |>
    set_codon(4L, "ATG") |>
    set_codon(298L, "TAA")
  d <- write_fixture(s, ann_row(898, 300))

  # over-extended on the first look, correct once the start has moved
  calls <- 0L
  hits_fn <- function(...) {
    calls <<- calls + 1L
    if (calls == 1L) hits(gap_leading = -2L) else hits()
  }
  res <- run_curate(d, hits_fn)

  expect_equal(res$pos1, 4L)
  expect_equal(res$pos2, 300L)
  expect_equal(res$length, 297L)
})

test_that("the rRNA punctuation model gives an origin-spanning rRNA a circular length", {
  # rrnS 810..200 wraps the origin and sits 11 bp downstream of trnF, so the
  # punctuation model pulls its start back to 800. The resulting length is the
  # 301 bp wrap arc, not the 601 bp complementary arc.
  s <- base_seq()
  ann <- rbind(
    ann_row(750, 799, gene = "trnF", type = "tRNA",
            start_codon = NA, stop_codon = NA),
    ann_row(810, 200, gene = "rrnS", type = "rRNA",
            start_codon = NA, stop_codon = NA)
  )
  ann$translation <- NA_character_
  d <- write_fixture(s, ann)

  res <- run_curate(d, function(...) hits())
  rrn <- res[res$gene == "rrnS", ]

  expect_equal(rrn$pos1, 800L)
  expect_equal(rrn$length, 301L)
})

test_that("the punctuation model uses a tRNA that sits across the origin", {
  # trnP 880..40 wraps, so it sorts LAST by pos1 while circularly sitting just
  # before rrnS 45..500. The 4 bp gap must still be closed.
  s <- base_seq()
  ann <- rbind(
    ann_row(45, 500, gene = "rrnS", type = "rRNA", start_codon = NA, stop_codon = NA),
    ann_row(880, 40, gene = "trnP", type = "tRNA", start_codon = NA, stop_codon = NA)
  )
  ann$translation <- NA_character_
  d <- write_fixture(s, ann)

  res <- run_curate(d, function(...) hits())
  rrn <- res[res$gene == "rrnS", ]

  expect_equal(rrn$pos1, 41L)
  expect_equal(rrn$length, 460L)
})

test_that("the punctuation model leaves a linear contig's edge rows alone", {
  s <- base_seq()
  ann <- rbind(
    ann_row(45, 500, gene = "rrnS", type = "rRNA", start_codon = NA, stop_codon = NA),
    ann_row(880, 40, gene = "trnP", type = "tRNA", start_codon = NA, stop_codon = NA)
  )
  ann$translation <- NA_character_
  d <- write_fixture(s, ann, circular = FALSE)

  res <- run_curate(d, function(...) hits())
  expect_equal(res$pos1[res$gene == "rrnS"], 45L)
})

test_that("two PCGs spanning the origin are both curated", {
  # cox1 850..150 (+) and nad1 880..60 (-) both cross the origin
  s <- base_seq() |>
    set_codon(850L, "ATG") |> set_codon(844L, "ATG") |> set_codon(148L, "TAA") |>
    set_codon(58L, "CAT") |> set_codon(64L, "CAT") |> set_codon(880L, "TTA")
  ann <- rbind(ann_row(850, 150), ann_row(880, 60, direction = "-", gene = "nad1"))
  d <- write_fixture(s, ann)
  writeLines(">ref\nMKKK", file.path(d, "featureProt", "nad1.fas"))

  res <- run_curate(d, function(...) hits(gap_leading = 2L))

  expect_equal(nrow(res), 2L)
  expect_equal(res$length[res$gene == "cox1"], 207L)
  expect_true(all(res$pos1 >= 1L & res$pos1 <= 900L))
  expect_true(all(res$pos2 >= 1L & res$pos2 <= 900L))
})
