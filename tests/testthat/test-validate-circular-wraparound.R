# Validation of features that span the origin of a circular assembly.
#
# An origin-spanning feature is stored pos1 > pos2. seq(9800, 100) counts DOWN
# through the middle of the contig, so the old overlap and containment tests saw
# the complementary arc: real neighbours were invisible and distant genes looked
# like they overlapped.

L <- 10000L

vrow <- function(gene, type, pos1, pos2, direction = "+", length = NULL) {
  data.frame(
    contig = "ctg1", type = type, gene = gene, product = gene,
    pos1 = as.integer(pos1), pos2 = as.integer(pos2),
    length = as.integer(length %||% (abs(pos2 - pos1) + 1L)),
    direction = direction,
    start_codon = "ATG", stop_codon = "TAA", anticodon = NA_character_,
    translation = strrep("M", 60L), refHits = "{}",
    stringsAsFactors = FALSE
  )
}

run_validate <- function(ann) {
  d <- withr::local_tempdir(.local_envir = parent.frame())
  ann_fn <- file.path(d, "x_annotations_1.csv")
  write.csv(ann, ann_fn, row.names = FALSE)
  cov_fn <- file.path(d, "cov.csv")
  write.csv(
    data.frame(
      SeqId = "ctg1", Position = seq_len(L), Call = "A",
      MeanDepth = 100, GC = 0.4, ErrorRate = 0
    ),
    cov_fn, row.names = FALSE
  )
  res <- validate_ctenophore_mito(
    annotations_fn = ann_fn, coverage_fn = cov_fn,
    params = params_ctenophore_mito(), out_dir = d
  )
  res$annotations
}

test_that("an origin-spanning rRNA is measured around the circle, not across it", {
  # rrnS 9800..100 is 301 bp; the stored length is the 9701 bp value the old
  # abs(pos2 - pos1) + 1 arithmetic produced, well over the 1000 bp rrnS limit.
  ann <- vrow("rrnS", "rRNA", 9800, 100, length = 9701L)
  res <- run_validate(ann)
  expect_false(isTRUE(grepl("exceeds max length", res$warnings[1])))
})

test_that("a tRNA inside the post-origin arm of a wrapping PCG is detected", {
  ann <- rbind(
    vrow("nad1", "PCG", 9800, 100),
    vrow("trnF", "tRNA", 1, 67)
  )
  res <- run_validate(ann)
  expect_match(res$warnings[res$gene == "trnF"], "tRNA within PCG or rRNA")
})

test_that("a distant gene is not reported as overlapping a wrapping feature", {
  # nad2 5000..6800 sits in the middle of the contig, i.e. inside the arc the
  # old descending seq() produced but nowhere near the real 9800..100 arms.
  ann <- rbind(
    vrow("nad1", "PCG", 9800, 100),
    vrow("nad2", "PCG", 5000, 6800)
  )
  res <- run_validate(ann)
  expect_false(any(grepl("max overlap", res$warnings, useBytes = TRUE), na.rm = TRUE))
})

test_that("a genuine overlap across the origin is still reported", {
  # trnP 9700..9850 overlaps nad1's pre-origin arm by 51 bp; nad1 is 301 bp, so
  # that is 17% - under the 25% max_overlap - but trnP itself is 151 bp, so from
  # trnP's side it is 34% and must warn.
  ann <- rbind(
    vrow("nad1", "PCG", 9800, 100),
    vrow("trnP", "tRNA", 9700, 9850)
  )
  res <- run_validate(ann)
  expect_match(res$warnings[res$gene == "trnP"], "exceeds max overlap")
})

test_that("a contig with no PCG or rRNA does not abort validation", {
  # The tRNA containment check filters to PCG/rRNA rows first. On an empty
  # result a rowwise filter still evaluates its predicate, and the scalar
  # circ_* helpers abort on zero-length input.
  ann <- rbind(vrow("trnF", "tRNA", 100, 167), vrow("trnP", "tRNA", 300, 367))
  expect_no_error(run_validate(ann))
})

test_that("a single wrapping annotation is validated on its own", {
  expect_no_error(run_validate(vrow("trnF", "tRNA", 9950, 20)))
})

test_that("two wrapping PCGs that overlap each other are both flagged", {
  # nad1 9700..50 (351 bp) and nad2 9900..120 (221 bp) share 151 bp
  ann <- rbind(vrow("nad1", "PCG", 9700, 50), vrow("nad2", "PCG", 9900, 120))
  res <- run_validate(ann)
  expect_true(all(grepl("exceeds max overlap", res$warnings)))
})

test_that("a wrapping tRNA inside a wrapping PCG is caught", {
  ann <- rbind(vrow("nad1", "PCG", 9500, 500), vrow("trnF", "tRNA", 9950, 20))
  res <- run_validate(ann)
  expect_match(res$warnings[res$gene == "trnF"], "tRNA within PCG or rRNA")
})

test_that("a wrapping tRNA sticking out of a wrapping PCG is not called contained", {
  ann <- rbind(vrow("nad1", "PCG", 9960, 10), vrow("trnF", "tRNA", 9950, 20))
  res <- run_validate(ann)
  expect_false(any(grepl("tRNA within", res$warnings), na.rm = TRUE))
})

test_that("a wrapping rRNA is measured around the circle, not across it", {
  # 1001 bp is over the 1000 bp rrnS limit only by the true arc, and the bogus
  # complementary arc (9001) would trip it for the wrong reason
  ann <- rbind(vrow("rrnS", "rRNA", 9500, 400, length = 9101L),
               vrow("trnF", "tRNA", 9990, 40, length = 9951L))
  res <- run_validate(ann)
  expect_false(isTRUE(grepl("exceeds max length", res$warnings[res$gene == "trnF"])))
})
