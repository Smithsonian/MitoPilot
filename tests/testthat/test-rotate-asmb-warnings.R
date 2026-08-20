# rotate_asmb() used to no-op in silence when the requested start gene was
# absent, which is how a PCG spanning the origin survived into curation.

asm <- Biostrings::DNAStringSet(strrep("ACGT", 250L))  # 1000 bp
names(asm) <- "ctg1 circular"

ann <- data.frame(
  contig = "ctg1", gene = c("trnP", "nad1", "cox1"),
  pos1 = c(10L, 100L, 400L), pos2 = c(80L, 300L, 900L),
  direction = "+", stringsAsFactors = FALSE
)

test_that("a missing start gene warns instead of silently skipping rotation", {
  expect_warning(
    res <- rotate_asmb(asm, ann, start_gene = "trnF"),
    "not annotated"
  )
  expect_equal(as.character(res[[1]]), as.character(asm))
  expect_equal(res[[2]], ann)
})

test_that("rotating through the middle of a feature warns", {
  # cox1 400..900 straddles the new origin at 500
  ann2 <- rbind(ann, data.frame(
    contig = "ctg1", gene = "trnW", pos1 = 500L, pos2 = 570L, direction = "+"
  ))
  expect_warning(rotate_asmb(asm, ann2, start_gene = "trnW"), "cuts through")
})

test_that("rotating to a gene in a clear intergenic gap is quiet", {
  expect_silent(rotate_asmb(asm, ann, start_gene = "nad1"))
})
