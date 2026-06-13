test_that("normalize_mitofinder_gene maps reference names to MitoPilot convention", {
  expect_equal(
    normalize_mitofinder_gene(c("COX1", "ND4L", "16S", "12S", "tRNA-Phe",
                                "trnL", "CYTB", "ATP6", "tRNA-Leu", "trnS2")),
    c("cox1", "nad4l", "rrnL", "rrnS", "trnF",
      "trnL", "cob", "atp6", "trnL", "trnS")
  )
})

test_that("normalize_mitofinder_gene strips MitoFinder descriptors and ref suffixes", {
  expect_equal(
    normalize_mitofinder_gene(c(
      "CYTB CDS 3' Partial CDS",   # trailing partial-CDS descriptor
      "COX1 CDS 5' Partial CDS",
      "ND5 CDS",
      "COX1_Danio_rerio",          # reference 'gene' qualifier split on "_"
      "16S ribosomal RNA",
      "tRNA-Ser CDS"
    )),
    c("cob", "cox1", "nad5", "cox1", "rrnL", "trnS")
  )
})

test_that("normalize_mitofinder_gene returns NA for unmappable names", {
  expect_true(all(is.na(
    normalize_mitofinder_gene(c("ORF42", "hypothetical protein", "", NA))
  )))
})

test_that(".parse_mitofinder_gff returns one row per exon and normalized fields", {
  wd <- tempfile("mf_test_")
  dir.create(file.path(wd, "x_Final_Results"), recursive = TRUE)
  gff <- c(
    "##gff-version 3",
    "ctg1\tMitoFinder\tgene\t10\t500\t.\t+\t.\tID=g_ND1;Name=ND1",
    "ctg1\tMitoFinder\tCDS\t10\t200\t.\t+\t0\tID=c1;Name=ND1",
    "ctg1\tMitoFinder\tCDS\t300\t500\t.\t+\t1\tID=c2;Name=ND1",
    "ctg1\tMitoFinder\ttRNA\t600\t670\t.\t-\t.\tName=tRNA-Phe;anticodon=gaa",
    "ctg1\tMitoFinder\trRNA\t700\t1200\t.\t+\t.\tName=16S"
  )
  writeLines(gff, file.path(wd, "x_Final_Results", "x.gff"))
  asm <- Biostrings::DNAStringSet(paste(rep("A", 1300), collapse = ""))
  names(asm) <- "ctg1"

  res <- .parse_mitofinder_gff(wd, asm, "2")

  expect_equal(nrow(res), 4L)
  expect_equal(sum(res$gene == "nad1"), 2L) # intron gene -> one row per exon
  expect_equal(res$type[res$gene == "trnF"], "tRNA")
  expect_equal(res$direction[res$gene == "trnF"], "-")
  expect_equal(res$product[res$gene == "trnF"], "tRNA-Phe")
  expect_equal(res$anticodon[res$gene == "trnF"], "GAA")
  expect_equal(res$gene[res$type == "rRNA"], "rrnL")
})

test_that("annotate_mitofinder returns typed empty frame when db missing", {
  asm <- Biostrings::DNAStringSet("ACGT")
  names(asm) <- "ctg1"
  expect_warning(
    res <- annotate_mitofinder(assembly = asm, mitofinder_db = "/no/such/file.gb"),
    "reference database not found"
  )
  expect_equal(nrow(res), 0L)
  expect_true(all(
    c("contig", "type", "gene", "product", "pos1", "pos2",
      "length", "direction", "tRNA_ID", "anticodon") %in% names(res)
  ))
})
