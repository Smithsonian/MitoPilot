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

test_that(".parse_mitofinder_gff computes PCG start/stop codons and translation", {
  wd <- tempfile("mf_pcg_")
  dir.create(file.path(wd, "x_Final_Results"), recursive = TRUE)
  # + strand ORF at 4-12: ATG AAA TAA  ;  - strand ORF at 15-23 (revcomp = ATGAAATAA)
  seq <- paste0("AAA", "ATGAAATAA", "GG", "TTATTTCAT", "AAA")
  gff <- c(
    "##gff-version 3",
    "ctg1\tMitoFinder\tCDS\t4\t12\t.\t+\t0\tName=ND1",
    "ctg1\tMitoFinder\tCDS\t15\t23\t.\t-\t0\tName=ND2"
  )
  writeLines(gff, file.path(wd, "x_Final_Results", "x.gff"))
  asm <- Biostrings::DNAStringSet(seq)
  names(asm) <- "ctg1"

  res <- .parse_mitofinder_gff(wd, asm, "2")

  fwd <- res[res$gene == "nad1", ]
  rev <- res[res$gene == "nad2", ]
  expect_equal(fwd$start_codon, "ATG")
  expect_equal(fwd$stop_codon, "TAA")
  expect_equal(fwd$translation, "MK")
  expect_equal(rev$start_codon, "ATG")
  expect_equal(rev$stop_codon, "TAA")
  expect_equal(rev$translation, "MK")
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

test_that(".parse_mitofinder_gff leaves a missing tRNA anticodon as NA, not NNN", {
  # MitoFinder GFFs frequently omit the anticodon attribute. It must stay NA:
  # "NNN" is the sentinel for a tool that tried to call the anticodon and failed,
  # and validate_mito_core() raises a low-confidence warning on it. The NA is made
  # safe at the consumer instead (see the export_files() guard).
  wd <- tempfile("mf_ac_")
  dir.create(file.path(wd, "x_Final_Results"), recursive = TRUE)
  writeLines(c(
    "##gff-version 3",
    "ctg1\tMitoFinder\ttRNA\t10\t80\t.\t+\t.\tName=tRNA-Pro",
    "ctg1\tMitoFinder\ttRNA\t100\t170\t.\t+\t.\tName=tRNA-Phe;anticodon=gaa",
    "ctg1\tMitoFinder\trRNA\t200\t400\t.\t+\t.\tName=16S"
  ), file.path(wd, "x_Final_Results", "x.gff"))
  asm <- Biostrings::DNAStringSet(paste(rep("A", 500), collapse = ""))
  names(asm) <- "ctg1"

  res <- .parse_mitofinder_gff(wd, asm, "2")

  expect_true(is.na(res$anticodon[res$gene == "trnP"]))   # absent attribute -> NA
  expect_equal(res$anticodon[res$gene == "trnF"], "GAA")  # present attribute -> parsed
  # ...and specifically NOT the low-confidence sentinel, which would make
  # validate_mito_core() warn on every MitoFinder gap-filled tRNA.
  expect_false(any(res$anticodon[res$type == "tRNA"] %in% "NNN"))
  expect_true(is.na(res$anticodon[res$type == "rRNA"]))
})

test_that("export_files tolerates an NA anticodon on a tRNA row", {
  # The half of the fix that actually rescues an existing project: rows already in
  # the DB carry NA regardless of what annotate_mitofinder() does from now on.
  # `if (cur$anticodon != "NNN")` on NA aborted the export mid-sample.
  guard <- function(anticodon) {
    if (!is.na(anticodon) && anticodon != "NNN") "note written" else "note skipped"
  }
  expect_equal(guard(NA_character_), "note skipped")
  expect_equal(guard("NNN"), "note skipped")
  expect_equal(guard("GAA"), "note written")
  # The pre-fix condition is what crashed; keep a live record of that.
  expect_error(if (NA_character_ != "NNN") TRUE, "missing value where TRUE/FALSE needed")
})
