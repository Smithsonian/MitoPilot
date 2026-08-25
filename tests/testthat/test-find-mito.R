hit <- function(qseqid, saccver, pident, length, bitscore, qlen) {
  data.frame(
    qseqid = qseqid, saccver = saccver, pident = pident,
    length = length, bitscore = bitscore, qlen = qlen
  )
}

test_that("a single strong full-length contig is selected", {
  res <- select_mito_contigs(hit("ctg1", "NC_001", 98, 16400, 30000, 16500))
  expect_equal(res$accession, "NC_001")
  expect_equal(res$candidates, "ctg1")
  expect_equal(res$evidence$selected, 1L)
  expect_true(is.na(res$evidence$reason))
})

test_that("a mitogenome split across contigs keeps every piece", {
  hits <- rbind(
    hit("ctg1", "NC_001", 97, 8000, 15000, 8100),
    hit("ctg2", "NC_001", 96, 5000, 9000, 5100),
    hit("ctg3", "NC_001", 95, 3000, 5000, 3100)
  )
  res <- select_mito_contigs(hits)
  expect_equal(res$candidates, c("ctg1", "ctg2", "ctg3"))
  expect_equal(sum(res$evidence$selected), 3L)
})

test_that("hits split over several pieces of one contig are pooled", {
  # Two HSPs of 4000 bp each cover 8000 of an 8100 bp contig; neither alone
  # would clear the fraction rule.
  hits <- rbind(
    hit("ctg1", "NC_001", 97, 4000, 7000, 8100),
    hit("ctg1", "NC_001", 96, 4000, 7000, 8100)
  )
  res <- select_mito_contigs(hits)
  expect_equal(res$candidates, "ctg1")
  expect_equal(res$evidence$aligned_length, 8000L)
})

test_that("a nuclear scaffold carrying a NUMT is rejected by the fraction rule", {
  hits <- rbind(
    hit("mito", "NC_001", 98, 16400, 30000, 16500),
    hit("scaffold_7", "NC_001", 92, 5000, 8000, 2000000)
  )
  res <- select_mito_contigs(hits)
  expect_equal(res$candidates, "mito")
  numt <- res$evidence[res$evidence$contig == "scaffold_7", ]
  expect_equal(numt$selected, 0L)
  expect_match(numt$reason, "possible NUMT")
})

test_that("contigs matching a different reference are dropped", {
  hits <- rbind(
    hit("ctg1", "NC_001", 98, 16400, 30000, 16500),
    hit("ctg9", "NC_999", 99, 900, 1500, 1000)
  )
  res <- select_mito_contigs(hits)
  expect_equal(res$candidates, "ctg1")
  expect_false("ctg9" %in% res$evidence$contig)
})

test_that("low identity and short alignments are rejected", {
  hits <- rbind(
    hit("ctg1", "NC_001", 98, 16400, 30000, 16500),
    hit("weak", "NC_001", 55, 4000, 3000, 5000),
    hit("short", "NC_001", 99, 200, 400, 300)
  )
  res <- select_mito_contigs(hits)
  expect_equal(res$candidates, "ctg1")
  expect_match(res$evidence$reason[res$evidence$contig == "weak"], "identity")
  expect_match(res$evidence$reason[res$evidence$contig == "short"], "aligned 200 bp")
})

test_that("the candidate cap keeps the best and records the rest", {
  hits <- do.call(rbind, lapply(1:5, function(i) {
    hit(paste0("ctg", i), "NC_001", 95, 6000 - i * 100, 9000 - i, 6100)
  }))
  res <- select_mito_contigs(hits, max_candidates = 2)
  expect_equal(res$candidates, c("ctg1", "ctg2"))
  dropped <- res$evidence[res$evidence$contig %in% c("ctg3", "ctg4", "ctg5"), ]
  expect_true(all(dropped$selected == 0L))
  expect_true(all(grepl("2-candidate cap", dropped$reason)))
})

test_that("an empty hits table yields no candidates", {
  res <- select_mito_contigs(NULL)
  expect_true(is.na(res$accession))
  expect_length(res$candidates, 0L)
  expect_equal(nrow(res$evidence), 0L)
})

test_that("confirm_mito_contigs applies the gene floor and ranks by gene count", {
  res <- confirm_mito_contigs(
    candidates = c("ctg1", "ctg2", "ctg3"),
    gene_counts = c(ctg1 = 5L, ctg2 = 13L),
    min_genes = 3
  )
  expect_equal(res$confirmed, c("ctg2", "ctg1"))
  expect_equal(unname(res$genes[["ctg3"]]), 0L)
})

test_that("notes describe each outcome", {
  ev_ok <- data.frame(selected = 1L, aligned_length = 16000L, contig = "ctg1",
                      reason = NA_character_)
  expect_match(
    find_mito_note(1200, ev_ok, "ctg1", "NC_001"),
    "found 1 mitochondrial contig of 1200 screened"
  )
  expect_match(
    find_mito_note(1200, ev_ok, character(0), "NC_001"),
    "carried too few mitochondrial genes"
  )
  ev_none <- data.frame(selected = 0L, aligned_length = 5000L, contig = "scaffold_7",
                        reason = "hit covers 0.2% of the contig, below 50% (possible NUMT)")
  expect_match(
    find_mito_note(1200, ev_none, character(0), "NC_001"),
    "no candidate contig of 1200 screened; best was scaffold_7"
  )
  expect_match(
    find_mito_note(1200, ev_none[0, ], character(0), NA_character_),
    "no BLAST hits among 1200 screened contigs"
  )
})

# MitoFinder renames contigs internally, so these fixtures mirror the two real
# output layouts: a single-contig run (.infos named after the job) and a
# multi-contig run (.infos named after each renamed contig).
write_mf_single <- function(dir, contig = "mito_ctg", n_genes = 15) {
  res <- file.path(dir, "job_MitoFinder_mitfi_Final_Results")
  dir.create(res, recursive = TRUE, showWarnings = FALSE)
  writeLines(c("Statistics for final sequence:", "",
               paste("Initial contig name:", contig), "Length: 16596"),
             file.path(res, "job.infos"))
  writeLines(paste0(">job@GENE", seq_len(n_genes), "\nACGT"),
             file.path(res, "job_mtDNA_contig_genes_NT.fasta"))
  writeLines(paste0(">job@GENE", seq_len(n_genes), "\nACGT"),
             file.path(res, "job_final_genes_NT.fasta"))
  dir
}

write_mf_multi <- function(dir, contigs = c("frag_a", "frag_b"), genes = c(4, 8)) {
  res <- file.path(dir, "job_MitoFinder_mitfi_Final_Results")
  dir.create(res, recursive = TRUE, showWarnings = FALSE)
  for (i in seq_along(contigs)) {
    writeLines(c(paste0("Statistics for contig ", i), "",
                 paste("Initial contig name:", contigs[i])),
               file.path(res, paste0("job_mtDNA_contig_", i, ".infos")))
    writeLines(paste0(">job@GENE", seq_len(genes[i]), "\nACGT"),
               file.path(res, paste0("job_mtDNA_contig_", i, "_genes_NT.fasta")))
  }
  # Aggregate file across contigs; counting it would double every contig.
  writeLines(paste0(">job@GENE", seq_len(sum(genes)), "\nACGT"),
             file.path(res, "job_final_genes_NT.fasta"))
  dir
}

test_that("gene counts map back to the original contig name (single contig)", {
  d <- withr::local_tempdir()
  write_mf_single(d)
  expect_equal(count_mitofinder_genes(d), c(mito_ctg = 15L))
})

test_that("gene counts map back per contig (multiple contigs)", {
  d <- withr::local_tempdir()
  write_mf_multi(d)
  counts <- count_mitofinder_genes(d)
  expect_equal(counts[["frag_a"]], 4L)
  expect_equal(counts[["frag_b"]], 8L)
  expect_length(counts, 2L)
})

test_that("no MitoFinder output yields no counts", {
  expect_length(count_mitofinder_genes(withr::local_tempdir()), 0L)
})

test_that("gene counting works when workdir is a relative path", {
  # Regression: the count runs while the working directory IS workdir, so a
  # relative path used to resolve against itself and silently return no genes.
  skip_on_os("windows")
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)
  writeLines(c(">mito_ctg", "ACGT"), "candidates.fasta")
  writeLines("fake genbank", "ref.gb")

  # Stub standing in for MitoFinder: writes its output layout into the working
  # directory it is invoked from, exactly as the real tool does.
  bin <- file.path(tmp, "bin")
  dir.create(bin)
  writeLines(
    c("#!/bin/sh",
      "mkdir -p job_MitoFinder_mitfi_Final_Results",
      "printf 'Initial contig name: mito_ctg\\n' > job_MitoFinder_mitfi_Final_Results/job.infos",
      "for i in 1 2 3; do printf '>job@GENE%s\\nACGT\\n' \"$i\"; done > job_MitoFinder_mitfi_Final_Results/job_mtDNA_contig_genes_NT.fasta"),
    file.path(bin, "mitofinder")
  )
  Sys.chmod(file.path(bin, "mitofinder"), "0755")
  withr::local_envvar(c(PATH = paste(bin, Sys.getenv("PATH"), sep = ":")))

  counts <- mitofinder_gene_counts(
    "candidates.fasta",
    mitofinder_db = "ref.gb",
    workdir = "out/mitofinder"
  )
  expect_equal(counts, c(mito_ctg = 3L))
})

test_that("a missing MitoFinder database is a clear error", {
  expect_error(
    mitofinder_gene_counts("x.fasta", mitofinder_db = "no/such/file.gb"),
    "custom_assembly_db"
  )
})
