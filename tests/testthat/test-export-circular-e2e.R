# End-to-end export of a circular assembly whose features span the origin.
#
# Several feature types wrap at once, which is what a real rotation through a
# gene-dense region produces. The .tbl needs two intervals per wrapping feature
# and the GFF needs start..(L + end); anything else submits the wrong gene.

L <- 3000L
withr::with_seed(3, {
  export_seq <- paste(sample(c("A", "C", "G", "T"), L, replace = TRUE), collapse = "")
})

erow <- function(gene, type, pos1, pos2, direction = "+", start_codon = "ATG",
                 stop_codon = "TAA", notes = NA_character_) {
  data.frame(
    ID = "s1", path = 1L, scaffold = 1L, contig = "s1.1.1", type = type,
    gene = gene, product = gene, pos1 = as.integer(pos1), pos2 = as.integer(pos2),
    length = as.integer(circ_len(pos1, pos2, L)), direction = direction,
    start_codon = start_codon, stop_codon = stop_codon,
    translation = strrep("M", 30L), anticodon = "TTC",
    partial_start = 0L, partial_stop = 0L, notes = notes, refHits = "{}",
    warnings = NA_character_, stringsAsFactors = FALSE
  )
}

# minimal project database, then export it
run_export <- function(annotations, topology = "circular") {
  d <- withr::local_tempdir(.local_envir = parent.frame())
  out_dir <- file.path(d, "out")
  dir.create(out_dir)
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(d, ".sqlite"))
  DBI::dbWriteTable(con, "annotations", annotations)
  DBI::dbWriteTable(con, "assemblies", data.frame(
    ID = "s1", path = 1L, scaffold = 1L, ignore = 0L, topology = topology,
    sequence = export_seq, stringsAsFactors = FALSE))
  DBI::dbWriteTable(con, "export", data.frame(
    ID = "s1", path = 1L, scaffold = 1L, export_group = "g1",
    export_time_stamp = NA_integer_, stringsAsFactors = FALSE))
  DBI::dbWriteTable(con, "annotate", data.frame(
    ID = "s1", path = 1L, scaffold = 1L, topology = topology, partial = "no",
    curate_opts = "default", stringsAsFactors = FALSE))
  DBI::dbWriteTable(con, "curate_opts", data.frame(
    curate_opts = "default", params = "{}", linear_complete = 0L,
    stringsAsFactors = FALSE))
  DBI::dbWriteTable(con, "samples", data.frame(
    ID = "s1", Taxon = "Testus testus", genetic_code = 2L, stringsAsFactors = FALSE))
  DBI::dbWriteTable(con, "assemble", data.frame(
    ID = "s1", blast_accession = "NC_000001", blast_accession_auto = 0L,
    poor_blast_ref = "ok", stringsAsFactors = FALSE))
  DBI::dbDisconnect(con)

  suppressMessages(suppressWarnings(export_files(
    group = "g1", out_dir = out_dir, generateAAalignments = FALSE,
    gene_export = FALSE, review = FALSE, summary_csv = FALSE
  )))
  list(
    tbl = readLines(file.path(out_dir, "s1", "export", "s1.tbl")),
    gff = readLines(file.path(out_dir, "s1", "export", "s1.gff"))
  )
}

test_that("every wrapping feature type exports as two intervals", {
  res <- run_export(rbind(
    erow("cox1", "PCG", 2900, 200),
    erow("nad5", "PCG", 2800, 100, direction = "-"),
    erow("trnF", "tRNA", 2950, 40, start_codon = NA, stop_codon = NA),
    erow("rrnS", "rRNA", 2960, 150, start_codon = NA, stop_codon = NA),
    erow("ctrl", "ctrl", 2700, 60, start_codon = NA, stop_codon = NA),
    erow("ORF.1", "ORF", 2500, 40, start_codon = NA, stop_codon = NA)
  ))

  # plus strand: pos1..L then 1..pos2
  expect_true(any(res$tbl == "2950\t3000\ttRNA"))
  expect_true(any(res$tbl == "2960\t3000\trRNA"))
  expect_true(any(res$tbl == "2700\t3000\tD-loop"))
  expect_true(any(grepl("2500\t3000\tCDS$", res$tbl)))
  # minus strand reads the same arc backwards
  expect_true(any(res$tbl == "3000\t2800"))
  # GFF runs past the end of the sequence instead
  expect_true(any(grepl("\ttRNA\t2950\t3040\t", res$gff)))
  expect_true(any(grepl("\tCDS\t2800\t3100\t.\t-\t", res$gff)))
})

test_that("a non-wrapping feature is untouched by the wrap handling", {
  res <- run_export(rbind(
    erow("cob", "PCG", 500, 900),
    erow("nad2", "PCG", 1000, 1400, direction = "-")
  ))
  expect_true(any(grepl("500\t900\tCDS$", res$tbl)))
  expect_true(any(grepl("1400\t1000\tCDS$", res$tbl)))
  expect_true(any(grepl("\tCDS\t500\t900\t", res$gff)))
})

test_that("a multi-exon gene crossing the origin reports the spliced span", {
  # exons 2900..100 and 300..500: the gene runs 2900..500 (402 bp), not the
  # near-whole-circle 300..100 that sorting by pos1 and taking the ends gives
  j <- "JOIN: mode=exon group=1"
  res <- run_export(rbind(
    erow("nad5", "PCG", 2900, 100, notes = j),
    erow("nad5", "PCG", 300, 500, notes = j)
  ))
  expect_true(any(grepl("2900\t3000\tgene$", res$tbl)))
  expect_true(any(grepl("\tgene\t2900\t3500\t", res$gff)))
})

test_that("a wrapping annotation on a linear unit does not produce negative coordinates", {
  res <- run_export(rbind(erow("cox1", "PCG", 2900, 200), erow("cob", "PCG", 500, 900)),
                    topology = "linear")
  coords <- unlist(regmatches(res$tbl, gregexpr("-[0-9]+", res$tbl)))
  expect_length(coords, 0L)
  expect_true(any(grepl("\tregion\t1\t3000\t", res$gff)))
})
