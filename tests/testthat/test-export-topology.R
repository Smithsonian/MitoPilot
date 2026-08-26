# Per-record topology in submission deflines.
#
# Export runs per assembly unit (ID, path, scaffold), so every record must carry
# ITS OWN topology. annotate.topology summarizes a whole unit ("fragmented"), and
# for a user-supplied assembly one annotate row covers every contig of the
# sample, so it can never be the source for a defline.

L <- 1200L
withr::with_seed(11, {
  topo_seq <- paste(sample(c("A", "C", "G", "T"), L, replace = TRUE), collapse = "")
})

# one PCG per unit, so linear-end trimming and the .tbl have something to work on
trow <- function(scaffold) {
  data.frame(
    ID = "s1", path = 1L, scaffold = as.integer(scaffold),
    contig = paste0("s1.1.", scaffold), type = "PCG",
    gene = "cox1", product = "cox1", pos1 = 100L, pos2 = 700L,
    length = 601L, direction = "+", start_codon = "ATG", stop_codon = "TAA",
    translation = strrep("M", 30L), anticodon = NA_character_,
    partial_start = 0L, partial_stop = 0L, notes = NA_character_, refHits = "{}",
    warnings = NA_character_, stringsAsFactors = FALSE
  )
}

# Minimal project database. `scaffold_topology` is the per-scaffold truth in
# `assemblies`; `annotate_topology` is the per-unit summary the annotate row
# carries. `annotate_scaffolds` limits which units get an annotate row at all
# (a user assembly has only one, for the first contig).
run_topology_export <- function(scaffold_topology,
                                annotate_topology,
                                annotate_scaffolds = 1L,
                                gene_export = FALSE) {
  d <- withr::local_tempdir(.local_envir = parent.frame())
  out_dir <- file.path(d, "out")
  dir.create(out_dir)
  scafs <- seq_along(scaffold_topology)
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(d, ".sqlite"))
  DBI::dbWriteTable(con, "annotations", do.call(rbind, lapply(scafs, trow)))
  DBI::dbWriteTable(con, "assemblies", data.frame(
    ID = "s1", path = 1L, scaffold = as.integer(scafs), ignore = 0L,
    topology = scaffold_topology, sequence = topo_seq, stringsAsFactors = FALSE))
  DBI::dbWriteTable(con, "export", data.frame(
    ID = "s1", path = 1L, scaffold = as.integer(scafs), export_group = "g1",
    export_time_stamp = NA_integer_, stringsAsFactors = FALSE))
  DBI::dbWriteTable(con, "annotate", data.frame(
    ID = "s1", path = 1L, scaffold = as.integer(annotate_scaffolds),
    topology = annotate_topology, partial = "no", curate_opts = "default",
    stringsAsFactors = FALSE))
  DBI::dbWriteTable(con, "curate_opts", data.frame(
    curate_opts = "default", params = "{}", linear_complete = 0L,
    stringsAsFactors = FALSE))
  DBI::dbWriteTable(con, "samples", data.frame(
    ID = "s1", Taxon = "Testus testus", genetic_code = 2L, stringsAsFactors = FALSE))
  DBI::dbWriteTable(con, "assemble", data.frame(
    ID = "s1", blast_accession = "NC_000001", blast_accession_auto = 0L,
    poor_blast_ref = "ok", stringsAsFactors = FALSE))
  DBI::dbDisconnect(con)

  suppressMessages(export_files(
    group = "g1", out_dir = out_dir, generateAAalignments = FALSE,
    gene_export = gene_export, review = FALSE, summary_csv = FALSE,
    # the app's gene template carries {topology}; export_files' own default
    # does not, so pass it explicitly to exercise that defline route
    fasta_header_gene = DEFAULT_FASTA_HEADER_GENE
  ))
  fas <- list.files(file.path(out_dir, "s1", "export"), pattern = "[.]fasta$",
                    full.names = TRUE)
  # the only gene in these fixtures, so its per-gene FASTAs are the gene route
  is_gene <- grepl("_cox1[.]fasta$", fas)
  hdrs <- function(f) sub("^>", "", grep("^>", readLines(f), value = TRUE))
  list(deflines = unlist(lapply(fas[!is_gene], hdrs)),
       gene_deflines = unlist(lapply(fas[is_gene], hdrs)),
       group_fasta = grep("^>", readLines(file.path(out_dir, "export", "g1", "g1.fasta")),
                          value = TRUE))
}

# defline for one seqid
defline_for <- function(res, seqid) {
  hit <- grep(paste0("^", seqid, " "), res$deflines, value = TRUE)
  expect_length(hit, 1L)
  hit
}

test_that("a mixed-topology sample gets each record's own topology", {
  res <- run_topology_export(
    scaffold_topology = c("circular", "linear"),
    annotate_topology = "fragmented"
  )

  expect_match(defline_for(res, "s1_p1_s1"), "[topology=circular]", fixed = TRUE)
  expect_match(defline_for(res, "s1_p1_s2"), "[topology=linear]", fixed = TRUE)

  # the joined string must never appear, in any defline or the group FASTA
  expect_false(any(grepl("circular;linear", res$deflines, fixed = TRUE)))
  expect_false(any(grepl(";", res$deflines, fixed = TRUE)))
  expect_false(any(grepl("circular;linear", res$group_fasta, fixed = TRUE)))
  expect_false(any(grepl("fragmented", res$deflines, fixed = TRUE)))

  # completeness follows the record, not the sample
  expect_match(defline_for(res, "s1_p1_s1"), "complete genome", fixed = TRUE)
  expect_match(defline_for(res, "s1_p1_s2"), "partial genome", fixed = TRUE)
})

test_that("the per-gene deflines carry the record's topology too", {
  # the app's gene template contains {topology}, so the gene route is a defline
  # of its own and must not inherit a sibling contig's value either
  res <- run_topology_export(
    scaffold_topology = c("circular", "linear"),
    annotate_topology = "fragmented",
    gene_export = TRUE
  )
  expect_length(res$gene_deflines, 2L)
  expect_match(
    grep("^s1_p1_s1_cox1 ", res$gene_deflines, value = TRUE),
    "[topology=circular]", fixed = TRUE
  )
  expect_match(
    grep("^s1_p1_s2_cox1 ", res$gene_deflines, value = TRUE),
    "[topology=linear]", fixed = TRUE
  )
  expect_false(any(grepl("circular;linear", res$gene_deflines, fixed = TRUE)))
  expect_false(any(grepl("fragmented", res$gene_deflines, fixed = TRUE)))
})

test_that("an all-circular sample gets circular on every record", {
  res <- run_topology_export(
    scaffold_topology = c("circular", "circular", "circular"),
    annotate_topology = "circular"
  )
  expect_length(res$deflines, 3L)
  expect_true(all(grepl("[topology=circular]", res$deflines, fixed = TRUE)))
  expect_true(all(grepl("complete genome", res$deflines, fixed = TRUE)))
})

test_that("an all-linear sample gets linear on every record", {
  res <- run_topology_export(
    scaffold_topology = c("linear", "linear"),
    annotate_topology = "linear"
  )
  expect_length(res$deflines, 2L)
  expect_true(all(grepl("[topology=linear]", res$deflines, fixed = TRUE)))
  expect_true(all(grepl("partial genome", res$deflines, fixed = TRUE)))
})

test_that("a single-scaffold sample is unchanged", {
  res <- run_topology_export(
    scaffold_topology = "circular",
    annotate_topology = "circular"
  )
  expect_length(res$deflines, 1L)
  expect_match(defline_for(res, "s1"), "[topology=circular]", fixed = TRUE)
})

test_that("the 'fragmented' annotate value still resolves to the kept scaffold", {
  # what the rescue this replaces was for: a multi-scaffold sample whose
  # annotate row says "fragmented" while a single scaffold survives the ignore
  # filter. The record must carry that scaffold's real topology.
  res <- run_topology_export(
    scaffold_topology = "circular",
    annotate_topology = "fragmented"
  )
  expect_match(defline_for(res, "s1"), "[topology=circular]", fixed = TRUE)
  expect_false(any(grepl("fragmented", res$deflines, fixed = TRUE)))
})

test_that("an unusable topology warns and never reaches the defline", {
  expect_warning(
    res <- run_topology_export(
      scaffold_topology = NA_character_,
      annotate_topology = "circular;linear"
    ),
    "no per-scaffold topology"
  )
  expect_match(defline_for(res, "s1"), "[topology=linear]", fixed = TRUE)
  expect_false(any(grepl("circular;linear", res$deflines, fixed = TRUE)))
  expect_false(any(grepl("NA", res$deflines, fixed = TRUE)))
})

# ---- The annotate-row topology summary, read from the shipped module -------

test_that("validate.nf collapses a mixed unit instead of joining with ';'", {
  nf <- readLines(
    system.file("nextflow/modules/validate.nf", package = "MitoPilot"),
    warn = FALSE
  )
  topo <- grep("^\\s*def topology = ", nf, value = TRUE)
  expect_length(topo, 1L)
  # a ';'-joined value reads as a topology downstream and would reach a defline
  expect_false(grepl("join(';')", topo, fixed = TRUE))
  expect_match(topo, "'fragmented'", fixed = TRUE)
  # the values are de-duplicated, so an all-circular unit still reads "circular"
  vals <- grep("^\\s*def topoVals = ", nf)
  expect_length(vals, 1L)
  expect_match(paste(nf[vals + 0:1], collapse = " "), ".unique()", fixed = TRUE)
})

# ---- The outlier-review recompute on a user-assembly-shaped project --------
#
# A user-supplied assembly is annotated as one genome, so a single `annotate` row
# covers every contig of the sample. Any per-unit lookup of the curation profile
# must therefore fall back to the sample's row, or the sibling contig queries
# `curate_opts` with no value at all.

pcg_row <- function(scaffold, gene, pos1, pos2) {
  data.frame(
    ID = "s1", path = 1L, scaffold = as.integer(scaffold), type = "PCG",
    gene = gene, product = gene, pos1 = as.integer(pos1), pos2 = as.integer(pos2),
    direction = "+", translation = strrep("M", 20L), notes = NA_character_,
    stringsAsFactors = FALSE
  )
}

# two contigs, one annotate row, a multi-exon gene on the SIBLING contig
userasmb_db <- function() {
  d <- withr::local_tempdir(.local_envir = parent.frame())
  db <- file.path(d, ".sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  DBI::dbWriteTable(con, "annotations", rbind(
    pcg_row(1L, "cox1", 100, 700),
    pcg_row(2L, "nad5", 100, 399),
    pcg_row(2L, "nad5", 600, 899)
  ))
  DBI::dbWriteTable(con, "assemblies", data.frame(
    ID = "s1", path = 1L, scaffold = 1:2, ignore = 0L,
    topology = c("circular", "linear"), sequence = topo_seq,
    stringsAsFactors = FALSE))
  DBI::dbWriteTable(con, "export", data.frame(
    ID = "s1", path = 1L, scaffold = 1:2, export_group = "g1",
    export_time_stamp = NA_integer_, stringsAsFactors = FALSE))
  DBI::dbWriteTable(con, "annotate", data.frame(
    ID = "s1", path = 1L, scaffold = 1L, topology = "fragmented",
    partial = "no", curate_opts = "default", stringsAsFactors = FALSE))
  DBI::dbWriteTable(con, "curate_opts", data.frame(
    curate_opts = "default",
    params = '{"default_rules":{"PCG":{"intron":true}}}',
    linear_complete = 0L, stringsAsFactors = FALSE))
  DBI::dbWriteTable(con, "samples", data.frame(
    ID = "s1", Taxon = "Testus testus", genetic_code = 2L, stringsAsFactors = FALSE))
  con
}

test_that("the outlier-review recompute survives a contig with no annotate row", {
  con <- userasmb_db()
  on.exit(DBI::dbDisconnect(con))

  ann <- suppressMessages(get_export_PCG_annotations(con, "g1"))

  # the two nad5 exons on the sibling contig merged into one starred record
  nad5 <- ann[ann$gene == "nad5", ]
  expect_equal(nrow(nad5), 1L)
  expect_equal(nad5$seqid, "*s1_p1_s2")
  # 600 nt spliced -> 200 residues
  expect_equal(nchar(nad5$translation), 200L)
  # the single-exon gene on the annotated contig is untouched
  expect_equal(ann$seqid[ann$gene == "cox1"], "s1_p1_s1")
})
