# The user-assembly app after contigs became independent units:
#
# 1. the Assemble table's topology column summarises per-contig values;
# 2. the Assemble tab no longer refuses to lock a fragmented sample, which is
#    what kept every one of them out of WF2;
# 3. the Export table and the summary CSV report the contig's own topology.

# --- 1. topology summary -----------------------------------------------------

test_that("contigs that agree report the single topology", {
  expect_equal(summarize_topology("circular"), "circular")
  expect_equal(summarize_topology("linear"), "linear")
  expect_equal(summarize_topology(rep("circular", 3L)), "circular")
  expect_equal(summarize_topology(rep("linear", 2L)), "linear")
})

test_that("contigs that disagree report a count of each", {
  expect_equal(
    summarize_topology(c("circular", "circular", "linear")),
    "2 circular, 1 linear"
  )
  expect_equal(
    summarize_topology(c("linear", "circular")),
    "1 circular, 1 linear"
  )
  # order of the input must not change the label
  expect_equal(
    summarize_topology(c("linear", "circular", "circular")),
    summarize_topology(c("circular", "linear", "circular"))
  )
})

test_that("no contigs at all gives NA, not an empty string", {
  expect_true(is.na(summarize_topology(character(0))))
})

test_that("a contig with no topology yet is counted, not dropped", {
  # dropping it produced "1 circular, 1 linear" for a three-contig sample: a
  # truthful label with a wrong contig count
  expect_equal(
    summarize_topology(c("circular", "linear", NA_character_)),
    "1 circular, 1 linear, 1 unknown"
  )
  expect_equal(
    summarize_topology(c("circular", "circular", "")),
    "2 circular, 1 unknown"
  )
  expect_equal(summarize_topology(c(NA_character_, "")), "unknown")
  # the counts always add up to the number of contigs handed in
  n_counted <- function(x) {
    lab <- summarize_topology(x)
    if (!grepl("[0-9]", lab)) length(x) else sum(as.integer(
      regmatches(lab, gregexpr("[0-9]+", lab))[[1]]
    ))
  }
  expect_equal(n_counted(c("circular", "linear", NA_character_)), 3L)
  expect_equal(n_counted(c("circular", "circular", "circular")), 3L)
})

# --- 2. the Assemble table ---------------------------------------------------

assemble_db <- function(assemblies) {
  # Every real assemblies row carries its sequence; tests that do not care about
  # it get a clean one so the ambiguous-base count is 0.
  if (!"sequence" %in% names(assemblies)) assemblies$sequence <- rep("ACGT", nrow(assemblies))
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(con, "assemble", data.frame(
    ID = c("mixed", "circ", "fresh"), assemble_lock = 0L, assemble_switch = 2L,
    topology = NA_character_, paths = 1L, scaffolds = 1L, length = 16000L,
    blast_accession = NA_character_, poor_blast_ref = NA_character_,
    blast_species = NA_character_, blast_pident = NA_real_,
    blast_qcovs = NA_real_, blast_evalue = NA_real_, blast_lineage = NA_character_,
    pre_opts = "default", find_mito_opts = "default", circularize_opts = "default",
    blast_opts = "default", assemble_notes = NA_character_,
    circularize_notes = NA_character_, find_mito_notes = NA_character_,
    join_notes = NA_character_,
    time_stamp = 1L, stringsAsFactors = FALSE
  ))
  DBI::dbWriteTable(con, "preprocess", data.frame(
    ID = c("mixed", "circ", "fresh"), reads = 1000L, trimmed_reads = 900L,
    mean_length = 150L, time_stamp = 1L, stringsAsFactors = FALSE
  ))
  DBI::dbWriteTable(con, "samples", data.frame(
    ID = c("mixed", "circ", "fresh"), Taxon = "Testus testus",
    topology = "linear", assembly = "in.fasta", stringsAsFactors = FALSE
  ))
  DBI::dbWriteTable(con, "assemblies", assemblies)
  con
}

test_that("the Assemble table summarises each sample's contig topologies", {
  con <- assemble_db(data.frame(
    ID = c("mixed", "mixed", "mixed", "circ", "circ"),
    path = 1L, scaffold = c(1L, 2L, 3L, 1L, 2L),
    topology = c("circular", "circular", "linear", "circular", "circular"),
    ignore = 0L, stringsAsFactors = FALSE
  ))
  withr::defer(DBI::dbDisconnect(con))

  out <- fetch_assemble_data_userAsmb(list(userData = list(con = con)))
  topo <- stats::setNames(out$topology, out$ID)

  expect_equal(topo[["mixed"]], "2 circular, 1 linear")
  expect_equal(topo[["circ"]], "circular")
  # no assemblies rows yet: the declared value is an input, not a measurement,
  # and must not be shown as though WF1 had confirmed it
  expect_equal(topo[["fresh"]], "linear (declared)")
})

test_that("ambiguous bases are counted for single-contig samples only", {
  con <- assemble_db(data.frame(
    ID = c("mixed", "mixed", "circ"),
    path = 1L, scaffold = c(1L, 2L, 1L),
    topology = "linear", ignore = 0L,
    sequence = c("ACGTNN", "ACGT", "ACRGTN"),
    stringsAsFactors = FALSE
  ))
  withr::defer(DBI::dbDisconnect(con))

  out <- fetch_assemble_data_userAsmb(list(userData = list(con = con)))
  amb <- stats::setNames(out$ambiguous_bases, out$ID)

  expect_equal(amb[["circ"]], 2L)
  # more than one active contig: no single number to report in the samples table
  expect_true(is.na(amb[["mixed"]]))
  expect_true(is.na(amb[["fresh"]]))
})

test_that("an ignored contig is left out of the ambiguous-base count", {
  con <- assemble_db(data.frame(
    ID = "mixed", path = 1L, scaffold = c(1L, 2L),
    topology = "linear", ignore = c(0L, 1L),
    sequence = c("ACGTN", "NNNNN"),
    stringsAsFactors = FALSE
  ))
  withr::defer(DBI::dbDisconnect(con))

  out <- fetch_assemble_data_userAsmb(list(userData = list(con = con)))
  expect_equal(out$ambiguous_bases[out$ID == "mixed"], 1L)
})

test_that("an ignored contig is left out of the summary", {
  con <- assemble_db(data.frame(
    ID = "mixed", path = 1L, scaffold = c(1L, 2L),
    topology = c("circular", "linear"), ignore = c(0L, 1L),
    stringsAsFactors = FALSE
  ))
  withr::defer(DBI::dbDisconnect(con))

  out <- fetch_assemble_data_userAsmb(list(userData = list(con = con)))
  expect_equal(out$topology[out$ID == "mixed"], "circular")
})

test_that("a sample with every contig ignored does not pass off the declared value", {
  con <- assemble_db(data.frame(
    ID = "mixed", path = 1L, scaffold = c(1L, 2L),
    topology = c("circular", "linear"), ignore = c(1L, 1L),
    stringsAsFactors = FALSE
  ))
  withr::defer(DBI::dbDisconnect(con))

  out <- fetch_assemble_data_userAsmb(list(userData = list(con = con)))
  # nothing measured survives, so the column falls back and says it fell back
  expect_equal(out$topology[out$ID == "mixed"], "linear (declared)")
})

test_that("a contig with no topology written yet is visible in the table", {
  con <- assemble_db(data.frame(
    ID = "mixed", path = 1L, scaffold = 1:3,
    topology = c("circular", "linear", NA_character_), ignore = 0L,
    stringsAsFactors = FALSE
  ))
  withr::defer(DBI::dbDisconnect(con))

  out <- fetch_assemble_data_userAsmb(list(userData = list(con = con)))
  expect_equal(out$topology[out$ID == "mixed"], "1 circular, 1 linear, 1 unknown")
})

test_that("a fresh project with no assemblies rows still renders", {
  con <- assemble_db(data.frame(
    ID = character(0), path = integer(0), scaffold = integer(0),
    topology = character(0), ignore = integer(0), stringsAsFactors = FALSE
  ))
  withr::defer(DBI::dbDisconnect(con))

  out <- fetch_assemble_data_userAsmb(list(userData = list(con = con)))
  expect_equal(unique(out$topology), "linear (declared)")
})

# --- 3. the lock guard -------------------------------------------------------

test_that("the Assemble tab no longer refuses to lock a fragmented sample", {
  # Read the installed namespace, not the source tree: R CMD check runs the
  # tests against an installed package, where R/ is gone.
  ns <- asNamespace("MitoPilot")
  src <- unlist(lapply(ls(ns, all.names = TRUE), function(nm) {
    obj <- get0(nm, envir = ns, inherits = FALSE)
    if (is.function(obj)) paste(deparse(obj), collapse = "\n") else NULL
  }))
  expect_true(length(src) > 0)
  expect_false(any(grepl("Multiple assemblies detected", src, fixed = TRUE)))
  expect_false(any(grepl("duplicated(assemblies)", src, fixed = TRUE)))
})

# The guard's real cost: every WF2 query gates on assemble_lock = 1, so a sample
# it refused to lock had no units at all. Run the shipped ANNOTATE query against
# a fragmented user assembly to show the lock is the only gate between it and
# WF2.
annotate_sql <- function() {
  p <- system.file("nextflow/modules/annotate_workflow.nf", package = "MitoPilot")
  if (!nzchar(p)) {
    p <- testthat::test_path("../..", "inst/nextflow/modules/annotate_workflow.nf")
  }
  nf <- readLines(p, warn = FALSE)
  start <- grep("^params.sqlRead", nf)
  expect_length(start, 1L)
  end <- start
  while (grepl("[+]\\s*$", nf[end])) end <- end + 1L
  block <- paste(nf[start:end], collapse = " ")
  # concatenate the quoted segments the way Groovy does
  seg <- gregexpr("'[^']*'|\"[^\"]*\"", block)
  seg <- regmatches(block, seg)[[1]]
  paste(substr(seg, 2L, nchar(seg) - 1L), collapse = "")
}

wf2_db <- function(assemble_lock) {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(con, "assemblies", data.frame(
    ID = "s1", path = 1L, scaffold = 1:3, ignore = 0L,
    blast_accession = paste0("ACC", 1:3), stringsAsFactors = FALSE
  ))
  DBI::dbWriteTable(con, "assemble", data.frame(
    ID = "s1", assemble_lock = as.integer(assemble_lock), assemble_opts = "user",
    stringsAsFactors = FALSE
  ))
  DBI::dbWriteTable(con, "annotate", data.frame(
    ID = "s1", path = 1L, scaffold = 1:3, annotate_switch = 1L,
    annotate_lock = 0L, annotate_opts = "default", stringsAsFactors = FALSE
  ))
  opts <- as.data.frame(c(
    list(annotate_opts = "default", cpus = 1L, memory = 4L, ref_db = "db",
         ref_dir = "dir", mitos_opts = "", use_mitos_best = 0L, trnaScan_opts = "",
         start_gene = "", arwen_opts = "", use_arwen = 0L, aragorn_opts = "",
         use_aragorn = 0L, use_mitofinder = 0L, mitofinder_db = "",
         mitofinder_new_genes = 0L, mitofinder_allow_introns = 0L,
         mitofinder_opts = "", coverage_trim = 0L, retain_low_conf_trna = 0L,
         use_mitos = 1L, use_trnaScan = 1L, rescue_no_trna = 0L)
  ), stringsAsFactors = FALSE)
  DBI::dbWriteTable(con, "annotate_opts", opts)
  DBI::dbWriteTable(con, "samples", data.frame(
    ID = "s1", genetic_code = 2L, stringsAsFactors = FALSE
  ))
  con
}

test_that("a locked fragmented sample hands every contig to WF2", {
  sql <- annotate_sql()

  con <- wf2_db(assemble_lock = 1L)
  withr::defer(DBI::dbDisconnect(con))
  locked <- DBI::dbGetQuery(con, sql)
  expect_equal(nrow(locked), 3L)
  expect_equal(sort(locked$scaffold), 1:3)

  # unlocked, the same sample is invisible: this is exactly what the guard
  # condemned every multi-contig user assembly to
  con2 <- wf2_db(assemble_lock = 0L)
  withr::defer(DBI::dbDisconnect(con2))
  expect_equal(nrow(DBI::dbGetQuery(con2, sql)), 0L)
})

# --- 4. the Export table and summary CSV -------------------------------------

export_db <- function(scaffold_topology, annotate_topology) {
  scafs <- seq_along(scaffold_topology)
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(con, "assemblies", data.frame(
    ID = "s1", path = 1L, scaffold = as.integer(scafs), ignore = 0L,
    topology = scaffold_topology, blast_accession = "NC_000001",
    blast_species = "Testus testus", blast_lineage = "Metazoa",
    blast_pident = 99, blast_qcovs = 99, stringsAsFactors = FALSE
  ))
  DBI::dbWriteTable(con, "annotate", data.frame(
    ID = "s1", path = 1L, scaffold = as.integer(scafs), annotate_lock = 1L,
    topology = annotate_topology, partial = NA_character_,
    curate_opts = "default", orf_opts = "default", length = 16000L,
    structure = "", PCGCount = 13L, tRNACount = 22L, rRNACount = 2L,
    missing = NA_character_, extra = NA_character_, warnings = NA_character_,
    stringsAsFactors = FALSE
  ))
  DBI::dbWriteTable(con, "annotations", data.frame(
    ID = "s1", path = 1L, scaffold = as.integer(scafs), type = "PCG",
    stringsAsFactors = FALSE
  ))
  DBI::dbWriteTable(con, "curate_opts", data.frame(
    curate_opts = "default", linear_complete = 0L, stringsAsFactors = FALSE
  ))
  DBI::dbWriteTable(con, "orf_opts", data.frame(
    orf_opts = "default", use_orffinder = 0L, stringsAsFactors = FALSE
  ))
  DBI::dbWriteTable(con, "samples", data.frame(
    ID = "s1", Taxon = "Testus testus", topology = "linear",
    R1 = NA_character_, R2 = NA_character_, stringsAsFactors = FALSE
  ))
  DBI::dbWriteTable(con, "assemble", data.frame(
    ID = "s1", assemble_lock = 1L, poor_blast_ref = NA_character_,
    stringsAsFactors = FALSE
  ))
  DBI::dbWriteTable(con, "export", data.frame(
    ID = "s1", path = 1L, scaffold = as.integer(scafs), export_group = "g1",
    stringsAsFactors = FALSE
  ))
  con
}

test_that("the Export table reports each contig's own topology", {
  con <- export_db(c("circular", "linear"), annotate_topology = "fragmented")
  withr::defer(DBI::dbDisconnect(con))

  out <- fetch_export_data(con = con)
  topo <- stats::setNames(out$topology, out$scaffold)
  expect_equal(topo[["1"]], "circular")
  expect_equal(topo[["2"]], "linear")
  expect_false(any(out$topology == "fragmented"))

  # completeness is derived from topology, so it has to follow the contig
  comp <- stats::setNames(out$completeness, out$scaffold)
  expect_equal(comp[["1"]], "complete genome")
  expect_equal(comp[["2"]], "partial genome")
})

test_that("a legacy joined topology never survives into the Export table", {
  con <- export_db(c("circular", "linear"), annotate_topology = "circular;linear")
  withr::defer(DBI::dbDisconnect(con))

  out <- fetch_export_data(con = con)
  expect_false(any(grepl(";", out$topology, fixed = TRUE)))
  expect_equal(sort(out$topology), c("circular", "linear"))
})

test_that("an unusable topology is coerced the way the defline is", {
  # assemblies has nothing for this contig and annotate carries the sentinel.
  # export_files() coerces that to "linear" (R/export.R, the unusable-value
  # branch, covered in test-export-topology.R), so the table must not show
  # "fragmented" next to a file that says "linear".
  con <- export_db(NA_character_, annotate_topology = "fragmented")
  withr::defer(DBI::dbDisconnect(con))

  out <- fetch_export_data(con = con)
  expect_equal(out$topology, "linear")
  expect_equal(out$completeness, "partial genome")
})

test_that("the regular pipeline's single-unit sample is unchanged", {
  con <- export_db("circular", annotate_topology = "circular")
  withr::defer(DBI::dbDisconnect(con))

  out <- fetch_export_data(con = con)
  expect_equal(nrow(out), 1L)
  expect_equal(out$topology, "circular")
  expect_equal(out$completeness, "complete genome")
})
