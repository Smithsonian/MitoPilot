# Sample-level topology for user-supplied assemblies. A draft genome with many
# contigs has no single topology, so the declaration is replaced with "multi".

write_fasta <- function(path, n) {
  seqs <- vapply(seq_len(n), function(i) {
    paste0(">contig", i, "\n", strrep("ACGT", 25))
  }, character(1))
  writeLines(seqs, path)
  path
}

test_that("a single-contig assembly keeps the declared topology", {
  td <- withr::local_tempdir()
  write_fasta(file.path(td, "s1.fasta"), 1)
  mapping <- data.frame(ID = "s1", Assembly = "s1.fasta", Topology = "circular")

  expect_equal(resolve_sample_topology(mapping, td), "circular")
})

test_that("a multi-contig assembly is recorded as multi", {
  td <- withr::local_tempdir()
  write_fasta(file.path(td, "s1.fasta"), 5)
  mapping <- data.frame(ID = "s1", Assembly = "s1.fasta", Topology = "linear")

  expect_silent(out <- resolve_sample_topology(mapping, td))
  expect_equal(out, "multi")
})

test_that("only a circular declaration on a multi-contig assembly warns", {
  # Declaring "linear" costs nothing: it is the default and an unknown contig is
  # treated as linear anyway. Declaring "circular" is an assertion we override.
  td <- withr::local_tempdir()
  write_fasta(file.path(td, "s1.fasta"), 5)
  mapping <- data.frame(ID = "s1", Assembly = "s1.fasta", Topology = "circular")

  expect_warning(out <- resolve_sample_topology(mapping, td), "s1")
  expect_equal(out, "multi")
})

test_that("a missing Topology column defaults to linear", {
  td <- withr::local_tempdir()
  write_fasta(file.path(td, "s1.fasta"), 1)
  mapping <- data.frame(ID = "s1", Assembly = "s1.fasta")

  expect_equal(resolve_sample_topology(mapping, td), "linear")
})

test_that("an unreadable assembly falls back to the declaration", {
  # Project setup must not fail just because the assembly directory is not
  # reachable from wherever the project is being created.
  td <- withr::local_tempdir()
  mapping <- data.frame(ID = "s1", Assembly = "missing.fasta", Topology = "circular")

  expect_equal(resolve_sample_topology(mapping, td), "circular")
  expect_equal(resolve_sample_topology(mapping, NULL), "circular")
})

test_that("new_db_userAsmb works without a Topology column", {
  td <- withr::local_tempdir()
  write_fasta(file.path(td, "s1.fasta"), 1)
  write_fasta(file.path(td, "s2.fasta"), 12)
  mapping_fn <- file.path(td, "mapping.csv")
  utils::write.csv(
    data.frame(
      ID = c("s1", "s2"),
      Taxon = c("Danio rerio", "Danio rerio"),
      Assembly = c("s1.fasta", "s2.fasta")
    ),
    mapping_fn, row.names = FALSE
  )

  db_path <- file.path(td, ".sqlite")
  new_db_userAsmb(db_path = db_path, mapping_fn = mapping_fn,
                  assembly_path = td, no_raw_data = TRUE)

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  samples <- DBI::dbReadTable(con, "samples")
  expect_equal(samples$topology[samples$ID == "s1"], "linear")
  expect_equal(samples$topology[samples$ID == "s2"], "multi")
})
