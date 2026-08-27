test_that("new_db_userAsmb creates the join_scaffolds toggle, off by default", {
  d <- withr::local_tempdir()
  mapping <- file.path(d, "mapping.csv")
  utils::write.csv(
    data.frame(ID = "S1", Taxon = "Conger oceanicus", Assembly = "S1.fasta",
               Topology = "linear", R1 = "S1_R1.fastq.gz", R2 = "S1_R2.fastq.gz"),
    mapping, row.names = FALSE
  )
  db <- file.path(d, ".sqlite")
  new_db_userAsmb(db_path = db, mapping_fn = mapping)

  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_true("join_scaffolds" %in% DBI::dbListFields(con, "assemble_opts"))
  expect_equal(
    DBI::dbGetQuery(con, "SELECT join_scaffolds FROM assemble_opts")$join_scaffolds,
    0L
  )
})

test_that("new_db_userAsmb honours join_scaffolds = TRUE", {
  d <- withr::local_tempdir()
  mapping <- file.path(d, "mapping.csv")
  utils::write.csv(
    data.frame(ID = "S1", Taxon = "Conger oceanicus", Assembly = "S1.fasta",
               Topology = "linear", R1 = "S1_R1.fastq.gz", R2 = "S1_R2.fastq.gz"),
    mapping, row.names = FALSE
  )
  db <- file.path(d, ".sqlite")
  new_db_userAsmb(db_path = db, mapping_fn = mapping, join_scaffolds = TRUE)

  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_equal(
    DBI::dbGetQuery(con, "SELECT join_scaffolds FROM assemble_opts")$join_scaffolds,
    1L
  )
})

test_that("a mitogenome in three same-reference pieces is kept whole", {
  # The UA_MULTI_FRAG shape: three ~6 kb pieces, one reference.
  hits <- do.call(rbind, lapply(1:3, function(i) {
    data.frame(qseqid = paste0("mito_contig_", i), saccver = "NC_083079.1",
               pident = 99, length = 6000, bitscore = 11000, qlen = 6008)
  }))
  res <- select_mito_contigs(hits)
  expect_equal(res$accession, "NC_083079.1")
  expect_length(res$candidates, 3L)
})

test_that("three contigs of one path are join-eligible", {
  asmb <- data.frame(ID = "UA_MULTI_FRAG", path = c(1, 1, 1), scaffold = 1:3)
  expect_true(scaffold_join_eligible(asmb))
})

test_that("a single-contig sample is not join-eligible", {
  expect_false(scaffold_join_eligible(data.frame(ID = "S", path = 1, scaffold = 1)))
})

test_that("contigs matching different references cancel the automatic join", {
  # This is the guard the user-assembly path relies on instead of a
  # contamination gate of its own: UA_MULTI_TWO must decline, not join.
  contaminated <- data.frame(
    scaffold = 1:2,
    blast_accession = c("NC_083079.1", "NC_083028.1")
  )
  expect_true(scaffold_hits_disagree(contaminated))

  fragmented <- data.frame(
    scaffold = 1:3,
    blast_accession = rep("NC_083079.1", 3)
  )
  expect_false(scaffold_hits_disagree(fragmented))
})

test_that("a scaffold with no BLAST hit is not a second opinion", {
  df <- data.frame(scaffold = 1:2,
                   blast_accession = c("NC_083079.1", "NO HIT"))
  expect_false(scaffold_hits_disagree(df))
})
