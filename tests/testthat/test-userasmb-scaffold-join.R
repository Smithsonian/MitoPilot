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
