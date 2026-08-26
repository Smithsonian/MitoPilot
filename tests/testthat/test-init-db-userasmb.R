test_that("new_db_userAsmb creates a usable project database", {
  # Regression: every CREATE TABLE statement must be valid SQLite. A bad
  # primary key here breaks project creation outright, and nothing else in the
  # suite builds a user-assembly database from scratch.
  td <- withr::local_tempdir()
  mapping_fn <- file.path(td, "mapping.csv")
  utils::write.csv(
    data.frame(
      ID = c("s1", "s2"),
      Taxon = c("Danio rerio", "Danio rerio"),
      Assembly = c("s1.fasta", "s2.fasta"),
      Topology = c("linear", "circular")
    ),
    mapping_fn, row.names = FALSE
  )

  db_path <- file.path(td, ".sqlite")
  new_db_userAsmb(db_path = db_path, mapping_fn = mapping_fn,
                  no_raw_data = TRUE)

  expect_true(file.exists(db_path))
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  expect_true(all(c("samples", "preprocess", "assemble", "assemblies",
                    "annotate", "annotations", "export", "mito_candidates",
                    "circularize_opts", "find_mito_opts",
                    "circularize_overlap", "circularize_depth") %in%
                    DBI::dbListTables(con)))
  expect_equal(nrow(DBI::dbReadTable(con, "samples")), 2L)
  expect_equal(nrow(DBI::dbReadTable(con, "preprocess")), 2L)
  expect_equal(nrow(DBI::dbReadTable(con, "assemble")), 2L)
  expect_false(any(grepl("circularization", schema_gaps(con))))
})
