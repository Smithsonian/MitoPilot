init_fixture <- function(d) {
  writeLines(c(">a", "ACGT", ">b", "ACGT"), file.path(d, "multi.fa"))
  writeLines(c(">a", "ACGT"), file.path(d, "one.fa"))
  writeLines(c("@r", "ACGT", "+", "IIII"), file.path(d, "S1_R1.fastq.gz"))
  writeLines(c("junk"), file.path(d, "S1_R2.fastq.gz"))
  m <- data.frame(
    ID = c("S1", "S1", "this_id_is_way_too_long_for_ncbi_seqids_x", "bad id", ""),
    Taxon = c("Danio rerio", "", "x", "x", "x"),
    R1 = c("S1_R1.fastq.gz", "S1_R1.fastq.gz", "nope_R1.fq.gz", "a", "a"),
    R2 = c("S1_R2.fastq.gz", "S1_R2.fastq.gz", "nope_R2.fq.gz", "a", "b"),
    Assembly = c("multi.fa", "one.fa", "missing.fa", "one.fa", ""),
    Topology = c("circular", "Circular", "", "linear", ""),
    Notes = "", genetic_code = 2
  )
  utils::write.csv(m, file.path(d, "mapping.csv"), row.names = FALSE)
  file.path(d, "mapping.csv")
}

test_that("preflight reports every problem at once and touches nothing", {
  d <- withr::local_tempdir()
  mapping <- init_fixture(d)
  proj <- file.path(d, "proj")
  err <- tryCatch(
    preflight_project(
      path = proj, mapping_fn = mapping, mapping_id = "ID", data_path = d,
      user_asmb = TRUE, assembly_path = d, find_mitogenome = TRUE,
      mitofinder_db = file.path(d, "one.fa"), executor = "nosuch",
      profile_dir = d, container = "x", genetic_code = 99,
      ncbi_api_key = "short", db_fun = new_db_userAsmb,
      dots = list(curate_target = "dog_mito", annotate_cpus = -1, bogus = 1,
                  linear_complete = "yes")
    ),
    error = function(e) conditionMessage(e)
  )
  expect_match(err, "aborted")
  expect_false(dir.exists(proj))
  txt <- err
  for (needle in c("reserved names", "empty ID", "duplicate ID", "over 40 characters",
                   "characters other than", "same file", "not found in", "not FASTQ",
                   "lowercase 'circular'", "Assembly: empty", "missing.fa",
                   "LOCUS", "No config found", "genetic_code", "unknown argument",
                   "annotate_cpus", "linear_complete", "dog_mito",
                   "entirely empty", "Taxon: empty", "holds 2 contigs")) {
    expect_match(txt, needle, fixed = TRUE, label = needle)
  }
  n_err <- length(grep("  ERROR", strsplit(err, "\n")[[1]]))
  expect_gte(n_err, 15L)
})

test_that("warnings alone do not stop project creation", {
  d <- withr::local_tempdir()
  writeLines(c(">a", "ACGT", ">b", "ACGT"), file.path(d, "multi.fa"))
  utils::write.csv(
    data.frame(ID = "S1", Taxon = "Danio rerio", Assembly = "multi.fa",
               Topology = "circular", Notes = ""),
    file.path(d, "mapping.csv"), row.names = FALSE
  )
  expect_message(
    out <- preflight_project(
      path = file.path(d, "proj"), mapping_fn = file.path(d, "mapping.csv"),
      mapping_id = "ID", data_path = NULL, no_raw_data = TRUE, user_asmb = TRUE,
      assembly_path = d, executor = "local", container = "x",
      db_fun = new_db_userAsmb
    ),
    "0 error"
  )
  expect_true(out)
})

test_that("a clean regular project passes silently", {
  d <- withr::local_tempdir()
  writeLines(c("@r", "ACGT", "+", "IIII"), file.path(d, "S1_R1.fastq.gz"))
  writeLines(c("@r", "ACGT", "+", "IIII"), file.path(d, "S1_R2.fastq.gz"))
  utils::write.csv(
    data.frame(ID = "S1", Taxon = "Danio rerio",
               R1 = "S1_R1.fastq.gz", R2 = "S1_R2.fastq.gz"),
    file.path(d, "mapping.csv"), row.names = FALSE
  )
  expect_silent(preflight_project(
    path = file.path(d, "proj"), mapping_fn = file.path(d, "mapping.csv"),
    mapping_id = "ID", data_path = d, executor = "local", container = "x"
  ))
})

test_that("existing database is refused without force, before any write", {
  d <- withr::local_tempdir()
  utils::write.csv(
    data.frame(ID = "S1", Taxon = "Danio rerio",
               R1 = "S1_R1.fastq.gz", R2 = "S1_R2.fastq.gz"),
    file.path(d, "mapping.csv"), row.names = FALSE
  )
  proj <- file.path(d, "proj")
  dir.create(proj)
  file.create(file.path(proj, ".sqlite"))
  expect_error(
    preflight_project(
      path = proj, mapping_fn = file.path(d, "mapping.csv"), mapping_id = "ID",
      data_path = "s3://bucket/reads", executor = "local", container = "x"
    ),
    "already exists"
  )
  expect_false(file.exists(file.path(proj, "mapping.csv")))
})

test_that("new_db rejects a mapping with several ID problems in one report", {
  d <- withr::local_tempdir()
  utils::write.csv(
    data.frame(ID = c("S1", "S1", "bad id"), Taxon = "x",
               R1 = c("a", "b", "c"), R2 = c("d", "e", "f")),
    file.path(d, "mapping.csv"), row.names = FALSE
  )
  err <- tryCatch(
    new_db(db_path = file.path(d, ".sqlite"), mapping_fn = file.path(d, "mapping.csv")),
    error = function(e) conditionMessage(e)
  )
  expect_match(err, "Mapping file aborted")
  expect_match(err, "duplicate ID")
  expect_match(err, "characters other than")
  expect_false(file.exists(file.path(d, ".sqlite")))
})
