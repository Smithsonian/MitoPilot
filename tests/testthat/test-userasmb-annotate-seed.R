# WF1 on a user-supplied assembly must seed one annotate row per non-ignored
# contig. WF2 reads its work list from assemblies JOIN annotate on
# (ID, path, scaffold), an inner join, so a contig with no annotate row is
# silently invisible: no error, just missing work.

nf_lines <- function(module) {
  p <- system.file(file.path("nextflow/modules", module), package = "MitoPilot")
  if (!nzchar(p)) p <- testthat::test_path("../..", "inst/nextflow/modules", module)
  readLines(p, warn = FALSE)
}

# Pull the seeder's SQL out of the shipped module so the test exercises the
# statement that actually runs, not a copy of it.
seed_sql <- function() {
  nf <- nf_lines("coverage_userAsmb_workflow.nf")
  start <- grep("'''INSERT INTO annotate", nf, fixed = TRUE)
  expect_length(start, 1L)
  end <- grep("''', db: 'sqlite')", nf, fixed = TRUE)
  end <- end[end >= start][1]
  block <- nf[start:end]
  block[1] <- sub("^.*'''", "", block[1])
  block[length(block)] <- sub("''',[[:space:]]*db:.*$", "", block[length(block)])
  paste(block, collapse = "\n")
}

seed_db <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbExecute(con, "CREATE TABLE annotate (
      ID TEXT NOT NULL, path INTEGER NOT NULL, scaffold INTEGER NOT NULL,
      annotate_switch INTEGER, annotate_lock INTEGER,
      annotate_opts TEXT, curate_opts TEXT, orf_opts TEXT,
      reviewed TEXT, partial TEXT, topology TEXT,
      PRIMARY KEY (ID, path, scaffold))")
  con
}

# One call of the seeder for a list of units, mirroring what nf-sqldb does with
# the channel: one execution of the statement per emitted tuple.
run_seed <- function(con, units) {
  sql <- seed_sql()
  for (u in units) {
    DBI::dbExecute(con, sql, params = list(
      u$ID, u$path, u$scaffold, u$topology, u$partial,
      "default", "default", "default"
    ))
  }
  DBI::dbReadTable(con, "annotate")
}

unit <- function(scaffold, topology = "linear", partial = "yes") {
  list(ID = "s1", path = 1L, scaffold = as.integer(scaffold),
       topology = topology, partial = partial)
}

test_that("a two-contig user assembly gets one annotate row per contig", {
  con <- seed_db()
  on.exit(DBI::dbDisconnect(con))
  # project init already wrote the (1,1) row
  DBI::dbExecute(con, "INSERT INTO annotate
    (ID, path, scaffold, annotate_switch, annotate_lock, annotate_opts,
     curate_opts, orf_opts, reviewed, partial)
    VALUES ('s1', 1, 1, 1, 0, 'default', 'default', 'default', 'no', 'no')")

  out <- run_seed(con, list(unit(1, "circular", "no"), unit(2)))
  expect_equal(nrow(out), 2L)
  expect_equal(sort(out$scaffold), c(1L, 2L))
  expect_true(all(out$annotate_switch == 1))
  expect_true(all(out$annotate_lock == 0))
  expect_equal(out$partial[out$scaffold == 2], "yes")
  # the pre-existing untouched row is refreshed with this run's topology
  expect_equal(out$topology[out$scaffold == 1], "circular")
})

test_that("re-running WF1 neither duplicates rows nor resets worked-on units", {
  con <- seed_db()
  on.exit(DBI::dbDisconnect(con))
  units <- list(unit(1), unit(2))
  run_seed(con, units)

  # user works on contig 1: annotated, locked, reviewed, custom options
  DBI::dbExecute(con, "UPDATE annotate SET annotate_switch = 2, annotate_lock = 1,
     reviewed = 'yes', curate_opts = 'custom', topology = 'circular', partial = 'no'
     WHERE ID = 's1' AND path = 1 AND scaffold = 1")

  out <- run_seed(con, units)
  expect_equal(nrow(out), 2L)  # no duplicates
  worked <- out[out$scaffold == 1, ]
  expect_equal(worked$annotate_switch, 2)
  expect_equal(worked$annotate_lock, 1)
  expect_equal(worked$reviewed, "yes")
  expect_equal(worked$curate_opts, "custom")
  expect_equal(worked$topology, "circular")
  expect_equal(worked$partial, "no")
})

test_that("an ignored contig is filtered out before the seeder", {
  nf <- nf_lines("coverage_userAsmb_workflow.nf")
  i <- grep("non-ignored units only", nf, fixed = TRUE)
  expect_length(i, 1L)
  expect_match(nf[i], "row[8] == 0", fixed = TRUE)

  # ignore = 0 for scaffold 1, ignore = 1 for scaffold 2 -> only scaffold 1 seeds
  con <- seed_db()
  on.exit(DBI::dbDisconnect(con))
  out <- run_seed(con, list(unit(1)))
  expect_equal(nrow(out), 1L)
  expect_equal(out$scaffold, 1L)
})

test_that("a fragmented user assembly is a note, not a failed sample", {
  nf <- nf_lines("coverage_userAsmb_workflow.nf")
  i <- grep("max_scaffolds > 1", nf, fixed = TRUE)
  expect_length(i, 1L)
  block <- paste(nf[i:(i + 5)], collapse = " ")
  expect_false(grepl("status", block, fixed = TRUE))
  expect_match(block, "disconnected contigs", fixed = TRUE)
  # more than one path is still a failure: user assemblies only ever have path 1
  j <- grep("max_paths > 1", nf, fixed = TRUE)
  expect_match(paste(nf[j:(j + 2)], collapse = " "), "status\\s*=\\s*'3'")
})
