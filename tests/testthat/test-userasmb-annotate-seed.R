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
      reviewed TEXT, problematic TEXT, ID_verified TEXT,
      partial TEXT, topology TEXT,
      PRIMARY KEY (ID, path, scaffold))")
  con
}

# One row of assemblies_ch, the channel the seeder is fed from:
# (ID, path, scaffold, length, length_raw, topology, time_stamp, sequence, ignore)
asmb_row <- function(scaffold, topology = "linear", ignore = 0L, ID = "s1") {
  list(ID, 1L, as.integer(scaffold), 16000L, 16000L, topology, 1L, "ACGT",
       as.integer(ignore))
}

# R MIRROR of the Groovy channel logic in COVERAGE_userAsmb_WRITE: the
# `.filter { row -> row[8] == 0 }` and the partial rule in the following `.map`.
# Groovy cannot be run from here, so this is a mirror of the predicate, not the
# real channel; the source-text assertions below pin the mirror to the module.
channel_units <- function(rows, linear_complete = 0L) {
  keep <- Filter(function(r) r[[9]] == 0L, rows)
  lapply(keep, function(r) {
    list(
      ID = r[[1]], path = r[[2]], scaffold = r[[3]], topology = r[[6]],
      partial = if (r[[6]] == "circular" || linear_complete == 1L) "no" else "yes"
    )
  })
}

# nf-sqldb runs the statement once per emitted tuple.
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

seed_rows <- function(con, rows, linear_complete = 0L) {
  run_seed(con, channel_units(rows, linear_complete))
}

init_row <- function(con, scaffold = 1L) {
  DBI::dbExecute(con, sprintf("INSERT INTO annotate
    (ID, path, scaffold, annotate_switch, annotate_lock, annotate_opts,
     curate_opts, orf_opts, reviewed, problematic, ID_verified, partial)
    VALUES ('s1', 1, %d, 1, 0, 'default', 'default', 'default',
            'no', 'no', 'no', 'no')", as.integer(scaffold)))
}

test_that("a two-contig user assembly gets one annotate row per contig", {
  con <- seed_db()
  on.exit(DBI::dbDisconnect(con))
  init_row(con)  # project init already wrote the (1,1) row

  out <- seed_rows(con, list(asmb_row(1, "circular"), asmb_row(2)))
  expect_equal(nrow(out), 2L)
  expect_equal(sort(out$scaffold), c(1L, 2L))
  expect_true(all(out$annotate_switch == 1))
  expect_true(all(out$annotate_lock == 0))
  expect_equal(out$partial[out$scaffold == 2], "yes")
  expect_equal(out$partial[out$scaffold == 1], "no")
  # the pre-existing untouched row is refreshed with this run's topology
  expect_equal(out$topology[out$scaffold == 1], "circular")
})

test_that("re-running WF1 neither duplicates rows nor resets worked-on units", {
  con <- seed_db()
  on.exit(DBI::dbDisconnect(con))
  rows <- list(asmb_row(1), asmb_row(2))
  seed_rows(con, rows)

  # user works on contig 1: annotated, locked, reviewed, custom options
  DBI::dbExecute(con, "UPDATE annotate SET annotate_switch = 2, annotate_lock = 1,
     reviewed = 'yes', curate_opts = 'custom', topology = 'circular', partial = 'no'
     WHERE ID = 's1' AND path = 1 AND scaffold = 1")

  out <- seed_rows(con, rows)
  expect_equal(nrow(out), 2L)  # no duplicates
  worked <- out[out$scaffold == 1, ]
  expect_equal(worked$annotate_switch, 2)
  expect_equal(worked$annotate_lock, 1)
  expect_equal(worked$reviewed, "yes")
  expect_equal(worked$curate_opts, "custom")
  expect_equal(worked$topology, "circular")
  expect_equal(worked$partial, "no")
})

test_that("an ignored contig seeds no row (mirror of the channel filter)", {
  con <- seed_db()
  on.exit(DBI::dbDisconnect(con))
  # two contigs, the second ignored: over-seeding here creates a zombie unit,
  # under-seeding makes WF2 drop real work, and neither errors anywhere.
  out <- seed_rows(con, list(asmb_row(1), asmb_row(2, ignore = 1L)))
  expect_equal(nrow(out), 1L)
  expect_equal(out$scaffold, 1L)

  # inverting the flag moves the row, so the assertion cannot pass by accident
  con2 <- seed_db()
  on.exit(DBI::dbDisconnect(con2), add = TRUE)
  out2 <- seed_rows(con2, list(asmb_row(1, ignore = 1L), asmb_row(2)))
  expect_equal(nrow(out2), 1L)
  expect_equal(out2$scaffold, 2L)
})

test_that("the module still filters on the assemblies_ch ignore slot", {
  # pins the R mirror above to the real predicate; deliberately narrow, so a
  # change to the filter fails here rather than drifting unnoticed
  nf <- nf_lines("coverage_userAsmb_workflow.nf")
  filt <- grep("^\\s*\\.filter \\{ row -> row\\[8\\] == 0 \\}", nf)
  expect_length(filt, 1L)
  # it must be filtering assemblies_ch, whose slot 8 is the ignore flag
  expect_match(nf[filt - 1L], "^\\s*assemblies_ch\\s*$")
})

test_that("the only live contig can be scaffold 2, not scaffold 1", {
  con <- seed_db()
  on.exit(DBI::dbDisconnect(con))
  init_row(con)  # init seeded (1,1); the FASTA's only passing contig is (1,2)
  out <- seed_rows(con, list(asmb_row(1, ignore = 1L), asmb_row(2, "circular")))
  # the (1,1) row survives untouched as an ignored unit; (1,2) is seeded and is
  # the unit WF2 will pick up
  expect_true(2L %in% out$scaffold)
  expect_equal(out$topology[out$scaffold == 2], "circular")
  expect_equal(out$annotate_switch[out$scaffold == 2], 1)
})

test_that("a sample with every contig ignored seeds nothing and does not error", {
  con <- seed_db()
  on.exit(DBI::dbDisconnect(con))
  expect_silent(out <- seed_rows(con, list(
    asmb_row(1, ignore = 1L), asmb_row(2, ignore = 1L)
  )))
  expect_equal(nrow(out), 0L)
})

test_that("a fragmented user assembly is a note, not a failed sample", {
  nf <- nf_lines("coverage_userAsmb_workflow.nf")
  i <- grep("^\\s*if \\(max_scaffolds > 1\\) \\{", nf)
  expect_length(i, 1L)
  # walk to the matching brace instead of assuming a fixed block length
  depth <- 0L
  j <- i
  repeat {
    depth <- depth + lengths(regmatches(nf[j], gregexpr("\\{", nf[j]))) -
      lengths(regmatches(nf[j], gregexpr("\\}", nf[j])))
    if (depth <= 0L) break
    j <- j + 1L
  }
  block <- paste(nf[i:j], collapse = " ")
  expect_false(grepl("status", block, fixed = TRUE))
  expect_match(block, "disconnected contigs", fixed = TRUE)
  # more than one path is still a failure: user assemblies only ever have path 1
  k <- grep("^\\s*if \\(max_paths > 1\\) \\{", nf)
  expect_length(k, 1L)
  expect_match(paste(nf[k:(k + 2)], collapse = " "), "status\\s*=\\s*'3'")
})

# A sample the mitogenome search fails keeps no contigs, so the annotate row
# seeded at project init points at nothing. WF1 must switch it off.
fail_units_sql <- function() {
  nf <- nf_lines("find_mito_workflow.nf")
  start <- grep("^params.sqlFailFindMitoUnits", nf)
  expect_length(start, 1L)
  line <- nf[start + 1L]
  sub("\"[[:space:]]*$", "", sub("^[^\"]*\"", "", line))
}

test_that("failing the mitogenome search switches off the sample's seeded unit", {
  con <- seed_db()
  withr::defer(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "INSERT INTO annotate
    (ID, path, scaffold, annotate_switch, annotate_lock, reviewed) VALUES
    ('nomito', 1, 1, 1, 0, 'no'),
    ('locked', 1, 1, 1, 1, 'no'),
    ('done',   1, 1, 1, 0, 'yes'),
    ('other',  1, 1, 1, 0, 'no')")

  DBI::dbExecute(con, fail_units_sql(), params = list("nomito"))

  state <- function(id) DBI::dbGetQuery(con,
    "SELECT annotate_switch FROM annotate WHERE ID = ?", params = list(id))$annotate_switch
  expect_equal(state("nomito"), 0L)   # the zombie
  expect_equal(state("other"), 1L)    # a different sample is untouched
})

test_that("a unit somebody has worked on survives the switch-off", {
  con <- seed_db()
  withr::defer(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "INSERT INTO annotate
    (ID, path, scaffold, annotate_switch, annotate_lock, reviewed) VALUES
    ('s1', 1, 1, 1, 1, 'no'),
    ('s1', 1, 2, 1, 0, 'yes'),
    ('s1', 1, 3, 1, 0, 'no')")

  DBI::dbExecute(con, fail_units_sql(), params = list("s1"))

  out <- DBI::dbGetQuery(con,
    "SELECT scaffold, annotate_switch FROM annotate WHERE ID = 's1' ORDER BY scaffold")
  expect_equal(out$annotate_switch, c(1L, 1L, 0L))  # locked, reviewed, untouched
})
