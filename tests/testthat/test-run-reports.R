# Run reports read from trimmed real Nextflow 25.10 logs in fixtures/runs.

fx <- function(name) test_path("fixtures", "runs", paste0(name, ".nextflow.log"))

# A project with one log per fixture under .runs/nextflow/<base>.nextflow.log
local_runs_project <- function(logs, env = parent.frame()) {
  d <- withr::local_tempdir(.local_envir = env)
  dir.create(run_dir(d, "nextflow"), recursive = TRUE)
  for (b in names(logs)) file.copy(fx(logs[[b]]), file.path(run_dir(d, "nextflow"), paste0(b, ".nextflow.log")))
  d
}

test_that("run_basename and run_dir name the run files", {
  t <- as.POSIXct("2026-09-29 15:10:05")
  expect_equal(run_basename("Assemble", t), "assemble_2026-09-29_15-10-05")
  expect_equal(run_basename(c("annotate", "assemble"), t), "annotate_2026-09-29_15-10-05")
  expect_equal(run_dir("/p", "logs"), file.path("/p", ".runs", "logs"))
  expect_equal(run_base_time("assemble_2026-09-29_15-10-05"), t)
})

test_that("nf_log_facts reads a clean run", {
  f <- nf_log_facts(fx("clean"))
  expect_equal(f$run, "suspicious_boltzmann")
  expect_equal(f$version, "25.10.6")
  expect_equal(f$entry, "WF1")
  expect_true(f$finished)
  expect_false(f$fatal)
  expect_true(is.na(f$error))
  expect_equal(f$stats[["succeeded"]], 152L)
  expect_equal(nrow(f$tasks), 0L)
  expect_true("SRR22396640" %in% f$samples)
  expect_equal(format(f$started, "%m-%d %H:%M:%S"), "09-08 12:51:07")
  expect_equal(as.numeric(difftime(f$ended, f$started, units = "secs")), 590, tolerance = 1)
})

test_that("nf_log_facts lists failed tasks with sample, step, exit, and cause", {
  d <- withr::local_tempdir()
  log <- file.path(d, "x.nextflow.log")
  wd <- file.path(d, "work", "d2", "72503c")
  dir.create(wd, recursive = TRUE)
  writeLines(c("GetOrganelle started", "", "Error: no seed reads found"), file.path(wd, ".command.err"))
  writeLines(sub("/proj/work/d2/72503c679596dd6924b430de3fc501", wd, readLines(fx("failures")),
    fixed = TRUE), log)
  f <- nf_log_facts(log)
  expect_equal(nrow(f$tasks), 1L)
  expect_equal(f$tasks$sample, "t2_MG676469_1")
  expect_equal(f$tasks$step, "assemble")
  expect_equal(f$tasks$exit, "2")
  expect_equal(f$tasks$cause, "the tool stopped with an error")
  expect_equal(f$tasks$stderr, "Error: no seed reads found")
  expect_equal(f$stats[["ignored"]], 1L)

  a <- nf_log_facts(fx("annotate_failures"))
  expect_equal(a$entry, "WF2")
  expect_equal(a$tasks$item, "SRR22396794.1.1")
  expect_equal(a$tasks$sample, "SRR22396794")
  expect_equal(a$samples, "SRR22396794")
})

test_that("a fatal error counts as a finish even with no Goodbye", {
  f <- nf_log_facts(fx("fatal"))
  expect_true(f$fatal)
  expect_true(f$finished)
  expect_equal(f$error, "Missing process or function getAt([0])")
  d <- withr::local_tempdir()
  l <- file.path(d, "l.log")
  writeLines(c("Jul-15 13:19:58.512 [main] ERROR nextflow.cli.Launcher - Unable to acquire lock on session with ID 1"), l)
  expect_true(nf_log_facts(l)$fatal)
  expect_match(nf_log_facts(l)$error, "Unable to acquire lock")
})

test_that("an unfinished, empty, or garbled log is not finished and does not error", {
  expect_false(nf_log_facts(fx("unfinished"))$finished)
  d <- withr::local_tempdir()
  e <- file.path(d, "e.log"); file.create(e)
  g <- file.path(d, "g.log"); writeBin(as.raw(c(0xff, 0xfe, 0x0a, 0x00, 0x41, 0x0a)), g)
  for (l in c(e, g, file.path(d, "missing.log"))) {
    f <- nf_log_facts(l)
    expect_false(f$finished)
    expect_false(f$readable)
    expect_equal(nrow(f$tasks), 0L)
  }
})

test_that("tag_sample strips path, scaffold, and accession suffixes", {
  expect_equal(tag_sample(c("S1", "S1.2", "S1.1.1", "S1.1.1.NC_062373.1", "t2_MG6_1")),
    c("S1", "S1", "S1", "S1", "t2_MG6_1"))
  expect_equal(tag_sample(c("S.1.1", "S.1.1.1"), ids = c("S", "S.1")), c("S.1", "S.1"))
})

test_that("exit codes map to a likely cause", {
  expect_match(nf_exit_cause(137L), "out of memory")
  expect_match(nf_exit_cause(140L), "scheduler")
  expect_match(nf_exit_cause(NA), "no exit status")
  expect_equal(nf_exit_cause(1L), "the tool stopped with an error")
})

test_that("run_report classifies the result", {
  d <- local_runs_project(c(
    assemble_2026 = "clean", assemble_2027 = "failures", assemble_2028 = "fatal",
    annotate_2029 = "unfinished"))
  log <- function(b) file.path(run_dir(d, "nextflow"), paste0(b, ".nextflow.log"))
  expect_equal(run_report(d, log("assemble_2026"))$meta$result, "finished")
  expect_equal(run_report(d, log("assemble_2027"))$meta$result, "failures")
  expect_equal(run_report(d, log("assemble_2028"))$meta$result, "failed")
  expect_equal(run_report(d, log("assemble_2026"), exit_status = 1L)$meta$result, "failed")
  expect_equal(run_report(d, log("annotate_2029"), exit_status = 130L, how = "stopped")$meta$result,
    "stopped")

  r <- run_report(d, log("assemble_2027"))
  expect_setequal(names(r$meta), RUN_META_FIELDS)
  expect_equal(r$meta$workflow, "assemble")
  expect_equal(r$meta$entry, "WF1")
  expect_equal(r$meta$exit_status, 0L)
  expect_equal(r$meta$n_failed_tasks, 1L)
  expect_equal(r$meta$n_samples, 1L)
  expect_equal(r$meta$launch, "app")
  expect_equal(r$meta$nextflow_version, "25.10.6")
  expect_match(r$text, "t2_MG676469_1 | assemble | 2 | the tool stopped with an error", fixed = TRUE)
  expect_match(r$text, "Result:    Finished with failures, exit status 0", fixed = TRUE)

  f <- run_report(d, log("assemble_2028"))
  expect_equal(f$meta$exit_status, 1L)
  expect_match(f$text, "Error:     Missing process", fixed = TRUE)
})

test_that("the scheduler log trailer gives the exit status and marks a job", {
  d <- local_runs_project(c(annotate_2026 = "unfinished"))
  dir.create(run_dir(d, "logs")); dir.create(run_dir(d, "jobs"))
  writeLines("#!/bin/sh", file.path(run_dir(d, "jobs"), "annotate_2026.sh"))
  writeLines(c("--- job ---", "[MitoPilot] nextflow exited with status 143"),
    file.path(run_dir(d, "logs"), "annotate_2026.log"))
  expect_equal(read_exit_trailer(d, "annotate_2026"), 143L)
  r <- run_report(d, file.path(run_dir(d, "nextflow"), "annotate_2026.nextflow.log"))
  expect_equal(r$meta$result, "failed")
  expect_equal(r$meta$exit_status, 143L)
  expect_equal(r$meta$launch, "job")
  expect_equal(sync_run_reports(d), "annotate_2026")
})

test_that("a garbled log still gives a report when the exit status is known", {
  d <- withr::local_tempdir()
  dir.create(run_dir(d, "nextflow"), recursive = TRUE)
  l <- file.path(run_dir(d, "nextflow"), "assemble_2026-09-29_10-00-00.nextflow.log")
  writeBin(as.raw(c(0xff, 0xfe, 0x0a)), l)
  r <- run_report(d, l, exit_status = 1L)
  expect_equal(r$meta$result, "failed")
  expect_equal(r$meta$started, "2026-09-29 10:00:00")
  expect_match(r$text, "(could not be read)", fixed = TRUE)
})

test_that("run_failed_samples lists Failed rows stamped inside the run", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "CREATE TABLE assemble(ID TEXT, assemble_switch INT, assemble_notes TEXT, time_stamp INT)")
  DBI::dbExecute(con, "INSERT INTO assemble VALUES ('NEW', 3, 'failed assembly', 200),
    ('OLD', 3, 'x', 50), ('OK', 2, NULL, 200), ('LATE', 3, 'y', 900)")
  DBI::dbExecute(con, "CREATE TABLE annotate(ID TEXT, path INT, scaffold INT, annotate_switch INT,
    annotate_notes TEXT, time_stamp INT)")
  DBI::dbExecute(con, "INSERT INTO annotate VALUES ('A', 1, 2, 3, NULL, 150)")
  DBI::dbExecute(con, "CREATE TABLE old(ID TEXT)")
  bad <- run_failed_samples(con, "assemble", 100, 500)
  expect_equal(bad$ID, "NEW")
  expect_equal(bad$note, "failed assembly")
  expect_equal(run_failed_samples(con, "annotate", 100, 500)$ID, "A.1.2")
  expect_equal(nrow(run_failed_samples(NULL, "assemble", 100)), 0L)
  expect_equal(nrow(run_failed_samples(con, "assemble", NA)), 0L)
  DBI::dbExecute(con, "ALTER TABLE assemble RENAME COLUMN time_stamp TO ts")
  expect_equal(nrow(run_failed_samples(con, "assemble", 100, 500)), 0L)
})

test_that("failed samples from the project DB land in the report", {
  d <- local_runs_project(c(assemble_2026 = "clean"))
  log <- file.path(run_dir(d, "nextflow"), "assemble_2026.nextflow.log")
  start <- as.numeric(nf_log_facts(log)$started)
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(d, ".sqlite"))
  DBI::dbExecute(con, "CREATE TABLE assemble(ID TEXT, assemble_switch INT, assemble_notes TEXT, time_stamp INT)")
  DBI::dbExecute(con, "INSERT INTO assemble VALUES (?, 3, 'no seed reads', ?)",
    params = list("SRR22396640", start + 2))
  DBI::dbDisconnect(con)
  r <- run_report(d, log)
  expect_equal(r$meta$result, "failures")
  expect_equal(r$meta$n_failed_samples, 1L)
  expect_match(r$text, "SRR22396640 | no seed reads", fixed = TRUE)
})

test_that("sync writes each finished run once and list_run_reports filters by workflow", {
  d <- local_runs_project(c(
    "assemble_2026-09-01_10-00-00" = "clean",
    "assemble_2026-09-02_10-00-00" = "failures",
    "annotate_2026-09-03_10-00-00" = "annotate_failures",
    "annotate_2026-09-04_10-00-00" = "unfinished"))
  w <- sync_run_reports(d)
  expect_setequal(w, c("assemble_2026-09-01_10-00-00", "assemble_2026-09-02_10-00-00",
    "annotate_2026-09-03_10-00-00"))
  expect_true(file.exists(file.path(run_dir(d, "reports"), "assemble_2026-09-01_10-00-00_report.txt")))
  expect_equal(sync_run_reports(d), character(0))

  a <- list_run_reports(d, "assemble")
  expect_equal(a$base, c("assemble_2026-09-02_10-00-00", "assemble_2026-09-01_10-00-00"))
  expect_equal(a$result, c("failures", "finished"))
  expect_type(a$n_failed_tasks, "integer")
  expect_true(all(file.exists(a$report)))

  n <- list_run_reports(d, "annotate")
  expect_equal(n$result, c("unfinished", "failures"))
  expect_true(is.na(n$report[1]))
  expect_equal(n$started[1], "2026-09-04 10:00:00")
  expect_equal(nrow(list_run_reports(d)), 4L)
})

test_that("a project with no .runs/ gives empty results, never an error", {
  d <- withr::local_tempdir()
  expect_equal(sync_run_reports(d), character(0))
  l <- list_run_reports(d, "assemble")
  expect_equal(nrow(l), 0L)
  expect_true(all(c(RUN_META_FIELDS, "report") %in% names(l)))
  expect_false(dir.exists(run_dir(d)))
})

test_that("nextflow_cmd logs to .runs/nextflow and resumes only with a Nextflow cache", {
  d <- withr::local_tempdir()
  cmd <- nextflow_cmd("assemble", path = d, source = "main.nf", base = "assemble_x")
  expect_equal(cmd[2], file.path(d, ".runs", "nextflow", "assemble_x.nextflow.log"))
  expect_false("-resume" %in% cmd)
  expect_false(dir.exists(run_dir(d)))
  dir.create(file.path(d, ".logs")); file.create(file.path(d, ".logs", "nextflow.log"))
  expect_false("-resume" %in% nextflow_cmd("assemble", path = d, source = "main.nf"))
  dir.create(file.path(d, ".nextflow", "cache"), recursive = TRUE)
  cmd <- nextflow_cmd("annotate", path = d, source = "main.nf")
  expect_true("-resume" %in% cmd)
  expect_match(cmd[2], "annotate_\\d{4}-\\d{2}-\\d{2}_\\d{2}-\\d{2}-\\d{2}\\.nextflow\\.log$")
  expect_true("WF2" %in% cmd)
})

test_that("submit scripts end by echoing and returning Nextflow's exit status", {
  for (s in list(submission_script("slurm", NULL, "nextflow run x", "j", "/tmp/j.log", nxf_ver = NA),
                 hydra_submission_script("nextflow run x", "j", "/tmp/j.log", nxf_ver = NA))) {
    i <- which(s == "nextflow run x")
    expect_equal(s[i + 1], "status=$?")
    expect_equal(s[i + 2], 'echo "[MitoPilot] nextflow exited with status $status"')
    expect_equal(utils::tail(s, 1), "exit $status")
  }
})

test_that("sync skips the live run, rewrites a broken report, and waits for a job's trailer", {
  b1 <- "assemble_2026-09-01_10-00-00"; b2 <- "assemble_2026-09-02_10-00-00"
  d <- local_runs_project(stats::setNames(c("clean", "clean"), c(b1, b2)))
  expect_equal(sync_run_reports(d, skip = b1), b2)
  expect_equal(sync_run_reports(d), b1)

  file.create(run_report_paths(d, b1)[["dcf"]])
  expect_equal(sync_run_reports(d), b1)

  b3 <- "annotate_2026-09-03_10-00-00"
  file.copy(fx("clean"), file.path(run_dir(d, "nextflow"), paste0(b3, ".nextflow.log")))
  dir.create(run_dir(d, "jobs")); dir.create(run_dir(d, "logs"))
  writeLines(c("nextflow run x", nf_exit_trailer()), file.path(run_dir(d, "jobs"), paste0(b3, ".sh")))
  expect_equal(sync_run_reports(d), character(0))
  writeLines(c("[MitoPilot] nextflow exited with status 1", "--- MitoPilot job done ---"),
             file.path(run_dir(d, "logs"), paste0(b3, ".log")))
  expect_equal(sync_run_reports(d), b3)
  r <- list_run_reports(d, "annotate")
  expect_equal(r$result, "failed")
  expect_equal(r$launch, "job")
})
