test_that("read_config_executor parses executor and queue from a config", {
  cfg <- tempfile()
  writeLines(c(
    "executor {",
    "    queueSize = 50",
    "}",
    "process {",
    "  executor = 'slurm'",
    "  queue = 'general'",
    "  preprocess {",
    "    executor = process.executor",
    "  }",
    "}"
  ), cfg)

  out <- read_config_executor(cfg)
  expect_equal(out$executor, "slurm")
  expect_equal(out$queue, "general")
})

test_that("read_config_executor defaults executor to local and drops placeholder queue", {
  cfg <- tempfile()
  writeLines(c(
    "process {",
    "  queue = '<<QUEUE>>'",
    "}"
  ), cfg)

  out <- read_config_executor(cfg)
  expect_equal(out$executor, "local")
  expect_null(out$queue)
})

test_that("read_config_executor parses each built-in scheduler template", {
  # template file name -> declared Nextflow executor
  expected <- c(slurm = "slurm", sge = "sge", pbs = "pbspro", lsf = "lsf")
  for (sched in names(expected)) {
    path <- app_sys(paste0("config.", sched))
    skip_if(!nzchar(path) || !file.exists(path))
    expect_equal(read_config_executor(path)$executor, expected[[sched]])
  }
})

test_that("submission_script maps pbspro to PBS directives", {
  lines <- submission_script(
    executor = "pbspro",
    queue = NULL,
    full_nf_cmd = "nextflow run foo",
    job_name = "j",
    log_file = "/tmp/j.log"
  )
  expect_true(any(grepl("#PBS", lines)))
})

test_that("submission_script emits the right directive prefix per scheduler", {
  prefixes <- c(slurm = "#SBATCH", sge = "#\\$", pbs = "#PBS", lsf = "#BSUB")
  for (sched in names(prefixes)) {
    lines <- submission_script(
      executor = sched,
      queue = "myq",
      full_nf_cmd = "nextflow run foo -entry WF1",
      job_name = "assemble_2026",
      log_file = "/tmp/assemble_2026.log"
    )
    expect_equal(lines[1], "#!/bin/sh")
    expect_true(any(grepl(prefixes[[sched]], lines)))
    expect_true(any(grepl("nextflow run foo -entry WF1", lines, fixed = TRUE)))
    expect_true(any(grepl("myq", lines, fixed = TRUE)))
  }
})

test_that("submission_script omits queue directive when queue is NULL", {
  lines <- submission_script(
    executor = "slurm",
    queue = NULL,
    full_nf_cmd = "nextflow run foo",
    job_name = "j",
    log_file = "/tmp/j.log"
  )
  expect_false(any(grepl("#SBATCH -p", lines, fixed = TRUE)))
})

test_that("submission_script falls back to a comment for unknown executors", {
  lines <- submission_script(
    executor = "local",
    queue = NULL,
    full_nf_cmd = "nextflow run foo",
    job_name = "j",
    log_file = "/tmp/j.log"
  )
  expect_true(any(grepl("No HPC scheduler resource block", lines)))
  expect_true(any(grepl("nextflow run foo", lines, fixed = TRUE)))
})
