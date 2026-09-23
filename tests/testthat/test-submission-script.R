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

test_that("submit_command maps executors to schedulers", {
  expect_equal(submit_command("slurm"), "sbatch")
  expect_equal(submit_command("sge"), "qsub")
  expect_equal(submit_command("pbspro"), "qsub")
  expect_equal(submit_command("lsf"), "bsub")
  expect_null(submit_command("local"))
})

test_that("submit template round-trips resource edits, regenerating dynamic bits", {
  wd <- tempfile()
  dir.create(wd)

  nf1 <- "nextflow run x -entry WF1 -resume"
  job1 <- "assemble_2026-01-01_00-00-00"
  log1 <- file.path(wd, paste0(job1, ".log"))

  # default script, then user edits the resource block + adds a module load
  script <- submission_script("slurm", "general", nf1, job1, log1)
  script <- sub("#SBATCH --mem=16G", "#SBATCH --mem=64G", script, fixed = TRUE)
  script <- c(script, "module load my/custom")

  save_submit_template(paste(script, collapse = "\n"), wd, nf1, job1, log1)
  expect_true(file.exists(submit_template_path(wd)))

  # next run: different workflow / job name / log, same saved resources
  nf2 <- "nextflow run x -entry WF2"
  job2 <- "annotate_2026-02-02_11-11-11"
  log2 <- file.path(wd, paste0(job2, ".log"))
  rebuilt <- build_submit_script(wd, "slurm", "general", nf2, job2, log2)

  expect_true(any(grepl("#SBATCH --mem=64G", rebuilt, fixed = TRUE)))
  expect_true(any(grepl("module load my/custom", rebuilt, fixed = TRUE)))
  expect_true(any(grepl(nf2, rebuilt, fixed = TRUE)))
  expect_true(any(grepl(job2, rebuilt, fixed = TRUE)))
  expect_false(any(grepl(job1, rebuilt, fixed = TRUE)))
  expect_false(any(grepl("-entry WF1", rebuilt, fixed = TRUE)))
})

test_that("build_submit_script falls back to default when no template saved", {
  wd <- tempfile()
  dir.create(wd)
  out <- build_submit_script(wd, "sge", NULL, "nextflow run x", "j", "/tmp/j.log")
  expect_equal(out[1], "#!/bin/sh")
  expect_true(any(grepl("#\\$ -N", out)))
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

test_that("read_config_executor returns native_activate when present", {
  cfg <- tempfile()
  writeLines(c("params.native_activate = '/opt/mp/activate.sh'",
               "process {", "  executor = 'slurm'", "}"), cfg)
  out <- read_config_executor(cfg)
  expect_equal(out$native_activate, "/opt/mp/activate.sh")
  cfg2 <- tempfile(); writeLines("process { executor = 'local' }", cfg2)
  expect_null(read_config_executor(cfg2)$native_activate)
})

test_that("submission_script sources the native env instead of commented examples", {
  lines <- submission_script("slurm", NULL, "nextflow run foo", "j", "/tmp/j.log",
                             env_setup = "/opt/mp/activate.sh")
  expect_true(any(lines == "source '/opt/mp/activate.sh'"))
  expect_false(any(grepl("# mamba activate MitoPilot_deps", lines, fixed = TRUE)))
  plain <- submission_script("slurm", NULL, "nextflow run foo", "j", "/tmp/j.log")
  expect_true(any(grepl("# mamba activate MitoPilot_deps", plain, fixed = TRUE)))
})

test_that("build_submit_script reads native_activate from the project config", {
  skip_on_os("windows")
  wd <- withr::local_tempdir()
  prefix <- withr::local_tempdir()
  bin <- file.path(prefix, "bin")
  dir.create(bin, recursive = TRUE)
  nf <- file.path(bin, "nextflow")
  writeLines(c("#!/bin/sh", "echo 'nextflow version 25.10.4'"), nf)
  Sys.chmod(nf, "0755")
  act <- file.path(prefix, "activate.sh")
  writeLines(paste0("export PATH=", bin, ":$PATH"), act)
  writeLines(c(paste0("params.native_activate = '", act, "'"),
               "process { executor = 'slurm' }"), file.path(wd, ".config"))
  lines <- build_submit_script(wd, "slurm", NULL, "nextflow run foo", "j", "/tmp/j.log")
  expect_true(any(lines == paste0("source '", act, "'")))
})

test_that("build_submit_script on Hydra sources activate.sh for a native project", {
  skip_on_os("windows")
  wd <- withr::local_tempdir()
  prefix <- withr::local_tempdir()
  bin <- file.path(prefix, "bin")
  dir.create(bin, recursive = TRUE)
  nf <- file.path(bin, "nextflow")
  writeLines(c("#!/bin/sh", "echo 'nextflow version 25.10.4'"), nf)
  Sys.chmod(nf, "0755")
  act <- file.path(prefix, "activate.sh")
  writeLines(paste0("export PATH=", bin, ":$PATH"), act)
  writeLines(c(paste0("params.native_activate = '", act, "'"),
               "process { executor = 'sge' }"), file.path(wd, ".config"))
  local_mocked_bindings(is_hydra_cluster = function() TRUE)
  lines <- build_submit_script(wd, "sge", NULL, "nextflow run foo", "j", "/tmp/j.log")
  expect_true(any(lines == "#$ -q lTWFM.sq"))
  expect_true(any(lines == paste0("source '", act, "'")))
  expect_false(any(grepl("module load", lines)))
  expect_true(any(lines == "export NXF_VER=25.10.4"))
})
