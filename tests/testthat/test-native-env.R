bootstrap_path <- function() system.file("native", "bootstrap_native.sh", package = "MitoPilot")

test_that("bootstrap script parses and --dry-run lists every step without writing", {
  skip_on_os(c("windows", "mac"))
  skip_if(!nzchar(bootstrap_path()))
  expect_equal(system2("bash", c("-n", bootstrap_path())), 0L)
  prefix <- withr::local_tempdir()
  out <- system2("bash", c(bootstrap_path(), "--prefix", prefix, "--dry-run",
                           "--with-optional"), stdout = TRUE, stderr = TRUE)
  for (step in c("mitopilot", "mitos", "trnascan", "aragorn", "bamreadcount",
                 "orffinder", "mitofinder", "ARWEN", "MitoFinder", "ORFfinder",
                 "BLAST DB", "activate.sh")) {
    expect_true(any(grepl(step, out, fixed = TRUE)), info = step)
  }
  expect_false(file.exists(file.path(prefix, "activate.sh")))
})

test_that("bootstrap rejects an unknown manager and a missing prefix", {
  skip_on_os(c("windows", "mac"))
  skip_if(!nzchar(bootstrap_path()))
  expect_false(identical(
    system2("bash", c(bootstrap_path(), "--prefix", tempdir(), "--manager", "uv", "--dry-run")),
    0L))
  expect_false(identical(system2("bash", c(bootstrap_path(), "--dry-run")), 0L))
})

test_that(".mp_condaenv maps empty, none, NULL, and the no-conda env var to NULL", {
  withr::local_envvar(c(MITOPILOT_NO_CONDA = ""))
  expect_null(.mp_condaenv(NULL))
  expect_null(.mp_condaenv(""))
  expect_null(.mp_condaenv("none"))
  expect_null(.mp_condaenv("NULL"))
  expect_equal(.mp_condaenv("mitos"), "mitos")
  withr::local_envvar(c(MITOPILOT_NO_CONDA = "1"))
  expect_null(.mp_condaenv("mitos"))
  expect_null(.mp_condaenv("base"))
})

test_that("bam_readcount_cmd drops conda run in native mode", {
  withr::local_envvar(c(MITOPILOT_NO_CONDA = ""))
  expect_match(bam_readcount_cmd("a.fa", "m.bam", "c.tsv"),
               "^conda run -n bam-readcount bam-readcount -w1 -f a.fa m.bam > c.tsv$")
  withr::local_envvar(c(MITOPILOT_NO_CONDA = "1"))
  expect_match(bam_readcount_cmd("a.fa", "m.bam", "c.tsv"),
               "^bam-readcount -w1 -f a.fa m.bam > c.tsv$")
})

test_that("native_env captures PATH and exports from an activate script", {
  skip_on_os(c("windows", "mac"))
  act <- withr::local_tempfile(fileext = ".sh")
  writeLines(c("export PATH=/opt/mp/envs/mitopilot/bin:$PATH",
               "export MITOPILOT_NO_CONDA=1",
               "export NXF_HOME=/opt/mp/nextflow_home"), act)
  env <- native_env(act)
  expect_true(startsWith(env[["PATH"]], "/opt/mp/envs/mitopilot/bin:"))
  expect_equal(env[["MITOPILOT_NO_CONDA"]], "1")
  expect_equal(env[["NXF_HOME"]], "/opt/mp/nextflow_home")
  expect_false("JAVA_HOME" %in% names(env))
})

test_that("native_env errors clearly on a missing script", {
  expect_error(native_env("/nope/activate.sh"), "activate")
})
