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
  withr::local_envvar(c(JAVA_HOME = NA))
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

fake_prefix <- function(tools) {
  prefix <- withr::local_tempdir(.local_envir = parent.frame())
  bin <- file.path(prefix, "envs", "mitopilot", "bin")
  dir.create(bin, recursive = TRUE)
  for (t in tools) {
    f <- file.path(bin, t)
    writeLines(c("#!/bin/sh", paste0("echo ", t, " 9.9.9")), f)
    Sys.chmod(f, "0755")
  }
  writeLines(c(paste0("export PATH=", bin, ":$PATH"), "export MITOPILOT_NO_CONDA=1"),
             file.path(prefix, "activate.sh"))
  prefix
}

core_tools <- c("R", "Rscript", "nextflow", "java", "fastp", "get_organelle_from_reads.py",
                "bowtie2", "bwa", "samtools", "minimap2", "blastn", "blastdbcmd",
                "makeblastdb", "runmitos", "tRNAscan-SE", "aragorn", "bam-readcount",
                "parallel", "file")

test_that("native_check reports every core tool found and optional ones missing", {
  skip_on_os(c("windows", "mac"))
  prefix <- fake_prefix(core_tools)
  withr::local_envvar(c(PATH = Sys.getenv("PATH")))
  res <- native_check(prefix, strict = FALSE)
  expect_true(all(res$found[res$required]))
  expect_false(any(res$found[res$tool %in% c("mitofinder", "arwen", "ORFfinder")]))
  expect_true(any(grepl("9.9.9", res$version[res$tool == "fastp"])))
})

test_that("native_check errors when a core tool is missing and strict", {
  skip_on_os(c("windows", "mac"))
  prefix <- fake_prefix(setdiff(core_tools, "fastp"))
  expect_error(native_check(prefix, strict = TRUE), "fastp")
})

test_that("native_setup applies PATH and the no-conda flag to the session", {
  skip_on_os(c("windows", "mac"))
  prefix <- fake_prefix(core_tools)
  withr::local_envvar(c(PATH = Sys.getenv("PATH"), MITOPILOT_NO_CONDA = "",
                        MITOPILOT_NATIVE_PREFIX = ""))
  suppressMessages(native_setup(prefix))
  expect_true(startsWith(Sys.getenv("PATH"), file.path(prefix, "envs", "mitopilot", "bin")))
  expect_equal(Sys.getenv("MITOPILOT_NO_CONDA"), "1")
})
