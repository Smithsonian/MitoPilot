# helper: pull the process-level executor from a generated config
nf_executor <- function(path) {
  txt <- readLines(path)
  line <- grep("^\\s*executor = '", txt, value = TRUE)[1]
  sub(".*executor = '([^']+)'.*", "\\1", line)
}

test_that("generate_config saves a reusable slurm profile", {
  pdir <- tempfile()
  out <- generate_config(
    name = "my_cluster",
    scheduler = "slurm",
    queue = "general",
    account = "myacct",
    container_engine = "apptainer",
    container_cache = "/scratch/sif",
    profile_dir = pdir
  )
  expect_true(file.exists(out))
  txt <- readLines(out)

  # cluster-level placeholders filled
  expect_true(any(grepl("executor = 'slurm'", txt, fixed = TRUE)))
  expect_true(any(grepl("queue = 'general'", txt, fixed = TRUE)))
  expect_true(any(grepl("--account=myacct", txt, fixed = TRUE)))
  expect_true(any(grepl("apptainer {", txt, fixed = TRUE)))
  expect_true(any(grepl("cacheDir = '/scratch/sif'", txt, fixed = TRUE)))

  # per-project placeholders intentionally left for new_project()
  expect_true(any(grepl("<<RAW_DIR>>", txt, fixed = TRUE)))
  expect_true(any(grepl("<<MIN_DEPTH>>", txt, fixed = TRUE)))

  # cluster-level placeholders fully resolved
  expect_false(any(grepl("<<CONTAINER_ENGINE>>", txt, fixed = TRUE)))
  expect_false(any(grepl("<<QUEUE>>", txt, fixed = TRUE)))
  expect_false(any(grepl("<<CLUSTER_OPTIONS>>", txt, fixed = TRUE)))
})

test_that("list_configs reports built-ins and saved profiles", {
  pdir <- tempfile()
  generate_config("clusterA", scheduler = "sge", profile_dir = pdir)
  cfgs <- list_configs(profile_dir = pdir)
  expect_true("clusterA" %in% cfgs$name)
  expect_equal(cfgs$type[cfgs$name == "clusterA"], "saved")
  # a known built-in is present too
  expect_true("local" %in% cfgs$name)
})

test_that("resolve_config prefers a saved profile over a built-in", {
  pdir <- tempfile()
  # shadow the built-in 'slurm' name with a saved profile
  generate_config("slurm", scheduler = "slurm", queue = "q", profile_dir = pdir)
  resolved <- resolve_config("slurm", profile_dir = pdir)
  expect_equal(normalizePath(resolved), normalizePath(file.path(pdir, "config.slurm")))
})

test_that("resolve_config errors on unknown executor", {
  pdir <- tempfile()
  expect_error(resolve_config("does_not_exist", profile_dir = pdir), "No config found")
})

test_that("overwrite guard protects existing profiles", {
  pdir <- tempfile()
  generate_config("dup", scheduler = "lsf", profile_dir = pdir)
  expect_error(generate_config("dup", scheduler = "lsf", profile_dir = pdir), "already exists")
  expect_silent(suppressMessages(
    generate_config("dup", scheduler = "lsf", profile_dir = pdir, overwrite = TRUE)
  ))
})

test_that("queue directive is dropped when no queue is given", {
  pdir <- tempfile()
  out <- generate_config("noq", scheduler = "pbs", profile_dir = pdir)
  txt <- readLines(out)
  expect_false(any(grepl("queue =", txt, fixed = TRUE)))
})

test_that("each scheduler template produces the right executor", {
  pdir <- tempfile()
  expect_equal(nf_executor(generate_config("s", "slurm", profile_dir = pdir)), "slurm")
  expect_equal(nf_executor(generate_config("g", "sge", profile_dir = pdir)), "sge")
  expect_equal(nf_executor(generate_config("p", "pbs", profile_dir = pdir)), "pbspro")
  expect_equal(nf_executor(generate_config("l", "lsf", profile_dir = pdir)), "lsf")
})

test_that("container_engine = 'none' writes a native block and drops the container", {
  pdir <- tempfile()
  out <- generate_config("nat", scheduler = "slurm", queue = "q",
                         container_engine = "none", native_prefix = "/opt/mp",
                         profile_dir = pdir)
  txt <- readLines(out)
  expect_true(any(grepl("process.beforeScript = 'source /opt/mp/activate.sh'", txt, fixed = TRUE)))
  expect_true(any(grepl("env.MITOPILOT_NO_CONDA = '1'", txt, fixed = TRUE)))
  expect_true(any(grepl("params.native_activate = '/opt/mp/activate.sh'", txt, fixed = TRUE)))
  expect_true(any(grepl("db_dir = '/opt/mp/ref_dbs/mito_metazoa'", txt, fixed = TRUE)))
  expect_false(any(grepl("<<CONTAINER_ID>>", txt, fixed = TRUE)))
  expect_true(any(grepl("^\\s*container = null$", txt)))
  expect_false(any(grepl("singularity {", txt, fixed = TRUE)))
  expect_false(any(grepl("docker {", txt, fixed = TRUE)))
  expect_false(any(grepl("<<CONTAINER_ENGINE>>", txt, fixed = TRUE)))
})

test_that("native mode works for the local scheduler too", {
  pdir <- tempfile()
  out <- generate_config("natloc", scheduler = "local", container_engine = "none",
                         native_prefix = "/opt/mp", profile_dir = pdir)
  txt <- readLines(out)
  expect_false(any(grepl("docker {", txt, fixed = TRUE)))
  expect_true(any(grepl("params.native_activate", txt, fixed = TRUE)))
})

test_that("native mode requires native_prefix", {
  expect_error(generate_config("x", scheduler = "slurm", container_engine = "none",
                               profile_dir = tempfile()), "native_prefix")
})

test_that("extract_container_engine preserves a native block on migration", {
  old <- c("process.beforeScript = 'source /opt/mp/activate.sh'",
           "env.MITOPILOT_NO_CONDA = '1'",
           "params.native_activate = '/opt/mp/activate.sh'",
           "process {", "  executor = 'slurm'", "}")
  expect_equal(extract_container_engine(old), native_config_block("/opt/mp"))
})

test_that("built-in local template still yields a docker block via new_project fill", {
  lines <- readLines(app_sys("config.local"))
  expect_true(any(grepl("<<CONTAINER_ENGINE>>", lines, fixed = TRUE)))
  filled <- fill_config(lines, list(CONTAINER_ENGINE = container_engine_block("docker")))
  expect_true(any(grepl("docker {", filled, fixed = TRUE)))
})

test_that("built-in slurm template yields a singularity block via new_project fill", {
  lines <- readLines(app_sys("config.slurm"))
  expect_true(any(grepl("<<CONTAINER_ENGINE>>", lines, fixed = TRUE)))
  filled <- fill_config(lines, list(CONTAINER_ENGINE = container_engine_block("singularity")))
  expect_true(any(grepl("singularity {", filled, fixed = TRUE)))
})

test_that("migrate_config keeps native mode when regenerating from a built-in template", {
  pdir <- tempfile(); proj <- tempfile(); dir.create(proj)
  prof <- generate_config("natmig", scheduler = "local", container_engine = "none",
                          native_prefix = "/opt/mp", profile_dir = pdir)
  old <- fill_config(readLines(prof), list(RAW_DIR = "/data", ASMB_DIR = "NA",
                                           MIN_DEPTH = "100", NCBI_API_KEY = ""))
  writeLines(old, file.path(proj, ".config"))
  expect_true(suppressMessages(migrate_config(proj, executor = "local", profile_dir = pdir)))
  txt <- readLines(file.path(proj, ".config"))
  expect_true(any(grepl("params.native_activate = '/opt/mp/activate.sh'", txt, fixed = TRUE)))
  expect_true(any(grepl("db_dir = '/opt/mp/ref_dbs/mito_metazoa'", txt, fixed = TRUE)))
  expect_true(any(grepl("^\\s*container = null$", txt)))
  expect_false(any(grepl("docker {", txt, fixed = TRUE)))
  expect_false(any(grepl("baked into the container", txt, fixed = TRUE)))
})
