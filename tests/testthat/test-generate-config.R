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
