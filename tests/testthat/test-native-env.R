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
