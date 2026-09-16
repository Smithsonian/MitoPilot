test_that("no pipeline input is staged from projectDir", {
  # projectDir is the installed R library; staging a file from it adds a bind
  # mount that Docker Desktop refuses (exit 125). Inputs must come from launchDir.
  nf <- list.files(system.file("nextflow", package = "MitoPilot"),
                   pattern = "\\.nf$", recursive = TRUE, full.names = TRUE)
  expect_gt(length(nf), 0)
  hits <- nf[vapply(nf, function(f) any(grepl("projectDir", readLines(f, warn = FALSE))),
                    logical(1))]
  expect_equal(basename(hits), character(0))
})
