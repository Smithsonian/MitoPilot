test_that("Hydra setup runs only on Hydra when qsub is missing", {
  skip_if(nzchar(Sys.which("qsub")), "qsub on PATH here")
  ran <- FALSE
  local_mocked_bindings(hydra_setup = function() { ran <<- TRUE; invisible(TRUE) })
  local_mocked_bindings(is_hydra_cluster = function() FALSE)
  expect_false(ensure_hydra_setup())
  expect_false(ran)
  local_mocked_bindings(is_hydra_cluster = function() TRUE)
  expect_message(ensure_hydra_setup(), "Hydra detected")
  expect_true(ran)
})
