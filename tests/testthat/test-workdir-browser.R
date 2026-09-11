test_that("find_workdirs keeps every workflow's tasks for a sample", {
  d <- withr::local_tempdir()
  dir.create(file.path(d, ".logs"))
  w <- file.path(d, "work"); dir.create(w)
  th <- function(name, wd) sprintf(
    "Sep-11 00:00:00.000 [x] DEBUG n.p.TaskProcessor - Task completed > TaskHandler[id: 1; name: %s; status: COMPLETED; exit: 0; error: -; workDir: %s]",
    name, wd)
  writeLines(c(
    th("WF1:ASSEMBLE:assemble (S1)", file.path(w, "a1")),
    th("WF1:ASSEMBLE:assemble (S1.1)", file.path(w, "a2")),
    th("WF2:ANNOTATE:annotate (S1.1.1)", file.path(w, "b1")),
    th("WF2:CURATE:curate (S1.1.1)", file.path(w, "c1")),
    th("WF2:CURATE:blast_ref_alignment (S1.1.1.NC_062373.1)", file.path(w, "d1")),
    th("WF2:CURATE:write_curated_result (S1)", file.path(w, "e1")),
    th("WF2:ANNOTATE:annotate (S10.1.1)", file.path(w, "f1")),
    th("WF1:ASSEMBLE:assemble (S2)", file.path(w, "g1"))
  ), file.path(d, ".logs", "nextflow.log"))

  out <- find_workdirs(d, "S1")
  expect_setequal(out$process, c("assemble", "annotate", "curate", "blast_ref_alignment"))
  expect_equal(nrow(out), 5L)
  expect_false(any(grepl("f1|g1|e1", out$workdir)))
})
