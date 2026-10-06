nf_lines <- list(
  start = c("Sep-29 17:06:22.109 [main] DEBUG nextflow.processor.TaskProcessor - Starting process > WF1:PRE:preprocess",
            "Sep-29 17:06:22.110 [main] DEBUG nextflow.processor.TaskProcessor - Starting process > WF1:ASM:assemble"),
  sub = c("Sep-29 17:06:22.223 [Task submitter] INFO  nextflow.Session - [f0/64f1a0] Submitted process > WF1:PRE:preprocess (S1)",
          "Sep-29 17:06:22.224 [Task submitter] INFO  nextflow.Session - [a1/000001] Submitted process > WF1:PRE:preprocess (S2)",
          "Sep-29 17:06:22.225 [Task submitter] INFO  nextflow.Session - [b2/000002] Cached process > WF1:ASM:assemble (S3)"),
  done = c("Sep-29 17:06:23.000 [Task monitor] DEBUG n.processor.TaskPollingMonitor - Task completed > TaskHandler[id: 1; name: WF1:PRE:preprocess (S1); status: COMPLETED; exit: 0; error: -; workDir: /w/f0]",
           "Sep-29 17:06:24.000 [Task monitor] DEBUG n.processor.TaskPollingMonitor - Task completed > TaskHandler[id: 2; name: WF1:PRE:preprocess (S2); status: COMPLETED; exit: 137; error: -; workDir: /w/a1]"),
  end = "Sep-29 17:06:30.000 [main] DEBUG nextflow.cli.Launcher - Execution complete -- Goodbye"
)
mk_log <- function(...) {
  f <- tempfile(fileext = ".nextflow.log")
  writeLines(c("Sep-29 17:06:20.000 [main] DEBUG nextflow.cli.Launcher - $> nextflow run main.nf", ...), f)
  f
}

test_that("progress counts tasks per process like Nextflow", {
  f <- mk_log(nf_lines$start, nf_lines$sub[1:2])
  p <- run_progress(f)
  expect_equal(p$process, c("WF1:PRE:preprocess", "WF1:ASM:assemble"))
  expect_equal(p$running, c(2L, 0L))
  txt <- run_progress_text(f, now = file.mtime(f))
  expect_match(txt, "Tasks in progress: 2")
  expect_match(txt, "WF1:PRE:preprocess +\\| 0 of 2, running: 2")
  f <- mk_log(nf_lines$start, nf_lines$sub, nf_lines$done)
  txt <- run_progress_text(f, now = file.mtime(f))
  expect_match(txt, "\\[100%\\] WF1:PRE:preprocess +\\| 2 of 2, failed: 1")
  expect_match(txt, "WF1:ASM:assemble +\\| 1 of 1, cached: 1")
})

test_that("a run with nothing started yet says so", {
  f <- mk_log()
  expect_match(run_progress_text(f, now = file.mtime(f)), "no samples have started processing yet")
  f <- mk_log(nf_lines$start)
  txt <- run_progress_text(f, now = file.mtime(f))
  expect_match(txt, "no samples have started processing yet")
  expect_match(txt, "WF1:ASM:assemble +\\| -")
})

test_that("a finished run without a report, and a stalled run, are named", {
  f <- mk_log(nf_lines$start, nf_lines$sub, nf_lines$done, nf_lines$end)
  expect_match(run_progress_text(f, now = file.mtime(f)), "Nextflow has finished, but the run report is not written yet")
  f <- mk_log(nf_lines$start, nf_lines$sub[1:2])
  expect_match(run_progress_text(f, now = file.mtime(f) + 7200), "Nextflow may have stopped")
  expect_equal(run_progress_text(file.path(tempdir(), "nope.log")), "The Nextflow log could not be read.")
})
