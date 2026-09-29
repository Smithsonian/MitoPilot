test_that("panels map to run workflows", {
  expect_equal(run_reports_workflow("Assemble"), "assemble")
  expect_equal(run_reports_workflow("Annotate"), "annotate")
  expect_null(run_reports_workflow("Export"))
  expect_null(run_reports_workflow(NULL))
})

test_that("the notice opens the active panel if it has new reports, else the newest", {
  b <- c("assemble_2026-09-01_10-00-00", "annotate_2026-09-02_10-00-00")
  expect_equal(run_reports_notice_workflow("Assemble", b), "assemble")
  expect_equal(run_reports_notice_workflow("Annotate", b), "annotate")
  expect_equal(run_reports_notice_workflow("Export", b), "annotate")
  expect_equal(run_reports_notice_workflow("Annotate", b[1]), "assemble")
})

test_that("result cells carry the label", {
  h <- run_result_html(c("finished", "failed", NA))
  expect_match(h[1], "Finished")
  expect_match(h[2], "mp-pill-danger")
  expect_equal(h[3], "")
})

run_reports_session <- function(dir, mode = "Assemble") {
  ms <- shiny::MockShinySession$new()
  ms$userData$dir <- dir
  ms$userData$mode <- mode
  for (f in c("refresh_assemble", "refresh_annotate")) gargoyle::init(f, session = ms)
  ms
}

test_that("run reports module writes, lists, and shows a finished run", {
  d <- withr::local_tempdir()
  dir.create(run_dir(d, "nextflow"), recursive = TRUE)
  base <- "assemble_2026-09-08_12-51-00"
  file.copy(test_path("fixtures", "runs", "clean.nextflow.log"),
            file.path(run_dir(d, "nextflow"), paste0(base, ".nextflow.log")))
  ms <- run_reports_session(d)
  shiny::testServer(run_reports_server, args = list(id = "rr"), session = ms, {
    session$userData$run_reports_check()
    expect_true(file.exists(run_report_paths(d, base)[["txt"]]))
    expect_equal(session$userData$run_reports_new, base)
    session$setInputs(open = 1)
    expect_equal(runs()$base, base)
    expect_match(output$tbl, "New")
    session$setInputs(tbl__reactable__selected = 1)
    expect_match(output$detail$html, "MitoPilot run report")
    # a second check writes nothing new
    expect_equal(sync_run_reports(d), character(0))
  })
})

test_that("the notice opens the panel of the new report", {
  d <- withr::local_tempdir()
  dir.create(run_dir(d, "nextflow"), recursive = TRUE)
  base <- "annotate_2026-09-08_12-51-00"
  file.copy(test_path("fixtures", "runs", "clean.nextflow.log"),
            file.path(run_dir(d, "nextflow"), paste0(base, ".nextflow.log")))
  ms <- run_reports_session(d, "Assemble")
  shiny::testServer(run_reports_server, args = list(id = "rr"), session = ms, {
    gargoyle::trigger("refresh_assemble", session = ms)
    session$flushReact()
    session$setInputs(notice = TRUE)
    expect_equal(runs()$base, base)
  })
})

test_that("run reports module copes with projects without .runs", {
  d <- withr::local_tempdir()
  ms <- run_reports_session(d, "Annotate")
  shiny::testServer(run_reports_server, args = list(id = "rr"), session = ms, {
    expect_no_error(session$userData$run_reports_check())
    session$setInputs(open = 1)
    expect_equal(nrow(runs()), 0L)
    session$userData$mode <- "Export"
    session$setInputs(open = 2)
    expect_null(runs())
  })
})

test_that("run reports button builds", {
  expect_match(as.character(run_reports_ui("rr")), "Run Reports")
})

test_that("the run modal names its log after the run, and a finished run gets a report", {
  run_one <- function(maker, srv) {
    proj <- withr::local_tempdir()
    suppressMessages(maker(path = proj, executor = "local", Rproj = FALSE))
    con <- DBI::dbConnect(RSQLite::SQLite(), file.path(proj, ".sqlite"))
    withr::defer(DBI::dbDisconnect(con))
    withr::local_options(MitoPilot.db = file.path(proj, ".sqlite"))
    ms <- shiny::MockShinySession$new()
    ms$userData$con <- con
    ms$userData$mode <- "Assemble"
    gargoyle::init("run_modal", "refresh_assemble", session = ms)
    shiny::testServer(srv, args = list(id = "run"), session = ms, {
      session$flushReact()
      gargoyle::trigger("run_modal", session = ms)
      session$flushReact()
      b <- run_base()
      expect_match(b, "^assemble_")
      expect_true(any(grepl(paste0(".runs/nextflow/", b, ".nextflow.log"), nf_cmd(), fixed = TRUE)))

      log <- file.path(run_dir(proj, "nextflow"), paste0(b, ".nextflow.log"))
      dir.create(dirname(log), recursive = TRUE)
      file.copy(test_path("fixtures", "runs", "clean.nextflow.log"), log)
      # a second launch from the same modal gets its own name
      Sys.sleep(1.1)
      expect_false(file.exists(claim_run_base()))
      expect_false(identical(run_base(), b))

      assign("run_log", log, envir = environment(end_run))
      p <- processx::process$new("true")
      p$wait()
      end_run(p, notify = FALSE)
      m <- read.dcf(run_report_paths(proj, b)[["dcf"]])
      expect_equal(m[1, "result"][[1]], "finished")
      expect_equal(m[1, "launch"][[1]], "app")
    })
  }
  run_one(function(...) new_test_project(n = 2, ...), pipeline_server)
  run_one(new_test_project_userAsmb, pipeline_server_userAsmb)
})

test_that("a saved submit template keeps the Nextflow command as a token", {
  b <- "assemble_2026-09-29_10-00-00"
  nfc <- paste0("nextflow -log /p/.runs/nextflow/", b, ".nextflow.log run x -resume")
  logf <- paste0("/p/.runs/logs/", b, ".log")
  tk <- tokenize_submit_script(c(paste("#SBATCH -J", b), paste("#SBATCH -o", logf), nfc), nfc, b, logf)
  expect_equal(tk, c("#SBATCH -J <<JOB_NAME>>", "#SBATCH -o <<LOG_FILE>>", "<<NEXTFLOW_CMD>>"))
})
