#' run_pipeline Server Functions
#'
#' @noRd

# Canonical workflow order (WF1 then WF2, plus userAsmb), by leaf process name.
# Used ONLY to render the progress board in workflow order; process identity is
# keyed by frame POSITION (see progress_update), never by this list.
MITOPILOT_PROCESS_ORDER <- c(
  # WF1 (Assemble)
  "preprocess", "assemble", "coverage", "coverage_userAsmb",
  "blast_genbank", "blast_ref_fetch", "blast_ref_stamp", "scaffold_join",
  # WF2 (Annotate)
  "annotate", "curate", "validate", "write_curated_result", "orf",
  "blast_ref_align"
)

# Resolve a Nextflow board token to its canonical leaf-process name, or NA when it
# can't be resolved unambiguously. Nextflow shrinks the name column when a long
# task tag widens a row, so a name may arrive full ("WF2:ANNOTATE:annotate"),
# truncated ("WF2...semble"), or collapsed to 1-2 chars ("WF2...te", ambiguous
# between annotate/curate/validate). Only used for best-effort render ordering;
# NA => keep frame position. Never used for row identity.
resolve_process_name <- function(token) {
  k <- sub("^.*:", "", token)                        # drop path prefix to last ':'
  stripped <- sub("^.*(\u2026|\\.\\.\\.)", "", k)     # drop leading ellipsis truncation
  if (nchar(stripped) < 3) return(NA_character_)
  hits <- MITOPILOT_PROCESS_ORDER[endsWith(MITOPILOT_PROCESS_ORDER, stripped)]
  if (length(hits) == 1) hits else NA_character_
}

# Render a position-keyed board (ordered list of list(line=, name=)) into workflow
# order: resolved names sort by MITOPILOT_PROCESS_ORDER; unresolved rows keep their
# frame (registration) position, which for our pipelines already equals workflow
# order. Returns the display lines.
render_progress_board <- function(board) {
  if (!length(board)) return(character(0))
  nm  <- vapply(board, function(x) x$name %||% NA_character_, character(1))
  ord <- match(nm, MITOPILOT_PROCESS_ORDER)
  na  <- is.na(ord)
  ord[na] <- length(MITOPILOT_PROCESS_ORDER) + which(na)  # preserve position order
  vapply(board[order(ord)], function(x) x$line, character(1))
}

pipeline_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    nf_cmd <- reactiveVal()
    process <- reactiveVal()
    process_out <- reactiveVal()
    job_submitting <- reactiveVal(FALSE)

    # Headless submission state (set when the run modal opens in headless mode)
    headless_base <- reactiveVal()   # job name / file stem
    headless_nf <- reactiveVal()     # full "nextflow ..." command string
    headless_exec <- reactiveVal()   # scheduler executor from .config
    headless_queue <- reactiveVal()  # queue from .config

    headless_work_dir <- function() dirname(getOption("MitoPilot.db") %||% ".")
    headless_log_file <- function() {
      file.path(headless_work_dir(), paste0(headless_base(), ".log"))
    }

    # Sync the editable script with the -resume toggle. Swap just the nextflow
    # command line so the user's other edits are preserved; full rebuild only if
    # that line is missing (e.g. first call or user removed it).
    refresh_headless_script <- function() {
      old_nfc <- headless_nf()
      new_nfc <- paste(c("nextflow", nf_cmd()), collapse = " ")
      headless_nf(new_nfc)
      current <- input$submit_script
      if (!is.null(current) && nzchar(current) &&
          !is.null(old_nfc) && grepl(old_nfc, current, fixed = TRUE)) {
        updated <- sub(old_nfc, new_nfc, current, fixed = TRUE)
      } else {
        updated <- paste(build_submit_script(
          headless_work_dir(), headless_exec(), headless_queue(),
          new_nfc, headless_base(), headless_log_file()
        ), collapse = "\n")
      }
      shiny::updateTextAreaInput(session, "submit_script", value = updated)
    }

    on("run_modal", {
      job_submitting(FALSE)
      # Generate Nextflow params ----
      nf_cmd(nextflow_cmd(session$userData$mode))
      message(nf_cmd())

      # Count samples to update ----
      if (session$userData$mode == "Assemble") {
        samples <- dplyr::tbl(session$userData$con, "assemble") |>
          dplyr::filter(assemble_switch %in% c(1, 4)) |>
          dplyr::pull(ID)
      }
      if (session$userData$mode == "Annotate") {
        samples <- dplyr::left_join(
          dplyr::tbl(session$userData$con, "assemble"),
          dplyr::tbl(session$userData$con, "annotate"),
          by = "ID"
        ) |>
          dplyr::filter(assemble_lock == 1 &
                          annotate_switch == 1) |>
          dplyr::pull(ID)
      }
      if (length(samples) == 0) {
        modalDialog(
          title = div(
            style = "display: flex; justify-content: space-between; align-items: center; height: 42px;",
            span(
              stringr::str_glue("{session$userData$mode} - nothing to update")
            ),
            span(id = ns("gears"), class = "gears paused")
          ),
          size = "l",
          h5("Nextflow Command:"),
          div(class = "code-block", paste(
            c("nextflow", nf_cmd()), collapse = " "
          )),
          footer = tagList(actionButton(ns("close"), "Close"))
        ) |> showModal()
        req(F)
      }

      # Headless: prepare an editable submission script for the modal
      headless <- isTRUE(getOption("MitoPilot.headless"))
      headless_ui <- NULL
      if (headless) {
        wd <- headless_work_dir()
        cfg <- read_config_executor(file.path(wd, ".config"))
        headless_exec(cfg$executor)
        headless_queue(cfg$queue)
        headless_base(paste(tolower(session$userData$mode),
                            format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), sep = "_"))
        nfc <- paste(c("nextflow", nf_cmd()), collapse = " ")
        headless_nf(nfc)
        script_init <- paste(
          build_submit_script(wd, cfg$executor, cfg$queue, nfc,
                              headless_base(), headless_log_file()),
          collapse = "\n"
        )
        headless_ui <- div(
          h5("Cluster submission script (edit as needed, then submit):"),
          shiny::textAreaInput(ns("submit_script"), label = NULL,
                               value = script_init, width = "100%", height = "320px")
        )
      }

      modalDialog(
        title = div(
          style = "display: flex; justify-content: space-between; align-items: center; height: 42px;",
          span(
            stringr::str_glue(
              "{session$userData$mode}: updating {length(samples)} samples"
            )
          ),
          span(id = ns("gears"), class = "gears paused")
        ),
        div(
          style = "display: flex; justify-content: space-between; align-items: left;",
          shinyWidgets::prettyCheckbox(
            ns("resume"),
            label = "Resume previous run?",
            status = "primary",
            inline = TRUE,
            value = TRUE
          )
        ),
        size = "l",
        if (!headless) h5("Nextflow Command:"),
        if (!headless) div(style = "display: flex; justify-content: space-between; align-items: left;", class = "code-block", textOutput(ns(
          "nf_code_block"
        ))),
        headless_ui,
        div(
          id = ns("progress_div"),
          h5("Progress:"),
          div(
            id = ns("progress_div_text"),
            style = "max-height: 300px; overflow-y: auto;",
            class = "code-block",
            textOutput(ns("progress_header")),
            textOutput(ns("progress_executor")),
            textOutput(ns("progress_process")),
            textOutput(ns("progress_footer"))
          )
        ) |> shinyjs::hidden(),
        footer = tagList(
          tags$div(style = "margin-bottom: 10px;", uiOutput(ns(
            "start_button_ui"
          ))),
          actionButton(ns("stop"), "Stop / Interrupt") |> shinyjs::hidden(),
          actionButton(ns("close"), "Close")
        )
      ) |> showModal()
    })

    output$start_button_ui <- renderUI({
      # Headless: never run Nextflow from the app; edit + submit a job script.
      if (isTRUE(getOption("MitoPilot.headless"))) {
        cmd <- submit_command(headless_exec())
        submit_btn <- if (is.null(cmd)) {
          shinyjs::disabled(actionButton(ns("submit_headless"), "Submit to Cluster"))
        } else {
          actionButton(ns("submit_headless"),
                       paste0("Submit to Cluster (", cmd, ")"), class = "btn-success")
        }
        return(tagList(
          submit_btn,
          actionButton(ns("save_script"), "Save Script Only")
        ))
      }

      is_hydra_cluster <- FALSE
      is_sedna_cluster <- FALSE

      # Use a try block to gracefully handle errors if the command fails
      motd_output <- try(readLines("/etc/hosts", warn = FALSE), silent = TRUE)

      if (!inherits(motd_output, "try-error") &&
          any(grepl("hydra", motd_output, ignore.case = TRUE))) {
        is_hydra_cluster <- TRUE
      } else if (!inherits(motd_output, "try-error") &&
                 any(grepl("sedna", motd_output, ignore.case = TRUE))) {
        is_sedna_cluster <- TRUE
      }

      if (is_hydra_cluster) {
        # If hydra is found, render a list containing both buttons
        tagList(
          actionButton(ns("start"), "Run from App"),
          actionButton(ns("submit_job"), "Submit as Job", class = "btn-success")
        )
      } else if (is_sedna_cluster) {
        # If hydra is found, render a list containing both buttons
        tagList(
          actionButton(ns("start"), "Run from App"),
          actionButton(ns("submit_job"), "Submit as Job", class = "btn-success")
        )
      } else {
        # Otherwise, render only the default start button
        actionButton(ns("start"), "Run from App", class = "btn-success")
      }
    })

    # Render nextflow command
    output$nf_code_block <- shiny::renderText({
      paste(c("nextflow", nf_cmd()), collapse = " ")
    })

    # Toggle "-resume" Nextflow option
    observeEvent(input$resume, {
      if (isTRUE(input$resume)) {
        nf_cmd(nextflow_cmd(session$userData$mode))
      } else {
        nf_cmd(stringr::str_remove(nextflow_cmd(session$userData$mode), pattern = "-resume"))
      }
      output$nf_code_block <- shiny::renderText({
        paste(c("nextflow", nf_cmd()), collapse = " ")
      })
      # Headless: keep the editable script in sync with the -resume toggle
      if (isTRUE(getOption("MitoPilot.headless")) && !is.null(headless_base())) {
        refresh_headless_script()
      }
    })

    # The logic for starting the process is moved into its own function.
    start_nf_process <- function() {
      prog_header(NULL)
      prog_executor(NULL)
      prog_pos(0L)
      prog_frame_open(FALSE)
      prog_board(list())
      prog_footer(NULL)
      shinyjs::hide("start_button_ui") # Hide the container with the start buttons
      shinyjs::show("stop")
      shinyjs::removeClass("gears", "paused")
      shinyjs::show("progress_div")

      if (!is.null(process()) && process()$is_alive()) {
        process()$kill()
      }

      # Pin the Nextflow engine to a MitoPilot-compatible version for this run.
      nxf_pin <- nf_pin_version()

      p <- processx::process$new(
        "nextflow",
        args = c(nf_cmd(), "-ansi-log"),
        stdout = "|",
        stderr = "|",
        env = c(
          "current",
          if (!is.na(nxf_pin)) c(NXF_VER = nxf_pin),
          NXF_ANSI_SUMMARY = TRUE,
          # Keep Nextflow's ANSI log from truncating process names; stable, full
          # names keep the progress parser's per-process keys consistent.
          COLUMNS = 500,
          SGE = "/cm/shared/apps/uge/8.8.1/age",
          SGE_ARCH = "lx-amd64",
          SGE_CELL = "age",
          SGE_ROOT = "/cm/shared/apps/uge/8.8.1"
        ),
        wd = dirname(getOption("MitoPilot.db") %||% ".")
      )
      process(p)
    }

    # The "Start Nextflow" button calls the shared function.
    observeEvent(input$start, {
      start_nf_process()
    })

    # Headless: write the (edited) script + remember its resource block for reuse
    write_headless_script <- function() {
      work_dir <- headless_work_dir()
      base <- headless_base()
      full_nf_cmd <- headless_nf()
      log_file_path <- headless_log_file()
      script_path <- file.path(work_dir, paste0(base, ".sh"))
      text <- input$submit_script %||% ""
      writeLines(strsplit(text, "\n", fixed = TRUE)[[1]], script_path)
      save_submit_template(text, work_dir, full_nf_cmd, base, log_file_path)
      script_path
    }

    # Headless: write the script only (user submits it manually)
    observeEvent(input$save_script, {
      tryCatch({
        script_path <- write_headless_script()
        shinyWidgets::sendSweetAlert(
          title = "Submission script saved",
          text = paste0(
            "Wrote ", basename(script_path), " to your project directory and ",
            "saved your resource settings for next time. Submit it yourself ",
            "(e.g. sbatch / qsub / bsub)."
          ),
          type = "success"
        )
        removeModal()
      }, error = function(e) {
        shinyWidgets::sendSweetAlert(
          title = "Failed to save submission script:",
          text = e$message, type = "error"
        )
      })
    })

    # Headless: write the script, remember it, and submit to the scheduler
    observeEvent(input$submit_headless, {
      tryCatch({
        script_path <- write_headless_script()
        res <- run_submit(script_path, headless_exec(), headless_work_dir())
        if (!isTRUE(res$success)) {
          stop(res$output)
        }
        shinyWidgets::sendSweetAlert(
          title = "Job submitted",
          text = paste0(res$command, ": ", res$output,
                        "\nLog: ", basename(headless_log_file())),
          type = "success"
        )
        removeModal()
      }, error = function(e) {
        shinyWidgets::sendSweetAlert(
          title = "Failed to submit job:",
          text = e$message, type = "error"
        )
      })
    })

    # create Hydra job script and submit
    observeEvent(input$submit_job, {
      req(!job_submitting())
      job_submitting(TRUE)
      shinyjs::disable(ns("submit_job"))

      # Let the user know submission is underway. The qsub/sbatch call below
      # blocks the R thread, so defer it to the next event-loop tick to let
      # this message render first.
      shinyWidgets::sendSweetAlert(
        title = "Submitting job...",
        text = "Hold tight, handing your job off to the scheduler. This can take a moment.",
        type = "info",
        btn_labels = NA,
        closeOnClickOutside = FALSE
      )

      later::later(function() {
       shiny::withReactiveDomain(session, {
      is_hydra_cluster <- FALSE
      is_sedna_cluster <- FALSE

      # Use a try block to gracefully handle errors if the command fails
      motd_output <- try(readLines("/etc/hosts", warn = FALSE), silent = TRUE)

      if (!inherits(motd_output, "try-error") &&
          any(grepl("hydra", motd_output, ignore.case = TRUE))) {
        is_hydra_cluster <- TRUE
      } else if (!inherits(motd_output, "try-error") &&
                 any(grepl("sedna", motd_output, ignore.case = TRUE))) {
        is_sedna_cluster <- TRUE
      }

      if (is_hydra_cluster) {
        tryCatch({
          work_dir <- dirname(getOption("MitoPilot.db") %||% ".")
          # nf_cmd() is reactive; read it via isolate() since this deferred
          # callback runs outside a reactive context.
          full_nf_cmd <- paste(c("nextflow", shiny::isolate(nf_cmd())), collapse = " ")

          # 1. Create a timestamp and a workflow label ("assemble" or "annotate").
          timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
          workflow_label <- tolower(session$userData$mode)

          # 2. Combine them for a unique base filename.
          base_filename <- paste(workflow_label, timestamp, sep = "_")

          # 3. Define the job name, log file path, and script path using the base filename.
          job_name <- base_filename
          log_file_path <- file.path(work_dir, paste0(base_filename, ".log"))
          script_path <- file.path(work_dir, paste0(base_filename, ".sh"))

          script_content <- hydra_submission_script(full_nf_cmd, job_name, log_file_path)

          # Write the script to the unique, timestamped file path.
          writeLines(script_content, script_path)

          # Submit the job using the new script name.
          submit_output <- system2(
            "qsub",
            args = basename(script_path),
            stdout = TRUE,
            stderr = TRUE
          )

          if (any(grepl("Your job", submit_output, ignore.case = TRUE))) {
            shinyWidgets::sendSweetAlert(
              title = "Success!",
              text = paste0(
                submit_output,
                ". You can monitor your job on Hydra with the `qstat` command or see `",
                paste0(base_filename, ".log"),
                "` in your project directory"
              ),
              type = "success"
            )
            removeModal()
          } else {
            stop(paste(submit_output, collapse = "\n"))
          }

        }, error = function(e) {
          job_submitting(FALSE)
          shinyjs::enable(ns("submit_job"))
          shinyWidgets::sendSweetAlert(title = "Failed to submit job:",
                                       text = e$message,
                                       type = "error")
        })
      } else if (is_sedna_cluster) {
        tryCatch({
          work_dir <- dirname(getOption("MitoPilot.db") %||% ".")
          # nf_cmd() is reactive; read it via isolate() since this deferred
          # callback runs outside a reactive context.
          full_nf_cmd <- paste(c("nextflow", shiny::isolate(nf_cmd())), collapse = " ")

          # 1. Create a timestamp and a workflow label ("assemble" or "annotate").
          timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
          workflow_label <- tolower(session$userData$mode)

          # 2. Combine them for a unique base filename.
          base_filename <- paste(workflow_label, timestamp, sep = "_")

          # 3. Define the job name, log file path, and script path using the base filename.
          job_name <- base_filename
          log_file_path <- file.path(work_dir, paste0(base_filename, ".log"))
          script_path <- file.path(work_dir, paste0(base_filename, ".sh"))

          script_content <- c(
            "#!/bin/sh",
            paste0("#SBATCH -J ", job_name),
            # Use the new dynamic job name
            paste0("#SBATCH -o ", log_file_path),
            # Use the new dynamic log file path
            "#SBATCH -p standard",
            "#SBATCH -c 1",
            "#SBATCH --mem=8G",
            "#SBATCH -t 24:00:00",
            "",
            'echo "---"',
            'echo + `date` job $SLURM_JOB_NAME started in $SLURM_JOB_PARTITION with jobID=$SLURM_JOBID on $SLURM_JOB_NODELIST',
            'echo "---"',
            "",
            "source ~/.bashrc",
            "mamba activate MitoPilot_deps",
            "",
            # Pin the Nextflow engine to a MitoPilot-compatible version.
            if (!is.na(nf_pin_version())) paste0("export NXF_VER=", nf_pin_version()),
            full_nf_cmd,
            "",
            'echo "---"',
            'echo = `date` job $SLURM_JOB_NAME done',
            'echo "---"'
          )

          # Write the script to the unique, timestamped file path.
          writeLines(script_content, script_path)

          # Submit the job using the new script name.
          submit_output <- system2(
            "sbatch",
            args = basename(script_path),
            stdout = TRUE,
            stderr = TRUE
          )

          if (any(grepl("[0-9]", submit_output, ignore.case = TRUE))) {
            shinyWidgets::sendSweetAlert(
              title = "Success!",
              text = paste0(
                "Job ID: ",
                submit_output,
                ". You can monitor your job on SEDNA with the `squeue` command or see `",
                paste0(base_filename, ".out"),
                "` in your project directory"
              ),
              type = "success"
            )
            removeModal()
          } else {
            stop(paste(submit_output, collapse = "\n"))
          }

        }, error = function(e) {
          job_submitting(FALSE)
          shinyjs::enable(ns("submit_job"))
          shinyWidgets::sendSweetAlert(title = "Failed to submit job:",
                                       text = e$message,
                                       type = "error")
        })
      }
       })
      }, delay = 0.05)
    })


    # Monitor progress ----
    prog_header <- reactiveVal()
    prog_executor <- reactiveVal()
    # Position-keyed board: identity is the process's ORDINAL within a redraw,
    # NOT its displayed name (which Nextflow truncates unpredictably, even to 1-2
    # ambiguous chars). The counter resets at the START OF EACH FRAME: an
    # `executor` line, OR the first process line after a blank (Nextflow reprints
    # the whole board every ~200ms; BEFORE the first task submits those redraws
    # have NO executor line, so they must be delimited by the blank separator or
    # they accumulate). prog_pos carries the running ordinal across observer ticks
    # (reads can split a frame); prog_frame_open tracks whether we are mid-frame.
    # prog_board is an ordered list; element = list(line=, name=).
    prog_pos <- reactiveVal(0L)
    prog_frame_open <- reactiveVal(FALSE)
    prog_board <- reactiveVal(list())
    prog_footer <- reactiveVal()
    progress_update <- function(process_out,
                                prog_header,
                                prog_executor,
                                prog_pos,
                                prog_frame_open,
                                prog_board,
                                prog_footer) {
      process_out <- cli::ansi_strip(process_out) # clean up ansi encoded output
      n <- length(process_out)
      is_exec <- stringr::str_detect(process_out, "^executor")
      is_blank <- grepl("^\\s*$", process_out)
      keys <- stringr::str_match(process_out,
                                 "^(?<prefix>\\[.+?\\]) (?<key>WF\\S*) +(?<suffix>.*)")
      is_prog <- !is.na(keys[, 1])
      routed <- rep(FALSE, n)
      # Header = the Nextflow banner printed before the first executor/progress
      # line; captured once, before any executor line has been seen.
      if (is.null(prog_executor)) {
        stop_at <- which(is_exec | is_prog)
        hstop <- if (length(stop_at)) min(stop_at) - 1L else n
        if (hstop >= 1) {
          prog_header <- paste(na.omit(c(
            prog_header, collapse_empty_lines(process_out[seq_len(hstop)])
          )), collapse = "\n")
          routed[seq_len(hstop)] <- TRUE
        }
      }
      # Single ordered pass. A frame starts at an `executor` line OR at the first
      # progress line after a blank (pre-submit redraws have no executor line);
      # either resets the position counter. Each progress line advances the
      # counter and upserts the board AT THAT POSITION, so the same process (row i)
      # is overwritten in place every redraw regardless of how its name is
      # truncated. A blank line closes the current frame.
      for (i in seq_len(n)) {
        if (is_blank[i]) prog_frame_open <- FALSE
        if (routed[i]) next
        if (is_exec[i]) {
          prog_executor <- process_out[i]
          prog_pos <- 0L
          prog_frame_open <- TRUE
          routed[i] <- TRUE
        } else if (is_prog[i]) {
          if (!prog_frame_open) {
            prog_pos <- 0L          # first process line of a new (executor-less) frame
            prog_frame_open <- TRUE
          }
          prog_pos <- prog_pos + 1L
          nm <- resolve_process_name(keys[i, "key"])
          prev <- if (prog_pos <= length(prog_board)) prog_board[[prog_pos]] else NULL
          if (is.na(nm) && !is.null(prev)) nm <- prev$name   # sticky name
          prog_board[[prog_pos]] <- list(line = keys[i, 1], name = nm)
          routed[i] <- TRUE
        }
      }
      # Anything not header/executor/progress (warnings, completion summary) -> footer.
      rest <- collapse_empty_lines(process_out[!routed])
      if (any(nchar(rest) > 0)) {
        prog_footer <- paste(na.omit(c(prog_footer, rest)), collapse = "\n")
      }
      list(
        prog_header = prog_header,
        prog_executor = prog_executor,
        prog_pos = prog_pos,
        prog_frame_open = prog_frame_open,
        prog_board = prog_board,
        prog_footer = prog_footer
      )
    }
    collapse_empty_lines <- function(x) {
      is_empty <- grepl("^\\s*$", x)
      if (all(is_empty)) {
        return(character(0))
      }
      first_nonempty <- which(!is_empty)[1]
      last_nonempty <- which(!is_empty)[length(which(!is_empty))]
      x <- x[first_nonempty:last_nonempty]
      is_empty <- is_empty[first_nonempty:last_nonempty]
      keep <- !is_empty |
        (is_empty & c(TRUE, !is_empty[-length(is_empty)]))
      x[keep]
    }
    apply_progress <- function(new_output) {
      if (length(new_output) == 0) return(invisible())
      update <- progress_update(
        new_output,
        prog_header(),
        prog_executor(),
        prog_pos(),
        prog_frame_open(),
        prog_board(),
        prog_footer()
      )
      prog_header(update$prog_header)
      prog_executor(update$prog_executor)
      prog_pos(update$prog_pos)
      prog_frame_open(update$prog_frame_open)
      prog_board(update$prog_board)
      prog_footer(update$prog_footer)
    }
    observe({
      req(process())
      invalidateLater(100)
      p <- process()
      if (p$is_alive()) {
        apply_progress(p$read_output_lines())
      } else {
        # Board is upserted live, so the final read leaves it already correct.
        apply_progress(p$read_output_lines())
        process(NULL)
        shinyjs::hide("stop")
        shinyjs::show("start_button_ui") # Show the button container again
        shinyjs::addClass("gears", "paused")
        trigger(paste0("refresh_", tolower(session$userData$mode)))
      }
    })

    # Render progress ----
    output$progress_header <- renderText({
      req(prog_header())
    })
    output$progress_executor <- renderText({
      req(prog_executor())
    })
    output$progress_process <- renderText({
      paste(render_progress_board(prog_board()), collapse = "\n")
    })
    output$progress_footer <- renderText({
      req(prog_footer())
    })

    # Stop ----
    observeEvent(input$stop, {
      if (!is.null(process()) && process()$is_alive()) {
        process()$kill()
      }
      process(NULL)
      shinyjs::hide("stop")
      shinyjs::show("start_button_ui") # Also show the buttons if stopped manually
      shinyjs::addClass("gears", "paused")
      trigger(paste0("refresh_", tolower(session$userData$mode)))
    })

    # Close modal ----
    observeEvent(input$close, {
      if (!is.null(process()) && process()$is_alive()) {
        process()$kill()
      }
      process(NULL)
      process_out("")
      removeModal()
    })
  })
}
