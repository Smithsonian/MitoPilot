#' run_pipeline Server Functions
#'
#' @noRd
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
      motd_output <- try(system2("cat",
                                 "/etc/hosts",
                                 stdout = TRUE,
                                 stderr = FALSE),
                         silent = TRUE)

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
      prog_process(list())
      prog_footer(NULL)
      shinyjs::hide("start_button_ui") # Hide the container with the start buttons
      shinyjs::show("stop")
      shinyjs::removeClass("gears", "paused")
      shinyjs::show("progress_div")

      if (!is.null(process()) && process()$is_alive()) {
        process()$kill()
      }

      p <- processx::process$new(
        "nextflow",
        args = c(nf_cmd(), "-ansi-log"),
        stdout = "|",
        stderr = "|",
        env = c(
          "current",
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
      motd_output <- try(system2("cat",
                                 "/etc/hosts",
                                 stdout = TRUE,
                                 stderr = FALSE),
                         silent = TRUE)

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
    prog_process <- reactiveVal(list())
    prog_footer <- reactiveVal()
    progress_update <- function(process_out,
                                prog_header,
                                prog_executor,
                                prog_process,
                                prog_footer) {
      remaining <- rep(T, length(process_out))
      process_out <- cli::ansi_strip(process_out) # clean up ansi encoded output
      executor_lines <- stringr::str_detect(process_out, "^executor")
      # Key on the full "WF..." process-name token (not a fixed 4-char tail):
      # per-scaffold BLAST tasks carry long tags that force Nextflow to truncate
      # names down to a few chars, which broke the old 4-char-minimum pattern and
      # dumped every redraw snapshot into the footer.
      keys <- stringr::str_match(process_out,
                                 "^(?<prefix>\\[.+?\\]) (?<key>WF\\S*) +(?<suffix>.*)")
      progress_lines <- !is.na(keys[, 1])
      if (length(prog_process) == 0) {
        header_stop <- which(executor_lines | progress_lines)
        header_stop <- ifelse(length(header_stop) == 0,
                              length(process_out),
                              min(header_stop) - 1)
        prog_header <- paste(na.omit(c(
          prog_header, collapse_empty_lines(process_out[seq_len(header_stop)])
        )), collapse = "\n")
        remaining[1:header_stop] <- F
      }
      if (any(executor_lines)) {
        prog_executor <- process_out[max(which(executor_lines))]
        remaining[executor_lines] <- F
      }
      # Key each process by its stable simple name. Nextflow truncates the
      # workflow-path prefix with an ellipsis and varies the name-column width
      # between redraws, so the raw "WF..." token is unstable and the pending
      # board (full names) would never collapse onto the running board. Reduce
      # to the process simple name (text after the last ':' or ellipsis).
      process_key <- function(token) {
        k <- sub("^.*:", "", token)                 # drop path prefix up to last ':'
        sub("^.*(\u2026|\\.\\.\\.)", "", k)  # drop leading ellipsis truncation
      }
      for (raw_key in na.omit(unique(keys[, 'key']))) {
        rows <- which(keys[, 'key'] == raw_key)
        line <- keys[rows[length(rows)], 1]          # latest full line for this token
        simple <- process_key(raw_key)
        # Suffix-merge names that Nextflow truncated into (e.g. 'st_genbank' and
        # 'blast_genbank') so both redraws land on one row, keyed by the fuller name.
        existing <- names(prog_process)
        match_k <- existing[vapply(existing, function(e)
          endsWith(e, simple) || endsWith(simple, e), logical(1))]
        canonical <- simple
        if (length(match_k) > 0) {
          canonical <- match_k[which.max(nchar(match_k))]
          if (nchar(simple) > nchar(canonical)) {
            prog_process[[simple]] <- prog_process[[canonical]]
            prog_process[[canonical]] <- NULL
            canonical <- simple
          }
        }
        prog_process[[canonical]] <- line
      }
      remaining[!is.na(keys[, 1])] <- F
      remaining <- process_out[remaining] |> collapse_empty_lines()
      if (any(nchar(remaining) > 0)) {
        prog_footer <- paste(na.omit(c(prog_footer, remaining)), collapse = "\n")
      }
      return({
        list(
          prog_header = prog_header,
          prog_executor = prog_executor,
          prog_process = prog_process,
          prog_footer = prog_footer
        )
      })
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
    observe({
      req(process())
      invalidateLater(100)
      p <- process()
      if (p$is_alive()) {
        new_output <- p$read_output_lines()
        if (length(new_output) > 0) {
          update <- progress_update(
            new_output,
            prog_header(),
            prog_executor(),
            prog_process(),
            prog_footer()
          )
          prog_header(update$prog_header)
          prog_executor(update$prog_executor)
          prog_process(update$prog_process)
          prog_footer(update$prog_footer)
        }
      } else {
        final_output <- p$read_output_lines()
        if (length(final_output) > 0) {
          update <- progress_update(
            final_output,
            prog_header(),
            prog_executor(),
            prog_process(),
            prog_footer()
          )
          prog_header(update$prog_header)
          prog_executor(update$prog_executor)
          prog_process(update$prog_process)
          prog_footer(update$prog_footer)
        }
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
      paste(prog_process(), collapse = "\n")
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
