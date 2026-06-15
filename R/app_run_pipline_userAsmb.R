#' run_pipeline Server Functions
#'
#' @noRd
pipeline_server_userAsmb <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    nf_cmd <- reactiveVal()
    process <- reactiveVal()
    process_out <- reactiveVal()
    job_submitting <- reactiveVal(FALSE)

    on("run_modal", {
      job_submitting(FALSE)
      # Generate Nextflow params ----
      nf_cmd(nextflow_cmd(session$userData$mode, userAsmbs = TRUE))
      message(nf_cmd())

      # Count samples to update ----
      if (session$userData$mode == "Assemble") {
        samples <- dplyr::tbl(session$userData$con, "assemble") |>
          dplyr::filter(assemble_switch == 1) |>
          dplyr::pull(ID)
      }
      if (session$userData$mode == "Annotate") {
        samples <- dplyr::left_join(
          dplyr::tbl(session$userData$con, "assemble"),
          dplyr::tbl(session$userData$con, "annotate"),
          by = "ID"
        ) |>
          dplyr::filter(assemble_lock == 1 & annotate_switch == 1) |>
          dplyr::pull(ID)
      }
      if (length(samples) == 0) {
        modalDialog(
          title = div(
            style = "display: flex; justify-content: space-between; align-items: center; height: 42px;",
            span(stringr::str_glue("{session$userData$mode} - nothing to update")),
            span(id = ns("gears"), class = "gears paused")
          ),
          size = "l",
          h5("Nextflow Command:"),
          div(
            class = "code-block",
            paste(c("nextflow", nf_cmd()), collapse = " ")
          ),
          footer = tagList(
            actionButton(
              ns("close"),
              "Close"
            )
          )
        ) |> showModal()
        req(F)
      }

      modalDialog(
        title = div(
          style = "display: flex; justify-content: space-between; align-items: center; height: 42px;",
          span(stringr::str_glue("{session$userData$mode}: updating {length(samples)} samples")),
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
        h5("Nextflow Command:"),
        div(
          style = "display: flex; justify-content: space-between; align-items: left;",
          class = "code-block",
          textOutput(ns("nf_code_block"))
        ),
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
          tags$div(
            style = "margin-bottom: 10px;",
            uiOutput(ns("start_button_ui"))
          ),
          actionButton(
            ns("stop"),
            "Stop / Interrupt"
          ) |> shinyjs::hidden(),
          actionButton(
            ns("close"),
            "Close"
          )
        )
      ) |> showModal()
    })

    output$start_button_ui <- renderUI({
      # Headless: never run Nextflow from the app; only write a submission script.
      if (isTRUE(getOption("MitoPilot.headless"))) {
        return(tagList(
          shinyjs::disabled(actionButton(ns("start"), "Run from App")),
          actionButton(ns("write_script"), "Write Submission Script", class = "btn-success")
        ))
      }

      is_hydra_cluster <- FALSE

      # Use a try block to gracefully handle errors if the command fails
      motd_output <- try(system2("cat", "/etc/hosts", stdout = TRUE, stderr = FALSE), silent = TRUE)

      if (!inherits(motd_output, "try-error") && any(grepl("hydra", motd_output, ignore.case = TRUE))) {
        is_hydra_cluster <- TRUE
      }

      if (is_hydra_cluster) {
        # If hydra is found, render a list containing both buttons
        tagList(
          actionButton(ns("start"), "Run from App", class = "btn-success"),
          actionButton(ns("submit_job"), "Submit as Job", class = "btn-secondary")
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
      if(isTRUE(input$resume)) { # if box is checked, keep "-resume" flag
        nf_cmd(nextflow_cmd(session$userData$mode,  userAsmbs = TRUE))
      } else { # if box is unchecked, remove "-resume" flag
        nf_cmd(stringr::str_remove(nextflow_cmd(session$userData$mode,  userAsmbs = TRUE),
                                   pattern = "-resume"))
      }
      output$nf_code_block <- shiny::renderText({
        paste(c("nextflow", nf_cmd()), collapse = " ")
      })
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
        env = c("current",
                NXF_ANSI_SUMMARY = TRUE,
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

    # Headless: write a cluster submission script (never submit it)
    observeEvent(input$write_script, {
      tryCatch({
        work_dir <- dirname(getOption("MitoPilot.db") %||% ".")
        full_nf_cmd <- paste(c("nextflow", nf_cmd()), collapse = " ")

        timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
        base_filename <- paste(tolower(session$userData$mode), timestamp, sep = "_")
        log_file_path <- file.path(work_dir, paste0(base_filename, ".log"))
        script_path <- file.path(work_dir, paste0(base_filename, ".sh"))

        cfg <- read_config_executor(file.path(work_dir, ".config"))
        script_content <- submission_script(
          executor = cfg$executor,
          queue = cfg$queue,
          full_nf_cmd = full_nf_cmd,
          job_name = base_filename,
          log_file = log_file_path
        )
        writeLines(script_content, script_path)

        shinyWidgets::sendSweetAlert(
          title = "Submission script written",
          text = paste0(
            "Wrote ", basename(script_path), " (executor: ", cfg$executor,
            ") to your project directory. Review/edit the environment setup, ",
            "then submit it yourself (e.g. sbatch / qsub / bsub)."
          ),
          type = "success"
        )
        removeModal()
      }, error = function(e) {
        shinyWidgets::sendSweetAlert(
          title = "Failed to write submission script:",
          text = e$message,
          type = "error"
        )
      })
    })

    # create Hydra job script and submit
    observeEvent(input$submit_job, {
      req(!job_submitting())
      job_submitting(TRUE)
      shinyjs::disable(ns("submit_job"))
      tryCatch({
        work_dir <- dirname(getOption("MitoPilot.db") %||% ".")
        full_nf_cmd <- paste(c("nextflow", nf_cmd()), collapse = " ")

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
          paste0("#$ -N ", job_name),        # Use the new dynamic job name
          paste0("#$ -o ", log_file_path),  # Use the new dynamic log file path
          "#$ -cwd -j y",
          "#$ -q lTWFM.sq",
          "#$ -l wfmq",
          "#$ -l mres=24G,h_data=24G,h_vmem=64G",
          "#$ -pe mthread 1",
          "#$ -S /bin/sh",
          "",
          'echo "---"',
          'echo "+ `date` job $JOB_NAME started in $QUEUE with jobID=$JOB_ID on $HOSTNAME"',
          'echo "---"',
          "",
          "source ~/.bashrc",
          "module load tools/java/21.0.2",
          "",
          "export NXF_OPTS=\"-Xms512m -Xmx20g -XX:MaxMetaspaceSize=512m -Xss256k\" # Java memory limits for 16G RSS constraint",
          full_nf_cmd,
          "",
          'echo "---"',
          'echo "= `date` job $JOB_NAME done"',
          'echo "---"'
        )

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
            text = paste0(submit_output, ". You can monitor your job on Hydra with the `qstat` command or see `",
                          paste0(base_filename, ".log"), "` in your project directory"),
            type = "success"
          )
          removeModal()
        } else {
          stop(paste(submit_output, collapse = "\n"))
        }

      }, error = function(e) {
        job_submitting(FALSE)
        shinyjs::enable(ns("submit_job"))
        shinyWidgets::sendSweetAlert(
          title = "Failed to submit job:",
          text = e$message,
          type = "error"
        )
      })
    })

    # Monitor progress ----
    prog_header <- reactiveVal()
    prog_executor <- reactiveVal()
    prog_process <- reactiveVal(list())
    prog_footer <- reactiveVal()
    progress_update <- function(process_out, prog_header, prog_executor, prog_process, prog_footer) {
      remaining <- rep(T, length(process_out))
      process_out <- cli::ansi_strip(process_out) # clean up ansi encoded output
      executor_lines <- stringr::str_detect(process_out, "^executor")
      keys <- stringr::str_match(
        process_out,
        "^(?<prefix>\\[.+\\]) WF[^\\s]+(?<key>\\S{4}) (?<suffix>.*)"
      )
      progress_lines <- !is.na(keys[,1])
      if (length(prog_process)==0) {
        header_stop <- which(executor_lines|progress_lines)
        header_stop <- ifelse(length(header_stop)==0, length(process_out), min(header_stop) - 1)
        prog_header <- paste(
          na.omit(c(
            prog_header,
            collapse_empty_lines(process_out[seq_len(header_stop)])
            )
          ),
          collapse = "\n"
        )
        remaining[1:header_stop] <- F
      }
      if(any(executor_lines)){
        prog_executor <- process_out[max(which(executor_lines))]
        remaining[executor_lines] <- F
      }
      for(key in na.omit(unique(keys[,'key']))){
        process_update <- keys[which(keys[,'key'] == key),]
        if(is.null(dim(process_update))){
          prog_process[[key]] <- process_update[1]
        }else{
          prog_process[[key]] <- process_update[nrow(process_update), 1]
        }
      }
      remaining[!is.na(keys[,1])] <- F
      remaining <- process_out[remaining] |> collapse_empty_lines()
      if(any(nchar(remaining)>0)){
        prog_footer <- paste(na.omit(c(prog_footer, remaining)),collapse = "\n")
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
      keep <- !is_empty | (is_empty & c(TRUE, !is_empty[-length(is_empty)]))
      x[keep]
    }
    observe({
      req(process())
      invalidateLater(100)
      p <- process()
      if (p$is_alive()) {
        new_output <- p$read_output_lines()
        if (length(new_output) > 0) {
          update <- progress_update(new_output, prog_header(), prog_executor(), prog_process(), prog_footer())
          prog_header(update$prog_header)
          prog_executor(update$prog_executor)
          prog_process(update$prog_process)
          prog_footer(update$prog_footer)
        }
      } else {
        final_output <- p$read_output_lines()
        if (length(final_output) > 0) {
          update <- progress_update(final_output, prog_header(), prog_executor(), prog_process(), prog_footer())
          prog_header(update$prog_header)
          prog_executor(update$prog_executor)
          prog_process(update$prog_process)
          prog_footer(update$prog_footer)
        }
        process(NULL)
        shinyjs::hide("stop")
        shinyjs::show("start")
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
      shinyjs::show("start")
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
