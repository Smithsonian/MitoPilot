#' run_pipeline Server Functions
#'
#' @noRd
pipeline_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    nf_cmd <- reactiveVal()
    process <- reactiveVal()
    process_out <- reactiveVal()

    on("run_modal", {
      # Generate Nextflow params ----
      nf_cmd(nextflow_cmd(session$userData$mode))

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
            span(stringr::str_glue("Update {session$userData$mode} pipeline - O Samples")),
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
          span(stringr::str_glue("Update {session$userData$mode} pipeline: {length(samples)} Samples")),
          span(id = ns("gears"), class = "gears paused")
        ),
        size = "l",
        h5("Nextflow Command:"),
        div(
          class = "code-block",
          paste(c("nextflow", nf_cmd()), collapse = " ")
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
          actionButton(
            ns("start"),
            "Start Nextflow"
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

    # Start ----
    observeEvent(input$start, {
      prog_header(NULL)
      prog_executor(NULL)
      prog_process(list())
      prog_footer(NULL)
      shinyjs::hide("start")
      shinyjs::show("stop")
      shinyjs::removeClass("gears", "paused")
      shinyjs::show("progress_div")

      # Kill any existing process
      if (!is.null(process()) && process()$is_alive()) {
        process()$kill()
      }

      # Start the new system process
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
        wd = dirname(getOption("MitoPilot.db") %||% here::here(".sqlite"))
      )
      process(p)
    })

    # Monitor progress ----
    prog_header <- reactiveVal()
    prog_executor <- reactiveVal()
    prog_process <- reactiveVal(list())
    prog_footer <- reactiveVal()
    progress_update <- function(process_out, prog_header, prog_executor, prog_process, prog_footer) {
      remaining <- rep(T, length(process_out))
      executor_lines <- stringr::str_detect(process_out, "^executor")
      keys <- stringr::str_match(
        process_out,
        "^(?<prefix>\\[.+\\]) WF[^\\s]+(?<key>\\S{10}) (?<suffix>.*)"
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
          trigger(paste0("refresh_", tolower(session$userData$mode)))
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
