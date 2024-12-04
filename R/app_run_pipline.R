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
      nf_cmd(update_mitopilot(session$userData$mode))

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
            textOutput(ns("progress_out"))
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
      process_out("")
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
        env = c("current", NXF_ANSI_SUMMARY = TRUE,
               SGE = "/cm/shared/apps/uge/8.8.1/age",
               SGE_ARCH = "lx-amd64",
               SGE_CELL = "age",
               SGE_ROOT = "/cm/shared/apps/uge/8.8.1"),
        wd = dirname(getOption("MitoPilot.db") %||% here::here(".sqlite"))
      )
      process(p)
    })

    # Monitor progress ----
    observe({
      req(process())
      invalidateLater(100)
      p <- process()
      if (p$is_alive()) {
        new_output <- p$read_output_lines()
        if (length(new_output) > 0) {
          process_out(paste(process_out(), paste(new_output, collapse = "\n"), sep = "\n"))
          trigger(paste0("refresh_", tolower(session$userData$mode)))
        }
      } else {
        final_output <- p$read_output_lines()
        if (length(final_output) > 0) {
          process_out(paste(process_out(), paste(final_output, collapse = "\n"), sep = "\n"))
        }
        process(NULL)
        shinyjs::hide("stop")
        shinyjs::show("start")
        shinyjs::addClass("gears", "paused")
        trigger(paste0("refresh_", tolower(session$userData$mode)))
      }
    })

    # Render progress ----
    output$progress_out <- renderText({
      req(process_out())
      # Send a message to scroll the progress_div
      session$sendCustomMessage("scrollProgress", list(id = ns("progress_div_text")))
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
