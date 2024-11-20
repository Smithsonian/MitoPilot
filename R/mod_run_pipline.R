#' run_pipline Server Functions
#'
#' @noRd
mod_run_pipline_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    process <- reactiveVal()

    on("run", {

      # Kill any existing process
      if (!is.null(process()) && process()$is_alive()) {
        process()$kill()
      }

      # Start the process
      args <- update_mitopilot(session$userData$mode)
      p <- processx::process$new(
        "nextflow",
        args = args,
        stdout = "|",
        stderr = "|",
        wd = dirname(getOption("MitoPilot.db") %||% here::here(".sqlite"))
      )
      process(p)

      nf_cmd(paste(c("nextflow",cmd), collapse = " "))

      modalDialog(
        title = stringr::str_glue("Running {session$userData$mode} pipline"),
        size = "l",
        h4("Nextflow Command:"),
        div(
          class = "code-block",
          textOutput(ns("nextflow_cmd"), inline = T)
        ),
        h4("Progress:"),
        div(
          class = "code-block",
          textOutput(ns("progress_out"), inline = T)
        ),
        footer = tagList(
          actionButton(
            ns("stop_run"),
            "Close"
          )
        )
      ) |> showModal()

    })

    # Monitor progress
    observe({
      req(process())
      invalidateLater(500)
      p <- process()
      if (process()$is_alive()) {
        new_output <- p$read_output_lines()
        if (length(new_output) > 0) {
          process_out(paste(process_out(), paste(new_output, collapse = "\n"), sep = "\n"))
        }
      } else {
        final_output <- p$read_output_lines()
        if (length(final_output) > 0) {
          process_out(paste(process_out(), paste(final_output, collapse = "\n"), sep = "\n"))
        }
        process(NULL)
      }

    })

    nf_cmd <- reactiveVal()
    output$nextflow_cmd <- renderText({
      req(nf_cmd())
    })

    process_out <- reactiveVal("")
    output$progress_out <- renderText({
      req(process_out())
    })

    observeEvent(input$stop_run, {
      if (!is.null(process()) && process()$is_alive()) {
        process()$kill()
      }
      process(NULL)
      process_out("")
      removeModal()
    })

  })
}

