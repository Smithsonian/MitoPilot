#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny gargoyle dbplyr
#' @noRd
app_server <- function(input, output, session) {
  # db connection ----
  db <- getOption("MitoPilot.db") %||% normalizePath(".sqlite")
  session$userData$dir <- dirname(db)
  if (!file.exists(db)) {
    shinyWidgets::sendSweetAlert(
      title = "Database not found",
      text = "The MitoPilot::gui() app requires a database to run. Please make sure your working directory is set to an active MitoPilot project, or use set the location of the database using, options(MitoPilot.db = 'path/to/the/.sqlite').",
      type = "error"
    )
  }
  session$userData$con <- DBI::dbConnect(RSQLite::SQLite(), dbname = db)
  session$onSessionEnded(function() {
    message("Session ended. Closing DB connection.")
    DBI::dbDisconnect(session$userData$con)
  })
  message(paste("Database attached:", db))

  # Publish / output directory ----
  dir_out <- readLines(file.path(dirname(db), ".config")) |>
    stringr::str_extract("publishDir.*") |>
    na.omit() |>
    stringr::str_remove("^[^'|^\"]+['\"]") |>
    stringr::str_extract("^[^'|^\"]+")
  session$userData$dir_out <- file.path(dirname(db), dir_out)
  # Genetic code ----
  session$userData$genetic_code <- readLines(file.path(dirname(db), ".config")) |>
    stringr::str_extract("genetic_code.*") |>
    na.omit() |>
    stringr::str_extract("[0-9]+$")

  # View mode ----
  observeEvent(input$mode, {
    session$userData$mode <- input$mode
    if(input$mode == "Export"){
      shinyjs::toggle("export_ctrls", condition = TRUE)
      shinyjs::toggle("asmb_ctrls", condition = FALSE)
      shinyjs::toggle("annot_ctrls", condition = FALSE)
    }else if(input$mode == "Assemble"){
      shinyjs::toggle("export_ctrls", condition = FALSE)
      shinyjs::toggle("asmb_ctrls", condition = TRUE)
      shinyjs::toggle("annot_ctrls", condition = FALSE)
    }else{
      shinyjs::toggle("export_ctrls", condition = FALSE)
      shinyjs::toggle("asmb_ctrls", condition = FALSE)
      shinyjs::toggle("annot_ctrls", condition = TRUE)
    }
  })

  # Reload Data
  observeEvent(input$refresh, {
    trigger(paste0("refresh_", tolower(session$userData$mode)))
  })
  # State
  #init("state")
  observeEvent(input$state, {
    trigger("state")
  })
  # Lock
  #init("lock")
  observeEvent(input$lock, {
    trigger("lock")
  })
  # Run
  init("run_modal")
  observeEvent(input$run_modal, {
    trigger("run_modal")
  })
  # ID_verified
  observeEvent(input$id_verified, {
    trigger("id_verified")
  })
  # mark problematic 
  observeEvent(input$problematic, {
    trigger("problematic")
  })
  # Export
  observeEvent(input$group, {
    trigger("group")
  })
  observeEvent(input$export, {
    trigger("export")
  })

  # Sub-modules ----
  pipeline_server("run")
  assemble_server("assemble")
  annotate_server("annotate")
  export_server("export")
}
