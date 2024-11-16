#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny gargoyle dbplyr
#' @noRd
app_server <- function(input, output, session) {
  # db connection ----
  db <- getOption("MitoPilot.db") %||% here::here(".sqlite")
  if (!file.exists(db)) {
    shinyWidgets::sendSweetAlert(
      title = "Database not found",
      text = "The MitoPilot::gui() app requires a database to run. Please make sure your working directory is set to an active MitoPilot project, or use set the location of the database using, options(MitoPilot.db = 'path/to/the/.sqlite').",
    )
  }
  session$userData$db <- DBI::dbConnect(RSQLite::SQLite(), dbname = db)

  # View mode ----
  observeEvent(input$mode, {
    session$userData$mode <- input$mode
    shinyjs::toggle("lock", condition = input$mode != "Submit")
    shinyjs::toggle("state", condition = input$mode != "Submit")
    shinyjs::toggle("group", condition = input$mode == "Submit")
    shinyjs::toggle("pregroup", condition = input$mode == "Submit")
  })

  # Reload Data
  init("refresh")
  observeEvent(input$refresh, {
    trigger("refresh")
  })

  # State
  init("state")
  observeEvent(input$state, {
    trigger("state")
  })
  # Lock
  init("lock")
  observeEvent(input$lock, {
    trigger("lock")
  })
  # Group
  init("pregroup")
  observeEvent(input$pregroup, {
    trigger("pregroup")
  })
  init("group")
  observeEvent(input$group, {
    trigger("group")
  })

  # Sub-modules ----
  assemble_server("assemble")
  # mod_Annotate_server("Annotate", grv)
  # mod_Submit_server("Submit", grv)
}
