#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny reactable
#' @noRd
app_ui <- function(request) {
  tagList(
    add_external_resources(),
    fluidPage(
      div(
        style = "display: flex; flex-direction: column;",
        div(
          style = "display: flex; flex-flow: row nowrap; align-items: center; gap: 1em;",
          div(
            shinyWidgets::pickerInput(
              inputId = "mode",
              width = 150,
              label = "",
              choices = c("Assemble", "Annotate", "Submit")
            )
          ),
          shinyWidgets::actionBttn(
            "refresh",
            label = NULL,
            icon = icon("sync"),
            style = "material-flat",
            size = "sm"
          ),
          shinyWidgets::actionBttn(
            "state",
            label = "State",
            style = "material-flat",
            size = "sm"
          ),
          shinyWidgets::actionBttn(
            "lock",
            label = "Lock",
            style = "material-flat",
            size = "sm"
          ),
          shinyWidgets::actionBttn(
            "run_modal",
            label = "Update",
            style = "material-flat",
            size = "sm"
          )
        ),
        div(
          style = "padding: 1em;",
          conditionalPanel(
            condition = "input.mode == 'Assemble'",
            assemble_ui("assemble")
          ),
          conditionalPanel(
            condition = "input.mode == 'Annotate'",
            annotate_ui("annotate")
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "MitoPilot"
    ),
    waiter::useWaiter(),
    rclipboard::rclipboardSetup(),
    shinyjs::useShinyjs()
  )
}
