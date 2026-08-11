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
          shinyWidgets::pickerInput(
            inputId = "mode",
            width = 150,
            label = "",
            choices = c("Assemble", "Annotate", "Export")
          ),
          shinyWidgets::actionBttn(
            "refresh",
            label = NULL,
            icon = icon("sync"),
            style = "material-flat",
            size = "sm"
          ),
          div(
            id = "asmb_ctrls",
            style = "display: flex; flex-flow: row nowrap; align-items: center; gap: 1em;",
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
              icon = icon("circle-play"),
              style = "material-flat",
              size = "sm"
            )
          ),
          div(
            id = "annot_ctrls",
            #id = "ctrls",
            style = "display: flex; flex-flow: row nowrap; align-items: center; gap: 1em;",
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
              "id_verified_top",
              label = "ID Verified",
              style = "material-flat",
              size = "sm"
            ),
            shinyWidgets::actionBttn(
              "problematic_top",
              label = "Mark Problematic",
              style = "material-flat",
              size = "sm"
            ),
            shinyWidgets::actionBttn(
              "partial_top",
              label = "Mark Partial",
              style = "material-flat",
              size = "sm"
            ),
            shinyWidgets::actionBttn(
              "run_modal",
              label = "Update",
              icon = icon("circle-play"),
              style = "material-flat",
              size = "sm"
            )
          ),
          div(
            id = "export_ctrls",
            style = "display: flex; flex-flow: row nowrap; align-items: center; gap: 1em;",
            shinyWidgets::actionBttn(
              "group",
              label = "Assign Group",
              style = "material-flat",
              size = "sm"
            ),
            shinyWidgets::actionBttn(
              "clear_group",
              label = "Clear Group",
              style = "material-flat",
              size = "sm"
            ),
            shinyWidgets::actionBttn(
              "export",
              label = "Export Data",
              style = "material-flat",
              size = "sm"
            )
          ),
          workdir_browser_ui("workdir_browser")
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
          ),
          conditionalPanel(
            condition = "input.mode == 'Export'",
            annotate_ui("export")
          )
        )
      )
    )
  )
}
