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
          mp_toolbar_button(
            "refresh",
            label = NULL,
            icon = mp_icon("arrows-rotate"),
            title = "Reload the table from the database"
          ),
          div(
            id = "asmb_ctrls",
            class = "mp-toolbar",
            mp_toolbar_button(
              "state", "State",
              title = "Set the pipeline state of the selected samples",
              needs_selection = TRUE
            ),
            mp_toolbar_button(
              "lock", "Lock",
              title = MP_LOCK_DEF("assemble"),
              needs_selection = TRUE
            ),
            mp_toolbar_button(
              "run_modal", "Update",
              icon = mp_icon("circle-play"), emphasis = "primary",
              title = "Review and launch the assembly pipeline"
            )
          ),
          div(
            id = "annot_ctrls",
            class = "mp-toolbar",
            mp_toolbar_button(
              "state", "State",
              title = "Set the pipeline state of the selected samples",
              needs_selection = TRUE
            ),
            mp_toolbar_button(
              "lock", "Lock",
              title = MP_LOCK_DEF("annotate"),
              needs_selection = TRUE
            ),
            mp_toolbar_button(
              "id_verified_top", "Mark ID Verified",
              title = "Mark or clear species ID verified on the selected samples",
              needs_selection = TRUE
            ),
            mp_toolbar_button(
              "problematic_top", "Mark Problematic",
              title = "Mark or clear problematic on the selected samples",
              needs_selection = TRUE
            ),
            mp_toolbar_button(
              "partial_top", "Mark Partial",
              title = "Mark or clear partial on the selected samples",
              needs_selection = TRUE
            ),
            mp_toolbar_button(
              "run_modal", "Update",
              icon = mp_icon("circle-play"), emphasis = "primary",
              title = "Review and launch the annotation pipeline"
            )
          ),
          div(
            id = "export_ctrls",
            class = "mp-toolbar",
            mp_toolbar_button(
              "group", "Assign Group",
              title = "Assign the selected samples to an export group",
              needs_selection = TRUE
            ),
            mp_toolbar_button(
              "clear_group", "Clear Group",
              title = "Remove the selected samples from their export group",
              needs_selection = TRUE
            ),
            mp_toolbar_button(
              "export", "Export Data",
              emphasis = "primary",
              title = "Open the export window for the selected samples"
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
            export_ui("export")
          )
        )
      )
    )
  )
}

#' A decorative glyph.
#'
#' `shiny::icon()` names every glyph after itself ("circle-play icon"), which
#' is never the action, and pairs that name with `role = "presentation"`. The
#' button's own text (or its `aria-label`) is the name; the glyph is decoration.
#'
#' @noRd
mp_icon <- function(name) {
  i <- icon(name)
  i$attribs$`aria-label` <- NULL
  i$attribs$role <- NULL
  tagAppendAttributes(i, `aria-hidden` = "true")
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
