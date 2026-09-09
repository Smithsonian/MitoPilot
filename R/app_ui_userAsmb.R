#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny reactable
#' @noRd
app_ui_userAsmb <- function(request) {
  # `lang` has to sit on the object shiny renders, which is this tagList.
  structure(lang = "en", tagList(
    add_external_resources(),
    fluidPage(
      tags$a(href = "#mp-content", class = "sr-only sr-only-focusable", "Skip to table"),
      div(
        style = "display: flex; flex-direction: column;",
        tags$header(
          role = "banner",
          uiOutput("app_header"),
          div(
            class = "mp-toolbar",
            shinyWidgets::radioGroupButtons(
              inputId = "mode",
              label = "Step:",
              choiceNames = list(
                HTML("<span class='mp-step-n'>1</span> Assemble"),
                HTML("<span class='mp-step-n'>2</span> Annotate"),
                HTML("<span class='mp-step-n'>3</span> Export")
              ),
              choiceValues = c("Assemble", "Annotate", "Export"),
              selected = "Assemble",
              size = "sm"
            ),
            mp_toolbar_button(
              "refresh",
              label = NULL,
              icon = mp_icon("arrows-rotate"),
              title = "Reload the table from the database"
            ),
            conditionalPanel(
              condition = "input.mode == 'Assemble'",
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
                emphasis = "primary",
                title = "Review and launch the assembly pipeline"
              )
            ),
            conditionalPanel(
              condition = "input.mode == 'Annotate'",
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
                emphasis = "primary",
                title = "Review and launch the annotation pipeline"
              )
            ),
            conditionalPanel(
              condition = "input.mode == 'Export'",
              id = "export_ctrls",
              class = "mp-toolbar",
              mp_toolbar_button(
                "group", "Group",
                title = "Assign the selected samples to an export group",
                needs_selection = TRUE
              ),
              mp_toolbar_button(
                "export", "Export Data",
                emphasis = "primary",
                title = "Open the export window for the selected samples"
              )
            ),
            workdir_browser_ui("workdir_browser")
          )
        ),
        tags$main(
          id = "mp-content",
          style = "padding: 1em;",
          conditionalPanel(
            condition = "input.mode == 'Assemble'",
            tags$h1(class = "mp-module-h1", "Assemble"),
            assemble_ui_userAsmb("assemble")
          ),
          conditionalPanel(
            condition = "input.mode == 'Annotate'",
            tags$h1(class = "mp-module-h1", "Annotate"),
            annotate_ui("annotate")
          ),
          conditionalPanel(
            condition = "input.mode == 'Export'",
            tags$h1(class = "mp-module-h1", "Export"),
            export_ui("export")
          )
        )
      )
    )
  ))
}
