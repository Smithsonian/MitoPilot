#' annotate UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
annotate_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tagList(
      reactable::reactableOutput(ns("table"))
    )
  )
}

#' annotate Server Functions
#'
#' @noRd
annotate_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Prepare data ----
    rv <- reactiveValues(
      curate_opts = dplyr::tbl(session$userData$con, "curate_opts") |>
        dplyr::collect(),
      annotate_opts = dplyr::tbl(session$userData$con, "annotate_opts") |>
        dplyr::collect(),
      data = fetch_annotate_data(),
      updating = NULL
    )

    # Refresh ----
    init("refresh_annotate")
    on("refresh_annotate", {
      rv$data <- fetch_annotate_data()
      updateReactable(
        "table",
        data = rv$data
      )
    })

    # Render table ----
    output$table <- renderReactable({
      isolate(req(rv$data)) |>
        reactable(
          compact = TRUE,
          defaultPageSize = 100,
          showPageSizeOptions = TRUE,
          onClick = "select",
          selection = "multiple",
          searchable = TRUE,
          defaultSorted = list(time_stamp = "desc"),
          height = 650,
          wrap = FALSE,
          pageSizeOptions = c(25, 50, 100, 200, 500),
          rowStyle = rt_highlight_row(),
          defaultColDef = colDef(align = "left", show = FALSE, maxWidth = 85),
          columns = list(
            `.selection` = colDef(show = T, sticky = "left", width = 28),
            annotate_lock = colDef(
              show = TRUE,
              sticky = "left",
              name = "",
              html = TRUE,
              width = 32,
              align = "center",
              cell = rt_dynamicIcon(
                c(
                  `0` = "fa fa-lock-open",
                  `1` = "fa fa-lock"
                )
              )
            ),
            annotate_switch = colDef(
              show = TRUE,
              sticky = "left",
              name = "",
              html = TRUE,
              width = 30,
              align = "center",
              cell = rt_dynamicIcon(
                c(
                  `0` = "fa fa-circle-notch",
                  `1` = "fa fa-hourglass",
                  `2` = "fa fa-circle-check",
                  `3` = "fa fa-triangle-exclamation"
                )
              )
            ),
            ID = colDef(
              show = TRUE,
              width = 120,
              sticky = "left",
              html = TRUE,
              cell = rt_longtext()
            ),
            Taxon = colDef(
              show = TRUE,
              width = 140,
              sticky = "left",
              html = TRUE,
              cell = rt_longtext()
            ),
            annotate_opts = colDef(
              show = TRUE,
              name = "Annotate Opts.",
              html = TRUE,
              width = 130,
              cell = rt_link(ns("set_annotate_opts"))
            ),
            curate_opts = colDef(
              show = TRUE,
              name = "Curate Opts.",
              html = TRUE,
              width = 110,
              cell = rt_link(ns("set_curate_opts"))
            ),
            length = colDef(
              show = TRUE,
              name = "Length",
              html = TRUE,
              cell = rt_longtext()
            ),
            topology = colDef(show = TRUE),
            scaffolds = colDef(show = TRUE, align = 'center'),
            PCGCount = colDef(show = TRUE, name = "# PCGs", align = 'center'),
            tRNACount = colDef(show = TRUE, name = "# tRNAs", align = 'center'),
            rRNACount = colDef(show = TRUE, name = "# rRNAs", align = 'center'),
            missing = colDef(show = TRUE, align = 'center', html = TRUE, cell = rt_longtext()),
            extra = colDef(show = TRUE, align = 'center'),
            warnings = colDef(show = TRUE, align = 'center'),
            time_stamp = colDef(
              show = TRUE,
              name = "Last Updated",
              html = T,
              width = 150,
              cell = rt_ts_date()
            ),
            annotate_notes = colDef(
              show = TRUE,
              name = "Notes",
              html = TRUE,
              align = "left",
              minWidth = 150,
              maxWidth = 400,
              cell = rt_longtext()
            ),
            view = colDef(
              show = TRUE,
              sticky = "right",
              name = "",
              html = TRUE,
              width = 80,
              align = "center",
              cell = rt_icon_bttn_text(ns("details"), "fas fa-square-arrow-up-right fa-xs")
            ),
            output = colDef(
              show = TRUE,
              sticky = "right",
              name = "",
              html = TRUE,
              width = 80,
              align = "center",
              cell = rt_icon_bttn_text(ns("output"), "fas fa-folder-open fa-xs")
            )
          )
        )
    })

    # update table ----
    init("update_annotate_table")
    on("update_annotate_table", {
      reactable::updateReactable(
        "table",
        data = rv$data  |>
          dplyr::mutate(
            output = dplyr::case_when(
              annotate_switch > 1 ~ "output",
              .default = NA_character_
            ),
            view = dplyr::case_when(
              annotate_switch > 1 ~ "details",
              .default = NA_character_
            )
          ),
        selected = reactable::getReactableState("table", "selected"),
        page = reactable::getReactableState("table", "page")
      )
    })

    # table selection ----
    selected <- reactive(reactable::getReactableState("table", "selected"))

    # Set State ----
    on("state", {
      req(session$userData$mode == "Annotate")
      req(selected())
      req(all(rv$data$annotate_lock[req(selected())] == 0))
      rv$updating <- rv$data |>
        dplyr::select(ID, annotate_switch) |>
        dplyr::slice(selected())
      current <- character(0)
      if (length(unique(rv$updating$annotate_switch)) == 1) {
        current <- rv$updating$annotate_switch[1]
      }
      showModal(
        modalDialog(
          title = "Select New State:",
          shinyWidgets::prettyRadioButtons(
            ns("new_state"),
            label = NULL,
            choices = c("Pre-Annotate (wait)" = 0, "Ready to Annotate" = 1, "Successful Aannotation" = 2),
            selected = current,
            shape = "square",
            status = "primary"
          ),
          size = "m",
          footer = tagList(
            actionButton(ns("update_state"), "Update"),
            modalButton("Cancel")
          )
        )
      )
    })
    observeEvent(input$update_state, {
      rv$updating$annotate_switch <- as.numeric(input$new_state)
      dplyr::tbl(session$userData$con, "annotate") |>
        dplyr::rows_update(
          rv$updating,
          unmatched = "ignore",
          in_place = TRUE,
          copy = TRUE,
          by = "ID"
        )
      rv$data <- rv$data |>
        dplyr::rows_update(
          rv$updating,
          by = "ID"
        )
      trigger("update_annotate_table")
      removeModal()
    })

    # Toggle lock ----
    on("lock", {
      req(session$userData$mode == "Annotate")
      req(selected())
      rv$updating <- rv$data |>
        dplyr::select(ID, annotate_lock) |>
        dplyr::slice(selected())
      lock_current <- as.numeric(names(which.max(table(rv$updating$annotate_lock))))
      rv$updating$annotate_lock <- as.numeric(!lock_current)
      dplyr::tbl(session$userData$con, "annotate") |>
        dplyr::rows_update(
          rv$updating,
          unmatched = "ignore",
          in_place = TRUE,
          copy = TRUE,
          by = "ID"
        )
      rv$data <- rv$data |>
        dplyr::rows_update(rv$updating, by = "ID")
      trigger("update_annotate_table")
    })

    # Set Annotate Options ----
    observeEvent(input$set_annotate_opts, {
      coming_soon()
    })

    # Set Curate Options ----
    observeEvent(input$set_curate_opts, {
      coming_soon()
    })

    # Download files ----
    observeEvent(input$download, {
      comming_soon()
    })

    # Open annotation details ----
    observeEvent(input$details, {
      rv$updating <- rv$data |> dplyr::slice(as.numeric(input$details))
      trigger("annotations_modal")
    })
    annotations_details_server(ns("annotations"), rv)
  })
}
