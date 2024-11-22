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
          defaultColDef = colDef(align = "left"),
          columns = list(
            `.selection` = colDef(show = T, sticky = "left"),
            annotate_lock = colDef(
              show = TRUE,
              sticky = "left",
              name = "",
              html = TRUE,
              width = 30,
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
              width = 150,
              sticky = "left",
              html = TRUE,
              cell = rt_longtext()
            ),
            Taxon = colDef(
              show = TRUE,
              width = 150,
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
              width = 130,
              cell = rt_link(ns("set_curate_opts"))
            ),
            length = colDef(
              show = TRUE,
              width = 120,
              name = "Length",
              html = TRUE,
              cell = rt_longtext()
            ),
            topology = colDef(show = TRUE),
            scaffolds = colDef(show = TRUE),
            PCGCount = colDef(show = TRUE, name = "# PCGs"),
            tRNACount = colDef(show = TRUE, name = "# tRNAs"),
            rRNACount = colDef(show = TRUE, name = "# rRNAs"),
            missing = colDef(show = TRUE),
            extra = colDef(show = TRUE),
            warnings = colDef(show = TRUE),
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
              minWidth = 150
            ),
            view = colDef(
              show = TRUE,
              sticky = "right",
              name = "",
              html = TRUE,
              width = 40,
              align = "center",
              cell = rt_icon_bttn(ns("details"), "fas fa-square-arrow-up-right")
            )
          )
        )
    })

    # Set Annotate Options ----
    observeEvent(input$set_annotate_opts, {
      coming_soon()
    })

    # Set Curate Options ----
    observeEvent(input$set_curate_opts, {
      coming_soon()
    })
  })
}
