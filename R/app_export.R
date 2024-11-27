#' export UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
export_ui <- function(id) {
  ns <- NS(id)
  tagList(
    reactableOutput(ns("table"))
  )
}

#' export Server Functions
#'
#' @noRd
export_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Prepare data ----
    rv <- reactiveValues(
      data = fetch_export_data(),
      updating = NULL
    )

    # Refresh ----
    init("refresh_export")
    on("refresh_export", {
      rv$data <- fetch_export_data()
      updateReactable(
        "table",
        data = rv$data
      )
    })

    # Render table ----
    ouput$table <- reactable::renderReactable({
      reactable::reactable(
        dat,
        compact = TRUE,
        defaultPageSize = 100,
        showPageSizeOptions = TRUE,
        onClick = "select",
        selection = "multiple",
        searchable = TRUE,
        resizable = TRUE,
        height = 680,
        wrap = FALSE,
        pageSizeOptions = c(25, 50, 100, 200, 500),
        rowStyle = reactable_highlight_selected(),
        defaultColDef = colDef(align = "center", maxWidth = 200),
        columns = list(
          #`.selection` = colDef(show=F),
          ID = colDef(show = T, width = 120, sticky = "left"),
          topology = colDef(show = T, width = 100),
          structure = colDef(show = T, maxWidth = 600),
          submission_group = colDef(
            show= T, name = "Group" ),
          submission_status = colDef(
            show= T, name = "Status" )
        )
      )
    })

  })
}

