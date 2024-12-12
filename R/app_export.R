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
export_server <- function(id) {
  moduleServer(id, function(input, output, session) {
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
      trigger("update_export_table")
    })

    # Render table ----
    output$table <- reactable::renderReactable({
      reactable::reactable(
        isolate(rv$data),
        compact = TRUE,
        language = reactable::reactableLang(
          noData = "No Completed / Locked Annotations Found"
        ),
        defaultPageSize = 100,
        showPageSizeOptions = TRUE,
        onClick = "select",
        selection = "multiple",
        searchable = TRUE,
        resizable = TRUE,
        height = 680,
        wrap = FALSE,
        pageSizeOptions = c(25, 50, 100, 200, 500),
        rowStyle = rt_highlight_row(),
        defaultColDef = colDef(align = "center", maxWidth = 140),
        columns = list(
          ID = colDef(show = T, width = 120, sticky = "left"),
          topology = colDef(show = T, width = 100),
          structure = colDef(show = T, maxWidth = 600),
          export_group = colDef(name = "Group", sticky = "right")
        )
      )
    })

    # update table ----
    init("update_export_table")
    on("update_export_table", {
      reactable::updateReactable(
        "table",
        data = rv$data,
        selected = reactable::getReactableState("table", "selected"),
        page = reactable::getReactableState("table", "page")
      )
    })

    # table selection ----
    selected <- reactive(reactable::getReactableState("table", "selected"))

    # Group ----
    init("group")
    on("group", {
      req(session$userData$mode == "Export")
      req(selected())
      if (any(!is.na(rv$data$export_group[selected()]))) {
        shinyWidgets::confirmSweetAlert(
          title = "Re-assign group?",
          text = "Some selected samples are already assigned to an export group. Assigning them to a new group will not automatically remove them from previously generated export files. Do you want to continue?",
          inputId = ns("group_confirm"),
          btn_labels = c("No", "Yes"),
          btn_colors = c("#0056b3", "#0056b3")
        )
        req(F)
      }
      trigger("group_modal")
    })
    observeEvent(input$group_confirm, {
      req(input$group_confirm)
      trigger("group_modal")
    })

    init("group_modal")
    on("group_modal", {
      rv$updating <- rv$data |> dplyr::slice(selected())
      topologies <- rv$updating |>
        dplyr::pull(topology) |>
        unique()
      structures <- rv$updating |>
        dplyr::pull(structure) |>
        unique()
      group_current <- rv$updating |>
        dplyr::pull(export_group) |>
        unique()
      modalDialog(
        title = "Submission Group",
        size = "l",
        easyClose = FALSE,
        stringr::str_glue(
          "<b># Selected:</b> {nrow(rv$updating)}"
        ) |> HTML() |> p(),
        stringr::str_glue(
          "<b># Topology:</b> {paste(topologies, collapse=', ')}"
        ) |> HTML() |> p(),
        HTML("<b>Structure:") |> p(),
        list_to_li(structures),
        hr(),
        selectizeInput(
          ns("group_name"),
          label = "Group Name:",
          choices = c("", sort(unique(rv$data$export_group))),
          selected = character(0),
          options = list(
            create = TRUE,
            maxItems = 1
          )
        ),
        footer = tagList(
          actionButton(ns("make_group"), "Create"),
          modalButton("Close")
        )
      ) |> showModal()
    })

    # Make Group ----
    observeEvent(input$make_group, {
      rv$updating$export_group <- req(input$group_name)
      rv$data <- rv$data |>
        dplyr::rows_update(rv$updating[, c("ID", "export_group")], by = "ID")
      dplyr::tbl(session$userData$con, "samples") |>
        dplyr::rows_update(
          rv$updating[, c("ID", "export_group")],
          unmatched = "ignore",
          in_place = TRUE,
          copy = TRUE,
          by = "ID"
        )
      trigger("update_export_table")
      removeModal()
    })

    # Export data ----
    init("export")
    on("export", {
      req(nrow(rv$data) > 0)
      choices <- sort(unique(rv$data$export_group))
      req(length(choices) > 0)
      modalDialog(
        title = div(
          style = "display: flex; justify-content: space-between; align-items: center; height: 42px;",
          span("Export Data"),
          span(id = ns("gears"), class = "gears paused")
        ),
        size = "l",
        shinyWidgets::pickerInput(
          ns("export_group"),
          "Export Group:",
          choices = choices
        ),
        textAreaInput(
          ns("fasta_header"),
          "Fasta Header Template (reference columns from your sample data using '{}'):",
          "{ID} [organism={Taxon}] [topology={topology}] [mgcode=2] [location=mitochondrion] {Taxon} mitochondrion, complete genome",
          width = "100%",
        ),
        shinyWidgets::prettyCheckbox(
          ns("include_alignments"),
          "Generate Group-level PCG alignment summary",
          value = T,
          status = "primary"
        ),
        footer = tagList(
          actionButton(ns("export_data"), "Export"),
          modalButton("Close")
        )
      ) |> showModal()
    })

    observeEvent(input$export_data, ignoreInit = T, {
      req(input$export_group)
      shinyjs::removeClass("gears", "paused")
      shinyjs::disable("export_data")
      export_files(
        group = input$export_group,
        fasta_header = input$fasta_header,
        generateAAalignments = input$include_alignments,
        out_dir = session$userData$dir_out
      )
      shinyjs::addClass("gears", "paused")
      shinyjs::enable("export_data")
    })
  })
}
