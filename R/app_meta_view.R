#' Metadata button for a table's filter row
#' @noRd
meta_view_button <- function(ns) {
  actionButton(ns("meta_view_open"), "Metadata", icon = icon("table-columns"),
               class = "btn-default mp-meta-btn",
               title = "Choose which metadata columns the tables show")
}

#' Field chooser table for the Metadata modal
#' @noRd
meta_view_picker_table <- function(fields) {
  reactable::reactable(
    fields[, c("source", "level", "field", "n_samples", "example")],
    selection = "multiple", onClick = "select", compact = TRUE, searchable = TRUE,
    defaultSelected = which(fields$shown), pagination = FALSE, height = 420,
    striped = TRUE, resizable = TRUE, class = "mp-meta-view-tbl",
    columns = list(
      source = reactable::colDef(
        name = "Source", width = 110, filterable = TRUE,
        cell = function(v) htmltools::tagList(meta_view_logo(v), v)
      ),
      level = reactable::colDef(name = "Level", minWidth = 110),
      field = reactable::colDef(name = "Field", minWidth = 160),
      n_samples = reactable::colDef(name = "Samples", width = 80, align = "center"),
      example = reactable::colDef(name = "Example", minWidth = 180, html = TRUE, cell = rt_longtext())
    )
  )
}

#' Metadata modal
#' @noRd
meta_view_modal <- function(ns, fields, wrap) {
  tbl <- ns("meta_view_tbl")
  src_btn <- function(label, value) {
    js <- if (is.na(value)) sprintf("Reactable.setFilter('%s', 'source', undefined)", tbl)
          else sprintf("Reactable.setFilter('%s', 'source', '%s')", tbl, value)
    tags$button(type = "button", class = "btn btn-default btn-sm", onclick = js, label)
  }
  modalDialog(
    title = "Metadata columns", size = "l", easyClose = TRUE,
    p("Pick the metadata columns to show in the Assemble, Annotate, and Export tables.",
      "This only changes what you see; exported files are set by Choose fields in",
      "the Export header template."),
    if (!nrow(fields)) {
      p(class = "mp-empty-state", "No metadata yet. Add columns to your mapping file and load",
        "them into the project with update_sample_metadata(), or fetch GEOME or GBIF",
        "records from the Metadata column.")
    } else {
      tagList(
        div(
          class = "mp-meta-view-bar",
          src_btn("All", NA), src_btn("Map file", "Map file"),
          src_btn("GEOME", "GEOME"), src_btn("GBIF", "GBIF"),
          tags$button(type = "button", class = "btn btn-default btn-sm",
                      onclick = sprintf("Reactable.getInstance('%s').toggleAllRowsSelected(false)", tbl),
                      "Clear all"),
          checkboxInput(ns("meta_view_wrap"), "Wrap long text (up to 3 lines)", value = wrap)
        ),
        reactable::reactableOutput(tbl)
      )
    },
    footer = mp_footer(primary = if (nrow(fields)) actionButton(ns("meta_view_save"), "Save"))
  )
}

#' Wire the Metadata button into a table module
#'
#' Call inside a table's moduleServer. Saving triggers the shared "meta_view"
#' flag, which every table watches.
#' @noRd
meta_view_setup <- function(input, output, session) {
  if (is.null(session$userData[["meta_view"]])) init("meta_view", session = session)
  con <- session$userData$con
  fields_rv <- reactiveVal(NULL)

  observeEvent(input$meta_view_open, {
    f <- meta_view_fields(con)
    fields_rv(f)
    showModal(meta_view_modal(session$ns, f, meta_view_wrap(con)))
  })
  output$meta_view_tbl <- reactable::renderReactable(meta_view_picker_table(req(fields_rv())))

  observeEvent(input$meta_view_save, {
    f <- req(fields_rv())
    sel <- reactable::getReactableState("meta_view_tbl", "selected") %||% integer(0)
    meta_view_save(con, f, f$key[sel], input$meta_view_wrap)
    removeModal()
    trigger("meta_view", session = session)
  })

  observe({
    watch("meta_view", session = session)
    n <- sum(meta_view_fields(con)$shown)
    updateActionButton(session, "meta_view_open", label = sprintf("Metadata (%d)", n))
  })
}
