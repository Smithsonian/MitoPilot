#' Metadata button for a table's filter row
#' @noRd
meta_view_button <- function(ns) {
  actionButton(ns("meta_view_open"), "Metadata", icon = icon("table-columns"),
               class = "btn-default mp-meta-btn",
               title = "Choose which metadata columns the tables show")
}

#' Field chooser table for the Metadata modal
#'
#' Show and Export ticks are checkboxes drawn from client-side state
#' (metaview.js), not reactable selection, so a row can carry two.
#' @noRd
meta_view_picker_table <- function(fields, tbl) {
  d <- fields[, c("key", "source", "level", "field", "n_samples", "example", "token")]
  d$show <- d$export <- ""
  d <- d[, c("show", "export", setdiff(names(d), c("show", "export")))]
  box <- function(kind, label, tip) reactable::colDef(
    name = label, width = 84, align = "center", sortable = FALSE, html = TRUE,
    header = htmlwidgets::JS(sprintf(
      "function() { return '<label class=\"mp-mv-head\" title=\"%s\"><input type=\"checkbox\" class=\"mp-mv-all\" data-tbl=\"%s\" data-kind=\"%s\"> %s</label>'; }",
      tip, tbl, kind, label)),
    cell = htmlwidgets::JS(sprintf("function(ci) { return mpMV.box('%s', ci.row.key, '%s'); }", tbl, kind)),
    filterable = kind == "show",
    filterMethod = if (kind == "show") htmlwidgets::JS(sprintf(
      "function(rows) { return rows.filter(function(r) { return mpMV.ticked('%s', r.values.key); }); }", tbl))
  )
  reactable::reactable(
    d, compact = TRUE, pagination = FALSE, height = 420, striped = TRUE, resizable = TRUE,
    class = "mp-meta-view-tbl",
    columns = list(
      show = box("show", "Show", "Tick or untick every row in view"),
      export = box("export", "Export", "Tick or untick every row in view"),
      key = reactable::colDef(show = FALSE),
      source = reactable::colDef(
        name = "Source", width = 110, filterable = TRUE,
        filterMethod = htmlwidgets::JS(
          "function(rows, id, v) { return rows.filter(function(r) { return v.indexOf(r.values.source) >= 0; }); }"),
        cell = function(v) htmltools::tagList(meta_view_logo(v), v)
      ),
      level = reactable::colDef(name = "Level", minWidth = 110, filterable = TRUE),
      field = reactable::colDef(name = "Field", minWidth = 150, filterable = TRUE),
      n_samples = reactable::colDef(
        name = "Samples", width = 80, align = "center",
        cell = function(v) {
          tot <- fields$n_total[1]
          htmltools::span(class = if (v < tot) "mp-mv-partial", title = if (v < tot)
            sprintf("Missing for %d of %d samples", tot - v, tot), sprintf("%d/%d", v, tot))
        }
      ),
      example = reactable::colDef(name = "Example", minWidth = 160, html = TRUE, cell = rt_longtext()),
      token = reactable::colDef(
        name = "Export Template", minWidth = 160, sortable = FALSE,
        cell = function(v) htmltools::tags$code(
          class = "mp-mv-token", title = paste(v, "(click to copy)"),
          onclick = "mpMV.copy(this)", v)
      )
    )
  )
}

#' Metadata modal: pick metadata columns to show in the tables and fields to
#' offer at export
#'
#' @param source source to filter to on open, or NULL for all
#' @param closed_input namespaced input id set when the modal closes, or NULL
#' @noRd
meta_view_modal <- function(ns, fields, wrap, link = TRUE, source = NULL, closed_input = NULL) {
  tbl <- ns("meta_view_tbl")
  btn <- function(label, js, cls = NULL, title = NULL) {
    tags$button(type = "button", class = paste("btn btn-default btn-sm", cls), onclick = js,
                title = title, label)
  }
  src_btn <- function(label) {
    tagAppendAttributes(btn(label, sprintf("mpMV.toggleSource('%s', this)", tbl), "mp-mv-toggle",
                            paste("Show", label, "rows; combine with other sources")),
                        `data-src` = label)
  }
  modalDialog(
    title = mp_modal_title("Metadata fields",
                           "Show adds a table column. Export makes a field usable in export header templates."),
    size = "xl", easyClose = TRUE,
    if (!nrow(fields)) {
      p(class = "mp-empty-state", "No metadata yet. Add columns to your mapping file and load",
        "them into the project with update_sample_metadata(), or fetch GEOME, GBIF, or NCBI",
        "records from the Metadata column.")
    } else {
      tagList(
        div(
          class = "mp-meta-view-bar",
          btn("All", sprintf("mpMV.clear('%s')", tbl), title = "Clear all filters"),
          src_btn("Map file"), src_btn("GEOME"), src_btn("GBIF"), src_btn("NCBI"),
          btn("GenBank-ready", sprintf("mpMV.toggleFilter('%s', 'level', 'GenBank-ready', this)", tbl),
              "mp-mv-toggle", "Only the fields combined into GenBank source modifiers"),
          btn("Ticked only", sprintf("mpMV.toggleFilter('%s', 'show', 'on', this)", tbl),
              "mp-mv-toggle", "Only rows ticked in Show or Export"),
          tags$input(type = "search", class = "form-control input-sm mp-meta-view-search",
                     placeholder = "Filter field names", `aria-label` = "Filter field names",
                     oninput = sprintf("Reactable.setFilter('%s', 'field', this.value || undefined)", tbl))
        ),
        div(
          class = "mp-meta-view-bar",
          tags$label(class = "mp-mv-link",
                     title = "Clicking Show or Export ticks both (map file rows only have Show)",
                     tags$input(type = "checkbox", checked = if (link) NA,
                                onchange = sprintf("mpMV.st['%s'].link = this.checked", tbl)),
                     " Tick both together"),
          checkboxInput(ns("meta_view_wrap"), "Wrap long text (up to 3 lines)", value = wrap)
        ),
        tags$script(HTML(sprintf("mpMV.init(%s);", jsonlite::toJSON(list(
          tbl = tbl, show = fields$key[fields$shown],
          export = fields$key[fields$export %in% TRUE],
          exportable = fields$key[!is.na(fields$export)], always = sum(is.na(fields$export)),
          link = link, source = source %||% NA), auto_unbox = TRUE)))),
        reactable::reactableOutput(ns("meta_view_tbl"))
      )
    },
    if (!is.null(closed_input)) {
      tags$script(HTML(sprintf(
        "$('#shiny-modal').one('hidden.bs.modal', function() { Shiny.setInputValue('%s', Date.now(), {priority: 'event'}); });",
        closed_input)))
    },
    footer = mp_footer(
      extra = if (nrow(fields)) span(id = paste0(tbl, "_count"), class = "text-muted mp-mv-count"),
      primary = if (nrow(fields)) tags$button(
        type = "button", class = "btn btn-primary", "Save",
        onclick = sprintf("mpMV.save('%s', '%s')", tbl, ns("meta_view_state")))
    )
  )
}

#' Wire the Metadata button into a table module
#'
#' Call inside a table's moduleServer. Saving triggers the shared "meta_view"
#' flag, which every table watches.
#' @return function(source = NULL, closed_input = NULL) that opens the modal
#' @noRd
meta_view_setup <- function(input, output, session) {
  if (is.null(session$userData[["meta_view"]])) init("meta_view", session = session)
  con <- session$userData$con
  fields_rv <- reactiveVal(NULL)

  open <- function(source = NULL, closed_input = NULL) {
    f <- meta_view_fields(con)
    fields_rv(f)
    showModal(meta_view_modal(session$ns, f, meta_view_wrap(con), meta_view_link(con),
                              source, closed_input))
  }
  observeEvent(input$meta_view_open, open())
  output$meta_view_tbl <- reactable::renderReactable(
    meta_view_picker_table(req(fields_rv()), session$ns("meta_view_tbl")))

  observeEvent(input$meta_view_state, {
    f <- req(fields_rv())
    st <- input$meta_view_state
    meta_view_save(con, f, unlist(st$show) %||% character(0), input$meta_view_wrap,
                   export_keys = unlist(st$export) %||% character(0), link = isTRUE(st$link))
    removeModal()
    trigger("meta_view", session = session)
  })

  observe({
    watch("meta_view", session = session)
    n <- sum(meta_view_fields(con)$shown)
    updateActionButton(session, "meta_view_open", label = sprintf("Metadata (%d)", n))
  })
  open
}
