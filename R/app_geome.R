#' Left-join GEOME fetch status onto a lazy samples-keyed table
#'
#' @param tbl a lazy dplyr table keyed by `ID`
#' @param db database connection (source of `geome_status`)
#' @return `tbl` with `geome` ("ok" | "failed" | "none") and `geome_message` added
#' @noRd
.geome_status_join <- function(tbl, db) {
  .geome_ensure_tables(db)
  tbl |>
    dplyr::left_join(
      dplyr::tbl(db, "geome_status") |>
        dplyr::select(ID, geome_status = status, geome_message = message),
      by = "ID"
    ) |>
    dplyr::mutate(geome = dplyr::case_when(
      geome_status == "ok" ~ "ok",
      geome_status == "failed" ~ "failed",
      TRUE ~ "none"
    )) |>
    dplyr::select(-geome_status)
}

#' reactable cell renderer for the GEOME status column
#'
#' Renders a clickable icon; clicking sends the row's ID to `inputId`.
#'
#' @param inputId namespaced Shiny input id to receive the clicked row's ID
#' @noRd
rt_geome <- function(inputId) {
  sprintf(
    "function(cellInfo) {
      var st = cellInfo.value || 'none';
      var row = cellInfo.row || {};
      var esc = function(s) { return String(s).replace(/&/g, '&amp;').replace(/'/g, '&#39;')
        .replace(/\"/g, '&quot;').replace(/</g, '&lt;').replace(/>/g, '&gt;'); };
      var cls = st === 'ok' ? 'fa-solid fa-earth-americas' :
        (st === 'failed' ? 'fa-solid fa-triangle-exclamation mp-fg-warning' : 'fa-regular fa-square-plus text-muted');
      var tip = st === 'ok' ? 'GEOME record fetched. Click to view.' :
        (st === 'failed' ? 'GEOME fetch failed: ' + (row['geome_message'] || 'unknown error') + '. Click to fix or retry.' :
        'No GEOME BCID. Click to add one.');
      return `<a href='#' class='mp-geome-cell' data-id='${esc(row['ID'])}' title='${esc(tip)}' aria-label='${esc(tip)}' ` +
        `onclick=\"event.preventDefault(); event.stopPropagation(); Shiny.setInputValue('%s', this.dataset.id, {priority: 'event'})\">` +
        `<i class='${cls}' aria-hidden='true'></i></a>`;
    }",
    inputId
  ) |>
    htmlwidgets::JS()
}

#' Shared colDef for the GEOME status column
#'
#' @param inputId namespaced Shiny input id to receive the clicked row's ID
#' @noRd
geome_col_def <- function(inputId, sticky = NULL, class = NULL) {
  reactable::colDef(
    show = TRUE, name = "GEOME", sticky = sticky, width = 70, align = "center",
    html = TRUE, filterable = FALSE, sortable = TRUE,
    class = class, headerClass = class,
    header = rt_header("GEOME", "GEOME metadata for this sample. Click an icon to view, add, or refresh."),
    cell = rt_geome(inputId)
  )
}

#' TRUE when any sample in the project has a non-blank GEOME BCID
#'
#' @param con database connection
#' @noRd
.geome_project_has_bcids <- function(con) {
  .geome_ensure_tables(con)
  DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM samples
                        WHERE GEOME_BCID IS NOT NULL AND TRIM(GEOME_BCID) != ''")$n > 0
}

#' Drop the GEOME group from a default column-group selection when the
#' project has no GEOME BCIDs
#'
#' @param groups character vector of group names
#' @param con database connection
#' @noRd
.geome_default_groups <- function(groups, con) {
  if (.geome_project_has_bcids(con)) groups else setdiff(groups, "GEOME")
}

#' Render one sample's GEOME records as level cards, root first
#'
#' @param recs `geome_records` rows for one sample (level, depth, bcid, field, value)
#' @param box_id DOM id of the scroll box holding the cards
#' @return Expand/Collapse all buttons and a scroll box of `<details>` cards
#' @noRd
geome_record_view <- function(recs, box_id = "geome-records") {
  if (!nrow(recs)) return(p(class = "text-muted", "No GEOME data stored for this sample yet."))
  toggle <- function(label, open) {
    tags$button(
      type = "button", class = "btn btn-default btn-sm", label,
      onclick = sprintf("document.querySelectorAll('#%s details').forEach(function(d) { d.open = %s; });",
                        box_id, tolower(open))
    )
  }
  lv <- unique(recs[order(-recs$depth), c("level", "depth", "bcid")])
  cards <- lapply(seq_len(nrow(lv)), function(i) {
    r <- recs[recs$depth == lv$depth[i], ]
    tags$details(
      open = NA, class = "mp-geome-level",
      tags$summary(
        strong(lv$level[i]),
        if (!is.na(lv$bcid[i])) tagList(" ", tags$a(
          href = paste0("https://geome-db.org/record/", lv$bcid[i]),
          target = "_blank", rel = "noopener", lv$bcid[i]))
      ),
      tags$table(class = "table table-sm",
        tags$tbody(lapply(seq_len(nrow(r)), function(j) {
          tags$tr(tags$th(r$field[j]), tags$td(r$value[j]))
        }))
      )
    )
  })
  tagList(
    div(style = "margin-bottom: 6px;", toggle("Expand all", TRUE), " ", toggle("Collapse all", FALSE)),
    div(id = box_id, style = "max-height: 60vh; overflow-y: auto;", cards)
  )
}

#' Replace the set of GEOME fields available at export
#'
#' @param con database connection
#' @param keys character vector of `combo:<name>` / `raw:<level>:<field>` keys
#' @noRd
.geome_save_fields <- function(con, keys) {
  DBI::dbWithTransaction(con, {
    DBI::dbExecute(con, "DELETE FROM geome_export_fields")
    if (length(keys)) DBI::dbAppendTable(con, "geome_export_fields", data.frame(key = unique(keys)))
  })
  invisible(keys)
}

#' Modal listing GEOME fields available at export
#'
#' @param ns module namespace function
#' @param s `geome_field_summary()` output
#' @noRd
geome_fields_modal <- function(ns, s) {
  combos <- s[s$kind == "combo", ]
  raw <- s[s$kind == "raw", ]
  modalDialog(
    title = mp_modal_title("GEOME fields for export",
                           "Ticked fields become columns you can use in header templates"),
    size = "l", easyClose = TRUE,
    h5("GenBank-ready combinations"),
    checkboxGroupInput(
      ns("geome_combos"), NULL, width = "100%",
      choiceValues = combos$key, selected = combos$key[combos$selected],
      choiceNames = lapply(seq_len(nrow(combos)), function(i) tagList(
        code(paste0("{", combos$col[i], "}")), " from ", combos$field[i], ": ",
        if (is.na(combos$example[i])) em("no samples") else
          tagList(tags$samp(combos$example[i]), sprintf(" (%d samples)", combos$n_samples[i]))
      ))
    ),
    h5("All GEOME fields"),
    reactable::reactableOutput(ns("geome_raw")),
    footer = mp_footer(primary = actionButton(ns("geome_fields_save"), "Save"), dismiss = "Cancel")
  )
}

#' GEOME viewer modal: view records, add/edit a BCID, fetch/refresh
#'
#' @param id module id
#' @param open reactive yielding the sample ID to open (from a `geome_open` input)
#' @param on_change function called after any DB write, so the caller can refresh its table
#' @noRd
geome_viewer_server <- function(id, open, on_change = function() NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    con <- session$userData$con
    rv <- reactiveValues(id = NULL, ver = 0L)
    bump <- function() { rv$ver <- rv$ver + 1L; on_change() }

    samples <- function() {
      DBI::dbGetQuery(con, "SELECT s.ID, s.Taxon, s.GEOME_BCID, g.status, g.message, g.fetched_at
                            FROM samples s LEFT JOIN geome_status g ON s.ID = g.ID ORDER BY s.ID")
    }

    observeEvent(open(), {
      rv$id <- open()
      s <- samples()
      lab <- paste0(s$ID, ifelse(is.na(s$status), "", ifelse(s$status == "failed", " (failed)", "")))
      modalDialog(
        title = mp_modal_title(
          tagList("GEOME metadata: ", textOutput(ns("hdr_id"), inline = TRUE)),
          subtitle = tagList("Taxon: ", textOutput(ns("hdr_taxon"), inline = TRUE))
        ),
        size = "l", easyClose = TRUE,
        fluidRow(
          column(3,
            selectInput(ns("sample"), "Sample", choices = stats::setNames(s$ID, lab),
                        selected = rv$id, width = "100%", selectize = FALSE, size = 15),
            uiOutput(ns("failed"))
          ),
          column(9, uiOutput(ns("detail")))
        ),
        footer = mp_footer(
          extra = actionButton(ns("refresh_all"), "Refresh all",
                               title = "Fetch every sample with a BCID again"),
          dismiss = "Close"
        )
      ) |> showModal()
    })

    observeEvent(input$sample, rv$id <- input$sample, ignoreInit = TRUE)

    output$hdr_id <- renderText(rv$id)
    output$hdr_taxon <- renderText({
      req(rv$id)
      s <- samples()
      s$Taxon[s$ID == rv$id] %|NA|% "NA"
    })

    output$failed <- renderUI({
      rv$ver
      s <- samples()
      bad <- s$ID[!is.na(s$status) & s$status == "failed"]
      if (!length(bad)) return(NULL)
      div(class = "mp-fg-warning", icon("triangle-exclamation"), " Failed: ",
          paste(bad, collapse = ", "))
    })

    output$detail <- renderUI({
      rv$ver
      req(rv$id)
      s <- samples()
      s <- s[s$ID == rv$id, ]
      recs <- DBI::dbGetQuery(con, "SELECT level, depth, bcid, field, value FROM geome_records WHERE ID = ?",
                              params = list(rv$id))
      tagList(
        div(class = "mp-geome-bcid",
          textInput(ns("bcid"), "GEOME BCID", value = s$GEOME_BCID %|NA|% "",
                    placeholder = "ark:/21547/...", width = "420px"),
          actionButton(ns("fetch"), "Fetch", icon = icon("arrows-rotate"))
        ),
        if (!is.na(s$status)) p(class = if (s$status == "failed") "mp-fg-warning" else "text-muted",
          if (s$status == "failed") paste("Last fetch failed:", s$message) else "Fetched",
          " ", format(as.POSIXct(s$fetched_at, origin = "1970-01-01"), "%Y-%m-%d %H:%M")),
        if (is.na(s$GEOME_BCID) && !nrow(recs)) p(class = "text-muted",
          "This sample has no GEOME BCID. Paste one above and click Fetch, or add a GEOME_BCID ",
          "column to your mapping file (see the GEOME Metadata article)."),
        geome_record_view(recs, box_id = ns("records"))
      )
    })

    observeEvent(input$fetch, {
      req(rv$id)
      tryCatch({
        val <- .geome_set_bcid(con, rv$id, input$bcid)
        if (!is.na(val)) {
          withProgress(message = "Fetching from GEOME", {
            res <- suppressWarnings(.geome_fetch_into(con, rv$id, val))
          })
          if (res$status == "failed") showNotification(res$message, type = "warning")
        }
        bump()
      }, error = function(e) showNotification(conditionMessage(e), type = "error"))
    })

    observeEvent(input$refresh_all, {
      s <- samples()
      s <- s[!is.na(s$GEOME_BCID), ]
      if (!nrow(s)) return(showNotification("No samples have a GEOME BCID", type = "message"))
      cache <- new.env()
      tryCatch({
        withProgress(message = "Fetching from GEOME", value = 0, {
          for (i in seq_len(nrow(s))) {
            suppressWarnings(.geome_fetch_into(con, s$ID[i], s$GEOME_BCID[i], cache))
            incProgress(1 / nrow(s), detail = s$ID[i])
          }
        })
        bump()
      }, error = function(e) showNotification(conditionMessage(e), type = "error"))
    })
  })
}
