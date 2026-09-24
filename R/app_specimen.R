#' Specimen metadata state per sample: worst of fetch status and conflicts
#'
#' @param con database connection
#' @return data.frame `ID`, `specimen` ("ok" | "failed" | "conflict" | "none"),
#'   `specimen_message` (tooltip text, one line per source then conflicts and notes)
#' @noRd
specimen_status <- function(con) {
  .meta_ensure_tables(con)
  cols <- vapply(META_SOURCES, function(s) s$col, character(1))
  s <- DBI::dbGetQuery(con, paste0("SELECT ID, ", paste(cols, collapse = ", "), " FROM samples"))
  st <- DBI::dbGetQuery(con, "SELECT ID, source, status, message FROM meta_status")
  cf <- specimen_conflicts(con)
  state <- msg <- character(nrow(s))
  for (i in seq_len(nrow(s))) {
    id <- s$ID[i]
    lines <- states <- character()
    for (src in names(META_SOURCES)) {
      ref <- s[[META_SOURCES[[src]]$col]][i]
      r <- st[st$ID == id & st$source == src, , drop = FALSE]
      if (nrow(r)) {
        states <- c(states, r$status[1])
        lines <- c(lines, if (r$status[1] == "ok") paste0(src, ": fetched") else
          paste0(src, ": failed (", r$message[1] %|NA|% "unknown error", ")"))
      } else if (!is.na(ref) && nzchar(ref)) {
        lines <- c(lines, paste0(src, ": not fetched yet"))
      }
    }
    k <- cf[cf$ID == id, , drop = FALSE]
    conf <- k$concept[k$status %in% "conflict"]
    note <- k$concept[k$status %in% "note"]
    if (length(conf)) lines <- c(lines, paste("Conflicts:", paste(conf, collapse = ", ")))
    if (length(note)) lines <- c(lines, paste("Notes:", paste(note, collapse = ", ")))
    state[i] <- if ("failed" %in% states) "failed" else if (length(conf)) "conflict" else
      if ("ok" %in% states) "ok" else "none"
    msg[i] <- if (length(lines)) paste(lines, collapse = "\n") else "No GEOME BCID or GBIF ID"
  }
  data.frame(ID = s$ID, specimen = state, specimen_message = msg)
}

#' Left-join specimen status onto a collected, ID-keyed data frame
#'
#' @param df data frame with an `ID` column
#' @param con database connection
#' @noRd
.specimen_status_join <- function(df, con) {
  out <- dplyr::left_join(df, specimen_status(con), by = "ID")
  out$specimen[is.na(out$specimen)] <- "none"
  out
}

#' reactable cell renderer for the Specimen column
#'
#' @param inputId namespaced Shiny input id to receive the clicked row's ID
#' @noRd
rt_specimen <- function(inputId) {
  sprintf(
    "function(cellInfo) {
      var st = cellInfo.value || 'none';
      var row = cellInfo.row || {};
      var esc = function(s) { return String(s).replace(/&/g, '&amp;').replace(/'/g, '&#39;')
        .replace(/\"/g, '&quot;').replace(/</g, '&lt;').replace(/>/g, '&gt;'); };
      var cls = {
        ok: 'fa-solid fa-earth-americas',
        failed: 'fa-solid fa-triangle-exclamation mp-fg-warning',
        conflict: 'fa-solid fa-flag mp-fg-warning',
        none: 'fa-regular fa-square-plus text-muted'
      }[st] || 'fa-regular fa-square-plus text-muted';
      var tip = (row['specimen_message'] || 'No GEOME BCID or GBIF ID') +
        (st === 'none' ? '. Click to add one.' : '\\nClick to view.');
      return `<a href='#' class='mp-specimen-cell' data-id='${esc(row['ID'])}' title='${esc(tip)}' aria-label='${esc(tip)}' ` +
        `onclick=\"event.preventDefault(); event.stopPropagation(); Shiny.setInputValue('%s', this.dataset.id, {priority: 'event'})\">` +
        `<i class='${cls}' aria-hidden='true'></i></a>`;
    }",
    inputId
  ) |>
    htmlwidgets::JS()
}

#' Shared colDef for the Specimen column
#'
#' @param inputId namespaced Shiny input id to receive the clicked row's ID
#' @noRd
specimen_col_def <- function(inputId, sticky = NULL, class = NULL) {
  reactable::colDef(
    show = TRUE, name = "Specimen", sticky = sticky, width = 80, align = "center",
    html = TRUE, filterable = FALSE, sortable = TRUE,
    class = class, headerClass = class,
    header = rt_header("Specimen", paste(
      "GEOME and GBIF metadata for this sample. Click an icon to view, add,",
      "compare, or refresh.")),
    cell = rt_specimen(inputId)
  )
}

#' TRUE when any sample has a non-blank GEOME BCID or GBIF ID
#'
#' @param con database connection
#' @noRd
.specimen_project_has_ids <- function(con) {
  .meta_ensure_tables(con)
  DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM samples
                        WHERE (GEOME_BCID IS NOT NULL AND TRIM(GEOME_BCID) != '')
                           OR (GBIF_ID IS NOT NULL AND TRIM(GBIF_ID) != '')")$n > 0
}

#' Drop the Specimen group from a default column-group selection when no
#' sample has a GEOME BCID or GBIF ID
#'
#' @param groups character vector of group names
#' @param con database connection
#' @noRd
.specimen_default_groups <- function(groups, con) {
  if (.specimen_project_has_ids(con)) groups else setdiff(groups, "Specimen")
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

#' Modal listing GEOME fields available at export
#'
#' @param ns module namespace function
#' @param s `meta_field_summary()` output
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
#' @param open reactive yielding the sample ID to open (from a `specimen_open` input)
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
                            FROM samples s LEFT JOIN meta_status g ON s.ID = g.ID AND g.source = 'GEOME'
                            ORDER BY s.ID")
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
      recs <- DBI::dbGetQuery(con, "SELECT level, depth, ref AS bcid, field, value FROM meta_records
                                    WHERE ID = ? AND source = 'GEOME'", params = list(rv$id))
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
        val <- .meta_set_ref(con, "GEOME", rv$id, input$bcid)
        if (!is.na(val)) {
          withProgress(message = "Fetching from GEOME", {
            res <- suppressWarnings(.meta_fetch_into(con, "GEOME", rv$id, val))
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
            suppressWarnings(.meta_fetch_into(con, "GEOME", s$ID[i], s$GEOME_BCID[i], cache))
            incProgress(1 / nrow(s), detail = s$ID[i])
          }
        })
        bump()
      }, error = function(e) showNotification(conditionMessage(e), type = "error"))
    })
  })
}
