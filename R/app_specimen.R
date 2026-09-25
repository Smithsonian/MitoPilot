#' Specimen metadata state per sample: worst of fetch status and conflicts
#'
#' @param con database connection
#' @return data.frame `ID`, `specimen` ("ok" | "failed" | "conflict" | "none"),
#'   `specimen_message` (tooltip text, one line per source then conflicts and not-checked items),
#'   `specimen_icons` (e.g. "GEOME:ok GBIF:failed conflict"; per-source ok | failed | pending)
#' @noRd
specimen_status <- function(con) {
  .meta_ensure_tables(con)
  cols <- vapply(META_SOURCES, function(s) s$col, character(1))
  s <- DBI::dbGetQuery(con, paste0("SELECT ID, ", paste(cols, collapse = ", "), " FROM samples"))
  st <- DBI::dbGetQuery(con, "SELECT ID, source, status, message FROM meta_status")
  cf <- specimen_conflicts(con)
  state <- msg <- icons <- character(nrow(s))
  for (i in seq_len(nrow(s))) {
    id <- s$ID[i]
    lines <- states <- ic <- character()
    for (src in names(META_SOURCES)) {
      ref <- s[[META_SOURCES[[src]]$col]][i]
      r <- st[st$ID == id & st$source == src, , drop = FALSE]
      if (nrow(r)) {
        states <- c(states, r$status[1])
        ic <- c(ic, paste0(src, ":", r$status[1]))
        lines <- c(lines, if (r$status[1] == "ok") paste0(src, ": fetched") else
          paste0(src, ": failed (", r$message[1] %|NA|% "unknown error", ")"))
      } else if (!is.na(ref) && nzchar(ref)) {
        lines <- c(lines, paste0(src, ": not fetched yet"))
        ic <- c(ic, paste0(src, ":pending"))
      }
    }
    k <- cf[cf$ID == id, , drop = FALSE]
    conf <- k$concept[k$status %in% "conflict"]
    unchecked <- k$concept[k$status %in% "not checked"]
    if (length(conf)) lines <- c(lines, paste("Conflicts:", paste(conf, collapse = ", ")))
    if (length(unchecked)) lines <- c(lines, paste("Not checked:", paste(unchecked, collapse = ", ")))
    state[i] <- if ("failed" %in% states) "failed" else if (length(conf)) "conflict" else
      if ("ok" %in% states) "ok" else "none"
    icons[i] <- paste(c(ic, if (length(conf)) "conflict"), collapse = " ")
    msg[i] <- if (length(lines)) paste(lines, collapse = "\n") else "No GEOME BCID or GBIF ID"
  }
  data.frame(ID = s$ID, specimen = state, specimen_message = msg, specimen_icons = icons)
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
      var row = cellInfo.row || {};
      var esc = function(s) { return String(s).replace(/&/g, '&amp;').replace(/'/g, '&#39;')
        .replace(/\"/g, '&quot;').replace(/</g, '&lt;').replace(/>/g, '&gt;'); };
      var codes = String(row['specimen_icons'] || '').split(' ').filter(Boolean);
      var logos = {GEOME: 'www/specimen/geome_g.png', GBIF: 'www/specimen/gbif_leaf.png'};
      var html = '';
      codes.forEach(function(c) {
        if (c === 'conflict') {
          html += `<i class='fa-solid fa-flag mp-fg-warning mp-spec-icon' aria-hidden='true'></i>`;
          return;
        }
        var p = c.split(':'), src = p[0], st = p[1];
        if (!logos[src]) return;
        html += `<span class='mp-spec-logo${st === 'ok' ? '' : ' mp-spec-faded'}'>` +
          `<img src='${logos[src]}' alt='' class='mp-spec-icon'>` +
          (st === 'failed' ? `<i class='fa-solid fa-triangle-exclamation mp-fg-warning mp-spec-badge' aria-hidden='true'></i>` : '') +
          `</span>`;
      });
      var none = html === '';
      if (none) html = `<i class='fa-regular fa-square-plus text-muted' aria-hidden='true'></i>`;
      var tip = (row['specimen_message'] || 'No GEOME BCID or GBIF ID') +
        (none ? '. Click to add one.' : '\\nClick to view.');
      return `<a href='#' class='mp-specimen-cell' data-id='${esc(row['ID'])}' title='${esc(tip)}' aria-label='${esc(tip)}' ` +
        `onclick=\"event.preventDefault(); event.stopPropagation(); Shiny.setInputValue('%s', this.dataset.id, {priority: 'event'})\">` +
        html + `</a>`;
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
    show = TRUE, name = "Specimen", sticky = sticky, width = 90, align = "center",
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

#' Render one sample's records from one source as level cards
#'
#' GEOME levels run root first; GBIF runs Occurrence, Dataset, Organization.
#'
#' @param recs `meta_records` rows for one sample and source (level, depth, ref, field, value)
#' @param source "GEOME" or "GBIF"
#' @param box_id DOM id of the scroll box holding the cards
#' @noRd
meta_record_view <- function(recs, source, box_id) {
  if (!nrow(recs)) return(p(class = "text-muted", paste("No", source, "data stored for this sample yet.")))
  toggle <- function(label, open) {
    tags$button(
      type = "button", class = "btn btn-default btn-sm", label,
      onclick = sprintf("document.querySelectorAll('#%s details').forEach(function(d) { d.open = %s; });",
                        box_id, tolower(open))
    )
  }
  link <- function(level, ref) {
    if (is.na(ref)) return(NULL)
    if (source == "GEOME") return(paste0("https://geome-db.org/record/", ref))
    switch(level,
      Occurrence = paste0("https://www.gbif.org/occurrence/", ref),
      Dataset = paste0("https://www.gbif.org/dataset/", ref),
      Organization = paste0("https://www.gbif.org/publisher/", ref),
      NULL)
  }
  cell <- function(field, value) {
    if (field != "issues") return(value)
    lapply(strsplit(value, ",", fixed = TRUE)[[1]], function(x) {
      tagList(span(class = "mp-pill mp-pill-warning", x), " ")
    })
  }
  ord <- if (source == "GEOME") -recs$depth else recs$depth
  lv <- unique(recs[order(ord), c("level", "depth", "ref")])
  lv <- lv[!duplicated(lv$depth), ]
  cards <- lapply(seq_len(nrow(lv)), function(i) {
    r <- recs[recs$depth == lv$depth[i], ]
    url <- link(lv$level[i], lv$ref[i])
    cit <- r$value[r$field == "citation"]
    tags$details(
      open = NA, class = "mp-meta-level",
      tags$summary(
        strong(lv$level[i]),
        if (!is.null(url)) tagList(" ", tags$a(href = url, target = "_blank", rel = "noopener", lv$ref[i]))
      ),
      if (length(cit)) p(class = "mp-meta-citation", em(cit[1])),
      tags$table(class = "table table-sm",
        tags$tbody(lapply(seq_len(nrow(r)), function(j) {
          tags$tr(tags$th(r$field[j]), tags$td(cell(r$field[j], r$value[j])))
        }))
      )
    )
  })
  tagList(
    div(style = "margin-bottom: 6px;", toggle("Expand all", TRUE), " ", toggle("Collapse all", FALSE)),
    div(id = box_id, style = "max-height: 50vh; overflow-y: auto;", cards)
  )
}

#' Compare tab table: one row per concept across CSV, GEOME, and GBIF
#'
#' @param cf `specimen_conflicts()` rows for one sample
#' @noRd
specimen_compare_view <- function(cf) {
  if (!nrow(cf)) return(p(class = "text-muted", "No sample selected."))
  dash <- function(x) if (is.na(x)) "-" else x
  tags$table(
    class = "table table-sm mp-spec-compare",
    tags$thead(tags$tr(tags$th("Item"), tags$th("CSV (column)"), tags$th("GEOME"),
                       tags$th("GBIF"), tags$th("Status"))),
    tags$tbody(lapply(seq_len(nrow(cf)), function(i) {
      r <- cf[i, ]
      cls <- if (identical(r$status, "conflict")) "mp-spec-conflict" else
        if (identical(r$status, "not checked")) "text-muted" else NULL
      tags$tr(
        class = cls,
        tags$td(r$concept),
        tags$td(dash(r$csv_value),
                if (!is.na(r$csv_column)) span(class = "text-muted", paste0(" (", r$csv_column, ")"))),
        tags$td(dash(r$geome_value)),
        tags$td(dash(r$gbif_value)),
        tags$td(dash(r$status))
      )
    }))
  )
}

#' "CSV columns..." control: pick the mapping-file column per concept
#'
#' @param ns module namespace function
#' @param current `specimen_csv_columns()` result
#' @param overrides named character vector from `meta_csv_map` (concept -> column(s))
#' @param choices mapping-file column names
#' @noRd
specimen_csv_map_ui <- function(ns, current, overrides, choices) {
  concepts <- setdiff(SPECIMEN_CONCEPTS, "taxon")
  tags$details(
    class = "mp-spec-map",
    tags$summary("CSV columns..."),
    opts_help(
      "Pick the mapping-file column MitoPilot compares for each item. Leave a ",
      "box empty to detect the column automatically, or pick (none) to skip ",
      "that item. Coordinates take one combined column, or latitude then longitude.",
      nested = TRUE
    ),
    lapply(concepts, function(k) {
      set <- k %in% names(overrides)
      sel <- if (!set) character() else if (nzchar(overrides[[k]])) {
        strsplit(overrides[[k]], ",", fixed = TRUE)[[1]]
      } else {
        "__none__"
      }
      auto <- if (length(current[[k]])) paste(current[[k]], collapse = " + ") else "none"
      selectizeInput(
        ns(paste0("map_", k)),
        label = if (set) k else paste0(k, " (auto: ", auto, ")"),
        choices = c("(none)" = "__none__", choices), selected = sel,
        multiple = TRUE, width = "100%",
        options = list(maxItems = if (k == "coordinates") 2 else 1,
                       placeholder = "detect automatically")
      )
    }),
    actionButton(ns("map_save"), "Save columns")
  )
}

#' Modal listing GEOME and GBIF fields available at export
#'
#' @param ns module namespace function
#' @param geome,gbif `meta_field_summary()` output for each source
#' @noRd
specimen_fields_modal <- function(ns, geome, gbif) {
  section <- function(source, s) {
    key <- tolower(source)
    combos <- s[s$kind == "combo", ]
    tagList(
      h4(source),
      h5("GenBank-ready combinations"),
      checkboxGroupInput(
        ns(paste0(key, "_combos")), NULL, width = "100%",
        choiceValues = combos$key, selected = combos$key[combos$selected],
        choiceNames = lapply(seq_len(nrow(combos)), function(i) tagList(
          code(paste0("{", combos$col[i], "}")), " from ", combos$field[i], ": ",
          if (is.na(combos$example[i])) em("no samples") else
            tagList(tags$samp(combos$example[i]), sprintf(" (%d samples)", combos$n_samples[i]))
        ))
      ),
      h5(paste("All", source, "fields")),
      reactable::reactableOutput(ns(paste0(key, "_raw")))
    )
  }
  modalDialog(
    title = mp_modal_title("Specimen fields for export",
                           "Ticked fields become columns you can use in header templates"),
    size = "l", easyClose = TRUE,
    section("GEOME", geome),
    tags$hr(),
    section("GBIF", gbif),
    footer = mp_footer(primary = actionButton(ns("specimen_fields_save"), "Save"), dismiss = "Cancel")
  )
}

#' Specimen metadata viewer: GEOME, GBIF, and Compare tabs
#'
#' @param id module id
#' @param open reactive yielding the sample ID to open (from a `specimen_open` input)
#' @param on_change function called after any DB write, so the caller can refresh its table
#' @noRd
specimen_viewer_server <- function(id, open, on_change = function() NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    con <- session$userData$con
    rv <- reactiveValues(id = NULL, ver = 0L)
    bump <- function() { rv$ver <- rv$ver + 1L; on_change() }
    empty_msg <- list(
      GEOME = paste("This sample has no GEOME BCID. Paste one above and click Fetch, or add a",
                    "GEOME_BCID column to your mapping file (see the Specimen Metadata article)."),
      GBIF = paste("This sample has no GBIF ID. Paste a gbifID or a gbif.org/occurrence link above",
                   "and click Fetch, or add a GBIF_ID column to your mapping file.")
    )

    samples <- function() {
      DBI::dbGetQuery(con, "SELECT ID, Taxon, GEOME_BCID, GBIF_ID FROM samples ORDER BY ID")
    }

    observeEvent(open(), {
      rv$id <- open()
      s <- samples()
      st <- specimen_status(con)
      mark <- st$specimen[match(s$ID, st$ID)]
      lab <- paste0(s$ID, ifelse(mark %in% "failed", " (failed)",
                                 ifelse(mark %in% "conflict", " (conflict)", "")))
      modalDialog(
        title = mp_modal_title(
          tagList("Specimen metadata: ", textOutput(ns("hdr_id"), inline = TRUE)),
          subtitle = tagList("Taxon: ", textOutput(ns("hdr_taxon"), inline = TRUE))
        ),
        size = "l", easyClose = TRUE,
        fluidRow(
          column(3,
            selectInput(ns("sample"), "Sample", choices = stats::setNames(s$ID, lab),
                        selected = rv$id, width = "100%", selectize = FALSE, size = 15),
            uiOutput(ns("failed"))
          ),
          column(9,
            tabsetPanel(
              id = ns("tab"),
              tabPanel("GEOME", uiOutput(ns("geome_detail"))),
              tabPanel("GBIF", uiOutput(ns("gbif_detail"))),
              tabPanel("Compare", uiOutput(ns("compare")))
            )
          )
        ),
        footer = mp_footer(
          extra = actionButton(ns("refresh_all"), "Refresh all",
                               title = "Fetch every sample's GEOME and GBIF records again"),
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
      st <- specimen_status(con)
      bad <- st$ID[st$specimen == "failed"]
      if (!length(bad)) return(NULL)
      div(class = "mp-fg-warning", icon("triangle-exclamation"), " Failed: ",
          paste(bad, collapse = ", "))
    })

    source_detail <- function(source) {
      rv$ver
      req(rv$id)
      src <- META_SOURCES[[source]]
      key <- tolower(source)
      ref <- DBI::dbGetQuery(con, paste0("SELECT ", src$col, " AS v FROM samples WHERE ID = ?"),
                             params = list(rv$id))$v
      ref <- if (length(ref)) ref[1] else NA_character_
      st <- DBI::dbGetQuery(con, "SELECT status, message, fetched_at FROM meta_status
                                  WHERE ID = ? AND source = ?", params = list(rv$id, source))
      recs <- DBI::dbGetQuery(con, "SELECT level, depth, ref, field, value FROM meta_records
                                    WHERE ID = ? AND source = ?", params = list(rv$id, source))
      tagList(
        div(class = "mp-meta-ref",
          textInput(ns(paste0(key, "_ref")), paste(source, src$id_label), value = ref %|NA|% "",
                    placeholder = if (source == "GEOME") "ark:/21547/..." else "6186461308",
                    width = "420px"),
          actionButton(ns(paste0(key, "_fetch")), "Fetch", icon = icon("arrows-rotate"))
        ),
        if (nrow(st)) p(class = if (st$status == "failed") "mp-fg-warning" else "text-muted",
          if (st$status == "failed") paste("Last fetch failed:", st$message) else "Fetched",
          " ", format(as.POSIXct(st$fetched_at, origin = "1970-01-01"), "%Y-%m-%d %H:%M")),
        if (is.na(ref) && !nrow(recs)) p(class = "text-muted", empty_msg[[source]]),
        meta_record_view(recs, source, box_id = ns(paste0(key, "_records")))
      )
    }
    output$geome_detail <- renderUI(source_detail("GEOME"))
    output$gbif_detail <- renderUI(source_detail("GBIF"))

    output$compare <- renderUI({
      rv$ver
      req(rv$id)
      m <- DBI::dbGetQuery(con, "SELECT concept, column FROM meta_csv_map")
      tagList(
        specimen_compare_view(specimen_conflicts(con, rv$id)),
        specimen_csv_map_ui(ns, specimen_csv_columns(con), stats::setNames(m$column, m$concept),
                            export_metadata_cols(DBI::dbListFields(con, "samples"), character()))
      )
    })

    fetch_one <- function(source) {
      req(rv$id)
      tryCatch({
        val <- .meta_set_ref(con, source, rv$id, input[[paste0(tolower(source), "_ref")]])
        if (!is.na(val)) {
          withProgress(message = paste("Fetching from", source), {
            res <- suppressWarnings(.meta_fetch_into(con, source, rv$id, val))
          })
          if (res$status == "failed") showNotification(res$message, type = "warning")
        }
        bump()
      }, error = function(e) showNotification(conditionMessage(e), type = "error"))
    }
    observeEvent(input$geome_fetch, fetch_one("GEOME"))
    observeEvent(input$gbif_fetch, fetch_one("GBIF"))

    observeEvent(input$map_save, {
      concepts <- setdiff(SPECIMEN_CONCEPTS, "taxon")
      map <- lapply(stats::setNames(nm = concepts), function(k) {
        v <- input[[paste0("map_", k)]]
        if (!length(v)) NA_character_ else if ("__none__" %in% v) "" else v
      })
      tryCatch({
        .spec_set_csv_map(con, map)
        bump()
        showNotification("CSV columns saved", type = "message")
      }, error = function(e) showNotification(conditionMessage(e), type = "error"))
    })

    observeEvent(input$refresh_all, {
      s <- samples()
      jobs <- do.call(rbind, lapply(names(META_SOURCES), function(src) {
        ref <- s[[META_SOURCES[[src]]$col]]
        keep <- !is.na(ref) & nzchar(ref)
        data.frame(ID = s$ID[keep], source = rep(src, sum(keep)), ref = ref[keep])
      }))
      if (!nrow(jobs)) {
        return(showNotification("No samples have a GEOME BCID or GBIF ID", type = "message"))
      }
      caches <- lapply(META_SOURCES, function(x) new.env())
      tryCatch({
        withProgress(message = "Fetching specimen records", value = 0, {
          for (i in seq_len(nrow(jobs))) {
            suppressWarnings(.meta_fetch_into(con, jobs$source[i], jobs$ID[i], jobs$ref[i],
                                              caches[[jobs$source[i]]]))
            incProgress(1 / nrow(jobs), detail = paste(jobs$ID[i], jobs$source[i]))
          }
        })
        bump()
      }, error = function(e) showNotification(conditionMessage(e), type = "error"))
    })
  })
}
