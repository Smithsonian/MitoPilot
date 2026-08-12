# Togglable column groups for the Assemble table. Cols not listed here
# (sticky cols, action buttons) are always shown.
ASSEMBLE_COL_GROUPS <- list(
  Options  = c("pre_opts", "assemble_opts", "blast_opts"),
  Stats    = c("trimmed_reads", "mean_length", "topology", "length",
               "paths", "scaffolds"),
  BLAST    = c("blast_accession", "blast_ref_status", "blast_species",
               "blast_lineage", "blast_pident", "blast_qcovs"),
  Metadata = c("time_stamp", "assemble_notes")
)
# The reverse lookup col -> group (see grp_class) tags colDefs with a CSS class
# so the column-group picker can show/hide columns via CSS without re-rendering
# the table (preserves filters, sort, page, selection).

# Status-filter pickers. Values match the assemble_lock / assemble_switch
# codes; each row is tagged with mp-lock-<v> / mp-state-<v> classes so
# unselected codes can be hidden via CSS (same mechanism as the column
# picker, so sort order, search, other filters, page, and selection survive).
ASSEMBLE_LOCK_CHOICES <- c("Unlocked" = "0", "Locked" = "1")
ASSEMBLE_STATE_CHOICES <- c(
  "Pre-Assembly" = "0",
  "Ready"        = "1",
  "In Progress"  = "4",
  "Success"      = "2",
  "Failed"       = "3"
)

#' Assemble table column definitions.
#'
#' Every colDef the two flavours disagree on lives in the `flavour` list at the
#' bottom; `shared` is identical for both. Column order here does not matter,
#' reactable orders the table by the data frame's own column order.
#'
#' @noRd
assemble_cols <- function(ns, userAsmb = FALSE, no_raw = FALSE) {
  .grp <- function(col) grp_class(col, ASSEMBLE_COL_GROUPS)

  shared <- list(
    `.selection` = colDef(show = T, sticky = "left", width = 28),
    assemble_lock = colDef(
      show = TRUE,
      sticky = "left",
      name = "",
      html = TRUE,
      width = 32,
      align = "center",
      filterable = FALSE,
      cell = rt_dynamicIcon(
        c(
          `0` = "fa fa-lock-open",
          `1` = "fa fa-lock"
        )
      )
    ),
    assemble_switch = colDef(
      show = TRUE,
      sticky = "left",
      name = "",
      html = TRUE,
      width = 30,
      align = "center",
      filterable = FALSE,
      cell = rt_dynamicIcon(
        c(
          `0` = "fa fa-hourglass",
          `1` = "fa fa-person-running",
          `2` = "fa fa-circle-check",
          `3` = "fa fa-triangle-exclamation",
          `4` = "fa fa-circle-half-stroke"
        )
      )
    ),
    ID = colDef(
      show = T,
      minWidth = 120,
      sticky = "left",
      html = T,
      cell = rt_longtext()
    ),
    Taxon = colDef(
      show = T,
      minWidth = 140,
      sticky = "left",
      html = T,
      cell = rt_longtext()
    ),
    # No-raw-data projects have no reads/coverage, so hide the read-derived
    # columns (preprocess opts, read count, read length) entirely.
    pre_opts = colDef(
      show = !no_raw, class = .grp("pre_opts"), headerClass = .grp("pre_opts"),
      name = "Preprocess Opts.",
      html = T,
      width = 130,
      cell = rt_link(ns("set_pre_opts"))
    ),
    trimmed_reads = colDef(
      show = !no_raw, class = .grp("trimmed_reads"), headerClass = .grp("trimmed_reads"),
      name = "Reads",
      filterable = FALSE,
      minWidth = 100
    ),
    mean_length = colDef(
      show = !no_raw, class = .grp("mean_length"), headerClass = .grp("mean_length"),
      name = "Read Length",
      filterable = FALSE,
      minWidth = 100
    ),
    blast_opts = colDef(
      show = TRUE, class = .grp("blast_opts"), headerClass = .grp("blast_opts"),
      name = "BLAST Opts.",
      html = T,
      width = 120,
      cell = rt_link(ns("set_blast_opts"))
    ),
    topology = colDef(
      show = TRUE, class = .grp("topology"), headerClass = .grp("topology"),
      width = 100,
      name = "Topology"
    ),
    scaffolds = colDef(
      show = TRUE, class = .grp("scaffolds"), headerClass = .grp("scaffolds"), width = 100, name = "# Scaffolds", align = "center"
    ),
    poor_blast_ref = colDef(show = FALSE),
    blast_hits = colDef(
      show = TRUE,
      name = "",
      filterable = FALSE,
      sortable = FALSE,
      html = TRUE,
      width = 140,
      align = "center",
      cell = rt_icon_bttn_text(ns("all_blast_hits"), "fas fa-list", "All BLAST Hits")
    ),
    time_stamp = colDef(
      show = TRUE, class = .grp("time_stamp"), headerClass = .grp("time_stamp"),
      name = "Last Updated",
      filterable = FALSE,
      html = T,
      width = 150,
      cell = rt_ts_date()
    ),
    assemble_notes = colDef(
      show = TRUE, class = .grp("assemble_notes"), headerClass = .grp("assemble_notes"),
      name = "Notes",
      html = TRUE,
      align = "left",
      minWidth = 150,
      cell = rt_longtext()
    ),
    view = colDef(
      show = TRUE,
      sticky = "right",
      filterable = FALSE,
      name = "",
      html = TRUE,
      width = 80,
      align = "center",
      cell = rt_icon_bttn_text(ns("details"), "fas fa-square-arrow-up-right fa-xs")
    ),
    output = colDef(
      show = TRUE,
      sticky = "right",
      filterable = FALSE,
      name = "",
      html = TRUE,
      width = 80,
      align = "center",
      cell = rt_icon_bttn_text(ns("output"), "fas fa-folder-open fa-xs")
    )
  )

  # Flavour-specific columns. `assembly` exists only in the user-assembly data
  # and `min_assembly_length` / `ignore_flags` only in the standard data, so a
  # colDef on the wrong side is a hard reactable error. `assemble_opts` is in
  # both, and is deliberately left undefined for userAsmb so the table shows no
  # "Assembly Opts." link. The JS cell body below keeps its original
  # indentation because that whitespace is part of the rendered payload.
  flavour <- if (userAsmb) {
    list(
      assembly = colDef(
        show = TRUE,
        minWidth = 140,
        name = "Input Assembly File",
        html = T,
        cell = rt_longtext()
      ),
      length = colDef(
        show = TRUE, class = .grp("length"), headerClass = .grp("length"),
        minWidth = 140,
        name = "Asmb. Length (raw)",
        filterable = FALSE,
        html = TRUE,
        cell = rt_longtext()
      ),
      paths = colDef(
        show = TRUE, class = .grp("paths"), headerClass = .grp("paths"),
        width = 100, name = "# Paths", align = "center",
        cell = JS("function(cellInfo){if(cellInfo.value<0){return -cellInfo.value };return cellInfo.value}"),
        style = JS("function(rowInfo){ if (rowInfo.values.paths < 0) return { backgroundColor: '#00000020' }}")
      ),
      blast_accession = colDef(
        show = TRUE, class = .grp("blast_accession"), headerClass = .grp("blast_accession"),
        name = "Top Hit",
        html = TRUE,
        width = 120,
        cell = rt_ncbi_link()
      ),
      blast_ref_status = colDef(
        show = TRUE, class = .grp("blast_ref_status"), headerClass = .grp("blast_ref_status"),
        name = "Ref Align",
        html = TRUE,
        width = 100,
        align = "center",
        filterable = TRUE,
        cell = rt_blast_ref_status()
      ),
      blast_species = colDef(
        show = TRUE, class = .grp("blast_species"), headerClass = .grp("blast_species"),
        name = "Species",
        html = TRUE,
        minWidth = 160,
        cell = rt_longtext()
      ),
      blast_lineage = colDef(
        show = TRUE, class = .grp("blast_lineage"), headerClass = .grp("blast_lineage"),
        name = "Lineage",
        html = TRUE,
        minWidth = 200,
        cell = rt_longtext()
      ),
      blast_pident = colDef(
        show = TRUE, class = .grp("blast_pident"), headerClass = .grp("blast_pident"),
        name = "% Ident",
        filterable = FALSE,
        width = 90,
        align = "center"
      ),
      blast_qcovs = colDef(
        show = TRUE, class = .grp("blast_qcovs"), headerClass = .grp("blast_qcovs"),
        name = "% Cov",
        filterable = FALSE,
        width = 90,
        align = "center"
      )
    )
  } else {
    list(
      assemble_opts = colDef(
        show = TRUE, class = .grp("assemble_opts"), headerClass = .grp("assemble_opts"),
        name = "Assembly Opts.",
        html = T,
        width = 130,
        cell = rt_link(ns("set_assemble_opts"))
      ),
      length = colDef(
        show = TRUE, class = .grp("length"), headerClass = .grp("length"),
        minWidth = 140,
        name = "Asmb. Length (ignored)",
        header = htmltools::HTML('Asmb. Length (<span style="color:#e74c3c;font-weight:bold">ignored</span>)'),
        filterable = FALSE,
        html = TRUE,
        cell = JS("function(cellInfo) {
                var val = cellInfo.value;
                if (!val) return val;
                var flagsStr = cellInfo.row['ignore_flags'];
                var flags = flagsStr ? String(flagsStr).split(';') : [];
                var parts = String(val).split(';');
                var colored = parts.map(function(p, i) {
                  var ign = flags[i];
                  if (ign === '1') {
                    return '<span style=\"color:#e74c3c;font-weight:bold\">' + p.trim() + '</span>';
                  }
                  return p.trim();
                });
                return colored.join('; ');
              }")
      ),
      min_assembly_length = colDef(show = FALSE),
      ignore_flags = colDef(show = FALSE),
      paths = colDef(
        show = TRUE, width = 100, name = "# Paths", align = "center",
        cell = JS("function(cellInfo){if(cellInfo.value<0){return -cellInfo.value };return cellInfo.value}"),
        style = JS("function(rowInfo){ if (rowInfo.values.paths < 0) return { backgroundColor: '#00000020' }}")
      ),
      blast_accession = colDef(
        show = TRUE, class = .grp("blast_accession"), headerClass = .grp("blast_accession"),
        name = "BLAST Top Hit",
        html = TRUE,
        width = 120,
        cell = rt_ncbi_link()
      ),
      blast_ref_status = colDef(
        show = TRUE, class = .grp("blast_ref_status"), headerClass = .grp("blast_ref_status"),
        name = "BLAST Ref Align",
        html = TRUE,
        minWidth = 130,
        resizable = TRUE,
        align = "center",
        filterable = TRUE,
        cell = rt_blast_ref_status()
      ),
      blast_species = colDef(
        show = TRUE, class = .grp("blast_species"), headerClass = .grp("blast_species"),
        name = "BLAST Species",
        html = TRUE,
        minWidth = 160,
        cell = rt_longtext()
      ),
      blast_lineage = colDef(
        show = TRUE, class = .grp("blast_lineage"), headerClass = .grp("blast_lineage"),
        name = "BLAST Lineage",
        html = TRUE,
        minWidth = 200,
        cell = rt_longtext()
      ),
      blast_pident = colDef(
        show = TRUE, class = .grp("blast_pident"), headerClass = .grp("blast_pident"),
        name = "BLAST % Ident",
        filterable = FALSE,
        minWidth = 90,
        align = "center"
      ),
      blast_qcovs = colDef(
        show = TRUE, class = .grp("blast_qcovs"), headerClass = .grp("blast_qcovs"),
        name = "BLAST % Cov",
        filterable = FALSE,
        minWidth = 90,
        align = "center"
      )
    )
  }

  c(shared, flavour)
}

#' assemble UI
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param userAsmb logical. TRUE for a user-assembly project.
#'
#' @noRd
assemble_ui <- function(id, userAsmb = FALSE) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("col_css")),
    div(
      style = "display: flex; flex-flow: row wrap; gap: 1em; align-items: flex-end;",
      shinyWidgets::pickerInput(
        inputId  = ns("lock_filter"),
        width    = "140px",
        label    = "Lock:",
        choices  = ASSEMBLE_LOCK_CHOICES,
        selected = ASSEMBLE_LOCK_CHOICES,
        multiple = TRUE,
        options  = list(
          `actions-box`          = TRUE,
          `select-all-text`      = "All",
          `deselect-all-text`    = "None",
          `selected-text-format` = "count > 0",
          width                  = "140px"
        )
      ),
      shinyWidgets::pickerInput(
        inputId  = ns("state_filter"),
        width    = "140px",
        label    = "State:",
        choices  = ASSEMBLE_STATE_CHOICES,
        selected = ASSEMBLE_STATE_CHOICES,
        multiple = TRUE,
        options  = list(
          `actions-box`          = TRUE,
          `select-all-text`      = "All",
          `deselect-all-text`    = "None",
          `selected-text-format` = "count > 0",
          width                  = "140px"
        )
      ),
      shinyWidgets::pickerInput(
        inputId  = ns("col_groups"),
        width    = "150px",
        label    = "Show columns:",
        choices  = names(ASSEMBLE_COL_GROUPS),
        selected = names(ASSEMBLE_COL_GROUPS),
        multiple = TRUE,
        options  = list(
          `actions-box`          = TRUE,
          `select-all-text`      = "All",
          `deselect-all-text`    = "None",
          `selected-text-format` = "count > 0",
          width                  = "150px"
        )
      ),
      shinyWidgets::airDatepickerInput(
        inputId     = ns("date_filter"),
        label       = "Updated between:",
        range       = TRUE,
        clearButton = TRUE,
        value       = NULL,
        width       = "220px",
        placeholder = "any time"
      )
    ),
    div(class = "mp-table-resize", reactableOutput(ns("table"))),
    div(
      style = "font-size: 0.85em; color: #555; margin-top: 4px;",
      textOutput(ns("n_selected"), inline = TRUE)
    ),
    # CSV export is standard-only.
    if (!userAsmb) {
      div(
        style = "margin-top: 12px; display: flex; gap: 8px;",
        downloadButton(ns("export_selected"), "Export Selected to CSV",
                       class = "btn-sm btn-default"),
        downloadButton(ns("export_all"), "Export All to CSV",
                       class = "btn-sm btn-default")
      )
    }
  )
}

#' assemble Server
#'
#' @param userAsmb logical. TRUE for a user-assembly project.
#'
#' @noRd
assemble_server <- function(id, userAsmb = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Help-doc icons (one observer per tool, registered once at module init).
    register_tool_help("fastp", input, reopen = function() pre_opts_modal(rv))
    register_tool_help("getOrganelle", input, reopen = function() assemble_opts_modal(rv))
    register_tool_help("mitofinder", input, reopen = function() assemble_opts_modal(rv))
    register_tool_help("blastn", input, reopen = function() blast_opts_modal(rv))

    # Prepare data ----
    rv <- reactiveValues(
      pre_opts = dplyr::tbl(session$userData$con, "pre_opts") |>
        dplyr::collect(),
      assemble_opts = dplyr::tbl(session$userData$con, "assemble_opts") |>
        dplyr::collect(),
      blast_opts = dplyr::tbl(session$userData$con, "blast_opts") |>
        dplyr::collect(),
      data = fetch_assemble_data(userAsmb = userAsmb),
      updating = NULL
    )

    # Date-range filter on "Last Updated" (time_stamp is epoch seconds). Empty
    # picker = no filter; end day is inclusive. Unlike the lock/state CSS filters
    # this subsets the rows, so selection may reset when the range changes.
    filtered_data <- reactive({
      req(rv$data)
      out <- rv$data
      dr <- input$date_filter
      if (!is.null(dr) && length(dr) == 2 && all(!is.na(dr))) {
        lo <- as.numeric(as.POSIXct(as.Date(dr[1])))
        hi <- as.numeric(as.POSIXct(as.Date(dr[2]) + 1))
        out <- out |>
          dplyr::filter(!is.na(time_stamp) & time_stamp >= lo & time_stamp < hi)
      }
      out
    })

    # Refresh table when the date-range filter changes (ignoreNULL = FALSE so
    # clearing the range restores all rows).
    observeEvent(input$date_filter, {
      req(rv$data)
      trigger("update_assemble_table")
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    # Refresh ----
    init("refresh_assemble")
    on("refresh_assemble", {
      rv$data <- fetch_assemble_data(userAsmb = userAsmb)
      updateReactable(
        "table",
        data = filtered_data()
      )
    })

    # Mirror the column-group picker so NULL (= user cleared all) is
    # distinguishable from the pre-init state. Default: all groups on.
    col_groups_rv <- reactiveVal(names(ASSEMBLE_COL_GROUPS))
    observeEvent(input$col_groups, {
      col_groups_rv(input$col_groups %||% character(0))
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    # Mirror the lock / state status pickers the same way, so clearing all
    # is distinguishable from the pre-init state. Default: all codes shown.
    lock_filter_rv <- reactiveVal(unname(ASSEMBLE_LOCK_CHOICES))
    observeEvent(input$lock_filter, {
      lock_filter_rv(input$lock_filter %||% character(0))
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
    state_filter_rv <- reactiveVal(unname(ASSEMBLE_STATE_CHOICES))
    observeEvent(input$state_filter, {
      state_filter_rv(input$state_filter %||% character(0))
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    no_raw <- isTRUE(session$userData$no_raw_data)

    # Inject a <style> tag that display:nones unselected column groups.
    # Hiding via CSS keeps the columns mounted in the DOM, so filters,
    # sort order, current page, and selection all survive toggling.
    # Also hide rows whose lock / state code is unselected. Rows are tagged
    # with mp-lock-<v> / mp-state-<v> classes (see rowClass below); hiding via
    # CSS keeps every row mounted, so sort, search, other column filters,
    # page, and selection all survive toggling.
    output$col_css <- renderUI({
      hidden_grp   <- setdiff(names(ASSEMBLE_COL_GROUPS), col_groups_rv())
      hidden_lock  <- setdiff(unname(ASSEMBLE_LOCK_CHOICES), lock_filter_rv())
      hidden_state <- setdiff(unname(ASSEMBLE_STATE_CHOICES), state_filter_rv())
      # Scope to THIS module's table so rules don't hit the shared mp-lock /
      # mp-state / mp-grp classes on the annotate and export tables.
      sel <- paste0("#", ns("table"), " ")
      rules <- c(
        if (length(hidden_grp))   paste0(sel, ".mp-grp-",   hidden_grp,   " { display: none !important; }"),
        if (length(hidden_lock))  paste0(sel, ".mp-lock-",  hidden_lock,  " { display: none !important; }"),
        if (length(hidden_state)) paste0(sel, ".mp-state-", hidden_state, " { display: none !important; }")
      )
      if (length(rules) == 0) return(NULL)
      tags$style(HTML(paste(rules, collapse = "\n")))
    })

    # Render table ----
    output$table <- renderReactable({
      isolate(req(filtered_data())) |>
        reactable(
          resizable = TRUE,
          filterable = TRUE,
          striped = TRUE,
          compact = TRUE,
          defaultPageSize = 100,
          showPageSizeOptions = TRUE,
          onClick = "select",
          selection = "multiple",
          searchable = TRUE,
          defaultSorted = list(time_stamp = "desc"),
          height = "100%",
          wrap = FALSE,
          pageSizeOptions = c(25, 50, 100, 200, 500),
          rowStyle = rt_highlight_row(),
          rowClass = JS("function(rowInfo) {
            if (!rowInfo || !rowInfo.values) return '';
            return 'mp-lock-' + rowInfo.values['assemble_lock'] +
                   ' mp-state-' + rowInfo.values['assemble_switch'];
          }"),
          # Header wrapping, user-assembly table only.
          theme = if (userAsmb) {
            reactable::reactableTheme(
              headerStyle = list(whiteSpace = "normal", lineHeight = "1.2", textAlign = "left")
            )
          } else {
            getOption("reactable.theme")
          },
          defaultColDef = colDef(align = "left", show = F),
          columns = assemble_cols(ns, userAsmb, no_raw)
        )
    })

    # update table ----
    init("update_assemble_table")
    on("update_assemble_table", {
      reactable::updateReactable(
        "table",
        data = filtered_data() |>
          dplyr::mutate(
            output = dplyr::case_when(
              assemble_switch > 1 ~ "output",
              .default = NA_character_
            ),
            view = dplyr::case_when(
              assemble_switch > 1 ~ "details",
              .default = NA_character_
            ),
            blast_hits = dplyr::case_when(
              assemble_switch > 1 ~ "All BLAST Hits",
              .default = NA_character_
            )
          ),
        selected = reactable::getReactableState("table", "selected"),
        page = reactable::getReactableState("table", "page")
      )
    })

    # table selection ----
    # Rows hidden by the lock/state filters stay mounted in reactable's row
    # model, so a shift-click range can select them. Drop currently-hidden rows
    # so bulk ops only ever touch visible samples.
    selected <- reactive({
      sel <- reactable::getReactableState("table", "selected")
      if (is.null(sel) || length(sel) == 0) return(sel)
      visible <- as.character(rv$data$assemble_lock)   %in% lock_filter_rv() &
                 as.character(rv$data$assemble_switch) %in% state_filter_rv()
      intersect(sel, which(visible))
    })

    output$n_selected <- renderText({
      paste0(length(selected()), " selected")
    })

    # Publish current selection so the work-dir browser can pre-select this sample
    observe({
      session$userData$wd_selected[["Assemble"]] <- unique(rv$data$ID[selected()])
    })

    # Prune hidden rows from reactable's actual selection whenever the
    # selection OR the filters change. Triggering on the selection itself is
    # what catches a shift-click range: hidden rows are removed immediately, so
    # they never persist in reactable's state to reappear when later revealed.
    observeEvent(
      list(reactable::getReactableState("table", "selected"),
           lock_filter_rv(), state_filter_rv()), {
      sel <- reactable::getReactableState("table", "selected")
      if (is.null(sel) || length(sel) == 0) return()
      visible <- as.character(rv$data$assemble_lock)   %in% lock_filter_rv() &
                 as.character(rv$data$assemble_switch) %in% state_filter_rv()
      keep <- intersect(sel, which(visible))
      if (length(keep) != length(sel)) {
        reactable::updateReactable("table", selected = keep)
      }
    }, ignoreInit = TRUE)

    # Set State ----
    # Same codes and order in both flavours; only the labels differ. The
    # user-assembly step computes coverage, it does not assemble.
    state_choices <- if (userAsmb) {
      c("Pre-Coverage (wait)" = 0,
        "Ready to Calculate Coverage" = 1,
        "In Progress" = 4,
        "Successful Coverage Calculation" = 2,
        "Failed Coverage Calculation" = 3)
    } else {
      c("Pre-Assembly (wait)" = 0, "Ready to Assemble" = 1, "In Progress" = 4, "Successful Assembly" = 2, "Failed / Problematic" = 3)
    }
    init("state")
    on("state", {
      req(session$userData$mode == "Assemble")
      req(selected())
      req(all(rv$data$assemble_lock[req(selected())] == 0))
      rv$updating <- rv$data |>
        dplyr::select(ID, assemble_switch) |>
        dplyr::slice(selected())
      current <- character(0)
      if (length(unique(rv$updating$assemble_switch)) == 1) {
        current <- rv$updating$assemble_switch[1]
      }
      showModal(
        modalDialog(
          title = "Select New State:",
          shinyWidgets::prettyRadioButtons(
            ns("new_state"),
            label = NULL,
            choices = state_choices,
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
      rv$updating$assemble_switch <- as.numeric(input$new_state)
      dplyr::tbl(session$userData$con, "assemble") |>
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
      trigger("update_assemble_table")
      removeModal()
    })

    # Toggle lock ----
    # Two different policies. Standard drops samples whose published output is
    # missing and locks the rest; user-assembly refuses the whole operation if
    # any sample still has more than one non-ignored assembly.
    init("lock")
    on("lock", {
      req(session$userData$mode == "Assemble")
      req(selected())
      rv$updating <- rv$data |>
        dplyr::select(ID, assemble_lock) |>
        dplyr::slice(selected())
      if (userAsmb) {
        ## Ensure single assemble ---
        # TODO - could relax this constraint
        assemblies <- dplyr::tbl(session$userData$con, "assemblies") |>
          dplyr::filter(ignore == 0 & ID %in% !!rv$updating$ID) |>
          dplyr::pull(ID)
        if (any(duplicated(assemblies))) {
          shinyWidgets::sendSweetAlert(
            title = "Multiple assemblies detected.",
            text = "Only one assembly 'path' for each sample can be locked for annotation. Please open the assembly details and 'ignore' all but one assembly or use the consensus trimming feature.",
            type = "warning"
          )
          req(F)
        }
        lock_current <- as.numeric(names(which.max(table(rv$updating$assemble_lock))))
        rv$updating$assemble_lock <- as.numeric(!lock_current)
      } else {
        # Locking advances every non-ignored (path, scaffold) unit for the sample
        # (multi-assembly). Each unit was seeded an annotate row at assemble time.
        lock_current <- as.numeric(names(which.max(table(rv$updating$assemble_lock))))
        upd <- rv$updating
        # Locking hands the sample to WF2, which rebuilds the published output path
        # from assemble_opts. Samples that never assembled are not affected.
        if (lock_current == 0) {
          stale <- tryCatch(
            stale_assemble_dirs(
              session$userData$con,
              session$userData$dir_out,
              ids = upd$ID,
              pending_only = FALSE
            ),
            error = function(e) NULL
          )
          if (!is.null(stale) && nrow(stale) > 0L) {
            upd <- upd |> dplyr::filter(!ID %in% stale$ID)
            shinyWidgets::sendSweetAlert(
              title = "Assembly output not found",
              text = shiny::tags$div(
                shiny::tags$p(
                  "These samples were NOT locked, because Annotation and Curation ",
                  "would look for assembly output that is not on disk:"
                ),
                shiny::tags$ul(stale_assemble_items(stale)),
                shiny::tags$p("Either:"),
                shiny::tags$ul(
                  shiny::tags$li("set the assembly parameter set back to the name that exists on disk, or"),
                  shiny::tags$li("re-run Assembly so the output is published under the assigned name.")
                ),
                shiny::tags$p(
                  if (nrow(upd) > 0L) {
                    "The rest of the selected samples were locked."
                  } else {
                    "No other samples remained, so nothing was locked."
                  }
                )
              ),
              html = TRUE,
              type = "error"
            )
            req(nrow(upd) > 0L)
          }
        }
        upd$assemble_lock <- as.numeric(!lock_current)
        rv$updating <- upd
      }
      dplyr::tbl(session$userData$con, "assemble") |>
        dplyr::rows_update(
          rv$updating,
          unmatched = "ignore",
          in_place = TRUE,
          copy = TRUE,
          by = "ID"
        )
      rv$data <- rv$data |>
        dplyr::rows_update(rv$updating, by = "ID")
      trigger("update_assemble_table")
      trigger("refresh_annotate")
      trigger("refresh_export")
    })

    # Set Pre-process Opts ----
    open_opts_modal(input, "set_pre_opts", rv, selected, pre_opts_modal)
    observeEvent(input$pre_opts, ignoreInit = T, {
      exists <- input$pre_opts %in% rv$pre_opts$pre_opts
      shinyWidgets::updatePrettyCheckbox(
        inputId = "edit_pre_opts",
        value = !exists
      )
      if (exists) {
        cur <- rv$pre_opts[rv$pre_opts$pre_opts == input$pre_opts, ]
        updateNumericInput(
          inputId = "pre_opts_cpus",
          value = cur$cpus
        )
        updateNumericInput(
          inputId = "pre_opts_memory",
          value = cur$memory
        )
        updateTextAreaInput(
          inputId = "fastp",
          value = cur$fastp
        )
      }
    })
    observeEvent(input$edit_pre_opts, ignoreInit = T, {
      shinyjs::toggleState("pre_opts_cpus", condition = input$edit_pre_opts)
      shinyjs::toggleState("pre_opts_memory", condition = input$edit_pre_opts)
      shinyjs::toggleState("fastp", condition = input$edit_pre_opts)
      opts_indirect_guard(
        input, rv, rv$data,
        opts_col = "pre_opts", edit_id = "edit_pre_opts",
        confirm_id = "editing_opts_indirect", noun = "pre-processing options",
        lock_col = "assemble_lock", by = "ID"
      )
    })
    opts_indirect_confirm(input, rv, "editing_opts_indirect", "edit_pre_opts")
    observeEvent(input$update_pre_opts, ignoreInit = T, {
      if (input$edit_pre_opts) {
        dplyr::tbl(session$userData$con, "pre_opts") |>
          dplyr::rows_upsert(
            data.frame(
              pre_opts = req(input$pre_opts),
              cpus = req(input$pre_opts_cpus),
              memory = req(input$pre_opts_memory),
              fastp = req(input$fastp)
            ),
            in_place = TRUE,
            copy = TRUE,
            by = "pre_opts"
          )
        rv$pre_opts <- dplyr::tbl(session$userData$con, "pre_opts") |>
          dplyr::collect()
      }
      ## Update Assembly / Pre-processing Tables ----
      update <- data.frame(
        ID = c(rv$updating$ID, rv$updating_indirect$ID),
        pre_opts = input$pre_opts,
        assemble_switch = 1
      )
      dplyr::tbl(session$userData$con, "preprocess") |>
        dplyr::rows_update(
          update[, c("ID", "pre_opts")],
          unmatched = "ignore",
          in_place = TRUE,
          copy = TRUE,
          by = "ID"
        )
      dplyr::tbl(session$userData$con, "assemble") |>
        dplyr::rows_update(
          update[, c("ID", "assemble_switch")],
          unmatched = "ignore",
          in_place = TRUE,
          copy = TRUE,
          by = "ID"
        )
      rv$data <- rv$data |>
        dplyr::rows_update(
          update,
          by = "ID"
        ) |>
        dplyr::mutate(
          output = dplyr::case_when(
            assemble_switch > 1 ~ "output",
            .default = NA_character_
          ),
          view = dplyr::case_when(
            assemble_switch > 1 ~ "details",
            .default = NA_character_
          )
        )
      rv$updating <- rv$updating_indirect <- NULL
      removeModal()
      trigger("update_assemble_table")
    })

    # Set Assemble Opts ----
    open_opts_modal(input, "set_assemble_opts", rv, selected, assemble_opts_modal)
    observeEvent(input$assemble_opts, ignoreInit = T, {
      exists <- input$assemble_opts %in% rv$assemble_opts$assemble_opts
      shinyWidgets::updatePrettyCheckbox(
        inputId = "edit_assemble_opts",
        value = !exists
      )
      if (exists) {
        cur <- rv$assemble_opts[rv$assemble_opts$assemble_opts == input$assemble_opts, ]
        updateNumericInput(
          inputId = "assemble_opts_cpus",
          value = cur$cpus
        )
        updateNumericInput(
          inputId = "assemble_opts_memory",
          value = cur$memory
        )
        updateNumericInput(
          inputId = "max_paths",
          value = cur$max_paths %||% 10
        )
        updateNumericInput(
          inputId = "max_scaffolds",
          value = cur$max_scaffolds %||% 10
        )
        updateNumericInput(
          inputId = "min_assembly_length",
          value = cur$min_assembly_length %||% 500
        )
        shinyWidgets::updatePrettyCheckbox(
          session = session,
          inputId = "join_scaffolds",
          value = as.logical(cur$join_scaffolds %||% 0)
        )
        updateTextAreaInput(
          inputId = "getOrganelle",
          value = cur$getOrganelle
        )
        updateTextAreaInput(
          inputId = "seeds_db",
          value = cur$seeds_db
        )
        updateTextAreaInput(
          inputId = "labels_db",
          value = cur$labels_db
        )
        updateTextAreaInput(
          inputId = "mf_db",
          value = cur$mitofinder_db
        )
        updateTextAreaInput(
          inputId = "mitofinder",
          value = cur$mitofinder
        )
        updateSelectizeInput(
          inputId = "assembler",
          selected = cur$assembler
        )
        # Each help line lives inside its input's container, so toggling the
        # input shows/hides its help too (no separate help_* toggles needed).
        if (cur$assembler == "GetOrganelle") {
          shinyjs::hide(id = "mitofinder")
          shinyjs::hide(id = "mf_db")
          shinyjs::show(id = "getOrganelle")
          shinyjs::show(id = "seeds_db")
          shinyjs::show(id = "labels_db")
        } else if (cur$assembler == "MitoFinder") {
          shinyjs::show(id = "mitofinder")
          shinyjs::show(id = "mf_db")
          shinyjs::hide(id = "getOrganelle")
          shinyjs::hide(id = "seeds_db")
          shinyjs::hide(id = "labels_db")
        }
      }
    })
    observeEvent(input$edit_assemble_opts, ignoreInit = T, {
      shinyjs::toggleState("assembler", condition = input$edit_assemble_opts)
      shinyjs::toggleState("assemble_opts_cpus", condition = input$edit_assemble_opts)
      shinyjs::toggleState("assemble_opts_memory", condition = input$edit_assemble_opts)
      shinyjs::toggleState("getOrganelle", condition = input$edit_assemble_opts)
      shinyjs::toggleState("seeds_db", condition = input$edit_assemble_opts)
      shinyjs::toggleState("labels_db", condition = input$edit_assemble_opts)
      shinyjs::toggleState("mf_db", condition = input$edit_assemble_opts)
      shinyjs::toggleState("mitofinder", condition = input$edit_assemble_opts)
      shinyjs::toggleState("max_paths", condition = input$edit_assemble_opts)
      shinyjs::toggleState("max_scaffolds", condition = input$edit_assemble_opts)
      shinyjs::toggleState("min_assembly_length", condition = input$edit_assemble_opts)
      shinyjs::toggleState("join_scaffolds", condition = input$edit_assemble_opts)
      opts_indirect_guard(
        input, rv, rv$data,
        opts_col = "assemble_opts", edit_id = "edit_assemble_opts",
        confirm_id = "editing_assemble_opts_indirect", noun = "assembly options",
        lock_col = "assemble_lock", by = "ID"
      )
    })
    opts_indirect_confirm(input, rv, "editing_assemble_opts_indirect", "edit_assemble_opts")
    # toggle parameters depending on selected assembler. Each help line lives
    # inside its input's container, so toggling the input carries its help too.
    observeEvent(input$assembler, {
      if (input$assembler == "GetOrganelle") {
        shinyjs::hide(id = "mitofinder")
        shinyjs::hide(id = "mf_db")
        shinyjs::show(id = "getOrganelle")
        shinyjs::show(id = "seeds_db")
        shinyjs::show(id = "labels_db")
      } else if (input$assembler == "MitoFinder") {
        shinyjs::show(id = "mitofinder")
        shinyjs::show(id = "mf_db")
        shinyjs::hide(id = "getOrganelle")
        shinyjs::hide(id = "seeds_db")
        shinyjs::hide(id = "labels_db")
      }
    })
    ## Save Changes ----
    observeEvent(input$update_assemble_opts, ignoreInit = T, {
      ## Add to params table if new or editing ----
      if (input$edit_assemble_opts) {
        dplyr::tbl(session$userData$con, "assemble_opts") |>
          dplyr::rows_upsert(
            data.frame(
              assemble_opts = req(input$assemble_opts),
              cpus = req(input$assemble_opts_cpus),
              memory = req(input$assemble_opts_memory),
              getOrganelle = req(input$getOrganelle),
              seeds_db = req(input$seeds_db),
              labels_db = req(input$labels_db),
              assembler = req(input$assembler),
              mitofinder_db = req(input$mf_db),
              mitofinder = req(input$mitofinder),
              max_paths = as.integer(req(input$max_paths)),
              max_scaffolds = as.integer(req(input$max_scaffolds)),
              min_assembly_length = as.integer(req(input$min_assembly_length)),
              join_scaffolds = as.integer(isTRUE(input$join_scaffolds))
            ),
            in_place = TRUE,
            copy = TRUE,
            by = "assemble_opts"
          )
        rv$assemble_opts <- dplyr::tbl(session$userData$con, "assemble_opts") |>
          dplyr::collect()
      }
      ## Update Assembly Table ----
      # Captured before the write, rv$updating is cleared below.
      prior <- dplyr::bind_rows(rv$updating, rv$updating_indirect) |>
        dplyr::select(dplyr::any_of(c("ID", "assemble_opts")))
      update <- data.frame(
        ID = c(rv$updating$ID, rv$updating_indirect$ID),
        assemble_opts = input$assemble_opts,
        assemble_switch = 1
      )
      dplyr::tbl(session$userData$con, "assemble") |>
        dplyr::rows_update(
          update,
          unmatched = "ignore",
          in_place = TRUE,
          copy = TRUE,
          by = "ID"
        )
      rv$data <- rv$data |>
        dplyr::rows_update(
          update,
          by = "ID"
        )
      ## Flag samples with no output under the new parameter set ----
      dir_out <- session$userData$dir_out
      unpublished <- prior |> dplyr::slice(0)
      if (nrow(prior) > 0L && "assemble_opts" %in% names(prior) &&
          length(dir_out) == 1L && !is.na(dir_out) && nzchar(dir_out)) {
        unpublished <- prior |>
          dplyr::filter(
            assemble_opts != input$assemble_opts,
            !dir.exists(assemble_out_dir(dir_out, ID, input$assemble_opts))
          )
      }
      rv$updating <- rv$updating_indirect <- NULL
      removeModal()
      trigger("update_assemble_table")
      if (nrow(unpublished) > 0L) {
        shown <- unpublished |> dplyr::slice(seq_len(min(nrow(unpublished), 10)))
        items <- lapply(seq_len(nrow(shown)), function(i) {
          shiny::tags$li(
            shiny::tags$b(shown$ID[i]),
            " previously used ",
            shiny::tags$code(shown$assemble_opts[i])
          )
        })
        if (nrow(unpublished) > nrow(shown)) {
          items <- c(items, list(shiny::tags$li(
            paste0("... and ", nrow(unpublished) - nrow(shown), " more")
          )))
        }
        shinyWidgets::sendSweetAlert(
          title = "No assembly output for this parameter set",
          text = shiny::tags$div(
            shiny::tags$p(
              "These samples are now assigned to parameter set ",
              shiny::tags$code(input$assemble_opts),
              ", which has no assembly output on disk:"
            ),
            shiny::tags$ul(items),
            shiny::tags$p(
              "Re-run Assembly before locking these samples, or set the ",
              "parameter set back to a name that exists on disk."
            )
          ),
          html = TRUE,
          type = "warning"
        )
      }
    })

    # Set BLAST Opts ----
    open_opts_modal(input, "set_blast_opts", rv, selected, blast_opts_modal)
    observeEvent(input$blast_opts, ignoreInit = T, {
      exists <- input$blast_opts %in% rv$blast_opts$blast_opts
      shinyWidgets::updatePrettyCheckbox(
        inputId = "edit_blast_opts",
        value = !exists
      )
      if (exists) {
        cur <- rv$blast_opts[rv$blast_opts$blast_opts == input$blast_opts, ]
        shinyWidgets::updatePrettyCheckbox(
          inputId = "run_blast",
          value = as.logical(cur$run_blast)
        )
        updateTextInput(inputId = "entrez_query", value = cur$entrez_query %||% "")
        updateNumericInput(inputId = "max_target_seqs", value = as.integer(cur$max_target_seqs %||% 5L))
        updateTextAreaInput(inputId = "extra_opts", value = cur$extra_opts %||% "")
        if (as.logical(cur$run_blast)) {
          shinyjs::show(id = "blast_entrez_group")
          shinyjs::show(id = "blast_mts_group")
          shinyjs::show(id = "blast_extra_group")
        } else {
          shinyjs::hide(id = "blast_entrez_group")
          shinyjs::hide(id = "blast_mts_group")
          shinyjs::hide(id = "blast_extra_group")
        }
      }
    })
    observeEvent(input$edit_blast_opts, ignoreInit = T, {
      shinyjs::toggleState("run_blast",       condition = input$edit_blast_opts)
      shinyjs::toggleState("entrez_query",    condition = input$edit_blast_opts)
      shinyjs::toggleState("max_target_seqs", condition = input$edit_blast_opts)
      shinyjs::toggleState("extra_opts",      condition = input$edit_blast_opts)
      opts_indirect_guard(
        input, rv, rv$data,
        opts_col = "blast_opts", edit_id = "edit_blast_opts",
        confirm_id = "editing_blast_opts_indirect", noun = "BLAST options",
        lock_col = "assemble_lock", by = "ID"
      )
    })
    opts_indirect_confirm(input, rv, "editing_blast_opts_indirect", "edit_blast_opts")
    # Show/hide entrez + extra opts when run_blast toggle changes
    observeEvent(input$run_blast, ignoreInit = T, {
      if (isTRUE(input$run_blast)) {
        shinyjs::show(id = "blast_entrez_group")
        shinyjs::show(id = "blast_mts_group")
        shinyjs::show(id = "blast_extra_group")
      } else {
        shinyjs::hide(id = "blast_entrez_group")
        shinyjs::hide(id = "blast_mts_group")
        shinyjs::hide(id = "blast_extra_group")
      }
    })
    observeEvent(input$update_blast_opts, ignoreInit = T, {
      if (input$edit_blast_opts) {
        dplyr::tbl(session$userData$con, "blast_opts") |>
          dplyr::rows_upsert(
            data.frame(
              blast_opts      = req(input$blast_opts),
              run_blast       = as.integer(isTRUE(input$run_blast)),
              entrez_query    = input$entrez_query %||% "mitochondrion[Location]",
              max_target_seqs = as.integer(input$max_target_seqs %||% 5L),
              extra_opts      = input$extra_opts %||% ""
            ),
            in_place = TRUE,
            copy = TRUE,
            by = "blast_opts"
          )
        rv$blast_opts <- dplyr::tbl(session$userData$con, "blast_opts") |>
          dplyr::collect()
      }
      update <- data.frame(
        ID = c(rv$updating$ID, rv$updating_indirect$ID),
        blast_opts = input$blast_opts,
        assemble_switch = 1L
      )
      dplyr::tbl(session$userData$con, "assemble") |>
        dplyr::rows_update(
          update,
          unmatched = "ignore",
          in_place = TRUE,
          copy = TRUE,
          by = "ID"
        )
      rv$data <- rv$data |>
        dplyr::rows_update(update, by = "ID")
      rv$updating <- rv$updating_indirect <- NULL
      removeModal()
      trigger("update_assemble_table")
    })

    # Open output folder ----
    observeEvent(input$output, ignoreInit = T, {
      pth <- file.path(
        session$userData$dir_out,
        rv$data$ID[as.numeric(input$output)],
        "assemble",
        rv$data$assemble_opts[as.numeric(input$output)]
      )
      open_path(pth)
    })

    # Open Assembly Details ----
    observeEvent(input$details, ignoreInit = T, {
      rv$updating <- rv$data |> dplyr::slice(as.numeric(input$details))
      trigger("coverage_modal")
    })
    assembly_coverage_details_server(ns("coverage_details"), rv)

    # Open All BLAST Hits ----
    observeEvent(input$all_blast_hits, ignoreInit = T, {
      rv$updating <- rv$data |> dplyr::slice(as.numeric(input$all_blast_hits))
      blast_hits_modal(rv)
    })

    # CSV Export ----
    # Standard-only, matching the download buttons in assemble_ui().
    if (!userAsmb) {
      csv_download_outputs(
        output, rv, selected, "assemble",
        c("ignore_flags", "min_assembly_length", "output", "view", "blast_hits", "poor_blast_ref")
      )
    }
  })
}
