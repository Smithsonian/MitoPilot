# Togglable column groups for the Assemble table. Cols not listed here
# (sticky cols, action buttons) are always shown.
ASSEMBLE_COL_GROUPS <- list(
  Options  = c("pre_opts", "assemble_opts", "blast_opts"),
  Stats    = c("trimmed_reads", "mean_length", "topology", "length",
               "paths", "scaffolds"),
  BLAST    = c("blast_accession", "blast_ref_status", "blast_species",
               "blast_lineage", "blast_pident", "blast_qcovs"),
  Metadata = c("time_stamp", "assemble_notes", "join_notes")
)
# Reverse lookup col -> group, used to tag colDefs with a CSS class so the
# column-group picker can show/hide columns via CSS without re-rendering
# the table (preserves filters, sort, page, selection).
ASSEMBLE_COL_GROUP_LOOKUP <- {
  out <- character()
  for (.g in names(ASSEMBLE_COL_GROUPS)) {
    for (.c in ASSEMBLE_COL_GROUPS[[.g]]) out[.c] <- .g
  }
  out
}

# Status-filter pickers. Values match the assemble_lock / assemble_switch
# codes; each row is tagged with mp-lock-<v> / mp-state-<v> classes so
# unselected codes can be hidden via CSS (same mechanism as the column
# picker, so sort order, search, other filters, page, and selection survive).
ASSEMBLE_LOCK_CHOICES <- c("Unlocked" = "0", "Locked" = "1")
# State labels, icons and codes come from MP_STATE_META (R/constants.R). They
# cannot be derived at the top level here: R/ is collated alphabetically and
# constants.R loads after this file.

#' assemble UI
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
assemble_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("col_css")),
    div(
      class = "mp-filter-row",
      mp_filter_picker(ns("lock_filter"), "Lock:", ASSEMBLE_LOCK_CHOICES,
                       width = "140px"),
      mp_filter_picker(ns("state_filter"), "State:", mp_state_choices("assemble"),
                       width = "150px"),
      shinyWidgets::airDatepickerInput(
        inputId     = ns("date_filter"),
        label       = "Updated between:",
        range       = TRUE,
        clearButton = TRUE,
        value       = NULL,
        width       = "220px",
        placeholder = "any time"
      ),
      div(
        style = paste(
          "margin-left: 12px; padding-left: 16px;",
          "border-left: 1px solid var(--mp-border, #ddd);"
        ),
        mp_filter_picker(ns("col_groups"), "Columns:", names(ASSEMBLE_COL_GROUPS),
                         width = "150px")
      )
    ),
    uiOutput(ns("n_selected")),
    div(class = "mp-table-resize", reactableOutput(ns("table"))),
    assemble_csv_row(ns)
  )
}

#' assemble Server
#'
#' @noRd
assemble_server <- function(id) {
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
      data = fetch_assemble_data(),
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
      rv$data <- fetch_assemble_data()
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
    state_filter_rv <- reactiveVal(MP_STATE_CODES[["assemble"]])
    observeEvent(input$state_filter, {
      state_filter_rv(input$state_filter %||% character(0))
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    # CSS class for a togglable column. Returned class is added to both the
    # body cell (class) and the header cell (headerClass) so the
    # whole column collapses when the matching group is unselected.
    .grp <- function(col) {
      g <- ASSEMBLE_COL_GROUP_LOOKUP[col]
      if (is.na(g)) NULL else paste0("mp-grp-", g)
    }

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
      hidden_state <- setdiff(MP_STATE_CODES[["assemble"]], state_filter_rv())
      # Scope to THIS module's table so rules don't hit the shared mp-lock /
      # mp-state / mp-grp classes on the annotate, export, and userAsmb tables.
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
    # Render order comes from the data frame, not this list. See
    # fetch_assemble_data().
    output$table <- renderReactable({
      tbl_data <- isolate(req(filtered_data()))
      tbl_data |>
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
          theme = reactable::reactableTheme(
            headerStyle = list(
              whiteSpace = "normal", lineHeight = "1.2",
              # wrap = FALSE puts .rt-nowrap on the table, which sets nowrap on
              # the inner div; the theme selector outranks it.
              "& .rt-th-inner" = list(whiteSpace = "normal", textOverflow = "clip"),
              "& .rt-text-content" = list(whiteSpace = "normal", textOverflow = "clip")
            )
          ),
          defaultColDef = colDef(show = FALSE),
          columns = list(
            `.selection` = colDef(show = T, sticky = "left", width = 28, align = "center"),
            assemble_lock = colDef(
              show = TRUE,
              sticky = "left",
              name = mp_col_name("assemble_lock"),
              header = mp_col_header("assemble_lock", tip = MP_LOCK_DEF("assemble")),
              html = TRUE,
              width = 52,
              align = "center",
              filterable = FALSE,
              cell = rt_dynamicIcon(
                icons = c(`0` = "fa fa-lock-open", `1` = "fa fa-lock"),
                labels = c(`0` = "Unlocked", `1` = MP_LOCK_DEF("assemble"))
              )
            ),
            assemble_switch = colDef(
              show = TRUE,
              sticky = "left",
              name = mp_col_name("assemble_switch"),
              header = mp_col_header("assemble_switch"),
              html = TRUE,
              width = 62,
              align = "center",
              filterable = FALSE,
              cell = rt_dynamicIcon(
                icons = assemble_state_icons("assemble"),
                labels = assemble_state_titles("assemble")
              )
            ),
            ID = colDef(
              show = T,
              name = mp_col_name("ID"),
              minWidth = mp_fit_width(tbl_data$ID),
              sticky = "left",
              html = T,
              cell = rt_longtext()
            ),
            Taxon = colDef(
              show = T,
              name = mp_col_name("Taxon"),
              minWidth = 140,
              sticky = "left",
              html = T,
              cell = rt_longtext()
            ),
            pre_opts = colDef(
              show = TRUE, class = .grp("pre_opts"), headerClass = .grp("pre_opts"),
              name = mp_col_name("pre_opts"),
              header = mp_col_header("pre_opts"),
              html = T,
              width = 130,
              cell = rt_link(ns("set_pre_opts"), title = "Edit preprocessing options",
                             lock_col = "assemble_lock")
            ),
            trimmed_reads = colDef(
              show = TRUE, class = .grp("trimmed_reads"), headerClass = .grp("trimmed_reads"),
              name = mp_col_name("trimmed_reads"),
              header = mp_col_header("trimmed_reads"),
              filterable = FALSE,
              align = "center",
              minWidth = 100
            ),
            mean_length = colDef(
              show = TRUE, class = .grp("mean_length"), headerClass = .grp("mean_length"),
              name = mp_col_name("mean_length"),
              header = mp_col_header("mean_length"),
              filterable = FALSE,
              align = "center",
              minWidth = 100
            ),
            assemble_opts = colDef(
              show = TRUE, class = .grp("assemble_opts"), headerClass = .grp("assemble_opts"),
              name = mp_col_name("assemble_opts"),
              header = mp_col_header("assemble_opts"),
              html = T,
              width = 130,
              cell = rt_link(ns("set_assemble_opts"), title = "Edit assembly options",
                             lock_col = "assemble_lock")
            ),
            blast_opts = colDef(
              show = TRUE, class = .grp("blast_opts"), headerClass = .grp("blast_opts"),
              name = mp_col_name("blast_opts"),
              header = mp_col_header("blast_opts"),
              html = T,
              width = 120,
              cell = rt_link(ns("set_blast_opts"), title = "Edit BLAST options",
                             lock_col = "assemble_lock")
            ),
            topology = colDef(
              show = TRUE, class = paste(.grp("topology"), "mp-note-cell"),
              headerClass = .grp("topology"),
              minWidth = 120,
              align = "center",
              name = mp_col_name("topology"),
              header = mp_col_header("topology"),
              html = TRUE, cell = rt_topology()
            ),
            length = colDef(
              show = TRUE, class = .grp("length"), headerClass = .grp("length"),
              minWidth = 140,
              align = "center",
              name = mp_col_name("length"),
              header = mp_col_header("length"),
              filterable = FALSE,
              html = TRUE,
              cell = JS("function(cellInfo) {
                var val = cellInfo.value;
                if (!val) return val;
                var flagsStr = cellInfo.row['ignore_flags'];
                var flags = flagsStr ? String(flagsStr).split(';') : [];
                var parts = String(val).split(';');
                var marked = parts.map(function(p, i) {
                  if (flags[i] === '1') {
                    return `<span class='mp-pill mp-pill-danger' ` +
                      `title='Ignored: left out of annotation and export'>` +
                      p.trim() + '</span>';
                  }
                  return p.trim();
                });
                return marked.join('; ');
              }")
            ),
            min_assembly_length = colDef(show = FALSE),
            ignore_flags = colDef(show = FALSE),
            paths = colDef(
              show = TRUE, class = .grp("paths"), headerClass = .grp("paths"),
              width = 80,
              align = "center",
              name = mp_col_name("paths"),
              header = mp_col_header("paths"),
              cell = JS("function(cellInfo){if(cellInfo.value<0){return -cellInfo.value };return cellInfo.value}"),
              style = JS("function(rowInfo){ if (rowInfo.values.paths < 0) return { backgroundColor: '#00000020' }}")
            ),
            scaffolds = colDef(
              show = TRUE, class = .grp("scaffolds"), headerClass = .grp("scaffolds"),
              width = 95,
              align = "center",
              name = mp_col_name("scaffolds"),
              header = mp_col_header("scaffolds")
            ),
            blast_accession = colDef(
              show = TRUE, class = .grp("blast_accession"), headerClass = .grp("blast_accession"),
              name = mp_col_name("blast_accession"),
              header = mp_col_header("blast_accession"),
              html = TRUE,
              width = 120,
              cell = rt_ncbi_link()
            ),
            poor_blast_ref = colDef(show = FALSE),
            blast_ref_status = colDef(
              show = TRUE, class = .grp("blast_ref_status"), headerClass = .grp("blast_ref_status"),
              name = mp_col_name("blast_ref_status"),
              header = mp_col_header("blast_ref_status"),
              html = TRUE,
              minWidth = 130,
              resizable = TRUE,
              align = "center",
              filterable = TRUE,
              cell = rt_blast_ref_status()
            ),
            blast_species = colDef(
              show = TRUE, class = .grp("blast_species"), headerClass = .grp("blast_species"),
              name = mp_col_name("blast_species"),
              header = mp_col_header("blast_species"),
              html = TRUE,
              minWidth = 160,
              cell = rt_longtext()
            ),
            blast_lineage = colDef(
              show = TRUE, class = .grp("blast_lineage"), headerClass = .grp("blast_lineage"),
              name = mp_col_name("blast_lineage"),
              header = mp_col_header("blast_lineage"),
              html = TRUE,
              minWidth = 200,
              cell = rt_longtext()
            ),
            blast_pident = colDef(
              show = TRUE, class = .grp("blast_pident"), headerClass = .grp("blast_pident"),
              name = mp_col_name("blast_pident"),
              header = mp_col_header("blast_pident"),
              filterable = FALSE,
              align = "center",
              width = 90
            ),
            blast_qcovs = colDef(
              show = TRUE, class = .grp("blast_qcovs"), headerClass = .grp("blast_qcovs"),
              name = mp_col_name("blast_qcovs"),
              header = mp_col_header("blast_qcovs"),
              filterable = FALSE,
              align = "center",
              width = 90
            ),
            time_stamp = colDef(
              show = TRUE, class = .grp("time_stamp"), headerClass = .grp("time_stamp"),
              name = mp_col_name("time_stamp"),
              header = mp_col_header("time_stamp"),
              filterable = FALSE,
              html = T,
              width = 150,
              align = "center",
              cell = rt_ts_date()
            ),
            assemble_notes = colDef(
              show = TRUE, class = paste(c(.grp("assemble_notes"), "mp-note-cell"), collapse = " "),
              headerClass = .grp("assemble_notes"),
              name = mp_col_name("assemble_notes"),
              header = mp_col_header("assemble_notes"),
              html = TRUE,
              minWidth = 150,
              cell = rt_longtext()
            ),
            join_notes = colDef(
              show = TRUE, class = paste(c(.grp("join_notes"), "mp-note-cell"), collapse = " "),
              headerClass = .grp("join_notes"),
              name = mp_col_name("join_notes"),
              header = mp_col_header("join_notes"),
              html = TRUE,
              minWidth = 150,
              cell = rt_longtext()
            ),
            blast_hits = colDef(
              show = TRUE,
              name = mp_col_name("blast_hits"),
              filterable = FALSE,
              sortable = FALSE,
              html = TRUE,
              width = 140,
              align = "center",
              cell = rt_icon_bttn_text(
                ns("all_blast_hits"), "fas fa-list", "All BLAST Hits",
                title = "Show every BLAST hit for this sample"
              )
            ),
            view = colDef(
              show = TRUE,
              sticky = "right",
              class = "mp-actions-sticky",
              headerClass = "mp-actions-sticky",
              filterable = FALSE,
              sortable = FALSE,
              name = mp_col_name("view"),
              html = TRUE,
              width = 90,
              align = "center",
              cell = rt_icon_bttn_text(
                ns("details"), "fas fa-square-arrow-up-right fa-xs",
                label = "Details",
                title = "Open the details window for this sample"
              )
            ),
            output = colDef(
              show = TRUE,
              sticky = "right",
              class = "mp-actions-sticky",
              headerClass = "mp-actions-sticky",
              filterable = FALSE,
              sortable = FALSE,
              name = mp_col_name("output"),
              html = TRUE,
              width = 90,
              align = "center",
              cell = rt_icon_bttn_text(
                ns("output"), "fas fa-folder-open fa-xs",
                label = "Output",
                title = "Open the output folder for this sample"
              )
            )
          )
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

    # The toolbar lives in the top-level UI, so this is scoped by container
    # class, not by id (theme T01).
    observe({
      shinyjs::toggleState(
        selector = "#asmb_ctrls .mp-needs-selection",
        condition = length(selected()) > 0
      )
    })

    # Rows the pickers and the date filter leave visible. reactable's own
    # search and column filters are client-side, so they are not counted.
    output$n_selected <- renderUI({
      vis <- filtered_data()
      shown <- sum(
        as.character(vis$assemble_lock) %in% lock_filter_rv() &
          as.character(vis$assemble_switch) %in% state_filter_rv()
      )
      assemble_table_status(shown, nrow(rv$data), length(selected()))
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
    init("state")
    on("state", {
      req(session$userData$mode == "Assemble")
      if (!need_selection(length(selected()))) return()
      if (!need_unlocked(assemble_locked_ids(rv, selected()))) return()
      rv$updating <- rv$data |>
        dplyr::select(ID, assemble_switch) |>
        dplyr::slice(selected())
      current <- character(0)
      if (length(unique(rv$updating$assemble_switch)) == 1) {
        current <- as.character(rv$updating$assemble_switch[1])
      }
      assemble_state_modal(rv$updating$ID, current)
    })
    observeEvent(input$update_state, {
      if (!isTruthy(input$new_state)) {
        mp_toast("Choose a state first.", type = "warning")
        return()
      }
      n <- nrow(rv$updating)
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
      mp_toast(paste0(
        mp_n(n, "sample"), " set to ",
        MP_STATE_META[[as.character(input$new_state)]]$label, "."
      ))
    })

    # Toggle lock ----
    init("lock")
    on("lock", {
      req(session$userData$mode == "Assemble")
      if (!need_selection(length(selected()))) return()
      assemble_lock_begin(rv, selected(), unit = "assembly")
    })
    observeEvent(input$lock_confirm, {
      if (isTRUE(input$lock_confirm)) assemble_lock_finish(rv)
    })


    # Set Pre-process Opts ----
    observeEvent(input$set_pre_opts, {
      rows <- assemble_opts_rows(rv, as.numeric(input$set_pre_opts), selected())
      if (is.null(rows)) return()
      rv$updating <- rv$data |> dplyr::slice(rows)
      rv$updating_indirect <- rv$updating |> dplyr::slice(0)
      pre_opts_modal(rv)
    })
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
        shinyWidgets::updatePrettyCheckbox(
          inputId = "dedup",
          value = grepl("--dedup", cur$fastp %||% "", fixed = TRUE)
        )
      }
    })
    observeEvent(input$edit_pre_opts, ignoreInit = T, {
      shinyjs::toggleState("pre_opts_cpus", condition = input$edit_pre_opts)
      shinyjs::toggleState("pre_opts_memory", condition = input$edit_pre_opts)
      shinyjs::toggleState("fastp", condition = input$edit_pre_opts)
      shinyjs::toggleState("dedup", condition = input$edit_pre_opts)
      # Check if editing opts that apply beyond selection
      if (input$edit_pre_opts && input$pre_opts %in% rv$data$pre_opts) {
        rv$updating_indirect <- rv$data |>
          dplyr::filter(pre_opts == input$pre_opts) |>
          dplyr::anti_join(rv$updating, by = "ID")

        # Prevent editing opts that apply to locked
        if (nrow(rv$updating_indirect) > 0L && any(rv$updating_indirect$assemble_lock == 1)) {
          mp_alert(
            title = "Locked samples cannot be edited",
            text = paste(
              "This parameter set is also used by locked samples, so its",
              "values cannot be changed. Unlock those samples first."
            ),
            type = "warning"
          )
          shinyWidgets::updatePrettyCheckbox(
            inputId = "edit_pre_opts",
            value = FALSE
          )
          req(F)
        }

        if (nrow(rv$updating_indirect) > 0L) {
          mp_confirm(
            "editing_opts_indirect",
            title = "Edit beyond the selection",
            text = paste0(
              "These preprocessing options also apply to ",
              mp_n(nrow(rv$updating_indirect), "sample"),
              " outside the current selection, which this edit will change too."
            ),
            action_label = "Continue"
          )
        }
      } else {
        rv$updating_indirect <- rv$updating |> dplyr::slice(0)
      }
    })
    # Confirm editing opts that apply beyond selection
    observeEvent(input$editing_opts_indirect, ignoreInit = T, {
      if (!input$editing_opts_indirect) {
        rv$updating_indirect <- rv$updating |> dplyr::slice(0)
        shinyWidgets::updatePrettyCheckbox(
          inputId = "edit_pre_opts",
          value = FALSE
        )
      }
    })
    observeEvent(input$update_pre_opts, ignoreInit = T, {
      if (input$edit_pre_opts) {
        dplyr::tbl(session$userData$con, "pre_opts") |>
          dplyr::rows_upsert(
            data.frame(
              pre_opts = req(input$pre_opts),
              cpus = req(input$pre_opts_cpus),
              memory = req(input$pre_opts_memory),
              fastp = .fastp_set_dedup(req(input$fastp), isTRUE(input$dedup))
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
          ),
        )
      rv$updating <- rv$updating_indirect <- NULL
      removeModal()
      trigger("update_assemble_table")
      mp_opts_saved_toast(nrow(update), input$pre_opts)
    })

    # Set Assemble Opts ----
    observeEvent(input$set_assemble_opts, {
      rows <- assemble_opts_rows(rv, as.numeric(input$set_assemble_opts), selected())
      if (is.null(rows)) return()
      rv$updating <- rv$data |> dplyr::slice(rows)
      rv$updating_indirect <- rv$updating |> dplyr::slice(0)
      assemble_opts_modal(rv)
    })
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
        updateTextInput(
          inputId = "maptoref_ref",
          value = (cur$maptoref_ref %||% NA_character_) %|NA|% ""
        )
        updateSelectInput(
          inputId = "maptoref_topology",
          selected = (cur$maptoref_topology %||% NA_character_) %|NA|% ""
        )
        updateSelectInput(inputId = "maptoref_mapper",
                          selected = cur$maptoref_mapper %||% "bowtie2")
        updateTextInput(inputId = "maptoref", value = cur$maptoref)
        updateTextInput(inputId = "maptoref_consensus", value = cur$maptoref_consensus)
        updateNumericInput(inputId = "maptoref_iter", value = cur$maptoref_iter)
        updateSelectizeInput(
          inputId = "assembler",
          selected = cur$assembler
        )
        maptoref_ids <- c("maptoref_ref", "maptoref_topology", "maptoref_mapper",
                          "maptoref", "maptoref_consensus", "maptoref_iter")
        # Each help line lives inside its input's container, so toggling the
        # input shows/hides its help too (no separate help_* toggles needed).
        if (cur$assembler == "GetOrganelle") {
          shinyjs::hide(id = "mitofinder")
          shinyjs::hide(id = "mf_db")
          shinyjs::show(id = "getOrganelle")
          shinyjs::show(id = "seeds_db")
          shinyjs::show(id = "labels_db")
          for (i in maptoref_ids) shinyjs::hide(id = i)
        } else if (cur$assembler == "MitoFinder") {
          shinyjs::show(id = "mitofinder")
          shinyjs::show(id = "mf_db")
          shinyjs::hide(id = "getOrganelle")
          shinyjs::hide(id = "seeds_db")
          shinyjs::hide(id = "labels_db")
          for (i in maptoref_ids) shinyjs::hide(id = i)
        } else if (cur$assembler == "MapToRef") {
          shinyjs::hide(id = "mitofinder")
          shinyjs::hide(id = "mf_db")
          shinyjs::hide(id = "getOrganelle")
          shinyjs::hide(id = "seeds_db")
          shinyjs::hide(id = "labels_db")
          for (i in maptoref_ids) shinyjs::show(id = i)
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
      for (i in c("maptoref_ref", "maptoref_topology", "maptoref_mapper",
                  "maptoref", "maptoref_consensus", "maptoref_iter")) {
        shinyjs::toggleState(i, condition = input$edit_assemble_opts)
      }
      # Check if editing opts that apply beyond selection
      if (input$edit_assemble_opts && input$assemble_opts %in% rv$data$assemble_opts) {
        rv$updating_indirect <- rv$data |>
          dplyr::filter(assemble_opts == input$assemble_opts) |>
          dplyr::anti_join(rv$updating, by = "ID")
        # Prevent editing opts that apply to locked samples
        if (nrow(rv$updating_indirect) > 0L && any(rv$updating_indirect$assemble_lock == 1)) {
          mp_alert(
            title = "Locked samples cannot be edited",
            text = paste(
              "This parameter set is also used by locked samples, so its",
              "values cannot be changed. Unlock those samples first."
            ),
            type = "warning"
          )
          shinyWidgets::updatePrettyCheckbox(
            inputId = "edit_assemble_opts",
            value = FALSE
          )
          req(F)
        }
        # Confirm editing opts that apply beyond selection
        if (nrow(rv$updating_indirect) > 0L) {
          mp_confirm(
            "editing_assemble_opts_indirect",
            title = "Edit beyond the selection",
            text = paste0(
              "These assembly options also apply to ",
              mp_n(nrow(rv$updating_indirect), "sample"),
              " outside the current selection, which this edit will change too."
            ),
            action_label = "Continue"
          )
        }
      } else {
        rv$updating_indirect <- rv$updating |> dplyr::slice(0)
      }
    })
    # Confirm editing opts that apply beyond selection
    observeEvent(input$editing_assemble_opts_indirect, ignoreInit = T, {
      if (!input$editing_assemble_opts_indirect) {
        rv$updating_indirect <- rv$updating |> dplyr::slice(0)
        shinyWidgets::updatePrettyCheckbox(
          inputId = "edit_assemble_opts",
          value = FALSE
        )
      }
    })
    # toggle parameters depending on selected assembler. Each help line lives
    # inside its input's container, so toggling the input carries its help too.
    # Switching mapper swaps the options box only when it still holds the
    # other mapper's default.
    observeEvent(input$maptoref_mapper, {
      cur <- trimws(input$maptoref %||% "")
      defaults <- c("bowtie2" = .mtr_default_bowtie2, "bwa-mem" = .mtr_default_bwa)
      if (cur %in% defaults) {
        updateTextInput(inputId = "maptoref",
                        value = defaults[[input$maptoref_mapper]])
      }
    }, ignoreInit = TRUE)
    observeEvent(input$assembler, {
      maptoref_ids <- c("maptoref_ref", "maptoref_topology", "maptoref_mapper",
                        "maptoref", "maptoref_consensus", "maptoref_iter")
      if (input$assembler == "GetOrganelle") {
        shinyjs::hide(id = "mitofinder")
        shinyjs::hide(id = "mf_db")
        shinyjs::show(id = "getOrganelle")
        shinyjs::show(id = "seeds_db")
        shinyjs::show(id = "labels_db")
        for (i in maptoref_ids) shinyjs::hide(id = i)
      } else if (input$assembler == "MitoFinder") {
        shinyjs::show(id = "mitofinder")
        shinyjs::show(id = "mf_db")
        shinyjs::hide(id = "getOrganelle")
        shinyjs::hide(id = "seeds_db")
        shinyjs::hide(id = "labels_db")
        for (i in maptoref_ids) shinyjs::hide(id = i)
      } else if (input$assembler == "MapToRef") {
        shinyjs::hide(id = "mitofinder")
        shinyjs::hide(id = "mf_db")
        shinyjs::hide(id = "getOrganelle")
        shinyjs::hide(id = "seeds_db")
        shinyjs::hide(id = "labels_db")
        for (i in maptoref_ids) shinyjs::show(id = i)
      }
    })
    ## Save Changes ----
    observeEvent(input$update_assemble_opts, ignoreInit = T, {
      ## Add to params table if new or editing ----
      if (input$edit_assemble_opts) {
        ref_value <- trimws(input$maptoref_ref %||% "")
        topology_value <- trimws(input$maptoref_topology %||% "")
        needs_topology <- identical(input$assembler, "MapToRef") &&
          nzchar(ref_value) &&
          !identical(.mtr_ref_class(ref_value), "accession") &&
          !grepl("\\.(gb|gbk|gbff)$", ref_value, ignore.case = TRUE) &&
          !nzchar(topology_value)
        if (needs_topology) {
          mp_alert(
            title = "Reference topology required",
            text = paste("Set the reference topology (circular or linear) for a",
                         "FASTA reference. A GenBank (.gb) reference takes its",
                         "topology from the file."),
            type = "error"
          )
          return()
        }
        if (identical(input$assembler, "MapToRef") &&
            grepl(.mtr_bad_chars_re, paste(ref_value,
                                           input$maptoref %||% "",
                                           input$maptoref_consensus %||% ""))) {
          mp_alert(
            title = "Invalid characters in MapToRef options",
            text = paste("The reference, bowtie2, and samtools consensus values",
                         "are passed through a shell call, so they cannot",
                         "contain a quote, dollar sign, backtick, or backslash."),
            type = "error"
          )
          return()
        }
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
              join_scaffolds = as.integer(isTRUE(input$join_scaffolds)),
              maptoref_ref = if (nzchar(ref_value)) ref_value else NA_character_,
              maptoref_mapper = input$maptoref_mapper %||% "bowtie2",
              maptoref = if (nzchar(trimws(input$maptoref %||% ""))) {
                input$maptoref
              } else if (identical(input$maptoref_mapper, "bwa-mem")) {
                .mtr_default_bwa
              } else {
                .mtr_default_bowtie2
              },
              maptoref_consensus = if (nzchar(trimws(input$maptoref_consensus %||% ""))) {
                input$maptoref_consensus
              } else {
                .mtr_default_consensus
              },
              maptoref_iter = as.integer(input$maptoref_iter %||% 5L) %|NA|% 5L,
              maptoref_topology = if (nzchar(topology_value)) topology_value else NA_character_
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
      mp_opts_saved_toast(nrow(update), input$assemble_opts)
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
        mp_alert(
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
    observeEvent(input$set_blast_opts, {
      rows <- assemble_opts_rows(rv, as.numeric(input$set_blast_opts), selected())
      if (is.null(rows)) return()
      rv$updating <- rv$data |> dplyr::slice(rows)
      rv$updating_indirect <- rv$updating |> dplyr::slice(0)
      blast_opts_modal(rv)
    })
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
        # isTRUE()/is.na() rather than a bare as.logical(): a row inserted by an
        # older MitoPilot has NA in these columns, which %||% does not catch.
        updateTextInput(
          inputId = "taxids",
          value = if (is.na(cur$taxids %||% NA)) "" else cur$taxids
        )
        shinyWidgets::updatePrettyCheckbox(
          inputId = "remote_blast",
          value = isTRUE(as.logical(cur$remote_blast %||% 0L))
        )
        shinyWidgets::updatePrettyCheckbox(
          inputId = "remote_fallback",
          value = !isFALSE(as.logical(cur$remote_fallback %||% 1L))
        )
        updateTextInput(inputId = "entrez_query", value = cur$entrez_query %||% "")
        updateNumericInput(inputId = "max_target_seqs", value = as.integer(cur$max_target_seqs %||% 5L))
        updateTextAreaInput(inputId = "extra_opts", value = cur$extra_opts %||% "")
        if (as.logical(cur$run_blast)) {
          shinyjs::show(id = "blast_taxids_group")
          shinyjs::show(id = "blast_remote_group")
          # Entrez query only applies to the remote search
          shinyjs::toggle(
            id = "blast_entrez_group",
            condition = isTRUE(as.logical(cur$remote_blast %||% 0L))
          )
          shinyjs::show(id = "blast_mts_group")
          shinyjs::show(id = "blast_extra_group")
        } else {
          shinyjs::hide(id = "blast_taxids_group")
          shinyjs::hide(id = "blast_remote_group")
          shinyjs::hide(id = "blast_entrez_group")
          shinyjs::hide(id = "blast_mts_group")
          shinyjs::hide(id = "blast_extra_group")
        }
      }
    })
    observeEvent(input$edit_blast_opts, ignoreInit = T, {
      shinyjs::toggleState("run_blast",       condition = input$edit_blast_opts)
      shinyjs::toggleState("taxids",          condition = input$edit_blast_opts)
      shinyjs::toggleState("remote_blast",    condition = input$edit_blast_opts)
      shinyjs::toggleState("remote_fallback", condition = input$edit_blast_opts)
      shinyjs::toggleState("entrez_query",    condition = input$edit_blast_opts)
      shinyjs::toggleState("max_target_seqs", condition = input$edit_blast_opts)
      shinyjs::toggleState("extra_opts",      condition = input$edit_blast_opts)
      # Check if editing opts that apply beyond selection
      if (input$edit_blast_opts && input$blast_opts %in% rv$data$blast_opts) {
        rv$updating_indirect <- rv$data |>
          dplyr::filter(blast_opts == input$blast_opts) |>
          dplyr::anti_join(rv$updating, by = "ID")
        if (nrow(rv$updating_indirect) > 0L && any(rv$updating_indirect$assemble_lock == 1)) {
          mp_alert(
            title = "Locked samples cannot be edited",
            text = paste(
              "This parameter set is also used by locked samples, so its",
              "values cannot be changed. Unlock those samples first."
            ),
            type = "warning"
          )
          shinyWidgets::updatePrettyCheckbox(inputId = "edit_blast_opts", value = FALSE)
          req(F)
        }
        if (nrow(rv$updating_indirect) > 0L) {
          mp_confirm(
            "editing_blast_opts_indirect",
            title = "Edit beyond the selection",
            text = paste0(
              "These BLAST options also apply to ",
              mp_n(nrow(rv$updating_indirect), "sample"),
              " outside the current selection, which this edit will change too."
            ),
            action_label = "Continue"
          )
        }
      } else {
        rv$updating_indirect <- rv$updating |> dplyr::slice(0)
      }
    })
    observeEvent(input$editing_blast_opts_indirect, ignoreInit = T, {
      if (!input$editing_blast_opts_indirect) {
        rv$updating_indirect <- rv$updating |> dplyr::slice(0)
        shinyWidgets::updatePrettyCheckbox(inputId = "edit_blast_opts", value = FALSE)
      }
    })
    # Show/hide search restriction + extra opts when run_blast toggle changes
    observeEvent(input$run_blast, ignoreInit = T, {
      if (isTRUE(input$run_blast)) {
        shinyjs::show(id = "blast_taxids_group")
        shinyjs::show(id = "blast_remote_group")
        shinyjs::toggle(id = "blast_entrez_group", condition = isTRUE(input$remote_blast))
        shinyjs::show(id = "blast_mts_group")
        shinyjs::show(id = "blast_extra_group")
      } else {
        shinyjs::hide(id = "blast_taxids_group")
        shinyjs::hide(id = "blast_remote_group")
        shinyjs::hide(id = "blast_entrez_group")
        shinyjs::hide(id = "blast_mts_group")
        shinyjs::hide(id = "blast_extra_group")
      }
    })
    # Entrez query is remote-only, so reveal it live with the remote toggle
    observeEvent(input$remote_blast, ignoreInit = T, {
      shinyjs::toggle(
        id = "blast_entrez_group",
        condition = isTRUE(input$run_blast) && isTRUE(input$remote_blast)
      )
    })
    observeEvent(input$update_blast_opts, ignoreInit = T, {
      if (input$edit_blast_opts) {
        # Numeric NCBI taxon IDs only; validated here, with no network lookup, so
        # the save path keeps working offline.
        taxids <- paste(
          trimws(strsplit(trimws(input$taxids %||% ""), ",")[[1]]),
          collapse = ","
        )
        if (nzchar(taxids) && !grepl("^[0-9]+(,[0-9]+)*$", taxids)) {
          mp_alert(
            title = "Invalid taxon restriction",
            text = paste0(
              "Enter comma-separated numeric NCBI taxon IDs (e.g. 7711 or ",
              "7711,6656), or leave the field blank. Look up IDs at ",
              "https://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi"
            ),
            type = "error"
          )
          req(F)
        }
        dplyr::tbl(session$userData$con, "blast_opts") |>
          dplyr::rows_upsert(
            data.frame(
              blast_opts      = req(input$blast_opts),
              run_blast       = as.integer(isTRUE(input$run_blast)),
              # An emptied field is stored as the historical default rather than
              # "": that is a no-op for the local search (so emptying the field is
              # the documented way out of a blocked legacy query) and it keeps a
              # remote search mitochondrion-restricted instead of hitting all of
              # core_nt, where a nuclear or NUMT record could win rank 1.
              # Forced to the default whenever Remote BLAST is off. The field is
              # hidden in that state, so a value left over from a previous remote
              # setup would otherwise be saved unseen and then fail every sample
              # at the local search, with the field needed to fix it invisible.
              entrez_query    = if (isTRUE(input$remote_blast) &&
                                    nzchar(trimws(input$entrez_query %||% ""))) {
                input$entrez_query
              } else {
                "mitochondrion[Location]"
              },
              taxids          = taxids,
              remote_blast    = as.integer(isTRUE(input$remote_blast)),
              remote_fallback = as.integer(input$remote_fallback %||% TRUE),
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
      mp_opts_saved_toast(nrow(update), input$blast_opts)
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
    .export_cols_drop <- c("ignore_flags", "min_assembly_length", "output", "view", "blast_hits", "poor_blast_ref")

    observe({
      shinyjs::toggleState("export_selected", condition = length(selected()) > 0)
    })

    output$export_selected <- downloadHandler(
      filename = function() paste0("assemble_selected_", Sys.Date(), ".csv"),
      content = function(file) {
        req(length(selected()) > 0)
        rv$data |>
          dplyr::slice(selected()) |>
          dplyr::select(-dplyr::any_of(.export_cols_drop)) |>
          write.csv(file, row.names = FALSE)
      }
    )

    output$export_all <- downloadHandler(
      filename = function() paste0("assemble_all_", Sys.Date(), ".csv"),
      content = function(file) {
        rv$data |>
          dplyr::select(-dplyr::any_of(.export_cols_drop)) |>
          write.csv(file, row.names = FALSE)
      }
    )
  })
}
