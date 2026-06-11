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
ASSEMBLE_STATE_CHOICES <- c(
  "Pre-Assembly" = "0",
  "Ready"        = "1",
  "In Progress"  = "4",
  "Success"      = "2",
  "Failed"       = "3"
)

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
      )
    ),
    reactableOutput(ns("table")),
    div(
      style = "margin-top: 12px; display: flex; gap: 8px;",
      downloadButton(ns("export_selected"), "Export Selected to CSV",
                     class = "btn-sm btn-default"),
      downloadButton(ns("export_all"), "Export All to CSV",
                     class = "btn-sm btn-default")
    )
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

    # Refresh ----
    init("refresh_assemble")
    on("refresh_assemble", {
      rv$data <- fetch_assemble_data()
      updateReactable(
        "table",
        data = rv$data
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
      hidden_state <- setdiff(unname(ASSEMBLE_STATE_CHOICES), state_filter_rv())
      rules <- c(
        if (length(hidden_grp))   paste0(".mp-grp-",   hidden_grp,   " { display: none !important; }"),
        if (length(hidden_lock))  paste0(".mp-lock-",  hidden_lock,  " { display: none !important; }"),
        if (length(hidden_state)) paste0(".mp-state-", hidden_state, " { display: none !important; }")
      )
      if (length(rules) == 0) return(NULL)
      tags$style(HTML(paste(rules, collapse = "\n")))
    })

    # Render table ----
    output$table <- renderReactable({
      isolate(req(rv$data)) |>
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
          height = 550,
          wrap = FALSE,
          pageSizeOptions = c(25, 50, 100, 200, 500),
          rowStyle = rt_highlight_row(),
          rowClass = JS("function(rowInfo) {
            if (!rowInfo || !rowInfo.values) return '';
            return 'mp-lock-' + rowInfo.values['assemble_lock'] +
                   ' mp-state-' + rowInfo.values['assemble_switch'];
          }"),
          defaultColDef = colDef(align = "left", show = F),
          columns = list(
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
            pre_opts = colDef(
              show = TRUE, class = .grp("pre_opts"), headerClass = .grp("pre_opts"),
              name = "Preprocess Opts.",
              html = T,
              width = 130,
              cell = rt_link(ns("set_pre_opts"))
            ),
            trimmed_reads = colDef(
              show = TRUE, class = .grp("trimmed_reads"), headerClass = .grp("trimmed_reads"),
              name = "Reads",
              filterable = FALSE,
              minWidth = 100
            ),
            mean_length = colDef(
              show = TRUE, class = .grp("mean_length"), headerClass = .grp("mean_length"),
              name = "Read Length",
              filterable = FALSE,
              minWidth = 100
            ),
            assemble_opts = colDef(
              show = TRUE, class = .grp("assemble_opts"), headerClass = .grp("assemble_opts"),
              name = "Assembly Opts.",
              html = T,
              width = 130,
              cell = rt_link(ns("set_assemble_opts"))
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
            scaffolds = colDef(
              show = TRUE, class = .grp("scaffolds"), headerClass = .grp("scaffolds"), width = 100, name = "# Scaffolds", align = "center"
            ),
            blast_accession = colDef(
              show = TRUE, class = .grp("blast_accession"), headerClass = .grp("blast_accession"),
              name = "BLAST Top Hit",
              html = TRUE,
              width = 120,
              cell = rt_ncbi_link()
            ),
            poor_blast_ref = colDef(show = FALSE),
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
        )
    })

    # update table ----
    init("update_assemble_table")
    on("update_assemble_table", {
      reactable::updateReactable(
        "table",
        data = rv$data |>
          dplyr::mutate(
            output = dplyr::case_when(
              assemble_switch > 1 ~ "output",
              .default = NA_character_
            ),
            view = dplyr::case_when(
              assemble_switch > 1 ~ "details",
              .default = NA_character_
            )
          ),
        selected = reactable::getReactableState("table", "selected"),
        page = reactable::getReactableState("table", "page")
      )
    })

    # table selection ----
    selected <- reactive(reactable::getReactableState("table", "selected"))

    # Set State ----
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
            choices = c("Pre-Assembly (wait)" = 0, "Ready to Assemble" = 1, "In Progress" = 4, "Successful Assembly" = 2, "Failed / Problematic" = 3),
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
    init("lock")
    on("lock", {
      req(session$userData$mode == "Assemble")
      req(selected())
      rv$updating <- rv$data |>
        dplyr::select(ID, assemble_lock) |>
        dplyr::slice(selected())
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
    observeEvent(input$set_pre_opts, {
      row <- as.numeric(input$set_pre_opts)
      if (length(selected()) > 0 && !row %in% selected()) {
        req(F)
      } else {
        selected <- c(row, selected()) |> unique()
      }
      req(all(rv$data$assemble_lock[selected] == 0))
      rv$updating <- rv$data |> dplyr::slice(selected)
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
      }
    })
    observeEvent(input$edit_pre_opts, ignoreInit = T, {
      shinyjs::toggleState("pre_opts_cpus", condition = input$edit_pre_opts)
      shinyjs::toggleState("pre_opts_memory", condition = input$edit_pre_opts)
      shinyjs::toggleState("fastp", condition = input$edit_pre_opts)
      # Check if editing opts that apply beyond selection
      if (input$edit_pre_opts && input$pre_opts %in% rv$data$pre_opts) {
        rv$updating_indirect <- rv$data |>
          dplyr::filter(pre_opts == input$pre_opts) |>
          dplyr::anti_join(rv$updating, by = "ID")

        # Prevent editing opts that apply to locked
        if (nrow(rv$updating_indirect) > 0L && any(rv$updating_indirect$assemble_lock == 1)) {
          shinyWidgets::sendSweetAlert(
            title = "Attempting to edit locked samples",
            text = "Processing parameters associated with locked samples can not be edited.",
            type = "warning"
          )
          shinyWidgets::updatePrettyCheckbox(
            inputId = "edit_pre_opts",
            value = FALSE
          )
          req(F)
        }

        if (nrow(rv$updating_indirect) > 0L) {
          shinyWidgets::confirmSweetAlert(
            inputId = "editing_opts_indirect",
            title = "Editing beyond selection",
            text = "You are attempting to edit pre-processing options that apply to samples beyond the current selection. Are you sure you want to proceed?",
            btn_colors = c("#0056b3", "#0056b3")
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
    observeEvent(input$set_assemble_opts, {
      row <- as.numeric(input$set_assemble_opts)
      if (length(selected()) > 0 && !row %in% selected()) {
        req(F)
      } else {
        selected <- c(row, selected()) |> unique()
      }
      req(all(rv$data$assemble_lock[selected] == 0))
      rv$updating <- rv$data |> dplyr::slice(selected)
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
      # Check if editing opts that apply beyond selection
      if (input$edit_assemble_opts && input$assemble_opts %in% rv$data$assemble_opts) {
        rv$updating_indirect <- rv$data |>
          dplyr::filter(assemble_opts == input$assemble_opts) |>
          dplyr::anti_join(rv$updating, by = "ID")
        # Prevent editing opts that apply to locked samples
        if (nrow(rv$updating_indirect) > 0L && any(rv$updating_indirect$assemble_lock == 1)) {
          shinyWidgets::sendSweetAlert(
            title = "Attempting to edit locked samples",
            text = "Processing parameters associated with locked samples can not be edited.",
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
          shinyWidgets::confirmSweetAlert(
            inputId = "editing_assemble_opts_indirect",
            title = "Editing beyond selection",
            text = "You are attempting to edit assembly options that apply to samples beyond the current selection. Are you sure you want to proceed?",
            btn_colors = c("#0056b3", "#0056b3")
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
    # toggle parameters depending on selected assembler
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
              min_assembly_length = as.integer(req(input$min_assembly_length))
            ),
            in_place = TRUE,
            copy = TRUE,
            by = "assemble_opts"
          )
        rv$assemble_opts <- dplyr::tbl(session$userData$con, "assemble_opts") |>
          dplyr::collect()
      }
      ## Update Assembly Table ----
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
      rv$updating <- rv$updating_indirect <- NULL
      removeModal()
      trigger("update_assemble_table")
    })

    # Set BLAST Opts ----
    observeEvent(input$set_blast_opts, {
      row <- as.numeric(input$set_blast_opts)
      if (length(selected()) > 0 && !row %in% selected()) {
        req(F)
      } else {
        selected <- c(row, selected()) |> unique()
      }
      req(all(rv$data$assemble_lock[selected] == 0))
      rv$updating <- rv$data |> dplyr::slice(selected)
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
        updateTextInput(inputId = "entrez_query", value = cur$entrez_query %||% "")
        updateTextAreaInput(inputId = "extra_opts", value = cur$extra_opts %||% "")
        if (as.logical(cur$run_blast)) {
          shinyjs::show(id = "blast_entrez_group")
          shinyjs::show(id = "blast_extra_group")
        } else {
          shinyjs::hide(id = "blast_entrez_group")
          shinyjs::hide(id = "blast_extra_group")
        }
      }
    })
    observeEvent(input$edit_blast_opts, ignoreInit = T, {
      shinyjs::toggleState("run_blast",    condition = input$edit_blast_opts)
      shinyjs::toggleState("entrez_query", condition = input$edit_blast_opts)
      shinyjs::toggleState("extra_opts",   condition = input$edit_blast_opts)
      # Check if editing opts that apply beyond selection
      if (input$edit_blast_opts && input$blast_opts %in% rv$data$blast_opts) {
        rv$updating_indirect <- rv$data |>
          dplyr::filter(blast_opts == input$blast_opts) |>
          dplyr::anti_join(rv$updating, by = "ID")
        if (nrow(rv$updating_indirect) > 0L && any(rv$updating_indirect$assemble_lock == 1)) {
          shinyWidgets::sendSweetAlert(
            title = "Attempting to edit locked samples",
            text = "Processing parameters associated with locked samples can not be edited.",
            type = "warning"
          )
          shinyWidgets::updatePrettyCheckbox(inputId = "edit_blast_opts", value = FALSE)
          req(F)
        }
        if (nrow(rv$updating_indirect) > 0L) {
          shinyWidgets::confirmSweetAlert(
            inputId = "editing_blast_opts_indirect",
            title = "Editing beyond selection",
            text = "You are attempting to edit BLAST options that apply to samples beyond the current selection. Are you sure you want to proceed?",
            btn_colors = c("#0056b3", "#0056b3")
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
    # Show/hide entrez + extra opts when run_blast toggle changes
    observeEvent(input$run_blast, ignoreInit = T, {
      if (isTRUE(input$run_blast)) {
        shinyjs::show(id = "blast_entrez_group")
        shinyjs::show(id = "blast_extra_group")
      } else {
        shinyjs::hide(id = "blast_entrez_group")
        shinyjs::hide(id = "blast_extra_group")
      }
    })
    observeEvent(input$update_blast_opts, ignoreInit = T, {
      if (input$edit_blast_opts) {
        dplyr::tbl(session$userData$con, "blast_opts") |>
          dplyr::rows_upsert(
            data.frame(
              blast_opts   = req(input$blast_opts),
              run_blast    = as.integer(isTRUE(input$run_blast)),
              entrez_query = input$entrez_query %||% "mitochondrion[Location]",
              extra_opts   = input$extra_opts %||% ""
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
      req(file.exists(pth))
      if (tolower(Sys.getenv("RSTUDIO_PROGRAM_MODE")) == "server") {
        rstudioapi::filesPaneNavigate(pth)
        req(F)
      } else {
        utils::browseURL(pth)
      }
    })

    # Open Assembly Details ----
    observeEvent(input$details, ignoreInit = T, {
      rv$updating <- rv$data |> dplyr::slice(as.numeric(input$details))
      trigger("coverage_modal")
    })
    assembly_coverage_details_server(ns("coverage_details"), rv)

    # CSV Export ----
    .export_cols_drop <- c("ignore_flags", "min_assembly_length", "output", "view", "poor_blast_ref")

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
