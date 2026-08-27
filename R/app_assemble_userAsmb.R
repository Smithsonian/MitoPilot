# Togglable column groups for the user-assemble table. Cols not listed here
# (sticky cols, action buttons, the always-shown Input Assembly File) are
# always visible. Mirrors ASSEMBLE_COL_GROUPS but drops assemble_opts (no
# assembler step in user-assemble mode).
ASSEMBLE_COL_GROUPS_USERASMB <- list(
  Options  = c("pre_opts", "find_mito_opts", "circularize_opts", "blast_opts"),
  Stats    = c("trimmed_reads", "mean_length", "topology", "length",
               "paths", "scaffolds"),
  BLAST    = c("blast_accession", "blast_ref_status", "blast_species",
               "blast_lineage", "blast_pident", "blast_qcovs"),
  Metadata = c("time_stamp", "assemble_notes", "circularize_notes",
               "find_mito_notes", "join_notes")
)
ASSEMBLE_COL_GROUP_LOOKUP_USERASMB <- {
  out <- character()
  for (.g in names(ASSEMBLE_COL_GROUPS_USERASMB)) {
    for (.c in ASSEMBLE_COL_GROUPS_USERASMB[[.g]]) out[.c] <- .g
  }
  out
}

#' assemble UI
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
assemble_ui_userAsmb <- function(id) {
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
        choices  = names(ASSEMBLE_COL_GROUPS_USERASMB),
        selected = names(ASSEMBLE_COL_GROUPS_USERASMB),
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
    )
  )
}

#' assemble Server
#'
#' @noRd
assemble_server_userAsmb <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    register_tool_help("fastp", input, reopen = function() pre_opts_modal(rv))
    register_tool_help("blastn", input, reopen = function() blast_opts_modal(rv))

    # Prepare data ----
    rv <- reactiveValues(
      pre_opts = dplyr::tbl(session$userData$con, "pre_opts") |>
        dplyr::collect(),
      blast_opts = dplyr::tbl(session$userData$con, "blast_opts") |>
        dplyr::collect(),
      circularize_opts = dplyr::tbl(session$userData$con, "circularize_opts") |>
        dplyr::collect(),
      find_mito_opts = dplyr::tbl(session$userData$con, "find_mito_opts") |>
        dplyr::collect(),
      data = fetch_assemble_data_userAsmb(),
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

    observeEvent(input$date_filter, {
      req(rv$data)
      trigger("update_assemble_table")
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    # Refresh ----
    init("refresh_assemble")
    on("refresh_assemble", {
      rv$data <- fetch_assemble_data_userAsmb()
      updateReactable(
        "table",
        data = filtered_data()
      )
    })

    # Column-group / status filters. Mirror the pickers so NULL (= user cleared
    # all) is distinguishable from the pre-init state. Defaults: everything on.
    col_groups_rv <- reactiveVal(names(ASSEMBLE_COL_GROUPS_USERASMB))
    observeEvent(input$col_groups, {
      col_groups_rv(input$col_groups %||% character(0))
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    lock_filter_rv <- reactiveVal(unname(ASSEMBLE_LOCK_CHOICES))
    observeEvent(input$lock_filter, {
      lock_filter_rv(input$lock_filter %||% character(0))
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
    state_filter_rv <- reactiveVal(unname(ASSEMBLE_STATE_CHOICES))
    observeEvent(input$state_filter, {
      state_filter_rv(input$state_filter %||% character(0))
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    # CSS class for a togglable column, added to both body cell and header so
    # the whole column collapses when its group is unselected.
    .grp <- function(col) {
      g <- ASSEMBLE_COL_GROUP_LOOKUP_USERASMB[col]
      if (is.na(g)) NULL else paste0("mp-grp-", g)
    }

    # No-raw-data projects have no reads/coverage, so hide the read-derived
    # columns (preprocess opts, read count, read length) entirely.
    no_raw <- isTRUE(session$userData$no_raw_data)

    # Hide unselected column groups and lock/state codes via CSS. Hiding (not
    # removing) keeps columns/rows mounted, so filters, sort, page, and
    # selection survive toggling.
    output$col_css <- renderUI({
      hidden_grp   <- setdiff(names(ASSEMBLE_COL_GROUPS_USERASMB), col_groups_rv())
      hidden_lock  <- setdiff(unname(ASSEMBLE_LOCK_CHOICES), lock_filter_rv())
      hidden_state <- setdiff(unname(ASSEMBLE_STATE_CHOICES), state_filter_rv())
      # Scope to THIS module's table so rules don't hit the shared mp-lock /
      # mp-state / mp-grp classes on the annotate, export, and assemble tables.
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
          theme = reactable::reactableTheme(
            headerStyle = list(whiteSpace = "normal", lineHeight = "1.2", textAlign = "left")
          ),
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
            topology = colDef(
              show = TRUE, class = .grp("topology"), headerClass = .grp("topology"),
              minWidth = 140,
              name = "Topology"
            ),
            assembly = colDef(
              show = TRUE,
              minWidth = 140,
              name = "Input Assembly File",
              html = T,
              cell = rt_longtext()
            ),
            pre_opts = colDef(
              show = !no_raw, class = .grp("pre_opts"), headerClass = .grp("pre_opts"),
              name = "Preprocess Opts.",
              html = T,
              width = 130,
              cell = rt_link(ns("set_pre_opts"))
            ),
            find_mito_opts = colDef(
              show = TRUE, class = .grp("find_mito_opts"), headerClass = .grp("find_mito_opts"),
              name = "Find Mito Opts.",
              html = T,
              width = 140,
              cell = rt_link(ns("set_find_mito_opts"))
            ),
            # The note doubles as the link to the search evidence.
            find_mito_notes = colDef(
              show = TRUE, class = .grp("find_mito_notes"), headerClass = .grp("find_mito_notes"),
              name = "Mito Search",
              minWidth = 180,
              html = T,
              cell = rt_link(ns("show_mito_candidates"))
            ),
            circularize_opts = colDef(
              show = TRUE, class = .grp("circularize_opts"), headerClass = .grp("circularize_opts"),
              name = "Circularize Opts.",
              html = T,
              width = 140,
              cell = rt_link(ns("set_circularize_opts"))
            ),
            # The note doubles as the link to the circularization evidence.
            circularize_notes = colDef(
              show = TRUE, class = .grp("circularize_notes"), headerClass = .grp("circularize_notes"),
              name = "Circularization",
              minWidth = 160,
              html = T,
              cell = rt_link(ns("show_circularize_details"))
            ),
            join_notes = colDef(
              show = TRUE, class = .grp("join_notes"), headerClass = .grp("join_notes"),
              name = "Scaffold Join Notes",
              html = TRUE,
              align = "left",
              minWidth = 150,
              cell = rt_longtext()
            ),
            blast_opts = colDef(
              show = T, class = .grp("blast_opts"), headerClass = .grp("blast_opts"),
              name = "BLAST Opts.",
              html = T,
              width = 120,
              cell = rt_link(ns("set_blast_opts"))
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
            scaffolds = colDef(
              show = TRUE, class = .grp("scaffolds"), headerClass = .grp("scaffolds"),
              width = 100, name = "# Scaffolds", align = "center"
            ),
            blast_accession = colDef(
              show = TRUE, class = .grp("blast_accession"), headerClass = .grp("blast_accession"),
              name = "Top Hit",
              html = TRUE,
              width = 120,
              cell = rt_ncbi_link()
            ),
            poor_blast_ref = colDef(show = FALSE),
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
            choices = c("Pre-Coverage (wait)" = 0, 
             "Ready to Calculate Coverage" = 1,
             "In Progress" = 4, 
             "Successful Coverage Calculation" = 2, 
             "Failed Coverage Calculation" = 3),
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
      # Locking advances every non-ignored contig of the sample. Each contig is
      # its own annotation unit and was seeded its own annotate row by WF1, so a
      # fragmented user assembly no longer has to be reduced to one contig.
      lock_current <- as.numeric(names(which.max(table(rv$updating$assemble_lock))))
      rv$updating$assemble_lock <- as.numeric(!lock_current)
      if (lock_current == 0) {
        # A locked sample is never admitted by WF1 (its query requires
        # assemble_lock = 0), so a pending join redo could never run and the
        # flag would sit at 1 forever, keeping the Update modal reporting work
        # that cannot be done. Locking resolves it.
        rv$updating$join_switch <- NA_integer_
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
      # One click can now hand several contigs to annotation, so say how many.
      # The lock itself is still per sample; the units are what WF2 will run.
      if (lock_current == 0) {
        n_units <- dplyr::tbl(session$userData$con, "assemblies") |>
          dplyr::filter(ignore == 0 & ID %in% !!rv$updating$ID) |>
          dplyr::count() |>
          dplyr::pull(n)
        shiny::showNotification(
          paste0(
            "Locked ", nrow(rv$updating),
            ngettext(nrow(rv$updating), " sample", " samples"),
            ": ", n_units, ngettext(n_units, " contig", " contigs"),
            " will be annotated."
          ),
          type = "message",
          duration = 5
        )
      }
      trigger("update_assemble_table")
      trigger("refresh_annotate")
      trigger("refresh_export")
    })

    # Redo Scaffold Join ----
    # Narrower than Set State: only queues the join (join_switch = 1),
    # assemble_switch is left alone so this never re-enters assembly.
    init("redo_join")
    on("redo_join", {
      req(session$userData$mode == "Assemble")
      req(selected())
      req(all(rv$data$assemble_lock[req(selected())] == 0))
      ids <- unique(rv$data$ID[selected()])

      asmb <- dplyr::tbl(session$userData$con, "assemblies") |>
        dplyr::filter(ID %in% ids, path > 0) |>
        dplyr::select(ID, path, scaffold) |>
        dplyr::collect()
      stale <- tryCatch(
        stale_assemble_dirs(
          session$userData$con,
          session$userData$dir_out,
          ids = ids,
          pending_only = FALSE
        ),
        error = function(e) NULL
      )
      missing_ids <- if (!is.null(stale)) stale$ID else character(0)
      # Scaffold-join toggle, per sample, via its assembly parameter set. A redo
      # on a toggled-off sample would run the join, get "skipped" back, and
      # finalise the sample as done, erasing a state-3 failure. Refused here.
      toggles <- tryCatch(
        dplyr::tbl(session$userData$con, "assemble") |>
          dplyr::filter(ID %in% ids) |>
          dplyr::select(ID, assemble_opts, blast_accession) |>
          dplyr::inner_join(
            dplyr::tbl(session$userData$con, "assemble_opts") |>
              dplyr::select(assemble_opts, join_scaffolds),
            by = "assemble_opts"
          ) |>
          dplyr::collect(),
        error = function(e) NULL
      )
      join_off_ids <- if (is.null(toggles) || nrow(toggles) == 0L) {
        character(0)
      } else {
        off <- is.na(toggles$join_scaffolds) | toggles$join_scaffolds == 0
        unique(toggles$ID[off])
      }
      # No reference accession means the join has nothing to align against. A
      # fragmented sample with BLAST off sits legitimately at state 2, and a
      # redo would report a missing input and mark it failed. Read the accession
      # from the database, not rv$data: the table blanks it for display whenever
      # a sample keeps more than one scaffold, which is every join-eligible
      # sample.
      no_ref_ids <- redo_join_no_ref_ids(ids, toggles)
      plan <- redo_join_plan(ids, asmb, missing_ids, join_off_ids, no_ref_ids)

      if (length(plan$not_eligible) > 0 || length(plan$missing_output) > 0 ||
          length(plan$join_off) > 0 || length(plan$no_ref) > 0) {
        shinyWidgets::sendSweetAlert(
          title = "Redo scaffold join not queued for some samples",
          text = shiny::tags$div(
            if (length(plan$not_eligible) > 0) {
              shiny::tags$p(
                "Not join-eligible (need exactly one assembler path fragmented ",
                "into more than one scaffold): ",
                tags$code(paste(plan$not_eligible, collapse = ", "))
              )
            },
            if (length(plan$missing_output) > 0) {
              shiny::tags$div(
                shiny::tags$p(
                  "Published assembly output is not on disk for a redo to use:"
                ),
                shiny::tags$ul(stale_assemble_items(
                  stale[stale$ID %in% plan$missing_output, , drop = FALSE]
                ))
              )
            },
            if (length(plan$join_off) > 0) {
              shiny::tags$p(
                "Scaffold joining is switched off in the assembly parameter ",
                "set, so a redo would report the sample as skipped and mark it ",
                "done. Turn 'join_scaffolds' on first for: ",
                tags$code(paste(plan$join_off, collapse = ", "))
              )
            },
            if (length(plan$no_ref) > 0) {
              shiny::tags$p(
                "No BLAST reference was ever selected, so the join has nothing ",
                "to align the scaffolds against. Set a reference (or run BLAST) ",
                "first for: ",
                tags$code(paste(plan$no_ref, collapse = ", "))
              )
            },
            shiny::tags$p(
              if (length(plan$ready) > 0) {
                "The rest of the selected samples were queued for a join redo."
              } else {
                "No samples were queued."
              }
            )
          ),
          html = TRUE,
          type = if (length(plan$ready) > 0) "warning" else "error"
        )
      }
      req(length(plan$ready) > 0)

      upd <- data.frame(ID = plan$ready, join_switch = 1L, stringsAsFactors = FALSE)
      dplyr::tbl(session$userData$con, "assemble") |>
        dplyr::rows_update(
          upd,
          unmatched = "ignore",
          in_place = TRUE,
          copy = TRUE,
          by = "ID"
        )
      rv$data <- rv$data |> dplyr::rows_update(upd, by = "ID")
      trigger("update_assemble_table")
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

    # Set Mitogenome Search Opts ----
    opts_modal_server(
      rv, "find_mito_opts",
      fields = c("find_mitogenome", "find_mitofinder_db", "find_min_contig_length",
                 "find_min_identity", "find_min_aligned_length",
                 "find_min_aligned_fraction", "find_max_candidates",
                 "find_min_genes", "find_opts_cpus", "find_opts_memory"),
      label = "mitogenome search options",
      modal = find_mito_opts_modal,
      save = function() {
        # A search with no reference database can never confirm anything, so
        # refuse to save that combination rather than fail mid-run.
        db_path <- trimws(input$find_mitofinder_db %||% "")
        if (isTRUE(input$find_mitogenome) && (!nzchar(db_path) || !file.exists(db_path))) {
          shinyWidgets::sendSweetAlert(
            title = "MitoFinder database not found",
            text = paste0(
              "The mitogenome search confirms candidates with MitoFinder, which needs ",
              "a GenBank reference database. Build one for your clade with ",
              "custom_assembly_db(db_type = \"mitofinder\") and enter the path to its ",
              ".gb file."
            ),
            type = "error"
          )
          req(F)
        }
        dplyr::tbl(session$userData$con, "find_mito_opts") |>
          dplyr::rows_upsert(
            data.frame(
              find_mito_opts       = req(input$find_mito_opts),
              attempt              = as.integer(isTRUE(input$find_mitogenome)),
              mitofinder_db        = db_path,
              min_contig_length    = as.integer(input$find_min_contig_length %||% 500L),
              min_identity         = as.numeric(input$find_min_identity %||% 70),
              min_aligned_length   = as.integer(input$find_min_aligned_length %||% 300L),
              min_aligned_fraction = as.numeric(input$find_min_aligned_fraction %||% 0.5),
              max_candidates       = as.integer(input$find_max_candidates %||% 20L),
              min_genes            = as.integer(input$find_min_genes %||% 3L),
              cpus                 = as.integer(input$find_opts_cpus %||% 4L),
              memory               = as.integer(input$find_opts_memory %||% 8L)
            ),
            in_place = TRUE,
            copy = TRUE,
            by = "find_mito_opts"
          )
        rv$find_mito_opts <- dplyr::tbl(session$userData$con, "find_mito_opts") |>
          dplyr::collect()
      },
      input = input, session = session, selected = selected
    )
    observeEvent(input$show_mito_candidates, {
      mito_candidates_modal(rv$data$ID[as.numeric(input$show_mito_candidates)])
    })
    observeEvent(input$show_circularize_details, {
      circularize_details_modal(
        rv, rv$data$ID[as.numeric(input$show_circularize_details)]
      )
    })

    # Paging between contigs: the selection lives in rv, so the body and both
    # plots follow it.
    observeEvent(input$circ_contig, {
      req(input$circ_contig %in% as.character(rv$circ_overlaps$contig))
      rv$circ_contig <- input$circ_contig
      circularize_load_evidence(rv, session = session)
    })

    output$circ_body <- renderUI({
      ev <- req(rv$circ_evidence)
      circularize_details_body(ev$overlap, ev$depth, session$ns)
    })

    output$circ_schematic <- renderPlot({
      ev <- req(rv$circ_evidence)$overlap
      blocks <- data.frame(
        xmin = c(ev$qstart, ev$sstart), xmax = c(ev$qend, ev$send),
        label = factor(c("5' end", "3' end"), levels = c("5' end", "3' end"))
      )
      ggplot2::ggplot() +
        ggplot2::geom_rect(
          ggplot2::aes(xmin = 1, xmax = ev$send, ymin = 0.45, ymax = 0.55),
          fill = "grey85"
        ) +
        ggplot2::geom_rect(
          data = blocks,
          ggplot2::aes(xmin = xmin, xmax = xmax, ymin = 0.3, ymax = 0.7,
                       fill = label)
        ) +
        ggplot2::scale_fill_manual(values = c("5' end" = "#0056b3",
                                              "3' end" = "#FF6670"),
                                   name = NULL) +
        ggplot2::scale_y_continuous(limits = c(0, 1), breaks = NULL) +
        ggplot2::labs(x = "contig position (bp)", y = NULL) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(legend.position = "bottom",
                       panel.grid.major.y = ggplot2::element_blank())
    })

    output$circ_depth <- renderPlot({
      ev <- req(rv$circ_evidence)
      d <- ev$depth
      req(nrow(d) > 0)
      long <- rbind(
        data.frame(rel_position = d$rel_position, depth = d$depth,
                   track = "assembly depth"),
        data.frame(rel_position = d$rel_position, depth = d$depth_spanning,
                   track = "crosses the seam")
      )
      ggplot2::ggplot(long, ggplot2::aes(x = rel_position, y = depth,
                                         color = track)) +
        ggplot2::annotate("rect",
                          xmin = -ev$overlap$min_overhang,
                          xmax = ev$overlap$min_overhang,
                          ymin = 0, ymax = Inf, alpha = 0.12, fill = "#0056b3") +
        ggplot2::geom_vline(xintercept = 0, linetype = "dashed",
                            color = "grey40") +
        ggplot2::geom_line() +
        ggplot2::scale_color_manual(values = c("assembly depth" = "grey50",
                                               "crosses the seam" = "#0056b3"),
                                    name = NULL) +
        ggplot2::labs(
          x = "bases from the seam (negative = 3' end, positive = 5' end)",
          y = "depth"
        ) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(legend.position = "bottom")
    })

    observeEvent(input$find_mito_opts, ignoreInit = T, {
      exists <- input$find_mito_opts %in% rv$find_mito_opts$find_mito_opts
      shinyWidgets::updatePrettyCheckbox(
        inputId = "edit_find_mito_opts",
        value = !exists
      )
      if (exists) {
        cur <- rv$find_mito_opts[
          rv$find_mito_opts$find_mito_opts == input$find_mito_opts,
        ]
        shinyWidgets::updatePrettyCheckbox(
          inputId = "find_mitogenome",
          value = isTRUE(as.logical(cur$attempt %||% 0L))
        )
        updateTextInput(inputId = "find_mitofinder_db", value = cur$mitofinder_db %||% "")
        updateNumericInput(inputId = "find_min_contig_length", value = cur$min_contig_length)
        updateNumericInput(inputId = "find_min_identity", value = cur$min_identity)
        updateNumericInput(inputId = "find_min_aligned_length", value = cur$min_aligned_length)
        updateNumericInput(inputId = "find_min_aligned_fraction", value = cur$min_aligned_fraction)
        updateNumericInput(inputId = "find_max_candidates", value = cur$max_candidates)
        updateNumericInput(inputId = "find_min_genes", value = cur$min_genes)
        updateNumericInput(inputId = "find_opts_cpus", value = cur$cpus)
        updateNumericInput(inputId = "find_opts_memory", value = cur$memory)
        shinyjs::toggle(
          id = "find_mito_params_group",
          condition = isTRUE(as.logical(cur$attempt %||% 0L))
        )
      }
    })
    # Parameters are meaningless with the search switched off
    observeEvent(input$find_mitogenome, ignoreInit = T, {
      shinyjs::toggle(
        id = "find_mito_params_group",
        condition = isTRUE(input$find_mitogenome)
      )
    })

    # Set Circularization Opts ----
    opts_modal_server(
      rv, "circularize_opts",
      fields = c("attempt_circularization", "circ_min_overlap", "circ_min_identity",
                 "circ_min_junction_reads", "circ_min_overhang",
                 "circ_opts_cpus", "circ_opts_memory"),
      label = "circularization options",
      modal = circularize_opts_modal,
      save = function() {
        # The read-based fields are absent from the modal in a no-raw-data
        # project, so fall back to the stored values rather than writing NULL.
        cur <- rv$circularize_opts[
          rv$circularize_opts$circularize_opts == input$circularize_opts,
        ]
        dplyr::tbl(session$userData$con, "circularize_opts") |>
          dplyr::rows_upsert(
            data.frame(
              circularize_opts   = req(input$circularize_opts),
              attempt            = as.integer(isTRUE(input$attempt_circularization)),
              min_overlap        = as.integer(input$circ_min_overlap %||% 220L),
              min_identity       = as.numeric(input$circ_min_identity %||% 99),
              min_junction_reads = as.integer(
                input$circ_min_junction_reads %||% cur$min_junction_reads %||% 5L
              ),
              min_overhang       = as.integer(
                input$circ_min_overhang %||% cur$min_overhang %||% 30L
              ),
              cpus               = as.integer(input$circ_opts_cpus %||% 4L),
              memory             = as.integer(input$circ_opts_memory %||% 8L)
            ),
            in_place = TRUE,
            copy = TRUE,
            by = "circularize_opts"
          )
        rv$circularize_opts <- dplyr::tbl(session$userData$con, "circularize_opts") |>
          dplyr::collect()
      },
      input = input, session = session, selected = selected
    )
    observeEvent(input$circularize_opts, ignoreInit = T, {
      exists <- input$circularize_opts %in% rv$circularize_opts$circularize_opts
      shinyWidgets::updatePrettyCheckbox(
        inputId = "edit_circularize_opts",
        value = !exists
      )
      if (exists) {
        cur <- rv$circularize_opts[
          rv$circularize_opts$circularize_opts == input$circularize_opts,
        ]
        shinyWidgets::updatePrettyCheckbox(
          inputId = "attempt_circularization",
          value = isTRUE(as.logical(cur$attempt %||% 0L))
        )
        updateNumericInput(inputId = "circ_min_overlap", value = cur$min_overlap)
        updateNumericInput(inputId = "circ_min_identity", value = cur$min_identity)
        updateNumericInput(inputId = "circ_min_junction_reads", value = cur$min_junction_reads)
        updateNumericInput(inputId = "circ_min_overhang", value = cur$min_overhang)
        updateNumericInput(inputId = "circ_opts_cpus", value = cur$cpus)
        updateNumericInput(inputId = "circ_opts_memory", value = cur$memory)
        shinyjs::toggle(
          id = "circ_params_group",
          condition = isTRUE(as.logical(cur$attempt %||% 0L))
        )
      }
    })
    # Thresholds and resources are meaningless with the step switched off
    observeEvent(input$attempt_circularization, ignoreInit = T, {
      shinyjs::toggle(
        id = "circ_params_group",
        condition = isTRUE(input$attempt_circularization)
      )
    })

    # Set BLAST Opts ----
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
    opts_modal_server(
      rv, "blast_opts",
      fields = c("run_blast", "taxids", "remote_blast", "remote_fallback",
                 "entrez_query", "max_target_seqs", "extra_opts"),
      label = "BLAST options",
      modal = blast_opts_modal,
      save = function() {
        # Numeric NCBI taxon IDs only; validated here, with no network lookup, so
        # the save path keeps working offline.
        taxids <- paste(
          trimws(strsplit(trimws(input$taxids %||% ""), ",")[[1]]),
          collapse = ","
        )
        if (nzchar(taxids) && !grepl("^[0-9]+(,[0-9]+)*$", taxids)) {
          shinyWidgets::sendSweetAlert(
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
      },
      input = input, session = session, selected = selected
    )

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
  })
}
