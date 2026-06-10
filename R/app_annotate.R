# Togglable column groups for the Annotate table. Cols not listed here
# (sticky cols, action buttons) are always shown.
ANNOTATE_COL_GROUPS <- list(
  Options  = c("annotate_opts", "curate_opts", "orf_opts"),
  Stats    = c("length_raw", "length", "topology", "scaffolds"),
  BLAST    = c("blast_ref_status", "blast_accession", "blast_species",
               "blast_lineage", "blast_pident", "blast_qcovs"),
  Counts   = c("PCGCount", "tRNACount", "rRNACount", "missing", "extra"),
  Review   = c("ID_verified", "reviewed", "problematic", "warnings"),
  Metadata = c("time_stamp", "annotate_notes")
)
ANNOTATE_COL_GROUP_LOOKUP <- {
  out <- character()
  for (.g in names(ANNOTATE_COL_GROUPS)) {
    for (.c in ANNOTATE_COL_GROUPS[[.g]]) out[.c] <- .g
  }
  out
}

# Status-filter pickers. Values match the annotate_lock / annotate_switch
# codes; each row is tagged with mp-lock-<v> / mp-state-<v> classes so
# unselected codes can be hidden via CSS (same mechanism as the column
# picker, so sort order, search, other filters, page, and selection survive).
ANNOTATE_LOCK_CHOICES <- c("Unlocked" = "0", "Locked" = "1")
ANNOTATE_STATE_CHOICES <- c(
  "Pre-Annotate" = "0",
  "In Progress"  = "1",
  "Success"      = "2",
  "Failed"       = "3"
)

#' annotate UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
annotate_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tagList(
      uiOutput(ns("col_css")),
      div(
        style = "display: flex; align-items: center; gap: 20px; flex-wrap: wrap;",
        shinyWidgets::pickerInput(
          inputId  = ns("lock_filter"),
          label    = "Lock:",
          choices  = ANNOTATE_LOCK_CHOICES,
          selected = ANNOTATE_LOCK_CHOICES,
          multiple = TRUE,
          options  = list(
            `actions-box`          = TRUE,
            `select-all-text`      = "All",
            `deselect-all-text`    = "None",
            `selected-text-format` = "count > 0",
            width                  = "140px"
          ),
          inline = TRUE
        ),
        shinyWidgets::pickerInput(
          inputId  = ns("state_filter"),
          label    = "State:",
          choices  = ANNOTATE_STATE_CHOICES,
          selected = ANNOTATE_STATE_CHOICES,
          multiple = TRUE,
          options  = list(
            `actions-box`          = TRUE,
            `select-all-text`      = "All",
            `deselect-all-text`    = "None",
            `selected-text-format` = "count > 0",
            width                  = "140px"
          ),
          inline = TRUE
        ),
        shinyWidgets::pickerInput(
          inputId  = ns("col_groups"),
          label    = "Show columns:",
          choices  = names(ANNOTATE_COL_GROUPS),
          selected = names(ANNOTATE_COL_GROUPS),
          multiple = TRUE,
          options  = list(
            `actions-box`          = TRUE,
            `select-all-text`      = "All",
            `deselect-all-text`    = "None",
            `selected-text-format` = "count > 0",
            width                  = "150px"
          ),
          inline = TRUE
        ),
        uiOutput(ns("warnings_select"))
      ),
      reactable::reactableOutput(ns("table")),
      div(
        style = "margin-top: 12px; display: flex; gap: 8px;",
        downloadButton(ns("export_selected"), "Export Selected to CSV",
                       class = "btn-sm btn-default"),
        downloadButton(ns("export_all"), "Export All to CSV",
                       class = "btn-sm btn-default")
      )
    )
  )
}

#' annotate Server Functions
#'
#' @noRd
annotate_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Help-doc icons (one observer per tool, registered once at module init).
    reopen_annotate <- function() annotate_opts_modal(rv)
    register_tool_help("mitos", input, reopen = reopen_annotate)
    register_tool_help("trnaScan-SE", input, reopen = reopen_annotate)
    register_tool_help("arwen", input, reopen = reopen_annotate)
    register_tool_help("aragorn", input, reopen = reopen_annotate)
    register_tool_help("orffinder", input, reopen = reopen_annotate)

    # Prepare data ----
    rv <- reactiveValues(
      curate_opts = dplyr::tbl(session$userData$con, "curate_opts") |>
        dplyr::collect(),
      annotate_opts = dplyr::tbl(session$userData$con, "annotate_opts") |>
        dplyr::collect(),
      orf_opts = dplyr::tbl(session$userData$con, "orf_opts") |>
        dplyr::collect(),
      data = fetch_annotate_data(),
      updating = NULL
    )

    # Refresh ----
    init("refresh_annotate")
    on("refresh_annotate", {
      rv$data <- fetch_annotate_data()
      updateReactable(
        "table",
        data = filtered_data()
      )
    })

    # Render dynamic footer with checkboxes
    output$warnings_select <- renderUI({
      req(rv$data)

      # Extract and flatten warnings_details column contains values delimited by semicolon
      warn_vals <- rv$data$warnings_details |>
        na.omit() |>
        strsplit(split = ";\\s*") |>
        unlist() |>
        unique() |>
        sort()

      # Use pickerInput to create a dropdown with checkboxes
      shinyWidgets::pickerInput(
        inputId = ns("warning_filters"),
        label = "Warnings column includes:",
        choices = warn_vals,
        selected = isolate(input$warning_filters) %||% warn_vals, # default to all selected
        multiple = TRUE, # enable multi-select
        options = list(
          `actions-box` = TRUE, # Display checkboxes in dropdown
          `selected-text-format` = "count > 0" # Show the number of selected items when more than 0 are selected
        ),
        inline = TRUE # Optional: puts it on the same line
      )
    })

    # Reactive filtered data
    filtered_data <- reactive({
      req(rv$data)
      selected <- input$warning_filters
      rv$data |> dplyr::mutate(
        warnings = purrr::map_int(warnings_details, function(wd) {
          # if (is.na(wd) || length(selected) == 0) return(0)
          wd_list <- strsplit(as.character(wd), ";")[[1]] |>
            trimws()
          sum(wd_list %in% selected)
        })
      )
    })

    # Mirror the column-group picker so NULL (= user cleared all) is
    # distinguishable from the pre-init state. Default: all groups on.
    col_groups_rv <- reactiveVal(names(ANNOTATE_COL_GROUPS))
    observeEvent(input$col_groups, {
      col_groups_rv(input$col_groups %||% character(0))
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    # Mirror the lock / state status pickers the same way, so clearing all
    # is distinguishable from the pre-init state. Default: all codes shown.
    lock_filter_rv <- reactiveVal(unname(ANNOTATE_LOCK_CHOICES))
    observeEvent(input$lock_filter, {
      lock_filter_rv(input$lock_filter %||% character(0))
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
    state_filter_rv <- reactiveVal(unname(ANNOTATE_STATE_CHOICES))
    observeEvent(input$state_filter, {
      state_filter_rv(input$state_filter %||% character(0))
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    # CSS class for a togglable column. Added to both body cells and the
    # header cell so the whole column collapses when the group is hidden.
    .grp <- function(col) {
      g <- ANNOTATE_COL_GROUP_LOOKUP[col]
      if (is.na(g)) NULL else paste0("mp-grp-", g)
    }

    # Inject a <style> tag that display:nones unselected column groups.
    # Hiding via CSS keeps columns mounted, so filters, sort, page, and
    # selection survive toggling.
    # Also hide rows whose lock / state code is unselected. Rows are tagged
    # with mp-lock-<v> / mp-state-<v> classes (see rowClass below); hiding via
    # CSS keeps every row mounted, so sort, search, other column filters,
    # page, and selection all survive toggling.
    output$col_css <- renderUI({
      hidden_grp   <- setdiff(names(ANNOTATE_COL_GROUPS), col_groups_rv())
      hidden_lock  <- setdiff(unname(ANNOTATE_LOCK_CHOICES), lock_filter_rv())
      hidden_state <- setdiff(unname(ANNOTATE_STATE_CHOICES), state_filter_rv())
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
      # isolate(req(rv$data)) |>
      # req(filtered_data())
      reactable(
        data = isolate(filtered_data()),
        compact = TRUE,
        striped = TRUE,
        language = reactable::reactableLang(
          noData = "No Completed / Locked Assemblies Found"
        ),
        defaultPageSize = 100,
        resizable = TRUE,
        showPageSizeOptions = TRUE,
        onClick = "select",
        selection = "multiple",
        searchable = TRUE,
        filterable = TRUE,
        defaultSorted = list(time_stamp = "desc"),
        height = 500,
        wrap = FALSE,
        pageSizeOptions = c(25, 50, 100, 200, 500),
        rowStyle = rt_highlight_row(),
        rowClass = JS("function(rowInfo) {
          if (!rowInfo || !rowInfo.values) return '';
          return 'mp-lock-' + rowInfo.values['annotate_lock'] +
                 ' mp-state-' + rowInfo.values['annotate_switch'];
        }"),
        defaultColDef = colDef(align = "left", show = FALSE),
        columns = list(
          `.selection` = colDef(show = T, sticky = "left", width = 28),
          annotate_lock = colDef(
            show = TRUE,
            sticky = "left",
            name = "",
            html = TRUE,
            filterable = FALSE,
            width = 32,
            align = "center",
            cell = rt_dynamicIcon(
              c(
                `0` = "fa fa-lock-open",
                `1` = "fa fa-lock"
              )
            )
          ),
          annotate_switch = colDef(
            show = TRUE,
            sticky = "left",
            name = "",
            html = TRUE,
            filterable = FALSE,
            width = 30,
            align = "center",
            cell = rt_dynamicIcon(
              c(
                `0` = "fa fa-hourglass",
                `1` = "fa fa-person-running",
                `2` = "fa fa-circle-check",
                `3` = "fa fa-triangle-exclamation"
              )
            )
          ),
          ID = colDef(
            show = TRUE,
            minWidth = 120,
            sticky = "left",
            html = TRUE,
            cell = rt_longtext()
          ),
          Taxon = colDef(
            show = TRUE,
            minWidth = 140,
            sticky = "left",
            html = TRUE,
            cell = rt_longtext()
          ),
          ID_verified = colDef(
            show = TRUE, class = .grp("ID_verified"), headerClass = .grp("ID_verified"),
            name = "ID Verified",
            html = TRUE,
            align = "center",
            width = 100,
            cell = rt_bool_badge()
          ),
          annotate_opts = colDef(
            show = TRUE, class = .grp("annotate_opts"), headerClass = .grp("annotate_opts"),
            name = "Annotate Opts.",
            html = TRUE,
            width = 130,
            cell = rt_link(ns("set_annotate_opts"))
          ),
          curate_opts = colDef(
            show = TRUE, class = .grp("curate_opts"), headerClass = .grp("curate_opts"),
            name = "Curate Opts.",
            html = TRUE,
            width = 110,
            cell = rt_link(ns("set_curate_opts"))
          ),
          orf_opts = colDef(
            show = TRUE, class = .grp("orf_opts"), headerClass = .grp("orf_opts"),
            name = "ORF Opts.",
            html = TRUE,
            width = 110,
            cell = rt_link(ns("set_orf_opts"))
          ),
          length_raw = colDef(
            show = TRUE, class = .grp("length_raw"), headerClass = .grp("length_raw"),
            name = "Asmb. Length (raw)",
            filterable = FALSE,
            html = TRUE,
            cell = rt_longtext()
          ),
          length = colDef(
            show = TRUE, class = .grp("length"), headerClass = .grp("length"),
            name = "Asmb. Length (trimmed)",
            filterable = FALSE,
            html = TRUE,
            cell = rt_longtext()
          ),
          topology = colDef(show = TRUE, class = .grp("topology"), headerClass = .grp("topology"), name = "Topology", align = "center"),
          scaffolds = colDef(show = TRUE, class = .grp("scaffolds"), headerClass = .grp("scaffolds"), name = "Scaffolds", align = "center"),
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
          blast_accession = colDef(
            show = TRUE, class = .grp("blast_accession"), headerClass = .grp("blast_accession"),
            name = "BLAST Top Hit",
            html = TRUE,
            width = 120,
            cell = rt_ncbi_link()
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
          PCGCount = colDef(show = TRUE, class = .grp("PCGCount"), headerClass = .grp("PCGCount"), name = "# PCGs", align = "center"),
          tRNACount = colDef(show = TRUE, class = .grp("tRNACount"), headerClass = .grp("tRNACount"), name = "# tRNAs", align = "center"),
          rRNACount = colDef(show = TRUE, class = .grp("rRNACount"), headerClass = .grp("rRNACount"), name = "# rRNAs", align = "center"),
          missing = colDef(show = TRUE, class = .grp("missing"), headerClass = .grp("missing"), name = "Missing", align = "center", html = TRUE, cell = rt_longtext()),
          extra = colDef(show = TRUE, class = .grp("extra"), headerClass = .grp("extra"), name = "Extra", align = "center", html = TRUE, cell = rt_longtext()),
          warnings = colDef(show = TRUE, class = .grp("warnings"), headerClass = .grp("warnings"), name = "Warnings", align = "center"),
          reviewed = colDef(
            show = TRUE, class = .grp("reviewed"), headerClass = .grp("reviewed"),
            name = "Reviewed",
            html = TRUE,
            align = "center",
            width = 100,
            cell = rt_bool_badge()
          ),
          problematic = colDef(
            show = TRUE, class = .grp("problematic"), headerClass = .grp("problematic"),
            name = "Problematic",
            html = TRUE,
            align = "center",
            width = 100,
            cell = rt_bool_badge(invert = TRUE, hide_no = TRUE)
          ),
          time_stamp = colDef(
            show = TRUE, class = .grp("time_stamp"), headerClass = .grp("time_stamp"),
            name = "Last Updated",
            filterable = FALSE,
            html = T,
            width = 150,
            cell = rt_ts_date()
          ),
          annotate_notes = colDef(
            show = TRUE, class = .grp("annotate_notes"), headerClass = .grp("annotate_notes"),
            name = "Notes",
            html = TRUE,
            align = "left",
            minWidth = 150,
            # maxWidth = 400,
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

    # watch for changes to warnings filter and update table
    observeEvent(input$warning_filters,
      {
        reactable::updateReactable(
          "table",
          data = filtered_data(),
          page = reactable::getReactableState("table", "page"),
          selected = reactable::getReactableState("table", "selected")
        )
      },
      ignoreNULL = FALSE
    )

    # update table ----
    init("update_annotate_table")
    on("update_annotate_table", {
      reactable::updateReactable(
        "table",
        # data = rv$data |>
        data = filtered_data() |>
          dplyr::mutate(
            output = dplyr::case_when(
              annotate_switch > 1 ~ "output",
              .default = NA_character_
            ),
            view = dplyr::case_when(
              annotate_switch > 1 ~ "details",
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
      req(session$userData$mode == "Annotate")
      req(selected())
      req(all(filtered_data()$annotate_lock[req(selected())] == 0))
      rv$updating <- filtered_data() |>
        dplyr::select(ID, annotate_switch) |>
        dplyr::slice(selected())
      current <- character(0)
      if (length(unique(rv$updating$annotate_switch)) == 1) {
        current <- rv$updating$annotate_switch[1]
      }
      showModal(
        modalDialog(
          title = "Select New State:",
          shinyWidgets::prettyRadioButtons(
            ns("new_state"),
            label = NULL,
            choices = c("Pre-Annotate (wait)" = 0, "Ready to Annotate" = 1, "Successful Annotation" = 2),
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
      rv$updating$annotate_switch <- as.numeric(input$new_state)
      dplyr::tbl(session$userData$con, "annotate") |>
        dplyr::rows_update(
          rv$updating,
          unmatched = "ignore",
          in_place = TRUE,
          copy = TRUE,
          by = "ID"
        )
      rv$data <- filtered_data() |>
        dplyr::rows_update(
          rv$updating,
          by = "ID"
        )
      trigger("update_annotate_table")
      removeModal()
    })

    # Toggle lock ----
    init("lock")
    on("lock", {
      req(session$userData$mode == "Annotate")
      req(selected())
      rv$updating <- filtered_data() |>
        dplyr::select(ID, annotate_lock) |>
        dplyr::slice(selected())
      lock_current <- as.numeric(names(which.max(table(rv$updating$annotate_lock))))
      rv$updating$annotate_lock <- as.numeric(!lock_current)
      dplyr::tbl(session$userData$con, "annotate") |>
        dplyr::rows_update(
          rv$updating,
          unmatched = "ignore",
          in_place = TRUE,
          copy = TRUE,
          by = "ID"
        )
      rv$data <- filtered_data() |>
        dplyr::rows_update(rv$updating, by = "ID")
      trigger("update_annotate_table")
      trigger("refresh_export")
    })

    # Toggle ID_verified
    init("id_verified_top")
    on("id_verified_top", {
      req(session$userData$mode == "Annotate")
      req(selected())
      rv$updating <- filtered_data() |>
        dplyr::select(ID, ID_verified) |>
        dplyr::slice(selected())
      ID_current <- sort(unique(rv$updating$ID_verified))[1]
      if (is.na(ID_current)) {
        rv$updating$ID_verified <- "yes"
      } else if (ID_current == "yes") {
        rv$updating$ID_verified <- "no"
      } else if (ID_current == "no") {
        rv$updating$ID_verified <- "yes"
      }
      dplyr::tbl(session$userData$con, "annotate") |>
        dplyr::rows_update(
          rv$updating,
          unmatched = "ignore",
          in_place = TRUE,
          copy = TRUE,
          by = "ID"
        )
      rv$data <- filtered_data() |>
        dplyr::rows_update(rv$updating, by = "ID")
      trigger("update_annotate_table")
    })

    # Toggle problematic
    init("problematic_top")
    on("problematic_top", {
      req(session$userData$mode == "Annotate")
      req(selected())
      rv$updating <- filtered_data() |>
        dplyr::select(ID, problematic) |>
        dplyr::slice(selected())
      ID_current <- sort(unique(rv$updating$problematic))[1]
      if (is.na(ID_current)) {
        rv$updating$problematic <- "yes"
      } else {
        rv$updating$problematic <- NA_character_
      }
      dplyr::tbl(session$userData$con, "annotate") |>
        dplyr::rows_update(
          rv$updating,
          unmatched = "ignore",
          in_place = TRUE,
          copy = TRUE,
          by = "ID"
        )
      rv$data <- filtered_data() |>
        dplyr::rows_update(rv$updating, by = "ID")
      trigger("update_annotate_table")
    })

    # Set Annotate Options ----
    observeEvent(input$set_annotate_opts, {
      row <- as.numeric(input$set_annotate_opts)
      if (length(selected()) > 0 && !row %in% selected()) {
        req(F)
      } else {
        selected <- c(row, selected()) |> unique()
      }
      req(all(filtered_data()$annotate_lock[selected] == 0))
      rv$updating <- filtered_data() |> dplyr::slice(selected)
      rv$updating_indirect <- rv$updating |> dplyr::slice(0)
      annotate_opts_modal(rv)
    })
    observeEvent(input$annotate_opts, ignoreInit = T, {
      exists <- input$annotate_opts %in% rv$annotate_opts$annotate_opts
      shinyWidgets::updatePrettyCheckbox(
        inputId = "edit_annotate_opts",
        value = !exists
      )
      if (exists) {
        cur <- rv$annotate_opts[rv$annotate_opts$annotate_opts == input$annotate_opts, ]
        cur_params <- rv$curate_opts$params[rv$curate_opts$curate_opts == rv$updating$curate_opts[1]] |>
          jsonlite::fromJSON()

        updateNumericInput(
          inputId = "annotate_opts_cpus",
          value = cur$cpus
        )
        updateNumericInput(
          inputId = "annotate_opts_memory",
          value = cur$memory
        )
        updateTextAreaInput(
          inputId = "mitos_opts",
          value = cur$mitos_opts
        )
        updateSelectizeInput(
          inputId = "mitos_ref_dir",
          selected = cur$ref_dir,
          choices = unique(rv$annotate_opts$ref_dir),
          options = list(
            create = TRUE,
            maxItems = 1
          )
        )
        updateSelectizeInput(
          inputId = "mitos_ref_db",
          selected = cur$ref_db,
          # choices = unique(rv$annotate_opts$ref_db),
          choices = c("Metazoa_RefSeq89", "Chordata"),
          options = list(
            create = TRUE,
            maxItems = 1
          )
        )
        updateTextInput(
          inputId = "trnaScan_opts",
          value = cur$trnaScan_opts
        )
        shinyWidgets::updatePrettyCheckbox(
          inputId = "use_mitos_best",
          value = isTRUE(as.logical(cur$use_mitos_best %||% 1L))
        )
        shinyWidgets::updatePrettyCheckbox(
          inputId = "use_arwen",
          value = isTRUE(as.logical(cur$use_arwen))
        )
        updateTextInput(
          inputId = "arwen_opts",
          value = cur$arwen_opts
        )
        shinyWidgets::updatePrettyCheckbox(
          inputId = "use_aragorn",
          value = isTRUE(as.logical(cur$use_aragorn))
        )
        updateTextInput(
          inputId = "aragorn_opts",
          value = cur$aragorn_opts
        )
        shinyWidgets::updatePrettyCheckbox(
          inputId = "coverage_trim",
          value = isTRUE(as.logical(cur$coverage_trim %||% 1L))
        )
        shinyWidgets::updatePrettyCheckbox(
          inputId = "feature_trim",
          value = isTRUE(as.logical(cur$feature_trim %||% 1L))
        )
        shinyWidgets::updatePrettyCheckbox(
          inputId = "retain_low_conf_trna",
          value = isTRUE(as.logical(cur$retain_low_conf_trna %||% 0L))
        )
        updateSelectizeInput(
          inputId = "start_gene",
          choices = MITO_GENE_CHOICES,
          selected = cur$start_gene %||% character(0),
          options = list(
            create = TRUE,
            maxItems = 1
          )
        )
      }
    })
    observeEvent(input$edit_annotate_opts, ignoreInit = T, {
      shinyjs::toggleState("annotate_opts_cpus", condition = input$edit_annotate_opts)
      shinyjs::toggleState("annotate_opts_memory", condition = input$edit_annotate_opts)
      shinyjs::toggleState("mitos_opts", condition = input$edit_annotate_opts)
      shinyjs::toggleState("use_mitos_best", condition = input$edit_annotate_opts)
      # shinyjs::toggleState("mitos_ref_dir", condition = input$edit_annotate_opts) # TODO: custom / alt ref db for mitos
      shinyjs::toggleState("mitos_ref_db", condition = input$edit_annotate_opts)
      shinyjs::toggleState("trnaScan_opts", condition = input$edit_annotate_opts)
      shinyjs::toggleState("use_arwen", condition = input$edit_annotate_opts)
      shinyjs::toggleState("arwen_opts", condition = input$edit_annotate_opts)
      shinyjs::toggleState("use_aragorn", condition = input$edit_annotate_opts)
      shinyjs::toggleState("aragorn_opts", condition = input$edit_annotate_opts)
      shinyjs::toggleState("coverage_trim", condition = input$edit_annotate_opts)
      shinyjs::toggleState("feature_trim", condition = input$edit_annotate_opts)
      shinyjs::toggleState("retain_low_conf_trna", condition = input$edit_annotate_opts)
      shinyjs::toggleState("start_gene", condition = input$edit_annotate_opts)
      # Check if editing opts that apply beyond selection
      if (input$edit_annotate_opts && input$annotate_opts %in% filtered_data()$annotate_opts) {
        rv$updating_indirect <- filtered_data() |>
          dplyr::filter(annotate_opts == input$annotate_opts) |>
          dplyr::anti_join(rv$updating, by = "ID")
        # Prevent editing opts that apply to locked samples
        if (nrow(rv$updating_indirect) > 0L && any(rv$updating_indirect$annotate_lock == 1)) {
          shinyWidgets::sendSweetAlert(
            title = "Attempting to edit locked samples",
            text = "Processing parameters associated with locked samples can not be edited.",
            type = "warning"
          )
          shinyWidgets::updatePrettyCheckbox(
            inputId = "edit_annotate_opts",
            value = FALSE
          )
          req(F)
        }
        # Confirm editing opts that apply beyond selection
        if (nrow(rv$updating_indirect) > 0L) {
          shinyWidgets::confirmSweetAlert(
            inputId = "editing_annotate_opts_indirect",
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
    observeEvent(input$editing_annotate_opts_indirect, ignoreInit = T, {
      if (!input$editing_annotate_opts_indirect) {
        rv$updating_indirect <- rv$updating |> dplyr::slice(0)
        shinyWidgets::updatePrettyCheckbox(
          inputId = "edit_annotate_opts",
          value = FALSE
        )
      }
    })
    ## Save Changes ----
    observeEvent(input$update_annotate_opts, {
      ## Add to params table if new or editing ----
      if (input$edit_annotate_opts) {
        dplyr::tbl(session$userData$con, "annotate_opts") |>
          dplyr::rows_upsert(
            data.frame(
              annotate_opts = req(input$annotate_opts),
              cpus = req(input$annotate_opts_cpus),
              memory = req(input$annotate_opts_memory),
              mitos_opts = req(input$mitos_opts),
              use_mitos_best = as.integer(isTRUE(input$use_mitos_best)),
              ref_dir = req(input$mitos_ref_dir),
              ref_db = req(input$mitos_ref_db),
              trnaScan_opts = req(input$trnaScan_opts),
              arwen_opts = req(input$arwen_opts),
              use_arwen = as.integer(isTRUE(input$use_arwen)),
              aragorn_opts = req(input$aragorn_opts),
              use_aragorn = as.integer(isTRUE(input$use_aragorn)),
              start_gene = req(input$start_gene),
              coverage_trim = as.integer(isTRUE(input$coverage_trim)),
              feature_trim = as.integer(isTRUE(input$feature_trim)),
              retain_low_conf_trna = as.integer(isTRUE(input$retain_low_conf_trna))
            ),
            in_place = TRUE,
            copy = TRUE,
            by = "annotate_opts"
          )
        rv$annotate_opts <- dplyr::tbl(session$userData$con, "annotate_opts") |>
          dplyr::collect()
      }
      ## Update Annotate Table ----
      update <- data.frame(
        ID = c(rv$updating$ID, rv$updating_indirect$ID),
        annotate_opts = input$annotate_opts,
        annotate_switch = 1
      )
      dplyr::tbl(session$userData$con, "annotate") |>
        dplyr::rows_update(
          update,
          unmatched = "ignore",
          in_place = TRUE,
          copy = TRUE,
          by = "ID"
        )
      rv$data <- filtered_data() |>
        dplyr::rows_update(
          update,
          by = "ID"
        )
      rv$updating <- rv$updating_indirect <- NULL
      removeModal()
      trigger("update_annotate_table")
    })

    # Set Curate Options ----
    observeEvent(input$set_curate_opts, {
      row <- as.numeric(input$set_curate_opts)
      if (length(selected()) > 0 && !row %in% selected()) {
        req(F)
      } else {
        selected <- c(row, selected()) |> unique()
      }
      req(all(filtered_data()$annotate_lock[selected] == 0))
      rv$updating <- filtered_data() |> dplyr::slice(selected)
      rv$updating_indirect <- rv$updating |> dplyr::slice(0)
      curate_opts_modal(rv)
    })
    observeEvent(input$curate_opts, ignoreInit = T, {
      exists <- input$curate_opts %in% rv$curate_opts$curate_opts
      shinyWidgets::updatePrettyCheckbox(
        inputId = "edit_curate_opts",
        value = !exists
      )
      if (exists) {
        cur <- rv$curate_opts[rv$curate_opts$curate_opts == input$curate_opts, ]
        updateNumericInput(
          inputId = "curate_opts_cpus",
          value = cur$cpus
        )
        updateNumericInput(
          inputId = "curate_opts_memory",
          value = cur$memory
        )
        updateNumericInput(
          inputId = "max_blast_hits",
          value = cur$max_blast_hits
        )
        updateSelectizeInput(
          inputId = "target",
          selected = cur$target,
          options = list(
            create = TRUE,
            maxItems = 1
          )
        )
        updateSelectizeInput(
          inputId = "curate_ref_dir",
          selected = cur$ref_dir,
          choices = unique(rv$curate_opts$ref_dir),
          options = list(
            create = TRUE,
            maxItems = 1
          )
        )
        updateSelectizeInput(
          inputId = "curate_ref_db",
          selected = cur$ref_db,
          choices = c("Metazoa_RefSeq89", "Metazoa_RefSeq231", "Metazoa_RefSeq231_custom", "Chordata", "Chordata_custom"),
          options = list(
            create = TRUE,
            maxItems = 1
          )
        )
        rv$params <- cur$params |> jsonlite::fromJSON()
      }
    })
    output$params <- listviewer::renderReactjson({
      listviewer::reactjson(
        req(rv$params),
        "Validataion Parameters",
        theme = "monokai",
        iconStyle = "triangle",
        collapsed = 2,
        enableClipboard = FALSE,
        displayObjectSize = FALSE,
        displayDataTypes = FALSE,
        onEdit = FALSE,
        onAdd = FALSE,
        onDelete = FALSE,
        onSelect = FALSE
      )
    })
    observeEvent(input$edit_curate_opts, ignoreInit = T, {
      shinyjs::toggleState("curate_opts_cpus", condition = input$edit_curate_opts)
      shinyjs::toggleState("curate_opts_memory", condition = input$edit_curate_opts)
      shinyjs::toggleState("max_blast_hits", condition = input$edit_curate_opts)
      shinyjs::toggleState("curate_ref_dir", condition = input$edit_curate_opts)
      shinyjs::toggleState("curate_ref_db", condition = input$edit_curate_opts)
      shinyjs::toggleState("target", condition = input$edit_curate_opts)
      shinyjs::toggleState("start_gene", condition = input$edit_curate_opts)
      # Check if editing opts that apply beyond selection
      if (input$edit_curate_opts && input$curate_opts %in% filtered_data()$curate_opts) {
        rv$updating_indirect <- filtered_data() |>
          dplyr::filter(curate_opts == input$curate_opts) |>
          dplyr::anti_join(rv$updating, by = "ID")
        # Prevent editing opts that apply to locked samples
        if (nrow(rv$updating_indirect) > 0L && any(rv$updating_indirect$annotate_lock == 1)) {
          shinyWidgets::sendSweetAlert(
            title = "Attempting to edit locked samples",
            text = "Processing parameters associated with locked samples can not be edited.",
            type = "warning"
          )
          shinyWidgets::updatePrettyCheckbox(
            inputId = "edit_curate_opts",
            value = FALSE
          )
          req(F)
        }
        # Confirm editing opts that apply beyond selection
        if (nrow(rv$updating_indirect) > 0L) {
          shinyWidgets::confirmSweetAlert(
            inputId = "editing_curate_opts_indirect",
            title = "Editing beyond selection",
            text = "You are attempting to edit assembly options that apply to samples beyond the current selection. Are you sure you want to proceed?",
            btn_colors = c("#0056b3", "#0056b3")
          )
        }
      } else {
        rv$updating_indirect <- rv$updating |> dplyr::slice(0)
      }
    })
    observeEvent(input$target, {
      rv$params <- do.call(paste0("params_", input$target), list()) |>
        jsonlite::toJSON(auto_unbox = TRUE)
      output$params <- listviewer::renderReactjson({
        listviewer::reactjson(
          req(rv$params),
          "Validataion Parameters",
          theme = "monokai",
          iconStyle = "triangle",
          collapsed = 2,
          enableClipboard = FALSE,
          displayObjectSize = FALSE,
          displayDataTypes = FALSE,
          onEdit = FALSE,
          onAdd = FALSE,
          onDelete = FALSE,
          onSelect = FALSE
        )
      })
    })
    # Confirm editing opts that apply beyond selection
    observeEvent(input$editing_curate_opts_indirect, ignoreInit = T, {
      if (!input$editing_curate_opts_indirect) {
        rv$updating_indirect <- rv$updating |> dplyr::slice(0)
        shinyWidgets::updatePrettyCheckbox(
          inputId = "edit_curate_opts",
          value = FALSE
        )
      }
    })
    ## Save Changes ----
    observeEvent(input$update_curate_opts, {
      ## Add to params table if new or editing ----
      if (input$edit_curate_opts) {
        params <- do.call(paste0("params_", input$target), list()) |>
          jsonlite::toJSON(auto_unbox = TRUE)
        dplyr::tbl(session$userData$con, "curate_opts") |>
          dplyr::rows_upsert(
            data.frame(
              curate_opts = req(input$curate_opts),
              cpus = req(input$curate_opts_cpus),
              memory = req(input$curate_opts_memory),
              max_blast_hits = req(input$max_blast_hits),
              ref_dir = req(input$curate_ref_dir),
              ref_db = req(input$curate_ref_db),
              target = req(input$target)
            ),
            in_place = TRUE,
            copy = TRUE,
            by = "curate_opts"
          )
        DBI::dbExecute(
          session$userData$con,
          stringr::str_glue(
            "UPDATE curate_opts SET params = '{params}' WHERE curate_opts = '{input$curate_opts}'"
          )
        )
        rv$curate_opts <- dplyr::tbl(session$userData$con, "curate_opts") |>
          dplyr::collect()
      }
      ## Update Annotate Table ----
      update <- data.frame(
        ID = c(rv$updating$ID, rv$updating_indirect$ID),
        curate_opts = input$curate_opts,
        annotate_switch = 1
      )
      dplyr::tbl(session$userData$con, "annotate") |>
        dplyr::rows_update(
          update,
          unmatched = "ignore",
          in_place = TRUE,
          copy = TRUE,
          by = "ID"
        )
      rv$data <- filtered_data() |>
        dplyr::rows_update(
          update,
          by = "ID"
        )
      rv$updating <- rv$updating_indirect <- NULL
      removeModal()
      trigger("update_annotate_table")
    })

    # Set ORF Options ----
    observeEvent(input$set_orf_opts, {
      row <- as.numeric(input$set_orf_opts)
      if (length(selected()) > 0 && !row %in% selected()) {
        req(F)
      } else {
        selected <- c(row, selected()) |> unique()
      }
      req(all(filtered_data()$annotate_lock[selected] == 0))
      rv$updating <- filtered_data() |> dplyr::slice(selected)
      rv$updating_indirect <- rv$updating |> dplyr::slice(0)
      orf_opts_modal(rv)
    })
    observeEvent(input$orf_opts, ignoreInit = T, {
      exists <- input$orf_opts %in% rv$orf_opts$orf_opts
      shinyWidgets::updatePrettyCheckbox(
        inputId = "edit_orf_opts",
        value = !exists
      )
      if (exists) {
        cur <- rv$orf_opts[rv$orf_opts$orf_opts == input$orf_opts, ]
        shinyWidgets::updatePrettyCheckbox(
          inputId = "use_orffinder",
          value = isTRUE(as.logical(cur$use_orffinder %||% 0L))
        )
        updateNumericInput(inputId = "orf_opts_cpus", value = cur$cpus)
        updateNumericInput(inputId = "orf_opts_memory", value = cur$memory)
        updateNumericInput(inputId = "orf_max_blast_hits", value = cur$max_blast_hits)
        updateNumericInput(inputId = "orf_min_len", value = cur$orf_min_len)
        updateNumericInput(inputId = "orf_max_overlap", value = cur$orf_max_overlap)
        updateTextInput(inputId = "orffinder_opts", value = cur$orffinder_opts)
        updateSelectizeInput(
          inputId = "orf_ref_dir",
          selected = cur$ref_dir,
          choices = unique(rv$orf_opts$ref_dir),
          options = list(create = TRUE, maxItems = 1)
        )
        updateSelectizeInput(
          inputId = "orf_ref_db",
          selected = cur$ref_db,
          choices = c("Metazoa_RefSeq89", "Metazoa_RefSeq231", "Metazoa_RefSeq231_custom", "Chordata", "Chordata_custom"),
          options = list(create = TRUE, maxItems = 1)
        )
      }
    })
    observeEvent(input$edit_orf_opts, ignoreInit = T, {
      shinyjs::toggleState("use_orffinder", condition = input$edit_orf_opts)
      shinyjs::toggleState("orf_opts_cpus", condition = input$edit_orf_opts)
      shinyjs::toggleState("orf_opts_memory", condition = input$edit_orf_opts)
      shinyjs::toggleState("orf_max_blast_hits", condition = input$edit_orf_opts)
      shinyjs::toggleState("orf_min_len", condition = input$edit_orf_opts)
      shinyjs::toggleState("orf_max_overlap", condition = input$edit_orf_opts)
      shinyjs::toggleState("orffinder_opts", condition = input$edit_orf_opts)
      shinyjs::toggleState("orf_ref_dir", condition = input$edit_orf_opts)
      shinyjs::toggleState("orf_ref_db", condition = input$edit_orf_opts)
      # Check if editing opts that apply beyond selection
      if (input$edit_orf_opts && input$orf_opts %in% filtered_data()$orf_opts) {
        rv$updating_indirect <- filtered_data() |>
          dplyr::filter(orf_opts == input$orf_opts) |>
          dplyr::anti_join(rv$updating, by = "ID")
        if (nrow(rv$updating_indirect) > 0L && any(rv$updating_indirect$annotate_lock == 1)) {
          shinyWidgets::sendSweetAlert(
            title = "Attempting to edit locked samples",
            text = "Processing parameters associated with locked samples can not be edited.",
            type = "warning"
          )
          shinyWidgets::updatePrettyCheckbox(inputId = "edit_orf_opts", value = FALSE)
          req(F)
        }
        if (nrow(rv$updating_indirect) > 0L) {
          shinyWidgets::confirmSweetAlert(
            inputId = "editing_orf_opts_indirect",
            title = "Editing beyond selection",
            text = "You are attempting to edit options that apply to samples beyond the current selection. Are you sure you want to proceed?",
            btn_colors = c("#0056b3", "#0056b3")
          )
        }
      } else {
        rv$updating_indirect <- rv$updating |> dplyr::slice(0)
      }
    })
    observeEvent(input$editing_orf_opts_indirect, ignoreInit = T, {
      if (!input$editing_orf_opts_indirect) {
        rv$updating_indirect <- rv$updating |> dplyr::slice(0)
        shinyWidgets::updatePrettyCheckbox(inputId = "edit_orf_opts", value = FALSE)
      }
    })
    ## Save Changes ----
    observeEvent(input$update_orf_opts, {
      if (input$edit_orf_opts) {
        dplyr::tbl(session$userData$con, "orf_opts") |>
          dplyr::rows_upsert(
            data.frame(
              orf_opts = req(input$orf_opts),
              use_orffinder = as.integer(isTRUE(input$use_orffinder)),
              cpus = req(input$orf_opts_cpus),
              memory = req(input$orf_opts_memory),
              orffinder_opts = input$orffinder_opts %||% "",
              orf_min_len = req(input$orf_min_len),
              orf_max_overlap = req(input$orf_max_overlap),
              max_blast_hits = req(input$orf_max_blast_hits),
              ref_dir = req(input$orf_ref_dir),
              ref_db = req(input$orf_ref_db)
            ),
            in_place = TRUE,
            copy = TRUE,
            by = "orf_opts"
          )
        rv$orf_opts <- dplyr::tbl(session$userData$con, "orf_opts") |>
          dplyr::collect()
      }
      update <- data.frame(
        ID = c(rv$updating$ID, rv$updating_indirect$ID),
        orf_opts = input$orf_opts,
        annotate_switch = 1
      )
      dplyr::tbl(session$userData$con, "annotate") |>
        dplyr::rows_update(
          update,
          unmatched = "ignore",
          in_place = TRUE,
          copy = TRUE,
          by = "ID"
        )
      rv$data <- filtered_data() |>
        dplyr::rows_update(update, by = "ID")
      rv$updating <- rv$updating_indirect <- NULL
      removeModal()
      trigger("update_annotate_table")
    })
    # Open output folder ----
    observeEvent(input$output, ignoreInit = T, {
      pth <- file.path(
        session$userData$dir_out,
        filtered_data()$ID[as.numeric(input$output)],
        "annotate"
      )
      req(file.exists(pth))
      if (tolower(Sys.getenv("RSTUDIO_PROGRAM_MODE")) == "server") {
        rstudioapi::filesPaneNavigate(pth)
        req(F)
      } else {
        utils::browseURL(pth)
      }
    })

    # Open annotation details ----
    observeEvent(input$details, {
      session$userData$in_outlier_review <- FALSE
      rv$updating <- filtered_data() |> dplyr::slice(as.numeric(input$details))
      trigger("annotations_modal")
    })

    # Open annotation details from the export outlier review (cross-tab jump)
    on("goto_annotate", {
      target <- session$userData$goto_annotate_target
      req(target, target$ID)
      hit <- rv$data |> dplyr::filter(ID == target$ID)
      req(nrow(hit) > 0)
      session$userData$in_outlier_review <- TRUE
      rv$updating <- hit |> dplyr::slice(1)
      trigger("annotations_modal")
    })

    annotations_details_server(ns("annotations"), rv)

    # CSV Export ----
    .export_cols_drop <- c("output", "view", "poor_blast_ref", "warnings_details")

    observe({
      shinyjs::toggleState("export_selected", condition = length(selected()) > 0)
    })

    output$export_selected <- downloadHandler(
      filename = function() paste0("annotate_selected_", Sys.Date(), ".csv"),
      content = function(file) {
        req(length(selected()) > 0)
        rv$data |>
          dplyr::slice(selected()) |>
          dplyr::select(-dplyr::any_of(.export_cols_drop)) |>
          write.csv(file, row.names = FALSE)
      }
    )

    output$export_all <- downloadHandler(
      filename = function() paste0("annotate_all_", Sys.Date(), ".csv"),
      content = function(file) {
        rv$data |>
          dplyr::select(-dplyr::any_of(.export_cols_drop)) |>
          write.csv(file, row.names = FALSE)
      }
    )
  })
}
