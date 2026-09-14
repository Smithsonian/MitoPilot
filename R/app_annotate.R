# Togglable column groups for the Annotate table. Cols not listed here
# (sticky cols, action buttons) are always shown.
ANNOTATE_COL_GROUPS <- list(
  Options  = c("annotate_opts", "curate_opts", "orf_opts"),
  Stats    = c("length_raw", "length", "topology", "scaffolds"),
  BLAST    = c("blast_ref_status", "blast_accession", "blast_species",
               "blast_lineage", "blast_pident", "blast_qcovs"),
  Counts   = c("PCGCount", "tRNACount", "rRNACount", "ORFCount", "missing", "extra"),
  Review   = c("ID_verified", "reviewed", "problematic", "partial", "warnings"),
  Export   = c("export_group", "export_time_stamp"),
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
# State labels come from mp_state_choices("annotate"); R sources constants.R
# after this file, so they are read inside function bodies, never at top level.
ANNOTATE_LOCK_CHOICES <- c("Unlocked" = "0", "Locked" = "1")
ANNOTATE_EXPORT_CHOICES <- c("Not Exported" = "0", "Exported" = "1")

# What choosing each state does to the next Update, shown under its radio.
ANNOTATE_STATE_CONSEQUENCE <- c(
  `0` = "Skipped by the next update.",
  `1` = "Processed by the next update; annotations already stored are replaced.",
  `2` = "Treated as finished; the next update skips it.",
  `3` = "Treated as finished with an error; the next update skips it."
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
    uiOutput(ns("col_css")),
    # Row filters left to right, then the column picker last (T13).
    div(
      class = "mp-filter-row",
      mp_filter_picker(ns("lock_filter"), "Lock:", ANNOTATE_LOCK_CHOICES,
                       width = "140px"),
      mp_filter_picker(ns("state_filter"), "State:", mp_state_choices("annotate"),
                       width = "140px"),
      mp_filter_picker(ns("export_filter"), "Exported:", ANNOTATE_EXPORT_CHOICES,
                       width = "140px"),
      uiOutput(ns("warnings_select")),
      shinyWidgets::airDatepickerInput(
        inputId    = ns("date_filter"),
        label      = "Updated between:",
        range      = TRUE,
        clearButton = TRUE,
        value      = NULL,
        width      = "220px",
        placeholder = "any time"
      ),
      mp_filter_picker(ns("col_groups"), "Columns:", names(ANNOTATE_COL_GROUPS),
                       width = "150px")
    ),
    uiOutput(ns("n_selected")),
    div(class = "mp-table-resize", reactable::reactableOutput(ns("table"))),
    mp_csv_download_row(ns)
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
    reopen_orf <- function() orf_opts_modal(rv)
    register_tool_help("mitos", input, reopen = reopen_annotate)
    register_tool_help("trnaScan-SE", input, reopen = reopen_annotate)
    register_tool_help("mitofinder", input, reopen = reopen_annotate)
    register_tool_help("arwen", input, reopen = reopen_annotate)
    register_tool_help("aragorn", input, reopen = reopen_annotate)
    register_tool_help("orffinder", input, reopen = reopen_orf)

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

    # Warning choices, normalised. "12 ambiguous bases in CDS" and "3 ambiguous
    # bases in CDS" are one warning type; only that warning carries a count.
    # The cell keeps the full string, the picker matches the type (T13).
    warn_type <- function(x) sub("^[0-9]+ ", "", trimws(x))

    output$warnings_select <- renderUI({
      req(rv$data)

      # warnings_details holds semicolon-delimited warning strings.
      warn_vals <- rv$data$warnings_details |>
        na.omit() |>
        strsplit(split = ";\\s*") |>
        unlist() |>
        warn_type() |>
        unique() |>
        sort()

      mp_filter_picker(
        ns("warning_filters"), "Warnings column includes:",
        choices = warn_vals,
        selected = isolate(input$warning_filters) %||% warn_vals,
        width = "220px"
      )
    })

    # Reactive filtered data
    filtered_data <- reactive({
      req(rv$data)
      selected <- input$warning_filters
      out <- rv$data |> dplyr::mutate(
        warnings = purrr::map_int(warnings_details, function(wd) {
          # if (is.na(wd) || length(selected) == 0) return(0)
          wd_list <- strsplit(as.character(wd), ";")[[1]] |>
            warn_type()
          sum(wd_list %in% selected)
        })
      )
      # Date-range filter on "Last Updated" (time_stamp is epoch seconds). Empty
      # picker = no filter; end day is inclusive. Unlike the lock/state CSS
      # filters this subsets the rows, so selection may reset when the range changes.
      dr <- input$date_filter
      if (!is.null(dr) && length(dr) == 2 && all(!is.na(dr))) {
        lo <- as.numeric(as.POSIXct(as.Date(dr[1])))
        hi <- as.numeric(as.POSIXct(as.Date(dr[2]) + 1))
        out <- out |>
          dplyr::filter(!is.na(time_stamp) & time_stamp >= lo & time_stamp < hi)
      }
      out
    })

    # Refresh the table when the date-range filter changes (ignoreNULL = FALSE so
    # clearing the range restores all rows).
    observeEvent(input$date_filter, {
      req(rv$data)
      updateReactable("table", data = filtered_data())
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

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
    state_filter_rv <- reactiveVal(MP_STATE_CODES$annotate)
    observeEvent(input$state_filter, {
      state_filter_rv(input$state_filter %||% character(0))
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
    export_filter_rv <- reactiveVal(unname(ANNOTATE_EXPORT_CHOICES))
    observeEvent(input$export_filter, {
      export_filter_rv(input$export_filter %||% character(0))
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    # CSS class for a togglable column. Added to both body cells and the
    # header cell so the whole column collapses when the group is hidden.
    .grp <- function(col) {
      g <- ANNOTATE_COL_GROUP_LOOKUP[col]
      if (is.na(g)) NULL else paste0("mp-grp-", g)
    }

    # One name and one header tooltip per field, from the shared registry.
    # `field` is the column name except for the two length columns, where
    # Annotate's `length` is the trimmed length.
    .nm <- function(field) unname(MP_COL_NAMES[[field]])
    .hd <- function(field, tip = unname(MP_COL_TIPS[field])) {
      rt_header(.nm(field), if (length(tip) == 0 || is.na(tip)) NULL else tip)
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
      hidden_state <- setdiff(MP_STATE_CODES$annotate, state_filter_rv())
      hidden_exp   <- setdiff(unname(ANNOTATE_EXPORT_CHOICES), export_filter_rv())
      # Hide the Path / Scaffold columns when every unit shares value 1 (single
      # path/scaffold everywhere -> no extra info). Reactive on rv$data so the
      # columns appear as soon as a multi-unit sample is locked, no app restart.
      d <- rv$data
      hide_path     <- !is.null(d) && nrow(d) > 0 && all(d$path == 1, na.rm = TRUE)
      hide_scaffold <- !is.null(d) && nrow(d) > 0 && all(d$scaffold == 1, na.rm = TRUE)
      # Scope to THIS module's table so rules don't hit the shared mp-lock /
      # mp-state / mp-grp classes on the assemble, userAsmb, and export tables.
      sel <- paste0("#", ns("table"), " ")
      rules <- c(
        if (hide_path)            paste0(sel, ".mp-col-path { display: none !important; }"),
        if (hide_scaffold)        paste0(sel, ".mp-col-scaffold { display: none !important; }"),
        if (length(hidden_grp))   paste0(sel, ".mp-grp-",   hidden_grp,   " { display: none !important; }"),
        if (length(hidden_lock))  paste0(sel, ".mp-lock-",  hidden_lock,  " { display: none !important; }"),
        if (length(hidden_state)) paste0(sel, ".mp-state-", hidden_state, " { display: none !important; }"),
        if (length(hidden_exp))   paste0(sel, ".mp-exp-",   hidden_exp,   " { display: none !important; }")
      )
      if (length(rules) == 0) return(NULL)
      tags$style(HTML(paste(rules, collapse = "\n")))
    })

    # Render table ----
    output$table <- renderReactable({
      # Rendered once; later changes arrive through updateReactable(), so
      # nothing here may read rv$data reactively (that would re-render and
      # drop the selection).
      tbl_data <- isolate(filtered_data())
      reactable(
        data = tbl_data,
        compact = TRUE,
        striped = TRUE,
        language = reactable::reactableLang(
          noData = "No assemblies are locked yet. Lock a finished sample in Assemble to see it here."
        ),
        defaultPageSize = 100,
        resizable = TRUE,
        showPageSizeOptions = TRUE,
        onClick = "select",
        selection = "multiple",
        searchable = TRUE,
        filterable = TRUE,
        defaultSorted = list(time_stamp = "desc"),
        height = "100%",
        wrap = FALSE,
        pageSizeOptions = c(25, 50, 100, 200, 500),
        rowStyle = rt_highlight_row(),
        rowClass = JS("function(rowInfo) {
          if (!rowInfo || !rowInfo.values) return '';
          var ets = rowInfo.values['export_time_stamp'];
          var exp = (ets != null && ets !== '') ? '1' : '0';
          return 'mp-lock-' + rowInfo.values['annotate_lock'] +
                 ' mp-state-' + rowInfo.values['annotate_switch'] +
                 ' mp-exp-' + exp;
        }"),
        # No align default: reactable then right-aligns numbers and
        # left-aligns text on its own (T10).
        defaultColDef = colDef(show = FALSE),
        theme = reactable::reactableTheme(
          headerStyle = list(whiteSpace = "normal", lineHeight = "1.2")
        ),
        columns = list(
          `.selection` = colDef(show = T, sticky = "left", width = 28, align = "center"),
          annotate_lock = colDef(
            show = TRUE,
            sticky = "left",
            name = .nm("annotate_lock"),
            header = .hd("annotate_lock", MP_LOCK_DEF("annotate")),
            html = TRUE,
            filterable = FALSE,
            sortable = FALSE,
            width = 50,
            align = "center",
            cell = rt_dynamicIcon(
              c(`0` = "fa fa-lock-open", `1` = "fa fa-lock"),
              labels = c(
                `0` = paste("Unlocked -", MP_LOCK_DEF("annotate")),
                `1` = paste("Locked -", MP_LOCK_DEF("annotate"))
              )
            )
          ),
          annotate_switch = colDef(
            show = TRUE,
            sticky = "left",
            name = .nm("annotate_switch"),
            header = .hd("annotate_switch"),
            html = TRUE,
            filterable = FALSE,
            sortable = FALSE,
            width = 62,
            align = "center",
            cell = rt_dynamicIcon(
              icons  = mp_state_icons("annotate"),
              labels = paste0(mp_state_labels("annotate"), " - ",
                              mp_state_tips("annotate")) |>
                stats::setNames(MP_STATE_CODES$annotate)
            )
          ),
          ID = colDef(
            show = TRUE,
            name = .nm("ID"),
            minWidth = mp_fit_width(tbl_data$ID),
            sticky = "left",
            html = TRUE,
            cell = rt_longtext()
          ),
          # Per-unit key: each (ID, path, scaffold) is its own row. Not sticky (only
          # lock/state/ID stay frozen). The classes let col_css hide a column when
          # every unit shares value 1 (no extra info).
          path = colDef(
            show = TRUE, name = .nm("path"), header = .hd("path"),
            class = "mp-col-path",
            headerClass = "mp-col-path", width = 90, align = "center", filterable = FALSE
          ),
          scaffold = colDef(
            show = TRUE, name = .nm("scaffold"), header = .hd("scaffold"),
            class = "mp-col-scaffold",
            headerClass = "mp-col-scaffold", width = 90, align = "center", filterable = FALSE
          ),
          Taxon = colDef(
            show = TRUE,
            name = .nm("Taxon"),
            minWidth = 140,
            html = TRUE,
            cell = rt_longtext()
          ),
          ID_verified = colDef(
            show = TRUE, class = .grp("ID_verified"), headerClass = .grp("ID_verified"),
            name = .nm("ID_verified"), header = .hd("ID_verified"),
            html = TRUE,
            align = "center",
            width = 150,
            cell = rt_bool_badge()
          ),
          annotate_opts = colDef(
            show = TRUE, class = .grp("annotate_opts"), headerClass = .grp("annotate_opts"),
            name = .nm("annotate_opts"), header = .hd("annotate_opts"),
            html = TRUE,
            width = 130,
            cell = rt_link(ns("set_annotate_opts"),
                           title = "Edit annotation options",
                           lock_col = "annotate_lock")
          ),
          curate_opts = colDef(
            show = TRUE, class = .grp("curate_opts"), headerClass = .grp("curate_opts"),
            name = .nm("curate_opts"), header = .hd("curate_opts"),
            html = TRUE,
            width = 110,
            cell = rt_link(ns("set_curate_opts"),
                           title = "Edit curation options",
                           lock_col = "annotate_lock")
          ),
          orf_opts = colDef(
            show = TRUE, class = .grp("orf_opts"), headerClass = .grp("orf_opts"),
            name = .nm("orf_opts"), header = .hd("orf_opts"),
            html = TRUE,
            width = 110,
            cell = rt_link(ns("set_orf_opts"),
                           title = "Edit ORF options",
                           lock_col = "annotate_lock")
          ),
          length_raw = colDef(
            show = TRUE, class = .grp("length_raw"), headerClass = .grp("length_raw"),
            name = .nm("length_raw"), header = .hd("length_raw"),
            minWidth = 125,
            filterable = FALSE,
            align = "center",
            html = TRUE,
            cell = rt_longtext()
          ),
          length = colDef(
            show = TRUE, class = .grp("length"), headerClass = .grp("length"),
            name = .nm("length_trimmed"), header = .hd("length_trimmed"),
            minWidth = 150,
            filterable = FALSE,
            align = "center",
            html = TRUE,
            cell = rt_longtext()
          ),
          topology = colDef(show = TRUE, class = .grp("topology"), headerClass = .grp("topology"),
                            name = .nm("topology"), header = .hd("topology"), align = "center",
                            html = TRUE, cell = rt_topology()),
          scaffolds = colDef(show = TRUE, class = .grp("scaffolds"), headerClass = .grp("scaffolds"),
                             name = .nm("scaffolds"), header = .hd("scaffolds"), align = "center"),
          poor_blast_ref = colDef(show = FALSE),
          blast_ref_status = colDef(
            show = TRUE, class = .grp("blast_ref_status"), headerClass = .grp("blast_ref_status"),
            name = .nm("blast_ref_status"), header = .hd("blast_ref_status"),
            html = TRUE,
            minWidth = 130,
            resizable = TRUE,
            align = "center",
            filterable = TRUE,
            cell = rt_blast_ref_status()
          ),
          blast_accession = colDef(
            show = TRUE, class = .grp("blast_accession"), headerClass = .grp("blast_accession"),
            name = .nm("blast_accession"), header = .hd("blast_accession"),
            html = TRUE,
            width = 120,
            cell = rt_ncbi_link(auto_col = "blast_accession_auto")
          ),
          blast_accession_auto = colDef(show = FALSE),
          blast_species = colDef(
            show = TRUE, class = .grp("blast_species"), headerClass = .grp("blast_species"),
            name = .nm("blast_species"), header = .hd("blast_species"),
            html = TRUE,
            minWidth = 160,
            cell = rt_longtext()
          ),
          blast_lineage = colDef(
            show = TRUE, class = .grp("blast_lineage"), headerClass = .grp("blast_lineage"),
            name = .nm("blast_lineage"), header = .hd("blast_lineage"),
            html = TRUE,
            minWidth = 200,
            cell = rt_longtext()
          ),
          blast_pident = colDef(
            show = TRUE, class = .grp("blast_pident"), headerClass = .grp("blast_pident"),
            name = .nm("blast_pident"), header = .hd("blast_pident"),
            filterable = FALSE,
            align = "center",
            minWidth = 130
          ),
          blast_qcovs = colDef(
            show = TRUE, class = .grp("blast_qcovs"), headerClass = .grp("blast_qcovs"),
            name = .nm("blast_qcovs"), header = .hd("blast_qcovs"),
            filterable = FALSE,
            align = "center",
            minWidth = 135
          ),
          PCGCount = colDef(show = TRUE, class = .grp("PCGCount"), headerClass = .grp("PCGCount"), name = .nm("PCGCount"), header = .hd("PCGCount"), align = "center"),
          tRNACount = colDef(show = TRUE, class = .grp("tRNACount"), headerClass = .grp("tRNACount"), name = .nm("tRNACount"), header = .hd("tRNACount"), align = "center"),
          rRNACount = colDef(show = TRUE, class = .grp("rRNACount"), headerClass = .grp("rRNACount"), name = .nm("rRNACount"), header = .hd("rRNACount"), align = "center"),
          ORFCount = colDef(show = TRUE, class = .grp("ORFCount"), headerClass = .grp("ORFCount"), name = .nm("ORFCount"), header = .hd("ORFCount"), align = "center"),
          missing = colDef(show = TRUE, class = .grp("missing"), headerClass = .grp("missing"),
                           name = .nm("missing"), header = .hd("missing"), html = TRUE, cell = rt_longtext()),
          extra = colDef(show = TRUE, class = .grp("extra"), headerClass = .grp("extra"),
                         name = .nm("extra"), header = .hd("extra"), html = TRUE, cell = rt_longtext()),
          warnings = colDef(show = TRUE, class = .grp("warnings"), headerClass = .grp("warnings"),
                            name = .nm("warnings"), header = .hd("warnings"), align = "center"),
          reviewed = colDef(
            show = TRUE, class = .grp("reviewed"), headerClass = .grp("reviewed"),
            name = .nm("reviewed"), header = .hd("reviewed"),
            html = TRUE,
            align = "center",
            width = 100,
            cell = rt_bool_badge()
          ),
          problematic = colDef(
            show = TRUE, class = .grp("problematic"), headerClass = .grp("problematic"),
            name = .nm("problematic"), header = .hd("problematic"),
            html = TRUE,
            align = "center",
            width = 100,
            cell = rt_bool_badge(yes_tone = "warning")
          ),
          partial = colDef(
            show = TRUE, class = .grp("partial"), headerClass = .grp("partial"),
            name = .nm("partial"), header = .hd("partial"),
            html = TRUE,
            align = "center",
            width = 100,
            cell = rt_bool_badge(yes_tone = "warning")
          ),
          export_group = colDef(
            show = TRUE, class = .grp("export_group"), headerClass = .grp("export_group"),
            name = .nm("export_group"), header = .hd("export_group"),
            minWidth = 120,
            cell = function(value) if (is.na(value) || !nzchar(value)) "" else value
          ),
          export_time_stamp = colDef(
            show = TRUE, class = .grp("export_time_stamp"), headerClass = .grp("export_time_stamp"),
            name = .nm("export_time_stamp"), header = .hd("export_time_stamp"),
            filterable = FALSE,
            html = TRUE,
            width = 170,
            align = "center",
            # JS cell so it re-renders on updateReactable(); shows the exported
            # pill + export date + the group the sample was exported under.
            cell = htmlwidgets::JS("
              function(cellInfo) {
                var v = cellInfo.value;
                if (v == null || v === '') return '';
                var opts = { year: 'numeric', month: 'numeric', day: 'numeric' };
                var date = new Date(1000*v).toLocaleDateString(undefined, opts);
                if (date === 'Invalid Date') return '';
                var row = cellInfo.row || {};
                var g = row.export_group;
                var grp = (g == null || g === '' || g === 'NA') ? '' : ' (' + g + ')';
                return `<span class='mp-pill mp-pill-success'>exported</span> ` +
                  date + grp;
              }
            ")
          ),
          time_stamp = colDef(
            show = TRUE, class = .grp("time_stamp"), headerClass = .grp("time_stamp"),
            name = .nm("time_stamp"), header = .hd("time_stamp"),
            filterable = FALSE,
            html = T,
            width = 150,
            align = "center",
            cell = rt_ts_date()
          ),
          annotate_notes = colDef(
            show = TRUE, class = .grp("annotate_notes"), headerClass = .grp("annotate_notes"),
            name = .nm("annotate_notes"),
            html = TRUE,
            minWidth = 150,
            # maxWidth = 400,
            cell = rt_longtext()
          ),
          view = colDef(
            show = TRUE,
            sticky = "right",
            filterable = FALSE,
            sortable = FALSE,
            name = .nm("view"),
            html = TRUE,
            width = 80,
            align = "center",
            cell = rt_icon_bttn_text(
              ns("details"), "fas fa-square-arrow-up-right fa-xs",
              label = "Details", title = "Open the details window for this assembly"
            )
          ),
          output = colDef(
            show = TRUE,
            sticky = "right",
            filterable = FALSE,
            sortable = FALSE,
            name = .nm("output"),
            html = TRUE,
            width = 80,
            align = "center",
            cell = rt_icon_bttn_text(
              ns("output"), "fas fa-folder-open fa-xs",
              label = "Output", title = "Open the output folder for this sample"
            )
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
    # Rows hidden by the lock/state filters stay mounted in reactable's row
    # model, so a shift-click range can select them. Drop currently-hidden rows
    # so bulk ops only ever touch visible samples.
    selected <- reactive({
      sel <- reactable::getReactableState("table", "selected")
      if (is.null(sel) || length(sel) == 0) return(sel)
      exp_code <- ifelse(is.na(rv$data$export_time_stamp), "0", "1")
      visible <- as.character(rv$data$annotate_lock)   %in% lock_filter_rv() &
                 as.character(rv$data$annotate_switch) %in% state_filter_rv() &
                 exp_code %in% export_filter_rv()
      intersect(sel, which(visible))
    })

    # Toolbar buttons that act on the selection are dead without one (T01).
    observe({
      shinyjs::toggleState(
        selector  = "#annot_ctrls .mp-needs-selection",
        condition = length(selected()) > 0
      )
    })

    # Row grain and counts, stated (T07). The first number counts the rows the
    # pickers and the date filter leave visible; reactable's own search box is
    # client-side only, so it is not reflected here.
    output$n_selected <- renderUI({
      d <- filtered_data()
      exp_code <- ifelse(is.na(d$export_time_stamp), "0", "1")
      visible <- as.character(d$annotate_lock)   %in% lock_filter_rv() &
                 as.character(d$annotate_switch) %in% state_filter_rv() &
                 exp_code %in% export_filter_rv()
      assemble_table_status(sum(visible), nrow(rv$data), length(selected()), noun = "assembly")
    })

    # Publish current selection so the work-dir browser can pre-select this sample
    observe({
      session$userData$wd_selected[["Annotate"]] <- unique(filtered_data()$ID[selected()])
    })

    # Prune hidden rows from reactable's actual selection whenever the
    # selection OR the filters change. Triggering on the selection itself is
    # what catches a shift-click range: hidden rows are removed immediately, so
    # they never persist in reactable's state to reappear when later revealed.
    observeEvent(
      list(reactable::getReactableState("table", "selected"),
           lock_filter_rv(), state_filter_rv(), export_filter_rv()), {
      sel <- reactable::getReactableState("table", "selected")
      if (is.null(sel) || length(sel) == 0) return()
      exp_code <- ifelse(is.na(rv$data$export_time_stamp), "0", "1")
      visible <- as.character(rv$data$annotate_lock)   %in% lock_filter_rv() &
                 as.character(rv$data$annotate_switch) %in% state_filter_rv() &
                 exp_code %in% export_filter_rv()
      keep <- intersect(sel, which(visible))
      if (length(keep) != length(sel)) {
        reactable::updateReactable("table", selected = keep)
      }
    }, ignoreInit = TRUE)

    # Set State ----
    init("state")
    on("state", {
      req(session$userData$mode == "Annotate")
      sel <- selected()
      if (!need_selection(length(sel))) return()
      d <- filtered_data()
      if (!need_unlocked(d$ID[sel][d$annotate_lock[sel] == 1], "assembly")) return()
      rv$updating <- d |>
        dplyr::select(ID, path, scaffold, annotate_switch) |>
        dplyr::slice(sel)
      current <- character(0)
      if (length(unique(rv$updating$annotate_switch)) == 1) {
        current <- as.character(rv$updating$annotate_switch[1])
      }
      showModal(
        modalDialog(
          title = mp_modal_title(
            paste("Set state for", mp_n(nrow(rv$updating), "assembly"))
          ),
          tags$p(class = "text-muted", mp_id_list(unique(rv$updating$ID))),
          if (length(current) == 0) {
            tags$p(class = "text-muted",
                   "The selected rows are not all in the same state.")
          },
          shinyWidgets::prettyRadioButtons(
            ns("new_state"),
            label = "New state",
            choiceValues = MP_STATE_SETTABLE,
            choiceNames = lapply(MP_STATE_SETTABLE, function(k) {
              tagList(
                tags$strong(MP_STATE_META[[k]]$label),
                tags$span(class = "text-muted", style = "font-size: 0.85em;",
                          paste0(" ", ANNOTATE_STATE_CONSEQUENCE[[k]]))
              )
            }),
            selected = if (length(current) && current %in% MP_STATE_SETTABLE) {
              current
            } else {
              character(0)
            },
            shape = "round",
            status = "primary"
          ),
          size = "m",
          easyClose = TRUE,
          footer = mp_footer(primary = actionButton(ns("update_state"), "Set state"))
        )
      )
    })
    observeEvent(input$update_state, {
      if (!isTruthy(input$new_state)) {
        mp_toast("Choose a state first.", type = "warning")
        return()
      }
      n <- nrow(rv$updating)
      rv$updating$annotate_switch <- as.numeric(input$new_state)
      dplyr::tbl(session$userData$con, "annotate") |>
        dplyr::rows_update(
          rv$updating,
          unmatched = "ignore",
          in_place = TRUE,
          copy = TRUE,
          by = c("ID", "path", "scaffold")
        )
      rv$data <- filtered_data() |>
        dplyr::rows_update(
          rv$updating,
          by = c("ID", "path", "scaffold")
        )
      trigger("update_annotate_table")
      removeModal()
      mp_toast(paste0(
        mp_n(n, "assembly"), " set to ",
        MP_STATE_META[[as.character(input$new_state)]]$label, "."
      ))
    })

    # Toggle lock ----
    # Locking reports; unlocking asks first, because it drops the units out of
    # Export and lets the next update overwrite curated annotations (T02).
    write_lock <- function(upd, n = nrow(upd)) {
      locking <- upd$annotate_lock[1] == 1
      dplyr::tbl(session$userData$con, "annotate") |>
        dplyr::rows_update(
          upd,
          unmatched = "ignore",
          in_place = TRUE,
          copy = TRUE,
          by = c("ID", "path", "scaffold")
        )
      rv$data <- filtered_data() |>
        dplyr::rows_update(upd, by = c("ID", "path", "scaffold"))
      trigger("update_annotate_table")
      trigger("refresh_export")
      if (n > 0L) {
        mp_toast(
          if (locking) {
            paste0(mp_n(n, "assembly"), " locked - ready to export.")
          } else {
            paste0(mp_n(n, "assembly"), " unlocked.")
          }
        )
      }
    }

    init("lock")
    on("lock", {
      req(session$userData$mode == "Annotate")
      if (!need_selection(length(selected()))) return()
      rv$updating <- filtered_data() |>
        dplyr::select(ID, path, scaffold, annotate_lock) |>
        dplyr::slice(selected())
      lock_current <- as.numeric(names(which.max(table(rv$updating$annotate_lock))))
      locking <- as.numeric(!lock_current) == 1

      # Only LOCKED units advance to Export (fetch_export_data filters
      # annotate_lock == 1), and a sample cannot export more than one assembly path
      # (paths are competing resolutions of one genome). Locking a single path is
      # therefore the natural way to choose it; only locking MORE THAN ONE path of a
      # sample is a problem. Block just that case, using the resulting locked state
      # (this action applied on top of what is already locked), and name the samples.
      if (locking) {
        ids <- unique(rv$updating$ID)
        locked_after <- dplyr::tbl(session$userData$con, "annotate") |>
          dplyr::filter(ID %in% !!ids) |>
          dplyr::select(ID, path, scaffold, annotate_lock) |>
          dplyr::collect()
        sel_key <- paste(rv$updating$ID, rv$updating$path, rv$updating$scaffold)
        is_sel <- paste(locked_after$ID, locked_after$path, locked_after$scaffold) %in% sel_key
        locked_after$annotate_lock[is_sel] <- 1L
        locked_after <- locked_after[locked_after$annotate_lock == 1, , drop = FALSE]
        multi_path <- names(which(
          tapply(locked_after$path, locked_after$ID,
                 function(p) length(unique(p))) > 1
        ))
        if (length(multi_path) > 0) {
          shown <- paste(utils::head(multi_path, 8), collapse = ", ")
          if (length(multi_path) > 8) {
            shown <- paste0(shown, ", and ", length(multi_path) - 8, " more")
          }
          mp_alert(
            title = "Only one assembly path can be locked per sample",
            text = stringr::str_glue(
              "{mp_n(length(multi_path), 'sample')} would have more than one ",
              "assembly path locked, but a sample can export only one: {shown}.\n\n",
              "Assembly paths are alternative resolutions of the same genome. Lock ",
              "just the correct path (leave the others unlocked), or 'ignore' the ",
              "extra paths in the Assemble module."
            ),
            type = "warning"
          )
          req(FALSE)
        }
      }

      new_lock <- as.numeric(!lock_current)
      # Rows already at the new value are rewritten but change nothing.
      n_changed <- sum(!rv$updating$annotate_lock %in% new_lock)
      rv$updating$annotate_lock <- new_lock
      if (locking) {
        write_lock(rv$updating, n_changed)
      } else {
        rv$lock_pending <- rv$updating
        rv$lock_pending_n <- n_changed
        mp_confirm(
          "unlock_confirm",
          title = paste("Unlock", mp_n(n_changed, "assembly")),
          text = paste0(
            "Unlocking removes ", mp_n(n_changed, "assembly"), " from Export. ",
            "Any assembly whose state is Ready to run will be re-annotated by ",
            "the next update, replacing your curated results."
          ),
          action_label = "Unlock",
          danger = TRUE
        )
      }
    })
    observeEvent(input$unlock_confirm, ignoreInit = TRUE, {
      upd <- rv$lock_pending
      n <- rv$lock_pending_n %||% nrow(upd)
      rv$lock_pending <- NULL
      if (isTRUE(input$unlock_confirm) && !is.null(upd)) write_lock(upd, n)
    })

    # Review flags ----
    # One predicate for all three: mp_flag_next() decides both what the click
    # writes and what the toolbar button says it will do (T02).
    REVIEW_FLAGS <- list(
      id_verified_top = list(col = "ID_verified", off = "no",
                             noun = "ID Verified", said = "ID verified"),
      problematic_top = list(col = "problematic", off = NA_character_,
                             noun = "Problematic", said = "problematic"),
      partial_top     = list(col = "partial", off = "no",
                             noun = "Partial", said = "partial")
    )

    flag_next <- function(key) {
      f <- REVIEW_FLAGS[[key]]
      sel <- selected()
      vals <- if (length(sel) == 0) character(0) else filtered_data()[[f$col]][sel]
      mp_flag_next(vals, on = "yes", off = f$off)
    }

    write_flag <- function(key, upd) {
      f <- REVIEW_FLAGS[[key]]
      rv$updating <- upd |> dplyr::select(ID, path, scaffold, dplyr::all_of(f$col))
      dplyr::tbl(session$userData$con, "annotate") |>
        dplyr::rows_update(
          rv$updating,
          unmatched = "ignore",
          in_place = TRUE,
          copy = TRUE,
          by = c("ID", "path", "scaffold")
        )
      rv$data <- filtered_data() |>
        dplyr::rows_update(rv$updating, by = c("ID", "path", "scaffold"))
      trigger("update_annotate_table")
      mp_toast(paste0(
        mp_n(nrow(upd), "assembly"),
        if (identical(upd[[f$col]][1], "yes")) " marked " else " cleared of ",
        f$said, "."
      ))
    }

    # The toolbar buttons live in the top-level UI, so their labels are updated
    # through the root session, not this module's namespace.
    observe({
      root <- session$rootScope()
      for (key in names(REVIEW_FLAGS)) {
        f <- REVIEW_FLAGS[[key]]
        verb <- if (identical(flag_next(key), "yes")) "Mark" else "Clear"
        updateActionButton(root, key, label = paste(verb, f$noun))
      }
    })

    toggle_flag <- function(key) {
      f <- REVIEW_FLAGS[[key]]
      req(session$userData$mode == "Annotate")
      if (!need_selection(length(selected()))) return()
      upd <- filtered_data() |>
        dplyr::select(ID, path, scaffold, dplyr::all_of(f$col), topology) |>
        dplyr::slice(selected())
      nxt <- mp_flag_next(upd[[f$col]], on = "yes", off = f$off)
      # Marking a closed circle "partial" contradicts itself: ask first.
      if (key == "partial_top" && identical(nxt, "yes") &&
          any(upd$topology == "circular", na.rm = TRUE)) {
        upd$partial <- "yes"
        rv$partial_pending <- upd
        mp_confirm(
          "partial_circular_confirm",
          title = "Mark a circular assembly as partial",
          text = paste(
            "One or more selected assemblies is circular. A closed circle",
            "represents the whole molecule, so flagging it 'partial' is",
            "contradictory. Consider using the Linearize button (in the",
            "annotation details view) to break the circle before submission."
          ),
          action_label = "Mark partial anyway"
        )
        return()
      }
      upd[[f$col]] <- nxt
      write_flag(key, upd)
    }

    init("id_verified_top")
    on("id_verified_top", toggle_flag("id_verified_top"))
    init("problematic_top")
    on("problematic_top", toggle_flag("problematic_top"))
    init("partial_top")
    on("partial_top", toggle_flag("partial_top"))

    observeEvent(input$partial_circular_confirm, ignoreInit = TRUE, {
      upd <- rv$partial_pending
      rv$partial_pending <- NULL
      if (isTRUE(input$partial_circular_confirm) && !is.null(upd)) {
        write_flag("partial_top", upd)
      }
    })

    # Set Annotate Options ----
    observeEvent(input$set_annotate_opts, {
      row <- as.numeric(input$set_annotate_opts)
      d <- filtered_data()
      if (!row_in_selection(row, selected(), d$ID[row], "assembly")) return()
      selected <- c(row, selected()) |> unique()
      if (!need_unlocked(d$ID[selected][d$annotate_lock[selected] == 1], "assembly")) return()
      rv$updating <- d |> dplyr::slice(selected)
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
          inputId = "use_mitos",
          value = isTRUE(as.logical(cur$use_mitos %||% 1L))
        )
        shinyWidgets::updatePrettyCheckbox(
          inputId = "use_trnaScan",
          value = isTRUE(as.logical(cur$use_trnaScan %||% 1L))
        )
        shinyWidgets::updatePrettyCheckbox(
          inputId = "use_mitos_best",
          value = isTRUE(as.logical(cur$use_mitos_best %||% 1L))
        )
        shinyWidgets::updatePrettyCheckbox(
          inputId = "rescue_no_trna",
          value = isTRUE(as.logical(cur$rescue_no_trna %||% 1L))
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
          inputId = "use_mitofinder",
          value = isTRUE(as.logical(cur$use_mitofinder %||% 0L))
        )
        updateTextInput(
          inputId = "mitofinder_db",
          value = cur$mitofinder_db %||% ""
        )
        shinyWidgets::updatePrettyCheckbox(
          inputId = "mitofinder_new_genes",
          value = isTRUE(as.logical(cur$mitofinder_new_genes %||% 0L))
        )
        shinyWidgets::updatePrettyCheckbox(
          inputId = "mitofinder_allow_introns",
          value = isTRUE(as.logical(cur$mitofinder_allow_introns %||% 0L))
        )
        updateTextInput(
          inputId = "mitofinder_opts",
          value = cur$mitofinder_opts %||% ""
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
          inputId = "ref_based_rc",
          value = isTRUE(as.logical(cur$ref_based_rc %||% 0L))
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
            create = FALSE,
            maxItems = 1
          )
        )
      }
    })
    observeEvent(input$edit_annotate_opts, ignoreInit = T, {
      shinyjs::toggleState("annotate_opts_cpus", condition = input$edit_annotate_opts)
      shinyjs::toggleState("annotate_opts_memory", condition = input$edit_annotate_opts)
      shinyjs::toggleState("use_mitos", condition = input$edit_annotate_opts)
      shinyjs::toggleState("mitos_opts", condition = input$edit_annotate_opts)
      shinyjs::toggleState("use_mitos_best", condition = input$edit_annotate_opts)
      shinyjs::toggleState("rescue_no_trna", condition = input$edit_annotate_opts)
      # shinyjs::toggleState("mitos_ref_dir", condition = input$edit_annotate_opts) # TODO: custom / alt ref db for mitos
      shinyjs::toggleState("mitos_ref_db", condition = input$edit_annotate_opts)
      shinyjs::toggleState("use_trnaScan", condition = input$edit_annotate_opts)
      shinyjs::toggleState("trnaScan_opts", condition = input$edit_annotate_opts)
      shinyjs::toggleState("use_arwen", condition = input$edit_annotate_opts)
      shinyjs::toggleState("arwen_opts", condition = input$edit_annotate_opts)
      shinyjs::toggleState("use_aragorn", condition = input$edit_annotate_opts)
      shinyjs::toggleState("aragorn_opts", condition = input$edit_annotate_opts)
      shinyjs::toggleState("use_mitofinder", condition = input$edit_annotate_opts)
      shinyjs::toggleState("mitofinder_db", condition = input$edit_annotate_opts)
      shinyjs::toggleState("mitofinder_new_genes", condition = input$edit_annotate_opts)
      shinyjs::toggleState("mitofinder_allow_introns", condition = input$edit_annotate_opts)
      shinyjs::toggleState("mitofinder_opts", condition = input$edit_annotate_opts)
      shinyjs::toggleState("coverage_trim", condition = input$edit_annotate_opts)
      shinyjs::toggleState("feature_trim", condition = input$edit_annotate_opts)
      shinyjs::toggleState("ref_based_rc", condition = input$edit_annotate_opts)
      shinyjs::toggleState("retain_low_conf_trna", condition = input$edit_annotate_opts)
      shinyjs::toggleState("start_gene", condition = input$edit_annotate_opts)
      # Check if editing opts that apply beyond selection
      if (input$edit_annotate_opts && input$annotate_opts %in% filtered_data()$annotate_opts) {
        rv$updating_indirect <- filtered_data() |>
          dplyr::filter(annotate_opts == input$annotate_opts) |>
          dplyr::anti_join(rv$updating, by = c("ID", "path", "scaffold"))
        # Prevent editing opts that apply to locked samples
        if (nrow(rv$updating_indirect) > 0L && any(rv$updating_indirect$annotate_lock == 1)) {
          mp_alert(
            title = "Locked assemblies use this parameter set",
            text = paste0(
              "This parameter set is also used by locked assemblies, so it ",
              "cannot be edited: ",
              mp_id_list(unique(rv$updating_indirect$ID[rv$updating_indirect$annotate_lock == 1])),
              ". Unlock them, or type a new parameter set name to create a copy."
            ),
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
          mp_confirm(
            "editing_annotate_opts_indirect",
            title = "Edit beyond the selection",
            text = paste(
              "These annotation options are also used by",
              mp_n(nrow(rv$updating_indirect), "assembly"),
              "outside the current selection. Editing them changes those too."
            ),
            action_label = "Edit anyway"
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
    # Show each tool's option inputs only when that tool is enabled; the
    # "use <tool>" toggle itself always stays visible.
    observeEvent(input$use_mitos, {
      on <- isTRUE(input$use_mitos)
      shinyjs::toggle("mitos_opts_box", condition = on)
      shinyjs::toggle("mitos_best_box", condition = on)
      shinyjs::toggle("mitos_ref_box", condition = on)
    })
    observeEvent(input$use_trnaScan, {
      shinyjs::toggle("trnascan_opts_box", condition = isTRUE(input$use_trnaScan))
    })
    observeEvent(input$use_mitofinder, {
      shinyjs::toggle("mitofinder_box", condition = isTRUE(input$use_mitofinder))
    })
    observeEvent(input$use_arwen, {
      shinyjs::toggle("arwen_box", condition = isTRUE(input$use_arwen))
    })
    observeEvent(input$use_aragorn, {
      shinyjs::toggle("aragorn_box", condition = isTRUE(input$use_aragorn))
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
              use_mitos = as.integer(isTRUE(input$use_mitos)),
              mitos_opts = req(input$mitos_opts),
              use_mitos_best = as.integer(isTRUE(input$use_mitos_best)),
              rescue_no_trna = as.integer(isTRUE(input$rescue_no_trna)),
              ref_dir = req(input$mitos_ref_dir),
              ref_db = req(input$mitos_ref_db),
              use_trnaScan = as.integer(isTRUE(input$use_trnaScan)),
              trnaScan_opts = req(input$trnaScan_opts),
              arwen_opts = req(input$arwen_opts),
              use_arwen = as.integer(isTRUE(input$use_arwen)),
              aragorn_opts = req(input$aragorn_opts),
              use_aragorn = as.integer(isTRUE(input$use_aragorn)),
              use_mitofinder = as.integer(isTRUE(input$use_mitofinder)),
              mitofinder_db = if (nzchar(input$mitofinder_db %||% "")) input$mitofinder_db else NA_character_,
              mitofinder_new_genes = as.integer(isTRUE(input$mitofinder_new_genes)),
              mitofinder_allow_introns = as.integer(isTRUE(input$mitofinder_allow_introns)),
              mitofinder_opts = input$mitofinder_opts %||% "",
              start_gene = req(input$start_gene),
              coverage_trim = as.integer(isTRUE(input$coverage_trim)),
              feature_trim = as.integer(isTRUE(input$feature_trim)),
              ref_based_rc = as.integer(isTRUE(input$ref_based_rc)),
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
      # Per-unit: target the exact (ID, path, scaffold) units (selected + indirect).
      update <- dplyr::bind_rows(
        rv$updating[, c("ID", "path", "scaffold")],
        rv$updating_indirect[, c("ID", "path", "scaffold")]
      )
      update$annotate_opts <- input$annotate_opts
      update$annotate_switch <- 1
      dplyr::tbl(session$userData$con, "annotate") |>
        dplyr::rows_update(
          update,
          unmatched = "ignore",
          in_place = TRUE,
          copy = TRUE,
          by = c("ID", "path", "scaffold")
        )
      rv$data <- filtered_data() |>
        dplyr::rows_update(
          update,
          by = c("ID", "path", "scaffold")
        )
      rv$updating <- rv$updating_indirect <- NULL
      removeModal()
      trigger("update_annotate_table")
      mp_opts_saved_toast(nrow(update), input$annotate_opts, "assembly")
    })

    # Set Curate Options ----
    observeEvent(input$set_curate_opts, {
      row <- as.numeric(input$set_curate_opts)
      d <- filtered_data()
      if (!row_in_selection(row, selected(), d$ID[row], "assembly")) return()
      selected <- c(row, selected()) |> unique()
      if (!need_unlocked(d$ID[selected][d$annotate_lock[selected] == 1], "assembly")) return()
      rv$updating <- d |> dplyr::slice(selected)
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
            create = FALSE,
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
          choices = c("Metazoa_RefSeq235", "Metazoa_RefSeq235_custom", "Metazoa_RefSeq231", "Metazoa_RefSeq231_custom", "Metazoa_RefSeq89", "Chordata", "Chordata_custom"),
          options = list(
            create = TRUE,
            maxItems = 1
          )
        )
        shinyWidgets::updatePrettyCheckbox(
          inputId = "linear_complete",
          value = isTRUE(as.integer(cur$linear_complete %||% 0L) == 1L)
        )
        rv$params <- cur$params |> jsonlite::fromJSON()
      }
    })
    output$params <- listviewer::renderReactjson({
      listviewer::reactjson(
        req(rv$params),
        "Validation Parameters",
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
      shinyjs::toggleState("genetic_code", condition = input$edit_curate_opts)
      shinyjs::toggleState("start_gene", condition = input$edit_curate_opts)
      shinyjs::toggleState("linear_complete", condition = input$edit_curate_opts)
      # Check if editing opts that apply beyond selection
      if (input$edit_curate_opts && input$curate_opts %in% filtered_data()$curate_opts) {
        rv$updating_indirect <- filtered_data() |>
          dplyr::filter(curate_opts == input$curate_opts) |>
          dplyr::anti_join(rv$updating, by = c("ID", "path", "scaffold"))
        # Prevent editing opts that apply to locked samples
        if (nrow(rv$updating_indirect) > 0L && any(rv$updating_indirect$annotate_lock == 1)) {
          mp_alert(
            title = "Locked assemblies use this parameter set",
            text = paste0(
              "This parameter set is also used by locked assemblies, so it ",
              "cannot be edited: ",
              mp_id_list(unique(rv$updating_indirect$ID[rv$updating_indirect$annotate_lock == 1])),
              ". Unlock them, or type a new parameter set name to create a copy."
            ),
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
          mp_confirm(
            "editing_curate_opts_indirect",
            title = "Edit beyond the selection",
            text = paste(
              "These curation options are also used by",
              mp_n(nrow(rv$updating_indirect), "assembly"),
              "outside the current selection. Editing them changes those too."
            ),
            action_label = "Edit anyway"
          )
        }
      } else {
        rv$updating_indirect <- rv$updating |> dplyr::slice(0)
      }
    })
    observeEvent(input$target, {
      # Guard against partial/invalid entries (e.g. while typing): only known
      # rulesets dispatch to params_<target>/curate_<target> functions.
      req(input$target %in% names(RULESET_MAP))
      rv$params <- do.call(paste0("params_", input$target), list()) |>
        jsonlite::toJSON(auto_unbox = TRUE)
      # Refresh the "Auto" label to show the code this target resolves to,
      # keeping any explicit override the user already selected.
      updateSelectizeInput(
        session,
        "genetic_code",
        choices = gcode_choices(input$target),
        selected = input$genetic_code %||% "auto"
      )
      output$params <- listviewer::renderReactjson({
        listviewer::reactjson(
          req(rv$params),
          "Validation Parameters",
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
        # Target must be a known ruleset (dispatches to params_<target>); block
        # save on a cleared/invalid selection rather than erroring.
        if (!isTRUE(input$target %in% names(RULESET_MAP))) {
          mp_alert(
            title = "No curation ruleset chosen",
            text = "Select a curation ruleset before saving these options.",
            type = "error"
          )
          return()
        }
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
              target = req(input$target),
              linear_complete = as.integer(isTRUE(input$linear_complete)),
              # "auto" (or empty) = auto-from-ruleset (NA); a number is an override.
              genetic_code = if (is.null(input$genetic_code) ||
                                 input$genetic_code %in% c("", "auto")) {
                NA_integer_
              } else {
                as.integer(input$genetic_code)
              }
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
      update <- dplyr::bind_rows(
        rv$updating[, c("ID", "path", "scaffold")],
        rv$updating_indirect[, c("ID", "path", "scaffold")]
      )
      update$curate_opts <- input$curate_opts
      update$annotate_switch <- 1
      dplyr::tbl(session$userData$con, "annotate") |>
        dplyr::rows_update(
          update,
          unmatched = "ignore",
          in_place = TRUE,
          copy = TRUE,
          by = c("ID", "path", "scaffold")
        )
      rv$data <- filtered_data() |>
        dplyr::rows_update(
          update,
          by = c("ID", "path", "scaffold")
        )
      # Genetic code follows the curation ruleset: recompute the per-sample
      # samples.genetic_code cache for the samples whose curate_opts (target or
      # override) may have changed.
      .sync_sample_genetic_codes(session$userData$con, ids = update$ID)
      rv$updating <- rv$updating_indirect <- NULL
      removeModal()
      trigger("update_annotate_table")
      mp_opts_saved_toast(nrow(update), input$curate_opts, "assembly")
    })

    # Set ORF Options ----
    observeEvent(input$set_orf_opts, {
      row <- as.numeric(input$set_orf_opts)
      d <- filtered_data()
      if (!row_in_selection(row, selected(), d$ID[row], "assembly")) return()
      selected <- c(row, selected()) |> unique()
      if (!need_unlocked(d$ID[selected][d$annotate_lock[selected] == 1], "assembly")) return()
      rv$updating <- d |> dplyr::slice(selected)
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
        updateNumericInput(inputId = "orf_min_len", value = cur$orf_min_len)
        updateNumericInput(inputId = "orf_max_overlap", value = cur$orf_max_overlap)
        updateTextInput(inputId = "orffinder_opts", value = cur$orffinder_opts)
      }
    })
    # Hide the per-run ORF parameters when ORF finding is turned off.
    observeEvent(input$use_orffinder, ignoreInit = F, {
      shinyjs::toggle("orf_param_opts", condition = isTRUE(input$use_orffinder))
    })
    observeEvent(input$edit_orf_opts, ignoreInit = T, {
      shinyjs::toggleState("use_orffinder", condition = input$edit_orf_opts)
      shinyjs::toggleState("orf_opts_cpus", condition = input$edit_orf_opts)
      shinyjs::toggleState("orf_opts_memory", condition = input$edit_orf_opts)
      shinyjs::toggleState("orf_min_len", condition = input$edit_orf_opts)
      shinyjs::toggleState("orf_max_overlap", condition = input$edit_orf_opts)
      shinyjs::toggleState("orf_nested", condition = input$edit_orf_opts)
      shinyjs::toggleState("orffinder_opts", condition = input$edit_orf_opts)
      # Check if editing opts that apply beyond selection
      if (input$edit_orf_opts && input$orf_opts %in% filtered_data()$orf_opts) {
        rv$updating_indirect <- filtered_data() |>
          dplyr::filter(orf_opts == input$orf_opts) |>
          dplyr::anti_join(rv$updating, by = c("ID", "path", "scaffold"))
        if (nrow(rv$updating_indirect) > 0L && any(rv$updating_indirect$annotate_lock == 1)) {
          mp_alert(
            title = "Locked assemblies use this parameter set",
            text = paste0(
              "This parameter set is also used by locked assemblies, so it ",
              "cannot be edited: ",
              mp_id_list(unique(rv$updating_indirect$ID[rv$updating_indirect$annotate_lock == 1])),
              ". Unlock them, or type a new parameter set name to create a copy."
            ),
            type = "warning"
          )
          shinyWidgets::updatePrettyCheckbox(inputId = "edit_orf_opts", value = FALSE)
          req(F)
        }
        if (nrow(rv$updating_indirect) > 0L) {
          mp_confirm(
            "editing_orf_opts_indirect",
            title = "Edit beyond the selection",
            text = paste(
              "These ORF options are also used by",
              mp_n(nrow(rv$updating_indirect), "assembly"),
              "outside the current selection. Editing them changes those too."
            ),
            action_label = "Edit anyway"
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
              orf_nested = as.integer(isTRUE(input$orf_nested))
            ),
            in_place = TRUE,
            copy = TRUE,
            by = "orf_opts"
          )
        rv$orf_opts <- dplyr::tbl(session$userData$con, "orf_opts") |>
          dplyr::collect()
      }
      update <- dplyr::bind_rows(
        rv$updating[, c("ID", "path", "scaffold")],
        rv$updating_indirect[, c("ID", "path", "scaffold")]
      )
      update$orf_opts <- input$orf_opts
      update$annotate_switch <- 1
      dplyr::tbl(session$userData$con, "annotate") |>
        dplyr::rows_update(
          update,
          unmatched = "ignore",
          in_place = TRUE,
          copy = TRUE,
          by = c("ID", "path", "scaffold")
        )
      rv$data <- filtered_data() |>
        dplyr::rows_update(update, by = c("ID", "path", "scaffold"))
      rv$updating <- rv$updating_indirect <- NULL
      removeModal()
      trigger("update_annotate_table")
      mp_opts_saved_toast(nrow(update), input$orf_opts, "assembly")
    })
    # Open output folder ----
    observeEvent(input$output, ignoreInit = T, {
      pth <- file.path(
        session$userData$dir_out,
        filtered_data()$ID[as.numeric(input$output)],
        "annotate"
      )
      open_path(pth)
    })

    # Open annotation details ----
    # Each table row is one (ID, path, scaffold) unit, so the clicked row is the
    # unit to edit.
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
      # If the jump specifies a unit, honour it; else take the first unit.
      if (!is.null(target$path) && !is.null(target$scaffold)) {
        hit <- hit |> dplyr::filter(path == target$path, scaffold == target$scaffold)
      }
      req(nrow(hit) > 0)
      session$userData$in_outlier_review <- TRUE
      rv$updating <- hit |> dplyr::slice(1)
      trigger("annotations_modal")
    })

    annotations_details_server(ns("annotations"), rv, table_id = ns("table"))

    # CSV Export ----
    .export_cols_drop <- c("output", "view", "poor_blast_ref", "warnings_details", "blast_accession_auto")

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
