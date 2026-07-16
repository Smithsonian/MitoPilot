# Togglable column groups for the Export table. Cols not listed here
# (sticky cols, action buttons) are always shown.
EXPORT_COL_GROUPS <- list(
  Options = c("curate_opts"),
  Stats   = c("topology", "structure"),
  BLAST   = c("blast_accession", "blast_ref_status", "blast_species",
              "blast_lineage")
)
EXPORT_COL_GROUP_LOOKUP <- {
  out <- character()
  for (.g in names(EXPORT_COL_GROUPS)) {
    for (.c in EXPORT_COL_GROUPS[[.g]]) out[.c] <- .g
  }
  out
}

# Inline grey "?" help icon matching the tool-help icons (tool_help_icon),
# but as a plain hover tooltip (native title) rather than a help modal.
export_help_icon <- function(tip) {
  shiny::icon(
    "circle-question",
    title = tip,
    style = "color: #888; margin-left: 4px; cursor: help;"
  )
}

# Label text followed by the help icon, for input labels and table headers.
export_help_label <- function(label, tip) {
  htmltools::tagList(label, export_help_icon(tip))
}

#' export UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
export_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("col_css")),
    div(
      style = "display: flex; align-items: flex-end; gap: 20px; flex-wrap: wrap;",
      shinyWidgets::pickerInput(
        inputId  = ns("col_groups"),
        width    = "150px",
        label    = "Show columns:",
        choices  = names(EXPORT_COL_GROUPS),
        selected = names(EXPORT_COL_GROUPS),
        multiple = TRUE,
        options  = list(
          `actions-box`          = TRUE,
          `select-all-text`      = "All",
          `deselect-all-text`    = "None",
          `selected-text-format` = "count > 0",
          width                  = "150px"
        )
      ),
      shinyWidgets::pickerInput(
        inputId  = ns("export_filter"),
        width    = "140px",
        label    = "Exported:",
        choices  = ANNOTATE_EXPORT_CHOICES,
        selected = ANNOTATE_EXPORT_CHOICES,
        multiple = TRUE,
        options  = list(
          `actions-box`          = TRUE,
          `select-all-text`      = "All",
          `deselect-all-text`    = "None",
          `selected-text-format` = "count > 0",
          width                  = "140px"
        )
      )
    ),
    div(class = "mp-table-resize", reactableOutput(ns("table"))),
    div(
      style = "font-size: 0.85em; color: #555; margin-top: 4px;",
      textOutput(ns("n_selected"), inline = TRUE)
    ),
    div(
      style = "margin-top: 12px; display: flex; gap: 8px;",
      downloadButton(ns("export_selected"), "Export Selected to CSV",
                     class = "btn-sm btn-default"),
      downloadButton(ns("export_all"), "Export All to CSV",
                     class = "btn-sm btn-default")
    )
  )
}

#' export Server Functions
#'
#' @noRd
export_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Prepare data ----
    rv <- reactiveValues(
      # curate_opts = dplyr::tbl(session$userData$con, "curate_opts") |>
      #  dplyr::collect(),
      data = fetch_export_data(),
      updating = NULL,
      outliers = NULL,    # flags tibble from flag_PCG_outliers()
      alns = NULL,        # named list of aligned AAStringSet (by gene)
      review_genes = NULL, # genes still pending review (drives navigation)
      review_idx = 1L,     # cursor into review_genes
      resolved = character(0), # "ID|gene" keys the user marked resolved;
                              # persists across flag/alignment recomputes
      # Currently selected header-template name, so the modal reopens with it
      export_template = "default",
      # Last-used review options, so the modal reopens with them (not defaults)
      opt_review = TRUE,
      opt_start = 10,
      opt_stop = 10,
      opt_ident = 60
    )

    # Refresh ----
    init("refresh_export")
    on("refresh_export", {
      rv$data <- fetch_export_data()
      trigger("update_export_table")
    })

    # Mirror the column-group picker so NULL (= user cleared all) is
    # distinguishable from the pre-init state. Default: all groups on.
    col_groups_rv <- reactiveVal(names(EXPORT_COL_GROUPS))
    observeEvent(input$col_groups, {
      col_groups_rv(input$col_groups %||% character(0))
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
    # Exported Yes/No filter. Rows are tagged with mp-exp-<0/1> (see rowClass)
    # so unselected states hide via CSS, same mechanism as the column picker.
    export_filter_rv <- reactiveVal(unname(ANNOTATE_EXPORT_CHOICES))
    observeEvent(input$export_filter, {
      export_filter_rv(input$export_filter %||% character(0))
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    .grp <- function(col) {
      g <- EXPORT_COL_GROUP_LOOKUP[col]
      if (is.na(g)) NULL else paste0("mp-grp-", g)
    }

    # CSS hide for unselected groups / exported states; keeps DOM intact so
    # filter/sort/page state survives toggling.
    output$col_css <- renderUI({
      hidden     <- setdiff(names(EXPORT_COL_GROUPS), col_groups_rv())
      hidden_exp <- setdiff(unname(ANNOTATE_EXPORT_CHOICES), export_filter_rv())
      # Hide path/scaffold when every unit shares value 1 (no extra info), mirroring
      # the Annotate table. Reactive on rv$data so they appear as soon as a
      # multi-unit sample is locked.
      d <- rv$data
      hide_path     <- !is.null(d) && nrow(d) > 0 && all(d$path == 1, na.rm = TRUE)
      hide_scaffold <- !is.null(d) && nrow(d) > 0 && all(d$scaffold == 1, na.rm = TRUE)
      # SeqID only carries information once it diverges from the ID
      hide_seqid    <- !is.null(d) && nrow(d) > 0 && all(d$seqid == d$ID, na.rm = TRUE)
      # Scope to THIS module's table so rules don't hit the shared mp-grp /
      # mp-exp classes on the assemble, userAsmb, and annotate tables.
      sel <- paste0("#", ns("table"), " ")
      rules <- c(
        if (hide_path)          paste0(sel, ".mp-col-path { display: none !important; }"),
        if (hide_scaffold)      paste0(sel, ".mp-col-scaffold { display: none !important; }"),
        if (hide_seqid)         paste0(sel, ".mp-col-seqid { display: none !important; }"),
        if (length(hidden))     paste0(sel, ".mp-grp-", hidden, " { display: none !important; }"),
        if (length(hidden_exp)) paste0(sel, ".mp-exp-", hidden_exp, " { display: none !important; }")
      )
      if (length(rules) == 0) return(NULL)
      tags$style(HTML(paste(rules, collapse = "\n")))
    })

    # Render table ----
    output$table <- reactable::renderReactable({
      reactable::reactable(
        isolate(rv$data),
        compact = TRUE,
        language = reactable::reactableLang(
          noData = "No Completed / Locked Annotations Found"
        ),
        defaultPageSize = 100,
        showPageSizeOptions = TRUE,
        onClick = "select",
        selection = "multiple",
        searchable = TRUE,
        resizable = TRUE,
        filterable = TRUE,
        height = "100%",
        wrap = FALSE,
        pageSizeOptions = c(25, 50, 100, 200, 500),
        striped = TRUE,
        rowStyle = rt_highlight_row(),
        # Tag rows exported (mp-exp-1) vs not (mp-exp-0) so the Exported picker
        # can hide unselected states via CSS.
        rowClass = htmlwidgets::JS("function(rowInfo) {
          if (!rowInfo || !rowInfo.values) return '';
          var ets = rowInfo.values['export_time_stamp'];
          return 'mp-exp-' + ((ets != null && ets !== '') ? '1' : '0');
        }"),
        defaultColDef = colDef(align = "left"),
        columns = list(
          ID = colDef(show = T, minWidth = 120, sticky = "left"),
          # One row per assembly unit; the classes let col_css hide these when every
          # unit shares value 1.
          path = colDef(
            show = TRUE, name = "Path", class = "mp-col-path",
            headerClass = "mp-col-path", width = 55, align = "center"
          ),
          scaffold = colDef(
            show = TRUE, name = "Scaffold", class = "mp-col-scaffold",
            headerClass = "mp-col-scaffold", width = 75, align = "center"
          ),
          # The GenBank record name this unit exports under; hidden when it is just
          # the ID (no fragmented sample in the project).
          seqid = colDef(
            show = TRUE, name = "SeqID", class = "mp-col-seqid",
            headerClass = "mp-col-seqid", minWidth = 130
          ),
          Taxon = colDef(show = T, name = "Taxon", minWidth = 140, html = TRUE, cell = rt_longtext()),
          curate_opts = colDef(
            show = TRUE, class = .grp("curate_opts"), headerClass = .grp("curate_opts"),
            name = "Curate Opts.",
            width = 110
          ),
          genetic_code = colDef(show = T, name = "Genetic Code", align = "center", width = 110),
          poor_blast_ref = colDef(show = FALSE),
          partial = colDef(show = FALSE),
          completeness = colDef(show = FALSE),
          length = colDef(show = FALSE),
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
            name = "BLAST Hit",
            html = TRUE,
            width = 120,
            cell = rt_ncbi_link(auto_col = "blast_accession_auto")
          ),
          blast_accession_auto = colDef(show = FALSE),
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
          topology = colDef(
            show = T, class = .grp("topology"), headerClass = .grp("topology"),
            name = "Topology", width = 100
          ),
          structure = colDef(
            show = T, class = .grp("structure"), headerClass = .grp("structure"),
            name = "Structure"
          ),
          PCGCount = colDef(show = T, name = "# PCGs", align = "center"),
          tRNACount = colDef(show = T, name = "# tRNAs", align = "center"),
          rRNACount = colDef(show = T, name = "# rRNAs", align = "center"),
          ORFCount = colDef(show = T, name = "# ORFs", align = "center"),
          missing = colDef(show = T, name = "Missing", align = "left", html = TRUE, cell = rt_longtext()),
          extra = colDef(show = T, name = "Extra", align = "left", html = TRUE, cell = rt_longtext()),
          warnings = colDef(show = T, name = "Warnings", align = "left", html = TRUE, cell = rt_longtext()),
          export_time_stamp = colDef(
            show = T, name = "Exported", html = TRUE, width = 150,
            filterable = FALSE, cell = rt_ts_date()
          ),
          export_group = colDef(show = T, name = "Export Group", sticky = "right")
        )
      )
    })

    # update table ----
    init("update_export_table")
    on("update_export_table", {
      reactable::updateReactable(
        "table",
        data = rv$data,
        selected = reactable::getReactableState("table", "selected"),
        page = reactable::getReactableState("table", "page")
      )
    })

    # table selection ----
    # Rows hidden by the Exported filter stay mounted, so drop them from the
    # selection so bulk export only touches visible samples.
    selected <- reactive({
      sel <- reactable::getReactableState("table", "selected")
      if (is.null(sel) || length(sel) == 0) return(sel)
      exp_code <- ifelse(is.na(rv$data$export_time_stamp), "0", "1")
      visible <- exp_code %in% export_filter_rv()
      intersect(sel, which(visible))
    })

    output$n_selected <- renderText({
      paste0(length(selected()), " selected")
    })

    # Publish current selection so the work-dir browser can pre-select this sample
    observe({
      session$userData$wd_selected[["Export"]] <- unique(rv$data$ID[selected()])
    })

    # CSV Export ----
    .export_cols_drop <- c("poor_blast_ref", "blast_accession_auto")

    observe({
      shinyjs::toggleState("export_selected", condition = length(selected()) > 0)
    })

    output$export_selected <- downloadHandler(
      filename = function() paste0("export_selected_", Sys.Date(), ".csv"),
      content = function(file) {
        req(length(selected()) > 0)
        rv$data |>
          dplyr::slice(selected()) |>
          dplyr::select(-dplyr::any_of(.export_cols_drop)) |>
          write.csv(file, row.names = FALSE)
      }
    )

    output$export_all <- downloadHandler(
      filename = function() paste0("export_all_", Sys.Date(), ".csv"),
      content = function(file) {
        rv$data |>
          dplyr::select(-dplyr::any_of(.export_cols_drop)) |>
          write.csv(file, row.names = FALSE)
      }
    )

    # Group ----
    init("group")
    on("group", {
      req(session$userData$mode == "Export")
      req(selected())
      if (any(!is.na(rv$data$export_group[selected()]))) {
        shinyWidgets::confirmSweetAlert(
          title = "Re-assign group?",
          text = "Some selected samples are already assigned to an export group. Assigning them to a new group will not automatically remove them from previously generated export files. Do you want to continue?",
          inputId = ns("group_confirm"),
          btn_labels = c("No", "Yes"),
          btn_colors = c("#0056b3", "#0056b3")
        )
        req(F)
      }
      trigger("group_modal")
    })
    observeEvent(input$group_confirm, {
      req(input$group_confirm)
      trigger("group_modal")
    })

    # Clear Group ----
    # Remove the export_group assignment from any selected samples that currently
    # have one. Always-visible button; a no-group selection is a silent no-op.
    init("clear_group")
    on("clear_group", {
      req(session$userData$mode == "Export")
      req(selected())
      rv$updating <- rv$data |> dplyr::slice(selected())
      if (!any(!is.na(rv$updating$export_group))) {
        req(FALSE)
      }
      shinyWidgets::confirmSweetAlert(
        title = "Clear group?",
        text = "Remove the selected samples from their export group? This will not automatically remove them from previously generated export files.",
        inputId = ns("clear_group_confirm"),
        btn_labels = c("No", "Yes"),
        btn_colors = c("#0056b3", "#0056b3")
      )
    })
    observeEvent(input$clear_group_confirm, {
      req(input$clear_group_confirm)
      assign_export_group(rep(NA_character_, nrow(rv$updating)))
    })

    init("group_modal")
    on("group_modal", {
      rv$updating <- rv$data |> dplyr::slice(selected())
      topologies <- rv$updating |>
        dplyr::pull(topology) |>
        unique()
      structures <- rv$updating |>
        dplyr::pull(structure) |>
        unique()
      group_current <- rv$updating |>
        dplyr::pull(export_group) |>
        unique()
      modalDialog(
        title = "Submission Group",
        size = "l",
        easyClose = FALSE,
        stringr::str_glue(
          "<b># Selected:</b> {nrow(rv$updating)}"
        ) |> HTML() |> p(),
        stringr::str_glue(
          "<b># Topology:</b> {paste(topologies, collapse=', ')}"
        ) |> HTML() |> p(),
        HTML("<b>Structure:") |> p(),
        list_to_li(structures),
        hr(),
        selectizeInput(
          ns("group_name"),
          label = "Group Name:",
          choices = c("", sort(unique(rv$data$export_group))),
          selected = character(0),
          options = list(
            create = TRUE,
            maxItems = 1
          )
        ),
        footer = tagList(
          actionButton(ns("make_group"), "Create"),
          modalButton("Close")
        )
      ) |> showModal()
    })

    # Make Group ----
    # Persist an export_group assignment for the current selection (rv$updating)
    # and refresh the table. `groups` is a character vector aligned to the rows
    # of rv$updating.
    assign_export_group <- function(groups) {
      rv$updating$export_group <- groups
      unit_key <- c("ID", "path", "scaffold")
      upd <- rv$updating[, c(unit_key, "export_group")]
      rv$data <- rv$data |>
        dplyr::rows_update(upd, by = unit_key)
      # Export state is keyed per unit, so selecting several scaffolds of one sample
      # yields distinct keys rather than duplicate IDs. Upsert, not update: a unit
      # that has never been grouped has no export row yet.
      dplyr::tbl(session$userData$con, "export") |>
        dplyr::rows_upsert(
          upd,
          in_place = TRUE,
          copy = TRUE,
          by = unit_key
        )
      trigger("update_export_table")
      removeModal()
    }

    observeEvent(input$make_group, {
      name <- req(input$group_name)
      if (any(!(grepl("^[a-zA-Z0-9_-]+$", name)))) {
        shinyWidgets::sendSweetAlert(
          title = "Invalid group name",
          text = "Group names must contain only alphanumeric characters, dashes, or underscores",
          type = "error"
        )
        return()
      }
      # GenBank submissions may contain only complete OR only partial
      # mitogenomes, never both. Warn on a mixed selection and offer to split it
      # into "<name>-complete" and "<name>-partial".
      n_complete <- sum(rv$updating$completeness == "complete genome", na.rm = TRUE)
      n_partial  <- sum(rv$updating$completeness == "partial genome", na.rm = TRUE)
      if (n_complete > 0 && n_partial > 0) {
        rv$pending_group_name <- name
        modalDialog(
          title = "Mixed complete and partial mitogenomes",
          size = "m",
          easyClose = FALSE,
          HTML(stringr::str_glue(
            "A GenBank submission may contain only complete <i>or</i> only ",
            "partial mitogenomes, not both. This selection has {n_complete} ",
            "complete and {n_partial} partial. Split into two groups, ",
            "'{name}-complete' and '{name}-partial', or keep them as one group?"
          )),
          footer = tagList(
            actionButton(ns("group_split"), "Split into two groups", class = "btn-primary"),
            actionButton(ns("group_keep_one"), "Keep as one mixed group"),
            actionButton(ns("group_back"), "Cancel")
          )
        ) |> showModal()
        return()
      }
      assign_export_group(rep(name, nrow(rv$updating)))
    })

    # Mixed-group warning actions.
    # Split into separate complete / partial export groups.
    observeEvent(input$group_split, {
      name <- req(rv$pending_group_name)
      groups <- ifelse(
        rv$updating$completeness == "complete genome",
        paste0(name, "-complete"),
        paste0(name, "-partial")
      )
      assign_export_group(groups)
      rv$pending_group_name <- NULL
    })
    # Override: keep the mixed selection as a single group.
    observeEvent(input$group_keep_one, {
      name <- req(rv$pending_group_name)
      assign_export_group(rep(name, nrow(rv$updating)))
      rv$pending_group_name <- NULL
    })
    # Cancel: return to the group-name modal so the selection can be adjusted.
    observeEvent(input$group_back, {
      rv$pending_group_name <- NULL
      trigger("group_modal")
    })

    # Export data ----
    init("export")
    on("export", {
      req(nrow(rv$data) > 0)
      choices <- sort(unique(rv$data$export_group))
      req(length(choices) > 0)
      # Saved templates + the currently selected one's header strings, plus the
      # columns available to reference
      con <- session$userData$con
      tmpl_choices <- list_export_templates(con)
      sel_tmpl <- if (rv$export_template %in% tmpl_choices) rv$export_template else "default"
      rv$export_template <- sel_tmpl
      opts <- get_export_opts(con, sel_tmpl)
      avail_cols <- paste(sort(names(rv$data)), collapse = ", ")
      cols_help <- p(
        style = "color: #666; font-size: 0.8em; margin: 0.25em 0 0.75em;",
        tags$b("Available columns: "), avail_cols
      )
      completeness_help <- p(
        style = "color: #666; font-size: 0.8em; margin: 0.25em 0 0.75em;",
        tags$b("{completeness}"),
        " expands to \"complete genome\" or \"partial genome\", auto-derived from ",
        "each assembly's topology (circular = complete, linear = partial), unless ",
        "overridden by the per-sample Partial flag (forces partial) or the ",
        "curation \"linear complete\" setting (forces linear assemblies to ",
        "complete). For correct GenBank submission, place {completeness} at the ",
        "end of the header."
      )
      modalDialog(
        title = div(
          style = "display: flex; justify-content: space-between; align-items: center; height: 42px;",
          span("Export Data"),
          span(id = ns("gears"), class = "gears paused")
        ),
        size = "l",
        # Export group + header-template selector share one row at equal width.
        # The template dropdown: pick a saved set to load, or type a new name to
        # create one from the current header boxes (like the analysis-opts
        # parameter-set dropdowns). Edits auto-save to the selected name.
        div(
          style = "display: flex; flex-flow: row nowrap; gap: 1em;",
          div(
            style = "flex: 1; min-width: 0;",
            shinyWidgets::pickerInput(
              ns("export_group"),
              "Export Group:",
              choices = choices,
              width = "100%"
            )
          ),
          div(
            style = "flex: 1; min-width: 0;",
            selectizeInput(
              ns("template_select"),
              "Header Template:",
              choices = tmpl_choices,
              selected = sel_tmpl,
              width = "100%",
              options = list(
                create = TRUE,
                maxItems = 1,
                placeholder = "select or type a new template name"
              )
            )
          )
        ),
        opts_help("Export Group bundles samples into one output set; Header Template ",
                  "is a reusable, named set of the FASTA header patterns below (type a ",
                  "new name to save one)."),
        tags$label(
          class = "control-label",
          "Mitogenome FASTA Header (reference columns from your sample data using '{}'):"
        ),
        cols_help,
        completeness_help,
        uiOutput(ns("fasta_header_status")),
        textAreaInput(
          ns("fasta_header"),
          NULL,
          opts$fasta_header,
          width = "100%"
        ),
        shinyWidgets::prettyCheckbox(
          ns("include_alignments"),
          "Generate Group-level PCG alignment summary",
          value = T,
          status = "primary"
        ),
        shinyWidgets::prettyCheckbox(
          ns("export_genes"),
          "Export individual protein-coding and rRNA genes",
          value = F,
          status = "primary"
        ),
        tags$label(
          class = "control-label",
          "Gene FASTA Header (reference columns from your sample data using '{}', gene names will be automatically added):"
        ),
        cols_help,
        uiOutput(ns("fasta_header_gene_status")),
        textAreaInput(
          ns("fasta_header_gene"),
          NULL,
          opts$fasta_header_gene,
          width = "100%"
        ),
        # PCG outlier review options, separated from the export options above
        tags$hr(style = "border-top: 1px solid #ccc; margin: 1em 0 0.75em;"),
        h4("PCG Annotation Outlier Review", style = "margin-top: 0;"),
        shinyWidgets::prettyCheckbox(
          ns("review_outliers"),
          "Review PCG annotations for outliers",
          value = rv$opt_review,
          status = "primary"
        ),
        conditionalPanel(
          condition = "input.review_outliers == true",
          ns = ns,
          div(
            style = "display: flex; flex-flow: row nowrap; gap: 1em;",
            div(
              style = "flex: 1",
              numericInput(
                ns("start_aa"),
                export_help_label(
                  "Flag start offset > (aa):",
                  "Flag genes with start position offset by +/- this many amino acids from the core alignment"
                ),
                value = rv$opt_start, min = 1, step = 1, width = "100%"
              )
            ),
            div(
              style = "flex: 1",
              numericInput(
                ns("stop_aa"),
                export_help_label(
                  "Flag stop offset > (aa):",
                  "Flag genes with stop position offset by +/- this many amino acids from the core alignment"
                ),
                value = rv$opt_stop, min = 1, step = 1, width = "100%"
              )
            ),
            div(
              style = "flex: 1",
              numericInput(
                ns("ident_pct"),
                export_help_label(
                  "Flag sequence identity < (%):",
                  "Mean % identity threshold to flag a gene versus all other genes in alignment group"
                ),
                value = rv$opt_ident, min = 1, max = 100, step = 1, width = "100%"
              )
            )
          )
        ),
        footer = tagList(
          actionButton(ns("export_data"), "Export"),
          modalButton("Close")
        )
      ) |> showModal()
    })

    # Live header-template validation -----------------------------------------
    # Debounce so we validate after typing pauses, not on every keystroke.
    hdr_main <- shiny::debounce(reactive(input$fasta_header), 400)
    hdr_gene <- shiny::debounce(reactive(input$fasta_header_gene), 400)

    # Render a green/amber/red status line beneath a header textarea.
    # level: "ok" (green), "warn" (amber, non-blocking), "error" (red, blocks).
    render_hdr_status <- function(res) {
      style_for <- switch(
        res$level %||% if (isTRUE(res$ok)) "ok" else "error",
        ok    = list(col = "#28a745", ic = "circle-check"),
        warn  = list(col = "#e0a800", ic = "triangle-exclamation"),
        error = list(col = "#d9534f", ic = "circle-xmark")
      )
      span(
        style = sprintf("color: %s; font-size: 0.85em;", style_for$col),
        shiny::icon(style_for$ic), " ", res$message
      )
    }

    output$fasta_header_status <- renderUI({
      render_hdr_status(validate_fasta_header(hdr_main(), rv$data, require_completeness = TRUE))
    })
    output$fasta_header_gene_status <- renderUI({
      render_hdr_status(validate_fasta_header(hdr_gene(), rv$data))
    })

    # Template selector ----------------------------------------------------
    # Like the analysis-opts parameter-set dropdowns: picking an existing name
    # loads its header strings; typing a new name creates a template from the
    # current boxes and adds it to the dropdown.
    observeEvent(input$template_select, {
      req(input$template_select)
      name <- input$template_select
      rv$export_template <- name
      con <- session$userData$con
      if (name %in% list_export_templates(con)) {
        o <- get_export_opts(con, name)
        updateTextAreaInput(session, "fasta_header", value = o$fasta_header)
        updateTextAreaInput(session, "fasta_header_gene", value = o$fasta_header_gene)
      } else if (isTRUE(validate_fasta_header(input$fasta_header, rv$data)$ok) &&
                 isTRUE(validate_fasta_header(input$fasta_header_gene, rv$data)$ok)) {
        # New name typed: seed the template from the current (valid) boxes.
        set_export_opts(con, input$fasta_header, input$fasta_header_gene, name = name)
        updateSelectizeInput(
          session, "template_select",
          choices = list_export_templates(con), selected = name,
          options = list(create = TRUE, maxItems = 1)
        )
      }
    }, ignoreInit = TRUE)

    # Auto-save header edits to the currently selected template (when both are
    # valid). Reacts only to box edits, not selection, so loading a template
    # never clobbers it. Invalid templates are never persisted.
    observeEvent(list(hdr_main(), hdr_gene()), {
      name <- input$template_select
      req(name, name %in% list_export_templates(session$userData$con))
      if (isTRUE(validate_fasta_header(input$fasta_header, rv$data)$ok) &&
          isTRUE(validate_fasta_header(input$fasta_header_gene, rv$data)$ok)) {
        set_export_opts(
          session$userData$con, input$fasta_header, input$fasta_header_gene,
          name = name
        )
      }
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # Validate both header boxes; show an error alert and return FALSE if either
    # is invalid (so a bad template can never reach export).
    valid_headers_or_alert <- function() {
      v_main <- validate_fasta_header(input$fasta_header, rv$data)
      v_gene <- validate_fasta_header(input$fasta_header_gene, rv$data)
      if (!isTRUE(v_main$ok) || !isTRUE(v_gene$ok)) {
        bad <- if (!isTRUE(v_main$ok)) v_main$message else v_gene$message
        which_t <- if (!isTRUE(v_main$ok)) "mitogenome" else "gene"
        shinyWidgets::sendSweetAlert(
          session = session,
          title = "Invalid FASTA header template",
          text = stringr::str_glue("The {which_t} header template is invalid: {bad}"),
          type = "error"
        )
        return(FALSE)
      }
      TRUE
    }

    run_export <- function() {
      group <- input$export_group
      # PCG review needs a multi-sample group; flag_PCG_outliers/export_files
      # only review when length(IDs) > 1.
      # Units, not samples: one sample can contribute several records, and the
      # outlier review needs >1 record to compare.
      n_units <- sum(rv$data$export_group == group, na.rm = TRUE)
      do_review <- isTRUE(input$review_outliers) && n_units > 1

      # Remember params so "Back to Review" can recompute against fresh edits and
      # the deferred write (after review) uses the same options. Stashed because
      # the export modal (and its inputs) is removed once the review modal opens.
      rv$review_group <- group
      rv$review_start <- input$start_aa %||% 10
      rv$review_stop <- input$stop_aa %||% 10
      rv$review_ident <- input$ident_pct %||% 60
      rv$export_params <- list(
        fasta_header = input$fasta_header,
        fasta_header_gene = input$fasta_header_gene,
        generateAAalignments = input$include_alignments,
        gene_export = input$export_genes
      )
      # Where the files will land; surfaced via a popup once the user is done.
      rv$export_done_path <- file.path(
        session$userData$dir_out, "export", group
      )

      shinyjs::removeClass("gears", "paused")
      shinyjs::disable("export_data")

      if (do_review) {
        # Review BEFORE writing files: edits made during review must land in the
        # DB first, otherwise the exported .fasta/.tbl/.gff would be stale.
        # Files are written on "Done" (see finalize_export).
        review_res <- flag_PCG_outliers(
          group = group,
          db = file.path(session$userData$dir, ".sqlite"),
          start_aa = rv$review_start,
          stop_aa = rv$review_stop,
          ident_pct = rv$review_ident
        )
        shinyjs::addClass("gears", "paused")
        shinyjs::enable("export_data")
        present_review(review_res)
      } else {
        # No review: write files immediately, then announce.
        write_export_files()
        shinyjs::addClass("gears", "paused")
        shinyjs::enable("export_data")
        show_export_done_alert()
      }
    }

    # Write the export files for the current group using the stashed options,
    # with review off (flagging already happened up front). Shown behind a waiter
    # overlay since this runs after the export modal is gone.
    write_export_files <- function() {
      p <- rv$export_params
      if (is.null(p)) return(invisible(NULL))
      export_files(
        group = rv$review_group,
        fasta_header = p$fasta_header,
        fasta_header_gene = p$fasta_header_gene,
        generateAAalignments = p$generateAAalignments,
        out_dir = session$userData$dir_out,
        gene_export = p$gene_export,
        review = FALSE,
        start_aa = rv$review_start,
        stop_aa = rv$review_stop,
        ident_pct = rv$review_ident
      )
      # Refresh the table so the newly-written export_time_stamp shows up.
      trigger("refresh_export")
    }

    # Finish a reviewed export: write files (now that all edits are committed to
    # the DB), then show the export-complete popup. Used both when the user clicks
    # "Done" and when review finds nothing to flag.
    finalize_export <- function(extra = NULL) {
      waiter::waiter_show(
        html = tagList(
          waiter::spin_fading_circles(),
          tags$h4(style = "color:white; margin-top:1em;", "Writing export files, hold tight...")
        ),
        color = "rgba(40,40,40,0.85)"
      )
      on.exit(waiter::waiter_hide())
      write_export_files()
      show_export_done_alert(extra = extra)
    }

    # Popup announcing where files were written. `extra` adds a second line
    # (e.g. the "no outliers flagged" note when review found nothing).
    show_export_done_alert <- function(extra = NULL) {
      path <- rv$export_done_path
      if (is.null(path)) return(invisible(NULL))
      # JS-safe single-quoted string for the clipboard onclick
      path_js <- gsub("'", "\\\\'", gsub("\\\\", "\\\\\\\\", path))
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Export complete",
        text = tagList(
          "Data exported to:",
          tags$div(
            style = paste(
              "display: flex; flex-direction: column; gap: 0.4em;",
              "margin-top: 0.5em;"
            ),
            tags$div(
              style = paste(
                "min-width: 0; background: #000; color: #fff;",
                "font-family: monospace; font-size: 0.8em; padding: 0.5em 0.6em; border-radius: 4px;",
                "white-space: normal; word-break: break-all; text-align: center;"
              ),
              path
            ),
            tags$div(
              style = "display: flex; flex-direction: row; gap: 0.4em; justify-content: center;",
              tags$button(
                type = "button",
                class = "btn btn-secondary",
                title = "Copy path",
                onclick = sprintf(
                  paste0(
                    "navigator.clipboard.writeText('%s');",
                    "var t=this.querySelector('span');",
                    "if(t){var o=t.innerText;t.innerText='Copied!';",
                    "setTimeout(function(){t.innerText=o;},1200);}"
                  ),
                  path_js
                ),
                shiny::icon("copy"),
                tags$span(
                  # Fixed width sized for "Copied!" so the button does not resize
                  # when the label changes on click.
                  style = "margin-left: 0.3em; display: inline-block; min-width: 4.5em; text-align: center;",
                  "Copy"
                )
              ),
              # Open the export folder in an environment-aware way (see open_path):
              # OS file browser locally, RStudio Files pane on Server (with a
              # notification), or a warning path on headless sessions.
              tags$button(
                type = "button",
                class = "btn btn-secondary",
                title = "Open export folder",
                onclick = sprintf(
                  "Shiny.setInputValue('%s', Math.random(), {priority: 'event'});",
                  ns("open_export_dir")
                ),
                shiny::icon("folder-open"),
                tags$span(
                  style = "margin-left: 0.3em; display: inline-block; min-width: 4.5em; text-align: center;",
                  "Open"
                )
              )
            )
          ),
          if (!is.null(extra)) tags$p(style = "margin-top: 0.75em;", extra)
        ),
        type = "success",
        html = TRUE
      )
    }

    # "Open" button in the export-complete popup: open the export folder in an
    # environment-aware way (open_path handles desktop / RStudio Server / headless
    # and notifies the user accordingly).
    observeEvent(input$open_export_dir, ignoreInit = TRUE, {
      open_path(rv$export_done_path)
    })

    # Load a review result into rv and open the modal (or report none found).
    # focus_gene: optional gene name to navigate to (e.g. the gene just reviewed
    # via "Back to Review"); falls back to the first gene when absent or no longer
    # flagged.
    present_review <- function(res, focus_gene = NULL) {
      rv$outliers <- res$flags
      rv$alns <- res$alignments
      flagged_genes <- unique(res$flags$gene)
      if (length(flagged_genes) > 0) {
        rv$review_genes <- flagged_genes
        rv$review_idx <- if (!is.null(focus_gene) && focus_gene %in% flagged_genes) {
          which(flagged_genes == focus_gene)[1]
        } else {
          1L
        }
        removeModal()
        trigger("outlier_modal")
      } else {
        # Nothing to review: write the files now, then announce.
        finalize_export(extra = "No outlier PCG annotations were flagged.")
      }
    }

    # Merge a single-gene recompute into the cached review state. Editing one gene
    # only changes that gene's alignment, so replace just its flags/alignment and
    # keep review_genes (and the user's position) stable, then navigate back to
    # the focal gene and reopen.
    merge_review <- function(res, gene) {
      rv$outliers <- dplyr::bind_rows(
        rv$outliers[rv$outliers$gene != gene, , drop = FALSE],
        res$flags
      )
      # res$alignments has the gene only when it still has a flagged sample
      rv$alns[[gene]] <- res$alignments[[gene]]
      # review_genes intentionally unchanged: genes stay in the list and are
      # marked resolved rather than removed.
      if (gene %in% rv$review_genes) {
        rv$review_idx <- which(rv$review_genes == gene)[1]
      }
      removeModal()
      trigger("outlier_modal")
    }

    # Returning from the annotate details modal: recompute against the (now
    # possibly edited) annotations so resolved flags drop off, then reopen. The
    # editing lock guarantees only the focal gene changed, so recompute just that
    # gene and merge it into the cached results instead of re-aligning every PCG.
    on("reopen_outlier_review", {
      req(rv$review_group)
      # The (sample, gene) just reviewed in the annotate details modal: mark it
      # resolved (survives the recompute below) and remember the gene so we can
      # navigate back to it.
      rr <- session$userData$resolve_on_return
      session$userData$resolve_on_return <- NULL
      if (!is.null(rr)) {
        rv$resolved <- union(rv$resolved, paste(rr$ID, rr$gene, sep = "|"))
      }
      # Show the overlay first, then defer the (blocking) recompute one tick so
      # the "hold tight" message actually paints before alignment starts.
      waiter::waiter_show(
        html = tagList(
          waiter::spin_fading_circles(),
          tags$h4(style = "color:white; margin-top:1em;", "Recomputing alignments, hold tight...")
        ),
        color = "rgba(40,40,40,0.85)"
      )
      shinyjs::delay(100, {
        focal <- if (!is.null(rr)) rr$gene else NULL
        res <- tryCatch(
          flag_PCG_outliers(
            group = rv$review_group,
            db = file.path(session$userData$dir, ".sqlite"),
            start_aa = rv$review_start %||% 10,
            stop_aa = rv$review_stop %||% 10,
            ident_pct = rv$review_ident %||% 60,
            genes = focal
          ),
          finally = waiter::waiter_hide()
        )
        # Scoped merge when we know the single edited gene and have cached state;
        # otherwise fall back to a full reload.
        if (!is.null(focal) && !is.null(rv$outliers)) {
          merge_review(res, focal)
        } else {
          present_review(res, focus_gene = focal)
        }
      })
    })

    # Remember review-option edits so the modal reopens with them
    observeEvent(input$review_outliers, rv$opt_review <- isTRUE(input$review_outliers))
    observeEvent(input$start_aa, {
      if (!is.null(input$start_aa) && !is.na(input$start_aa)) rv$opt_start <- input$start_aa
    })
    observeEvent(input$stop_aa, {
      if (!is.null(input$stop_aa) && !is.na(input$stop_aa)) rv$opt_stop <- input$stop_aa
    })
    observeEvent(input$ident_pct, {
      if (!is.null(input$ident_pct) && !is.na(input$ident_pct)) rv$opt_ident <- input$ident_pct
    })

    # Samples in this group contributing more than one record. Multi-PATH samples
    # are rejected outright by export_files(); this catches the other shape, a
    # single genome fragmented across scaffolds, which would otherwise export as
    # several incomplete records with no signal that anything is wrong.
    fragmented_samples <- function(group) {
      d <- rv$data[!is.na(rv$data$export_group) & rv$data$export_group == group, ]
      if (nrow(d) == 0) return(character(0))
      counts <- table(d$ID)
      names(counts)[counts > 1]
    }

    check_overwrite_then_export <- function() {
      export_path <- file.path(session$userData$dir_out, "export", input$export_group)
      if (dir.exists(export_path)) {
        shinyWidgets::confirmSweetAlert(
          session = session,
          inputId = ns("overwrite_confirm"),
          title = "Export already exists",
          text = stringr::str_glue(
            "Export files for group '{input$export_group}' already exist. Overwrite them?"
          ),
          type = "warning",
          btn_labels = c("Cancel", "Overwrite"),
          btn_colors = c("#0056b3", "#d9534f")
        )
        return()
      }
      run_export()
    }

    observeEvent(input$export_data, ignoreInit = T, {
      req(input$export_group)
      # Block export if either header template is invalid (would crash str_glue_data)
      if (!valid_headers_or_alert()) return()
      frag <- fragmented_samples(input$export_group)
      if (length(frag) > 0) {
        shown <- paste(utils::head(frag, 5), collapse = ", ")
        if (length(frag) > 5) shown <- paste0(shown, ", and ", length(frag) - 5, " more")
        shinyWidgets::confirmSweetAlert(
          session = session,
          inputId = ns("fragmented_confirm"),
          title = "Some samples export as multiple records",
          text = stringr::str_glue(
            "{length(frag)} sample(s) have more than one assembly and will each ",
            "produce a SEPARATE GenBank record: {shown}.\n\n",
            "That is correct when the scaffolds really are different genomes. If a ",
            "sample is instead ONE genome broken into fragments, each record will ",
            "be submitted as an incomplete genome. Cancel and use consensus ",
            "trimming / scaffold joining to combine them, or 'ignore' all but one ",
            "scaffold."
          ),
          type = "warning",
          btn_labels = c("Cancel", "Export anyway"),
          btn_colors = c("#0056b3", "#d9534f")
        )
        return()
      }
      check_overwrite_then_export()
    })

    observeEvent(input$fragmented_confirm, ignoreInit = T, {
      req(input$fragmented_confirm)
      check_overwrite_then_export()
    })

    observeEvent(input$overwrite_confirm, ignoreInit = T, {
      req(input$overwrite_confirm)
      run_export()
    })

    # Outlier review ----
    # Gene currently under review, and that gene's flagged samples
    current_gene <- reactive({
      req(rv$review_genes, rv$review_idx)
      req(rv$review_idx <= length(rv$review_genes))
      rv$review_genes[[rv$review_idx]]
    })
    current_flags <- reactive({
      g <- current_gene()
      rv$outliers[rv$outliers$gene == g, , drop = FALSE]
    })
    # Sample (by label) to highlight in the MSA; cleared when the gene changes
    highlight_label <- reactiveVal(NULL)

    # Bumped on every (re)open of the review modal so the MSA widget gets a fresh
    # output id and is rebuilt from scratch for the gene currently shown (the focal
    # gene on "Back to Review"). Prev/Next navigate within an already-open modal and
    # reuse the same id (standard reactive update), so only the shown gene rebuilds.
    # This fixes the stale MSA when returning to the same focal gene after an edit.
    aln_nonce <- reactiveVal(0L)

    init("outlier_modal")
    on("outlier_modal", {
      req(length(rv$review_genes) > 0)
      highlight_label(NULL)
      # Fresh output id for this open -> the shown gene's MSA rebuilds from scratch
      # (no stale htmlwidget binding reuse). Register the renderer for that id here.
      aln_nonce(isolate(aln_nonce()) + 1L)
      output[[paste0("review_aln_", isolate(aln_nonce()))]] <- msaR::renderMsaR({
        g <- current_gene()
        aln <- rv$alns[[g]]
        req(aln)
        # Move the picked sample to the top and mark it so it stands out
        hl <- highlight_label()
        if (!is.null(hl) && hl %in% names(aln)) {
          aln <- aln[c(which(names(aln) == hl), which(names(aln) != hl))]
          names(aln)[1] <- paste0(">> ", names(aln)[1])
        }
        msaR::msaR(
          aln,
          overviewbox = FALSE,
          seqlogo = FALSE,
          menu = FALSE,
          conservation = TRUE,
          labelNameLength = 150,
          colorscheme = "zappo",
          alignmentHeight = review_aln_height()
        )
      })
      modalDialog(
        title = "PCG Annotation Outlier Review",
        size = "l",
        div(
          style = "margin-bottom: 0.5em; font-weight: bold;",
          textOutput(ns("review_header"))
        ),
        p(
          style = "color: #666; font-size: 0.9em;",
          "Review the alignment below to decide whether the flagged samples ",
          "need to be revised. Click 'edit' to jump to the annotation editor ",
          "for a sample, or skip the gene if the flags look benign."
        ),
        uiOutput(ns("review_aln_ui")),
        tags$hr(),
        reactableOutput(ns("review_table")),
        footer = tagList(
          actionButton(ns("review_prev"), "Prev"),
          actionButton(ns("review_next"), "Next"),
          actionButton(ns("skip_gene"), "Mark gene resolved", class = "btn-success"),
          actionButton(ns("review_done"), "Done", class = "btn-primary")
        )
      ) |> showModal()
    })

    output$review_header <- renderText({
      req(rv$review_genes)
      sprintf(
        "Gene %d of %d: %s",
        rv$review_idx, length(rv$review_genes), toupper(current_gene())
      )
    })

    # Size the alignment viewport to the number of sequences so small groups
    # don't leave a large blank gap below the rows.
    review_aln_height <- reactive({
      g <- current_gene()
      aln <- rv$alns[[g]]
      req(aln)
      min(400L, max(120L, as.integer(length(aln) * 18 + 40)))
    })

    # Dynamic id (tracks aln_nonce) so each modal open binds a fresh msaR widget;
    # the renderer for this id is registered in on("outlier_modal").
    output$review_aln_ui <- renderUI({
      msaR::msaROutput(
        ns(paste0("review_aln_", aln_nonce())),
        height = paste0(review_aln_height() + 10, "px")
      )
    })

    output$review_table <- renderReactable({
      df <- current_flags()
      req(nrow(df) > 0)
      keys <- paste(df$ID, df$gene, sep = "|")
      df <- df |>
        dplyr::transmute(
          Sample = label,
          Issue = issue,
          `Start offset (aa)` = start_offset,
          `Stop offset (aa)` = stop_offset,
          `Identity (%)` = pct_identity,
          resolved = keys %in% rv$resolved,
          edit = "edit"
        )
      # Show signed offsets with an explicit "+" for positive values
      signed_cell <- htmlwidgets::JS(
        "function(ci){var v=ci.value; if(v===null||v===undefined) return ''; return v>0?('+'+v):(''+v);}"
      )
      # Dim + strike-through rows the user has marked resolved
      resolved_row_style <- htmlwidgets::JS(
        "function(rowInfo){ if(rowInfo && rowInfo.values && rowInfo.values['resolved']) return {opacity:0.5, textDecoration:'line-through'}; }"
      )
      reactable::reactable(
        df,
        sortable = TRUE,
        highlight = TRUE,
        rowStyle = resolved_row_style,
        defaultColDef = reactable::colDef(html = TRUE),
        columns = list(
          Sample = reactable::colDef(
            cell = rt_link(ns("review_pick"))
          ),
          `Start offset (aa)` = reactable::colDef(
            cell = signed_cell,
            header = export_help_label(
              "Start offset (aa)",
              "Number of amino acids this sample's start extends past (+) or falls short of (-) the core alignment."
            )
          ),
          `Stop offset (aa)` = reactable::colDef(
            cell = signed_cell,
            header = export_help_label(
              "Stop offset (aa)",
              "Number of amino acids this sample's stop extends past (+) or falls short of (-) the core alignment."
            )
          ),
          `Identity (%)` = reactable::colDef(
            header = export_help_label(
              "Identity (%)",
              "Mean percent identity of this sample versus rest of samples in alignment group."
            )
          ),
          resolved = reactable::colDef(
            name = "Resolved",
            width = 90,
            align = "center",
            cell = rt_bool_bttn(
              ns("toggle_resolved"),
              "fas fa-circle-check",
              "far fa-circle"
            )
          ),
          edit = reactable::colDef(
            name = "",
            width = 80,
            align = "center",
            cell = rt_icon_bttn_text(ns("goto_annot"), "fas fa-pen-to-square fa-xs")
          )
        )
      )
    })

    # Toggle a (sample, gene) as resolved; kept in rv$resolved so it survives
    # alignment/flag recomputes (Back to Review).
    observeEvent(input$toggle_resolved, {
      fr <- current_flags()[as.integer(input$toggle_resolved), ]
      req(nrow(fr) == 1)
      key <- paste(fr$ID, fr$gene, sep = "|")
      rv$resolved <- if (key %in% rv$resolved) setdiff(rv$resolved, key) else c(rv$resolved, key)
    })

    # Click a sample name -> highlight it (moved to top, marked) in the MSA
    observeEvent(input$review_pick, {
      idx <- as.integer(input$review_pick)
      lbl <- current_flags()$label[idx]
      req(length(lbl) == 1, !is.na(lbl))
      highlight_label(lbl)
    })

    observeEvent(input$review_prev, {
      highlight_label(NULL)
      rv$review_idx <- max(1L, rv$review_idx - 1L)
    })
    observeEvent(input$review_next, {
      highlight_label(NULL)
      rv$review_idx <- min(length(rv$review_genes), rv$review_idx + 1L)
    })
    # Finish review: close the modal, write the files (now that all edits are in
    # the DB), and surface the export-complete popup.
    observeEvent(input$review_done, {
      removeModal()
      finalize_export()
    })
    # Mark gene completed: flag every sample for this gene as resolved (kept in
    # rv$resolved so the rows survive recompute and show struck-through) rather than
    # removing the gene from the review list. Advance to the next gene if any.
    observeEvent(input$skip_gene, {
      highlight_label(NULL)
      fr <- current_flags()
      req(nrow(fr) > 0)
      keys <- paste(fr$ID, fr$gene, sep = "|")
      rv$resolved <- union(rv$resolved, keys)
      if (rv$review_idx < length(rv$review_genes)) {
        rv$review_idx <- rv$review_idx + 1L
      }
    })

    # Jump to the annotate details modal for the chosen flagged sample
    observeEvent(input$goto_annot, {
      fr <- current_flags()[as.integer(input$goto_annot), ]
      req(nrow(fr) == 1)
      # Carry the unit: the flag belongs to one scaffold, and without path/scaffold
      # the Annotate side falls back to the sample's first unit.
      session$userData$goto_annotate_target <- list(
        ID = fr$ID, path = fr$path, scaffold = fr$scaffold,
        gene = fr$gene, issue = fr$issue,
        start_offset = fr$start_offset, stop_offset = fr$stop_offset,
        pct_identity = fr$pct_identity
      )
      removeModal()
      trigger("goto_annotate")
    })
  })
}
