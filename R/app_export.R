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
    shinyWidgets::pickerInput(
      inputId  = ns("col_groups"),
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
      ),
      inline = TRUE
    ),
    reactableOutput(ns("table"))
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
      review_idx = 1L      # cursor into review_genes
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

    .grp <- function(col) {
      g <- EXPORT_COL_GROUP_LOOKUP[col]
      if (is.na(g)) NULL else paste0("mp-grp-", g)
    }

    # CSS hide for unselected groups; keeps DOM intact so filter/sort/page
    # state survives toggling.
    output$col_css <- renderUI({
      hidden <- setdiff(names(EXPORT_COL_GROUPS), col_groups_rv())
      if (length(hidden) == 0) return(NULL)
      rules <- paste0(".mp-grp-", hidden,
                      " { display: none !important; }",
                      collapse = "\n")
      tags$style(HTML(rules))
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
        height = 550,
        wrap = FALSE,
        pageSizeOptions = c(25, 50, 100, 200, 500),
        striped = TRUE,
        rowStyle = rt_highlight_row(),
        defaultColDef = colDef(align = "left"),
        columns = list(
          ID = colDef(show = T, minWidth = 120, sticky = "left"),
          curate_opts = colDef(
            show = TRUE, class = .grp("curate_opts"), headerClass = .grp("curate_opts"),
            name = "Curate Opts.",
            width = 110
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
          topology = colDef(
            show = T, class = .grp("topology"), headerClass = .grp("topology"),
            name = "Topology", width = 100
          ),
          structure = colDef(
            show = T, class = .grp("structure"), headerClass = .grp("structure"),
            name = "Structure"
          ),
          export_group = colDef(name = "Group", sticky = "right")
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
    selected <- reactive(reactable::getReactableState("table", "selected"))

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
    observeEvent(input$make_group, {
      rv$updating$export_group <- req(input$group_name)
      if(any(!(grepl("^[a-zA-Z0-9_-]+$", input$group_name)))){
        shinyWidgets::sendSweetAlert(
          title = "Invalid group name",
          text = "Group names must contain only alphanumeric characters, dashes, or underscores",
          type = "error"
        )
        return()
      }
      rv$data <- rv$data |>
        dplyr::rows_update(rv$updating[, c("ID", "export_group")], by = "ID")
      dplyr::tbl(session$userData$con, "samples") |>
        dplyr::rows_update(
          rv$updating[, c("ID", "export_group")],
          unmatched = "ignore",
          in_place = TRUE,
          copy = TRUE,
          by = "ID"
        )
      trigger("update_export_table")
      removeModal()
    })

    # Export data ----
    init("export")
    on("export", {
      req(nrow(rv$data) > 0)
      choices <- sort(unique(rv$data$export_group))
      req(length(choices) > 0)
      modalDialog(
        title = div(
          style = "display: flex; justify-content: space-between; align-items: center; height: 42px;",
          span("Export Data"),
          span(id = ns("gears"), class = "gears paused")
        ),
        size = "l",
        shinyWidgets::pickerInput(
          ns("export_group"),
          "Export Group:",
          choices = choices
        ),
        textAreaInput(
          ns("fasta_header"),
          "Fasta Header Template (reference columns from your sample data using '{}'):",
          "{ID} [organism={Taxon}] [topology={topology}] [mgcode={genetic_code}] [location=mitochondrion] {Taxon} mitochondrion, complete genome",
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
        textAreaInput(
          ns("fasta_header_gene"),
          "Fasta Header Template for Gene Export (reference columns from your sample data using '{}', gene names will be automatically added):",
          "{ID} [organism={Taxon}] [topology={topology}] [mgcode={genetic_code}] [location=mitochondrion] {Taxon}",
          width = "100%"
        ),
        # PCG outlier review options, separated from the export options above
        tags$hr(style = "border-top: 1px solid #ccc; margin: 1em 0 0.75em;"),
        h4("PCG Annotation Outlier Review", style = "margin-top: 0;"),
        shinyWidgets::prettyCheckbox(
          ns("review_outliers"),
          export_help_label(
            "Review PCG annotations for outliers",
            "After exporting, align each protein-coding gene across samples and flag annotations with start/stop offsets or low identity."
          ),
          value = T,
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
                  "Flag a sample when its start extends past, or falls short of, the alignment core by more than this many residues."
                ),
                value = 10, min = 1, step = 1, width = "100%"
              )
            ),
            div(
              style = "flex: 1",
              numericInput(
                ns("stop_aa"),
                export_help_label(
                  "Flag stop offset > (aa):",
                  "Flag a sample when its stop extends past, or falls short of, the alignment core by more than this many residues."
                ),
                value = 10, min = 1, step = 1, width = "100%"
              )
            ),
            div(
              style = "flex: 1",
              numericInput(
                ns("ident_pct"),
                export_help_label(
                  "Flag sequence identity < (%):",
                  "Flag a sample when its mean percent identity to the rest of the group falls below this value."
                ),
                value = 60, min = 1, max = 100, step = 1, width = "100%"
              )
            )
          )
        ),
        div(
          id = ns("output_path"),
          h4("Data exported to:"),
          div(
            class = "code-block",
            style = "padding: 0.25em;white-space: normal;",
            id = ns("out_path"),
            textOutput(ns("out_path_location")),
          )
        ) |> shinyjs::hidden(),
        footer = tagList(
          actionButton(ns("export_data"), "Export"),
          modalButton("Close")
        )
      ) |> showModal()
    })

    run_export <- function() {
      shinyjs::removeClass("gears", "paused")
      shinyjs::disable("export_data")
      review_res <- export_files(
        group = input$export_group,
        fasta_header = input$fasta_header,
        fasta_header_gene = input$fasta_header_gene,
        generateAAalignments = input$include_alignments,
        out_dir = session$userData$dir_out,
        gene_export = input$export_genes,
        review = isTRUE(input$review_outliers),
        start_aa = input$start_aa %||% 10,
        stop_aa = input$stop_aa %||% 10,
        ident_pct = input$ident_pct %||% 60
      )
      shinyjs::show("output_path")
      output$out_path_location <- renderText({
        paste0(session$userData$dir_out, "/export/", input$export_group)
      })
      shinyjs::addClass("gears", "paused")
      shinyjs::enable("export_data")

      # Remember params so "Back to Review" can recompute against fresh edits
      rv$review_group <- input$export_group
      rv$review_start <- input$start_aa %||% 10
      rv$review_stop <- input$stop_aa %||% 10
      rv$review_ident <- input$ident_pct %||% 60

      # Surface outlier review (if any flagged genes)
      if (isTRUE(input$review_outliers) && !is.null(review_res)) {
        present_review(review_res)
      }
    }

    # Load a review result into rv and open the modal (or report none found)
    present_review <- function(res) {
      rv$outliers <- res$flags
      rv$alns <- res$alignments
      flagged_genes <- unique(res$flags$gene)
      if (length(flagged_genes) > 0) {
        rv$review_genes <- flagged_genes
        rv$review_idx <- 1L
        removeModal()
        trigger("outlier_modal")
      } else {
        shinyWidgets::sendSweetAlert(
          session = session,
          title = "Outlier review",
          text = "No outlier PCG annotations were flagged.",
          type = "success"
        )
      }
    }

    # Returning from the annotate details modal: recompute against the (now
    # possibly edited) annotations so resolved flags drop off, then reopen.
    on("reopen_outlier_review", {
      req(rv$review_group)
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
        res <- tryCatch(
          flag_PCG_outliers(
            group = rv$review_group,
            db = file.path(session$userData$dir, ".sqlite"),
            start_aa = rv$review_start %||% 10,
            stop_aa = rv$review_stop %||% 10,
            ident_pct = rv$review_ident %||% 60
          ),
          finally = waiter::waiter_hide()
        )
        present_review(res)
      })
    })

    observeEvent(input$export_data, ignoreInit = T, {
      req(input$export_group)
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

    init("outlier_modal")
    on("outlier_modal", {
      req(length(rv$review_genes) > 0)
      highlight_label(NULL)
      modalDialog(
        title = "PCG Annotation Outlier Review",
        size = "l",
        div(
          style = "margin-bottom: 0.5em; font-weight: bold;",
          textOutput(ns("review_header"))
        ),
        p(
          style = "color: #666; font-size: 0.9em;",
          "Review the alignment below to decide whether the flagged samples are ",
          "need to be revised. Click 'edit' to jump to the annotation editor ",
          "for a sample, or skip the gene if the flags look benign."
        ),
        uiOutput(ns("review_aln_ui")),
        tags$hr(),
        reactableOutput(ns("review_table")),
        footer = tagList(
          actionButton(ns("review_prev"), "Prev"),
          actionButton(ns("review_next"), "Next"),
          actionButton(ns("skip_gene"), "Skip this gene"),
          modalButton("Done")
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

    output$review_aln_ui <- renderUI({
      msaR::msaROutput(ns("review_aln"), height = paste0(review_aln_height() + 10, "px"))
    })

    output$review_aln <- msaR::renderMsaR({
      g <- current_gene()
      aln <- rv$alns[[g]]
      req(aln)
      # Move the picked sample to the top and mark it so it stands out
      hl <- highlight_label()
      if (!is.null(hl) && hl %in% names(aln)) {
        aln <- aln[c(which(names(aln) == hl), which(names(aln) != hl))]
        names(aln)[1] <- paste0("▶ ", names(aln)[1])
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

    output$review_table <- renderReactable({
      df <- current_flags()
      req(nrow(df) > 0)
      df <- df |>
        dplyr::transmute(
          Sample = label,
          Issue = issue,
          `Start offset (aa)` = start_offset,
          `Stop offset (aa)` = stop_offset,
          `Identity (%)` = pct_identity,
          edit = "edit"
        )
      # Show signed offsets with an explicit "+" for positive values
      signed_cell <- htmlwidgets::JS(
        "function(ci){var v=ci.value; if(v===null||v===undefined) return ''; return v>0?('+'+v):(''+v);}"
      )
      reactable::reactable(
        df,
        sortable = TRUE,
        highlight = TRUE,
        defaultColDef = reactable::colDef(html = TRUE),
        columns = list(
          Sample = reactable::colDef(
            cell = rt_link(ns("review_pick"))
          ),
          `Start offset (aa)` = reactable::colDef(
            cell = signed_cell,
            header = export_help_label(
              "Start offset (aa)",
              "Residues this sample's start extends past (+) or falls short of (-) the alignment's well-aligned core."
            )
          ),
          `Stop offset (aa)` = reactable::colDef(
            cell = signed_cell,
            header = export_help_label(
              "Stop offset (aa)",
              "Residues this sample's stop extends past (+) or falls short of (-) the alignment's well-aligned core."
            )
          ),
          `Identity (%)` = reactable::colDef(
            header = export_help_label(
              "Identity (%)",
              "Mean percent identity of this sample to the rest of the group in the alignment."
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
    observeEvent(input$skip_gene, {
      highlight_label(NULL)
      g <- current_gene()
      rv$review_genes <- setdiff(rv$review_genes, g)
      if (length(rv$review_genes) == 0) {
        removeModal()
      } else {
        rv$review_idx <- min(rv$review_idx, length(rv$review_genes))
      }
    })

    # Jump to the annotate details modal for the chosen flagged sample
    observeEvent(input$goto_annot, {
      fr <- current_flags()[as.integer(input$goto_annot), ]
      req(nrow(fr) == 1)
      session$userData$goto_annotate_target <- list(
        ID = fr$ID, gene = fr$gene, issue = fr$issue,
        start_offset = fr$start_offset, stop_offset = fr$stop_offset,
        pct_identity = fr$pct_identity
      )
      removeModal()
      trigger("goto_annotate")
    })
  })
}
