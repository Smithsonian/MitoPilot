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
      updating = NULL
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
      export_files(
        group = input$export_group,
        fasta_header = input$fasta_header,
        fasta_header_gene = input$fasta_header_gene,
        generateAAalignments = input$include_alignments,
        out_dir = session$userData$dir_out,
        gene_export = input$export_genes
      )
      shinyjs::show("output_path")
      output$out_path_location <- renderText({
        paste0(session$userData$dir_out, "/export/", input$export_group)
      })
      shinyjs::addClass("gears", "paused")
      shinyjs::enable("export_data")
    }

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
  })
}
