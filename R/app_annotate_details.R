#' annotate_details Server Functions
#'
#' @import patchwork
#' @noRd
annotations_details_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Prepare modal data ----
    init("annotations_modal")
    on("annotations_modal", {
      req(rv$updating$topology != "fragmented") # TODO! modify to handle fragmented assemblies
      rv$align_refSeq <- TRUE

      ## Load annotations ----
      rv$annotations <- dplyr::tbl(session$userData$con, "annotations") |>
        dplyr::filter(ID == rv$updating$ID) |>
        dplyr::arrange(pos1) |>
        dplyr::collect() |>
        dplyr::mutate(
          fas = "nt",
          faa = dplyr::case_when(
            type == "PCG" ~ "aa",
            .default = NA_character_
          )
        )
      ## Load coverage ----
      # TODO - get from db (need to fix NA="" issue)
      rv$coverage <- file.path(
        session$userData$dir_out,
        rv$updating$ID,
        "annotate"
      ) |>
        list.files(pattern = "coverageStats", full.names = T) |>
        read.csv()

      rv$annotations |>
        reactable(
          compact = TRUE,
          wrap = FALSE,
          width = "100%",
          onClick = "select",
          selection = "single",
          defaultPageSize = 50,
          height = 250,
          rowStyle = rt_highlight_row(),
          defaultColDef = colDef(width = 60, align = "center", show = F),
          columns = list(
            type = colDef(
              show = T, align = "left",
              filterMethod =
                JS("function(rows, columnId, filterValue) {
                    const pattern = new RegExp(filterValue, 'i')

                    return rows.filter(function(row) {
                      return pattern.test(row.values[columnId])
                    })
              }")
            ),
            gene = colDef(show = T, align = "left"),
            pos1 = colDef(show = T),
            pos2 = colDef(show = T),
            length = colDef(show = T, width = 70),
            direction = colDef(show = T, width = 75),
            notes = colDef(show = T, width = 175, html = T, cell = rt_longtext(), align = "center"),
            warnings = colDef(show = T, width = 180, html = T, cell = rt_longtext(), align = "center"),
            fas = colDef(
              name = "", show = T, html = T, sticky = "right", width = 44,
              cell = rt_icon_bttn_text(ns("copy_fas"), "fas fa-copy")
            ),
            faa = colDef(
              name = "", show = T, html = T, sticky = "right", width = 44,
              cell = rt_icon_bttn_text(ns("copy_faa"), "fas fa-copy")
            )
          )
        ) |>
        table()

      annotate_details_modal(rv) |> showModal()
    })

    # Copy Fasta ----
    observeEvent(input$copy_fas, {
      idx <- as.numeric(input$copy_fas)
      name <- paste0(
        ">",
        paste(rv$annotations[idx, c("ID", "path", "scaffold")], collapse = ".") |>
          paste(rv$annotations$gene[idx])
      )
      seq <- dplyr::tbl(session$userData$con, "assemblies") |>
        dplyr::filter(ID == !!rv$annotations$ID[idx]) |>
        dplyr::filter(path == !!rv$annotations$path[idx]) |>
        dplyr::filter(scaffold == !!rv$annotations$scaffold[idx]) |>
        dplyr::collect() |>
        dplyr::pull(sequence) |>
        stringr::str_sub(rv$annotations$pos1[idx], rv$annotations$pos2[idx])
      session$sendCustomMessage(
        "copy_to_clipboard", list(text = paste(name, seq, sep = "\n"))
      )
    })
    observeEvent(input$copy_faa, {
      idx <- as.numeric(input$copy_faa)
      name <- paste0(
        ">",
        paste(rv$annotations[idx, c("ID", "path", "scaffold")], collapse = ".") |>
          paste(rv$annotations$gene[idx])
      )
      seq <- rv$annotations$translation[idx]
      session$sendCustomMessage(
        "copy_to_clipboard", list(text = paste(name, seq, sep = "\n"))
      )
    })

    # Close Modal ----
    observeEvent(input$close, {
      if (length(rv$editing$idx) > 0) {
        shinyWidgets::sendSweetAlert(
          title = "Unsaved Edits!",
          text = "Discard or save edits before selecting a new annotation"
        )
        req(F)
      }
      rv$annotations <- NULL
      rv$coverage <- NULL
      rv$table_filter <- NULL
      rv$alignment <- NULL
      rv$coverage_width <- NULL
      trigger("update_annotate_table")
      removeModal()
    })
    ## Lock and Close ----
    observeEvent(input$lock, {
      if (length(rv$editing$idx) > 0) {
        shinyWidgets::sendSweetAlert(
          title = "Unsaved Edits!",
          text = "Discard or save edits before selecting a new annotation"
        )
        req(F)
      }
      if (as.numeric(rv$updating$annotate_lock) != 1) {
        rv$updating$annotate_lock <- 1
        dplyr::tbl(session$userData$con, "annotate") |>
          dplyr::rows_update(
            rv$updating[, c("ID", "annotate_lock")],
            by = "ID",
            unmatched = "ignore",
            copy = TRUE,
            in_place = TRUE
          )
        rv$data <- rv$data |>
          dplyr::rows_update(rv$updating[, c("ID", "annotate_lock")], by = "ID")
      }
      shinyjs::click("close")
    })

    # Render table ----
    table <- reactiveVal()
    output$table <- reactable::renderReactable(table())
    selected <- reactive({
      sel <- reactable::getReactableState("table", "selected")
      isolate(idx <- rv$editing$idx)
      if (length(idx) == 1 && sel != idx) {
        shinyWidgets::sendSweetAlert(
          title = "Unsaved Edits!",
          text = "Discard or save edits before selecting a new annotation"
        )
        reactable::updateReactable(
          "table",
          selected = idx
        )
        return(idx)
      }
      if (length(sel) == 0 | length(idx) == 1 && sel == idx) {
        return(sel)
      }
      isolate({
        if (rv$annotations$type[sel] == "PCG" & length(rv$alignment) != 0) {
          trigger("align_now")
        } else {
          rv$alignment <- NULL
        }
      })
      shinyWidgets::updateSwitchInput(inputId = "edit_mode", value = FALSE)
      return(sel)
    })

    # Coverage Map ----
    output$coverage_map <- renderUI({
      req(rv$coverage)
      rv$genes_plot <- rv$annotations |>
        dplyr::filter(pos1 > 0) |>
        dplyr::mutate(
          type = factor(type, levels = c("ctrl", "PCG", "rRNA", "tRNA"))
        ) |>
        ggplot2::ggplot() +
        ggplot2::aes(xmin = pos1, xmax = pos2, forward = direction == "+", fill = type, y = scaffold, label = gene) +
        gggenes::geom_gene_arrow(
          arrow_body_height = ggplot2::unit(6, "mm"),
          arrowhead_height = ggplot2::unit(6, "mm"),
          arrowhead_width = ggplot2::unit(1, "mm")
        ) +
        gggenes::geom_gene_label(
          align = "left",
          height = ggplot2::unit(4, "mm")
        ) +
        ggplot2::scale_fill_manual(values = c("#FAA34A85", "#60BD6885", "#5DA5DA85", "#F17CB085")) +
        ggplot2::scale_x_continuous(
          expand = c(0, 0),
          limits = c(
            1,
            max(c(rv$coverage$Position, rv$annotations$pos2))
          ),
          breaks = seq(1000, max(rv$coverage$Position), by = 1000)
        ) +
        ggplot2::coord_cartesian(clip = "off") +
        ggthemes::theme_tufte() +
        ggplot2::theme(
          legend.position = "none",
          axis.title = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank(),
          plot.margin = ggplot2::margin(0, 0, 0, 0, "mm")
        )

      y_breaks <- scales::pretty_breaks()(range(rv$coverage$Depth))
      rv$coverage_plot <- rv$coverage |>
        dplyr::mutate(
          Errors = ErrorRate > 0.05
        ) |>
        ggplot2::ggplot() +
        ggplot2::aes(x = Position, y = Depth) +
        ggplot2::geom_label(
          data = data.frame(
            x = rep(seq(1000, max(rv$coverage$Position), by = 1000), length(y_breaks)),
            y = rep(y_breaks, each = length(seq(1000, max(rv$coverage$Position), by = 1000))),
            label = rep(y_breaks, each = length(seq(1000, max(rv$coverage$Position), by = 1000)))
          ),
          ggplot2::aes(x = x, y = y, label = label),
          inherit.aes = FALSE,
          fill = "#FFFFFF50",
          color = "#00000050",
          label.size = 0,
          size = 3
        ) +
        ggplot2::geom_vline(
          linewidth = 1,
          ggplot2::aes(xintercept = Position, color = Errors)
        ) +
        ggplot2::geom_line() +
        ggplot2::scale_color_manual(values = c("#00000000", "#FF667040")) +
        ggplot2::scale_y_continuous(breaks = y_breaks) +
        ggplot2::scale_x_continuous(
          expand = c(0, 0),
          limits = c(
            1,
            max(c(rv$coverage$Position, rv$annotations$pos2))
          ),
          breaks = seq(1000, max(rv$coverage$Position), by = 1000)
        ) +
        ggplot2::coord_cartesian(clip = "off") +
        ggthemes::theme_tufte() +
        ggplot2::theme(
          legend.position = "none",
          axis.title = ggplot2::element_blank(),
          axis.text = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank(),
          panel.grid.major.y = ggplot2::element_line(
            linetype = "dotted", color = "#00000050"
          ),
          plot.margin = ggplot2::margin(0, 0, 0, 2, "mm")
        )
      # plot with dynamic width
      plotOutput(ns("coverage_plot"), width = paste0(rv$updating$length, "px"), height = "125px")
    })
    output$coverage_plot <- renderPlot({
      req(rv$coverage_plot, rv$coverage)
      combined_plot <- rv$coverage_plot / rv$genes_plot + plot_layout(heights = c(3, 1))
      print(combined_plot)
    })
    ## Auto scroll ----
    observeEvent(selected(), {
      req(selected())
      session$sendCustomMessage(
        "hScroll", list(id = ns("coverageDiv"), px = as.numeric(rv$annotations$pos1[selected()]))
      )
    })

    # MSA ----
    init("align_now")
    observeEvent(input$align, ignoreInit = T, {
      trigger("align_now")
    })
    on("align_now", {
      req(rv$annotations$type[selected()] == "PCG")

      hits <- rv$annotations$refHits[selected()] |> json_parse(T)

      rv$alignment$seqs <- hits |>
        dplyr::pull(target, name = Taxon)

      focal <- rv$annotations$translation[selected()] |>
        setNames(paste(rv$annotations$gene[selected()], "(focal)"))

      rv$alignment$aln <- c(focal, rv$alignment$seqs) |>
        Biostrings::AAStringSet() |>
        DECIPHER::AlignSeqs(verbose = FALSE)

      rv$alignment$alignmentHeight <- 20 + (length(rv$alignment$seqs) * 20)
      rv$alignment$id <- stringr::str_glue(
        "<b>Max Similarity:</b> {round(max(hits$similarity),1)}%"
      )
    })
    output$msa_div <- renderUI({
      req(rv$alignment$aln)
      rv$alignment$stop <- stringr::str_glue(
        "<b>Stop Codon:</b> {rv$annotations$stop_codon[selected()]}"
      )
      rv$alignment$start <- stringr::str_glue(
        "<b>Start Codon:</b> {rv$annotations$start_codon[selected()]}"
      )
      rv$alignment$internal_stop <- ifelse(
        stringr::str_detect(rv$annotations$translation[selected()], "\\*"),
        paste("<span>", as.character(icon("warning")), "<b>Internal Stop Detected</b>", as.character(icon("warning")), "<span>"),
        ""
      )
      msa <- msaR::renderMsaR(
        msaR::msaR(
          rv$alignment$aln,
          overviewbox = FALSE,
          seqlogo = FALSE,
          menu = FALSE,
          conservation = TRUE,
          labelNameLength = 150,
          colorscheme = "zappo",
          rowheight = 20,
          alignmentHeight = min(rv$alignment$alignmentHeight, 200)
        )
      )
      div(
        style = "margin: 30px 5px 5px 5px;",
        div(
          style = "display: flex; gap: 25px;",
          p(HTML(isolate(rv$alignment$id))),
          p(HTML(isolate(rv$alignment$start))),
          p(HTML(isolate(rv$alignment$stop))),
          p(HTML(isolate(rv$alignment$internal_stop)))
        ),
        msa
      ) |> tagList()
    })

    # Notes ----
    notes_update <- debounce(reactive(input$notes), 500)
    observeEvent(notes_update(), ignoreInit = T, ignoreNULL = T, {
      req(notes_update() != (rv$updating$annotate_notes %|NA|% ""))
      rv$updating$annotate_notes <- notes_update() |>
        stringr::str_remove_all(",")
      dplyr::tbl(session$userData$con, "annotate") |>
        dplyr::rows_update(
          rv$updating[, c("ID", "annotate_notes")],
          by = "ID",
          unmatched = "ignore",
          copy = TRUE,
          in_place = TRUE
        )
      rv$data <- rv$data |>
        dplyr::rows_update(rv$updating[, c("ID", "annotate_notes")], by = "ID")
      trigger("update_annotate_table")
    })

    # Delete Annotation ----
    # TODO - need to update the annotate record with change in counts
    observeEvent(input$delete, {
      if (length(selected()) == 0) {
        shinyWidgets::sendSweetAlert(
          title = "No annotation selected"
        )
        req(F)
      }
      req(selected())
      shinyWidgets::confirmSweetAlert(
        inputId = ns("confirm_delete"),
        title = "Delete annotation",
        text = "This will completely remove the selected annotation. Details of the gene name and position of the deleted annotation will be added to the notes section."
      )
    })
    observeEvent(input$confirm_delete, {
      req(input$confirm_delete)
      update <- rv$annotations[selected(), ] |>
        mutate(
          pos1 = 0,
          pos2 = 0,
          length = 0
        )
      note <- stringr::str_glue(
        "DELETED: from {rv$annotations$pos1[selected()]}-{rv$annotations$pos2[selected()]}"
      )
      update$warnings <- paste(note, rv$annotations$warnings[selected()], sep = "; ")
      update$edited <- 1
      rv$annotations <- rv$annotations |>
        dplyr::slice(-selected()) |>
        dplyr::bind_rows(update) |>
        dplyr::arrange(pos1)
      dplyr::tbl(session$userData$con, "annotations") |>
        dplyr::rows_delete(
          rv$updating[, c("ID")],
          by = "ID",
          unmatched = "ignore",
          copy = TRUE,
          in_place = TRUE
        )
      dplyr::tbl(session$userData$con, "annotations") |>
        dplyr::rows_insert(
          rv$annotations,
          copy = TRUE,
          in_place = TRUE
        )
      reactable::updateReactable(
        "table",
        data = rv$annotations
      )
    })

    # Linearize ----
    observeEvent(input$linearize, {
      if (rv$updating$topology != "circular") {
        shinyWidgets::sendSweetAlert(
          title = "Assembly is already linear."
        )
        req(F)
      }
      if (length(selected()) != 1) {
        shinyWidgets::sendSweetAlert(
          title = "Select an annotation to set the break point (before/after)."
        )
        req(F)
      }
      shinyWidgets::confirmSweetAlert(
        inputId = ns("linearize_loc"),
        title = "Linearize Assembly!",
        text = stringr::str_glue(
          "Do you want to set the breakpoint before or after the selected gene ({rv$annotations$gene[selected()]})?"
        ),
        btn_labels = c("After", "Before"),
        btn_colors = c("#0056b3", "#0056b3"),
        cancelOnDismiss = FALSE,
        showCloseButton = TRUE
      )
    })
    ## Confirm linearize cut ----
    observeEvent(input$linearize_loc, {
      # Trim Before Selected Gene
      if (input$linearize_loc) {
        start <- rv$annotations$pos1[selected()]
      }
      # Trim After Selected Gene
      if (!input$linearize_loc) {
        start <- rv$annotations$pos2[selected()] + 1
      }

      # Ensure that the split does not occur inside any annotations
      chk <- rv$annotations |>
        dplyr::filter(pos1 < start & pos2 > start)
      if (nrow(chk) > 0) {
        shinyWidgets::sendSweetAlert(
          title = "Operation failed",
          text = "The selected break point would split the {rv$annotations$gene[selected()]} annotation."
        )
        req(F)
      }

      ## Get Sequence ----
      assembly <- dplyr::tbl(session$userData$con, "assemblies") |>
        dplyr::filter(ID == !!rv$annotations$ID[selected()]) |>
        dplyr::filter(path == !!rv$annotations$path[selected()]) |>
        dplyr::filter(scaffold == !!rv$annotations$scaffold[selected()]) |>
        dplyr::collect() |>
        dplyr::pull(sequence) |>
        setNames(
          paste(rv$annotations$ID[selected(), c("ID", "path", "scaffold")], collapse = ".") |>
            paste("linear")
        ) |>
        Biostrings::DNAStringSet()

      ## Rotate to cut point ----
      if (start > 1) {
        assembly <- Biostrings::xscat(
          Biostrings::subseq(assembly, start, assembly@ranges@width),
          Biostrings::subseq(assembly, 1, start - 1)
        )

        ## Rotate coverage ----
        # TODO! use database
        rv$coverage <- bind_rows(
          rv$coverage[start:assembly@ranges@width, ],
          rv$coverage[1:(start - 1), ]
        ) |> mutate(Position = row_number())
        readr::write_csv(
          rv$coverage,
          file.path(
            session$userData$dir_out,
            rv$updating$ID,
            "annotate",
            paste0(rv$updating$ID, "_coverageStats_", rv$annotations$path[selected()], ".csv")
          ),
          row.names = FALSE, quote = "none"
        )

        ## Update annotations ----
        rv$annotations <- rv$annotations |>
          dplyr::mutate(
            pos1 = dplyr::case_when(
              pos1 == 0 ~ 0,
              pos1 >= start ~ pos1 - start + 1,
              pos1 < start ~ assembly@ranges@width - start + pos1 + 1
            ),
            pos2 = dplyr::case_when(
              pos2 == 0 ~ 0,
              pos2 >= start ~ pos2 - start + 1,
              pos2 < start ~ assembly@ranges@width - start + pos2 + 1
            )
          )
        dplyr::tbl(session$userData$con, "annotations") |>
          dplyr::rows_delete(
            rv$updating[, c("ID")],
            by = "ID",
            unmatched = "ignore",
            copy = TRUE,
            in_place = TRUE
          )
        dplyr::tbl(session$userData$con, "annotations") |>
          dplyr::rows_insert(
            rv$annotations,
            copy = TRUE,
            in_place = TRUE
          )

        reactable::updateReactable(
          "table",
          data = rv$annotations
        )
      }

      ## Update assembly sequence record ----
      dplyr::tbl(session$userData$con, "assemblies") |>
        dplyr::rows_update(
          data.frame(
            ID = rv$annotations$ID[selected()],
            path = rv$annotations$path[selected()],
            scaffold = rv$annotations$scaffold[selected()],
            sequence = as.character(assembly),
            topology = "linear"
          ),
          by = c("ID", "path", "scaffold"),
          unmatched = "ignore",
          copy = TRUE,
          in_place = TRUE
        )
      Biostrings::writeXStringSet(
        assembly,
        file.path(
          session$userData$dir_out,
          rv$updating$ID,
          "annotate",
          paste0(rv$updating$ID, "_assembly_", rv$annotations$path[selected()], ".fasta")
        )
      )

      ## Update Annotate Table ----
      rv$updating$topology <- "linear"
      note <- stringr::str_glue(
        "EDITED: linearized circular assembly after rotating {start-1} bp"
      )
      rv$updating$annotate_notes <- paste(note, rv$updating$annotate_notes, sep = "; ")
      updateTextAreaInput(
        inputId = "notes",
        value = rv$updating$annotate_notes
      )
      dplyr::tbl(session$userData$con, "annotate") |>
        dplyr::rows_update(
          rv$updating[, c("ID", "topology", "annotate_notes")],
          unmatched = "ignore",
          copy = TRUE,
          in_place = TRUE
        )
      rv$data <- rv$data |>
        dplyr::rows_update(
          rv$updating[, c("ID", "topology", "annotate_notes")],
          by = "ID"
        )
    }) # END LINEARIZE
  })
}

#' Annotations Modal
#'
#' @param rv reactiveValues
#' @param session shiny session
#'
#' @noRd
annotate_details_modal <- function(rv, session = getDefaultReactiveDomain()) {
  ns <- session$ns
  modalDialog(
    title = stringr::str_glue("Annotations: {rv$updating$ID} - {rv$updating$Taxon}"),
    size = "l",
    tags$details(
      open = TRUE,
      tags$summary("Annotation Table"),
      reactableOutput(ns("table"), width = "100%")
    ),
    hr(),
    tags$details(
      tags$summary("Coverage Map"),
      div(
        id = ns("coverageDiv"),
        style = "width: 100%; overflow-x: auto; padding: 5mm;",
        uiOutput(ns("coverage_map"))
      )
    ),
    tags$details(
      id = ns("alignment_div"),
      style = "margin-bottom: 1em;",
      tags$summary(
        "Alignment",
        onclick = sprintf("Shiny.onInputChange('%s', Math.random())", ns("align"))
      ),
      div(
        style = "display: flex; gap: 10px;",
        shinyWidgets::switchInput(
          ns("edit_mode"),
          label = "Edit Mode",
          onStatus = "primary",
          labelWidth = "60px",
          size = "mini",
          width = "auto"
        ),
        shinyjs::hidden(
          div(
            id = ns("edit_mode_ctrls"),
            style = "display: flex; gap: 40px;",
            fluidRow(
              #   shinyWidgets::prettyCheckbox(
              #     ns("single_codon"),
              #     label = "single codon",
              #     inline = TRUE
              #   ),
              #   actionBttn_with_style(
              #     ns("start_add"), label=NULL, icon('plus'),
              #     style='jelly', size='xs', custom_style="color: #787878 !important; display: flex; align-items: center;"
              #   ),
              #   div(
              #     style = "display: flex; align-items: center;",
              #     p("START")
              #   ),
              #   actionBttn_with_style(
              #     ns("start_minus"), label=NULL, icon('minus'),
              #     style='jelly', size='xs', custom_style="color: #787878 !important; display: flex; align-items: center;"
              #   )
              # ),
              # fluidRow(
              #   actionBttn_with_style(
              #     ns("stop_add"), label=NULL, icon('plus'),
              #     style='jelly', size='xs', custom_style="color: #787878 !important; display: flex; align-items: center;"
              #   ),
              #   div(
              #     style = "display: flex; align-items: center;",
              #     p("STOP")
              #   ),
              #   actionBttn_with_style(
              #     ns("stop_minus"), label=NULL, icon('minus'),
              #     style='jelly', size='xs', custom_style="color: #787878 !important; display: flex; align-items: center;"
              #   )
              # ),
              # fluidRow(
              #   actionBttn_with_style(
              #     ns("save_edits"), label="Save", icon=icon('save'),
              #     style='jelly', size='xs', custom_style="display: flex; align-items: center; color: #787878 !important;"
              #   )
            )
          )
        )
      ),
      uiOutput(ns("msa_div")),
    ),
    tags$details(
      tags$summary("Notes"),
      textAreaInput(
        ns("notes"),
        label = NULL,
        value = rv$updating$annotate_notes %|NA|% "",
        width = "100%"
      )
    ),
    # tags$details(
    #   style = "margin-bottom: 1em;",
    #   tags$summary(
    #     "Manual Annotation",
    #     style="margin: 0.5em;"
    #   ),
    #   div(
    #     id=ns("manualDiv"),
    #     style = "width: 100%; overflow-x: auto; padding: 5mm;",
    #     fluidRow(
    #       style="display: flex; gap: 40px; align-items: center;",
    #       shinyWidgets::pickerInput(
    #         ns("manual_type"),
    #         choices=c("","ctrl"),
    #         width = "120px"
    #       ),
    #       numericInput(
    #         ns("manual_start"),
    #         "Start",
    #         value=0,
    #         min=1, step=1, width="80px"
    #       ),
    #       numericInput(
    #         ns("manual_stop"),
    #         "Stop",
    #         value=0,
    #         min=1, step=1, width="80px"
    #       ),
    #       actionButton(
    #         ns("manual_add"),
    #         "Add Annotation",
    #         icon("plus")
    #       )
    #     )
    #   )
    # ),
    easyClose = F,
    footer = tagList(
      actionButton(ns("linearize"), "Linearize"),
      actionButton(ns("delete"), "Delete"),
      actionButton(ns("lock"), "Lock&Close"),
      actionButton(ns("close"), "Close")
    )
  )
}
