#' annotate_details Server Functions
#'
#' @import patchwork
#' @noRd

# Shared gene-type fill palette for coverage and synteny plots.
# Keep entries in the same order as the `type` factor levels used downstream.
gene_type_fill <- c(
  ctrl = "#FAA34A",
  PCG  = "#60BD68",
  rRNA = "#5DA5DA",
  tRNA = "#F17CB0"
)
gene_type_alpha <- 0.5

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
        dplyr::filter(ID == !!rv$updating$ID) |>
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

      ## Load BLAST reference annotations ----
      rv$blast_ref <- dplyr::tbl(session$userData$con, "blast_ref_annotations") |>
        dplyr::filter(ID == !!rv$updating$ID) |>
        dplyr::collect()

      ## Load BLAST reference alignment ----
      rv$blast_ref_aln <- tryCatch(
        dplyr::tbl(session$userData$con, "blast_ref_alignment") |>
          dplyr::filter(ID == !!rv$updating$ID) |>
          dplyr::collect(),
        error = function(e) NULL
      )

      annotate_details_modal(rv) |> showModal()
      render_annotations_table(Sys.time())
    })

    # Compact status pill renderer. `state` is one of "yes" / "no" / NA;
    # `invert = TRUE` flips the color mapping so "yes" reads as warning.
    status_badge <- function(label, state, invert = FALSE) {
      val <- if (is.na(state)) "na" else as.character(state)
      bg <- if (val == "yes") {
        if (invert) "#fde8d0" else "#d4edda"
      } else if (val == "no") {
        if (invert) "#d4edda" else "#fde8d0"
      } else {
        "#e9ecef"
      }
      fg <- if (val == "yes") {
        if (invert) "#7d4a1e" else "#2d6a4f"
      } else if (val == "no") {
        if (invert) "#2d6a4f" else "#7d4a1e"
      } else {
        "#6c757d"
      }
      span(
        style = paste0(
          "background:", bg, "; color:", fg,
          "; border-radius:3px; padding:2px 8px; font-size:0.75em;",
          " font-weight: 600; white-space: nowrap;"
        ),
        paste0(label, ": ", toupper(val))
      )
    }

    # Title-area passive badges: ID verified / Reviewed / Problematic.
    output$status_badges <- shiny::renderUI({
      tagList(
        status_badge("ID verified", rv$updating$ID_verified),
        status_badge("Reviewed",    rv$updating$reviewed),
        status_badge("Problematic", rv$updating$problematic, invert = TRUE)
      )
    })

    # Footer toggle buttons: clicking still drives the same input$ID_verified /
    # input$reviewed / input$problematic observers below; visual state reflects
    # the current value so the user sees what each click will flip.
    toggle_btn <- function(id, label, state, invert = FALSE) {
      val <- if (is.na(state)) "na" else as.character(state)
      cls <- if (val == "yes") {
        if (invert) "btn btn-warning" else "btn btn-success"
      } else if (val == "no") {
        if (invert) "btn btn-success" else "btn btn-default"
      } else {
        "btn btn-default"
      }
      ico <- if (val == "yes") {
        shiny::icon(if (invert) "triangle-exclamation" else "check")
      } else if (val == "no") {
        shiny::icon("xmark")
      } else {
        shiny::icon("question")
      }
      actionButton(id, label, icon = ico, class = cls)
    }
    output$status_toggles <- shiny::renderUI({
      tagList(
        toggle_btn(ns("ID_verified"), "ID verified", rv$updating$ID_verified),
        toggle_btn(ns("reviewed"),    "Reviewed",    rv$updating$reviewed),
        toggle_btn(ns("problematic"), "Problematic", rv$updating$problematic,
                   invert = TRUE)
      )
    })

    # Render table ----
    render_annotations_table <- reactiveVal()
    output$table <- reactable::renderReactable({
      req(render_annotations_table())
      isolate(rv$annotations) |>
        reactable(
          compact = TRUE,
          wrap = FALSE,
          width = "100%",
          onClick = "select",
          selection = "single",
          filterable = TRUE,
          defaultPageSize = 50,
          height = 250,
          rowStyle = rt_highlight_row(),
          defaultColDef = colDef(maxWidth = 80, align = "center", show = F),
          columns = list(
            type = colDef(
              show = T,
              align = "left",
              cell = function(value) {
                color <- switch(value %||% "",
                  ctrl  = "#FAA34A",
                  PCG   = "#60BD68",
                  rRNA  = "#5DA5DA",
                  tRNA  = "#F17CB0",
                  "#888888"
                )
                htmltools::span(
                  style = paste0(
                    "background:", color, "30;",
                    "color:#111111;",
                    "border:1px solid ", color, ";",
                    "border-radius:3px;",
                    "padding:1px 4px;",
                    "font-size:11px;",
                    "white-space:nowrap;"
                  ),
                  value %||% ""
                )
              }
            ),
            gene = colDef(show = T,
                          align = "left",
                          maxWidth = 300,
                          resizable = TRUE,
                          html = T,
                          cell = rt_longtext()),
            pos1 = colDef(show = T),
            pos2 = colDef(show = T),
            length = colDef(show = T),
            direction = colDef(show = T),
            tool = colDef(
              show = T,
              name = "tool",
              align = "center",
              maxWidth = 100,
              cell = function(value) {
                color <- switch(value %||% "",
                  "MITOS2"      = "#444444",
                  "tRNAscan-SE" = "#666666",
                  "ARWEN"       = "#888888",
                  "ARAGORN"     = "#AAAAAA",
                  "#CCCCCC"
                )
                htmltools::span(
                  style = paste0(
                    "background:", color, "30;",
                    "color:#111111;",
                    "border:1px solid ", color, ";",
                    "border-radius:3px;",
                    "padding:1px 4px;",
                    "font-size:11px;",
                    "white-space:nowrap;"
                  ),
                  value %||% ""
                )
              }
            ),
            notes = colDef(
              show = T,
              maxWidth = 1000,
              html = T,
              cell = rt_longtext(),
              align = "left",
              resizable = TRUE
            ),
            warnings = colDef(
              show = T,
              maxWidth = 1000,
              html = T,
              cell = rt_longtext(),
              align = "left",
              resizable = TRUE
            ),
            fas = colDef(
              name = "", show = T, html = T, width = 60, sticky = "right",
              cell = rt_icon_bttn_text(ns("copy_fas"), "fas fa-copy fa-xs")
            ),
            faa = colDef(
              name = "", show = T, html = T, width = 60, sticky = "right",
              cell = rt_icon_bttn_text(ns("copy_faa"), "fas fa-copy fa-xs")
            )
          )
        )
    })

    ## Table selection ----
    sel <- reactiveVal("init")

    # Synteny-overview click anchor: alignment column to center zoom on.
    # Set by clicking the overview plot; cleared when user picks a new gene row.
    zoom_click_col <- reactiveVal(NULL)
    selected <- reactive({
      sel <- reactable::getReactableState("table", "selected")
      # Check for unsaved edits
      isolate({
        req(rv$annotations)
        shinyjs::toggle("aln_div", condition = length(sel) > 0 && rv$annotations$type[sel] == "PCG")
        is_deleted <- length(sel) > 0 && stringr::str_detect(rv$annotations$gene[sel], "_DELETED_")
        shinyjs::toggle("annotation_action_btns", condition = length(sel) > 0 && !is_deleted)
        shinyjs::toggle("annotation_restore_btn", condition = is_deleted)
        # No row selected: skip sel-indexed branches so consumers (e.g. the
        # synteny zoom plot, which also accepts a click anchor) don't break.
        if (length(sel) == 0) {
          return(sel)
        }
        if (identical(sel, rv$editing$idx)) {
          return(sel)
        }
        if (!is.null(rv$editing) && rv$annotations$translation[sel] != rv$editing$backup$translation) {
          shinyWidgets::sendSweetAlert(
            title = "Unsaved Edits!",
            text = "Discard or save edits before selecting a new annotation"
          )
          reactable::updateReactable(
            "table",
            selected = rv$editing$idx
          )
          return(rv$editing$idx)
        }
        rv$alignment <- rv$local_hits <- NULL
        ref_msa_cache$msa <- NULL
        ref_msa_cache$key <- NULL
        if (rv$annotations$type[sel] == "PCG" & length(rv$alignment) != 0) {
          trigger("align_now")
        } else {
          toggleDetails(ns("alignment_div"), FALSE)
        }
      })
      return(sel)
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
      if (!is.null(rv$editing) && rv$annotations$translation[selected()] != rv$editing$backup$translation) {
        shinyWidgets::sendSweetAlert(
          title = "Unsaved Edits!",
          text = "Discard or save edits before selecting a new annotation"
        )
        req(F)
      }
      # Update Annotate table counts
      retained_annotations <- rv$annotations |>
        dplyr::filter(!stringr::str_detect(gene, "_DELETED_"))
      rv$updating$PCGCount = sum(retained_annotations$type == "PCG")
      rv$updating$tRNACount = sum(retained_annotations$type == "tRNA")
      rv$updating$rRNACount = sum(retained_annotations$type == "rRNA")
      dplyr::tbl(session$userData$con, "annotate") |>
        dplyr::rows_update(
          rv$updating[, c("ID", "PCGCount", "tRNACount", "rRNACount")],
          by = "ID",
          unmatched = "ignore",
          copy = TRUE,
          in_place = TRUE
        )
      rv$data <- rv$data |>
        dplyr::rows_update(rv$updating[, c("ID", "PCGCount", "tRNACount", "rRNACount")], by = "ID")
      rv$annotations <- NULL
      rv$coverage <- NULL
      rv$table_filter <- NULL
      rv$alignment <- NULL
      rv$coverage_width <- NULL
      rv$editing <- NULL
      ref_msa_cache$msa <- NULL
      ref_msa_cache$key <- NULL
      trigger("update_annotate_table")
      removeModal()
    })
    ## Lock and Close ----
    observeEvent(input$lock, {
      if (!is.null(rv$editing) && rv$annotations$translation[selected()] != rv$editing$backup$translation) {
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

    # Snapshot of the sample fields the figures actually use. Updated only when one
    # of them changes, so toggling unrelated state (reviewed / problematic / ID
    # verified) leaves this value untouched and does not invalidate the coverage /
    # synteny figures, which would otherwise re-render on every click.
    fig_ctx <- reactiveVal(NULL)
    observe({
      u <- rv$updating
      if (is.null(u) || is.null(u$ID)) {
        if (!is.null(fig_ctx())) fig_ctx(NULL)
        return()
      }
      ctx <- list(
        ID              = u$ID,
        length          = u$length,
        topology        = u$topology,
        blast_accession = u$blast_accession,
        blast_species   = u$blast_species,
        poor_blast_ref  = u$poor_blast_ref
      )
      if (!identical(ctx, fig_ctx())) fig_ctx(ctx)
    })

    # Coverage Map ----
    output$coverage_map <- renderUI({
      req(rv$coverage, fig_ctx())
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
          arrowhead_width = ggplot2::unit(1, "mm"),
          alpha = gene_type_alpha
        ) +
        gggenes::geom_gene_label(
          align = "left",
          height = ggplot2::unit(4, "mm")
        ) +
        ggplot2::scale_fill_manual(values = gene_type_fill) +
        ggplot2::scale_x_continuous(
          expand = c(0, 0),
          limits = c(
            1,
            max(c(rv$coverage$Position, rv$annotations$pos2))
          ),
          breaks = seq(1000, max(rv$coverage$Position), by = 1000),
          labels = format(seq(1000, max(rv$coverage$Position), by = 1000), big.mark = ",")
        ) +
        ggplot2::coord_cartesian(clip = "off") +
        ggthemes::theme_tufte() +
        ggplot2::theme(
          legend.position = "none",
          axis.title  = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank(),
          # Match coverage plot's bottom-axis bounding box (invisible) so cowplot
          # align="lr" doesn't pad either panel.
          axis.text.x = ggplot2::element_text(size = 7, color = NA),
          axis.ticks.y = ggplot2::element_blank(),
          axis.ticks.x = ggplot2::element_line(color = NA, linewidth = 0.4),
          axis.ticks.length.x = ggplot2::unit(1.5, "mm"),
          plot.margin = ggplot2::margin(0, 0, 0, 0, "mm")
        )

      y_breaks <- scales::pretty_breaks()(range(rv$coverage$Depth))
      cov_max  <- max(rv$coverage$Position)
      minor_tick_x <- setdiff(seq(50, cov_max, by = 50), seq(1000, cov_max, by = 1000))
      major_tick_x <- seq(1000, cov_max, by = 1000)
      depth_rng    <- range(rv$coverage$Depth)
      y_bottom     <- depth_rng[1]
      y_tick_minor <- depth_rng[1] + diff(depth_rng) * 0.04
      major_tick_labels <- format(major_tick_x, big.mark = ",")

      # Bin coverage to ~one point per output pixel (max-depth per bin) to
      # cut geom_line vertex count on huge mitogenomes - visually lossless
      # at 1px/bp display, ~10x faster path stroke.
      target_pts  <- min(nrow(rv$coverage), 4000L)
      bin_size    <- max(1L, ceiling(cov_max / target_pts))
      cov_line_df <- rv$coverage |>
        dplyr::mutate(.bin = ((Position - 1L) %/% bin_size) * bin_size + 1L) |>
        dplyr::summarise(Depth = max(Depth), .by = .bin) |>
        dplyr::rename(Position = .bin) |>
        dplyr::arrange(Position)

      # Only positions flagged as Errors get a red vline; everything else is
      # invisible so emit no geom for them (drawing 16k transparent vlines was
      # the dominant render cost).
      err_df <- rv$coverage |>
        dplyr::filter(!is.na(ErrorRate) & ErrorRate > 0.05) |>
        dplyr::select(Position)

      rv$coverage_plot <- ggplot2::ggplot(cov_line_df) +
        ggplot2::aes(x = Position, y = Depth) +
        ggplot2::geom_vline(
          data = err_df,
          ggplot2::aes(xintercept = Position),
          inherit.aes = FALSE,
          color = "#FF667040", linewidth = 1
        ) +
        ggplot2::geom_label(
          data = data.frame(
            x = rep(major_tick_x, length(y_breaks)),
            y = rep(y_breaks, each = length(major_tick_x)),
            label = rep(y_breaks, each = length(major_tick_x))
          ),
          ggplot2::aes(x = x, y = y, label = label),
          inherit.aes = FALSE,
          fill = "#FFFFFF50",
          color = "#00000050",
          label.size = 0,
          size = 3
        ) +
        ggplot2::geom_segment(
          data = data.frame(x = minor_tick_x),
          ggplot2::aes(x = x, xend = x, y = y_bottom, yend = y_tick_minor),
          inherit.aes = FALSE,
          color = "#00000060", linewidth = 0.3
        ) +
        ggplot2::geom_line() +
        ggplot2::scale_y_continuous(breaks = y_breaks) +
        ggplot2::scale_x_continuous(
          expand = c(0, 0),
          limits = c(
            1,
            max(c(rv$coverage$Position, rv$annotations$pos2))
          ),
          breaks = major_tick_x,
          labels = major_tick_labels
        ) +
        ggplot2::coord_cartesian(clip = "off") +
        ggthemes::theme_tufte() +
        ggplot2::theme(
          legend.position = "none",
          axis.title = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_text(size = 7, color = "#000000B0"),
          axis.ticks.y = ggplot2::element_blank(),
          axis.ticks.x = ggplot2::element_line(color = "#000000B0", linewidth = 0.4),
          axis.ticks.length.x = ggplot2::unit(1.5, "mm"),
          panel.grid.major.y = ggplot2::element_line(
            linetype = "dotted", color = "#00000050"
          ),
          plot.margin = ggplot2::margin(0, 0, 0, 0, "mm")
        )
      # plot with dynamic width
      #plotOutput(ns("coverage_plot"), width = paste0(rv$updating$length, "px"), height = "125px")  # OLD CODE, problems with Cairo
      shiny::imageOutput(ns("coverage_plot"), width = paste0(fig_ctx()$length, "px"), height = "125px")
    })
    # Use renderImage + ragg::agg_png to bypass Cairo's per-dimension image
    # surface limit (~16384 px on common libcairo builds), which silently
    # truncated the right edge of large mitogenome plots under renderPlot.
    output$coverage_plot <- shiny::renderImage(
      {
        req(rv$coverage_plot, rv$coverage, fig_ctx())
        w <- fig_ctx()$length
        h <- 125L
        outfile <- tempfile(fileext = ".png")
        ragg::agg_png(outfile, width = w, height = h, units = "px", res = 72)
        combined_plot <- cowplot::plot_grid(
          rv$coverage_plot, rv$genes_plot,
          ncol = 1, align = "v", axis = "lr",
          rel_heights = c(3, 1)
        )
        print(combined_plot)
        dev.off()
        list(
          src = outfile,
          contentType = "image/png",
          width = w,
          height = h,
          alt = "Coverage map"
        )
      },
      deleteFile = TRUE
    )
    # BLAST Reference Synteny ----
    output$synteny_ui <- renderUI({
      req(rv$blast_ref)
      ctx <- req(fig_ctx())
      req(nrow(rv$blast_ref) > 0)
      # Don't render synteny when no current BLAST ref exists on this sample
      # (e.g. BLAST disabled in opts). Stale rows may linger in
      # blast_ref_annotations from a prior run with BLAST enabled.
      req(!is.na(ctx$blast_accession),
          nzchar(ctx$blast_accession))
      w          <- max(ctx$length %||% 800L, 800L)
      sample_lbl <- ctx$ID
      ref_lbl    <- ctx$blast_species %||% ctx$blast_accession
      ref_acc    <- ctx$blast_accession
      has_aln    <- !is.null(rv$blast_ref_aln) && nrow(rv$blast_ref_aln) > 0 &&
        isTRUE(nzchar(rv$blast_ref_aln$aligned_sample[1])) &&
        isTRUE(nzchar(rv$blast_ref_aln$aligned_ref[1]))
      plot_h     <- if (has_aln) "280px" else "200px"
      lbl <- function(h, ...) div(
        style = paste0("height: ", h, "; display: flex; align-items: center; word-break: break-word;"),
        ...
      )
      lbl_col <- if (has_aln) {
        tagList(
          lbl("120px", sample_lbl),
          lbl("40px", div(
            style = "position: relative; width: 100%; height: 40px; display: flex; align-items: center;",
            div(style = "position: absolute; top: 0; right: 0; font-size: 9px; color: #aaa; line-height: 1;", "100%"),
            div(style = "color: #888; font-size: 10px;", "identity"),
            div(style = "position: absolute; bottom: 0; right: 0; font-size: 9px; color: #aaa; line-height: 1;", "0%")
          )),
          lbl("120px", div(div(ref_acc), div(style = "color: #888; font-size: 10px;", ref_lbl)))
        )
      } else {
        tagList(
          lbl("100px", sample_lbl),
          lbl("100px", div(div(ref_acc), div(style = "color: #888; font-size: 10px;", ref_lbl)))
        )
      }
      is_poor <- isTRUE(ctx$poor_blast_ref == "poor")
      tagList(
        div(
          style = "display: flex; justify-content: start; margin-bottom: 6px;",
          shinyWidgets::prettyToggle(
            ns("poor_blast_ref_toggle"),
            label_on  = "Poor reference flagged",
            label_off = "Flag as poor reference",
            icon_on   = shiny::icon("flag"),
            icon_off  = shiny::icon("flag"),
            status_on  = "warning",
            status_off = "default",
            value = is_poor,
            inline = TRUE
          )
        ),
        if (isTRUE(ctx$topology == "linear")) {
          div(
            class = "alert alert-warning",
            style = "padding: 6px 10px; font-size: 0.85em; margin-bottom: 6px;",
            shiny::icon("triangle-exclamation"),
            " One or more assemblies is linear; alignment below may be poor"
          )
        },
        if (has_aln) {
          div(
            style = "display: flex; margin: 0; padding: 0; line-height: 1;",
            div(style = "flex-shrink: 0; width: 160px;"),
            div(style = "flex: 1; text-align: center; font-size: 11px; color: #888; margin: 0; padding: 0;",
                "click to zoom")
          )
        },
        div(
          style = "display: flex; align-items: flex-start;",
          div(
            style = paste0(
              "flex-shrink: 0; width: 160px; font-size: 11px; ",
              "padding-right: 6px; box-sizing: border-box;"
            ),
            lbl_col
          ),
          div(
            id = ns("syntenyScrollDiv"),
            style = paste0("overflow-x: auto; flex: 1;",
                           if (has_aln) " cursor: zoom-in;" else ""),
            imageOutput(ns("synteny_plot"), width = paste0(w, "px"), height = plot_h,
                       click = ns("synteny_click"))
          )
        ),
        if (has_aln) {
          conditionalPanel(
            condition = "input.synteny_zoom == true", ns = ns,
            div(
              style = "margin-top: 10px; border-top: 1px solid #ddd; padding-top: 10px;",
              uiOutput(ns("synteny_zoom_ui"))
            )
          )
        }
      )
    })
    output$synteny_zoom_ctrl <- renderUI({
      req(!is.null(rv$blast_ref_aln))
      req(nrow(rv$blast_ref_aln) > 0)
      div(
        style = "display: flex; align-items: center;",
        tags$style(HTML(sprintf(
          "#%s .pretty { margin-bottom: 0; }",
          ns("synteny_zoom")
        ))),
        shinyWidgets::prettyCheckbox(
          ns("synteny_zoom"),
          label = "Zoom to selected gene",
          status = "primary",
          inline = TRUE
        )
      )
    })
    output$synteny_plot <- shiny::renderImage(
      {
      req(rv$blast_ref, rv$annotations, rv$coverage, fig_ctx())
      req(nrow(rv$blast_ref) > 0)

      img_w <- max(fig_ctx()$length %||% 800L, 800L)
      ref_length   <- rv$blast_ref$ref_length[1]
      sample_genes <- rv$annotations |> dplyr::filter(pos1 > 0)
      sample_len   <- max(c(rv$coverage$Position, sample_genes$pos2), na.rm = TRUE)

      gene_track <- list(
        gggenes::geom_gene_arrow(
          arrow_body_height = ggplot2::unit(12, "mm"),
          arrowhead_height  = ggplot2::unit(12, "mm"),
          arrowhead_width   = ggplot2::unit(2, "mm"),
          alpha = gene_type_alpha
        ),
        gggenes::geom_gene_label(align = "left", height = ggplot2::unit(6, "mm")),
        ggplot2::scale_fill_manual(values = gene_type_fill),
        ggplot2::scale_x_continuous(expand = c(0, 0), limits = c(0, 100)),
        ggplot2::coord_cartesian(clip = "off"),
        ggthemes::theme_tufte(),
        ggplot2::theme(
          legend.position = "none",
          axis.title      = ggplot2::element_blank(),
          axis.text.y     = ggplot2::element_blank(),
          axis.ticks.y    = ggplot2::element_blank(),
          plot.margin     = ggplot2::margin(2, 0, 2, 0, "mm")
        )
      )

      has_aln <- !is.null(rv$blast_ref_aln) && nrow(rv$blast_ref_aln) > 0 &&
        isTRUE(nzchar(rv$blast_ref_aln$aligned_sample[1])) &&
        isTRUE(nzchar(rv$blast_ref_aln$aligned_ref[1]))

      img_h <- if (has_aln) 280L else 200L
      outfile <- tempfile(fileext = ".png")
      ragg::agg_png(outfile, width = img_w, height = img_h, units = "px", res = 72)
      on.exit(if (!is.null(dev.list())) dev.off(), add = TRUE)

      if (has_aln) {
        aln_rotation   <- as.integer(rv$blast_ref_aln$rotation[1])
        aligned_sample <- rv$blast_ref_aln$aligned_sample[1]
        aligned_ref    <- rv$blast_ref_aln$aligned_ref[1]
        s_chars <- strsplit(aligned_sample, "")[[1]]
        r_chars <- strsplit(aligned_ref,    "")[[1]]
        aln_len <- length(s_chars)

        # Non-gap index: s_nongap[i] = alignment column for sample position i
        s_nongap <- which(s_chars != "-")
        r_nongap <- which(r_chars != "-")

        # Project sample position (original coords) -> 0-100 in alignment space
        s_to_pct <- function(pos) {
          idx <- pmin(pmax(as.integer(pos), 1L), length(s_nongap))
          s_nongap[idx] / aln_len * 100
        }
        # Project ref position (original coords) -> rotate -> 0-100 in alignment space
        r_to_pct <- function(pos) {
          pos_r <- ((as.integer(pos) - 1L - aln_rotation) %% ref_length) + 1L
          idx   <- pmin(pmax(pos_r, 1L), length(r_nongap))
          r_nongap[idx] / aln_len * 100
        }

        # Gene data frames in alignment coordinates
        sample_df <- sample_genes |>
          dplyr::mutate(
            type = factor(type, levels = c("ctrl", "PCG", "rRNA", "tRNA")),
            xmin = s_to_pct(pos1), xmax = s_to_pct(pos2)
          )
        ref_df <- rv$blast_ref |>
          dplyr::mutate(
            type = factor(type, levels = c("ctrl", "PCG", "rRNA", "tRNA")),
            xmin = r_to_pct(pos1), xmax = r_to_pct(pos2)
          )

        # Classify each alignment column
        aln_class <- dplyr::case_when(
          s_chars == "-" ~ "gap_s",
          r_chars == "-" ~ "gap_r",
          tolower(s_chars) == tolower(r_chars) ~ "match",
          TRUE ~ "mismatch"
        )

        # Rolling-window % identity area plot - avoids per-column sub-pixel
        # rendering artifacts at overview scale.
        win      <- min(10L, aln_len)
        is_match <- as.integer(aln_class == "match")
        # stats::filter does the rolling mean in C; ~20x faster than vapply for
        # mitogenome-scale alignments.
        win_pct  <- as.numeric(stats::filter(is_match, rep(1 / win, win), sides = 1)) * 100
        win_pct  <- win_pct[!is.na(win_pct)]
        n_wins   <- length(win_pct)
        # Pad x=0 and x=100 with edge values to fill to plot boundaries
        aln_win_df <- data.frame(
          x = c(0, (seq_len(n_wins) + win / 2 - 0.5) / aln_len * 100, 100),
          y = c(win_pct[1], win_pct, win_pct[n_wins])
        )

        aln_plot <- ggplot2::ggplot(aln_win_df,
          ggplot2::aes(x = x, y = y)
        ) +
          ggplot2::geom_area(fill = "#60BD68", colour = NA) +
          ggplot2::geom_line(colour = "#60BD68") +
          ggplot2::scale_x_continuous(expand = c(0, 0), limits = c(0, 100)) +
          ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) +
          ggplot2::coord_cartesian(clip = "off") +
          ggthemes::theme_tufte() +
          ggplot2::theme(
            legend.position  = "none",
            axis.title       = ggplot2::element_blank(),
            axis.text        = ggplot2::element_blank(),
            axis.ticks       = ggplot2::element_blank(),
            panel.background = ggplot2::element_rect(fill = "#F0F0F0", colour = NA),
            plot.margin      = ggplot2::margin(1, 0, 1, 0, "mm")
          )

        sample_plot <- ggplot2::ggplot(sample_df) +
          ggplot2::aes(xmin = xmin, xmax = xmax, forward = direction == "+",
                       fill = type, y = 0, label = gene) +
          gene_track

        ref_plot <- ggplot2::ggplot(ref_df) +
          ggplot2::aes(xmin = xmin, xmax = xmax, forward = direction == "+",
                       fill = type, y = 0, label = gene) +
          gene_track

        print(sample_plot / aln_plot / ref_plot +
                patchwork::plot_layout(heights = c(3, 1, 3)))

      } else {
        # Fallback: no alignment - 2-track view with normalised genome coordinates
        anchor_gene <- sample_genes |>
          dplyr::arrange(pos1) |> dplyr::pull(gene) |> head(1)
        anchor_ref <- rv$blast_ref |>
          dplyr::filter(gene == anchor_gene) |> dplyr::arrange(pos1)
        rotation <- if (fig_ctx()$topology == "circular" && nrow(anchor_ref) > 0) {
          as.integer(anchor_ref$pos1[1]) - 1L
        } else {
          0L
        }
        ref_rotated <- rv$blast_ref |>
          dplyr::mutate(
            pos1_r = as.integer(((as.integer(pos1) - 1L - rotation) %% ref_length) + 1L),
            pos2_r = as.integer(((as.integer(pos2) - 1L - rotation) %% ref_length) + 1L)
          ) |>
          dplyr::mutate(
            pos2_r = dplyr::if_else(pos2_r < pos1_r, pos2_r + ref_length, pos2_r),
            pos1_r = dplyr::if_else(pos2_r > ref_length, 1L, pos1_r)
          ) |>
          dplyr::mutate(type = factor(type, levels = c("ctrl", "PCG", "rRNA", "tRNA")))
        sample_df  <- sample_genes |>
          dplyr::mutate(type = factor(type, levels = c("ctrl", "PCG", "rRNA", "tRNA")))
        sample_pct <- sample_df |>
          dplyr::mutate(xmin = pos1 / sample_len * 100, xmax = pos2 / sample_len * 100)
        ref_pct    <- ref_rotated |>
          dplyr::mutate(xmin = pos1_r / ref_length * 100, xmax = pos2_r / ref_length * 100)
        sample_plot <- ggplot2::ggplot(sample_pct) +
          ggplot2::aes(xmin = xmin, xmax = xmax, forward = direction == "+",
                       fill = type, y = 0, label = gene) + gene_track
        ref_plot <- ggplot2::ggplot(ref_pct) +
          ggplot2::aes(xmin = xmin, xmax = xmax, forward = direction == "+",
                       fill = type, y = 0, label = gene) + gene_track
        print(sample_plot / ref_plot + patchwork::plot_layout(heights = c(1, 1)))
      }
      dev.off()
      list(
        src = outfile,
        contentType = "image/png",
        width = img_w,
        height = img_h,
        alt = "Synteny plot"
      )
      },
      deleteFile = TRUE
    )

    # Zoomed base-pair view of selected gene's alignment region ----
    output$synteny_zoom_ui <- renderUI({
      req(isTRUE(input$synteny_zoom))
      has_aln <- !is.null(rv$blast_ref_aln) && nrow(rv$blast_ref_aln) > 0 &&
        isTRUE(nzchar(rv$blast_ref_aln$aligned_sample[1])) &&
        isTRUE(nzchar(rv$blast_ref_aln$aligned_ref[1]))
      if (!has_aln) {
        return(div(style = "color: #888; font-style: italic; font-size: 11px;",
                   "No alignment available."))
      }
      sel_idx <- selected()
      click_col <- zoom_click_col()
      if ((is.null(sel_idx) || length(sel_idx) == 0) && is.null(click_col)) {
        return(div(style = "color: #888; font-style: italic; font-size: 11px;",
                   "Select a gene row, or click the synteny plot above, to view a base-pair alignment."))
      }
      win <- zoom_window_rv()
      px_per_col <- 14L
      plot_w <- as.integer(win * px_per_col)
      ctx <- req(fig_ctx())
      sample_lbl <- ctx$ID
      ref_acc    <- ctx$blast_accession
      header_txt <- if (!is.null(click_col)) {
        s_chars   <- strsplit(rv$blast_ref_aln$aligned_sample[1], "")[[1]]
        anchor_bp <- as.integer(cumsum(s_chars != "-")[click_col])
        sprintf("Zoom: clicked region | sample pos ~%d | %d bp window",
                anchor_bp, win)
      } else {
        gene_name <- rv$annotations$gene[sel_idx]
        gene_pos1 <- as.integer(rv$annotations$pos1[sel_idx])
        gene_pos2 <- as.integer(rv$annotations$pos2[sel_idx])
        sprintf("Zoom: %s | sample pos %d-%d | %d bp window",
                gene_name, gene_pos1, gene_pos2, win)
      }
      div(
        div(style = "font-size: 11px; color: #555; margin-bottom: 4px;",
            header_txt),
        div(
          style = "display: flex; align-items: flex-start;",
          div(
            # Labels pinned to track centres in the 180 px plot.
            # y limits: -0.5 to 4.9 (range 5.4). 2 mm plot.margin ~ 3 % of 180 px.
            # top % = 3.15 + (4.9 - track_y) / 5.4 * 93.7
            style = paste0("flex-shrink: 0; width: 160px; font-size: 11px; ",
                           "padding-right: 6px; box-sizing: border-box; ",
                           "position: relative; height: 180px;"),
            div(style = "position: absolute; top: 19%; transform: translateY(-50%); color: #888; font-size: 10px;",
                "sample annotations"),
            div(style = "position: absolute; top: 36%; transform: translateY(-50%); word-break: break-word;",
                sample_lbl),
            div(style = "position: absolute; top: 54%; transform: translateY(-50%); color: #888; font-size: 10px;",
                "identity"),
            div(style = "position: absolute; top: 71%; transform: translateY(-50%); word-break: break-word;",
                ref_acc),
            div(style = "position: absolute; top: 88%; transform: translateY(-50%); color: #888; font-size: 10px;",
                "ref annotations")
          ),
          div(
            style = "overflow-x: auto; flex: 1;",
            plotOutput(ns("synteny_zoom_plot"),
                       width = paste0(plot_w, "px"), height = "180px")
          )
        ),
        div(
          style = "display: flex; align-items: center; gap: 4px; margin-top: 6px;",
          tags$style(HTML(sprintf(
            paste0(
              "#%s.shiny-input-container {",
              "  display: flex !important; flex-direction: row;",
              "  align-items: center; gap: 6px;",
              "  width: auto !important; margin-bottom: 0; }",
              "#%s.shiny-input-container label { margin-bottom: 0; font-weight: normal; white-space: nowrap; }",
              "#%s.shiny-input-container input { width: 70px !important; }"
            ),
            ns("synteny_zoom_window"), ns("synteny_zoom_window"), ns("synteny_zoom_window")
          ))),
          numericInput(ns("synteny_zoom_window"), label = "window size (bp)",
                       value = isolate(input$synteny_zoom_window) %||% 200L,
                       min = 30L, max = 2000L, step = 50L,
                       width = "auto")
        )
      )
    })

    # Open BLAST Reference Synteny details when zoom is enabled
    observeEvent(input$synteny_zoom, ignoreInit = TRUE, {
      if (isTRUE(input$synteny_zoom)) {
        shinyjs::runjs(sprintf(
          "var d = document.getElementById('%s'); if (d) d.open = true;",
          ns("blast_synteny_details")
        ))
      }
    })

    # Validated window size - only invalidates when value actually changes (breaks render loop)
    zoom_window_rv <- reactiveVal(200L)

    # Clamp the window-size input to [30, 2000] and drive zoom_window_rv
    observeEvent(input$synteny_zoom_window, ignoreInit = TRUE, {
      v <- input$synteny_zoom_window
      if (is.null(v) || is.na(v)) return()
      clamped <- max(30L, min(2000L, as.integer(v)))
      if (clamped != v) updateNumericInput(session, "synteny_zoom_window", value = clamped)
      if (clamped != zoom_window_rv()) zoom_window_rv(clamped)
    })

    output$synteny_zoom_plot <- renderPlot({
      req(isTRUE(input$synteny_zoom))
      req(rv$blast_ref_aln, rv$annotations, rv$blast_ref)
      req(nrow(rv$blast_ref_aln) > 0)
      req(nzchar(rv$blast_ref_aln$aligned_sample[1]))
      sel_idx   <- selected()
      click_col <- zoom_click_col()
      req(length(sel_idx) > 0 || !is.null(click_col))

      ref_length     <- rv$blast_ref$ref_length[1]
      aln_rotation   <- as.integer(rv$blast_ref_aln$rotation[1])
      aligned_sample <- rv$blast_ref_aln$aligned_sample[1]
      aligned_ref    <- rv$blast_ref_aln$aligned_ref[1]
      s_chars <- strsplit(aligned_sample, "")[[1]]
      r_chars <- strsplit(aligned_ref,    "")[[1]]
      aln_len <- length(s_chars)
      s_nongap <- which(s_chars != "-")
      r_nongap <- which(r_chars != "-")

      win <- zoom_window_rv()
      anchor_bp <- if (!is.null(click_col)) {
        # Sample bp position at the clicked alignment column
        as.integer(cumsum(s_chars != "-")[click_col])
      } else {
        as.integer(rv$annotations$pos1[sel_idx])
      }
      # Window starts 20 bp upstream of anchor (clamped to sequence bounds)
      start_idx <- pmin(pmax(anchor_bp - 20L, 1L), length(s_nongap))
      win_start <- max(1L, s_nongap[start_idx])
      win_end   <- min(aln_len, win_start + win - 1L)
      win_start <- max(1L, win_end - win + 1L)
      win_cols  <- win_start:win_end
      win_s <- s_chars[win_cols]
      win_r <- r_chars[win_cols]

      col_class <- dplyr::case_when(
        win_s == "-" ~ "gap",
        win_r == "-" ~ "gap",
        tolower(win_s) == tolower(win_r) ~ "match",
        TRUE ~ "mismatch"
      )
      mid_fill <- dplyr::case_when(
        col_class == "match"    ~ "#60BD68",
        col_class == "mismatch" ~ "#E55330",
        TRUE                    ~ "#CCCCCC"
      )
      base_color <- function(b) {
        u <- toupper(b)
        dplyr::case_when(
          u == "A" ~ "#D9342B",
          u == "C" ~ "#3878C5",
          u == "G" ~ "#E6A500",
          u == "T" ~ "#2D9E3F",
          b == "-" ~ "#BBBBBB",
          TRUE     ~ "#666666"
        )
      }

      df <- data.frame(
        col      = seq_along(win_cols),
        s_char   = win_s,
        r_char   = win_r,
        s_color  = base_color(win_s),
        r_color  = base_color(win_r),
        mid_fill = mid_fill,
        stringsAsFactors = FALSE
      )

      rle_fill    <- rle(mid_fill)
      rle_ends    <- cumsum(rle_fill$lengths)
      rle_starts  <- c(1L, head(rle_ends, -1L) + 1L)
      identity_df <- data.frame(
        xmin = rle_starts - 0.5,
        xmax = rle_ends   + 0.5,
        fill = rle_fill$values,
        stringsAsFactors = FALSE
      )

      type_color <- function(t) {
        out <- unname(gene_type_fill[as.character(t)])
        ifelse(is.na(out), "#888888", out)
      }
      to_local <- function(aln_col) aln_col - win_start + 1L

      # Sample genes overlapping the window - project pos1/pos2 to alignment cols
      sg <- rv$annotations |> dplyr::filter(pos1 > 0)
      sg_aln1 <- s_nongap[pmin(pmax(as.integer(sg$pos1), 1L), length(s_nongap))]
      sg_aln2 <- s_nongap[pmin(pmax(as.integer(sg$pos2), 1L), length(s_nongap))]
      sg_in <- sg_aln2 >= win_start & sg_aln1 <= win_end
      sample_gene_df <- if (any(sg_in)) {
        data.frame(
          xmin = to_local(pmax(sg_aln1[sg_in], win_start)) - 0.5,
          xmax = to_local(pmin(sg_aln2[sg_in], win_end)) + 0.5,
          gene = sg$gene[sg_in],
          fill = type_color(sg$type[sg_in]),
          forward = sg$direction[sg_in] == "+",
          stringsAsFactors = FALSE
        )
      } else NULL

      # Ref genes - rotate to alignment-ref coords first, then map via r_nongap
      rg <- rv$blast_ref
      rg_pos1_r <- ((as.integer(rg$pos1) - 1L - aln_rotation) %% ref_length) + 1L
      rg_pos2_r <- ((as.integer(rg$pos2) - 1L - aln_rotation) %% ref_length) + 1L
      rg_ok <- rg_pos2_r >= rg_pos1_r  # skip wrap-around genes
      rg_aln1 <- r_nongap[pmin(pmax(rg_pos1_r, 1L), length(r_nongap))]
      rg_aln2 <- r_nongap[pmin(pmax(rg_pos2_r, 1L), length(r_nongap))]
      rg_in <- rg_ok & rg_aln2 >= win_start & rg_aln1 <= win_end
      ref_gene_df <- if (any(rg_in)) {
        data.frame(
          xmin = to_local(pmax(rg_aln1[rg_in], win_start)) - 0.5,
          xmax = to_local(pmin(rg_aln2[rg_in], win_end)) + 0.5,
          gene = rg$gene[rg_in],
          fill = type_color(rg$type[rg_in]),
          forward = rg$direction[rg_in] == "+",
          stringsAsFactors = FALSE
        )
      } else NULL

      # Vertical guide lines every 10 cols (behind everything)
      guide_x <- seq(10L, length(win_cols), by = 10L)

      # Layout: y=0 ref gene, y=1 ref bases, y=2 identity, y=3 sample bases, y=4 sample gene
      p <- ggplot2::ggplot(df) +
        ggplot2::geom_segment(
          data = data.frame(x = guide_x + 0.5),
          ggplot2::aes(x = x, xend = x, y = -0.5, yend = 4.9),
          inherit.aes = FALSE,
          colour = "#A0A0A0", linewidth = 0.6
        ) +
        # Sample gene track
        (if (!is.null(sample_gene_df)) list(
          gggenes::geom_gene_arrow(
            data = sample_gene_df,
            ggplot2::aes(xmin = xmin, xmax = xmax, y = 4, fill = fill, forward = forward),
            inherit.aes = FALSE, alpha = gene_type_alpha,
            arrow_body_height = ggplot2::unit(6, "mm"),
            arrowhead_height  = ggplot2::unit(6, "mm"),
            arrowhead_width   = ggplot2::unit(3, "mm")
          ),
          gggenes::geom_gene_label(
            data = sample_gene_df,
            ggplot2::aes(xmin = xmin, xmax = xmax, y = 4, label = gene),
            inherit.aes = FALSE, align = "left",
            height = ggplot2::unit(4.5, "mm")
          )
        ) else NULL) +
        # Identity bar - merged runs reduce render items
        ggplot2::geom_rect(
          data = identity_df,
          ggplot2::aes(xmin = xmin, xmax = xmax, ymin = 1.65, ymax = 2.35, fill = fill),
          inherit.aes = FALSE
        ) +
        # Sample base letters
        ggplot2::geom_text(
          ggplot2::aes(x = col, y = 3, label = s_char, colour = s_color),
          family = "mono", size = 5, fontface = "bold"
        ) +
        # Ref base letters
        ggplot2::geom_text(
          ggplot2::aes(x = col, y = 1, label = r_char, colour = r_color),
          family = "mono", size = 5, fontface = "bold"
        ) +
        # Ref gene track
        (if (!is.null(ref_gene_df)) list(
          gggenes::geom_gene_arrow(
            data = ref_gene_df,
            ggplot2::aes(xmin = xmin, xmax = xmax, y = 0, fill = fill, forward = forward),
            inherit.aes = FALSE, alpha = gene_type_alpha,
            arrow_body_height = ggplot2::unit(6, "mm"),
            arrowhead_height  = ggplot2::unit(6, "mm"),
            arrowhead_width   = ggplot2::unit(3, "mm")
          ),
          gggenes::geom_gene_label(
            data = ref_gene_df,
            ggplot2::aes(xmin = xmin, xmax = xmax, y = 0, label = gene),
            inherit.aes = FALSE, align = "left",
            height = ggplot2::unit(4.5, "mm")
          )
        ) else NULL) +
        ggplot2::scale_fill_identity() +
        ggplot2::scale_colour_identity() +
        ggplot2::scale_x_continuous(expand = c(0, 0),
                                    limits = c(0.5, length(win_cols) + 0.5)) +
        ggplot2::scale_y_continuous(limits = c(-0.5, 4.9), expand = c(0, 0)) +
        ggplot2::coord_cartesian(clip = "off") +
        ggthemes::theme_tufte() +
        ggplot2::theme(
          legend.position = "none",
          axis.title      = ggplot2::element_blank(),
          axis.text       = ggplot2::element_blank(),
          axis.ticks      = ggplot2::element_blank(),
          plot.margin     = ggplot2::margin(2, 2, 2, 2, "mm")
        )
      p
    })

    # Synteny overview click -> zoom centered at click x.
    # Patchwork plots can break ggplot's data-space coordmap, so we compute the
    # fraction along the plot width using CSS pixel coords (which are reliable),
    # divided by the plot's pixel width (set in the UI).
    observeEvent(input$synteny_click, {
      ev <- input$synteny_click
      req(ev)
      has_aln <- !is.null(rv$blast_ref_aln) && nrow(rv$blast_ref_aln) > 0 &&
        isTRUE(nzchar(rv$blast_ref_aln$aligned_sample[1]))
      if (!has_aln) return()
      aln_len   <- nchar(rv$blast_ref_aln$aligned_sample[1])
      plot_w_px <- max(rv$updating$length %||% 800L, 800L)
      px <- if (!is.null(ev$coords_css$x)) {
        ev$coords_css$x
      } else {
        (ev$x %||% 0) / 100 * plot_w_px
      }
      frac <- max(0, min(1, px / plot_w_px))
      col  <- max(1L, min(aln_len, as.integer(round(frac * aln_len))))
      zoom_click_col(col)
      shinyjs::runjs(sprintf(
        paste0("var el=document.getElementById('%s');",
               "if(el && !el.checked){el.checked=true;",
               "Shiny.setInputValue('%s', true);}"),
        ns("synteny_zoom"), ns("synteny_zoom")
      ))
    })

    ## Auto scroll ----
    observeEvent(selected(), {
      req(selected())
      # New gene selection overrides any prior click anchor
      zoom_click_col(NULL)
      session$sendCustomMessage(
        "hScroll", list(id = ns("coverageDiv"), px = as.numeric(rv$annotations$pos1[selected()]))
      )
      if (!is.null(rv$blast_ref) && nrow(rv$blast_ref) > 0 && !is.null(rv$coverage)) {
        sample_genes <- rv$annotations |> dplyr::filter(pos1 > 0)
        sample_len <- max(c(rv$coverage$Position, sample_genes$pos2), na.rm = TRUE)
        w <- max(rv$updating$length %||% 800L, 800L)
        scroll_px <- as.numeric(rv$annotations$pos1[selected()]) / sample_len * w
        session$sendCustomMessage(
          "hScroll", list(id = ns("syntenyScrollDiv"), px = scroll_px)
        )
      }
    })

    # MSA ----
    # Non-reactive cache for the reference-only MSA. Avoids re-running
    # DECIPHER::AlignSeqs(refs) on every codon edit (refs don't change).
    ref_msa_cache <- new.env(parent = emptyenv())
    ref_msa_cache$msa <- NULL
    ref_msa_cache$key <- NULL

    init("align_now")
    observeEvent(input$align, ignoreInit = T, {
      shinyWidgets::updatePrettyCheckbox(
        inputId = "local_blast",
        value = FALSE
      )
      trigger("align_now")
    })
    # Debounce align_now: rapid codon-edit clicks collapse into one MSA rebuild
    # after the user pauses for ~250ms.
    align_now_dbnc <- shiny::debounce(
      shiny::reactive(gargoyle::watch("align_now")),
      250
    )
    observeEvent(align_now_dbnc(), ignoreInit = TRUE, {
      # check if user wants to use fewer reference samples in alignment
      if (isTRUE(input$reduce_align)){ n_hits = 5 } else { n_hits = Inf }

      req(rv$annotations$type[selected()] == "PCG")

      hits <- rv$local_hits %||% json_parse(rv$annotations$refHits[selected()], TRUE) |>
        dplyr::slice_head(n = n_hits)

      focal <- rv$annotations$translation[selected()] |>
        setNames(paste(rv$annotations$gene[selected()], "(focal)"))

      new_alignment <- list()
      if(nrow(hits)==0){
        new_alignment$seqs <- character(0)
        new_alignment$aln <- Biostrings::AAStringSet(focal)
        new_alignment$alignmentHeight <- 40
        new_alignment$id <- stringr::str_glue(
          "<b>Max Similarity:</b> n/a"
        )
      }else{
        new_alignment$seqs <- hits |>
          dplyr::pull(target, name = Taxon)
        # Cache the reference-only MSA across codon edits: the references don't
        # change while the user walks codons, so reuse the prior MSA and add
        # the (cheap) focal sequence via profile alignment.
        ref_key <- paste(selected(), n_hits, paste(new_alignment$seqs, collapse = "|"), sep = "::")
        if (!identical(ref_msa_cache$key, ref_key)) {
          ref_set <- Biostrings::AAStringSet(new_alignment$seqs)
          ref_msa_cache$msa <- if (length(ref_set) == 1L) {
            ref_set
          } else {
            DECIPHER::AlignSeqs(ref_set, verbose = FALSE)
          }
          ref_msa_cache$key <- ref_key
        }
        focal_set <- Biostrings::AAStringSet(focal)
        new_alignment$aln <- DECIPHER::AlignProfiles(
          focal_set, ref_msa_cache$msa
        )
        new_alignment$alignmentHeight <- 20 + (length(new_alignment$seqs) * 20)
        new_alignment$id <- stringr::str_glue(
          "<b>Max Similarity:</b> {ifelse(max(hits$similarity)<25,'-',paste0(round(max(hits$similarity),1),'%'))}"
        )
      }
      new_alignment$stop <- stringr::str_glue(
        "<b>Stop Codon:</b> {rv$annotations$stop_codon[selected()]}"
      )
      new_alignment$start <- stringr::str_glue(
        "<b>Start Codon:</b> {rv$annotations$start_codon[selected()]}"
      )
      new_alignment$internal_stop <- ifelse(
        stringr::str_detect(rv$annotations$translation[selected()], "\\*"),
        paste("<span>", as.character(icon("warning")), "<b>Internal Stop Detected</b>", as.character(icon("warning")), "<span>"),
        ""
      )
      rv$alignment <- new_alignment
    })
    output$msa_header <- renderUI({
      div(
        style = "display: flex; gap: 25px;",
        p(HTML(rv$alignment$id)),
        p(HTML(rv$alignment$start)),
        p(HTML(rv$alignment$stop)),
        p(HTML(rv$alignment$internal_stop))
      )
    })
    output$msa <- msaR::renderMsaR({
      msa <- msaR::msaR(
        req(rv$alignment$aln),
        overviewbox = FALSE,
        seqlogo = FALSE,
        menu = FALSE,
        conservation = TRUE,
        labelNameLength = 200,
        colorscheme = "zappo",
        rowheight = 20,
        alignmentHeight = min(rv$alignment$alignmentHeight, 200)
      )
      isolate({
        if (rv$editing$stop_aln %||% FALSE) {
          later::later({
            ~ session$sendCustomMessage("rightScroll", list(foo = "bar"))
          })
        }
      })
      return(msa)
    })


    # Notes ----
    notes_update <- debounce(reactive(input$notes), 500)
    observeEvent(notes_update(), ignoreInit = T, ignoreNULL = T, {
      cleaned <- notes_update() |> stringr::str_remove_all(",")
      # Compare against the last-saved value (from rv$data) rather than rv$updating,
      # so clearing notes back to empty still persists.
      saved <- (rv$data$annotate_notes[rv$data$ID == rv$updating$ID])[1]
      req(cleaned != (saved %|NA|% ""))
      # Persist without mutating rv$updating: writing notes into rv$updating would
      # invalidate every figure that reads it (coverage/synteny), reloading them on
      # each keystroke.
      notes_df <- data.frame(ID = rv$updating$ID, annotate_notes = cleaned)
      dplyr::tbl(session$userData$con, "annotate") |>
        dplyr::rows_update(
          notes_df,
          by = "ID",
          unmatched = "ignore",
          copy = TRUE,
          in_place = TRUE
        )
      rv$data <- rv$data |>
        dplyr::rows_update(notes_df, by = "ID")
      trigger("update_annotate_table")
    })

    # Delete Annotation ----
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
        text = "This will completely remove the selected annotation. Details of the gene name and position of the deleted annotation will be added to the notes section.",
        btn_colors = c("#0056b3", "#0056b3")
      )
    })
    observeEvent(input$confirm_delete, {
      req(input$confirm_delete)
      update <- rv$annotations[selected(), ] |>
        dplyr::mutate(
          pos1 = 0,
          pos2 = 0,
          length = 0,
          time_stamp = as.numeric(Sys.time()),
          gene = paste0(rv$annotations[selected(), "gene"], "_DELETED_", as.numeric(Sys.time())) # hack to make sure deleted gene has a unique key (ID + path + scaffold + gene + pos1)
        )
      note <- stringr::str_glue(
        "DELETED: from {rv$annotations$pos1[selected()]}-{rv$annotations$pos2[selected()]}"
      )
      update$notes <- paste(note, rv$annotations$notes[selected()] %|NA|% "", sep = "; ") |>
        stringr::str_remove("; $")
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
          rv$annotations |>
            dplyr::select(-faa, -fas),
          by = "ID",
          conflict = "ignore",
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
      assembly <- get_assembly(
        ID = rv$annotations$ID[selected()],
        path = rv$annotations$path[selected()],
        scaffold = rv$annotations$scaffold[selected()],
        con = session$userData$con
      )
      ## Rotate to cut point ----
      if (start > 1) {
        assembly <- Biostrings::xscat(
          Biostrings::subseq(assembly, start, assembly@ranges@width),
          Biostrings::subseq(assembly, 1, start - 1)
        )

        ## Rotate coverage ----
        # TODO! use database
        rv$coverage <- dplyr::bind_rows(
          rv$coverage[start:assembly@ranges@width, ],
          rv$coverage[1:(start - 1), ]
        ) |>
          dplyr::mutate(Position = dplyr::row_number())
        readr::write_csv(
          rv$coverage,
          file.path(
            session$userData$dir_out,
            rv$updating$ID,
            "annotate",
            paste0(rv$updating$ID, "_coverageStats_", rv$annotations$path[selected()], ".csv")
          ),
          quote = "none"
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
            rv$annotations |>
              dplyr::select(-faa, -fas),
            by = "ID",
            conflict = "ignore",
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
      # Read the live notes input (rv$updating$annotate_notes is no longer synced on
      # every keystroke) so notes typed this session are preserved.
      cur_notes <- (input$notes %||% rv$updating$annotate_notes) %|NA|% ""
      rv$updating$annotate_notes <- paste(note, cur_notes, sep = "; ")
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

    # Mark ID verified ----
    observeEvent(input$ID_verified, {
      if(is.na(rv$updating$ID_verified)) {
        updateActionButton(session, "ID_verified")
        rv$updating$ID_verified <- "yes"
        dplyr::tbl(session$userData$con, "annotate") |>
          dplyr::rows_update(
            rv$updating[, c("ID", "ID_verified")],
            by = "ID",
            unmatched = "ignore",
            copy = TRUE,
            in_place = TRUE
          )
        rv$data <- rv$data |>
          dplyr::rows_update(rv$updating[, c("ID", "ID_verified")], by = "ID")
      } else if(as.character(rv$updating$ID_verified) == "no"){
        updateActionButton(session, "ID_verified")
        rv$updating$ID_verified <- "yes"
        dplyr::tbl(session$userData$con, "annotate") |>
          dplyr::rows_update(
            rv$updating[, c("ID", "ID_verified")],
            by = "ID",
            unmatched = "ignore",
            copy = TRUE,
            in_place = TRUE
          )
        rv$data <- rv$data |>
          dplyr::rows_update(rv$updating[, c("ID", "ID_verified")], by = "ID")
      } else {
        updateActionButton(session, "ID_verified")
        rv$updating$ID_verified <- "no"
        dplyr::tbl(session$userData$con, "annotate") |>
          dplyr::rows_update(
            rv$updating[, c("ID", "ID_verified")],
            by = "ID",
            unmatched = "ignore",
            copy = TRUE,
            in_place = TRUE
          )
        rv$data <- rv$data |>
          dplyr::rows_update(rv$updating[, c("ID", "ID_verified")], by = "ID")
      }
    }) # END ID VERIFIED

    # Mark as reviewed ----
    observeEvent(input$reviewed, {
      if (as.character(rv$updating$reviewed) == "no") {
        updateActionButton(session, "reviewed")
        rv$updating$reviewed <- "yes"
        dplyr::tbl(session$userData$con, "annotate") |>
          dplyr::rows_update(
            rv$updating[, c("ID", "reviewed")],
            by = "ID",
            unmatched = "ignore",
            copy = TRUE,
            in_place = TRUE
          )
        rv$data <- rv$data |>
          dplyr::rows_update(rv$updating[, c("ID", "reviewed")], by = "ID")
      } else {
        updateActionButton(session, "reviewed")
        rv$updating$reviewed <- "no"
        dplyr::tbl(session$userData$con, "annotate") |>
          dplyr::rows_update(
            rv$updating[, c("ID", "reviewed")],
            by = "ID",
            unmatched = "ignore",
            copy = TRUE,
            in_place = TRUE
          )
        rv$data <- rv$data |>
          dplyr::rows_update(rv$updating[, c("ID", "reviewed")], by = "ID")
      }
    }) # END REVIEWED

    # Mark as problematic ----
    observeEvent(input$problematic, {
      if (is.na(rv$updating$problematic)) {
        updateActionButton(session, "problematic")
        rv$updating$problematic <- "yes"
        dplyr::tbl(session$userData$con, "annotate") |>
          dplyr::rows_update(
            rv$updating[, c("ID", "problematic")],
            by = "ID",
            unmatched = "ignore",
            copy = TRUE,
            in_place = TRUE
          )
        rv$data <- rv$data |>
          dplyr::rows_update(rv$updating[, c("ID", "problematic")], by = "ID")
      } else {
        updateActionButton(session, "problematic")
        rv$updating$problematic <- NA_character_
        dplyr::tbl(session$userData$con, "annotate") |>
          dplyr::rows_update(
            rv$updating[, c("ID", "problematic")],
            by = "ID",
            unmatched = "ignore",
            copy = TRUE,
            in_place = TRUE
          )
        rv$data <- rv$data |>
          dplyr::rows_update(rv$updating[, c("ID", "problematic")], by = "ID")
      }
    }) # END PROBLEMATIC

    # Poor BLAST reference toggle ----
    observeEvent(input$poor_blast_ref_toggle, ignoreInit = TRUE, {
      val <- if (isTRUE(input$poor_blast_ref_toggle)) "poor" else "good"
      rv$updating$poor_blast_ref  <- val
      rv$updating$blast_ref_status <- val
      dplyr::tbl(session$userData$con, "assemble") |>
        dplyr::rows_update(
          data.frame(ID = rv$updating$ID, poor_blast_ref = val),
          by = "ID",
          unmatched = "ignore",
          copy = TRUE,
          in_place = TRUE
        )
      # Update both poor_blast_ref (source) and blast_ref_status (derived
      # alias used by the reactable cell renderer) so the Annotate table
      # reflects the new state on modal close via update_annotate_table.
      rv$data <- rv$data |>
        dplyr::rows_update(
          data.frame(
            ID               = rv$updating$ID,
            poor_blast_ref   = val,
            blast_ref_status = val
          ),
          by = "ID"
        )
    })

    # Edit Annotation ----
    observeEvent(input$edit_mode, {
      shinyjs::show("edit_mode_ctrls")
      shinyjs::show("save_edits")
      shinyjs::show("discard_edits")
      shinyjs::hide("edit_mode")
      rv$editing$idx <- selected()
      rv$editing$backup <- rv$annotations[selected(), ]
      rv$editing$params <- dplyr::left_join(
        dplyr::tbl(session$userData$con, "annotate") |>
          dplyr::select(ID, curate_opts) |>
          dplyr::filter(ID == !!rv$updating$ID),
        dplyr::tbl(session$userData$con, "curate_opts"),
        by = "curate_opts"
      ) |>
        dplyr::pull(params) |>
        json_parse() |>
        {
          \(x) modifyList(x$rules[[rv$annotations$gene[selected()]]], x$default_rules[[rv$annotations$type[selected()]]])
        }()
      rv$editing$assembly <- get_assembly(
        ID = rv$annotations$ID[selected()],
        path = rv$annotations$path[selected()],
        scaffold = rv$annotations$scaffold[selected()],
        con = session$userData$con
      )
    })

    ## Re align if user wants to show fewer reference samples ----
    observeEvent(input$reduce_align, {
      trigger("align_now")
    })

    ## Edit start-add ----
    init("start-add-simple")
    on("start-add-simple", {
      rv$editing$stop_aln <- FALSE
      codon <- "INIT"
      pos1 <- rv$annotations$pos1[selected()]
      pos2 <- rv$annotations$pos2[selected()]
      if (rv$annotations$direction[selected()] == "+") {
        while (codon %nin% rv$editing$params$start_codons) {
          pos1 <- pos1 - 3
          req(pos1 > 0)
          codon <- rv$editing$assembly |>
            Biostrings::subseq(pos1, pos1 + 2) |>
            as.character()
          if (isTRUE(input$single_codon)) break
        }
        rv$annotations$translation[selected()] <- rv$editing$assembly |>
          Biostrings::subseq(pos1, pos2 - nchar(rv$annotations$stop_codon[selected()])) |>
          Biostrings::translate(genetic.code = session$userData$gcode)
      }
      if (rv$annotations$direction[selected()] == "-") {
        while (codon %nin% rv$editing$params$start_codons) {
          pos2 <- pos2 + 3
          req(pos2 <= rv$editing$assembly@ranges@width)
          codon <- rv$editing$assembly |>
            Biostrings::subseq(pos2 - 2, pos2) |>
            Biostrings::reverseComplement() |>
            as.character()
          if (isTRUE(input$single_codon)) break
        }
        rv$annotations$translation[selected()] <- rv$editing$assembly |>
          Biostrings::subseq(pos1 + nchar(rv$annotations$stop_codon[selected()]), pos2) |>
          Biostrings::reverseComplement() |>
          Biostrings::translate(genetic.code = session$userData$gcode) |>
          as.character()
      }
      rv$annotations$pos1[selected()] <- pos1
      rv$annotations$pos2[selected()] <- pos2
      rv$annotations$start_codon[selected()] <- codon
      rv$annotations$length[selected()] <- abs(pos1 - pos2) + 1
      rv$annotations$start_codon[selected()] <- unname(codon)
    })
    init("start-add-simple-5") # speed things up and avoid making big alignments lots of times
    on("start-add-simple-5", {
      rv$editing$stop_aln <- FALSE
      codon <- "INIT"
      pos1 <- rv$annotations$pos1[selected()]
      pos2 <- rv$annotations$pos2[selected()]
      if (rv$annotations$direction[selected()] == "+") {
        for(counter in 1:5){
          keep_going <- TRUE # modified loop logic to avoid premature exit
          while (keep_going) {
            pos1 <- pos1 - 3
            req(pos1 > 0)
            codon <- rv$editing$assembly |>
              Biostrings::subseq(pos1, pos1 + 2) |>
              as.character()
            if (isTRUE(input$single_codon)) break
            keep_going <- codon %nin% rv$editing$params$start_codons
          }
        }
        rv$annotations$translation[selected()] <- rv$editing$assembly |>
          Biostrings::subseq(pos1, pos2 - nchar(rv$annotations$stop_codon[selected()])) |>
          Biostrings::translate(genetic.code = session$userData$gcode)
      }
      if (rv$annotations$direction[selected()] == "-") {
        for(counter in 1:5){
          keep_going <- TRUE # modified loop logic to avoid premature exit
          while (keep_going) {
            pos2 <- pos2 + 3
            req(pos2 <= rv$editing$assembly@ranges@width)
            codon <- rv$editing$assembly |>
              Biostrings::subseq(pos2 - 2, pos2) |>
              Biostrings::reverseComplement() |>
              as.character()
            if (isTRUE(input$single_codon)) break
            keep_going <- codon %nin% rv$editing$params$start_codons
          }
        }
        rv$annotations$translation[selected()] <- rv$editing$assembly |>
          Biostrings::subseq(pos1 + nchar(rv$annotations$stop_codon[selected()]), pos2) |>
          Biostrings::reverseComplement() |>
          Biostrings::translate(genetic.code = session$userData$gcode) |>
          as.character()
      }
      rv$annotations$pos1[selected()] <- pos1
      rv$annotations$pos2[selected()] <- pos2
      rv$annotations$start_codon[selected()] <- codon
      rv$annotations$length[selected()] <- abs(pos1 - pos2) + 1
      rv$annotations$start_codon[selected()] <- unname(codon)
    })
    init("start-add-simple-10") # speed things up and avoid making big alignments lots of times
    on("start-add-simple-10", {
      rv$editing$stop_aln <- FALSE
      codon <- "INIT"
      pos1 <- rv$annotations$pos1[selected()]
      pos2 <- rv$annotations$pos2[selected()]
      if (rv$annotations$direction[selected()] == "+") {
        for(counter in 1:10){
          keep_going <- TRUE # modified loop logic to avoid premature exit
          while (keep_going) {
            pos1 <- pos1 - 3
            req(pos1 > 0)
            codon <- rv$editing$assembly |>
              Biostrings::subseq(pos1, pos1 + 2) |>
              as.character()
            if (isTRUE(input$single_codon)) break
            keep_going <- codon %nin% rv$editing$params$start_codons
          }
        }
        rv$annotations$translation[selected()] <- rv$editing$assembly |>
          Biostrings::subseq(pos1, pos2 - nchar(rv$annotations$stop_codon[selected()])) |>
          Biostrings::translate(genetic.code = session$userData$gcode)
      }
      if (rv$annotations$direction[selected()] == "-") {
        for(counter in 1:10){
          keep_going <- TRUE # modified loop logic to avoid premature exit
          while (keep_going) {
            pos2 <- pos2 + 3
            req(pos2 <= rv$editing$assembly@ranges@width)
            codon <- rv$editing$assembly |>
              Biostrings::subseq(pos2 - 2, pos2) |>
              Biostrings::reverseComplement() |>
              as.character()
            if (isTRUE(input$single_codon)) break
            keep_going <- codon %nin% rv$editing$params$start_codons
          }
        }
        rv$annotations$translation[selected()] <- rv$editing$assembly |>
          Biostrings::subseq(pos1 + nchar(rv$annotations$stop_codon[selected()]), pos2) |>
          Biostrings::reverseComplement() |>
          Biostrings::translate(genetic.code = session$userData$gcode) |>
          as.character()
      }
      rv$annotations$pos1[selected()] <- pos1
      rv$annotations$pos2[selected()] <- pos2
      rv$annotations$start_codon[selected()] <- codon
      rv$annotations$length[selected()] <- abs(pos1 - pos2) + 1
      rv$annotations$start_codon[selected()] <- unname(codon)
    })
    observeEvent(input$`start-add-10`, {
      message("moving start position +10...")
      trigger("start-add-simple-10")
      shinyjs::delay(50, {
        trigger("re_align")
        message("DONE")
      })
    })
    observeEvent(input$`start-add-5`, {
      message("moving start position +5...")
      trigger("start-add-simple-5")
      shinyjs::delay(50, {
        trigger("re_align")
        message("DONE")
      })
    })
    observeEvent(input$`start-add`, {
      trigger("start-add-simple")
      trigger("re_align")
    })

    ## Edit start-minus ----
    init("start-minus-simple")
    on("start-minus-simple", {
      rv$editing$stop_aln <- FALSE
      codon <- "INIT"
      pos1 <- rv$annotations$pos1[selected()]
      pos2 <- rv$annotations$pos2[selected()]
      if (rv$annotations$direction[selected()] == "+") {
        while (codon %nin% rv$editing$params$start_codons) {
          pos1 <- pos1 + 3
          req(pos1 < pos2)
          codon <- rv$editing$assembly |>
            Biostrings::subseq(pos1, pos1 + 2) |>
            as.character()
          if (isTRUE(input$single_codon)) break
        }
        rv$annotations$translation[selected()] <- rv$editing$assembly |>
          Biostrings::subseq(pos1, pos2 - nchar(rv$annotations$stop_codon[selected()])) |>
          Biostrings::translate(genetic.code = session$userData$gcode) |>
          as.character()
      }
      if (rv$annotations$direction[selected()] == "-") {
        while (codon %nin% rv$editing$params$start_codons) {
          pos2 <- pos2 - 3
          req(pos2 > pos1)
          codon <- rv$editing$assembly |>
            Biostrings::subseq(pos2 - 2, pos2) |>
            Biostrings::reverseComplement() |>
            as.character()
          if (isTRUE(input$single_codon)) break
        }
        rv$annotations$translation[selected()] <- rv$editing$assembly |>
          Biostrings::subseq(pos1 + nchar(rv$annotations$stop_codon[selected()]), pos2) |>
          Biostrings::reverseComplement() |>
          Biostrings::translate(genetic.code = session$userData$gcode) |>
          as.character()
      }
      rv$annotations$pos1[selected()] <- pos1
      rv$annotations$pos2[selected()] <- pos2
      rv$annotations$length[selected()] <- abs(pos1 - pos2) + 1
      rv$annotations$start_codon[selected()] <- unname(codon)
    })
    init("start-minus-simple-5") # speed things up and avoid making big alignments lots of times
    on("start-minus-simple-5", {
      rv$editing$stop_aln <- FALSE
      codon <- "INIT"
      pos1 <- rv$annotations$pos1[selected()]
      pos2 <- rv$annotations$pos2[selected()]
      if (rv$annotations$direction[selected()] == "+") {
        for(counter in 1:5){
          keep_going <- TRUE # modified loop logic to avoid premature exit
          while (keep_going) {
            pos1 <- pos1 + 3
            req(pos1 > 0)
            codon <- rv$editing$assembly |>
              Biostrings::subseq(pos1, pos1 + 2) |>
              as.character()
            if (isTRUE(input$single_codon)) break
            keep_going <- codon %nin% rv$editing$params$start_codons
          }
        }
        rv$annotations$translation[selected()] <- rv$editing$assembly |>
          Biostrings::subseq(pos1, pos2 - nchar(rv$annotations$stop_codon[selected()])) |>
          Biostrings::translate(genetic.code = session$userData$gcode)
      }
      if (rv$annotations$direction[selected()] == "-") {
        for(counter in 1:5){
          keep_going <- TRUE # modified loop logic to avoid premature exit
          while (keep_going) {
            pos2 <- pos2 - 3
            req(pos2 <= rv$editing$assembly@ranges@width)
            codon <- rv$editing$assembly |>
              Biostrings::subseq(pos2 - 2, pos2) |>
              Biostrings::reverseComplement() |>
              as.character()
            if (isTRUE(input$single_codon)) break
            keep_going <- codon %nin% rv$editing$params$start_codons
          }
        }
        rv$annotations$translation[selected()] <- rv$editing$assembly |>
          Biostrings::subseq(pos1 + nchar(rv$annotations$stop_codon[selected()]), pos2) |>
          Biostrings::reverseComplement() |>
          Biostrings::translate(genetic.code = session$userData$gcode) |>
          as.character()
      }
      rv$annotations$pos1[selected()] <- pos1
      rv$annotations$pos2[selected()] <- pos2
      rv$annotations$start_codon[selected()] <- codon
      rv$annotations$length[selected()] <- abs(pos1 - pos2) + 1
      rv$annotations$start_codon[selected()] <- unname(codon)
    })
    init("start-minus-simple-10") # speed things up and avoid making big alignments lots of times
    on("start-minus-simple-10", {
      rv$editing$stop_aln <- FALSE
      codon <- "INIT"
      pos1 <- rv$annotations$pos1[selected()]
      pos2 <- rv$annotations$pos2[selected()]
      if (rv$annotations$direction[selected()] == "+") {
        for(counter in 1:10){
          keep_going <- TRUE # modified loop logic to avoid premature exit
          while (keep_going) {
            pos1 <- pos1 + 3
            req(pos1 > 0)
            codon <- rv$editing$assembly |>
              Biostrings::subseq(pos1, pos1 + 2) |>
              as.character()
            if (isTRUE(input$single_codon)) break
            keep_going <- codon %nin% rv$editing$params$start_codons
          }
        }
        rv$annotations$translation[selected()] <- rv$editing$assembly |>
          Biostrings::subseq(pos1, pos2 - nchar(rv$annotations$stop_codon[selected()])) |>
          Biostrings::translate(genetic.code = session$userData$gcode)
      }
      if (rv$annotations$direction[selected()] == "-") {
        for(counter in 1:10){
          keep_going <- TRUE # modified loop logic to avoid premature exit
          while (keep_going) {
            pos2 <- pos2 - 3
            req(pos2 <= rv$editing$assembly@ranges@width)
            codon <- rv$editing$assembly |>
              Biostrings::subseq(pos2 - 2, pos2) |>
              Biostrings::reverseComplement() |>
              as.character()
            if (isTRUE(input$single_codon)) break
            keep_going <- codon %nin% rv$editing$params$start_codons
          }
        }
        rv$annotations$translation[selected()] <- rv$editing$assembly |>
          Biostrings::subseq(pos1 + nchar(rv$annotations$stop_codon[selected()]), pos2) |>
          Biostrings::reverseComplement() |>
          Biostrings::translate(genetic.code = session$userData$gcode) |>
          as.character()
      }
      rv$annotations$pos1[selected()] <- pos1
      rv$annotations$pos2[selected()] <- pos2
      rv$annotations$start_codon[selected()] <- codon
      rv$annotations$length[selected()] <- abs(pos1 - pos2) + 1
      rv$annotations$start_codon[selected()] <- unname(codon)
    })
    observeEvent(input$`start-minus-10`, {
      message("moving start position -10...")
      trigger("start-minus-simple-10")
      shinyjs::delay(50, {
        trigger("re_align")
        message("DONE")
      })
    })
    observeEvent(input$`start-minus-5`, {
      message("moving start position -5...")
      trigger("start-minus-simple-5")
      shinyjs::delay(50, {
        trigger("re_align")
        message("DONE")
      })
    })
    observeEvent(input$`start-minus`, {
      trigger("start-minus-simple")
      trigger("re_align")
    })

    ## Edit stop-add ----
    init("stop-add-simple")
    on("stop-add-simple" , {
      rv$editing$stop_aln <- TRUE
      codon <- "INIT"
      pos1 <- rv$annotations$pos1[selected()]
      pos2 <- rv$annotations$pos2[selected()]
      if (rv$annotations$direction[selected()] == "+") {
        pos2 <- pos2 + (3 - nchar(rv$annotations$stop_codon[selected()]))
        while (!any(stringr::str_detect(rv$editing$params$stop_codons, paste0("^", codon)))) {
          pos2 <- pos2 + 3
          req(pos2 <= rv$editing$assembly@ranges@width)
          codon <- rv$editing$assembly |>
            Biostrings::subseq(pos2 - 2, pos2) |>
            as.character() |>
            stringr::str_extract(paste0("^", rv$editing$params$stop_codons)) |>
            na.omit() |>
            purrr::pluck(1)
          if (isTRUE(input$single_codon) && length(codon) > 0) break
          if (isTRUE(input$single_codon) && length(codon) == 0) {
            codon <- rv$editing$assembly |>
              Biostrings::subseq(pos2 - 2, pos2) |>
              as.character()
            break
          }
          codon <- codon %||% "INIT"
        }
        pos2 <- pos2 - (3 - nchar(codon))
        rv$annotations$translation[selected()] <- rv$editing$assembly |>
          Biostrings::subseq(pos1, pos2 - nchar(codon)) |>
          Biostrings::translate(genetic.code = session$userData$gcode) |>
          as.character()
      }
      if (rv$annotations$direction[selected()] == "-") {
        pos1 <- pos1 - (3 - nchar(rv$annotations$stop_codon[selected()]))
        while (!any(stringr::str_detect(rv$editing$params$stop_codons, paste0("^", codon)))) {
          pos1 <- pos1 - 3
          req(pos1 >= 1)
          codon <- rv$editing$assembly |>
            Biostrings::subseq(pos1, pos1 + 2) |>
            Biostrings::reverseComplement() |>
            as.character() |>
            stringr::str_extract(paste0("^", rv$editing$params$stop_codons)) |>
            na.omit() |>
            purrr::pluck(1)
          if (isTRUE(input$single_codon) && length(codon) > 0) break
          if (isTRUE(input$single_codon) && length(codon) == 0) {
            codon <- rv$editing$assembly |>
              Biostrings::subseq(pos1, pos1 + 2) |>
              Biostrings::reverseComplement() |>
              as.character()
            break
          }
          codon <- codon %||% "INIT"
        }
        pos1 <- pos1 + (3 - nchar(codon))
        rv$annotations$translation[selected()] <- rv$editing$assembly |>
          Biostrings::subseq(pos1 + nchar(codon), pos2) |>
          Biostrings::reverseComplement() |>
          Biostrings::translate(genetic.code = session$userData$gcode) |>
          as.character()
      }
      rv$annotations$pos1[selected()] <- pos1
      rv$annotations$pos2[selected()] <- pos2
      rv$annotations$length[selected()] <- abs(pos1 - pos2) + 1
      rv$annotations$stop_codon[selected()] <- unname(codon)
    })
    init("stop-add-simple-5")
    on("stop-add-simple-5" , {
      rv$editing$stop_aln <- TRUE
      codon <- "INIT"
      pos1 <- rv$annotations$pos1[selected()]
      pos2 <- rv$annotations$pos2[selected()]
      if (rv$annotations$direction[selected()] == "+") {
        pos2 <- pos2 + (3 - nchar(rv$annotations$stop_codon[selected()]))
        for(counter in 1:5){
          keep_going <- TRUE # modified loop logic to avoid premature exit
          while (keep_going) {
            pos2 <- pos2 + 3
            req(pos2 <= rv$editing$assembly@ranges@width)
            codon <- rv$editing$assembly |>
              Biostrings::subseq(pos2 - 2, pos2) |>
              as.character() |>
              stringr::str_extract(paste0("^", rv$editing$params$stop_codons)) |>
              na.omit() |>
              purrr::pluck(1)
            if (isTRUE(input$single_codon) && length(codon) > 0) break
            if (isTRUE(input$single_codon) && length(codon) == 0) {
              codon <- rv$editing$assembly |>
                Biostrings::subseq(pos2 - 2, pos2) |>
                as.character()
              break
            }
            codon <- codon %||% "INIT"
            keep_going <- !any(stringr::str_detect(rv$editing$params$stop_codons, paste0("^", codon)))
          }
        }
        pos2 <- pos2 - (3 - nchar(codon))
        rv$annotations$translation[selected()] <- rv$editing$assembly |>
          Biostrings::subseq(pos1, pos2 - nchar(codon)) |>
          Biostrings::translate(genetic.code = session$userData$gcode) |>
          as.character()
      }
      if (rv$annotations$direction[selected()] == "-") {
        pos1 <- pos1 - (3 - nchar(rv$annotations$stop_codon[selected()]))
        for(counter in 1:5){
          keep_going <- TRUE # modified loop logic to avoid premature exit
          while (keep_going) {
            pos1 <- pos1 - 3
            req(pos1 >= 1)
            codon <- rv$editing$assembly |>
              Biostrings::subseq(pos1, pos1 + 2) |>
              Biostrings::reverseComplement() |>
              as.character() |>
              stringr::str_extract(paste0("^", rv$editing$params$stop_codons)) |>
              na.omit() |>
              purrr::pluck(1)
            if (isTRUE(input$single_codon) && length(codon) > 0) break
            if (isTRUE(input$single_codon) && length(codon) == 0) {
              codon <- rv$editing$assembly |>
                Biostrings::subseq(pos1, pos1 + 2) |>
                Biostrings::reverseComplement() |>
                as.character()
              break
            }
            codon <- codon %||% "INIT"
            keep_going <- !any(stringr::str_detect(rv$editing$params$stop_codons, paste0("^", codon)))
          }
        }
        pos1 <- pos1 + (3 - nchar(codon))
        rv$annotations$translation[selected()] <- rv$editing$assembly |>
          Biostrings::subseq(pos1 + nchar(codon), pos2) |>
          Biostrings::reverseComplement() |>
          Biostrings::translate(genetic.code = session$userData$gcode) |>
          as.character()
      }
      rv$annotations$pos1[selected()] <- pos1
      rv$annotations$pos2[selected()] <- pos2
      rv$annotations$length[selected()] <- abs(pos1 - pos2) + 1
      rv$annotations$stop_codon[selected()] <- unname(codon)
    })
    init("stop-add-simple-10")
    on("stop-add-simple-10" , {
      rv$editing$stop_aln <- TRUE
      codon <- "INIT"
      pos1 <- rv$annotations$pos1[selected()]
      pos2 <- rv$annotations$pos2[selected()]
      if (rv$annotations$direction[selected()] == "+") {
        pos2 <- pos2 + (3 - nchar(rv$annotations$stop_codon[selected()]))
        for(counter in 1:10){
          keep_going <- TRUE # modified loop logic to avoid premature exit
          while (keep_going) {
            pos2 <- pos2 + 3
            req(pos2 <= rv$editing$assembly@ranges@width)
            codon <- rv$editing$assembly |>
              Biostrings::subseq(pos2 - 2, pos2) |>
              as.character() |>
              stringr::str_extract(paste0("^", rv$editing$params$stop_codons)) |>
              na.omit() |>
              purrr::pluck(1)
            if (isTRUE(input$single_codon) && length(codon) > 0) break
            if (isTRUE(input$single_codon) && length(codon) == 0) {
              codon <- rv$editing$assembly |>
                Biostrings::subseq(pos2 - 2, pos2) |>
                as.character()
              break
            }
            codon <- codon %||% "INIT"
            keep_going <- !any(stringr::str_detect(rv$editing$params$stop_codons, paste0("^", codon)))
          }
        }
        pos2 <- pos2 - (3 - nchar(codon))
        rv$annotations$translation[selected()] <- rv$editing$assembly |>
          Biostrings::subseq(pos1, pos2 - nchar(codon)) |>
          Biostrings::translate(genetic.code = session$userData$gcode) |>
          as.character()
      }
      if (rv$annotations$direction[selected()] == "-") {
        pos1 <- pos1 - (3 - nchar(rv$annotations$stop_codon[selected()]))
        for(counter in 1:10){
          keep_going <- TRUE # modified loop logic to avoid premature exit
          while (keep_going) {
            pos1 <- pos1 - 3
            req(pos1 >= 1)
            codon <- rv$editing$assembly |>
              Biostrings::subseq(pos1, pos1 + 2) |>
              Biostrings::reverseComplement() |>
              as.character() |>
              stringr::str_extract(paste0("^", rv$editing$params$stop_codons)) |>
              na.omit() |>
              purrr::pluck(1)
            if (isTRUE(input$single_codon) && length(codon) > 0) break
            if (isTRUE(input$single_codon) && length(codon) == 0) {
              codon <- rv$editing$assembly |>
                Biostrings::subseq(pos1, pos1 + 2) |>
                Biostrings::reverseComplement() |>
                as.character()
              break
            }
            codon <- codon %||% "INIT"
            keep_going <- !any(stringr::str_detect(rv$editing$params$stop_codons, paste0("^", codon)))
          }
        }
        pos1 <- pos1 + (3 - nchar(codon))
        rv$annotations$translation[selected()] <- rv$editing$assembly |>
          Biostrings::subseq(pos1 + nchar(codon), pos2) |>
          Biostrings::reverseComplement() |>
          Biostrings::translate(genetic.code = session$userData$gcode) |>
          as.character()
      }
      rv$annotations$pos1[selected()] <- pos1
      rv$annotations$pos2[selected()] <- pos2
      rv$annotations$length[selected()] <- abs(pos1 - pos2) + 1
      rv$annotations$stop_codon[selected()] <- unname(codon)
    })
    observeEvent(input$`stop-add-10`, {
      message("moving stop position +10...")
      trigger("stop-add-simple-10")
      shinyjs::delay(50, {
        trigger("re_align")
        message("DONE")
      })
    })
    observeEvent(input$`stop-add-5`, {
      message("moving stop position +5...")
      trigger("stop-add-simple-5")
      shinyjs::delay(50, {
        trigger("re_align")
        message("DONE")
      })
    })
    observeEvent(input$`stop-add`, {
      trigger("stop-add-simple")
      trigger("re_align")
    })

    ## Edit stop-minus ----
    init("stop-minus-simple")
    on("stop-minus-simple", {
      rv$editing$stop_aln <- TRUE
      codon <- "INIT"
      pos1 <- rv$annotations$pos1[selected()]
      pos2 <- rv$annotations$pos2[selected()]
      if (rv$annotations$direction[selected()] == "+") {
        pos2 <- pos2 + (3 - nchar(rv$annotations$stop_codon[selected()]))
        while (!any(stringr::str_detect(rv$editing$params$stop_codons, paste0("^", codon)))) {
          pos2 <- pos2 - 3
          req(pos2 <= rv$editing$assembly@ranges@width)
          codon <- rv$editing$assembly |>
            Biostrings::subseq(pos2 - 2, pos2) |>
            as.character() |>
            stringr::str_extract(paste0("^", rv$editing$params$stop_codons)) |>
            na.omit() |>
            purrr::pluck(1)
          if (isTRUE(input$single_codon) && length(codon) > 0){
            break
          }
          if (isTRUE(input$single_codon) && length(codon) == 0) {
            codon <- rv$editing$assembly |>
              Biostrings::subseq(pos2 - 2, pos2) |>
              as.character()
            break
          }
          codon <- codon %||% "INIT"
        }
        pos2 <- pos2 - (3 - nchar(codon))
        rv$annotations$translation[selected()] <- rv$editing$assembly |>
          Biostrings::subseq(pos1, pos2 - nchar(codon)) |>
          Biostrings::translate(genetic.code = session$userData$gcode) |>
          as.character()
      }
      if (rv$annotations$direction[selected()] == "-") {
        pos1 <- pos1 - (3 - nchar(rv$annotations$stop_codon[selected()]))
        while (!any(stringr::str_detect(rv$editing$params$stop_codons, paste0("^", codon)))) {
          pos1 <- pos1 + 3
          req(pos1 >= 1)
          codon <- rv$editing$assembly |>
            Biostrings::subseq(pos1, pos1 + 2) |>
            Biostrings::reverseComplement() |>
            as.character() |>
            stringr::str_extract(paste0("^", rv$editing$params$stop_codons)) |>
            na.omit() |>
            purrr::pluck(1)
          if (isTRUE(input$single_codon) && length(codon) > 0){
            break
          }
          if (isTRUE(input$single_codon) && length(codon) == 0) {
            codon <- rv$editing$assembly |>
              Biostrings::subseq(pos1, pos1 + 2) |>
              Biostrings::reverseComplement() |>
              as.character()
            break
          }
          codon <- codon %||% "INIT"
        }
        pos1 <- pos1 + (3 - nchar(codon))
        rv$annotations$translation[selected()] <- rv$editing$assembly |>
          Biostrings::subseq(pos1 + nchar(codon), pos2) |>
          Biostrings::reverseComplement() |>
          Biostrings::translate(genetic.code = session$userData$gcode) |>
          as.character()
      }
      rv$annotations$pos1[selected()] <- pos1
      rv$annotations$pos2[selected()] <- pos2
      rv$annotations$length[selected()] <- abs(pos1 - pos2) + 1
      rv$annotations$stop_codon[selected()] <- unname(codon)
    })
    init("stop-minus-simple-5")
    on("stop-minus-simple-5", {
      rv$editing$stop_aln <- TRUE
      codon <- "INIT"
      pos1 <- rv$annotations$pos1[selected()]
      pos2 <- rv$annotations$pos2[selected()]
      if (rv$annotations$direction[selected()] == "+") {
        pos2 <- pos2 + (3 - nchar(rv$annotations$stop_codon[selected()]))
        for(counter in 1:5){
          keep_going <- TRUE # modified loop logic to avoid premature exit
          while (keep_going) {
            pos2 <- pos2 - 3
            req(pos2 <= rv$editing$assembly@ranges@width)
            codon <- rv$editing$assembly |>
              Biostrings::subseq(pos2 - 2, pos2) |>
              as.character() |>
              stringr::str_extract(paste0("^", rv$editing$params$stop_codons)) |>
              na.omit() |>
              purrr::pluck(1)
            if (isTRUE(input$single_codon) && length(codon) > 0){
              break
            }
            if (isTRUE(input$single_codon) && length(codon) == 0) {
              codon <- rv$editing$assembly |>
                Biostrings::subseq(pos2 - 2, pos2) |>
                as.character()
              break
            }
            codon <- codon %||% "INIT"
            keep_going <- !any(stringr::str_detect(rv$editing$params$stop_codons, paste0("^", codon)))
          }
        }
        pos2 <- pos2 - (3 - nchar(codon))
        rv$annotations$translation[selected()] <- rv$editing$assembly |>
          Biostrings::subseq(pos1, pos2 - nchar(codon)) |>
          Biostrings::translate(genetic.code = session$userData$gcode) |>
          as.character()
      }
      if (rv$annotations$direction[selected()] == "-") {
        pos1 <- pos1 - (3 - nchar(rv$annotations$stop_codon[selected()]))
        for(counter in 1:5){
          keep_going <- TRUE # modified loop logic to avoid premature exit
          while (keep_going) {
            pos1 <- pos1 + 3
            req(pos1 >= 1)
            codon <- rv$editing$assembly |>
              Biostrings::subseq(pos1, pos1 + 2) |>
              Biostrings::reverseComplement() |>
              as.character() |>
              stringr::str_extract(paste0("^", rv$editing$params$stop_codons)) |>
              na.omit() |>
              purrr::pluck(1)
            if (isTRUE(input$single_codon) && length(codon) > 0){
              break
            }
            if (isTRUE(input$single_codon) && length(codon) == 0) {
              codon <- rv$editing$assembly |>
                Biostrings::subseq(pos1, pos1 + 2) |>
                Biostrings::reverseComplement() |>
                as.character()
              break
            }
            codon <- codon %||% "INIT"
            keep_going <- !any(stringr::str_detect(rv$editing$params$stop_codons, paste0("^", codon)))
          }
        }
        pos1 <- pos1 + (3 - nchar(codon))
        rv$annotations$translation[selected()] <- rv$editing$assembly |>
          Biostrings::subseq(pos1 + nchar(codon), pos2) |>
          Biostrings::reverseComplement() |>
          Biostrings::translate(genetic.code = session$userData$gcode) |>
          as.character()
      }
      rv$annotations$pos1[selected()] <- pos1
      rv$annotations$pos2[selected()] <- pos2
      rv$annotations$length[selected()] <- abs(pos1 - pos2) + 1
      rv$annotations$stop_codon[selected()] <- unname(codon)
    })
    init("stop-minus-simple-10")
    on("stop-minus-simple-10", {
      rv$editing$stop_aln <- TRUE
      codon <- "INIT"
      pos1 <- rv$annotations$pos1[selected()]
      pos2 <- rv$annotations$pos2[selected()]
      if (rv$annotations$direction[selected()] == "+") {
        pos2 <- pos2 + (3 - nchar(rv$annotations$stop_codon[selected()]))
        for(counter in 1:10){
          keep_going <- TRUE # modified loop logic to avoid premature exit
          while (keep_going) {
            pos2 <- pos2 - 3
            req(pos2 <= rv$editing$assembly@ranges@width)
            codon <- rv$editing$assembly |>
              Biostrings::subseq(pos2 - 2, pos2) |>
              as.character() |>
              stringr::str_extract(paste0("^", rv$editing$params$stop_codons)) |>
              na.omit() |>
              purrr::pluck(1)
            if (isTRUE(input$single_codon) && length(codon) > 0){
              break
            }
            if (isTRUE(input$single_codon) && length(codon) == 0) {
              codon <- rv$editing$assembly |>
                Biostrings::subseq(pos2 - 2, pos2) |>
                as.character()
              break
            }
            codon <- codon %||% "INIT"
            keep_going <- !any(stringr::str_detect(rv$editing$params$stop_codons, paste0("^", codon)))
          }
        }
        pos2 <- pos2 - (3 - nchar(codon))
        rv$annotations$translation[selected()] <- rv$editing$assembly |>
          Biostrings::subseq(pos1, pos2 - nchar(codon)) |>
          Biostrings::translate(genetic.code = session$userData$gcode) |>
          as.character()
      }
      if (rv$annotations$direction[selected()] == "-") {
        pos1 <- pos1 - (3 - nchar(rv$annotations$stop_codon[selected()]))
        for(counter in 1:10){
          keep_going <- TRUE # modified loop logic to avoid premature exit
          while (keep_going) {
            pos1 <- pos1 + 3
            req(pos1 >= 1)
            codon <- rv$editing$assembly |>
              Biostrings::subseq(pos1, pos1 + 2) |>
              Biostrings::reverseComplement() |>
              as.character() |>
              stringr::str_extract(paste0("^", rv$editing$params$stop_codons)) |>
              na.omit() |>
              purrr::pluck(1)
            if (isTRUE(input$single_codon) && length(codon) > 0){
              break
            }
            if (isTRUE(input$single_codon) && length(codon) == 0) {
              codon <- rv$editing$assembly |>
                Biostrings::subseq(pos1, pos1 + 2) |>
                Biostrings::reverseComplement() |>
                as.character()
              break
            }
            codon <- codon %||% "INIT"
            keep_going <- !any(stringr::str_detect(rv$editing$params$stop_codons, paste0("^", codon)))
          }
        }
        pos1 <- pos1 + (3 - nchar(codon))
        rv$annotations$translation[selected()] <- rv$editing$assembly |>
          Biostrings::subseq(pos1 + nchar(codon), pos2) |>
          Biostrings::reverseComplement() |>
          Biostrings::translate(genetic.code = session$userData$gcode) |>
          as.character()
      }
      rv$annotations$pos1[selected()] <- pos1
      rv$annotations$pos2[selected()] <- pos2
      rv$annotations$length[selected()] <- abs(pos1 - pos2) + 1
      rv$annotations$stop_codon[selected()] <- unname(codon)
    })

    observeEvent(input$`stop-minus-10`, {
      message("moving start position -10...")
      trigger("stop-minus-simple-10")
      shinyjs::delay(50, {
        trigger("re_align")
        message("DONE")
      })
    })
    observeEvent(input$`stop-minus-5`, {
      message("moving start position -5...")
      trigger("stop-minus-simple-5")
      shinyjs::delay(50, {
        trigger("re_align")
        message("DONE")
      })
    })
    observeEvent(input$`stop-minus`, {
      trigger("stop-minus-simple")
      trigger("re_align")
    })

    ## RE-align after edit ----
    init("re_align")
    on("re_align", {
      # check if user wants to use fewer reference samples in alignment
      if (isTRUE(input$reduce_align)){ n_hits = 5 } else { n_hits = Inf }
      ### Calculate new stats ----
      focal <- rv$annotations$translation[selected()]
      hits <-
        {
          rv$local_hits %||% json_parse(rv$annotations$refHits[selected()], T)
        } |>
        dplyr::slice_head(n = n_hits) |>
        dplyr::mutate(
          similarity = compare_aa(focal, target, "similarity"),
          pctid = compare_aa(focal, target, "pctId"),
          gap_leading = count_end_gaps(focal, target, "leading"),
          gap_trailing = count_end_gaps(focal, target, "trailing"),
          .after = "eval",
          .by = dplyr::everything()
        ) |>
        dplyr::arrange(dplyr::desc(similarity))

      temp_hits <- json_string(hits)
      # Keep rv$alignment around (incl. cached ref_msa); align_now rebuilds it.
      trigger("align_now")
    })

    # Discard edits ----
    observeEvent(input$discard_edits, {
      rv$annotations <- rv$annotations[-selected(), ] |>
        dplyr::bind_rows(rv$editing$backup) |>
        dplyr::arrange(pos1)
      reactable::updateReactable(
        "table",
        data = rv$annotations,
        selected = selected()
      )
      shinyjs::hide("edit_mode_ctrls")
      shinyjs::hide("save_edits")
      shinyjs::hide("discard_edits")
      shinyjs::show("edit_mode")
      rv$editing <- NULL
      trigger("align_now")
    })

    # Save edits ----
    observeEvent(input$save_edits, {
      ### Calculate new stats for all reference seqs ----
      focal <- rv$annotations$translation[selected()]
      hits <-
        {
          rv$local_hits %||% json_parse(rv$annotations$refHits[selected()], T)
        } |>
        dplyr::mutate(
          similarity = compare_aa(focal, target, "similarity"),
          pctid = compare_aa(focal, target, "pctId"),
          gap_leading = count_end_gaps(focal, target, "leading"),
          gap_trailing = count_end_gaps(focal, target, "trailing"),
          .after = "eval",
          .by = dplyr::everything()
        ) |>
        dplyr::arrange(dplyr::desc(similarity))
      rv$annotations$refHits[selected()] <- json_string(hits)

      dplyr::tbl(session$userData$con, "annotations") |>
        dplyr::rows_delete(
          dplyr::distinct(rv$annotations[, c("ID")]),
          by = "ID",
          unmatched = "ignore",
          copy = TRUE,
          in_place = TRUE
        )
      dplyr::tbl(session$userData$con, "annotations") |>
        dplyr::rows_insert(
          rv$annotations |>
            dplyr::select(-faa, -fas),
          by = "ID",
          conflict = "ignore",
          copy = TRUE,
          in_place = TRUE
        )
      shinyjs::hide("edit_mode_ctrls")
      shinyjs::hide("discard_edits")
      shinyjs::hide("save_edits")
      shinyjs::show("edit_mode")
      rv$editing <- NULL
    })
    # Local Blast ----
    observeEvent(input$local_blast, ignoreInit = T, {
      req(input$local_blast)
      req(is.null(rv$local_hits))
      # Check for local blast db
      rv$local_db <- rv$local_db %||% getOption("MitoPilot.local.db")
      if (length(rv$local_db) == 0) {
        shinyWidgets::sendSweetAlert(
          title = "No local database found!",
          text = "Run options('MitoPilot.local.db' = '/path/to/local/blastp/db') - add to .Rprofile for persistence."
        )
        shinyWidgets::updatePrettyCheckbox(
          inputId = "local_blast",
          value = FALSE
        )
        req(F)
      }
      # Check for edit mode
      if (length(rv$editing) > 0) {
        shinyWidgets::sendSweetAlert(
          title = "In edit mode!"
        )
        shinyWidgets::updatePrettyCheckbox(
          inputId = "local_blast",
          value = !input$local_blast
        )
        req(F)
      }
      shinyjs::toggle("refresh_blast", condition = input$local_blast)
      shinyjs::click("refresh_blast")
    })
    observeEvent(input$local_blast, ignoreInit = T, {
      req(!input$local_blast)
      req(!is.null(rv$local_hits))
      max_blast_hits <- dplyr::left_join(
        dplyr::tbl(session$userData$con, "annotate") |>
          dplyr::select(ID, curate_opts) |>
          dplyr::filter(ID == !!rv$updating$ID),
        dplyr::tbl(session$userData$con, "curate_opts"),
        by = "curate_opts") |>
        dplyr::pull(max_blast_hits)
      # Check for edit mode
      if (length(rv$editing) > 0) {
        shinyWidgets::sendSweetAlert(
          title = "In edit mode!"
        )
        shinyWidgets::updatePrettyCheckbox(
          inputId = "local_blast",
          value = !input$local_blast
        )
        req(F)
      }
      rv$local_hits <- NULL
      trigger("align_now")
      shinyjs::toggle("refresh_blast", condition = input$local_blast)
    })
    observeEvent(input$run_blast, ignoreInit = T, {
      rv$alignment <- NULL
      rv$local_hits <- get_top_hits_local(
        req(rv$local_db),
        rv$annotations$translation[selected()],
        max_blast_hits
      )
      trigger("align_now")
    })

    # Merge Annotations ----
    observeEvent(input$merge, {
      if (length(selected()) == 0) {
        shinyWidgets::sendSweetAlert(title = "No annotation selected")
        req(F)
      }
      sel_type <- rv$annotations$type[selected()]
      if (!sel_type %in% c("PCG", "rRNA")) {
        shinyWidgets::sendSweetAlert(
          title = "Merge only available for PCGs and rRNAs",
          text = "Select a protein-coding gene or ribosomal RNA annotation to merge."
        )
        req(F)
      }
      sel_gene <- rv$annotations$gene[selected()]
      dup_idx <- which(
        rv$annotations$gene == sel_gene &
        rv$annotations$type == sel_type &
        !stringr::str_detect(rv$annotations$gene, "_DELETED_")
      )
      if (length(dup_idx) < 2) {
        shinyWidgets::sendSweetAlert(
          title = "Nothing to merge",
          text = stringr::str_glue("Only one non-deleted {sel_gene} annotation exists.")
        )
        req(F)
      }
      dup_anns <- rv$annotations[dup_idx, ]
      choices <- setNames(
        as.list(as.character(dup_idx)),
        paste0(dup_anns$gene, ": ", dup_anns$pos1, "-", dup_anns$pos2, " (", dup_anns$direction, ")")
      )
      output$merge_choices <- renderUI({
        checkboxGroupInput(
          ns("merge_selected_rows"),
          label = NULL,
          choices = choices,
          selected = as.character(dup_idx)
        )
      })
      shinyjs::show("merge_select_div")
    })

    observeEvent(input$cancel_merge, {
      shinyjs::hide("merge_select_div")
    })

    observeEvent(input$confirm_merge, {
      rows_to_merge <- as.integer(req(input$merge_selected_rows))
      if (length(rows_to_merge) < 2) {
        shinyWidgets::sendSweetAlert(title = "Select at least 2 annotations to merge")
        req(F)
      }
      merge_anns <- rv$annotations[rows_to_merge, ]
      if (length(unique(merge_anns$path)) > 1 ||
          length(unique(merge_anns$scaffold)) > 1 ||
          length(unique(merge_anns$direction)) > 1) {
        shinyWidgets::sendSweetAlert(
          title = "Cannot merge",
          text = "All selected annotations must be on the same path, scaffold, and strand direction."
        )
        req(F)
      }
      new_pos1 <- min(merge_anns$pos1)
      new_pos2 <- max(merge_anns$pos2)
      direction <- merge_anns$direction[1]
      sel_gene <- merge_anns$gene[1]
      sel_type <- merge_anns$type[1]
      base_idx <- if (selected() %in% rows_to_merge) selected() else rows_to_merge[1]
      merged <- rv$annotations[base_idx, ]
      merged$pos1 <- new_pos1
      merged$pos2 <- new_pos2
      merged$length <- abs(new_pos2 - new_pos1) + 1
      merged$edited <- 1L
      merged$time_stamp <- as.numeric(Sys.time())
      if (sel_type == "PCG") {
        assembly <- get_assembly(
          ID = merge_anns$ID[1],
          path = merge_anns$path[1],
          scaffold = merge_anns$scaffold[1],
          con = session$userData$con
        )
        if (direction == "+") {
          merged$start_codon <- assembly |>
            Biostrings::subseq(new_pos1, new_pos1 + 2) |>
            as.character()
          merged$stop_codon <- assembly |>
            Biostrings::subseq(new_pos2 - 2, new_pos2) |>
            as.character()
          merged$translation <- assembly |>
            Biostrings::subseq(new_pos1, new_pos2 - nchar(merged$stop_codon)) |>
            Biostrings::translate(
              genetic.code = session$userData$gcode
            ) |>
            as.character()
        } else {
          merged$start_codon <- assembly |>
            Biostrings::subseq(new_pos2 - 2, new_pos2) |>
            Biostrings::reverseComplement() |>
            as.character()
          merged$stop_codon <- assembly |>
            Biostrings::subseq(new_pos1, new_pos1 + 2) |>
            Biostrings::reverseComplement() |>
            as.character()
          merged$translation <- assembly |>
            Biostrings::subseq(new_pos1 + nchar(merged$stop_codon), new_pos2) |>
            Biostrings::reverseComplement() |>
            Biostrings::translate(
              genetic.code = session$userData$gcode
            ) |>
            as.character()
        }
      }
      base_orig_pos1 <- rv$annotations$pos1[base_idx]
      base_orig_pos2 <- rv$annotations$pos2[base_idx]
      note <- stringr::str_glue(
        "MERGED: {length(rows_to_merge)} {sel_gene} annotations into {new_pos1}-{new_pos2} (from {base_orig_pos1}-{base_orig_pos2})"
      )
      merged$notes <- paste(note, merged$notes %|NA|% "", sep = "; ") |>
        stringr::str_remove("; $")
      to_delete_idx <- setdiff(rows_to_merge, base_idx)
      ts <- as.numeric(Sys.time())
      deleted_rows <- purrr::imap(to_delete_idx, function(i, j) {
        orig_pos1 <- rv$annotations$pos1[i]
        orig_pos2 <- rv$annotations$pos2[i]
        rv$annotations[i, ] |>
          dplyr::mutate(
            notes = paste(
              stringr::str_glue("DELETED: from {orig_pos1}-{orig_pos2}"),
              notes %|NA|% "",
              sep = "; "
            ) |> stringr::str_remove("; $"),
            pos1 = 0L,
            pos2 = 0L,
            length = 0L,
            time_stamp = ts + j,
            gene = paste0(gene, "_DELETED_", ts + j),
            edited = 1L
          )
      }) |>
        dplyr::bind_rows()
      rv$annotations <- rv$annotations[-rows_to_merge, ] |>
        dplyr::bind_rows(merged) |>
        dplyr::bind_rows(deleted_rows) |>
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
          rv$annotations |> dplyr::select(-faa, -fas),
          by = "ID",
          conflict = "ignore",
          copy = TRUE,
          in_place = TRUE
        )
      shinyjs::hide("merge_select_div")
      reactable::updateReactable(
        "table",
        data = rv$annotations
      )
    })

    # Restore Annotation ----
    restore_do_save <- function() {
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
          rv$annotations |> dplyr::select(-faa, -fas),
          by = "ID",
          conflict = "ignore",
          copy = TRUE,
          in_place = TRUE
        )
      reactable::updateReactable("table", data = rv$annotations)
    }

    observeEvent(input$restore, {
      req(length(selected()) > 0)
      sel_row <- rv$annotations[selected(), ]
      req(stringr::str_detect(sel_row$gene, "_DELETED_"))
      orig_range <- stringr::str_match(sel_row$notes, "DELETED: from (\\d+)-(\\d+)")
      if (is.na(orig_range[1])) {
        shinyWidgets::sendSweetAlert(
          title = "Cannot restore",
          text = "Could not determine original position from annotation notes."
        )
        req(F)
      }
      orig_gene <- stringr::str_remove(sel_row$gene, "_DELETED_.*$")
      merged_idx <- which(
        rv$annotations$gene == orig_gene &
        stringr::str_detect(dplyr::coalesce(rv$annotations$notes, ""), "^MERGED:")
      )
      if (length(merged_idx) > 0) {
        shinyWidgets::confirmSweetAlert(
          inputId = ns("confirm_restore_merged"),
          title = stringr::str_glue("Un-merge {orig_gene}?"),
          text = stringr::str_glue(
            "This annotation was deleted during a merge. Restoring will undo the entire merge: all deleted {orig_gene} annotations will be restored and the merged annotation will be reverted to its original bounds."
          ),
          btn_colors = c("#0056b3", "#0056b3")
        )
      } else {
        orig_pos1 <- as.integer(orig_range[2])
        orig_pos2 <- as.integer(orig_range[3])
        conflict <- rv$annotations |>
          dplyr::filter(
            gene == orig_gene,
            pos1 == orig_pos1,
            !stringr::str_detect(gene, "_DELETED_")
          )
        if (nrow(conflict) > 0) {
          shinyWidgets::sendSweetAlert(
            title = "Cannot restore",
            text = stringr::str_glue(
              "An active annotation for {orig_gene} at {orig_pos1}-{orig_pos2} already exists."
            )
          )
          req(F)
        }
        restored <- sel_row |>
          dplyr::mutate(
            gene = orig_gene,
            pos1 = orig_pos1,
            pos2 = orig_pos2,
            length = abs(orig_pos2 - orig_pos1) + 1,
            notes = stringr::str_remove(notes, "^DELETED: from \\d+-\\d+(; )?"),
            edited = 1L,
            time_stamp = as.numeric(Sys.time())
          )
        rv$annotations <- rv$annotations[-selected(), ] |>
          dplyr::bind_rows(restored) |>
          dplyr::arrange(pos1)
        restore_do_save()
      }
    })

    observeEvent(input$confirm_restore_merged, {
      req(input$confirm_restore_merged)
      req(length(selected()) > 0)
      sel_row <- rv$annotations[selected(), ]
      orig_gene <- stringr::str_remove(sel_row$gene, "_DELETED_.*$")
      merged_idx <- which(
        rv$annotations$gene == orig_gene &
        stringr::str_detect(dplyr::coalesce(rv$annotations$notes, ""), "^MERGED:")
      )
      req(length(merged_idx) > 0)
      merged_row <- rv$annotations[merged_idx[1], ]
      merged_orig_range <- stringr::str_match(
        merged_row$notes, "\\(from (\\d+)-(\\d+)\\)"
      )
      if (is.na(merged_orig_range[1])) {
        shinyWidgets::sendSweetAlert(
          title = "Cannot un-merge",
          text = "Original bounds of the merged annotation could not be determined."
        )
        req(F)
      }
      merged_orig_pos1 <- as.integer(merged_orig_range[2])
      merged_orig_pos2 <- as.integer(merged_orig_range[3])
      assembly <- get_assembly(
        ID = merged_row$ID,
        path = merged_row$path,
        scaffold = merged_row$scaffold,
        con = session$userData$con
      )
      direction <- merged_row$direction
      reverted <- merged_row |>
        dplyr::mutate(
          pos1 = merged_orig_pos1,
          pos2 = merged_orig_pos2,
          length = abs(merged_orig_pos2 - merged_orig_pos1) + 1,
          notes = stringr::str_remove(notes, "^MERGED:[^;]*(; )?"),
          edited = 1L,
          time_stamp = as.numeric(Sys.time())
        )
      if (merged_row$type == "PCG") {
        if (direction == "+") {
          reverted$start_codon <- assembly |>
            Biostrings::subseq(merged_orig_pos1, merged_orig_pos1 + 2) |>
            as.character()
          reverted$stop_codon <- assembly |>
            Biostrings::subseq(merged_orig_pos2 - 2, merged_orig_pos2) |>
            as.character()
          reverted$translation <- assembly |>
            Biostrings::subseq(
              merged_orig_pos1,
              merged_orig_pos2 - nchar(reverted$stop_codon)
            ) |>
            Biostrings::translate(
              genetic.code = session$userData$gcode
            ) |>
            as.character()
        } else {
          reverted$start_codon <- assembly |>
            Biostrings::subseq(merged_orig_pos2 - 2, merged_orig_pos2) |>
            Biostrings::reverseComplement() |>
            as.character()
          reverted$stop_codon <- assembly |>
            Biostrings::subseq(merged_orig_pos1, merged_orig_pos1 + 2) |>
            Biostrings::reverseComplement() |>
            as.character()
          reverted$translation <- assembly |>
            Biostrings::subseq(
              merged_orig_pos1 + nchar(reverted$stop_codon),
              merged_orig_pos2
            ) |>
            Biostrings::reverseComplement() |>
            Biostrings::translate(
              genetic.code = session$userData$gcode
            ) |>
            as.character()
        }
      }
      all_deleted_idx <- which(
        stringr::str_detect(
          rv$annotations$gene,
          paste0("^", orig_gene, "_DELETED_")
        )
      )
      restored_rows <- purrr::map(all_deleted_idx, function(i) {
        row <- rv$annotations[i, ]
        del_range <- stringr::str_match(row$notes, "DELETED: from (\\d+)-(\\d+)")
        if (is.na(del_range[1])) return(NULL)
        p1 <- as.integer(del_range[2])
        p2 <- as.integer(del_range[3])
        row |>
          dplyr::mutate(
            gene = orig_gene,
            pos1 = p1,
            pos2 = p2,
            length = abs(p2 - p1) + 1,
            notes = stringr::str_remove(notes, "^DELETED: from \\d+-\\d+(; )?"),
            edited = 1L,
            time_stamp = as.numeric(Sys.time())
          )
      }) |>
        purrr::compact() |>
        dplyr::bind_rows()
      remove_idx <- c(merged_idx, all_deleted_idx)
      rv$annotations <- rv$annotations[-remove_idx, ] |>
        dplyr::bind_rows(reverted) |>
        dplyr::bind_rows(restored_rows) |>
        dplyr::arrange(pos1)
      restore_do_save()
    })
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

  topo      <- rv$updating$topology %||% "unknown"
  topo_icon <- switch(topo, circular = "\u21ba", linear = "\u2194", "?")
  topo_badge <- span(
    style = paste0(
      "background:", if (topo == "circular") "#cce5ff" else if (topo == "linear") "#fff3cd" else "#e9ecef", ";",
      "color:",      if (topo == "circular") "#004085" else if (topo == "linear") "#856404" else "#6c757d", ";",
      "border-radius:3px;padding:2px 8px;font-size:0.75em;font-weight:600;white-space:nowrap;"
    ),
    paste(topo_icon, toupper(topo))
  )

  modalDialog(
    title = div(
      style = "display: flex; align-items: center; gap: 12px; flex-wrap: wrap;",
      span(stringr::str_glue("Annotations: {rv$updating$ID} - {rv$updating$Taxon}")),
      topo_badge,
      uiOutput(ns("status_badges"), inline = TRUE)
    ),
    size = "l",
    easyClose = F,
    tags$details(
      id = ns("annotation_table_details"),
      open = TRUE,
      tags$summary("Annotation Table"),
      reactableOutput(ns("table"), width = "100%")
    ),
    div(
      id = ns("annotation_btns_wrapper"),
      shinyjs::hidden(
        div(
          id = ns("annotation_action_btns"),
          style = "display: flex; align-items: center; gap: 8px; margin: 6px 0;",
          actionButton(ns("merge"), "Merge PCGs/rRNAs"),
          actionButton(ns("delete"), "Delete"),
          uiOutput(ns("synteny_zoom_ctrl"))
        )
      ),
      shinyjs::hidden(
        div(
          id = ns("annotation_restore_btn"),
          style = "display: flex; gap: 8px; margin: 6px 0;",
          actionButton(ns("restore"), "Restore")
        )
      ),
      shinyjs::hidden(
        div(
          id = ns("merge_select_div"),
        style = "border: 1px solid #ccc; border-radius: 4px; padding: 10px; margin: 6px 0;",
        tags$b("Select annotations to merge:"),
        uiOutput(ns("merge_choices")),
        div(
          style = "display: flex; gap: 8px; margin-top: 8px;",
          shinyWidgets::actionBttn(
            ns("confirm_merge"),
            label = "Confirm Merge",
            style = "material-flat",
            size = "xs",
            icon = icon("object-group")
          ),
          shinyWidgets::actionBttn(
            ns("cancel_merge"),
            label = "Cancel",
            style = "material-flat",
            size = "xs",
            icon = icon("times")
          )
        )
      )
    )
  ),
  tags$script(HTML(sprintf(
      "document.getElementById('%s').addEventListener('toggle', function() {
         var w = document.getElementById('%s');
         if (w) w.style.display = this.open ? '' : 'none';
       });
       document.addEventListener('keydown', function(e) {
         if ((e.key === 'ArrowUp' || e.key === 'ArrowDown') &&
             e.target && e.target.id === '%s') {
           e.preventDefault();
         }
       });",
      ns("annotation_table_details"), ns("annotation_btns_wrapper"),
      ns("synteny_zoom_window")
    ))),
  tags$style(HTML(sprintf(
    paste0(
      "#%s::-webkit-inner-spin-button,",
      "#%s::-webkit-outer-spin-button { -webkit-appearance: none; margin: 0; }",
      "#%s { -moz-appearance: textfield; appearance: textfield; }"
    ),
    ns("synteny_zoom_window"), ns("synteny_zoom_window"), ns("synteny_zoom_window")
  ))),
    tags$hr(style = "margin: 4px 0; border: none; border-top: 1px solid #e0e0e0;"),
    tags$details(
      tags$summary("Coverage Map"),
      div(
        style = "padding: 2px 5mm 0 5mm; font-size: 0.85em; color: #555;",
        tags$span(
          style = "display: inline-block; width: 10px; height: 10px; background: #FF667040; vertical-align: middle; margin-right: 4px;"
        ),
        "mean error rate > 5% (possible sequencing or assembly errors)."
      ),
      div(
        id = ns("coverageDiv"),
        style = "width: 100%; overflow-x: auto; padding: 5mm;",
        uiOutput(ns("coverage_map"))
      )
    ),
    tags$details(
      id = ns("blast_synteny_details"),
      tags$summary("BLAST Reference Synteny"),
      div(
        style = "padding: 2px 5mm;",
        uiOutput(ns("synteny_ui"))
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
        id = ns("aln_div"),
        div(
          id = ns("aln_ctlr_div"),
          style = "display: flex; flex-flow: row nowrap; align-items: center; gap: 2em; margin-top: 0.5em; height: 50px;",
          div(
            style = "gap: 0.5em;",
            shinyWidgets::actionBttn(
              ns("edit_mode"),
              label = "Edit",
              style = "material-flat",
              size = "xs",
              icon = icon("edit")
            ),
            shinyWidgets::actionBttn(
              ns("save_edits"),
              label = "Save",
              style = "material-flat",
              size = "xs",
              icon = icon("save")
            ) |> shinyjs::hidden(),
            shinyWidgets::actionBttn(
              ns("discard_edits"),
              label = "Reset",
              style = "material-flat",
              size = "xs",
              icon = icon("rotate-left")
            ) |> shinyjs::hidden()
          ),
          div(
            id = ns("edit_mode_ctrls"),
            style = "display: flex; flex-flow: row nowrap; align-items: center; gap: 3em;",
            div(
              style = "display: flex; flex-flow: row nowrap; align-items: center;",
              tags$button(
                class = "icon-circle grow",
                onclick = stringr::str_glue("Shiny.setInputValue('{ns('start-minus-10')}', 'minus-10', {{priority: 'event'}})"),
                tags$span(
                  style = "font-size: 0.75em;",  # This matches fa-xs sizing
                  "-10"
                )
              ),
              tags$button(
                class = "icon-circle grow",
                onclick = stringr::str_glue("Shiny.setInputValue('{ns('start-minus-5')}', 'minus-5', {{priority: 'event'}})"),
                tags$span(
                  style = "font-size: 0.75em;",  # This matches fa-xs sizing
                  "-5"
                )
              ),
              tags$button(
                class = "icon-circle grow",
                onclick = stringr::str_glue("Shiny.setInputValue('{ns('start-minus')}', 'minus', {{priority: 'event'}})"),
                tags$span(
                  style = "font-size: 0.75em;",  # This matches fa-xs sizing
                  "-1"
                )
              ),
              div(style = "margin: 00.5em;", "START"),
              tags$button(
                class = "icon-circle grow",
                onclick = stringr::str_glue("Shiny.setInputValue('{ns('start-add')}', 'plus', {{priority: 'event'}})"),
                tags$span(
                  style = "font-size: 0.75em;",  # This matches fa-xs sizing
                  "+1"
                )
              ),
              tags$button(
                class = "icon-circle grow",
                onclick = stringr::str_glue("Shiny.setInputValue('{ns('start-add-5')}', 'plus-5', {{priority: 'event'}})"),
                tags$span(
                  style = "font-size: 0.75em;",  # This matches fa-xs sizing
                  "+5"
                )
              ),
              tags$button(
                class = "icon-circle grow",
                onclick = stringr::str_glue("Shiny.setInputValue('{ns('start-add-10')}', 'plus-10', {{priority: 'event'}})"),
                tags$span(
                  style = "font-size: 0.75em;",  # This matches fa-xs sizing
                  "+10"
                )
              )
            ),
            div(
              style = "display: flex; flex-flow: row nowrap; align-items: center;",
              tags$button(
                class = "icon-circle grow",
                onclick = stringr::str_glue("Shiny.setInputValue('{ns('stop-minus-10')}', 'minus-10', {{priority: 'event'}})"),
                tags$span(
                  style = "font-size: 0.75em;",  # This matches fa-xs sizing
                  "-10"
                )
              ),
              tags$button(
                class = "icon-circle grow",
                onclick = stringr::str_glue("Shiny.setInputValue('{ns('stop-minus-5')}', 'minus-5', {{priority: 'event'}})"),
                tags$span(
                  style = "font-size: 0.75em;",  # This matches fa-xs sizing
                  "-5"
                )
              ),
              tags$button(
                class = "icon-circle grow",
                onclick = stringr::str_glue("Shiny.setInputValue('{ns('stop-minus')}', 'minus', {{priority: 'event'}})"),
                tags$span(
                  style = "font-size: 0.75em;",  # This matches fa-xs sizing
                  "-1"
                )
              ),
              div(style = "margin: 00.5em;", "STOP"),
              tags$button(
                class = "icon-circle grow",
                onclick = stringr::str_glue("Shiny.setInputValue('{ns('stop-add')}', 'plus', {{priority: 'event'}})"),
                tags$span(
                  style = "font-size: 0.75em;",  # This matches fa-xs sizing
                  "+1"
                )
              ),
              tags$button(
                class = "icon-circle grow",
                onclick = stringr::str_glue("Shiny.setInputValue('{ns('stop-add-5')}', 'plus-5', {{priority: 'event'}})"),
                tags$span(
                  style = "font-size: 0.75em;",  # This matches fa-xs sizing
                  "+5"
                )
              ),
              tags$button(
                class = "icon-circle grow",
                onclick = stringr::str_glue("Shiny.setInputValue('{ns('stop-add-10')}', 'plus-10', {{priority: 'event'}})"),
                tags$span(
                  style = "font-size: 0.75em;",  # This matches fa-xs sizing
                  "+10"
                )
              )
            ),
            div(
              style = "padding-top: 14px;",
              shinyWidgets::prettyCheckbox(
                ns("single_codon"),
                label = "single codon",
                status = "primary",
                inline = TRUE
              )
            )
          ) |> shinyjs::hidden()
        ),
        div(
          id = ns("edit_mode_ctrls_extra"),
          style = "display: flex; flex: 1; justify-content: left; gap: 0; align-items: center",
          div(
            shinyWidgets::prettyCheckbox(
              ns("reduce_align"),
              label = "Align fewer refs",
              status = "primary",
              inline = TRUE
            )
          ),
          div(
            shinyWidgets::prettyCheckbox(
              ns("local_blast"),
              label = "Local blast",
              status = "primary",
              inline = TRUE
            ),
            tags$i(
              id = ns("refresh_blast"),
              class = "fas fa-sync grow",
              onclick = stringr::str_glue(
                "Shiny.setInputValue('{ns('run_blast')}', 'go', {{priority: 'event'}})"
              )
            ) |> shinyjs::hidden()
          )
        ),
        div(
          style = "margin: 30px 5px 5px 5px;",
          uiOutput(ns("msa_header")),
          msaR::msaROutput(ns("msa"))
        )
      ) |> shinyjs::hidden()
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
    footer = tagList(
      uiOutput(ns("status_toggles"), inline = TRUE),
      actionButton(ns("linearize"), "Linearize"),
      actionButton(ns("lock"), "Lock&Close"),
      actionButton(ns("close"), "Close")
    )
  )
}
