#' coverage_details Server Functions
#'
#' @noRd
assembly_coverage_details_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Local state for consensus review (separate from rv to avoid pollution)
    lrv <- reactiveValues(consensus_review = NULL)

    init("coverage_modal")
    on("coverage_modal", {
      rv$alignment <- NULL
      lrv$consensus_review <- NULL
      rv$focal_assembly <- dplyr::tbl(session$userData$con, "assemblies") |>
        dplyr::filter(ID == !!rv$updating$ID) |>
        dplyr::select(
          ignore, ID, path, scaffold, topology, length_raw, length, sequence,
          dplyr::any_of(c("blast_accession", "blast_species", "blast_pident", "blast_qcovs", "blast_lineage"))
        ) |>
        dplyr::collect() |>
        dplyr::mutate(
          view_coverage = NA_character_
        )

      # TODO - refactor to handle fragmented assemblies
      if (any(rv$focal_assembly$scaffold > 1)) {
        shinyWidgets::sendSweetAlert(
          title = "Fragmented assembly",
          text = "Interactive mode is not currently supported for fragmented assemblies.",
          type = "warning"
        )
        req(F)
      }

      flag_row <- path_discrepancy_flags(session$userData$con) |>
        dplyr::filter(ID == !!rv$updating$ID)
      flag <- if (nrow(flag_row) == 0) "none" else flag_row$path_flag[1]
      reasons <- if (nrow(flag_row) == 0) "" else (flag_row$path_flag_reasons[1] %|NA|% "")
      flag_color <- c(ok = "#2ca02c", minor = "#ff7f0e", divergent = "#d62728")[flag]
      if (is.na(flag_color)) flag_color <- "#cccccc"

      modalDialog(
        title = tagList(
          div(
            stringr::str_glue("Assembly details for ID: {rv$updating$ID} "),
            if (flag != "none")
              tags$span(
                style = sprintf(
                  "display:inline-block;margin-left:8px;padding:2px 10px;border-radius:10px;background:%s;color:white;font-size:0.7em;vertical-align:middle;",
                  flag_color
                ),
                paste0(flag, if (nzchar(reasons)) paste0(" — ", reasons) else "")
              )
          ),
          div(
            style = "font-size: 0.85em; font-weight: normal; color: #555; margin-top: 4px;",
            stringr::str_glue("Taxon: {rv$updating$Taxon %|NA|% 'NA'}")
          )
        ),
        size = "l",
        reactableOutput(ns("table"), width = "100%"),
        uiOutput(ns("msa_div")),
        uiOutput(ns("consensus_panel")),
        div(
          style = "margin: 10px;",
          textAreaInput(
            ns("notes"),
            label = "Notes:",
            value = rv$updating$assemble_notes %|NA|% character(0),
            width = "100%"
          )
        ),
        footer = tagList(
          div(
            style = "display: flex; justify-content: right; gap: 0.5em;",
            uiOutput(ns("clip")) |> shinyjs::hidden(),
            actionButton(ns("align"), "Align", icon("align-justify")) |> shinyjs::hidden(),
            actionButton(ns("build_consensus"), "Build Consensus", icon("dna")) |> shinyjs::hidden(),
            actionButton(ns("finalize_consensus"), "Finalize Consensus", icon("check"), class = "btn-success") |> shinyjs::hidden(),
            actionButton(ns("exit_review"), "Exit Review", icon("xmark")) |> shinyjs::hidden(),
            actionButton(ns("close_modal"), "Close")
          )
        )
      ) |>
        showModal()
    })

    # Render table ----
    output$table <- renderReactable({
      rv$focal_assembly |>
        reactable(
          compact = TRUE,
          wrap = FALSE,
          width = "100%",
          onClick = "select",
          selection = "multiple",
          defaultPageSize = 20,
          rowStyle = rt_highlight_row(),
          defaultColDef = colDef(align = "center"),
          columns = list(
            ignore = colDef(
              width = 60,
              html = TRUE, align = "center",
              cell = rt_bool_bttn(ns("ignore"), "fa fa-circle-xmark", "far fa-circle")
            ),
            #ID = colDef(
            #  align = "left", minWidth = 80, resizable = TRUE, html = T, cell = rt_longtext()
            #),
            path = colDef(
              name = "Path", width = 60, align = "center"
            ),
            scaffold = colDef(
              name = "Scaffold", width = 80, align = "center"
            ),
            topology = colDef(
              name = "Topology", width = 90, align = "center"
            ),
            length_raw = colDef(
              name = "Length (raw)", width = 110, align = "center"
            ),
            length = colDef(
              name = "Length (trimmed)", width = 130, align = "center"
            ),
            sequence = colDef(show = FALSE),
            blast_accession = colDef(
              name = "BLAST Top Hit", minWidth = 120, resizable = TRUE, align = "center", html = TRUE,
              cell = rt_ncbi_link()
            ),
            blast_species = colDef(
              name = "BLAST Species", minWidth = 160, resizable = TRUE, align = "left", html = TRUE,
              cell = rt_longtext()
            ),
            blast_pident = colDef(
              name = "% Ident", width = 80, align = "center"
            ),
            blast_qcovs = colDef(
              name = "% Cov", width = 80, align = "center"
            ),
            blast_lineage = colDef(
              name = "BLAST Lineage", minWidth = 200, resizable = TRUE, align = "left", html = TRUE,
              cell = rt_longtext()
            ),
            view_coverage = colDef(
              name = "", html = T, width = 70, align = "center", sticky = "right",
              cell = rt_icon_bttn_text(ns("view_coverage"), "fas fa-eye fa-xs", "view")
            )
          )
        )
    })

    # Close modal ----
    observeEvent(input$close_modal, ignoreInit = T, {
      removeModal()
      trigger("refresh_assemble")
    })

    # Table selection ----
    selected <- reactive(reactable::getReactableState("table", "selected"))
    observe({
      in_review <- isTRUE(lrv$consensus_review$active)
      has_selection <- length(selected()) > 0
      has_multi <- length(selected()) > 1
      is_aligned <- !is.null(rv$alignment)

      shinyjs::toggle("clip", condition = has_selection && !in_review)
      shinyjs::toggle("align", condition = has_multi && !in_review)
      shinyjs::toggle("build_consensus", condition = is_aligned && !in_review && length(rv$alignment$mismatch_cols) > 0)
      shinyjs::toggle("finalize_consensus", condition = in_review)
      shinyjs::toggle("exit_review", condition = in_review)
    })

    # Ignore bttn ----
    observeEvent(input$ignore, {
      row <- as.numeric(input$ignore)
      rv$focal_assembly$ignore[row] <- as.numeric(!rv$focal_assembly$ignore[row])
      dplyr::tbl(session$userData$con, "assemblies") |>
        dplyr::rows_update(
          data.frame(
            ID = rv$focal_assembly$ID[row],
            path = rv$focal_assembly$path[row],
            scaffold = rv$focal_assembly$scaffold[row],
            ignore = rv$focal_assembly$ignore[row]
          ),
          in_place = TRUE,
          unmatched = "ignore",
          copy = TRUE,
          by = c("ID", "path", "scaffold")
        )
      reactable::updateReactable(
        "table",
        data = rv$focal_assembly,
        selected = selected()
      )

      # Auto-promote/demote based on number of non-ignored scaffolds/paths.
      # Single remaining scaffold/path -> mark successful (2). For multi-
      # scaffold assemblies this is correct because BLAST info already exists;
      # multi-path assemblies will lack BLAST info, which is a known gap.
      n_active <- sum(rv$focal_assembly$ignore == 0)
      if (n_active == 1L && isTRUE(rv$updating$assemble_switch == 3)) {
        dplyr::tbl(session$userData$con, "assemble") |>
          dplyr::rows_update(
            data.frame(ID = rv$updating$ID, assemble_switch = 2L),
            in_place = TRUE,
            copy = TRUE,
            unmatched = "ignore",
            by = "ID"
          )
        rv$updating$assemble_switch <- 2L
        rv$data <- rv$data |>
          dplyr::rows_update(
            data.frame(ID = rv$updating$ID, assemble_switch = 2L),
            by = "ID"
          )
        shiny::showNotification(
          "Auto-promoted to successful \u2014 1 scaffold/path remaining.",
          type = "message",
          duration = 5
        )
      } else if (n_active > 1L && isTRUE(rv$updating$assemble_switch == 2)) {
        dplyr::tbl(session$userData$con, "assemble") |>
          dplyr::rows_update(
            data.frame(ID = rv$updating$ID, assemble_switch = 3L),
            in_place = TRUE,
            copy = TRUE,
            unmatched = "ignore",
            by = "ID"
          )
        rv$updating$assemble_switch <- 3L
        rv$data <- rv$data |>
          dplyr::rows_update(
            data.frame(ID = rv$updating$ID, assemble_switch = 3L),
            by = "ID"
          )
        shiny::showNotification(
          "Reverted to needs attention \u2014 multiple scaffolds/paths active.",
          type = "warning",
          duration = 5
        )
      }
    })

    # Notes ----
    notes_update <- reactive({
      input$notes
    }) |> debounce(500)
    observeEvent(notes_update(), ignoreInit = T, ignoreNULL = T, {
      req(input$notes != (rv$updating$assemble_notes %|NA|% ""))
      rv$updating$assemble_notes <- input$notes |>
        stringr::str_remove_all(",")
      dplyr::tbl(session$userData$con, "assemble") |>
        dplyr::rows_update(
          rv$updating[, c("ID", "assemble_notes")],
          in_place = TRUE,
          unmatched = "ignore",
          copy = TRUE,
          by = "ID"
        )
      rv$data <- rv$data |>
        dplyr::rows_update(rv$updating[c("ID", "assemble_notes")], by = "ID")
    })

    # View Coverage PDF ----
    observeEvent(input$view_coverage, {
      row <- as.numeric(input$view_coverage)
      url <- file.path(
        dirname(getOption("MitoPilot.db") %||% "."),
        "out", rv$updating$ID,
        "assemble", rv$updating$assemble_opts,
        paste0(rv$updating$ID, "_assembly_",
               rv$focal_assembly$path[row], "_",
               rv$focal_assembly$scaffold[row], "_coverage.pdf")
      ) |>
        browseURL()
    })

    # Copy as fasta ----
    output$clip <- renderUI({
      fasta <- purrr::map(req(selected()), ~ {
        seqid <- paste(paste0(">", rv$updating$ID),
          rv$focal_assembly$path[.x], rv$focal_assembly$scaffold[.x],
          sep = "."
        ) |>
          paste(rv$focal_assembly$topology[.x])
        seq <- rv$focal_assembly$sequence[.x] |>
          stringr::str_replace_all(paste0("(.{80})"), "\\1\n")
        c(seqid, seq)
      }) |>
        purrr::flatten() |>
        paste(collapse = "\n")
      rclipboard::rclipButton(
        inputId = ns("clipbtn"),
        label = "Fasta",
        clipText = fasta %||% "",
        icon = icon("copy"),
        modal = TRUE
      )
    })

    # Align ----
    wait_align <- waiter::Waiter$new(
      id = ns("table"),
      html = waiter::spin_3(),
      color = waiter::transparent(.5)
    )
    observeEvent(input$align, {
      lrv$consensus_review <- NULL
      wait_align$show()
      seqs <- rv$focal_assembly$sequence[selected()] |> Biostrings::DNAStringSet()
      names(seqs) <- purrr::map_chr(selected(), ~ {
        paste(
          rv$updating$ID,
          rv$focal_assembly$path[.x],
          rv$focal_assembly$scaffold[.x],
          sep = "."
        )
      })
      rv$alignment$seqs <- DECIPHER::AlignSeqs(seqs, verbose = F, processors = NULL)

      dist_mat <- DECIPHER::DistanceMatrix(
        rv$alignment$seqs,
        includeTerminalGaps = T,
        type = "matrix",
        verbose = FALSE
      )
      rv$alignment$dist_matrix <- dist_mat
      dist_off <- dist_mat[upper.tri(dist_mat)]
      dists <- range(dist_off)

      if (dists[1] != dists[2]) {
        rv$alignment$pct_id_range <- stringr::str_glue(
          "Pairwise similarity: {round(100-100*dists[1],4)}% - {round(100-100*dists[2],4)}%"
        )
      } else {
        rv$alignment$pct_id_range <- stringr::str_glue(
          "Pairwise similarity: {round(100-100*dists[1],4)}%"
        )
      }

      # Aligned matrix and per-column match indicator
      seqs_mat <- as.matrix(rv$alignment$seqs)
      align_pos <- apply(seqs_mat, 2, function(x) length(unique(x)) == 1)
      rv$alignment$seqs_mat <- seqs_mat
      rv$alignment$align_pos <- align_pos
      rv$alignment$mismatch_cols <- which(!align_pos)
      rv$alignment$n_aln <- length(align_pos)
      rv$alignment$nav_idx <- if (length(rv$alignment$mismatch_cols) > 0) 1L else 0L
      rv$alignment$click_pos <- NA_integer_

      # Longest consensus block (existing logic)
      align_rle <- rle(align_pos)
      rv$alignment$consEnd <- sum(align_rle$lengths[1:which.max(align_rle$lengths & align_rle$values)])
      rv$alignment$consStart <- rv$alignment$consEnd - align_rle$lengths[which.max(align_rle$lengths & align_rle$values)] + 1

      if (rv$updating$topology == "circular" & rv$alignment$consStart == 1) {
        rv$alignment$consStart <- c(
          max(which(!align_pos)) + 1, rv$alignment$consStart
        )
        rv$alignment$consEnd <- c(
          length(align_pos), rv$alignment$consEnd
        )
      } else if (rv$updating$topology == "circular" & rv$alignment$consEnd == rv$alignment$seqs@ranges@width[1]) {
        rv$alignment$consStart <- c(
          rv$alignment$consStart, 1
        )
        rv$alignment$consEnd <- c(
          rv$alignment$consEnd, min(which(!align_pos)) - 1
        )
      }

      rv$alignment$consLen <- purrr::map2_dbl(rv$alignment$consStart, rv$alignment$consEnd, ~ {
        .y - .x + 1
      }) |> sum()

      rv$alignment$consRegion <- stringr::str_glue(
        "Longest Consensus Region: ",
        "{rv$alignment$consLen} bp ",
        "({round(100*rv$alignment$consLen/max(rv$alignment$seqs@ranges@width),2)}% of total length)"
      )

      rv$alignment$alignmentHeight <- 5 + (length(selected()) * 20)

      # Per-path coverage with raw→alignment position mapping
      paths_used <- rv$focal_assembly$path[selected()]
      rv$alignment$paths_used <- paths_used

      cov_list <- purrr::map(seq_along(paths_used), function(i) {
        p <- paths_used[i]
        csv_path <- file.path(
          session$userData$dir_out,
          rv$updating$ID,
          "assemble",
          rv$updating$assemble_opts,
          paste0(rv$updating$ID, "_assembly_", p, "_coverageStats.csv")
        )
        if (!file.exists(csv_path)) return(NULL)
        cov <- tryCatch(read.csv(csv_path), error = function(e) NULL)
        if (is.null(cov) || nrow(cov) == 0) return(NULL)
        row_chars <- seqs_mat[i, ]
        nongap <- which(row_chars != "-")
        n <- min(nrow(cov), length(nongap))
        cov <- cov[seq_len(n), , drop = FALSE]
        cov$align_position <- nongap[seq_len(n)]
        cov$path <- p
        cov
      })
      cov_list <- cov_list[!vapply(cov_list, is.null, logical(1))]
      rv$alignment$coverage <- if (length(cov_list) > 0) do.call(rbind, cov_list) else NULL
    })

    # Mismatch nav ----
    observeEvent(input$nav_next, {
      req(rv$alignment, length(rv$alignment$mismatch_cols) > 0)
      m <- rv$alignment$mismatch_cols
      in_review <- isTRUE(lrv$consensus_review$active)
      if (in_review) {
        cur <- lrv$consensus_review$mismatch_idx %||% 1L
        lrv$consensus_review$mismatch_idx <- min(cur + 1L, length(m))
      }
      cur_nav <- rv$alignment$nav_idx %||% 0L
      rv$alignment$nav_idx <- min(cur_nav + 1L, length(m))
      rv$alignment$click_pos <- m[rv$alignment$nav_idx]
    })
    observeEvent(input$nav_prev, {
      req(rv$alignment, length(rv$alignment$mismatch_cols) > 0)
      m <- rv$alignment$mismatch_cols
      in_review <- isTRUE(lrv$consensus_review$active)
      if (in_review) {
        cur <- lrv$consensus_review$mismatch_idx %||% 1L
        lrv$consensus_review$mismatch_idx <- max(cur - 1L, 1L)
      }
      cur_nav <- rv$alignment$nav_idx %||% 1L
      rv$alignment$nav_idx <- max(cur_nav - 1L, 1L)
      rv$alignment$click_pos <- m[rv$alignment$nav_idx]
    })

    output$nav_pos <- renderText({
      req(rv$alignment)
      m <- rv$alignment$mismatch_cols
      if (length(m) == 0) return("0 mismatches")
      idx <- rv$alignment$nav_idx %||% 1L
      sprintf("Mismatch %d / %d  —  column %d", idx, length(m), m[idx])
    })

    # Plotly: mismatch track ----
    output$mismatch_track <- plotly::renderPlotly({
      req(rv$alignment)
      m <- rv$alignment$mismatch_cols
      cur <- if (!is.null(rv$alignment$click_pos) && !is.na(rv$alignment$click_pos)) {
        rv$alignment$click_pos
      } else if (length(m) > 0) m[rv$alignment$nav_idx %||% 1L] else NA_integer_

      # In review mode, color columns by decision status
      cr <- lrv$consensus_review
      decided_cols <- integer(0)
      undecided_cols <- m
      if (!is.null(cr) && isTRUE(cr$active)) {
        decided_mask <- !is.na(cr$decisions)
        decided_cols <- cr$valid_cols[decided_mask]
        undecided_cols <- setdiff(m, decided_cols)
      }

      p <- plotly::plot_ly(source = "mismatch_src") |>
        plotly::layout(
          xaxis = list(title = "Alignment column", range = c(1, rv$alignment$n_aln), zeroline = FALSE),
          yaxis = list(showticklabels = FALSE, zeroline = FALSE, showgrid = FALSE, range = c(0, 1)),
          margin = list(l = 40, r = 10, t = 5, b = 30),
          showlegend = FALSE,
          shapes = if (!is.na(cur)) list(list(
            type = "line", x0 = cur, x1 = cur, y0 = 0, y1 = 1,
            line = list(color = "#000", width = 1, dash = "dot")
          )) else list()
        )
      if (length(undecided_cols) > 0) {
        p <- p |> plotly::add_segments(
          x = undecided_cols, xend = undecided_cols, y = 0, yend = 1,
          line = list(color = "#d62728", width = 1),
          hoverinfo = "x", name = "undecided"
        )
      }
      if (length(decided_cols) > 0) {
        p <- p |> plotly::add_segments(
          x = decided_cols, xend = decided_cols, y = 0, yend = 1,
          line = list(color = "#2ca02c", width = 1),
          hoverinfo = "x", name = "decided"
        )
      }
      plotly::event_register(p, "plotly_click")
    })

    # Plotly click → set drilldown / review position ----
    observe({
      ev <- plotly::event_data("plotly_click", source = "mismatch_src")
      req(ev, rv$alignment)
      pos <- round(ev$x)
      rv$alignment$click_pos <- pos
      m <- rv$alignment$mismatch_cols
      hit <- which(m == pos)
      if (length(hit) > 0) {
        rv$alignment$nav_idx <- hit[1]
        if (isTRUE(lrv$consensus_review$active)) {
          valid_hit <- which(lrv$consensus_review$valid_cols == pos)
          if (length(valid_hit) > 0) lrv$consensus_review$mismatch_idx <- valid_hit[1]
        }
      }
    })

    # Plotly: per-path coverage tracks ----
    output$coverage_tracks <- plotly::renderPlotly({
      req(rv$alignment)
      cov <- rv$alignment$coverage
      paths <- rv$alignment$paths_used
      if (is.null(cov) || length(paths) == 0) {
        return(
          plotly::plot_ly() |>
            plotly::layout(
              annotations = list(list(
                text = "No coverage CSVs found for selected paths.",
                xref = "paper", yref = "paper", x = 0.5, y = 0.5, showarrow = FALSE
              )),
              xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)
            )
        )
      }
      cur <- if (!is.null(rv$alignment$click_pos) && !is.na(rv$alignment$click_pos)) {
        rv$alignment$click_pos
      } else NA_integer_
      plots <- lapply(paths, function(p) {
        d <- cov[cov$path == p, ]
        d <- d[order(d$align_position), ]
        plotly::plot_ly(d, x = ~align_position, y = ~Depth,
          type = "scatter", mode = "lines",
          line = list(color = "#1f77b4"),
          hovertemplate = paste0("Path ", p, "<br>Col: %{x}<br>Depth: %{y}<extra></extra>")
        ) |>
          plotly::layout(
            yaxis = list(title = paste0("p", p, " depth")),
            shapes = if (!is.na(cur)) list(list(
              type = "line", x0 = cur, x1 = cur, y0 = 0, y1 = 1,
              yref = "paper", line = list(color = "#000", width = 1, dash = "dot")
            )) else list()
          )
      })
      plotly::subplot(plots, nrows = length(plots), shareX = TRUE, titleY = TRUE) |>
        plotly::layout(
          xaxis = list(range = c(1, rv$alignment$n_aln), title = "Alignment column"),
          showlegend = FALSE,
          margin = list(l = 60, r = 10, t = 5, b = 30)
        )
    })

    # Plotly: pairwise identity heatmap ----
    output$identity_heatmap <- plotly::renderPlotly({
      req(rv$alignment, rv$alignment$dist_matrix)
      d <- rv$alignment$dist_matrix
      if (nrow(d) < 2) return(plotly::plot_ly())
      ident <- (1 - d) * 100
      labs <- rownames(ident)
      if (is.null(labs)) labs <- as.character(seq_len(nrow(ident)))
      plotly::plot_ly(
        x = labs, y = labs, z = ident, type = "heatmap",
        colorscale = "Viridis", zauto = FALSE,
        zmin = min(ident, na.rm = TRUE), zmax = 100,
        hovertemplate = "%{y} vs %{x}<br>%{z:.2f}%<extra></extra>"
      ) |> plotly::layout(
        xaxis = list(tickangle = -45),
        yaxis = list(autorange = "reversed"),
        margin = list(l = 100, r = 10, t = 10, b = 100)
      )
    })

    # Drilldown ----
    output$drilldown <- renderReactable({
      req(rv$alignment)
      pos <- rv$alignment$click_pos
      if (is.null(pos) || is.na(pos)) return(NULL)
      seqs_mat <- rv$alignment$seqs_mat
      if (pos < 1 || pos > ncol(seqs_mat)) return(NULL)
      d <- data.frame(
        path = rv$alignment$paths_used,
        base = seqs_mat[, pos],
        stringsAsFactors = FALSE
      )
      cov <- rv$alignment$coverage
      if (!is.null(cov)) {
        sub <- cov[cov$align_position == pos, c("path", "Depth", "ErrorRate", "GC"), drop = FALSE]
        d <- dplyr::left_join(d, sub, by = "path")
      }
      reactable(
        d, compact = TRUE,
        defaultColDef = colDef(align = "center"),
        columns = list(
          path = colDef(name = "Path", width = 60),
          base = colDef(name = "Base", width = 60)
        )
      )
    })

    output$msa_div <- renderUI({
      req(rv$alignment)

      msa <- msaR::renderMsaR(
        msaR::msaR(
          rv$alignment$seqs,
          alignmentHeight = rv$alignment$alignmentHeight,
          overviewbox = TRUE,
          seqlogo = FALSE,
          menu = FALSE,
          conservation = TRUE,
          overviewboxHeight = 20
        )
      )

      wait_align$hide()

      n_paths <- length(rv$alignment$paths_used)
      cov_height <- max(120, 80 * n_paths)

      div(
        style = "margin: 20px 5px 30px 5px;",
        h5("Mismatches"),
        div(
          style = "display:flex; gap:0.5em; align-items:center; margin-bottom:0.3em;",
          actionButton(ns("nav_prev"), "Prev", icon("angle-left"), class = "btn-sm"),
          actionButton(ns("nav_next"), "Next", icon("angle-right"), class = "btn-sm"),
          tags$span(textOutput(ns("nav_pos"), inline = TRUE),
            style = "margin-left: 0.5em; font-family: monospace;")
        ),
        plotly::plotlyOutput(ns("mismatch_track"), height = "70px"),
        h5("Per-path coverage"),
        plotly::plotlyOutput(ns("coverage_tracks"), height = sprintf("%dpx", cov_height)),
        h5("Alignment"),
        msa,
        p(rv$alignment$pct_id_range),
        p(rv$alignment$consRegion),
        h5("Position drilldown"),
        p(class = "text-muted", style = "font-size: 0.85em;",
          "Click a mismatch column above or use Prev/Next to inspect per-path base + coverage."),
        reactableOutput(ns("drilldown")),
        h5("Pairwise identity"),
        plotly::plotlyOutput(ns("identity_heatmap"), height = sprintf("%dpx", max(180, 30 * n_paths + 120))),
        actionButton(ns("trim_consensus"), "Trim Consensus")
      ) |> tagList()
    })

    # ---- Consensus review state machine ----

    # Enter review mode
    observeEvent(input$build_consensus, ignoreInit = TRUE, {
      req(rv$alignment)
      m <- rv$alignment$mismatch_cols
      seqs_mat <- rv$alignment$seqs_mat
      backbone_row <- 1L
      # Only review positions where backbone is not a gap (substitutions only)
      backbone_gaps <- seqs_mat[backbone_row, m] == "-"
      valid_cols <- m[!backbone_gaps]
      if (length(valid_cols) == 0) {
        shinyWidgets::sendSweetAlert(
          title = "Nothing to review",
          text = "All mismatch positions are insertions relative to the backbone (gaps). No substitutions to resolve.",
          type = "info"
        )
        return()
      }
      lrv$consensus_review <- list(
        active = TRUE,
        backbone_row = backbone_row,
        valid_cols = valid_cols,
        mismatch_idx = 1L,
        decisions = setNames(
          rep(NA_character_, length(valid_cols)),
          as.character(valid_cols)
        )
      )
      rv$alignment$nav_idx <- 1L
      rv$alignment$click_pos <- valid_cols[1]
    })

    # Exit review mode
    observeEvent(input$exit_review, ignoreInit = TRUE, {
      lrv$consensus_review <- NULL
    })

    # Accept decision for current position
    observeEvent(input$cons_accept, ignoreInit = TRUE, {
      req(lrv$consensus_review$active)
      cr <- lrv$consensus_review
      m <- cr$valid_cols
      idx <- cr$mismatch_idx
      if (idx > length(m)) return()
      pos <- m[idx]
      lrv$consensus_review$decisions[as.character(pos)] <- input$cons_choice
      next_idx <- min(idx + 1L, length(m) + 1L)
      lrv$consensus_review$mismatch_idx <- next_idx
      if (next_idx <= length(m)) {
        rv$alignment$nav_idx <- which(rv$alignment$mismatch_cols == m[next_idx])[1]
        rv$alignment$click_pos <- m[next_idx]
      }
    })

    # Skip (keep backbone) at current position
    observeEvent(input$cons_skip, ignoreInit = TRUE, {
      req(lrv$consensus_review$active)
      cr <- lrv$consensus_review
      m <- cr$valid_cols
      idx <- cr$mismatch_idx
      if (idx > length(m)) return()
      pos <- m[idx]
      backbone_base <- rv$alignment$seqs_mat[cr$backbone_row, pos]
      lrv$consensus_review$decisions[as.character(pos)] <- backbone_base
      next_idx <- min(idx + 1L, length(m) + 1L)
      lrv$consensus_review$mismatch_idx <- next_idx
      if (next_idx <= length(m)) {
        rv$alignment$nav_idx <- which(rv$alignment$mismatch_cols == m[next_idx])[1]
        rv$alignment$click_pos <- m[next_idx]
      }
    })

    # Consensus review panel (replaces drilldown when active)
    output$consensus_panel <- renderUI({
      cr <- lrv$consensus_review
      if (is.null(cr) || !isTRUE(cr$active)) return(NULL)

      m <- cr$valid_cols
      idx <- cr$mismatch_idx
      n_reviewed <- sum(!is.na(cr$decisions))

      if (idx > length(m)) {
        return(div(
          class = "alert alert-success",
          style = "margin: 10px 5px;",
          icon("check-circle"),
          sprintf(" All %d positions reviewed. Click 'Finalize Consensus' to generate the sequence.", length(m))
        ))
      }

      pos <- m[idx]
      seqs_mat <- rv$alignment$seqs_mat
      bases <- seqs_mat[, pos]
      non_gap <- bases[bases != "-"]
      tbl_counts <- sort(table(non_gap), decreasing = TRUE)
      majority_base <- names(tbl_counts)[1]
      majority_count <- as.integer(tbl_counts[1])
      backbone_base <- seqs_mat[cr$backbone_row, pos]
      is_tie <- length(tbl_counts) > 1 && tbl_counts[1] == tbl_counts[2]
      n_paths <- length(bases)
      iupac <- iupac_code(names(tbl_counts))

      # Build choice list
      choices <- c()
      if (!is_tie) {
        choices <- c(choices, setNames(
          majority_base,
          sprintf("Majority: %s  (%d / %d paths)", majority_base, majority_count, n_paths)
        ))
      } else {
        choices <- c(choices, setNames(
          iupac,
          sprintf("Tied majority — IUPAC ambiguity code: %s", iupac)
        ))
      }
      if (backbone_base != majority_base && !is_tie) {
        choices <- c(choices, setNames(
          backbone_base,
          sprintf("Keep backbone: %s", backbone_base)
        ))
      }
      if (!is_tie && iupac != majority_base) {
        choices <- c(choices, setNames(iupac, sprintf("IUPAC ambiguity: %s", iupac)))
      }
      choices <- c(choices, "N" = "N  (ambiguous)")
      for (b in setdiff(names(tbl_counts), c(majority_base, backbone_base))) {
        choices <- c(choices, setNames(b, sprintf("Minority: %s  (%d path%s)", b, tbl_counts[b], if (tbl_counts[b] > 1) "s" else "")))
      }

      prev_decision <- cr$decisions[as.character(pos)]
      selected_choice <- if (!is.na(prev_decision)) prev_decision else if (!is_tie) majority_base else iupac

      div(
        style = "border: 1px solid #bee5eb; border-radius: 6px; padding: 14px; margin: 10px 5px; background: #f0f9ff;",
        div(
          style = "display:flex; align-items:center; justify-content:space-between; margin-bottom:8px;",
          tags$h5(style = "margin:0;",
            sprintf("Consensus Review — Mismatch %d / %d  (column %d)", idx, length(m), pos)
          ),
          tags$span(
            style = sprintf("padding:3px 10px; border-radius:10px; font-size:0.8em; color:white; background:%s;",
              if (n_reviewed == length(m)) "#2ca02c" else "#6c757d"),
            sprintf("%d / %d decided", n_reviewed, length(m))
          )
        ),
        div(
          style = "display:flex; gap:0.5em; flex-wrap:wrap; margin-bottom:10px;",
          lapply(seq_along(bases), function(i) {
            b <- bases[i]
            bg <- if (b == "-") "#e9ecef" else if (b == majority_base) "#d4edda" else "#f8d7da"
            tags$span(
              style = sprintf(
                "padding:4px 10px; border-radius:4px; font-family:monospace; font-size:1.1em; background:%s; border:1px solid #ccc;",
                bg
              ),
              sprintf("path %s: %s", rv$alignment$paths_used[i], b)
            )
          })
        ),
        if (is_tie) tags$p(
          class = "text-warning",
          style = "margin-bottom:6px; font-size:0.9em;",
          icon("triangle-exclamation"), " Tied majority — no single base has more votes. IUPAC ambiguity code pre-selected."
        ),
        selectInput(
          ns("cons_choice"),
          label = "Decision for this position:",
          choices = choices,
          selected = selected_choice,
          width = "100%"
        ),
        div(
          style = "display:flex; gap:0.5em;",
          actionButton(ns("cons_accept"), "Accept", class = "btn-success btn-sm", icon = icon("check")),
          actionButton(ns("cons_skip"), "Skip (keep backbone)", class = "btn-secondary btn-sm")
        )
      )
    })

    # Finalize Consensus ----
    observeEvent(input$finalize_consensus, ignoreInit = TRUE, {
      req(rv$alignment, lrv$consensus_review$active)
      if (rv$updating$assemble_lock == 1) {
        shinyWidgets::sendSweetAlert(title = "Assembly Locked!", type = "warning")
        req(F)
      }

      cr <- lrv$consensus_review
      seqs_mat <- rv$alignment$seqs_mat
      backbone_row <- cr$backbone_row
      backbone_chars <- seqs_mat[backbone_row, ]
      nongap <- which(backbone_chars != "-")
      consensus_chars <- backbone_chars[nongap]

      # Apply decisions; build edit log
      edit_log <- list()
      for (pos_str in names(cr$decisions)) {
        dec <- cr$decisions[[pos_str]]
        if (is.na(dec)) next
        pos <- as.integer(pos_str)
        raw_idx <- which(nongap == pos)
        if (length(raw_idx) == 0) next
        old_base <- consensus_chars[raw_idx]
        if (dec != old_base) {
          consensus_chars[raw_idx] <- dec
          edit_log <- c(edit_log, list(list(
            aln_col = pos,
            raw_pos = as.integer(raw_idx),
            from = old_base,
            to = dec
          )))
        }
      }

      consensus_seq <- paste(consensus_chars, collapse = "")
      consensus_bs <- Biostrings::DNAStringSet(consensus_seq)
      names(consensus_bs) <- paste(rv$updating$ID, 0, 0, sep = ".") |> paste("linear")

      out_fasta <- file.path(
        session$userData$dir_out, rv$updating$ID, "assemble",
        rv$updating$assemble_opts,
        paste0(rv$updating$ID, "_assembly_0.fasta")
      )
      Biostrings::writeXStringSet(consensus_bs, out_fasta)

      # Coverage stats from backbone path
      backbone_path <- rv$alignment$paths_used[backbone_row]
      cov_in <- file.path(
        session$userData$dir_out, rv$updating$ID, "assemble",
        rv$updating$assemble_opts,
        paste0(rv$updating$ID, "_assembly_", backbone_path, "_coverageStats.csv")
      )
      coverage <- tryCatch(read.csv(cov_in), error = function(e) NULL)
      if (!is.null(coverage)) {
        coverage <- dplyr::mutate(coverage,
          Position = dplyr::row_number(),
          SeqId = stringr::str_replace(SeqId, "[0-9]+\\.[0-9]+$", "0.0")
        )
        readr::write_csv(
          coverage,
          file.path(
            session$userData$dir_out, rv$updating$ID, "assemble",
            rv$updating$assemble_opts,
            paste0(rv$updating$ID, "_assembly_0_coverageStats.csv")
          ),
          quote = "none", na = ""
        )
      }

      edit_positions_json <- if (length(edit_log) > 0)
        as.character(jsonlite::toJSON(edit_log, auto_unbox = TRUE))
      else
        "[]"

      # Ignore all current assemblies for this ID
      DBI::dbExecute(
        session$userData$con,
        sprintf("UPDATE assemblies SET ignore = 1 WHERE ID = '%s';", rv$updating$ID)
      )

      # Build insert row
      new_row <- data.frame(
        ID = rv$updating$ID,
        path = 0L,
        scaffold = 0L,
        topology = "linear",
        length = nchar(consensus_seq),
        sequence = consensus_seq,
        depth = if (!is.null(coverage)) paste(coverage$Depth, collapse = " ") else NA_character_,
        gc = if (!is.null(coverage)) paste(coverage$GC, collapse = " ") else NA_character_,
        errors = if (!is.null(coverage)) paste(coverage$ErrorRate, collapse = " ") else NA_character_,
        ignore = 0L,
        edited = 1L,
        time_stamp = as.numeric(Sys.time()),
        stringsAsFactors = FALSE
      )
      if ("edit_positions" %in% DBI::dbListFields(session$userData$con, "assemblies")) {
        new_row$edit_positions <- edit_positions_json
      }

      dplyr::tbl(session$userData$con, "assemblies") |>
        dplyr::rows_upsert(new_row, by = c("ID", "path", "scaffold"), copy = TRUE, in_place = TRUE)

      # Summary note for assemble table
      n_edits <- length(edit_log)
      edit_detail <- if (n_edits > 0) {
        changes <- paste(
          vapply(edit_log, function(x) sprintf("col%d:%s→%s", x$aln_col, x$from, x$to), character(1)),
          collapse = "; "
        )
        sprintf(
          "Consensus built from %d paths (%d edit%s: %s). ",
          length(rv$alignment$paths_used), n_edits,
          if (n_edits == 1) "" else "s", changes
        )
      } else {
        sprintf(
          "Consensus built from %d paths (0 edits — backbone identical at all reviewed positions). ",
          length(rv$alignment$paths_used)
        )
      }

      update <- data.frame(
        ID = rv$updating$ID,
        paths = -abs(rv$updating$paths),
        assemble_lock = 1L,
        topology = "linear",
        assemble_notes = paste0(edit_detail, rv$updating$assemble_notes %|NA|% ""),
        stringsAsFactors = FALSE
      )
      rv$data <- rv$data |> dplyr::rows_update(update, by = "ID")
      dplyr::tbl(session$userData$con, "assemble") |>
        dplyr::rows_update(update, by = "ID", copy = TRUE, in_place = TRUE, unmatched = "ignore")

      lrv$consensus_review <- NULL
      rv$updating <- rv$data |> dplyr::filter(ID == !!rv$updating$ID)
      trigger("coverage_modal")
    })

    # Trim Consensus ----
    observeEvent(input$trim_consensus, {
      if (rv$updating$assemble_lock == 1) {
        shinyWidgets::sendSweetAlert(
          title = "Assembly Locked!",
          type = "warning"
        )
        req(F)
      }

      # Make new assembly
      trimmed <- purrr::map2_chr(rv$alignment$consStart, rv$alignment$consEnd, ~ {
        Biostrings::subseq(rv$alignment$seqs[1], .x, .y) |> as.character()
      }) |>
        paste(collapse = "") |>
        Biostrings::DNAStringSet()
      names(trimmed) <- paste(
        rv$updating$ID,
        0, 0,
        sep = "."
      ) |> paste("linear")
      Biostrings::writeXStringSet(
        trimmed,
        file.path(
          session$userData$dir_out,
          rv$updating$ID,
          "assemble",
          rv$updating$assemble_opts,
          paste0(rv$updating$ID, "_assembly_0.fasta")
        )
      )

      # Updated coverage stats file
      coverage <- file.path(
        session$userData$dir_out,
        rv$updating$ID,
        "assemble",
        rv$updating$assemble_opts,
        paste0(rv$updating$ID, "_assembly_", rv$focal_assembly$path[selected()[1]], "_coverageStats.csv")
      ) |> read.csv()

      start_offset <- (stringr::str_extract(as.character(rv$alignment$seqs[1]), "^-+") |> nchar()) %|NA|% 0

      coverage <- purrr::map2_dfr(rv$alignment$consStart, rv$alignment$consEnd, ~ {
        coverage[(.x + start_offset):(.y + start_offset), ]
      }) |>
        dplyr::mutate(
          Position = dplyr::row_number(),
          SeqId = stringr::str_replace(SeqId, "[0-9]+\\.[0-9]+$", "0.0")
        )

      readr::write_csv(
        coverage,
        file.path(
          session$userData$dir_out,
          rv$updating$ID,
          "assemble",
          rv$updating$assemble_opts,
          paste0(rv$updating$ID, "_assembly_0_coverageStats.csv")
        ),
        quote = "none", na = ""
      )

      ## Update assemblies table ----
      DBI::dbExecute(
        session$userData$con,
        stringr::str_glue("UPDATE assemblies SET ignore = 1 WHERE ID = '{rv$updating$ID}';")
      )
      trimmed_assembly <- data.frame(
        ID = rv$updating$ID,
        path = 0,
        scaffold = 0,
        topology = "linear",
        length = trimmed@ranges@width,
        length_raw = trimmed@ranges@width,
        sequence = unname(as.character(trimmed)),
        depth = paste(coverage$Depth, collapse = " "),
        gc = paste(coverage$GC, collapse = " "),
        errors = paste(coverage$ErrorRate, collapse = " "),
        ignore = 0,
        edited = 1,
        time_stamp = as.numeric(Sys.time())
      )
      dplyr::tbl(session$userData$con, "assemblies") |>
        dplyr::rows_upsert(
          trimmed_assembly,
          by = c("ID", "path", "scaffold"),
          copy = T,
          in_place = T
        )

      update <- data.frame(
        ID = rv$updating$ID,
        paths = -abs(rv$updating$paths),
        assemble_lock = 1,
        topology = "linear",
        assemble_notes = paste(
          "Assembly edited - multi-path getOrganelle output trimmed for consensus. ",
          rv$updating$assemble_notes
        ) |>
          stringr::str_remove("Unable to resolve single assembly from reads")
      )
      rv$data <- rv$data |>
        dplyr::rows_update(
          update,
          by = "ID"
        )
      dplyr::tbl(session$userData$con, "assemble") |>
        dplyr::rows_update(
          update,
          by = "ID",
          copy = T,
          in_place = T,
          unmatched = "ignore"
        )

      rv$updating <- rv$data |>
        dplyr::filter(ID == !!rv$updating$ID)

      trigger("coverage_modal")
    })
  })
}
