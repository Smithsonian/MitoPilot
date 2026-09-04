#' Narrowest and widest zoom windows, in bases.
#' @noRd
MTR_VIEW_MIN_BP <- 100L

#' Points drawn in the coverage track before binning kicks in.
#' @noRd
MTR_VIEW_POINTS <- 2000L

#' Narrowest and widest read pileup windows, in bases.
#' @noRd
MTR_PILEUP_MIN_BP <- 100L
MTR_PILEUP_MAX_BP <- 1000L

#' Default pileup window, in bases.
#' @noRd
MTR_PILEUP_BP <- 300L

#' Screen pixels per base in the pileup. Fixed, so bases keep the same width
#' (and the letters the same size) at every window; the plot is drawn wider
#' than the modal and scrolls sideways.
#' @noRd
MTR_PILEUP_PX <- 11

#' y of the consensus row, and of the first read row below the sequence band.
#' @noRd
MTR_PILEUP_CONS_Y <- -1.7
MTR_PILEUP_READ_TOP <- -3

#' Placeholder for the MapToRef coverage and pileup viewer
#'
#' Rendered inside the assembly details modal; empty for samples that were not
#' assembled with MapToRef.
#'
#' @param id module id
#' @return a uiOutput
#'
#' @noRd
maptoref_viewer_ui <- function(id) {
  uiOutput(shiny::NS(id, "view"))
}

#' MapToRef coverage and pileup viewer
#'
#' @noRd
maptoref_viewer_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    state <- reactiveValues(
      paths = NULL, depth = NULL, features = NULL, summary = NULL,
      ref_seq = NA_character_, cons_seq = NA_character_, len = 0L,
      has_work = FALSE,
      win_center = 0, win_size = 0, pileup_center = NA_real_
    )

    win_range <- reactive({
      req(state$len > 0L)
      half <- state$win_size / 2
      lo <- max(1, round(state$win_center - half))
      hi <- min(state$len, round(state$win_center + half))
      c(lo, hi)
    })

    set_window <- function(center, size) {
      size <- max(MTR_VIEW_MIN_BP, min(state$len, round(size)))
      half <- size / 2
      center <- max(half, min(state$len - half, center))
      state$win_size <- size
      state$win_center <- center
      updateNumericInput(session, "win_size", value = size)
    }

    on("coverage_modal", {
      p <- maptoref_paths(
        session$userData$dir_out, rv$updating$ID, rv$updating$assemble_opts
      )
      state$paths <- p
      state$has_work <- dir.exists(p$work)
      state$depth <- maptoref_read_depth(p$depth)
      state$features <- maptoref_read_features(p$features)
      state$summary <- maptoref_read_summary(p$summary)
      state$ref_seq <- maptoref_read_seq(p$ref_fasta)
      state$cons_seq <- maptoref_read_seq(p$consensus)
      state$len <- nrow(state$depth)
      state$pileup_center <- NA_real_
      state$win_size <- state$len
      state$win_center <- state$len / 2
    })

    output$view <- renderUI({
      if (!isTRUE(state$has_work)) {
        return(NULL)
      }
      if (state$len == 0L) {
        return(tags$div(
          style = "margin: 10px 0; color: #888; font-size: 0.9em;",
          "MapToRef coverage table not found for this sample. Re-run ",
          "Assembly to produce the coverage and read files."
        ))
      }
      tags$div(
        class = "maptoref-viewer",
        style = paste0("margin-top: 20px; border-top: 2px solid #bbb; ",
                       "padding-top: 10px;"),
        tags$h4(
          "MapToRef reference coverage",
          style = paste0("margin: 0 0 8px 0; font-size: 20px; ",
                         "font-weight: 700; color: #2c3e50;")
        ),
        uiOutput(ns("header")),
        tags$details(
          id = ns("map_details"),
          open = TRUE,
          tags$summary("Coverage map"),
          tags$div(
            style = "display:flex; gap:8px; align-items:flex-end; margin:6px 0;",
            actionButton(ns("zoom_out"), "-", class = "btn-sm"),
            actionButton(ns("zoom_in"), "+", class = "btn-sm"),
            actionButton(ns("zoom_reset"), "Full view", class = "btn-sm"),
            numericInput(ns("win_size"), "Window (bp)",
                         value = state$len, min = MTR_VIEW_MIN_BP,
                         max = state$len, step = 100, width = "140px"),
            numericInput(ns("pileup_size"), "Pileup window (bp)",
                         value = MTR_PILEUP_BP, min = MTR_PILEUP_MIN_BP,
                         max = MTR_PILEUP_MAX_BP, step = 50, width = "160px")
          ),
          tags$div(
            style = "position:relative;",
            plotOutput(
              ns("tracks"), height = "420px",
              hover = hoverOpts(ns("tracks_hover"), delay = 100,
                                delayType = "throttle"),
              click = ns("tracks_click"),
              brush = brushOpts(ns("tracks_brush"), direction = "x",
                                resetOnNew = TRUE)
            ),
            uiOutput(ns("tooltip"))
          )
        ),
        tags$details(
          id = ns("pileup_details"),
          tags$summary("Read pileup"),
          uiOutput(ns("pileup_ui"))
        )
      )
    })

    output$header <- renderUI({
      s <- state$summary
      fld <- function(k) {
        v <- unname(s[k])
        if (is.null(v) || is.na(v)) "not recorded" else v
      }
      n_pct <- suppressWarnings(
        round(100 * as.numeric(fld("n_count")) /
                as.numeric(fld("reference_length")), 1)
      )
      item <- function(label, value) {
        tags$span(
          style = "margin-right:18px;",
          tags$b(label), " ", value
        )
      }
      tags$div(
        style = "font-size:90%; padding-bottom:4px;",
        item("Reference:", fld("accession")),
        item("Organism:", fld("organism")),
        item("Length:", paste0(fld("reference_length"), " bp")),
        item("Source:", fld("reference_source")),
        item("Reads mapped:", fld("reads_mapped_final")),
        item("Mean depth:", round(mean(state$depth$Depth), 1)),
        item("Uncalled:", paste0(n_pct, "%")),
        if (nrow(state$features) == 0L) {
          tags$span(
            style = "color:#888;",
            "Reference has no annotation record."
          )
        }
      )
    })

    observeEvent(input$zoom_in, {
      set_window(state$win_center, state$win_size / 2)
    })
    observeEvent(input$zoom_out, {
      set_window(state$win_center, state$win_size * 2)
    })
    observeEvent(input$zoom_reset, {
      set_window(state$len / 2, state$len)
    })
    observeEvent(input$win_size, ignoreInit = TRUE, {
      req(input$win_size, state$len > 0L)
      if (!isTRUE(all.equal(input$win_size, state$win_size))) {
        set_window(state$win_center, input$win_size)
      }
    })
    observeEvent(input$tracks_brush, {
      b <- input$tracks_brush
      req(b)
      # Clear the selection before redrawing: a brush left on the plot is
      # re-mapped onto the new axes and sent back, which zooms again forever.
      session$resetBrush("tracks_brush")
      set_window((b$xmin + b$xmax) / 2, b$xmax - b$xmin)
    })

    output$tracks <- renderPlot({
      req(state$len > 0L)
      rng <- win_range()
      .mtr_view_tracks(state$depth, state$features, rng)
    })

    output$tooltip <- renderUI({
      h <- input$tracks_hover
      req(h, state$len > 0L)
      pos <- round(h$x)
      req(pos >= 1L, pos <= state$len)
      d <- state$depth$Depth[pos]
      f <- state$features
      gene <- f$gene[f$start <= pos & f$end >= pos]
      tags$div(
        style = paste0(
          "position:absolute; z-index:100; pointer-events:none; ",
          "background:rgba(255,255,255,0.92); border:1px solid #999; ",
          "border-radius:3px; padding:3px 6px; font-size:85%; ",
          "left:", h$coords_css$x + 12, "px; top:", h$coords_css$y + 12, "px;"
        ),
        tags$div(tags$b("Position: "), format(pos, big.mark = ",")),
        tags$div(tags$b("Depth: "), d),
        if (length(gene) > 0L) tags$div(tags$b("Gene: "), gene[1])
      )
    })

    observeEvent(input$tracks_click, {
      req(input$tracks_click, state$len > 0L)
      state$pileup_center <- input$tracks_click$x
      # Clicking the map is a request to see the reads, so open the section.
      shinyjs::runjs(sprintf(
        "var d=document.getElementById('%s'); if(d){d.open=true;}",
        ns("pileup_details")
      ))
    })

    pileup_range <- reactive({
      req(!is.na(state$pileup_center), state$len > 0L)
      size <- input$pileup_size
      if (!shiny::isTruthy(size)) size <- MTR_PILEUP_BP
      size <- max(MTR_PILEUP_MIN_BP, min(MTR_PILEUP_MAX_BP, size))
      half <- size / 2
      center <- max(half, min(state$len - half, state$pileup_center))
      c(max(1, round(center - half)), min(state$len, round(center + half)))
    })

    pileup_data <- reactive({
      rng <- pileup_range()
      maptoref_window_reads(
        state$paths$bam, rng[1], rng[2], state$ref_seq
      )
    })

    output$pileup_ui <- renderUI({
      if (is.na(state$pileup_center)) {
        return(tags$p(
          style = "color:#888; margin-top:8px;",
          "Click the coverage plot to see the reads at that position."
        ))
      }
      if (!file.exists(state$paths$bam %||% "")) {
        return(tags$p(
          style = "color:#888; margin-top:8px;",
          "No read alignments were kept for this sample. Re-run Assembly to ",
          "enable the read view."
        ))
      }
      w <- pileup_data()
      rng <- pileup_range()
      n_rows <- if (nrow(w$reads) > 0L) max(w$reads$row) else 0L
      height <- max(320, 120 + 14 * n_rows)
      width <- round(MTR_PILEUP_PX * (diff(rng) + 1))
      # Scroll so the clicked base lands in the middle of the visible strip.
      offset <- MTR_PILEUP_PX * (state$pileup_center - rng[1])
      tagList(
        uiOutput(ns("pileup_note")),
        tags$div(
          style = "display:flex; align-items:flex-start;",
          plotOutput(ns("pileup_labels"), width = "112px",
                     height = paste0(height, "px")),
          tags$div(
            style = "flex:1; min-width:0;",
            # Empty strip whose only job is to put a second scrollbar above
            # the plot, kept in step with the real one below.
            tags$div(
              id = ns("pileup_scroll_top"),
              style = paste0("overflow-x:auto; overflow-y:hidden; ",
                             "width:100%; height:16px;"),
              tags$div(style = paste0("width:", width, "px; height:1px;"))
            ),
            tags$div(
              id = ns("pileup_scroll"),
              style = "overflow-x:auto; overflow-y:hidden; width:100%;",
              plotOutput(ns("pileup"), width = paste0(width, "px"),
                         height = paste0(height, "px"))
            )
          )
        ),
        tags$script(HTML(sprintf(
          paste0(
            "setTimeout(function(){",
            "var a=document.getElementById('%s'),",
            "b=document.getElementById('%s');",
            "if(!a||!b){return;}",
            "var busy=false;",
            "function sync(src,dst){if(busy){return;}busy=true;",
            "dst.scrollLeft=src.scrollLeft;busy=false;}",
            "a.onscroll=function(){sync(a,b);};",
            "b.onscroll=function(){sync(b,a);};",
            "b.scrollLeft=%f - b.clientWidth/2;a.scrollLeft=b.scrollLeft;",
            "}, 0);"
          ),
          ns("pileup_scroll_top"), ns("pileup_scroll"), offset
        )))
      )
    })

    output$pileup_note <- renderUI({
      w <- pileup_data()
      rng <- pileup_range()
      txt <- if (w$n_total == 0L) {
        "No reads in this window."
      } else if (w$n_shown < w$n_total) {
        paste0("Showing ", format(w$n_shown, big.mark = ","), " of ",
               format(w$n_total, big.mark = ","), " reads.")
      } else {
        paste0(format(w$n_total, big.mark = ","), " reads.")
      }
      tags$div(
        style = "font-size:85%; color:#666; margin-top:6px;",
        paste0(format(rng[1], big.mark = ","), " - ",
               format(rng[2], big.mark = ","), " bp. "), txt
      )
    })

    output$pileup_labels <- renderPlot({
      w <- pileup_data()
      .mtr_view_pileup_labels(
        if (nrow(w$reads) > 0L) max(w$reads$row) else 0L
      )
    })

    output$pileup <- renderPlot({
      rng <- pileup_range()
      .mtr_view_pileup(pileup_data(), state$ref_seq, state$cons_seq, rng)
    }) |>
      shiny::bindCache(state$paths$bam, pileup_range(), input$pileup_size)
  })
}

#' Contiguous zero-depth runs inside one window
#'
#' Drawn as bands rather than one line per base: at full-genome zoom a line per
#' uncovered base smears into a solid red block.
#'
#' @param pos integer positions with zero depth, ascending
#' @param min_w minimum band width, so single-base gaps stay visible
#' @return a data.frame of xmin/xmax, or NULL
#'
#' @noRd
.mtr_zero_runs <- function(pos, min_w = 1) {
  if (length(pos) == 0L) {
    return(NULL)
  }
  brk <- c(0L, which(diff(pos) != 1L), length(pos))
  starts <- pos[utils::head(brk, -1L) + 1L]
  ends <- pos[brk[-1L]]
  pad <- pmax(0, (min_w - (ends - starts + 1)) / 2)
  data.frame(xmin = starts - 0.5 - pad, xmax = ends + 0.5 + pad)
}

#' Coverage and gene track for one window
#'
#' Drawn as a single ggplot panel, with the genes in a band below zero. A
#' patchwork of two plots would look the same but reaches the browser without a
#' coordmap, which breaks hover, click, and brush-to-zoom.
#'
#' Gene labels are drawn only when the window is narrow enough for them to be
#' legible; across a whole mitogenome 38 labels collide into a smear.
#'
#' @param depth data.frame with Position and Depth
#' @param features annotation frame from maptoref_read_features()
#' @param rng length-2 numeric, the visible window
#' @return a ggplot
#'
#' @noRd
.mtr_view_tracks <- function(depth, features, rng) {
  full <- depth[depth$Position >= rng[1] & depth$Position <= rng[2], , drop = FALSE]
  d <- maptoref_bin_depth(full, MTR_VIEW_POINTS)
  ymax <- max(c(d$Depth, 1))
  gene_y <- -0.11 * ymax
  brk <- pretty(c(0, ymax))
  # Gaps come from the unbinned depths: binning keeps the max per bin, so a
  # short gap inside a covered bin would otherwise disappear.
  gaps <- .mtr_zero_runs(full$Position[full$Depth == 0],
                         min_w = max(1, 0.002 * diff(rng)))
  p <- ggplot2::ggplot(d, ggplot2::aes(x = Position, y = Depth)) +
    {
      if (!is.null(gaps)) {
        ggplot2::geom_rect(
          data = gaps, inherit.aes = FALSE,
          ggplot2::aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
          fill = "#f2b8a8", alpha = 0.55
        )
      }
    } +
    ggplot2::geom_area(fill = "#4c72b0", color = NA)

  f <- features[features$end >= rng[1] & features$start <= rng[2], , drop = FALSE]
  if (nrow(f) > 0L) {
    f$y <- gene_y
    p <- p +
      gggenes::geom_gene_arrow(
        data = f, inherit.aes = FALSE,
        ggplot2::aes(xmin = start, xmax = end, y = y,
                     forward = strand != "-", fill = type),
        arrowhead_height = grid::unit(4, "mm"),
        arrowhead_width = grid::unit(2, "mm"),
        arrow_body_height = grid::unit(3, "mm")
      ) +
      ggplot2::scale_fill_manual(
        values = c("CDS" = "#8fb3d9", "tRNA" = "#c7e0a8",
                   "rRNA" = "#f2c297", "D-loop" = "#d9c7e8",
                   "gene" = "#cccccc"),
        na.value = "#cccccc"
      )
    if (diff(rng) <= 2000) {
      p <- p + ggplot2::geom_text(
        data = f, inherit.aes = FALSE,
        ggplot2::aes(x = (start + end) / 2, y = y, label = gene),
        size = 2.6, vjust = -1.8
      )
    }
  }

  p +
    ggplot2::scale_x_continuous(labels = scales::label_comma()) +
    ggplot2::scale_y_continuous(breaks = brk[brk >= 0]) +
    ggplot2::coord_cartesian(
      xlim = rng, ylim = c(2 * gene_y, ymax * 1.03), expand = FALSE
    ) +
    ggplot2::labs(x = "Reference position (bp)", y = "Depth") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "none",
      panel.background = ggplot2::element_rect(fill = "#fafafa", color = NA),
      panel.grid.major = ggplot2::element_line(color = "#c4c4c4",
                                               linewidth = 0.3),
      panel.grid.minor = ggplot2::element_blank()
    )
}

#' Base colours shared by the sequence rows and the mismatch letters.
#' @noRd
MTR_BASE_COLORS <- c(A = "#3aa03a", C = "#2f6fb5", G = "#e0a030",
                     T = "#cc4b4b", N = "#999999")

#' Vertical extent of a pileup plot
#'
#' The plot and its sticky label gutter are two separate images that must line
#' up row for row, so both are drawn against these limits.
#'
#' @param n_rows number of stacked read rows
#' @return length-2 numeric, bottom then top
#'
#' @noRd
.mtr_pileup_ylim <- function(n_rows) {
  c(MTR_PILEUP_READ_TOP - n_rows - 0.6, 0.9)
}

#' Sticky row labels for the pileup
#'
#' Drawn as its own image beside the scrolling pileup, so the row names stay in
#' view however far the reads are scrolled. Everything but the labels matches
#' .mtr_view_pileup()'s layout, which keeps the two panels aligned.
#'
#' @param n_rows number of stacked read rows
#' @return a ggplot
#'
#' @noRd
.mtr_view_pileup_labels <- function(n_rows) {
  ggplot2::ggplot() +
    ggplot2::annotate(
      "text", x = 1, y = c(0, MTR_PILEUP_CONS_Y),
      label = c("Reference", "Consensus"),
      hjust = 1, size = 4.2, fontface = "bold", color = "#444444"
    ) +
    ggplot2::scale_x_continuous(limits = c(0, 1), breaks = 0.5, labels = " ") +
    ggplot2::coord_cartesian(ylim = .mtr_pileup_ylim(n_rows)) +
    ggplot2::labs(x = "Reference position (bp)", y = NULL) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      # Kept, but invisible: they reserve the same strip the scrolling plot
      # spends on its x axis, so the rows stay level.
      axis.text.x = ggplot2::element_text(color = NA),
      axis.title.x = ggplot2::element_text(color = NA),
      plot.margin = ggplot2::margin(5.5, 0, 5.5, 5.5)
    )
}

#' Read pileup for one window
#'
#' @param win result of maptoref_window_reads()
#' @param ref_seq,cons_seq full-length reference and consensus strings
#' @param rng length-2 numeric, the pileup window
#' @return a ggplot
#'
#' @noRd
.mtr_view_pileup <- function(win, ref_seq, cons_seq, rng) {
  pos <- seq(max(1, rng[1]), rng[2])
  # The plot is drawn at a fixed pixel width per base and scrolls sideways, so
  # letters are always legible and never need to be dropped.
  seq_row <- function(s, label, y) {
    # A window with no overlap at all leaves substr() empty, and a zero-row
    # base vector against a one-row label would not recycle.
    if (is.na(s) || pos[1] > nchar(s)) {
      return(NULL)
    }
    b <- strsplit(substr(s, pos[1], pos[length(pos)]), "", fixed = TRUE)[[1]]
    data.frame(pos = pos[seq_along(b)], base = b, track = label, y = y,
               stringsAsFactors = FALSE)
  }
  bases <- rbind(
    seq_row(ref_seq, "Reference", 0),
    seq_row(cons_seq, "Consensus", MTR_PILEUP_CONS_Y)
  )

  n_rows <- if (nrow(win$reads) > 0L) max(win$reads$row) else 0L
  p <- ggplot2::ggplot() +
    ggplot2::scale_x_continuous(labels = scales::label_comma(),
                                expand = ggplot2::expansion(0)) +
    ggplot2::coord_cartesian(
      xlim = c(rng[1] - 0.5, rng[2] + 0.5), ylim = .mtr_pileup_ylim(n_rows)
    ) +
    ggplot2::scale_fill_manual(values = MTR_BASE_COLORS, na.value = "#bbbbbb") +
    ggplot2::scale_color_manual(values = MTR_BASE_COLORS, na.value = "#bbbbbb") +
    ggplot2::labs(x = "Reference position (bp)", y = NULL) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "none",
      panel.grid = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank()
    )

  if (!is.null(bases)) {
    p <- p +
      ggplot2::geom_tile(
        data = bases,
        ggplot2::aes(x = pos, y = y, fill = base),
        height = 1.4, alpha = 0.8
      ) +
      ggplot2::geom_text(
        data = bases,
        ggplot2::aes(x = pos, y = y, label = base),
        size = 4.4, fontface = "bold", color = "white"
      )
  }

  if (nrow(win$reads) > 0L) {
    r <- win$reads
    r$y <- MTR_PILEUP_READ_TOP - r$row
    p <- p +
      ggplot2::geom_rect(
        data = r,
        ggplot2::aes(xmin = start - 0.5, xmax = end + 0.5,
                     ymin = y - 0.35, ymax = y + 0.35,
                     group = row),
        fill = ifelse(r$strand == "-", "#d5d8e0", "#c3ccd9"), color = NA
      )
    if (nrow(win$del) > 0L) {
      dl <- win$del
      dl$y <- MTR_PILEUP_READ_TOP - dl$row
      p <- p + ggplot2::geom_segment(
        data = dl,
        ggplot2::aes(x = start - 0.5, xend = end + 0.5,
                     y = y, yend = y),
        color = "#555555", linewidth = 0.3
      )
    }
    if (nrow(win$mm) > 0L) {
      mm <- win$mm
      mm$y <- MTR_PILEUP_READ_TOP - mm$row
      p <- p + ggplot2::geom_text(
        data = mm,
        ggplot2::aes(x = pos, y = y, label = base, color = base),
        size = 3.6, fontface = "bold"
      )
    }
    if (nrow(win$ins) > 0L) {
      ins <- win$ins
      ins$y <- MTR_PILEUP_READ_TOP - ins$row
      p <- p + ggplot2::geom_segment(
        data = ins,
        ggplot2::aes(x = pos + 0.5, xend = pos + 0.5,
                     y = y - 0.4, yend = y + 0.4),
        color = "#7b3fa0", linewidth = 0.7
      )
    }
  }
  p
}
