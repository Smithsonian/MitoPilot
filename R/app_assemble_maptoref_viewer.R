#' Narrowest and widest zoom windows, in bases.
#' @noRd
MTR_VIEW_MIN_BP <- 100L

#' Points drawn in the coverage track before binning kicks in.
#' @noRd
MTR_VIEW_POINTS <- 2000L

#' MapToRef coverage and pileup viewer
#'
#' @noRd
maptoref_viewer_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    init("maptoref_modal")

    state <- reactiveValues(
      paths = NULL, depth = NULL, features = NULL, summary = NULL,
      ref_seq = NA_character_, cons_seq = NA_character_, len = 0L,
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

    on("maptoref_modal", {
      p <- maptoref_paths(
        session$userData$dir_out, rv$updating$ID, rv$updating$assemble_opts
      )
      state$paths <- p
      state$depth <- maptoref_read_depth(p$depth)
      state$features <- maptoref_read_features(p$features)
      state$summary <- maptoref_read_summary(p$summary)
      state$ref_seq <- maptoref_read_seq(p$ref_fasta)
      state$cons_seq <- maptoref_read_seq(p$consensus)
      state$len <- nrow(state$depth)
      state$pileup_center <- NA_real_

      if (state$len == 0L) {
        shinyWidgets::sendSweetAlert(
          title = "No MapToRef coverage data",
          text = tags$div(
            tags$p(
              "No coverage table was found for ", tags$b(rv$updating$ID),
              " at:"
            ),
            tags$ul(tags$li(tags$code(p$depth))),
            tags$p(
              "Re-run Assembly for this sample to produce the MapToRef ",
              "coverage and read files."
            )
          ),
          html = TRUE, type = "error"
        )
        return()
      }

      state$win_size <- state$len
      state$win_center <- state$len / 2

      showModal(modalDialog(
        title = NULL, size = "xl", easyClose = TRUE, footer = NULL,
        tags$div(
          class = "maptoref-viewer",
          uiOutput(ns("header")),
          tags$div(
            style = "display:flex; gap:8px; align-items:flex-end; margin:6px 0;",
            actionButton(ns("zoom_out"), "-", class = "btn-sm"),
            actionButton(ns("zoom_in"), "+", class = "btn-sm"),
            actionButton(ns("zoom_reset"), "Full view", class = "btn-sm"),
            numericInput(ns("win_size"), "Window (bp)",
                         value = state$len, min = MTR_VIEW_MIN_BP,
                         max = state$len, step = 100, width = "140px"),
            numericInput(ns("pileup_size"), "Pileup window (bp)",
                         value = 200, min = 50, max = 1000, step = 50,
                         width = "160px")
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
          ),
          uiOutput(ns("pileup_ui"))
        )
      ))
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
        tags$h4(rv$updating$ID, style = "margin:0 0 4px 0;"),
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
    })

    pileup_range <- reactive({
      req(!is.na(state$pileup_center), state$len > 0L)
      size <- input$pileup_size
      if (!shiny::isTruthy(size)) size <- 200
      size <- max(50, min(1000, size))
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
      n_rows <- if (nrow(w$reads) > 0L) max(w$reads$row) else 0L
      height <- max(320, 90 + 14 * n_rows)
      tagList(
        uiOutput(ns("pileup_note")),
        plotOutput(ns("pileup"), height = paste0(height, "px"))
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

    output$pileup <- renderPlot({
      rng <- pileup_range()
      .mtr_view_pileup(pileup_data(), state$ref_seq, state$cons_seq, rng)
    }) |>
      shiny::bindCache(state$paths$bam, pileup_range(), input$pileup_size)
  })
}

#' Gene arrow track for one window
#'
#' Labels are drawn only when the window is narrow enough for them to be
#' legible; across a whole mitogenome 38 labels collide into a smear.
#'
#' @param features annotation frame from maptoref_read_features()
#' @param rng length-2 numeric, the visible window
#' @return a ggplot
#'
#' @noRd
.mtr_view_features <- function(features, rng) {
  f <- features[features$end >= rng[1] & features$start <= rng[2], , drop = FALSE]
  base <- ggplot2::ggplot() +
    ggplot2::coord_cartesian(xlim = rng, expand = FALSE) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none")
  if (nrow(f) == 0L) {
    return(base)
  }
  f$y <- "genes"
  p <- base +
    gggenes::geom_gene_arrow(
      data = f,
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
      data = f,
      ggplot2::aes(x = (start + end) / 2, y = y, label = gene),
      size = 2.6, vjust = -1.6
    )
  }
  p
}

#' Coverage and annotation panels for one window
#'
#' @param depth data.frame with Position and Depth
#' @param features annotation frame from maptoref_read_features()
#' @param rng length-2 numeric, the visible window
#' @return a ggplot, or a patchwork of the annotation panel above coverage
#'
#' @noRd
.mtr_view_tracks <- function(depth, features, rng) {
  d <- depth[depth$Position >= rng[1] & depth$Position <= rng[2], , drop = FALSE]
  d <- maptoref_bin_depth(d, MTR_VIEW_POINTS)
  zero <- d[d$Depth == 0, , drop = FALSE]
  cov <- ggplot2::ggplot(d, ggplot2::aes(x = Position, y = Depth)) +
    {
      if (nrow(zero) > 0L) {
        ggplot2::geom_vline(
          data = zero, ggplot2::aes(xintercept = Position),
          color = "#FF6670", linewidth = 0.4
        )
      }
    } +
    ggplot2::geom_area(fill = "#4c72b0", color = NA) +
    ggplot2::scale_x_continuous(labels = scales::label_comma()) +
    ggplot2::coord_cartesian(xlim = rng, expand = FALSE) +
    ggplot2::labs(x = "Reference position (bp)", y = "Depth") +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())

  if (nrow(features) == 0L) {
    return(cov)
  }
  patchwork::wrap_plots(
    .mtr_view_features(features, rng), cov,
    ncol = 1, heights = c(1, 5)
  )
}

#' Base colours shared by the sequence rows and the mismatch letters.
#' @noRd
MTR_BASE_COLORS <- c(A = "#3aa03a", C = "#2f6fb5", G = "#e0a030",
                     T = "#cc4b4b", N = "#999999")

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
  letters_on <- diff(rng) <= 300
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
    seq_row(cons_seq, "Consensus", -1)
  )

  p <- ggplot2::ggplot() +
    ggplot2::scale_x_continuous(labels = scales::label_comma()) +
    ggplot2::coord_cartesian(
      xlim = c(rng[1] - 0.5, rng[2] + 0.5), expand = FALSE
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
        height = 0.85, alpha = 0.75
      )
    if (letters_on) {
      p <- p + ggplot2::geom_text(
        data = bases,
        ggplot2::aes(x = pos, y = y, label = base),
        size = 2.4, color = "white"
      )
    }
    p <- p + ggplot2::annotate(
      "text", x = rng[1], y = c(0, -1), label = c("Reference", "Consensus"),
      hjust = 0, vjust = -1.2, size = 2.6, color = "#666666"
    )
  }

  if (nrow(win$reads) > 0L) {
    r <- win$reads
    r$y <- -1 - r$row
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
      dl$y <- -1 - dl$row
      p <- p + ggplot2::geom_segment(
        data = dl,
        ggplot2::aes(x = start - 0.5, xend = end + 0.5,
                     y = y, yend = y),
        color = "#555555", linewidth = 0.3
      )
    }
    if (nrow(win$mm) > 0L) {
      mm <- win$mm
      mm$y <- -1 - mm$row
      if (letters_on) {
        p <- p + ggplot2::geom_text(
          data = mm,
          ggplot2::aes(x = pos, y = y, label = base,
                       color = base),
          size = 2.4, fontface = "bold"
        )
      } else {
        p <- p + ggplot2::geom_tile(
          data = mm,
          ggplot2::aes(x = pos, y = y, fill = base),
          height = 0.7
        )
      }
    }
    if (nrow(win$ins) > 0L) {
      ins <- win$ins
      ins$y <- -1 - ins$row
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
