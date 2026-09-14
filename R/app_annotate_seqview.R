# R/app_annotate_seqview.R

#' Sequence viewer payload (tools/nt_viewer_spec.md, section 4)
#'
#' Pure: no Shiny, no database. Soft-deleted rows (pos1 == 0) are dropped;
#' the browser owns all coordinate arithmetic, so positions pass through as
#' stored (1-based, inclusive, pos1 > pos2 for an origin-crossing feature).
#' @noRd
seqview_payload <- function(annotations, seq, topology, unit,
                            selected = NULL, version = 0L, coverage = NULL) {
  seq <- toupper(as.character(seq)[1])
  a <- annotations
  yes <- function(x) {
    x <- tolower(as.character(x))
    length(x) == 1L && !is.na(x) && x %in% c("yes", "true", "1")
  }
  rows <- which(!is.na(a$pos1) & a$pos1 > 0 & !is.na(a$pos2))
  feats <- lapply(rows, function(i) {
    notes <- as.character(a$notes[i])
    if (is.na(notes)) notes <- ""
    joined <- regmatches(notes, regexpr("JOIN: mode=[A-Za-z]+ group=[0-9]+", notes))
    tr <- as.character(a$translation[i])
    f <- list(
      row = i,
      type = as.character(a$type[i]),
      gene = as.character(a$gene[i]),
      pos1 = as.integer(a$pos1[i]),
      pos2 = as.integer(a$pos2[i]),
      dir = as.character(a$direction[i]),
      partial5 = yes(a$partial_start[i]),
      partial3 = yes(a$partial_stop[i]),
      notes = substr(notes, 1L, 80L)
    )
    if (identical(f$type, "PCG") && !is.na(tr) && nzchar(tr)) f$translation <- tr
    if (length(joined) == 1L) f$joined <- joined
    f
  })
  sel <- if (length(selected) == 1L && !is.na(selected) && selected %in% rows) {
    as.integer(selected)
  }
  out <- list(
    unit = unit, len = nchar(seq), topology = topology, seq = seq,
    version = as.integer(version), selected = sel, features = feats
  )
  # Per-base read depth and error rate, indexed by position; NA -> null.
  if (is.data.frame(coverage) && nrow(coverage) > 0 && nchar(seq) > 0) {
    i <- match(seq_len(nchar(seq)), coverage$Position)
    out$depth <- as.integer(coverage$Depth[i])
    out$err <- round(as.numeric(coverage$ErrorRate[i]), 4)
  }
  out
}

#' Sequence viewer section (tools/nt_viewer_spec.md, section 3)
#' @noRd
seqview_ui <- function(id) {
  ns <- NS(id)
  btn <- function(action, label, icon = NULL, title = NULL) {
    tags$button(
      type = "button", class = "btn btn-default", `data-mpseq` = action,
      title = title, `aria-label` = title %||% label, icon, label
    )
  }
  tags$details(
    id = ns("section"),
    tags$summary("Sequence"),
    div(
      class = "mp-seqview-controls",
      mp_checkbox(ns("show_cov"), label = "Coverage", value = TRUE),
      mp_checkbox(ns("show_err"), label = "Error rate", value = TRUE),
      mp_checkbox(ns("show_nt"), label = "Nucleotides", value = TRUE),
      mp_checkbox(ns("show_aa"), label = "Amino acids", value = TRUE),
      numericInput(ns("goto"), "Position:", value = NA, min = 1, step = 1, width = "130px"),
      btn("fit", "Fit gene"),
      btn("whole", "Whole genome"),
      btn("zoom_in", NULL, icon("magnifying-glass-plus"), "Zoom in"),
      btn("zoom_out", NULL, icon("magnifying-glass-minus"), "Zoom out")
    ),
    div(class = "mp-coverage-caption",
        paste("Drag or scroll sideways to pan, scroll or pinch to zoom; click a gene to select its row.",
              "Letters appear when zoomed in. Error rate bars turn red above 5%.")),
    uiOutput(ns("empty")),
    div(
      class = "mp-seqview",
      tags$canvas(id = ns("canvas"), class = "mp-seqview-canvas"),
      tags$div(id = ns("tip"), class = "mp-maptoref-tip", hidden = NA)
    )
  )
}

#' @param rv the annotate module's reactive values (annotations, updating, editing)
#' @param tick reactiveVal bumped when the assembly sequence changes
#' @param selected reactive of the annotation table's selected row indices
#' @noRd
seqview_server <- function(id, rv, tick, selected) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Re-read when the window (re)opens or the sequence is rewritten; version
    # bumps only when the string differs.
    unit_seq <- reactive({
      tick()
      gargoyle::watch("annotations_modal")
      s <- rv$editing$assembly
      if (is.null(s)) {
        s <- tryCatch(
          get_assembly(rv$updating$ID, rv$updating$path, rv$updating$scaffold,
                       session$userData$con),
          error = function(e) NULL
        )
      }
      if (is.null(s) || length(s) == 0L) return(NULL)
      as.character(s[[1]])
    })
    version <- reactiveVal(0L)
    last_seq <- NULL
    observeEvent(unit_seq(), {
      s <- unit_seq()
      if (!identical(s, last_seq)) {
        last_seq <<- s
        version(isolate(version()) + 1L)
      }
    }, ignoreNULL = FALSE)

    # A selection counts only if the table reported it since the last open:
    # the reactable input persists while the window is closed.
    open_n <- reactiveVal(0L)
    sel_n <- reactiveVal(-1L)
    observeEvent(gargoyle::watch("annotations_modal"), open_n(open_n() + 1L))
    observeEvent(selected(), sel_n(isolate(open_n())), ignoreNULL = FALSE)
    cur_sel <- reactive({
      s <- selected()
      if (sel_n() == open_n() && length(s) == 1L) s else NULL
    })

    observe({
      req(rv$annotations)
      s <- unit_seq()
      if (is.null(s)) {
        output$empty <- renderUI(div(class = "mp-coverage-caption",
                                     "No sequence stored for this assembly."))
        return()
      }
      output$empty <- renderUI(NULL)
      p <- seqview_payload(
        rv$annotations, s, rv$updating$topology %||% "linear",
        paste(rv$updating$ID, rv$updating$path, rv$updating$scaffold, sep = "."),
        selected = cur_sel(), version = version(), coverage = rv$coverage
      )
      p$id <- ns("canvas")
      p$input <- ns("pick")
      session$sendCustomMessage("mpseq", p)
    })

    observeEvent(cur_sel(), {
      session$sendCustomMessage("mpseq_select", list(id = ns("canvas"), row = cur_sel()))
    })

    list(pick = reactive(input$pick))
  })
}
