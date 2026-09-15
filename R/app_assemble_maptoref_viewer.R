#' Placeholder for the MapToRef sequence viewer
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

#' MapToRef results in the sequence viewer (tools/maptoref_seqview_spec.md)
#'
#' @noRd
maptoref_viewer_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    state <- reactiveValues(
      paths = NULL, depth = NULL, features = NULL, summary = NULL,
      ref_seq = NA_character_, cons_seq = NA_character_, len = 0L,
      ref_now = NA_character_, has_work = FALSE
    )
    version <- reactiveVal(0L)
    reads_note <- reactiveVal(NULL)

    on("coverage_modal", {
      p <- maptoref_paths(
        session$userData$dir_out, rv$updating$ID, rv$updating$assemble_opts
      )
      state$paths <- p
      state$has_work <- dir.exists(p$work)
      state$depth <- maptoref_read_depth(p$depth)
      state$features <- maptoref_read_features(p$features)
      state$summary <- maptoref_read_summary(p$summary)
      state$ref_now <- .mtr_ref_now(session$userData$con, rv$updating$ID)
      state$ref_seq <- maptoref_read_seq(p$ref_fasta)
      state$cons_seq <- maptoref_read_seq(p$consensus)
      # The depth table is padded to the reference length, so this is the
      # payload `len`.
      state$len <- nrow(state$depth)
      reads_note(NULL)
      version(isolate(version()) + 1L)
    })

    has_bam <- reactive(isTRUE(file.exists(state$paths$bam %||% "")))
    cons_mismatch <- reactive(
      !is.na(state$cons_seq) && !is.na(state$ref_seq) &&
        nchar(state$cons_seq) != nchar(state$ref_seq)
    )

    output$view <- renderUI({
      if (!isTRUE(state$has_work)) {
        return(NULL)
      }
      if (state$len == 0L) {
        return(tags$div(
          class = "mp-maptoref",
          tags$b("MapToRef reference coverage"),
          tags$div(
            class = "mp-coverage-caption",
            "No coverage table for this sample. Run Update on it to produce ",
            "the coverage and read files."
          )
        ))
      }
      btn <- function(action, label, icon = NULL, title = NULL) {
        tags$button(
          type = "button", class = "btn btn-default", `data-mpseq` = action,
          title = title, `aria-label` = title %||% label, icon, label
        )
      }
      goto <- numericInput(ns("goto"), NULL, value = NA, min = 1, step = 1, width = "130px")
      goto <- htmltools::tagQuery(goto)$find("input")$
        addAttrs(placeholder = "Go to position", `aria-label` = "Go to position")$allTags()
      reads_box <- mp_checkbox(ns("show_reads"), label = "Reads", value = TRUE)
      if (!has_bam()) reads_box <- shinyjs::disabled(reads_box)
      tags$div(
        class = "mp-maptoref",
        tags$b("MapToRef reference coverage"),
        uiOutput(ns("header")),
        div(
          class = "mp-seqview-controls",
          goto,
          btn("whole", "Whole genome"),
          btn("zoom_in", NULL, icon("magnifying-glass-plus"), "Zoom in"),
          btn("zoom_out", NULL, icon("magnifying-glass-minus"), "Zoom out"),
          mp_checkbox(ns("show_cov"), label = "Coverage", value = TRUE),
          mp_checkbox(ns("show_nt"), label = "Nucleotides", value = TRUE),
          reads_box
        ),
        div(class = "mp-coverage-caption",
            paste("Drag or scroll sideways to pan, scroll or pinch to zoom.",
                  "Letters appear when zoomed in; reads appear under 1,000 bp.")),
        uiOutput(ns("note")),
        div(
          class = "mp-seqview",
          tags$canvas(id = ns("canvas"), class = "mp-seqview-canvas"),
          tags$div(id = ns("tip"), class = "mp-maptoref-tip", hidden = NA)
        )
      )
    })

    output$header <- renderUI({
      s <- state$summary
      # Older runs never wrote every key (e.g. reference_source); omit the
      # field rather than print a "not recorded" placeholder for it.
      fld <- function(k) {
        v <- unname(s[k])
        if (is.null(v) || is.na(v)) NA_character_ else v
      }
      ref_len <- fld("reference_length")
      n_pct <- suppressWarnings(
        round(100 * as.numeric(fld("n_count")) / as.numeric(ref_len), 1)
      )
      item <- function(label, value) {
        if (is.null(value) || is.na(value)) return(NULL)
        tags$span(class = "mp-maptoref-field", tags$b(label), " ", value)
      }
      tags$div(
        class = "mp-maptoref-meta",
        item("Reference:", fld("accession")),
        item("Organism:", fld("organism")),
        item("Length:", if (!is.na(ref_len)) paste0(ref_len, " bp")),
        item("Source:", fld("reference_source")),
        item("Reads mapped:", fld("reads_mapped_final")),
        item("Mean depth:", round(mean(state$depth$Depth), 1)),
        item("Uncalled bases:", if (!is.na(n_pct)) paste0(n_pct, "%")),
        # The run records the reference it was given; a different value on the
        # sample now means this assembly is behind its settings.
        if (!is.na(fld("reference")) && !is.na(state$ref_now) &&
            .mtr_ref_key(fld("reference")) != .mtr_ref_key(state$ref_now)) {
          tags$span(class = "mp-maptoref-field mp-maptoref-stale",
                    tags$b("Reference changed since this assembly:"),
                    paste0(" now ", state$ref_now, ". Run Update on the sample to re-map."))
        },
        if (nrow(state$features) == 0L) {
          tags$span(class = "mp-maptoref-field mp-maptoref-nofeat",
                    "Reference has no annotation record.")
        },
        if (cons_mismatch()) {
          tags$span(class = "mp-maptoref-field mp-maptoref-nofeat",
                    "Consensus row: not shown (length differs from the reference).")
        }
      )
    })

    # The canvas lives inside output$view, so the payload goes out after the
    # flush that inserts it.
    observe({
      req(isTRUE(state$has_work), state$len > 0L)
      p <- maptoref_seqview_payload(
        state$depth, state$features, state$ref_seq, state$cons_seq,
        unname(state$summary["reference_topology"]), isolate(rv$updating$ID), version()
      )
      p$id <- ns("canvas")
      if (has_bam()) p$readsInput <- ns("reads_req")
      session$onFlushed(function() session$sendCustomMessage("mpseq", p), once = TRUE)
    })

    observeEvent(input$reads_req, {
      r <- input$reads_req
      req(state$len > 0L, has_bam())
      start <- as.integer(r$start)
      end <- as.integer(r$end)
      req(is.finite(start), is.finite(end), start <= end)
      len <- state$len
      rd <- function(a, b) maptoref_window_reads(state$paths$bam, a, b, state$ref_seq)
      circular <- identical(unname(state$summary["reference_topology"]), "circular")
      if (circular && start < 1L) {
        w <- maptoref_merge_reads(rd(len + start, len), rd(1L, end))
      } else if (circular && end > len) {
        w <- maptoref_merge_reads(rd(start, len), rd(1L, end - len))
      } else {
        start <- max(1L, start)
        end <- min(len, end)
        w <- rd(start, end)
      }
      reply <- maptoref_reads_reply(w, start, end, r$nonce)
      reply$id <- ns("canvas")
      session$sendCustomMessage("mpseq_reads", reply)
      rng <- paste0(format(start, big.mark = ","), "-", format(end, big.mark = ","))
      reads_note(if (w$n_total == 0L) {
        paste0("No reads in ", rng, ".")
      } else {
        paste0(
          "Showing ", format(w$n_shown, big.mark = ","), " of ",
          format(w$n_total, big.mark = ","), " reads in ", rng, ".",
          if (w$n_shown < w$n_total) " The deepest 100 rows are shown."
        )
      })
    })

    output$note <- renderUI({
      if (!has_bam()) {
        return(div(class = "mp-coverage-caption",
                   "No read alignments were kept for this sample. Run Update on it to keep them."))
      }
      n <- reads_note()
      if (is.null(n)) NULL else div(class = "mp-coverage-caption", n)
    })
    # The viewer hides this note whenever it stops drawing reads, so it must
    # keep rendering while hidden.
    outputOptions(output, "note", suspendWhenHidden = FALSE)
  })
}
