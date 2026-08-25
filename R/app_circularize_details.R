#' Turn a stored gapped alignment into per-column plot data
#'
#' The two copies are stored as gapped strings rather than one row per base, so
#' the split happens here, once, for the window actually being drawn.
#'
#' @param aln_query,aln_subject gapped alignment strings of equal length
#' @param from,to 1-based inclusive column bounds, clamped to the alignment
#'
#' @return data frame with `col`, `base_q`, `base_s`, `match`
#'
#' @noRd
circularize_aln_df <- function(aln_query, aln_subject, from = 1L, to = NULL) {
  empty <- data.frame(
    col = integer(0), base_q = character(0),
    base_s = character(0), match = logical(0)
  )
  if (length(aln_query) != 1L || is.na(aln_query) || !nzchar(aln_query)) {
    return(empty)
  }
  q <- strsplit(aln_query, "")[[1]]
  s <- strsplit(aln_subject, "")[[1]]
  n <- min(length(q), length(s))
  from <- as.integer(from)
  from <- if (length(from) != 1L || is.na(from)) 1L else max(1L, from)
  to <- as.integer(to)
  to <- if (length(to) != 1L || is.na(to)) n else min(n, to)
  if (from > to) {
    return(empty)
  }
  idx <- from:to
  data.frame(
    col = idx,
    base_q = q[idx],
    base_s = s[idx],
    match = q[idx] == s[idx]
  )
}

#' Show the evidence behind a sample's circularization call
#'
#' @param rv assemble module reactiveValues; the evidence is stashed here so the
#'   plot renderers re-run when a different sample's modal is opened
#' @param id sample ID
#' @param session current shiny session
#'
#' @noRd
circularize_details_modal <- function(rv, id, session = getDefaultReactiveDomain()) {
  ns <- session$ns

  ov <- dplyr::tbl(session$userData$con, "circularize_overlap") |>
    dplyr::filter(ID == !!id) |>
    dplyr::collect()

  if (nrow(ov) == 0L) {
    shinyWidgets::show_alert(
      title = "No circularization evidence",
      text = paste0(
        "No end overlap was found for this sample, so there is nothing to ",
        "compare. Samples with a multi-contig assembly, or with the step ",
        "switched off, have no evidence either."
      ),
      type = "info"
    )
    return(invisible(NULL))
  }
  ov <- ov[1, ]

  depth <- dplyr::tbl(session$userData$con, "circularize_depth") |>
    dplyr::filter(ID == !!id) |>
    dplyr::collect()
  # Ignore rows left by an earlier run with a wider window. NA window_bp
  # means no reads were mapped this run, so any depth rows are stale.
  if (nrow(depth) > 0L) {
    if (is.na(ov$window_bp)) {
      depth <- depth[0, ]
    } else {
      depth <- depth[abs(depth$rel_position) <= ov$window_bp, ]
    }
  }

  rv$circ_evidence <- list(overlap = ov, depth = depth)

  aln_len <- if (is.na(ov$aln_query)) 0L else nchar(ov$aln_query)
  outcome <- if (isTRUE(as.logical(ov$accepted))) "overlap accepted" else "overlap not used"

  showModal(modalDialog(
    title = stringr::str_glue("Circularization: {id}"),
    size = "l",
    easyClose = TRUE,
    opts_help(
      "The redundant overlap the search found between the contig ends, and the ",
      "read depth across the junction trimming it produced. Use it to judge ",
      "whether the overlap is a real circular junction or a repeat."
    ),
    tags$p(tags$b(outcome), if (!is.na(ov$reason) && nzchar(ov$reason)) paste0(": ", ov$reason)),
    reactable::reactable(
      data.frame(
        `Aligned length (bp)` = ov$length,
        `Identity (%)` = ov$pident,
        Mismatches = ov$mismatches,
        `Contig start` = paste0(ov$qstart, "-", ov$qend),
        `Contig end` = paste0(ov$sstart, "-", ov$send),
        `Trimmed (bp)` = ov$trimmed,
        `Junction reads` = ifelse(
          is.na(ov$junction_reads), "no reads",
          paste0(ov$junction_reads, " / ", ov$min_junction_reads, " required")
        ),
        check.names = FALSE
      ),
      compact = TRUE, sortable = FALSE, highlight = FALSE
    ),
    tags$hr(),
    tags$b("Where the overlap sits"),
    plotOutput(ns("circ_schematic"), height = "90px"),
    tags$b("Aligned copies"),
    div(
      style = "display: flex; align-items: center; gap: 1em;",
      sliderInput(ns("circ_aln_from"), "Alignment column:",
                  min = 1L, max = max(1L, aln_len), value = 1L,
                  step = 10L, width = "100%")
    ),
    plotOutput(ns("circ_alignment"), height = "120px"),
    if (nrow(depth) > 0L) {
      tagList(
        tags$b("Read depth across the junction"),
        plotOutput(ns("circ_depth"), height = "180px")
      )
    } else {
      tags$p(tags$em(
        "No junction coverage: this project has no raw reads, or the overlap ",
        "was not used, so no reads were mapped."
      ))
    },
    footer = modalButton("Close")
  ))

  invisible(NULL)
}
