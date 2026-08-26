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

#' Base colours for the aligned copies
#'
#' Same palette the assembly details modal uses for its path alignments, so a
#' base is the same colour wherever the app draws sequence.
#'
#' @noRd
CIRC_BASE_COLORS <- c(
  A = "#a2fa8c", C = "#ffd18c", G = "#f38d8a",
  T = "#8ab8f5", U = "#8ab8f5", `-` = "#BBBBBB"
)

#' Caption above a panel, matching the assembly details modal
#'
#' @param ... caption text
#' @param top top margin in px
#'
#' @noRd
circ_caption <- function(..., top = 10) {
  div(
    style = paste0("font-size: 11px; color: #555; margin: ", top, "px 0 4px 0;"),
    ...
  )
}

#' Contig length before and after trimming
#'
#' @param contig_length length of the contig handed to the search
#' @param trimmed bp removed
#'
#' @return single label string
#'
#' @noRd
circ_length_label <- function(contig_length, trimmed) {
  if (length(contig_length) != 1L || is.na(contig_length)) {
    return("unknown")
  }
  trimmed <- if (length(trimmed) != 1L || is.na(trimmed)) 0L else trimmed
  if (trimmed <= 0L) {
    return(format(contig_length, big.mark = ","))
  }
  paste0(format(contig_length, big.mark = ","), " -> ",
         format(contig_length - trimmed, big.mark = ","))
}

#' Render the overlap alignment as scrollable coloured sequence
#'
#' HTML rather than a plot: a 1200 bp overlap drawn as tiles is a raster tens of
#' thousands of pixels wide, while this scrolls natively at any length.
#'
#' @param ov one row of `circularize_overlap`
#'
#' @return a shiny tag
#'
#' @noRd
circularize_aln_html <- function(ov) {
  aln <- circularize_aln_df(ov$aln_query, ov$aln_subject)
  if (nrow(aln) == 0L) {
    return(tags$p(tags$em("No alignment stored for this sample.")))
  }

  blank <- function(x) if (length(x) != 1L || is.na(x)) "" else x
  ql <- blank(ov$q_ctx_left); qr <- blank(ov$q_ctx_right)
  sl <- blank(ov$s_ctx_left); sr <- blank(ov$s_ctx_right)

  # The two copies keep different amounts of flanking sequence, so pad the
  # shorter side to keep the rows in register with each other.
  pad <- function(x, n) paste0(strrep(" ", n - nchar(x)), x)
  rpad <- function(x, n) paste0(x, strrep(" ", n - nchar(x)))
  nl <- max(nchar(ql), nchar(sl))
  nr <- max(nchar(qr), nchar(sr))

  chars <- function(x) if (nchar(x) == 0L) character(0) else strsplit(x, "")[[1]]
  q_all <- c(chars(pad(ql, nl)), aln$base_q, chars(rpad(qr, nr)))
  s_all <- c(chars(pad(sl, nl)), aln$base_s, chars(rpad(sr, nr)))
  is_aln <- c(rep(FALSE, nl), rep(TRUE, nrow(aln)), rep(FALSE, nr))
  matched <- c(rep(NA, nl), aln$match, rep(NA, nr))

  # A mismatched column is what the reader is hunting for, so it gets the loud
  # mark and a matched column gets a faint tick.
  cell <- function(b, aligned, mismatch) {
    if (identical(b, " ")) {
      return(tags$span(class = "mp-circ-b", HTML("&nbsp;")))
    }
    bg <- if (!aligned) "#EEEEEE" else CIRC_BASE_COLORS[[toupper(b)]] %||% "#666666"
    tags$span(
      class = if (mismatch) "mp-circ-b mp-circ-x" else "mp-circ-b",
      style = paste0("background:", bg, ";", if (!aligned) "color:#888888;" else ""),
      b
    )
  }
  bar <- function(m) {
    if (is.na(m)) {
      tags$span(class = "mp-circ-b", HTML("&nbsp;"))
    } else if (m) {
      tags$span(class = "mp-circ-b mp-circ-match", HTML("&nbsp;"))
    } else {
      tags$span(class = "mp-circ-b mp-circ-mm", HTML("&nbsp;"))
    }
  }

  row <- function(label, content) {
    div(class = "mp-circ-row", tags$span(class = "mp-circ-lab", label), content)
  }

  n_mm <- sum(!aln$match)
  tagList(
    tags$style(HTML(paste0(
      ".mp-circ-scroll{overflow-x:auto;white-space:nowrap;border:1px solid #ddd;",
      "border-radius:4px;padding:8px 0;background:#FFF;}",
      ".mp-circ-row{white-space:nowrap;line-height:1.4;}",
      ".mp-circ-lab{display:inline-block;width:70px;font-size:11px;color:#555;",
      "text-align:right;padding-right:8px;position:sticky;left:0;background:#FFF;}",
      ".mp-circ-b{display:inline-block;width:11px;text-align:center;",
      "font-family:\"Courier New\", Courier, monospace;font-size:12px;}",
      ".mp-circ-match{border-top:1px solid #CCC;height:6px;}",
      ".mp-circ-mm{background:#E55330;height:8px;border-radius:1px;}",
      ".mp-circ-x{outline:1px solid #E55330;outline-offset:-1px;font-weight:bold;}"
    ))),
    div(
      class = "mp-circ-scroll",
      row("5' end", lapply(seq_along(q_all), function(i) cell(q_all[i], is_aln[i], isTRUE(!matched[i])))),
      row("", lapply(matched, bar)),
      row("3' end", lapply(seq_along(s_all), function(i) cell(s_all[i], is_aln[i], isTRUE(!matched[i]))))
    ),
    div(
      style = "font-size: 11px; color: #555; margin: 4px 0 0 0;",
      if (n_mm == 0L) {
        "No mismatches: the two copies are identical over the whole overlap."
      } else {
        paste0(n_mm, " mismatched position", if (n_mm == 1L) "" else "s",
               " marked in red.")
      }
    )
  )
}

#' One-line outcome for a stored overlap row
#'
#' @param ov one row of `circularize_overlap`
#'
#' @return single label string
#'
#' @noRd
circ_outcome <- function(ov) {
  if (isTRUE(as.logical(ov$accepted))) "overlap accepted" else "overlap not used"
}

#' Picker choices for a sample's contigs
#'
#' Named so the dropdown reads "contig (outcome)" while the value stays the
#' contig name the evidence tables are keyed on.
#'
#' @param ov_all all `circularize_overlap` rows for one sample
#'
#' @return named character vector
#'
#' @noRd
circ_contig_choices <- function(ov_all) {
  labels <- vapply(
    seq_len(nrow(ov_all)),
    function(i) paste0(ov_all$contig[i], " (", circ_outcome(ov_all[i, ]), ")"),
    character(1)
  )
  stats::setNames(as.character(ov_all$contig), labels)
}

#' Caption saying how much of the sample the picker covers
#'
#' @param n_evidence contigs with a stored overlap row
#' @param n_contigs non-ignored contigs in `assemblies`
#'
#' @return single label string
#'
#' @noRd
circ_coverage_label <- function(n_evidence, n_contigs) {
  if (length(n_contigs) != 1L || is.na(n_contigs) || n_contigs < n_evidence) {
    return(paste0("Evidence for ", n_evidence,
                  ngettext(n_evidence, " contig.", " contigs.")))
  }
  paste0(
    "Evidence for ", n_evidence, " of ", n_contigs, " contigs.",
    if (n_evidence < n_contigs) {
      " The rest had no end overlap to compare, so they are not listed."
    } else {
      ""
    }
  )
}

#' Load one contig's evidence into the module's reactiveValues
#'
#' The panels read `rv$circ_evidence`, so the selection has to land in a
#' reactive store: a non-reactive stash would leave the plots on the previously
#' selected contig.
#'
#' @param rv assemble module reactiveValues
#' @param session current shiny session
#'
#' @noRd
circularize_load_evidence <- function(rv, session = getDefaultReactiveDomain()) {
  ov_all <- rv$circ_overlaps
  contig <- rv$circ_contig
  if (is.null(ov_all) || nrow(ov_all) == 0L || is.null(contig)) {
    rv$circ_evidence <- NULL
    return(invisible(NULL))
  }
  ov <- ov_all[ov_all$contig == contig, , drop = FALSE]
  if (nrow(ov) == 0L) {
    rv$circ_evidence <- NULL
    return(invisible(NULL))
  }
  ov <- ov[1, ]

  # Depth must follow the contig the overlap row belongs to, or a fragmented
  # sample blends every contig into one plot.
  id <- rv$circ_id
  depth <- dplyr::tbl(session$userData$con, "circularize_depth") |>
    dplyr::filter(ID == !!id, contig == !!contig) |>
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
  invisible(NULL)
}

#' Body of the circularization modal, for one contig
#'
#' Separate from the modal so it can be re-rendered when the picker changes
#' without rebuilding the dialog around it.
#'
#' @param ov one row of `circularize_overlap`
#' @param depth that contig's `circularize_depth` rows
#' @param ns module namespace function
#'
#' @return a shiny tag list
#'
#' @noRd
circularize_details_body <- function(ov, depth, ns) {
  tagList(
    tags$p(
      tags$b(circ_outcome(ov)),
      if (!is.na(ov$reason) && nzchar(ov$reason)) paste0(": ", ov$reason)
    ),
    reactable::reactable(
      data.frame(
        `Contig length (bp)` = circ_length_label(ov$contig_length, ov$trimmed),
        `Aligned length (bp)` = ov$length,
        `Identity (%)` = ov$pident,
        Mismatches = ov$mismatches,
        `5' copy` = paste0(ov$qstart, "-", ov$qend),
        `3' copy` = paste0(ov$sstart, "-", ov$send),
        `Trimmed (bp)` = ov$trimmed,
        `Junction reads` = ifelse(
          is.na(ov$junction_reads), "no reads",
          paste0(ov$junction_reads, " / ", ov$min_junction_reads, " required")
        ),
        check.names = FALSE
      ),
      compact = TRUE, sortable = FALSE, highlight = FALSE
    ),
    circ_caption("Where the overlap sits on the contig:", top = 14),
    plotOutput(ns("circ_schematic"), height = "90px"),
    circ_caption(paste(
      "The whole overlap, with up to 50 bp of flanking contig sequence greyed",
      "either side. Scroll sideways to read it:"
    ), top = 14),
    circularize_aln_html(ov),
    if (nrow(depth) > 0L) {
      tagList(
        circ_caption(paste(
          "Assembly depth either side of the seam, the 3' end to the left and",
          "the 5' end to the right, and how much of it comes from reads that",
          "cross the seam. A real junction carries depth straight across:"
        ), top = 14),
        plotOutput(ns("circ_depth"), height = "200px")
      )
    } else {
      circ_caption(paste(
        "No junction coverage: this project has no raw reads, or the overlap",
        "was not used, so no reads were mapped."
      ), top = 14)
    }
  )
}

#' Show the evidence behind a sample's circularization calls
#'
#' Circularization is decided per contig, so the modal pages between the
#' contigs the search found evidence for. It declines only when the sample has
#' no evidence at all; a sample with evidence for some of its contigs shows
#' what it has.
#'
#' @param rv assemble module reactiveValues; the evidence and the current
#'   selection are stashed here so the panels re-run when either changes
#' @param id sample ID
#' @param session current shiny session
#'
#' @noRd
circularize_details_modal <- function(rv, id, session = getDefaultReactiveDomain()) {
  ns <- session$ns

  ov_all <- dplyr::tbl(session$userData$con, "circularize_overlap") |>
    dplyr::filter(ID == !!id) |>
    dplyr::collect() |>
    dplyr::arrange(contig)

  if (nrow(ov_all) == 0L) {
    shinyWidgets::show_alert(
      title = "No circularization evidence",
      text = paste0(
        "No end overlap was found for any contig of this sample, so there is ",
        "nothing to compare. Samples with the step switched off have no ",
        "evidence either."
      ),
      type = "info"
    )
    return(invisible(NULL))
  }

  # Contigs the search found no overlap for have no evidence row at all, so the
  # picker is not a list of the sample's contigs. Say how many of them it covers
  # rather than letting a short list imply a short assembly.
  n_contigs <- dplyr::tbl(session$userData$con, "assemblies") |>
    dplyr::filter(ID == !!id, ignore == 0) |>
    dplyr::count() |>
    dplyr::pull(n)

  rv$circ_id <- id
  rv$circ_overlaps <- ov_all
  rv$circ_contig <- as.character(ov_all$contig[1])
  circularize_load_evidence(rv, session = session)

  showModal(modalDialog(
    title = stringr::str_glue("Circularization: {id}"),
    size = "l",
    easyClose = TRUE,
    opts_help(
      "The redundant overlap the search found between the contig ends, and the ",
      "read depth across the junction trimming it produced. Use it to judge ",
      "whether the overlap is a real circular junction or a repeat."
    ),
    selectInput(
      ns("circ_contig"), "Contig",
      choices = circ_contig_choices(ov_all),
      selected = rv$circ_contig,
      width = "100%"
    ),
    circ_caption(circ_coverage_label(nrow(ov_all), n_contigs), top = 0),
    uiOutput(ns("circ_body")),
    footer = modalButton("Close")
  ))

  invisible(NULL)
}
