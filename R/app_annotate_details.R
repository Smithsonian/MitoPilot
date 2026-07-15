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
  tRNA = "#F17CB0",
  ORF  = "#4D4D4D"
)
gene_type_alpha <- 0.5

# Split gene rows that wrap the x-axis boundary into two arrow segments.
#
# A feature that crosses the origin of a circular sequence has its start beyond
# its end, so after mapping to plot coordinates `xmin > xmax`. gggenes can't draw
# such a feature as one arrow, so split it into `[xmin, x_hi]` and `[x_lo, xmax]`
# (the two arcs on either side of the boundary). The gene label is kept on the
# longer piece only to avoid a duplicated label. `df` must already carry numeric
# `xmin`/`xmax` columns (in the same coordinate space as `x_lo`/`x_hi`).
split_wrapped_genes <- function(df, x_lo, x_hi) {
  if (nrow(df) == 0 || !all(c("xmin", "xmax") %in% names(df)) ||
      !any(df$xmin > df$xmax, na.rm = TRUE)) {
    return(df)
  }
  pieces <- lapply(seq_len(nrow(df)), function(i) {
    row <- df[i, , drop = FALSE]
    if (isTRUE(row$xmin > row$xmax)) {
      seg_hi <- row
      seg_hi$xmax <- x_hi # [xmin, x_hi]
      seg_lo <- row
      seg_lo$xmin <- x_lo # [x_lo, xmax]
      # keep the label on the longer arc only
      if ((x_hi - row$xmin) >= (row$xmax - x_lo)) {
        seg_lo$gene <- ""
      } else {
        seg_hi$gene <- ""
      }
      dplyr::bind_rows(seg_hi, seg_lo)
    } else {
      row
    }
  })
  dplyr::bind_rows(pieces)
}

# Build an absolutely-positioned HTML overlay of gene-name labels for a static
# plot image. Each gene becomes a block at its pixel x-range; the label inside
# uses CSS `position: sticky; left: 0` so it stays pinned at the scroll
# container's left edge while the block is in view, then hands off to the next
# gene as blocks scroll past. `direction` ("+"/"-") prefixes a ">"/"<" marker.
# x_lo/x_hi are the plot's x-scale limits; img_w is the image width in px.
# `inset` is the fixed pixel gap between the image edge and the plot panel
# (theme/patchwork margins), so labels track the panel rather than the image.
gene_label_overlay <- function(df, img_w, x_lo, x_hi, track_top, track_height,
                               scale_y = 2, scale_x = 1.5, inset = 0,
                               arrow_w = 0.3) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  df <- df[!is.na(df$gene) & nzchar(df$gene), , drop = FALSE]
  if (nrow(df) == 0) return(NULL)
  span_px <- (img_w - 2 * inset) / (x_hi - x_lo)
  blocks <- lapply(seq_len(nrow(df)), function(i) {
    r <- df[i, , drop = FALSE]
    left  <- inset + (min(r$xmin, r$xmax) - x_lo) * span_px
    width <- abs(r$xmax - r$xmin) * span_px
    fwd <- identical(as.character(r$direction), "+")
    # Directional arrow drawn as a CSS border-triangle, not a text glyph: it is
    # pure geometry, so it occupies the exact block height, stays narrow, and
    # cannot be clipped the way a stretched glyph's ink is. The point faces the
    # gene name (+ points right, - points left).
    mk_w <- max(4L, round(track_height * arrow_w))
    arrow_border <- if (fwd) {
      sprintf("border-left:%dpx solid #808080;", mk_w)
    } else {
      sprintf("border-right:%dpx solid #808080;", mk_w)
    }
    marker_span <- htmltools::div(
      style = sprintf(
        paste0("flex:none; width:0; height:0; opacity:0.5; ",
               "border-top:%.1fpx solid transparent; ",
               "border-bottom:%.1fpx solid transparent; %s"),
        track_height / 2, track_height / 2, arrow_border
      )
    )
    # scale_x grows the name's visual width past its (unscaled) layout box toward
    # the arrow; reserve that overflow as margin on the arrow-facing side so the
    # name and arrow don't overlap (~6 px per char at 10 px font, less the gap).
    name_overflow <- max(0, (scale_x - 1) * nchar(r$gene) * 6 - 4)
    name_margin <- if (fwd) {
      sprintf(" margin-right:%.0fpx;", name_overflow)
    } else {
      sprintf(" margin-left:%.0fpx;", name_overflow)
    }
    name_span <- htmltools::tags$span(
      # Gene name, size unchanged (scale_x/scale_y only).
      style = sprintf(paste0(
        "white-space:nowrap; font-size:10px; line-height:1; color:#000; ",
        "padding:0 2px; transform:scale(%s,%s); transform-origin:%s center;%s"),
        scale_x, scale_y, if (fwd) "left" else "right", name_margin
      ),
      r$gene
    )
    # + strand: name then arrow, left-justified, pinned to the viewport's left
    # edge. - strand: arrow then name, right-justified, pinned to the right edge.
    inner <- if (fwd) list(name_span, marker_span) else list(marker_span, name_span)
    # Inset a few px so a pinned label is not flush against the scroll
    # container's clip edge (which would shave the arrow ink).
    sticky_style <- sprintf(
      "position:sticky; %s:3px; z-index:1; display:flex; align-items:center; height:100%%; gap:4px;",
      if (fwd) "left" else "right"
    )
    htmltools::div(
      # No overflow:hidden here: an overflow-clipping ancestor would become the
      # sticky label's scroll context and pin it to this (non-scrolling) block
      # instead of the scroll viewport. Labels may overlap a neighbour, but stay
      # visible for any annotation on screen.
      style = sprintf(
        paste0("position:absolute; left:%.2fpx; width:%.2fpx; top:%.0fpx; ",
               "height:%.0fpx; display:flex; align-items:center; ",
               "justify-content:%s; pointer-events:none;"),
        left, width, track_top, track_height, if (fwd) "flex-start" else "flex-end"
      ),
      htmltools::div(style = sticky_style, inner)
    )
  })
  htmltools::div(
    style = sprintf(
      "position:absolute; top:0; left:0; width:%.0fpx; height:100%%; pointer-events:none;",
      img_w
    ),
    blocks
  )
}

annotations_details_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Skips the count recompute on the initial annotation load (validated counts
    # already in the db); set TRUE right before each load, see update_counts().
    skip_count_update <- FALSE

    # Persist rv$annotations to the DB. Delete is scoped to the exact
    # (ID, path, scaffold) units currently loaded (never the whole ID) so
    # sibling annotation units of a multi-assembly sample are never wiped.
    persist_annotations <- function() {
      units <- dplyr::distinct(rv$annotations[, c("ID", "path", "scaffold")])
      dplyr::tbl(session$userData$con, "annotations") |>
        dplyr::rows_delete(
          units,
          by = c("ID", "path", "scaffold"),
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
    }

    # Write the given columns of rv$updating to this unit's annotate row. Keyed on
    # (ID, path, scaffold) so a multi-assembly sample's sibling units are never
    # touched (a by="ID" write would clobber every unit).
    update_annotate_unit <- function(cols) {
      dplyr::tbl(session$userData$con, "annotate") |>
        dplyr::rows_update(
          rv$updating[, c("ID", "path", "scaffold", cols)],
          by = c("ID", "path", "scaffold"),
          unmatched = "ignore",
          copy = TRUE,
          in_place = TRUE
        )
    }

    # Outlier-review flag for the sample we jumped in to fix (NULL otherwise),
    # used to render a reminder banner of what to edit.
    outlier_flag <- reactiveVal(NULL)
    output$outlier_flag_banner <- renderUI({
      info <- outlier_flag()
      if (is.null(info)) return(NULL)
      div(
        style = paste(
          "background:#fff3cd; border:1px solid #ffe69c; color:#664d03;",
          "border-radius:4px; padding:8px 12px; margin-bottom:8px;",
          "display:flex; align-items:center; gap:8px;"
        ),
        icon("triangle-exclamation"),
        span(
          tags$b(stringr::str_glue("Outlier review - {toupper(info$gene)}: ")),
          info$issue,
          tags$span(
            style = "color:#8a6d3b; margin-left:6px;",
            stringr::str_glue(
              "(start {sprintf('%+d', info$start_offset)} aa, ",
              "stop {sprintf('%+d', info$stop_offset)} aa, ",
              "identity {info$pct_identity}%)"
            )
          ),
          tags$span(
            style = "margin-left:6px;",
            "- only this gene is editable in review mode; adjust its start/stop ",
            "position, then use 'Back to Review'."
          )
        )
      )
    })

    # Active synteny-plot reference accession (user pick, else top hit).
    active_ref_acc <- reactiveVal(NULL)

    # Load reference annotations / sequence / alignment for one accession into rv.
    # Used on modal open and whenever the user switches the reference in the picker.
    load_blast_ref <- function(acc) {
      if (is.null(acc) || is.na(acc) || !nzchar(acc) || acc == "NO HIT") {
        rv$blast_ref     <- NULL
        rv$blast_ref_seq <- NULL
        rv$blast_ref_aln <- NULL
        return(invisible())
      }
      rv$blast_ref <- tryCatch(
        dplyr::tbl(session$userData$con, "blast_ref_annotations") |>
          dplyr::filter(accession == !!acc) |>
          dplyr::collect(),
        error = function(e) NULL
      )
      rv$blast_ref_seq <- tryCatch(
        dplyr::tbl(session$userData$con, "blast_ref_sequences") |>
          dplyr::filter(accession == !!acc) |>
          dplyr::collect(),
        error = function(e) NULL
      )
      rv$blast_ref_aln <- tryCatch(
        dplyr::tbl(session$userData$con, "blast_ref_alignment") |>
          dplyr::filter(ID == !!rv$updating$ID,
                        path == !!rv$updating$path,
                        scaffold == !!rv$updating$scaffold,
                        accession == !!acc) |>
          dplyr::collect(),
        error = function(e) NULL
      )
    }

    # Prepare modal data ----
    init("annotations_modal")
    on("annotations_modal", {
      req(rv$updating$topology != "fragmented") # TODO! modify to handle fragmented assemblies
      rv$align_refSeq <- TRUE

      ## Per-sample genetic code ----
      # Genetic code auto-selects from this sample's curation ruleset and is
      # cached in samples.genetic_code. Resolve it here so codon-edit
      # translations use the right table even when samples in one project carry
      # different codes. Falls back to the project-level cached code.
      rv$gcode <- tryCatch({
        gc <- dplyr::tbl(session$userData$con, "samples") |>
          dplyr::filter(ID == !!rv$updating$ID) |>
          dplyr::pull(genetic_code)
        if (length(gc) == 1 && !is.na(gc)) {
          Biostrings::getGeneticCode(as.character(as.integer(gc)))
        } else {
          session$userData$gcode
        }
      }, error = function(e) session$userData$gcode)

      ## Load annotations ----
      skip_count_update <<- TRUE  # this load carries validated counts; don't recompute
      rv$annotations <- dplyr::tbl(session$userData$con, "annotations") |>
        dplyr::filter(ID == !!rv$updating$ID,
                      path == !!rv$updating$path,
                      scaffold == !!rv$updating$scaffold) |>
        dplyr::arrange(pos1) |>
        dplyr::collect() |>
        dplyr::mutate(
          fas = "nt",
          faa = dplyr::case_when(
            type == "PCG" ~ "aa",
            .default = NA_character_
          )
        )
      # Coerce manual partial flags (NULL -> 0). Columns are absent on old
      # projects until backwards_compatibility() runs; leave them absent so the
      # save path (which re-inserts every column) matches the DB schema and the
      # partial UI stays hidden.
      if ("partial_start" %in% names(rv$annotations)) {
        rv$annotations$partial_start <- tidyr::replace_na(as.integer(rv$annotations$partial_start), 0L)
      }
      if ("partial_stop" %in% names(rv$annotations)) {
        rv$annotations$partial_stop <- tidyr::replace_na(as.integer(rv$annotations$partial_stop), 0L)
      }

      ## Load coverage ----
      # TODO - get from db (need to fix NA="" issue)
      # Coverage stats are written per unit (ID_coverageStats_<path>_<scaffold>.csv).
      rv$coverage <- local({
        dir <- file.path(session$userData$dir_out, rv$updating$ID, "annotate")
        f <- file.path(dir, paste0(rv$updating$ID, "_coverageStats_",
                                   rv$updating$path, "_", rv$updating$scaffold, ".csv"))
        if (!file.exists(f)) {
          # Fall back to whatever coverageStats file exists (legacy naming).
          alt <- list.files(dir, pattern = "coverageStats", full.names = TRUE)
          f <- if (length(alt) > 0) alt[1] else f
        }
        read.csv(f)
      })

      ## Load BLAST candidate references (rank-ordered; rank 1 = top hit) ----
      # Candidates are stored per (ID, path, scaffold) - never merged. The
      # annotation reference is inherited from the single scaffold the user kept
      # when finalizing the assembly: the scaffold whose hits include the sample's
      # current reference (blast_accession, which a user "Set as reference" may
      # have overwritten from the rank-1 default). Fall back to the best-scoring
      # scaffold if no match, so a divergent scaffold's candidate list can't leak
      # into another scaffold's reference pick.
      rv$blast_ref_candidates <- tryCatch({
        all_cand <- dplyr::tbl(session$userData$con, "blast_ref_candidates") |>
          dplyr::filter(ID == !!rv$updating$ID) |>
          dplyr::collect()
        if (nrow(all_cand) == 0) {
          NULL
        } else {
          # Prefer this unit's own (path, scaffold) candidate list (multi-assembly:
          # each unit shows its own scaffold's hits, not the sample-level ref's).
          src <- all_cand[all_cand$path == rv$updating$path &
                            all_cand$scaffold == rv$updating$scaffold,
                          c("path", "scaffold"), drop = FALSE]
          # NULL-safe: blast_accession may be absent from rv$updating, and %|NA|%
          # errors on a NULL (is.na(NULL) is length 0). Coalesce to "".
          ref_acc <- (rv$updating[["blast_accession"]] %||% NA) %|NA|% ""
          if (nrow(src) == 0) {
            src <- all_cand[!is.na(all_cand$accession) & all_cand$accession == ref_acc,
                            c("path", "scaffold"), drop = FALSE]
          }
          if (nrow(src) == 0) {
            # No match: pick the best-scoring scaffold's list (its rank-1 score).
            r1 <- all_cand[all_cand$rank == 1, , drop = FALSE]
            sc <- ifelse(is.na(r1$pident), 0, r1$pident) * ifelse(is.na(r1$qcovs), 0, r1$qcovs)
            src <- r1[which.max(sc), c("path", "scaffold"), drop = FALSE]
          }
          all_cand[all_cand$path == src$path[1] & all_cand$scaffold == src$scaffold[1], ] |>
            dplyr::arrange(rank)
        }
      }, error = function(e) NULL)

      ## Active synteny reference, resolved per unit: this unit's "Set as best
      ## reference" override, else THIS scaffold's own BLAST top hit, else none.
      ## Shared with the Annotate/Export tables and the export note so every surface
      ## names the same reference.
      acc0 <- resolve_unit_blast_ref(
        session$userData$con,
        rv$updating$ID, rv$updating$path, rv$updating$scaffold
      )
      active_ref_acc(acc0)
      load_blast_ref(acc0)

      annotate_details_modal(rv) |> showModal()
      render_annotations_table(Sys.time())

      # Cross-tab jump from the export outlier review: auto-select the flagged
      # gene's row so the START/STOP editor opens directly on it. Delay lets the
      # reactable widget render before we set its selection.
      goto <- session$userData$goto_annotate_target
      if (!is.null(goto) && !is.null(goto$gene) &&
          identical(goto$ID, rv$updating$ID)) {
        # Remember the flag so the banner can remind the user what to edit
        outlier_flag(goto)
        gidx <- which(rv$annotations$gene == goto$gene)
        if (length(gidx) > 0) {
          gidx <- gidx[[1]]
          tbl_id <- session$ns("table")
          shinyjs::delay(500, {
            reactable::updateReactable("table", selected = gidx, session = session)
            # Scroll the (single-page) table so the selected gene is visible
            shinyjs::runjs(sprintf(
              "var t=document.getElementById('%s'); if(t){var r=t.querySelectorAll('.rt-tbody .rt-tr'); if(r[%d]){r[%d].scrollIntoView({block:'center', behavior:'smooth'});}}",
              tbl_id, gidx - 1L, gidx - 1L
            ))
          })
        }
        session$userData$goto_annotate_target <- NULL
      } else {
        outlier_flag(NULL)
      }
    })

    # Compact status pill renderer. `state` is one of "yes" / "no" / NA;
    # `invert = TRUE` flips the color mapping so "yes" reads as warning.
    # neutral_no: render a "no" value with the neutral (grey) styling rather than
    # a coloured one. Used for the Partial badge, where "no" means a complete
    # assembly and a green "good" colour is misleading.
    status_badge <- function(label, state, invert = FALSE, neutral_no = FALSE) {
      val <- if (is.na(state)) "na" else as.character(state)
      # Colour decision separate from the displayed text so "no" can read NO but
      # render neutral.
      color_val <- if (neutral_no && val == "no") "na" else val
      bg <- if (color_val == "yes") {
        if (invert) "#fde8d0" else "#d4edda"
      } else if (color_val == "no") {
        if (invert) "#d4edda" else "#fde8d0"
      } else {
        "#e9ecef"
      }
      fg <- if (color_val == "yes") {
        if (invert) "#7d4a1e" else "#2d6a4f"
      } else if (color_val == "no") {
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
        status_badge("Problematic", rv$updating$problematic, invert = TRUE),
        status_badge("Partial Mito",     rv$updating$partial, invert = TRUE, neutral_no = TRUE)
      )
    })

    # Footer toggle buttons: clicking still drives the same input$ID_verified /
    # input$reviewed / input$problematic observers below; visual state reflects
    # the current value so the user sees what each click will flip.
    # neutral_no: style a "no" value as the neutral default button rather than a
    # coloured one (for Partial, where "no" = complete and green is misleading).
    toggle_btn <- function(id, label, state, invert = FALSE, neutral_no = FALSE) {
      val <- if (is.na(state)) "na" else as.character(state)
      cls_val <- if (neutral_no && val == "no") "na" else val
      cls <- if (cls_val == "yes") {
        if (invert) "btn btn-warning" else "btn btn-success"
      } else if (cls_val == "no") {
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

    # HTML label summarizing the manual partial flags for annotation row `idx`.
    partial_label <- function(idx) {
      if (!all(c("partial_start", "partial_stop") %in% names(rv$annotations))) return("")
      tags <- c(
        if (isTRUE(as.integer(rv$annotations$partial_start[idx]) == 1L)) "5' partial",
        if (isTRUE(as.integer(rv$annotations$partial_stop[idx]) == 1L)) "3' partial"
      )
      if (length(tags) > 0) paste0("<b>Partial:</b> ", paste(tags, collapse = ", ")) else ""
    }
    output$status_toggles <- shiny::renderUI({
      tagList(
        toggle_btn(ns("ID_verified"), "ID verified", rv$updating$ID_verified),
        toggle_btn(ns("reviewed"),    "Reviewed",    rv$updating$reviewed),
        toggle_btn(ns("problematic"), "Problematic", rv$updating$problematic,
                   invert = TRUE),
        toggle_btn(ns("partial"),     "Partial",     rv$updating$partial,
                   invert = TRUE, neutral_no = TRUE)
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
              name = "Type",
              align = "left",
              html = TRUE,
              # JS (not R) cell renderer so the badge re-renders client-side on
              # updateReactable() after an edit; an R cell function is only run
              # at full render, leaving stale badges when rows are re-sorted.
              cell = htmlwidgets::JS("
                function(cellInfo) {
                  var colors = {ctrl:'#FAA34A', PCG:'#60BD68', rRNA:'#5DA5DA', tRNA:'#F17CB0', ORF:'#4D4D4D'};
                  var v = cellInfo.value || '';
                  var c = colors[v] || '#888888';
                  var badge = '<span style=\"background:' + c + '30;color:#111111;border:1px solid ' + c +
                    ';border-radius:3px;padding:1px 4px;font-size:11px;white-space:nowrap;\">' + v + '</span>';
                  // Non-standard MitoFinder gene: PCG from MitoFinder with no
                  // canonical product. Flag so the user knows it can be renamed.
                  var row = cellInfo.row || {};
                  var noProduct = (row.product == null || row.product === '' || row.product === 'NA');
                  if (v === 'PCG' && row.tool === 'MitoFinder' && noProduct) {
                    badge += ' <span style=\"background:#FAA34A30;color:#111111;border:1px solid #FAA34A;' +
                      'border-radius:3px;padding:1px 4px;font-size:10px;white-space:nowrap;\">non-std</span>';
                  }
                  return badge;
                }
              ")
            ),
            gene = colDef(show = T,
                          name = "Gene",
                          align = "left",
                          maxWidth = 300,
                          resizable = TRUE,
                          html = T,
                          cell = rt_longtext()),
            pos1 = colDef(show = T, name = "Start"),
            pos2 = colDef(show = T, name = "End"),
            length = colDef(show = T, name = "Length"),
            direction = colDef(show = T, name = "Direction"),
            partial_start = colDef(
              show = T,
              name = "Partial",
              html = T,
              align = "center",
              maxWidth = 90,
              # JS cell (re-renders on updateReactable) reading the stored 5'/3'
              # partial flags (partial_start/partial_stop are 5'/3' in the gene's
              # orientation) so partiality is visible without entering edit mode.
              cell = htmlwidgets::JS("
                function(cellInfo) {
                  var row = cellInfo.row || {};
                  function pill(t) {
                    return '<span style=\"background:#FAA34A30;color:#111111;border:1px solid #FAA34A;' +
                      'border-radius:3px;padding:1px 4px;font-size:11px;white-space:nowrap;\">' + t + '</span>';
                  }
                  var out = [];
                  if (row.partial_start == 1) out.push(pill(\"5'\"));
                  if (row.partial_stop == 1) out.push(pill(\"3'\"));
                  return out.join(' ');
                }
              ")
            ),
            tool = colDef(
              show = T,
              name = "Tool",
              align = "left",
              maxWidth = 100
            ),
            notes = colDef(
              show = T,
              name = "Notes",
              maxWidth = 1000,
              html = T,
              cell = rt_longtext(),
              align = "left",
              resizable = TRUE
            ),
            warnings = colDef(
              show = T,
              name = "Warnings",
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

    # Holds a pending join (rows + mode) awaiting confirmation when warnings apply.
    pending_join <- reactiveVal(NULL)

    # Synteny-overview click anchor: alignment column to center zoom on.
    # Set by clicking the overview plot; cleared when user picks a new gene row.
    zoom_click_col <- reactiveVal(NULL)
    # Previous selection, so a switch between segments of the same joined gene can
    # keep the alignment view open instead of collapsing it.
    last_sel <- reactiveVal(NULL)
    selected <- reactive({
      sel <- reactable::getReactableState("table", "selected")
      # Check for unsaved edits
      isolate({
        req(rv$annotations)
        shinyjs::toggle("aln_div", condition = length(sel) > 0 && rv$annotations$type[sel] %in% c("PCG", "ORF", "rRNA"))
        is_deleted <- length(sel) > 0 && stringr::str_detect(rv$annotations$gene[sel], "_DELETED_")
        is_orf <- length(sel) > 0 && rv$annotations$type[sel] == "ORF"
        # An assigned ORF keeps tool == "ORFfinder" but a non-ORF type; offer the
        # button for both so the assignment can be edited or removed.
        is_assigned_orf <- length(sel) > 0 &&
          isTRUE(rv$annotations$tool[sel] == "ORFfinder") &&
          isTRUE(rv$annotations$type[sel] != "ORF")
        # Non-standard MitoFinder gene: editable/renameable like an unassigned ORF.
        is_nonstd <- length(sel) > 0 && isTRUE(is_nonstandard_mito_gene(
          rv$annotations$gene[sel], rv$annotations$type[sel], rv$annotations$tool[sel]
        ))
        is_joined <- length(sel) > 0 && stringr::str_detect(
          dplyr::coalesce(rv$annotations$notes[sel], ""), "^JOIN: "
        )
        shinyjs::toggle("annotation_action_btns", condition = length(sel) > 0 && !is_deleted)
        shinyjs::toggle("unjoin_btn", condition = is_joined && !is_deleted)
        shinyjs::toggle("annotation_restore_btn", condition = is_deleted)
        shinyjs::toggle("assign_gene_btn", condition = (is_orf || is_assigned_orf || is_nonstd) && !is_deleted)
        updateActionButton(
          inputId = "assign_gene",
          label = if (is_assigned_orf) "Edit gene assignment" else "Assign gene name"
        )
        # No row selected: skip sel-indexed branches so consumers (e.g. the
        # synteny zoom plot, which also accepts a click anchor) don't break.
        if (length(sel) == 0) {
          return(sel)
        }
        prev <- last_sel()
        last_sel(sel)
        # Review mode: only the focal (flagged) gene is editable. If the user
        # selects a different row, warn and snap the selection back so only one
        # gene can change between "Back to Review" recomputes.
        if (isTRUE(session$userData$in_outlier_review)) {
          info <- outlier_flag()
          if (!is.null(info) && !is.null(info$gene)) {
            focal_idx <- which(rv$annotations$gene == info$gene)
            if (length(focal_idx) > 0) {
              focal_idx <- focal_idx[[1]]
              if (!identical(sel, focal_idx)) {
                shinyWidgets::sendSweetAlert(
                  title = "Review mode",
                  text = paste0(
                    "Only ", toupper(info$gene),
                    " can be edited while reviewing this outlier. Use ",
                    "'Back to Review' to return to the outlier list."
                  ),
                  type = "info"
                )
                reactable::updateReactable("table", selected = focal_idx)
                return(focal_idx)
              }
            }
          }
        }
        if (identical(sel, rv$editing$idx)) {
          return(sel)
        }
        # In a join edit session, switching to a sibling segment of the same group
        # just changes the active segment (its edits are already in rv$annotations);
        # do not warn about unsaved edits or snap the selection back.
        if (!is.null(rv$editing) && !is.null(rv$editing$join_grp) &&
            sel %in% join_members(rv$editing$join_grp)) {
          rv$editing$idx <- sel
          trigger("align_now")
          return(sel)
        }
        if (!is.null(rv$editing) && editing_unsaved(rv$editing$idx)) {
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
        # Clicked away to a different gene (not the edited row, not a join
        # sibling) with no pending edits: cleanly exit the edit session so its
        # controls and spliced/join state don't carry over onto the new gene.
        if (!is.null(rv$editing)) {
          shinyjs::hide("edit_mode_ctrls")
          shinyjs::hide("save_edits")
          shinyjs::hide("discard_edits")
          shinyjs::show("edit_mode")
          rv$editing <- NULL
        }
        # Switching between segments of the same joined gene (e.g. clicking a
        # block in the spliced-CDS panel) should refresh the alignment in place,
        # not collapse it - the spliced alignment is the same gene either way.
        pg <- join_grp_of(prev)
        ng <- join_grp_of(sel)
        same_join <- !is.na(pg) && !is.na(ng) && identical(pg, ng)
        was_open <- length(rv$alignment) != 0
        rv$alignment <- rv$local_hits <- NULL
        ref_msa_cache$msa <- NULL
        ref_msa_cache$key <- NULL
        if (rv$annotations$type[sel] %in% c("PCG", "rRNA") &&
            (same_join || was_open)) {
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

    # TRUE when the annotation being edited has changes not yet saved. Checks the
    # raw fields, not just the translation: the manual partial 5'/3' flags and the
    # poly-A stop trim change partial_start/partial_stop/stop_codon/positions
    # without altering the translation, so a translation-only test would let the
    # user close/lock and silently drop those edits.
    editing_unsaved <- function(idx = selected()) {
      if (is.null(rv$editing) || is.null(rv$editing$backup)) return(FALSE)
      bak <- rv$editing$backup
      flds <- c("translation", "pos1", "pos2", "start_codon", "stop_codon",
                "partial_start", "partial_stop")
      # Join session: backup holds every segment. Compare the whole group as an
      # order-independent signature (positions may re-sort during editing).
      if (!is.null(rv$editing$join_grp)) {
        cur <- rv$annotations[join_members(rv$editing$join_grp), , drop = FALSE]
        sig <- function(df) {
          cols <- lapply(flds, function(f) {
            if (f %in% names(df)) as.character(df[[f]]) else rep(NA, nrow(df))
          })
          paste(sort(do.call(paste, c(cols, sep = ""))), collapse = "")
        }
        return(!identical(sig(cur), sig(bak)))
      }
      sel <- idx
      if (length(sel) != 1) return(FALSE)
      changed <- function(f) {
        a <- if (f %in% names(rv$annotations)) rv$annotations[[f]][sel] else NA
        b <- if (f %in% names(bak)) bak[[f]] else NA
        !isTRUE(a == b) && !(is.na(a) && is.na(b))
      }
      any(vapply(
        c("translation", "pos1", "pos2", "start_codon", "stop_codon", "partial_start", "partial_stop"),
        changed, logical(1)
      ))
    }

    # Close Modal ----
    observeEvent(input$close, {
      # Nothing to do if the modal state is already cleared (e.g. a second/spurious
      # close after rv$annotations was nulled below) - avoids filter() on NULL.
      req(!is.null(rv$annotations))
      if (editing_unsaved()) {
        shinyWidgets::sendSweetAlert(
          title = "Unsaved Edits!",
          text = "Discard or save edits before closing"
        )
        req(F)
      }
      # Update Annotate table counts
      retained_annotations <- rv$annotations |>
        dplyr::filter(!stringr::str_detect(gene, "_DELETED_"))
      rv$updating$PCGCount = sum(retained_annotations$type == "PCG")
      rv$updating$tRNACount = sum(retained_annotations$type == "tRNA")
      rv$updating$rRNACount = sum(retained_annotations$type == "rRNA")
      update_annotate_unit(c("PCGCount", "tRNACount", "rRNACount"))
      rv$data <- rv$data |>
        dplyr::rows_update(rv$updating[, c("ID", "path", "scaffold", "PCGCount", "tRNACount", "rRNACount")], by = c("ID", "path", "scaffold"))
      rv$annotations <- NULL
      rv$coverage <- NULL
      rv$table_filter <- NULL
      rv$alignment <- NULL
      rv$coverage_width <- NULL
      rv$editing <- NULL
      ref_msa_cache$msa <- NULL
      ref_msa_cache$key <- NULL
      trigger("update_annotate_table")
      # Re-fetch so a multi-assembly sample's parent row re-rolls correctly (the
      # in-place rv$data writes above are per-ID and cannot roll up sibling units).
      trigger("refresh_annotate")
      removeModal()
      # If we arrived here from the export outlier review, hop back to it
      if (isTRUE(session$userData$return_to_review)) {
        session$userData$return_to_review <- FALSE
        session$userData$in_outlier_review <- FALSE
        trigger("reopen_outlier_review")
      }
    })

    # Return to the export outlier review (saves via the standard close path)
    observeEvent(input$back_to_review, {
      session$userData$return_to_review <- TRUE
      # Record the (sample, gene) just reviewed so the review modal can mark it
      # resolved and navigate back to that gene. goto_annotate_target is cleared
      # when this modal opens, so read the persisted flag instead.
      info <- outlier_flag()
      if (!is.null(info) && !is.null(info$ID) && !is.null(info$gene)) {
        session$userData$resolve_on_return <- list(ID = info$ID, gene = info$gene)
      }
      shinyjs::click("close")
    })
    ## Lock and Close ----
    observeEvent(input$lock, {
      if (editing_unsaved()) {
        shinyWidgets::sendSweetAlert(
          title = "Unsaved Edits!",
          text = "Discard or save edits before locking"
        )
        req(F)
      }
      if (as.numeric(rv$updating$annotate_lock) != 1) {
        rv$updating$annotate_lock <- 1
        update_annotate_unit("annotate_lock")
        rv$data <- rv$data |>
          dplyr::rows_update(rv$updating[, c("ID", "path", "scaffold", "annotate_lock")], by = c("ID", "path", "scaffold"))
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
      # Use [[ ]] (not $): rv$updating can transiently lack these columns and
      # tibble $ warns on missing columns, while [[ returns NULL silently.
      ctx <- list(
        ID              = u[["ID"]],
        length          = u[["length"]],
        topology        = u[["topology"]],
        blast_accession = u[["blast_accession"]],
        blast_species   = u[["blast_species"]],
        poor_blast_ref  = u[["poor_blast_ref"]]
      )
      if (!identical(ctx, fig_ctx())) fig_ctx(ctx)
    })

    # Coverage Map ----
    output$coverage_map <- renderUI({
      req(rv$coverage, fig_ctx())
      # Split features that wrap the circular origin (pos1 > pos2) into two arrows.
      cov_seq_len <- max(rv$coverage$Position)
      # 1 kb axis ticks; empty for short scaffolds (seq(1000, <1000, by=1000)
      # errors "wrong sign in 'by'").
      xbreaks <- if (cov_seq_len >= 1000) seq(1000, cov_seq_len, by = 1000) else numeric(0)
      genes_df <- rv$annotations |>
        dplyr::filter(pos1 > 0) |>
        dplyr::mutate(
          type = factor(type, levels = c("ctrl", "PCG", "rRNA", "tRNA", "ORF")),
          xmin = pos1, xmax = pos2
        ) |>
        split_wrapped_genes(x_lo = 1, x_hi = cov_seq_len)
      rv$genes_plot <- genes_df |>
        ggplot2::ggplot() +
        ggplot2::aes(xmin = xmin, xmax = xmax, forward = direction == "+", fill = type, y = scaffold, label = gene) +
        gggenes::geom_gene_arrow(
          arrow_body_height = ggplot2::unit(6, "mm"),
          arrowhead_height = ggplot2::unit(6, "mm"),
          arrowhead_width = ggplot2::unit(1, "mm"),
          alpha = gene_type_alpha
        ) +
        ggplot2::scale_fill_manual(values = gene_type_fill) +
        ggplot2::scale_x_continuous(
          expand = c(0, 0),
          limits = c(
            1,
            max(c(rv$coverage$Position, rv$annotations$pos2))
          ),
          breaks = xbreaks,
          labels = format(xbreaks, big.mark = ",")
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
      cov_max  <- cov_seq_len
      minor_tick_x <- setdiff(
        if (cov_max >= 50) seq(50, cov_max, by = 50) else numeric(0), xbreaks
      )
      major_tick_x <- xbreaks
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
      img_w <- fig_ctx()$length
      x_hi  <- max(c(rv$coverage$Position, rv$annotations$pos2))
      div(
        style = sprintf("position:relative; width:%dpx; height:125px;", as.integer(img_w)),
        shiny::imageOutput(ns("coverage_plot"), width = paste0(img_w, "px"), height = "125px"),
        # Block = measured gene-arrow band (px 96-112 in the 125px cowplot image).
        gene_label_overlay(genes_df, img_w = img_w, x_lo = 1, x_hi = x_hi,
                           track_top = 96, track_height = 16, scale_y = 1.6,
                           arrow_w = 0.6)
      )
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
    # Gene-block coords (alignment-space %) stashed by the synteny image render,
    # consumed by the sticky-label overlay (output$synteny_labels).
    synteny_overlay <- reactiveVal(NULL)
    # Overview canvas width, shared by the UI container, the image render and the
    # click->column mapping (they must agree or labels and clicks misalign).
    # With an alignment the x-axis spans alignment columns, and the aligner pads
    # the window out to the full reference, so a short scaffold still needs a
    # reference-width canvas: sizing by the scaffold squeezes every reference gene
    # label into a few hundred px (a 928 bp scaffold vs a 21 kb reference is ~23x).
    synteny_plot_w <- reactive({
      aln <- rv$blast_ref_aln
      n <- if (!is.null(aln) && nrow(aln) > 0 && isTRUE(nzchar(aln$aligned_sample[1]))) {
        nchar(aln$aligned_sample[1])
      } else {
        fig_ctx()$length %||% 800L
      }
      max(as.integer(n), 800L)
    })
    # Sample position (original coords) -> canvas px on the overview. The x-axis is
    # alignment-column space whenever an alignment is shown, so a plain
    # pos/sample_len mapping lands in the wrong place (and ignores strand); mirror
    # the projection the plot itself uses.
    synteny_proj <- reactive({
      w   <- synteny_plot_w()
      aln <- rv$blast_ref_aln
      if (is.null(aln) || nrow(aln) == 0 || !isTRUE(nzchar(aln$aligned_sample[1]))) {
        sample_genes <- rv$annotations |> dplyr::filter(pos1 > 0)
        sample_len   <- max(c(rv$coverage$Position, sample_genes$pos2), na.rm = TRUE)
        return(function(pos) as.numeric(pos) / sample_len * w)
      }
      s_chars  <- strsplit(aln$aligned_sample[1], "")[[1]]
      aln_len  <- length(s_chars)
      s_nongap <- which(s_chars != "-")
      n_s      <- length(s_nongap)
      strand   <- aln$strand[1] %||% "+"
      function(pos) {
        idx <- as.integer(pos)
        if (identical(strand, "-")) idx <- n_s - idx + 1L
        idx <- pmin(pmax(idx, 1L), n_s)
        s_nongap[idx] / aln_len * w
      }
    })
    output$synteny_ui <- renderUI({
      ctx <- req(fig_ctx())
      # Need at least one candidate BLAST reference for this sample. The picker is
      # rendered even when the active reference has no annotations, so the user can
      # switch away from an unannotated top hit.
      cand <- rv$blast_ref_candidates
      req(!is.null(cand), nrow(cand) > 0)
      active_acc <- active_ref_acc() %||% ctx$blast_accession
      req(!is.null(active_acc), !is.na(active_acc), nzchar(active_acc))

      has_ref    <- !is.null(rv$blast_ref) && nrow(rv$blast_ref) > 0
      w          <- synteny_plot_w()
      sample_lbl <- ctx$ID
      cand_row   <- cand[cand$accession == active_acc, ]
      ref_lbl    <- if (nrow(cand_row) > 0 && !is.na(cand_row$species[1]) && nzchar(cand_row$species[1])) {
        cand_row$species[1]
      } else {
        ctx$blast_species %||% active_acc
      }
      ref_acc    <- active_acc
      has_aln    <- has_ref && !is.null(rv$blast_ref_aln) && nrow(rv$blast_ref_aln) > 0 &&
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
      # Reference picker (rank-ordered; default = active accession). Switching only
      # changes which reference the synteny plot displays, not the curation result.
      ref_choices <- stats::setNames(
        cand$accession,
        sprintf(
          "%s%s (%s) | pid %s%% cov %s%%",
          ifelse(is.na(cand$rank), "", paste0(cand$rank, ". ")),
          ifelse(is.na(cand$species) | !nzchar(cand$species), cand$accession, cand$species),
          cand$accession,
          ifelse(is.na(cand$pident), "?", format(cand$pident, trim = TRUE)),
          ifelse(is.na(cand$qcovs), "?", format(cand$qcovs, trim = TRUE))
        )
      )
      picker <- if (length(ref_choices) > 0) {
        div(
          id = ns("synteny_ref_picker"),
          style = "margin-bottom: 6px; max-width: 820px;",
          # Compact + single-line: shrink the selectize font and keep the selected
          # item / dropdown options on one line (long "species (acc) | pid.. cov.."
          # labels otherwise wrap in the box). Let the dropdown grow to fit.
          tags$style(HTML(sprintf(
            paste0(
              "#%1$s .selectize-input, #%1$s .selectize-dropdown { font-size: 11px; }",
              "#%1$s .selectize-input > .item, #%1$s .selectize-dropdown .option { white-space: nowrap; }",
              "#%1$s .selectize-dropdown { width: auto !important; min-width: 100%%; }"
            ),
            ns("synteny_ref_picker")
          ))),
          shiny::selectInput(
            ns("synteny_ref_select"),
            label = "Reference genome",
            choices = ref_choices,
            selected = active_acc,
            width = "100%"
          ),
          # Explicit commit: the picker above is view-only; this makes the viewed
          # candidate the sample's reference (tables + .tbl note + synteny default).
          local({
            cur_ref <- (ctx$blast_accession %||% NA) %|NA|% ""
            if (nzchar(active_acc) && !identical(active_acc, cur_ref)) {
              div(
                style = "margin-top: 2px;",
                shinyWidgets::actionBttn(
                  ns("synteny_set_ref"),
                  label = paste0("Set ", active_acc, " as best reference"),
                  style = "material-flat", size = "xs", color = "primary",
                  icon = shiny::icon("check")
                ),
                div(style = "font-size: 11px; color: #888; margin-top: 3px;",
                    "Overwrites the sample's best reference, shown in the Annotate/Export ",
                    "tables and used in the .tbl reference-comparison note.")
              )
            } else {
              div(style = "font-size: 11px; color: #888; margin-top: 2px;",
                  "Current best reference.")
            }
          })
        )
      }
      no_ref_msg <- if (!has_ref) {
        div(
          class = "alert alert-info",
          style = "padding: 6px 10px; font-size: 0.85em; margin-bottom: 6px;",
          "Selected reference has no usable annotations. Choose another reference above."
        )
      }
      tagList(
        div(
          style = "display: flex; justify-content: start; margin-bottom: 6px;",
          shinyWidgets::prettyToggle(
            ns("poor_blast_ref_toggle"),
            label_on  = "Best reference flagged as poor",
            label_off = "Flag best reference as poor",
            icon_on   = shiny::icon("flag"),
            icon_off  = shiny::icon("flag"),
            status_on  = "warning",
            status_off = "default",
            value = is_poor,
            inline = TRUE
          )
        ),
        picker,
        no_ref_msg,
        if (has_ref && isTRUE(ctx$topology == "linear")) {
          div(
            class = "alert alert-warning",
            style = "padding: 6px 10px; font-size: 0.85em; margin-bottom: 6px;",
            shiny::icon("triangle-exclamation"),
            " One or more assemblies is linear; alignment below may be poor"
          )
        },
        if (has_aln) {
          div(
            style = "display: flex; align-items: center; margin: 0; padding: 0; line-height: 1;",
            # The zoom control lives here, with the plot it drives. It must not sit
            # in the annotation action-button group: that group is hidden until a
            # gene row is selected, which suspends this output and leaves the
            # checkbox out of the DOM, so click-to-zoom could never open it.
            div(style = "flex-shrink: 0; width: 160px;",
                uiOutput(ns("synteny_zoom_ctrl"))),
            div(style = "flex: 1; text-align: center; font-size: 11px; color: #888; margin: 0; padding: 0;",
                "click to zoom")
          )
        },
        if (has_ref) div(
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
            div(
              style = sprintf("position: relative; width: %dpx;", as.integer(w)),
              imageOutput(ns("synteny_plot"), width = paste0(w, "px"), height = plot_h,
                         click = ns("synteny_click")),
              uiOutput(ns("synteny_labels"))
            )
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
        style = "display: flex; align-items: center; align-self: stretch;",
        # Match the checkbox control height to the sibling buttons (~34px) and
        # center its box + label, so it lines up on the row midline rather than
        # sitting at the top.
        tags$style(HTML(sprintf(
          paste0(
            "#%s .pretty { margin: 0; min-height: 34px; display: inline-flex; ",
            "align-items: center; }"
          ),
          ns("synteny_zoom")
        ))),
        shinyWidgets::prettyCheckbox(
          ns("synteny_zoom"),
          label = "Zoom",
          status = "primary",
          inline = TRUE
        )
      )
    })
    output$synteny_plot <- shiny::renderImage(
      {
      req(rv$blast_ref, rv$annotations, rv$coverage, fig_ctx())
      req(nrow(rv$blast_ref) > 0)

      img_w <- synteny_plot_w()
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
        # When the scaffold aligned on the reverse strand, aligned_sample is the
        # scaffold's reverse-complement, so a sample position P (original coords)
        # is the (n - P + 1)-th non-gap base and gene orientation is flipped.
        aln_strand     <- rv$blast_ref_aln$strand[1] %||% "+"
        s_chars <- strsplit(aligned_sample, "")[[1]]
        r_chars <- strsplit(aligned_ref,    "")[[1]]
        aln_len <- length(s_chars)

        # Non-gap index: s_nongap[i] = alignment column for sample position i
        s_nongap <- which(s_chars != "-")
        r_nongap <- which(r_chars != "-")
        n_s <- length(s_nongap)

        # Project sample position (original coords) -> 0-100 in alignment space
        s_to_pct <- function(pos) {
          idx <- as.integer(pos)
          if (identical(aln_strand, "-")) idx <- n_s - idx + 1L
          idx <- pmin(pmax(idx, 1L), n_s)
          s_nongap[idx] / aln_len * 100
        }
        # Project ref position (original coords) -> rotate -> 0-100 in alignment space
        r_to_pct <- function(pos) {
          pos_r <- ((as.integer(pos) - 1L - aln_rotation) %% ref_length) + 1L
          idx   <- pmin(pmax(pos_r, 1L), length(r_nongap))
          r_nongap[idx] / aln_len * 100
        }

        # Gene data frames in alignment coordinates. Features that wrap the
        # circular origin (or straddle the rotation point in the reference) map
        # to xmin > xmax; split them into two arcs at the 0/100 boundary. On the
        # reverse strand, s_to_pct reverses coordinate order, so take min/max and
        # flip arrow direction (a '+' scaffold gene points '-' vs the reference).
        sample_df <- sample_genes |>
          dplyr::mutate(type = factor(type, levels = c("ctrl", "PCG", "rRNA", "tRNA", "ORF")))
        if (identical(aln_strand, "-")) {
          sample_df <- sample_df |>
            dplyr::mutate(
              .x1 = s_to_pct(pos1), .x2 = s_to_pct(pos2),
              xmin = pmin(.x1, .x2), xmax = pmax(.x1, .x2),
              direction = ifelse(direction == "+", "-", "+")
            ) |>
            dplyr::select(-.x1, -.x2)
        } else {
          sample_df <- sample_df |>
            dplyr::mutate(xmin = s_to_pct(pos1), xmax = s_to_pct(pos2))
        }
        sample_df <- sample_df |>
          split_wrapped_genes(x_lo = 0, x_hi = 100)
        ref_df <- rv$blast_ref |>
          dplyr::mutate(
            type = factor(type, levels = c("ctrl", "PCG", "rRNA", "tRNA", "ORF")),
            xmin = r_to_pct(pos1), xmax = r_to_pct(pos2)
          ) |>
          split_wrapped_genes(x_lo = 0, x_hi = 100)

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

        synteny_overlay(list(
          has_aln = TRUE, img_w = img_w,
          sample_df = sample_df, ref_df = ref_df
        ))

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
        # Rotate reference coordinates to the shared anchor gene (per endpoint,
        # modulo the reference length). A feature that wraps the circular origin
        # or straddles the rotation point then maps to xmin > xmax and is split
        # into two arcs at the 0/100 boundary below.
        ref_rotated <- rv$blast_ref |>
          dplyr::mutate(
            pos1_r = as.integer(((as.integer(pos1) - 1L - rotation) %% ref_length) + 1L),
            pos2_r = as.integer(((as.integer(pos2) - 1L - rotation) %% ref_length) + 1L)
          ) |>
          dplyr::mutate(type = factor(type, levels = c("ctrl", "PCG", "rRNA", "tRNA", "ORF")))
        sample_df  <- sample_genes |>
          dplyr::mutate(type = factor(type, levels = c("ctrl", "PCG", "rRNA", "tRNA", "ORF")))
        sample_pct <- sample_df |>
          dplyr::mutate(xmin = pos1 / sample_len * 100, xmax = pos2 / sample_len * 100) |>
          split_wrapped_genes(x_lo = 0, x_hi = 100)
        ref_pct    <- ref_rotated |>
          dplyr::mutate(xmin = pos1_r / ref_length * 100, xmax = pos2_r / ref_length * 100) |>
          split_wrapped_genes(x_lo = 0, x_hi = 100)
        sample_plot <- ggplot2::ggplot(sample_pct) +
          ggplot2::aes(xmin = xmin, xmax = xmax, forward = direction == "+",
                       fill = type, y = 0, label = gene) + gene_track
        ref_plot <- ggplot2::ggplot(ref_pct) +
          ggplot2::aes(xmin = xmin, xmax = xmax, forward = direction == "+",
                       fill = type, y = 0, label = gene) + gene_track
        synteny_overlay(list(
          has_aln = FALSE, img_w = img_w,
          sample_df = sample_pct, ref_df = ref_pct
        ))

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

    # Sticky gene-name labels overlaid on the synteny image. Two gene tracks
    # (sample on top, reference on bottom); band positions track the patchwork
    # layout heights (3:1:3 with alignment, 1:1 without).
    output$synteny_labels <- renderUI({
      ov <- synteny_overlay()
      req(ov)
      # Block = measured gene-arrow bands (px). has_aln sample 41-74 / ref
      # 194-227 (280px image); no_aln sample 30-63 / ref 125-158 (200px image).
      if (ov$has_aln) {
        sample_top <- 41; ref_top <- 194
      } else {
        sample_top <- 30; ref_top <- 125
      }
      tagList(
        gene_label_overlay(ov$sample_df, img_w = ov$img_w, x_lo = 0, x_hi = 100,
                           track_top = sample_top, track_height = 33, inset = 5),
        gene_label_overlay(ov$ref_df, img_w = ov$img_w, x_lo = 0, x_hi = 100,
                           track_top = ref_top, track_height = 33, inset = 5)
      )
    })

    # Zoomed base-pair view of selected gene's alignment region ----
    # Gene-block coords (local alignment cols) stashed by the zoom plot render,
    # consumed by the sticky-label overlay (output$synteny_zoom_labels).
    zoom_overlay <- reactiveVal(NULL)
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
      ref_acc    <- active_ref_acc() %||% ctx$blast_accession
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
            div(
              style = sprintf("position: relative; width: %dpx;", plot_w),
              plotOutput(ns("synteny_zoom_plot"),
                         width = paste0(plot_w, "px"), height = "180px"),
              uiOutput(ns("synteny_zoom_labels"))
            )
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
      # On the reverse strand aligned_sample is the scaffold's reverse-complement,
      # so a sample position P (original coords) is the (n_s - P + 1)-th non-gap
      # base and gene orientation is flipped (mirrors the overview plot).
      aln_strand <- rv$blast_ref_aln$strand[1] %||% "+"
      s_chars <- strsplit(aligned_sample, "")[[1]]
      r_chars <- strsplit(aligned_ref,    "")[[1]]
      aln_len <- length(s_chars)
      s_nongap <- which(s_chars != "-")
      r_nongap <- which(r_chars != "-")
      n_s <- length(s_nongap)
      # Sample original position -> alignment column (RC order on reverse strand)
      s_pos_to_col <- function(pos) {
        idx <- as.integer(pos)
        if (identical(aln_strand, "-")) idx <- n_s - idx + 1L
        idx <- pmin(pmax(idx, 1L), n_s)
        s_nongap[idx]
      }

      win <- zoom_window_rv()
      # Anchor alignment column: the clicked column, or the selected gene's start
      # column (strand-aware). Back off ~20 non-gap bases upstream (aln order).
      anchor_col <- if (!is.null(click_col)) {
        as.integer(click_col)
      } else {
        s_pos_to_col(rv$annotations$pos1[sel_idx])
      }
      anchor_ng <- as.integer(cumsum(s_chars != "-")[anchor_col])
      start_idx <- pmin(pmax(anchor_ng - 20L, 1L), n_s)
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
      # Per-base letter colour.
      base_color <- function(b) {
        u <- toupper(b)
        dplyr::case_when(
          u == "A" ~ "#4faf45",
          u == "C" ~ "#e0a53f",
          u == "G" ~ "#e0555a",
          u == "T" ~ "#4a90d9",
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
      # (strand-aware). On the reverse strand pos1 maps above pos2 in alignment
      # order, so take min/max for the block and flip the arrow direction.
      sg <- rv$annotations |> dplyr::filter(pos1 > 0)
      sg_c1 <- s_pos_to_col(sg$pos1)
      sg_c2 <- s_pos_to_col(sg$pos2)
      sg_lo <- pmin(sg_c1, sg_c2)
      sg_hi <- pmax(sg_c1, sg_c2)
      sg_in <- sg_hi >= win_start & sg_lo <= win_end
      sample_gene_df <- if (any(sg_in)) {
        data.frame(
          xmin = to_local(pmax(sg_lo[sg_in], win_start)) - 0.5,
          xmax = to_local(pmin(sg_hi[sg_in], win_end)) + 0.5,
          gene = sg$gene[sg_in],
          fill = type_color(sg$type[sg_in]),
          forward = if (identical(aln_strand, "-")) sg$direction[sg_in] != "+" else sg$direction[sg_in] == "+",
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

      # Stash gene blocks for the sticky-label overlay (forward -> +/- marker).
      mk_ov <- function(d) {
        if (is.null(d)) return(NULL)
        d$direction <- ifelse(d$forward, "+", "-")
        d
      }
      zoom_overlay(list(
        sample_df = mk_ov(sample_gene_df),
        ref_df    = mk_ov(ref_gene_df),
        n_cols    = length(win_cols),
        plot_w    = as.integer(win * 14L)
      ))

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

    # Sticky gene-name labels overlaid on the zoom plot. Kept small to fit the
    # ~16 px arrow blocks. Arrow centres (px): sample 34, ref 159 (180px plot);
    # ~6 px panel inset from the 2 mm plot margin.
    output$synteny_zoom_labels <- renderUI({
      ov <- zoom_overlay()
      req(ov)
      x_lo <- 0.5; x_hi <- ov$n_cols + 0.5
      tagList(
        gene_label_overlay(ov$sample_df, img_w = ov$plot_w, x_lo = x_lo, x_hi = x_hi,
                           track_top = 26, track_height = 16,
                           scale_x = 1, scale_y = 1, inset = 6, arrow_w = 0.6),
        gene_label_overlay(ov$ref_df, img_w = ov$plot_w, x_lo = x_lo, x_hi = x_hi,
                           track_top = 151, track_height = 16,
                           scale_x = 1, scale_y = 1, inset = 6, arrow_w = 0.6)
      )
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
      plot_w_px <- synteny_plot_w()
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
        scroll_px <- synteny_proj()(rv$annotations$pos1[selected()])
        session$sendCustomMessage(
          "hScroll", list(id = ns("syntenyScrollDiv"), px = scroll_px)
        )
      }
    })

    # Scroll the overview to the scaffold's aligned window. On a reference-width
    # canvas a short scaffold occupies a small slice (a 928 bp scaffold is ~4% of a
    # 21 kb reference) that would otherwise sit off-screen. onFlushed defers the
    # scroll until the plot container exists.
    observeEvent(rv$blast_ref_aln, {
      aln <- rv$blast_ref_aln
      req(!is.null(aln), nrow(aln) > 0, isTRUE(nzchar(aln$aligned_sample[1])))
      s_chars <- strsplit(aln$aligned_sample[1], "")[[1]]
      nz <- which(s_chars != "-")
      req(length(nz) > 0)
      px <- max(0, (nz[1] - 1L) / length(s_chars) * synteny_plot_w() - 50)
      session$onFlushed(function() {
        session$sendCustomMessage(
          "hScroll", list(id = ns("syntenyScrollDiv"), px = px)
        )
      }, once = TRUE)
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
    # Percent identity over the aligned (gap-free) columns of a 2-sequence
    # nucleotide alignment. Returns NA when there is no comparable column.
    dna_pct_identity <- function(aln) {
      if (length(aln) < 2L) return(NA_real_)
      m <- as.matrix(aln)
      a <- m[1, ]; b <- m[2, ]
      keep <- a != "-" & b != "-"
      if (!any(keep)) return(NA_real_)
      100 * sum(a[keep] == b[keep]) / sum(keep)
    }

    # Build a nucleotide MSA for an rRNA row: the focal rRNA sequence aligned to
    # the matching rRNA region of the per-sample remote BLAST reference genome
    # (Source A, always shown when available) plus any featureNuc multi-reference
    # hits stored in refHits (Source B). Returns a list shaped like the protein
    # `new_alignment` so the msa header/widget render it.
    build_rrna_alignment <- function(sel) {
      gene <- rv$annotations$gene[sel]
      out <- list(
        start = "", stop = "",
        partial = partial_label(sel), internal_stop = "",
        colorscheme = "nucleotide",
        render_nonce = isolate(rv$alignment$render_nonce %||% 0L) + 1L
      )

      # Focal rRNA nucleotide sequence from the assembly (revcomp on - strand).
      asm <- rv$editing$assembly %||% tryCatch(get_assembly(
        ID = rv$annotations$ID[sel], path = rv$annotations$path[sel],
        scaffold = rv$annotations$scaffold[sel], con = session$userData$con
      ), error = function(e) NULL)
      focal_seq <- tryCatch({
        s <- Biostrings::subseq(asm, rv$annotations$pos1[sel], rv$annotations$pos2[sel])
        if (rv$annotations$direction[sel] == "-") s <- Biostrings::reverseComplement(s)
        as.character(s)
      }, error = function(e) NA_character_)

      if (is.na(focal_seq) || !nzchar(focal_seq)) {
        out$aln <- Biostrings::DNAStringSet(setNames("N", paste(gene, "(focal)")))
        out$alignmentHeight <- 40
        out$id <- "<b>rRNA sequence unavailable</b>"
        return(out)
      }
      focal <- setNames(focal_seq, paste(gene, "(focal)"))

      refs <- character(0)   # named reference sequences (remote ref + featureNuc hits)
      best_sim <- NA_real_   # header similarity (from featureNuc hits when present)

      # Source A: rRNA region of the per-sample remote BLAST reference genome.
      # Always included (mirrors how the remote hit is prepended for PCGs) so the
      # GenBank reference shows even when featureNuc multi-reference hits exist.
      if (!is.null(rv$blast_ref) && !is.null(rv$blast_ref_seq) &&
          nrow(rv$blast_ref) > 0 && nrow(rv$blast_ref_seq) > 0) {
        refrow <- rv$blast_ref[rv$blast_ref$type == "rRNA" & rv$blast_ref$gene == gene, , drop = FALSE]
        if (nrow(refrow) > 0) {
          refrow <- refrow[1, ]
          ref_dna <- tryCatch(Biostrings::DNAString(rv$blast_ref_seq$sequence[1]),
                              error = function(e) NULL)
          if (!is.null(ref_dna)) {
            ref_seq_chr <- tryCatch({
              s <- Biostrings::subseq(ref_dna, refrow$pos1, refrow$pos2)
              if (identical(refrow$direction, "-")) s <- Biostrings::reverseComplement(s)
              as.character(s)
            }, error = function(e) NULL)
            if (!is.null(ref_seq_chr) && nzchar(ref_seq_chr)) {
              ref_name <- paste0(rv$blast_ref_seq$accession[1] %||% "reference", " (GenBank ref)")
              refs <- c(refs, stats::setNames(ref_seq_chr, ref_name))
            }
          }
        }
      }

      # Source B: featureNuc multi-reference hits stored in refHits by curation
      # (populated when the featureNuc/<gene>.fas BLAST DBs exist).
      hits <- tryCatch(json_parse(rv$annotations$refHits[sel], TRUE), error = function(e) NULL)
      if (is.data.frame(hits) && nrow(hits) > 0 && "target" %in% names(hits)) {
        refs <- c(refs, stats::setNames(hits$target, hits$Taxon %||% hits$acc))
        best_sim <- suppressWarnings(max(hits$similarity, na.rm = TRUE))
      }

      # Drop duplicate reference sequences (e.g. the GenBank ref also in featureNuc).
      refs <- refs[!duplicated(unname(refs))]

      if (length(refs) == 0) {
        out$aln <- Biostrings::DNAStringSet(focal)
        out$alignmentHeight <- 40
        out$id <- "<b>No rRNA reference available</b>"
        return(out)
      }

      dna_set <- Biostrings::DNAStringSet(c(focal, refs))
      aln <- tryCatch(DECIPHER::AlignSeqs(dna_set, verbose = FALSE),
                      error = function(e) dna_set)
      out$aln <- aln
      out$alignmentHeight <- 20 + length(aln) * 20
      # Header: best featureNuc similarity if available, else focal-vs-ref identity.
      if (!is.finite(best_sim)) {
        best_sim <- tryCatch(dna_pct_identity(aln), error = function(e) NA_real_)
      }
      out$id <- if (!is.finite(best_sim)) "<b>Max Similarity:</b> n/a" else
        paste0("<b>Max Similarity:</b> ", round(best_sim, 1), "%")
      out
    }

    on("align_now", {
      # Clear any "hold tight" overlay from a +/- edit once alignment finishes;
      # on.exit so it also clears if a req() below aborts. No-op when not shown.
      on.exit(waiter::waiter_hide(), add = TRUE)
      # check if user wants to use fewer reference samples in alignment
      if (isTRUE(input$reduce_align)){ n_hits = 5 } else { n_hits = Inf }

      # rRNA: nucleotide-level alignment against the BLAST reference rRNA region.
      # Handled separately (no protein translation / per-gene protein hits).
      if (isTRUE(rv$annotations$type[selected()] == "rRNA")) {
        rv$alignment <- build_rrna_alignment(selected())
        req(FALSE)  # alignment built; skip the protein path below
      }

      req(rv$annotations$type[selected()] %in% c("PCG", "ORF"))
      is_orf <- rv$annotations$type[selected()] == "ORF"
      # An ORF assigned a gene keeps tool == "ORFfinder" but a non-ORF type.
      feat_gene <- rv$annotations$gene[selected()]
      is_assigned_orf <- isTRUE(rv$annotations$tool[selected()] == "ORFfinder") && !is_orf
      # Non-standard MitoFinder gene: hits come from the combined gene DB and
      # carry a candidate-gene column, so label them like ORF hits.
      is_nonstd <- isTRUE(is_nonstandard_mito_gene(
        feat_gene, rv$annotations$type[selected()], rv$annotations$tool[selected()]
      ))

      using_local <- !is.null(rv$local_hits)
      # For a joined gene, hits come from the group's representative member (the
      # active segment may have none), so the alignment stays populated.
      hits <- rv$local_hits %||% json_parse(rv$annotations$refHits[align_hits_idx(selected())], TRUE)
      # For an ORF assigned a gene, the combined-DB hits already cover every
      # per-gene reference set (top hits per gene). Decide what to show:
      #   - gene present in hits  -> restrict to that gene (gene-specific align)
      #   - standard PCG, absent  -> no hits to that gene's DB (show message)
      #   - custom name           -> keep all hits (full-DB align + gene prefix)
      # Skipped for local BLAST (no per-gene structure in those hits).
      gene_filtered <- FALSE
      no_gene_db_hits <- FALSE
      if (is_assigned_orf && !using_local) {
        if (is.data.frame(hits) && "gene" %in% names(hits) && feat_gene %in% hits$gene) {
          hits <- hits[hits$gene == feat_gene, , drop = FALSE]
          gene_filtered <- TRUE
        } else if (feat_gene %in% MITO_PCG_GENES) {
          no_gene_db_hits <- TRUE
        }
      }
      hits <- hits |> dplyr::slice_head(n = n_hits)

      focal <- focal_for(selected()) |>
        setNames(paste(
          rv$annotations$gene[selected()],
          if (is_orf) "(ORF, focal)" else "(focal)"
        ))

      new_alignment <- list()
      if (no_gene_db_hits) {
        new_alignment$seqs <- character(0)
        new_alignment$aln <- Biostrings::AAStringSet(focal)
        new_alignment$alignmentHeight <- 40
        new_alignment$id <- stringr::str_glue(
          "<b>No BLAST hits against the {feat_gene} reference database.</b>"
        )
      }else if(nrow(hits)==0){
        new_alignment$seqs <- character(0)
        new_alignment$aln <- Biostrings::AAStringSet(focal)
        new_alignment$alignmentHeight <- 40
        new_alignment$id <- stringr::str_glue(
          "<b>Max Similarity:</b> n/a"
        )
      }else{
        # Label hits with their candidate gene (alongside the taxon) for any ORF
        # whose hits are not restricted to a single assigned gene: unassigned
        # ORFs and custom-name assignments (all-gene hits kept). A standard
        # assignment (filtered above) and real PCGs drop the redundant prefix.
        show_gene_prefix <- (is_orf || is_nonstd || (is_assigned_orf && !gene_filtered)) &&
          "gene" %in% names(hits)
        hit_labels <- if (show_gene_prefix) {
          paste(hits$gene, "|", hits$Taxon)
        } else {
          hits$Taxon
        }
        new_alignment$seqs <- setNames(hits$target, hit_labels)
        # Cache the reference-only MSA: references don't change as the user walks
        # codons, so reuse it and add the focal sequence via profile alignment.
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
      # For a joined gene, show the spliced gene's terminal codons (not the active
      # internal segment's), matching the displayed spliced protein.
      active_sp <- if (identical(rv$annotations$type[selected()], "PCG") &&
                       seg_role(selected())$join) {
        spliced_active(selected())
      } else {
        NULL
      }
      new_alignment$stop <- stringr::str_glue(
        "<b>Stop Codon:</b> {active_sp$stop_codon %||% rv$annotations$stop_codon[selected()]}"
      )
      new_alignment$start <- stringr::str_glue(
        "<b>Start Codon:</b> {active_sp$start_codon %||% rv$annotations$start_codon[selected()]}"
      )
      new_alignment$partial <- partial_label(selected())
      # Use the displayed protein (spliced translation for a joined gene) so the
      # internal-stop warning appears in the same header spot for all PCGs.
      new_alignment$internal_stop <- ifelse(
        stringr::str_detect(unname(focal), "\\*"),
        paste0(
          "<span style=\"color:#c00; font-weight:bold;\">",
          as.character(icon("triangle-exclamation")),
          " internal stop codon</span>"
        ),
        ""
      )
      # Nonce so each edit invalidates the render (reactiveValues dedupes
      # identical values, missing repeat-codon content changes).
      new_alignment$render_nonce <- isolate(rv$alignment$render_nonce %||% 0L) + 1L
      rv$alignment <- new_alignment
    })
    output$msa_header <- renderUI({
      div(
        style = "display: flex; gap: 25px;",
        p(HTML(rv$alignment$id)),
        p(HTML(rv$alignment$start)),
        p(HTML(rv$alignment$stop)),
        p(HTML(rv$alignment$partial)),
        p(HTML(rv$alignment$internal_stop))
      )
    })

    # Spliced-CDS preview for joined PCGs: shows the concatenated translation as
    # one colored block per exon/segment (5'->3'). Clicking a block selects that
    # segment so the edit controls act on it. The boundary indicator for joins.
    output$join_preview <- renderUI({
      sel <- selected()
      req(length(sel) == 1)
      req(identical(rv$annotations$type[sel], "PCG"))
      grp <- grp_of(sel)
      req(length(grp) == 1, !is.na(grp))
      mem <- join_members(grp)
      req(length(mem) >= 2)
      sp <- spliced_active(sel)
      if (is.null(sp)) {
        return(div(
          class = "alert alert-warning",
          style = "padding: 6px 10px; margin: 4px 0;",
          icon("triangle-exclamation"),
          paste(
            " Spliced CDS could not be translated - the segment lengths may not",
            "sum to a multiple of 3. Adjust a junction by nucleotide."
          )
        ))
      }
      prot <- sp$translation
      segs <- sp$segments
      palette <- c("#9ecae1", "#a1d99b", "#fdae6b", "#bcbddc", "#fa9fb5", "#c7e9c0")
      blocks <- lapply(seq_len(nrow(segs)), function(i) {
        aa1 <- segs$aa_start[i]
        aa2 <- segs$aa_end[i]
        real_idx <- mem[segs$member_row[i]]
        is_active <- isTRUE(real_idx == sel)
        col <- palette[((i - 1) %% length(palette)) + 1]
        tags$span(
          title = stringr::str_glue(
            "exon {i}: aa {aa1}-{aa2}, nt {segs$pos1[i]}-{segs$pos2[i]}"
          ),
          onclick = stringr::str_glue(
            "Shiny.setInputValue('{ns('join_seg_click')}', {real_idx}, {{priority: 'event'}})"
          ),
          style = paste0(
            "cursor: pointer; padding: 2px 1px; font-family: 'Courier New', Courier, monospace; ",
            "background-color:", col, ";",
            if (is_active) " outline: 2px solid #c00; outline-offset: -2px; font-weight: bold;" else ""
          ),
          substr(prot, aa1, aa2)
        )
      })
      div(
        style = "margin-bottom: 6px;",
        tags$b("Spliced CDS "),
        tags$span(
          style = "color:#666;",
          stringr::str_glue(
            "({nchar(prot)} aa, {nrow(segs)} segments - click a segment to edit it)"
          )
        ),
        div(
          style = "word-break: break-all; line-height: 1.6; margin-top: 4px;",
          blocks
        )
      )
    })

    observeEvent(input$join_seg_click, {
      idx <- suppressWarnings(as.integer(input$join_seg_click))
      req(length(idx) == 1, !is.na(idx))
      reactable::updateReactable("table", selected = idx)
    })

    # Live nucleotide context for a PCG's boundaries, so the user can see the
    # actual bases (not just the AA translation) at the start/stop (and, for a
    # joined gene, the splice junctions). Shows ~12 flanking bases, a cut marker,
    # and the in-CDS bases grouped into reading-frame codons. For a joined gene
    # the flank bases that fall in a neighbouring exon are highlighted. Shown for
    # any PCG in both view and edit mode; updates on each nudge.
    output$junction_context <- renderUI({
      sel <- selected()
      req(length(sel) == 1)
      req(identical(rv$annotations$type[sel], "PCG"))
      # Use the loaded edit assembly if editing, else fetch it (viewer works in
      # both edit and view mode).
      asm <- rv$editing$assembly %||% tryCatch(get_assembly(
        ID = rv$annotations$ID[sel], path = rv$annotations$path[sel],
        scaffold = rv$annotations$scaffold[sel], con = session$userData$con
      ), error = function(e) NULL)
      req(!is.null(asm))
      width <- asm@ranges@width
      dir <- rv$annotations$direction[sel]
      p1 <- rv$annotations$pos1[sel]
      p2 <- rv$annotations$pos2[sel]
      flank <- 12L; inN <- 15L
      grp <- grp_of(sel)
      is_join <- !is.na(grp)
      seglen <- function(i) as.integer(abs(rv$annotations$pos2[i] - rv$annotations$pos1[i]) + 1L)
      # Members of this gene, 5'->3'. A single-feature PCG is its own only member
      # (no neighbouring segments to highlight).
      mem <- if (is_join) join_members(grp) else sel
      k <- match(sel, mem)
      cum_before <- if (k > 1) sum(vapply(mem[seq_len(k - 1)], seglen, integer(1))) else 0L
      this_len <- seglen(sel)

      gseq <- function(a, b) {
        a <- max(1L, as.integer(a)); b <- min(as.integer(width), as.integer(b))
        if (a > b) return("")
        as.character(Biostrings::subseq(asm, a, b))
      }
      rc <- function(s) if (nchar(s) == 0) s else
        as.character(Biostrings::reverseComplement(Biostrings::DNAString(s)))
      # insert spaces so codon boundaries align; phase0 = bases already used in
      # the current codon at the first base of `s`.
      codon_group <- function(s, phase0) {
        if (nchar(s) == 0) return(s)
        chars <- strsplit(s, "")[[1]]
        used <- phase0 %% 3
        out <- character(0)
        for (ch in chars) {
          if (used == 0 && length(out) > 0) out <- c(out, " ")
          out <- c(out, ch); used <- (used + 1L) %% 3L
        }
        paste(out, collapse = "")
      }
      inside  <- function(s) tags$span(style = "color:#111;", s)
      cut <- tags$span(style = "color:#c00; font-weight:bold;", "|")
      # Per-member genomic bounds + cumulative spliced offset (bases before each
      # member, 5'->3'), so flanking bases that fall in a neighbouring exon can be
      # coloured AND grouped into the spliced reading frame of that exon.
      mem_len <- vapply(mem, seglen, integer(1))
      mem_cum <- cumsum(c(0L, mem_len))[seq_along(mem)]
      lo_of <- vapply(mem, function(i) as.integer(min(rv$annotations$pos1[i], rv$annotations$pos2[i])), integer(1))
      hi_of <- vapply(mem, function(i) as.integer(max(rv$annotations$pos1[i], rv$annotations$pos2[i])), integer(1))
      pos_member <- function(pos) {
        hit <- which(pos >= lo_of & pos <= hi_of)
        if (length(hit)) hit[1] else 0L
      }
      # Spliced-CDS phase (bases already used in the current codon) at a genomic
      # position within member k, using k's 5' end as its coding start.
      pos_phase <- function(pos, k) {
        off <- if (dir == "-") hi_of[k] - pos else pos - lo_of[k]
        (mem_cum[k] + off) %% 3
      }
      comp1 <- function(chars) {
        m <- c(A = "T", T = "A", G = "C", C = "G", a = "t", t = "a",
               g = "c", c = "g")
        out <- m[chars]; out[is.na(out)] <- chars[is.na(out)]; unname(out)
      }
      # Render a flanking (outside-CDS) window with per-base colouring: bases
      # inside a neighbouring exon of this gene are shown blue + uppercase
      # (colourblind-friendly Okabe-Ito blue) and codon-grouped in that exon's
      # reading frame; intron/intergenic bases stay muted lowercase. gstart/gend
      # are ascending genomic coords; revcomp reverses + complements for display
      # on the - strand so it reads 5'->3'.
      flank_span <- function(gstart, gend, revcomp) {
        gstart <- max(1L, as.integer(gstart)); gend <- min(as.integer(width), as.integer(gend))
        if (gstart > gend) return(NULL)
        chars <- strsplit(as.character(Biostrings::subseq(asm, gstart, gend)), "")[[1]]
        positions <- gstart:gend
        if (revcomp) { chars <- rev(comp1(chars)); positions <- rev(positions) }
        cls <- vapply(positions, pos_member, integer(1))
        spans <- list(); i <- 1L; n <- length(chars)
        while (i <= n) {
          j <- i
          while (j < n && cls[j + 1L] == cls[i]) j <- j + 1L
          seg <- paste(chars[i:j], collapse = "")
          spans[[length(spans) + 1L]] <- if (cls[i] > 0L) {
            tags$span(
              style = "color:#0072B2;",
              codon_group(toupper(seg), pos_phase(positions[i], cls[i]))
            )
          } else {
            tags$span(style = "color:#aa99aa;", tolower(seg))
          }
          i <- j + 1L
        }
        spans
      }
      # Whether more assembly sequence continues past the shown flank at each end
      # (i.e. we are not at a linear-assembly boundary). Used to add a "..."
      # continuation marker at the outer edge of each line.
      if (dir == "-") {
        more5 <- (p2 + flank) < width
        more3 <- (p1 - flank) > 1L
      } else {
        more5 <- (p1 - flank) > 1L
        more3 <- (p2 + flank) < width
      }
      ell <- function() tags$span(style = "color:#888; margin: 0 2px;", "...")
      # 5' end of the segment (reads 5'->3'): flank | inside-codons. Inside
      # windows are clamped to [p1, p2] so a short segment never shows adjacent
      # (intron / neighbouring-segment) bases as if they were coding.
      if (dir == "-") {
        flank5 <- flank_span(p2 + 1, p2 + flank, TRUE)
        in5 <- rc(gseq(max(p1, p2 - inN + 1), p2))
      } else {
        flank5 <- flank_span(p1 - flank, p1 - 1, FALSE)
        in5 <- gseq(p1, min(p2, p1 + inN - 1))
      }
      line5 <- tags$div(
        style = "font-family: 'Courier New', Courier, monospace; white-space: nowrap;",
        tags$span(style = "color:#666; margin-right:6px;", "5'"),
        if (more5) ell(),
        flank5, cut, inside(codon_group(in5, cum_before %% 3))
      )
      # 3' end of the segment: inside-codons | flank
      phase3 <- (cum_before + max(0L, this_len - inN)) %% 3
      if (dir == "-") {
        in3 <- rc(gseq(p1, min(p2, p1 + inN - 1))); flank3 <- flank_span(p1 - flank, p1 - 1, TRUE)
      } else {
        in3 <- gseq(max(p1, p2 - inN + 1), p2); flank3 <- flank_span(p2 + 1, p2 + flank, FALSE)
      }
      line3 <- tags$div(
        style = "font-family: 'Courier New', Courier, monospace; white-space: nowrap;",
        inside(codon_group(in3, phase3)), cut, flank3,
        if (more3) ell(),
        tags$span(style = "color:#666; margin-left:6px;", "3'")
      )
      # Bases of the segment interior hidden between the two shown windows.
      mid_bp <- this_len - nchar(in5) - nchar(in3)
      gap_line <- if (mid_bp > 0) tags$div(
        style = "font-family: 'Courier New', Courier, monospace; color:#888; font-size:0.8em; margin: 1px 0;",
        paste0("\u22ee ", mid_bp, " bp \u22ee")
      )
      div(
        class = "mp-edit-group",
        # Keep the horizontal scroll on an inner wrapper: putting overflow-x on
        # the .mp-edit-group itself makes the browser clip overflow-y too, which
        # cuts off the absolutely-positioned caption sitting above the border.
        style = "margin: 6px 0; padding-top: 14px;",
        tags$span(
          class = "mp-edit-group-label",
          if (is_join) "Active segment junctions (nt)" else "Nucleotide boundaries (nt)"
        ),
        tags$div(style = "overflow-x: auto;", line5, gap_line, line3),
        tags$div(
          style = "color:#888; font-size:0.75em; margin-top:2px;",
          tags$span(style = "color:#c00; font-weight:bold;", "|"), " = boundary; ",
          if (is_join) tagList(
            tags$span(style = "color:#0072B2;", "neighbouring segment"), "; "
          ),
          "spaces = codon frame"
        )
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
        colorscheme = rv$alignment$colorscheme %||% "zappo",
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
      notes_df <- data.frame(ID = rv$updating$ID, path = rv$updating$path,
                             scaffold = rv$updating$scaffold, annotate_notes = cleaned)
      dplyr::tbl(session$userData$con, "annotate") |>
        dplyr::rows_update(
          notes_df,
          by = c("ID", "path", "scaffold"),
          unmatched = "ignore",
          copy = TRUE,
          in_place = TRUE
        )
      rv$data <- rv$data |>
        dplyr::rows_update(notes_df[, c("ID", "path", "scaffold", "annotate_notes")], by = c("ID", "path", "scaffold"))
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
          gene = paste0(rv$annotations[selected(), "gene"], "_DELETED_", as.numeric(Sys.time())) # timestamp suffix keeps the deleted gene's key unique
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
      persist_annotations()
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
            paste0(rv$updating$ID, "_coverageStats_", rv$annotations$path[selected()],
                 "_", rv$annotations$scaffold[selected()], ".csv")
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
        persist_annotations()

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
          paste0(rv$updating$ID, "_assembly_", rv$annotations$path[selected()],
                 "_", rv$annotations$scaffold[selected()], ".fasta")
        )
      )

      ## Update Annotate Table ----
      rv$updating$topology <- "linear"
      note <- stringr::str_glue(
        "EDITED: linearized circular assembly after rotating {start-1} bp"
      )
      # Read the live notes input so notes typed this session are preserved.
      cur_notes <- (input$notes %||% rv$updating$annotate_notes) %|NA|% ""
      rv$updating$annotate_notes <- paste(note, cur_notes, sep = "; ")
      updateTextAreaInput(
        inputId = "notes",
        value = rv$updating$annotate_notes
      )
      update_annotate_unit(c("topology", "annotate_notes"))
      rv$data <- rv$data |>
        dplyr::rows_update(
          rv$updating[, c("ID", "path", "scaffold", "topology", "annotate_notes")],
          by = c("ID", "path", "scaffold")
        )
    }) # END LINEARIZE

    # Mark ID verified ----
    observeEvent(input$ID_verified, {
      if(is.na(rv$updating$ID_verified)) {
        updateActionButton(session, "ID_verified")
        rv$updating$ID_verified <- "yes"
        update_annotate_unit("ID_verified")
        rv$data <- rv$data |>
          dplyr::rows_update(rv$updating[, c("ID", "path", "scaffold", "ID_verified")], by = c("ID", "path", "scaffold"))
      } else if(as.character(rv$updating$ID_verified) == "no"){
        updateActionButton(session, "ID_verified")
        rv$updating$ID_verified <- "yes"
        update_annotate_unit("ID_verified")
        rv$data <- rv$data |>
          dplyr::rows_update(rv$updating[, c("ID", "path", "scaffold", "ID_verified")], by = c("ID", "path", "scaffold"))
      } else {
        updateActionButton(session, "ID_verified")
        rv$updating$ID_verified <- "no"
        update_annotate_unit("ID_verified")
        rv$data <- rv$data |>
          dplyr::rows_update(rv$updating[, c("ID", "path", "scaffold", "ID_verified")], by = c("ID", "path", "scaffold"))
      }
    }) # END ID VERIFIED

    # Mark as reviewed ----
    observeEvent(input$reviewed, {
      if (as.character(rv$updating$reviewed) == "no") {
        updateActionButton(session, "reviewed")
        rv$updating$reviewed <- "yes"
        update_annotate_unit("reviewed")
        rv$data <- rv$data |>
          dplyr::rows_update(rv$updating[, c("ID", "path", "scaffold", "reviewed")], by = c("ID", "path", "scaffold"))
      } else {
        updateActionButton(session, "reviewed")
        rv$updating$reviewed <- "no"
        update_annotate_unit("reviewed")
        rv$data <- rv$data |>
          dplyr::rows_update(rv$updating[, c("ID", "path", "scaffold", "reviewed")], by = c("ID", "path", "scaffold"))
      }
    }) # END REVIEWED

    # Mark as problematic ----
    observeEvent(input$problematic, {
      if (is.na(rv$updating$problematic)) {
        updateActionButton(session, "problematic")
        rv$updating$problematic <- "yes"
        update_annotate_unit("problematic")
        rv$data <- rv$data |>
          dplyr::rows_update(rv$updating[, c("ID", "path", "scaffold", "problematic")], by = c("ID", "path", "scaffold"))
      } else {
        updateActionButton(session, "problematic")
        rv$updating$problematic <- NA_character_
        update_annotate_unit("problematic")
        rv$data <- rv$data |>
          dplyr::rows_update(rv$updating[, c("ID", "path", "scaffold", "problematic")], by = c("ID", "path", "scaffold"))
      }
    }) # END PROBLEMATIC

    # Mark as partial ----
    apply_partial <- function(value) {
      updateActionButton(session, "partial")
      rv$updating$partial <- value
      update_annotate_unit("partial")
      rv$data <- rv$data |>
        dplyr::rows_update(rv$updating[, c("ID", "path", "scaffold", "partial")], by = c("ID", "path", "scaffold"))
    }
    observeEvent(input$partial, {
      if (!isTRUE(rv$updating$partial == "yes")) {
        # turning partial on: warn first if the assembly is circular
        if (isTRUE(rv$updating$topology == "circular")) {
          shinyWidgets::confirmSweetAlert(
            inputId = ns("partial_circular_confirm"),
            title = "Mark circular assembly as partial?",
            text = paste(
              "This assembly is circular. A closed circle represents the whole",
              "molecule, so flagging it 'partial' is contradictory. Use the",
              "Linearize button to break the circle before submission, or mark",
              "it partial anyway."
            ),
            type = "warning",
            btn_labels = c("Cancel", "Mark partial anyway"),
            btn_colors = c("#6c757d", "#0056b3")
          )
          req(F)
        }
        apply_partial("yes")
      } else {
        apply_partial("no")
      }
    })
    observeEvent(input$partial_circular_confirm, ignoreInit = TRUE, {
      if (isTRUE(input$partial_circular_confirm)) {
        apply_partial("yes")
      }
    }) # END PARTIAL

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

    # Reference picker: view-only. Switching just repaints the synteny plot for the
    # selected candidate; it does NOT change the sample's reference. Committing a new
    # reference is an explicit action (the "Set as reference genome" button below).
    observeEvent(input$synteny_ref_select, ignoreInit = TRUE, {
      acc <- input$synteny_ref_select
      req(acc, nzchar(acc))
      if (identical(acc, active_ref_acc())) return()
      active_ref_acc(acc)
      load_blast_ref(acc)
    })

    # "Set as reference genome": make the currently-viewed candidate THIS unit's
    # reference. Written per unit to blast_ref_override, not to the sample-level
    # assemble.blast_accession: the candidate list you pick from is this scaffold's
    # own, so a sample-level write would relabel sibling scaffolds that never hit the
    # accession. Drives the Annotate/Export tables, the export note and the synteny
    # default. Does not re-curate.
    observeEvent(input$synteny_set_ref, ignoreInit = TRUE, {
      acc <- active_ref_acc()
      req(acc, !is.na(acc), nzchar(acc))
      cur <- (rv$updating[["blast_accession"]] %||% NA) %|NA|% ""
      if (identical(acc, cur)) return()
      cand <- rv$blast_ref_candidates
      crow <- if (!is.null(cand)) cand[!is.na(cand$accession) & cand$accession == acc, , drop = FALSE]
      species <- if (!is.null(crow) && nrow(crow) > 0) crow$species[1] else NA_character_
      pident  <- if (!is.null(crow) && nrow(crow) > 0) crow$pident[1]  else NA_real_
      qcovs   <- if (!is.null(crow) && nrow(crow) > 0) crow$qcovs[1]   else NA_real_
      lineage <- tryCatch({
        l <- dplyr::tbl(session$userData$con, "blast_ref_sequences") |>
          dplyr::filter(accession == !!acc) |>
          dplyr::pull(lineage)
        if (length(l) > 0) l[1] else NA_character_
      }, error = function(e) NA_character_)
      # This scaffold's own BLAST top hit: choosing it again clears the override
      # rather than storing one that just restates the automatic answer.
      auto_acc <- tryCatch(
        dplyr::tbl(session$userData$con, "assemblies") |>
          dplyr::filter(ID == !!rv$updating$ID & path == !!rv$updating$path &
                          scaffold == !!rv$updating$scaffold) |>
          dplyr::pull("blast_accession"),
        error = function(e) character(0)
      )
      is_auto <- length(auto_acc) > 0 && identical(as.character(auto_acc[1]), acc)

      ok <- tryCatch({
        if (is_auto) {
          DBI::dbExecute(
            session$userData$con,
            "DELETE FROM blast_ref_override WHERE ID = ? AND path = ? AND scaffold = ?",
            params = list(rv$updating$ID, rv$updating$path, rv$updating$scaffold)
          )
        } else {
          DBI::dbExecute(
            session$userData$con,
            "INSERT OR REPLACE INTO blast_ref_override
               (ID, path, scaffold, accession, time_stamp)
             VALUES (?, ?, ?, ?, ?)",
            params = list(rv$updating$ID, rv$updating$path, rv$updating$scaffold,
                          acc, as.integer(Sys.time()))
          )
        }
        TRUE
      }, error = function(e) { showNotification(paste("Failed to set reference:", conditionMessage(e)), type = "error"); FALSE })
      req(ok)

      # Reflect in the modal (fig_ctx / picker) and the Annotate table (rv$data), for
      # THIS unit only.
      rv$updating$blast_accession <- acc
      if ("blast_species" %in% names(rv$updating)) rv$updating$blast_species <- species
      if ("blast_pident"  %in% names(rv$updating)) rv$updating$blast_pident  <- pident
      if ("blast_qcovs"   %in% names(rv$updating)) rv$updating$blast_qcovs   <- qcovs
      if ("blast_lineage" %in% names(rv$updating)) rv$updating$blast_lineage <- lineage
      set_cols <- list(blast_accession = acc, blast_species = species,
                       blast_pident = pident, blast_qcovs = qcovs, blast_lineage = lineage)
      set_cols <- set_cols[intersect(names(set_cols), names(rv$data))]
      rv$data <- rv$data |>
        dplyr::rows_update(
          data.frame(ID = rv$updating$ID, path = rv$updating$path,
                     scaffold = rv$updating$scaffold, set_cols,
                     stringsAsFactors = FALSE),
          by = c("ID", "path", "scaffold"), unmatched = "ignore"
        )
      showNotification(paste0("Reference set to ", acc, " for ", rv$updating$ID), type = "message")
    })

    # Join-group editing helpers ----
    # A joined gene (multi-exon / ribosomal-slippage) is stored as several
    # annotation rows sharing a "JOIN: ... group=<id>" marker. The editor treats
    # the group as one entity: the selected row is the "active segment", controls
    # act on it, and the spliced-CDS preview / protein alignment are computed over
    # all members via splice_join_cds() (shared with export).

    # Group id for a row's JOIN marker, or NA if it is not a join member.
    join_grp_of <- function(idx) {
      if (length(idx) != 1) return(NA_character_)
      stringr::str_match(
        rv$annotations$notes[idx] %|NA|% "", "^JOIN: mode=\\w+ group=(\\d+)"
      )[, 2]
    }
    # Current member row indices for a group, ordered 5'->3'.
    join_members <- function(grp) {
      if (length(grp) != 1 || is.na(grp)) return(integer(0))
      idx <- which(stringr::str_detect(
        dplyr::coalesce(rv$annotations$notes, ""),
        paste0("^JOIN: mode=\\w+ group=", grp, "\\b")
      ))
      if (length(idx) == 0) return(idx)
      idx <- idx[order(rv$annotations$pos1[idx])]
      if (rv$annotations$direction[idx[1]] == "-") idx <- rev(idx)
      idx
    }
    # Resolve the join group for a row. During an edit session the stored group
    # is authoritative, but only for rows that actually belong to it; any other
    # row (e.g. after the user clicks away to a different gene) derives its group
    # from its own notes, so a stale edit session never leaks its spliced view /
    # segment roles onto an unrelated gene.
    grp_of <- function(sel) {
      eg <- rv$editing$join_grp
      if (!is.null(eg) && length(sel) == 1 && !is.na(sel) &&
          sel %in% join_members(eg)) {
        return(eg)
      }
      join_grp_of(sel)
    }
    # Role of the active segment within its group: which terminal end(s) it owns.
    # Non-join annotations own both ends.
    seg_role <- function(sel) {
      grp <- grp_of(sel)
      if (length(grp) != 1 || is.na(grp)) {
        return(list(join = FALSE, is_5 = TRUE, is_3 = TRUE, n = 1L))
      }
      mem <- join_members(grp)
      list(
        join = TRUE,
        is_5 = isTRUE(sel == mem[1]),
        is_3 = isTRUE(sel == mem[length(mem)]),
        n = length(mem)
      )
    }
    # Spliced CDS for the active segment's group, or NULL on failure (e.g. a
    # transient out-of-frame length mid-edit). Uses the loaded edit assembly.
    spliced_active <- function(sel) {
      grp <- grp_of(sel)
      if (length(grp) != 1 || is.na(grp)) return(NULL)
      mem <- join_members(grp)
      if (length(mem) < 2) return(NULL)
      asm <- rv$editing$assembly
      if (is.null(asm)) {
        asm <- tryCatch(get_assembly(
          ID = rv$annotations$ID[sel],
          path = rv$annotations$path[sel],
          scaffold = rv$annotations$scaffold[sel],
          con = session$userData$con
        ), error = function(e) NULL)
      }
      if (is.null(asm)) return(NULL)
      tryCatch(
        splice_join_cds(rv$annotations[mem, ], asm, rv$gcode),
        error = function(e) NULL
      )
    }
    # Focal protein for the alignment: spliced CDS for a joined PCG, else the
    # row's own translation.
    focal_for <- function(sel) {
      if (identical(rv$annotations$type[sel], "PCG") && seg_role(sel)$join) {
        sp <- spliced_active(sel)
        if (!is.null(sp)) return(sp$translation)
      }
      rv$annotations$translation[sel]
    }
    # Row whose refHits represent the whole gene for alignment. For a joined gene
    # the active segment may have no hits of its own (internal/3' segments), so
    # use the first member (5'->3') that carries non-empty refHits, else `sel`.
    align_hits_idx <- function(sel) {
      grp <- grp_of(sel)
      if (length(grp) != 1 || is.na(grp)) return(sel)
      mem <- join_members(grp)
      has_hits <- vapply(mem, function(i) {
        rh <- rv$annotations$refHits[i]
        !is.na(rh) && nzchar(rh) && rh != "[]"
      }, logical(1))
      if (any(has_hits)) mem[which(has_hits)[1]] else sel
    }
    # TRUE if [new_pos1, new_pos2] would intersect another member of idx's join
    # group (genomic overlap; members share path/scaffold by construction).
    seg_would_overlap <- function(idx, new_pos1, new_pos2) {
      grp <- grp_of(idx)
      if (length(grp) != 1 || is.na(grp)) return(FALSE)
      others <- setdiff(join_members(grp), idx)
      if (length(others) == 0) return(FALSE)
      lo <- min(new_pos1, new_pos2)
      hi <- max(new_pos1, new_pos2)
      any(vapply(others, function(i) {
        olo <- min(rv$annotations$pos1[i], rv$annotations$pos2[i])
        ohi <- max(rv$annotations$pos1[i], rv$annotations$pos2[i])
        lo <= ohi && olo <= hi
      }, logical(1)))
    }

    # Edit Annotation ----
    observeEvent(input$edit_mode, {
      shinyjs::show("edit_mode_ctrls")
      shinyjs::show("save_edits")
      shinyjs::show("discard_edits")
      shinyjs::hide("edit_mode")
      rv$editing$idx <- selected()
      grp <- join_grp_of(selected())
      rv$editing$join_grp <- if (is.na(grp)) NULL else grp
      # Back up the whole group so discard restores every segment.
      rv$editing$backup <- if (is.null(rv$editing$join_grp)) {
        rv$annotations[selected(), ]
      } else {
        rv$annotations[join_members(rv$editing$join_grp), ]
      }
      rv$editing$params <- dplyr::left_join(
        dplyr::tbl(session$userData$con, "annotate") |>
          dplyr::select(ID, path, scaffold, curate_opts) |>
          dplyr::filter(ID == !!rv$updating$ID,
                        path == !!rv$updating$path,
                        scaffold == !!rv$updating$scaffold) |>
          dplyr::select(ID, curate_opts),
        dplyr::tbl(session$userData$con, "curate_opts"),
        by = "curate_opts"
      ) |>
        dplyr::pull(params) |>
        json_parse() |>
        {
          \(x) {
            # ORFs have no curation ruleset; inherit PCG defaults so the
            # start/stop codon-search editing controls have valid codon lists.
            rule_type <- rv$annotations$type[selected()]
            if (identical(rule_type, "ORF")) rule_type <- "PCG"
            modifyList(
              x$default_rules[[rule_type]] %||% list(),
              x$rules[[rv$annotations$gene[selected()]]] %||% list()
            )
          }
        }()
      rv$editing$assembly <- get_assembly(
        ID = rv$annotations$ID[selected()],
        path = rv$annotations$path[selected()],
        scaffold = rv$annotations$scaffold[selected()],
        con = session$userData$con
      )
      # rRNA uses the nucleotide-boundary editor (no codon search); show the
      # codon controls only for protein-coding rows.
      shinyjs::toggle(
        "codon_edit_ctrls",
        condition = !identical(rv$annotations$type[selected()], "rRNA")
      )
      # Codon START/STOP search only applies to the segment owning the gene's
      # 5'/3' end (whole gene for non-join annotations).
      role <- seg_role(selected())
      shinyjs::toggle("start_search_ctrl", condition = role$is_5)
      shinyjs::toggle("stop_search_ctrl", condition = role$is_3)
    })

    # Re-toggle codon search controls when the active segment changes within a
    # join edit session.
    observeEvent(selected(), {
      req(rv$editing, length(selected()) == 1, !is.null(rv$editing$join_grp))
      role <- seg_role(selected())
      shinyjs::toggle("start_search_ctrl", condition = role$is_5)
      shinyjs::toggle("stop_search_ctrl", condition = role$is_3)
    })

    ## Re align if user wants to show fewer reference samples ----
    observeEvent(input$reduce_align, {
      trigger("align_now")
    })

    ## Edit start-add ----
    # Step magnitude (codons per click) comes from a numeric box (separate for
    # START and STOP); clamp to [1, 50] and fall back to 1 for empty/invalid.
    edit_step_size <- function(id) {
      n <- suppressWarnings(as.integer(input[[id]]))
      if (length(n) == 0 || is.na(n) || n < 1L) 1L else min(n, 50L)
    }
    # "Hold tight" overlay during a start/stop edit + re-alignment, so rapid
    # repeated +/- clicks don't queue up while the alignment recomputes. Hidden
    # in the align_now handler once the (slow) alignment finishes.
    show_edit_waiter <- function(message = "Updating alignment, hold tight...") {
      waiter::waiter_show(
        html = tagList(
          waiter::spin_fading_circles(),
          tags$h4(style = "color:white; margin-top:1em;", message)
        ),
        color = "rgba(40,40,40,0.85)"
      )
    }
    # Keep the displayed box value inside [1, 50] when the user types directly.
    for (.id in c("start_step_size", "stop_step_size")) {
      local({
        id <- .id
        observeEvent(input[[id]], {
          n <- input[[id]]
          if (length(n) == 0 || is.na(n)) return()
          clamped <- max(1, min(as.integer(n), 50))
          if (!identical(as.integer(n), as.integer(clamped))) {
            updateNumericInput(session, id, value = clamped)
          }
        })
      })
    }
    init("start-add-simple")
    on("start-add-simple", {
      rv$editing$stop_aln <- FALSE
      codon <- "INIT"
      n_steps <- edit_step_size("start_step_size")
      pos1 <- rv$annotations$pos1[selected()]
      pos2 <- rv$annotations$pos2[selected()]
      if (rv$annotations$direction[selected()] == "+") {
        for (counter in seq_len(n_steps)) {
          keep_going <- TRUE
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
          Biostrings::translate(genetic.code = rv$gcode)
      }
      if (rv$annotations$direction[selected()] == "-") {
        for (counter in seq_len(n_steps)) {
          keep_going <- TRUE
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
          Biostrings::translate(genetic.code = rv$gcode) |>
          as.character()
      }
      rv$annotations$pos1[selected()] <- pos1
      rv$annotations$pos2[selected()] <- pos2
      rv$annotations$length[selected()] <- abs(pos1 - pos2) + 1
      rv$annotations$start_codon[selected()] <- unname(codon)
    })
    observeEvent(input$`start-add`, {
      show_edit_waiter()
      trigger("start-add-simple")
      shinyjs::delay(50, {
        trigger("re_align")
      })
    })

    ## Edit start-minus ----
    init("start-minus-simple")
    on("start-minus-simple", {
      rv$editing$stop_aln <- FALSE
      codon <- "INIT"
      n_steps <- edit_step_size("start_step_size")
      pos1 <- rv$annotations$pos1[selected()]
      pos2 <- rv$annotations$pos2[selected()]
      if (rv$annotations$direction[selected()] == "+") {
        for (counter in seq_len(n_steps)) {
          keep_going <- TRUE
          while (keep_going) {
            pos1 <- pos1 + 3
            req(pos1 < pos2)
            codon <- rv$editing$assembly |>
              Biostrings::subseq(pos1, pos1 + 2) |>
              as.character()
            if (isTRUE(input$single_codon)) break
            keep_going <- codon %nin% rv$editing$params$start_codons
          }
        }
        rv$annotations$translation[selected()] <- rv$editing$assembly |>
          Biostrings::subseq(pos1, pos2 - nchar(rv$annotations$stop_codon[selected()])) |>
          Biostrings::translate(genetic.code = rv$gcode) |>
          as.character()
      }
      if (rv$annotations$direction[selected()] == "-") {
        for (counter in seq_len(n_steps)) {
          keep_going <- TRUE
          while (keep_going) {
            pos2 <- pos2 - 3
            req(pos2 > pos1)
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
          Biostrings::translate(genetic.code = rv$gcode) |>
          as.character()
      }
      rv$annotations$pos1[selected()] <- pos1
      rv$annotations$pos2[selected()] <- pos2
      rv$annotations$length[selected()] <- abs(pos1 - pos2) + 1
      rv$annotations$start_codon[selected()] <- unname(codon)
    })
    observeEvent(input$`start-minus`, {
      show_edit_waiter()
      trigger("start-minus-simple")
      shinyjs::delay(50, {
        trigger("re_align")
      })
    })

    ## Edit stop-add ----
    init("stop-add-simple")
    on("stop-add-simple" , {
      rv$editing$stop_aln <- TRUE
      codon <- "INIT"
      n_steps <- edit_step_size("stop_step_size")
      pos1 <- rv$annotations$pos1[selected()]
      pos2 <- rv$annotations$pos2[selected()]
      if (rv$annotations$direction[selected()] == "+") {
        pos2 <- pos2 + (3 - nchar(rv$annotations$stop_codon[selected()]))
        for (counter in seq_len(n_steps)) {
          keep_going <- TRUE
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
          Biostrings::translate(genetic.code = rv$gcode) |>
          as.character()
      }
      if (rv$annotations$direction[selected()] == "-") {
        pos1 <- pos1 - (3 - nchar(rv$annotations$stop_codon[selected()]))
        for (counter in seq_len(n_steps)) {
          keep_going <- TRUE
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
          Biostrings::translate(genetic.code = rv$gcode) |>
          as.character()
      }
      rv$annotations$pos1[selected()] <- pos1
      rv$annotations$pos2[selected()] <- pos2
      rv$annotations$length[selected()] <- abs(pos1 - pos2) + 1
      rv$annotations$stop_codon[selected()] <- unname(codon)
    })
    observeEvent(input$`stop-add`, {
      show_edit_waiter()
      trigger("stop-add-simple")
      shinyjs::delay(50, {
        trigger("re_align")
      })
    })

    ## Edit stop-minus ----
    init("stop-minus-simple")
    on("stop-minus-simple", {
      rv$editing$stop_aln <- TRUE
      codon <- "INIT"
      n_steps <- edit_step_size("stop_step_size")
      pos1 <- rv$annotations$pos1[selected()]
      pos2 <- rv$annotations$pos2[selected()]
      if (rv$annotations$direction[selected()] == "+") {
        pos2 <- pos2 + (3 - nchar(rv$annotations$stop_codon[selected()]))
        for (counter in seq_len(n_steps)) {
          keep_going <- TRUE
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
          Biostrings::translate(genetic.code = rv$gcode) |>
          as.character()
      }
      if (rv$annotations$direction[selected()] == "-") {
        pos1 <- pos1 - (3 - nchar(rv$annotations$stop_codon[selected()]))
        for (counter in seq_len(n_steps)) {
          keep_going <- TRUE
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
          Biostrings::translate(genetic.code = rv$gcode) |>
          as.character()
      }
      rv$annotations$pos1[selected()] <- pos1
      rv$annotations$pos2[selected()] <- pos2
      rv$annotations$length[selected()] <- abs(pos1 - pos2) + 1
      rv$annotations$stop_codon[selected()] <- unname(codon)
    })
    observeEvent(input$`stop-minus`, {
      show_edit_waiter()
      trigger("stop-minus-simple")
      shinyjs::delay(50, {
        trigger("re_align")
      })
    })

    ## Manual partial flags + poly-A stop ----
    # Reactive controls shown in edit mode for PCGs: flag the 5'/3' end as
    # partial (honored by export as < / > location markers) and trim the stop
    # to a 1-2 bp partial (T / TA) completed by a 3' poly-A tail.
    # TRUE when the terminal codon is not in the gene-specific allowed list from
    # the curation params (an undetermined / partial end). Empty allowed list ->
    # no check.
    start_codon_invalid <- function(sel) {
      # Only the 5'-terminal segment of a join owns the gene's start codon.
      if (!seg_role(sel)$is_5) return(FALSE)
      allowed <- rv$editing$params$start_codons
      sc <- rv$annotations$start_codon[sel]
      !is.null(allowed) && length(allowed) > 0 && isTRUE(nzchar(sc)) && sc %nin% allowed
    }
    stop_codon_invalid <- function(sel) {
      # Only the 3'-terminal segment of a join owns the gene's stop codon.
      if (!seg_role(sel)$is_3) return(FALSE)
      allowed <- rv$editing$params$stop_codons
      ec <- rv$annotations$stop_codon[sel]
      !is.null(allowed) && length(allowed) > 0 && isTRUE(nzchar(ec)) && ec %nin% allowed
    }

    output$partial_ctrls <- renderUI({
      req(rv$editing)
      req(all(c("partial_start", "partial_stop") %in% names(rv$annotations)))
      sel <- selected()
      req(length(sel) == 1)
      is_rrna <- identical(rv$annotations$type[sel], "rRNA")
      req(rv$annotations$type[sel] == "PCG" || is_rrna)
      # In a join, the 5'-terminal segment owns the gene's 5' end and the
      # 3'-terminal segment owns the 3' end; internal segments own neither.
      role <- seg_role(sel)
      # Highlight a partial end when the flag is set OR (PCG only) the terminal
      # codon is not an allowed gene codon.
      ps <- isTRUE(as.integer(rv$annotations$partial_start[sel]) == 1L) ||
        (!is_rrna && start_codon_invalid(sel))
      pe <- isTRUE(as.integer(rv$annotations$partial_stop[sel]) == 1L) ||
        (!is_rrna && stop_codon_invalid(sel))
      tagList(
        # poly-A stop trim applies to the gene's 3' end only (and not rRNA).
        if (!is_rrna && role$is_3) actionButton(
          ns("polyA_stop"), "poly-A stop",
          icon = icon("scissors"),
          class = "btn btn-default btn-sm"
        ),
        if (role$is_5 || role$is_3) tags$span(
          style = "font-weight: bold; margin-left: 1em;", "PARTIAL"
        ),
        if (role$is_5) actionButton(
          ns("toggle_partial_start"), "5'",
          class = if (ps) "btn btn-warning btn-sm" else "btn btn-default btn-sm"
        ),
        if (role$is_3) actionButton(
          ns("toggle_partial_stop"), "3'",
          class = if (pe) "btn btn-warning btn-sm" else "btn btn-default btn-sm"
        )
      )
    })

    observeEvent(input$toggle_partial_start, {
      req(rv$editing, length(selected()) == 1)
      cur <- as.integer(rv$annotations$partial_start[selected()]) %|NA|% 0L
      rv$annotations$partial_start[selected()] <- if (isTRUE(cur == 1L)) 0L else 1L
      if (!is.null(rv$alignment)) {
        rv$alignment$partial <- partial_label(selected())
      }
      # Refresh the feature table so the "Partial" (5'/3') badge reflects the
      # toggle immediately; keep the current selection so editing continues.
      reactable::updateReactable("table", data = rv$annotations, selected = selected())
    })
    observeEvent(input$toggle_partial_stop, {
      req(rv$editing, length(selected()) == 1)
      cur <- as.integer(rv$annotations$partial_stop[selected()]) %|NA|% 0L
      rv$annotations$partial_stop[selected()] <- if (isTRUE(cur == 1L)) 0L else 1L
      if (!is.null(rv$alignment)) {
        rv$alignment$partial <- partial_label(selected())
      }
      reactable::updateReactable("table", data = rv$annotations, selected = selected())
    })

    ## rRNA nucleotide-boundary editor ----
    # rRNAs do not code, so there is no codon search: the user nudges the 5'/3'
    # boundary by N nucleotides. Moving an end auto-flags it as partial (the exact
    # boundary is uncertain); the PARTIAL toggle buttons let the user clear it.
    rrna_step_size <- function() {
      n <- suppressWarnings(as.integer(input$rrna_step_size))
      if (length(n) == 0 || is.na(n) || n < 1L) 1L else min(n, 500L)
    }
    # TRUE when the active row should expose the nucleotide-boundary nudge:
    # rRNA (no codon search) or a joined-PCG segment (junctions are not codons).
    nt_nudge_row <- function(sel) {
      length(sel) == 1 && (
        identical(rv$annotations$type[sel], "rRNA") ||
        (identical(rv$annotations$type[sel], "PCG") && seg_role(sel)$join)
      )
    }
    output$rrna_edit_ctrls <- renderUI({
      req(rv$editing)
      sel <- selected()
      req(length(sel) == 1)
      req(nt_nudge_row(sel))
      nudge <- function(id, sym) {
        tags$button(
          class = "icon-circle grow",
          onclick = stringr::str_glue(
            "Shiny.setInputValue('{ns(id)}', Math.random(), {{priority: 'event'}})"
          ),
          tags$span(style = "font-size: 0.75em;", sym)
        )
      }
      # Caption distinguishes the nucleotide nudge from the codon-search box.
      role <- seg_role(sel)
      cap <- "Nucleotide edit"
      # For a joined PCG the start/stop-owning terminal end is edited via codon
      # search (not nucleotides), so only expose the nt nudge on junction (non-
      # terminal) ends. rRNA has no codon search, so it keeps both ends.
      is_pcg <- identical(rv$annotations$type[sel], "PCG")
      show_5 <- !(is_pcg && role$is_5)
      show_3 <- !(is_pcg && role$is_3)
      seg_5 <- if (show_5) list(
        tags$span(style = "font-weight: bold;", "5'"),
        nudge("rrna-5-out", "+"),   # extend 5' (grow upstream)
        nudge("rrna-5-in", "\u2212")
      )
      seg_3 <- if (show_3) list(
        tags$span(style = "font-weight: bold;", "3'"),
        nudge("rrna-3-in", "\u2212"),
        nudge("rrna-3-out", "+")    # extend 3' (grow downstream)
      )
      step_box <- list(
        div(
          class = "mp-step-box",
          style = "width: 56px; margin: 0 0.2em;",
          numericInput(
            # Retain the chosen step across re-renders of this renderUI (it
            # re-renders on each nudge because it depends on rv$editing).
            ns("rrna_step_size"), label = NULL,
            value = isolate(input$rrna_step_size) %||% 1,
            min = 1, max = 500, step = 1, width = "56px"
          )
        ),
        tags$span(style = "font-size: 0.75em; color: #666; margin-right: 0.4em;", "nt")
      )
      tagList(div(
        class = "mp-edit-group",
        style = "display: flex; align-items: center;",
        tags$span(class = "mp-edit-group-label", cap),
        div(
          style = "display: flex; flex-flow: row nowrap; align-items: center; gap: 0.4em;",
          seg_5, step_box, seg_3
        )
        )
      )
    })

    # Move an rRNA boundary by the step size. `end` is "5"/"3"; `action` is
    # "extend"/"trim". Strand decides which physical coordinate each end maps to.
    adjust_rrna <- function(end, action) {
      sel <- selected()
      req(rv$editing, length(sel) == 1)
      req(nt_nudge_row(sel))
      is_rrna <- identical(rv$annotations$type[sel], "rRNA")
      # A joined PCG's start/stop-owning terminal end is edited via codon search,
      # not the nucleotide nudge; ignore any nudge on that end.
      if (!is_rrna) {
        role <- seg_role(sel)
        if ((end == "5" && role$is_5) || (end == "3" && role$is_3)) {
          return(invisible(NULL))
        }
      }
      step <- rrna_step_size()
      dir <- rv$annotations$direction[sel]
      pos1 <- rv$annotations$pos1[sel]
      pos2 <- rv$annotations$pos2[sel]
      width <- rv$editing$assembly@ranges@width
      # 5' maps to pos1 on +, pos2 on -; 3' maps to pos2 on +, pos1 on -.
      end_is_pos1 <- (end == "5") == (dir != "-")
      outward <- action == "extend"
      if (end_is_pos1) {
        pos1 <- pos1 + (if (outward) -step else step)
      } else {
        pos2 <- pos2 + (if (outward) step else -step)
      }
      pos1 <- max(1L, as.integer(pos1))
      pos2 <- min(as.integer(width), as.integer(pos2))
      req(pos1 < pos2)
      # Block edits that would push this segment into a neighbouring segment of
      # the same joined gene.
      if (seg_would_overlap(sel, pos1, pos2)) {
        shinyWidgets::sendSweetAlert(
          title = "Segments would overlap",
          text = "That move would overlap another segment of this gene. Adjust the other segment first.",
          type = "warning"
        )
        return(invisible(NULL))
      }
      rv$annotations$pos1[sel] <- pos1
      rv$annotations$pos2[sel] <- pos2
      rv$annotations$length[sel] <- abs(pos2 - pos1) + 1L
      # rRNA: moving an end means the exact boundary is uncertain, so auto-flag it
      # partial. For a joined-PCG segment the moved end is usually an internal
      # splice junction (not a partial gene end), so do not auto-flag.
      if (is_rrna) {
        if (end == "5" && "partial_start" %in% names(rv$annotations)) {
          rv$annotations$partial_start[sel] <- 1L
        }
        if (end == "3" && "partial_stop" %in% names(rv$annotations)) {
          rv$annotations$partial_stop[sel] <- 1L
        }
        if (!is.null(rv$alignment)) rv$alignment$partial <- partial_label(sel)
      }
      show_edit_waiter()
      shinyjs::delay(50, trigger("re_align"))
    }
    observeEvent(input$`rrna-5-out`, adjust_rrna("5", "extend"))
    observeEvent(input$`rrna-5-in`,  adjust_rrna("5", "trim"))
    observeEvent(input$`rrna-3-out`, adjust_rrna("3", "extend"))
    observeEvent(input$`rrna-3-in`,  adjust_rrna("3", "trim"))

    # Trim the stop codon by one terminal base (TAA -> TA -> T), shrinking the
    # CDS by 1 bp so it no longer overlaps a downstream feature. The removed
    # base(s) are completed by the mRNA poly-A tail; export marks this via
    # transl_except when nchar(stop_codon) < 3.
    observeEvent(input$polyA_stop, {
      req(rv$editing, length(selected()) == 1)
      req(rv$annotations$type[selected()] == "PCG")
      stop_codon <- rv$annotations$stop_codon[selected()]
      if (is.na(stop_codon) || nchar(stop_codon) <= 1) {
        shinyWidgets::sendSweetAlert(
          session, title = "Stop already minimal",
          text = "Stop codon is already a single base (T).", type = "info"
        )
        req(FALSE)
      }
      new_stop <- stringr::str_sub(stop_codon, 1, nchar(stop_codon) - 1)
      if (new_stop %nin% rv$editing$params$stop_codons) {
        shinyWidgets::sendSweetAlert(
          session, title = "Invalid partial stop",
          text = paste0("'", new_stop, "' is not an allowed stop for this gene."),
          type = "warning"
        )
        req(FALSE)
      }
      # Drop one base from the 3' end (last base of the stop codon).
      if (rv$annotations$direction[selected()] == "+") {
        rv$annotations$pos2[selected()] <- rv$annotations$pos2[selected()] - 1L
      } else {
        rv$annotations$pos1[selected()] <- rv$annotations$pos1[selected()] + 1L
      }
      rv$annotations$length[selected()] <- rv$annotations$length[selected()] - 1L
      rv$annotations$stop_codon[selected()] <- new_stop
      show_edit_waiter()
      shinyjs::delay(50, {
        trigger("re_align")
      })
    })

    ## RE-align after edit ----
    init("re_align")
    on("re_align", {
      # rRNA has no protein hit stats to recompute; just rebuild the nt alignment.
      if (!identical(rv$annotations$type[selected()], "rRNA")) {
        ### Calculate new stats (on the full hit set; align_now slices for display)
        focal <- focal_for(selected())
        hits <-
          {
            rv$local_hits %||% json_parse(rv$annotations$refHits[align_hits_idx(selected())], T)
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

        # Persist recomputed stats so align_now and the "Max Similarity" header
        # read the fresh values (previously computed then discarded).
        if (!is.null(rv$local_hits)) {
          rv$local_hits <- hits
        } else {
          rv$annotations$refHits[align_hits_idx(selected())] <- json_string(hits)
        }
      }
      # Keep rv$alignment around (incl. cached ref_msa); align_now rebuilds it.
      trigger("align_now")
    })

    # Discard edits ----
    observeEvent(input$discard_edits, {
      # Restore the whole group for a join session, else just the edited row.
      drop_idx <- if (!is.null(rv$editing$join_grp)) {
        join_members(rv$editing$join_grp)
      } else {
        selected()
      }
      rv$annotations <- rv$annotations[-drop_idx, ] |>
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
      # Show overlay first, then defer the (blocking) stat recompute + DB writes
      # one tick so the "hold tight" message paints before work starts.
      show_edit_waiter("Saving, hold tight...")
      shinyjs::delay(100, {
        # finally = waiter_hide() guarantees the overlay clears even if the
        # recompute/write errors. (on.exit is unreliable inside shinyjs::delay.)
        tryCatch({
          ### Recompute per-hit stats against the edited focal sequence ----
          # recompute_hit_stats aligns each pair once (vectorized pwalign) instead
          # of the old row-by-row compare_aa/count_end_gaps (4 alignments per hit).
          # rRNA edits have no protein hits to recompute; just persist positions.
          if (!identical(rv$annotations$type[selected()], "rRNA")) {
            hits_idx <- align_hits_idx(selected())
            focal <- focal_for(selected())
            hits <- rv$local_hits %||% json_parse(rv$annotations$refHits[hits_idx], TRUE)
            stats <- recompute_hit_stats(focal, hits$target)
            hits <- hits |>
              dplyr::mutate(
                similarity = stats$similarity,
                pctid = stats$pctid,
                gap_leading = stats$gap_leading,
                gap_trailing = stats$gap_trailing,
                .after = "eval"
              ) |>
              dplyr::arrange(dplyr::desc(similarity))
            rv$annotations$refHits[hits_idx] <- json_string(hits)
          }

          persist_annotations()
          shinyjs::hide("edit_mode_ctrls")
          shinyjs::hide("discard_edits")
          shinyjs::hide("save_edits")
          shinyjs::show("edit_mode")
          rv$editing <- NULL
        }, finally = waiter::waiter_hide())
      })
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
          dplyr::select(ID, path, scaffold, curate_opts) |>
          dplyr::filter(ID == !!rv$updating$ID,
                        path == !!rv$updating$path,
                        scaffold == !!rv$updating$scaffold) |>
          dplyr::select(ID, curate_opts),
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

    observeEvent(list(input$merge_method, input$join_type), {
      is_join <- isTRUE(input$merge_method == "join")
      shinyjs::toggle("join_type_div", condition = is_join)
      shinyjs::toggle(
        "slippage_note_div",
        condition = is_join && isTRUE(input$join_type == "frameshift")
      )
    })

    # Persist rv$annotations to the DB (delete this unit's rows, re-insert).
    save_annotations <- function() {
      persist_annotations()
    }

    # Tag selected rows as a join group instead of collapsing them. Segments stay
    # as visible rows; export combines them into a single joined annotation.
    do_join_merge <- function(rows_to_merge, merge_anns, join_mode, slip_note = NULL) {
      # Unique group id = one past the max existing group in this sample. A
      # whole-second Sys.time() collides when two joins happen in the same second.
      existing_grps <- as.integer(
        stringr::str_match(rv$annotations$notes %|NA|% "", "group=(\\d+)")[, 2]
      )
      grp <- if (all(is.na(existing_grps))) 1L else max(existing_grps, na.rm = TRUE) + 1L
      marker <- stringr::str_glue("JOIN: mode={join_mode} group={grp}")
      # frameshift note travels to export in the marker; ";" would break the
      # "; "-joined notes field, so swap it for ","
      if (identical(join_mode, "frameshift") && !is.null(slip_note) && nzchar(trimws(slip_note))) {
        marker <- paste0(marker, " note=", stringr::str_replace_all(trimws(slip_note), ";", ","))
      }
      rv$annotations$notes[rows_to_merge] <- purrr::map_chr(
        rv$annotations$notes[rows_to_merge],
        ~ paste(marker, .x %|NA|% "", sep = "; ") |> stringr::str_remove("; $")
      )
      rv$annotations$edited[rows_to_merge] <- 1L
      rv$annotations$time_stamp[rows_to_merge] <- as.numeric(Sys.time())
      save_annotations()
      shinyjs::hide("merge_select_div")
      reactable::updateReactable("table", data = rv$annotations)
    }

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

      # Join mode: warn (intron rule + slippage), then tag as a join group. The
      # actual commit happens in confirm_join (or directly if no warnings apply).
      if (isTRUE(input$merge_method == "join")) {
        # Block joining features that are already part of a join; the user must
        # un-join them first (nested/overlapping join groups are not supported).
        if (any(stringr::str_detect(dplyr::coalesce(merge_anns$notes, ""), "^JOIN: "))) {
          shinyWidgets::sendSweetAlert(
            title = "Already joined",
            text = "One or more selected features are already part of a joined gene. Un-join them first before creating a new join.",
            type = "warning"
          )
          req(F)
        }
        join_mode <- input$join_type %||% "exon"
        sel_gene <- merge_anns$gene[1]
        sel_type <- merge_anns$type[1]
        warn_msgs <- character(0)
        if (!isTRUE(gene_allows_intron(sel_gene, sel_type))) {
          warn_msgs <- c(warn_msgs, stringr::str_glue(
            "{sel_gene} is not configured to allow introns/joined features in the curation rules. A joined export may be unexpected for this gene."
          ))
        }
        if (identical(join_mode, "frameshift")) {
          warn_msgs <- c(warn_msgs,
            "The 'ribosomal_slippage' exception may not be accepted by GenBank without further explanation or supporting evidence. Contact GenBank curation staff before submitting."
          )
        }
        slip_note <- if (identical(join_mode, "frameshift")) input$slippage_note else NULL
        pending_join(list(rows = rows_to_merge, anns = merge_anns, mode = join_mode, slip_note = slip_note))
        if (length(warn_msgs) > 0) {
          shinyWidgets::confirmSweetAlert(
            inputId = ns("confirm_join"),
            title = "Proceed with join?",
            text = paste(warn_msgs, collapse = " "),
            type = "warning",
            btn_labels = c("Cancel", "Join anyway")
          )
        } else {
          do_join_merge(rows_to_merge, merge_anns, join_mode, slip_note)
        }
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
              genetic.code = rv$gcode
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
              genetic.code = rv$gcode
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
      persist_annotations()
      shinyjs::hide("merge_select_div")
      reactable::updateReactable(
        "table",
        data = rv$annotations
      )
    })

    # TRUE if the gene's curation ruleset allows introns (joined/exon features).
    gene_allows_intron <- function(gene, type) {
      rule_type <- if (identical(type, "ORF")) "PCG" else type
      rules <- tryCatch(
        dplyr::left_join(
          dplyr::tbl(session$userData$con, "annotate") |>
            dplyr::select(ID, path, scaffold, curate_opts) |>
            dplyr::filter(ID == !!rv$updating$ID,
                          path == !!rv$updating$path,
                          scaffold == !!rv$updating$scaffold) |>
            dplyr::select(ID, curate_opts),
          dplyr::tbl(session$userData$con, "curate_opts"),
          by = "curate_opts"
        ) |>
          dplyr::pull(params) |>
          json_parse(),
        error = function(e) NULL
      )
      if (is.null(rules)) return(FALSE)
      merged <- modifyList(
        rules$default_rules[[rule_type]] %||% list(),
        rules$rules[[gene]] %||% list()
      )
      isTRUE(merged$intron)
    }

    observeEvent(input$confirm_join, {
      pj <- pending_join()
      if (isTRUE(input$confirm_join) && !is.null(pj)) {
        do_join_merge(pj$rows, pj$anns, pj$mode, pj$slip_note)
      }
      pending_join(NULL)
    })

    observeEvent(input$unjoin, {
      req(length(selected()) > 0)
      sel_notes <- rv$annotations$notes[selected()] %|NA|% ""
      grp <- stringr::str_match(sel_notes, "^JOIN: mode=\\w+ group=(\\d+)")[, 2]
      req(!is.na(grp))
      grp_idx <- which(stringr::str_detect(
        dplyr::coalesce(rv$annotations$notes, ""),
        paste0("^JOIN: mode=\\w+ group=", grp, "\\b")
      ))
      rv$annotations$notes[grp_idx] <- stringr::str_remove(
        rv$annotations$notes[grp_idx],
        "^JOIN: mode=\\w+ group=\\d+( note=[^;]*)?(; )?"
      )
      rv$annotations$edited[grp_idx] <- 1L
      rv$annotations$time_stamp[grp_idx] <- as.numeric(Sys.time())
      save_annotations()
      reactable::updateReactable("table", data = rv$annotations)
    })

    # Restore Annotation ----
    restore_do_save <- function() {
      persist_annotations()
      reactable::updateReactable("table", data = rv$annotations)
    }

    # Reopen the details modal and force the annotations table to re-render with
    # current rv$annotations (the render isolates rv$annotations, gated on
    # render_annotations_table; see the normal open path).
    reopen_details <- function() {
      annotate_details_modal(rv) |> showModal()
      render_annotations_table(Sys.time())
    }

    # Apply an ORF -> gene assignment to one annotation row of rv$annotations:
    # sets gene/type/product, appends a "{old} assigned to {gene}" note (so the
    # original ORF.N can be recovered on removal), and flags it edited. Does not
    # save or refresh. Shared by confirm_assign_gene and the bulk auto-assign.
    apply_gene_assignment <- function(idx, gene) {
      old_gene <- rv$annotations$gene[idx]
      rv$annotations$gene[idx] <- gene
      rv$annotations$type[idx] <- mito_gene_type(gene)
      # `[` (not `[[`) so a custom gene absent from CDS_key yields NA.
      rv$annotations$product[idx] <- unname(CDS_key[gene]) %|NA|% NA_character_
      note <- stringr::str_glue("{old_gene} assigned to {gene}")
      existing <- rv$annotations$notes[idx] %|NA|% ""
      rv$annotations$notes[idx] <- if (nzchar(existing)) paste(note, existing, sep = "; ") else note
      rv$annotations$edited[idx] <- 1L
    }

    # Lightweight count refresh after an in-modal annotation edit (delete, ORF
    # gene assignment, merge, restore, ...). Recomputes the annotate-table count
    # columns (PCG/tRNA/rRNA/ORF) plus missing/extra from the current
    # annotations, persists them, and propagates to the annotate and export
    # tables. Deliberately NOT a full re-validation.
    update_counts <- function() {
      con <- session$userData$con
      id <- rv$updating$ID
      p  <- rv$updating$path
      s  <- rv$updating$scaffold
      req(length(id) == 1)
      retained <- rv$annotations |>
        dplyr::filter(!stringr::str_detect(gene, "_DELETED_"))
      pcg  <- sum(retained$type == "PCG",  na.rm = TRUE)
      trna <- sum(retained$type == "tRNA", na.rm = TRUE)
      rrna <- sum(retained$type == "rRNA", na.rm = TRUE)
      # missing/extra from this unit's curation count rules
      me <- tryCatch({
        co <- dplyr::tbl(con, "annotate") |>
          dplyr::filter(ID == !!id, path == !!p, scaffold == !!s) |>
          dplyr::pull(curate_opts)
        pj <- dplyr::tbl(con, "curate_opts") |>
          dplyr::filter(curate_opts == !!co) |> dplyr::pull(params)
        params <- jsonlite::fromJSON(pj)
        compute_missing_extra(retained, params$rules, params$default_rules)
      }, error = function(e) list(missing = NA_character_, extra = NA_character_))
      # This unit's row in rv$data (per-unit table).
      i <- which(rv$data$ID == id & rv$data$path == p & rv$data$scaffold == s)[1]
      # ORF count is derived in the table; keep blank when ORF finding is off
      orf_blank <- "ORFCount" %in% names(rv$data) && !is.na(i) &&
        isTRUE(is.na(rv$data$ORFCount[i]))
      orf <- if (orf_blank) NA_integer_ else as.integer(sum(retained$type == "ORF", na.rm = TRUE))
      # Short-circuit if nothing the count/missing/extra columns track changed
      # (e.g. position-only codon edits) to avoid needless writes/refreshes.
      same <- function(a, b) (is.na(a) && is.na(b)) || (!is.na(a) && !is.na(b) && a == b)
      if (!is.na(i) &&
          identical(as.integer(rv$data$PCGCount[i]), as.integer(pcg)) &&
          identical(as.integer(rv$data$tRNACount[i]), as.integer(trna)) &&
          identical(as.integer(rv$data$rRNACount[i]), as.integer(rrna)) &&
          same(rv$data$ORFCount[i], orf) &&
          same(rv$data$missing[i], me$missing %||% NA_character_) &&
          same(rv$data$extra[i], me$extra %||% NA_character_)) {
        return(invisible(NULL))
      }
      # Persist the stored count columns to this unit's annotate row
      upd <- data.frame(
        ID = id, path = p, scaffold = s,
        PCGCount = pcg, tRNACount = trna, rRNACount = rrna,
        missing = me$missing %||% NA_character_, extra = me$extra %||% NA_character_,
        stringsAsFactors = FALSE
      )
      dplyr::tbl(con, "annotate") |>
        dplyr::rows_update(upd, by = c("ID", "path", "scaffold"), unmatched = "ignore",
                           in_place = TRUE, copy = TRUE)
      # Keep rv$updating in sync (the close handler reads these counts)
      rv$updating$PCGCount  <- pcg
      rv$updating$tRNACount <- trna
      rv$updating$rRNACount <- rrna
      rv$data <- rv$data |>
        dplyr::rows_update(
          data.frame(ID = id, path = p, scaffold = s,
                     PCGCount = pcg, tRNACount = trna, rRNACount = rrna,
                     ORFCount = orf, missing = upd$missing, extra = upd$extra,
                     stringsAsFactors = FALSE),
          by = c("ID", "path", "scaffold")
        )
      trigger("update_annotate_table")
      trigger("refresh_export")
    }

    # Recompute counts whenever the annotations change from an edit. The initial
    # load already carries validated counts, so skip that first set.
    observeEvent(rv$annotations, {
      if (skip_count_update) {
        skip_count_update <<- FALSE
        return()
      }
      update_counts()
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # Assign a gene name to an ORF ----
    # Relabels the selected ORF with a chosen (or custom) mitochondrial gene
    # name, sets the corresponding feature type, flags it as edited, and records
    # a note. Does NOT re-run curation (start/stop trimming, refHit rules).
    observeEvent(input$assign_gene, {
      req(length(selected()) > 0)
      sel_type <- rv$annotations$type[selected()]
      is_assigned <- isTRUE(rv$annotations$tool[selected()] == "ORFfinder") &&
        isTRUE(sel_type != "ORF")
      req(sel_type == "ORF" || is_assigned)
      cur_gene <- rv$annotations$gene[selected()]

      # For an unassigned ORF, suggest a gene from its stored BLAST hits.
      guess <- if (!is_assigned) {
        guess_orf_gene(rv$annotations$refHits[selected()])
      } else NULL
      choices <- if (is_assigned) {
        union(MITO_PCG_GENES, cur_gene)
      } else if (!is.null(guess)) {
        union(MITO_PCG_GENES, guess$gene)
      } else {
        MITO_PCG_GENES
      }
      selected_choice <- if (is_assigned) cur_gene else if (!is.null(guess)) guess$gene else character(0)
      suggestion_ui <- if (is_assigned) {
        NULL
      } else if (!is.null(guess)) {
        txt <- stringr::str_glue("Suggested: {guess$gene} - {round(guess$similarity, 1)}% similarity")
        if (!is.na(guess$taxon)) txt <- paste0(txt, " to ", guess$taxon)
        helpText(txt)
      } else {
        helpText("No confident BLAST match - pick a gene name manually.")
      }

      showModal(modalDialog(
        title = if (is_assigned) "Edit ORF gene assignment" else "Assign gene name to ORF",
        selectizeInput(
          ns("assign_gene_choice"),
          label = "Gene name (pick a standard mitochondrial PCG or type a custom name):",
          choices = choices,
          selected = selected_choice,
          options = list(create = TRUE, maxItems = 1, placeholder = "e.g. nad6 or a custom name")
        ),
        suggestion_ui,
        footer = tagList(
          actionButton(ns("confirm_assign_gene"), "Assign"),
          if (is_assigned) actionButton(ns("remove_assign_gene"), "Remove assignment"),
          actionButton(ns("cancel_assign_gene"), "Cancel")
        ),
        easyClose = TRUE
      ))
    })
    observeEvent(input$confirm_assign_gene, {
      req(length(selected()) > 0)
      gene <- input$assign_gene_choice
      req(!is.null(gene))
      gene <- trimws(gene)
      req(nzchar(gene))
      # Restrict gene names to characters that are safe in feature tables, GFF
      # attributes, FASTA headers, and the export's shell/file paths (which embed
      # the gene name via system("cat ...") and file paths).
      if (!grepl("^[A-Za-z0-9_.-]+$", gene)) {
        shinyWidgets::sendSweetAlert(
          title = "Invalid gene name",
          text = paste(
            "Gene names may only contain letters, numbers, underscores, dots,",
            "and hyphens (no spaces or other special characters)."
          )
        )
        req(F)
      }
      idx <- selected()
      # Guard against composite-PK collision (same scaffold + gene + pos1)
      collision <- which(
        rv$annotations$gene == gene &
          rv$annotations$pos1 == rv$annotations$pos1[idx] &
          rv$annotations$scaffold == rv$annotations$scaffold[idx]
      )
      collision <- setdiff(collision, idx)
      if (length(collision) > 0) {
        shinyWidgets::sendSweetAlert(
          title = "Cannot assign",
          text = stringr::str_glue("An annotation named '{gene}' already exists at this position.")
        )
        req(F)
      }
      apply_gene_assignment(idx, gene)
      restore_do_save()
      reopen_details()
    })

    # Remove a gene assignment, reverting the feature to an unassigned ORF.
    observeEvent(input$remove_assign_gene, {
      req(length(selected()) > 0)
      idx <- selected()
      notes_cur <- rv$annotations$notes[idx] %|NA|% ""
      # Recover the original ORF.N name recorded in the assignment note.
      orig <- stringr::str_match(notes_cur, "(ORF\\.\\d+) assigned to")[, 2]
      if (is.na(orig)) {
        shinyWidgets::sendSweetAlert(
          title = "Cannot remove assignment",
          text = "Could not determine the original ORF name from the annotation notes."
        )
        req(F)
      }
      kept <- strsplit(notes_cur, "; ", fixed = TRUE)[[1]]
      kept <- kept[!stringr::str_detect(kept, " assigned to ")]
      rv$annotations$gene[idx] <- orig
      rv$annotations$type[idx] <- "ORF"
      rv$annotations$product[idx] <- NA_character_
      rv$annotations$notes[idx] <- if (length(kept)) paste(kept, collapse = "; ") else NA_character_
      rv$annotations$edited[idx] <- 1L
      restore_do_save()
      reopen_details()
    })

    # Cancel: return to the annotation details modal (not the annotate table).
    observeEvent(input$cancel_assign_gene, {
      reopen_details()
    })

    # Bulk auto-assign: confirm, then apply BLAST-based guesses to every
    # unassigned ORF whose top hit clears the similarity threshold.
    observeEvent(input$auto_assign_orfs, {
      req(rv$annotations)
      orf_idx <- which(
        rv$annotations$type == "ORF" &
          !stringr::str_detect(dplyr::coalesce(rv$annotations$gene, ""), "_DELETED_")
      )
      if (length(orf_idx) == 0) {
        shinyWidgets::sendSweetAlert(
          title = "No ORFs to assign",
          text = "There are no unassigned ORF annotations in this sample."
        )
        return()
      }
      shinyWidgets::confirmSweetAlert(
        inputId = ns("confirm_auto_assign_orfs"),
        title = "Auto-assign ORF gene names?",
        text = stringr::str_glue(
          "Each unassigned ORF with a confident BLAST match (>= {ORF_ASSIGN_SIM_THRESHOLD}% ",
          "similarity to a standard mitochondrial gene) will be relabeled. ",
          "Low-confidence ORFs are left unchanged. You can undo any assignment ",
          "individually via 'Remove assignment'."
        ),
        btn_colors = c("#6c757d", "#0056b3")
      )
    })
    observeEvent(input$confirm_auto_assign_orfs, {
      req(isTRUE(input$confirm_auto_assign_orfs))
      req(rv$annotations)
      orf_idx <- which(
        rv$annotations$type == "ORF" &
          !stringr::str_detect(dplyr::coalesce(rv$annotations$gene, ""), "_DELETED_")
      )
      assigned <- 0L
      for (idx in orf_idx) {
        guess <- guess_orf_gene(rv$annotations$refHits[idx])
        if (is.null(guess)) next
        apply_gene_assignment(idx, guess$gene)
        assigned <- assigned + 1L
      }
      left <- length(orf_idx) - assigned
      if (assigned > 0L) {
        restore_do_save()
      }
      reopen_details()
      shinyWidgets::sendSweetAlert(
        title = "Auto-assign complete",
        text = stringr::str_glue("Assigned {assigned} ORF{ifelse(assigned == 1, '', 's')}; {left} left unassigned."),
        type = if (assigned > 0L) "success" else "info"
      )
    })

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
              genetic.code = rv$gcode
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
              genetic.code = rv$gcode
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
    uiOutput(ns("outlier_flag_banner")),
    tags$details(
      id = ns("annotation_table_details"),
      open = TRUE,
      tags$summary("Annotation Table"),
      reactableOutput(ns("table"), width = "100%")
    ),
    div(
      id = ns("annotation_btns_wrapper"),
      div(
        style = "display: flex; align-items: center; gap: 8px; margin: 6px 0;",
        actionButton(
          ns("auto_assign_orfs"),
          "Auto-assign ORFs",
          icon = icon("wand-magic-sparkles")
        ),
        shinyjs::hidden(
          div(
            id = ns("annotation_action_btns"),
            style = "display: contents;",
            actionButton(ns("merge"), "Merge PCGs/rRNAs"),
            shinyjs::hidden(
              div(
                id = ns("unjoin_btn"),
                style = "display: contents;",
                actionButton(ns("unjoin"), "Un-join")
              )
            ),
            actionButton(ns("delete"), "Delete"),
            shinyjs::hidden(
              div(
                id = ns("assign_gene_btn"),
                actionButton(ns("assign_gene"), "Assign gene name")
              )
            )
          )
        ),
        shinyjs::hidden(
          div(
            id = ns("annotation_restore_btn"),
            style = "display: contents;",
            actionButton(ns("restore"), "Restore")
          )
        )
      ),
      shinyjs::hidden(
        div(
          id = ns("merge_select_div"),
        style = "border: 1px solid #ccc; border-radius: 4px; padding: 10px; margin: 6px 0;",
        tags$b("Select annotations to merge:"),
        uiOutput(ns("merge_choices")),
        radioButtons(
          ns("merge_method"),
          "Merge method",
          choices = c(
            "Span region (single annotation)" = "span",
            "Join segments (joined feature)" = "join"
          ),
          selected = "span"
        ),
        shinyjs::hidden(
          div(
            id = ns("join_type_div"),
            radioButtons(
              ns("join_type"),
              "Join type",
              choices = c(
                "True exons (spliced)" = "exon",
                "Translational frameshift / RNA editing" = "frameshift"
              ),
              selected = "exon"
            )
          )
        ),
        shinyjs::hidden(
          div(
            id = ns("slippage_note_div"),
            textInput(
              ns("slippage_note"),
              "Note (added with ribosomal_slippage exception)",
              value = "frameshift mechanism unknown"
            )
          )
        ),
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
            class = "mp-edit-ctrls",
            style = "display: flex; flex-flow: row nowrap; align-items: center; gap: 1.5em;",
            # Separate step boxes (codons per click) for START and STOP, each
            # clamped to [1, 50] both in the UI and server-side. The - / +
            # buttons sit to the left of each box; all on a single row.
            # Zero the numericInput's default bottom margin and the pretty
            # checkbox's vertical margin so every element lines up vertically.
            tags$style(HTML(stringr::str_glue(
              ".mp-step-box .form-group {{ margin-bottom: 0; }}",
              ".mp-step-box .form-control {{ height: 28px; padding: 2px 4px; }}",
              # Bordered, captioned box around a group of edit buttons so the
              # codon-search and nucleotide-junction controls are distinguishable.
              ".mp-edit-group {{ border: 1px solid #ccc; border-radius: 5px; ",
              "  padding: 10px 8px 6px; position: relative; }}",
              ".mp-edit-group > .mp-edit-group-label {{ position: absolute; ",
              "  top: -9px; left: 8px; background: #fff; padding: 0 4px; ",
              "  font-size: 12px; font-weight: bold; color: #666; }}"
            ))),
            # Codon START/STOP search controls (PCG/ORF only); hidden for rRNA,
            # which uses the nucleotide-boundary editor instead.
            div(
              id = ns("codon_edit_ctrls"),
              class = "mp-edit-group",
              style = "display: flex; flex-flow: row nowrap; align-items: center; gap: 1.5em;",
            tags$span(class = "mp-edit-group-label", "Codon edit"),
            div(
              id = ns("start_search_ctrl"),
              style = "display: flex; flex-flow: row nowrap; align-items: center; gap: 0.4em;",
              tags$span(style = "font-weight: bold;", "START"),
              tags$button(
                class = "icon-circle grow",
                onclick = stringr::str_glue("Shiny.setInputValue('{ns('start-add')}', 'plus', {{priority: 'event'}})"),
                tags$span(style = "font-size: 0.75em;", "+")
              ),
              tags$button(
                class = "icon-circle grow",
                onclick = stringr::str_glue("Shiny.setInputValue('{ns('start-minus')}', 'minus', {{priority: 'event'}})"),
                tags$span(style = "font-size: 0.75em;", "\u2212")
              ),
              div(
                class = "mp-step-box",
                style = "width: 48px;",
                numericInput(
                  ns("start_step_size"),
                  label = NULL,
                  value = 1,
                  min = 1,
                  max = 50,
                  step = 1,
                  width = "48px"
                )
              )
            ),
            tags$label(
              style = paste(
                "display: flex; align-items: center; gap: 4px;",
                "margin: 0; font-weight: normal; cursor: pointer;"
              ),
              tags$input(
                type = "checkbox",
                style = "margin: 0; vertical-align: middle;",
                onchange = stringr::str_glue(
                  "Shiny.setInputValue('{ns('single_codon')}', this.checked, {{priority: 'event'}})"
                )
              ),
              "single codon"
            ),
            div(
              id = ns("stop_search_ctrl"),
              style = "display: flex; flex-flow: row nowrap; align-items: center; gap: 0.4em;",
              tags$span(style = "font-weight: bold;", "STOP"),
              tags$button(
                class = "icon-circle grow",
                onclick = stringr::str_glue("Shiny.setInputValue('{ns('stop-minus')}', 'minus', {{priority: 'event'}})"),
                tags$span(style = "font-size: 0.75em;", "\u2212")
              ),
              tags$button(
                class = "icon-circle grow",
                onclick = stringr::str_glue("Shiny.setInputValue('{ns('stop-add')}', 'plus', {{priority: 'event'}})"),
                tags$span(style = "font-size: 0.75em;", "+")
              ),
              div(
                class = "mp-step-box",
                style = "width: 48px;",
                numericInput(
                  ns("stop_step_size"),
                  label = NULL,
                  value = 1,
                  min = 1,
                  max = 50,
                  step = 1,
                  width = "48px"
                )
              )
            )
            ),
            # rRNA nucleotide-boundary nudge controls (shown for rRNA only); kept
            # left of the partial buttons.
            uiOutput(ns("rrna_edit_ctrls"), inline = TRUE),
            # Manual partial-end flags + poly-A stop trim (PCG/rRNA). Rendered
            # reactively so button state reflects the selected gene.
            uiOutput(ns("partial_ctrls"), inline = TRUE)
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
          uiOutput(ns("join_preview")),
          uiOutput(ns("junction_context")),
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
      if (isTRUE(session$userData$in_outlier_review)) {
        actionButton(
          ns("back_to_review"), "Back to Review",
          icon = icon("arrow-left"),
          class = "btn-success",
          style = "float: left;"
        )
      },
      uiOutput(ns("status_toggles"), inline = TRUE),
      actionButton(ns("linearize"), "Linearize"),
      actionButton(ns("lock"), "Lock & Close"),
      actionButton(ns("close"), "Close")
    )
  )
}
