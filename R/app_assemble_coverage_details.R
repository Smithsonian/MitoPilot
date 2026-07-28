#' coverage_details Server Functions
#'
#' @noRd
assembly_coverage_details_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    init("coverage_modal")

    # Reads and writes below rebuild the published output path from
    # assemble_opts, so it is absent when the option set was reassigned or out/
    # was moved. Report it, never create it: a directory holding only the Path 0
    # files would look complete while the raw path assemblies stay orphaned.
    require_assemble_output <- function(pth) {
      if (length(pth) == 1L && !is.na(pth) && nzchar(pth) && file.exists(pth)) {
        return(TRUE)
      }
      on_disk <- tryCatch(
        assemble_dirs_on_disk(session$userData$dir_out, rv$updating$ID),
        error = function(e) character(0)
      )
      shinyWidgets::sendSweetAlert(
        title = "Assembly output not found",
        text = tags$div(
          tags$p(
            "The assembly output for ", tags$b(rv$updating$ID),
            " is not where the project database expects it:"
          ),
          tags$ul(tags$li(tags$code(pth))),
          if (length(on_disk) > 0) {
            tags$p(
              "Assembly parameter sets published on disk: ",
              tags$code(paste(on_disk, collapse = ", "))
            )
          } else {
            tags$p("No assembly output is published for this sample.")
          },
          tags$p(
            "Re-run Assembly for this sample, or set its assembly parameter ",
            "set back to the name that exists on disk."
          )
        ),
        html = TRUE,
        type = "error"
      )
      FALSE
    }

    # "How do I choose?" guidance modal (reopens this modal on close).
    register_tool_help("assembly_paths", input,
                       reopen = function() trigger("coverage_modal"))

    on("coverage_modal", {
      rv$alignment <- NULL
      # Navigation state kept separate from rv$alignment so block changes don't
      # invalidate (and re-render) the msaR widget, which would reset its scroll.
      rv$cur_block <- 0L
      rv$focal_assembly <- dplyr::tbl(session$userData$con, "assemblies") |>
        dplyr::filter(ID == !!rv$updating$ID) |>
        dplyr::select(
          ignore, ID, path, scaffold, topology, length_raw, length, sequence, depth, gc, errors,
          dplyr::any_of(c("blast_accession", "blast_species", "blast_pident", "blast_qcovs", "blast_evalue", "blast_lineage"))
        ) |>
        dplyr::collect() |>
        dplyr::mutate(
          view_coverage = NA_character_
        )

      # Multi-scaffold guard. A single fragmented path is "join eligible" (the
      # scaffold-join tools handle it); multiple competing fragmented paths stay
      # blocked (ambiguous which path's scaffolds to join). Both still disable the
      # MSA/consensus Align tools, which only compare complete single-scaffold paths.
      raw_asmb <- rv$focal_assembly |> dplyr::filter(path > 0)
      rv$asmb_join_eligible <- scaffold_join_eligible(raw_asmb)
      rv$asmb_multiscaffold_blocked <-
        nrow(raw_asmb) > 0 && max(table(raw_asmb$path)) > 1 && !rv$asmb_join_eligible
      rv$asmb_multiscaffold <- rv$asmb_join_eligible || rv$asmb_multiscaffold_blocked
      rv$join_layout <- NULL
      rv$join_ref_len <- NULL
      rv$join_zoom_anchor <- NULL
      # Auto-run reference-guided mapping on open so the layout + mapping plot show
      # without an extra click; the Auto-layout button re-runs after edits.
      if (isTRUE(rv$asmb_join_eligible)) {
        compute_join_layout(choose_reference(raw_asmb), notify = FALSE)
      }
      rv$asmb_dir <- file.path(
        session$userData$dir_out, rv$updating$ID, "assemble",
        rv$updating$assemble_opts
      )

      # Score paths (recommend-only) ----
      # Read each path's coverageStats CSV (all scaffolds) to feed depth/error
      # into the scoring engine, then merge per-path score/flags onto the table.
      cov_by_path <- {
        paths <- unique(rv$focal_assembly$path)
        stats <- lapply(paths, function(p) {
          f <- file.path(
            session$userData$dir_out, rv$updating$ID, "assemble",
            rv$updating$assemble_opts,
            paste0(rv$updating$ID, "_assembly_", p, "_coverageStats.csv")
          )
          if (file.exists(f)) {
            tryCatch(read.csv(f, stringsAsFactors = FALSE), error = function(e) NULL)
          } else NULL
        })
        stats::setNames(stats, as.character(paths))
      }
      rv$path_scores <- tryCatch(
        score_assembly_paths(rv$focal_assembly, cov_by_path),
        error = function(e) data.frame()
      )
      if (nrow(rv$path_scores) > 0) {
        m <- rv$path_scores[match(rv$focal_assembly$path, rv$path_scores$path), ]
        rv$focal_assembly$path_flags <- m$flags
      } else {
        rv$focal_assembly$path_flags <- NA_character_
      }
      # Path 0 is the edited/consensus sequence, not a raw assembler path; no flags.
      rv$focal_assembly$path_flags[rv$focal_assembly$path == 0] <- ""
      rv$focal_assembly <- dplyr::relocate(
        rv$focal_assembly, path_flags,
        .after = scaffold
      )

      modalDialog(
        title = tagList(
          div(stringr::str_glue("Assembly details for ID: {rv$updating$ID}")),
          div(
            style = "font-size: 0.85em; font-weight: normal; color: #555; margin-top: 4px;",
            stringr::str_glue("Taxon: {rv$updating$Taxon %|NA|% 'NA'}")
          )
        ),
        size = "l",
        if (isTRUE(rv$asmb_multiscaffold_blocked)) {
          div(
            style = paste("margin-bottom: 12px; padding: 10px; border: 1px solid #E55330;",
                          "border-radius: 4px; background: #fdf3f0; font-size: 0.9em;"),
            tags$b("This assembly has multiple competing paths, each fragmented into multiple scaffolds."),
            div(style = "margin-top: 6px;",
                paste("Automatic scaffold joining is only supported for a single fragmented",
                      "path. The alignment and conflict-review tools only compare complete",
                      "single-scaffold paths, so neither tool applies here.")),
            div(style = "margin-top: 6px;",
                paste("To annotate this sample, ignore all but one scaffold using the",
                      "ignore buttons in the table below; the single remaining scaffold",
                      "becomes the assembly used for annotation. Otherwise inspect the",
                      "assembly outputs manually:")),
            tags$code(style = "display: block; margin-top: 6px; word-break: break-all;",
                      rv$asmb_dir)
          )
        },
        div(
          style = "margin-bottom: 8px; font-size: 0.9em; color: #555;",
          "Multiple assembly paths? ",
          actionLink(
            ns("help_assembly_paths"),
            label = "How do I choose?",
            icon = icon("circle-question")
          )
        ),
        reactableOutput(ns("table"), width = "100%"),
        uiOutput(ns("consensus_admin")),
        uiOutput(ns("scaffold_join_div")),
        uiOutput(ns("msa_div")),
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
              name = "Ignore",
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
            path_flags = colDef(
              name = "Flags", minWidth = 200, resizable = TRUE, align = "left",
              html = TRUE, cell = rt_longtext()
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
            depth = colDef(show = FALSE),
            gc = colDef(show = FALSE),
            errors = colDef(show = FALSE),
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
            blast_evalue = colDef(
              name = "E-value", width = 90, align = "center"
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
      shinyjs::toggle("clip", condition = length(selected()) > 0)
      shinyjs::toggle("align", condition = !isTRUE(rv$asmb_multiscaffold) && length(selected()) > 1)
      shinyjs::toggle("msa_div", condition = !isTRUE(rv$asmb_multiscaffold) && length(selected()) > 1)
    })

    # Keep the per-unit annotate row in sync when a scaffold's ignore flag is
    # toggled (multi-assembly). Un-ignore seeds a row mirroring the assemble-time
    # seed (opts inherited from the sample's min-path annotate row); ignore prunes
    # it, so ignored units never carry a stale annotate/annotation unit.
    reconcile_annotate_ignore <- function(ID, path, scaffold, ignore) {
      con <- session$userData$con
      if (as.integer(ignore) == 1L) {
        DBI::dbExecute(
          con,
          "DELETE FROM annotate WHERE ID = ? AND path = ? AND scaffold = ?",
          params = list(ID, path, scaffold)
        )
      } else {
        DBI::dbExecute(
          con,
          paste0(
            "INSERT OR IGNORE INTO annotate ",
            "(ID, path, scaffold, topology, partial, annotate_opts, curate_opts, ",
            "orf_opts, annotate_switch, annotate_lock, reviewed) ",
            "SELECT asm.ID, asm.path, asm.scaffold, asm.topology, ",
            "CASE WHEN asm.topology = 'circular' OR co.linear_complete = 1 ",
            "THEN 'no' ELSE 'yes' END, ",
            "an.annotate_opts, an.curate_opts, an.orf_opts, 1, 0, 'no' ",
            "FROM assemblies asm ",
            "LEFT JOIN (SELECT ID, annotate_opts, curate_opts, orf_opts, MIN(path) ",
            "FROM annotate GROUP BY ID) an ON an.ID = asm.ID ",
            "LEFT JOIN curate_opts co ON co.curate_opts = an.curate_opts ",
            "WHERE asm.ID = ? AND asm.path = ? AND asm.scaffold = ?"
          ),
          params = list(ID, path, scaffold)
        )
      }
    }

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
      reconcile_annotate_ignore(
        rv$focal_assembly$ID[row],
        rv$focal_assembly$path[row],
        rv$focal_assembly$scaffold[row],
        rv$focal_assembly$ignore[row]
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
      pdf_path <- file.path(
        dirname(getOption("MitoPilot.db") %||% "."),
        "out", rv$updating$ID,
        "assemble", rv$updating$assemble_opts,
        paste0(rv$updating$ID, "_assembly_",
               rv$focal_assembly$path[row], "_",
               rv$focal_assembly$scaffold[row], "_coverage.pdf")
      )
      req(require_assemble_output(pdf_path))
      # browseURL() errors when no browser is configured (headless/server).
      tryCatch(browseURL(pdf_path), error = function(e) {
        shiny::showNotification(
          paste0("Cannot open a PDF viewer from this session. Path: ", pdf_path),
          type = "warning",
          duration = 10
        )
      })
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
      wait_align$show()
      seqs <- rv$focal_assembly$sequence[selected()] |> Biostrings::DNAStringSet()
      path_labels <- purrr::map_chr(selected(), ~ {
        p <- rv$focal_assembly$path[.x]
        s <- rv$focal_assembly$scaffold[.x]
        if (is.na(s) || s == 0) paste0("P", p) else paste0("P", p, ".S", s)
      })
      names(seqs) <- make.unique(path_labels)
      rv$alignment$seqs <- DECIPHER::AlignSeqs(seqs, verbose = F, processors = NULL)
      dists <- DECIPHER::DistanceMatrix(
        rv$alignment$seqs,
        includeTerminalGaps = T,
        type = "dist",
        verbose = FALSE
      ) |>
        range()
      if (dists[1] != dists[2]) {
        rv$alignment$pct_id_range <- stringr::str_glue(
          "Pairwise similarity: {round(100-100*dists[1],4)}% - {round(100-100*dists[2],4)}%"
        )
      } else {
        rv$alignment$pct_id_range <- stringr::str_glue(
          "Pairwise similarity: {round(100-100*dists[1],4)}%"
        )
      }

      # Run length encoded aligned positions
      align_pos <- rv$alignment$seqs |>
        as.matrix() |>
        apply(2, function(x) {
          length(unique(x)) == 1
        })
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

      # Conflict block detection ----
      # `align_pos` is TRUE where all selected paths agree at that column.
      # Conflict = mismatch run (any disagreement, includes gaps) of >= min_len.
      min_block_len <- 1L
      aln_mat <- rv$alignment$seqs |> as.matrix()
      aln_len <- ncol(aln_mat)
      conflict_pos <- !align_pos
      cf_rle <- rle(conflict_pos)
      cf_ends <- cumsum(cf_rle$lengths)
      cf_starts <- c(1L, head(cf_ends, -1L) + 1L)
      keep <- cf_rle$values & cf_rle$lengths >= min_block_len
      if (any(keep)) {
        starts <- cf_starts[keep]
        ends   <- cf_ends[keep]
        block_stats <- purrr::map2_dfr(starts, ends, function(s, e) {
          sub <- aln_mat[, s:e, drop = FALSE]
          # SNP cols: no gaps, >1 distinct base. Indel cols: any gap.
          gap_cols <- apply(sub, 2, function(x) any(x == "-"))
          var_cols <- apply(sub, 2, function(x) length(unique(toupper(x))) > 1)
          data.frame(
            aln_start = s,
            aln_end   = e,
            len       = e - s + 1L,
            n_indels  = sum(gap_cols & var_cols),
            n_snps    = sum(!gap_cols & var_cols)
          )
        })
        block_stats$block_id <- seq_len(nrow(block_stats))
        rv$alignment$conflicts <- block_stats
      } else {
        rv$alignment$conflicts <- data.frame(
          block_id = integer(), aln_start = integer(), aln_end = integer(),
          len = integer(), n_indels = integer(), n_snps = integer()
        )
      }
      rv$alignment$aln_mat <- aln_mat
      rv$alignment$aln_len <- aln_len

      # Project per-path coverage stats onto MSA columns ----
      sel_rows <- selected()
      # Helper: align a per-position vector to MSA columns using gap positions.
      align_to_msa <- function(vec, i) {
        if (is.null(vec) || length(vec) == 0L) return(rep(NA_real_, aln_len))
        aln_chars <- aln_mat[i, ]
        nongap_cols <- which(aln_chars != "-")
        out <- rep(NA_real_, aln_len)
        n <- min(length(vec), length(nongap_cols))
        out[nongap_cols[seq_len(n)]] <- vec[seq_len(n)]
        out
      }
      stats_per_path <- purrr::map(seq_along(sel_rows), function(i) {
        idx <- sel_rows[i]
        # Try DB-stored depth first (single-value-per-position string)
        depth_raw <- rv$focal_assembly$depth[idx]
        d_db <- NULL
        if (!is.na(depth_raw) && nzchar(depth_raw)) {
          d_db <- suppressWarnings(as.numeric(strsplit(depth_raw, " ")[[1]]))
          if (length(d_db) < 2L) d_db <- NULL
        }
        # Always try to read coverageStats CSV for richer stats (Correct, ErrorRate).
        p <- rv$focal_assembly$path[idx]
        s <- rv$focal_assembly$scaffold[idx]
        csv_path <- file.path(
          session$userData$dir_out,
          rv$updating$ID,
          "assemble",
          rv$updating$assemble_opts,
          paste0(rv$updating$ID, "_assembly_", p, "_coverageStats.csv")
        )
        cov <- NULL
        if (file.exists(csv_path)) {
          cov <- tryCatch(read.csv(csv_path, stringsAsFactors = FALSE),
                          error = function(e) NULL)
          if (!is.null(cov) && "SeqId" %in% names(cov)) {
            seq_id <- paste(rv$updating$ID, p, s, sep = ".")
            cov_path <- cov[cov$SeqId == seq_id, , drop = FALSE]
            if (nrow(cov_path) > 0) cov <- cov_path
          }
        }
        d <- d_db
        corr <- NULL
        err <- NULL
        gc <- NULL
        if (!is.null(cov) && "Depth" %in% names(cov)) {
          if (is.null(d)) d <- suppressWarnings(as.numeric(cov$Depth))
          if ("Correct" %in% names(cov)) {
            corr <- suppressWarnings(as.numeric(cov$Correct))
          }
          if ("ErrorRate" %in% names(cov)) {
            # ErrorRate is stored as character with '#' prefix when masked.
            err_chr <- sub("^#", "", as.character(cov$ErrorRate))
            err <- suppressWarnings(as.numeric(err_chr))
          }
          if ("GC" %in% names(cov)) gc <- suppressWarnings(as.numeric(cov$GC))
        }
        list(
          depth   = align_to_msa(d, i),
          correct = align_to_msa(corr, i),
          error   = align_to_msa(err, i),
          gc      = align_to_msa(gc, i)
        )
      })
      nm <- rownames(aln_mat)
      rv$alignment$depth_aligned   <- setNames(purrr::map(stats_per_path, "depth"),   nm)
      rv$alignment$correct_aligned <- setNames(purrr::map(stats_per_path, "correct"), nm)
      rv$alignment$error_aligned   <- setNames(purrr::map(stats_per_path, "error"),   nm)
      rv$alignment$gc_aligned      <- setNames(purrr::map(stats_per_path, "gc"),      nm)

      # Per-block classification (likely cause + suggested tools) ----
      cf <- rv$alignment$conflicts
      if (nrow(cf) > 0) {
        dl <- rv$alignment$depth_aligned
        el <- rv$alignment$error_aligned
        fin <- function(x) x[is.finite(x)]
        # Do the selected paths disagree taxonomically? (possible NUMT signal)
        sel_species <- rv$focal_assembly$blast_species[sel_rows]
        sel_lineage <- if ("blast_lineage" %in% names(rv$focal_assembly)) {
          rv$focal_assembly$blast_lineage[sel_rows]
        } else rep(NA_character_, length(sel_rows))
        key <- ifelse(!is.na(sel_lineage) & nzchar(sel_lineage), sel_lineage, sel_species)
        tops <- unique(.top_taxon(key[!is.na(key)]))
        blast_div <- length(tops) > 1L
        is_circ <- isTRUE(tolower(rv$updating$topology) == "circular")
        cls <- lapply(seq_len(nrow(cf)), function(i) {
          b <- cf[i, ]
          cols <- seq.int(b$aln_start, b$aln_end)
          min_d <- suppressWarnings(min(unlist(lapply(dl, function(v) fin(v[cols]))), na.rm = TRUE))
          max_e <- if (!is.null(el)) {
            suppressWarnings(max(unlist(lapply(el, function(v) fin(v[cols]))), na.rm = TRUE))
          } else NA_real_
          if (!is.finite(min_d)) min_d <- NA_real_
          if (!is.finite(max_e)) max_e <- NA_real_
          at_junc <- is_circ && (b$aln_start <= 1L || b$aln_end >= rv$alignment$aln_len)
          classify_conflict_block(
            list(len = b$len, n_snps = b$n_snps, n_indels = b$n_indels,
                 min_depth = min_d, max_error = max_e),
            at_junction = at_junc, blast_divergent = blast_div
          )
        })
        cf$cause <- vapply(cls, function(x) x$cause, character(1))
        cf$label <- vapply(cls, function(x) x$label, character(1))
        rv$alignment$conflicts <- cf
        rv$alignment$block_class <- cls
      } else {
        rv$alignment$block_class <- list()
      }

      # Reset navigator + per-block resolution decisions
      rv$decisions <- list()
      # Default base path = the first selected path.
      rv$base_label <- nm[1]
      rv$cur_block <- if (nrow(rv$alignment$conflicts) > 0) 1L else 0L
      # Unique signature for this alignment, used as a plot cache key so that
      # revisiting a previously rendered block returns the cached image.
      rv$alignment$sig <- paste(rv$updating$ID, as.numeric(Sys.time()), sep = "-")
    })
    output$msa_div <- renderUI({
      # Only render once the alignment is fully built. During the rapid
      # coverage_modal re-triggers, rv$alignment can briefly exist with some
      # fields unset; requiring seqs + conflicts avoids length-zero `if`s below.
      req(rv$alignment, rv$alignment$seqs, rv$alignment$conflicts)

      wait_align$hide()

      # Summary stats ----
      cf <- rv$alignment$conflicts
      n_blocks <- if (is.null(cf)) 0L else nrow(cf)
      aln_len <- rv$alignment$aln_len %||% 0L
      total_cf <- if (n_blocks > 0) sum(cf$len) else 0L
      pct_cf <- if (aln_len > 0) round(100 * total_cf / aln_len, 2) else 0
      longest <- if (n_blocks > 0) max(cf$len) else 0L
      n_indels <- if (n_blocks > 0) sum(cf$n_indels) else 0L
      n_snps <- if (n_blocks > 0) sum(cf$n_snps) else 0L
      summary_txt <- if (n_blocks == 0) {
        "No conflict blocks detected."
      } else {
        sprintf(
          "Conflicts: %d blocks | %d bp total (%.2f%%) | longest %d bp | %d SNP cols, %d indel cols",
          n_blocks, total_cf, pct_cf, longest, n_snps, n_indels
        )
      }

      # center the chevron within each nav button (default render looks left-justified)
      nav_btn_style <- "display: inline-flex; align-items: center; justify-content: center; width: 40px;"

      navigator <- if (n_blocks > 0) {
        div(
          style = "margin-top: 12px; padding: 8px; border: 1px solid #ddd; border-radius: 4px;",
          div(
            style = "display: flex; align-items: center; gap: 8px; margin-bottom: 6px;",
            actionButton(ns("prev_block"), icon("chevron-left"),
                         style = nav_btn_style),
            actionButton(ns("next_block"), icon("chevron-right"),
                         style = nav_btn_style),
            div(style = "font-size: 12px; color: #555;", uiOutput(ns("block_info"), inline = TRUE))
          ),
          uiOutput(ns("block_interp")),
          div(style = "font-size: 11px; color: #555; margin: 6px 0 4px 0;",
              paste("Overview, full alignment (red = conflict blocks, blue box = zoomed window).",
                    "Mean depth and mean error rate across paths:")),
          plotOutput(ns("minimap_plot"), height = "70px"),
          plotOutput(ns("minimap_error"), height = "70px"),
          div(style = "font-size: 11px; color: #555; margin: 10px 0 4px 0;",
              paste("Zoomed, column-aligned: nucleotide alignment, per-path depth, error rate,",
                    "and read support (green = matches called base, black = mismatch).",
                    "Black lines bracket the current conflict:")),
          plotOutput(ns("zoom_plot"), height = paste0(zoom_plot_height(), "px")),
          div(style = "font-size: 11px; color: #555; margin: 14px 0 4px 0;",
              "Conflict block summary (click a row to jump to that block):"),
          reactableOutput(ns("blocks_table")),
          uiOutput(ns("resolve_ui")),
          div(
            style = "display: flex; align-items: center; gap: 8px; margin-top: 10px;",
            actionButton(ns("prev_block_btm"), icon("chevron-left"),
                         style = nav_btn_style),
            actionButton(ns("next_block_btm"), icon("chevron-right"),
                         style = nav_btn_style)
          )
        )
      } else NULL

      msa <- msaR::renderMsaR(
        msaR::msaR(
          rv$alignment$seqs,
          alignmentHeight = rv$alignment$alignmentHeight,
          overviewbox = TRUE,
          seqlogo = FALSE,
          menu = FALSE,
          colorscheme = "nucleotide",
          conservation = TRUE,
          overviewboxHeight = 20
        )
      )

      div(
        style = "margin: 30px 5px 30px 5px;",
        msa,
        p(rv$alignment$pct_id_range),
        p(rv$alignment$consRegion),
        p(style = "font-weight: bold;", summary_txt),
        navigator,
        div(
          style = "display: flex; gap: 8px; margin-top: 10px;",
          actionButton(ns("trim_consensus"), "Trim to consensus",
                       icon = icon("scissors"),
                       class = "btn-primary",
                       title = paste("Keep only the longest region where all selected paths",
                                     "agree; discards the conflicting ends. Asks to confirm.")),
          if (n_blocks > 0) {
            actionButton(ns("build_resolved"),
                         "Build resolved assembly",
                         icon = icon("wand-magic-sparkles"),
                         class = "btn-primary",
                         title = paste("Combine the per-block resolution choices and the base",
                                       "path into a single consensus (Path 0). Blocks left unset",
                                       "are N-masked. Confirms topology first."))
          }
        )
      ) |> tagList()
    })

    # Navigator handlers ----
    # Arrows move the TABLE selection only; the selection observer below is the
    # single writer of current_block. (Writing current_block here and also
    # syncing the table from current_block creates a feedback loop.)
    go_prev_block <- function() {
      req(rv$alignment$conflicts, nrow(rv$alignment$conflicts) > 0)
      n <- nrow(rv$alignment$conflicts)
      cur <- rv$cur_block %||% 1L
      reactable::updateReactable("blocks_table",
                                 selected = if (cur <= 1L) n else cur - 1L)
    }
    go_next_block <- function() {
      req(rv$alignment$conflicts, nrow(rv$alignment$conflicts) > 0)
      n <- nrow(rv$alignment$conflicts)
      cur <- rv$cur_block %||% 1L
      reactable::updateReactable("blocks_table",
                                 selected = if (cur >= n) 1L else cur + 1L)
    }
    # top and bottom navigation arrows drive the same table selection
    observeEvent(input$prev_block, go_prev_block())
    observeEvent(input$next_block, go_next_block())
    observeEvent(input$prev_block_btm, go_prev_block())
    observeEvent(input$next_block_btm, go_next_block())

    output$block_info <- renderUI({
      req(rv$alignment$conflicts, nrow(rv$alignment$conflicts) > 0)
      cur <- rv$cur_block %||% 1L
      b <- rv$alignment$conflicts[cur, ]
      lbl <- if (!is.null(b$label) && !is.na(b$label)) {
        sprintf(" &middot; <b>%s</b>", b$label)
      } else ""
      HTML(sprintf(
        "Block <b>%d / %d</b> &middot; aln cols %d&ndash;%d &middot; %d bp &middot; %d SNP, %d indel%s",
        cur, nrow(rv$alignment$conflicts), b$aln_start, b$aln_end, b$len, b$n_snps, b$n_indels, lbl
      ))
    })

    # Center the msaR viewer on the current conflict block ----
    observe({
      req(rv$alignment$conflicts, rv$alignment$aln_len)
      cf <- rv$alignment$conflicts
      cur <- rv$cur_block %||% 0L
      req(cur >= 1L, cur <= nrow(cf))
      b <- cf[cur, ]
      session$sendCustomMessage("msaScrollToCol", list(
        col = (b$aln_start + b$aln_end) / 2,
        alnLen = rv$alignment$aln_len
      ))
    })

    # Interpretation strip: plain-language cause + advice for current block ----
    output$block_interp <- renderUI({
      req(rv$alignment$block_class, length(rv$alignment$block_class) > 0)
      cur <- rv$cur_block %||% 1L
      cl <- rv$alignment$block_class[[cur]]
      req(!is.null(cl))
      div(
        style = paste(
          "margin: 4px 0 8px 0; padding: 6px 10px; font-size: 12px;",
          "background: #f7f7f7; border-left: 3px solid #E55330; border-radius: 3px;"
        ),
        tags$b(cl$label), ": ", cl$advice
      )
    })

    # Shared zoom window (current block +/- padding) ----
    # Padding (bp) shown on either side of the current conflict block in the
    # zoomed views. Shared so the nucleotide alignment, depth/error bars, and
    # the minimap marker all cover exactly the same window.
    ZOOM_PAD <- 20L
    zoom_win <- reactive({
      req(rv$alignment$conflicts, rv$alignment$aln_len)
      cf <- rv$alignment$conflicts
      cur <- rv$cur_block %||% 0L
      if (nrow(cf) == 0L || cur < 1L || cur > nrow(cf)) return(NULL)
      b <- cf[cur, ]
      c(max(1L, b$aln_start - ZOOM_PAD),
        min(rv$alignment$aln_len, b$aln_end + ZOOM_PAD))
    })

    # Pixel height of the combined zoomed plot. Computed in the UI (plotOutput
    # height) rather than via renderPlot's `height` arg, because bindCache()
    # does not forward a dynamic height function (the plot would collapse).
    zoom_plot_height <- function() {
      n_seq <- if (!is.null(rv$alignment$aln_mat)) nrow(rv$alignment$aln_mat) else 2L
      n_path <- if (!is.null(rv$alignment$depth_aligned)) {
        length(rv$alignment$depth_aligned)
      } else 2L
      aln_h <- as.integer(20 + n_seq * 11)               # ~half height per path
      bar_h <- as.integer((40 + n_path * 70) * 0.75)      # 25% shorter bars
      aln_h + 3L * bar_h + 30L
    }

    # Zoomed sub-MSA palette. Hex values match the exact shades the msaR widget
    # renders for the "nucleotide" colorscheme so the zoomed tiles match it.
    base_color_nt <- function(b) {
      u <- toupper(b)
      dplyr::case_when(
        u == "A" ~ "#a2fa8c",
        u == "C" ~ "#ffd18c",
        u == "G" ~ "#f38d8a",
        u == "T" ~ "#8ab8f5",
        u == "U" ~ "#8ab8f5",
        b == "-" ~ "#BBBBBB",
        TRUE     ~ "#666666"
      )
    }

    # Combined zoomed view: nucleotide alignment tiles stacked above per-path
    # depth and error-rate bar plots, all on the SAME alignment-column x axis.
    # patchwork aligns the panels so the columns line up exactly; bars are one
    # per bp position so they register against the tiles above.
    output$zoom_plot <- renderPlot({
      req(rv$alignment$conflicts, nrow(rv$alignment$conflicts) > 0)
      cur <- rv$cur_block %||% 1L
      cf <- rv$alignment$conflicts
      b <- cf[cur, ]
      aln_mat <- rv$alignment$aln_mat
      aln_len <- rv$alignment$aln_len
      win_start <- max(1L, b$aln_start - ZOOM_PAD)
      win_end   <- min(aln_len, b$aln_end + ZOOM_PAD)
      cols <- win_start:win_end
      xlim <- c(win_start - 0.5, win_end + 0.5)
      path_levels <- names(rv$alignment$depth_aligned) %||% rownames(aln_mat)

      # Shared x scale + conflict-position highlight applied to every sub-plot:
      # two black vertical lines bracketing the current conflict block.
      x_scale <- ggplot2::scale_x_continuous(
        limits = xlim, expand = c(0, 0),
        minor_breaks = seq(win_start - 0.5, win_end + 0.5, by = 1)
      )
      bracket <- ggplot2::geom_vline(
        xintercept = c(b$aln_start - 0.5, b$aln_end + 0.5),
        color = "black", linewidth = 0.5
      )
      # Light horizontal guide lines for the depth/error/read panels. Zero
      # vertical panel spacing so the black conflict brackets read as one
      # continuous line down the whole stack.
      hguides <- ggplot2::theme(
        panel.grid.minor.y = ggplot2::element_blank(),
        panel.grid.minor.x = ggplot2::element_line(color = "grey90", linewidth = 0.2),
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_line(color = "grey85", linewidth = 0.3),
        panel.spacing.y = grid::unit(6, "pt")
      )

      # --- (A) Nucleotide tiles, absolute alignment columns ---
      sub <- aln_mat[, cols, drop = FALSE]
      seq_names <- rownames(sub)
      n_seq <- nrow(sub)
      dfa <- expand.grid(row = seq_len(n_seq), ci = seq_along(cols),
                         KEEP.OUT.ATTRS = FALSE)
      dfa$col <- cols[dfa$ci]
      dfa$base <- as.vector(sub)
      dfa$fill <- base_color_nt(dfa$base)
      dfa$y <- n_seq - dfa$row + 1L

      p_aln <- ggplot2::ggplot(dfa) +
        ggplot2::geom_tile(ggplot2::aes(x = col, y = y, fill = I(fill)),
                           color = "white", linewidth = 0.2) +
        ggplot2::geom_text(ggplot2::aes(x = col, y = y, label = base),
                           size = 5, color = "#111111", fontface = "bold",
                           family = "mono") +
        bracket +
        ggplot2::scale_y_continuous(breaks = seq_len(n_seq),
                                    labels = rev(seq_names), expand = c(0, 0)) +
        # Column-number axis on TOP of the nucleotide panel (bottom panel keeps
        # the matching axis), so both ends of the stack are labelled.
        ggplot2::scale_x_continuous(limits = xlim, expand = c(0, 0),
                                    position = "top") +
        ggplot2::labs(x = "alignment column", y = NULL) +
        ggplot2::theme_minimal(base_size = 11, base_family = "sans") +
        ggplot2::theme(
          panel.grid = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_text(size = 10, color = "#333333"),
          axis.text.x = ggplot2::element_text(size = 9, color = "#555555"),
          axis.title.x = ggplot2::element_text(size = 10, color = "#333333"),
          legend.position = "none"
        )

      # --- per-path single-value bar plot builder (depth / error) ---
      bar_panel <- function(aligned, ylab, fill, show_x) {
        df <- melt_per_path(aligned, "val", aln_len)
        df <- df[df$col >= win_start & df$col <= win_end, , drop = FALSE]
        df$path <- factor(df$path, levels = path_levels)
        if (all(is.na(df$val))) {
          return(
            ggplot2::ggplot() +
              ggplot2::annotate("text", x = mean(xlim), y = 0.5,
                                label = paste("No", ylab, "data."),
                                size = 3.5, color = "#888888") +
              x_scale + ggplot2::labs(x = NULL, y = ylab) +
              ggplot2::theme_void()
          )
        }
        p <- ggplot2::ggplot(df) +
          ggplot2::geom_col(ggplot2::aes(x = col, y = val), width = 1,
                            fill = fill, color = "grey80", linewidth = 0.15,
                            na.rm = TRUE) +
          bracket +
          ggplot2::facet_wrap(~ path, ncol = 1, scales = "fixed",
                              strip.position = "right") +
          x_scale +
          ggplot2::labs(x = if (show_x) "alignment column" else NULL, y = ylab) +
          ggplot2::theme_minimal(base_size = 11, base_family = "sans") +
          hguides +
          ggplot2::theme(
            legend.position = "none",
            axis.title = ggplot2::element_text(size = 10, color = "#333333"),
            axis.text = ggplot2::element_text(size = 9, color = "#555555"),
            strip.text.y.right = ggplot2::element_text(angle = 0, size = 10,
                                                       color = "#333333", hjust = 0)
          )
        if (!show_x) {
          p <- p + ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                                  axis.ticks.x = ggplot2::element_blank())
        }
        p
      }

      # --- per-path read-support stacked bars (match vs mismatch) ---
      support_panel <- function(show_x) {
        df <- purrr::imap_dfr(rv$alignment$depth_aligned, function(d, nm) {
          corr <- rv$alignment$correct_aligned[[nm]]
          depth <- d[cols]; depth[is.na(depth)] <- 0
          if (is.null(corr) || all(is.na(corr))) {
            correct <- depth
          } else {
            correct <- corr[cols]; correct[is.na(correct)] <- 0
          }
          mismatch <- pmax(depth - correct, 0)
          rbind(
            data.frame(col = cols, path = nm, kind = "match",    n = correct,
                       stringsAsFactors = FALSE),
            data.frame(col = cols, path = nm, kind = "mismatch", n = mismatch,
                       stringsAsFactors = FALSE)
          )
        })
        df$path <- factor(df$path, levels = path_levels)
        df$kind <- factor(df$kind, levels = c("mismatch", "match"))
        if (sum(df$n, na.rm = TRUE) == 0) {
          return(
            ggplot2::ggplot() +
              ggplot2::annotate("text", x = mean(xlim), y = 0.5,
                                label = "No per-base read data.",
                                size = 3.5, color = "#888888") +
              x_scale + ggplot2::labs(x = NULL, y = "reads") +
              ggplot2::theme_void()
          )
        }
        p <- ggplot2::ggplot(df, ggplot2::aes(x = col, y = n, fill = kind)) +
          ggplot2::geom_col(width = 1, color = "grey80", linewidth = 0.15) +
          bracket +
          ggplot2::scale_fill_manual(values = c(match = "#009E73",
                                                mismatch = "#000000")) +
          ggplot2::facet_wrap(~ path, ncol = 1, scales = "fixed",
                              strip.position = "right") +
          x_scale +
          ggplot2::labs(x = if (show_x) "alignment column" else NULL,
                        y = "reads", fill = NULL) +
          ggplot2::theme_minimal(base_size = 11, base_family = "sans") +
          hguides +
          ggplot2::theme(
            legend.position = "bottom",
            axis.title = ggplot2::element_text(size = 10, color = "#333333"),
            axis.text = ggplot2::element_text(size = 9, color = "#555555"),
            strip.text.y.right = ggplot2::element_text(angle = 0, size = 10,
                                                       color = "#333333", hjust = 0)
          )
        if (!show_x) {
          p <- p + ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                                  axis.ticks.x = ggplot2::element_blank())
        }
        p
      }

      p_depth   <- bar_panel(rv$alignment$depth_aligned, "depth", "#337ab7", FALSE)
      p_error   <- bar_panel(rv$alignment$error_aligned, "error rate", "#C0392B", FALSE)
      p_support <- support_panel(TRUE)

      n_path <- length(path_levels)
      patchwork::wrap_plots(
        p_aln, p_depth, p_error, p_support, ncol = 1,
        heights = c(max(1, n_seq * 0.5), n_path * 1.5, n_path * 1.5, n_path * 1.5)
      ) &
        ggplot2::theme(plot.margin = ggplot2::margin(3, 4, 3, 4))
    }) |>
      shiny::bindCache(rv$alignment$sig, rv$cur_block)

    # Helper: build long-format df for a named list of per-path vectors.
    melt_per_path <- function(lst, value_col, aln_len) {
      path_levels <- names(lst)
      df <- purrr::imap_dfr(lst, function(v, nm) {
        d <- data.frame(col = seq_len(aln_len), val = v, path = nm,
                        stringsAsFactors = FALSE)
        d
      })
      df$path <- factor(df$path, levels = path_levels)
      names(df)[names(df) == "val"] <- value_col
      df
    }


    # Full-length overview: mean of a per-path aligned stat across the whole
    # alignment, with all conflict blocks shaded, the current zoom window boxed,
    # and the current conflict block bracketed by black vertical lines.
    overview_plot <- function(values_list, line_color, ylab) {
      aln_len <- rv$alignment$aln_len
      mat <- do.call(rbind, lapply(values_list, function(v) {
        if (length(v) == aln_len) v else rep(NA_real_, aln_len)
      }))
      mean_v <- colMeans(mat, na.rm = TRUE)
      mean_v[is.nan(mean_v)] <- NA_real_
      df <- data.frame(col = seq_len(aln_len), val = mean_v)

      cf  <- rv$alignment$conflicts
      cur <- rv$cur_block %||% 0L
      win <- zoom_win()

      p <- ggplot2::ggplot()
      if (!is.null(cf) && nrow(cf) > 0) {
        p <- p + ggplot2::geom_rect(
          data = cf,
          ggplot2::aes(xmin = aln_start - 0.5, xmax = aln_end + 0.5,
                       ymin = -Inf, ymax = Inf),
          fill = "#E55330", alpha = 0.30, inherit.aes = FALSE
        )
      }
      if (!is.null(win)) {
        p <- p + ggplot2::annotate(
          "rect", xmin = win[1], xmax = win[2],
          ymin = -Inf, ymax = Inf,
          fill = "#08306b", alpha = 0.45
        )
      }
      p <- p + ggplot2::geom_line(
        data = df, ggplot2::aes(x = col, y = val),
        color = line_color, linewidth = 0.3, na.rm = TRUE
      )
      p +
        ggplot2::scale_x_continuous(expand = c(0, 0), limits = c(1, aln_len)) +
        ggplot2::labs(x = NULL, y = ylab) +
        ggplot2::theme_minimal(base_size = 10, base_family = "sans") +
        ggplot2::theme(
          panel.grid = ggplot2::element_blank(),
          axis.text = ggplot2::element_text(size = 8, color = "#777777"),
          axis.text.y = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank(),
          axis.title.y = ggplot2::element_text(size = 9, color = "#555555"),
          plot.margin = ggplot2::margin(2, 4, 2, 4)
        )
    }

    output$minimap_plot <- renderPlot({
      req(rv$alignment$depth_aligned, rv$alignment$aln_len)
      overview_plot(rv$alignment$depth_aligned, "#555555", "depth")
    }) |>
      shiny::bindCache(rv$alignment$sig, rv$cur_block)
    output$minimap_error <- renderPlot({
      req(rv$alignment$error_aligned, rv$alignment$aln_len)
      overview_plot(rv$alignment$error_aligned, "#C0392B", "error")
    }) |>
      shiny::bindCache(rv$alignment$sig, rv$cur_block)

    # Block summary table (click to navigate) ----
    output$blocks_table <- renderReactable({
      req(rv$alignment$conflicts, rv$alignment$depth_aligned)
      cf <- rv$alignment$conflicts
      if (nrow(cf) == 0L) return(NULL)
      dl <- rv$alignment$depth_aligned
      el <- rv$alignment$error_aligned
      # Per-block summary stats: min depth, mean depth, max error across paths.
      finite_only <- function(x) x[is.finite(x)]
      safe_min  <- function(x) { x <- finite_only(x); if (!length(x)) NA_real_ else min(x) }
      safe_mean <- function(x) { x <- finite_only(x); if (!length(x)) NA_real_ else mean(x) }
      safe_max  <- function(x) { x <- finite_only(x); if (!length(x)) NA_real_ else max(x) }
      summarize_block <- function(b) {
        cols <- seq.int(b$aln_start, b$aln_end)
        min_d  <- vapply(dl, function(v) safe_min(v[cols]),  numeric(1))
        mean_d <- vapply(dl, function(v) safe_mean(v[cols]), numeric(1))
        max_e  <- if (!is.null(el)) {
          vapply(el, function(v) safe_max(v[cols]), numeric(1))
        } else NA_real_
        data.frame(
          min_depth  = round(safe_min(min_d),  1),
          mean_depth = round(safe_mean(mean_d), 1),
          max_error  = round(safe_max(max_e), 3)
        )
      }
      extras <- purrr::map_dfr(seq_len(nrow(cf)), function(i) summarize_block(cf[i, ]))
      tbl <- cbind(cf, extras)
      keep_cols <- c("block_id", "label", "aln_start", "aln_end", "len",
                     "n_snps", "n_indels", "min_depth", "mean_depth", "max_error")
      tbl <- tbl[, intersect(keep_cols, names(tbl))]
      # Read current_block without taking a reactive dependency: the table is
      # re-rendered only when the alignment/conflicts change, while block
      # navigation just updates the highlighted row via updateReactable() below.
      # (A reactive dependency here would re-render the table on every arrow
      # press and fight the navigation handlers.)
      cur <- isolate(rv$cur_block) %||% 0L
      reactable(
        tbl,
        compact = TRUE,
        defaultPageSize = 10,
        selection = "single",
        onClick = "select",
        defaultSelected = if (cur >= 1L && cur <= nrow(tbl)) cur else NULL,
        defaultColDef = colDef(align = "center"),
        columns = list(
          block_id  = colDef(name = "Block", width = 70),
          label     = colDef(name = "Likely Cause", minWidth = 180, align = "left",
                             html = TRUE, cell = rt_longtext()),
          aln_start = colDef(name = "Start"),
          aln_end   = colDef(name = "End"),
          len       = colDef(name = "Length"),
          n_snps    = colDef(name = "SNPs"),
          n_indels  = colDef(name = "Indels"),
          min_depth  = colDef(name = "Min Depth"),
          mean_depth = colDef(name = "Mean Depth"),
          max_error  = colDef(name = "Max Error Rate")
        )
      )
    })

    # Single writer of current_block: the blocks_table selection (set by a row
    # click or by the arrow handlers via updateReactable()).
    observe({
      sel <- reactable::getReactableState("blocks_table", "selected")
      req(length(sel) == 1L, !is.na(sel))
      sel <- as.integer(sel)
      if (!identical(sel, rv$cur_block)) {
        rv$cur_block <- sel
      }
    })

    # Resolution toolbox UI (per-block) ----
    # Human-readable labels for each resolution mode.
    res_tool_labels <- c(
      path     = "Use one path's bases (splice)",
      majority = "Coverage-majority base",
      iupac    = "IUPAC ambiguity (SNP only)",
      nmask    = "Mask with N"
    )
    output$resolve_ui <- renderUI({
      req(rv$alignment$conflicts, nrow(rv$alignment$conflicts) > 0)
      cur <- rv$cur_block %||% 1L
      b <- rv$alignment$conflicts[cur, ]
      labs <- rownames(rv$alignment$aln_mat)
      cl <- rv$alignment$block_class[[cur]]

      # Tools available for this block (IUPAC only when no indels).
      avail <- c("path", "majority", "nmask")
      if (isTRUE(b$n_indels == 0L)) avail <- c(avail, "iupac")
      rec <- intersect(cl$tools %||% character(0), avail)
      ordered <- c(rec, setdiff(avail, rec))

      stored <- rv$decisions[[as.character(cur)]]
      # Default selection = N-mask (matches the build default for unset blocks).
      sel_mode <- stored$mode %||% "nmask"
      sel_path <- if (!is.null(stored$row)) labs[stored$row] else labs[1]

      # Summary of decisions explicitly made so far.
      made <- rv$decisions
      n_set <- sum(vapply(made, function(d) !is.null(d), logical(1)))

      div(
        style = paste(
          "margin-top: 14px; padding: 10px; border: 1px solid #cdd;",
          "border-radius: 4px; background: #fbfcfd;"
        ),
        tags$b("Resolve conflicts into a single assembly (Path 0)"),
        div(style = "font-size: 11px; color: #777; margin: 2px 0 8px 0;",
            "Set how to resolve the current block, navigate to others, then build. ",
            "Blocks left unset are N-masked."),
        radioButtons(
          ns("res_mode"),
          label = sprintf("Block %d (%s):", cur, cl$label %||% "conflict"),
          choices = stats::setNames(ordered, res_tool_labels[ordered]),
          selected = sel_mode, inline = TRUE
        ),
        if (sel_mode == "path") {
          selectInput(ns("res_path"), "Path to use for this block:",
                      choices = labs, selected = sel_path, width = "260px")
        },
        tags$hr(style = "margin: 8px 0;"),
        selectInput(ns("base_path"),
                    "Base path (backbone for the non-conflicting regions):",
                    choices = labs,
                    selected = rv$base_label %||% labs[1],
                    width = "320px"),
        div(style = "font-size: 11px; color: #777; margin: -4px 0 8px 0;",
            paste("Provides the sequence for the non-conflicting (agreed) regions.",
                  "Conflict blocks you don't resolve above are filled with N.")),
        div(style = "font-size: 11px; color: #777; margin-bottom: 8px;",
            sprintf("%d of %d conflict block(s) resolved; the rest will be N-masked.",
                    n_set, nrow(rv$alignment$conflicts)))
      )
    })

    # Store the per-block decision when the controls change ----
    # Guard against writing identical values: resolve_ui re-renders on
    # decisions changes and re-sets the inputs, so an unconditional write would
    # invalidate -> re-render -> write -> ... (infinite loop).
    observeEvent(list(input$res_mode, input$res_path), ignoreInit = TRUE, {
      cur <- rv$cur_block %||% 0L
      req(cur >= 1L)
      row <- match(input$res_path %||% NA_character_, rownames(rv$alignment$aln_mat))
      new_dec <- list(
        mode = input$res_mode %||% "base",
        row  = if (is.na(row)) NULL else as.integer(row)
      )
      if (!identical(rv$decisions[[as.character(cur)]], new_dec)) {
        rv$decisions[[as.character(cur)]] <- new_dec
      }
    })
    observeEvent(input$base_path, ignoreInit = TRUE, {
      if (!identical(rv$base_label, input$base_path)) {
        rv$base_label <- input$base_path
      }
    })

    # Persist a resolved/edited single sequence as Path 0 (shared writer) ----
    # Compose assemble notes for an edit: replace any previous edit note (so
    # repeated consensus generation does not stack them) while leaving other
    # notes intact. Edit notes are wrapped in [Assembly edited: ...] so they can
    # be found and removed regardless of internal punctuation.
    compose_edit_notes <- function(edit_text) {
      prior <- rv$updating$assemble_notes %|NA|% ""
      prior <- stringr::str_remove_all(prior, "\\[Assembly edited:[^\\]]*\\]\\s*")
      prior <- stringr::str_remove(prior, "Unable to resolve single assembly from reads")
      prior <- stringr::str_trim(prior)
      stringr::str_trim(paste0("[Assembly edited: ", edit_text, "] ", prior))
    }

    # --- BLAST hit inheritance for the consensus (Path 0) -------------------
    blast_cols_names <- c("blast_accession", "blast_species", "blast_pident",
                          "blast_qcovs", "blast_evalue", "blast_lineage")

    # Pull the blast columns out of a chosen candidate row (or NULL)
    blast_cols <- function(blast_row) {
      if (is.null(blast_row) || nrow(blast_row) == 0) return(NULL)
      keep <- intersect(blast_cols_names, names(blast_row))
      if (length(keep) == 0) return(NULL)
      blast_row[1, keep, drop = FALSE]
    }

    # Distinct BLAST hits among the given (non-consensus) paths
    consensus_blast_candidates <- function(paths) {
      fa <- rv$focal_assembly
      if (is.null(fa) || !"blast_accession" %in% names(fa)) return(NULL)
      sub <- fa[fa$path %in% paths & fa$path > 0 &
                  !is.na(fa$blast_accession) & nzchar(fa$blast_accession), , drop = FALSE]
      if (nrow(sub) == 0) return(NULL)
      keep <- intersect(blast_cols_names, names(sub))
      unique(sub[, keep, drop = FALSE])
    }

    # Resolve which BLAST hit to inherit, then run finalize(blast_row). If the
    # contributing paths carry more than one distinct hit, ask the user first.
    resolve_blast_then <- function(paths, finalize) {
      cand <- consensus_blast_candidates(paths)
      if (is.null(cand) || nrow(cand) == 0) return(finalize(NULL))
      if (nrow(cand) == 1) return(finalize(cand))
      sp <- if ("blast_species" %in% names(cand)) cand$blast_species else rep(NA, nrow(cand))
      labels <- sprintf(
        "%s (%s%s%s)",
        ifelse(is.na(sp) | !nzchar(sp), "unknown sp.", sp),
        cand$blast_accession,
        if ("blast_pident" %in% names(cand)) paste0(", pident ", cand$blast_pident, "%") else "",
        if ("blast_qcovs" %in% names(cand)) paste0(", qcov ", cand$blast_qcovs, "%") else ""
      )
      rv$consensus_finalize <- finalize
      rv$consensus_blast_choices <- cand
      showModal(modalDialog(
        title = "Multiple BLAST hits among paths",
        radioButtons(ns("consensus_blast_choice"),
                     "Choose a BLAST hit to assign to the consensus assembly:",
                     choiceNames = labels, choiceValues = as.character(seq_len(nrow(cand)))),
        footer = tagList(modalButton("Cancel"),
                         actionButton(ns("consensus_blast_confirm"), "Assign hit")),
        easyClose = FALSE
      ))
    }

    observeEvent(input$consensus_blast_confirm, {
      removeModal()
      fin <- rv$consensus_finalize
      cand <- rv$consensus_blast_choices
      idx <- suppressWarnings(as.integer(input$consensus_blast_choice))
      rv$consensus_finalize <- NULL
      rv$consensus_blast_choices <- NULL
      req(!is.null(fin), !is.null(cand), !is.na(idx))
      fin(cand[idx, , drop = FALSE])
    })

    # Resolve the consensus topology, then run finalize(topology). The default is
    # inherited from the contributing paths: "circular" only when every source
    # path is circular, else "linear". The user confirms/overrides because manual
    # editing or trimming can break a circular molecule.
    resolve_topology_then <- function(paths, finalize) {
      topo <- rv$focal_assembly |>
        dplyr::filter(path %in% !!paths) |>
        dplyr::distinct(path, topology)
      inherited <- if (nrow(topo) > 0 &&
                       all(tolower(topo$topology) == "circular", na.rm = TRUE)) {
        "circular"
      } else {
        "linear"
      }
      path_lines <- topo |>
        dplyr::arrange(path) |>
        dplyr::mutate(line = sprintf("Path %s: %s", path, ifelse(is.na(topology), "NA", topology))) |>
        dplyr::pull(line)
      rv$consensus_topology_finalize <- finalize
      showModal(modalDialog(
        title = "Confirm consensus topology",
        div(
          style = "font-size: 0.9em; color: #555; margin-bottom: 8px;",
          "Source path topologies:",
          tags$ul(lapply(path_lines, tags$li))
        ),
        radioButtons(ns("consensus_topology_choice"),
                     "Topology to assign to the consensus assembly:",
                     choices = c("linear", "circular"), selected = inherited),
        footer = tagList(modalButton("Cancel"),
                         actionButton(ns("consensus_topology_confirm"), "Confirm topology")),
        easyClose = FALSE
      ))
    }

    observeEvent(input$consensus_topology_confirm, {
      removeModal()
      fin <- rv$consensus_topology_finalize
      topology <- input$consensus_topology_choice
      rv$consensus_topology_finalize <- NULL
      req(!is.null(fin), topology %in% c("linear", "circular"))
      fin(topology)
    })

    # Mirror the consensus (Path 0) assembly metadata into the annotate row so the
    # Annotate table shows path/length/topology/scaffolds immediately. Normally these
    # are only set later by the curate step (curate_workflow.nf), leaving a freshly
    # built consensus blank in the Annotate table.
    sync_consensus_annotate <- function(ID, length, topology = "linear") {
      # Mirror scaffold_join_workflow.nf's sqlSyncAnnotateJoin: upsert the single
      # joined unit (ID,0,0), inheriting the sample's option sets from an existing
      # annotate row. The original per-scaffold annotate rows are left in place but
      # never selected (their assemblies are ignore=1 and children filter ignore=0).
      # rows_update(by="ID") cannot be used: a multi-scaffold sample now has several
      # annotate rows, and they must collapse to the one (0,0) unit.
      DBI::dbExecute(
        session$userData$con,
        paste0(
          "INSERT OR REPLACE INTO annotate ",
          "(ID, path, scaffold, scaffolds, topology, length, partial, ",
          "annotate_opts, curate_opts, orf_opts, annotate_switch, annotate_lock, ",
          "reviewed, time_stamp) ",
          "SELECT ?, 0, 0, 1, ?, ?, ",
          "CASE WHEN ? = 'circular' OR co.linear_complete = 1 THEN 'no' ELSE 'yes' END, ",
          "an.annotate_opts, an.curate_opts, an.orf_opts, 1, 0, 'no', ? ",
          "FROM (SELECT annotate_opts, curate_opts, orf_opts FROM annotate ",
          "WHERE ID = ? ORDER BY path, scaffold LIMIT 1) an ",
          "LEFT JOIN curate_opts co ON co.curate_opts = an.curate_opts"
        ),
        params = list(ID, topology, as.integer(length), topology,
                      as.numeric(Sys.time()), ID)
      )
    }

    persist_path0 <- function(seq_str, depth_vec, gc_vec, err_vec, note, blast_row = NULL,
                              topology = "linear") {
      ID <- rv$updating$ID
      dir <- file.path(session$userData$dir_out, ID, "assemble",
                       rv$updating$assemble_opts)
      req(require_assemble_output(dir))
      # Shared with the Nextflow auto-join: writes {ID}_assembly_0.fasta and the
      # matching _coverageStats.csv in the layout annotate() reads.
      write_joined_files(dir, ID, seq_str, depth_vec, gc_vec, err_vec, topology)

      DBI::dbExecute(
        session$userData$con,
        stringr::str_glue("UPDATE assemblies SET ignore = 1 WHERE ID = '{ID}';")
      )
      bl <- blast_cols(blast_row)
      a <- joined_assemblies_row(ID, seq_str, depth_vec, gc_vec, err_vec, topology)
      if (!is.null(bl)) a <- cbind(a, bl)
      dplyr::tbl(session$userData$con, "assemblies") |>
        dplyr::rows_upsert(a, by = c("ID", "path", "scaffold"), copy = T, in_place = T)

      update <- data.frame(
        ID = ID, paths = -abs(rv$updating$paths), assemble_lock = 1,
        topology = topology,
        assemble_notes = compose_edit_notes(note)
      )
      if (!is.null(bl)) update <- cbind(update, bl)
      rv$data <- rv$data |> dplyr::rows_update(update, by = "ID")
      dplyr::tbl(session$userData$con, "assemble") |>
        dplyr::rows_update(update, by = "ID", copy = T, in_place = T,
                           unmatched = "ignore")
      sync_consensus_annotate(ID, nchar(seq_str), topology)
      rv$updating <- rv$data |> dplyr::filter(ID == !!ID)
      trigger("coverage_modal")
    }

    # Multi-scaffold join editor (single-path fragmented assemblies) ----
    # Raw scaffold rows for the (single) fragmented path.
    join_scaffold_rows <- function() {
      fa <- rv$focal_assembly
      if (is.null(fa)) return(NULL)
      fa[!is.na(fa$path) & fa$path > 0, , drop = FALSE]
    }

    # BLAST row for the joined Path 0: the reference used for the join, with
    # %ident/%cov/evalue blanked (no longer valid for the joined assembly).
    join_reference_blast_row <- function(accession, rows) {
      if (is.null(accession) || is.na(accession) || !nzchar(accession)) return(NULL)
      sub <- rows[!is.na(rows$blast_accession) & rows$blast_accession == accession, , drop = FALSE]
      get1 <- function(col) if (nrow(sub) && col %in% names(sub)) sub[[col]][1] else NA_character_
      data.frame(
        blast_accession = accession,
        blast_species = get1("blast_species"),
        blast_pident = NA_real_, blast_qcovs = NA_real_, blast_evalue = NA_real_,
        blast_lineage = get1("blast_lineage"),
        stringsAsFactors = FALSE
      )
    }

    # Load the precomputed reference mapping for the given accession and store the
    # layout (+ ref length) so the editor controls and mapping plot populate.
    # Mappings are computed once by the Nextflow scaffold-join and read here, so
    # the app needs no minimap2. notify=TRUE warns when mappings/reference are
    # missing (manual button); notify=FALSE stays silent on automatic modal open.
    compute_join_layout <- function(accession, notify = TRUE) {
      rows <- join_scaffold_rows()
      if (is.null(rows) || nrow(rows) <= 1) return(invisible(FALSE))
      ref_seq <- join_reference_seq(accession)
      if (is.na(ref_seq)) {
        if (notify) shinyWidgets::sendSweetAlert(
          title = "No reference sequence",
          text = "The chosen reference has no cached sequence. Run BLAST/ref-fetch first.",
          type = "warning")
        return(invisible(FALSE))
      }
      mappings <- load_scaffold_mappings(session$userData$con, rv$updating$ID, accession)
      if (is.null(mappings)) {
        if (notify) shinyWidgets::sendSweetAlert(
          title = "No precomputed mapping",
          text = paste("No scaffold->reference mapping is cached for this reference.",
                       "Re-run the assembly workflow (WF1) to compute it."),
          type = "warning")
        return(invisible(FALSE))
      }
      seqs <- stats::setNames(rows$sequence, as.character(rows$scaffold))
      rv$join_ref_len <- nchar(ref_seq)
      lay <- derive_scaffold_layout(mappings, nchar(ref_seq), isTRUE(input$join_circular))
      rv$join_layout <- lay
      # Store reference + oriented included scaffolds for the base-pair zoom view.
      rv$join_ref_seq <- ref_seq
      inc <- lay[lay$include, , drop = FALSE]
      rv$join_oriented <- stats::setNames(
        lapply(seq_len(nrow(inc)), function(i) {
          sq <- seqs[[as.character(inc$scaffold[i])]]
          if (isTRUE(inc$rc[i])) rc_seq(sq) else sq
        }), as.character(inc$scaffold))
      rv$join_zoom_anchor <- NULL
      invisible(TRUE)
    }

    # Reference sequence for an accession from the blast_ref_sequences cache.
    join_reference_seq <- function(accession) {
      if (is.null(accession) || is.na(accession) || !nzchar(accession)) return(NA_character_)
      out <- tryCatch(
        dplyr::tbl(session$userData$con, "blast_ref_sequences") |>
          dplyr::filter(accession == !!accession) |>
          dplyr::pull(sequence),
        error = function(e) character(0)
      )
      if (length(out) == 0) NA_character_ else out[[1]]
    }

    output$scaffold_join_div <- renderUI({
      req(isTRUE(rv$asmb_join_eligible))
      rows <- join_scaffold_rows()
      req(!is.null(rows), nrow(rows) > 1)
      accs <- unique(rows$blast_accession[!is.na(rows$blast_accession) & nzchar(rows$blast_accession)])
      default_ref <- choose_reference(rows)
      disagree <- scaffold_hits_disagree(rows)
      div(
        style = paste("margin: 8px 0; padding: 10px; border: 1px solid #b9c6d6;",
                      "border-radius: 4px; background: #f4f8fc; font-size: 0.9em;"),
        tags$b("Join scaffolds into one assembly"),
        div(style = "margin-top: 4px; color: #555;",
            paste("This path is fragmented into multiple scaffolds. Reference-guided",
                  "layout orders and orients them; you can override order/orientation",
                  "below, then build the joined Path 0.")),
        if (disagree) div(
          style = paste("margin-top: 8px; padding: 8px; border: 1px solid #d9534f;",
                        "border-radius: 4px; background: #fdf3f2; color: #a0241c;"),
          tags$b("Warning: scaffolds map to different references."),
          div(style = "margin-top: 4px;",
              paste("These scaffolds carry different BLAST hits, so joining them may",
                    "produce poor overlaps and an unreliable assembly. Review the",
                    "mapping below before joining.")),
          checkboxInput(ns("join_override_diff"),
                        "I understand the risk; allow joining anyway", value = FALSE)
        ),
        div(style = "display: flex; gap: 12px; align-items: flex-end; margin-top: 8px; flex-wrap: wrap;",
            selectInput(ns("join_reference"), "Reference",
                        choices = accs, selected = default_ref, width = "200px"),
            div(style = "padding-bottom: 6px;",
                checkboxInput(ns("join_circular"), "Circular", value = FALSE)),
            actionButton(ns("join_autolayout"), "Re-map to reference",
                         icon = icon("wand-magic-sparkles")) |>
              (\(b) if (length(accs) == 0) shinyjs::disabled(b) else b)()
        ),
        uiOutput(ns("join_layout_ui")),
        uiOutput(ns("join_map_div")),
        div(style = "margin-top: 8px;",
            actionButton(ns("join_build"), "Build joined assembly (Path 0)",
                         icon = icon("compress"), class = "btn-primary"))
      )
    })

    # Reference-mapping visualization (shown once Auto-layout has run).
    output$join_map_div <- renderUI({
      req(isTRUE(rv$asmb_join_eligible), !is.null(rv$join_layout))
      tagList(
        div(style = "font-size: 11px; color: #888; text-align: center; margin-top: 6px;",
            "click the plot to zoom to a base-pair alignment"),
        plotOutput(ns("join_map_plot"), height = "220px",
                   click = ns("join_map_click")),
        uiOutput(ns("join_zoom_ui"))
      )
    })
    output$join_map_plot <- renderPlot({
      req(!is.null(rv$join_layout), !is.null(rv$join_ref_len))
      rows <- join_scaffold_rows()
      slen <- if (!is.null(rows)) stats::setNames(rows$length, as.character(rows$scaffold)) else NULL
      plot_scaffold_mapping(rv$join_layout, rv$join_ref_len, slen)
    })

    # Click the overview -> anchor the base-pair zoom at that reference position.
    observeEvent(input$join_map_click, {
      x <- input$join_map_click$x
      req(!is.null(x), !is.null(rv$join_ref_len))
      rv$join_zoom_anchor <- max(1L, min(as.integer(rv$join_ref_len), as.integer(round(x))))
    })

    zoom_win_rv <- reactiveVal(60L)
    observeEvent(input$join_zoom_window, ignoreInit = TRUE, {
      v <- input$join_zoom_window
      if (is.null(v) || is.na(v)) return()
      clamped <- max(20L, min(400L, as.integer(v)))
      if (clamped != v) updateNumericInput(session, "join_zoom_window", value = clamped)
      if (clamped != zoom_win_rv()) zoom_win_rv(clamped)
    })

    output$join_zoom_ui <- renderUI({
      req(!is.null(rv$join_zoom_anchor))
      win <- zoom_win_rv()
      plot_w <- as.integer(win * 15L)
      div(
        style = "margin-top: 10px; border-top: 1px solid #ddd; padding-top: 8px;",
        div(style = "font-size: 11px; color: #555; margin-bottom: 4px;",
            sprintf("Base-pair alignment to reference | center ~%d bp | %d bp window",
                    rv$join_zoom_anchor, win)),
        div(style = "overflow-x: auto;",
            plotOutput(ns("join_zoom_plot"), width = paste0(plot_w, "px"),
                       height = "160px")),
        numericInput(ns("join_zoom_window"), "window size (bp)",
                     value = isolate(input$join_zoom_window) %||% 60L,
                     min = 20L, max = 400L, step = 20L, width = "140px")
      )
    })

    output$join_zoom_plot <- renderPlot({
      req(!is.null(rv$join_zoom_anchor), !is.null(rv$join_ref_seq),
          !is.null(rv$join_oriented), !is.null(rv$join_layout))
      win <- zoom_win_rv()
      anchor <- rv$join_zoom_anchor
      ws <- max(1L, anchor - win %/% 2L)
      we <- min(nchar(rv$join_ref_seq), ws + win - 1L)
      lay <- rv$join_layout
      inc <- lay[lay$include, , drop = FALSE]
      bm <- zoom_window_base_maps(rv$join_ref_seq, inc, rv$join_oriented, ws, we)
      plot_scaffold_zoom(rv$join_ref_seq, bm, inc$scaffold, ws, we)
    })

    # Per-scaffold order + orientation controls, seeded from the current layout.
    output$join_layout_ui <- renderUI({
      req(isTRUE(rv$asmb_join_eligible))
      rows <- join_scaffold_rows()
      req(!is.null(rows), nrow(rows) > 1)
      lay <- rv$join_layout
      scaffolds <- if (!is.null(lay)) lay$scaffold else as.character(rows$scaffold)
      order_seed <- if (!is.null(lay)) lay$order else seq_along(scaffolds)
      rc_seed <- if (!is.null(lay)) lay$rc else rep(FALSE, length(scaffolds))
      inc_seed <- if (!is.null(lay) && "include" %in% names(lay)) lay$include else rep(TRUE, length(scaffolds))
      qcov <- if (!is.null(lay) && "qcov" %in% names(lay)) lay$qcov else rep(NA_real_, length(scaffolds))
      tagList(
        div(style = "margin-top: 8px; font-weight: bold; color: #555;",
            "Scaffold layout (only included scaffolds go into Path 0)"),
        lapply(seq_along(scaffolds), function(i) {
          s <- scaffolds[i]
          qc <- if (!is.na(qcov[i])) sprintf(" (%.0f%% mapped)", 100 * qcov[i]) else ""
          div(style = "display: flex; gap: 12px; align-items: center; margin-top: 4px;",
              checkboxInput(ns(paste0("join_inc_", s)), NULL, value = isTRUE(inc_seed[i]),
                            width = "30px"),
              span(style = "width: 150px;", sprintf("Scaffold %s%s", s, qc)),
              numericInput(ns(paste0("join_order_", s)), NULL,
                           value = order_seed[i], min = 1, width = "80px"),
              checkboxInput(ns(paste0("join_rc_", s)), "reverse-comp",
                            value = isTRUE(rc_seed[i])))
        })
      )
    })

    observeEvent(input$join_autolayout, {
      compute_join_layout(input$join_reference, notify = TRUE)
    })

    observeEvent(input$join_build, {
      rows <- join_scaffold_rows()
      req(!is.null(rows), nrow(rows) > 1)
      # Conflicting BLAST hits: block the join until the user explicitly overrides.
      if (scaffold_hits_disagree(rows) && !isTRUE(input$join_override_diff)) {
        shinyWidgets::sendSweetAlert(
          title = "Scaffolds map to different references",
          text = paste("Joining scaffolds with different BLAST hits is risky.",
                       "Check 'allow joining anyway' to override."),
          type = "warning")
        req(FALSE)
      }
      scaffolds <- as.character(rows$scaffold)
      ord <- vapply(scaffolds, function(s) {
        v <- input[[paste0("join_order_", s)]]
        if (is.null(v) || is.na(v)) NA_real_ else as.numeric(v)
      }, numeric(1))
      rc <- vapply(scaffolds, function(s) isTRUE(input[[paste0("join_rc_", s)]]), logical(1))
      inc <- vapply(scaffolds, function(s) {
        v <- input[[paste0("join_inc_", s)]]
        if (is.null(v)) TRUE else isTRUE(v)
      }, logical(1))
      if (!any(inc)) {
        shinyWidgets::sendSweetAlert(
          title = "No scaffolds selected",
          text = "Include at least one scaffold to build the joined assembly.",
          type = "warning")
        req(FALSE)
      }
      ord[is.na(ord)] <- seq_along(ord)[is.na(ord)]
      o <- order(ord)
      layout <- data.frame(
        scaffold = scaffolds[o], order = seq_along(o), rc = rc[o],
        gap_before = NA_real_, mapped = TRUE, include = inc[o], stringsAsFactors = FALSE)
      # Reuse reference-derived gaps only when the manual order matches auto.
      lay <- rv$join_layout
      if (!is.null(lay) && identical(layout$scaffold, lay$scaffold)) {
        layout$gap_before <- lay$gap_before
      }
      scaffolds_df <- data.frame(
        scaffold = scaffolds, sequence = rows$sequence,
        depth = rows$depth, gc = rows$gc, errors = rows$errors,
        stringsAsFactors = FALSE)
      # 100 N is the standard "gap of unknown length" convention; not user-tunable.
      # ref_seq omitted so circular rotation (minimap-based) is skipped in the app;
      # the molecule still circularizes (end-overlap trim) when applicable.
      res <- assemble_from_layout(scaffolds_df, layout, gap_len = 100L,
                                  circular = isTRUE(input$join_circular),
                                  ref_seq = NULL)
      if (count_ambiguities(res$seq) > 0) {
        res$note <- paste0(res$note,
                           " WARNING: contains ambiguous bases (IUPAC/N) - may cause ",
                           "problems in annotation; MITOS does not handle them well.")
      }
      # Inherit the BLAST hit from the reference used for the join (accession +
      # species + lineage), but blank %ident / %cov / evalue: those described a
      # single scaffold's hit and are meaningless for the joined assembly.
      blast_row <- join_reference_blast_row(input$join_reference, rows)
      persist_path0(res$seq, res$depth, res$gc, res$errors, res$note,
                    blast_row, res$topology)
    })

    # Delete the edited consensus (Path 0) and revert the sample ----
    output$consensus_admin <- renderUI({
      req(rv$focal_assembly)
      if (!any(rv$focal_assembly$path == 0, na.rm = TRUE)) return(NULL)
      div(
        style = "margin: 8px 0;",
        actionButton(ns("delete_consensus"), "Delete consensus (Path 0)",
                     icon = icon("trash"), class = "btn-danger"),
        span(style = "font-size: 11px; color: #777; margin-left: 8px;",
             paste("Removes the edited Path 0, restores all paths (un-ignored),",
                   "unlocks the sample, and clears the edit note."))
      )
    })

    observeEvent(input$delete_consensus, {
      shinyWidgets::ask_confirmation(
        inputId = ns("delete_consensus_confirm"),
        title = "Delete consensus (Path 0)?",
        text = paste(
          "This deletes the edited consensus sequence (Path 0), brings back all the",
          "original assembly paths (un-ignored), unlocks the sample, and removes the",
          "edit note. The original paths themselves are not changed."
        ),
        type = "warning",
        btn_labels = c("Cancel", "Delete consensus"),
        btn_colors = c("#6c757d", "#d9534f")
      )
    })

    observeEvent(input$delete_consensus_confirm, {
      req(isTRUE(input$delete_consensus_confirm))
      ID <- rv$updating$ID

      # Remove Path 0 from the DB and its files; un-ignore all remaining paths.
      DBI::dbExecute(
        session$userData$con,
        stringr::str_glue("DELETE FROM assemblies WHERE ID = '{ID}' AND path = 0;")
      )
      DBI::dbExecute(
        session$userData$con,
        stringr::str_glue("UPDATE assemblies SET ignore = 0 WHERE ID = '{ID}';")
      )
      dir <- file.path(session$userData$dir_out, ID, "assemble",
                       rv$updating$assemble_opts)
      unlink(file.path(dir, c(paste0(ID, "_assembly_0.fasta"),
                              paste0(ID, "_assembly_0_coverageStats.csv"))))

      # Number of paths/scaffolds remaining (excludes the just-deleted Path 0).
      n_remaining <- sum(rv$focal_assembly$path != 0, na.rm = TRUE)
      new_notes <- (rv$updating$assemble_notes %|NA|% "") |>
        stringr::str_remove_all("\\[Assembly edited:[^\\]]*\\]\\s*") |>
        stringr::str_trim()

      update <- data.frame(
        ID = ID,
        paths = abs(rv$updating$paths),
        assemble_lock = 0L,
        assemble_switch = if (n_remaining > 1L) 3L else 2L,
        assemble_notes = new_notes
      )
      rv$data <- rv$data |> dplyr::rows_update(update, by = "ID")
      dplyr::tbl(session$userData$con, "assemble") |>
        dplyr::rows_update(update, by = "ID", copy = T, in_place = T,
                           unmatched = "ignore")
      rv$updating <- rv$data |> dplyr::filter(ID == !!ID)
      trigger("coverage_modal")
    })

    # Build resolved assembly from per-block decisions ----
    observeEvent(input$build_resolved, {
      if (rv$updating$assemble_lock == 1) {
        shinyWidgets::sendSweetAlert(title = "Assembly Locked!", type = "warning")
        req(F)
      }
      aln_mat <- rv$alignment$aln_mat
      labs <- rownames(aln_mat)
      base_row <- match(rv$base_label %||% labs[1], labs)
      if (is.na(base_row)) base_row <- 1L
      cf <- rv$alignment$conflicts
      # Unset conflict blocks default to N-masking (safe, explicit) rather than
      # silently inheriting the base path's bases.
      decisions <- lapply(seq_len(nrow(cf)), function(i) {
        rv$decisions[[as.character(i)]] %||% list(mode = "nmask")
      })
      res <- build_resolved_sequence(
        aln_mat, cf, decisions, base_row = base_row,
        support = rv$alignment$correct_aligned
      )
      req(nzchar(res$seq))

      # Reconstruct per-base coverage from MSA-aligned vectors. For synthesized
      # bases (IUPAC/N, src_row = NA) use the mean across paths at that column.
      dl <- rv$alignment$depth_aligned
      el <- rv$alignment$error_aligned
      gl <- rv$alignment$gc_aligned
      pull <- function(vlist, col, row) {
        if (!is.na(row)) {
          v <- vlist[[labs[row]]]
          if (!is.null(v) && col <= length(v) && is.finite(v[col])) return(v[col])
        }
        vals <- vapply(vlist, function(v) {
          if (!is.null(v) && col <= length(v)) v[col] else NA_real_
        }, numeric(1))
        if (any(is.finite(vals))) mean(vals[is.finite(vals)]) else NA_real_
      }
      mp <- res$map
      depth_vec <- mapply(function(c, r) pull(dl, c, r), mp$aln_col, mp$src_row)
      err_vec   <- mapply(function(c, r) pull(el, c, r), mp$aln_col, mp$src_row)
      gc_vec    <- mapply(function(c, r) pull(gl, c, r), mp$aln_col, mp$src_row)

      modes <- vapply(decisions, function(d) d$mode %||% "base", character(1))
      note <- paste0("multi-path resolved per-block (",
                     paste(sort(unique(modes[modes != "base"])), collapse = "/"), ")")
      # Warn if the result contains ambiguous bases (IUPAC codes or Ns), which
      # can break downstream annotation (MITOS does not handle them well).
      if (any(c("iupac", "nmask") %in% modes) || count_ambiguities(res$seq) > 0) {
        note <- paste0(
          note,
          " WARNING: contains ambiguous bases (IUPAC/N) - may cause problems in ",
          "annotation; MITOS does not handle ambiguous base calls well."
        )
        shinyWidgets::sendSweetAlert(
          title = "Ambiguous bases added",
          text = paste(
            "This resolved assembly contains ambiguous bases (IUPAC codes or Ns).",
            "These can cause problems during annotation - MITOS in particular does",
            "not handle ambiguous base calls well. A warning has been added to the",
            "assembly notes."
          ),
          type = "warning"
        )
      }
      # Inherit a BLAST hit from the contributing paths (prompt if >1 distinct),
      # then confirm topology (inherited from the same paths) before writing.
      cand_paths <- rv$focal_assembly$path[rv$focal_assembly$path > 0]
      resolve_topology_then(cand_paths, function(topology) {
        resolve_blast_then(cand_paths, function(blast_row) {
          persist_path0(res$seq, depth_vec, gc_vec, err_vec, note, blast_row, topology)
        })
      })
    })


    # Trim Consensus ----
    # Ask for confirmation first - "Trim Consensus" is destructive/ambiguous.
    observeEvent(input$trim_consensus, {
      if (rv$updating$assemble_lock == 1) {
        shinyWidgets::sendSweetAlert(
          title = "Assembly Locked!",
          type = "warning"
        )
        req(F)
      }
      shinyWidgets::ask_confirmation(
        inputId = ns("trim_confirm"),
        title = "Trim to consensus?",
        text = paste(
          "This keeps ONLY the single longest region where all selected paths agree and",
          "discards everything outside it (including the conflicting ends), saving the",
          "result as a new trimmed Path 0. The original paths are kept but ignored and the",
          "assembly is locked. Best used only when the disagreements are at the edges."
        ),
        type = "warning",
        btn_labels = c("Cancel", "Trim to consensus"),
        btn_colors = c("#6c757d", "#E55330")
      )
    })

    observeEvent(input$trim_confirm, {
      req(isTRUE(input$trim_confirm))

      sel <- selected()
      # Inherit a BLAST hit from the selected paths (prompt if >1 distinct),
      # then write everything. Deferred so a cancelled picker writes nothing.
      finalize_trim <- function(blast_row, topology = "linear") {
        bl <- blast_cols(blast_row)
        out_dir <- file.path(
          session$userData$dir_out,
          rv$updating$ID,
          "assemble",
          rv$updating$assemble_opts
        )
        cov_csv <- file.path(
          out_dir,
          paste0(rv$updating$ID, "_assembly_",
                 rv$focal_assembly$path[sel[1]], "_coverageStats.csv")
        )
        # Checked before anything is written, so a missing output cannot leave a
        # half-written Path 0 behind.
        req(require_assemble_output(out_dir), require_assemble_output(cov_csv))

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
      ) |> paste(topology)
      Biostrings::writeXStringSet(
        trimmed,
        file.path(out_dir, paste0(rv$updating$ID, "_assembly_0.fasta"))
      )

      # Updated coverage stats file
      coverage <- read.csv(cov_csv)

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
        file.path(out_dir, paste0(rv$updating$ID, "_assembly_0_coverageStats.csv")),
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
        topology = topology,
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
      if (!is.null(bl)) trimmed_assembly <- cbind(trimmed_assembly, bl)
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
        topology = topology,
        assemble_notes = compose_edit_notes(
          "multi-path getOrganelle output trimmed for consensus"
        )
      )
      if (!is.null(bl)) update <- cbind(update, bl)
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
      sync_consensus_annotate(rv$updating$ID, trimmed@ranges@width, topology)

      rv$updating <- rv$data |>
        dplyr::filter(ID == !!rv$updating$ID)

      trigger("coverage_modal")
      } # end finalize_trim

      cand_paths <- rv$focal_assembly$path[sel]
      resolve_topology_then(cand_paths, function(topology) {
        resolve_blast_then(cand_paths, function(blast_row) {
          finalize_trim(blast_row, topology)
        })
      })
    })
  })
}
