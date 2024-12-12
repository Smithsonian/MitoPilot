#' coverage_details Server Functions
#'
#' @noRd
assembly_coverage_details_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    init("coverage_modal")
    on("coverage_modal", {
      rv$alignment <- NULL
      rv$focal_assembly <- dplyr::tbl(session$userData$con, "assemblies") |>
        dplyr::filter(ID == !!rv$updating$ID) |>
        dplyr::select(ID, path, scaffold, topology, length, sequence, ignore) |>
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

      modalDialog(
        title = stringr::str_glue("Assembly details for ID: {rv$updating$ID} "),
        size = "l",
        reactableOutput(ns("table"), width = "100%"),
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
          defaultColDef = colDef(maxWidth = 80, align = "center"),
          columns = list(
            ID = colDef(
              align = "left", minWidth = 100, html = T, cell = rt_longtext()
            ),
            scaffold = colDef(show = FALSE),
            sequence = colDef(
              minWidth = 250, maxWidth = 1000, align = "center", html = TRUE,
              cell = rt_longtext()
            ),
            ignore = colDef(
              html = TRUE, align = "center",
              cell = rt_bool_bttn(ns("ignore"), "fa fa-circle-xmark", "far fa-circle")
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
      trigger("update_assemble_table")
    })

    # Table selection ----
    selected <- reactive(reactable::getReactableState("table", "selected"))
    observe({
      shinyjs::toggle("clip", condition = length(selected()) > 0)
      shinyjs::toggle("align", condition = length(selected()) > 1)
      shinyjs::toggle("msa_div", condition = length(selected()) > 1)
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
      url <- file.path(
        dirname(getOption("MitoPilot.db") %||% here::here(".sqlite")),
        "out", rv$updating$ID,
        "assemble", rv$updating$assemble_opts,
        paste0(rv$updating$ID, "_assembly_", rv$focal_assembly$path[as.numeric(input$view_coverage)], "_coverage.pdf")
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

      div(
        style = "margin: 30px 5px 30px 5px;",
        msa,
        p(rv$alignment$pct_id_range),
        p(rv$alignment$consRegion),
        actionButton(ns("trim_consensus"), "Trim Consensus")
      ) |> tagList()
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
