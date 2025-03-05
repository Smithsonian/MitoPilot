#' Populate annotate table
#'
#' @param db database connection
#' @param session reactive session
#'
#' @noRd
fetch_annotate_data <- function(session = getDefaultReactiveDomain()) {
  db <- session$userData$con

  annotate <- dplyr::tbl(db, "annotate")

  assemble <- dplyr::tbl(db, "assemble") |>
    dplyr::filter(assemble_lock == 1) |>
    dplyr::select(ID)

  taxa <- dplyr::tbl(db, "samples") |>
    dplyr::select(ID, Taxon)

  dplyr::left_join(assemble, annotate, by = "ID") |>
    dplyr::left_join(taxa, by = "ID") |>
    dplyr::select(
      annotate_lock,
      annotate_switch,
      ID,
      Taxon,
      annotate_opts,
      curate_opts,
      length,
      topology,
      scaffolds,
      structure,
      PCGCount,
      tRNACount,
      rRNACount,
      missing,
      extra,
      warnings,
      reviewed,
      time_stamp,
      annotate_notes
    ) |>
    dplyr::collect() |>
    dplyr::arrange(dplyr::desc(time_stamp)) |>
    dplyr::mutate(
      output = dplyr::case_when(
        annotate_switch > 1 ~ "output",
        .default = NA_character_
      ),
      view = dplyr::case_when(
        annotate_switch > 1 ~ "details",
        .default = NA_character_
      )
    )
}

#' Get top BLASTP hits
#'
#' If blastp is not available on the path loaded in system(), set
#' options("MitoPilot.blastp" = "/path/to/blastp/executable")
#'
#' @param ref_db reference database
#' @param query query sequeencs
#'
#' @export
#'
get_top_hits_local <- function(
    ref_db = NULL,
    query = NULL) {
  stringr::str_glue(
    "-db {ref_db}",
    "-best_hit_score_edge 0.01",
    "-max_hsps 1",
    "-qcov_hsp_perc 80",
    "-max_target_seqs 1000",
    "-outfmt '6 salltitles evalue sseq'",
    "-query -",
    .sep = " "
  ) |>
    system2(getOption("MitoPilot.blastp", "blastp"), args = _, input = query, stdout = TRUE) |>
    purrr::map_dfr(~ {
      df <- data.frame(stringr::str_split(.x, "\\t", simplify = T))
      colnames(df) <- c("hit", "eval", "target")
      df |>
        dplyr::mutate(
          Taxon = stringr::str_extract(hit, "(?<=\\[).*?(?=\\])"),
          eval = as.numeric(eval)
        )
    }) |>
    dplyr::arrange(eval) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      pctid = compare_aa(query, target, "pctId"),
      similarity = compare_aa(query, target, "similarity"),
      gap_leading = count_end_gaps(query, target, "leading"),
      gap_trailing = count_end_gaps(query, target, "trailing"),
      .after = "eval"
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(dplyr::desc(similarity))
}

#' Update the annotation options
#'
#' @param rv the local reactive vals object
#' @param session current shiny session
#'
#' @noRd
annotate_opts_modal <- function(rv = NULL, session = getDefaultReactiveDomain()) {
  ns <- session$ns

  current <- list()
  if (length(unique(rv$updating$annotate_opts)) == 1) {
    current <- rv$annotate_opts[rv$annotate_opts$annotate_opts == rv$updating$annotate_opts[1], ]
  }

  showModal(
    modalDialog(
      title = stringr::str_glue("Setting Annotation Options for {nrow(rv$updating)} Samples"),
      div(
        style = "display: flex; flex-flow: row nowrap; align-items: center; gap: 2em;",
        selectizeInput(
          ns("annotate_opts"),
          label = "Parameter set name:",
          choices = rv$annotate_opts$annotate_opts,
          selected = current$annotate_opts,
          options = list(
            create = TRUE,
            maxItems = 1
          )
        ),
        div(
          class = "form-group shiny-input-container",
          style = "margin-top: 39px;",
          shinyWidgets::prettyCheckbox(
            ns("edit_annotate_opts"),
            label = "Edit",
            value = FALSE,
            status = "primary"
          )
        )
      ),
      div(
        style = "display: flex; flex-flow: row nowrap; align-items: center; gap: 2em;",
        div(
          style = "flex: 1",
          numericInput(
            ns("annotate_opts_cpus"), "CPUs:",
            width = "100%",
            value = current$cpus %||% numeric(0)
          ) |> shinyjs::disabled()
        ),
        div(
          style = "flex: 1",
          numericInput(
            ns("annotate_opts_memory"), "Memory (GB):",
            width = "100%",
            value = current$memory %||% numeric(0)
          ) |> shinyjs::disabled()
        )
      ),
      textInput(
        ns("mitos_opts"),
        label = "Mitos2 options:",
        value = current$mitos_opts %||% character(0),
        width = "100%"
      ) |> shinyjs::disabled(),
      div(
        style = "display: flex; flex-flow: row nowrap; align-items: center; gap: 2em;",
        div(
          style = "flex: 1",
          selectizeInput(
            ns("mitos_ref_dir"),
            label = "ref_dir",
            choices = unique(rv$annotate_opts$ref_dir),
            selected = current$annotate_opts %||% character(0),
            width = "100%",
            options = list(
              create = TRUE,
              maxItems = 1
            )
          ) |> shinyjs::disabled()
        ),
        div(
          style = "flex: 1",
          selectizeInput(
            ns("mitos_ref_db"),
            label = "ref_db",
            choices = unique(rv$annotate_opts$ref_db),
            selected = current$annotate_opts %||% character(0),
            width = "100%",
            options = list(
              create = TRUE,
              maxItems = 1
            )
          ) |> shinyjs::disabled()
        )
      ),
      textInput(
        ns("trnaScan_opts"),
        label = "trnAScan-SE options:",
        value = current$trnaScan_opts %||% character(0),
        width = "100%"
      ) |> shinyjs::disabled(),
      size = "m",
      footer = tagList(
        actionButton(ns("update_annotate_opts"), "Update"),
        modalButton("Cancel")
      )
    )
  )
}

#' Update the curation options
#'
#' @param rv the local reactive vals object
#' @param session current shiny session
#' @import reactR
#' @noRd
curate_opts_modal <- function(rv = NULL, session = getDefaultReactiveDomain()) {
  ns <- session$ns
  req(rv$updating)
  current <- list()
  rv$params <- rv$curate_opts$params[rv$curate_opts$curate_opts == rv$updating$curate_opts[1]] |>
    jsonlite::fromJSON()
  if (length(unique(rv$updating$curate_opts)) == 1) {
    current <- rv$curate_opts[rv$curate_opts$curate_opts == rv$updating$curate_opts[1], ]
  }


  showModal(
    modalDialog(
      title = stringr::str_glue("Setting Curation Options for {nrow(rv$updating)} Samples"),
      div(
        style = "display: flex; flex-flow: row nowrap; align-items: center; gap: 2em;",
        selectizeInput(
          ns("curate_opts"),
          label = "Parameter set name:",
          choices = rv$curate_opts$curate_opts,
          selected = current$curate_opts,
          options = list(
            create = TRUE,
            maxItems = 1
          )
        ),
        div(
          class = "form-group shiny-input-container",
          style = "margin-top: 39px;",
          shinyWidgets::prettyCheckbox(
            ns("edit_curate_opts"),
            label = "Edit",
            value = FALSE,
            status = "primary"
          )
        )
      ),
      div(
        style = "display: flex; flex-flow: row nowrap; align-items: center; gap: 2em;",
        div(
          style = "flex: 1",
          numericInput(
            ns("curate_opts_cpus"), "CPUs:",
            width = "100%",
            value = current$cpus %||% numeric(0)
          ) |> shinyjs::disabled()
        ),
        div(
          style = "flex: 1",
          numericInput(
            ns("curate_opts_memory"), "Memory (GB):",
            width = "100%",
            value = current$memory %||% numeric(0)
          ) |> shinyjs::disabled()
        )
      ),
      textInput(
        ns("target"),
        label = "Target:",
        value = current$target %||% character(0),
        width = "100%"
      ) |> shinyjs::disabled(),
      listviewer::reactjsonOutput(ns("params")),
      size = "m",
      footer = tagList(
        actionButton(ns("update_curate_opts"), "Update"),
        modalButton("Cancel")
      )
    )
  )
}
