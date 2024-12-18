#' Populate assemble table
#'
#' @param db database connection
#' @param session reactive session
#'
#' @noRd
fetch_assemble_data <- function(session = getDefaultReactiveDomain()) {
  db <- session$userData$con

  preprocess <- dplyr::tbl(db, "preprocess") |>
    dplyr::select(!time_stamp)

  assemble <- dplyr::tbl(db, "assemble")

  taxa <- dplyr::tbl(db, "samples") |>
    dplyr::select(ID, Taxon)

  dplyr::left_join(assemble, preprocess, by = "ID") |>
    dplyr::left_join(taxa, by = "ID") |>
    dplyr::collect() |>
    dplyr::arrange(dplyr::desc(time_stamp)) |>
    dplyr::relocate(
      assemble_lock,
      assemble_switch,
      ID,
      Taxon,
      pre_opts,
      assemble_opts,
      reads,
      trimmed_reads,
      mean_length,
      topology,
      length,
      paths,
      scaffolds,
      time_stamp,
      assemble_notes
    ) |>
    dplyr::mutate(
      output = dplyr::case_when(
        assemble_switch > 1 ~ "output",
        .default = NA_character_
      ),
      view = dplyr::case_when(
        assemble_switch > 1 ~ "details",
        .default = NA_character_
      )
    )
}

#' Update the preprocessing options
#'
#' @param rv the local reactive vals object
#' @param session current shiny session
#'
#' @noRd
pre_opts_modal <- function(rv = NULL, session = getDefaultReactiveDomain()) {
  ns <- session$ns

  current <- list()
  if (length(unique(rv$updating$pre_opts)) == 1) {
    current <- rv$pre_opts[rv$pre_opts$pre_opts == rv$updating$pre_opts[1], ]
  }

  showModal(
    modalDialog(
      title = stringr::str_glue("Setting Pre-processing Options for {nrow(rv$updating)} Samples"),
      div(
        style = "display: flex; flex-flow: row nowrap; align-items: center; gap: 2em;",
        selectizeInput(
          ns("pre_opts"),
          label = "Parameter set name:",
          choices = rv$pre_opts$pre_opts,
          selected = current$pre_opts,
          options = list(
            create = TRUE,
            maxItems = 1
          )
        ),
        div(
          class = "form-group shiny-input-container",
          style = "margin-top: 39px;",
          shinyWidgets::prettyCheckbox(
            ns("edit_pre_opts"),
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
            ns("pre_opts_cpus"), "CPUs:",
            width = "100%",
            value = current$cpus %||% numeric(0)
          ) |> shinyjs::disabled()
        ),
        div(
          style = "flex: 1",
          numericInput(
            ns("pre_opts_memory"), "Memory (GB):",
            width = "100%",
            value = current$memory %||% numeric(0)
          ) |> shinyjs::disabled()
        )
      ),
      textInput(
        ns("fastp"),
        label = "fastp options",
        value =  current$fastp %||% character(0),
        width = "100%"
      ) |> shinyjs::disabled(),
      size = "m",
      footer = tagList(
        actionButton(ns("update_pre_opts"), "Update"),
        modalButton("Cancel")
      )
    )
  )
}

#' Update the assemble options
#'
#' @param rv the local reactive vals object
#' @param session current shiny session
#'
#' @noRd
assemble_opts_modal <- function(rv = NULL, session = getDefaultReactiveDomain()) {
  ns <- session$ns

  current <- list()
  if (length(unique(rv$updating$assemble_opts)) == 1) {
    current <- rv$assemble_opts[rv$assemble_opts$assemble_opts == rv$updating$assemble_opts[1], ]
  }

  showModal(
    modalDialog(
      title = stringr::str_glue("Setting Assembly Options for {nrow(rv$updating)} Samples"),
      div(
        style = "display: flex; flex-flow: row nowrap; align-items: center; gap: 2em;",
        selectizeInput(
          ns("assemble_opts"),
          label = "Parameter set name:",
          choices = rv$assemble_opts$assemble_opts,
          selected = current$assemble_opts,
          options = list(
            create = TRUE,
            maxItems = 1
          )
        ),
        div(
          class = "form-group shiny-input-container",
          style = "margin-top: 39px;",
          shinyWidgets::prettyCheckbox(
            ns("edit_assemble_opts"),
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
            ns("assemble_opts_cpus"), "CPUs:",
            width = "100%",
            value = current$cpus %||% numeric(0)
          ) |> shinyjs::disabled()
        ),
        div(
          style = "flex: 1",
          numericInput(
            ns("assemble_opts_memory"), "Memory (GB):",
            width = "100%",
            value = current$memory %||% numeric(0)
          ) |> shinyjs::disabled()
        )
      ),
      textInput(
        ns("getOrganelle"),
        label = "getOrganelle options",
        value = current$getOrganelle %||% character(0),
        width = "100%"
      ) |> shinyjs::disabled(),
      textInput(
        ns("seeds_db"), "getOrganelle Seeds:",
        value = current$seeds_db %||% character(0),
        width = "100%"
      ) |> shinyjs::disabled(),
      size = "m",
      footer = tagList(
        actionButton(ns("update_assemble_opts"), "Update"),
        modalButton("Cancel")
      )
    )
  )
}

#' Get assembly from database
#'
#' @param ID sample ID
#' @param path assembly getOrganelle path
#' @param scaffold scaffold name(s) to get (NULL for all, default)
#' @param con database connection
#'
#' @export
get_assembly <- function(ID, path, scaffold = NULL, con) {
  qry <- dplyr::tbl(con, "assemblies") |>
    dplyr::filter(ID == !!ID & path == !!path) |>
    dplyr::select(ID, path, scaffold, topology, sequence) |>
    dplyr::arrange(scaffold) |>
    dplyr::collect()
  if (!is.null(scaffold)) {
    qry <- dplyr::filter(qry, scaffold %in% !!scaffold)
  }
  qry |>
    tidyr::unite("scaffold_name", c(ID, path, scaffold), sep = ".") |>
    tidyr::unite("seq_name", c(scaffold_name, topology), sep = " ") |>
    dplyr::pull(sequence, name = "seq_name") |>
    Biostrings::DNAStringSet()
}
