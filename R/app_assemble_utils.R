#' Populate assemble table
#'
#' @param db database connection
#' @param session reactive session
#'
#' @noRd
fetch_assemble_data <- function(session = getDefaultReactiveDomain()) {
  db <- session$userData$db

  preprocess <- dplyr::tbl(db, "preprocess") |>
    dplyr::select(!time_stamp)

  assemble <- dplyr::tbl(db, "assemble")

  dplyr::left_join(assemble, preprocess, by = "ID") |>
    dplyr::collect() |>
    dplyr::arrange(dplyr::desc(time_stamp)) |>
    dplyr::relocate(
      assemble_lock,
      assemble_switch,
      ID,
      pre_opts,
      reads,
      trimmed_reads,
      mean_length,
      assemble_opts,
      topology,
      length,
      paths,
      scaffolds,
      time_stamp,
      assemble_notes
    ) |>
    dplyr::mutate(view = NA_character_)
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
      numericInput(
        ns("pre_opts_cpus"), "CPUs:",
        value = current$cpus %||% numeric(0)
      ) |> shinyjs::disabled(),
      numericInput(
        ns("pre_opts_memory"), "Memory (GB):",
        value = current$memory %||% numeric(0)
      ) |> shinyjs::disabled(),
      textAreaInput(
        ns("fastp"),
        label = "fastp options",
        value =  current$fastp %||% character(0),
        width = "100%"
      ) |> shinyjs::disabled(),
      size = "xl",
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
      numericInput(
        ns("assemble_opts_cpus"), "CPUs:",
        value = current$cpus %||% numeric(0)
        ) |> shinyjs::disabled(),
      numericInput(
        ns("assemble_opts_memory"), "Memory (GB):",
        value = current$memory %||% numeric(0)
        ) |> shinyjs::disabled(),
      textAreaInput(
        ns("getOrganelle"),
        label = "getOrganelle options",
        value = current$getOrganelle %||% character(0),
        width = "100%"
      ) |> shinyjs::disabled(),
      textInput(
        ns("seeds_db"), "getOrganelle Seeds:",
        value = current$seeds_db %||% character(0),
        ) |> shinyjs::disabled(),
      size = "xl",
      footer = tagList(
        actionButton(ns("update_assemble_opts"), "Update"),
        modalButton("Cancel")
      )
    )
  )
}
