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
      lock,
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
      assembly_notes
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

  current <- character(0)
  if (length(unique(rv$updating$pre_opts)) == 1) {
    current <- rv$updating$pre_opts[1]
  }

  showModal(
    modalDialog(
      title = stringr::str_glue("Setting Pre-processing Options for {nrow(rv$updating)} Samples"),
      selectizeInput(
        ns("pre_opts"),
        label = "Parameter set name:",
        choices = rv$pre_opts$pre_opts,
        selected = current,
        options = list(
          create = TRUE,
          maxItems = 1
        )
      ),
      numericInput(ns("pre_opts_cpus"), "CPUs:", value = numeric(0)),
      numericInput(ns("pre_opts_memory"), "Memory (GB):", value = numeric(0)),
      textAreaInput(
        ns("fastp"),
        label = "fastp options",
        value = character(0),
        width = "100%"
      ),
      shinyWidgets::prettyCheckbox(
        ns("set_state"),
        label = "Toggle State",
        value = TRUE,
        status = "primary"
      ),
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

  current <- character(0)
  if (length(unique(rv$updating$assemble_opts)) == 1) {
    current <- rv$updating$assemble_opts[1]
  }

  showModal(
    modalDialog(
      title = stringr::str_glue("Setting Assembly Options for {nrow(rv$updating)} Samples"),
      selectizeInput(
        ns("assemble_opts"),
        label = "Parameter set name:",
        choices = rv$assemble_opts$assemble_opts,
        selected = current,
        options = list(
          create = TRUE,
          maxItems = 1
        )
      ),
      numericInput(ns("assemble_opts_cpus"), "CPUs:", value = numeric(0)),
      numericInput(ns("assemble_opts_memory"), "Memory (GB):", value = numeric(0)),
      textAreaInput(
        ns("getOrganelle"),
        label = "getOrganelle options",
        value = character(0),
        width = "100%"
      ),
      textInput(ns("seeds_db"), "getOrganelle Seeds:", value = rv$assemble_opts$seeds_db[1]),
      shinyWidgets::prettyCheckbox(
        ns("set_state"),
        label = "Toggle State",
        value = TRUE,
        status = "primary"
      ),
      size = "xl",
      footer = tagList(
        actionButton(ns("update_assemble_opts"), "Update"),
        modalButton("Cancel")
      )
    )
  )
}
