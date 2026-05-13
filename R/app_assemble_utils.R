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
      blast_opts,
      reads,
      trimmed_reads,
      mean_length,
      topology,
      length,
      paths,
      scaffolds,
      blast_accession,
      blast_species,
      blast_pident,
      blast_qcovs,
      blast_evalue,
      blast_lineage,
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

  } else {
    shinyWidgets::show_alert(
      title = "Multiple preprocess parameter sets selected",
      text = "Cannot edit different parameter sets simultaneously",
      type = "error",
      closeOnClickOutside = FALSE,
    )
  }
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
            selectizeInput(
              ns("assembler"),
              label = "Assembler:",
              choices = c("GetOrganelle", "MitoFinder"),
              selected = current$assembler %||% character(0),
              width = "100%",
              options = list(
                create = TRUE,
                maxItems = 1
              )
            ) |> shinyjs::disabled()
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
          ns("mitofinder"),
          label = "MitoFinder options",
          value = current$mitofinder %||% character(0),
          width = "100%"
        ) |> shinyjs::disabled(),
        textInput(
          ns("mf_db"),
          label = "MitoFinder Database:",
          value = current$mitofinder_db %||% character(0),
          width = "100%"
        ) |> shinyjs::disabled(),
        textInput(
          ns("getOrganelle"),
          label = "getOrganelle options",
          value = current$getOrganelle %||% character(0),
          width = "100%"
        ) |> shinyjs::disabled(),
        textInput(
          ns("seeds_db"),
          label = "getOrganelle Seeds:",
          value = current$seeds_db %||% character(0),
          width = "100%"
        ) |> shinyjs::disabled(),
        textInput(
          ns("labels_db"),
          label = "getOrganelle Labels:",
          value = current$labels_db %||% character(0),
          width = "100%"
        ) |> shinyjs::disabled(),
        div(
          style = "display: flex; flex-flow: row nowrap; align-items: center; gap: 2em;",
          div(
            style = "flex: 1",
            numericInput(
              ns("max_paths"), "Max assembly paths:",
              width = "100%",
              min = 1,
              step = 1,
              value = current$max_paths %||% 10
            ) |> shinyjs::disabled()
          ),
          div(
            style = "flex: 1",
            numericInput(
              ns("max_scaffolds"), "Max scaffolds:",
              width = "100%",
              min = 1,
              step = 1,
              value = current$max_scaffolds %||% 10
            ) |> shinyjs::disabled()
          )
        ),
        tags$p(
          class = "text-muted",
          style = "margin-top: -8px; font-size: 0.85em;",
          "Samples above max paths or max scaffolds threshold will be marked as failed"
        ),
        div(
          style = "display: flex; flex-flow: row nowrap; align-items: center; gap: 2em;",
          div(
            style = "flex: 1",
            numericInput(
              ns("min_assembly_length"), "Min assembly length (bp):",
              width = "100%",
              min = 1,
              step = 1,
              value = current$min_assembly_length %||% 500
            ) |> shinyjs::disabled()
          )
        ),
        tags$p(
          class = "text-muted",
          style = "margin-top: -8px; font-size: 0.85em;",
          "Scaffolds shorter than this threshold are stored but ignored for additional processing"
        ),
        size = "m",
        footer = tagList(
          actionButton(ns("update_assemble_opts"), "Update"),
          modalButton("Cancel")
        )
      )
    )

    if(current$assembler == "GetOrganelle"){
      shinyjs::hide(id = "mitofinder")
      shinyjs::hide(id = "mf_db")
    } else if(current$assembler == "MitoFinder"){
      shinyjs::hide(id = "getOrganelle")
      shinyjs::hide(id = "seeds_db")
      shinyjs::hide(id = "labels_db")
    }
  } else {
    shinyWidgets::show_alert(
      title = "Multiple assembly parameter sets selected",
      text = "Cannot edit different parameter sets simultaneously",
      type = "error",
      closeOnClickOutside = FALSE,
    )
  }
}

#' Update the BLAST options
#'
#' @param rv the local reactive vals object
#' @param session current shiny session
#'
#' @noRd
blast_opts_modal <- function(rv = NULL, session = getDefaultReactiveDomain()) {
  ns <- session$ns

  if (length(unique(rv$updating$blast_opts)) == 1) {
    current <- rv$blast_opts[rv$blast_opts$blast_opts == rv$updating$blast_opts[1], ]

    showModal(
      modalDialog(
        title = stringr::str_glue("Setting BLAST Options for {nrow(rv$updating)} Samples"),
        div(
          style = "display: flex; flex-flow: row nowrap; align-items: center; gap: 2em;",
          selectizeInput(
            ns("blast_opts"),
            label = "Parameter set name:",
            choices = rv$blast_opts$blast_opts,
            selected = current$blast_opts,
            options = list(
              create = TRUE,
              maxItems = 1
            )
          ),
          div(
            class = "form-group shiny-input-container",
            style = "margin-top: 39px;",
            shinyWidgets::prettyCheckbox(
              ns("edit_blast_opts"),
              label = "Edit",
              value = FALSE,
              status = "primary"
            )
          )
        ),
        shinyWidgets::prettyCheckbox(
          ns("run_blast"),
          label = "Run remote BLAST search using using assembly as query",
          value = as.logical(current$run_blast %||% 1L),
          status = "primary"
        ) |> shinyjs::disabled(),
        div(
          id = ns("blast_entrez_group"),
          tags$label(
            "Entrez query -",
            tags$a("Entrez help documentation",
              href = "https://www.ncbi.nlm.nih.gov/books/NBK3837/",
              target = "_blank"
            )
          ),
          textInput(
            ns("entrez_query"),
            label = NULL,
            value = current$entrez_query %||% "mitochondrion[Location]",
            width = "100%"
          ) |> shinyjs::disabled()
        ),
        div(
          id = ns("blast_extra_group"),
          tags$label("Additional blastn options"),
          tags$p(
            class = "text-muted",
            style = "margin-bottom: 4px; font-size: 0.85em;",
            "Cannot override: -outfmt, -max_target_seqs, -max_hsps."
          ),
          textAreaInput(
            ns("extra_opts"),
            label = NULL,
            value = current$extra_opts %||% "",
            width = "100%",
            rows = 2
          ) |> shinyjs::disabled()
        ),
        size = "m",
        footer = tagList(
          actionButton(ns("update_blast_opts"), "Update"),
          modalButton("Cancel")
        )
      )
    )

    if (!as.logical(current$run_blast %||% 1L)) {
      shinyjs::hide(id = "blast_entrez_group")
      shinyjs::hide(id = "blast_extra_group")
    }

  } else {
    shinyWidgets::show_alert(
      title = "Multiple BLAST parameter sets selected",
      text = "Cannot edit different parameter sets simultaneously",
      type = "error",
      closeOnClickOutside = FALSE
    )
  }
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
