#' Populate assemble table
#'
#' @param db database connection
#' @param session reactive session
#'
#' @noRd
fetch_assemble_data_userAsmb <- function(session = getDefaultReactiveDomain()) {
  db <- session$userData$con

  preprocess <- dplyr::tbl(db, "preprocess") |>
    dplyr::select(!time_stamp)

  assemble <- dplyr::tbl(db, "assemble") |>
    dplyr::select(-topology) # we only want user-supplied topology, from the samples table

  taxa <- dplyr::tbl(db, "samples") |>
    dplyr::select(ID, Taxon, topology, assembly)

  out <- dplyr::left_join(assemble, preprocess, by = "ID") |>
    dplyr::left_join(taxa, by = "ID") |>
    dplyr::collect() |>
    dplyr::arrange(dplyr::desc(time_stamp)) |>
    dplyr::mutate(
      blast_ref_status = poor_blast_ref,
      blast_hits = dplyr::if_else(assemble_switch > 1, "All BLAST Hits", NA_character_)
    )

  out |>
    dplyr::relocate(
      assemble_lock,
      assemble_switch,
      ID,
      Taxon,
      assembly,
      topology,
      pre_opts,
      blast_opts,
      reads,
      trimmed_reads,
      mean_length,
      length,
      paths,
      scaffolds,
      blast_accession,
      blast_ref_status,
      blast_species,
      blast_pident,
      blast_qcovs,
      blast_evalue,
      blast_lineage,
      blast_hits,
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


#' Get assembly from database
#'
#' @param ID sample ID
#' @param path assembly getOrganelle path
#' @param scaffold scaffold name(s) to get (NULL for all, default)
#' @param con database connection
#'
#' @export
get_assembly_userAsmb <- function(ID, path, scaffold = NULL, con) {
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
        opts_help("Reusable named set of options applied to the selected samples; ",
                  "check Edit to change values or type a new name to create a set."),
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
          label = tagList("fastp options", tool_help_icon("fastp")),
          value =  current$fastp %||% character(0),
          width = "100%"
        ) |> shinyjs::disabled(),
        opts_help("Command-line flags passed to fastp, which trims adapters and ",
                  "filters low-quality reads.",
                  href = "https://github.com/OpenGene/fastp"),
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
        opts_help("Reusable named set of options applied to the selected samples; ",
                  "check Edit to change values or type a new name to create a set."),
        shinyWidgets::prettyCheckbox(
          ns("run_blast"),
          label = "Run BLAST reference search using assembly as query",
          value = as.logical(current$run_blast %||% 1L),
          status = "primary"
        ) |> shinyjs::disabled(),
        opts_help("BLAST each assembly against the bundled local database of ",
                  "metazoan mitogenomes to find the closest reference (used for ",
                  "orientation and curation). Annotations for the winning ",
                  "reference are still fetched from NCBI."),
        local_blast_db_note(session$userData$dir_out),
        div(
          id = ns("blast_taxids_group"),
          tags$label(
            "Restrict search to taxon IDs (optional) -",
            tags$a("NCBI Taxonomy Browser",
              href = "https://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi",
              target = "_blank"
            )
          ),
          textInput(
            ns("taxids"),
            label = NULL,
            value = current$taxids %||% "",
            placeholder = "e.g. 7711 for Chordata, or 7711,6656",
            width = "100%"
          ) |> shinyjs::disabled(),
          opts_help("Comma-separated NUMERIC NCBI taxon IDs; taxon names are not ",
                    "accepted. Leave blank to search the whole database. Applies to ",
                    "both the local and the remote search.")
        ),
        div(
          id = ns("blast_remote_group"),
          shinyWidgets::prettyCheckbox(
            ns("remote_blast"),
            label = "Remote BLAST",
            value = as.logical(current$remote_blast %||% 0L),
            status = "primary"
          ) |> shinyjs::disabled(),
          opts_help("Search NCBI over the network instead of the bundled local ",
                    "database. Much slower, rate limited, and requires internet ",
                    "access; use it only to reach sequences the local database ",
                    "does not contain."),
          shinyWidgets::prettyCheckbox(
            ns("remote_fallback"),
            label = "Fall back to remote BLAST when no local hit",
            value = as.logical(current$remote_fallback %||% 1L),
            status = "primary"
          ) |> shinyjs::disabled(),
          opts_help("If the local search finds no significant hit, retry the search ",
                    "once against NCBI.")
        ),
        div(
          id = ns("blast_entrez_group"),
          # Remote-only setting: hidden unless the remote toggle is on
          style = if (isTRUE(as.logical(current$remote_blast %||% 0L))) NULL else "display: none;",
          tags$label(
            "Entrez query (remote BLAST only) -",
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
          ) |> shinyjs::disabled(),
          opts_help("Restricts a REMOTE BLAST search to GenBank records matching ",
                    "this Entrez filter (default limits hits to mitochondrial ",
                    "sequences). The local database search cannot apply it; use ",
                    "taxon IDs above instead. Leave anything other than the default ",
                    "here and the local search will refuse to run.")
        ),
        div(
          id = ns("blast_mts_group"),
          tags$label("Candidate reference mitogenomes to retain"),
          numericInput(
            ns("max_target_seqs"),
            label = NULL,
            value = as.integer(current$max_target_seqs %||% 5L),
            min = 1, max = 50, step = 1, width = "120px"
          ) |> shinyjs::disabled(),
          opts_help("Number of top BLAST hits kept per sample (-max_target_seqs).")
        ),
        div(
          id = ns("blast_extra_group"),
          tags$label(tagList("Additional blastn options", tool_help_icon("blastn"))),
          tags$p(
            class = "text-muted",
            style = "margin-bottom: 4px; font-size: 0.85em;",
            "Extra flags passed to blastn. Cannot override: -outfmt, -max_hsps, or ",
            "-max_target_seqs."
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
      shinyjs::hide(id = "blast_taxids_group")
      shinyjs::hide(id = "blast_remote_group")
      shinyjs::hide(id = "blast_entrez_group")
      shinyjs::hide(id = "blast_mts_group")
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

