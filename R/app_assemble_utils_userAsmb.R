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
      find_mito_opts,
      circularize_opts,
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
      assemble_notes,
      circularize_notes,
      find_mito_notes
    ) |>
    dplyr::mutate(
      output = dplyr::case_when(
        assemble_switch > 1 ~ "output",
        .default = NA_character_
      ),
      view = dplyr::case_when(
        assemble_switch > 1 ~ "details",
        .default = NA_character_
      ),
      # Link to the search evidence, shown only once a search has run
      mito_candidates = dplyr::case_when(
        !is.na(find_mito_notes) ~ "candidates",
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


#' Wire up the shared behaviour of an Assemble options modal
#'
#' Every options column in the Assemble table (mitogenome search, circularization,
#' BLAST) behaves the same way: clicking the cell opens a modal for the selected
#' rows, the "Edit" checkbox unlocks the fields, editing a parameter set that
#' reaches beyond the selection asks for confirmation, and Update writes the
#' sample-to-parameter-set assignment back. Only the fields and the parameter-set
#' save itself differ, so those come in as arguments.
#'
#' @param rv the local reactive vals object
#' @param name options column / table name, e.g. "blast_opts"
#' @param fields input ids the Edit checkbox enables and disables
#' @param label human-readable name for the confirmation prompt, e.g.
#'   "BLAST options"
#' @param modal function(rv) that shows the modal
#' @param save function() that validates and upserts the parameter set. Called
#'   only when Edit is checked; use `req(FALSE)` inside it to abort the update.
#' @param input,session the module's input object and session
#' @param selected reactive giving the currently selected table rows
#'
#' @noRd
opts_modal_server <- function(rv, name, fields, label, modal, save,
                              input, session, selected) {
  edit_id <- paste0("edit_", name)
  indirect_id <- paste0("editing_", name, "_indirect")

  observeEvent(input[[paste0("set_", name)]], {
    row <- as.numeric(input[[paste0("set_", name)]])
    if (length(selected()) > 0 && !row %in% selected()) {
      req(F)
    }
    rows <- c(row, selected()) |> unique()
    req(all(rv$data$assemble_lock[rows] == 0))
    rv$updating <- rv$data |> dplyr::slice(rows)
    rv$updating_indirect <- rv$updating |> dplyr::slice(0)
    modal(rv)
  })

  observeEvent(input[[edit_id]], ignoreInit = T, {
    editing <- isTRUE(input[[edit_id]])
    for (fld in fields) {
      shinyjs::toggleState(fld, condition = editing)
    }
    set_name <- input[[name]]
    if (editing && set_name %in% rv$data[[name]]) {
      rv$updating_indirect <- rv$data |>
        dplyr::filter(.data[[name]] == set_name) |>
        dplyr::anti_join(rv$updating, by = "ID")
      if (nrow(rv$updating_indirect) > 0L && any(rv$updating_indirect$assemble_lock == 1)) {
        shinyWidgets::sendSweetAlert(
          title = "Attempting to edit locked samples",
          text = "Processing parameters associated with locked samples can not be edited.",
          type = "warning"
        )
        shinyWidgets::updatePrettyCheckbox(inputId = edit_id, value = FALSE)
        req(F)
      }
      if (nrow(rv$updating_indirect) > 0L) {
        shinyWidgets::confirmSweetAlert(
          inputId = indirect_id,
          title = "Editing beyond selection",
          text = paste0(
            "You are attempting to edit ", label, " that apply to samples beyond ",
            "the current selection. Are you sure you want to proceed?"
          ),
          btn_colors = c("#0056b3", "#0056b3")
        )
      }
    } else {
      rv$updating_indirect <- rv$updating |> dplyr::slice(0)
    }
  })

  observeEvent(input[[indirect_id]], ignoreInit = T, {
    if (!input[[indirect_id]]) {
      rv$updating_indirect <- rv$updating |> dplyr::slice(0)
      shinyWidgets::updatePrettyCheckbox(inputId = edit_id, value = FALSE)
    }
  })

  observeEvent(input[[paste0("update_", name)]], ignoreInit = T, {
    if (isTRUE(input[[edit_id]])) {
      save()
    }
    update <- data.frame(
      ID = c(rv$updating$ID, rv$updating_indirect$ID),
      assemble_switch = 1L
    )
    update[[name]] <- input[[name]]
    dplyr::tbl(session$userData$con, "assemble") |>
      dplyr::rows_update(
        update,
        unmatched = "ignore",
        in_place = TRUE,
        copy = TRUE,
        by = "ID"
      )
    rv$data <- rv$data |>
      dplyr::rows_update(update, by = "ID")
    rv$updating <- rv$updating_indirect <- NULL
    removeModal()
    trigger("update_assemble_table")
  })
}

#' Update the circularization options
#'
#' Settings for the optional WF1 step that trims a redundant end-to-start
#' overlap from linear user assemblies. See [circularize_asmb()].
#'
#' @param rv the local reactive vals object
#' @param session current shiny session
#'
#' @noRd
circularize_opts_modal <- function(rv = NULL, session = getDefaultReactiveDomain()) {
  ns <- session$ns

  if (length(unique(rv$updating$circularize_opts)) != 1) {
    shinyWidgets::show_alert(
      title = "Multiple circularization parameter sets selected",
      text = "Cannot edit different parameter sets simultaneously",
      type = "error",
      closeOnClickOutside = FALSE,
    )
    return(invisible(NULL))
  }

  # Read-based confirmation is impossible without raw data, so those two
  # thresholds stay hidden in a no-raw-data project.
  no_raw <- isTRUE(session$userData$no_raw_data)
  current <- rv$circularize_opts[
    rv$circularize_opts$circularize_opts == rv$updating$circularize_opts[1],
  ]

  showModal(
    modalDialog(
      title = stringr::str_glue("Setting Circularization Options for {nrow(rv$updating)} Samples"),
      div(
        style = "display: flex; flex-flow: row nowrap; align-items: center; gap: 2em;",
        selectizeInput(
          ns("circularize_opts"),
          label = "Parameter set name:",
          choices = rv$circularize_opts$circularize_opts,
          selected = current$circularize_opts,
          options = list(
            create = TRUE,
            maxItems = 1
          )
        ),
        div(
          class = "form-group shiny-input-container",
          style = "margin-top: 39px;",
          shinyWidgets::prettyCheckbox(
            ns("edit_circularize_opts"),
            label = "Edit",
            value = FALSE,
            status = "primary"
          )
        )
      ),
      opts_help("Reusable named set of options applied to the selected samples; ",
                "check Edit to change values or type a new name to create a set."),
      shinyWidgets::prettyCheckbox(
        ns("attempt_circularization"),
        label = "Attempt to circularize linear assemblies",
        value = isTRUE(as.logical(current$attempt %||% 0L)),
        status = "primary"
      ) |> shinyjs::disabled(),
      opts_help("Assemblers often report a circular mitogenome as a linear contig ",
                "whose end repeats its start. When switched on, the contig is ",
                "BLASTed against itself, any redundant overlap is trimmed, and the ",
                "assembly is relabeled circular. Only linear, single-contig ",
                "assemblies are considered; everything else is left untouched."),
      div(
        id = ns("circ_params_group"),
        div(
          style = "display: flex; flex-flow: row nowrap; align-items: center; gap: 2em;",
          div(
            style = "flex: 1",
            numericInput(
              ns("circ_min_overlap"), "Min. overlap (bp):",
              width = "100%",
              value = current$min_overlap %||% numeric(0)
            ) |> shinyjs::disabled()
          ),
          div(
            style = "flex: 1",
            numericInput(
              ns("circ_min_identity"), "Min. identity (%):",
              width = "100%",
              value = current$min_identity %||% numeric(0)
            ) |> shinyjs::disabled()
          )
        ),
        opts_help("How long and how similar the overlap between the contig ends must ",
                  "be before it is treated as a redundant copy rather than a repeat."),
        if (!no_raw) {
          tagList(
            div(
              style = "display: flex; flex-flow: row nowrap; align-items: center; gap: 2em;",
              div(
                style = "flex: 1",
                numericInput(
                  ns("circ_min_junction_reads"), "Min. junction reads:",
                  width = "100%",
                  value = current$min_junction_reads %||% numeric(0)
                ) |> shinyjs::disabled()
              ),
              div(
                style = "flex: 1",
                numericInput(
                  ns("circ_min_overhang"), "Min. read overhang (bp):",
                  width = "100%",
                  value = current$min_overhang %||% numeric(0)
                ) |> shinyjs::disabled()
              )
            ),
            opts_help("Reads are mapped across the new junction to confirm it. An ",
                      "assembly stays linear unless at least this many reads cross ",
                      "the junction, each extending the given number of bases past ",
                      "it on both sides.")
          )
        },
        div(
          style = "display: flex; flex-flow: row nowrap; align-items: center; gap: 2em;",
          div(
            style = "flex: 1",
            numericInput(
              ns("circ_opts_cpus"), "CPUs:",
              width = "100%",
              value = current$cpus %||% numeric(0)
            ) |> shinyjs::disabled()
          ),
          div(
            style = "flex: 1",
            numericInput(
              ns("circ_opts_memory"), "Memory (GB):",
              width = "100%",
              value = current$memory %||% numeric(0)
            ) |> shinyjs::disabled()
          )
        )
      ),
      size = "m",
      footer = tagList(
        actionButton(ns("update_circularize_opts"), "Update"),
        modalButton("Cancel")
      )
    )
  )

  if (!isTRUE(as.logical(current$attempt %||% 0L))) {
    shinyjs::hide(id = "circ_params_group")
  }
}

#' Update the mitogenome search options
#'
#' Settings for the optional WF1 step that locates mitochondrial contigs inside
#' a large user-supplied assembly. See [find_mito()].
#'
#' @param rv the local reactive vals object
#' @param session current shiny session
#'
#' @noRd
find_mito_opts_modal <- function(rv = NULL, session = getDefaultReactiveDomain()) {
  ns <- session$ns

  if (length(unique(rv$updating$find_mito_opts)) != 1) {
    shinyWidgets::show_alert(
      title = "Multiple mitogenome search parameter sets selected",
      text = "Cannot edit different parameter sets simultaneously",
      type = "error",
      closeOnClickOutside = FALSE,
    )
    return(invisible(NULL))
  }

  current <- rv$find_mito_opts[
    rv$find_mito_opts$find_mito_opts == rv$updating$find_mito_opts[1],
  ]

  showModal(
    modalDialog(
      title = stringr::str_glue("Setting Mitogenome Search Options for {nrow(rv$updating)} Samples"),
      div(
        style = "display: flex; flex-flow: row nowrap; align-items: center; gap: 2em;",
        selectizeInput(
          ns("find_mito_opts"),
          label = "Parameter set name:",
          choices = rv$find_mito_opts$find_mito_opts,
          selected = current$find_mito_opts,
          options = list(
            create = TRUE,
            maxItems = 1
          )
        ),
        div(
          class = "form-group shiny-input-container",
          style = "margin-top: 39px;",
          shinyWidgets::prettyCheckbox(
            ns("edit_find_mito_opts"),
            label = "Edit",
            value = FALSE,
            status = "primary"
          )
        )
      ),
      opts_help("Reusable named set of options applied to the selected samples; ",
                "check Edit to change values or type a new name to create a set."),
      shinyWidgets::prettyCheckbox(
        ns("find_mitogenome"),
        label = "Search the assembly for mitochondrial contigs",
        value = isTRUE(as.logical(current$attempt %||% 0L)),
        status = "primary"
      ) |> shinyjs::disabled(),
      opts_help("Use this when your FASTA holds a whole assembly rather than a ",
                "mitogenome. Contigs are BLASTed against the bundled metazoan ",
                "mitogenome database, the survivors are confirmed with MitoFinder, ",
                "and only those continue through the pipeline. A sample where ",
                "nothing is confirmed is marked failed."),
      div(
        id = ns("find_mito_params_group"),
        textInput(
          ns("find_mitofinder_db"),
          label = "MitoFinder reference database (.gb):",
          value = current$mitofinder_db %||% "",
          width = "100%"
        ) |> shinyjs::disabled(),
        opts_help("GenBank database used to confirm candidate contigs. Build one ",
                  "for your clade with custom_assembly_db(db_type = \"mitofinder\"). ",
                  "Required while the search is switched on."),
        div(
          style = "display: flex; flex-flow: row nowrap; align-items: center; gap: 2em;",
          div(
            style = "flex: 1",
            numericInput(
              ns("find_min_contig_length"), "Min. contig length (bp):",
              width = "100%", value = current$min_contig_length %||% numeric(0)
            ) |> shinyjs::disabled()
          ),
          div(
            style = "flex: 1",
            numericInput(
              ns("find_min_identity"), "Min. identity (%):",
              width = "100%", value = current$min_identity %||% numeric(0)
            ) |> shinyjs::disabled()
          )
        ),
        opts_help("Contigs shorter than the minimum length are never searched, ",
                  "which keeps the short tail of a draft genome out of the BLAST."),
        div(
          style = "display: flex; flex-flow: row nowrap; align-items: center; gap: 2em;",
          div(
            style = "flex: 1",
            numericInput(
              ns("find_min_aligned_length"), "Min. aligned length (bp):",
              width = "100%", value = current$min_aligned_length %||% numeric(0)
            ) |> shinyjs::disabled()
          ),
          div(
            style = "flex: 1",
            numericInput(
              ns("find_min_aligned_fraction"), "Min. aligned fraction:",
              width = "100%", min = 0, max = 1, step = 0.05,
              value = current$min_aligned_fraction %||% numeric(0)
            ) |> shinyjs::disabled()
          )
        ),
        opts_help("The aligned fraction is the NUMT filter: a real mitochondrial ",
                  "contig is almost entirely mitochondrial, while a nuclear ",
                  "scaffold carrying a NUMT aligns over a small slice of itself."),
        div(
          style = "display: flex; flex-flow: row nowrap; align-items: center; gap: 2em;",
          div(
            style = "flex: 1",
            numericInput(
              ns("find_max_candidates"), "Max. candidates:",
              width = "100%", value = current$max_candidates %||% numeric(0)
            ) |> shinyjs::disabled()
          ),
          div(
            style = "flex: 1",
            numericInput(
              ns("find_min_genes"), "Min. genes to confirm:",
              width = "100%", value = current$min_genes %||% numeric(0)
            ) |> shinyjs::disabled()
          )
        ),
        opts_help("Only the best candidates go to MitoFinder, and a candidate is ",
                  "confirmed once it carries at least this many mitochondrial genes."),
        div(
          style = "display: flex; flex-flow: row nowrap; align-items: center; gap: 2em;",
          div(
            style = "flex: 1",
            numericInput(
              ns("find_opts_cpus"), "CPUs:",
              width = "100%", value = current$cpus %||% numeric(0)
            ) |> shinyjs::disabled()
          ),
          div(
            style = "flex: 1",
            numericInput(
              ns("find_opts_memory"), "Memory (GB):",
              width = "100%", value = current$memory %||% numeric(0)
            ) |> shinyjs::disabled()
          )
        )
      ),
      size = "m",
      footer = tagList(
        actionButton(ns("update_find_mito_opts"), "Update"),
        modalButton("Cancel")
      )
    )
  )

  if (!isTRUE(as.logical(current$attempt %||% 0L))) {
    shinyjs::hide(id = "find_mito_params_group")
  }
}

#' Show the evidence behind a sample's mitogenome search
#'
#' @param id sample ID
#' @param session current shiny session
#'
#' @noRd
mito_candidates_modal <- function(id, session = getDefaultReactiveDomain()) {
  rows <- dplyr::tbl(session$userData$con, "mito_candidates") |>
    dplyr::filter(ID == !!id) |>
    dplyr::collect() |>
    dplyr::arrange(dplyr::desc(selected), rank, dplyr::desc(aligned_length))

  if (nrow(rows) == 0L) {
    shinyWidgets::show_alert(
      title = "No search results",
      text = "This sample has no mitogenome search records yet.",
      type = "info"
    )
    return(invisible(NULL))
  }

  showModal(
    modalDialog(
      title = stringr::str_glue("Mitogenome Search Candidates: {id}"),
      size = "l",
      easyClose = TRUE,
      opts_help("Every contig the search considered, best first. 'Kept' contigs ",
                "were carried forward as scaffolds of the assembly; the rest were ",
                "dropped for the reason shown."),
      reactable::reactable(
        rows |>
          dplyr::transmute(
            Contig = contig,
            `Length (bp)` = length,
            Reference = accession,
            `Identity (%)` = pident,
            `Aligned (bp)` = aligned_length,
            `Aligned fraction` = round(aligned_fraction, 3),
            Genes = genes,
            Outcome = dplyr::if_else(selected == 1L, "kept", "dropped"),
            Reason = dplyr::coalesce(reason, "")
          ),
        defaultPageSize = 15,
        compact = TRUE,
        highlight = TRUE,
        wrap = FALSE,
        resizable = TRUE,
        columns = list(
          Contig = reactable::colDef(minWidth = 160),
          Reason = reactable::colDef(minWidth = 260)
        )
      ),
      footer = modalButton("Close")
    )
  )
}
