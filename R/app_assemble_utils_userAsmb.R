#' Summarise a sample's per-contig topology values
#'
#' Circularization is decided per contig, so a sample no longer has one
#' topology. Report the single value when the contigs agree, and a count of each
#' otherwise. A contig whose topology has not been written yet counts as
#' "unknown" rather than being dropped, so the counts always add up to the
#' number of contigs handed in.
#'
#' @param topology character vector, one value per non-ignored contig
#'
#' @return "circular", "linear", or a mixture such as "2 circular, 1 linear";
#'   NA when there are no contigs at all
#'
#' @noRd
summarize_topology <- function(topology) {
  topology <- as.character(topology)
  if (length(topology) == 0L) {
    return(NA_character_)
  }
  topology[is.na(topology) | !nzchar(topology)] <- "unknown"
  n <- table(topology)
  if (length(n) == 1L) {
    return(names(n))
  }
  paste(paste(as.integer(n), names(n)), collapse = ", ")
}

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

  # Topology is decided per contig. Summarise the sample's non-ignored contigs.
  # A sample with none (never run, or every contig ignored) has no measured
  # topology at all, so it falls back to the value the user declared and says
  # so: the declared value is an input, not a result.
  # Ambiguous bases are counted per contig at the same time. A sample with more
  # than one active contig has no single number to show, so it is left blank
  # here and reported per contig in the assembly details table.
  unit_topology <- dplyr::tbl(db, "assemblies") |>
    dplyr::select(ID, topology, ignore, sequence) |>
    dplyr::collect() |>
    dplyr::group_by(ID) |>
    dplyr::summarise(
      unit_topology = summarize_topology(topology[ignore == 0]),
      ambiguous_bases = {
        active <- which(ignore %in% 0)
        if (length(active) == 1L) ambiguous_base_count(sequence[active]) else NA_integer_
      },
      .groups = "drop"
    )

  out <- dplyr::left_join(assemble, preprocess, by = "ID") |>
    dplyr::left_join(taxa, by = "ID") |>
    dplyr::collect() |>
    dplyr::left_join(unit_topology, by = "ID") |>
    dplyr::mutate(topology = dplyr::case_when(
      !is.na(unit_topology) ~ unit_topology,
      is.na(topology) ~ NA_character_,
      # "multi" is derived from the assembly, not declared by the user.
      topology == "multi" ~ "multi",
      .default = paste0(topology, " (declared)")
    )) |>
    dplyr::select(-unit_topology) |>
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
      ambiguous_bases,
      paths,
      scaffolds,
      blast_accession,
      blast_ref_status,
      blast_species,
      blast_lineage,
      blast_pident,
      blast_qcovs,
      blast_evalue,
      time_stamp,
      assemble_notes,
      circularize_notes,
      find_mito_notes,
      join_notes
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
    ) |>
    # The three action columns render last and adjacent (theme T19).
    dplyr::relocate(blast_hits, output, view, .after = dplyr::last_col())
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
    rows <- assemble_opts_rows(
      rv, as.numeric(input[[paste0("set_", name)]]), selected(), session = session
    )
    if (is.null(rows)) return()
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
        mp_alert(
          title = "Locked samples cannot be edited",
          text = paste(
            "This parameter set is also used by locked samples, so its",
            "values cannot be changed. Unlock those samples first."
          ),
          type = "warning"
        )
        shinyWidgets::updatePrettyCheckbox(inputId = edit_id, value = FALSE)
        req(F)
      }
      if (nrow(rv$updating_indirect) > 0L) {
        mp_confirm(
          indirect_id,
          title = "Edit beyond the selection",
          text = paste0(
            "These ", label, " also apply to ",
            mp_n(nrow(rv$updating_indirect), "sample"),
            " outside the current selection, which this edit will change too."
          ),
          action_label = "Continue",
          session = session
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
    mp_opts_saved_toast(nrow(update), input[[name]], session = session)
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
    mp_alert(
      title = "Multiple circularization parameter sets selected",
      text = paste(
        "One modal edits one parameter set. Select samples that share a set,",
        "or edit them one set at a time."
      ),
      type = "warning"
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
      title = mp_modal_title(
        paste("Circularization options for", mp_n(nrow(rv$updating), "sample"))
      ),
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
          class = "form-group shiny-input-container mp-opts-checkbox",
          mp_checkbox(ns("edit_circularize_opts"), label = "Edit", value = FALSE)
        )
      ),
      opts_help("Reusable named set of options applied to the selected samples; ",
                "check Edit to change values or type a new name to create a set. ",
                "Saving re-queues the selected samples: their state becomes Ready to run."),
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
      ),
      mp_checkbox(
        ns("attempt_circularization"),
        label = "Attempt to circularize linear assemblies",
        value = isTRUE(as.logical(current$attempt %||% 0L))
      ) |> shinyjs::disabled(),
      opts_help("Assemblers often report a circular mitogenome as a linear contig ",
                "whose end repeats its start. When switched on, the contig is ",
                "BLASTed against itself, any redundant overlap is trimmed, and the ",
                "assembly is relabeled circular. Every contig in the assembly is ",
                "tried, up to 100 contigs; assemblies above that limit are skipped ",
                "and reported in the Circularization note."),
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
        }
      ),
      size = "m",
      footer = mp_footer(primary = actionButton(ns("update_circularize_opts"), "Save"))
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
    mp_alert(
      title = "Multiple mitogenome search parameter sets selected",
      text = paste(
        "One modal edits one parameter set. Select samples that share a set,",
        "or edit them one set at a time."
      ),
      type = "warning"
    )
    return(invisible(NULL))
  }

  current <- rv$find_mito_opts[
    rv$find_mito_opts$find_mito_opts == rv$updating$find_mito_opts[1],
  ]

  showModal(
    modalDialog(
      title = mp_modal_title(
        paste("Mitogenome search options for", mp_n(nrow(rv$updating), "sample"))
      ),
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
          class = "form-group shiny-input-container mp-opts-checkbox",
          mp_checkbox(ns("edit_find_mito_opts"), label = "Edit", value = FALSE)
        )
      ),
      opts_help("Reusable named set of options applied to the selected samples; ",
                "check Edit to change values or type a new name to create a set. ",
                "Saving re-queues the selected samples: their state becomes Ready to run."),
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
      ),
      mp_checkbox(
        ns("find_mitogenome"),
        label = "Search the assembly for mitochondrial contigs",
        value = isTRUE(as.logical(current$attempt %||% 0L))
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
              ns("find_min_aligned_fraction"), "Min. aligned fraction (0-1):",
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
                  "confirmed once it carries at least this many mitochondrial genes.")
      ),
      size = "m",
      footer = mp_footer(primary = actionButton(ns("update_find_mito_opts"), "Save"))
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
    mp_alert(
      title = "No search results",
      text = "This sample has no mitogenome search records yet.",
      type = "info"
    )
    return(invisible(NULL))
  }

  showModal(
    modalDialog(
      title = mp_modal_title(paste("Mitogenome search candidates for", id)),
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

#' Get assembly from database (user-supplied assemblies)
#'
#' Deprecated: identical to [get_assembly()], which serves both project types.
#'
#' @inheritParams get_assembly
#' @export
get_assembly_userAsmb <- function(ID, path, scaffold = NULL, con) {
  .Deprecated("get_assembly")
  get_assembly(ID = ID, path = path, scaffold = scaffold, con = con)
}

