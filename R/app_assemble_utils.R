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

  assemble_opts_tbl <- dplyr::tbl(db, "assemble_opts") |>
    dplyr::select(assemble_opts, min_assembly_length)

  # Per-sample BLAST display rule (n_total = rows in assemblies for this ID):
  #   n_total NA               -> keep sample-level value (assembly not yet run)
  #   n_kept == 1              -> show kept scaffold's BLAST
  #   n_kept != 1 (incl. 0)    -> blank
  blast_cols <- c("blast_accession", "blast_species", "blast_pident",
                  "blast_qcovs", "blast_evalue", "blast_lineage")

  assemblies_tbl <- dplyr::tbl(db, "assemblies") |>
    dplyr::select(ID, length, ignore, dplyr::any_of(blast_cols)) |>
    dplyr::collect()

  total_counts <- assemblies_tbl |>
    dplyr::count(ID, name = "n_total")

  kept <- assemblies_tbl |> dplyr::filter(ignore == 0)

  kept_counts <- kept |>
    dplyr::count(ID, name = "n_kept")

  kept_single <- kept |>
    dplyr::semi_join(dplyr::filter(kept_counts, n_kept == 1L), by = "ID") |>
    dplyr::select(ID, dplyr::any_of(blast_cols)) |>
    dplyr::rename_with(~ paste0(.x, "_kept"), dplyr::any_of(blast_cols))

  # Per-scaffold length + ignore (sorted by length desc) so the "Asmb. Length"
  # cell can color each scaffold red when ignore == 1. Replaces the deduped
  # length string from assemble_workflow.nf because we need one-to-one mapping
  # with the ignore vector.
  length_ignore <- assemblies_tbl |>
    dplyr::arrange(ID, dplyr::desc(length)) |>
    dplyr::summarise(
      length_per_scaffold = paste(length, collapse = ";"),
      ignore_flags        = paste(ignore, collapse = ";"),
      .by = "ID"
    )

  swap_blast <- function(df, col) {
    kept_col <- paste0(col, "_kept")
    if (!col %in% names(df) || !kept_col %in% names(df)) return(df)
    na_val <- if (is.numeric(df[[col]])) NA_real_ else NA_character_
    df[[col]] <- dplyr::case_when(
      is.na(df$n_total)                        ~ df[[col]],
      df$n_kept == 1L & !is.na(df[[kept_col]]) ~ df[[kept_col]],
      df$n_kept == 1L                          ~ df[[col]],  # fall back to assemble-level value
      .default = na_val
    )
    df
  }

  out <- dplyr::left_join(assemble, preprocess, by = "ID") |>
    dplyr::left_join(taxa, by = "ID") |>
    dplyr::left_join(assemble_opts_tbl, by = "assemble_opts") |>
    dplyr::collect() |>
    dplyr::left_join(total_counts, by = "ID") |>
    dplyr::left_join(kept_counts, by = "ID") |>
    dplyr::left_join(kept_single, by = "ID") |>
    dplyr::left_join(length_ignore, by = "ID") |>
    dplyr::mutate(
      length = dplyr::coalesce(length_per_scaffold, length)
    ) |>
    (\(df) purrr::reduce(blast_cols, swap_blast, .init = df))() |>
    dplyr::select(-n_total, -n_kept, -length_per_scaffold,
                  -dplyr::any_of(paste0(blast_cols, "_kept"))) |>
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
                  "filters low-quality reads before assembly.",
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
        opts_help("Reusable named set of options applied to the selected samples; ",
                  "check Edit to change values or type a new name to create a set."),
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
        # Assembler choice + its tool-specific options, boxed off.
        div(
          style = paste(
            "border: 1px solid #ddd; border-radius: 6px;",
            "padding: 12px 14px 4px; margin: 8px 0 14px;"
          ),
        tags$div(
          style = "font-weight: bold; margin-bottom: 8px;",
          "Assembler"
        ),
        div(
          style = "display: flex; flex-flow: row nowrap; align-items: center; gap: 2em;",
          div(
            style = "flex: 1",
            selectizeInput(
              ns("assembler"),
              label = NULL,
              choices = c("GetOrganelle", "MitoFinder"),
              selected = current$assembler %||% character(0),
              width = "100%",
              options = list(
                create = FALSE,
                maxItems = 1
              )
            ) |> shinyjs::disabled()
          )
        ),
        opts_help("Tool used to assemble the mitogenome from reads: ",
                  tags$a(href = "https://github.com/Kinggerm/GetOrganelle",
                         target = "_blank", rel = "noopener", "GetOrganelle"),
                  " or ",
                  tags$a(href = "https://github.com/RemiAllio/MitoFinder",
                         target = "_blank", rel = "noopener", "MitoFinder"),
                  "; the relevant tool options appear below."),
        # Each tool's help line is appended INSIDE its input's container (not as a
        # standalone <p>), so it shows/hides together with the field: shinyjs::hide
        # on the input hides its .shiny-input-container, and the help with it.
        textInput(
          ns("mitofinder"),
          label = tagList("MitoFinder options", tool_help_icon("mitofinder")),
          value = current$mitofinder %||% character(0),
          width = "100%"
        ) |> shinyjs::disabled() |>
          tagAppendChild(opts_help(
            "Extra command-line flags passed to MitoFinder.",
            href = "https://github.com/RemiAllio/MitoFinder",
            id = ns("help_mitofinder"), nested = TRUE)),
        textInput(
          ns("mf_db"),
          label = "MitoFinder Database:",
          value = current$mitofinder_db %||% character(0),
          width = "100%"
        ) |> shinyjs::disabled() |>
          tagAppendChild(opts_help(
            "Path to the MitoFinder reference database (GenBank .gb format) ",
            "used to seed the assembly.",
            href = "https://smithsonian.github.io/MitoPilot/articles/custom_dbs.html",
            id = ns("help_mf_db"), nested = TRUE)),
        textInput(
          ns("getOrganelle"),
          label = tagList("getOrganelle options", tool_help_icon("getOrganelle")),
          value = current$getOrganelle %||% character(0),
          width = "100%"
        ) |> shinyjs::disabled() |>
          tagAppendChild(opts_help(
            "Extra command-line flags passed to GetOrganelle.",
            href = "https://github.com/Kinggerm/GetOrganelle/wiki",
            id = ns("help_getOrganelle"), nested = TRUE)),
        textInput(
          ns("seeds_db"),
          label = "getOrganelle Seeds:",
          value = current$seeds_db %||% character(0),
          width = "100%"
        ) |> shinyjs::disabled() |>
          tagAppendChild(opts_help(
            "Seed database: reference sequences GetOrganelle uses to start ",
            "recruiting mitochondrial reads.",
            href = "https://smithsonian.github.io/MitoPilot/articles/custom_dbs.html",
            id = ns("help_seeds_db"), nested = TRUE)),
        textInput(
          ns("labels_db"),
          label = "getOrganelle Labels:",
          value = current$labels_db %||% character(0),
          width = "100%"
        ) |> shinyjs::disabled() |>
          tagAppendChild(opts_help(
            "Label database: reference genes GetOrganelle uses to identify ",
            "and extend mitochondrial contigs.",
            href = "https://smithsonian.github.io/MitoPilot/articles/custom_dbs.html",
            id = ns("help_labels_db"), nested = TRUE))
        ),
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
        div(
          class = "form-group shiny-input-container",
          shinyWidgets::prettyCheckbox(
            ns("join_scaffolds"),
            label = "Automatically join multi-scaffold assemblies (Path 0)",
            value = as.logical(current$join_scaffolds %||% 0),
            status = "primary"
          ) |> shinyjs::disabled()
        ),
        tags$p(
          class = "text-muted",
          style = "margin-top: -8px; font-size: 0.85em;",
          paste("Off by default. When on, single-path fragmented assemblies are",
                "reference-ordered into a joined Path 0 (only if scaffolds share a",
                "BLAST hit). Scaffold-to-reference mappings are always computed so",
                "the in-app join editor works regardless of this setting.")
        ),
        size = "m",
        footer = tagList(
          actionButton(ns("update_assemble_opts"), "Update"),
          modalButton("Cancel")
        )
      )
    )

    # Hide the non-selected assembler's inputs. Each help line lives inside its
    # input's container, so hiding the input hides its help too - do NOT hide the
    # help_* ids separately, or showing the input later won't bring the help back.
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

#' Modal listing all BLAST hits for one sample, kept separate per scaffold/path
#'
#' Opened from the "All BLAST Hits" table button. BLAST hits are stored per
#' (ID, path, scaffold) - never merged across scaffolds/paths - so the modal
#' shows one collapsible, ranked candidate list per path/scaffold (accession,
#' species, percents, e-value, per-accession lineage from blast_ref_sequences).
#' Accessions are hyperlinks to NCBI. Header mirrors the assembly-details modal.
#'
#' @param rv the local reactive vals object (uses rv$updating$ID / $Taxon)
#' @param session current shiny session
#' @noRd
blast_hits_modal <- function(rv = NULL, session = getDefaultReactiveDomain()) {
  ns <- session$ns
  con <- session$userData$con
  id <- rv$updating$ID[1]
  taxon <- rv$updating$Taxon[1] %|NA|% "NA"

  cand <- tryCatch(
    dplyr::tbl(con, "blast_ref_candidates") |>
      dplyr::filter(ID == !!id) |>
      dplyr::left_join(
        dplyr::tbl(con, "blast_ref_sequences") |>
          dplyr::select(accession, lineage),
        by = "accession"
      ) |>
      dplyr::select(path, scaffold, rank, accession, species, pident, qcovs, evalue, lineage) |>
      dplyr::collect() |>
      dplyr::arrange(path, scaffold, rank),
    error = function(e) data.frame()
  )

  cand_cols <- list(
    rank = reactable::colDef(name = "Rank", maxWidth = 60, align = "center"),
    accession = reactable::colDef(
      name = "BLAST Hit", html = TRUE, minWidth = 120, cell = rt_ncbi_link()
    ),
    species = reactable::colDef(
      name = "BLAST Species", html = TRUE, minWidth = 160, cell = rt_longtext()
    ),
    pident = reactable::colDef(name = "% Ident", maxWidth = 90, align = "center"),
    qcovs = reactable::colDef(name = "% Cov", maxWidth = 90, align = "center"),
    evalue = reactable::colDef(name = "E-value", maxWidth = 100, align = "center"),
    lineage = reactable::colDef(
      name = "BLAST Lineage", html = TRUE, minWidth = 220, cell = rt_longtext()
    )
  )

  body <- if (nrow(cand) == 0) {
    div(
      style = "padding: 8px; color: #555;",
      "No BLAST hits are available for this sample yet."
    )
  } else {
    combos <- unique(cand[, c("path", "scaffold")])
    combos <- combos[order(combos$path, combos$scaffold), , drop = FALSE]
    multi <- nrow(combos) > 1
    lapply(seq_len(nrow(combos)), function(i) {
      p <- combos$path[i]; s <- combos$scaffold[i]
      sub <- cand[cand$path == p & cand$scaffold == s,
                  c("rank", "accession", "species", "pident", "qcovs", "evalue", "lineage")]
      tbl <- reactable::reactable(
        sub, defaultExpanded = TRUE, bordered = TRUE, highlight = TRUE,
        compact = TRUE, wrap = FALSE,
        defaultColDef = reactable::colDef(align = "left"),
        columns = cand_cols
      )
      # One collapsible section per (path, scaffold). Open by default so the
      # embedded table sizes correctly. For a single scaffold, no wrapper.
      if (!multi) return(tbl)
      tags$details(
        open = NA,
        style = "margin-top: 12px;",
        tags$summary(
          style = "cursor: pointer; font-weight: bold;",
          stringr::str_glue("Path {p} / Scaffold {s} ({nrow(sub)} hits)")
        ),
        div(style = "margin-top: 8px;", tbl)
      )
    })
  }

  showModal(modalDialog(
    title = tagList(
      div(stringr::str_glue("All BLAST hits for ID: {id}")),
      div(
        style = "font-size: 0.85em; font-weight: normal; color: #555; margin-top: 4px;",
        stringr::str_glue("Taxon: {taxon}")
      )
    ),
    size = "l",
    body,
    easyClose = TRUE,
    footer = modalButton("Close")
  ))
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
