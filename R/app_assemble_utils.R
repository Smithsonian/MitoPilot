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
    dplyr::select(assemble_opts, min_assembly_length, assembler)

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
      blast_hits = dplyr::if_else(assemble_switch > 1, "All BLAST Hits", NA_character_),
      maptoref_ref = mtr_display_ref(assembler, maptoref_ref, maptoref_topology)
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
      maptoref_ref,
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
      blast_lineage,
      blast_pident,
      blast_qcovs,
      blast_evalue,
      time_stamp,
      assemble_notes,
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
        title = mp_modal_title(
          paste("Preprocessing options for", mp_n(nrow(rv$updating), "sample"))
        ),
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
            class = "form-group shiny-input-container mp-opts-checkbox",
            mp_checkbox(ns("edit_pre_opts"), label = "Edit", value = FALSE)
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
          label = tagList("fastp options:", tool_help_icon("fastp")),
          value =  current$fastp %||% character(0),
          width = "100%"
        ) |> shinyjs::disabled(),
        opts_help("Command-line flags passed to fastp, which trims adapters and ",
                  "filters low-quality reads.",
                  href = "https://github.com/OpenGene/fastp"),
        mp_checkbox(
          ns("dedup"),
          label = "Remove duplicate reads (fastp --dedup)",
          value = grepl("--dedup", current$fastp %||% "", fixed = TRUE)
        ) |> shinyjs::disabled(),
        opts_help("Drops PCR and optical duplicates before assembly. Lowers ",
                  "depth; in the shipped test data it removed about a fifth of ",
                  "the reads and changed almost no calls."),
        size = "m",
        footer = mp_footer(primary = actionButton(ns("update_pre_opts"), "Save"))
      )
    )

  } else {
    mp_alert(
      title = "Multiple preprocess parameter sets selected",
      text = paste(
        "One modal edits one parameter set. Select samples that share a set,",
        "or edit them one set at a time."
      ),
      type = "warning"
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
        title = mp_modal_title(
          paste("Assembly options for", mp_n(nrow(rv$updating), "sample"))
        ),
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
            class = "form-group shiny-input-container mp-opts-checkbox",
            mp_checkbox(ns("edit_assemble_opts"), label = "Edit", value = FALSE)
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
              choices = c("GetOrganelle", "MitoFinder", "MapToRef"),
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
                         target = "_blank", rel = "noopener", "GetOrganelle",
                         .noWS = "outside"),
                  ", ",
                  tags$a(href = "https://github.com/RemiAllio/MitoFinder",
                         target = "_blank", rel = "noopener", "MitoFinder",
                         .noWS = "outside"),
                  ", or MapToRef, which maps your reads to a reference ",
                  "mitogenome you supply; the relevant tool options appear below."),
        # Each tool's help line is appended INSIDE its input's container (not as a
        # standalone <p>), so it shows/hides together with the field: shinyjs::hide
        # on the input hides its .shiny-input-container, and the help with it.
        textInput(
          ns("mitofinder"),
          label = tagList("MitoFinder options:", tool_help_icon("mitofinder")),
          value = current$mitofinder %||% character(0),
          width = "100%"
        ) |> shinyjs::disabled() |>
          tagAppendChild(opts_help(
            "Extra command-line flags passed to MitoFinder.",
            href = "https://github.com/RemiAllio/MitoFinder",
            id = ns("help_mitofinder"), nested = TRUE)),
        textInput(
          ns("mf_db"),
          label = "MitoFinder database:",
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
          label = tagList("GetOrganelle options:", tool_help_icon("getOrganelle")),
          value = current$getOrganelle %||% character(0),
          width = "100%"
        ) |> shinyjs::disabled() |>
          tagAppendChild(opts_help(
            "Extra command-line flags passed to GetOrganelle.",
            href = "https://github.com/Kinggerm/GetOrganelle/wiki",
            id = ns("help_getOrganelle"), nested = TRUE)),
        textInput(
          ns("seeds_db"),
          label = "GetOrganelle seed database:",
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
          label = "GetOrganelle label database:",
          value = current$labels_db %||% character(0),
          width = "100%"
        ) |> shinyjs::disabled() |>
          tagAppendChild(opts_help(
            "Label database: reference genes GetOrganelle uses to identify ",
            "and extend mitochondrial contigs.",
            href = "https://smithsonian.github.io/MitoPilot/articles/custom_dbs.html",
            id = ns("help_labels_db"), nested = TRUE)),
        selectInput(
          ns("maptoref_mapper"),
          label = "MapToRef mapper:",
          choices = c("bowtie2", "bwa-mem", "bwa-aln"),
          selected = current$maptoref_mapper %||% "bowtie2",
          width = "100%"
        ) |> shinyjs::disabled() |>
          tagAppendChild(opts_help(
            "Read mapper for every pass. bowtie2 and bwa-mem suit modern ",
            "reads; bwa-aln suits short or damaged reads (ancient DNA, ",
            "degraded museum tissue). The first pass, against your ",
            "reference, runs with relaxed seeding added to the options below ",
            "so a distant reference still recruits reads; later passes map to ",
            "the sample's own consensus with the options as given.",
            id = ns("help_maptoref_mapper"), nested = TRUE)),
        textInput(
          ns("maptoref"),
          label = tagList(
            "MapToRef mapper options:",
            mtr_mapper_help(current$maptoref_mapper %||% "bowtie2", ns)
          ),
          value = current$maptoref %||% character(0),
          width = "100%"
        ) |> shinyjs::disabled() |>
          tagAppendChild(mtr_mapper_help_text(current$maptoref_mapper %||% "bowtie2", ns)),
        textInput(
          ns("maptoref_consensus"),
          label = tagList("MapToRef samtools consensus options:",
                          tool_help_icon("samtools-consensus")),
          value = current$maptoref_consensus %||% character(0),
          width = "100%"
        ) |> shinyjs::disabled() |>
          tagAppendChild(opts_help(
            "Flags passed to samtools consensus. -d sets the depth below which ",
            "a site is called N, --min-BQ the base-quality floor. Options ",
            "MitoPilot sets itself are refused.",
            href = "https://www.htslib.org/doc/samtools-consensus.html",
            id = ns("help_maptoref_consensus"), nested = TRUE)),
        numericInput(
          ns("maptoref_iter"),
          label = "Maximum passes:",
          value = current$maptoref_iter %||% 5,
          min = 1, step = 1,
          width = "100%"
        ) |> shinyjs::disabled() |>
          tagAppendChild(opts_help(
            "Maximum passes of consensus-back-into-reference. The loop stops ",
            "early when the sequence stops changing. Try 10 to 25 for a ",
            "distant reference.",
            id = ns("help_maptoref_iter"), nested = TRUE))
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
        opts_help("Samples above the max paths or max scaffolds threshold are ",
                  "marked as failed."),
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
        opts_help("Scaffolds shorter than this threshold are stored, but left ",
                  "out of any further processing."),
        div(
          class = "form-group shiny-input-container",
          mp_checkbox(
            ns("join_scaffolds"),
            label = "Automatically join multi-scaffold assemblies (Path 0)",
            value = as.logical(current$join_scaffolds %||% 0)
          ) |> shinyjs::disabled()
        ),
        opts_help(
          "Off by default. When on, single-path fragmented assemblies are ",
          "reference-ordered into a joined Path 0 (only if scaffolds share a ",
          "BLAST hit). Scaffold-to-reference mappings are always computed so ",
          "the in-app join editor works regardless of this setting."
        ),
        size = "m",
        footer = mp_footer(primary = actionButton(ns("update_assemble_opts"), "Save"))
      )
    )

    # Hide the non-selected assembler's inputs. Each help line lives inside its
    # input's container, so hiding the input hides its help too - do NOT hide the
    # help_* ids separately, or showing the input later won't bring the help back.
    maptoref_ids <- c("maptoref_mapper",
                      "maptoref", "maptoref_consensus", "maptoref_iter")
    if(current$assembler == "GetOrganelle"){
      shinyjs::hide(id = "mitofinder")
      shinyjs::hide(id = "mf_db")
      for (i in maptoref_ids) shinyjs::hide(id = i)
    } else if(current$assembler == "MitoFinder"){
      shinyjs::hide(id = "getOrganelle")
      shinyjs::hide(id = "seeds_db")
      shinyjs::hide(id = "labels_db")
      for (i in maptoref_ids) shinyjs::hide(id = i)
    } else if(current$assembler == "MapToRef"){
      shinyjs::hide(id = "getOrganelle")
      shinyjs::hide(id = "seeds_db")
      shinyjs::hide(id = "labels_db")
      shinyjs::hide(id = "mitofinder")
      shinyjs::hide(id = "mf_db")
    }
  } else {
    mp_alert(
      title = "Multiple assembly parameter sets selected",
      text = paste(
        "One modal edits one parameter set. Select samples that share a set,",
        "or edit them one set at a time."
      ),
      type = "warning"
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
        title = mp_modal_title(
          paste("BLAST options for", mp_n(nrow(rv$updating), "sample"))
        ),
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
            class = "form-group shiny-input-container mp-opts-checkbox",
            mp_checkbox(ns("edit_blast_opts"), label = "Edit", value = FALSE)
          )
        ),
        opts_help("Reusable named set of options applied to the selected samples; ",
                  "check Edit to change values or type a new name to create a set. ",
                  "Saving re-queues the selected samples: their state becomes Ready to run."),
        mp_checkbox(
          ns("run_blast"),
          label = "Run BLAST reference search using assembly as query",
          value = as.logical(current$run_blast %||% 1L)
        ) |> shinyjs::disabled(),
        opts_help("BLAST each assembly against the bundled local database of ",
                  "metazoan mitogenomes to find the closest reference (used for ",
                  "orientation and curation). Annotations for the winning ",
                  "reference are still fetched from NCBI."),
        local_blast_db_note(session$userData$dir_out),
        div(
          id = ns("blast_taxids_group"),
          textInput(
            ns("taxids"),
            label = tagList(
              "Restrict search to taxon IDs (optional):",
              tags$a("NCBI Taxonomy Browser",
                href = "https://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi",
                target = "_blank", rel = "noopener"
              )
            ),
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
          mp_checkbox(
            ns("remote_blast"),
            label = "Remote BLAST",
            value = as.logical(current$remote_blast %||% 0L)
          ) |> shinyjs::disabled(),
          opts_help("Search NCBI over the network instead of the bundled local ",
                    "database. Much slower, rate limited, and requires internet ",
                    "access; use it only to reach sequences the local database ",
                    "does not contain."),
          mp_checkbox(
            ns("remote_fallback"),
            label = "Fall back to remote BLAST when no local hit",
            value = as.logical(current$remote_fallback %||% 1L)
          ) |> shinyjs::disabled(),
          opts_help("If the local search finds no significant hit, retry the search ",
                    "once against NCBI.")
        ),
        div(
          id = ns("blast_entrez_group"),
          # Remote-only setting: hidden unless the remote toggle is on
          style = if (isTRUE(as.logical(current$remote_blast %||% 0L))) NULL else "display: none;",
          textInput(
            ns("entrez_query"),
            label = tagList(
              "Entrez query (remote BLAST only):",
              tags$a("Entrez help documentation",
                href = "https://www.ncbi.nlm.nih.gov/books/NBK3837/",
                target = "_blank", rel = "noopener"
              )
            ),
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
          numericInput(
            ns("max_target_seqs"),
            label = "Candidate reference mitogenomes to retain:",
            value = as.integer(current$max_target_seqs %||% 5L),
            min = 1, max = 50, step = 1, width = "120px"
          ) |> shinyjs::disabled(),
          opts_help("Number of top BLAST hits kept per sample (-max_target_seqs).")
        ),
        div(
          id = ns("blast_extra_group"),
          textAreaInput(
            ns("extra_opts"),
            label = tagList("Additional blastn options:", tool_help_icon("blastn")),
            value = current$extra_opts %||% "",
            width = "100%",
            rows = 2
          ) |> shinyjs::disabled(),
          opts_help("Extra flags passed to blastn. Cannot override -outfmt, ",
                    "-max_hsps, or -max_target_seqs.")
        ),
        size = "m",
        footer = mp_footer(primary = actionButton(ns("update_blast_opts"), "Save"))
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
    mp_alert(
      title = "Multiple BLAST parameter sets selected",
      text = paste(
        "One modal edits one parameter set. Select samples that share a set,",
        "or edit them one set at a time."
      ),
      type = "warning"
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
          stringr::str_glue("Path {p} / Scaffold {s} ({mp_n(nrow(sub), 'hit')})")
        ),
        div(style = "margin-top: 8px;", tbl)
      )
    })
  }

  showModal(modalDialog(
    title = mp_modal_title(
      paste("All BLAST hits for", id),
      subtitle = paste("Taxon:", taxon)
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

#' Rewrite a sample's assemble summary from its active contigs
#'
#' `assemble.length` and `assemble.scaffolds` describe what is currently active,
#' so every action that changes which contigs are ignored has to refresh them.
#' Lengths are listed in full, longest first: three equal fragments read as
#' "6008;6008;6008" rather than collapsing to a single value that looks like a
#' total.
#'
#' @param con database connection
#' @param id sample ID
#'
#' @return (invisibly) list(length, scaffolds) as written
#'
#' @noRd
refresh_assemble_summary <- function(con, id) {
  lens <- DBI::dbGetQuery(
    con, "SELECT length FROM assemblies WHERE ID = ? AND ignore = 0",
    params = list(id)
  )$length
  lens <- sort(as.integer(lens), decreasing = TRUE)

  out <- list(
    length = if (length(lens)) paste(lens, collapse = ";") else NA_character_,
    scaffolds = length(lens)
  )
  DBI::dbExecute(
    con, "UPDATE assemble SET length = ?, scaffolds = ? WHERE ID = ?",
    params = list(out$length, out$scaffolds, id)
  )
  invisible(out)
}

#' Set-state modal, shared by both Assemble variants
#'
#' Words, order and consequences all come from `MP_STATE_META`, so the filter
#' picker, the state icon column and this modal can never disagree (theme T06).
#'
#' @param ids sample IDs in the selection
#' @param current current state code when the selection shares one, else empty
#' @param session current shiny session
#'
#' @noRd
assemble_state_modal <- function(ids, current = character(0),
                                 session = getDefaultReactiveDomain()) {
  ns <- session$ns
  codes <- MP_STATE_SETTABLE
  effect <- c(
    `0` = "Skipped by the next Update.",
    `1` = "Processed by the next Update; results this module already stored are replaced.",
    `2` = "Treated as finished; the next Update skips it.",
    `3` = "Treated as finished with an error; the next Update skips it."
  )
  choice_names <- lapply(codes, function(k) {
    tagList(
      tags$strong(MP_STATE_META[[k]]$label),
      tags$span(class = "text-muted", style = "font-size: 0.85em;",
                paste0(" - ", effect[[k]]))
    )
  })
  showModal(
    modalDialog(
      title = mp_modal_title(paste("Set state for", mp_n(length(ids), "sample"))),
      tags$p(class = "text-muted", mp_id_list(ids)),
      if (length(current) == 1 && current == "4") {
        tags$p(class = "text-muted",
               paste("These are In progress. The pipeline sets that state;",
                     "choose another to take them out of it."))
      } else if (length(current) == 0) {
        tags$p(class = "text-muted", "The selected rows are not all in the same state.")
      },
      shinyWidgets::prettyRadioButtons(
        ns("new_state"),
        label = "New state",
        choiceValues = codes,
        choiceNames = choice_names,
        selected = if (length(current) == 1 && current %in% codes) current else character(0),
        shape = "round",
        status = "primary"
      ),
      size = "m",
      easyClose = TRUE,
      footer = mp_footer(primary = actionButton(ns("update_state"), "Set state"))
    )
  )
}

#' Count the annotation units a set of samples hands to WF2
#'
#' Locking advances every non-ignored (path, scaffold) unit, so the lock toast
#' can say how much work the click created (theme T02).
#'
#' @param con database connection
#' @param ids sample IDs
#'
#' @noRd
count_annotate_units <- function(con, ids) {
  out <- tryCatch(
    dplyr::tbl(con, "assemblies") |>
      dplyr::filter(ignore == 0 & ID %in% !!ids) |>
      dplyr::count() |>
      dplyr::pull(n),
    error = function(e) NA_integer_
  )
  if (length(out) != 1 || is.na(out)) NA_integer_ else as.integer(out)
}

#' Accessible name for each state glyph: the label plus its meaning.
#'
#' @param module "assemble" or "annotate"
#'
#' @noRd
assemble_state_titles <- function(module) {
  codes <- MP_STATE_CODES[[module]]
  stats::setNames(
    paste0(mp_state_labels(module), " - ", mp_state_tips(module)),
    codes
  )
}

#' Lock or unlock the selected samples
#'
#' Locking is the routine, reversible half of the toggle, so it writes and
#' reports. Unlocking drops the samples out of Annotate, so it asks first and
#' writes from `rv$lock_pending` in [assemble_lock_finish()] (theme T02).
#'
#' @param rv the local reactive vals object
#' @param rows row indices of the current selection
#' @param unit noun for one annotation unit ("assembly" or "contig")
#' @param session current shiny session
#'
#' @noRd
assemble_lock_begin <- function(rv, rows, unit = "assembly",
                                session = getDefaultReactiveDomain()) {
  upd <- rv$data |>
    dplyr::select(ID, assemble_lock, join_switch) |>
    dplyr::slice(rows)
  # A mixed selection is already collapsed to one direction by majority.
  lock_current <- as.numeric(names(which.max(table(upd$assemble_lock))))
  if (lock_current != 0) {
    rv$lock_pending <- upd |> dplyr::select(ID, assemble_lock)
    n_locked <- sum(upd$assemble_lock %in% 1)
    mp_confirm(
      session$ns("lock_confirm"),
      title = paste("Unlock", mp_n(n_locked, "sample")),
      text = paste0(
        "Unlocking removes ", mp_n(n_locked, "sample"), " from Annotate. ",
        "Any sample whose state is Ready to run will be re-assembled by the ",
        "next update, replacing its current results."
      ),
      action_label = "Unlock",
      danger = TRUE,
      session = session
    )
    return(invisible(NULL))
  }
  # Locking hands the sample to WF2, which rebuilds the published output path
  # from assemble_opts. Samples whose output is not on disk are held back.
  stale <- tryCatch(
    stale_assemble_dirs(
      session$userData$con, session$userData$dir_out,
      ids = upd$ID, pending_only = FALSE
    ),
    error = function(e) NULL
  )
  if (!is.null(stale) && nrow(stale) > 0L) {
    upd <- upd |> dplyr::filter(!ID %in% stale$ID)
    mp_alert(
      title = "Assembly output not found",
      text = shiny::tags$div(
        shiny::tags$p(
          "These samples were not locked, because Annotation and Curation ",
          "would look for assembly output that is not on disk:"
        ),
        shiny::tags$ul(stale_assemble_items(stale)),
        shiny::tags$p("Either:"),
        shiny::tags$ul(
          shiny::tags$li("set the assembly parameter set back to the name that exists on disk, or"),
          shiny::tags$li("re-run Assembly so the output is published under the assigned name.")
        ),
        shiny::tags$p(
          if (nrow(upd) > 0L) {
            "The rest of the selected samples were locked."
          } else {
            "No other samples remained, so nothing was locked."
          }
        )
      ),
      type = "error", html = TRUE, session = session
    )
    if (nrow(upd) == 0L) {
      return(invisible(NULL))
    }
  }
  # Rows that were already locked are written again but change nothing, so the
  # toast counts only the rows whose value moved.
  changed <- upd |> dplyr::filter(!assemble_lock %in% 1)
  joins <- sum(changed$join_switch %in% 1, na.rm = TRUE)
  units <- count_annotate_units(session$userData$con, changed$ID)
  apply_assemble_lock(rv, upd |> dplyr::select(ID, assemble_lock), 1, session)
  if (nrow(changed) > 0L) {
    mp_toast(
      assemble_lock_message(nrow(changed), units, unit, joins),
      type = "success", session = session
    )
  }
  invisible(NULL)
}

#' Write the unlock the user confirmed
#'
#' @param rv the local reactive vals object
#' @param session current shiny session
#'
#' @noRd
assemble_lock_finish <- function(rv, session = getDefaultReactiveDomain()) {
  upd <- rv$lock_pending
  rv$lock_pending <- NULL
  if (is.null(upd) || nrow(upd) == 0L) {
    return(invisible(NULL))
  }
  n_locked <- sum(upd$assemble_lock %in% 1)
  apply_assemble_lock(rv, upd, 0, session)
  if (n_locked > 0L) {
    mp_toast(paste0(mp_n(n_locked, "sample"), " unlocked."), session = session)
  }
  invisible(NULL)
}

#' Write one lock value to the database and to the table data
#'
#' @param rv the local reactive vals object
#' @param upd data frame of ID / assemble_lock rows
#' @param lock 1 to lock, 0 to unlock
#' @param session current shiny session
#'
#' @noRd
apply_assemble_lock <- function(rv, upd, lock, session = getDefaultReactiveDomain()) {
  upd$assemble_lock <- as.numeric(lock)
  # A locked sample is never admitted by WF1, so a queued join could never run
  # and the flag would sit at 1 forever. Locking resolves it.
  if (lock == 1) {
    upd$join_switch <- NA_integer_
  }
  upd$time_stamp <- as.integer(Sys.time())
  dplyr::tbl(session$userData$con, "assemble") |>
    dplyr::rows_update(
      upd,
      unmatched = "ignore", in_place = TRUE, copy = TRUE, by = "ID"
    )
  rv$data <- rv$data |> dplyr::rows_update(upd, by = "ID")
  trigger("update_assemble_table")
  trigger("refresh_annotate")
  trigger("refresh_export")
  invisible(upd)
}

#' The sentence a lock reports
#'
#' @param n_samples,n_units how many samples were locked, and how many
#'   annotation units that hands to the next step
#' @param unit noun for one annotation unit
#' @param n_joins how many queued scaffold joins the lock cleared
#'
#' @noRd
assemble_lock_message <- function(n_samples, n_units, unit = "assembly",
                                  n_joins = 0) {
  msg <- paste0(mp_n(n_samples, "sample"), " locked")
  if (!is.na(n_units)) {
    msg <- paste0(msg, " - ", mp_n(n_units, unit),
                  " will be annotated on the next update")
  }
  msg <- paste0(msg, ".")
  if (n_joins > 0) {
    msg <- paste0(msg, " A queued scaffold join was cleared for ",
                  mp_n(n_joins, "sample"), ".")
  }
  msg
}

#' IDs of the selected rows that are locked
#'
#' @param rv the local reactive vals object
#' @param rows row indices
#'
#' @noRd
assemble_locked_ids <- function(rv, rows) {
  rows <- rows[!is.na(rows)]
  if (length(rows) == 0) {
    return(character(0))
  }
  rv$data$ID[rows][rv$data$assemble_lock[rows] == 1]
}

#' Rows an options-cell click applies to, or NULL once it has explained itself
#'
#' Replaces the bare `req(FALSE)` that used to abort the click in silence
#' whenever the clicked row sat outside a non-empty selection (theme T01).
#'
#' @param rv the local reactive vals object
#' @param row the clicked row index
#' @param sel the current selection
#' @param session current shiny session
#'
#' @noRd
assemble_opts_rows <- function(rv, row, sel, session = getDefaultReactiveDomain()) {
  if (!row_in_selection(row, sel, rv$data$ID[row], session = session)) {
    return(NULL)
  }
  rows <- unique(c(row, sel))
  if (!need_unlocked(assemble_locked_ids(rv, rows), session = session)) {
    return(NULL)
  }
  rows
}

#' What an options save did: the set applied, and the re-queue that follows
#'
#' @param n rows written
#' @param set name of the parameter set applied
#' @param noun unit noun for those rows
#' @param session current shiny session
#'
#' @noRd
mp_opts_saved_toast <- function(n, set, noun = "sample",
                                session = getDefaultReactiveDomain()) {
  mp_toast(
    paste0(mp_n(n, noun), " switched to ", set, " and set to Ready to run."),
    session = session
  )
}

#' Canonical header text for a table column
#'
#' @param field data-frame column name, or a semantic key from `MP_COL_NAMES`
#'
#' @noRd
mp_col_name <- function(field) unname(MP_COL_NAMES[[field]])

#' Canonical header, with its tooltip when the name needs one
#'
#' @param field see [mp_col_name()]
#' @param tip override tooltip; defaults to the `MP_COL_TIPS` entry
#'
#' @noRd
mp_col_header <- function(field, tip = NULL) {
  if (is.null(tip)) {
    tip <- unname(MP_COL_TIPS[field])
    if (is.na(tip)) tip <- NULL
  }
  rt_header(mp_col_name(field), tip)
}

#' Column width that fits the longest value in `x`
#'
#' 8.5px per character is measured for the default Helvetica/Arial stack at
#' 14px; 26px is the compact cell padding plus a safety margin (theme T09).
#'
#' @param x the column values
#' @param floor,cap width bounds, so one pathological value cannot eat the
#'   viewport
#'
#' @noRd
mp_fit_width <- function(x, floor = 120, cap = 260) {
  n <- suppressWarnings(max(nchar(as.character(x)), na.rm = TRUE))
  if (!is.finite(n)) {
    return(floor)
  }
  max(floor, min(cap, ceiling(8.5 * n) + 26))
}

#' The two CSV download buttons, labelled for what they actually do
#'
#' `mp_csv_download_row()` calls the first download "filtered", which
#' reactable 0.4.5 cannot report. Both buttons say what they really send
#' (theme T13).
#'
#' @param ns the module's namespace function
#'
#' @noRd
assemble_csv_row <- function(ns) {
  row <- mp_csv_download_row(ns)
  labels <- c("Download selected rows", "Download all rows")
  for (i in seq_along(row$children)) {
    row$children[[i]]$children[[2]] <- labels[i]
  }
  row
}

#' The row-count and selection line above a sample table
#'
#' @param n_visible,n_total,n_selected counts for the current view
#'
#' @noRd
assemble_table_status <- function(n_visible, n_total, n_selected, noun = "sample") {
  div(
    class = "mp-table-status", role = "status", `aria-live` = "polite",
    span(
      "Showing ", tags$b(n_visible), " of ", tags$b(n_total), " ",
      sub("^\\S+ ", "", mp_n(n_total, noun))
    ),
    span(class = "mp-sep", `aria-hidden` = "true", HTML("&middot;")),
    span(tags$b(n_selected), " selected"),
    # Filtered-out rows are hidden by CSS, so reactable's own noData never fires.
    if (n_visible == 0) {
      span(class = "mp-empty-state", "No rows match the current filters.")
    }
  )
}


# Mapper help: one "?" icon and one help line per mapper, only the selected
# mapper's pair visible. Toggled client-side when the mapper select changes.
.mtr_mapper_help_spec <- list(
  "bowtie2" = list(
    text = c("Flags passed to bowtie2 on every pass. Presets: --fast-local, ",
             "--sensitive-local, --very-sensitive-local (default), ",
             "--very-sensitive-local -N 1 for a distant reference."),
    href = "https://bowtie-bio.sourceforge.net/bowtie2/manual.shtml"),
  "bwa-mem" = list(
    text = c("Flags passed to bwa mem on every pass. Empty (default) uses bwa's ",
             "own defaults; for example -B 2 -T 20 for a distant reference."),
    href = "https://bio-bwa.sourceforge.net/bwa.shtml"),
  "bwa-aln" = list(
    text = c("Flags passed to bwa aln on every pass (bwa sampe pairs the ",
             "mates). Default -l 1024 -n 0.01 -o 2 disables seeding and ",
             "relaxes the edit distance, the usual ancient-DNA settings."),
    href = "https://bio-bwa.sourceforge.net/bwa.shtml")
)

#' @noRd
mtr_mapper_help <- function(selected, ns) {
  tagList(lapply(names(.mtr_mapper_help_spec), function(m) {
    el <- tags$span(id = ns(paste0("help_icon_", m)), tool_help_icon(m))
    if (identical(m, selected)) el else shinyjs::hidden(el)
  }))
}

#' @noRd
mtr_mapper_help_text <- function(selected, ns) {
  tagList(lapply(names(.mtr_mapper_help_spec), function(m) {
    sp <- .mtr_mapper_help_spec[[m]]
    el <- do.call(opts_help, c(as.list(sp$text), list(
      href = sp$href, id = ns(paste0("help_maptoref_", m)), nested = TRUE)))
    if (identical(m, selected)) el else shinyjs::hidden(el)
  }))
}

#' Set one sample's MapToRef reference and, for a FASTA, its topology
#' @noRd
maptoref_ref_modal <- function(id, ref, topology, session = getDefaultReactiveDomain()) {
  ns <- session$ns
  showModal(
    modalDialog(
      title = mp_modal_title(paste("MapToRef reference for", id)),
      textInput(
        ns("maptoref_ref_value"),
        label = "Reference:",
        value = ref %|NA|% "",
        width = "100%"
      ) |> tagAppendChild(opts_help(
        "Absolute path on this machine, URL, or NCBI nucleotide accession ",
        "(for example NC_002333) of one complete mitogenome to map this ",
        "sample's reads against. Blank clears the reference. Saving ",
        "re-queues the sample.", nested = TRUE)),
      {
        el <- selectInput(
          ns("maptoref_ref_topology"),
          label = "Reference topology:",
          choices = c("", "circular", "linear"),
          selected = topology %|NA|% "",
          width = "100%"
        ) |> tagAppendChild(opts_help(
          "Required for a FASTA reference, whose header carries no topology. ",
          "A GenBank record or accession supplies its own.", nested = TRUE))
        if (!.mtr_needs_topology(ref %|NA|% "")) shinyjs::hidden(el) else el
      },
      footer = mp_footer(primary = actionButton(ns("update_maptoref_ref"), "Save"))
    )
  )
}
