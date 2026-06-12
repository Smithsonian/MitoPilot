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
    dplyr::select(ID, blast_accession, blast_species, blast_lineage, blast_pident, blast_qcovs,
                  dplyr::any_of("poor_blast_ref"))

  taxa <- dplyr::tbl(db, "samples") |>
    dplyr::select(ID, Taxon)

  annotations <- dplyr::tbl(db, "annotations") |>
    dplyr::select(ID, type, warnings) |>
    dplyr::collect() |>
    dplyr::group_by(ID) |>
    dplyr::summarise(
      warnings_details = warnings[warnings != ""] |>
        paste(collapse = "; "),
      ORFCount = sum(type == "ORF")
    )

  # use_orffinder per sample, to blank the ORF count when ORF finding is off
  orf_enabled <- dplyr::tbl(db, "annotate") |>
    dplyr::select(ID, orf_opts) |>
    dplyr::left_join(dplyr::tbl(db, "orf_opts"), by = "orf_opts") |>
    dplyr::select(ID, use_orffinder) |>
    dplyr::collect()

  assemblies_length <- dplyr::tbl(db, "assemblies") |>
    dplyr::filter(ignore != 1) |>
    dplyr::group_by(ID, path) |>
    dplyr::summarise(length_raw = sum(length_raw, na.rm = TRUE), .groups = "drop") |>
    dplyr::collect() |>
    dplyr::mutate(path = as.character(path))

  dplyr::left_join(assemble, annotate, by = "ID") |>
    dplyr::left_join(taxa, by = "ID") |>
    dplyr::collect() |>
    dplyr::left_join(annotations, by = "ID") |>
    dplyr::left_join(orf_enabled, by = "ID") |>
    dplyr::left_join(assemblies_length, by = c("ID", "path")) |>
    dplyr::select(
      annotate_lock,
      annotate_switch,
      ID,
      Taxon,
      ID_verified,
      annotate_opts,
      curate_opts,
      orf_opts,
      length_raw,
      length,
      topology,
      scaffolds,
      blast_accession,
      blast_species,
      blast_lineage,
      blast_pident,
      blast_qcovs,
      structure,
      PCGCount,
      tRNACount,
      rRNACount,
      ORFCount,
      missing,
      extra,
      warnings,
      reviewed,
      problematic,
      time_stamp,
      annotate_notes,
      warnings_details,
      use_orffinder,
      dplyr::any_of("poor_blast_ref")
    ) |>
    # Blank the ORF count when ORF finding is disabled for the sample
    dplyr::mutate(
      ORFCount = dplyr::if_else(
        is.na(use_orffinder) | use_orffinder != 1L,
        NA_integer_,
        as.integer(ORFCount)
      )
    ) |>
    dplyr::select(-use_orffinder) |>
    dplyr::arrange(dplyr::desc(time_stamp)) |>
    dplyr::mutate(blast_ref_status = poor_blast_ref) |>
    dplyr::relocate(blast_ref_status, .after = blast_accession) |>
    dplyr::mutate(
      ID_verified = dplyr::coalesce(ID_verified, "no"),
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
#' @param query query sequences
#' @param max_blast_hits Maximum number of top BLAST hits to retain (default = 10)
#'
#' @export
#'
get_top_hits_local <- function(
  ref_db = NULL,
  query = NULL,
  max_blast_hits = 10
) {
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
    dplyr::arrange(dplyr::desc(similarity)) |>
    dplyr::slice_head(n = as.numeric(max_blast_hits))
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
    cur_params <- rv$curate_opts$params[rv$curate_opts$curate_opts == rv$updating$curate_opts[1]] |>
      jsonlite::fromJSON()

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
        div(
          style = "display: flex; flex-flow: row nowrap; align-items: center; gap: 2em;",
          div(
            style = "flex: 1",
            textInput(
              ns("mitos_opts"),
              label = tagList("Mitos2 options:", tool_help_icon("mitos")),
              value = current$mitos_opts %||% character(0),
              width = "100%"
            ) |> shinyjs::disabled()
          ),
          div(
            style = "margin-top: 24px;",
            shinyWidgets::prettyCheckbox(
              ns("use_mitos_best"),
              label = "Use --best flag",
              value = isTRUE(as.logical(current$use_mitos_best %||% 1L)),
              status = "primary"
            ) |> shinyjs::disabled()
          )
        ),
        div(
          style = "display: flex; flex-flow: row nowrap; align-items: center; gap: 2em;",
          div(
            style = "flex: 1; min-width: 0; word-wrap : break-word; word-break: break-word;",
            selectizeInput(
              ns("mitos_ref_dir"),
              label = "ref_dir",
              choices = unique(rv$annotate_opts$ref_dir),
              selected = current$ref_dir %||% character(0),
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
              # choices = unique(rv$annotate_opts$ref_db),
              choices = c("Metazoa_RefSeq89", "Chordata"),
              selected = current$ref_db %||% character(0),
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
          label = tagList("trnAScan-SE options:", tool_help_icon("trnaScan-SE")),
          value = current$trnaScan_opts %||% character(0),
          width = "100%"
        ) |> shinyjs::disabled(),
        shinyWidgets::prettyCheckbox(
          ns("use_arwen"),
          label = "Also use ARWEN for tRNA prediction",
          value = isTRUE(as.logical(current$use_arwen)),
          status = "primary"
        ) |> shinyjs::disabled(),
        textInput(
          ns("arwen_opts"),
          label = tagList("ARWEN options:", tool_help_icon("arwen")),
          value = current$arwen_opts %||% character(0),
          width = "100%"
        ) |> shinyjs::disabled(),
        shinyWidgets::prettyCheckbox(
          ns("use_aragorn"),
          label = "Also use ARAGORN for tRNA prediction",
          value = isTRUE(as.logical(current$use_aragorn)),
          status = "primary"
        ) |> shinyjs::disabled(),
        textInput(
          ns("aragorn_opts"),
          label = tagList("ARAGORN options:", tool_help_icon("aragorn")),
          value = current$aragorn_opts %||% character(0),
          width = "100%"
        ) |> shinyjs::disabled(),
        shinyWidgets::prettyCheckbox(
          ns("coverage_trim"),
          label = "Trim low-coverage ends of linear assemblies",
          value = isTRUE(as.logical(current$coverage_trim %||% 1L)),
          status = "primary"
        ) |> shinyjs::disabled(),
        shinyWidgets::prettyCheckbox(
          ns("feature_trim"),
          label = "Trim un-annotated ends of linear contigs (by gene features)",
          value = isTRUE(as.logical(current$feature_trim %||% 1L)),
          status = "primary"
        ) |> shinyjs::disabled(),
        shinyWidgets::prettyCheckbox(
          ns("retain_low_conf_trna"),
          label = "Retain low-confidence (NNN anticodon) tRNAs",
          value = isTRUE(as.logical(current$retain_low_conf_trna %||% 0L)),
          status = "primary"
        ) |> shinyjs::disabled(),
        div(
          selectizeInput(
            ns("start_gene"),
            label = "starting gene for circular assemblies",
            choices = MITO_GENE_CHOICES,
            selected = current$start_gene %||% character(0),
            width = "100%",
            options = list(
              create = TRUE,
              maxItems = 1
            )
          ) |> shinyjs::disabled()
        ),
        size = "m",
        footer = tagList(
          actionButton(ns("update_annotate_opts"), "Update"),
          modalButton("Cancel")
        )
      )
    )
  } else {
    shinyWidgets::show_alert(
      title = "Multiple annotation parameter sets selected",
      text = "Cannot edit different parameter sets simultaneously",
      type = "error",
      closeOnClickOutside = FALSE,
    )
  }
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
          ),
          div(
            style = "flex: 1",
            numericInput(
              ns("max_blast_hits"),
              label = "Max BLAST hits:",
              value = current$max_blast_hits %||% character(0),
              min = 1,
              max = 1000,
              width = "100%"
            ) |> shinyjs::disabled()
          )
        ),
        div(
          style = "display: flex; align-items: center; gap: 2em",
          div(
            style = "flex: 1; min-width: 0; word-wrap : break-word; word-break: break-word;",
            selectizeInput(
              ns("curate_ref_dir"),
              label = "ref_dir",
              choices = unique(rv$curate_opts$ref_dir),
              selected = current$ref_dir %||% character(0),
              width = "100%",
              options = list(
                create = TRUE,
                maxItems = 1
              ),
            ) |> shinyjs::disabled()
          ),
          div(
            style = "flex: 1",
            selectizeInput(
              ns("curate_ref_db"),
              label = "ref_db",
              # choices = unique(rv$annotate_opts$ref_db),
              choices = c("Metazoa_RefSeq89", "Metazoa_RefSeq231", "Metazoa_RefSeq231_custom", "Chordata", "Chordata_custom"),
              selected = current$ref_db %||% character(0),
              width = "100%",
              options = list(
                create = TRUE,
                maxItems = 1
              )
            ) |> shinyjs::disabled()
          )
        ),
        div(
          style = "flex: 1",
          selectizeInput(
            ns("target"),
            label = "Target:",
            # Values = full keys (dispatch); labels = "Scientific (common)".
            choices = local({
              tg <- sort(names(RULESET_MAP))
              stats::setNames(tg, vapply(tg, function(k)
                paste0(RULESET_MAP[[k]]$ncbi, " (", RULESET_MAP[[k]]$label, ")"),
                character(1)))
            }),
            selected = current$target %||% character(0),
            width = "100%",
            options = list(
              create = TRUE,
              maxItems = 1
            )
          ) |> shinyjs::disabled()
        ),
        listviewer::reactjsonOutput(ns("params")),
        size = "m",
        footer = tagList(
          actionButton(ns("update_curate_opts"), "Update"),
          modalButton("Cancel")
        )
      )
    )
  } else {
    shinyWidgets::show_alert(
      title = "Multiple curation parameter sets selected",
      text = "Cannot edit different parameter sets simultaneously",
      type = "error",
      closeOnClickOutside = FALSE,
    )
  }
}

#' Update the ORF-finder options
#'
#' @param rv the local reactive vals object
#' @param session current shiny session
#'
#' @noRd
orf_opts_modal <- function(rv = NULL, session = getDefaultReactiveDomain()) {
  ns <- session$ns
  req(rv$updating)
  current <- list()

  if (length(unique(rv$updating$orf_opts)) == 1) {
    current <- rv$orf_opts[rv$orf_opts$orf_opts == rv$updating$orf_opts[1], ]
    # Fall back to the default parameter set if the sample's orf_opts is unset
    # or doesn't match a saved set, so the fields are never blank.
    if (nrow(current) == 0) {
      current <- rv$orf_opts[rv$orf_opts$orf_opts == "default", ]
    }

    # The per-run parameters are only relevant when ORF finding is enabled, so
    # they live in one div that is hidden while "Run ORF finder step" is off.
    orf_on <- isTRUE(as.logical(current$use_orffinder %||% 0L))
    orf_param_opts <- div(
      id = ns("orf_param_opts"),
      div(
        style = "display: flex; flex-flow: row nowrap; align-items: center; gap: 2em;",
        div(
          style = "flex: 1",
          numericInput(
            ns("orf_opts_cpus"), "CPUs:",
            width = "100%",
            value = current$cpus %||% numeric(0)
          ) |> shinyjs::disabled()
        ),
        div(
          style = "flex: 1",
          numericInput(
            ns("orf_opts_memory"), "Memory (GB):",
            width = "100%",
            value = current$memory %||% numeric(0)
          ) |> shinyjs::disabled()
        )
      ),
      div(
        style = "display: flex; flex-flow: row nowrap; align-items: center; gap: 2em;",
        div(
          style = "flex: 1",
          numericInput(
            ns("orf_min_len"),
            label = "Min ORF length (nt):",
            value = current$orf_min_len %||% character(0),
            min = 30,
            width = "100%"
          ) |> shinyjs::disabled()
        ),
        div(
          style = "flex: 1",
          numericInput(
            ns("orf_max_overlap"),
            label = "Max overlap w/ existing annotations (fraction of ORF length):",
            value = current$orf_max_overlap %||% character(0),
            min = 0,
            max = 1,
            step = 0.05,
            width = "100%"
          ) |> shinyjs::disabled()
        )
      ),
      shinyWidgets::prettyCheckbox(
        ns("orf_nested"),
        label = "Include nested/overlapping ORFs",
        value = isTRUE(as.logical(current$orf_nested %||% 0L)),
        status = "primary"
      ) |> shinyjs::disabled(),
      textInput(
        ns("orffinder_opts"),
        label = tagList("ORFfinder options:", tool_help_icon("orffinder")),
        value = current$orffinder_opts %||% character(0),
        width = "100%"
      ) |> shinyjs::disabled(),
      helpText(
        "The genetic code (-g) and minimum length (-ml) are set automatically",
        "from the project genetic code and the Min ORF length above; do not set",
        "them here."
      )
    )
    if (!orf_on) orf_param_opts <- shinyjs::hidden(orf_param_opts)

    showModal(
      modalDialog(
        title = stringr::str_glue("Setting ORF-finder Options for {nrow(rv$updating)} Samples"),
        helpText(
          "When running the ORF step, consider disabling un-annotated end trimming",
          "in the annotation options"
        ),
        div(
          style = "display: flex; flex-flow: row nowrap; align-items: center; gap: 2em;",
          selectizeInput(
            ns("orf_opts"),
            label = "Parameter set name:",
            choices = rv$orf_opts$orf_opts,
            selected = current$orf_opts,
            options = list(
              create = TRUE,
              maxItems = 1
            )
          ),
          div(
            class = "form-group shiny-input-container",
            style = "margin-top: 39px;",
            shinyWidgets::prettyCheckbox(
              ns("edit_orf_opts"),
              label = "Edit",
              value = FALSE,
              status = "primary"
            )
          )
        ),
        shinyWidgets::prettyCheckbox(
          ns("use_orffinder"),
          label = "Run ORF finder step (after curation; finds ORFs in unannotated regions)",
          value = isTRUE(as.logical(current$use_orffinder %||% 0L)),
          status = "primary"
        ) |> shinyjs::disabled(),
        orf_param_opts,
        size = "m",
        footer = tagList(
          actionButton(ns("update_orf_opts"), "Update"),
          modalButton("Cancel")
        )
      )
    )
  } else {
    shinyWidgets::show_alert(
      title = "Multiple ORF parameter sets selected",
      text = "Cannot edit different parameter sets simultaneously",
      type = "error",
      closeOnClickOutside = FALSE,
    )
  }
}
