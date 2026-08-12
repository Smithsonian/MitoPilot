#' Per-unit annotate frame (one row per (ID, path, scaffold) annotation unit)
#'
#' Multi-assembly: a locked sample can carry more than one annotation unit, each
#' its own row in the Annotate table. This returns one row per non-ignored unit,
#' with the full per-unit detail the table and annotation editor need. Wrapped by
#' [fetch_annotate_data] (adds the details/output actions + column order).
#'
#' @param session reactive session
#' @noRd
fetch_annotate_units <- function(session = getDefaultReactiveDomain()) {
  db <- session$userData$con

  annotate <- dplyr::tbl(db, "annotate")

  # Only locked samples advance; poor_blast_ref (ref-fetch status) is per sample.
  assemble <- dplyr::tbl(db, "assemble") |>
    dplyr::filter(assemble_lock == 1) |>
    dplyr::select(ID, dplyr::any_of("poor_blast_ref"))

  taxa <- dplyr::tbl(db, "samples") |>
    dplyr::select(ID, Taxon)

  # Export state is per unit, so a fragmented sample's scaffolds can legitimately
  # carry different groups / export times rather than one broadcast sample value.
  export_state <- dplyr::tbl(db, "export") |>
    dplyr::select(ID, path, scaffold, export_group, export_time_stamp)

  # Per-unit annotations rollup (warnings + ORF count) keyed on the unit.
  annotations <- dplyr::tbl(db, "annotations") |>
    dplyr::select(ID, path, scaffold, type, warnings) |>
    dplyr::collect() |>
    dplyr::group_by(ID, path, scaffold) |>
    dplyr::summarise(
      warnings_details = warnings[!is.na(warnings) & warnings != ""] |>
        paste(collapse = "; "),
      ORFCount = sum(type == "ORF"),
      .groups = "drop"
    )

  # use_orffinder per sample, to blank the ORF count when ORF finding is off
  orf_enabled <- dplyr::tbl(db, "annotate") |>
    dplyr::select(ID, path, scaffold, orf_opts) |>
    dplyr::left_join(dplyr::tbl(db, "orf_opts"), by = "orf_opts") |>
    dplyr::select(ID, path, scaffold, use_orffinder) |>
    dplyr::collect()

  # Per-unit assembly facts: raw length, topology, and THIS scaffold's own BLAST
  # hit. Each scaffold/unit is independent, so BLAST is per scaffold (from
  # assemblies), not the sample-level assemble.blast_*. The ignore!=1 filter (via
  # the inner join) gates units to a non-ignored assembly (annotate can retain
  # stale rows after a Path-0 join, whose assemblies are ignore=1).
  assemblies_unit <- dplyr::tbl(db, "assemblies") |>
    dplyr::filter(ignore != 1) |>
    dplyr::select(ID, path, scaffold, length_raw, asm_topology = topology) |>
    dplyr::collect() |>
    # Resolved reference: the user's per-unit override if set, else this scaffold's
    # own BLAST top hit. Shared with the Export table, synteny default and export
    # note so all four name the same reference.
    dplyr::left_join(unit_ref_facts(db), by = c("ID", "path", "scaffold"))

  dplyr::inner_join(annotate, assemble, by = "ID") |>
    dplyr::left_join(taxa, by = "ID") |>
    dplyr::left_join(export_state, by = c("ID", "path", "scaffold")) |>
    dplyr::collect() |>
    dplyr::inner_join(assemblies_unit, by = c("ID", "path", "scaffold")) |>
    dplyr::left_join(annotations, by = c("ID", "path", "scaffold")) |>
    dplyr::left_join(orf_enabled, by = c("ID", "path", "scaffold")) |>
    # topology: curated (annotate) value once validated, else the assembly's.
    dplyr::mutate(topology = dplyr::coalesce(topology, asm_topology)) |>
    dplyr::select(
      annotate_lock,
      annotate_switch,
      ID,
      path,
      scaffold,
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
      # this scaffold's own BLAST top hit; the table marks the cell when it differs
      # from the displayed (overridden) accession
      blast_accession_auto,
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
      partial,
      time_stamp,
      export_group,
      export_time_stamp,
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
    dplyr::arrange(ID, path, scaffold) |>
    # Ref-align status is only meaningful when this unit has a real BLAST hit.
    dplyr::mutate(
      blast_ref_status = dplyr::if_else(
        is.na(blast_accession) | blast_accession == "NO HIT",
        NA_character_, poor_blast_ref
      )
    ) |>
    dplyr::relocate(blast_ref_status, .after = blast_accession) |>
    dplyr::mutate(ID_verified = dplyr::coalesce(ID_verified, "no"))
}

#' Populate the Annotate table - one row per (ID, path, scaffold) unit
#'
#' Multi-assembly: each non-ignored scaffold/path is its own annotation unit and
#' its own row (no parent/child rollup). Per-sample columns (Taxon, BLAST ref)
#' repeat across a sample's unit rows. `path`/`scaffold` are shown so the user can
#' tell units apart and they key every per-row action.
#'
#' @param session reactive session
#' @noRd
fetch_annotate_data <- function(session = getDefaultReactiveDomain()) {
  fetch_annotate_units(session) |>
    dplyr::mutate(
      output = dplyr::case_when(
        annotate_switch > 1 ~ "output",
        .default = NA_character_
      ),
      view = dplyr::case_when(
        annotate_switch > 1 ~ "details",
        .default = NA_character_
      )
    ) |>
    dplyr::arrange(dplyr::desc(time_stamp), ID, path, scaffold) |>
    # reactable renders in data-frame order: keep main's layout with path/scaffold
    # inserted right after ID.
    dplyr::select(
      dplyr::any_of(c(
        "annotate_lock", "annotate_switch", "ID", "path", "scaffold", "Taxon",
        "ID_verified", "annotate_opts", "curate_opts", "orf_opts", "length_raw",
        "length", "topology", "scaffolds", "blast_accession", "blast_ref_status",
        "blast_accession_auto", "blast_species", "blast_lineage", "blast_pident",
        "blast_qcovs", "structure", "PCGCount", "tRNACount", "rRNACount",
        "ORFCount", "missing", "extra", "warnings", "reviewed", "problematic",
        "partial", "time_stamp", "export_group", "export_time_stamp",
        "annotate_notes", "warnings_details", "poor_blast_ref", "output", "view"
      )),
      dplyr::everything()
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
        opts_help("Reusable named set of options applied to the selected samples; ",
                  "check Edit to change values or type a new name to create a set."),
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
            selectizeInput(
              ns("start_gene"),
              label = "starting gene for circular assemblies",
              choices = MITO_GENE_CHOICES,
              selected = current$start_gene %||% character(0),
              width = "100%",
              options = list(
                create = FALSE,
                maxItems = 1
              )
            ) |> shinyjs::disabled()
          )
        ),
        opts_help("Circular mitogenomes are rotated to begin at this gene, giving a ",
                  "consistent start position across samples (e.g. tRNA-Phe in vertebrates)."),
        # Annotation tools section, boxed off from the surrounding options.
        div(
          style = paste(
            "border: 1px solid #ddd; border-radius: 6px;",
            "padding: 12px 14px 4px; margin: 8px 0 14px;"
          ),
        tags$div(
          style = "font-weight: bold; margin-bottom: 8px;",
          "Annotation tools"
        ),
        opts_help("When merging results from multiple annotators, higher-priority ",
                  "tools win and lower-priority tools only fill gaps. Genes and rRNAs: ",
                  "MITOS2 > MitoFinder. tRNAs: tRNAscan-SE > ARWEN > ARAGORN > MITOS2."),
        shinyWidgets::prettyCheckbox(
          ns("use_mitos"),
          label = "Use MITOS2",
          value = isTRUE(as.logical(current$use_mitos %||% 1L)),
          status = "primary"
        ) |> shinyjs::disabled(),
        div(
          id = ns("mitos_opts_box"),
          textInput(
            ns("mitos_opts"),
            label = tagList("MITOS2 options:", tool_help_icon("mitos")),
            value = current$mitos_opts %||% character(0),
            width = "100%"
          ) |> shinyjs::disabled()
        ),
        div(
          id = ns("mitos_best_box"),
          style = "display: flex; flex-flow: row nowrap; align-items: center; margin-bottom: 8px;",
          shinyWidgets::prettyCheckbox(
            ns("use_mitos_best"),
            label = "Keep only best of overlapping predictions (--best)",
            value = isTRUE(as.logical(current$use_mitos_best %||% 1L)),
            status = "primary"
          ) |> shinyjs::disabled()
        ),
        opts_help("MITOS2 is the primary annotator (genes, tRNAs, rRNAs). The ",
                  "options box takes extra MITOS2 flags; '--best' keeps only the ",
                  "top-scoring of overlapping predictions.",
                  href = "http://mitos2.bioinf.uni-leipzig.de/index.py"),
        div(
          id = ns("rescue_no_trna_box"),
          style = "display: flex; flex-flow: row nowrap; align-items: center; margin-bottom: 8px;",
          shinyWidgets::prettyCheckbox(
            ns("rescue_no_trna"),
            label = "Rescue rRNAs/PCGs lost to tRNA overlap",
            value = isTRUE(as.logical(current$rescue_no_trna %||% 1L)),
            status = "primary"
          ) |> shinyjs::disabled()
        ),
        opts_help("Runs MITOS2 a second time without tRNA prediction to recover ",
                  "rRNAs or PCGs that MITOS2 drops when a tRNA overlaps them. ",
                  "Adds some runtime."),
        div(
          id = ns("mitos_ref_box"),
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
          opts_help("Location (ref_dir) and name (ref_db) of the MITOS2 reference ",
                    "database used for annotation; pick a clade closest to your samples.",
                    href = "https://smithsonian.github.io/MitoPilot/articles/custom_dbs.html")
        ),
        shinyWidgets::prettyCheckbox(
          ns("use_trnaScan"),
          label = "Use tRNAscan-SE",
          value = isTRUE(as.logical(current$use_trnaScan %||% 1L)),
          status = "primary"
        ) |> shinyjs::disabled(),
        div(
          id = ns("trnascan_opts_box"),
          textInput(
            ns("trnaScan_opts"),
            label = tagList("trnAScan-SE options:", tool_help_icon("trnaScan-SE")),
            value = current$trnaScan_opts %||% character(0),
            width = "100%"
          ) |> shinyjs::disabled()
        ),
        shinyWidgets::prettyCheckbox(
          ns("use_mitofinder"),
          label = "Also use MitoFinder for annotation",
          value = isTRUE(as.logical(current$use_mitofinder %||% 0L)),
          status = "primary"
        ) |> shinyjs::disabled(),
        div(
          id = ns("mitofinder_box"),
          textInput(
            ns("mitofinder_db"),
            label = "MitoFinder reference database (.gb format)",
            value = current$mitofinder_db %||% character(0),
            width = "100%"
          ) |> shinyjs::disabled(),
          opts_help("GenBank-format (.gb) reference used by MitoFinder to identify ",
                    "genes. The MitoPilot custom_assembly_db() function can generate ",
                    "this annotation database for a clade.",
                    href = "https://smithsonian.github.io/MitoPilot/articles/custom_dbs.html"),
          div(
            style = "display: flex; flex-flow: row nowrap; align-items: center; gap: 2em;",
            div(
              style = "flex: 2",
              textInput(
                ns("mitofinder_opts"),
                label = tagList("MitoFinder options:", tool_help_icon("mitofinder")),
                value = current$mitofinder_opts %||% character(0),
                width = "100%"
              ) |> shinyjs::disabled()
            ),
            div(
              style = "margin-top: 24px;",
              shinyWidgets::prettyCheckbox(
                ns("mitofinder_new_genes"),
                label = "Annotate non-standard genes (--new-genes)",
                value = isTRUE(as.logical(current$mitofinder_new_genes %||% 0L)),
                status = "primary"
              ) |> shinyjs::disabled()
            ),
            div(
              style = "margin-top: 24px;",
              shinyWidgets::prettyCheckbox(
                ns("mitofinder_allow_introns"),
                label = "Search for genes with introns (--allow-intron)",
                value = isTRUE(as.logical(current$mitofinder_allow_introns %||% 0L)),
                status = "primary"
              ) |> shinyjs::disabled()
            )
          )
        ),
        shinyWidgets::prettyCheckbox(
          ns("use_arwen"),
          label = "Also use ARWEN for tRNA prediction",
          value = isTRUE(as.logical(current$use_arwen)),
          status = "primary"
        ) |> shinyjs::disabled(),
        div(
          id = ns("arwen_box"),
          textInput(
            ns("arwen_opts"),
            label = tagList("ARWEN options:", tool_help_icon("arwen")),
            value = current$arwen_opts %||% character(0),
            width = "100%"
          ) |> shinyjs::disabled()
        ),
        shinyWidgets::prettyCheckbox(
          ns("use_aragorn"),
          label = "Also use ARAGORN for tRNA prediction",
          value = isTRUE(as.logical(current$use_aragorn)),
          status = "primary"
        ) |> shinyjs::disabled(),
        div(
          id = ns("aragorn_box"),
          textInput(
            ns("aragorn_opts"),
            label = tagList("ARAGORN options:", tool_help_icon("aragorn")),
            value = current$aragorn_opts %||% character(0),
            width = "100%"
          ) |> shinyjs::disabled()
        )
        ),
        shinyWidgets::prettyCheckbox(
          ns("coverage_trim"),
          label = "Trim low-coverage ends of linear assemblies",
          value = isTRUE(as.logical(current$coverage_trim %||% 1L)),
          status = "primary"
        ) |> shinyjs::disabled(),
        opts_help("Remove poorly supported bases at the ends of linear contigs based ",
                  "on read depth."),
        shinyWidgets::prettyCheckbox(
          ns("feature_trim"),
          label = "Trim un-annotated ends of linear contigs (by gene features)",
          value = isTRUE(as.logical(current$feature_trim %||% 1L)),
          status = "primary"
        ) |> shinyjs::disabled(),
        opts_help("Trim linear contig ends that extend past the outermost annotated ",
                  "gene features."),
        shinyWidgets::prettyCheckbox(
          ns("ref_based_rc"),
          label = "Reverse-complement assembly to match reference orientation",
          value = isTRUE(as.logical(current$ref_based_rc %||% 0L)),
          status = "primary"
        ) |> shinyjs::disabled(),
        opts_help("Reverse-complement each contig when it aligns better to the top ",
                  "BLAST reference reversed. Off by default; the rRNA and start-gene ",
                  "heuristics usually orient correctly. Enable for taxa they cannot ",
                  "resolve, e.g. scyphozoan jellyfish with rRNAs on opposite strands."),
        shinyWidgets::prettyCheckbox(
          ns("retain_low_conf_trna"),
          label = "Retain low-confidence (NNN anticodon) tRNAs",
          value = isTRUE(as.logical(current$retain_low_conf_trna %||% 0L)),
          status = "primary"
        ) |> shinyjs::disabled(),
        opts_help("Keep predicted tRNAs whose anticodon could not be confidently ",
                  "determined (reported with an NNN anticodon)."),
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
        opts_help("Reusable named set of options applied to the selected samples; ",
                  "check Edit to change values or type a new name to create a set."),
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
          )
        ),
        div(
          style = "display: flex; flex-flow: row nowrap; align-items: center; gap: 2em;",
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
        opts_help("Maximum number of reference BLAST hits per gene used when ",
                  "validating and curating annotations."),
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
              choices = c("Metazoa_RefSeq235", "Metazoa_RefSeq235_custom", "Metazoa_RefSeq231", "Metazoa_RefSeq231_custom", "Metazoa_RefSeq89", "Chordata", "Chordata_custom"),
              selected = current$ref_db %||% character(0),
              width = "100%",
              options = list(
                create = TRUE,
                maxItems = 1
              )
            ) |> shinyjs::disabled()
          )
        ),
        opts_help("Location (ref_dir) and name (ref_db) of the reference database ",
                  "used to validate gene boundaries during curation. rRNA reference ",
                  "data is only included in Metazoa_RefSeq235 (and custom databases ",
                  "built from it); with other databases the annotation editor's rRNA ",
                  "alignment falls back to the per-sample BLAST reference genome.",
                  href = "https://smithsonian.github.io/MitoPilot/articles/custom_dbs.html"),
        div(
          style = "flex: 1",
          selectizeInput(
            ns("target"),
            label = "Target:",
            # Values = full keys (dispatch); labels = "Scientific (common)".
            # Ordered alphabetically by the displayed label.
            choices = local({
              tg <- names(RULESET_MAP)
              labs <- vapply(tg, function(k)
                paste0(RULESET_MAP[[k]]$ncbi, " (", RULESET_MAP[[k]]$label, ")"),
                character(1))
              ord <- order(labs)
              stats::setNames(tg[ord], labs[ord])
            }),
            selected = current$target %||% character(0),
            width = "100%",
            options = list(
              create = FALSE,
              maxItems = 1
            )
          ) |> shinyjs::disabled()
        ),
        opts_help("Taxonomic ruleset of expected genes and naming conventions used ",
                  "to curate annotations; pick the clade closest to your samples.",
                  href = "https://smithsonian.github.io/MitoPilot/articles/Ruleset-Browser.html"),
        div(
          style = "flex: 1",
          selectizeInput(
            ns("genetic_code"),
            label = "Genetic code:",
            # "auto" = auto-from-ruleset; explicit values override the ruleset default.
            choices = gcode_choices(current$target %||% "fish_mito"),
            selected = if (is.null(current$genetic_code) || is.na(current$genetic_code)) {
              "auto"
            } else {
              as.character(current$genetic_code)
            },
            width = "100%",
            options = list(maxItems = 1)
          ) |> shinyjs::disabled()
        ),
        opts_help("NCBI translation table used for protein-coding genes. 'Auto' sets the genetic code based on selected ",
                  "ruleset. Or pick a specific table to override it for these samples.",
                  href = "https://www.ncbi.nlm.nih.gov/Taxonomy/Utils/wprintgc.cgi"),
        div(
          class = "form-group shiny-input-container",
          shinyWidgets::prettyCheckbox(
            ns("linear_complete"),
            label = "Complete mitogenomes are linear (label linear assemblies as 'complete genome' on export)",
            value = isTRUE(as.integer(current$linear_complete %||% 0L) == 1L),
            status = "primary"
          ) |> shinyjs::disabled()
        ),
        div(
          style = "margin-bottom: 14px; height: 300px; overflow: auto;",
          listviewer::reactjsonOutput(ns("params"))
        ),
        opts_help("Per-gene curation rules (expected length, start/stop codons, etc.) ",
                  "for the selected target.",
                  href = "https://smithsonian.github.io/MitoPilot/articles/Fish-Mitogenome-Curation.html"),
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
      opts_help("Also report ORFs that fall inside or overlap another ORF, rather ",
                "than only the longest one per region."),
      textInput(
        ns("orffinder_opts"),
        label = tagList("ORFfinder options:", tool_help_icon("orffinder")),
        value = current$orffinder_opts %||% character(0),
        width = "100%"
      ) |> shinyjs::disabled(),
      opts_help("Extra command-line flags passed to NCBI ORFfinder.",
                href = "https://www.ncbi.nlm.nih.gov/orffinder/"),
      helpText(
        "The genetic code (-g) and minimum length (-ml) are set automatically",
        "from the sample's curation ruleset (genetic code) and the Min ORF length",
        "above; do not set them here."
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
        opts_help("Reusable named set of options applied to the selected samples; ",
                  "check Edit to change values or type a new name to create a set."),
        shinyWidgets::prettyCheckbox(
          ns("use_orffinder"),
          label = "Run ORF finder step (after curation; finds ORFs in unannotated regions)",
          value = isTRUE(as.logical(current$use_orffinder %||% 0L)),
          status = "primary"
        ) |> shinyjs::disabled(),
        opts_help("Optional step that scans unannotated regions for open ",
                  "reading frames using NCBI ORFfinder.",
                  href = "https://www.ncbi.nlm.nih.gov/orffinder/"),
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
