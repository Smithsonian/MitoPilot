# Canonical mitochondrial gene names, shared across the app (annotation options
# start-gene selector, ORF gene-assignment selector, etc.). Grouped by feature
# type so callers can both offer the names and infer the feature type.

#' Standard mitochondrial protein-coding genes
#' @noRd
MITO_PCG_GENES <- c(
  "nad1", "nad2", "nad3", "nad4", "nad4l", "nad5", "nad6",
  "cob", "cox1", "cox2", "cox3", "atp6", "atp8"
)

#' Standard mitochondrial rRNA genes
#' @noRd
MITO_RRNA_GENES <- c("rrnL", "rrnS")

#' Standard mitochondrial tRNA genes
#'
#' `trnX` is an "undetermined tRNA" placeholder used by some invertebrate
#' rulesets (e.g. bivalves) for tRNAs whose identity cannot be resolved.
#' @noRd
MITO_TRNA_GENES <- c(
  "trnA", "trnC", "trnD", "trnE", "trnF", "trnG", "trnH", "trnI", "trnK",
  "trnL", "trnM", "trnN", "trnP", "trnQ", "trnR", "trnS", "trnT", "trnV",
  "trnW", "trnY", "trnX"
)

#' Additional protein-coding genes used by some clade rulesets beyond the
#' standard 13 (extra mitochondrial ORFs / accessory genes found in
#' invertebrate and sponge mitogenomes).
#'
#' Note: `dpo` covers the medusozoan linear-mtDNA terminal DNA polymerase B ORF.
#' Its GenBank synonyms `polB` and `dnaB` (the "replication helicase" label is a
#' misannotation) are normalized to `dpo`, so they are not listed separately.
#' @noRd
MITO_EXTRA_PCG_GENES <- c(
  "atp9", "mttb", "msh1", "dpo", "lagli", "rvt", "im"
)

#' Flat list of canonical mitochondrial gene names (rRNA, PCG, tRNA), ordered for
#' use as selectInput choices.
#' @noRd
MITO_GENE_CHOICES <- c(MITO_RRNA_GENES, MITO_PCG_GENES, MITO_EXTRA_PCG_GENES, MITO_TRNA_GENES)

#' Minimum BLAST similarity (%) for an auto-suggested ORF gene assignment.
#' @noRd
ORF_ASSIGN_SIM_THRESHOLD <- 60

#' Infer the MitoPilot feature `type` for a gene name.
#'
#' Returns "rRNA"/"tRNA" for recognized rRNA/tRNA genes, otherwise "PCG"
#' (ORFs assigned a custom or protein-coding gene name are coding features).
#'
#' @param gene gene name
#' @noRd
mito_gene_type <- function(gene) {
  if (gene %in% MITO_RRNA_GENES) {
    "rRNA"
  } else if (gene %in% MITO_TRNA_GENES) {
    "tRNA"
  } else {
    "PCG"
  }
}

#' Identify non-standard MitoFinder genes.
#'
#' A non-standard gene is a MitoFinder-called PCG whose name is not one of the
#' canonical mitochondrial gene names. These are kept (not dropped) and edited
#' like ORFs: BLASTed against the combined gene DB and renameable. Vectorized.
#'
#' @param gene,type,tool annotation columns
#' @noRd
is_nonstandard_mito_gene <- function(gene, type, tool) {
  !is.na(gene) & nzchar(gene) &
    (type %in% "PCG") & (tool %in% "MitoFinder") &
    !(gene %in% MITO_GENE_CHOICES)
}

# ---------------------------------------------------------------------------
# Shared UI vocabularies (T06 state registry, T10 column names, T16/T24 colors)
# ---------------------------------------------------------------------------
# Sourced last: R/ collates alphabetically and DESCRIPTION has no Collate:, so
# constants.R loads AFTER every app_*.R. Read these inside a *_ui()/*_server()
# body only; never derive a top-level constant from them in an app_*.R file.

#' Pipeline state registry.
#'
#' Codes are the stored `assemble_switch` / `annotate_switch` integers and never
#' change. Code 1 means "queued, the next Update will process it" in every
#' module. Order is lifecycle order, not numeric order.
#' @noRd
MP_STATE_META <- list(
  `0` = list(label = "On hold",      icon = "fa fa-hourglass",
             tip = "Skipped by the next Update until you set it to Ready to run."),
  `1` = list(label = "Ready to run", icon = "fa fa-list-check",
             tip = "Queued: the next Update will process it."),
  `4` = list(label = "In progress",  icon = "fa fa-circle-half-stroke",
             tip = "Being processed by the running pipeline."),
  `2` = list(label = "Success",      icon = "fa fa-circle-check",
             tip = "Finished without errors."),
  `3` = list(label = "Failed",       icon = "fa fa-triangle-exclamation",
             tip = "Stopped with an error - see Notes.")
)

#' Codes each module can display. Only WF1 ever writes 4.
#' @noRd
MP_STATE_CODES <- list(
  assemble = c("0", "1", "4", "2", "3"),
  annotate = c("0", "1", "2", "3")
)

#' Codes a user may set by hand. 4 is machine-set only.
#' @noRd
MP_STATE_SETTABLE <- c("0", "1", "2", "3")

#' Named code vector for a state picker: names are labels, values are codes.
#' @param module "assemble" or "annotate"
#' @noRd
mp_state_choices <- function(module) {
  mp_state_field(module, "label", named = TRUE)
}

#' Icon classes for a state column, in module code order.
#' @param module "assemble" or "annotate"
#' @noRd
mp_state_icons <- function(module) mp_state_field(module, "icon")

#' State labels, in module code order.
#' @param module "assemble" or "annotate"
#' @noRd
mp_state_labels <- function(module) mp_state_field(module, "label")

#' State tooltips, in module code order.
#' @param module "assemble" or "annotate"
#' @noRd
mp_state_tips <- function(module) mp_state_field(module, "tip")

#' Pull one field of MP_STATE_META for a module, keyed by code.
#' @param module,field,named see callers
#' @noRd
mp_state_field <- function(module, field, named = FALSE) {
  codes <- MP_STATE_CODES[[match.arg(module, names(MP_STATE_CODES))]]
  out <- vapply(codes, function(k) MP_STATE_META[[k]][[field]], character(1))
  if (!named) return(out)
  names(codes) <- out
  codes
}

#' Hex colors that cannot live in CSS because SweetAlert takes them as values.
#' Everything else is a CSS token in inst/app/www/custom.css.
#' @noRd
# Mirrors --mp-primary / --mp-danger / --mp-text-muted-ish grey in custom.css.
MP_COLORS <- c(primary = "#337ab7", danger = "#b02a37", grey = "#6c757d")

#' Canonical table column headers, keyed by data-frame column name.
#'
#' Three keys are semantic, not literal, because one column name means
#' different things in different tables: use "length" for the Assemble table,
#' "length_raw" for the userAsmb `length` column and Annotate `length_raw`, and
#' "length_trimmed" for the Annotate `length` column.
#' @noRd
MP_COL_NAMES <- c(
  assemble_lock       = "Lock",
  annotate_lock       = "Lock",
  assemble_switch     = "State",
  annotate_switch     = "State",
  ID                  = "ID",
  Taxon               = "Taxon",
  path                = "Path #",
  scaffold            = "Scaffold #",
  seqid               = "SeqID",
  assembly            = "Input Assembly File",
  pre_opts            = "Preprocess Opts.",
  assemble_opts       = "Assembly Opts.",
  blast_opts          = "BLAST Opts.",
  find_mito_opts      = "Find Mito Opts.",
  circularize_opts    = "Circularize Opts.",
  annotate_opts       = "Annotate Opts.",
  curate_opts         = "Curate Opts.",
  orf_opts            = "ORF Opts.",
  trimmed_reads       = "Reads",
  mean_length         = "Read Length",
  topology            = "Topology",
  length              = "Assembly Length (bp)",
  length_raw          = "Raw Length (bp)",
  length_trimmed      = "Trimmed Length (bp)",
  ambiguous_bases     = "Ambiguous Bases",
  paths               = "# Paths",
  scaffolds           = "# Scaffolds",
  blast_accession     = "Top BLAST Hit",
  blast_ref_status    = "BLAST Ref Align",
  blast_species       = "BLAST Species",
  blast_lineage       = "BLAST Lineage",
  blast_pident        = "BLAST % Identity",
  blast_qcovs         = "BLAST % Coverage",
  blast_hits          = "All BLAST Hits",
  genetic_code        = "Genetic Code",
  structure           = "Gene Order",
  PCGCount            = "# PCGs",
  tRNACount           = "# tRNAs",
  rRNACount           = "# rRNAs",
  ORFCount            = "# ORFs",
  missing             = "Missing",
  extra               = "Extra",
  warnings            = "Warnings",
  ID_verified         = "Species ID Verified",
  reviewed            = "Reviewed",
  problematic         = "Problematic",
  partial             = "Partial",
  export_group        = "Export Group",
  export_time_stamp   = "Exported",
  time_stamp          = "Last Updated",
  assemble_notes      = "Notes",
  annotate_notes      = "Notes",
  join_notes          = "Scaffold Join Notes",
  find_mito_notes     = "Mito Search",
  circularize_notes   = "Circularization",
  view                = "Details",
  output              = "Output",
  view_coverage       = "Coverage",
  resolved            = "Resolved",
  edit                = "Edit"
)

#' Header tooltips for the columns whose name does not explain itself.
#' Same keys as MP_COL_NAMES. Sentence case, no terminal period.
#' @noRd
MP_COL_TIPS <- c(
  assemble_switch     = "Pipeline state; hover a row icon for its meaning",
  annotate_switch     = "Pipeline state; hover a row icon for its meaning",
  path                = "Assembly path number for this unit; 0 is a joined consensus assembly",
  scaffold            = "Scaffold number within the path; 0 is a joined consensus assembly",
  seqid               = "Sequence identifier written to the exported FASTA",
  assembly            = "Assembly file you supplied for this sample",
  pre_opts            = "Named set of read preprocessing parameters",
  assemble_opts       = "Named set of assembly parameters",
  blast_opts          = "Named set of BLAST search parameters",
  find_mito_opts      = "Named set of mitogenome search parameters",
  circularize_opts    = "Named set of circularization parameters",
  annotate_opts       = "Named set of annotation parameters",
  curate_opts         = "Named set of curation parameters",
  orf_opts            = "Named set of ORF finder parameters",
  trimmed_reads       = "Read pairs left after preprocessing",
  mean_length         = "Mean read length after preprocessing, in bp",
  topology            = "Circular or linear, as reported by the assembler",
  length              = "Total assembly length in bp; scaffolds marked ignored are excluded",
  length_raw          = "Assembly length in bp before trimming",
  length_trimmed      = "Assembly length in bp after trimming unannotated ends",
  ambiguous_bases     = "Number of non-ACGT bases in the assembly",
  paths               = "Number of alternative assembly paths for this sample",
  scaffolds           = "Number of scaffolds in this assembly",
  blast_accession     = "GenBank accession of the best BLAST hit, used as the reference",
  blast_ref_status    = "How well this assembly aligns to its BLAST reference",
  blast_species       = "Species of the top BLAST hit",
  blast_lineage       = "NCBI lineage of the top BLAST hit",
  blast_pident        = "Percent identity to the top BLAST hit",
  blast_qcovs         = "Percent of the assembly covered by the top BLAST hit",
  genetic_code        = "NCBI translation table used for protein-coding genes",
  structure           = "Order and strand of the annotated genes",
  missing             = "Expected genes that were not found",
  extra               = "Genes found more often than expected",
  warnings            = "Curation warnings stored for this assembly",
  ID_verified         = "Whether a curator has confirmed the species identification",
  reviewed            = "Whether a curator has reviewed this assembly",
  problematic         = "Flagged by a curator as needing attention",
  partial             = "Flagged as an incomplete mitogenome",
  export_group        = "Named group this assembly is exported with",
  export_time_stamp   = "When this assembly was last exported",
  time_stamp          = "When this row was last changed",
  join_notes          = "What the scaffold join step reported",
  find_mito_notes     = "What the mitogenome search step reported",
  circularize_notes   = "What the circularization step reported"
)

# Lock vocabulary shared by the toolbar tooltip, the Lock column header, the
# details-window banner and the Lock & Close footer note (theme T02).
MP_UNIT <- c(assemble = "sample", annotate = "assembly")
MP_UNITS <- c(assemble = "samples", annotate = "assemblies")
MP_NEXT <- c(assemble = "Annotate", annotate = "Export")
MP_LOCK_DEF <- function(mod = c("assemble", "annotate")) {
  mod <- match.arg(mod)
  sprintf(
    paste(
      "Locked %s are finished with this step: their options cannot be changed,",
      "the next update will not re-run them, and they move on to %s.",
      "You can unlock at any time."
    ),
    MP_UNITS[[mod]], MP_NEXT[[mod]]
  )
}
