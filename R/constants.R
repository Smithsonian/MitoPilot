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
#' @noRd
MITO_EXTRA_PCG_GENES <- c(
  "atp9", "mttb", "msh1", "dpo", "lagli", "rvt", "dnaB", "im"
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
