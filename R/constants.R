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
#' @noRd
MITO_TRNA_GENES <- c(
  "trnA", "trnC", "trnD", "trnE", "trnF", "trnG", "trnH", "trnI", "trnK",
  "trnL", "trnM", "trnN", "trnP", "trnQ", "trnR", "trnS", "trnT", "trnV",
  "trnW", "trnY"
)

#' Flat list of canonical mitochondrial gene names (rRNA, PCG, tRNA), ordered for
#' use as selectInput choices.
#' @noRd
MITO_GENE_CHOICES <- c(MITO_RRNA_GENES, MITO_PCG_GENES, MITO_TRNA_GENES)

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
