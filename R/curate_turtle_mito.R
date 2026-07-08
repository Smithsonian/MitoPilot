#' Annotation curation for turtle mitogenomes
#'
#' Thin wrapper around the shared curation core with the turtle genetic code.
#' See `curate_mito_core()` for the full argument list and behavior.
#'
#' @param annotations_fn Path to the annotations file (csv)
#' @param assembly_fn Path to the assembly file (fasta)
#' @param coverage_fn Path to the coverage file (csv)
#' @param genetic_code Genetic code to use (default = 2)
#' @param out_dir Path to the output directory
#' @param max_blast_hits Maximum number of top BLAST hits to retain (default = 10)
#' @param params Nested list of curation parameters. Can also provided as a
#'   base64 encoded json string.
#' @param ref_dir Path to reference directory for curation
#' @param blast_ref_file Path to a JSON file of remote BLAST reference hits to inject into the local curation database (default = NULL)
#' @param feature_trim Trim feature coordinates to assembly boundaries (default = TRUE)
#' @param ref_based_rc Reverse-complement contigs to match the top BLAST reference strand (default = FALSE)
#' @param blast_accession Accession of the top BLAST hit, used as the orientation reference for ref_based_rc (default = NULL)
#'
#' @export
#'
curate_turtle_mito <- function(
    annotations_fn = NULL,
    assembly_fn = NULL,
    coverage_fn = NULL,
    genetic_code = 2,
    out_dir = NULL,
    max_blast_hits = 10,
    params = NULL,
    ref_dir = NULL,
    blast_ref_file = NULL,
    feature_trim = TRUE,
    ref_based_rc = FALSE,
    blast_accession = NULL) {
  curate_mito_core(
    annotations_fn = annotations_fn,
    assembly_fn = assembly_fn,
    coverage_fn = coverage_fn,
    genetic_code = genetic_code,
    out_dir = out_dir,
    max_blast_hits = max_blast_hits,
    params = params,
    ref_dir = ref_dir,
    blast_ref_file = blast_ref_file,
    feature_trim = feature_trim,
    flip_rRNA_minus_strand = TRUE,
    ref_based_rc = ref_based_rc,
    blast_accession = blast_accession
  )
}

