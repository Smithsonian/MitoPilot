# Generate thin curate_<clade>_mito / validate_<clade>_mito wrappers around the
# shared curate_mito_core() / validate_mito_core(). Reproducible: rerun to
# regenerate all clade wrapper files. Run from the package root.
#
#   Rscript data-raw/refactor_clade_wrappers.R

clades <- tibble::tribble(
  ~key,               ~gc, ~flip,  ~label,
  # ---- existing (rewritten as wrappers) ----
  "fish",             2,   TRUE,   "fish",
  "bird",             2,   TRUE,   "bird",
  "turtle",           2,   TRUE,   "turtle",
  "mammal",           2,   TRUE,   "mammal",
  "lepidosaur",       2,   TRUE,   "lepidosaur",
  "starfish",         9,   TRUE,   "sea star",
  "diptera",          5,   FALSE,  "true fly",
  "copepod",          5,   TRUE,   "copepod",
  "octocoral",        4,   TRUE,   "octocoral",
  "hexacoral",        4,   TRUE,   "hexacoral",
  "ctenophore",       4,   TRUE,   "ctenophore",
  "annelid",          5,   TRUE,   "annelid",
  # ---- new ----
  "ascidiacea",       13,  TRUE,   "ascidian",
  "bivalvia",         5,   TRUE,   "bivalve",
  "bryozoa",          5,   TRUE,   "bryozoan",
  "crinoidea",        9,   TRUE,   "crinoid",
  "demospongiae",     4,   TRUE,   "demosponge",
  "echinoidea",       9,   TRUE,   "sea urchin",
  "gastropoda",       5,   TRUE,   "gastropod",
  "holothuroidea",    9,   TRUE,   "sea cucumber",
  "homoscleromorpha", 4,   TRUE,   "homoscleromorph sponge",
  "malacostraca",     5,   TRUE,   "malacostracan",
  "hydrozoa",         4,   TRUE,   "hydrozoan",
  "nemertea",         5,   TRUE,   "nemertean",
  "ophiuroidea",      9,   TRUE,   "brittle star",
  "platyhelminthes",  9,   TRUE,   "flatworm",
  "polychaeta",       5,   TRUE,   "polychaete",
  "pycnogonida",      5,   TRUE,   "sea spider",
  "sipuncula",        5,   TRUE,   "peanut worm",
  "thaliacea",        13,  TRUE,   "thaliacean",
  "thecostraca",      5,   TRUE,   "barnacle"
)

curate_tmpl <- function(key, gc, flip, label) {
  sprintf('#\' Annotation curation for %s mitogenomes
#\'
#\' Thin wrapper around the shared curation core with the %s genetic code.
#\' See `curate_mito_core()` for the full argument list and behavior.
#\'
#\' @param annotations_fn Path to the annotations file (csv)
#\' @param assembly_fn Path to the assembly file (fasta)
#\' @param coverage_fn Path to the coverage file (csv)
#\' @param genetic_code Genetic code to use (default = %d)
#\' @param out_dir Path to the output directory
#\' @param max_blast_hits Maximum number of top BLAST hits to retain (default = 10)
#\' @param params Nested list of curation parameters. Can also provided as a
#\'   base64 encoded json string.
#\' @param ref_dir Path to reference directory for curation
#\' @param blast_ref_file Path to a JSON file of remote BLAST reference hits to inject into the local curation database (default = NULL)
#\' @param feature_trim Trim feature coordinates to assembly boundaries (default = TRUE)
#\'
#\' @export
#\'
curate_%s_mito <- function(
    annotations_fn = NULL,
    assembly_fn = NULL,
    coverage_fn = NULL,
    genetic_code = %d,
    out_dir = NULL,
    max_blast_hits = 10,
    params = NULL,
    ref_dir = NULL,
    blast_ref_file = NULL,
    feature_trim = TRUE) {
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
    flip_rRNA_minus_strand = %s
  )
}
', label, label, gc, key, gc, toupper(as.character(flip)))
}

validate_tmpl <- function(key, label) {
  sprintf('#\' Annotation validation for %s mitogenomes
#\'
#\' Thin wrapper around the shared validation core. See `validate_mito_core()`
#\' for details.
#\'
#\' @param annotations_fn path to annotations file (csv)
#\' @param coverage_fn path to coverage file (csv)
#\' @param params nested list of curation/validation parameters. Can also be
#\'   provided as a base64 encoded JSON string.
#\' @param out_dir output directory
#\'
#\' @export
#\'
validate_%s_mito <- function(
    annotations_fn = NULL,
    coverage_fn = NULL,
    params = list(),
    out_dir = NULL) {
  validate_mito_core(
    annotations_fn = annotations_fn,
    coverage_fn = coverage_fn,
    params = params,
    out_dir = out_dir
  )
}
', label, key)
}

for (i in seq_len(nrow(clades))) {
  r <- clades[i, ]
  writeLines(curate_tmpl(r$key, r$gc, r$flip, r$label),
             file.path("R", sprintf("curate_%s_mito.R", r$key)))
  writeLines(validate_tmpl(r$key, r$label),
             file.path("R", sprintf("validate_%s_mito.R", r$key)))
}

# Remove stale mis-named file (function was curate_octocoral_mito in curate_octacoral_mito.R)
if (file.exists("R/curate_octacoral_mito.R")) file.remove("R/curate_octacoral_mito.R")

cat(sprintf("Wrote %d curate + %d validate wrappers.\n", nrow(clades), nrow(clades)))
