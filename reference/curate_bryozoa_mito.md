# Annotation curation for bryozoan mitogenomes

Thin wrapper around the shared curation core with the bryozoan genetic
code. See \`curate_mito_core()\` for the full argument list and
behavior.

## Usage

``` r
curate_bryozoa_mito(
  annotations_fn = NULL,
  assembly_fn = NULL,
  coverage_fn = NULL,
  genetic_code = 5,
  out_dir = NULL,
  max_blast_hits = 10,
  params = NULL,
  ref_dir = NULL,
  blast_ref_file = NULL,
  feature_trim = TRUE,
  ref_based_rc = FALSE,
  blast_accession = NULL
)
```

## Arguments

- annotations_fn:

  Path to the annotations file (csv)

- assembly_fn:

  Path to the assembly file (fasta)

- coverage_fn:

  Path to the coverage file (csv)

- genetic_code:

  Genetic code to use (default = 5)

- out_dir:

  Path to the output directory

- max_blast_hits:

  Maximum number of top BLAST hits to retain (default = 10)

- params:

  Nested list of curation parameters. Can also provided as a base64
  encoded json string.

- ref_dir:

  Path to reference directory for curation

- blast_ref_file:

  Path to a JSON file of remote BLAST reference hits to inject into the
  local curation database (default = NULL)

- feature_trim:

  Trim feature coordinates to assembly boundaries (default = TRUE)

- ref_based_rc:

  Reverse-complement contigs to match the top BLAST reference strand
  (default = FALSE)

- blast_accession:

  Accession of the top BLAST hit, used as the orientation reference for
  ref_based_rc (default = NULL)
