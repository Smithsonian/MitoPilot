# Annotation curation for octocoral mitogenomes

Annotation curation for octocoral mitogenomes

## Usage

``` r
curate_octocoral_mito(
  annotations_fn = NULL,
  assembly_fn = NULL,
  coverage_fn = NULL,
  genetic_code = 4,
  out_dir = NULL,
  max_blast_hits = 100,
  params = NULL,
  ref_dir = NULL,
  blast_ref_file = NULL
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

  Genetic code to use (default = 4)

- out_dir:

  Path to the output directory

- max_blast_hits:

  Maximum number of top BLAST hits to retain (default = 100)

- params:

  Nested list of curation parameters. Can also provided as a base64
  encoded json string.

- ref_dir:

  Path to reference directory for curation

- blast_ref_file:

  Path to a JSON file of remote BLAST reference hits to inject into the
  local curation database (default = NULL)
