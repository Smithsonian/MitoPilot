# Annotation curation for lepidosaur mitogenomes

Annotation curation for lepidosaur mitogenomes

## Usage

``` r
curate_lepidosaur_mito(
  annotations_fn = NULL,
  assembly_fn = NULL,
  coverage_fn = NULL,
  genetic_code = 2,
  out_dir = NULL,
  max_blast_hits = 100,
  params = NULL,
  ref_dir = NULL
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

  Genetic code to use (default = 2)

- out_dir:

  Path to the output directory

- max_blast_hits:

  Maximum number of top BLAST hits to retain (default = 100)

- params:

  Nested list of curation parameters. Can also provided as a base64
  encoded json string.

- ref_dir:

  Path to reference directory for curation
