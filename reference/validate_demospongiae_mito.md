# Annotation validation for demosponge mitogenomes

Thin wrapper around the shared validation core. See
\`validate_mito_core()\` for details.

## Usage

``` r
validate_demospongiae_mito(
  annotations_fn = NULL,
  coverage_fn = NULL,
  params = list(),
  out_dir = NULL
)
```

## Arguments

- annotations_fn:

  path to annotations file (csv)

- coverage_fn:

  path to coverage file (csv)

- params:

  nested list of curation/validation parameters. Can also be provided as
  a base64 encoded JSON string.

- out_dir:

  output directory
