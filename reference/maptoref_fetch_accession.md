# Resolve an NCBI accession to a MapToRef reference file

Downloads the GenBank record for the accession from NCBI, so the
reference carries its own topology.

## Usage

``` r
maptoref_fetch_accession(accession, out_dir = ".", log_fn = NULL)
```

## Arguments

- accession:

  An NCBI nucleotide accession, with or without its version (for example
  NC_002333 or NC_002333.1). Case insensitive.

- out_dir:

  Directory to write into; the file is placed in `<out_dir>/maptoref/`.

- log_fn:

  Optional path to an assembler log file to append to.

## Value

A list with `file`, `source` ("ncbi"), and the uppercased `accession`
that was resolved. Stops when NCBI has no record.
