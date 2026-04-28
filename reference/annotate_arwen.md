# Use ARWEN to annotate tRNAs in a mitochondrial genome assembly

Use ARWEN to annotate tRNAs in a mitochondrial genome assembly

## Usage

``` r
annotate_arwen(
  assembly = NULL,
  arwen_opts = "-mtx",
  genetic_code = "2",
  circular = TRUE,
  out = NULL
)
```

## Arguments

- assembly:

  DNAStringSet assembly to annotate

- arwen_opts:

  command line options for ARWEN (default = "-mtx")

- genetic_code:

  NCBI translation table number passed to ARWEN via -gc\<N\> (default =
  "2")

- circular:

  logical; TRUE for circular topology (-c), FALSE for linear (-l)

- out:

  output file path
