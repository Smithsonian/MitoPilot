# Use ARAGORN to annotate tRNAs in a mitochondrial genome assembly

Use ARAGORN to annotate tRNAs in a mitochondrial genome assembly

## Usage

``` r
annotate_aragorn(
  assembly = NULL,
  aragorn_opts = "-m -gcstd",
  genetic_code = "2",
  circular = TRUE,
  condaenv = "aragorn"
)
```

## Arguments

- assembly:

  DNAStringSet assembly to annotate

- aragorn_opts:

  command line options for ARAGORN (default = "-m -gcstd")

- genetic_code:

  NCBI translation table number passed to ARAGORN via -gc\<N\> (default
  = "2")

- circular:

  logical; TRUE for circular topology (-c), FALSE for linear (-l)

- condaenv:

  conda environment containing ARAGORN (default = "aragorn")
