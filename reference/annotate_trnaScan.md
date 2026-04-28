# Use tRNAscan-SE to annotate tRNAs in a mitochondrial genome assembly

Use tRNAscan-SE to annotate tRNAs in a mitochondrial genome assembly

## Usage

``` r
annotate_trnaScan(
  assembly = NULL,
  rotate = TRUE,
  trnaScan_opts = "-M vert -X 20",
  cpus = 4,
  out = NULL,
  condaenv = "base"
)
```

## Arguments

- assembly:

  text string of the assembly to annotate

- rotate:

  should the assembly be rotated? default = TRUE

- trnaScan_opts:

  command line options for tRNAscan-SE (defatult = \`-M vert -X 20\`)

- cpus:

  number of cpus to use (default = 4)

- out:

  output file name

- condaenv:

  conda environment to use (default = "base")
