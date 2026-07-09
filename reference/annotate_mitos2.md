# Annotate mitochondrial genomes using MITOS2

Annotate mitochondrial genomes using MITOS2

## Usage

``` r
annotate_mitos2(
  assembly = NULL,
  topology = "circular",
  genetic_code = "2",
  ref_db = "Chordata",
  mitos_opts = "--best --intron 0 --oril 0",
  out = NULL,
  condaenv = "mitos",
  rescue_no_trna = TRUE
)
```

## Arguments

- assembly:

  a DNAString object

- topology:

  "circular" or "linear"

- genetic_code:

  NCBI genetic code number (default: 2)

- ref_db:

  Mitos2 reference database (default: "Chordata")

- mitos_opts:

  Additional command line options for MITOS2

- out:

  output directory

- condaenv:

  Conda environment to run MITOS2 (default: "mitos")

- rescue_no_trna:

  Run MITOS2 a second time with tRNA prediction disabled (\`–trna 0\`)
  and add any PCGs/rRNAs it uniquely recovers to the full-run
  annotations (default: TRUE). MITOS2 discards rRNAs/PCGs whose locus
  overlaps a predicted tRNA; the tRNA-free run recovers them (e.g.
  scyphozoan rrnS).
