# Read the local BLAST database provenance published by the pipeline

The database lives inside the container, so the app cannot read it
directly: on HPC the container only exists on the compute nodes.
Instead, blast_genbank copies the database's VERSION file next to each
sample's hits, and this reads the most recently written copy.

## Usage

``` r
local_blast_db_info(dir_out)
```

## Arguments

- dir_out:

  Project output directory (`session$userData$dir_out`).

## Value

A named list with the parsed VERSION fields plus `built_date` (ISO date
only), or NULL when no local search has run in this project or the file
carries no usable build date.
