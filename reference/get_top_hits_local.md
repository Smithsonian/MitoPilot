# Get top BLASTP hits

If blastp is not available on the path loaded in system(), set
options("MitoPilot.blastp" = "/path/to/blastp/executable")

## Usage

``` r
get_top_hits_local(ref_db = NULL, query = NULL, max_blast_hits = 10)
```

## Arguments

- ref_db:

  reference database

- query:

  query sequences

- max_blast_hits:

  Maximum number of top BLAST hits to retain (default = 10)
