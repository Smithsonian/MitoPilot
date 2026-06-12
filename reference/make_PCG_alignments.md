# Generate HTML report woth PCG alignments

Generate HTML report woth PCG alignments

## Usage

``` r
make_PCG_alignments(
  export_group = NULL,
  db = NULL,
  out_path = NULL,
  start_aa = 10,
  stop_aa = 10,
  ident_pct = 60
)
```

## Arguments

- export_group:

  Name of the submission group

- db:

  path for sqlite database

- out_path:

  path for output files

- start_aa:

  Start-offset threshold (amino acids) for the flagged-outlier section
  of the report. Default 10.

- stop_aa:

  Stop-offset threshold (amino acids) for the flagged-outlier section of
  the report. Default 10.

- ident_pct:

  Identity threshold (percent) for the flagged-outlier section of the
  report. Default 60.
