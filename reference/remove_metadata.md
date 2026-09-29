# Remove fetched GEOME, GBIF, and NCBI metadata

Deletes the records MitoPilot fetched from GEOME, GBIF, or NCBI, and any
IDs it found by following links between them. Your mapping-file columns,
the IDs you supplied (in the mapping file, in the app, or with
\`fetch\_\*()\`), and your export and table field choices are kept, so
the data can be fetched again at any time.

## Usage

``` r
remove_metadata(path = ".", sources = names(META_SOURCES), ids = NULL)
```

## Arguments

- path:

  Path to the project directory (default = current working directory)

- sources:

  Sources to remove: any of \`"GEOME"\`, \`"GBIF"\`, \`"NCBI"\` (default
  all).

- ids:

  Sample IDs to remove data for. Default: every sample.

## Value

Invisibly, NULL.
