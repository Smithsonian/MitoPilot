# Fetch GBIF occurrence metadata for project samples

Looks up each sample's GBIF occurrence (gbifID), plus the dataset and
publisher it belongs to, and stores everything in the project database
for viewing in the app and use at export. GBIF IDs can change when a
dataset is republished; a sample whose ID no longer resolves keeps its
previous data and shows a failed fetch.

## Usage

``` r
fetch_gbif(path = ".", ids = NULL, gbifs = NULL)
```

## Arguments

- path:

  Path to the project directory (default = current working directory)

- ids:

  Sample IDs to fetch. Default: every sample with a GBIF ID.

- gbifs:

  Optional GBIF occurrence IDs (or gbif.org occurrence links) to set for
  \`ids\` first (same length as \`ids\`). A blank value removes that
  sample's GBIF ID and its GBIF data.

## Value

Invisibly, a data frame of \`ID\`, \`status\`, and \`message\`.
