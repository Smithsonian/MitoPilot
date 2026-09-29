# Fetch GEOME metadata for project samples

Looks up each sample's GEOME BCID, walks up its parent records (e.g.
Tissue, Sample, Event) and adds expedition and project details, then
stores everything in the project database for viewing in the app and use
at export.

## Usage

``` r
fetch_geome(path = ".", ids = NULL, bcids = NULL, link_sources = NULL)
```

## Arguments

- path:

  Path to the project directory (default = current working directory)

- ids:

  Sample IDs to fetch. Default: every sample with a BCID.

- bcids:

  Optional BCIDs to set for \`ids\` first (same length as \`ids\`). A
  blank value removes that sample's BCID and its GEOME data.

- link_sources:

  Follow links to GEOME, GBIF, or NCBI records named in the fetched
  records, filling in IDs a sample does not have yet. NULL (default)
  uses the project setting.

## Value

Invisibly, a data frame of \`ID\`, \`status\`, and \`message\`.
