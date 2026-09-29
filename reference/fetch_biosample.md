# Fetch NCBI BioSample and BioProject metadata for project samples

Looks up each sample's NCBI BioSample, directly or through an SRA
accession (SRR/ERR/DRR run, SRX experiment, or SRS sample), plus the
BioProject(s) it belongs to, and stores everything in the project
database for viewing in the app and use at export. Set the environment
variable \`ENTREZ_KEY\` to an NCBI API key for faster lookups.

## Usage

``` r
fetch_biosample(path = ".", ids = NULL, biosamples = NULL, link_sources = NULL)
```

## Arguments

- path:

  Path to the project directory (default = current working directory)

- ids:

  Sample IDs to fetch. Default: every sample with a BioSample value.

- biosamples:

  Optional BioSample or SRA accessions to set for \`ids\` first (same
  length as \`ids\`). A blank value removes that sample's BioSample and
  its NCBI data.

- link_sources:

  Follow links to GEOME, GBIF, or NCBI records named in the fetched
  records, filling in IDs a sample does not have yet. NULL (default)
  uses the project setting.

## Value

Invisibly, a data frame of \`ID\`, \`status\`, and \`message\`.
