# Choose which mapping-file columns are compared with GEOME and GBIF

MitoPilot compares specimen details (coordinates, collection date,
country, locality, voucher, collector, sex, and life stage) between your
mapping file, GEOME, and GBIF, and flags disagreements. It finds the
mapping-file columns by name; use this function when a column has a name
it does not recognize, or to stop comparing one. The Taxon column is
always compared.

## Usage

``` r
set_metadata_columns(path = ".", ...)
```

## Arguments

- path:

  Path to the project directory (default = current working directory)

- ...:

  Named \`concept = column\` pairs. Concepts: \`coordinates\`,
  \`collection_date\`, \`country\`, \`locality\`, \`voucher\`,
  \`collector\`, \`sex\`, \`dev_stage\`. \`coordinates\` takes one
  combined column or two columns (latitude, then longitude). \`NA\`
  returns a concept to automatic detection; \`""\` stops comparing it.

## Value

Invisibly, a data frame of each concept and the column(s) now used
(\`NA\` when none).
