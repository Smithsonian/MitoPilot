# Update project database metadata.

Update the metadata for an existing project database. Cannot update ID,
R1, or R2, but can add new metadata columns. Creates a backup of the
existing database prior to updating.

## Usage

``` r
update_sample_metadata(
  path = ".",
  update_mapping_fn = NULL,
  mapping_id = "ID",
  mapping_taxon = "Taxon"
)
```

## Arguments

- path:

  Path to the project directory (default = current working directory)

- update_mapping_fn:

  Path to the update mapping CSV file. Must contain columns "ID" and
  "Taxon"

- mapping_id:

  Column name of the update mapping file to use as the primary key

- mapping_taxon:

  Column name of the update mapping file containing a Taxonomic
  identifier (eg, species name)
