# Add samples to project database

Add new samples to an existing project database. All samples will
inherit the default processing options. Creates a backup of the existing
database prior to updating.

## Usage

``` r
add_samples(
  path = ".",
  update_mapping_fn = NULL,
  mapping_id = "ID",
  mapping_taxon = "Taxon",
  mapping_geome = "GEOME_BCID",
  fetch_geome = TRUE,
  mapping_gbif = "GBIF_ID",
  fetch_gbif = TRUE
)
```

## Arguments

- path:

  Path to the project directory (default = current working directory)

- update_mapping_fn:

  Path to the update mapping CSV file. Must contain columns "ID",
  "Taxon, "R1", and "R2". May include additional columns with other
  sample metadata, and an optional `Reference` column naming a
  per-sample MapToRef reference (file path, URL, or NCBI accession). A
  FASTA reference also needs a `Reference_topology` column (circular or
  linear). Both values are stored on the sample and used when its
  parameter set assembles with MapToRef. `Reference` is a reserved
  column name: it is never stored as sample metadata, so rename the
  column if you use it for something else.

- mapping_id:

  Column name of the update mapping file to use as the primary key

- mapping_taxon:

  Column name of the update mapping file containing a Taxonomic
  identifier (eg, species name)

- mapping_geome:

  Name of the mapping-file column holding GEOME BCIDs

- fetch_geome:

  Fetch GEOME metadata for samples with a BCID during setup (default
  TRUE). Set FALSE when offline and run \[fetch_geome()\] later.

- mapping_gbif:

  Name of the mapping-file column holding GBIF occurrence IDs

- fetch_gbif:

  Fetch GBIF metadata for samples with a GBIF ID (default TRUE). Set
  FALSE when offline and run \[fetch_gbif()\] later.
