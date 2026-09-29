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
  mapping_taxon = "Taxon",
  mapping_geome = "GEOME_BCID",
  fetch_geome = TRUE,
  mapping_gbif = "GBIF_ID",
  fetch_gbif = TRUE,
  mapping_biosample = "BioSample",
  fetch_biosample = TRUE,
  link_sources = FALSE
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

- mapping_biosample:

  Name of the mapping-file column holding NCBI BioSample or SRA
  accessions (optional). Must not be the sample ID column. Stored as
  \`BioSample\`. See \`vignette("Specimen-Metadata")\`.

- fetch_biosample:

  Fetch NCBI metadata for samples with a BioSample value (default TRUE).
  Set FALSE when offline and run \[fetch_biosample()\] later.

- link_sources:

  Follow links between GEOME, GBIF, and NCBI records for these samples
  (default FALSE). Linking also runs when the project setting is on;
  this argument does not change the setting. See
  \`vignette("Specimen-Metadata")\`.
