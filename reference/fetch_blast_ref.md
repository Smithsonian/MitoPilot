# Fetch and parse NCBI GFF3 annotations and FASTA sequence for a BLAST top hit

Downloads the GFF3 record and, optionally, the FASTA sequence for the
given accession from NCBI EFetch. The GFF3-derived annotations are
written to `output_file` as a CSV suitable for ingestion into the
`blast_ref_annotations` SQLite table. When `sequence_file` is supplied
the raw nucleotide sequence (no FASTA header, no line-breaks) is written
there for ingestion into the `blast_ref_sequences` table.

## Usage

``` r
fetch_blast_ref(
  accession,
  output_file,
  sequence_file = NULL,
  genetic_code_file = NULL,
  json_file = NULL,
  blast_species = NULL,
  blast_evalue = NULL
)
```

## Arguments

- accession:

  NCBI accession number (e.g. "NC_012345.1")

- output_file:

  path to write the annotations CSV

- sequence_file:

  optional path to write the plain nucleotide sequence

- genetic_code_file:

  optional path to write the NCBI translation table number

- json_file:

  optional path to write a JSON bundle used by curation

- blast_species:

  optional BLAST hit species label to include in JSON

- blast_evalue:

  optional BLAST hit e-value to include in JSON
