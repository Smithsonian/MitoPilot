# Re-stamp per-sample BLAST hit metadata onto a cached reference JSON

The BLAST reference JSON produced by \[fetch_blast_ref()\] is cached and
shared across all samples whose top hit is the same accession (see the
`blast_ref_fetch` Nextflow process). Every field is accession-derived
except the per-sample BLAST hit metadata, so after a sample copies the
cached JSON this rewrites `blast_species` and `blast_evalue` for that
sample. No-op if the file is missing or unreadable.

## Usage

``` r
patch_blast_ref_meta(json_file, blast_species = NULL, blast_evalue = NULL)
```

## Arguments

- json_file:

  path to the JSON file to patch in place

- blast_species:

  BLAST hit species/title for this sample

- blast_evalue:

  BLAST hit e-value for this sample
