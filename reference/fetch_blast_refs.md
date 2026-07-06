# Fetch NCBI references for many accessions using batched EFetch requests

Reduces NCBI round-trips by fetching all accessions' GFF3 and FASTA in
one request each (plus one taxonomy request per unique taxid), then
writing one \`ref\_\<accession\>/\` subdirectory per accession
(annotations CSV, sequence, genetic code, JSON) under `out_dir`. The
concatenated responses are split by accession key, so a scrambled batch
can never misassign a record. Any accession the batch cannot produce
(missing/invalid chunk, or a failed batched HTTP call) falls back to a
per-accession \[fetch_blast_ref()\] call. A fully failed accession
produces no output dir (its samples are flagged downstream), matching
the old one-accession-per-task behavior.

## Usage

``` r
fetch_blast_refs(
  accessions,
  out_dir = ".",
  blast_species = NULL,
  blast_evalue = NULL
)
```

## Arguments

- accessions:

  character vector of NCBI accessions

- out_dir:

  directory to create the per-accession \`ref\_\<accession\>/\` dirs in

- blast_species, blast_evalue:

  unused (per-sample metadata is stamped downstream); kept for signature
  parity with \[fetch_blast_ref()\].
