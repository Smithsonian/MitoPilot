# Pre-compute a whole-genome pairwise alignment of a sample against its BLAST reference and write the result for ingestion into the `blast_ref_alignment` SQLite table.

Called from the `blast_ref_align` Nextflow process after annotation is
complete. Both sequences are passed as plain nucleotide strings (no
headers, no line-breaks) so that this function can run entirely from
file-based inputs without a database connection.

## Usage

``` r
compute_blast_ref_alignment(assembly_seq, ref_seq, rotation = 0L, output_file)
```

## Arguments

- assembly_seq:

  nucleotide sequence string for the sample assembly

- ref_seq:

  nucleotide sequence string for the BLAST reference genome

- rotation:

  integer offset (0-based) used to rotate the reference so that it
  starts at the same anchor gene as the sample assembly. Equals
  `pos1 - 1` of the anchor gene in the reference's
  `blast_ref_annotations` record.

- output_file:

  path to write a one-row CSV with columns `aligned_sample`,
  `aligned_ref`, `rotation`, `ref_length`
