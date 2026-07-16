# Generate export NCBI files

Generate export NCBI files

## Usage

``` r
export_files(
  group = NULL,
  IDs = NULL,
  fasta_header =
    paste("{seqid} [organism={Taxon}] [topology={topology}] [mgcode={genetic_code}]",
    "[location=mitochondrion] {Taxon} mitochondrion, {completeness}"),
  fasta_header_gene = paste("{seqid} [organism={Taxon}] [mgcode={genetic_code}]",
    "[location=mitochondrion] {Taxon}"),
  out_dir = NULL,
  generateAAalignments = T,
  gene_export = F,
  review = TRUE,
  start_aa = 10,
  stop_aa = 10,
  ident_pct = 60,
  summary_csv = TRUE
)
```

## Arguments

- group:

  (optional) export group names

- IDs:

  One or more sample IDs to export. If not provided all samples in the
  export group will be exported

- fasta_header:

  Template for mitogenome fasta headers. Uses glue syntax (i.e. \`...\`)
  to insert values from the samples table

- fasta_header_gene:

  Template for gene fasta headers. Uses glue syntax (i.e. \`...\`) to
  insert values from the samples table

- out_dir:

  directory to save the exported files

- generateAAalignments:

  Generate group-level amino acid alignments (default: TRUE)

- gene_export:

  Export FASTAs and feature tables for individual genes? (default:
  FALSE)

- review:

  Run the PCG annotation outlier review after writing files and return
  the flagged results? (default: TRUE)

- start_aa:

  Start-offset threshold (amino acids) passed to
  \[flag_PCG_outliers()\]. Default 10.

- stop_aa:

  Stop-offset threshold (amino acids) passed to \[flag_PCG_outliers()\].
  Default 10.

- ident_pct:

  Identity threshold (percent) passed to \[flag_PCG_outliers()\].
  Default 60.

- summary_csv:

  Write a per-sample summary CSV (organism, topology, completeness, gene
  counts, reference, etc.) into the export directory? (default: TRUE)

## Value

Invisibly, the list returned by \[flag_PCG_outliers()\] when \`review\`
is TRUE (and a group of \>1 sample is exported), otherwise \`NULL\`.
