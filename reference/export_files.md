# Generate export NCBI files

Generate export NCBI files

## Usage

``` r
export_files(
  group = NULL,
  IDs = NULL,
  fasta_header =
    paste("{ID} [organism={Taxon}] [topology={topology}] [mgcode={genetic_code}]",
    "[location=mitochondrion] {Taxon} mitochondrion, complete genome"),
  fasta_header_gene = paste("{ID} [organism={Taxon}] [mgcode={genetic_code}]",
    "[location=mitochondrion] {Taxon}"),
  out_dir = NULL,
  generateAAalignments = T,
  gene_export = F
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
