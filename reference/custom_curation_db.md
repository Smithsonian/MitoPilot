# Generate a custom curation database

Generate a custom curation database from user-supplied table of
translated (amino acid) mitochondrial gene sequences. Requires a CSV
file containing three columns: "SeqID" = unique name to be used for
sequence, "Gene" = name of gene, and "FASTA" = name of fasta file
containing the protein sequence. Combines your sequences with Metazoa or
Chordata NCBI RefSeq data. Make sure to carefully consider what you are
adding to the custom database. You should only use high-confidence
sequences. Poor quality reference data will result in poorly curated
gene models.

## Usage

``` r
custom_curation_db(
  path = ".",
  genes_to_add = NULL,
  gene_fasta_dir = NULL,
  path_to_makeblastdb = NULL,
  base_db = "Metazoa"
)
```

## Arguments

- path:

  Path to the project directory (default = current working directory)

- genes_to_add:

  Full path to CSV file containing three columns: SeqID = unique name to
  be used for sequence, Gene = name of gene, FASTA = name of fasta file
  containing the sequence

- gene_fasta_dir:

  Full path to directory containing your gene FASTA files, one file per
  sequence

- path_to_makeblastdb:

  Full path to makeblastdb, only necessary if not already in your PATH

- base_db:

  Which base NCBI RefSeq database to use, "Metazoa" or "Chordata"?
  Default = "Metazoa"

## Details

Values in "Gene" column of your CSV must only include the following gene
abbreviations:  
nad1 = "NADH dehydrogenase subunit 1",  
nad2 = "NADH dehydrogenase subunit 2",  
cox1 = "cytochrome c oxidase subunit 1",  
cox2 = "cytochrome c oxidase subunit 2",  
cox3 = "cytochrome c oxidase subunit 3",  
atp8 = "ATP synthase F0 subunit 8",  
atp6 = "ATP synthase F0 subunit 6",  
atp9 = "ATP synthase F0 subunit 9",  
cox3 = "cytochrome c oxidase subunit 3",  
nad3 = "NADH dehydrogenase subunit 3",  
nad4l = "NADH dehydrogenase subunit 4L",  
nad4 = "NADH dehydrogenase subunit 4",  
nad5 = "NADH dehydrogenase subunit 5",  
nad6 = "NADH dehydrogenase subunit 6",  
cob = "cytochrome b",  
dpo = "DNA-polymerase",  
lagli = "homing endonuclease",  
msh1 = "MutS mismatch DNA repair protein",  
mttb = "trimethylamine methyltransferase"
