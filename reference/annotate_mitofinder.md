# Annotate a mitogenome assembly with MitoFinder (WF2, optional)

Runs MitoFinder in annotation-only mode (\`-a\`) on an existing
assembly, independent of which assembler produced the contigs. Parses
MitoFinder's feature-level GFF output into the common MitoPilot
annotation data frame so the calls can be deduplicated against the other
annotation tools. MitoFinder is treated as the lowest-priority tool by
\`annotate()\` (it only fills gaps the higher-priority tools left).

## Usage

``` r
annotate_mitofinder(
  assembly = NULL,
  mitofinder_db = NULL,
  genetic_code = "2",
  new_genes = FALSE,
  allow_introns = FALSE,
  mitofinder_opts = "",
  cpus = 4,
  condaenv = NULL,
  workdir = NULL
)
```

## Arguments

- assembly:

  a DNAStringSet object

- mitofinder_db:

  path to a MitoFinder reference database (GenBank \`.gb\`), e.g. built
  with \`custom_assembly_db(db_type = "mitofinder")\`.

- genetic_code:

  NCBI genetic code number (default: 2)

- new_genes:

  logical; pass \`–new-genes\` to annotate non-standard genes (default:
  FALSE)

- allow_introns:

  logical; pass \`–allow-intron\` to search for genes with introns
  (default: FALSE). See note above re: ruleset \`intron = TRUE\`.

- mitofinder_opts:

  additional free-form command line options for MitoFinder

- cpus:

  number of processors for MitoFinder (\`-p\`)

- condaenv:

  conda environment containing MitoFinder, or NULL to run on PATH
  (default: NULL, mirroring the WF1 assembler call).

- workdir:

  working directory for the MitoFinder run (default: a tempdir).

## Value

a data frame with the common annotation columns (\`contig, type, gene,
product, pos1, pos2, length, direction, tRNA_ID, anticodon, start_codon,
stop_codon, translation\`). The codon/translation fields are populated
for PCGs (mirroring \`annotate_mitos2()\`). Empty (correctly typed) when
MitoFinder is off or finds nothing.

## Details

Intron-containing genes annotated with \`–allow-intron\` are returned as
multiple rows sharing one gene name (one per exon), matching the model
used by \`export.R\` / \`validate\`. Only merged correctly downstream
when the gene's curate ruleset has \`intron = TRUE\`; otherwise the exon
rows are not merged.
