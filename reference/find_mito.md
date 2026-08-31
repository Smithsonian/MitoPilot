# Find the mitochondrial contigs in a user-supplied assembly

Finds the mitochondrial contigs in a whole assembly: merges the BLAST
screen output, selects candidate contigs, extracts them from the
assembly with \`samtools faidx\`, confirms them with MitoFinder, and
writes the files the pipeline consumes. Called by the Assemble workflow;
not intended for direct use.

## Usage

``` r
find_mito(
  assembly_fn = NULL,
  hits_fn = NULL,
  id = "sample",
  mitofinder_db = NULL,
  genetic_code = 2,
  min_identity = 70,
  min_aligned_length = 300,
  min_aligned_fraction = 0.5,
  max_candidates = 20,
  min_genes = 3,
  cpus = 4,
  out_dir = "."
)
```

## Arguments

- assembly_fn:

  Path to the sample's full assembly (fasta)

- hits_fn:

  Chunk hit files written by the screen

- id:

  Sample ID, used to name the output fasta

- mitofinder_db:

  Path to a MitoFinder GenBank database, built with
  \[custom_assembly_db()\] (\`db_type = "mitofinder"\`)

- genetic_code:

  NCBI translation table passed to MitoFinder

- min_identity, min_aligned_length, min_aligned_fraction,
  max_candidates:

  Screen thresholds, see \[select_mito_contigs()\]

- min_genes:

  Mitochondrial genes a contig must carry to be confirmed

- cpus:

  Number of CPUs for MitoFinder

- out_dir:

  Directory for the outputs

## Value

(invisibly) a list with \`confirmed\`, \`note\` and \`evidence\`
