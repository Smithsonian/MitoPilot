# Mitogenome Annotation Wrapper

Uses Mitos2 and tRNAscan-SE to annotate a mitogenome assembly.

## Usage

``` r
annotate(
  assembly_fn = NULL,
  coverage_fn = NULL,
  cpus = 4,
  genetic_code = "2",
  ref_db = "Chordata",
  ref_dir = "/home/harpua/Jzonah/MitoPilot/ref_dbs/Mitos2",
  mitos_opts = "--intron 0 --oril 0",
  mitos_condaenv = "mitos",
  trnaScan_opts = "-M vert -X 20",
  trnaScan_condaenv = "base",
  arwen_opts = "-mtx",
  use_arwen = FALSE,
  aragorn_opts = "-m -gcstd",
  aragorn_condaenv = "aragorn",
  use_aragorn = FALSE,
  use_mitos_best = TRUE,
  start_gene = "trnF",
  coverage_trim = TRUE,
  ignore_scaffolds = NULL,
  out_dir = NULL
)
```

## Arguments

- assembly_fn:

  Path to the mitogenome assembly FASTA file.

- coverage_fn:

  Path to the mitogenome assembly coverage stats CSV file.

- cpus:

  Number of CPUs to use.

- genetic_code:

  Genetic code to use for annotation (default: 2).

- ref_db:

  Reference Mitos2 database to use for annotation (default: "Chordata").

- ref_dir:

  Path to the Mitos2 reference database.

- mitos_opts:

  Additional command line options for MITOS2.

- mitos_condaenv:

  Conda environment to run MITOS2 (default: "mitos").

- trnaScan_opts:

  Additional command line options for tRNAscan-SE.

- trnaScan_condaenv:

  Conda environment to run tRNAscan-SE (default: "base").

- arwen_opts:

  Additional command line options for ARWEN (default: "-mtx").

- use_arwen:

  logical; whether to run ARWEN tRNA prediction (default: FALSE).

- aragorn_opts:

  Additional command line options for ARAGORN (default: "-m -gcstd").

- aragorn_condaenv:

  Conda environment containing ARAGORN (default: "aragorn").

- use_aragorn:

  logical; whether to run ARAGORN tRNA prediction (default: FALSE).

- use_mitos_best:

  logical; whether to pass –best to MITOS2 (default: FALSE).

- start_gene:

  name of gene (PCG, rRNA, or tRNA) to start circular assembly (default
  = "trnF")

- coverage_trim:

  logical; whether to trim low-coverage ends of linear assemblies
  (default: TRUE).

- ignore_scaffolds:

  Comma-separated scaffold numbers (e.g. "1,3") to drop from the
  assembly before annotation. These correspond to the \`scaffold\`
  column in the assemblies table and reflect the per-scaffold \`ignore\`
  flag.

- out_dir:

  Output directory.
