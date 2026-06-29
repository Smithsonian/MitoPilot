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
  use_mitos = TRUE,
  mitos_opts = "--intron 0 --oril 0",
  mitos_condaenv = "mitos",
  use_trnaScan = TRUE,
  trnaScan_opts = "-M vert -X 20",
  trnaScan_condaenv = "base",
  arwen_opts = "-mtx",
  use_arwen = FALSE,
  aragorn_opts = "-m -gcstd",
  aragorn_condaenv = "aragorn",
  use_aragorn = FALSE,
  use_mitos_best = TRUE,
  use_mitofinder = FALSE,
  mitofinder_db = NULL,
  mitofinder_new_genes = FALSE,
  mitofinder_allow_introns = FALSE,
  mitofinder_opts = "",
  mitofinder_condaenv = NULL,
  start_gene = "trnF",
  coverage_trim = TRUE,
  retain_low_conf_trna = FALSE,
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

- use_mitos:

  logical; whether to run MITOS2 annotation (default: TRUE).

- mitos_opts:

  Additional command line options for MITOS2.

- mitos_condaenv:

  Conda environment to run MITOS2 (default: "mitos").

- use_trnaScan:

  logical; whether to run tRNAscan-SE annotation (default: TRUE).

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

- use_mitofinder:

  logical; whether to run MitoFinder annotation (lowest priority;
  default: FALSE).

- mitofinder_db:

  path to a MitoFinder reference database (GenBank .gb).

- mitofinder_new_genes:

  logical; pass –new-genes to MitoFinder (default: FALSE).

- mitofinder_allow_introns:

  logical; pass –allow-intron to MitoFinder (default: FALSE).

- mitofinder_opts:

  additional command line options for MitoFinder.

- mitofinder_condaenv:

  conda environment containing MitoFinder, or NULL for PATH (default:
  NULL).

- start_gene:

  name of gene (PCG, rRNA, or tRNA) to start circular assembly (default
  = "trnF")

- coverage_trim:

  logical; whether to trim low-coverage ends of linear assemblies
  (default: TRUE).

- retain_low_conf_trna:

  logical; whether to keep low-confidence tRNAs with an unresolved
  ("NNN") anticodon. When FALSE (default) these are dropped and are not
  allowed to suppress overlapping MITOS2 tRNA predictions. When TRUE
  they are retained (and may suppress overlapping MITOS2 predictions,
  the original behavior).

- ignore_scaffolds:

  Comma-separated scaffold numbers (e.g. "1,3") to drop from the
  assembly before annotation. These correspond to the \`scaffold\`
  column in the assemblies table and reflect the per-scaffold \`ignore\`
  flag.

- out_dir:

  Output directory.
