# Initialize a test project for user-supplied assemblies

Sets up a test project for the user-assembly workflow, with nine samples
that between them cover the shapes an assembly can arrive in: a linear
mitogenome, a circular one, one that is circular but reported as linear,
and six multi-contig assemblies: one holding a single mitogenome, one
holding none, one holding two from different species, one holding a
mitogenome that needs circularizing, one holding two mitogenomes of
which one needs circularizing, and one holding a mitogenome split across
three contigs. The assemblies ship with the package; the raw reads are
the same fish data used by \[new_test_project()\].

## Usage

``` r
new_test_project_userAsmb(
  path = ".",
  n = Inf,
  full_size = FALSE,
  executor = "local",
  container = paste0("macguigand/mitopilot:", utils::packageVersion("MitoPilot")),
  Rproj = TRUE,
  force = FALSE,
  ...
)
```

## Arguments

- path:

  path to the directory for the test project (default = current working
  directory). Will be created if it does not already exist.

- n:

  how many samples to include in the test project (Default = Inf,
  include all)

- full_size:

  (logical) Use the full size raw reads (default = FALSE). Setting to
  TRUE will download the reads from ENA, which will require several GB
  and will take some time to complete. By default the smaller
  pre-filtered read files packaged with MitoPilot are used.

- executor:

  The executor to use for running the nextflow pipeline. A built-in
  template ("local" (default), "awsbatch", "slurm", "sge", "pbs", "lsf",
  "NMNH_Hydra", "NOAA_SEDNA") or a saved profile from
  \[generate_config()\]. See \[list_configs()\].

- container:

  The container to use for running the pipeline.

- Rproj:

  (logical) Initialize and open an RStudio project in the project
  directory (default = TRUE). This has no effect if not running
  interactively in RStudio.

- force:

  (logical) Force recreating of existing project database and config
  files (default = FALSE).

- ...:

  Additional arguments passed to \[new_project_userAsmb()\]

## Details

The project is created with the mitogenome search, the circularization
attempt, and scaffold joining all switched on, since that is what these
samples are for.
