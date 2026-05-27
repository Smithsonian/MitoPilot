# Initialize a test project

This function will set up a test project and fectch associated data from
ENA. The \`n\` parameter can be used to limit the number of species used
in the test project for faster set up.

## Usage

``` r
new_test_project(
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

  path the the directory for the test project (default = currect working
  directory). Will be created if it does not already exists.

- n:

  how many samples to include in the test project (Default = Inf,
  include all)

- full_size:

  (logical) Use the full size test data set (default = FALSE). Setting
  to TRUE will download the raw data from ENA, which will require 10GB
  and will take some time to complete. By default a set of smaller
  pre-filtered input files will be fetched from the MitoPilot github
  repo.

- executor:

  The executor to use for running the nextflow pipeline. A built-in
  template ("local" (default), "awsbatch", "slurm", "sge", "pbs", "lsf",
  "NMNH_Hydra", "NOAA_SEDNA") or a saved profile from
  \[generate_config()\]. See \[list_configs()\].

- container:

  The container to use for running the pipeline.

- Rproj:

  (logical) Initialize and open an RStudio project in the project
  directory (default = TRUE). This has now effect if not running
  interactively in RStudio.

- force:

  (logical) Force recreating of existing project database and config
  files (default = FALSE).

- ...:

  Additional arguments passed \`init_db()\`
