# Generate Nextflow command to run pipline

Generate Nextflow command to run pipline

## Usage

``` r
nextflow_cmd(
  workflow = c("assemble", "annotate"),
  path = NULL,
  source = app_sys("nextflow"),
  userAsmbs = FALSE,
  base = run_basename(workflow)
)
```

## Arguments

- workflow:

  Which module to update (default = c("assemble", "annotate"))

- path:

  MitoPilot project directory

- source:

  Nextflow script source. By default, this will be in the \`nextflow/\`
  subdirectory of the package installation.

- userAsmbs:

  User supplied assemblies, TRUE/FALSE? (default = FALSE)

- base:

  Run name used for the log file,
  \`\<path\>/.runs/nextflow/\<base\>.nextflow.log\`.
