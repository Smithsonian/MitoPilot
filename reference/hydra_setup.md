# Configure the R session environment for the NMNH Hydra cluster

RStudio Server sessions on Hydra start with a stripped \`PATH\` that
omits the Univa Grid Engine, Java, and user \`~/bin\` directories, so
Nextflow cannot find \`qsub\` and job submission fails. Call this once
per session, before launching the MitoPilot app, to prepend those
directories to \`PATH\`. Has no effect (and warns) when not running on
Hydra.

## Usage

``` r
hydra_setup()
```

## Value

Invisibly, \`TRUE\` if the Hydra environment was applied, otherwise
\`FALSE\`.
