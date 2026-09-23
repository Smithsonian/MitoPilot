# Use a native (no-container) MitoPilot environment in this R session

Applies the PATH and variables from \`\<prefix\>/activate.sh\` (written
by \`inst/native/install_mitopilot_native.sh\`) to the current session,
so \`nextflow\`, \`java\`, and every pipeline tool resolve here exactly
as they do inside pipeline tasks. Call it once after
\`library(MitoPilot)\` when running the app from RStudio Server or any R
session that was not started from a shell where \`activate.sh\` was
already sourced. The app itself reads the same file from the project
\`.config\`, so this is only needed for console use.

## Usage

``` r
native_setup(prefix = Sys.getenv("MITOPILOT_NATIVE_PREFIX"))
```

## Arguments

- prefix:

  Directory given to \`install_mitopilot_native.sh –prefix\`. Defaults
  to \`MITOPILOT_NATIVE_PREFIX\` if set.

## Value

(invisibly) \`TRUE\`.
