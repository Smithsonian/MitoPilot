# Check a native (no-container) MitoPilot environment

Sources \`\<prefix\>/activate.sh\` and looks up every pipeline tool.
Core tools are needed for the default workflow; optional tools back the
MitoFinder, ARWEN, and ORFfinder options and are absent after
\`install_mitopilot_native.sh –no-optional\`.

## Usage

``` r
native_check(prefix = Sys.getenv("MITOPILOT_NATIVE_PREFIX"), strict = TRUE)
```

## Arguments

- prefix:

  Directory given to \`install_mitopilot_native.sh –prefix\`. Defaults
  to \`MITOPILOT_NATIVE_PREFIX\` if set.

- strict:

  Error if any core tool is missing (default \`TRUE\`).

## Value

(invisibly) a data.frame with columns \`tool\`, \`required\`, \`found\`,
\`path\`, \`version\`.
