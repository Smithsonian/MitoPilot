# List available executor configs

Reports the package built-in templates plus any saved cluster profiles.
The \`name\` column is what you pass to \`new_project(executor = ...)\`.

## Usage

``` r
list_configs(profile_dir = mitopilot_config_dir())
```

## Arguments

- profile_dir:

  User profile directory (see \[mitopilot_config_dir()\]).

## Value

A data.frame with columns \`name\`, \`type\` ("builtin" or "saved"), and
\`path\`.
