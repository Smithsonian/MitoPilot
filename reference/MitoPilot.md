# Open The MitoPilot GUI

Open The MitoPilot GUI

## Usage

``` r
MitoPilot(host = NULL, port = NULL, launch.browser = NULL, ...)
```

## Arguments

- host:

  character. Address to bind the Shiny server to. Use \`"0.0.0.0"\` to
  allow connections over an SSH tunnel from a remote machine. Default
  \`NULL\` lets Shiny choose (loopback).

- port:

  integer. Port for the Shiny server to listen on. Default \`NULL\` lets
  Shiny pick a random port.

- launch.browser:

  logical. Whether to open a local browser when the server starts.
  Default \`NULL\` resolves to \`interactive()\`, so desktop sessions
  auto-open but headless/remote sessions do not.

- ...:

  additional arguments passed to \`run_app()\`.
