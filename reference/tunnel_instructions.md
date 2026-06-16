# Print SSH tunnel instructions for a headless GUI session

Prints, ready to copy, the \`ssh -L\` command that forwards a local port
to the cluster node running the MitoPilot Shiny server, plus the URL to
open in a local browser. The node hostname is read from the live
session; the login host is unknown to R and emitted as a \`\<cluster\>\`
placeholder.

## Usage

``` r
tunnel_instructions(port)
```

## Arguments

- port:

  integer. The port the Shiny server is listening on.
