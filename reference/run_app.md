# Run the Shiny Application

Run the Shiny Application

## Usage

``` r
run_app(
  onStart = NULL,
  options = NULL,
  enableBookmarking = NULL,
  uiPattern = "/",
  host = NULL,
  port = NULL,
  launch.browser = NULL,
  ...
)
```

## Arguments

- onStart:

  A function that will be called before the app is actually run. This is
  only needed for `shinyAppObj`, since in the `shinyAppDir` case, a
  `global.R` file can be used for this purpose.

- options:

  Named options that should be passed to the `runApp` call (these can be
  any of the following: "port", "launch.browser", "host", "quiet",
  "display.mode" and "test.mode"). You can also specify `width` and
  `height` parameters which provide a hint to the embedding environment
  about the ideal height/width for the app.

- enableBookmarking:

  Can be one of `"url"`, `"server"`, or `"disable"`. The default value,
  `NULL`, will respect the setting from any previous calls to
  [`enableBookmarking()`](https://rdrr.io/pkg/shiny/man/enableBookmarking.html).
  See
  [`enableBookmarking()`](https://rdrr.io/pkg/shiny/man/enableBookmarking.html)
  for more information on bookmarking your app.

- uiPattern:

  A regular expression that will be applied to each `GET` request to
  determine whether the `ui` should be used to handle the request. Note
  that the entire request path must match the regular expression in
  order for the match to be considered successful.

- host:

  character. Address to bind the Shiny server to. Use \`"0.0.0.0"\` to
  allow connections over an SSH tunnel from a remote machine. Default
  \`NULL\` lets Shiny choose (loopback).

- port:

  integer. Port for the Shiny server to listen on. Default \`NULL\` lets
  Shiny pick a random port.

- launch.browser:

  logical. Whether to open a local browser when the server starts.
  Default \`NULL\` resolves to \`interactive()\`.

- ...:

  arguments to pass to golem_opts. See \`?golem::get_golem_options\` for
  more details.
