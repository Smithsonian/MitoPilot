# Interactive browser for MitoPilot curation rulesets

Generates a self-contained, interactive HTML visualization of the
taxon-specific curation rulesets bundled with MitoPilot. The left side
of the page is a collapsible taxonomy tree (you can scroll, expand, and
collapse clades); the right side displays the curation rules for the
currently selected clade.

## Usage

``` r
ruleset_browser(
  output_file = tempfile(fileext = ".html"),
  open = interactive(),
  refresh_cache = FALSE,
  targets = NULL
)
```

## Arguments

- output_file:

  Path for the generated HTML file. Default is a temporary file. The
  file is fully self-contained (no external dependencies) and can be
  shared or embedded in documentation.

- open:

  Logical, open the result in a browser when done? Default is
  [`interactive()`](https://rdrr.io/r/base/interactive.html).

- refresh_cache:

  Logical, ignore any cached NCBI lineages and re-fetch? Default
  `FALSE`.

- targets:

  Optional character vector to limit the browser to a subset of rulesets
  (e.g. `c("fish_mito", "bird_mito")`). Default is all rulesets.

## Value

(Invisibly) the path to the generated HTML file.

## Details

The taxonomy backbone is built by querying NCBI's Taxonomy database
(<https://www.ncbi.nlm.nih.gov/taxonomy>) for the full lineage of each
ruleset's anchor clade and merging those lineages into a single tree.
Each leaf in the tree corresponds to one MitoPilot ruleset (e.g.
`fish_mito`). NCBI lineages are cached locally so repeat calls work
offline.

## Examples

``` r
if (FALSE) { # \dontrun{
# Build and open the ruleset browser
ruleset_browser()

# Save to a specific location without opening
ruleset_browser("MitoPilot_rulesets.html", open = FALSE)
} # }
```
