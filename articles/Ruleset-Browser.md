# Curation Ruleset Browser

MitoPilot ships taxon-specific curation rulesets for a range of metazoan
clades. The interactive browser below lets you explore them: navigate
the taxonomy tree on the left (scroll, expand, and collapse clades),
then select a highlighted clade to view its curation rules on the right,
including the genetic code, gene-level length/overlap constraints, and
start/stop codons.

The taxonomy backbone is built from [NCBI
Taxonomy](https://www.ncbi.nlm.nih.gov/taxonomy).

[Open the browser in a full
window](https://smithsonian.github.io/MitoPilot/ruleset-browser.md)

## Regenerating the browser

The embedded page is a self-contained snapshot. To rebuild it
(e.g. after editing a ruleset), regenerate the asset and rebuild the
site:

``` r

MitoPilot::ruleset_browser(
  output_file = "pkgdown/assets/ruleset-browser.html",
  open = FALSE
)
pkgdown::build_site()
```

You can also generate a standalone copy for local use at any time:

``` r

MitoPilot::ruleset_browser()
```
