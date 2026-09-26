# Normalize GBIF occurrence IDs

Normalize GBIF occurrence IDs

## Usage

``` r
gbif_normalize_id(x)
```

## Arguments

- x:

  Vector of gbifIDs: digits, numbers, or a gbif.org / api.gbif.org
  occurrence URL. Smithsonian NMNH EZIDs (ark:/65665/3..., n2t.net or
  collections.nmnh.si.edu links) are also accepted.

## Value

Character vector of digit-only IDs or canonical NMNH ARKs, NA where the
input is blank or not an occurrence ID.
