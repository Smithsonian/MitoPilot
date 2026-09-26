# Normalize GEOME BCIDs to bare ARKs

Normalize GEOME BCIDs to bare ARKs

## Usage

``` r
geome_normalize_bcid(x)
```

## Arguments

- x:

  Character vector of BCIDs, optionally with an n2t.net or geome-db.org
  URL prefix.

## Value

Character vector of \`ark:/NNNNN/...\` identifiers, NA where the input
is blank or not an ARK.
