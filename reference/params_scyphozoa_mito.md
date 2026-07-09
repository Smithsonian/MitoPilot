# Default curation and validation parameters for Scyphozoa mitochondria

Draft ruleset derived from a GenBankMitoReview survey of 39 complete
Scyphozoa (true jellyfish) mitogenomes (translation table 4). Scyphozoa,
like other Medusozoa, have a linear mitogenome carrying the standard 13
PCGs, two rRNAs, and only two tRNAs (trnM, trnW). The linear termini
bear a variably annotated DNA-polymerase-B accessory ORF (dpo; GenBank
synonyms polB / dnaB are normalized to dpo) and, in some lineages, a
named orf314 ORF; both are treated as optional (count 0-1). Modeled on
the Hydrozoa ruleset, its closest bundled relative.

## Usage

``` r
params_scyphozoa_mito(alt = list())
```

## Arguments

- alt:

  (optional) named list of default values to modify
