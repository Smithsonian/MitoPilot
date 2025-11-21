# Rotate circular mitogenome to start at given gene

Rotate circular mitogenome to start at given gene

## Usage

``` r
rotate_asmb(assembly = NULL, annotations = NULL, start_gene = "trnF")
```

## Arguments

- assembly:

  Biostring containing mitogenome assembly

- annotations:

  tibble containing mitogenome annotation, produced by \`annotate.R\`

- start_gene:

  name of gene (PCG, rRNA, or tRNA) to start circular assembly (default
  = "trnF")
