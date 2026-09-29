# Normalize NCBI BioSample or SRA accessions

Normalize NCBI BioSample or SRA accessions

## Usage

``` r
ncbi_normalize_id(x)
```

## Arguments

- x:

  Vector of BioSample accessions (SAMN, SAMEA, SAMD), BioSample numbers,
  SRA accessions (SRR/ERR/DRR runs, SRX experiments, SRS samples), or
  ncbi.nlm.nih.gov biosample / sra links.

## Value

Character vector of upper-case IDs, NA where the input is blank or not a
BioSample or SRA ID.
