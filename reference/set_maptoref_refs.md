# Set per-sample MapToRef references

Assigns a MapToRef reference mitogenome to individual samples in an
existing project. A reference has one home, the sample row; the
parameter set the sample is on supplies the mapper and its options. A
FASTA reference also needs a topology (circular or linear) in the third
column. A blank reference clears both values.

## Usage

``` r
set_maptoref_refs(path = ".", refs = NULL)
```

## Arguments

- path:

  Path to the project directory (default = current working directory)

- refs:

  A CSV path or a data frame. The first column holds sample IDs and the
  second holds references; column names are ignored, but a CSV must have
  a header row (its first line is not read as data). An optional third
  column holds the topology, required for a FASTA reference; any further
  columns are ignored. A reference is an absolute file path to a
  single-record GenBank or FASTA mitogenome, a URL, or an NCBI
  nucleotide accession (for example NC_002333). Blank clears the
  sample's reference.

## Value

Invisibly, the IDs that still have no reference.

## Details

Samples whose reference actually changes are queued for (re-)assembly,
the same way changing a sample's parameter set does in the Assemble
module.
