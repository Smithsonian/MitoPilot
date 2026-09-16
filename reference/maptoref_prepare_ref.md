# Read and validate a MapToRef reference mitogenome

Accepts a single-record GenBank file (first non-blank line starts with
LOCUS) or a single-record FASTA. Unlike the custom assembly database
parser, no organelle qualifier is required.

## Usage

``` r
maptoref_prepare_ref(
  ref_file,
  topology = NA_character_,
  genetic_code = NA_integer_,
  out_dir = "."
)
```

## Arguments

- ref_file:

  Path to the reference file.

- topology:

  "circular" or "linear". Required for a FASTA reference. For GenBank,
  the LOCUS line wins when it states one; otherwise this value is used
  and is required.

- genetic_code:

  The sample's genetic code, used only to warn when the reference
  disagrees.

- out_dir:

  Directory to write the \`maptoref/\` working files into.

## Value

A list with seq, length, topology, accession, organism, transl_table,
and notes.
