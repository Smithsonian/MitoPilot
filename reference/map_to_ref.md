# Map-to-reference mitogenome assembly

Maps a sample's reads to a reference mitogenome, feeds the consensus
back in as the next mapping reference until it stops changing, then
calls the published sequence from a final pass over all reads. The
reference base never enters the published sequence.

## Usage

``` r
map_to_ref(
  id,
  ref,
  reads_1,
  reads_2,
  bowtie2_opts = "--very-sensitive-local",
  consensus_opts = "-d 3 --min-BQ 20",
  iter_cap = 5,
  topology = NA_character_,
  genetic_code = NA_integer_,
  cpus = 4,
  out_dir = ".",
  ref_value = NA_character_,
  mapper = "bowtie2"
)
```

## Arguments

- id:

  Sample ID.

- ref:

  Path to the reference (.gb or FASTA, one record).

- reads_1, reads_2:

  Preprocessed paired reads.

- bowtie2_opts:

  Flags passed verbatim to the chosen mapper.

- consensus_opts:

  Flags passed to samtools consensus after validation.

- iter_cap:

  Maximum number of iteration passes.

- topology:

  "circular" or "linear"; required for a FASTA reference, ignored for
  GenBank.

- genetic_code:

  The sample's genetic code, used only for a warning.

- cpus:

  Threads.

- out_dir:

  Output directory.

- ref_value:

  The reference exactly as the user configured it: an absolute file
  path, a URL, or an NCBI nucleotide accession. An accession is
  downloaded from GenBank here; anything else means `ref` is already the
  staged reference file.

- mapper:

  Read mapper: "bowtie2", "bwa-mem", or "bwa-aln" (bwa aln plus sampe,
  for short or damaged reads such as ancient DNA). The first pass
  against the user's reference runs with relaxed seeding appended to the
  flags; later passes and the final pass use the flags as given.

## Value

invisibly TRUE on success, FALSE after writing the failure sentinel.
