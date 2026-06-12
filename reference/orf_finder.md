# Find open reading frames in unannotated regions

Optional annotation step (WF2) that runs after curation/validation. Uses
NCBI ORFfinder to locate open reading frames, retains only those falling
in regions without an existing annotation (allowing a user-set degree of
overlap), keeps the longest ORF when several overlap the same
unannotated region, and BLASTs each surviving ORF against the combined
curation protein database so the annotate-details modal can show
candidate gene identities. ORFs are written as annotations named
\`ORF.1\`, \`ORF.2\`, ... with \`type = "ORF"\`.

## Usage

``` r
orf_finder(
  annotations_fn = NULL,
  assembly_fn = NULL,
  genetic_code = "2",
  orffinder_opts = "-s 1",
  orffinder_condaenv = "orffinder",
  orf_min_len = 300,
  orf_max_overlap = 0.1,
  orf_nested = FALSE,
  ref_dir = ".",
  max_blast_hits = 10,
  blast_condaenv = "base",
  out_dir = NULL
)
```

## Arguments

- annotations_fn:

  Path to the (final, validated) annotations TSV.

- assembly_fn:

  Path to the curated assembly FASTA.

- genetic_code:

  NCBI translation table number (default: "2").

- orffinder_opts:

  Extra ORFfinder command-line options (default: "-s 1"). \`-g\`,
  \`-ml\`, and \`-n\` are set automatically; do not include them here.

- orffinder_condaenv:

  Conda environment containing ORFfinder (default: "orffinder"). Set
  NULL to use ORFfinder on the PATH.

- orf_min_len:

  Minimal ORF length in nucleotides (default: 300).

- orf_max_overlap:

  Maximum overlap with existing annotations, as a fraction of the ORF
  length, before an ORF is discarded (default: 0.1).

- orf_nested:

  Logical; if TRUE pass `-n false` to ORFfinder so nested/overlapping
  ORFs are reported (default: FALSE).

- ref_dir:

  Path to the curation reference directory (must contain a
  \`featureProt/\` subdirectory of per-gene protein FASTAs).

- max_blast_hits:

  Maximum number of BLAST hits to retain per ORF (default: 10).

- blast_condaenv:

  Conda environment containing blastp/makeblastdb (default: "base"). Set
  NULL to use them on the PATH.

- out_dir:

  Output directory for the ORF annotations TSV.
