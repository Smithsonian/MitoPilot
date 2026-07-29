# Flag outlier PCG annotations in an export group

For each protein-coding gene in an export group, aligns the amino-acid
translations across samples and flags annotations that are likely
mis-positioned: those whose start or stop extends past, or falls short
of, the alignment's well-occupied core by more than a set number of
residues (pointing at a start/stop codon placed too long or too short)
and those that align poorly to the rest of the group (a low
sequence-identity catch-all for badly annotated regions).

## Usage

``` r
flag_PCG_outliers(
  group,
  db,
  start_aa = 10,
  stop_aa = 10,
  ident_pct = 60,
  genes = NULL
)
```

## Arguments

- group:

  Name of the export group.

- db:

  Path to the project SQLite database.

- start_aa:

  Start-offset threshold (amino acids). A sample is flagged when its
  start extends past, or falls short of, the alignment core by more than
  this many residues. Default 10.

- stop_aa:

  Stop-offset threshold (amino acids). As \`start_aa\`, but for the stop
  end. Default 10.

- ident_pct:

  Identity threshold (percent). A sample is flagged when its mean
  pairwise identity to the rest of the group is below this. Default 60.

- genes:

  Optional character vector of gene names. When supplied, only these
  genes are aligned and flagged (the rest are skipped), e.g. to
  recompute a single gene edited via "Back to Review". Default \`NULL\`
  (all genes).

## Value

A list with two elements:

- flags:

  A tibble with one row per flagged (sample, gene): \`ID\`, \`label\`,
  \`path\`, \`scaffold\`, \`gene\`, \`pct_identity\`, \`start_offset\`,
  \`stop_offset\`, \`start_flag\`, \`stop_flag\`, \`identity_flag\`,
  \`issue\`.

- alignments:

  A named list (by gene) of clustering-ordered aligned \`AAStringSet\`
  objects, for every gene that has a flagged sample (plus any explicitly
  requested via \`genes\`, even if their last flag was cleared).

- samples:

  A named list (by gene) of tibbles listing every unit in the gene's
  alignment (\`ID\`, \`label\`, \`path\`, \`scaffold\`), flagged or not,
  so the review UI can edit any sample of the gene.
