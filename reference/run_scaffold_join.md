# Nextflow entry point: build + persist a joined assembly (file outputs only)

Reads the per-scaffold assembly FASTA + coverage CSVs + the reference
FASTA, joins, and writes the joined \`ID_assembly_0.fasta\`,
\`ID_assembly_0_coverageStats.csv\`, and a \`ID_joined_row.csv\` (one
row of \`assemblies\` columns) for the workflow to \`sqlInsert\`. No DB
connection is opened here; DB writes are done in Groovy (see
scaffold_join_workflow.nf).

## Usage

``` r
run_scaffold_join(
  assembly_fasta,
  coverage_csvs,
  ref_fasta,
  ID,
  out_dir,
  gap_len = 100,
  circular = FALSE,
  db = NULL,
  auto_join = TRUE,
  scaffold_hits = NULL
)
```

## Arguments

- assembly_fasta:

  path to the multi-scaffold assembly FASTA (headers \`ID.path.scaffold
  \[topology\]\`).

- coverage_csvs:

  character vector of per-scaffold coverageStats CSV paths.

- ref_fasta:

  path to the reference mitogenome FASTA.

- ID:

  sample id.

- out_dir:

  directory to write outputs into.

- gap_len:

  default N-gap length for unknown junctions.

- circular:

  whether to mark the joined molecule circular.

- db:

  optional DBI connection; unused for file-only outputs (DB writes are
  done in Groovy) and kept for call-site compatibility.

- auto_join:

  when TRUE (and the scaffolds' BLAST hits agree) build + write the
  joined Path 0. When FALSE (toggle off), when the hits disagree, or
  when a junction cannot be sized from the reference, only the
  precomputed scaffold-\>reference mappings are written, leaving the
  sample fragmented for manual review in the app.

- scaffold_hits:

  optional ';'-separated string of per-scaffold BLAST hits
  ("scaffold\|accession\|pident") passed from the workflow. Preferred
  over a DB read of \`assemblies.blast_accession\`, which is written by
  an async UPDATE with no happens-before relative to this step (so it
  can still be NULL here).

## Details

Every exit also writes \`join_status.txt\` ("joined", "declined" when
the join ran and refused, or "skipped" when it is toggled off) and
\`join_note.txt\` (a reason for "declined", empty otherwise), so the
workflow can tell each of those apart from a crashed task, which writes
neither file.
