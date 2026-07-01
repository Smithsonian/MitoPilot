# Update old project database for backwards compatibility

Migrates an old project's SQLite database to the current schema and
refreshes the curation rules. Each migration step is guarded by an
existence check, so the function is idempotent and safe to re-run;
already-current projects report "nothing to update" and exit without
changes. Migrations include, among others:

- `annotate`: "reviewed", "ID_verified", "problematic", "partial",
  "orf_opts" columns.

- `annotate_opts`: "start_gene", ARWEN / Aragorn / MitoFinder / tRNAscan
  / MITOS toggles and options, coverage/feature trimming, and reference
  db columns.

- `assemble_opts`: "assembler", "mitofinder_db"/"mitofinder",
  "max_paths", "max_scaffolds", "min_assembly_length", "join_scaffolds".

- `curate_opts`: "max_blast_hits", "ref_dir", "ref_db",
  "linear_complete" (and rewriting the legacy in-container Mitos2 ref
  path to the GitHub-hosted reference db).

- `assemble`: "poor_blast_ref" (migrated from `samples` and normalized
  to TEXT), BLAST result columns, "blast_opts".

- `samples`: numeric "genetic_code" (rebuilding any legacy TEXT column
  as INTEGER so assembly does not crash).

- New tables: "orf_opts", "blast_opts", "export_opts",
  "scaffold_mappings", "assemblies", "assembly_blast", and the
  "blast_ref_annotations"/"blast_ref_sequences"/"blast_ref_alignment"
  set.

## Usage

``` r
backwards_compatibility(path = ".", executor = NULL, update_config = TRUE)
```

## Arguments

- path:

  Path to the project directory (default = current working directory)

- executor:

  The executor whose config template to regenerate from. Same options as
  \[new_project()\]: a built-in template ("local", "awsbatch", "slurm",
  "sge", "pbs", "lsf", "NMNH_Hydra", "NOAA_SEDNA") or the name of a
  saved cluster profile (see \[list_configs()\]). Required when
  \`update_config = TRUE\`.

- update_config:

  Regenerate the Nextflow \`.config\` from the \`executor\` template
  (default \`TRUE\`). Set to \`FALSE\` to migrate only the SQLite
  database and leave the existing \`.config\` untouched.

## Details

When \`update_config = TRUE\` the Nextflow \`.config\` is regenerated
wholesale from the template for the supplied \`executor\` (rather than
patched line by line): the project-specific values (raw/asmb dirs, min
depth, genetic code, NCBI key, and, for generic HPC templates, queue /
penv / clusterOptions / container engine) are carried over, the
container is bumped to the current MitoPilot version, and the previous
\`.config\` is saved to a timestamped \`.config.bak.\<timestamp\>\`
backup. Because an existing config may carry hand edits that cannot be
parsed reliably (e.g. a multi-line \`clusterOptions\` closure), the
executor must be given explicitly and you should review the regenerated
config against the backup.
