# Generate and save a reusable cluster config profile

Configure your HPC cluster once and reuse it for every project. This
builds a Nextflow config from a generic scheduler template, fills in the
cluster-level settings you provide (queue, account / clusterOptions,
container engine), and saves it as a named profile in
\[mitopilot_config_dir()\]. Afterwards, \`new_project(executor =
"\<name\>")\` (or \`new_project_userAsmb()\`) will find and use it
automatically, filling in the remaining per-project values.

## Usage

``` r
generate_config(
  name,
  scheduler = c("slurm", "sge", "pbs", "lsf", "local", "awsbatch"),
  container_engine = c("auto", "singularity", "apptainer", "docker"),
  container_cache = NULL,
  container_run_options = NULL,
  queue = NULL,
  account = NULL,
  cluster_options = NULL,
  penv = "mthread",
  profile_dir = mitopilot_config_dir(),
  interactive = FALSE,
  overwrite = FALSE
)
```

## Arguments

- name:

  Profile name. Saved as \`config.\<name\>\`; pass this as the
  \`executor\` argument to \`new_project()\`.

- scheduler:

  Base template to build on. One of "slurm", "sge", "pbs", "lsf",
  "local", or "awsbatch".

- container_engine:

  Container runtime. "auto" picks docker for local/awsbatch and
  singularity for HPC schedulers; or set explicitly to "singularity",
  "apptainer", or "docker".

- container_cache:

  Optional cacheDir for singularity/apptainer.

- container_run_options:

  Optional runOptions for singularity/apptainer (e.g. bind mounts).

- queue:

  Partition / queue name. If \`NULL\`, the queue directive is omitted
  (cluster default is used).

- account:

  Optional accounting / project string. Folded into clusterOptions per
  scheduler (SLURM \`–account=\`, PBS \`-A\`, LSF \`-P\`, SGE \`-P\`).

- cluster_options:

  Optional raw clusterOptions string appended verbatim.

- penv:

  SGE parallel environment (default "mthread"); ignored for other
  schedulers.

- profile_dir:

  Directory to save the profile in (default \[mitopilot_config_dir()\]).

- interactive:

  If \`TRUE\` and running interactively, prompt for any unset queue /
  account / cluster_options / container values.

- overwrite:

  Overwrite an existing profile of the same name (default \`FALSE\`).

## Value

(invisibly) the path to the saved profile.

## Details

Per-project placeholders (\`\<\<CONTAINER_ID\>\>\`, \`\<\<RAW_DIR\>\>\`,
\`\<\<ASMB_DIR\>\>\`, \`\<\<MIN_DEPTH\>\>\`, \`\<\<NCBI_API_KEY\>\>\`)
are intentionally left in the saved profile for the project-init step to
complete.
