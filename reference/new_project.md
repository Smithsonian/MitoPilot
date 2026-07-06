# Initialize new MitoPilot Project

Initialize new MitoPilot Project

## Usage

``` r
new_project(
  path = ".",
  mapping_fn = NULL,
  mapping_id = "ID",
  data_path = NULL,
  min_depth = 2e+06,
  genetic_code = NULL,
  executor = c("local", "awsbatch", "slurm", "sge", "pbs", "lsf", "NMNH_Hydra",
    "NOAA_SEDNA"),
  container = paste0("macguigand/mitopilot:", utils::packageVersion("MitoPilot")),
  custom_seeds_db = NULL,
  custom_labels_db = NULL,
  config = NULL,
  profile_dir = mitopilot_config_dir(),
  ncbi_api_key = NULL,
  Rproj = TRUE,
  force = FALSE,
  ...
)
```

## Arguments

- path:

  Path to the project directory (default = current working directory)

- mapping_fn:

  Path to a mapping file. Should be a csv that minimally includes an
  \`ID\` column with a unique identifier for each sample, a \`Taxon\`
  column containing taxonomic information for each sample, and columns
  \`R1\` and \`R2\` specifying the names of the raw paired read inputs.
  May include additional columns with other sample metadata.

- mapping_id:

  The name of the column in the mapping file that contains the unique
  sample identifiers (default = "ID").

- data_path:

  Path to the directory where the raw data is located. Can be a AWS s3
  bucket even if not using AWS for pipeline execution..

- min_depth:

  Minimum number of paired sequences after pre-processing to proceed
  with assembly (default: 2000000 reads)

- genetic_code:

  Optional NCBI translation table override. Default \`NULL\`
  auto-selects the genetic code from each sample's curation ruleset.
  Supplying a number sets a project-wide override on the default
  curation options. See
  https://www.ncbi.nlm.nih.gov/Taxonomy/Utils/wprintgc.cgi

- executor:

  The executor to use for running the nextflow pipeline. May be a
  built-in template ("local" (default), "awsbatch", "slurm", "sge",
  "pbs", "lsf", "NMNH_Hydra", "NOAA_SEDNA") or the name of a saved
  cluster profile created with \[generate_config()\]. See
  \[list_configs()\] for available names.

- container:

  The docker container to use for pipeline execution.

- custom_seeds_db:

  Full path to custom seeds database for GetOrganelle

- custom_labels_db:

  Full path to custom labels database for GetOrganelle

- config:

  (optional) provide a path to an existing custom nextflow config file.
  If not provided a config file template will be created based on the
  specified executor.

- profile_dir:

  Directory searched for saved cluster profiles when resolving
  \`executor\` (default \[mitopilot_config_dir()\]).

- ncbi_api_key:

  Optional NCBI API key string. Used to raise NCBI request rate limits
  for the remote BLAST + GenBank fetch steps. See
  \<https://www.ncbi.nlm.nih.gov/datasets/docs/v2/api/api-keys/\>. May
  be left empty and edited later in \`.config\`
  (\`params.ncbi_api_key\`).

- Rproj:

  (logical) Initialize and open an RStudio project in the project
  directory (default = TRUE). This option has no effect if not running
  interactively in RStudio.

- force:

  (logical) Force recreating of existing project database and config
  files (default = FALSE).

- ...:

  Additional arguments passed as default processing parameters to
  \`new_db()\`
