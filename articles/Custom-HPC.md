# Custom HPC Setup

## Running MitoPilot on your own HPC cluster

MitoPilot ships built-in executor templates for `local`, `awsbatch`, the
Smithsonian Hydra cluster (`NMNH_Hydra`), and the NOAA SEDNA cluster
(`NOAA_SEDNA`). For any other cluster, MitoPilot includes **generic
templates** for the four most common schedulers, plus a helper function
to build a Nextflow config for your cluster once and reuse it for every
project.

| Scheduler | `scheduler =` | Nextflow executor |
|----|----|----|
| SLURM | `"slurm"` | `slurm` |
| SGE / UGE | `"sge"` | `sge` |
| PBS Pro / OpenPBS / Torque | `"pbs"` | `pbspro` (edit to `pbs` for Torque/OpenPBS) |
| IBM Spectrum LSF | `"lsf"` | `lsf` |

### Configure MitoPilot for your cluster

[`generate_config()`](https://smithsonian.github.io/MitoPilot/reference/generate_config.md)
builds a Nextflow config from a generic template, fills in your
cluster-specific settings, and saves it as a **named profile** in your
persistent MitoPilot config directory (see
[`mitopilot_config_dir()`](https://smithsonian.github.io/MitoPilot/reference/mitopilot_config_dir.md)).

``` r

library(MitoPilot)

generate_config(
  name             = "my_cluster",     # what you'll pass to new_project(executor = ...)
  scheduler        = "slurm",
  queue            = "general",         # partition / queue (omit to use the cluster default)
  account          = "my_allocation",   # folded into clusterOptions (SLURM --account, PBS -A, LSF -P, SGE -P)
  cluster_options  = "--qos=long",      # any extra raw scheduler directives (optional)
  container_engine = "apptainer",       # "singularity", "apptainer", or "docker"
  container_cache  = "/scratch/$USER/sif" # optional cacheDir for singularity/apptainer
)
```

This writes `config.my_cluster` to your config directory. Prefer to be
prompted interactively? Pass `interactive = TRUE` and leave the cluster
values unset.

### Reuse your cluster config for every project

Once saved, the profile name behaves like any built-in executor:

``` r

new_project(
  path       = "path/to/project",
  mapping_fn = "path/to/mapping.csv",
  data_path  = "path/to/raw_data",
  executor   = "my_cluster"
)
```

[`new_project()`](https://smithsonian.github.io/MitoPilot/reference/new_project.md)
resolves the saved profile and fills in the per-project values (raw data
directory, container image, genetic code, etc.), leaving a ready-to-run
`.config` in the project directory.

List everything available (built-in templates plus your saved profiles):

``` r

list_configs()
```

### Per-process resources

The generated config sets defaults for CPUs and memory that work for
most datasets. Per-step resource requests (`coverage`,
`blast_ref_align`, `blast_gb`) live in the `params { }` block of the
`.config` file and can be edited by hand. Memory and CPU for all other
steps in the workflow are pulled dynamically from the project database
and can be set directly in the MitoPilot app.

### Validate before running

If `nextflow` is on your `PATH`, you can sanity-check the generated
config:

``` bash
cd path/to/project
cp .config nextflow.config
nextflow config .          # prints the resolved config, or a parse error
rm nextflow.config
```

After reviewing the `.config` file, launch the pipeline from the
MitoPilot app or submit it as a batch job using your cluster’s normal
submission tools (e.g. `sbatch`, `qsub`, `bsub`) wrapping the
`nextflow run` command. See the [NMNH
Hydra](https://smithsonian.github.io/MitoPilot/articles/NMNH-Hydra.md)
and [NOAA
SEDNA](https://smithsonian.github.io/MitoPilot/articles/NOAA-SEDNA.md)
vignettes for batch submission examples you can adapt.
