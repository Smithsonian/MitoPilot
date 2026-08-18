# HPC Cluster Support

## Running MitoPilot on your own HPC cluster

The following steps will help you get started running MitoPilot on a HPC
cluster. We have specific instructions for the [Smithsonian Hydra
cluster](https://smithsonian.github.io/MitoPilot/articles/NMNH-Hydra.html)
and the [NOAA SEDNA
cluster](https://smithsonian.github.io/MitoPilot/articles/NOAA-SEDNA.html).

This page covers the cluster-specific parts only: getting an R session,
writing a config, and submitting work. Everything about using MitoPilot
itself is the same as in the [Get
Started](https://smithsonian.github.io/MitoPilot/articles/MitoPilot.md)
walkthrough, so run through that first if you have not already.

### Launch an R session on the cluster

Everything that follows (creating a cluster config, initializing
projects, launching the app) runs from an **R session on the cluster**.
You have two ways to get one.

#### Option A: the MitoPilot container

The MitoPilot Docker image is a single self-contained “box” that already
includes R, MitoPilot, and every tool the pipeline uses. You run the R
*inside* that box. You do not need to install R, the MitoPilot package,
or any other tools on the cluster.

A quick vocabulary primer if containers are new to you:

- **Docker image**: the prebuilt box of software. MitoPilot publishes
  one on [Docker Hub](https://hub.docker.com/r/macguigand/mitopilot).
- **Singularity / Apptainer**: the container runtime that nearly every
  HPC cluster provides (Docker itself is usually not permitted on shared
  clusters). It can run a Docker image directly. The two names are
  interchangeable here.
- **`.sif` file**: Singularity’s single-file copy of the image. You
  build it once, then reuse it.

**1. Pull the image once.** From an interactive session or compute node
(not the login node):

The image tag must match your installed MitoPilot version. Get it with
`packageVersion("MitoPilot")`, which for this build of the documentation
is 1.5.2.

``` bash
singularity pull mitopilot.sif docker://macguigand/mitopilot:<version>
```

**2. Start an R session.** By default a Singularity container can only
see your **home directory**. Bind any other locations your projects or
data live in (for example `/scratch` or `/pool`) with `--bind`, and pin
the working directory with `--pwd` (the container otherwise starts in
your home, so use absolute paths in R):

``` bash
cd /scratch/$USER/my_runs
singularity exec --bind /scratch,/pool --pwd $PWD mitopilot.sif R
```

#### Option B: RStudio Server or a local R install

If your cluster offers **RStudio Server**, or you already have R with
the MitoPilot package installed on the cluster, you can run every step
below in that R session instead of the container. Open RStudio in your
browser (or start R as usual) and run the same commands. If using
RStudio Server, you can launch the app normally and skip the SSH-tunnel
section.

The rest of this guide works the same in either R session.

### Configure MitoPilot for your cluster

MitoPilot ships built-in executor templates for `local`, `awsbatch`, the
Smithsonian Hydra cluster (`NMNH_Hydra`), and the NOAA SEDNA cluster
(`NOAA_SEDNA`). For any other cluster, MitoPilot includes **generic
templates** for the four most common schedulers, plus a helper function
to build a Nextflow config for your cluster once and reuse it for every
project. See below for details.

Pass the value in the middle column to
`generate_config(scheduler = ...)`:

| Scheduler                | Value     | Nextflow executor |
|--------------------------|-----------|-------------------|
| SLURM                    | `"slurm"` | `slurm`           |
| SGE / UGE                | `"sge"`   | `sge`             |
| PBS Pro, OpenPBS, Torque | `"pbs"`   | `pbspro` [^1]     |
| IBM Spectrum LSF         | `"lsf"`   | `lsf`             |

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

**Note:** Saved cluster configs need no extra binding if running R with
Docker/Singularity. The
[`generate_config()`](https://smithsonian.github.io/MitoPilot/reference/generate_config.md)
writes its profiles to `~/.config/R/MitoPilot` inside your home
directory, and Singularity mounts your home automatically. A profile you
save once therefore persists and is found by
[`new_project()`](https://smithsonian.github.io/MitoPilot/reference/new_project.md)
on every later run, with no `--bind` needed. (This only breaks if you
deliberately launch with `--contain` or `--no-home`, which detach the
home directory.)

### Reuse your cluster config for every project

Once saved, the profile name behaves like any built-in executor:

``` r
new_project(
  ...
  executor   = "my_cluster",
  ...
)
```

[`new_project()`](https://smithsonian.github.io/MitoPilot/reference/new_project.md)
resolves the saved profile and fills in the per-project values (raw data
directory, container image, genetic code, etc.), leaving a ready-to-run
`.config` in the project directory. When running from the container, use
**absolute paths** so the project lands where you expect (the container
starts in your home directory).

List everything available (built-in templates plus your saved profiles):

``` r

list_configs()
```

### The project `.config` file

[`new_project()`](https://smithsonian.github.io/MitoPilot/reference/new_project.md)
writes a `.config` file into the project directory, built from your
executor’s template. It may contain placeholders in the form
`<<PARAMETER_NAME>>` that you need to fill in. Every generated config
includes `rawDir = '<<RAW_DIR>>'`, which must point at the directory
holding the raw reads named in your mapping file. Review the `.config`
after creating a project and make sure nothing is left unfilled. The
same file is where you point individual processing steps at a different
container image.

If you would rather supply a Nextflow config of your own instead of
using a template or a saved profile, pass it directly:

``` r

new_project(..., config = "path/to/.config")
```

### Per-process resources

The generated config sets defaults for CPUs and memory that work for
most datasets. Per-step resource requests (`coverage`,
`blast_ref_align`, `blast_gb`) live in the `params { }` block of the
`.config` file and can be edited by hand. Note that `blast_gb` now runs
a real local BLAST search against the mitogenome database packaged in
the container, so it asks for CPUs rather than just waiting on the
network. Memory and CPU for all other steps in the workflow are pulled
dynamically from the project database and can be set directly in the
MitoPilot app.

### Validate before running

If `nextflow` is on your `PATH`, you can sanity-check the generated
config by running the following commands in a terminal:

``` bash
cd path/to/project
nextflow config .config          # prints the resolved config, or a parse error
```

### Accessing the MitoPilot app over an SSH tunnel

The MitoPilot app is just a Shiny web server over the project’s SQLite
database and Nextflow. It needs **no browser and no X11 on the
cluster**, so you do not need RStudio Server just to use the app.
Instead, run the server on a cluster node with a fixed host and port,
then reach it from your local computer over an SSH tunnel. (If you are
using RStudio Server instead, skip this section: just call
[`MitoPilot()`](https://smithsonian.github.io/MitoPilot/reference/MitoPilot.md)
and it opens in your browser.)

**1. On the cluster**, launch MitoPilot from your project directory and
tell it not to open a browser. From the container in one line:

``` bash
proj=/scratch/$USER/my_runs/project01
singularity exec --bind /scratch --pwd "$proj" mitopilot.sif \
  R -e "setwd('$proj'); MitoPilot::MitoPilot(host='0.0.0.0', port=7591, launch.browser=FALSE)"
```

or from an R session already in the project directory:

``` r

library(MitoPilot)
MitoPilot(host = "0.0.0.0", port = 7591, launch.browser = FALSE)
```

As the server starts it prints the exact tunnel command using the live
node hostname, for example:

    ssh -N -L 7591:NODE:7591 <user>@<cluster>

**2. On your local computer**, open the tunnel (substitute your username
and login host. If MitoPilot is running on a compute node reached via
`srun --pty`, the printed `NODE` should be that compute node.

``` bash
ssh -N -L 7591:NODE:7591 <user>@<cluster>
```

**3. In your web browser**, open:

    http://localhost:7591

Leave both the cluster R session and the local `ssh` session open while
you use the app. This replaces the RStudio Server requirement for users
who only need the app.

**Caveat:** in-app features that open a file or browser on the server
side (the “Open output folder” button, the HTML annotation report, and
the ruleset browser) will not work here, since they try to open on the
cluster rather than your local computer. The main assemble / annotate /
curate app is unaffected.

### Running the pipeline

When running MitoPilot via a ssh tunnel, it will **not** run Nextflow on
the node hosting the app, since that would tie up an interactive node or
the login node. Instead, when you start a workflow, the update window
shows a ready-to-edit **cluster submission script**:

- It is pre-filled with the correct scheduler directives (`#SBATCH`,
  `#$`, `#PBS`, or `#BSUB`) for your project’s executor.
- Edit the resources, and add your environment setup where indicated
  (for example `module load java`, `mamba activate ...`, or
  `export NXF_SINGULARITY_CACHEDIR=...`). Those lines are pre-filled as
  commented examples.
- Click **“Submit to Cluster”** to submit it with your scheduler’s
  command (`sbatch` / `qsub` / `bsub`), or **“Save Script Only”** to
  write the script and submit it yourself.
- Your edits to the resource block are remembered per project (saved to
  `.mitopilot_submit.template`), so the next run pre-fills them. The
  “Resume previous run?” checkbox still adds or removes `-resume`
  without disturbing your edits.

**Note:** “Submit to Cluster” calls `sbatch` / `qsub` / `bsub` directly,
and those commands are usually **not** available inside a Singularity
container. If you launched the MitoPilot app from a container, use
**“Save Script Only”** and then submit the written `.sh` from a normal
cluster shell. The submitted job runs on a compute node and calls
Nextflow, so make sure `java` and `nextflow` are available via the
script’s environment-setup lines.

[^1]: The generated config uses the `pbspro` executor. On Torque or
    OpenPBS, edit that line in the `.config` to `pbs`.
