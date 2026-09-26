# Installing Without Containers

## Running MitoPilot without Docker or Singularity

Some clusters do not allow Docker, Singularity, or Apptainer. MitoPilot
can run without them: every tool the pipeline needs installs into a
folder you own with a conda-style package manager, and no root access is
required. This page sets that up once per cluster.

**What you get.** The same pinned tool versions as the MitoPilot
container (R 4.5.2, Nextflow, GetOrganelle, MITOS2, BLAST+, and the
rest), a local copy of the metazoan BLAST database, and the MitoPilot R
package, all under one directory. Nothing uses user namespaces, ptrace,
FUSE, or setuid binaries.

### Requirements

| Requirement | Notes |
|----|----|
| Linux x86_64, glibc 2.17 or newer | RHEL/Rocky/Alma 7+, Ubuntu 18.04+, Debian 10+ |
| About 20 GB of disk on a filesystem that allows executables | This becomes `<install_dir>` below |
| Outbound HTTPS from the node where you run the install script | conda-forge, bioconda, GitHub, and NCBI are contacted once |
| `curl`, `tar`, `bash` | Present on every cluster |
| Optional: an existing `conda`, `mamba`, or `pixi` | If absent, the install script downloads `micromamba` (a single binary) for you |

### 1. Run the install script

Download the install script and the environment definitions from the
MitoPilot release you want (the script and the R package must be the
same version):

``` bash
ver=1.5.7
curl -L https://github.com/Smithsonian/MitoPilot/archive/refs/tags/${ver}.tar.gz | tar -xz
cd MitoPilot-${ver}/inst/native
```

**Pick your install directory first.** Everything on this page refers to
it as `<install_dir>`, for example
`/scratch/genomics/jsmith/mitopilot_native`. It needs about 20 GB on a
filesystem that allows executables. Pass it as `–prefix`; everything
MitoPilot needs goes inside that one directory.

``` bash
bash install_mitopilot_native.sh --prefix <install_dir>
```

That builds the core environments (30 to 90 minutes, mostly downloads),
fetches the BLAST database, and installs the MitoPilot R package into
the bundled R. Options:

| Flag | Effect |
|----|----|
| `--manager pixi` | Use pixi instead of micromamba; `mamba` and `conda` also accepted |
| `--no-optional` | Skip MitoFinder, ARWEN, and NCBI ORFfinder (saves about 2 GB; these are off by default in projects anyway) |
| `--skip-blast-db` | Skip the 289 MB database; reference search falls back to NCBI over the network |
| `--blast-db-url URL` | Download the database from another location (for example a mirror inside your cluster) |
| `--mitopilot-ref 1.5.7` | Install a MitoPilot tag from GitHub instead of the source tree the script came from |
| `--mitopilot-source PATH` | Install the R package from a local directory or tarball instead |
| `--skip-mitopilot` | Do not install the MitoPilot R package (you install it yourself, see step 4) |
| `--dry-run` | Show what would happen |

Re-running the script is safe; finished steps are skipped.

The install script sets strict channel priority (conda-forge over
bioconda). If you build the environments by hand from
`inst/native/envs/*.yml`, pass `--channel-priority strict` or set
`CONDA_CHANNEL_PRIORITY=strict`.

**What the install directory contains.** `envs/` holds one environment
per tool group (`mitopilot` for R, Java, Nextflow, and the assembly
tools, plus `mitos`, `trnascan`, `aragorn`, `bamreadcount`, `orffinder`,
and `mitofinder`), `ref_dbs/` the BLAST database, `opt/` the hand-built
optional tools, `nextflow_home/` Nextflow’s plugin and engine cache, and
`activate.sh`. The environments are created by path, not by name, so
`conda env list` shows them without names. That is deliberate:
everything stays under one directory you can move or delete, and nothing
lands among your own named environments. You never activate them
yourself; `activate.sh` does.

This is a one-time install. Nothing in the install directory is rebuilt
at run time, and only a release that changes tool pins needs a re-run
(see Updating).

### 2. Check the environment

From any R that has MitoPilot installed (the bundled one is at
`<install_dir>/envs/mitopilot/bin/R`):

``` r

library(MitoPilot)
native_check("<install_dir>")
```

Every core tool should show `found TRUE`. Optional tools are missing
only if you installed with `--no-optional`.

### 3. Save a config profile (once)

This step is needed for both a single workstation and a cluster. Without
it, `new_project(executor = "local")` uses the built-in template, which
runs the pipeline in Docker. The profile tells MitoPilot to use the
install directory instead, and it is saved so every later project can
reuse it by name.

**Workstation or a single node, no scheduler:**

``` r

library(MitoPilot)
generate_config(
  name             = "local_native",
  scheduler        = "local",
  container_engine = "none",
  native_prefix    = "<install_dir>"      # the install location
)
```

**Cluster with a scheduler:**

``` r

generate_config(
  name             = "my_cluster",
  scheduler        = "slurm",             # or sge, pbs, lsf
  queue            = "general",
  account          = "my_allocation",
  container_engine = "none",
  native_prefix    = "<install_dir>"      # the install location
)
```

**NMNH Hydra or NOAA SEDNA:** pass the built-in cluster template as
`scheduler` to keep its tuned resource settings (parallel environment,
memory flags, himem selection). This form is only for
`container_engine = "none"`; with containers, use
`new_project(executor = "NMNH_Hydra")` as before.

``` r

generate_config(
  name             = "hydra_native",
  scheduler        = "NMNH_Hydra",         # or NOAA_SEDNA
  container_engine = "none",
  native_prefix    = "<install_dir>"
)
```

Then create projects with `new_project(..., executor = "local_native")`
or `executor = "my_cluster"`, exactly as in [HPC cluster
support](https://smithsonian.github.io/MitoPilot/articles/Custom-HPC.md).
The profile makes every pipeline task source `<install_dir>/activate.sh`
before it runs, so no module loads or conda activation are needed on
compute nodes.

### 4. Launch the app

Which R runs the **app** does not matter, as long as MitoPilot is
installed in it and it is version 4.4 or newer. Pipeline tasks always
use the bundled R.

**From a terminal (SSH tunnel).** Source the environment, start R, and
follow [Accessing the MitoPilot app over an SSH
tunnel](https://smithsonian.github.io/MitoPilot/articles/Custom-HPC.html#accessing-the-mitopilot-app-over-an-ssh-tunnel):

``` bash
source <install_dir>/activate.sh
cd /path/to/my_project
R -e "MitoPilot::MitoPilot(host='0.0.0.0', port=7591, launch.browser=FALSE)"
```

**From RStudio Server or Open OnDemand.** Install MitoPilot in that R
once (`BiocManager::install("Smithsonian/MitoPilot")`), then in each
session:

``` r

library(MitoPilot)
native_setup("<install_dir>")
```

[`native_setup()`](https://smithsonian.github.io/MitoPilot/reference/native_setup.md)
puts the bundled Nextflow and Java on this session’s PATH, together with
the job scheduler commands (`sbatch`, `qsub`, or `bsub`) and their
environment variables as they were when the install script ran, since
RStudio Server sessions do not inherit a login shell’s PATH. If the
scheduler lives somewhere else on your cluster, edit the “job scheduler”
lines near the end of `activate.sh`. To make it automatic, add
`MITOPILOT_NATIVE_PREFIX=<install_dir>` to `~/.Renviron` and call
[`native_setup()`](https://smithsonian.github.io/MitoPilot/reference/native_setup.md)
with no arguments, or add the call to `~/.Rprofile`. The **Start**
button in the app reads the project config and finds the environment on
its own, so it works even without
[`native_setup()`](https://smithsonian.github.io/MitoPilot/reference/native_setup.md);
the setup is what makes
[`library(MitoPilot)`](https://github.com/Smithsonian/MitoPilot) report
the right Nextflow version and lets you run `nextflow` from the R
console.

**Batch submission.** When the app offers a cluster submission script,
the line `source <install_dir>/activate.sh` is already filled in.

**On a workstation.** With `activate.sh` sourced,
[`MitoPilot()`](https://smithsonian.github.io/MitoPilot/reference/MitoPilot.md)
from the bundled R opens your browser through `xdg-open`, which the
bundled R uses because it ships with no browser setting of its own. On a
headless machine there is nothing to open; pass `launch.browser = FALSE`
and use the tunnel above.

**Warning.** A user-level `NXF_VER` (for example in `~/.Renviron` or
`~/.bashrc`) overrides the bundled Nextflow in every R session. The
**Start** button and the submit script pin the bundled version
themselves, but
[`library(MitoPilot)`](https://github.com/Smithsonian/MitoPilot) will
report the other version and console calls to `nextflow` will download
it. Remove the setting on a cluster that uses the native install.

### Updating

Update the R package inside the bundled R with
`<install_dir>/envs/mitopilot/bin/Rscript -e 'BiocManager::install("Smithsonian/MitoPilot@<version>")'`,
and in any other R you use for the app. When a release changes tool pins
(see `inst/native/VERSIONS.md` in the release), delete
`<install_dir>/envs` and re-run the install script.

### Troubleshooting

- **Solver errors during install**: run again with `--manager mamba` if
  you have one, or copy the failing `envs/*.yml` name into an issue.
- **`Could not find nextflow` in RStudio Server**: call
  [`native_setup()`](https://smithsonian.github.io/MitoPilot/reference/native_setup.md).
- **Jobs fail with `runmitos: command not found`**: the task did not
  source `activate.sh`; check the project `.config` has
  `process.beforeScript`.
- **Disk quota exceeded**: the environments are large; `<install_dir>`
  needs about 20 GB.
- **`'browser' must be a non-empty character string`**: an older
  MitoPilot in the bundled R; update it, or pass
  `launch.browser = FALSE`.
- **A different Java or Nextflow wins on PATH**: source `activate.sh`
  last, after any `module load`; it puts the bundled environment first.
- **[`native_check()`](https://smithsonian.github.io/MitoPilot/reference/native_check.md)
  shows the wrong Nextflow version**: look for an `NXF_VER` in
  `~/.Renviron` or your shell profile.
- **`sbatch: command not found` from the Start button in RStudio
  Server**: the install script did not see the scheduler when it ran
  (for example it ran on a node without one). Add
  `mp_path_add '/path/to/scheduler/bin'` to the job scheduler block in
  `activate.sh`, or run the install script again from a login node.

### Shared install by an administrator

The install script needs no root, only a writable directory, so an
administrator can run it once into shared space and every user sources
the same `activate.sh`. Three adjustments for a read-only install
directory:

1.  **Nextflow home must be writable per user.** `activate.sh` only sets
    `NXF_HOME` when it is unset, so users export their own (for example
    `~/.nextflow`) before sourcing, or the administrator edits that
    line. Nextflow writes plugins and engine downloads there.
2.  **Permissions.** After the install, `chmod -R a+rX <install_dir>`.
    At run time everything writes into the project and the Nextflow work
    directory, never into the install directory. Prefer `micromamba`,
    `mamba`, or `conda` for a shared install: pixi’s activation may try
    to repair an environment it considers stale, which fails on a
    read-only tree.
3.  **The R package.** The administrator updates MitoPilot inside the
    bundled R; users may also install it in their own R for the app.

A module file is the natural wrapper. With Lmod:

``` bash
-- /apps/modulefiles/mitopilot/1.5.7.lua
setenv("MITOPILOT_NATIVE_PREFIX", "<install_dir>")
setenv("NXF_HOME", pathJoin(os.getenv("HOME"), ".nextflow"))
execute { cmd = "source <install_dir>/activate.sh", modeA = { "load" } }
```

Users then `module load mitopilot`, and
[`native_setup()`](https://smithsonian.github.io/MitoPilot/reference/native_setup.md)
needs no argument.
`generate_config(container_engine = "none", native_prefix = "<install_dir>")`
is unchanged for everyone.
