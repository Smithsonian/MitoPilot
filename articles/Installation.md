# Installation and Requirements

Everything on this page happens once per machine. When it is done, go to
[Get
Started](https://smithsonian.github.io/MitoPilot/articles/MitoPilot.md)
and run the test project.

## What you need

| Requirement | Notes |
|----|----|
| [R](https://www.r-project.org/) \>= 4.4.0 | RStudio is recommended but not required |
| [Java](https://www.oracle.com/java/technologies/downloads/) 17 or newer | Required by Nextflow. Must be visible in the same shell that runs Nextflow |
| [Nextflow](https://www.nextflow.io/docs/latest/install.html) 24.10.x - 25.10.x | See the version note below |
| [Docker](https://docs.docker.com/engine/install/) or [Singularity / Apptainer](https://docs.sylabs.io/guides/latest/user-guide/quick_start.html) | Docker for a local machine, Singularity or Apptainer on nearly every HPC cluster |
| Disk space | About 15 GB for the container image, plus room for your data and the Nextflow `work/` directory |
| Internet access | Reference annotations and taxonomy are fetched from NCBI |

MitoPilot itself installs none of the bioinformatics tools it runs.
fastp, GetOrganelle, MitoFinder, bowtie2, BLAST+, MITOS2, tRNAscan-SE,
ARWEN, ARAGORN, and ORFfinder all live inside the container image, which
Nextflow pulls for you on the first run.

We provide cluster-specific instructions for two sites:

- [Smithsonian
  Hydra](https://smithsonian.github.io/MitoPilot/articles/NMNH-Hydra.md)
- [NOAA
  SEDNA](https://smithsonian.github.io/MitoPilot/articles/NOAA-SEDNA.md)

For any other cluster, see [HPC cluster
support](https://smithsonian.github.io/MitoPilot/articles/Custom-HPC.md).

## Install the R package

Open R and run:

``` r

if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager")
}
BiocManager::install("Smithsonian/MitoPilot", ask = FALSE)
```

The first install pulls a long list of dependencies and takes a while.
Warnings about system library paths that are not writeable are usually
harmless.

Alternatively, clone the repository and install from the project folder:

``` r

devtools::install()
```

Then load the package:

``` r

library(MitoPilot)
```

You should see a welcome message naming the Nextflow version MitoPilot
will use. If Nextflow cannot be found, make sure it is on your `PATH` in
the same R session (check with `Sys.which("nextflow")`).

### Updating, pinning, and rolling back

``` r

remove.packages("MitoPilot")
BiocManager::install("Smithsonian/MitoPilot")
.rs.restartR()   # or restart R yourself
```

Installing without a version tag tracks the latest development state. To
pin a specific release, or to roll back, name the tag:

``` r

BiocManager::install("Smithsonian/MitoPilot@1.5.2")
```

Each MitoPilot version pairs with a matching container image, and
[`new_project()`](https://smithsonian.github.io/MitoPilot/reference/new_project.md)
writes that image tag into the project’s `.config` when the project is
created. An existing project therefore keeps running the pipeline code
baked into its original image until you update it. The
[changelog](https://smithsonian.github.io/MitoPilot/news/index.md) lists
the container tag for every release.

**Warning.** Some releases change the project database schema. After
updating, existing projects may need
[`backwards_compatibility()`](https://smithsonian.github.io/MitoPilot/reference/backwards_compatibility.md)
before they will open. That migration is irreversible and a migrated
project cannot be reopened by an older MitoPilot, so back up your
projects before updating. The changelog says which releases are
affected.

## Nextflow version compatibility

MitoPilot checks your Nextflow version when the package is loaded and
when you create or launch a project. If your version is **too old**, the
package refuses to load and prompts you to update. If it is **too new**,
MitoPilot automatically sets the `NXF_VER` environment variable to a
compatible version.

The pipeline is tested and supported on **Nextflow 24.10.x through
25.10.x**. Versions outside this range are known to break it:

- **Nextflow 26.0 and newer** removed an internal method (`NF.isDsl2()`)
  that the [`nf-sqldb`](https://github.com/nextflow-io/nf-sqldb) plugin
  calls at load time. No released `nf-sqldb` version currently supports
  Nextflow 26+.
- **Versions older than 24.10** silently ignore the pipeline’s
  `workflow.failOnIgnore` setting.

If you install Nextflow through conda or mamba, pin it, because the
channels track the newest release:
`mamba install 'bioconda::nextflow<26'`.

## Container cache directory

MitoPilot runs each processing step inside a container (Docker locally,
or Singularity/Apptainer on most HPC clusters). The container image is
fairly large, so on first use it must be downloaded and, for
Singularity/Apptainer, converted to a single `.sif` file. By default
Nextflow caches this image inside each project’s `work/` directory,
which means every new project re-downloads and rebuilds the same image.
On shared cluster filesystems this can be slow enough to exceed the
default pull timeout and cause the run to fail.

To download the image once and reuse it across all projects, point
Nextflow at a single, persistent cache directory by setting an
environment variable before launching MitoPilot (for example in your
`~/.bashrc` or job submission script):

``` bash
# Singularity / Apptainer (most HPC clusters)
export NXF_SINGULARITY_CACHEDIR=/path/to/persistent/singularity_cache
export NXF_APPTAINER_CACHEDIR=/path/to/persistent/apptainer_cache

# Docker (local)
# Docker manages its own image cache, so no setting is required.
```

Choose a location with enough space that persists between sessions, not
a per-project or temporary scratch directory. If image pulls still time
out on a slow filesystem, add a `pullTimeout` setting to the
`singularity { }` block of the project’s `.config` file, for example
`pullTimeout = "3 h"`.

**Tip.** Each MitoPilot version pairs with a matching container image,
so after updating the package you may want to clear a stale cached
image. Remove the specific old image rather than the whole cache
(`singularity cache clean –name mitopilot-<old version>.sif`, or
`docker rmi macguigand/mitopilot:<old version>`), since clearing
everything also removes images belonging to other people and other
pipelines.

## Next steps

- [Get
  Started](https://smithsonian.github.io/MitoPilot/articles/MitoPilot.md)
  walks the whole pipeline using the built-in test project.
- [HPC cluster
  support](https://smithsonian.github.io/MitoPilot/articles/Custom-HPC.md)
  covers executors, cluster profiles, and running the app over an SSH
  tunnel.
- [Troubleshooting](https://smithsonian.github.io/MitoPilot/articles/Troubleshooting.md)
  and the [FAQ](https://smithsonian.github.io/MitoPilot/articles/FAQ.md)
  cover what to do when a run fails.
