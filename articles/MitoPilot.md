# Get Started

MitoPilot takes you from raw Illumina paired-end reads to polished
mitogenome records ready for submission to NCBI GenBank. The heavy
lifting is a [Nextflow](https://www.nextflow.io/) pipeline; you drive it
from an R Shiny app that shows you every sample, assembly, and
annotation. MitoPilot automatically curates annotations based on
clade-specific rulesets and also allows you to manually adjust
annotations when needed.

Start here. This page gets MitoPilot installed and the built-in test
project running, then hands off to a page per pipeline module. Work
through them in order; the whole test project takes an afternoon at
most.

HYDRA **Are you a Smithsonian user?** Boxes like this one flag the
places where usage on the Smithsonian Hydra computing cluster differs
from a generic setup. Before starting this tutorial, work through the
Hydra [Get an R
session](https://smithsonian.github.io/MitoPilot/articles/NMNH-Hydra.html#get-an-r-session-on-hydra)
and [Install
Nextflow](https://smithsonian.github.io/MitoPilot/articles/NMNH-Hydra.html#install-nextflow)
sections. Users can also check out the [2025 Smithsonian workshop
webpage](https://smithsonianworkshops.github.io/MitoPilot_workshop_2025/),
although it is slightly outdated.

------------------------------------------------------------------------

## Before you begin

MitoPilot needs R, Java, Nextflow, and a container runtime (Docker
locally, or Singularity/Apptainer on a cluster), plus about 15 GB of
disk for the container image. [Installation and
Requirements](https://smithsonian.github.io/MitoPilot/articles/Installation.md)
has the full list and the install commands. Work through that page
first, then come back here.

Everything in this tutorial happens in **R**, preferably from RStudio.
The only steps that need a **terminal** are installing Nextflow and, on
a cluster, checking on submitted jobs; those are called out where they
come up.

Load the package:

``` r

library(MitoPilot)
```

You should see a welcome message naming the Nextflow version MitoPilot
will use.

HYDRA **Run
[`hydra_setup()`](https://smithsonian.github.io/MitoPilot/reference/hydra_setup.md)
once per session.** RStudio Server sessions on Hydra start with a
stripped `PATH` that leaves out the job scheduler engine, Java, and your
`~/bin`, so Nextflow cannot find `qsub` and job submission fails. Before
creating a project or opening the app, you must call the following
function:

``` r
library(MitoPilot)
hydra_setup()
```

It puts the Hydra Java and job scheduler directories at the front of
your `PATH` and pins a compatible Nextflow version for the session. It
has no effect anywhere else, and warns if you are not on Hydra.

------------------------------------------------------------------------

## Set up the test project

MitoPilot ships with a small test dataset: 15 samples of pre-filtered
Illumina reads, mostly marine fishes. It is deliberately messy. Some
samples assemble cleanly, one has far too little data, one produces two
competing assemblies, and two produce fragmented assemblies. This allows
you to see more complex situations that need user judgment.

``` r

library(MitoPilot)

# Anywhere you have space. MitoPilot creates the directory if needed.
wd <- "~/MitoPilot_test/run_01"

new_test_project(
  path = wd,
  executor = "local",
  full_size = FALSE,
  Rproj = FALSE
)
```

`executor = "local"` runs everything on the machine you are sitting at
and uses Docker. To run on a cluster instead, see [HPC cluster
support](https://smithsonian.github.io/MitoPilot/articles/Custom-HPC.md)
and pass the matching executor name. `full_size = FALSE` uses the small
pre-filtered reads that ship with the package; `TRUE` downloads about 10
GB of raw data, which takes much longer to process.

MitoPilot lists the samples as it sets them up, then writes a hidden
`.config` file holding the Nextflow settings for the project directory.
The defaults are fine for the test project.

HYDRA **Project setup.** Pass `executor = “NMNH_Hydra”` so work is
dispatched to compute nodes instead of running on the login or RStudio
node. Put projects under `/pool` or `/scratch`. Home directories are far
too small and projects cannot be run from `/store`.

You can also bring your own assemblies to MitoPilot for annotation,
curation, and export.
[`new_test_project_userAsmb()`](https://smithsonian.github.io/MitoPilot/reference/new_test_project_userAsmb.md)
sets up the equivalent test project, covering the common cases. See
[Using Your Own
Assemblies](https://smithsonian.github.io/MitoPilot/articles/Your-Own-Assemblies.md).

------------------------------------------------------------------------

## Open the app

Once your test project is initialized, run the following in your R
session:

``` r

wd <- "~/MitoPilot_test/run_01"
setwd(wd)     # the app must be launched from the project directory
MitoPilot()
```

![Sample table in the Assemble module before the pipeline has
run](figures/get-started/assemble-table-prerun.png)

Sample table in the Assemble module before the pipeline has run

This is a fresh project, so the result columns are still empty: the
sample metadata comes from your mapping file, and everything else fills
in as the pipeline runs.

Each row is a sample. The dropdown at the top left switches between the
three pipeline modules, **Assemble**, **Annotate**, and **Export**,
which you work through in that order. The circular arrow refreshes the
table.

Clicking a column header sorts alphanumerically. The search box at the
top right filters everything, and most columns have their own filter
box.

Two icons sit to the left of each sample ID:

- **The lock** ( unlocked, locked). Unlocked samples are available for
  the next pipeline run to process. Locking a sample freezes it in the
  current module and makes it available for the next module. Select
  samples and use the `LOCK` button to toggle.
- **The state.** Status of the sample in the current module. The
  pipeline sets these automatically, but you can override them with the
  `STATE` button. This is how you can park samples you do not want to
  process yet or reset samples that have already completed the pipeline.
  There are five states:

| State | Meaning |
|----|----|
|  Hold / Waiting | Ready to be updated, but will be skipped on the next run |
|  Ready to Run | Will be updated on the next run |
|  In Progress | Partway through the current module |
|  Completed Successfully | Processed without problems |
|  Completed with Warning | Finished, but may have failed or needs manual review |

The Annotate module has no separate half-circle state; a unit being
worked on shows the runner icon until it finishes.

------------------------------------------------------------------------

## Test project walkthrough

Now you’re ready to start running the test project samples through each
MitoPilot module. Follow the links below to begin.

[1.
Assemble](https://smithsonian.github.io/MitoPilot/articles/Test-Project-Assemble.md)
[2.
Annotate](https://smithsonian.github.io/MitoPilot/articles/Test-Project-Annotate.md)
[3.
Export](https://smithsonian.github.io/MitoPilot/articles/Test-Project-Export.md)

## Where to go next

Once you’re comfortable with MitoPilot, try [starting a
project](https://smithsonian.github.io/MitoPilot/articles/Your-Own-Project.md)
with your own sequence data!

Additional articles for you to explore:

- [Building custom
  databases](https://smithsonian.github.io/MitoPilot/articles/custom_dbs.md)
- [Handling difficult
  assemblies](https://smithsonian.github.io/MitoPilot/articles/Difficult-Assemblies.md)
- [Curation ruleset
  browser](https://smithsonian.github.io/MitoPilot/articles/Ruleset-Browser.md)
  and [curation and validation
  details](https://smithsonian.github.io/MitoPilot/articles/Curation-and-Validation.md)
- [HPC cluster
  support](https://smithsonian.github.io/MitoPilot/articles/Custom-HPC.md),
  [Smithsonian
  Hydra](https://smithsonian.github.io/MitoPilot/articles/NMNH-Hydra.md),
  and [NOAA
  SEDNA](https://smithsonian.github.io/MitoPilot/articles/NOAA-SEDNA.md)
- [FAQ](https://smithsonian.github.io/MitoPilot/articles/FAQ.md) and
  [troubleshooting](https://smithsonian.github.io/MitoPilot/articles/Troubleshooting.md)
