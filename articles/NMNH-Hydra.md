# NMNH Hydra Setup

This page covers what is specific to the Smithsonian NMNH Hydra cluster.
Once you are set up here, the rest of MitoPilot works exactly as in the
[Get
Started](https://smithsonian.github.io/MitoPilot/articles/MitoPilot.md)
walkthrough, with `executor = "NMNH_Hydra"`.

You need a Hydra account; see the [Hydra policies
page](https://confluence.si.edu/display/HPC/Hydra+Policies).

## Get an R session on Hydra

The simplest route is the dedicated RStudio Server at
<https://galaxy.si.edu/R4>, which you reach in a browser with your Hydra
credentials and which can see `/pool`, `/scratch`, and `/store`. It is a
shared resource, so use it for interactive work and submit long runs as
jobs. Details on the [Hydra RStudio
page](https://confluence.si.edu/display/HPC/Dedicated+RStudio+Server).

If you would rather run your own RStudio Server session, start one on a
login node and tunnel to it:

``` bash
conda deactivate                       # avoid package conflicts
module load tools/R/RStudio/server
start-rstudio-server
```

The command prints the `ssh -N -L ...` line to run in a terminal on your
own computer. Leave both terminals open and point a browser at
`http://localhost:8787`. If port 8787 is taken, pass a different one
with `start-rstudio-server -port 8890`.

**Note.** Use Chrome or Firefox. There are known problems running the
MitoPilot app in Safari.

## Install Nextflow

Hydra has no Nextflow module yet, so install your own copy once. Run
these in a **terminal** on Hydra: either an SSH session to a login node,
or the Terminal tab in your RStudio Server session (bottom left, next to
Console).

``` bash
cd ~
module load tools/java/21.0.2
curl -s https://get.nextflow.io | bash
chmod +x nextflow
mkdir -p ~/bin
mv nextflow ~/bin/nextflow
echo 'export PATH="${HOME}/bin:${PATH}"' >> ~/.bashrc
source ~/.bashrc
```

You must load the Java module (`module load tools/java/21.0.2`) whenever
you use Nextflow from a shell or a job script.

## Set up the R session

Everything from here on runs in **R**, in the Console of your RStudio
Server session.

An RStudio Server session on Hydra starts with a stripped `PATH` that
leaves out the job scheduler, Java, and your `~/bin`, so Nextflow cannot
find `qsub` and job submission fails.
[`hydra_setup()`](https://smithsonian.github.io/MitoPilot/reference/hydra_setup.md)
fixes that. Call it once per session, right after loading the package
and before creating a project or opening the app:

``` r

library(MitoPilot)
hydra_setup()
```

It puts the Hydra Java and job scheduler directories at the front of
your `PATH`, checks your Nextflow version, and pins a compatible one for
the session. It warns and changes nothing if you are not on Hydra, so it
is safe to leave in a script you also run elsewhere.

## Create projects with the Hydra executor

``` r

new_test_project(
  path = "/pool/public/genomics/<<USER>>/MitoPilot/test_project",
  executor = "NMNH_Hydra"
)
```

Use the same `executor = "NMNH_Hydra"` with
[`new_project()`](https://smithsonian.github.io/MitoPilot/reference/new_project.md)
for your own data. Work under `/pool` or `/scratch`. Home directories
are too small for pipeline work directories, and projects cannot be run
from `/store`.

The full walkthrough, with Hydra-specific notes at each step, is on the
[Get
Started](https://smithsonian.github.io/MitoPilot/articles/MitoPilot.md)
page.

## Running jobs

On Hydra the app offers **Submit as Job** alongside **Run from App**,
which submits the workflow to the cluster for you. Monitor it by running
`qstat` in a terminal and reading the log files, then reopen the app
when it finishes. For large sample sets this is the right choice;
running from the app ties the pipeline to your session.

The generic mechanics of job scripts, resources, and the SSH tunnel are
covered in [HPC cluster
support](https://smithsonian.github.io/MitoPilot/articles/Custom-HPC.md).

**Tip.** Clear your Singularity cache with `singularity cache clean`
after updating MitoPilot, so the matching container image is pulled.
