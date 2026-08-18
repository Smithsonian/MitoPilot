# MitoPilot

Please see the [documentation
website](https://smithsonian.github.io/MitoPilot/) for more details.

# Overview

MitoPilot is a package for the assembly and annotation of mitochondrial
genomes from genome skimming data. The core application consists of a
[Nextflow](https://www.nextflow.io/docs/latest/index.html) pipeline that
is wrapped in an R package, which includes an R-Shiny graphical
interface to monitor and interact with processing parameters and
outputs. Currently the pipeline expects paired-end Illumina reads as the
raw input and performs the following steps.

1.  Mitogenome assembly
    - [fastp](https://github.com/OpenGene/fastp) for quality control and
      adapter trimming
    - [GetOrganelle](https://github.com/Kinggerm/GetOrganelle) (default)
      or [MitoFinder](https://github.com/RemiAllio/MitoFinder) for
      mitogenome assembly
    - [bowtie2](https://github.com/BenLangmead/bowtie2) for read mapping
      to calculate coverage and error rates.
    - [NCBI BLAST](https://blast.ncbi.nlm.nih.gov/Blast.cgi) against a
      local database of all annotated metazoan mitogenomes in GenBank,
      packaged in the MitoPilot container, to find the closest reference
      for automatic and manual curation
2.  Mitogenome annotation
    - [MITOS2](https://gitlab.com/Bernt/MITOS) for rRNA, PCG, and tRNA
      annotation
    - [tRNAscan-SE](https://github.com/UCSC-LoweLab/tRNAscan-SE) for
      tRNA annotation
    - [MitoFinder](https://github.com/RemiAllio/MitoFinder) for rRNA and
      PCG annotation (optional)
    - [ARWEN](https://doi.org/10.1093/bioinformatics/btm573) for tRNA
      annotation (optional)
    - [ARAGORN](https://doi.org/10.1093/nar/gkh152) for tRNA annotation
      (optional)
    - [ORFfinder](https://www.ncbi.nlm.nih.gov/orffinder/) identify
      additional open reading frames (ORFs) (optional)
    - Custom scripts for gene boundary refinement and annotation file
      formatting
    - Validation to flag possible issues or known errors that would be
      rejected by NCBI GenBank
    - Manual curation of annotations using the integrated Shiny App
3.  Data export
    - Custom scripts to export data in a format suitable for submission
      to NCBI GenBank

Optionally, MitoPilot can proceed straight to annotation and curation if
the user supplies mitogenome assemblies with the
[`new_project_userAsmb()`](https://smithsonian.github.io/MitoPilot/reference/new_project_userAsmb.md)
function.

![](reference/figures/workflow_overview.png)

# Installation

MitoPilot needs R (\>= 4.4.0), Java 17+, Nextflow (24.10.x - 25.10.x),
and a container runtime (Docker locally, or Singularity/Apptainer on a
cluster).

``` r

if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager")
}
BiocManager::install("Smithsonian/MitoPilot")
```

See [Installation and
Requirements](https://smithsonian.github.io/MitoPilot/articles/Installation.html)
for the full requirements, disk space, updating and version pinning, and
container cache setup. Cluster-specific instructions are available for
[Smithsonian
Hydra](https://smithsonian.github.io/MitoPilot/articles/NMNH-Hydra.html)
and [NOAA
SEDNA](https://smithsonian.github.io/MitoPilot/articles/NOAA-SEDNA.html);
for any other cluster see [HPC cluster
support](https://smithsonian.github.io/MitoPilot/articles/Custom-HPC.html).

# Quick start

MitoPilot ships a small pre-filtered test dataset. Running it end to end
is the recommended way to verify your installation and learn the
interface before using your own data.

The [Get
Started](https://smithsonian.github.io/MitoPilot/articles/MitoPilot.html)
tutorial walks the whole pipeline using this test project.

Want to skip straight to using MitoPilot with your own data? Head on
over to [Starting Your Own
Project](https://smithsonian.github.io/MitoPilot/articles/Your-Own-Project.html).

# Taxonomic Scope

MitoPilot was initially built for fish mitogenomes, but It has since
been extended with curation and validation rulesets for the groups
below.

| Clade | Common name | curate_target | Status |
|----|----|----|----|
| [Actinopterygii](https://www.ncbi.nlm.nih.gov/datasets/taxonomy/7898/) | Ray-finned fishes | `fish_mito` | Tested |
| [Annelida](https://www.ncbi.nlm.nih.gov/datasets/taxonomy/6340/) | Annelids | `annelid_mito` | Testing in progress |
| [Ascidiacea](https://www.ncbi.nlm.nih.gov/datasets/taxonomy/7713/) | Sea squirts | `ascidiacea_mito` | Untested |
| [Asteroidea](https://www.ncbi.nlm.nih.gov/datasets/taxonomy/7588/) | Sea stars | `starfish_mito` | Tested |
| [Aves](https://www.ncbi.nlm.nih.gov/datasets/taxonomy/8782/) | Birds | `bird_mito` | Untested |
| [Bivalvia](https://www.ncbi.nlm.nih.gov/datasets/taxonomy/6544/) | Bivalves | `bivalvia_mito` | Untested |
| [Bryozoa](https://www.ncbi.nlm.nih.gov/datasets/taxonomy/10205/) | Bryozoans | `bryozoa_mito` | Untested |
| [Copepoda](https://www.ncbi.nlm.nih.gov/datasets/taxonomy/6830/) | Copepods | `copepod_mito` | Testing in progress |
| [Crinoidea](https://www.ncbi.nlm.nih.gov/datasets/taxonomy/35069/) | Crinoids | `crinoidea_mito` | Untested |
| [Ctenophora](https://www.ncbi.nlm.nih.gov/datasets/taxonomy/10197/) | Ctenophores | `ctenophore_mito` | Testing in progress |
| [Demospongiae](https://www.ncbi.nlm.nih.gov/datasets/taxonomy/6042/) | Demosponges | `demospongiae_mito` | Untested |
| [Diptera](https://www.ncbi.nlm.nih.gov/datasets/taxonomy/7147/) | True flies | `diptera_mito` | Tested |
| [Echinoidea](https://www.ncbi.nlm.nih.gov/datasets/taxonomy/7625/) | Sea urchins | `echinoidea_mito` | Untested |
| [Gastropoda](https://www.ncbi.nlm.nih.gov/datasets/taxonomy/6448/) | Gastropods | `gastropoda_mito` | Testing in progress |
| [Hexacorallia](https://www.ncbi.nlm.nih.gov/datasets/taxonomy/6102/) | Hexacorals | `hexacoral_mito` | Tested |
| [Holothuroidea](https://www.ncbi.nlm.nih.gov/datasets/taxonomy/7705/) | Sea cucumbers | `holothuroidea_mito` | Untested |
| [Homoscleromorpha](https://www.ncbi.nlm.nih.gov/datasets/taxonomy/80999/) | Homoscleromorph sponges | `homoscleromorpha_mito` | Untested |
| [Hydrozoa](https://www.ncbi.nlm.nih.gov/datasets/taxonomy/6074/) | Hydrozoans | `hydrozoa_mito` | Testing in progress |
| [Lepidosauria](https://www.ncbi.nlm.nih.gov/datasets/taxonomy/8504/) | Lepidosaurs | `lepidosaur_mito` | Untested |
| [Malacostraca](https://www.ncbi.nlm.nih.gov/datasets/taxonomy/6681/) | Malacostracans | `malacostraca_mito` | Testing in progress |
| [Mammalia](https://www.ncbi.nlm.nih.gov/datasets/taxonomy/40674/) | Mammals | `mammal_mito` | Untested |
| [Nemertea](https://www.ncbi.nlm.nih.gov/datasets/taxonomy/6217/) | Ribbon worms | `nemertea_mito` | Testing in progress |
| [Octocorallia](https://www.ncbi.nlm.nih.gov/datasets/taxonomy/6132/) | Octocorals | `octocoral_mito` | Tested |
| [Ophiuroidea](https://www.ncbi.nlm.nih.gov/datasets/taxonomy/7618/) | Brittle stars | `ophiuroidea_mito` | Untested |
| [Platyhelminthes](https://www.ncbi.nlm.nih.gov/datasets/taxonomy/6157/) | Flatworms | `platyhelminthes_mito` | Untested |
| [Polychaeta](https://www.ncbi.nlm.nih.gov/datasets/taxonomy/6341/) | Polychaetes | `polychaeta_mito` | Testing in progress |
| [Pycnogonida](https://www.ncbi.nlm.nih.gov/datasets/taxonomy/57294/) | Sea spiders | `pycnogonida_mito` | Untested |
| [Scyphozoa](https://www.ncbi.nlm.nih.gov/datasets/taxonomy/6142/) | True jellyfishes | `scyphozoa_mito` | Testing in progress |
| [Sipuncula](https://www.ncbi.nlm.nih.gov/datasets/taxonomy/6433/) | Peanut worms | `sipuncula_mito` | Untested |
| [Testudines](https://www.ncbi.nlm.nih.gov/datasets/taxonomy/8459/) | Turtles | `turtle_mito` | Tested |
| [Thaliacea](https://www.ncbi.nlm.nih.gov/datasets/taxonomy/30304/) | Salps | `thaliacea_mito` | Testing in progress |
| [Thecostraca](https://www.ncbi.nlm.nih.gov/datasets/taxonomy/116172/) | Barnacles | `thecostraca_mito` | Untested |

See the [curation ruleset
browser](https://smithsonian.github.io/MitoPilot/articles/Ruleset-Browser.html)
for details about each curation ruleset. The curation ruleset can be set
individually for each sample in the `Curate Opts.` window in the
MitoPilot app.

All curation rulesets ship inside the Docker image
([macguigand/MitoPilot](https://hub.docker.com/repository/docker/macguigand/mitopilot)).
If MitoPilot doesn’t have a curation ruleset for your taxonomic group,
please open an [issue](https://github.com/Smithsonian/MitoPilot/issues)
or contact Dan MacGuigan at <macguigand@si.edu>.

For groups other than fishes, make sure you build or pick the
appropriate reference databases. There are three independent kinds of
databases:

- **Assembly** references for GetOrganelle or MitoFinder.
  [`MitoPilot::custom_assembly_db()`](https://smithsonian.github.io/MitoPilot/reference/custom_assembly_db.md)
  builds these for a clade automatically, with no external tools
  required. See [building custom
  databases](https://smithsonian.github.io/MitoPilot/articles/custom_dbs.html).
- **Annotation** references for MITOS2. MitoPilot includes Chordata and
  Metazoa databases, selectable in the `Annotate Opts.` window.
- **Curation** references, chosen independently of the annotation
  database. Bundled options are `Metazoa_RefSeq235` (the default),
  `Metazoa_RefSeq231`, `Metazoa_RefSeq89`, and `Chordata`. MitoPilot
  also folds each sample’s assembly BLAST results into the curation
  references automatically.

# Documentation

| Page | What it covers |
|----|----|
| [Get Started](https://smithsonian.github.io/MitoPilot/articles/MitoPilot.html) | Full walkthrough, then starting your own project |
| [Installation and Requirements](https://smithsonian.github.io/MitoPilot/articles/Installation.html) | Prerequisites, installing, updating, container cache |
| [HPC cluster support](https://smithsonian.github.io/MitoPilot/articles/Custom-HPC.html) | Executors, cluster profiles, SSH tunnel, submitting runs |
| [Curation ruleset browser](https://smithsonian.github.io/MitoPilot/articles/Ruleset-Browser.html) | What each clade ruleset enforces |
| [Building custom databases](https://smithsonian.github.io/MitoPilot/articles/custom_dbs.html) | Assembly and curation reference databases |
| [Handling difficult assemblies](https://smithsonian.github.io/MitoPilot/articles/Difficult-Assemblies.html) | Resolving competing assemblies and fragmented scaffolds |
| [FAQ](https://smithsonian.github.io/MitoPilot/articles/FAQ.html) and [Troubleshooting](https://smithsonian.github.io/MitoPilot/articles/Troubleshooting.html) | Common questions and pipeline failures |
| [Reference](https://smithsonian.github.io/MitoPilot/reference/index.html) | All functions |
| [Changelog](https://smithsonian.github.io/MitoPilot/news/index.html) | Release notes and container tags |
