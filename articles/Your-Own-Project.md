# Starting Your Own Project

Once you are comfortable with how MitoPilot works, starting your own
project is mostly a matter of pointing MitoPilot at your sequence data
and telling it what kind of organism you are working on.

### What you need

- **A directory of paired-end Illumina reads.** Two gzipped FASTQ files
  per sample. The data directory does not have to be inside the project
  directory.
- **A mapping file.** A CSV describing your samples.

### The mapping file

Four columns are required:

| Column | Contents |
|----|----|
| `ID` | Unique identifier for the sample. Used as the SeqID at export, so keep it short and free of spaces. |
| `Taxon` | Taxonomic information. Only included for your own benefit, so no required format. |
| `R1` | File name of the forward reads (name only, not a path). |
| `R2` | File name of the reverse reads (name only, not a path). |

Any other columns you add are carried along and can be pulled into
GenBank FASTA headers at export, so this is the place to put voucher
numbers, BioSample accessions, collection data, and anything else your
submission needs.

    ID,Taxon,R1,R2,Voucher,BioSample
    OCT001,Muricea elongata,OCT001_R1.fastq.gz,OCT001_R2.fastq.gz,USNM:1234567,SAMN00000001
    OCT002,Leptogorgia virgulata,OCT002_R1.fastq.gz,OCT002_R2.fastq.gz,USNM:1234568,SAMN00000002

You do not have to get the mapping file columns right the first time.
[`update_sample_metadata()`](https://smithsonian.github.io/MitoPilot/reference/update_sample_metadata.md)
adds new columns or revises the values in existing projects. Close the
app before updating the metadata, since this function needs sole access
to the project database.

**Note.** If your identifier column is not called `ID`, you can pass its
name with `mapping_id` instead of renaming the column.

### Initialize the project

``` r

library(MitoPilot)

new_project(
  path = "~/my_mitogenomes/run_01",
  mapping_fn = "~/my_mitogenomes/mapping.csv",
  data_path = "~/my_mitogenomes/raw_data",
  executor = "local"
)
```

That is the minimum. Everything below is optional, but for anything
other than a ray-finned fish you will want at least `curate_target` and
custom assembly databases.

Run from RStudio, this also creates and opens a new RStudio project. On
RStudio Server on a cluster you usually want `Rproj = FALSE` to stay in
your current session.

#### Arguments worth setting

Many of these options can also be set in the MitoPilot app. Setting them
at project initialization may save you time.

**`curate_target`** picks the curation and validation ruleset, which
controls expected gene content, allowed start and stop codons, gene
naming, and the genetic code. The default is `"fish_mito"`. Browse the
available rulesets in the [curation ruleset
browser](https://smithsonian.github.io/MitoPilot/articles/Ruleset-Browser.md)
and pass the one matching your clade, for example
`curate_target = "octocoral_mito"`.

**`genetic_code`** is normally left alone: MitoPilot takes it from the
curation ruleset. Set it only when you need to override that, using an
[NCBI translation table
number](https://www.ncbi.nlm.nih.gov/Taxonomy/Utils/wprintgc.cgi).

**`custom_seeds_db` and `custom_labels_db`** point GetOrganelle at
reference sequences for your group. The defaults are for fishes, but you
can build databases for any clade with the R function
[`custom_assembly_db()`](https://smithsonian.github.io/MitoPilot/reference/custom_assembly_db.md),
see [building custom
databases](https://smithsonian.github.io/MitoPilot/articles/custom_dbs.md).
Give absolute paths, not paths starting with `~`.

**`assembler` and `mitofinder_db`** switch the assembler to
`"MitoFinder"` and point it at a reference database. The reference
database must be GenBank format (`.gb`) and can be a local path or a
URL. The default is the zebrafish mitogenome, so supply your own for
anything that is not a fish. Leave both alone to stay on GetOrganelle.

**`annotate_ref_db`** selects the MITOS2 reference database. The default
is `"Chordata"`; `"Metazoa_RefSeq89"` is the general-purpose alternative
and is the right choice for most invertebrates.

**`min_depth`** is the minimum number of read pairs after pre-processing
for a sample to continue (default 2,000,000). Lower it if your reads
have already been filtered or baited.

**`linear_complete`** should be `TRUE` for taxa whose complete
mitogenome is genuinely linear, will cause export to label them
“complete genome” rather than “partial”.

**`ncbi_api_key`** raises your NCBI request limits. Worth setting even
though the BLAST search itself is local, because MitoPilot fetches
annotations and taxonomic lineage for each BLAST hit directly from NCBI.
Get an API key from
[NCBI](https://www.ncbi.nlm.nih.gov/datasets/docs/v2/api/api-keys/).

**`executor`** decides where the work runs: `"local"`, one of the
generic cluster templates (`"slurm"`, `"sge"`, `"pbs"`, `"lsf"`,
`"awsbatch"`), a site profile (`"NMNH_Hydra"`, `"NOAA_SEDNA"`), or a
profile you saved yourself with
[`generate_config()`](https://smithsonian.github.io/MitoPilot/reference/generate_config.md).
See [HPC cluster
support](https://smithsonian.github.io/MitoPilot/articles/Custom-HPC.md).

**Processing parameters** for the pipeline modules can also be set here,
not just in the app. Anything you pass to
[`new_project()`](https://smithsonian.github.io/MitoPilot/reference/new_project.md)
overrides the stored default for every sample in the new project, for
example:

``` r

new_project(
  mapping_fn = "path/to/mapping_file.csv",
  executor = "local",
  assemble_memory = 24,
  getOrganelle = "-F 'anonym' -R 20 -k '21,45,65,85,105,115' -J 1 -M 1 --expected-max-size 20000 --target-genome-size 16500"
)
```

For the complete list of parameters that can be set at initialization,
see the
[`new_db()`](https://smithsonian.github.io/MitoPilot/reference/new_db.md)
documentation.

Below is a more complete `new_project` example for an octocoral dataset
on a SLURM cluster:

``` r

new_project(
  path = "~/octocorals/run_01",
  mapping_fn = "~/octocorals/mapping.csv",
  data_path = "~/octocorals/raw_data",
  executor = "slurm",
  curate_target = "octocoral_mito",
  annotate_ref_db = "Metazoa_RefSeq89",
  custom_seeds_db = "/data/refs/octocoral_seeds.fasta",
  custom_labels_db = "/data/refs/octocoral_labels.fasta",
  min_depth = 500000,
  ncbi_api_key = "YOUR_KEY"
)
```

After setting up the project, open the app from the project directory
and work through Assemble, Annotate, and Export exactly as in the test
project:

``` r

setwd("~/octocorals/run_01")
MitoPilot()
```

**Tip.** Run a handful of samples through the whole pipeline before
analyzing hundreds. Curation settings that are wrong for your clade are
much cheaper to discover on a small test run.

HYDRA **A Hydra project in full.** Keep the data and the project on
`/pool` or `/scratch` (not `/store`), and call
[`hydra_setup()`](https://smithsonian.github.io/MitoPilot/reference/hydra_setup.md)
before anything else in the session:

``` r
library(MitoPilot)
hydra_setup()

new_project(
  path = "/pool/public/genomics/<<USER>>/octocorals/run_01",
  mapping_fn = "/pool/public/genomics/<<USER>>/octocorals/mapping.csv",
  data_path = "/pool/public/genomics/<<USER>>/octocorals/raw_data",
  executor = "NMNH_Hydra",
  curate_target = "octocoral_mito",
  ncbi_api_key = "YOUR_KEY"
)
```

Reference databases you pass to the `new_project` function must be
readable from the compute nodes. Best to put them on shared storage
rather than in your home directory.

### Adding samples later

[`add_samples()`](https://smithsonian.github.io/MitoPilot/reference/add_samples.md)
appends new rows to an existing project from an additional mapping file,
so a project can grow as sequencing comes in rather than being
re-created from scratch.
[`update_sample_metadata()`](https://smithsonian.github.io/MitoPilot/reference/update_sample_metadata.md)
revises or adds metadata columns, and
[`update_sample_seqdata()`](https://smithsonian.github.io/MitoPilot/reference/update_sample_seqdata.md)
updates the raw-data file names.

Close the app before using any of them, since they need sole access to
the project database. Each one backs the database up first, so you can
revert.

### Using pre-existing mitogenome assemblies

If your mitogenomes were assembled elsewhere, for example by mapping to
a reference in Geneious, use
[`new_project_userAsmb()`](https://smithsonian.github.io/MitoPilot/reference/new_project_userAsmb.md)
instead. MitoPilot will skip assembly and let you perform annotation,
curation, and export.

It needs two extra columns in the mapping file:

| Column | Contents |
|----|----|
| `Assembly` | File name of the assembly FASTA for the sample (one file per sample) |
| `Topology` | `circular` or `linear` |

``` r

new_project_userAsmb(
  path = "~/my_assemblies/run_01",
  mapping_fn = "~/my_assemblies/mapping.csv",
  data_path = "~/my_assemblies/raw_data",
  assembly_path = "~/my_assemblies/assemblies",
  executor = "local"
)
```

You still run the Assemble module, but it only maps reads to your
assemblies to compute coverage and error rates.

If you have no reads at all, set `no_raw_data = TRUE` and omit
`data_path`. MitoPilot will skip read mapping entirely and hide the
coverage columns that depend on it.

**Warning.** A project holds either MitoPilot assemblies or externally
supplied ones, never both. Use separate projects if you have a mix.

------------------------------------------------------------------------

## Test project walkthrough

If you have not run the test project yet, it is the fastest way to learn
the interface before committing your own data:

[1.
Assemble](https://smithsonian.github.io/MitoPilot/articles/Test-Project-Assemble.md)
[2.
Annotate](https://smithsonian.github.io/MitoPilot/articles/Test-Project-Annotate.md)
[3.
Export](https://smithsonian.github.io/MitoPilot/articles/Test-Project-Export.md)
