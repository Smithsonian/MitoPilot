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
| `ID` | Unique identifier for the sample. Used as the SeqID at export, so at most 40 characters, using only letters, digits, dashes, underscores, and colons. |
| `Taxon` | Taxonomic information. Only included for your own benefit, so no required format. |
| `R1` | File name of the forward reads (name only, not a path). |
| `R2` | File name of the reverse reads (name only, not a path). |

Any other columns you add are carried along: they show in the Export
table (the **Metadata** entry in its Columns picker hides or shows them
together) and can be pulled into GenBank FASTA headers at export, so
this is the place to put voucher numbers, BioSample accessions,
collection data, and anything else your submission needs.

    ID,Taxon,R1,R2,Voucher,BioSample
    OCT001,Muricea elongata,OCT001_R1.fastq.gz,OCT001_R2.fastq.gz,USNM:1234567,SAMN00000001
    OCT002,Leptogorgia virgulata,OCT002_R1.fastq.gz,OCT002_R2.fastq.gz,USNM:1234568,SAMN00000002

Two column names are reserved for the MapToRef assembler, which maps
reads to a reference mitogenome chosen per sample (see [choosing an
assembly
method](https://smithsonian.github.io/MitoPilot/articles/Assembly-Methods.md)).
`Reference` holds a file path, a URL, or an NCBI nucleotide accession
(MitoPilot will automatically download when needed) A FASTA reference
also needs a `Reference_topology` column (`circular` or `linear`), since
a FASTA header carries no topology.

    ID,Taxon,R1,R2,Reference,Reference_topology
    OCT001,Muricea elongata,OCT001_R1.fastq.gz,OCT001_R2.fastq.gz,ref/NC_002333.gb,
    OCT002,Leptogorgia virgulata,OCT002_R1.fastq.gz,OCT002_R2.fastq.gz,ref/mito.fasta,circular

If your reference list is not ready at project creation, leave the
columns out and supply them later with
[`set_maptoref_refs()`](https://smithsonian.github.io/MitoPilot/reference/set_maptoref_refs.md),
using a CSV with an ID column, a reference column, and an optional
topology column.

``` r

set_maptoref_refs(refs = "my_refs.csv")   # ID, reference, and optionally topology
```

Or you can set references using the **MapToRef Ref** cell in the app’s
Assemble table.

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

Many of these can also be changed later in the MitoPilot app, but
setting them at initialization applies them to every sample at once.

##### Taxonomy and curation

**`curate_target`** picks the curation and validation ruleset, which
controls expected gene content, allowed start and stop codons, gene
naming, and the genetic code. The default is `"fish_mito"`. Browse the
available rulesets in the [curation ruleset
browser](https://smithsonian.github.io/MitoPilot/articles/Ruleset-Browser.md)
and pass the one matching your clade, for example
`curate_target = "octocoral_mito"`.

**`annotate_ref_db`** selects the MITOS2 reference database. The default
is `"Chordata"`; `"Metazoa_RefSeq89"` is the general-purpose alternative
and is the right choice for most invertebrates.

**`linear_complete`** should be `TRUE` for taxa whose complete
mitogenome is genuinely linear, so that export labels them “complete
genome” rather than “partial”.

##### Assembly

**`assembler`** chooses `"GetOrganelle"` (default), `"MitoFinder"`, or
`"MapToRef"`. See [choosing an assembly
method](https://smithsonian.github.io/MitoPilot/articles/Assembly-Methods.md)
for more info. MapToRef sets the reference per sample through the
mapping file, as described [above](#the-mapping-file), so it has no
database argument here.

**`custom_seeds_db` and `custom_labels_db`** point GetOrganelle at
reference sequences for your group. The defaults are for fishes, but you
can build databases for any clade with
[`custom_assembly_db()`](https://smithsonian.github.io/MitoPilot/reference/custom_assembly_db.md),
see [building custom
databases](https://smithsonian.github.io/MitoPilot/articles/custom_dbs.md).
Give absolute paths, not paths starting with `~`.

**`mitofinder_db`** points MitoFinder at a GenBank-format (`.gb`)
reference database, given as a local path or a URL. The default is the
zebrafish mitogenome, so supply your own for anything that is not a
fish.

##### Data and compute

**`min_depth`** is the minimum number of read pairs after pre-processing
for a sample to continue (default 2,000,000). Lower it if your reads
have already been filtered or baited.

**`executor`** decides where the work runs: `"local"`, one of the
generic cluster templates (`"slurm"`, `"sge"`, `"pbs"`, `"lsf"`,
`"awsbatch"`), a site profile (`"NMNH_Hydra"`, `"NOAA_SEDNA"`), or a
profile you saved yourself with
[`generate_config()`](https://smithsonian.github.io/MitoPilot/reference/generate_config.md).
See [HPC cluster
support](https://smithsonian.github.io/MitoPilot/articles/Custom-HPC.md).

**`ncbi_api_key`** raises your NCBI request limits. Worth setting even
though the BLAST search itself is local, because MitoPilot fetches
annotations and taxonomic lineage for each BLAST hit directly from NCBI.
Get an API key from
[NCBI](https://www.ncbi.nlm.nih.gov/datasets/docs/v2/api/api-keys/).

##### Any other pipeline parameter

Every processing parameter shown in the app’s options windows can also
be passed to
[`new_project()`](https://smithsonian.github.io/MitoPilot/reference/new_project.md),
which overrides the stored default for every sample in the new project:

``` r

new_project(
  mapping_fn = "path/to/mapping_file.csv",
  executor = "local",
  assemble_memory = 24,
  getOrganelle = "-F 'anonym' -R 20 -k '21,45,65,85,105,115' -J 1 -M 1 --expected-max-size 20000 --target-genome-size 16500"
)
```

For the complete list, see the
[`new_db()`](https://smithsonian.github.io/MitoPilot/reference/new_db.md)
documentation.

#### A complete example

An octocoral dataset on a SLURM cluster:

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

[Using Your Own
Assemblies](https://smithsonian.github.io/MitoPilot/articles/Your-Own-Assemblies.md)
covers the extra mapping columns, projects with no raw reads, and the
optional steps for whole-genome assemblies: finding the mitochondrial
contigs, circularizing linear ones, and joining a mitogenome that
arrived in several pieces.

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
