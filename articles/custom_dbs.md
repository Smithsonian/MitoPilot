# Building Custom Databases

## Why use a custom reference database?

Currently, MitoPilot comes packaged with assembly reference databases
for fishes. If you are working on any other taxonomic group, you will
need to compile databases of mitochondrial sequences for your clade. It
may also be helpful to create a custom database for curation of the
protein-coding gene predictions if your taxa are poorly represented in
NCBI RefSeq.

## What parts of the MitoPilot pipeline use reference databases?

- GetOrganelle or MitoFinder (Assemble module)
- Mitos2 (Annotate module)
- Automatic and manual curation of protein-coding genes (Annotate
  module)

## Build custom databases for GetOrganelle

Before proceeding, consider reviewing the GetOrganelle
[paper](https://doi.org/10.1186/s13059-020-02154-5) and
[documentation](https://github.com/Kinggerm/GetOrganelle/wiki/FAQ#how-to-assemble-a-target-organelle-genome-using-my-own-reference)
to better understand the required database architecture.

GetOrganelle uses two databases, both in FASTA format:

- A “seed” database containing complete (or partial) mitochondrial
  genomes
- A “label” database containing individual mitochondrial gene sequences

### Recommended: `MitoPilot::custom_assembly_db()`

The easiest way to build custom assembly databases is the
[`MitoPilot::custom_assembly_db()`](https://smithsonian.github.io/MitoPilot/reference/custom_assembly_db.md)
function. It queries NCBI GenBank for all mitochondrial records in your
clade, downloads them, and builds the GetOrganelle “seed” and “label”
databases (and/or a MitoFinder database) for you.

``` r

library(MitoPilot)

# Build both GetOrganelle and MitoFinder databases for starfish,
# using all GenBank mitogenomes (not just RefSeq).
custom_assembly_db(
  clade = "Asteroidea",
  db_path = "~/MitoPilot_reference_dbs", # store outside your project directories for reuse
  db_type = "both"
)
```

Key arguments:

- `clade` - taxon name; validated against NCBI taxonomy (an invalid name
  returns an error).
- `db_path` - directory for the databases. Use a location **outside**
  your MitoPilot project directories so the databases can be reused
  across projects.
- `db_type` - `"getorganelle"`, `"mitofinder"`, or `"both"` (default).
- `refseq_only` - set `TRUE` to restrict to RefSeq mitogenomes (default
  `FALSE` = all mitogenomes).
- `search_terms` - optional extra [advanced GenBank
  query](https://www.ncbi.nlm.nih.gov/nuccore/advanced) terms, combined
  with the clade via `AND`, e.g. `'"PRJNA720393"[BioProject]'`. These
  are validated before any download.
- `retain_genbank` - keep the raw `genbank.gb` file for
  GetOrganelle-only builds.

Databases are written to a dated, clade-named sub-directory of
`db_path`, for example `Asteroidea_all_2026-06-11/`, containing:

- `getorganelle_seed.fasta` - the GetOrganelle seed database
- `getorganelle_label.fasta` - the GetOrganelle label database
- `mitofinder_<clade>_<source>_<date>.gb` - the MitoFinder database (if
  requested)
- `README.txt` and `manifest.json` - the full query, NCBI taxid, record
  counts, and the date/time GenBank was accessed

When finished, the function will print instructions for how to use the
new database with MitoPilot,

You can provide the path to your GetOrganelle databases with the
`custom_seeds_db` and `custom_labels_db` arguments of
[`MitoPilot::new_project`](https://smithsonian.github.io/MitoPilot/reference/new_project.md)
function when initializing a project. Alternatively, you can specify the
GetOrganelle database in the assembly options section of the MitoPilot
GUI.

**Note on un-annotated sequences:** Some mitochondrial records have no
annotated genes (e.g. D-loop or poorly annotated sequences). Rather than
requiring manual review,
[`custom_assembly_db()`](https://smithsonian.github.io/MitoPilot/reference/custom_assembly_db.md)
automatically adds such a sequence to the seed database if it is long
enough to likely be a near-complete mitogenome (by default, at least
0.8x the median length of the complete mitogenomes in the download).
Tune this with `include_nogene` and `nogene_min_frac`.

### Inspecting custom databases

Here are a few helpful one-liners to inspect and manipulate FASTA files.

***Count the number of sequences in a FASTA file:***

    grep -c ">" getorganelle_label.fasta

***Generate list of FASTA headers:***

    grep ">" getorganelle_label.fasta

***Generate list of unique gene names:***

    grep ">" getorganelle_label.fasta | cut -f1 -d" " | sort | uniq

***Calculate sequence lengths:***

    cat my_file.fasta | awk '$0 ~ ">" {if (NR > 1) {print c;} c=0;printf substr($0,2,100) "\t"; } $0 !~ ">" {c+=length($0);} END { print c; }'

***Extract specific sequences from a FASTA file with
[seqkit](https://bioinf.shenwei.me/seqkit/):***

    # Extract sequences based on names.txt
    # names.txt should contain full sequences headers, one per line, but no ">" at start
    module load bio/seqkit/2.8.1 # ONLY FOR SMITHSONIAN HYDRA CLUSTER
    seqkit grep -f -n names.txt file.fasta > file_subset.fasta

***Remove sequences from a FASTA file with
[seqkit](https://bioinf.shenwei.me/seqkit/):***

    # Remove sequences based on name patterns listed in names.txt
    # one pattern per line
    module load bio/seqkit/2.8.1 # ONLY FOR SMITHSONIAN HYDRA CLUSTER
    seqkit grep -v -f names.txt file.fasta > file_subset.fasta

### Adding your own sequences to a custom database

To use unpublished mitogenomes in your custom GetOrganelle seed
database, you will need to combine multiple FASTA files. GetOrganelle
does not require any specific format for the sequence names in the seed
database.

You can easily combine FASTA files with the Linux `cat` command:

    cat getorganelle_seed.fasta my_mitogenomes.fasta more_mitogenomes.fasta > final_seed_db.fasta

You could also add unpublished individual gene sequences to a custom
GetOrganelle label database in a similar manner.

## Build custom databases for MitoFinder

The [MitoFinder
documentation](https://github.com/RemiAllio/MitoFinder?tab=readme-ov-file#how-to-get-reference-mitochondrial-genomes-from-ncbi)
has instructions on how to build a reference database.

The MitoFinder reference database is simple: a GenBank formatted file
(`.gb`) containing one or more annotated mitogenomes. The easiest way to
build one is
[`MitoPilot::custom_assembly_db()`](https://smithsonian.github.io/MitoPilot/reference/custom_assembly_db.md)
with `db_type = "mitofinder"` (see
[above](#recommended-mitopilotcustom_assembly_db)). Alternatively, this
file can be downloaded from a GenBank query in a web browser.

You can provide the path to your MitoFinder database with the
`mitofinder_db` argument of
[`MitoPilot::new_project`](https://smithsonian.github.io/MitoPilot/reference/new_project.md)
function when initializing a project. Alternatively, you can specify the
MitoFinder database in the assembly options section of the MitoPilot
GUI.

Assembly of contigs with MitoFinder is completely de novo. The
MitoFinder reference database is only used to “label” putative
mitochondrial contigs. Thus, the species in your reference database can
be fairly distant relatives of your samples.

## Build custom databases for protein-coding gene curation

By default, MitoPilot uses gene sequences from NCBI RefSeq to finetune
start and stop codon positions for your annotations. The curation
database is specified by the `ref_dir` and `ref_db` sections of the
Curation Opt. panel in the Shiny app.

We have provided a helper function
[`MitoPilot::custom_curation_db`](https://smithsonian.github.io/MitoPilot/reference/custom_curation_db.md),
which will allow you to supplement the RefSeq databases with your own
gene sequences. Using a custom database can greatly improve automatic
curation if your focal clade is poorly represented in RefSeq.

Sequences can come from non-RefSeq mitogenomes on GenBank or from your
own prior annotations. Make sure to carefully consider what you are
adding to the custom database. You should only use high-confidence
sequences, as poor quality reference data will result in poorly curated
gene models.

Please see the
[`MitoPilot::custom_curation_db`](https://smithsonian.github.io/MitoPilot/reference/custom_curation_db.md)
documentation for further instructions.
