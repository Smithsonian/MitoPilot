# Initialize a new project database

Initialize a new project database

## Usage

``` r
new_db_userAsmb(
  db_path = "./.sqlite",
  mapping_fn = NULL,
  mapping_id = "ID",
  mapping_taxon = "Taxon",
  genetic_code = 2,
  annotate_cpus = 6,
  annotate_memory = 36,
  annotate_ref_db = "Chordata",
  annotate_ref_dir =
    "https://raw.githubusercontent.com/Smithsonian/MitoPilot/refs/heads/main/ref_dbs/Mitos2",
  mitos_opts = "--intron 0 --oril 0",
  trnaScan_opts = "-M vert -X 20",
  arwen_opts = "-mtx",
  aragorn_opts = "-m -gcstd",
  curate_cpus = 4,
  curate_memory = 8,
  curate_target = "fish_mito",
  max_blast_hits = 100,
  curate_params = NULL
)
```

## Arguments

- db_path:

  Path to the new database file

- mapping_fn:

  Path to the mapping CSV file. Must contain columns "ID", "Taxon, "R1",
  "R2", "Assembly", and "Topology"

- mapping_id:

  Column name of the mapping file to use as the primary key

- mapping_taxon:

  Column name of the mapping file containing a Taxonomic identifier (eg,
  species name)

- genetic_code:

  Translation table for your organisms. See NCBI website for more info
  https://www.ncbi.nlm.nih.gov/Taxonomy/Utils/wprintgc.cgi

- annotate_cpus:

  Default \# cpus for annotation

- annotate_memory:

  Default memory (GB) for annotation

- annotate_ref_db:

  Default Mitos2 reference database

- annotate_ref_dir:

  Default Mitos2 reference database directory

- mitos_opts:

  Default MITOS2 command line options

- trnaScan_opts:

  Default tRNAscan-SE command line options

- arwen_opts:

  Default ARWEN command line options

- aragorn_opts:

  Default ARAGORN command line options

- curate_cpus:

  Default \# cpus for curation

- curate_memory:

  Default memory (GB) for curation

- curate_target:

  Default target database for curation

- max_blast_hits:

  Maximum number of top BLAST hits to retain (default = 100)

- curate_params:

  Default curation parameters
