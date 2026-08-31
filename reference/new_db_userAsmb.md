# Initialize a new project database

Initialize a new project database

## Usage

``` r
new_db_userAsmb(
  db_path = "./.sqlite",
  mapping_fn = NULL,
  mapping_id = "ID",
  mapping_taxon = "Taxon",
  assembly_path = NULL,
  genetic_code = NULL,
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
  curate_ref_db = "Metazoa_RefSeq235",
  max_blast_hits = 10,
  linear_complete = FALSE,
  curate_params = NULL,
  orf_cpus = 4,
  orf_memory = 8,
  orffinder_opts = "-s 1",
  orf_min_len = 300,
  orf_max_overlap = 0.1,
  min_assembly_length = 500,
  join_scaffolds = FALSE,
  find_mitogenome = FALSE,
  mitofinder_db = NULL,
  find_min_contig_length = 500,
  find_min_identity = 70,
  find_min_aligned_length = 300,
  find_min_aligned_fraction = 0.5,
  find_max_candidates = 20,
  find_min_genes = 3,
  find_cpus = 4,
  find_memory = 8,
  attempt_circularization = FALSE,
  circularize_min_overlap = 220,
  circularize_min_identity = 99,
  circularize_min_junction_reads = 5,
  circularize_min_overhang = 30,
  circularize_cpus = 4,
  circularize_memory = 8,
  no_raw_data = FALSE
)
```

## Arguments

- db_path:

  Path to the new database file

- mapping_fn:

  Path to the mapping CSV file. Must contain columns "ID", "Taxon", and
  "Assembly", plus "R1" and "R2" unless \`no_raw_data = TRUE\`. An
  optional "Topology" column may declare "circular" or "linear" for a
  single-contig assembly; blank or missing is treated as linear, and a
  multi-contig assembly is always recorded as "multi".

- mapping_id:

  Column name of the mapping file to use as the primary key

- mapping_taxon:

  Column name of the mapping file containing a Taxonomic identifier (eg,
  species name)

- assembly_path:

  Directory holding the user-supplied assembly files. Used to count each
  assembly's contigs so a multi-contig assembly is recorded with
  topology "multi".

- genetic_code:

  Optional NCBI translation table override. Default \`NULL\`
  auto-selects from the curation ruleset; a number sets an override on
  the default curate_opts set.
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

- curate_ref_db:

  Default curation reference database (default = "Metazoa_RefSeq235",
  the only bundled DB with rRNA BLAST references)

- max_blast_hits:

  Maximum number of top BLAST hits to retain (default = 10)

- linear_complete:

  Treat linear assemblies as complete genomes for the export
  "completeness" field? By default only circular assemblies are labeled
  "complete genome" and linear assemblies "partial genome". Set TRUE for
  taxa whose complete mitogenome is genuinely linear (default = FALSE).
  Editable later in the curation-options modal.

- curate_params:

  Default curation parameters

- orf_cpus:

  CPUs for the optional ORF-finder step (default = 4)

- orf_memory:

  Memory (GB) for the optional ORF-finder step (default = 8)

- orffinder_opts:

  Default NCBI ORFfinder options (default = "-s 1")

- orf_min_len:

  Minimal ORF length in nucleotides (default = 300)

- orf_max_overlap:

  Maximum overlap with existing annotations, as a fraction of the ORF
  length, before an ORF is discarded (default = 0.1)

- min_assembly_length:

  Minimum scaffold length to include in analysis (default = 500)

- join_scaffolds:

  (logical) Order a fragmented assembly against its BLAST reference into
  one joined sequence during WF1 (default = FALSE). Samples whose
  contigs match different reference mitogenomes are left alone. So is a
  sample with a junction the reference cannot size, since NCBI expects
  the number of Ns to be the estimated gap length.

- find_mitogenome:

  Search each user-supplied assembly for its mitochondrial contigs
  before the rest of WF1 runs (default = FALSE). See \[find_mito()\].

- mitofinder_db:

  Path to a MitoFinder GenBank database, built with
  \[custom_assembly_db()\] (\`db_type = "mitofinder"\`). Required when
  \`find_mitogenome = TRUE\`.

- find_min_contig_length:

  Contigs shorter than this are never searched, bp (default = 500)

- find_min_identity:

  Percent identity required against the reference (default = 70)

- find_min_aligned_length:

  Aligned bases required (default = 300)

- find_min_aligned_fraction:

  Fraction of the contig the alignment must cover (default = 0.5). The
  NUMT filter.

- find_max_candidates:

  Most contigs carried into MitoFinder confirmation (default = 20)

- find_min_genes:

  Mitochondrial genes a contig must carry to be confirmed (default = 3)

- find_cpus:

  Default \# cpus for the search steps (default = 4)

- find_memory:

  Default memory (GB) for the search steps (default = 8)

- attempt_circularization:

  Try to circularize user assemblies in WF1 (default = FALSE). Each
  contig is attempted on its own, so a fragmented assembly is eligible;
  a single-contig assembly declared "circular" is skipped. See
  \[circularize_asmb()\].

- circularize_min_overlap:

  Shortest accepted self-overlap, bp (default = 220)

- circularize_min_identity:

  Percent identity required for the self-overlap (default = 99)

- circularize_min_junction_reads:

  Reads that must span the new junction before an assembly is called
  circular (default = 5). Ignored when the project has no raw data.

- circularize_min_overhang:

  Bases a read must extend past the junction on each side to count
  (default = 30)

- circularize_cpus:

  Default \# cpus for the circularization step (default = 4)

- circularize_memory:

  Default memory (GB) for the circularization step (default = 8)

- no_raw_data:

  (logical) Initialize a project with no raw reads (default = FALSE).
  When TRUE, annotation coverage trimming (\`coverage_trim\`) is
  disabled since no read-depth information is available.
