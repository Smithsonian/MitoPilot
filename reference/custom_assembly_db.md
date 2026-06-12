# Build custom GetOrganelle and/or MitoFinder reference databases

Downloads mitochondrial records from NCBI GenBank for a given clade and
builds reference databases for the MitoPilot Assemble module. Replaces
the external Entrez Direct / python / biopython workflow with a pure-R
implementation, so no external command-line tools are required.

## Usage

``` r
custom_assembly_db(
  clade,
  db_path,
  db_type = c("both", "getorganelle", "mitofinder"),
  refseq_only = FALSE,
  mito_only = TRUE,
  genomic_only = TRUE,
  search_terms = NULL,
  include_nogene = TRUE,
  nogene_min_frac = 0.8,
  nogene_min_length = 12000,
  drop_no_product = TRUE,
  retain_genbank = FALSE,
  overwrite = FALSE,
  api_key = NULL
)
```

## Arguments

- clade:

  Clade (taxon) name, validated against NCBI taxonomy.

- db_path:

  Directory in which to store the databases. Use a location outside your
  MitoPilot project directories so databases can be reused across
  projects.

- db_type:

  Which database(s) to build: "both" (default), "getorganelle", or
  "mitofinder".

- refseq_only:

  If TRUE, restrict to RefSeq mitogenomes. Default FALSE (all
  mitogenomes).

- mito_only:

  If TRUE (default), restrict to records flagged as mitochondrial in
  origin (NCBI \`mitochondrion\[filter\]\`). Excludes nuclear records
  that merely mention "mitochondrial" (e.g. nuclear-encoded mito genes,
  NUMTs). Set FALSE to broaden recall.

- genomic_only:

  If TRUE (default), restrict to genomic DNA (NCBI
  \`biomol_genomic\[PROP\]\`), excluding mRNA/rRNA/tRNA/cRNA records.
  Set FALSE to broaden recall.

- search_terms:

  Optional additional NCBI advanced-query term(s) combined with the
  clade via AND, e.g. `'"PRJNA720393"[BioProject]'`. Validated before
  download.

- include_nogene:

  If TRUE (default), add unannotated mitochondrial sequences to the
  GetOrganelle seed database when long enough (see `nogene_min_frac`).

- nogene_min_frac:

  Keep an unannotated sequence if its length is at least this fraction
  of the median complete-mitogenome length. Default 0.8.

- nogene_min_length:

  Absolute minimum length (bp) used when no complete mitogenomes are
  available to derive a threshold. Default 12000.

- drop_no_product:

  If TRUE (default), drop single-gene sequences lacking a GenBank
  product (often poorly annotated) from the label database.

- retain_genbank:

  If TRUE, keep the raw genbank.gb file for GetOrganelle-only builds.
  Ignored when a MitoFinder database is built (the .gb file is the
  MitoFinder database).

- overwrite:

  If TRUE, replace the dated output directory if it already exists and
  is non-empty. If FALSE (default), an existing non-empty output
  directory is an error.

- api_key:

  Optional NCBI API key (raises the rate limit). Defaults to the
  `NCBI_API_KEY` or `ENTREZ_KEY` environment variable if set.

## Details

Two database types are supported:

- **GetOrganelle** - a seed FASTA (whole/partial mitogenomes) and a
  label FASTA (individual gene sequences).

- **MitoFinder** - a single GenBank (.gb) file containing only complete
  mitochondrial genomes.

Databases are written to a dated, clade-named sub-directory of `db_path`
(e.g. `Percidae_refseq_2026-06-11`), recording when GenBank was
accessed. A `README.txt` and `manifest.json` capture the full query,
taxid, record counts, and access time.

The historical manual review of unannotated ("nogene") sequences is
automated: such sequences are added to the seed database only if they
are long enough relative to the complete mitogenomes in the download
(see `include_nogene` / `nogene_min_frac`).
