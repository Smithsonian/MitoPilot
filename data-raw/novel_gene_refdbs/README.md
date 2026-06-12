# Novel-gene reference protein DBs — build status & review

Several new invertebrate clades declare protein-coding genes that have no
existing `ref_dbs/Mitos2/.../featureProt/<gene>.fas` reference DB:

| gene      | clades                                   | tractable as auto NCBI fetch? |
|-----------|------------------------------------------|-------------------------------|
| `rvt`     | Bryozoa, Demospongiae, Polychaeta, Sipuncula | **No** — see note below       |
| `dnaB`    | Hydrozoa                                  | No — no name-matched RefSeq protein |
| `im`      | Bryozoa, Sipuncula                        | No — ambiguous gene symbol    |
| `orf167`  | Demospongiae                              | No — single-taxon hypothetical ORF |
| `orf1535` | Demospongiae                              | No — single-taxon hypothetical ORF |
| `orf314`  | Hydrozoa                                  | No — single-taxon hypothetical ORF |
| `orf`     | Nemertea, Pycnogonida                     | No — generic/unnamed ORF      |

## Why no DBs were committed

`build_novel_gene_refdbs.R` can fetch RefSeq proteins from NCBI by gene name +
clade taxon, but the results are **not biologically reliable** for these genes:

- An automated `"reverse transcriptase" + txid<clade> + srcdb_refseq` query for
  `rvt` returned 50 sequences that were predominantly **`XP_` model proteins
  from nuclear genome annotation** (e.g. *Watersipora subatra* retroelement RTs,
  2024 aa) — i.e. **nuclear retroelement contamination**, not the
  mitochondrial intron-encoded reverse transcriptase the rulesets need.
- `dnaB` and the named `orf*` genes have no clean RefSeq protein family to query
  by name; they are mitogenome-specific hypothetical ORFs.

So none of these were promoted into `ref_dbs/`. The provenance of the rejected
`rvt` pull is preserved in `../novel_gene_refdb_provenance.csv` for the record.

## How the clades still work today

`curate_mito_core()` skips BLAST for any gene whose `featureProt/<gene>.fas` is
absent (graceful guard), so all 19 new clades run end-to-end. Genes without a
ref DB simply are not curated against references and get a "low reference
similarity" validation flag, which is the correct conservative behavior.

## To finish (maintainer, manual)

For each gene you want curated against references:
1. Manually assemble a vetted set of **mitochondrial** protein sequences
   (curated from published clade mitogenomes, not nuclear paralogs).
2. Header format must follow the featureProt contract (hyphen-free gene token):
   `>{accession}:{gene}-1-1-{len} {Species}`
3. `makeblastdb -in <gene>.fas -dbtype prot` (conda env `blast`).
4. Move `<gene>.fas` + index into `ref_dbs/Mitos2/Metazoa/featureProt/` and
   rebuild the distributed `Metazoa_RefSeq*.tar.gz` (see `R/custom_curation_db.R`
   for how the tarball is consumed).

`build_novel_gene_refdbs.R` remains as a starting scaffold — tighten the Entrez
queries to mitochondrial sources before trusting any auto-fetched set.
