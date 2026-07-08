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
| `orf314`  | Hydrozoa, Scyphozoa                       | **Built** — see note below    |
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

## `orf314` (built)

The by-name protein-DB approach above fails for `orf314` (a mitogenome-specific
hypothetical ORF with no named RefSeq protein family). Instead,
`build_orf314_refdb.R` downloads cnidarian mitochondrial **nucleotide** records
that annotate orf314 (`"Cnidaria"[Organism] AND orf314[All Fields]`, RefSeq **not**
required), extracts each orf314 CDS translation from the record features,
dereplicates (drops <60 aa fragments; caps 3 per genus to tame oversampled
*Alatina*), and builds the protein BLAST DB. The result is 25 sequences spanning
16 Medusozoa genera (Scyphozoa, Cubozoa, Staurozoa, trachyline Hydrozoa), ~95-121
aa, all in one self-BLAST homology cluster. Provenance: per-accession rows in
`../novel_gene_refdb_provenance.csv` (status `included`).

The `orf314.fas` + BLAST index were added into the shipped tarball
`ref_dbs/Mitos2/Metazoa_RefSeq235.tar.gz` under `Metazoa_RefSeq235/featureProt/`
(extract tarball, drop in the files, re-tar). `curate_mito_core()` then BLASTs any
`orf314`-named feature against it automatically (ref DB path is
`featureProt/orf314.fas`), no code change needed.

NOTE ON PROVENANCE: every other sequence in the bundle is a RefSeq accession;
these 25 orf314 seqs are the only non-RefSeq content. The "RefSeq235" name is
treated as a base-release tag, and the tarball root carries a `MANIFEST.txt`
documenting the orf314 addition. `build_curation_db.R` rebuilds the bundle from
RefSeq only, so on a RefSeq-release bump the orf314 files + MANIFEST must be
re-applied (a comment there flags this).
