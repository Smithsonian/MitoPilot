# Sample metadata: NCBI BioSample / BioProject source

Date: 2026-09-28
Branch: `genbank-biosample-metadata` (off main 51ccdca)
Builds on: `tools/specimen_metadata_gbif_spec.md` (the `meta_*` source system)

## Goals

1. Let a user attach an NCBI BioSample (or an SRA accession that links to one) to each
   sample, fetch the BioSample record plus its linked BioProject(s), show it in the app, and
   offer its fields at export, exactly like GEOME and GBIF.
2. Add NCBI as a third source in the Compare tab and export-conflict warning (flag only,
   never merge).
3. Let the sample ID column double as the BioSample/SRA column.

Non-goals: batch efetch (one sample at a time, like GEOME/GBIF); automatic lookup without the
user asking; NCBI login; writing BioSample/BioProject into GenBank submission files beyond
ordinary export tokens.

## NCBI facts relied on (verified live 2026-09-28)

E-utilities base `https://eutils.ncbi.nlm.nih.gov/entrez/eutils`.

- `efetch.fcgi?db=biosample&id=<SAMN.. | numeric uid>&retmode=xml` returns
  `<BioSampleSet><BioSample accession=".." id="..">`. Unknown ID returns an empty
  `<BioSampleSet></BioSampleSet>` (HTTP 200).
- BioSample XML: `Ids/Id` (primary accession, `db_label="Sample name"`, `db="SRA"` SRS),
  `Description/Title`, `Description/Organism@taxonomy_id` + `OrganismName`, `Owner/Name`,
  `Package`, `Attributes/Attribute@attribute_name` (+ `@harmonized_name` when standard),
  `Links/Link@target="bioproject"` with `label` = PRJ accession and text = numeric uid,
  root attrs `publication_date`, `last_update`, `submission_date`.
- Harmonized attribute names match GenBank source modifiers: `collection_date`,
  `geo_loc_name` (`Country: region, locality`), `lat_lon` (`d.dd N d.dd W`),
  `specimen_voucher`, `collected_by`, `identified_by`, `sex`, `dev_stage`, `isolate`,
  `tissue`. Missing values are free text such as `missing`, `not collected`,
  `not applicable`, `not provided`, `restricted access`.
- `efetch.fcgi?db=bioproject&id=<numeric uid>&retmode=xml`: `Project/ProjectID/ArchiveID@accession`,
  `ProjectDescr/Name|Title|Description`, `ProjectDescr/Relevance/*`,
  `ProjectType//Target@material|capture|sample_scope`, `ProjectDataTypeSet/DataType`,
  `Submission@submitted|last_update`, `Submission//Organization/Name`,
  `ProjectLinks//Hierarchical[@type="TopAdmin"]/MemberID@accession` (umbrella project).
- SRA: `esearch.fcgi?db=sra&term=<acc>[accn]` -> uid (works for SRR/ERR/DRR runs, SRX
  experiments, SRS samples). `esummary.fcgi?db=sra&id=<uid>&retmode=json` ->
  `result[uid].expxml` (escaped XML with `<Biosample>`, `<Bioproject>`, `Experiment@acc`,
  `Study@acc|name`, `Platform@instrument_model`, `Library_descriptor/*`, `Submitter@center_name`)
  and `result[uid].runs` (`<Run acc=..>`).
- Rate limit 3 requests/s without an API key, 10/s with `api_key=`. `tool` and `email`
  params are recommended.
- Examples: `SAMN63236902` (ocean pout, full harmonized attributes, one BioProject
  `PRJNA1422759` with umbrella `PRJNA1422710`); `SRR21844202` -> `SAMN29555051` /
  `PRJNA720393` (shipped test fish).

## IDs

- Reserved samples column `BioSample` (added by `.meta_ensure_tables`, like `GBIF_ID`).
- `ncbi_normalize_id(x)` (exported): trims, uppercases, strips
  `https://www.ncbi.nlm.nih.gov/biosample/` and `.../sra/` URL prefixes and trailing `/`.
  Valid: BioSample `^SAM(N|EA|D)[0-9]+$`, bare BioSample uid `^[0-9]+$`, SRA
  `^[SED]R[RXS][0-9]+$`. Anything else -> NA.
- The stored value is what the user typed, normalized. The resolved BioSample accession is
  kept in `meta_records` (BioSample level `ref`), not in `samples`.
- Registry entry `META_SOURCES$NCBI`: `col = "BioSample"`, `label = "NCBI"`,
  `id_label = "BioSample or SRA accession"`, `arg = "biosamples"`.

## Fetch

- `.ncbi_get(endpoint, query)`: httr2 GET, user agent as GBIF, `tool=MitoPilot`, `api_key`
  from env `ENTREZ_KEY` when set; 30 s timeout; retry on 429/5xx; throttle so requests are
  >= 0.34 s apart (0.11 s with a key) using a package-level timestamp. Errors: network ->
  "could not reach NCBI (...)", HTTP >= 400 -> "NCBI returned HTTP <n>".
- XML parsed with `xml2` (new Imports entry).
- `.ncbi_fetch_chain(ref, cache)`:
  1. If `ref` is SRA: esearch + esummary; not found -> error "SRA accession <x> not found";
     no `<Biosample>` -> error "SRA accession <x> has no linked BioSample". Emits level
     `SRA` (depth 0, ref = typed accession) with fields `accession`, `experiment`, `study`,
     `study_title`, `title`, `runs` (comma-joined run accessions), `platform`, `instrument`,
     `library_strategy`, `library_source`, `library_selection`, `library_layout`,
     `center`, `biosample`, `bioproject`. Continue with the BioSample accession.
  2. efetch BioSample; empty set -> error "BioSample <x> not found". Emits level
     `BioSample` (depth 1, ref = accession) with fields `accession`, `title`, `organism`,
     `taxonomy_id`, `sample_name`, `sra_sample`, `owner`, `package`, `publication_date`,
     `last_update`, then one field per attribute named by `harmonized_name` if present
     else `attribute_name` (first wins on duplicate names). Values stored raw, including
     "missing"-type text.
  3. For each linked BioProject (order as listed), efetch by uid (cached per run); emits
     level `BioProject` at depth 2, 3, ... (ref = PRJ accession) with fields `accession`,
     `name`, `title`, `description`, `relevance`, `material`, `capture`, `sample_scope`,
     `data_type`, `organization`, `submitted`, `last_update`, `umbrella`. A failed
     BioProject fetch is skipped (keeps the BioSample), like GBIF dataset/org.
- Exported `fetch_biosample(path = ".", ids = NULL, biosamples = NULL, from_id = FALSE)`:
  same contract as `fetch_gbif()`; `from_id = TRUE` first sets each target sample's
  `BioSample` to its own sample ID where that ID is a valid NCBI/SRA ID (others untouched,
  reported in a message). `ids` limits which samples.
- Refresh rules unchanged (keyed by ID + source): failed refresh keeps prior data;
  changing the ID clears the record; blank clears.

## Setup hooks

- `mapping_biosample = "BioSample"` and `fetch_biosample = TRUE` added to `new_project()`,
  `new_project_userAsmb()`, `new_db()`, `new_db_userAsmb()`, `add_samples()`,
  `update_sample_metadata()`, mirroring `mapping_gbif`/`fetch_gbif`. Fetch only happens
  when the named column exists and has IDs (the user asked for it by supplying them).
- `mapping_biosample` may name the same column as `mapping_id`. `.meta_take_cols()` must
  never drop the ID or Taxon source column, and must never drop a column another source
  or `mapping_id`/`mapping_taxon` also uses.
- `check_mapping()`: `BioSample` reserved when the mapping column differs; warns on values
  that are not BioSample/SRA IDs. Preflight checks `eutils.ncbi.nlm.nih.gov` when IDs are
  present and fetching is on.

## Export tokens (NCBI GenBank-ready combos)

Built from the BioSample level; values in the "missing" set (case-insensitive: `missing`,
`not collected`, `not applicable`, `not provided`, `restricted access`, `unknown`, `na`,
`n/a`, `none`, `-`, and those words with a `: ` suffix such as `missing: control sample`)
count as empty.

| token | from | rule |
|---|---|---|
| `ncbi_lat_lon` | lat_lon | `d.dd N d.dd W` kept; a decimal pair `lat, lon` or `lat lon` converted with the GEOME formatter |
| `ncbi_collection_date` | collection_date | as is if ISO `YYYY`, `YYYY-MM`, `YYYY-MM-DD`; other forms passed through |
| `ncbi_geo_loc_name` | geo_loc_name | as is |
| `ncbi_specimen_voucher` | specimen_voucher | as is |
| `ncbi_collected_by` | collected_by | as is |
| `ncbi_identified_by` | identified_by | as is |
| `ncbi_sex` | sex | lowercased |
| `ncbi_dev_stage` | dev_stage | lowercased |
| `ncbi_biosample` | BioSample `accession` | as is |
| `ncbi_bioproject` | first BioProject `accession` | as is |

Raw fields: `ncbi_<Level>_<field>` via the existing `.meta_key_col` sanitizer (e.g.
`ncbi_BioSample_Common_name`).

## Compare

`specimen_conflicts()` gains `ncbi_value`; comparison and status logic generalize from the
fixed CSV/GEOME/GBIF triple to CSV plus every `META_SOURCES` entry. NCBI values per concept:

| concept | NCBI value |
|---|---|
| coordinates | `ncbi_lat_lon` combo |
| collection_date | `ncbi_collection_date` combo |
| country | part of `geo_loc_name` before `:` |
| locality | part of `geo_loc_name` after `:`, trimmed |
| voucher | `ncbi_specimen_voucher` combo |
| collector | `ncbi_collected_by` combo |
| sex | `ncbi_sex` combo |
| dev_stage | `ncbi_dev_stage` combo |
| taxon | BioSample `organism` |

Missing-set values are blank (never conflict). Agreement rules unchanged. Compare table
gets an NCBI column; export warning table gets an NCBI column; concept detection for
`{ncbi_*}` tokens follows the same token -> concept map.

## App

- Icon `inst/app/www/specimen/ncbi_helix.png`, 64x64: the white helix from the public-domain
  NLM/NCBI logo (Wikimedia `US-NLM-NCBI-Logo.svg`) on NCBI blue `#336699`, text cropped
  off. Also a CSS logo class `mp-meta-logo-ncbi` for metadata column headers.
- Metadata column: NCBI logo per sample alongside GEOME/GBIF (same ok/faded/failed states);
  tooltips and empty-text say "GEOME, GBIF, or NCBI".
- Viewer: NCBI tab after GBIF. ID box (label "BioSample or SRA accession", placeholder
  `SAMN29555051 or SRR21844202`), Fetch, and a "Use sample ID" button that copies the
  sample's own ID into the box and fetches (error notification if the ID is not an
  NCBI/SRA ID). Level cards in order SRA, BioSample, BioProject(s), linked to
  `https://www.ncbi.nlm.nih.gov/sra/<acc>`, `/biosample/<acc>`, `/bioproject/<acc>`.
  "Refresh all" covers NCBI.
- Metadata field picker (on-screen) and Set Export Metadata picker: NCBI section.
- Export Data token chips: NCBI group (`BioSample` + ticked NCBI tokens).
- All hard-coded `c("GEOME", "GBIF")` lists in app code become `names(META_SOURCES)`.

## Backwards compatibility

`.meta_ensure_tables` adds the `BioSample` column to old projects; the compat check in
`backwards_compatibility.R` that requires `GEOME_BCID`/`GBIF_ID` also requires `BioSample`.

## Docs

`vignettes/Specimen-Metadata.Rmd` title becomes "Sample metadata: GEOME, GBIF, and NCBI";
new section on BioSample/SRA IDs, `fetch_biosample()`, `from_id`, the NCBI tab, tokens,
`ENTREZ_KEY`. NEWS entry. pkgdown reference entries for `fetch_biosample` and
`ncbi_normalize_id`.

## Testing

- Offline fixtures under `tests/testthat/fixtures/ncbi/`: BioSample XML for
  `SAMN63236902` and `SAMN29555051`, empty BioSampleSet, BioProject XML for 1422759 and
  720393, SRA esearch XML + esummary JSON for `SRR21844202`, an SRA esearch with Count 0.
- Unit: `ncbi_normalize_id` cases; chain for BioSample-only, SRA-resolved, not-found,
  no-BioProject, BioProject failure; attribute naming (harmonized vs raw, duplicates);
  combos incl. missing-set and lat_lon forms; `from_id`; `.meta_take_cols` with
  `mapping_biosample == mapping_id`; conflicts with NCBI values.
- Existing GEOME/GBIF tests pass unchanged except where column lists now include NCBI.
- App: module startup test (MockShinySession) with an NCBI source present.
- Real-browser pass on a demo project using the shipped fish SRR IDs as sample IDs:
  "Use sample ID", Refresh all, icon states, NCBI tab cards, Compare with a deliberate
  CSV conflict, pickers, token chips, and an exported defline with `{ncbi_*}` tokens.
