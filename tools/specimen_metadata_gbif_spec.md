# Specimen metadata: GBIF integration, CSV/GEOME/GBIF reconciliation, export token list

Date: 2026-09-24
Branch: `geome-metadata` (continues the GEOME work; builds on `tools/geome_metadata_spec.md`)
API research: `dev/gbif/gbif_api_research.md` (gitignored)

## Goals

1. Let a user attach a GBIF occurrence `gbifID` to each sample, fetch that occurrence
   plus its dataset and publisher, show it in the app, and offer its fields at export,
   the same way GEOME works.
2. Generalize the GEOME machinery into one "specimen metadata" system with a `source`
   dimension (GEOME, GBIF), so the app has one icon column, one viewer, one picker.
3. Reconcile metadata from the user's CSV, GEOME, and GBIF by **flagging disagreements
   only**. MitoPilot never merges or picks a value; each source keeps its own tokens and
   the user chooses what goes in the header template.
4. Replace the Export Data modal's flat "Available columns" lists with grouped,
   clickable token chips.

Non-goals: rgbif dependency (two or three httr2 GET calls cover it); GBIF login, bulk
downloads, or search; auto-finding a GBIF record from a GEOME ARK; merging values;
GenBank's own country vocabulary beyond ISO matching.

## GBIF API facts relied on (verified live 2026-09-24)

- `GET https://api.gbif.org/v1/occurrence/{gbifID}`: interpreted record, flat Darwin
  Core fields; `gbifID` == `key`. No auth, no fixed rate limit (429 possible).
- Unknown or removed ID: HTTP 404, plain-text body.
- `GET /v1/dataset/{datasetKey}`: title, citation (text), license, DOI.
- `GET /v1/organization/{publishingOrgKey}`: publisher title, homepage.
- `GET /v1/enumeration/country`: 250 entries with `iso2`, `iso3`, `title`.
- Interpreted record carries `issues` (array of codes such as `COUNTRY_COORDINATE_MISMATCH`,
  `ZERO_COORDINATE`, `COORDINATE_INVALID`, `COORDINATE_OUT_OF_RANGE`, `COORDINATE_ROUNDED`).
- `country` is a full English name, `countryCode` ISO alpha-2.
- `eventDate` may be year only or a full ISO date/interval; `year`/`month`/`day` also given.
- gbifIDs are not guaranteed stable across dataset republishing; `occurrenceID` is the
  publisher's own identifier and is stored as a fallback anchor.
- Some GEOME projects publish to GBIF (e.g. DIPnet, publisher "The Genomic Observatories
  Metadatabase (GeOMe)"); their `occurrenceID`/`materialSampleID` hold GEOME ARKs.
- Examples: `6186461308` (UF fish specimen, coordinates, full date, catalog number);
  `2336663130` (GEOME-published DIPnet record).

## Data model (generalizes the GEOME tables)

`samples` gains reserved column `GBIF_ID` (alongside `GEOME_BCID`).

| table | columns | notes |
|---|---|---|
| `meta_records` | ID, source, level, depth, ref, field, value | PK (ID, source, depth, field); replaces `geome_records` |
| `meta_status` | ID, source, ref, status, message, fetched_at | PK (ID, source); replaces `geome_status` |
| `meta_export_fields` | key | `<source>:combo:<name>` or `<source>:raw:<level>:<field>`; replaces `geome_export_fields` |
| `meta_csv_map` | concept, column | user overrides of CSV column detection (see Reconciliation) |

- `source` values: `GEOME`, `GBIF`. `ref` holds the BCID / ARK or gbifID / key of that level.
- GBIF levels: `Occurrence` (depth 0), `Dataset` (1), `Organization` (2).
- `issues` is stored as one field (`issues`, comma-joined codes) on the Occurrence level.
- Migration: `.meta_ensure_tables(con)` (successor of `.geome_ensure_tables`) creates the
  `meta_*` tables, adds `GEOME_BCID`/`GBIF_ID` if missing, and if old `geome_*` tables
  exist copies their rows into `meta_*` with `source = 'GEOME'` (export keys prefixed
  `geome:`), then drops the old tables, in one transaction.
- Existing GEOME rules carry over unchanged, keyed by (ID, source): failed refresh of the
  same ID keeps prior data; changing an ID clears the old record; blank ID clears.

## GBIF fetch

- `gbif_normalize_id(x)`: trims; accepts `https://www.gbif.org/occurrence/<n>` and
  `https://api.gbif.org/v1/occurrence/<n>`; returns digits-only string or NA.
- `.gbif_get(path)`: httr2 with user agent, 30 s timeout, retry on 429/5xx.
  Messages: 404 -> "GBIF occurrence not found (IDs can change when a dataset is
  republished)"; other 4xx/5xx -> "GBIF returned HTTP <n>"; network -> "could not reach
  GBIF (...)".
- `.gbif_fetch_chain(id, cache)`: occurrence, then dataset and organization (cached per
  run by key; failures there keep the occurrence).
- Exported `fetch_gbif(path = ".", ids = NULL, gbifs = NULL)`: same contract as
  `fetch_geome()`.
- Shared core: GEOME and GBIF fetch-into/set-id/drop use one internal implementation
  parameterized by source (`.meta_fetch_into(con, source, ids, refs)` etc.).
- Hooks: `mapping_gbif = "GBIF_ID"` and `fetch_gbif = TRUE` in `new_project()`,
  `new_project_userAsmb()`, `new_db()`, `new_db_userAsmb()`, `add_samples()`,
  `update_sample_metadata()`, mirroring `mapping_geome`/`fetch_geome`; `check_mapping()`
  treats `GBIF_ID` as reserved and warns on non-numeric IDs; preflight checks
  `api.gbif.org` reachability when GBIF IDs are present.

## GBIF GenBank-ready combinations

Built only from the Occurrence level; empty unless required parts exist.

| token | from | rule |
|---|---|---|
| `gbif_lat_lon` | decimalLatitude, decimalLongitude | same format as GEOME; empty if issues contain ZERO_COORDINATE, COORDINATE_INVALID, or COORDINATE_OUT_OF_RANGE |
| `gbif_collection_date` | year, month, day (fallback eventDate if it is a single ISO date) | `YYYY`, `YYYY-MM`, or `YYYY-MM-DD`; intervals ignored |
| `gbif_geo_loc_name` | country, stateProvince, locality | `Country: region, locality`, same rules as GEOME |
| `gbif_specimen_voucher` | institutionCode, collectionCode, catalogNumber | `inst:coll:cat`; `inst:cat` without collection; `cat` alone; catalogNumber required; skipped if catalogNumber is a URL or ARK |
| `gbif_collected_by` | recordedBy | as is |
| `gbif_identified_by` | identifiedBy | as is |
| `gbif_sex` | sex | lowercased |
| `gbif_dev_stage` | lifeStage | lowercased |

Raw fields: `gbif_<Level>_<field>` (sanitized like GEOME). GEOME tokens are unchanged.

## Reconciliation (flag only)

Concepts compared: coordinates, collection_date, country, locality, voucher, collector,
sex, dev_stage, taxon.

CSV column detection (case-insensitive; first match wins; stored overrides win):

| concept | CSV names tried |
|---|---|
| coordinates | `lat_lon`; or pair `lat`/`latitude`/`decimalLatitude` + `lon`/`long`/`longitude`/`decimalLongitude` |
| collection_date | `collection_date`, `date`, `eventDate` |
| country | `country`, `geo_loc_name` (country part before `:`) |
| locality | `locality` |
| voucher | `specimen_voucher`, `voucher`, `catalogNumber` |
| collector | `collected_by`, `collector`, `recordedBy` |
| sex | `sex` |
| dev_stage | `dev_stage`, `life_stage`, `lifeStage` |
| taxon | `Taxon` (always) |

Overrides: `set_metadata_columns(path = ".", ...)` (named args concept = column, `NA`
clears) and a mapping control in the viewer's Compare tab; both write `meta_csv_map`.

Source values per concept: GEOME and GBIF use the same source fields as their combos
(taxon: `scientificName`; country: GBIF `countryCode`, GEOME/CSV names via the country
table).

Normalization and agreement:

| concept | agree when | severity if not |
|---|---|---|
| coordinates | both parse (decimal or `d.dd N/S d.dd E/W`) and differ by <= 0.01 degree in each axis | conflict; unparseable value = note |
| collection_date | equal at the coarser of the two precisions | conflict; unparseable value = note |
| country | same ISO2 (names mapped via `inst/extdata/countries.csv`, built from GBIF's enumeration, plus common aliases e.g. USA, United States of America) | conflict; unmappable name vs code = note |
| voucher | catalog numbers equal after dropping institution/collection prefixes and whitespace, case-insensitive | conflict |
| taxon | first two words equal, case-insensitive | conflict |
| locality, collector | trimmed, case-insensitive equal | note |
| sex, dev_stage | lowercased equal | note |

Only sources that both have a value are compared; blanks never conflict. Status per
concept per sample: `agree`, `note`, `conflict`, or `single` (one source only). Displayed status for non-judged differences was renamed from `note` to `not checked` (2026-09-25, user decision).

Engine: `specimen_conflicts(con, ids = NULL)` returns ID, concept, csv_column,
csv_value, geome_value, gbif_value, status. It is the only comparison code; the icon
column, viewer Compare tab, and export warning all use it.

## App

### Specimen column (was GEOME column)
- Same place (after Taxon), same Columns-picker group (renamed `Specimen`), off by
  default when no sample has any GEOME or GBIF ID.
- One icon per sample, worst state wins: failed (warning triangle), conflict (orange
  flag), ok (globe), none (plus).
- Tooltip: one line per source with status, plus "Conflicts: <concepts>" and
  "Notes: <concepts>" when present.

### Specimen metadata viewer (was GEOME viewer)
- Title "Specimen metadata: <ID>", subtitle "Taxon: <Taxon>" (live); sample list with
  markers; fixed footer (Refresh all fetches both sources, Close).
- Tabs: **GEOME** (current content), **GBIF** (gbifID box + Fetch, level cards with
  `issues` as badges and the dataset citation, Expand/Collapse all), **Compare**
  (table: concept | CSV (column) | GEOME | GBIF | status; conflict rows orange, notes
  grey; "CSV columns..." control to set/clear the mapping per concept).
- Record cards stay in the scroll box; header, ID box, tabs, and footer stay fixed.

### Export
- "GEOME Fields" toolbar button becomes "Specimen Fields"; the picker has GEOME and GBIF
  sections (combos, then raw fields). Nothing ticked by default.
- Export-time warning: before writing, collect the concepts the active header templates
  use (from `{geome_*}`/`{gbif_*}` combo and raw tokens mapped to concepts, and from CSV
  columns mapped to concepts). If `specimen_conflicts()` has any `conflict` for an
  exported sample on one of those concepts, show a modal listing sample x concept with
  the differing values and buttons "Export anyway" / "Cancel". Notes never trigger it.

### Export Data modal: available columns
Replace the two comma lists with grouped, collapsible chip lists plus a filter box:

| group | contents | default |
|---|---|---|
| Basics | seqid, ID, Taxon, genetic_code, topology, completeness, path, scaffold | open |
| Your CSV columns | mapping-file columns (not reserved) | open |
| GEOME | GEOME_BCID + ticked GEOME tokens | open if any |
| GBIF | GBIF_ID + ticked GBIF tokens | open if any |
| Reference (BLAST) | blast_accession, blast_ref_status, blast_species, blast_lineage | collapsed |
| Assembly and annotation | length, structure, PCGCount, tRNACount, rRNACount, ORFCount, missing, extra, warnings, partial, curate_opts | collapsed |

- Never listed: annotate_switch, blast_accession_auto, poor_blast_ref,
  export_time_stamp, export_group, and app-only status columns.
- Each chip inserts its text at the cursor of the header box last focused (FASTA or
  gene header); default target is the FASTA header box.
- GenBank-ready combo chips insert the full snippet, e.g. `[lat_lon={geome_lat_lon}]`;
  `{Taxon}` stays a plain token.
- Hover shows the first exported row's value.
- GEOME/GBIF group headers link to the Specimen Fields picker; empty groups say how to
  add fields.
- Grouping lives in one helper `export_token_groups(data, sample_cols, ticked_keys)`
  returning a list of groups -> tokens, unit-tested; the UI renders it.

## Docs

- `vignettes/GEOME-Metadata.Rmd` becomes `vignettes/Specimen-Metadata.Rmd` ("Specimen
  metadata: GEOME and GBIF"); pkgdown entry and cross-links updated. Adds GBIF IDs,
  GBIF combos and issues, the Compare tab, CSV column mapping, the export warning, the
  new token chips; refreshed screenshots (column, viewer tabs incl. Compare, picker,
  Export Data token list). NEWS updated.

## Testing

- Offline fixtures: occurrence `6186461308`, occurrence `2336663130`, their dataset and
  organization, a 404; `gbif_normalize_id` cases; combo rules incl. issue-gated lat_lon,
  eventDate forms, voucher triplet.
- Migration: a DB with `geome_*` tables (as created by the current branch) migrates to
  `meta_*` with identical GEOME behavior; all existing GEOME tests pass after renaming.
- Reconciliation: each normalizer; `specimen_conflicts` agree/note/conflict/single cases;
  CSV auto-detect and overrides.
- Export: warning trigger logic (template concepts x conflicts) as a pure function;
  `export_token_groups`.
- App: module startup test for all four panels (MockShinySession, not testServer).
- Real-browser pass on a demo project with CSV + GEOME + GBIF values including one
  deliberate conflict: icon states, viewer tabs, Compare + mapping, picker, token chips
  insert at cursor, export warning, and an actual export defline.
