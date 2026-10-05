# NMNH user toggle in Export: design spec

Date: 2026-10-05. Branch `nmnh-export-toggle` (off `devel`) in MitoPilot and RiboPilot.

## Goal

An "NMNH user" switch in the Export modal that adds the two source modifiers NMNH
requires on GenBank submissions to every FASTA header, fills them per sample from
project metadata, checks they are correct, and reports and helps fix samples that
are missing or wrong.

Required modifiers:

- `[specimen_voucher=<Darwin Core triplet>]`, e.g. `USNM:FISH:487075`
- `[voucherURI=<NMNH EZID of the SPECIMEN record>]`, e.g.
  `http://n2t.net/ark:/65665/30f303846-9d23-4069-881e-e43ff718ef9f`

`voucherURI` is not on NCBI's public FASTA modifier list; the user confirmed NCBI
accepts it for NMNH. Emit as written.

## 1. Toggle and template behaviour

- Switch `nmnh_user` sits above the header template boxes. Its state is derived
  from the main template: ON iff it contains both NMNH tokens. No new storage;
  saving a template saves the NMNH state.
- NMNH tokens (exact text appended, single leading space):
  `[specimen_voucher={nmnh_specimen_voucher}] [voucherURI={nmnh_voucherURI}]`
- Turning ON:
  1. Scan each affected template for an existing `specimen_voucher` or
     `voucherURI` modifier (case-insensitive, `-`, `_`, and space equivalent;
     reuse `duplicate_modifiers()` normalisation).
  2. If found: confirm dialog listing them, "Replace with NMNH values?". Yes
     removes those modifiers (whole `[...]` block); Cancel reverts switch to OFF
     and changes nothing.
  3. Append tokens to the end of the main template, and of the gene template
     (MitoPilot `fasta_header_gene`) / marker template (RiboPilot
     `fasta_header_marker`) when that export is enabled.
  4. Show the NMNH panel (column dropdowns + status line) and run resolve/check.
- Turning OFF: remove the exact token text. If the text is not present verbatim
  (user edited it), leave the template and show a toast.
- Duplicates: header validation raises an error (blocks export) when either
  modifier appears more than once in a glued header. Other modifiers keep the
  existing warn-only behaviour.
- Empty values: at write time, a header containing `[specimen_voucher=]` or
  `[voucherURI=]` (empty, optional whitespace) has that block removed. Only these
  two modifiers.

## 2. Value resolution

Per sample, per modifier, the first valid value in this order:

1. Mapfile columns chosen in two dropdowns in the NMNH panel ("Voucher column",
   "Voucher URI column", plus "(none)"). Preselected by name:
   voucher: `specimen_voucher`, `voucher`, `catalogNumber` (reuse
   `.SPEC_CSV_NAMES` / `meta_csv_map` concept `voucher`);
   URI: `voucherURI`, `voucher_uri`, `occurrenceID`, `ezid`, `ark`.
   Stored in `meta_csv_map` as concepts `nmnh_voucher` and `nmnh_uri`.
2. GBIF linked record (`meta_records` source gbif, level Occurrence): voucher
   from institutionCode/collectionCode/catalogNumber via the code map below; URI
   from `occurrenceID`.
3. NCBI BioSample: `specimen_voucher` (else `genbankSpecimenVoucher`); URI = any
   NMNH ARK found in any attribute value.
4. GEOME: `genbankSpecimenVoucher` / voucher fields; URI = any NMNH ARK in any
   field.

Cross-fill: URI present but voucher missing -> voucher from the GBIF record of
the URI. Voucher present but URI missing -> existing `.gbif_find_voucher()`;
unique match gives `occurrenceID`. Every value carries its source label
(`mapfile`, `GBIF`, `NCBI`, `GEOME`, `derived from URI`, `derived from voucher`,
`entered`).

A value from a higher-priority source that fails validation is reported, and the
next source is tried; if a lower source supplies a valid value it is used and
the report notes the skipped invalid one.

## 3. Validation

### Voucher (offline)

- NMNH code map (GBIF collectionCode, case-insensitive -> NCBI BioCollections):
  `FISH->USNM:FISH`, `BIRDS->USNM:Birds`, `MAMM->USNM:MAMM`, `HERP->USNM:Herp`,
  `IZ->USNM:IZ`, `ENT->USNM:ENT`, `US->US` (Botany, doublet `US:<catalog>`).
- Valid: `USNM:<code>:<catalog>` with code in {FISH, Birds, MAMM, Herp, IZ, ENT,
  Botany}, or `US:<catalog>`. Catalog `[A-Za-z0-9][A-Za-z0-9.-]*`.
- Rejected: `USNM:LAB:*` (DNA bank, bio_material), other institutions, missing
  collection for USNM.
- Auto-fixes (reported as "fixed"): code case (`fish`->`FISH`); repeated
  institution prefix in catalog (`USNM:FISH:USNM 123`->`USNM:FISH:123`);
  `USNM 123` + known collection -> triplet. Reuse `.parse_voucher()`.

### URI

- Format: accept bare `ark:/65665/3...`, http/https `n2t.net`, `ezid.cdlib.org`,
  `arks.org`, `collections.nmnh.si.edu/...ark=`, hyphenated or not. Normalise to
  `http://n2t.net/ark:/65665/3` + 8-4-4-4-12 hex. Reject `m3` (media) and other
  NAANs. Reuse `.nmnh_normalize_ark()`.
- Specimen check (online): GBIF `occurrence/search?occurrenceID=<uri>`.
  - `PRESERVED_SPECIMEN` (or dataset 821cc27a-...) -> pass.
  - `MATERIAL_SAMPLE` with ResourceRelationship `relatedResourceID` containing
    `guid=<parent ark>` -> replace with parent URI, report "tissue ARK replaced
    with parent specimen".
  - Material sample without parent, other basis, or no hit -> problem.
- Consistency: if GBIF record of the final URI exists, its derived voucher must
  equal the final voucher (compare normalised); mismatch -> problem.
- Cache: table `nmnh_ark_cache(ark PK, basis, parent_ark, voucher, checked_at)`
  in the project DB. Cached rows reused; no expiry (manual "Re-check" button
  clears rows for the export group).
- Offline / GBIF error: format checks only; status line says specimen check
  skipped.

## 4. Report and fixing

- Status line in the NMNH panel: "All N samples ready" (green) or
  "K of N samples have NMNH problems [View report]" (amber).
- Report modal (opens from link, and automatically on Export click if problems
  remain):
  - Warning text: NMNH records should not be submitted to GenBank without both
    values.
  - reactable, scrollable, one row per problem sample; columns: `ID`,
    `specimen_voucher`, `voucherURI`. Each cell shows value (or blank) plus the
    problem/fix note and source. Fixed-only samples listed in a collapsed
    "Auto-fixed" section.
  - Voucher and URI cells are editable (text input). Edits are validated with
    the same rules on blur; valid edits are written to the `samples` columns
    chosen in the dropdowns (if "(none)", create `specimen_voucher` /
    `voucherURI` columns and select them). Source label becomes `entered`.
  - "Copy table" button (TSV to clipboard, same pattern as app_export.R
    clipboard JS).
  - "Download CSV for bulk edit": CSV of problem samples with `ID`, `Taxon`,
    voucher column, URI column (column names = dropdown choices), ready for
    `update_sample_metadata()`.
  - "Upload edited CSV": fileInput; runs `update_sample_metadata(path,
    update_mapping_fn = <upload>)` (it backs up the DB), then re-runs resolve and
    check.
- Export click with problems: confirm "Export anyway?" (danger). Proceeding
  writes headers with the empty modifiers stripped.

## 5. Code layout

- New `R/nmnh.R` (identical in both repos, header "Mirrors MitoPilot's
  nmnh.R"): pure helpers. `nmnh_tokens()`, `nmnh_template_add()`,
  `nmnh_template_remove()`, `nmnh_existing_mods()`, `nmnh_normalize_voucher()`,
  `nmnh_normalize_uri()`, `nmnh_gbif_check()`, `nmnh_resolve(con, ids, cols)`
  returning a tibble (ID, nmnh_specimen_voucher, nmnh_voucherURI, sources,
  notes, ok), `nmnh_strip_empty()`.
- Export data: add `nmnh_specimen_voucher` / `nmnh_voucherURI` columns to the
  per-sample export data (MitoPilot `fetch_export_data()` and `export_files()`
  `dat`; RiboPilot `export_header_data()`), only when the template uses them.
- App wiring: MitoPilot `R/app_export.R`; RiboPilot `R/app_modals.R` +
  its export server code.
- Validation: duplicate-modifier error for the two NMNH modifiers in
  `validate_fasta_header()` (both repos).

## 6. Testing

- Unit (`tests/testthat/test-nmnh.R`, both repos): template add/remove/
  conflict detect; voucher normalise/validate (all codes, LAB reject, fixes);
  URI normalise (all input forms, m3 reject); GBIF check with mocked responses
  (specimen pass, tissue->parent, tissue no parent, no hit, offline); resolve
  priority order and cross-fill; empty-modifier strip; duplicate error.
- Live smoke test (manual, skip on CRAN/CI): real ARKs above.
- App: open Export on a test project, toggle on/off, confirm conflict dialog,
  report, edit, CSV round trip.

## Out of scope

- NMNH site internal endpoint and EDAN API.
- General empty-modifier stripping for other modifiers.
