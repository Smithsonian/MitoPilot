# GEOME metadata integration: design spec

Date: 2026-09-24
Branch: `geome-metadata` (off `main` at add6d78)
API research notes: `dev/geome/geome_api_research.md` (gitignored)

## Goal

Let a user attach a GEOME BCID (ARK identifier) to each sample. MitoPilot fetches
that record and every ancestor up the GEOME parent tree (e.g. Tissue -> Sample ->
Event -> Expedition/Project), stores it in the project database, shows the full
record in the app, and lets the user pick which fields (raw or GenBank-ready
combinations) become available at export.

Non-goals (this round):
- No automatic filling of export templates / source modifiers. The user chooses.
- No auth for private GEOME projects. GEOME OAuth (`POST /oauth/accessToken`,
  password grant supported) needs a client ID + secret issued by the GEOME team;
  the geomedb R package has no auth either. Follow-up once credentials exist:
  `geome_login()` storing a token in the user config dir, bearer header added in
  `.geome_get()`, refresh on expiry.
- No writing back to GEOME.

## GEOME API facts relied on

- `GET https://api.geome-db.org/records/{bcid}?includeParent=true` returns
  `record` (flat field -> value) plus a one-level `parent` object carrying
  `parent.bcid`. It never recurses; loop until the response has no `parent`.
- Follow `parent.bcid`, never project-specific linkage fields (`materialSampleID`,
  `eventID`), whose names vary by project config.
- Top record carries `projectId` + `expeditionCode`:
  `GET /projects/{projectId}/expeditions/{expeditionCode}` for expedition metadata.
  `GET /projects/{projectId}` returns 405; project metadata comes from
  `GET /projects?includePublic=true` filtered on `projectId`.
- Unknown BCIDs return HTTP 500, not 404.
- Public records need no auth. No documented rate limit.
- Responses may be gzip; httr2 handles this.
- n2t.net ARK URLs redirect to HTML, not JSON; strip to the bare ARK and call the API.
- Example public BCID: `ark:/21547/CYC2CMPI38181.1` (Tissue -> Sample -> Event, project 75).

## Data model

### `samples.GEOME_BCID` (reserved column)
- Optional. Holds the bare ARK (`ark:/21547/...`), normalized on write.
- `new_project()`, `new_project_userAsmb()`, `new_db()`, `new_db_userAsmb()` gain
  `mapping_geome = "GEOME_BCID"`, handled like `mapping_id`/`mapping_taxon`: the
  named CSV column is renamed to `GEOME_BCID`. Absent column = feature unused.
- `check_mapping()` treats `GEOME_BCID` as reserved (collision checks as for other
  reserved names) and validates BCID format (warn, do not fail).
- `export_metadata_cols()` treats `GEOME_BCID` as MitoPilot-owned (not shown in the
  generic Metadata group).

### `geome_records`
One row per sample x level x field.

| column | meaning |
|---|---|
| ID | sample ID |
| level | GEOME entity (`Tissue`, `Sample`, `Event`, ...), or `Expedition`, `Project` |
| depth | 0 = the sample's own BCID, increasing toward the root |
| bcid | BCID of that level's record (NA for Project) |
| field | field name as returned by GEOME |
| value | character value |

Empty/NA values are not stored.

### `geome_status`
One row per sample with a BCID: `ID, bcid, status ('ok'|'failed'), message, fetched_at`.
Drives the app icon and tooltip.

### `geome_export_fields`
Project-wide list of fields the user ticked: `key` (`combo:<name>` or
`raw:<level>:<field>`). Nothing ticked by default.

## Fetch layer (`R/geome.R`)

- `geome_normalize_bcid(x)`: trims, strips `https://n2t.net/` and similar
  prefixes, returns bare `ark:/...` or NA.
- `geome_fetch_chain(bcid)`: loops `/records/{bcid}?includeParent=true`, follows
  `parent.bcid`, then fetches expedition and project metadata. Guards: max 10
  levels, stop on repeated BCID. Uses httr2 with user agent, timeout, retry.
  Per-call in-memory cache keyed by BCID so shared ancestors are fetched once.
  Returns a long data frame (level, depth, bcid, field, value) or an error condition.
- `fetch_geome(path = ".", ids = NULL, bcids = NULL)` (exported): for the given
  samples (default: all with a BCID), sets `samples.GEOME_BCID` if `bcids` given,
  fetches, and per sample replaces `geome_records` + `geome_status` rows in one
  transaction. A failed fetch writes `status = 'failed'` + message and keeps any
  previous `geome_records` rows. Ends with one summary warning listing failures.
  All work runs in the R session / app (driver side), never in Nextflow.

## Hooks into existing functions

- `new_db()` / `new_db_userAsmb()` (and the `new_project*` wrappers): store
  `GEOME_BCID`; if any present and `fetch_geome = TRUE` (new arg), call the fetch.
  Create the three `geome_*` tables for every new project (empty if unused).
- `add_samples()`: accepts `mapping_geome`; fetches for newly added samples.
- `update_sample_metadata()`: accepts `mapping_geome`; refetches samples whose BCID
  changed or was added; drops `geome_records`/`geome_status` rows for samples whose
  BCID was cleared.
- Older projects: tables created lazily on first GEOME use (`CREATE TABLE IF NOT
  EXISTS`), and `GEOME_BCID` added via `ALTER TABLE` like other new columns.
- Init network check: when BCIDs are present, also check `api.geome-db.org` reachability.

## GenBank-ready combinations

Computed from raw fields at read time (not stored). Each is empty unless all its
required parts exist. Source fields are searched across all levels, nearest
(lowest depth) first.

| key | built from | example output |
|---|---|---|
| `geome_lat_lon` | decimalLatitude, decimalLongitude | `17.53 S 149.83 W` |
| `geome_collection_date` | yearCollected, monthCollected, dayCollected | `2009-11-05`, `2009-11`, `2009` |
| `geome_geo_loc_name` | country, then locality or stateProvince | `French Polynesia: Moorea` |
| `geome_specimen_voucher` | catalogNumber (required), institutionCode (optional prefix) | `UF:12345` |
| `geome_collected_by` | collectorList | as is |
| `geome_tissue_type` | tissueType | as is |
| `geome_sex` | sex | as is |
| `geome_dev_stage` | lifeStage | as is |

Exact GEOME field names get confirmed against real records during implementation;
the table above is the target mapping.

## Export integration

- `fetch_export_data()` joins one column per ticked field: combos as
  `geome_<combo>`, raw fields as `geome_<Level>_<field>`. Unticked fields never
  enter export data.
- Ticked columns appear in the Export tab's Metadata column group and can be used
  as `{geome_...}` tokens in FASTA header templates (existing `str_glue_data`
  path; validation already accepts any column in the row).
- No template is changed automatically.

## App

### GEOME column (Assemble, userAsmb Assemble, Annotate, Export tables)
- Always shown, placed immediately after Taxon.
- Icon states: filled = fetched OK; warning = BCID present but fetch failed
  (tooltip gives reason); blank = no BCID.
- Click on any state opens the GEOME viewer for that sample. Annotate rows of the
  same sample share the icon.

### GEOME viewer modal (shared module, all panels)
- Left: sample picker, with status markers.
- Right: selected sample's record, grouped by level from root (Project) down to
  the sample's own level, each a collapsible card of field/value rows. Each level
  links to `https://geome-db.org/record/<bcid>`.
- BCID input for the selected sample with Fetch/Refresh; last fetched time.
- Project-wide actions: Refresh all; list of failed samples.
- Empty state when the sample has no BCID explains how to add one.

### Export field picker (Export panel only)
- Opened from a "GEOME Fields" button in the Export toolbar (Shiny shows one
  modal at a time, and the header template editor is itself inside the Export
  modal).
- Top: GenBank combos, each with a preview value, count of samples with a value,
  and tick box.
- Below: all raw fields seen in the project (level, field, samples with value,
  example value, tick box).
- Saves to `geome_export_fields`; Export table refreshes.

## Errors and edge cases

- Bad format, 404, 401/403 (private), timeout, network down: per-sample `failed`
  status + message; batch continues.
- Parent loop guard (10 levels, repeated BCID).
- Refetch is transactional per sample; failed refetch keeps prior data.
- Missing ticked field for a sample: empty value at export.
- Duplicate BCIDs across samples allowed (fetched once via cache).

## Documentation

New vignette `vignettes/GEOME-Metadata.Rmd` (title "GEOME Metadata"), added to the
`Usage` article group in `_pkgdown.yml` after `custom_dbs`. Style matches existing
Usage vignettes (html_vignette, code chunks `eval = FALSE`). Sections:

1. What GEOME is and what MitoPilot pulls (record + parent chain up to project).
2. Finding a sample's BCID in GEOME (record page / ARK format; n2t.net URLs accepted).
3. Adding BCIDs at project setup: CSV column + `mapping_geome`, `fetch_geome`
   arg to skip fetching offline.
4. Adding or changing BCIDs later: `add_samples()`, `update_sample_metadata()`,
   `fetch_geome()` (refresh one, some, or all samples).
5. Viewing records in the app: GEOME column icons, viewer modal, refresh, failed list.
6. Using GEOME fields at export: field picker, GenBank-ready combinations table
   (key, source fields, example), raw field naming (`geome_<Level>_<field>`),
   worked example of a FASTA header template using `{geome_lat_lon}` etc.
7. Limits and troubleshooting: public records only, failure reasons, what happens
   on refetch failure, network requirement.

Screenshots of the GEOME column, viewer, and field picker via the existing docs
screenshot harness (`dev/docs_shots`), saved under `vignettes/figures/`.
Short cross-links from `Your-Own-Project.Rmd` (mapping file section) and
`Test-Project-Export.Rmd` (header templates section). `man/` pages for new and
changed exported functions via roxygen.

## Testing

- Unit (offline, recorded JSON fixtures under `tests/testthat/fixtures/geome/`):
  chain walk, loop guard, BCID normalization, each combo formatter (incl.
  coordinate hemisphere/decimal handling, partial dates), failure handling.
- DB: `mapping_geome` rename at init; `add_samples` fetch; `update_sample_metadata`
  refetch/drop; export join adds only ticked `geome_*` columns; lazy table creation
  on an old project.
- Live: one test against `ark:/21547/CYC2CMPI38181.1`, `skip_on_cran()` + skip offline.
- Manual: app check on a test project with a few real public BCIDs (icons, viewer,
  picker, template tokens in exported FASTA headers).
