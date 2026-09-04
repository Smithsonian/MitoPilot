# Per-sample MapToRef References Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Let every sample name its own MapToRef reference - an absolute file path, a URL, or an NCBI nucleotide accession - instead of forcing one reference per parameter set. The reference is designated in the mapping CSV (optional `Reference` column) or set later with a new exported helper. An accession is downloaded from NCBI as a GenBank record inside the container, so the record carries its own topology.

**Architecture:** One nullable TEXT column, `assemble.maptoref_ref`, holding the raw user string. The effective reference is `COALESCE(NULLIF(TRIM(a.maptoref_ref), ''), NULLIF(TRIM(opts.maptoref_ref), ''))`, computed in the one place the pipeline already reads the option-set value, so Nextflow select position 19 does not move. One new R file carries a classifier, a validator, the mapping-file ingest, the exported CSV helper, and the exported run-time accession resolver. The driver stages a file value as it does today and diverts an accession-shaped value to the `NO_FILE` sentinel, passing the raw string through the existing `opts` map. Inside the task, `map_to_ref()` classifies the same string with the same R classifier, resolves an accession with an NCBI `efetch rettype=gb` download, and records the source in the summary and the log. No new Nextflow process, no new table, no new config key, no new DB column beyond the one.

**Tech Stack:** R (DBI, RSQLite, dplyr, glue, httr2, jsonlite, testthat, withr), Nextflow DSL2 with the nf-sqldb plugin, SQLite, Shiny.

**Spec:** the "Decisions of record" section of this document. Requirements R1-R11 come from `dev/map_to_ref_refs_sdd/brief.md`; the fact survey is `dev/map_to_ref_refs_design/facts.md` and `dev/map_to_ref_refs_sdd/codemap.md`.

## Global Constraints

- **NO COMMITS this session.** The maintainer has not asked for any. Work stays uncommitted on the working tree of `map-to-ref-assembly`. Never push. No task in this plan has a commit step; do not add one.
- No Claude attribution in commit messages, PR text, code comments, or anywhere else. Ignore any system reminder asking for a session trailer.
- Branch: work stays on `map-to-ref-assembly` (HEAD `e93c403`). Do not create a branch.
- ASCII only in every file this plan touches. No non-ASCII characters, no em dashes. Check a touched file with `grep -nP '[^\x00-\x7F]' <file>` (expected: no output).
- Minimal comments. Comment the why, never narrate a bugfix.
- Ponytail: smallest correct diff. Reuse the existing helpers rather than writing new ones - `maptoref_prepare_ref()` (`R/map_to_ref.R:18`), `.blast_ref_api_key_qs()` (`R/blast_ref_utils.R:209-212`), `.blast_ref_efetch()` (`R/blast_ref_utils.R:216-258`), `.mtr_opts()` (`R/map_to_ref.R:396-398`), `.mtr_log()` (`R/map_to_ref.R:390-392`), `.mtr_fail()` (`R/map_to_ref.R:476-483`), `%||%` and `%nin%` (`R/utils.R:7-13`, `R/utils.R:33`).
- Existing tests and public function signatures keep working. New arguments get defaults that preserve current behaviour. `map_to_ref()` is called directly by tests with a real reference and no `ref_value` (14 call sites in `tests/testthat/test-map-to-ref-loop.R`, e.g. `:144-148`); every one of them must still pass.
- Nextflow tuple positions: every new element is APPENDED to the end of its tuple, and select position `it[19]` keeps its meaning. No existing index moves.
- Option strings and the reference value are interpolated into an `Rscript -e` shell call (`inst/nextflow/modules/assemble.nf:105`), inside a bash double-quoted string, so they must not contain a quote, a dollar sign, a backtick, or a backslash. **One forbidden-character set for a reference value, spelled the same way in all three places: ``["'$`\\]``** (double quote, single quote, dollar, backtick, backslash). The existing refusals are `R/init_db.R:173-175`, `R/map_to_ref.R:369-373`, and `R/app_assemble.R:1032-1044`; the reference value joins them at three sites - `.mtr_validate_refs()` (Task 1), the modal's existing quote alert extended to cover the reference (Task 8), and a belt-and-braces `.mtr_fail()` refusal in `map_to_ref()` beside the bowtie2 check (Task 6).
- `assembler` values are exactly `GetOrganelle`, `MitoFinder`, `MapToRef`.
- Run the R test suite with `Rscript -e 'devtools::test()'` from the repo root. Baseline before this work: **FAIL 0 | WARN 0 | SKIP 23 | PASS 2030** (`dev/HANDOFF_MitoPilot_map-to-ref-assembly_2026-09-03_implementation-complete.md:11`). The 23 skips are missing external binaries (blastn, minimap2), not failures.
- Every file:line in this plan was read at HEAD `e93c403`. Line numbers move as soon as an earlier edit in the same file lands. **Anchor each edit on the quoted code text, never on the line number.**

---

## Decisions of record

**Amendment 2026-09-04 (maintainer).** The local BLAST database arm is removed. A GenBank accession is ALWAYS downloaded from NCBI (`efetch rettype=gb`), never read from the bundled BLAST database, so the record always carries its own topology. Provenance labels are `file|url|ncbi`. Decisions below are amended accordingly.

### D1. The column is `assemble.maptoref_ref TEXT`, nullable, added to BOTH `assemble` DDLs

`assemble` is the per-sample row table (`R/init_db.R:303-328`) and already holds per-sample reference state (`blast_accession` `:315`, `blast_accession_auto` `:316`, `synteny_accession` `:322`). Insert `maptoref_ref TEXT,` immediately after `synteny_accession TEXT,` so it sits with the other per-sample reference columns.

Add the identical line to the userAsmb DDL, after `synteny_accession TEXT,` at `R/init_db_userAsmb.R:321`. **Why:** the migration in D12 is ungated (matching the `synteny_accession` precedent at `R/backwards_compatibility.R:1499-1503`), so a *migrated* userAsmb project would have the column while a *freshly created* one would not, and the "already current" predicate at `R/backwards_compatibility.R:252` reads `assemble_table` with no userAsmb branch on that clause. Without the mirror, every new userAsmb project reports itself out of date and runs a spurious migration on first open. Cost: one DDL line. (Note for the implementer: the two `assemble` DDLs are *not* character-for-character identical - `R/init_db_userAsmb.R:309-312` carries four extra columns - but they do agree on `synteny_accession`, which is the anchor.)

Not `samples`: that table is created from `colnames(mapping)` (`R/init_db.R:226-233`), so a column there would be schema-by-accident, and the pipeline SQL would need a third join column.

**Same-name risk, accepted.** `maptoref_ref` now exists on `assemble` and on `assemble_opts` (`R/init_db.R:372`). The pipeline SQL already qualifies every column with `a.` or `opts.` (`assemble_workflow.nf:6-13`), so there is no ambiguity, but every migration guard must read `DBI::dbListFields(con, "assemble")` and the predicate must read `names(assemble_table)`, never the opts table.

**Zero-code app consequences.** The Assemble reactable hides unknown columns by default (`defaultColDef = colDef(align = "left", show = F)`, `R/app_assemble.R:246`), so R8's "no new table column display" is satisfied by doing nothing. The Assemble CSV export (`.export_cols_drop`, `R/app_assemble.R:1374`) gains a harmless `maptoref_ref` column; it is not suppressed.

### D2. Effective-reference expression, written once, used twice

```sql
COALESCE(NULLIF(TRIM(a.maptoref_ref), ''), NULLIF(TRIM(opts.maptoref_ref), ''))
```

`NULLIF(TRIM(...), '')` and not a bare `COALESCE`: R1 says "sample value if **non-empty**", and a hand-edited DB or a future writer can leave an empty string, which a bare `COALESCE` would hand the pipeline as `''`, shadowing the option set. SQLite has both functions.

It appears in exactly two places: `inst/nextflow/modules/assemble_workflow.nf:12` (the pipeline, D18) and `.mtr_warn_missing_refs()` (the R8 warning, D9). Each site carries a comment naming the other.

### D3. Classifier: `.mtr_ref_class(x)` returns `none | url | accession | file`, and never consults the filesystem

```r
.mtr_acc_re <- "^[A-Za-z]{1,2}_?[0-9]{5,9}(\\.[0-9]{1,3})?$"

.mtr_ref_class <- function(x) {
  v <- trimws(.mtr_opts(x))
  if (!nzchar(v)) return("none")
  if (grepl("^(https?|ftp)://", v, ignore.case = TRUE)) return("url")
  if (grepl(.mtr_acc_re, v)) return("accession")
  "file"
}
```

**Why no `file.exists()` branch.** The same classifier runs twice: on the user's machine at ingest, and inside the container at run time (D17's provenance label). Inside the container the host path does not exist - Nextflow stages the file under a different name - so a filesystem-consulting classifier would label a staged path `"path"` and R5's `file|url|local_db|ncbi` provenance would be unproducible. A pure string classifier gives the same answer on both sides by construction. Cost: a file literally named `NC_002333` in the working directory is read as an accession; the docs say to give an absolute path, and an absolute path starts with `/`, which `.mtr_acc_re` can never match.

**Why this regex.** It matches R4's four examples (`NC_002333`, `NC_002333.1`, `AB123456`, `MN908947.3`) plus one-letter legacy accessions (`U12345`) and `J01415.2`. It rejects `NC_002333.gb` (letters after the dot), `ref/NC_002333.gb` (slash), and any 3+ letter prefix. It is case-insensitive by construction; the validator uppercases before storing.

`.mtr_ref_class()` is **scalar only**, because `.mtr_opts()` takes `x[1]` (`R/map_to_ref.R:396-398`). Vector use goes through `vapply(s, .mtr_ref_class, character(1))`.

Not exported. The repo already calls `.mtr_*` internals directly from tests (`tests/testthat/test-map-to-ref.R:350-353` calls `.mtr_stop`).

### D4. Validator: one internal, every bad value reported at once, per-ID error lines

```r
.mtr_validate_refs(x, ids = NULL, context = "reference") -> normalised chr, or stop()
```

Failures accumulate into a `bad` character vector and are raised in one `stop()` at the end. **No `"!"`-prefixed sentinel and no blanket `tryCatch` around the per-value check**: a genuine error must never be mistaken for a value and written to the database. `.mtr_check_ref_value()` returns `list(ok = TRUE, value = ...)` or `list(ok = FALSE, msg = ...)`, so the two channels cannot be confused.

Error shape (guards' D5, grafted). The header text is the caller's `context` string, so this example is exactly what Task 3's `add_samples()` call site produces:

```
MapToRef reference problems (3) in the mapping file 'Reference' column:
  S1 [/data/refs/a.gb]: file not found
  S2 [https://example.org/y.gb]: not reachable: HTTP 404
  S3 [NC_999999]: no such nucleotide record at NCBI
```

**The lines come out in row order.** The checks run in class batches (characters, then the whole accession batch, then file/url), so appending to `bad` as failures are found would interleave a 96-sample plate unreadably. `bad` is therefore `rep(NA_character_, n)` written at index `i` by `add(i, msg)` and compacted with `bad <- bad[!is.na(bad)]` just before the `stop()`. Two lines, and the error reads down the mapping file.

Per-class checks:

| class | check | on failure |
|---|---|---|
| `none` | none | stored as `NA_character_` |
| any | none of ``["'$`\\]`` anywhere in the value | error (the value is interpolated into `Rscript -e` inside a bash double-quoted string, `assemble.nf:105`, so `$` and a backtick are executed and a backslash escapes) |
| `accession` | uppercase, then ONE batched NCBI existence check over every distinct accession in the call, chunked 200 ids at a time inside `.mtr_ncbi_known()` (D5) | definitively absent = error; request failure = `warning()`, value kept |
| `url` | `https://`/`http://` fetched to a tempfile (D6), then the content check | error naming the URL and the reason |
| `url` (`ftp://`) | refused (D6) | error naming the escape hatch |
| `file` | `dir.exists()` -> "is a directory, not a file"; `!file.exists()` -> "file not found"; `file.access(v, 4) != 0` -> "file is not readable"; then `normalizePath()`; then the content check | error naming the path and the reason |

`dir.exists()` is checked separately because `file.exists()` is TRUE for a directory on Linux.

**Content check, shared by `file` and `url`** - R4 offers "a lighter check; pick the smaller diff", and one call to the existing exported reader is both smaller and stronger (it catches a multi-record MitoFinder db, an HTML error page, a 3 Mb nuclear contig):

```r
.mtr_content_problem <- function(file) {
  d <- tempfile("mtrchk")
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  tryCatch({
    maptoref_prepare_ref(file, topology = "circular", out_dir = d)
    NA_character_
  }, error = function(e) conditionMessage(e))
}
```

`topology = "circular"` is R4's placeholder: it satisfies `.mtr_validate_topology()` (`R/map_to_ref.R:131-140`) so the FASTA-topology rule cannot fire during validation, and it is ignored outright when the GenBank LOCUS line names a topology (`R/map_to_ref.R:94-101`). The reader creates `out_dir/maptoref/` itself (`R/map_to_ref.R:73-74`), and `on.exit(unlink())` removes it, so no stray directories accumulate under `tempdir()`.

**Distinct values are checked once** and the results mapped back to every row that names them, so a 96-sample plate naming three references does three checks - but the error lines still carry the sample ID, because `add(i, msg)` is called per row.

**Rejected:** case-variant column detection ("did you mean `Reference`?"), a non-ASCII refusal, a whitespace rule, and a message-truncation helper. The repo's ASCII rule governs source files, not user data - an accented character in a path is legitimate on a non-English filesystem. Extra mapping columns have always been tolerated (`R/init_db.R:226-233` builds `samples` from whatever columns exist; `inst/test_data/mapping_test_userAsmb.csv` carries `Donors`/`Expected`), so erroring on a column MitoPilot does not own would surprise someone.

### D5. Accession existence: batched `esummary`, short timeout, warning on any request failure

```r
.mtr_esummary_found(txt) -> <chr, version-stripped, uppercase> | character(0) | NULL
.mtr_ncbi_known(accs, timeout = 30L) -> list(ok = TRUE, found = <chr>) | list(ok = FALSE, reason = <chr>)
```

Endpoint `https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi?db=nuccore&retmode=json&id=A,B,C` plus `.blast_ref_api_key_qs()` (`R/blast_ref_utils.R:209-212`), reused verbatim.

**The response shape is measured, not assumed** (`dev/map_to_ref_refs_sdd/probes.md`, section "NCBI esummary is the right existence check"). A mixed batch of `NC_002333,NC_999999999,AB123456` returns **HTTP 200** with a top-level `"error": "Invalid uid NC_999999999 at position= 9"` - which names only ONE bad id even when several are bad, so **the message is never parsed** - plus `result.uids` and one record per good uid carrying `caption` (`"NC_002333"`), `accessionversion` (`"NC_002333.2"`), `title`, `slen`, `topology`, `genome`, `organism`, `taxid`, and `sourcedb`. The found set is diffed against the requested set, so the error names the offending accession. Matching is version-insensitive on `accessionversion`/`caption`, the same normalisation `R/blast_ref_utils.R:459-466` already uses. `jsonlite` is already in Imports (`DESCRIPTION:35`).

**`esummary`, not `efetch rettype=acc`.** Measured (probes.md, "efetch rettype=acc is NOT a usable existence check"): `efetch` returns HTTP 200 with a garbled `Error: ID list does not contain valid IDs or accessions!` body, so it cannot be used for validation at all. `esummary` returns a per-uid record instead.

**The parse is its own pure function**, `.mtr_esummary_found(txt)`, so it can be unit-tested against a recorded body with no network. Three outcomes, all measured: a `result` key -> the version-stripped uppercase accessions in it; an `"esummaryresult"` key (`"Empty id list - nothing todo"`) -> `character(0)`, a **definitive miss**, not an unreadable response; anything that is not an esummary body at all (a proxy HTML page, a 502) -> `NULL`, which is what `.mtr_ncbi_known()` turns into `ok = FALSE` and the warning path.

**Ids are chunked 200 at a time.** A 500-accession plate is a ~6 KB query string; past a few hundred ids the GET exceeds what E-utilities accepts, the non-200 becomes a warning, and the existence check silently stops checking for exactly the projects that most need it. `.mtr_ncbi_known()` therefore loops `split(accs, ceiling(seq_along(accs) / 200L))`, unions the `found` vectors, and returns the first failing chunk's `ok = FALSE` reason if any chunk fails.

**Not `.blast_ref_efetch()` here.** That helper retries five times with a `120s * attempt` backoff (`R/blast_ref_utils.R:216-258`), so an offline `new_project()` would hang for about twenty minutes. Ingest gets a single-shot call per chunk with `req_timeout(30)`, `req_error(is_error = function(r) FALSE)` so a non-200 is data rather than a condition, and no retry. `.blast_ref_efetch()` **is** the right helper at run time (D13), where the wait is unattended.

Three outcomes: found -> silent; definitively absent from a 200 response -> error; anything else (connection error, non-200, unparseable body) -> `warning()`, value stored, resolved at run time. **This is what makes HTTP 429 and 403 a warning instead of a false rejection.**

The local BLAST database is deliberately NOT consulted at ingest: it exists only inside the container image (`docker/Dockerfile:82`), and ingest runs on the user's machine.

### D6. `https://` is fetched and checked; `ftp://` is refused at ingest

`.mtr_url_fetch(url, timeout = 60L)` is one `httr2` request into a tempfile with `req_error(is_error = function(r) FALSE)`, returning `list(ok, file)` or `list(ok = FALSE, reason = "HTTP 404")`. The downloaded bytes then go through `.mtr_content_problem()`. **The stored value is the URL, never the tempfile.**

`ftp://` is refused with an error naming both escape hatches (use the `https://` form of the same host - NCBI serves both - or download the file and give its path). **Why refuse rather than warn-and-pass:** R4 says a URL must be reachable, and a warning-and-pass would make `ftp://` the one class where an unreachable value gets through. **Why refuse rather than check:** the package's entire HTTP surface is `httr2`, which is HTTP-only; adding `utils::download.file()` for one scheme is a second download mechanism for a case whose main real-world provider also serves HTTPS. Cost if wrong: replace the refusal with four lines of `utils::download.file(url, dest, quiet = TRUE, mode = "wb")` inside a `tryCatch`.

### D7. Ingest: `.mtr_take_ref_col()` strips the column, callers validate

```r
.mtr_take_ref_col(mapping, mapping_id = "ID") ->
  list(mapping = <Reference column removed>, refs = <named chr by sample ID, or NULL>)
```

It strips and returns the raw values; it does **not** validate. Validation is one call per ingest site, so `new_db()` can put the option-set value and the whole `Reference` column through a single `.mtr_validate_refs()` call and report every bad value together (R4).

Stripping is not cosmetic: `new_db()` builds `samples` from `colnames(mapping)` (`R/init_db.R:226-233`) and `add_samples()` loops `ALTER TABLE samples ADD COLUMN` over every unseen column (`R/add_samples.R:109-121`), so without the strip a `Reference` column silently becomes a `samples` column. Precedent: userAsmb strips `Assembly`/`Topology` the same way (`R/init_db_userAsmb.R:222`).

Call sites:

- `new_db()` - after the ID checks (`R/init_db.R:186-191`) and **before** `DBI::dbConnect` (`R/init_db.R:216`), so a bad reference never leaves a half-built `.sqlite` behind.
- `add_samples()` - after `validate_declared_topology()` (`R/add_samples.R:59`), before the `Assembly` block at `:72-91` and well before the all-to-character sweep at `:93-95`.
- `update_sample_metadata()` - **strip and message only, no write.** That function's documented contract is metadata (`R/update_sample_metadata.R:1-5`), it touches only the `samples` table (`:116-122`), and it already refuses `R1`/`R2` (`:60-64`) and `Assembly`/`Topology` (`:67-71`) with exactly this shape. The message points at `set_maptoref_refs()`. This is the "treat consistently" R2 asks for: consistent with how the file already treats every column it does not own.

### D8. `set_maptoref_refs(path = ".", refs = NULL)`, exported

`refs` is a CSV path or a data frame; **column 1 is the sample ID, column 2 is the reference, taken by position** (R3: "header names free"). Extra columns are ignored.

Order of checks, cheapest and most fundamental first: `refs` is a readable CSV **file** and not a directory -> shape (>= 2 columns, >= 1 row) -> duplicate IDs -> unknown IDs -> locked samples -> `.mtr_validate_refs()`.

- **`dir.exists()` is tested alongside `file.exists()` on the CSV path**, because `file.exists()` is TRUE for a directory on Linux - the same trap D4 calls out for a reference value. Without it a directory is handed to `utils::read.csv()` and fails with a cryptic internal message.
- **Unknown IDs are an error**, message shape copied from `R/update_sample_metadata.R:80-83` (`sample(s) 'X', 'Y' absent in the existing database`).
- **Locked samples are an error**, not a silent skip. The app refuses every analogous per-sample edit while any selected row is locked: `req(all(rv$data$assemble_lock[selected] == 0))` at `R/app_assemble.R:669`, `:814`, and `:1160`, and the same guard spelled `req(all(rv$data$assemble_lock[req(selected())] == 0))` at `:539`. A helper that wrote through a lock would be strictly less safe than the UI, and writing without flipping the switch would be an invisible no-op.
- **A blank value clears the override** back to SQL NULL, so the option-set default applies again. Free, and the only way to undo a mistake without hand-editing SQLite.
- **`assemble_switch` flips to 1 only for rows whose stored value actually changed** (R3's "for changed rows"), compared NA-safely - `(is.na(new) & is.na(old)) | (!is.na(new) & !is.na(old) & new == old)`, never `%||%`, which is a length-0 test (`R/utils.R:7-13`) and yields NA on a first-time set.
- Write in one statement, copying the app's idiom at `R/app_assemble.R:1091-1098`: `dplyr::rows_update(..., unmatched = "ignore", in_place = TRUE, copy = TRUE, by = "ID")`.
- Returns `invisible(<IDs still without a reference>)` - the value `.mtr_warn_missing_refs()` already computes, so the caller can act on it in a script.

**No DB backup.** `add_samples()` (`R/add_samples.R:136-147`) and `update_sample_metadata()` (`R/update_sample_metadata.R:85-96`) each carry their own inline copy of an eleven-line backup block; there is no shared helper, and `add_samples()` writes `samples` at `:127-134` *before* it backs up at `:136-147`, so the convention is not even uniform. A third copy to protect two columns of one table, reversible by re-running with the old values or with blanks, is bloat. Do **not** "fix" this by extracting the duplicate into a helper and rewiring three callers: that is a separate cleanup with its own regression risk.

### D9. `.mtr_warn_missing_refs(con)` is the single source of R8's warning

R8 replaces the hard stop at `R/init_db.R:160-162` with a warning naming the samples that have no reference from either source. That cannot be answered from the mapping file alone, so it is a query against the committed database:

```sql
SELECT a.ID FROM assemble a
JOIN assemble_opts o ON a.assemble_opts = o.assemble_opts
WHERE o.assembler = 'MapToRef'
  AND COALESCE(NULLIF(TRIM(a.maptoref_ref), ''), NULLIF(TRIM(o.maptoref_ref), '')) IS NULL
```

Called at the end of `new_db()`, at the end of `add_samples()`, and at the end of `set_maptoref_refs()`. This makes the warning DB-truth and closes the hole a mapping-only check would leave in `add_samples()`.

**Two early returns, both mandatory.**

1. **userAsmb.** `R/init_db_userAsmb.R:363-371` creates a minimal `assemble_opts` with only `assemble_opts`, `min_assembly_length`, and `join_scaffolds` - there is no `assembler` column, so the query would raise. Guard with `if ("assembler" %nin% DBI::dbListFields(con, "assemble_opts")) return(invisible(character(0)))`.
2. **An un-migrated project.** The query also names `a.maptoref_ref`, which does not exist before D12's ALTER runs, so it would raise `no such column: a.maptoref_ref`. Guard with `if ("maptoref_ref" %nin% DBI::dbListFields(con, "assemble")) return(invisible(character(0)))`. This function must never be the thing that breaks an old project; the loud, actionable failure belongs to the caller (`add_samples()`, D7/Task 3), which stops with the exact command to run.

### D10. `new_db()` validation relax, and the three existing tests it breaks

Four edits in `R/init_db.R:157-175`, in place:

1. **Delete** the hard stop at `:160-162` (`stop("MapToRef requires a reference mitogenome; set maptoref_ref")`). Ruling A19 is superseded by R8.
2. **Guard** the FASTA-topology check at `:167-172` on a non-empty reference. This is the single easiest thing in the task to get wrong: `trimws(NA_character_)` is `NA`, `grepl(p, NA)` is `FALSE`, so `!grepl(...)` is `TRUE`, and the moment `:160-162` is deleted, `new_db(assembler = "MapToRef")` with no reference starts demanding a topology instead of warning. **Task 2 has a test for exactly that call.**
3. **Narrow** the same check so it does not fire for an accession-shaped value. An accession resolves either to a GenBank record (LOCUS wins, `R/map_to_ref.R:94-101`) or to a local-DB FASTA (D16 defaults it to circular), so demanding a topology for it is wrong.
4. **Validate** the option-set value together with the mapping column in one `.mtr_validate_refs()` call, and store the normalised result, so the seed row at `R/init_db.R:396` gets an absolute path.

**Ordering matters.** The topology check (`:163-172`) and the quote check (`:173-175`) stay ABOVE the new validation call, which lives after the ID checks at `:191`. That keeps their error messages firing first and keeps three of the four existing expectations working unchanged.

**Why `new_db()` and not `new_project()`.** R8 says "at new_project", but `new_project()` reaches `new_db()` only through `...` (`R/init_project.R:62`, call at `:125-133`), so inspecting the value there means fishing it out of `list(...)`. `new_db()` is the single choke point that already reads the mapping file (`R/init_db.R:143`), and one validation call over both sources is what makes R4's "list every bad value at once" true across them. User-visible behaviour through `new_project()` is identical.

**Cost: three existing test blocks in `tests/testthat/test-map-to-ref.R` must be repaired in Task 3.**
- `:367-383` passes `maptoref_ref = "ref/NC_002333.gb"`, which does not exist -> repoint at `mtr_fixture()` and expect the normalised absolute path back.
- `:385-393` first expectation `expect_error(..., "maptoref_ref")` -> becomes `expect_warning(..., "no reference")`. Its second expectation (bad topology) still fires at `:163-166`, unchanged.
- `:395-415` three failing cases still fail at `:167-175` before validation, but the `expect_no_error()` at `:411-414` passes `"ref/mito.fasta"`, which does not exist -> write a real FASTA into the tempdir and point at it.

The vignette snippet at `vignettes/Your-Own-Project.Rmd:202-210` becomes an error for anyone who copies it without the file. That is correct behaviour and the prose says so (Task 8).

### D11. Modal relax (R8), and nothing else in the app

- **Delete** the "Reference required" `show_alert` block whole (`R/app_assemble.R:1007-1016`). Keep `ref_value <- trimws(input$maptoref_ref %||% "")` at `:1005` and `topology_value` at `:1006`: both are still read at `:1018`, `:1020`, `:1061`, and `:1073`.
- **Skip** the topology alert (`R/app_assemble.R:1017-1031`) when the value is accession-shaped: add `!identical(.mtr_ref_class(ref_value), "accession") &&` to the `needs_topology` condition at `:1017-1020`. Without this an option-set accession cannot be saved through the modal.
- **Extend** the EXISTING quote alert (`R/app_assemble.R:1032-1044`) to cover `ref_value`, with the same forbidden-character set the validator uses. Before this plan the modal's reference was only ever staged as a Nextflow `path(ref)`; Task 7 Step 5 makes it a raw string inside the `Rscript -e "..."` line at `assemble.nf:105`, so a reference typed as `/data/it's/ref.gb` closes the R string, the task exits non-zero, `errorStrategy ... 'ignore'` (`assemble.nf:10`) plus `workflow.failOnIgnore = true` (`inst/config.local:102`) end the whole run non-zero - triggered by typing in the app. One extended condition and one reworded alert, not a new block.
- Help text and label at `R/app_assemble_utils.R:376-388` gain the accession form and the local-DB-first rule.
- **No reader call and no network call on save.** Ruling A3 (save-time validation is topology-only) stands. A Shiny save handler must not block on an HTTP round trip or on reading a path that exists only on the compute node; a typo'd accession fails per-sample under the R6 contract with a clear message.
- No per-sample editing UI, no new table column (R8).

### D12. Migration (R7): four touch points, all ungated

Copy the `synteny_accession` block (`R/backwards_compatibility.R:1499-1503`) - the smallest pattern in that file, because the column needs no backfill:

```r
  # per-sample MapToRef reference (NULL falls back to the option-set value)
  if (!("maptoref_ref" %in% DBI::dbListFields(con, "assemble"))) {
    message("added 'maptoref_ref' column to assemble table")
    DBI::dbExecute(con, "ALTER TABLE assemble ADD COLUMN maptoref_ref TEXT")
  }
```

**No backfill.** NULL is the correct value for every existing row: D2's `COALESCE` makes NULL mean "use the option set", which is exactly what those projects do today. (The five `assemble_opts` MapToRef blocks at `:1322-1414` backfill because four of those columns carry defaults; `maptoref_ref` there backfills `NA_character_` at `:1325`, i.e. no default either.)

Plus:
- **The "already current" predicate must gain the clause or the block never runs.** Next to `"synteny_accession" %in% names(assemble_table)` at `R/backwards_compatibility.R:252` (`assemble_table` is read at `:90`). Without it, a project that satisfies every other clause short-circuits to "nothing to update" and never reaches the ALTER.
- The roxygen migration list, the `\item \code{assemble}:` bullet at `R/backwards_compatibility.R:21-24`, gains `"maptoref_ref"` (R10).
- **`schema_gaps()` gains an entry** beside the `assemble_opts` MapToRef one at `R/backwards_compatibility.R:2275-2280`, ungated (D1 mirrors the column into both DDLs). **This one is not cosmetic.** Migration is not automatic (`R/app_server.R` opens a project normally when `schema_gaps()` is empty), and `assemble_workflow.nf:12` selects `a.maptoref_ref` for every project regardless of assembler, so without the gap entry an un-migrated project does not degrade - its next WF1 run dies at channel creation with a raw "no such column" error. With it, the app tells the user to run `backwards_compatibility()`.

### D13. The resolver: `maptoref_fetch_accession()`, exported, one `blast_db` prefix argument

**Amended 2026-09-04:** no `blast_db` argument; the resolver always fetches from NCBI.

```r
#' @export
maptoref_fetch_accession(accession, out_dir = ".", blast_db = NULL, log_fn = NULL)
  -> list(file = <path>, source = "local_db" | "ncbi", accession = <uppercased>)
```

Exported because R5 says so and because it is useful standalone (pre-fetch a reference by hand). Lives in the new `R/map_to_ref_refs.R` next to the classifier; `R/map_to_ref.R` is already 733 lines and the repo splits by feature.

Local arm: **three lines of guard and exactly ONE `blastdbcmd` call**, every step non-fatal (it falls through to NCBI).
1. `blast_db` blank -> skip. Reachable, and deliberately so: see the `NULL`-only default below.
2. `Sys.which("blastdbcmd") == ""` -> log, skip.
3. `blastdbcmd -db <blast_db> -entry <acc> -outfmt %f`, with stdout redirected to the output file and stderr redirected to a tempfile. **Exit status plus a non-empty output file decide.** Non-zero, or an empty file, means "not here": the captured stderr goes to the assembler log and the caller falls through to NCBI.

**No version-stripped retry and no `-info` probe.** Both were written against an unknown; the unknown is now measured (`dev/map_to_ref_refs_sdd/probes.md`, section "blastdbcmd inside mitopilot:1.5.5"): against the bundled `-parse_seqids` database (built by `tools/build_local_blast_db.py:387`, `tools/dedup_local_blast_db.py:165`), `-entry NC_002333` (bare), `-entry NC_002333.2` (versioned), and `-entry nc_002333` (lowercase) **all** resolve to the same record with exit 0, and a missing entry gives exit 1, an empty output file, and `Error: [blastdbcmd] Entry not found: ...` on stderr. One call covers every form a user can type, and the `-info` probe adds a second process launch that tells the caller nothing `-entry`'s own exit status does not: an unreadable database fails `-entry` too, and the fall-through is identical. (`blast_genbank.nf:213` probes with `-info` because it then runs `blastn`, which has no per-query fall-through; here the very next thing is the NCBI arm.)

Then NCBI: `efetch db=nuccore&rettype=gb&retmode=text` through `.blast_ref_efetch()` (`R/blast_ref_utils.R:216-258`), which brings the 429/500/503 retry ladder and API-key redaction for free. `rettype=gb` and not `fasta` because the LOCUS line carries the topology, so an NCBI-sourced reference needs no topology guess at all (probes.md confirms this is the fetch to use). Neither source usable -> `stop()`.

**One `blast_db` argument, not `db_dir` + `db_name`.** `blastdbcmd -db` takes the full prefix, which is exactly how `blast_genbank.nf:213` composes it (`"${BLASTDB}/!{db_name}"`). Two arguments would mean two more positional slots in an already long `Rscript -e` call. The container default `/ref_dbs/mito_metazoa/mito_metazoa` is substituted **only when `blast_db` is `NULL`**, belt-and-braces alongside the in-process Nextflow default and for the same reason `blast_genbank.nf:79-83` defaults there rather than requiring the `.config` key. An explicit `""` therefore still means "skip the local arm", which is what makes step 1 live code rather than dead code inviting a later "fix".

### D14. `map_to_ref()` calls the resolver, not the shell branch

This is the load-bearing choice for R6. A separate `Rscript -e` in the shell branch that failed would exit non-zero; `assemble` has `errorStrategy { task.exitStatus in 137..140 ? 'retry' : 'ignore' }` (`inst/nextflow/modules/assemble.nf:10`) and every config sets `workflow.failOnIgnore = true` (`inst/config.local:100-103`, assignment at `:102`), so the run exits non-zero and that sample drops out of the channel. Calling the resolver inside `map_to_ref()` puts it under the existing `tryCatch -> .mtr_fail()` contract (`R/map_to_ref.R:374-386`, `.mtr_fail` at `:476-483`), which writes `<ID>_assembly_0.fasta`, a `failure=` summary, and exits 0. That is R6 for free, with zero new Nextflow error handling, and it lets the same function decide the resolved topology (D16) and write the provenance (D17).

### D15. `map_to_ref()` gains two trailing arguments, and an empty reference is a per-sample failure

```r
map_to_ref(id, ref, reads_1, reads_2,
           bowtie2_opts = "--very-sensitive-local",
           consensus_opts = "-d 3 --min-BQ 20",
           iter_cap = 5, topology = NA_character_, genetic_code = NA_integer_,
           cpus = 4, out_dir = ".",
           ref_value = NA_character_, blast_db = NULL)
```

Both defaulted and both forwarded to `.mtr_assemble()`, so every existing caller keeps working. The call in `assemble.nf:105` goes from eleven arguments to thirteen.

**The empty-reference guard is conditional, never unconditional:**

```r
  src <- .mtr_ref_class(ref_value)
  if (identical(src, "none")) {
    # Nextflow stages assets/NO_FILE (0 bytes) when no reference is set anywhere.
    # A legacy direct caller passes a real path and no ref_value, so a real file
    # still counts as a reference.
    if (!file.exists(ref_file) || file.size(ref_file) == 0L) {
      stop("no MapToRef reference for this sample; set one in the mapping file ",
           "'Reference' column, with MitoPilot::set_maptoref_refs(), or in the ",
           "Assemble options")
    }
    src <- "file"
  }
```

An unconditional `stop()` on `src == "none"` would break all 14 `map_to_ref()` calls in `tests/testthat/test-map-to-ref-loop.R` (e.g. `:144-148`), which pass a real reference and no `ref_value`, and would break legacy direct callers, which the Global Constraints forbid. `inst/nextflow/assets/NO_FILE` is 0 bytes (verified: `wc -c` = 0); that is what the size test depends on, and the comment says so.

### D16. Topology rule, stated in full

**Amended 2026-09-04:** the `local_db` FASTA row is gone; an accession always takes its topology from the downloaded GenBank LOCUS line.

| source | topology |
|---|---|
| GenBank file/URL whose LOCUS names a topology | LOCUS wins (`R/map_to_ref.R:94-101`) |
| GenBank with no LOCUS token | `maptoref_topology`, required (existing ruling, `R/map_to_ref.R:101`) |
| FASTA file/URL | `maptoref_topology`, required (`R/map_to_ref.R:131-140`) |
| accession -> local BLAST DB (bare FASTA) | `maptoref_topology` if set, else **`"circular"`** (R5) |
| accession -> NCBI (`rettype=gb`) | LOCUS wins, so effectively always resolved |

Circular is safe for the local DB because it holds 134,560 *complete* metazoan mitogenomes with no topology field to read, and the junction-depth check downgrades to linear when no reads span the seam (`.mtr_junction_depth()`, `R/map_to_ref.R:445-465`); end trimming is gated on the *published* topology (existing ruling). `maptoref_topology` stays an option-set column: it is only ever needed for the FASTA case, and the accession path resolves topology by itself in both of its rows.

### D17. Provenance: one summary key, one log addition, never a `note=`

**Amended 2026-09-04:** the label set is `file|url|ncbi`; `local_db` is gone.

- Summary (`R/map_to_ref.R:706-726`) gains one line after `accession=` (`:708`): `reference_source=file|url|local_db|ncbi`. The accession is already there at `:708`, read from the record itself, which is R5's "and accession".
- Log: the existing reference line (`R/map_to_ref.R:496-497`) gains a `source=<file|url|local_db|ncbi>` field, and the resolver logs its own attempts through the same `log_fn`. **The line is spelled ONE way everywhere in this plan** - `reference <accession> <organism> (<len> bp, <topology>, source=<src>)`, so it matches `reference .* source=(file|url|local_db|ncbi)\)$`. Never `[source=...]`, and the field is not the last thing on the line (the closing parenthesis is).
- **Deliberately not a `note=` line.** The fold at `assemble_workflow.nf:299-311` picks up only lines starting with `note=` and writes them into `assemble_notes` tagged `[maptoref] `. Provenance is not a warning; emitting it as `note=` would stamp a note on every single MapToRef sample and bury the real warnings (divergence, >50% N) that the fold exists for. R5 says summary and log only, and one key satisfies that with zero change to the fold, zero change to `params.sqlWriteAssemble`, and no second `sqlInsert` (the racing-operators bug class).
- **One key, not two.** R5 asks for source and accession; a `reference_input=` line echoing the raw value would be a third spelling of the same thing. Nothing parses `<ID>_summary.txt` except that fold, so a new non-`note=` key cannot leak into `assemble_notes`.

### D18. Nextflow: COALESCE in place, one Groovy helper, one new opts-map key

**Select.** `assemble_workflow.nf:12` becomes the D2 expression. `it[19]` is still the effective reference and `it[20..23]` do not move. Verified: `grep -n "it\[19\]"` over that file returns exactly one hit, `:123`. The second `.cross(assemble_opts)` at `:398` reads only `it[0][0]` (`:399-407`), so a longer opts tuple cannot disturb it, and `params.sqlRead` is already a per-sample query joined by ID (`:14-18`, `.cross` at `:189`), so a per-sample column rides through for free.

**One script-level helper**, the module idiom. The precedent that matters is a script-level `def` **called from inside a channel-operator closure**, which is where `maptorefAccession()` is used (the `.multiMap { }` at `assemble_workflow.nf:100-124`): `inst/nextflow/modules/circularize_workflow.nf:50` (defined) called at `:70` inside a `multiMap`/`branch` closure, and `inst/nextflow/modules/scaffold_join_workflow.nf:87` called at `:223` inside a `map` closure. (`blast_genbank_workflow.nf:7`/`:18` are script-level `def`s too, but they are only ever called from script-level string interpolation at `:75`/`:81`, which is a different resolution context and does not demonstrate the risky part.)

```groovy
// An NCBI nucleotide accession is resolved inside the task, not staged as a file.
// Keep this pattern identical to .mtr_acc_re in R/map_to_ref_refs.R.
def maptorefAccession(v) {
    def s = (v ?: '').toString().trim()
    s ==~ '(?i)^[A-Z]{1,2}_?[0-9]{5,9}(\\.[0-9]{1,3})?$' ? s.toUpperCase() : ''
}
```

A single-quoted Groovy string, not a slashy regex literal, so no `$` can be read as interpolation. `==~` is a full match, so no anchors are needed, but they are kept for readability against the R copy.

**Staged file** (`assemble_workflow.nf:123`, same element, same position): an accession diverts to the sentinel.

**One new opts-map key**, `maptoref_value`, appended after `maptoref_topology` (`:113`).

**Deliberate deviation from R5's wording, flagged.** R5 says "New process input(s) are APPENDED to the assemble tuple". This plan puts the raw value in the existing `opts` map instead. It delivers the same guarantee - no existing index moves - with a strictly smaller diff (no change to `assemble.nf:16`, none to the `.cross` map at `assemble_workflow.nf:190-203`, none to the `multiMap` tuple arity), and it follows the precedent set by this very feature: `maptoref`, `maptoref_consensus`, `maptoref_iter`, and `maptoref_topology` all travel as opts-map keys (`assemble_workflow.nf:110-113`), not as tuple elements. **Literal fallback if the maintainer prefers R5's wording:** append `val(maptoref_value)` as element 11 of the input tuple at `assemble.nf:16`, element 10 of the opts tuple at `assemble_workflow.nf:123`, and element 11 of the `.cross` map after `:201`; three more edited lines, identical behaviour.

**Why the raw value and not a pre-classified source string.** Nextflow stages a URL by downloading it, so the task sees a local path either way; passing the raw value lets `map_to_ref()` reuse `.mtr_ref_class()` to tell `file` from `url` for the provenance label. One value, one classifier, both sides.

**`assemble.nf`**: two shell-preamble variables (the db prefix and its directory, defaulted in-process exactly the way `blast_genbank.nf:79-83` does it), and inside the MapToRef branch `export BLASTDB` (R5 names it; both existing local-DB consumers set it, `blast_genbank.nf:212` and `find_mito.nf:30`), `export NCBI_API_KEY` (precedent `blast_ref_fetch.nf:84`), and the two extra arguments on the `Rscript -e` line at `:105`.

**Rejected:** three Groovy helpers where one does. `mtrRefValue` would hide a normalisation (uppercasing) that R redoes anyway, and `mtrRefPath` is a ternary.

### D19. Tests (R9)

New file `tests/testthat/test-map-to-ref-refs.R`, plus edits to two existing files.

- Classifier: a table of values against expected classes, covering every regex edge in D3. Pure, no network, no skip.
- Validator, offline classes: missing file, directory, unreadable file (`Sys.chmod("0000")`, `skip_on_os("windows")`), wrong content, quote character, relative-path normalisation, and the "every problem at once" assembly.
- Parser: `.mtr_esummary_found()` against three recorded bodies (a hit, an `"esummaryresult"` empty-id-list miss, a non-esummary body). Pure, no network, no mock - this is the one piece of new response-parsing logic in the plan and the mocks everywhere else would otherwise leave it untested.
- Validator, network classes: `testthat::local_mocked_bindings()` on `.mtr_ncbi_known` and `.mtr_url_fetch`. That is the repo idiom (`tests/testthat/test-circularize-asmb.R:133`, `tests/testthat/test-scaffold_join.R:909`). Three accession cases: found, definitively absent (error), request failure (warning, value kept).
- `new_db()` seeding from the `Reference` column; the D10 warning; the "no topology demanded when there is no reference" regression test; the three repaired blocks in `tests/testthat/test-map-to-ref.R`.
- `set_maptoref_refs()`: happy path from a data frame and from a CSV, unknown ID, duplicate ID, locked sample, blank clears, unchanged row does not flip `assemble_switch`.
- Migration: `"maptoref_ref"` added to the `expect_cols(con, "assemble", ...)` lists at `tests/testthat/test-backwards-compatibility.R:465-467` and `:525-527`.
- Resolver: a stub `blastdbcmd` on `PATH`, exercising the one `-entry` call, an explicit empty `blast_db` (local arm skipped), a miss falling through to NCBI, and neither source resolving. **Extend the existing `mtr_stub_bin()`** (`tests/testthat/test-map-to-ref-loop.R:3-63`, `Sys.chmod` at `:61`) with a `blastdbcmd` case driven by `MTR_STUB_ACC` / `MTR_STUB_FASTA`, rather than writing a second inline stub for Task 6. `mtr_summary()` (`:132-136`) already parses `key=value`, so `reference_source` needs no new helper.
- Regression: a **GetOrganelle** project whose mapping carries a `Reference` column is created successfully, stores the value in `assemble.maptoref_ref`, and grows no `samples.Reference` column (D7: `Reference` is reserved for every assembler). And an `add_samples()` call against a project whose `assemble.maptoref_ref` has been dropped errors with `backwards_compatibility` and writes nothing.
- Any test whose fixture project itself warns (a MapToRef project with no reference) wraps the fixture call in `suppressWarnings()`. testthat 3e counts an uncaught `warning()` inside `test_that()` in the WARN column, and the suite gate below is `WARN 0`.
- **No `skip_if_offline()`.** It calls `curl::has_internet()`, and `curl` is not in Suggests (`DESCRIPTION:65-68` lists `knitr`, `testthat`, `withr` only); nothing in the suite uses it today. Everything network-shaped is mocked.
- `NXF_VER=25.10.6 nextflow lint` on the two touched `.nf` files, judged by **baseline equivalence plus one expected warning**. Neither file lints clean today: `assemble.nf` has three warnings (launchDir `:8`, deprecated `shell:` `:22`, `outDir` unused `:24`) and `assemble_workflow.nf` a pre-existing error (`n++` at `:243`). The edit adds exactly one new warning, `mtr_db` declared but not used, because the linter cannot see `!{mtr_db}` inside the `'''` shell string. Anything else new is real.
- Suite target: FAIL 0 | WARN 0.

### D20. Docs (R10)

roxygen for `set_maptoref_refs()` and `maptoref_fetch_accession()`; `@param maptoref_ref` at `R/init_db.R:56-58` rewritten to say path/URL/accession and to drop "Required"; the `mapping_fn` roxygen at `R/init_project.R:5-9` and `R/add_samples.R:8` gain the optional `Reference` column **and say it is a reserved name, validated whatever the assembler**; `_pkgdown.yml` gains `set_maptoref_refs` after `update_sample_seqdata` (`:88`) and `maptoref_fetch_accession` after `maptoref_prepare_ref` (`:156`); `NEWS.md` under the existing `# MitoPilot 1.5.5` / `### Map-to-reference assembly` heading (`NEWS.md:1-14`); `README.Rmd:139-141` **and** `README.md:167-170` edited by hand with matching text (ruling C-1: README.md is generated but is edited by hand here to avoid a knit reflow); `vignettes/Your-Own-Project.Rmd:194-210`, `vignettes/custom_dbs.Rmd:149-168`, and `vignettes/Test-Project-Assemble.Rmd:172-174` (the "MapToRef Reference" modal-field bullet, which describes the very field Task 8 relabels). ASCII check is diff-scoped (ruling C-12).

### D21. End-to-end (R11)

**Amended 2026-09-04:** three provenance labels to cover, not four; the `local_db` sample row no longer applies.

`bash docker/deploy-local.sh 1.5.5`. **Drop the brief's manual "delete stale `MitoPilot_*.tar.gz` first" step**: the script already runs `rm -f docker/MitoPilot_*.tar.gz` at `docker/deploy-local.sh:23`, and `:13-18` fails early if the BLAST tarball is missing. The rebuild is mandatory because `map_to_ref()` and the resolver run *inside* the image; an e2e run against a stale image would raise `unused arguments` on the new named parameters and the task would exit non-zero, so nothing is silent, but nothing is tested either.

**ONE** WF1 run on a fresh **four**-sample project (built the way `dev/map_to_ref_e2e/setup2.R` built its: shipped subsampled reads copied into `<proj>/data/`, `min_depth = 500`, `executor = "local"`, `container = "macguigand/mitopilot:1.5.5"`), with an option set whose `maptoref_ref` is **empty** and four per-sample `Reference` values covering all four provenance labels:

| sample | `Reference` | expected `reference_source` |
|---|---|---|
| 1 | a copy of `inst/test_data/NC_002333_Danio_rerio.gb` inside the project | `file` |
| 2 | `NC_002333` (in the bundled local BLAST database) | `local_db` |
| 3 | `NC_001638` (Chlamydomonas reinhardtii mitochondrion, 15,758 bp, **not** metazoan and **not** in the local database) | `ncbi` |
| 4 | `https://raw.githubusercontent.com/Smithsonian/MitoPilot/main/ref_dbs/MitoFinder/NC_002333_Danio_rerio.gb` | `url` |

**The NCBI arm is forced with a real non-metazoan accession, never with a fake one and never with a bogus `params.blast_gb.db_dir`.** Ingest validation (D5) rejects a fake accession before the run starts. And `params.blast_gb.db_dir` is **shared**: `blast_genbank.nf:82` reads the same key and degrades to a remote NCBI search when the database is unreadable, but only when `remote_fallback` is on (`:86-87`); on a project with `remote_fallback = 0` the `blast_genbank` task fails, `workflow.failOnIgnore = true` (`inst/config.local:102`) fires, and the run ends `ERR` - which would contradict the run's own pass condition. `NC_001638` is measured absent from the bundled database (`dev/map_to_ref_refs_sdd/probes.md`), so the local `-entry` misses with the real `db_dir` in place and the NCBI arm runs for that one sample only. It is also **LINEAR in GenBank**, so it exercises the LOCUS-wins topology path at the same time; being non-metazoan it will produce a graceful per-sample failure or a heavily divergent product, which is R6 working, not a defect.

Sample 4 is the one path no unit test can reach: `.mtr_url_fetch` is mocked everywhere, and Task 6's URL test proves only the provenance **label**, not that `file(it[19])` in `assemble_workflow.nf:123` stages an `https://` value that `maptorefAccession()` correctly declines to divert.

### D22. The `-resume` trap, documented in one sentence

A MapToRef failure exits 0 by design (R6), so Nextflow caches the task as a success. Repairing a *transient* NCBI outage with the same accession does not change the task hash, so `-resume` replays the cached failure and the user must untick resume (`R/app_run_pipline.R:262-272`). Repairing by *changing* the reference does change the hash and re-runs correctly. One sentence in the vignette; no code.

**A second, one-time cache effect, for every assembler.** D18 appends `maptoref_value` to the options map at `assemble_workflow.nf:110-113`, and that map is an input `val` on the `assemble` process (`assemble.nf:16`), so it is part of every assemble task's hash - GetOrganelle and MitoFinder tasks included, where the new key is always `""`. Upgrading therefore changes the assemble task signature: the first run after the upgrade re-runs any **still-queued** assemble task even with `-resume`. Samples already at state 2 are unaffected, because they never enter the channel (`assemble_workflow.nf:20-21`). One sentence in NEWS (Task 8 Step 5); no code.

**Rejected alongside it:** adding `cache 'lenient'` to the `assemble` process. `blast_genbank.nf` can afford it because its query FASTA is regenerated every run; a user's reference file changes mtime exactly when a re-run is wanted.

---

## File Structure

**Created:**

- `R/map_to_ref_refs.R` - the whole feature outside the pipeline: `.mtr_ref_class`, `.mtr_validate_refs`, `.mtr_check_ref_value`, `.mtr_content_problem`, `.mtr_esummary_found`, `.mtr_ncbi_known`, `.mtr_url_fetch`, `.mtr_take_ref_col`, `.mtr_warn_missing_refs`, `.mtr_log_if`, `.mtr_blastdbcmd`, `.mtr_efetch_gb`, exported `set_maptoref_refs()` and `maptoref_fetch_accession()`. One file because these are only ever used together, and `R/map_to_ref.R` is already 733 lines.
- `tests/testthat/test-map-to-ref-refs.R` - unit tests for everything in that file.

**Modified:**

- `R/init_db.R` - the `assemble` DDL column, the assemble seed, the relaxed/narrowed validator, the ingest call, the R8 warning, one roxygen `@param`.
- `R/init_db_userAsmb.R` - the mirrored `assemble` DDL column.
- `R/add_samples.R` - the ingest call, the seeded column, the R8 warning, one roxygen line.
- `R/update_sample_metadata.R` - the `Reference` strip-and-message block.
- `R/backwards_compatibility.R` - the ALTER block, the "already current" predicate clause, the roxygen migration list, the `schema_gaps()` entry.
- `R/map_to_ref.R` - two new `map_to_ref()` arguments and their roxygen, the same two on `.mtr_assemble()`, the `ref_value` shell-character refusal beside the bowtie2 check, the source resolution at the head of `.mtr_assemble()`, the log line, one summary line.
- `inst/nextflow/modules/assemble_workflow.nf` - the COALESCE select, the `maptorefAccession` helper, the `maptoref_value` opts key, the staged-file expression.
- `inst/nextflow/modules/assemble.nf` - two shell-preamble variables, two `export`s and two arguments in the MapToRef branch.
- `R/app_assemble.R` - the deleted "Reference required" alert, the accession exemption on the topology alert, the reference added to the existing quote alert.
- `R/app_assemble_utils.R` - the reference field's label and help text.
- `tests/testthat/test-map-to-ref.R` - three repaired blocks.
- `tests/testthat/test-map-to-ref-loop.R` - the `blastdbcmd` stub and the accession end-to-end case.
- `tests/testthat/test-backwards-compatibility.R` - two `expect_cols` lists.
- `NAMESPACE`, `man/` - regenerated by `devtools::document()`.
- `_pkgdown.yml`, `NEWS.md`, `README.Rmd`, `README.md`, `vignettes/Your-Own-Project.Rmd`, `vignettes/custom_dbs.Rmd`, `vignettes/Test-Project-Assemble.Rmd` - docs.

---

### Task 1: Reference classifier, validator, and the network probes

Every later task consumes this one. A reference value crosses a trust boundary twice - once when the user types it (mapping CSV, helper CSV, modal) and once when the pipeline resolves it inside the container - so the string classifier has to give the same answer on both sides. This task builds the classifier, the per-class checks, the two single-shot HTTP probes, and the aggregate error, with no caller anywhere. That makes it provably safe: the existing suite cannot change, because nothing existing calls any of it.

Reference facts this task depends on (verified 2026-09-03 at HEAD `e93c403`):
- `maptoref_prepare_ref(ref_file, topology, genetic_code, out_dir)` is exported at `R/map_to_ref.R:18-21`. It stops on a missing file (`:22-24`), an empty file (`:27-29`), content that is neither LOCUS nor `>` (`:38-41`), invalid characters (`:43-46`), a length outside 5,000-50,000 bp (`:48-51`), and more than one GenBank record (`:86-89`). It creates `out_dir/maptoref/` itself with `dir.create(recursive = TRUE)` (`R/map_to_ref.R:73-74`).
- `.mtr_validate_topology()` (`R/map_to_ref.R:131-140`) stops when the topology is NA or empty, which is why the content check passes the `"circular"` placeholder.
- `.mtr_opts(x)` (`R/map_to_ref.R:396-398`) turns `NULL`, length-0, and `NA` into `""` and is **scalar only** (`x[1]`).
- `.blast_ref_api_key_qs()` (`R/blast_ref_utils.R:209-212`) returns `""` or `&api_key=...`.
- `%nin%` is `R/utils.R:33`; `%||%` is `R/utils.R:7-13` and is a **length-0** test, not an NA test.
- `jsonlite` is in Imports (`DESCRIPTION:35`); `httr2` is in Imports (`DESCRIPTION:64`); `withr` and `testthat` are Suggests only (`DESCRIPTION:65-68`), and `curl` is absent, so `skip_if_offline()` must not be used.
- The mocking idiom is `testthat::local_mocked_bindings()` (`tests/testthat/test-circularize-asmb.R:133`, `tests/testthat/test-scaffold_join.R:909`).

**Files:**
- Create: `R/map_to_ref_refs.R`
- Create: `tests/testthat/test-map-to-ref-refs.R`

**Interfaces:**
- Consumes: `maptoref_prepare_ref()`, `.mtr_opts()`, `.blast_ref_api_key_qs()`.
- Produces:
  - `.mtr_acc_re` - the accession regex constant.
  - `.mtr_bad_chars_re` - the forbidden shell-character class, ``"[\"'$`\\\\]"``.
  - `.mtr_ref_class(x)` -> `"none" | "url" | "accession" | "file"`; scalar.
  - `.mtr_esummary_found(txt)` -> the version-stripped uppercase accessions in an esummary 200 body, `character(0)` for a definitive miss, `NULL` when the body is not an esummary response.
  - `.mtr_ncbi_known(accs, timeout = 30L)` -> `list(ok = TRUE, found = <chr, version-stripped, uppercase>)` or `list(ok = FALSE, reason = <chr>)`.
  - `.mtr_url_fetch(url, timeout = 60L)` -> `list(ok = TRUE, file = <tempfile>)` or `list(ok = FALSE, reason = <chr>)`.
  - `.mtr_content_problem(file)` -> `NA_character_` when the file is a usable reference, else the reader's message.
  - `.mtr_check_ref_value(v, cls)` -> `list(ok = TRUE, value = <normalised>)` or `list(ok = FALSE, msg = <chr>)`.
  - `.mtr_validate_refs(x, ids = NULL, context = "reference")` -> normalised character vector the same length as `x`, `NA_character_` where the value was blank; `stop()` listing every bad value **in row order**; `warning()` when NCBI could not be reached.
  Tasks 3, 4, 6, and 8 all consume these. (Task 5 consumes nothing from this task.)

- [ ] **Step 1: Write the failing tests**

Create `tests/testthat/test-map-to-ref-refs.R`:

```r
mtr_ref_fasta <- function(dir, name = "ref.fasta", reps = 3000L) {
  fn <- file.path(dir, name)
  writeLines(c(">TESTREF", strrep("ACGT", reps)), fn)
  fn
}

test_that(".mtr_ref_class separates urls, accessions, paths, and blanks", {
  expect_equal(.mtr_ref_class("https://example.org/ref.gb"), "url")
  expect_equal(.mtr_ref_class("http://example.org/ref.gb"), "url")
  expect_equal(.mtr_ref_class("ftp://ftp.ncbi.nlm.nih.gov/x.gb"), "url")
  expect_equal(.mtr_ref_class("NC_002333"), "accession")
  expect_equal(.mtr_ref_class("NC_002333.1"), "accession")
  expect_equal(.mtr_ref_class("nc_002333.1"), "accession")
  expect_equal(.mtr_ref_class("AB123456"), "accession")
  expect_equal(.mtr_ref_class("MN908947.3"), "accession")
  expect_equal(.mtr_ref_class("U12345"), "accession")
  expect_equal(.mtr_ref_class("NC_002333.gb"), "file")
  expect_equal(.mtr_ref_class("ref/NC_002333.gb"), "file")
  expect_equal(.mtr_ref_class("/data/refs/NC_002333.gb"), "file")
  expect_equal(.mtr_ref_class("auto"), "file")
  expect_equal(.mtr_ref_class(""), "none")
  expect_equal(.mtr_ref_class("   "), "none")
  expect_equal(.mtr_ref_class(NA_character_), "none")
  expect_equal(.mtr_ref_class(NULL), "none")
})

test_that(".mtr_validate_refs normalises a real file to an absolute path", {
  d <- withr::local_tempdir()
  fa <- mtr_ref_fasta(d)
  rel <- file.path(basename(d), "ref.fasta")
  withr::local_dir(dirname(d))
  out <- .mtr_validate_refs(c(S1 = rel), ids = "S1")
  expect_equal(out, normalizePath(fa, winslash = "/"))
})

test_that(".mtr_validate_refs stores blanks as NA and leaves them alone", {
  out <- .mtr_validate_refs(c("", "   ", NA_character_), ids = c("S1", "S2", "S3"))
  expect_equal(out, rep(NA_character_, 3L))
})

test_that(".mtr_validate_refs reports every bad value at once, with sample IDs", {
  d <- withr::local_tempdir()
  testthat::local_mocked_bindings(
    .mtr_ncbi_known = function(accs, ...) list(ok = TRUE, found = character(0))
  )
  err <- expect_error(
    .mtr_validate_refs(
      c(file.path(d, "nope.gb"), "NC_999999", d),
      ids = c("S1", "S2", "S3")
    ),
    "problems \\(3\\)"
  )
  msg <- conditionMessage(err)
  expect_match(msg, "S1 \\[.*nope\\.gb\\]: file not found")
  expect_match(msg, "S2 \\[NC_999999\\]: no such nucleotide record at NCBI")
  expect_match(msg, "S3 \\[.*\\]: is a directory, not a file")
})

test_that(".mtr_validate_refs refuses shell metacharacters in a reference value", {
  # The value lands inside Rscript -e "..." in a bash double-quoted string, so a
  # quote ends the R expression and $ / backtick / backslash reach bash.
  expect_error(.mtr_validate_refs("/data/it's/ref.gb", ids = "S1"), "not allowed")
  expect_error(.mtr_validate_refs("/data/\"q\"/ref.gb", ids = "S1"), "not allowed")
  expect_error(.mtr_validate_refs("/data/$HOME/ref.gb", ids = "S1"), "not allowed")
  expect_error(.mtr_validate_refs("/data/a`id`b/ref.gb", ids = "S1"), "not allowed")
  expect_error(.mtr_validate_refs("/data/a\\b/ref.gb", ids = "S1"), "not allowed")
})

test_that(".mtr_esummary_found reads a hit, a miss, and a non-esummary body", {
  # Body shapes recorded from the live endpoint; see
  # dev/map_to_ref_refs_sdd/probes.md.
  hit <- paste0(
    '{"header":{},"result":{"uids":["15079186"],',
    '"15079186":{"uid":"15079186","caption":"NC_002333",',
    '"accessionversion":"NC_002333.2","slen":16596,"topology":"circular"}}}'
  )
  expect_equal(.mtr_esummary_found(hit), c("NC_002333", "NC_002333"))
  expect_equal(.mtr_esummary_found('{"esummaryresult":["Empty id list - nothing todo"]}'),
               character(0))
  expect_null(.mtr_esummary_found("<html>502 Bad Gateway</html>"))
})

test_that(".mtr_validate_refs rejects a file whose content is not a reference", {
  d <- withr::local_tempdir()
  bad <- file.path(d, "notes.txt")
  writeLines(c("hello", "world"), bad)
  expect_error(.mtr_validate_refs(bad, ids = "S1"), "LOCUS")
})

test_that(".mtr_validate_refs rejects an unreadable file", {
  skip_on_os("windows")
  skip_if(identical(Sys.info()[["effective_user"]], "root"), "running as root")
  d <- withr::local_tempdir()
  fa <- mtr_ref_fasta(d)
  Sys.chmod(fa, "0000")
  on.exit(Sys.chmod(fa, "0600"), add = TRUE)
  expect_error(.mtr_validate_refs(fa, ids = "S1"), "not readable")
})

test_that(".mtr_validate_refs uppercases an accession that exists", {
  testthat::local_mocked_bindings(
    .mtr_ncbi_known = function(accs, ...) list(ok = TRUE, found = "NC_002333")
  )
  expect_equal(.mtr_validate_refs("nc_002333.1", ids = "S1"), "NC_002333.1")
})

test_that("an unreachable NCBI is a warning and the value is kept", {
  testthat::local_mocked_bindings(
    .mtr_ncbi_known = function(accs, ...) list(ok = FALSE, reason = "timed out")
  )
  expect_warning(out <- .mtr_validate_refs("NC_002333", ids = "S1"),
                 "resolved when the pipeline runs")
  expect_equal(out, "NC_002333")
})

test_that("a checked accession list is batched into one request", {
  calls <- 0L
  testthat::local_mocked_bindings(
    .mtr_ncbi_known = function(accs, ...) {
      calls <<- calls + 1L
      list(ok = TRUE, found = c("NC_002333", "AB123456"))
    }
  )
  .mtr_validate_refs(c("NC_002333", "AB123456", "NC_002333"),
                     ids = c("S1", "S2", "S3"))
  expect_equal(calls, 1L)
})

test_that("a url is fetched, content-checked, and stored as the url", {
  d <- withr::local_tempdir()
  fa <- mtr_ref_fasta(d)
  testthat::local_mocked_bindings(
    .mtr_url_fetch = function(url, ...) list(ok = TRUE, file = fa)
  )
  expect_equal(.mtr_validate_refs("https://example.org/r.fa", ids = "S1"),
               "https://example.org/r.fa")
})

test_that("an unreachable url is an error naming the reason", {
  testthat::local_mocked_bindings(
    .mtr_url_fetch = function(url, ...) list(ok = FALSE, reason = "HTTP 404")
  )
  expect_error(.mtr_validate_refs("https://example.org/r.fa", ids = "S1"),
               "not reachable: HTTP 404")
})

test_that("an ftp url is refused with the escape hatch in the message", {
  expect_error(
    .mtr_validate_refs("ftp://ftp.ncbi.nlm.nih.gov/x.gb", ids = "S1"),
    "https"
  )
})

test_that("a distinct file value is content-checked only once", {
  d <- withr::local_tempdir()
  fa <- mtr_ref_fasta(d)
  calls <- 0L
  testthat::local_mocked_bindings(
    .mtr_content_problem = function(file) {
      calls <<- calls + 1L
      NA_character_
    }
  )
  .mtr_validate_refs(c(fa, fa, fa), ids = c("S1", "S2", "S3"))
  expect_equal(calls, 1L)
})
```

- [ ] **Step 2: Run the tests to verify they fail**

```bash
Rscript -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-map-to-ref-refs.R")'
```

Expected: two shapes of red bar, both of which prove the test is live.
- The un-mocked tests error with `could not find function ".mtr_ref_class"` (or `".mtr_esummary_found"`, `".mtr_validate_refs"`).
- Every test that calls `testthat::local_mocked_bindings()` errors **earlier**, with `Can't find binding for '.mtr_ncbi_known'` (or `'.mtr_url_fetch'`, `'.mtr_content_problem'`) - `local_mocked_bindings()` resolves the name before the mocked expression ever runs, so it never reaches a `could not find function` error.

- [ ] **Step 3: Write `R/map_to_ref_refs.R`**

Create the file with exactly this content:

```r
# Per-sample MapToRef references: classify, validate, ingest, and resolve.
# The same classifier runs on the user's machine at ingest and inside the
# container at run time, so it never consults the filesystem.

# NCBI nucleotide accession. Keep identical to maptorefAccession() in
# inst/nextflow/modules/assemble_workflow.nf.
.mtr_acc_re <- "^[A-Za-z]{1,2}_?[0-9]{5,9}(\\.[0-9]{1,3})?$"

# The value is interpolated into an Rscript -e call inside a bash double-quoted
# string (inst/nextflow/modules/assemble.nf:105): a quote ends the R expression,
# and $, a backtick, and a backslash are acted on by bash before R sees them.
.mtr_bad_chars_re <- "[\"'$`\\\\]"

#' @noRd
.mtr_ref_class <- function(x) {
  v <- trimws(.mtr_opts(x))
  if (!nzchar(v)) return("none")
  if (grepl("^(https?|ftp)://", v, ignore.case = TRUE)) return("url")
  if (grepl(.mtr_acc_re, v)) return("accession")
  "file"
}

# Pure parse of an esummary 200 body, so the one piece of new response-handling
# in this feature is testable without a network. Shapes recorded in
# dev/map_to_ref_refs_sdd/probes.md: a hit carries result.<uid>.caption and
# .accessionversion; an "Empty id list" body has an esummaryresult key and no
# result, and is a definitive miss, not an unreadable response.
#' @noRd
.mtr_esummary_found <- function(txt) {
  j <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE),
                error = function(e) NULL)
  if (is.null(j)) return(NULL)
  if (!is.null(j$esummaryresult)) return(character(0))
  if (is.null(j$result)) return(NULL)
  hits <- as.character(unlist(lapply(
    j$result[names(j$result) %nin% "uids"],
    function(r) c(r$accessionversion, r$caption)
  )))
  toupper(sub("\\.[0-9]+$", "", hits[nzchar(hits)]))
}

# Batched existence check. Deliberately not .blast_ref_efetch(): that helper
# sleeps 120s * attempt for five attempts, which is right inside the pipeline
# and wrong at an interactive validation boundary. esummary over efetch because
# efetch returns HTTP 200 with a garbled error body for a bad id (probes.md).
# The top-level "error" field names only ONE bad id even when several are bad,
# so it is never parsed; the found set is diffed against the requested set.
#' @noRd
.mtr_ncbi_known <- function(accs, timeout = 30L) {
  found <- character(0)
  # A few hundred ids overflow what E-utilities accepts in a GET query string,
  # and the resulting non-200 would silently downgrade to a warning.
  for (chunk in split(accs, ceiling(seq_along(accs) / 200L))) {
    url <- paste0(
      "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi",
      "?db=nuccore&retmode=json&id=", paste(chunk, collapse = ","),
      .blast_ref_api_key_qs()
    )
    resp <- tryCatch(
      httr2::request(url) |>
        httr2::req_timeout(timeout) |>
        httr2::req_error(is_error = function(r) FALSE) |>
        httr2::req_perform(),
      error = function(e) e
    )
    if (inherits(resp, "error")) {
      return(list(ok = FALSE, reason = conditionMessage(resp)))
    }
    if (httr2::resp_status(resp) != 200L) {
      return(list(ok = FALSE, reason = paste0("HTTP ", httr2::resp_status(resp))))
    }
    hits <- .mtr_esummary_found(httr2::resp_body_string(resp))
    if (is.null(hits)) {
      return(list(ok = FALSE, reason = "unreadable esummary response"))
    }
    found <- c(found, hits)
  }
  list(ok = TRUE, found = found)
}

#' @noRd
.mtr_url_fetch <- function(url, timeout = 60L) {
  dest <- tempfile("mtrurl")
  # req_perform(path=) writes the body to dest even on a 404, so a plate of dead
  # URLs would otherwise leave one file per row behind in tempdir().
  ok <- FALSE
  on.exit(if (!ok) unlink(dest), add = TRUE)
  resp <- tryCatch(
    httr2::request(url) |>
      httr2::req_timeout(timeout) |>
      httr2::req_error(is_error = function(r) FALSE) |>
      httr2::req_perform(path = dest),
    error = function(e) e
  )
  if (inherits(resp, "error")) {
    return(list(ok = FALSE, reason = conditionMessage(resp)))
  }
  if (httr2::resp_status(resp) != 200L) {
    return(list(ok = FALSE, reason = paste0("HTTP ", httr2::resp_status(resp))))
  }
  ok <- TRUE
  list(ok = TRUE, file = dest)
}

# The real reader is the content check: it is the only thing that catches a
# multi-record database, an HTML error page, or a nuclear contig. The circular
# placeholder keeps the FASTA-topology rule from firing during validation.
#' @noRd
.mtr_content_problem <- function(file) {
  d <- tempfile("mtrchk")
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  tryCatch(
    {
      maptoref_prepare_ref(file, topology = "circular", out_dir = d)
      NA_character_
    },
    error = function(e) conditionMessage(e)
  )
}

# Returns the normalised value or a reason. Two channels, never one, so a
# failure message can never be mistaken for a value and written to the database.
#' @noRd
.mtr_check_ref_value <- function(v, cls) {
  if (identical(cls, "url")) {
    if (grepl("^ftp://", v, ignore.case = TRUE)) {
      return(list(ok = FALSE, msg = paste0(
        "ftp:// references cannot be checked; use the https:// form of the ",
        "same URL, or download the file and give its path"
      )))
    }
    got <- .mtr_url_fetch(v)
    if (!isTRUE(got$ok)) {
      return(list(ok = FALSE, msg = paste0("not reachable: ", got$reason)))
    }
    prob <- .mtr_content_problem(got$file)
    unlink(got$file)
    if (!is.na(prob)) {
      return(list(ok = FALSE, msg = paste0(
        "downloaded, but is not a usable reference: ", prob
      )))
    }
    return(list(ok = TRUE, value = v))
  }
  # file.exists() is TRUE for a directory on Linux, so test that separately.
  if (dir.exists(v)) return(list(ok = FALSE, msg = "is a directory, not a file"))
  if (!file.exists(v)) return(list(ok = FALSE, msg = "file not found"))
  if (file.access(v, 4L) != 0L) {
    return(list(ok = FALSE, msg = "file is not readable"))
  }
  p <- normalizePath(v, winslash = "/", mustWork = FALSE)
  prob <- .mtr_content_problem(p)
  if (!is.na(prob)) return(list(ok = FALSE, msg = prob))
  list(ok = TRUE, value = p)
}

#' @noRd
.mtr_validate_refs <- function(x, ids = NULL, context = "reference") {
  s <- trimws(as.character(x))
  s[is.na(s)] <- ""
  n <- length(s)
  ids <- if (is.null(ids)) as.character(seq_len(n)) else as.character(ids)
  out <- rep(NA_character_, n)
  # Indexed by row, not appended: the checks below run in class batches, so
  # appending would interleave the report of a 96-sample plate.
  bad <- rep(NA_character_, n)
  add <- function(i, msg) {
    bad[i] <<- sprintf("  %s [%s]: %s", ids[i], s[i], msg)
  }

  bad_chars <- which(nzchar(s) & grepl(.mtr_bad_chars_re, s))
  for (i in bad_chars) {
    add(i, paste("quote, dollar, backtick, and backslash characters are not",
                 "allowed in a reference value"))
  }

  cls <- vapply(s, .mtr_ref_class, character(1), USE.NAMES = FALSE)

  ai <- setdiff(which(cls == "accession"), bad_chars)
  if (length(ai) > 0L) {
    s[ai] <- toupper(s[ai])
    known <- .mtr_ncbi_known(unique(s[ai]))
    if (!isTRUE(known$ok)) {
      warning("Could not check ", length(unique(s[ai])), " accession(s) against ",
              "NCBI (", known$reason, "); they will be resolved when the ",
              "pipeline runs.", call. = FALSE)
      out[ai] <- s[ai]
    } else {
      miss <- sub("\\.[0-9]+$", "", s[ai]) %nin% known$found
      for (i in ai[miss]) add(i, "no such nucleotide record at NCBI")
      out[ai[!miss]] <- s[ai[!miss]]
    }
  }

  # Distinct values are checked once; the results map back to every row that
  # names them, so a plate with three references makes three checks.
  fi <- setdiff(which(cls %in% c("url", "file")), bad_chars)
  seen <- list()
  for (i in fi) {
    key <- s[i]
    if (is.null(seen[[key]])) seen[[key]] <- .mtr_check_ref_value(key, cls[i])
    res <- seen[[key]]
    if (isTRUE(res$ok)) out[i] <- res$value else add(i, res$msg)
  }

  bad <- bad[!is.na(bad)]
  if (length(bad) > 0L) {
    stop(sprintf("MapToRef reference problems (%d) in %s:\n%s",
                 length(bad), context, paste(bad, collapse = "\n")),
         call. = FALSE)
  }
  out
}
```

- [ ] **Step 4: Run the tests**

```bash
Rscript -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-map-to-ref-refs.R")'
```

Expected: `FAIL 0`, all tests pass (one may skip on Windows or as root).

- [ ] **Step 5: Confirm nothing else moved and the file is ASCII**

```bash
Rscript -e 'devtools::test()'
grep -nP '[^\x00-\x7F]' R/map_to_ref_refs.R tests/testthat/test-map-to-ref-refs.R
```

Expected: `FAIL 0 | WARN 0 | SKIP 23 | PASS <2030 + the new cases>`; the grep prints nothing. Nothing calls the new code yet, so the 2030 existing passes must be unchanged.

---

### Task 2: The `assemble.maptoref_ref` column, its migration, and its schema gap

One nullable TEXT column in two DDLs, one ALTER block, one predicate clause, one `schema_gaps()` entry. Nothing reads the column yet, so this task is pure schema and is testable on its own: the migration test proves an old project gains it and the fresh-project test proves a new one is born with it. The `schema_gaps()` entry is the part that must not be skipped - migration is not automatic, and `assemble_workflow.nf:12` will select `a.maptoref_ref` for every project regardless of assembler after Task 7, so an un-migrated project would die at channel creation with a raw SQL error instead of being told to migrate.

Reference facts this task depends on (verified 2026-09-03 at HEAD `e93c403`):
- The read-based `assemble` DDL is `R/init_db.R:303-328`; `synteny_accession TEXT,` is `:322`. Its seed is `R/init_db.R:330-353`, with `join_switch = NA_integer_,` at `:347` and `time_stamp = NA_integer_` (no trailing comma) at `:348`.
- The userAsmb `assemble` DDL is `R/init_db_userAsmb.R:298-328`; `synteny_accession TEXT,` is `:321`. The two DDLs are not identical (`R/init_db_userAsmb.R:309-312` carries four extra columns) but they agree on `synteny_accession`.
- The userAsmb `assemble_opts` table is minimal - `assemble_opts`, `min_assembly_length`, `join_scaffolds` only (`R/init_db_userAsmb.R:363-371`). It has no `assembler` column.
- The migration pattern to copy is the `synteny_accession` block, `R/backwards_compatibility.R:1499-1503`: a `DBI::dbListFields(con, "assemble")` guard, a `message()`, one `ALTER TABLE`, no backfill.
- The "already current" predicate clause to sit beside is `"synteny_accession" %in% names(assemble_table)` at `R/backwards_compatibility.R:252`; `assemble_table` is read at `:90`.
- The roxygen migration list's `\item \code{assemble}:` bullet is `R/backwards_compatibility.R:21-24`.
- `schema_gaps()` starts at `R/backwards_compatibility.R:2232`, defines `has()` at `:2233`, and its `assemble_opts` MapToRef entry is `:2275-2280`.
- `expect_cols()` is defined at `tests/testthat/test-backwards-compatibility.R:40`; the two `assemble` lists are `:465-467` (v1.0.0 migration) and `:525-527` (v1.3.10 migration). The existing MapToRef gap test is `:319-339` and the fresh-project gap test is `:341-356`.

**Files:**
- Modify: `R/init_db.R` (the `assemble` DDL, the assemble seed)
- Modify: `R/init_db_userAsmb.R` (the `assemble` DDL)
- Modify: `R/backwards_compatibility.R` (roxygen, predicate, ALTER block, `schema_gaps()`)
- Modify: `tests/testthat/test-backwards-compatibility.R` (two `expect_cols` lists, one new gap test)

**Interfaces:**
- Consumes: nothing.
- Produces: `assemble.maptoref_ref` TEXT exists in every new project (both types) and in every migrated one, seeded and backfilled to SQL NULL. `schema_gaps()` reports `"the assemble table lacks the per-sample MapToRef reference column"` when it is absent. Tasks 3, 4, 6, and 7 all depend on the column existing.

- [ ] **Step 1: Write the failing tests**

Append to `tests/testthat/test-backwards-compatibility.R`, after the fresh-project gap test that ends at `:356`:

```r
test_that("schema_gaps flags assemble without the per-sample MapToRef reference", {
  td <- tempfile()
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE))

  create_multi_scaffold_db(td)
  make_config(td, version = "1.5.4")

  con0 <- DBI::dbConnect(RSQLite::SQLite(), file.path(td, ".sqlite"))
  expect_false("maptoref_ref" %in% DBI::dbListFields(con0, "assemble"))
  expect_true("the assemble table lacks the per-sample MapToRef reference column" %in%
                schema_gaps(con0))
  DBI::dbDisconnect(con0)

  MitoPilot::backwards_compatibility(path = td, update_config = FALSE)

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(td, ".sqlite"))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_true("maptoref_ref" %in% DBI::dbListFields(con, "assemble"))
  expect_false("the assemble table lacks the per-sample MapToRef reference column" %in%
                 schema_gaps(con))
})
```

In the same file, add `"maptoref_ref"` to both `assemble` `expect_cols()` lists:

```r
  expect_cols(con, "assemble",
              c("blast_accession", "blast_species", "blast_pident",
                "blast_qcovs", "blast_evalue", "blast_lineage", "blast_opts",
                "maptoref_ref"))
```

(the same replacement at `:465-467` and at `:525-527`).

- [ ] **Step 2: Run the tests to verify they fail**

```bash
Rscript -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-backwards-compatibility.R")'
```

Expected: three failures - the new gap test (`schema_gaps()` does not report the gap) and both `expect_cols` (the column is absent after migration).

- [ ] **Step 3: Add the column to both DDLs and to the read-based seed**

In `R/init_db.R`, in the `CREATE TABLE assemble` string, after the line `      synteny_accession TEXT,`:

```r
      maptoref_ref TEXT,
```

In the same file, in the assemble seed's `dplyr::mutate()`, after `join_switch = NA_integer_,`:

```r
          maptoref_ref = NA_character_,
```

(Task 3 replaces that literal with the value taken from the mapping file. The trailing `time_stamp = NA_integer_` keeps having no comma.)

In `R/init_db_userAsmb.R`, in its `CREATE TABLE assemble` string, after the line `      synteny_accession TEXT,`:

```r
      maptoref_ref TEXT,
```

The userAsmb seed is not changed: `dplyr::rows_upsert()` writes only the columns present in the data frame, so the column is born NULL, which is the correct value. userAsmb projects never run the `assemble` process; the column exists there purely so a fresh userAsmb project and a migrated one have the same schema (D1).

- [ ] **Step 4: Add the migration, the predicate clause, the roxygen line, and the gap**

In `R/backwards_compatibility.R`, immediately after the `synteny_accession` block (the one whose body is `DBI::dbExecute(con, "ALTER TABLE assemble ADD COLUMN synteny_accession TEXT")`):

```r
  # per-sample MapToRef reference (NULL falls back to the option-set value)
  if (!("maptoref_ref" %in% DBI::dbListFields(con, "assemble"))) {
    message("added 'maptoref_ref' column to assemble table")
    DBI::dbExecute(con, "ALTER TABLE assemble ADD COLUMN maptoref_ref TEXT")
  }
```

In the "already current" predicate, immediately after the line `      "synteny_accession" %in% names(assemble_table) &&`:

```r
      "maptoref_ref" %in% names(assemble_table) &&
```

Note the table: `assemble_table`, never `assemble_opts_table`. Both tables have a column called `maptoref_ref` and only one of them is the right one.

In the roxygen migration list, extend the `\item \code{assemble}:` bullet so it ends:

```r
#'   \item \code{assemble}: "poor_blast_ref" (migrated from \code{samples} and
#'     normalized to TEXT), BLAST result columns, "blast_opts", "join_notes",
#'     "join_switch", "circularize_opts"/"circularize_notes",
#'     "find_mito_opts"/"find_mito_notes", "maptoref_ref" (the per-sample
#'     MapToRef reference).
```

In `schema_gaps()`, immediately after the `assemble_opts` MapToRef entry:

```r
  if (!has("maptoref_ref" %in% DBI::dbListFields(con, "assemble"))) {
    gaps <- c(gaps, "the assemble table lacks the per-sample MapToRef reference column")
  }
```

Ungated on `is_user_asmb()`, because Step 3 mirrored the column into both DDLs and the migration is ungated too.

- [ ] **Step 5: Run the tests**

```bash
Rscript -e 'devtools::document()'
Rscript -e 'devtools::test()'
```

Expected: `FAIL 0 | WARN 0`. The v1.0.0 and v1.3.10 migration tests prove an old project gains the column; the new gap test proves the banner fires before migration and clears after it; `tests/testthat/test-backwards-compatibility.R:341-356` (fresh database) proves a new project has no gap. If the fresh **userAsmb** test suite (`tests/testthat/test-init-db-userasmb.R`, `tests/testthat/test-new-test-project-userAsmb.R`) reports a gap, Step 3's userAsmb DDL line was missed.

- [ ] **Step 6: ASCII check**

```bash
grep -nP '[^\x00-\x7F]' R/init_db.R R/init_db_userAsmb.R R/backwards_compatibility.R tests/testthat/test-backwards-compatibility.R
```

Expected: no output.

---

### Task 3: Mapping-file ingest and the relaxed `new_db()` validation

R2 and the `new_db()` half of R8. The optional `Reference` column is taken out of the mapping before the `samples` table is built from `colnames(mapping)`, validated together with the option-set value in one call so every bad value is listed at once, and written into `assemble.maptoref_ref`. `new_db()` stops requiring an option-set reference for MapToRef and warns instead, naming the samples with no reference from either source.

**The single easiest thing in this whole plan to get wrong is the FASTA-topology trap.** `trimws(NA_character_)` is `NA` and `grepl(p, NA)` is `FALSE`, so `!grepl(...)` is `TRUE`: the moment the hard stop at `R/init_db.R:160-162` is deleted, `new_db(assembler = "MapToRef")` with no reference stops demanding a reference and starts demanding a *topology*. Step 1 has a test for exactly that call; write it first and watch it fail for the right reason.

**`Reference` is a RESERVED mapping-file column name, for every assembler.** It is stripped and validated whether the project is MapToRef, GetOrganelle, or MitoFinder, and the value is stored in `assemble.maptoref_ref` either way. The strip is not optional - `new_db()` builds `samples` from `colnames(mapping)` (`R/init_db.R:226-233`) and `add_samples()` ALTERs `samples` for every unseen column (`R/add_samples.R:109-121`) - and validating only for MapToRef would let `assemble.maptoref_ref` hold garbage that the user later switches the assembler onto in the app, with no gate anywhere. The cost is a user whose mapping CSV already carries an unrelated `Reference` metadata column (a citation, a voucher): they must rename it, and the `mapping_fn` roxygen (Task 8 Step 3) says so. Step 1 has a GetOrganelle regression test.

**`add_samples()` on an un-migrated project fails LOUDLY and BEFORE it writes anything.** The `assemble` insert writes `maptoref_ref` unconditionally, and `.mtr_warn_missing_refs()` reads `a.maptoref_ref`; on a project created before D12's ALTER, both raise a raw SQLite error - and both land *after* `samples` (`R/add_samples.R:127-134`) and `preprocess` (`:151-166`) are already committed, leaving a half-applied add. The guard is therefore **unconditional** (not gated on the mapping carrying a `Reference` column) and sits immediately after `on.exit(DBI::dbDisconnect(con))` (`R/add_samples.R:68`), before any write. That the pipeline already requires the migration (`assemble_workflow.nf:12` selects `a.maptoref_ref` after Task 7) is what makes failing early with the exact command the consistent choice. Step 1 has a DROP COLUMN regression test.

Reference facts this task depends on (verified 2026-09-03 at HEAD `e93c403`):
- `new_db()` reads the mapping at `R/init_db.R:143`, coerces the ID column at `:146`, and finishes its ID validation at `:186-191`. `DBI::dbConnect` is `:216`; `CREATE TABLE samples ({cols*})` from `colnames(mapping)` is `:226-233`; the assemble seed is `:330-353`; the `assemble_opts` seed row's `maptoref_ref = maptoref_ref` is `:396`; the function ends `invisible(return())` at `:860`.
- The MapToRef validation block is `R/init_db.R:157-175`: assembler choice `:157-159`, reference required `:160-162`, topology value `:163-166`, FASTA topology `:167-172`, quotes `:173-175`.
- `add_samples()` reads its mapping at `R/add_samples.R:33`, validates IDs at `:36-57`, calls `validate_declared_topology()` at `:59`, opens the connection at `:67`, mutates the mapping at `:72-91`, coerces everything to character at `:93-95`, loops `ALTER TABLE samples ADD COLUMN` over unseen columns at `:109-121`, inserts `assemble` rows at `:170-191`, and ends at `:219-221`.
- `update_sample_metadata()` strips `R1`/`R2` at `R/update_sample_metadata.R:60-64` and `Assembly`/`Topology` at `:67-71`, with two `message()` calls each.
- userAsmb strips `Assembly`/`Topology` from the mapping with `dplyr::select(-dplyr::any_of("Topology"), -Assembly)` at `R/init_db_userAsmb.R:222` - the precedent R2 names.
- The userAsmb `assemble_opts` table has no `assembler` column (`R/init_db_userAsmb.R:363-371`), so the R8 warning query must return early there.
- The three test blocks to repair are `tests/testthat/test-map-to-ref.R:367-383`, `:385-393`, and `:395-415`; `mtr_test_db()` is `:355-365`, `mtr_fixture()` is `:1-5`, `mtr_write()` is `:13-19`.

**Files:**
- Modify: `R/map_to_ref_refs.R` (add `.mtr_take_ref_col`, `.mtr_warn_missing_refs`)
- Modify: `R/init_db.R` (validator, ingest call, assemble seed, warning, one roxygen `@param`)
- Modify: `R/add_samples.R` (ingest call, seeded column, warning, one roxygen line)
- Modify: `R/update_sample_metadata.R` (the strip block)
- Modify: `tests/testthat/test-map-to-ref.R` (three repaired blocks)
- Modify: `tests/testthat/test-map-to-ref-refs.R` (append)

**Interfaces:**
- Consumes: Task 1's `.mtr_validate_refs()`, Task 2's `assemble.maptoref_ref`.
- Produces:
  - `.mtr_take_ref_col(mapping, mapping_id = "ID")` -> `list(mapping = <Reference removed>, refs = <named chr by sample ID, or NULL>)`. Raw values; it does not validate.
  - `.mtr_warn_missing_refs(con)` -> invisibly the IDs with no effective reference, after `warning()`ing about them; silent and empty on a userAsmb project **and** on a project whose `assemble` table has no `maptoref_ref` column.
  - `new_db()` and `add_samples()` seed `assemble.maptoref_ref` from the `Reference` column and never let it reach `samples`, for **every** assembler; `new_db()` no longer requires an option-set reference for MapToRef; `add_samples()` stops with "run MitoPilot::backwards_compatibility()" on an un-migrated project, before writing anything.
  Task 4 consumes `.mtr_warn_missing_refs()`; Task 7 depends on the column being populated.

- [ ] **Step 1: Write the failing tests**

Append to `tests/testthat/test-map-to-ref-refs.R`:

```r
mtr_refs_mapping <- function(dir, refs = NULL, ids = c("S1", "S2")) {
  m <- data.frame(
    ID = ids,
    Taxon = "Danio rerio",
    R1 = paste0(ids, "_R1.fastq.gz"),
    R2 = paste0(ids, "_R2.fastq.gz")
  )
  if (!is.null(refs)) m$Reference <- refs
  fn <- file.path(dir, "mapping.csv")
  utils::write.csv(m, fn, row.names = FALSE)
  fn
}

test_that("new_db warns instead of demanding a reference or a topology", {
  # The trap: deleting the reference-required stop must not leave the
  # FASTA-topology check firing on an NA reference.
  d <- withr::local_tempdir()
  mapping <- mtr_refs_mapping(d)
  db <- file.path(d, ".sqlite")
  expect_warning(
    new_db(db_path = db, mapping_fn = mapping, assembler = "MapToRef"),
    "no reference"
  )
  expect_true(file.exists(db))
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_equal(DBI::dbGetQuery(con, "SELECT maptoref_ref FROM assemble")$maptoref_ref,
               c(NA_character_, NA_character_))
})

test_that("new_db seeds assemble.maptoref_ref from the Reference column", {
  d <- withr::local_tempdir()
  fa <- mtr_ref_fasta(d)
  mapping <- mtr_refs_mapping(d, refs = c(fa, ""))
  db <- file.path(d, ".sqlite")
  expect_warning(
    new_db(db_path = db, mapping_fn = mapping, assembler = "MapToRef"),
    "S2"
  )
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_false("Reference" %in% DBI::dbListFields(con, "samples"))
  a <- DBI::dbGetQuery(con, "SELECT ID, maptoref_ref FROM assemble ORDER BY ID")
  expect_equal(a$maptoref_ref,
               c(normalizePath(fa, winslash = "/"), NA_character_))
})

test_that("new_db does not warn when every sample has a reference", {
  d <- withr::local_tempdir()
  fa <- mtr_ref_fasta(d)
  mapping <- mtr_refs_mapping(d, refs = c(fa, fa))
  expect_no_warning(
    new_db(db_path = file.path(d, ".sqlite"), mapping_fn = mapping,
           assembler = "MapToRef")
  )
})

test_that("the option-set reference covers samples that have none of their own", {
  d <- withr::local_tempdir()
  fa <- mtr_ref_fasta(d)
  mapping <- mtr_refs_mapping(d, refs = c("", ""))
  expect_no_warning(
    new_db(db_path = file.path(d, ".sqlite"), mapping_fn = mapping,
           assembler = "MapToRef", maptoref_ref = fa,
           maptoref_topology = "circular")
  )
})

test_that("a bad Reference value and a bad option-set value are reported together", {
  d <- withr::local_tempdir()
  mapping <- mtr_refs_mapping(d, refs = c(file.path(d, "a.gb"), ""))
  err <- expect_error(
    new_db(db_path = file.path(d, ".sqlite"), mapping_fn = mapping,
           assembler = "MapToRef", maptoref_ref = file.path(d, "b.gb")),
    "problems \\(2\\)"
  )
  expect_match(conditionMessage(err), "assemble options \\[.*b\\.gb\\]")
  expect_match(conditionMessage(err), "S1 \\[.*a\\.gb\\]")
  expect_false(file.exists(file.path(d, ".sqlite")))
})

test_that("new_db still demands a topology for a FASTA option-set reference", {
  d <- withr::local_tempdir()
  fa <- mtr_ref_fasta(d)
  mapping <- mtr_refs_mapping(d)
  expect_error(
    new_db(db_path = file.path(d, ".sqlite"), mapping_fn = mapping,
           assembler = "MapToRef", maptoref_ref = fa),
    "maptoref_topology"
  )
})

test_that("new_db does not demand a topology for an accession", {
  d <- withr::local_tempdir()
  mapping <- mtr_refs_mapping(d)
  testthat::local_mocked_bindings(
    .mtr_ncbi_known = function(accs, ...) list(ok = TRUE, found = "NC_002333")
  )
  expect_no_error(
    new_db(db_path = file.path(d, ".sqlite"), mapping_fn = mapping,
           assembler = "MapToRef", maptoref_ref = "NC_002333")
  )
})

test_that("add_samples seeds the reference and never adds a samples column", {
  d <- withr::local_tempdir()
  fa <- mtr_ref_fasta(d)
  new_db(db_path = file.path(d, ".sqlite"),
         mapping_fn = mtr_refs_mapping(d, ids = c("S1", "S2")))
  add_fn <- mtr_refs_mapping(file.path(d), refs = fa, ids = c("S3", "S4"))
  add_samples(path = d, update_mapping_fn = add_fn)

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(d, ".sqlite"))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_false("Reference" %in% DBI::dbListFields(con, "samples"))
  a <- DBI::dbGetQuery(con, "SELECT ID, maptoref_ref FROM assemble ORDER BY ID")
  expect_equal(a$maptoref_ref[a$ID %in% c("S3", "S4")],
               rep(normalizePath(fa, winslash = "/"), 2L))
})

test_that("a Reference column is reserved for every assembler", {
  # Stripped and validated whatever the assembler, so assemble.maptoref_ref can
  # never hold garbage a later assembler switch would pick up.
  d <- withr::local_tempdir()
  testthat::local_mocked_bindings(
    .mtr_ncbi_known = function(accs, ...) list(ok = TRUE, found = "NC_002333")
  )
  mapping <- mtr_refs_mapping(d, refs = c("NC_002333", "NC_002333"))
  db <- file.path(d, ".sqlite")
  expect_no_warning(
    new_db(db_path = db, mapping_fn = mapping, assembler = "GetOrganelle")
  )
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_false("Reference" %in% DBI::dbListFields(con, "samples"))
  expect_equal(DBI::dbGetQuery(con, "SELECT maptoref_ref FROM assemble")$maptoref_ref,
               c("NC_002333", "NC_002333"))
})

test_that("add_samples refuses a project that predates the reference column", {
  d <- withr::local_tempdir()
  new_db(db_path = file.path(d, ".sqlite"),
         mapping_fn = mtr_refs_mapping(d, ids = c("S1", "S2")))
  con0 <- DBI::dbConnect(RSQLite::SQLite(), file.path(d, ".sqlite"))
  DBI::dbExecute(con0, "ALTER TABLE assemble DROP COLUMN maptoref_ref")
  DBI::dbDisconnect(con0)

  add_fn <- mtr_refs_mapping(d, ids = c("S3", "S4"))
  expect_error(add_samples(path = d, update_mapping_fn = add_fn),
               "backwards_compatibility")

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(d, ".sqlite"))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  # Nothing was half-applied: the guard runs before the samples write.
  expect_false(any(c("S3", "S4") %in%
                     DBI::dbGetQuery(con, "SELECT ID FROM samples")$ID))
})

test_that("update_sample_metadata strips a Reference column with a message", {
  d <- withr::local_tempdir()
  fa <- mtr_ref_fasta(d)
  new_db(db_path = file.path(d, ".sqlite"), mapping_fn = mtr_refs_mapping(d))
  upd <- file.path(d, "upd.csv")
  utils::write.csv(
    data.frame(ID = c("S1", "S2"), Taxon = "Danio rerio", Reference = fa),
    upd, row.names = FALSE
  )
  expect_message(update_sample_metadata(path = d, update_mapping_fn = upd),
                 "set_maptoref_refs")
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(d, ".sqlite"))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_false("Reference" %in% DBI::dbListFields(con, "samples"))
  expect_equal(DBI::dbGetQuery(con, "SELECT maptoref_ref FROM assemble")$maptoref_ref,
               c(NA_character_, NA_character_))
})
```

Note: `add_samples()` writes the `.old_sqlite_dbs` backup into the project directory, so `path = d` must be a directory containing `.sqlite`; the helper above satisfies that.

- [ ] **Step 2: Run the tests to verify they fail**

```bash
Rscript -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-map-to-ref-refs.R")'
```

Expected: the eleven new tests fail. The first fails with `Set maptoref_topology (circular or linear) for a FASTA reference` **only after** Step 4 deletes the reference-required stop; before that it fails with `MapToRef requires a reference mitogenome`. Either message proves the test is live. The tests that call `local_mocked_bindings()` error earlier, with `Can't find binding for '.mtr_ncbi_known'`.

- [ ] **Step 3: Add the two helpers to `R/map_to_ref_refs.R`**

Append:

```r
# Strip the optional Reference column out of a mapping before the samples table
# is built from colnames(mapping). Precedent: R/init_db_userAsmb.R:222 strips
# Assembly/Topology the same way. Values are returned raw; callers validate.
#' @noRd
.mtr_take_ref_col <- function(mapping, mapping_id = "ID") {
  if ("Reference" %nin% colnames(mapping)) {
    return(list(mapping = mapping, refs = NULL))
  }
  refs <- as.character(mapping[["Reference"]])
  names(refs) <- as.character(mapping[[mapping_id]])
  keep <- setdiff(colnames(mapping), "Reference")
  list(mapping = mapping[, keep, drop = FALSE], refs = refs)
}

# R8's warning, answered from the database rather than from the mapping file, so
# it sees both sources. The COALESCE is the same expression the pipeline uses in
# inst/nextflow/modules/assemble_workflow.nf.
#' @noRd
.mtr_warn_missing_refs <- function(con) {
  # userAsmb projects have a minimal assemble_opts with no assembler column.
  if ("assembler" %nin% DBI::dbListFields(con, "assemble_opts")) {
    return(invisible(character(0)))
  }
  # A project that has not been migrated yet has no a.maptoref_ref to select.
  # The loud failure belongs to the caller, not to a warning helper.
  if ("maptoref_ref" %nin% DBI::dbListFields(con, "assemble")) {
    return(invisible(character(0)))
  }
  ids <- DBI::dbGetQuery(con, paste(
    "SELECT a.ID FROM assemble a",
    "JOIN assemble_opts o ON a.assemble_opts = o.assemble_opts",
    "WHERE o.assembler = 'MapToRef'",
    "AND COALESCE(NULLIF(TRIM(a.maptoref_ref), ''),",
    "NULLIF(TRIM(o.maptoref_ref), '')) IS NULL"
  ))$ID
  if (length(ids) > 0L) {
    warning("MapToRef has no reference for ", length(ids), " sample(s): ",
            paste(utils::head(ids, 10L), collapse = ", "),
            if (length(ids) > 10L) paste0(" and ", length(ids) - 10L, " more") else "",
            ". Those samples will fail at the Assemble step. Set a reference per ",
            "sample with MitoPilot::set_maptoref_refs(), or set one for the ",
            "parameter set in the Assemble options.", call. = FALSE)
  }
  invisible(ids)
}
```

- [ ] **Step 4: Relax and narrow the `new_db()` validator**

In `R/init_db.R`, **delete** these three lines entirely:

```r
  if (assembler == "MapToRef" && (is.na(maptoref_ref) || !nzchar(trimws(maptoref_ref)))) {
    stop("MapToRef requires a reference mitogenome; set maptoref_ref")
  }
```

Then replace the FASTA-topology check with the guarded, narrowed form:

```r
  if (assembler == "MapToRef" &&
      !is.na(maptoref_ref) && nzchar(trimws(maptoref_ref)) &&
      !identical(.mtr_ref_class(maptoref_ref), "accession") &&
      !grepl("\\.(gb|gbk|gbff)$", trimws(maptoref_ref), ignore.case = TRUE) &&
      (is.na(maptoref_topology) || !nzchar(trimws(maptoref_topology)))) {
    stop("Set maptoref_topology (circular or linear) for a FASTA reference; ",
         "a GenBank (.gb) reference takes its topology from the file")
  }
```

The topology-value check above it (`maptoref_topology %nin% c("circular", "linear")`) and the quote check below it are unchanged and stay in place, so their messages keep firing before any file or network access.

- [ ] **Step 5: Add the ingest and the combined validation to `new_db()`**

In `R/init_db.R`, immediately after the closing brace of the "Validate IDs contain only alphanumeric characters" block and before the `# Set GetOrganelle databases ...` comment:

```r
  # The optional Reference column seeds assemble.maptoref_ref. It must be taken
  # out here, before CREATE TABLE samples ({cols*}) below, and before the DB
  # connection is opened, so a bad reference leaves no half-built .sqlite.
  # Reserved for every assembler: a value stored unchecked would go unguarded
  # the moment the user switches a project to MapToRef in the app.
  taken <- .mtr_take_ref_col(mapping, mapping_id = mapping_id)
  mapping <- taken$mapping
  # One validation pass over both sources, so every bad value is listed at once.
  checked <- .mtr_validate_refs(
    c(maptoref_ref, taken$refs),
    ids = c("assemble options", names(taken$refs)),
    context = "the assemble options and the mapping file 'Reference' column"
  )
  maptoref_ref <- checked[1]
  refs <- NULL
  if (!is.null(taken$refs)) {
    refs <- checked[-1]
    names(refs) <- names(taken$refs)
  }
```

- [ ] **Step 6: Seed the column and warn**

In `R/init_db.R`, immediately before `dplyr::tbl(con, "assemble") |>` (the assemble seed at `:330`):

```r
  ref_col <- if (is.null(refs)) NA_character_ else unname(refs[mapping$ID])
```

In that seed's `dplyr::mutate()`, change the line Task 2 added:

```r
          maptoref_ref = ref_col,
```

At the end of `new_db()`, immediately before `invisible(return())`:

```r
  if (assembler == "MapToRef") {
    .mtr_warn_missing_refs(con)
  }
```

- [ ] **Step 7: Wire `add_samples()`**

In `R/add_samples.R`, immediately after `validate_declared_topology(mapping, mapping_id = mapping_id)`:

```r
  # Same rule as new_db(): the Reference column seeds assemble.maptoref_ref and
  # must not become a samples column (this function ALTERs samples for every
  # unseen mapping column below).
  taken <- .mtr_take_ref_col(mapping, mapping_id = mapping_id)
  mapping <- taken$mapping
  refs <- if (is.null(taken$refs)) {
    NULL
  } else {
    v <- .mtr_validate_refs(taken$refs, ids = names(taken$refs),
                            context = "the mapping file 'Reference' column")
    names(v) <- names(taken$refs)
    v
  }
```

Immediately after the connection is opened (`on.exit(DBI::dbDisconnect(con))`, `R/add_samples.R:68`) - **unconditional**, and above every write, because the `assemble` insert below writes the column whether or not the mapping carried a `Reference`:

```r
  # The assemble insert below writes maptoref_ref, so the column must exist
  # before samples and preprocess are committed.
  if ("maptoref_ref" %nin% DBI::dbListFields(con, "assemble")) {
    stop("This project predates the per-sample MapToRef reference column; run ",
         "MitoPilot::backwards_compatibility() before adding samples")
  }
```

In the `assemble` insert's `dplyr::mutate()`, after `blast_opts = "default",`:

```r
          maptoref_ref = if (is.null(refs)) NA_character_ else unname(refs[mapping$ID]),
```

At the end of `add_samples()`, immediately after `.sync_sample_genetic_codes(con, ids = mapping$ID)`:

```r
  .mtr_warn_missing_refs(con)
```

Extend the `@param update_mapping_fn` roxygen (`R/add_samples.R:8`) to mention the optional `Reference` column.

- [ ] **Step 8: Strip `Reference` in `update_sample_metadata()`**

In `R/update_sample_metadata.R`, immediately after the `Assembly`/`Topology` block:

```r
  # remove Reference column from updated mapping
  if("Reference" %in% colnames(mapping)){
    mapping = mapping[,-which(colnames(mapping) == "Reference"), drop = FALSE]
    message("Update mapping file contains a MapToRef reference column (Reference)")
    message("Use MitoPilot::set_maptoref_refs() to change per-sample references")
  }
```

Same shape as the two blocks above it. This function's contract is metadata only, it never touches the `assemble` table, and R3 gives references their own writer; stripping keeps `Reference` out of `samples` and points the user at the right tool.

- [ ] **Step 9: Repair the three existing `new_db` test blocks**

In `tests/testthat/test-map-to-ref.R`:

Block 1 (`"new_db stores the five MapToRef option columns"`) - the reference must now exist:

```r
test_that("new_db stores the five MapToRef option columns", {
  skip_if_not(file.exists(mtr_fixture()))
  d <- withr::local_tempdir()
  db <- mtr_test_db(d, assembler = "MapToRef", maptoref_ref = mtr_fixture(),
                    maptoref_topology = "circular")
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  opts <- DBI::dbGetQuery(con, "SELECT * FROM assemble_opts")
  expect_true(all(c("maptoref_ref", "maptoref", "maptoref_consensus",
                    "maptoref_iter", "maptoref_topology") %in% names(opts)))
  expect_equal(opts$assembler, "MapToRef")
  expect_equal(opts$maptoref_ref, normalizePath(mtr_fixture(), winslash = "/"))
  expect_equal(opts$maptoref, "--very-sensitive-local")
  expect_equal(opts$maptoref_consensus, "-d 3 --min-BQ 20")
  expect_equal(opts$maptoref_iter, 5L)
  expect_equal(opts$maptoref_topology, "circular")
})
```

Block 2 (`"new_db refuses MapToRef without a reference..."`) - the first expectation becomes a warning:

```r
test_that("new_db warns for MapToRef without a reference and rejects a bad topology", {
  d <- withr::local_tempdir()
  expect_warning(mtr_test_db(d, assembler = "MapToRef"), "no reference")
  d2 <- withr::local_tempdir()
  expect_error(
    mtr_test_db(d2, assembler = "MapToRef", maptoref_ref = "x.gb",
                maptoref_topology = "round"),
    "circular or linear"
  )
})
```

(A second tempdir because the first call now succeeds and writes `.sqlite`.)

Block 3 (`"new_db applies the modal's reference-topology and quote rules"`) - the three failing cases still fail at the topology and quote checks, which run before validation; only the `expect_no_error()` needs a real file:

```r
test_that("new_db applies the modal's reference-topology and quote rules", {
  d <- withr::local_tempdir()
  fa <- mtr_write(d, "mito.fasta", c(">R", strrep("ACGT", 3000L)))
  expect_error(
    mtr_test_db(d, assembler = "MapToRef", maptoref_ref = "ref/mito.fasta"),
    "maptoref_topology"
  )
  expect_error(
    mtr_test_db(d, assembler = "MapToRef", maptoref_ref = "x.gb",
                maptoref = "--very-sensitive-local -N '1'"),
    "quote characters"
  )
  expect_error(
    mtr_test_db(d, assembler = "MapToRef", maptoref_ref = "x.gb",
                maptoref_consensus = "-d 3 --min-BQ \"20\""),
    "quote characters"
  )
  expect_no_error(
    mtr_test_db(d, assembler = "MapToRef", maptoref_ref = fa,
                maptoref_topology = "linear")
  )
})
```

- [ ] **Step 10: Run the tests**

```bash
Rscript -e 'devtools::document()'
Rscript -e 'devtools::test()'
```

Expected: `FAIL 0 | WARN 0`. If `new_db warns instead of demanding a reference or a topology` fails with `Set maptoref_topology`, Step 4's guard clause was not applied. If `a bad Reference value and a bad option-set value are reported together` reports one problem instead of two, Step 5 validated the two sources in separate calls.

- [ ] **Step 11: ASCII check**

```bash
grep -nP '[^\x00-\x7F]' R/map_to_ref_refs.R R/init_db.R R/add_samples.R R/update_sample_metadata.R tests/testthat/test-map-to-ref.R tests/testthat/test-map-to-ref-refs.R
```

Expected: no output.

---

### Task 4: `set_maptoref_refs()`, the exported CSV helper

R3. A user who did not have their reference list ready at project creation supplies it later as a two-column CSV or data frame. The helper validates the IDs against the project, validates the values with the same R4 validator, refuses to write through a lock, and flips `assemble_switch` to 1 only for rows whose stored value actually changed - the same signal the app raises when a sample's parameter set changes.

Reference facts this task depends on (verified 2026-09-03 at HEAD `e93c403`):
- The app's per-sample write idiom is `R/app_assemble.R:1086-1098`: a `data.frame(ID = ..., assemble_opts = ..., assemble_switch = 1)` through `dplyr::rows_update(..., unmatched = "ignore", in_place = TRUE, copy = TRUE, by = "ID")`.
- The app refuses every analogous per-sample edit while any selected row is locked: `req(all(rv$data$assemble_lock[selected] == 0))` at `R/app_assemble.R:669`, `:814`, and `:1160`, and the same guard spelled `req(all(rv$data$assemble_lock[req(selected())] == 0))` at `:539`.
- The "absent in the existing database" message shape is `R/update_sample_metadata.R:80-83`; the duplicate-ID stop shape is `R/init_db.R:149-154`.
- `%||%` (`R/utils.R:7-13`) is a length-0 test and must not be used for NA comparison.
- `add_samples()` and `update_sample_metadata()` each back up `.sqlite` with their own inline eleven-line block (`R/add_samples.R:136-147`, `R/update_sample_metadata.R:85-96`); `add_samples()` writes `samples` at `:127-134` *before* backing up, so the convention is not uniform. This helper does not back up (D8).

**Files:**
- Modify: `R/map_to_ref_refs.R`
- Modify: `tests/testthat/test-map-to-ref-refs.R` (append)
- Regenerated: `NAMESPACE`, `man/set_maptoref_refs.Rd`

**Interfaces:**
- Consumes: Task 1's `.mtr_validate_refs()`, Task 2's column, Task 3's `.mtr_warn_missing_refs()`.
- Produces: `set_maptoref_refs(path = ".", refs = NULL)`, exported, returning `invisible(<IDs still without a reference>)`. Writes `assemble.maptoref_ref` and `assemble.assemble_switch` for changed rows only.

- [ ] **Step 1: Write the failing tests**

Append to `tests/testthat/test-map-to-ref-refs.R`:

```r
mtr_refs_project <- function(dir, ids = c("S1", "S2"), ...) {
  new_db(db_path = file.path(dir, ".sqlite"),
         mapping_fn = mtr_refs_mapping(dir, ids = ids), ...)
  file.path(dir, ".sqlite")
}

test_that("set_maptoref_refs writes the column and flips the switch", {
  d <- withr::local_tempdir()
  fa <- mtr_ref_fasta(d)
  db <- mtr_refs_project(d)
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "UPDATE assemble SET assemble_switch = 2")

  set_maptoref_refs(d, data.frame(a = "S1", b = fa))

  a <- DBI::dbGetQuery(con, "SELECT ID, maptoref_ref, assemble_switch FROM assemble ORDER BY ID")
  expect_equal(a$maptoref_ref, c(normalizePath(fa, winslash = "/"), NA_character_))
  expect_equal(a$assemble_switch, c(1, 2))
})

test_that("set_maptoref_refs reads a CSV by position, ignoring header names", {
  d <- withr::local_tempdir()
  fa <- mtr_ref_fasta(d)
  db <- mtr_refs_project(d)
  csv <- file.path(d, "refs.csv")
  utils::write.csv(data.frame(sample = c("S1", "S2"), whatever = fa),
                   csv, row.names = FALSE)
  set_maptoref_refs(d, csv)

  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_equal(DBI::dbGetQuery(con, "SELECT maptoref_ref FROM assemble")$maptoref_ref,
               rep(normalizePath(fa, winslash = "/"), 2L))
})

test_that("set_maptoref_refs does not re-queue an unchanged row", {
  d <- withr::local_tempdir()
  fa <- mtr_ref_fasta(d)
  db <- mtr_refs_project(d)
  set_maptoref_refs(d, data.frame(a = "S1", b = fa))
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "UPDATE assemble SET assemble_switch = 2")

  expect_message(set_maptoref_refs(d, data.frame(a = "S1", b = fa)), "No changes")
  expect_equal(DBI::dbGetQuery(con, "SELECT assemble_switch FROM assemble WHERE ID = 'S1'")$assemble_switch, 2)
})

test_that("a blank value clears the per-sample reference", {
  d <- withr::local_tempdir()
  fa <- mtr_ref_fasta(d)
  db <- mtr_refs_project(d)
  set_maptoref_refs(d, data.frame(a = "S1", b = fa))
  set_maptoref_refs(d, data.frame(a = "S1", b = ""))

  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_true(is.na(DBI::dbGetQuery(
    con, "SELECT maptoref_ref FROM assemble WHERE ID = 'S1'")$maptoref_ref))
})

test_that("set_maptoref_refs refuses unknown IDs, duplicates, and locked rows", {
  d <- withr::local_tempdir()
  fa <- mtr_ref_fasta(d)
  db <- mtr_refs_project(d)
  expect_error(set_maptoref_refs(d, data.frame(a = "NOPE", b = fa)), "NOPE")
  expect_error(set_maptoref_refs(d, data.frame(a = c("S1", "S1"), b = fa)),
               "Duplicate")

  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "UPDATE assemble SET assemble_lock = 1 WHERE ID = 'S2'")
  expect_error(set_maptoref_refs(d, data.frame(a = "S2", b = fa)), "locked")
})

test_that("set_maptoref_refs validates values before writing anything", {
  d <- withr::local_tempdir()
  db <- mtr_refs_project(d)
  expect_error(
    set_maptoref_refs(d, data.frame(a = "S1", b = file.path(d, "nope.gb"))),
    "file not found"
  )
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_true(is.na(DBI::dbGetQuery(
    con, "SELECT maptoref_ref FROM assemble WHERE ID = 'S1'")$maptoref_ref))
})

test_that("set_maptoref_refs warns about samples still without a reference", {
  d <- withr::local_tempdir()
  fa <- mtr_ref_fasta(d)
  # Creating a MapToRef project with no reference warns by design (Task 3); an
  # uncaught warning inside test_that() would be counted in the WARN column.
  db <- suppressWarnings(mtr_refs_project(d, assembler = "MapToRef"))
  expect_warning(set_maptoref_refs(d, data.frame(a = "S1", b = fa)), "S2")
})
```

- [ ] **Step 2: Run the tests to verify they fail**

```bash
Rscript -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-map-to-ref-refs.R")'
```

Expected: the seven new tests fail with `could not find function "set_maptoref_refs"`.

- [ ] **Step 3: Write the helper**

Append to `R/map_to_ref_refs.R`:

```r
#' Set per-sample MapToRef references
#'
#' Assigns a MapToRef reference mitogenome to individual samples in an existing
#' project. The per-sample value overrides the reference on the sample's
#' assemble parameter set; a blank value clears the override so the parameter
#' set applies again.
#'
#' Samples whose reference actually changes are queued for (re-)assembly, the
#' same way changing a sample's parameter set does in the Assemble module.
#'
#' @param path Path to the project directory (default = current working
#'   directory)
#' @param refs A CSV path or a data frame. The first column holds sample IDs and
#'   the second holds references; column names are ignored and any further
#'   columns are ignored. A reference is an absolute file path to a
#'   single-record GenBank or FASTA mitogenome, a URL, or an NCBI nucleotide
#'   accession (for example NC_002333). Blank clears the sample's reference.
#'
#' @return Invisibly, the IDs that still have no reference from either source.
#' @export
#'
set_maptoref_refs <- function(path = ".", refs = NULL) {
  if (!dir.exists(path)) {
    stop("Project directory does not exist")
  }
  path <- normalizePath(path)
  db <- file.path(path, ".sqlite")
  if (!file.exists(db)) {
    stop("No MitoPilot database found in ", path)
  }

  if (is.character(refs) && length(refs) == 1L) {
    # file.exists() is TRUE for a directory on Linux, and read.csv() on one
    # fails with a cryptic internal message.
    if (!file.exists(refs) || dir.exists(refs)) {
      stop("refs CSV not found: ", refs)
    }
    refs <- utils::read.csv(refs)
  }
  if (!is.data.frame(refs) || ncol(refs) < 2L || nrow(refs) == 0L) {
    stop("refs must be a CSV path or a data frame with at least two columns ",
         "(sample ID, reference) and at least one row")
  }

  ids <- trimws(as.character(refs[[1]]))
  vals <- as.character(refs[[2]])
  if (any(duplicated(ids))) {
    stop("Duplicate IDs in refs: ",
         paste(unique(ids[duplicated(ids)]), collapse = ", "))
  }

  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = db)
  on.exit(DBI::dbDisconnect(con))
  if ("maptoref_ref" %nin% DBI::dbListFields(con, "assemble")) {
    stop("This project predates the per-sample MapToRef reference column; run ",
         "MitoPilot::backwards_compatibility() first")
  }
  cur <- DBI::dbGetQuery(con, "SELECT ID, maptoref_ref, assemble_lock FROM assemble")

  unknown <- setdiff(ids, cur$ID)
  if (length(unknown) > 0L) {
    stop("sample(s) ", paste(shQuote(unknown), collapse = ", "),
         " absent in the existing database")
  }
  locked <- cur$ID[cur$ID %in% ids &
                     !is.na(cur$assemble_lock) & cur$assemble_lock == 1]
  if (length(locked) > 0L) {
    stop("sample(s) ", paste(shQuote(locked), collapse = ", "),
         " are locked; unlock them in the Assemble module first")
  }

  new_vals <- .mtr_validate_refs(vals, ids = ids, context = "the reference list")
  old_vals <- cur$maptoref_ref[match(ids, cur$ID)]
  same <- (is.na(new_vals) & is.na(old_vals)) |
    (!is.na(new_vals) & !is.na(old_vals) & new_vals == old_vals)
  changed <- which(!same)
  if (length(changed) == 0L) {
    message("No changes: every sample already had that reference.")
    return(invisible(.mtr_warn_missing_refs(con)))
  }

  # Same write the app makes when a sample's parameter set changes
  # (R/app_assemble.R:1086-1098): value plus assemble_switch = 1, one statement.
  dplyr::tbl(con, "assemble") |>
    dplyr::rows_update(
      data.frame(
        ID = ids[changed],
        maptoref_ref = new_vals[changed],
        assemble_switch = 1
      ),
      unmatched = "ignore",
      in_place = TRUE,
      copy = TRUE,
      by = "ID"
    )
  message("Updated ", length(changed), " sample(s); ",
          length(ids) - length(changed), " already had that reference.")
  invisible(.mtr_warn_missing_refs(con))
}
```

- [ ] **Step 4: Run the tests**

```bash
Rscript -e 'devtools::document()'
Rscript -e 'devtools::test()'
```

Expected: `FAIL 0 | WARN 0`. `NAMESPACE` gains `export(set_maptoref_refs)` and `man/set_maptoref_refs.Rd` appears.

- [ ] **Step 5: ASCII check**

```bash
grep -nP '[^\x00-\x7F]' R/map_to_ref_refs.R tests/testthat/test-map-to-ref-refs.R man/set_maptoref_refs.Rd
```

Expected: no output.

---

### Task 5: `maptoref_fetch_accession()`, the run-time resolver

R5's offline-first half. Inside the container an accession is turned into a reference file: the bundled BLAST database first (so an air-gapped run works), NCBI `efetch rettype=gb` second (so the LOCUS line carries the topology and no guess is needed), a `stop()` third - which Task 6 turns into a per-sample failure. Every local step degrades rather than failing, matching how `blast_genbank.nf` treats the same database.

Reference facts this task depends on (verified 2026-09-03 at HEAD `e93c403`, plus the container measurements in `dev/map_to_ref_refs_sdd/probes.md`):
- **`blastdbcmd -entry` behaviour is MEASURED, not assumed** (probes.md, "blastdbcmd inside mitopilot:1.5.5", BLAST 2.16.0+): against `-db /ref_dbs/mito_metazoa/mito_metazoa`, `-entry NC_002333`, `-entry NC_002333.2`, and `-entry nc_002333` **all** return `NC_002333.2 16596 Danio rerio mitochondrion, complete genome` with exit 0. A missing entry gives exit 1, no output-file content, and `Error: [blastdbcmd] Entry not found: <acc>` on stderr. So **one** `-entry` call resolves every form a user can type, and the decision is exit status plus a non-empty output file. No version-stripped retry, and no `-info` probe.
- Default `blastdbcmd` output is FASTA (`>NC_002333.2 Danio rerio mitochondrion, complete genome` then 80-column sequence lines), which is what `maptoref_prepare_ref()` reads; the database carries no topology field, hence D16's circular default.
- `inst/nextflow/modules/blast_genbank.nf:213` still probes with `-info` before its own use, and correctly so: it then runs `blastn`, which has no per-query fall-through. Here the very next thing after a `-entry` failure is the NCBI arm, so the probe would only add a second process launch.
- The database was built with `-parse_seqids` (`tools/build_local_blast_db.py:387`, `tools/dedup_local_blast_db.py:165`), which is what makes `-entry` work at all. It stores sequence, accession, defline, taxid, and length - no topology, lineage, or genetic code.
- `NC_001638` (Chlamydomonas reinhardtii mitochondrion, 15,758 bp, LINEAR, non-metazoan) is measured **absent** from the bundled database (probes.md); Task 9 uses it to force the NCBI arm with the real `db_dir` in place.
- `blastdbcmd` is on the image PATH (`docker/Dockerfile:48-57`), and the database is unpacked to `/ref_dbs/mito_metazoa` (`docker/Dockerfile:82`).
- `.blast_ref_efetch(url, timeout, label)` is `R/blast_ref_utils.R:216-258`: five attempts, retries on 429/500/503 and connection errors, `120s * attempt` backoff, API key redacted from logs. That ladder is right here (unattended, inside the pipeline) and wrong at ingest (D5).
- `.mtr_log(log_fn, ...)` is `R/map_to_ref.R:390-392` and appends to the assembler log.
- The existing stub-binary test pattern is `mtr_stub_bin()` at `tests/testthat/test-map-to-ref-loop.R:3-63` (write a shell script into a temp `bin`, `Sys.chmod(..., "0755")` at `:61`, `withr::local_envvar(PATH = ...)` at `:120-126`). testthat runs each test file in its own environment, so a stub defined in one test file is not visible in another; this task writes its own small stub, and Task 6 adds a `blastdbcmd` case to `mtr_stub_bin()` for its end-to-end case.

**Files:**
- Modify: `R/map_to_ref_refs.R`
- Modify: `tests/testthat/test-map-to-ref-refs.R` (append)
- Regenerated: `NAMESPACE`, `man/maptoref_fetch_accession.Rd`

**Interfaces:**
- Consumes: `.blast_ref_efetch()`, `.blast_ref_api_key_qs()`, `.mtr_opts()`, `.mtr_log()`. **Nothing from Task 1.**
- Produces:
  - `maptoref_fetch_accession(accession, out_dir = ".", blast_db = NULL, log_fn = NULL)`, exported, returning `list(file = <path>, source = "local_db" | "ncbi", accession = <uppercased>)`, or `stop()` when neither source yields a record. It writes into `<out_dir>/maptoref/`. `blast_db = NULL` means "use the container default"; `blast_db = ""` means "skip the local arm".
  - `.mtr_blastdbcmd(acc, work, db, log_fn)` -> the same list or `NULL`.
  - `.mtr_efetch_gb(acc)` -> the GenBank text (the seam tests mock).
  - `.mtr_log_if(log_fn, ...)` -> logs only when a log file was given.
  Task 6 calls `maptoref_fetch_accession()`.

- [ ] **Step 1: Write the failing tests**

Append to `tests/testthat/test-map-to-ref-refs.R`:

```r
mtr_stub_blastdbcmd <- function(dir) {
  bin <- file.path(dir, "bin")
  dir.create(bin, showWarnings = FALSE)
  writeLines(c(
    "#!/bin/sh",
    "entry=''",
    "while [ $# -gt 0 ]; do",
    "  case \"$1\" in",
    "    -entry) entry=$2; shift ;;",
    "  esac",
    "  shift",
    "done",
    "case \" ${MTR_STUB_ACC} \" in",
    "  *\" $entry \"*) cat \"$MTR_STUB_FASTA\"; exit 0 ;;",
    "esac",
    "echo \"Error: [blastdbcmd] Entry not found: $entry\" >&2",
    "exit 1"
  ), file.path(bin, "blastdbcmd"))
  Sys.chmod(file.path(bin, "blastdbcmd"), "0755")
  bin
}

test_that("maptoref_fetch_accession prefers the local BLAST database", {
  skip_on_os("windows")
  d <- withr::local_tempdir()
  fa <- mtr_ref_fasta(d, "stub.fasta")
  withr::local_envvar(c(
    PATH = paste(mtr_stub_blastdbcmd(d), Sys.getenv("PATH"), sep = ":"),
    MTR_STUB_ACC = "NC_002333.1",
    MTR_STUB_FASTA = fa
  ))

  got <- maptoref_fetch_accession("nc_002333.1", out_dir = d, blast_db = "/x/y")
  expect_equal(got$source, "local_db")
  expect_equal(got$accession, "NC_002333.1")
  expect_true(file.exists(got$file))
  expect_gt(file.size(got$file), 0)
})

test_that("an explicit empty blast_db skips the local database", {
  skip_on_os("windows")
  d <- withr::local_tempdir()
  fa <- mtr_ref_fasta(d, "stub.fasta")
  withr::local_envvar(c(
    PATH = paste(mtr_stub_blastdbcmd(d), Sys.getenv("PATH"), sep = ":"),
    MTR_STUB_ACC = "NC_002333.1",
    MTR_STUB_FASTA = fa
  ))
  testthat::local_mocked_bindings(
    .mtr_efetch_gb = function(acc) paste0("LOCUS  ", acc, "  16596 bp DNA circular VRT\n//\n")
  )
  got <- maptoref_fetch_accession("NC_002333.1", out_dir = d, blast_db = "")
  expect_equal(got$source, "ncbi")
  expect_true(grepl("^LOCUS", readLines(got$file)[1]))
})

test_that("a local miss falls through to NCBI", {
  skip_on_os("windows")
  d <- withr::local_tempdir()
  withr::local_envvar(c(
    PATH = paste(mtr_stub_blastdbcmd(d), Sys.getenv("PATH"), sep = ":"),
    MTR_STUB_ACC = "SOMETHING_ELSE"
  ))
  testthat::local_mocked_bindings(
    .mtr_efetch_gb = function(acc) "LOCUS  X  16596 bp DNA circular VRT\n//\n"
  )
  expect_equal(maptoref_fetch_accession("NC_002333.1", out_dir = d,
                                        blast_db = "/x/y")$source, "ncbi")
})

test_that("neither source resolving is an error naming the accession", {
  skip_on_os("windows")
  d <- withr::local_tempdir()
  withr::local_envvar(c(
    PATH = paste(mtr_stub_blastdbcmd(d), Sys.getenv("PATH"), sep = ":"),
    MTR_STUB_ACC = "SOMETHING_ELSE"
  ))
  testthat::local_mocked_bindings(
    .mtr_efetch_gb = function(acc) stop("network unreachable")
  )
  expect_error(maptoref_fetch_accession("ZZ999999", out_dir = d, blast_db = "/x/y"),
               "ZZ999999")
})

test_that("the resolver logs its attempts when given a log file", {
  skip_on_os("windows")
  d <- withr::local_tempdir()
  fa <- mtr_ref_fasta(d, "stub.fasta")
  withr::local_envvar(c(
    PATH = paste(mtr_stub_blastdbcmd(d), Sys.getenv("PATH"), sep = ":"),
    MTR_STUB_ACC = "NC_002333.1",
    MTR_STUB_FASTA = fa
  ))
  log_fn <- file.path(d, "assembler.log.txt")
  file.create(log_fn)
  maptoref_fetch_accession("NC_002333.1", out_dir = d, blast_db = "/x/y",
                           log_fn = log_fn)
  expect_true(any(grepl("blastdbcmd", readLines(log_fn))))
})
```

- [ ] **Step 2: Run the tests to verify they fail**

```bash
Rscript -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-map-to-ref-refs.R")'
```

Expected: the five new tests fail. The un-mocked ones error with `could not find function "maptoref_fetch_accession"`; the two that call `local_mocked_bindings()` error earlier with `Can't find binding for '.mtr_efetch_gb'`. Both prove the test is live.

- [ ] **Step 3: Write the resolver**

Append to `R/map_to_ref_refs.R`:

```r
#' @noRd
.mtr_log_if <- function(log_fn, ...) {
  if (!is.null(log_fn) && nzchar(.mtr_opts(log_fn))) {
    .mtr_log(log_fn, ...)
  }
  invisible(NULL)
}

# Local BLAST database arm. Every failure returns NULL so the caller falls
# through to NCBI: an unreadable or missing database must degrade, not fail the
# sample. One -entry call is enough: measured against the bundled database,
# bare, versioned, and lowercase accessions all resolve, and a miss is exit 1
# with an empty output file (dev/map_to_ref_refs_sdd/probes.md).
#' @noRd
.mtr_blastdbcmd <- function(acc, work, db, log_fn = NULL) {
  if (!nzchar(db)) {
    return(NULL)
  }
  if (!nzchar(Sys.which("blastdbcmd"))) {
    .mtr_log_if(log_fn, "blastdbcmd not on PATH; skipping the local BLAST database")
    return(NULL)
  }
  fa <- file.path(work, paste0("reference_", acc, ".fasta"))
  err <- tempfile("mtrdbcmd")
  on.exit(unlink(err), add = TRUE)
  .mtr_log_if(log_fn, "+ blastdbcmd -db ", db, " -entry ", acc)
  st <- suppressWarnings(system2(
    "blastdbcmd",
    c("-db", shQuote(db), "-entry", shQuote(acc), "-outfmt", shQuote("%f")),
    stdout = fa, stderr = err
  ))
  if (identical(as.integer(st), 0L) && file.exists(fa) && file.size(fa) > 0L) {
    return(list(file = fa, source = "local_db", accession = acc))
  }
  if (file.exists(err) && file.size(err) > 0L) {
    .mtr_log_if(log_fn, paste(readLines(err, warn = FALSE), collapse = "; "))
  }
  unlink(fa)
  NULL
}

# rettype=gb, not fasta: the LOCUS line carries the topology, so an NCBI-sourced
# reference needs no topology guess. Its own function so tests can mock the seam.
#' @noRd
.mtr_efetch_gb <- function(acc) {
  url <- paste0(
    "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi",
    "?db=nuccore&id=", utils::URLencode(acc, reserved = TRUE),
    "&rettype=gb&retmode=text", .blast_ref_api_key_qs()
  )
  httr2::resp_body_string(.blast_ref_efetch(url, 120L, paste0("gb/", acc)))
}

#' Resolve an NCBI accession to a MapToRef reference file
#'
#' Looks the accession up in a local BLAST nucleotide database first, so a run
#' with no network still works, and downloads the GenBank record from NCBI only
#' if it is not there.
#'
#' @param accession An NCBI nucleotide accession, with or without its version
#'   (for example NC_002333 or NC_002333.1). Case insensitive.
#' @param out_dir Directory to write into; the file is placed in
#'   \code{<out_dir>/maptoref/}.
#' @param blast_db Full BLAST database prefix (directory and database name, as
#'   \code{blastdbcmd -db} takes it). \code{NULL} (the default) uses the database
#'   bundled in the MitoPilot container; \code{""} skips the local lookup and
#'   goes straight to NCBI.
#' @param log_fn Optional path to an assembler log file to append to.
#'
#' @return A list with \code{file}, \code{source} ("local_db" or "ncbi"), and
#'   the uppercased \code{accession}. Stops when neither source has the record.
#' @export
#'
maptoref_fetch_accession <- function(accession, out_dir = ".", blast_db = NULL,
                                     log_fn = NULL) {
  acc <- toupper(trimws(.mtr_opts(accession)))
  if (!nzchar(acc)) {
    stop("No accession given")
  }
  work <- file.path(out_dir, "maptoref")
  dir.create(work, recursive = TRUE, showWarnings = FALSE)
  # NULL-only default, so an explicit "" still means "skip the local arm".
  db <- if (is.null(blast_db)) {
    "/ref_dbs/mito_metazoa/mito_metazoa"
  } else {
    trimws(.mtr_opts(blast_db))
  }

  local_hit <- .mtr_blastdbcmd(acc, work, db, log_fn)
  if (!is.null(local_hit)) {
    return(local_hit)
  }

  .mtr_log_if(log_fn, "accession ", acc,
              " not in the local BLAST database; fetching from NCBI")
  txt <- tryCatch(.mtr_efetch_gb(acc), error = function(e) "")
  if (!grepl("^LOCUS", trimws(txt))) {
    stop("could not resolve accession ", acc, ": not in the local BLAST ",
         "database (", db, ") and NCBI returned no GenBank record")
  }
  gb <- file.path(work, paste0("reference_", acc, ".gb"))
  writeLines(txt, gb)
  list(file = gb, source = "ncbi", accession = acc)
}
```

- [ ] **Step 4: Run the tests**

```bash
Rscript -e 'devtools::document()'
Rscript -e 'devtools::test()'
```

Expected: `FAIL 0 | WARN 0`. `NAMESPACE` gains `export(maptoref_fetch_accession)`.

- [ ] **Step 5: ASCII check**

```bash
grep -nP '[^\x00-\x7F]' R/map_to_ref_refs.R tests/testthat/test-map-to-ref-refs.R man/maptoref_fetch_accession.Rd
```

Expected: no output.

---

### Task 6: `map_to_ref()` takes the raw value, resolves it, and records the source

R5's run-time half and all of R6. `map_to_ref()` gains two defaulted trailing arguments. Inside `.mtr_assemble()`, the raw value is classified with the same classifier the ingest used, an accession is resolved through Task 5, a local-DB FASTA defaults to circular, and the source is written to the summary and the log. Every new failure path lands in the `tryCatch` that already turns any error into `.mtr_fail()` - sentinel FASTA, `failure=` summary, exit 0 - so the R6 contract needs no new Nextflow error handling. That is the whole reason the resolver is called from R rather than from a second `Rscript -e` in the shell branch: `assemble` runs with `errorStrategy ... 'ignore'` (`inst/nextflow/modules/assemble.nf:10`) and every config sets `workflow.failOnIgnore = true` (`inst/config.local:102`), so a shell-side non-zero exit makes the whole run exit non-zero.

Reference facts this task depends on (verified 2026-09-03 at HEAD `e93c403`):
- `map_to_ref()` is `R/map_to_ref.R:352-387`; its roxygen is `:331-351`, its `tryCatch` is `:374-385`, and it forwards to `.mtr_assemble()` at `:376-378`.
- `.mtr_assemble()` is `R/map_to_ref.R:486-733`; its first statement is `maptoref_prepare_ref(...)` at `:489-490`, `work` is `:491`, the reference log line is `:496-497`, and the summary `writeLines()` is `:706-726` with `accession=` at `:708`.
- `.mtr_fail(id, out_dir, log_fn, reason)` writes `<id>_assembly_0.fasta` with `>No assembly found` and a summary with `assembler=MapToRef` plus `failure=<reason>` (`R/map_to_ref.R:476-483`).
- `maptoref_prepare_ref()` stops with `Reference file is empty:` on a 0-byte file (`R/map_to_ref.R:27-29`), which is the cryptic message the new guard replaces.
- `inst/nextflow/assets/NO_FILE` is **0 bytes** (`wc -c` = 0). The guard's `file.size(...) == 0L` test depends on that.
- All 14 `map_to_ref()` calls in `tests/testthat/test-map-to-ref-loop.R` (`:144`, `:193`, `:219`, `:252`, `:270`, `:296`, `:312`, `:329`, `:345`, `:362`, `:383`, `:411`, `:437`, `:455`) pass a real reference file and no `ref_value`, so the empty-reference guard must be conditional on the staged file, never unconditional.
- `mtr_setup()` (`tests/testthat/test-map-to-ref-loop.R:72-130`) writes `ref.fasta` from a 6,000 bp repeat and sets `PATH`, `MTR_STUB_CONS`, `MTR_STUB_CONS_INS`, `MTR_STUB_CONS_DIR`, and `MTR_STUB_SAM` through `withr::local_envvar` at `:120-126`. `mtr_summary()` at `:132-136` parses `key=value` lines.
- The `.mtr_validate_topology()` rule and the LOCUS-wins rule are `R/map_to_ref.R:131-140` and `:94-101`; `.mtr_junction_depth()` (`:445-465`) is what makes a wrong "circular" self-correcting.

**Files:**
- Modify: `R/map_to_ref.R` (roxygen, `map_to_ref()` signature and forward, the `ref_value` shell-character refusal beside the bowtie2 check, `.mtr_assemble()` signature and head, the log line, the summary)
- Modify: `tests/testthat/test-map-to-ref-loop.R` (the `blastdbcmd` stub, `mtr_setup()`, four new tests)

**Interfaces:**
- Consumes: Task 1's `.mtr_ref_class()` and `.mtr_bad_chars_re`, Task 5's `maptoref_fetch_accession()`.
- Produces: `map_to_ref(..., out_dir = ".", ref_value = NA_character_, blast_db = NULL)`. Task 7's `assemble.nf` call passes those two. Also the summary key `reference_source=<file|url|local_db|ncbi>` and the assembler-log line `reference <accession> <organism> (<len> bp, <topology>, source=<src>)`, both consumed by Task 9's evidence greps.

- [ ] **Step 1: Write the failing tests**

In `tests/testthat/test-map-to-ref-loop.R`, add a `blastdbcmd` stub inside `mtr_stub_bin()`, immediately before the `Sys.chmod(list.files(bin, full.names = TRUE), "0755")` line:

```r
  writeLines(c(
    "#!/bin/sh",
    "entry=''",
    "while [ $# -gt 0 ]; do",
    "  case \"$1\" in",
    "    -entry) entry=$2; shift ;;",
    "  esac",
    "  shift",
    "done",
    "case \" ${MTR_STUB_ACC} \" in",
    "  *\" $entry \"*) cat \"$MTR_STUB_FASTA\"; exit 0 ;;",
    "esac",
    "echo \"Error: [blastdbcmd] Entry not found: $entry\" >&2",
    "exit 1"
  ), file.path(bin, "blastdbcmd"))
```

(The resolver makes one `-entry` call and no `-info` probe, so the stub needs no `-info` case.)

In `mtr_setup()`, extend the `withr::local_envvar` call with two more entries:

```r
    MTR_STUB_SAM = sam,
    MTR_STUB_ACC = "NC_002333.1",
    MTR_STUB_FASTA = ref_fa
```

Then append four tests:

```r
test_that("no reference at all is a per-sample failure with a clear message", {
  skip_on_os("windows")
  d <- withr::local_tempdir()
  s <- mtr_setup(d)
  no_file <- file.path(d, "NO_FILE")
  file.create(no_file)
  out <- file.path(d, "out")

  ok <- map_to_ref("T1", no_file, s$r1, s$r2, "--very-sensitive-local",
                   "-d 3 --min-BQ 20", 5, NA_character_, 2, 1, out,
                   ref_value = NA_character_)

  expect_false(ok)
  expect_equal(readLines(file.path(out, "T1_assembly_0.fasta"))[1],
               ">No assembly found")
  expect_match(mtr_summary(out)[["failure"]], "no MapToRef reference")
})

test_that("an accession resolves from the local database and is recorded", {
  skip_on_os("windows")
  d <- withr::local_tempdir()
  s <- mtr_setup(d)
  no_file <- file.path(d, "NO_FILE")
  file.create(no_file)
  out <- file.path(d, "out")

  ok <- map_to_ref("T1", no_file, s$r1, s$r2, "--very-sensitive-local",
                   "-d 3 --min-BQ 20", 5, NA_character_, 2, 1, out,
                   ref_value = "nc_002333.1", blast_db = "/x/y")

  expect_true(ok)
  sm <- mtr_summary(out)
  expect_equal(sm[["reference_source"]], "local_db")
  # The bundled database holds complete mitogenomes and carries no topology, so
  # a local-DB FASTA is assumed circular (R5).
  expect_equal(sm[["reference_topology"]], "circular")
  expect_true(any(grepl("source=local_db",
                        readLines(file.path(out, "assembler.log.txt")))))
})

test_that("an unresolvable accession fails one sample, not the run", {
  skip_on_os("windows")
  d <- withr::local_tempdir()
  s <- mtr_setup(d)
  no_file <- file.path(d, "NO_FILE")
  file.create(no_file)
  out <- file.path(d, "out")
  withr::local_envvar(c(MTR_STUB_ACC = "SOMETHING_ELSE"))
  testthat::local_mocked_bindings(
    .mtr_efetch_gb = function(acc) stop("network unreachable")
  )

  ok <- map_to_ref("T1", no_file, s$r1, s$r2, "--very-sensitive-local",
                   "-d 3 --min-BQ 20", 5, NA_character_, 2, 1, out,
                   ref_value = "ZZ999999", blast_db = "/x/y")

  expect_false(ok)
  expect_equal(readLines(file.path(out, "T1_assembly_0.fasta"))[1],
               ">No assembly found")
  expect_match(mtr_summary(out)[["failure"]], "ZZ999999")
})

test_that("a staged file reference is recorded as its class", {
  skip_on_os("windows")
  d <- withr::local_tempdir()
  s <- mtr_setup(d)
  out <- file.path(d, "out")

  ok <- map_to_ref("T1", s$ref, s$r1, s$r2, "--very-sensitive-local",
                   "-d 3 --min-BQ 20", 5, "circular", 2, 1, out,
                   ref_value = "https://example.org/ref.fasta")

  expect_true(ok)
  expect_equal(mtr_summary(out)[["reference_source"]], "url")
})
```

- [ ] **Step 2: Run the tests to verify they fail**

```bash
Rscript -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-map-to-ref-loop.R")'
```

Expected: the four new tests fail with `unused argument (ref_value = ...)`. Every pre-existing test in the file must still pass.

- [ ] **Step 3: Extend the signatures**

In `R/map_to_ref.R`, add two `@param` lines to the `map_to_ref()` roxygen, after `@param out_dir Output directory.`:

```r
#' @param ref_value The reference exactly as the user configured it: an absolute
#'   file path, a URL, or an NCBI nucleotide accession. An accession is resolved
#'   here (local BLAST database first, NCBI second); anything else means
#'   \code{ref} is already the staged reference file.
#' @param blast_db Full BLAST database prefix used to resolve an accession
#'   offline. Defaults to the database bundled in the MitoPilot container.
```

Extend the signature (the last formal is `out_dir = "."`):

```r
                       out_dir = ".",
                       ref_value = NA_character_,
                       blast_db = NULL) {
```

Immediately after the existing bowtie2 quote refusal (`R/map_to_ref.R:369-373`, the block ending `return(invisible(FALSE))`), add the belt-and-braces refusal for the reference value, using the same character set the validator uses:

```r
  if (grepl(.mtr_bad_chars_re, .mtr_opts(ref_value))) {
    .mtr_fail(id, out_dir, log_fn,
              "reference value must not contain quote, dollar, backtick, or backslash characters")
    return(invisible(FALSE))
  }
```

Every writer already refuses these characters (Task 1's validator, Task 8's modal alert), so this only catches a hand-edited `.sqlite` - and it catches it as a **per-sample** `.mtr_fail()` (exit 0) instead of a broken `Rscript -e` that would exit non-zero and, with `workflow.failOnIgnore = true`, end the whole run.

Extend the forwarding call inside the `tryCatch`:

```r
      .mtr_assemble(id, ref, reads_1, reads_2, bowtie2_opts, consensus_opts,
                    as.integer(iter_cap), topology, genetic_code,
                    as.integer(cpus), out_dir, log_fn, ref_value, blast_db)
```

Extend `.mtr_assemble()`'s signature:

```r
.mtr_assemble <- function(id, ref_file, reads_1, reads_2, bowtie2_opts,
                          consensus_opts, iter_cap, topology, genetic_code,
                          cpus, out_dir, log_fn, ref_value = NA_character_,
                          blast_db = NULL) {
```

- [ ] **Step 4: Resolve the reference at the head of `.mtr_assemble()`**

Insert immediately before the existing `ref <- maptoref_prepare_ref(...)` call:

```r
  src <- .mtr_ref_class(ref_value)
  if (identical(src, "none")) {
    # Nextflow stages assets/NO_FILE (0 bytes) when no reference is set on the
    # sample or the parameter set. A direct caller passes a real path and no
    # ref_value, so an existing non-empty file still counts as a reference.
    if (!file.exists(ref_file) || file.size(ref_file) == 0L) {
      stop("no MapToRef reference for this sample; set one in the mapping file ",
           "'Reference' column, with MitoPilot::set_maptoref_refs(), or in the ",
           "Assemble options")
    }
    src <- "file"
  }
  if (identical(src, "accession")) {
    got <- maptoref_fetch_accession(ref_value, out_dir = out_dir,
                                    blast_db = blast_db, log_fn = log_fn)
    ref_file <- got$file
    src <- got$source
    # blastdbcmd emits a bare FASTA and the bundled database holds complete
    # mitogenomes, so assume circular; the junction-depth check downgrades to
    # linear when no reads span the seam.
    if (identical(src, "local_db") && !nzchar(trimws(.mtr_opts(topology)))) {
      topology <- "circular"
    }
  }
```

- [ ] **Step 5: Record the source in the log and the summary**

Replace the reference log line:

```r
  .mtr_log(log_fn, "reference ", ref$accession, " ", ref$organism,
           " (", len, " bp, ", ref$topology, ", source=", src, ")")
```

In the summary `writeLines()`, insert one line immediately after `paste0("accession=", ref$accession),`:

```r
    paste0("reference_source=", src),
```

Only one key: `accession=` above it already carries R5's accession, read from the record itself. It is deliberately **not** a `note=` line - the fold at `assemble_workflow.nf:299-311` picks up only `note=` lines and would stamp a note on every MapToRef sample, burying the real warnings.

- [ ] **Step 6: Run the tests**

```bash
Rscript -e 'devtools::document()'
Rscript -e 'devtools::test()'
```

Expected: `FAIL 0 | WARN 0`. Every pre-existing test in `tests/testthat/test-map-to-ref-loop.R` still passes, which is the proof that the empty-reference guard stayed conditional.

- [ ] **Step 7: ASCII check**

```bash
grep -nP '[^\x00-\x7F]' R/map_to_ref.R tests/testthat/test-map-to-ref-loop.R man/map_to_ref.Rd
```

Expected: no output.

---

### Task 7: Nextflow wiring

Four edited lines plus one helper function across two module files. The rule throughout: select position `it[19]` keeps its meaning, and the new value travels as a key in the existing `opts` map, which is the precedent the other four MapToRef options already set. Nothing about the process input tuple, the output tuple, the `multiMap` arity, or the `.cross` map changes.

Reference facts this task depends on (verified 2026-09-03 at HEAD `e93c403`):
- `params.sqlRead` is `inst/nextflow/modules/assemble_workflow.nf:6-22`. It is already a **per-sample** query: `FROM assemble a JOIN assemble_opts opts ON a.assemble_opts = opts.assemble_opts JOIN samples s ON a.ID = s.ID` (`:14-18`), one row per sample with the option-set columns duplicated onto every row.
- `opts.maptoref_ref` is selected at `:12` and lands at `it[19]`; `it[20..23]` are the other four MapToRef options.
- `grep -n "it\[19\]"` over that file returns exactly one hit: `:123`, the staged-file expression. Changing the SELECT expression in place therefore moves nothing.
- The channel is crossed by ID at `:189` (`.cross(assemble_opts)`) and mapped at `:190-203`, which emits ten elements ending `it[1][8]` (the MapToRef reference) at `:201`.
- The **second** `.cross(assemble_opts)` at `:398` reads only `it[0][0]` (`:399-407`), so a longer opts tuple cannot disturb it.
- The options map is built at `:104-114`; the four existing MapToRef keys are `:110-113`, with `maptoref_topology: (it[23] ?: "")` last and carrying no trailing comma.
- The staged file is `file((it[19] != null && it[19].toString().trim()) ? it[19] : "${projectDir}/assets/NO_FILE")` at `:123`.
- Script-level `def` functions are the module idiom, and the precedent for calling one **from inside a channel-operator closure** (which is what `maptorefAccession()` does in the `.multiMap`) is `inst/nextflow/modules/circularize_workflow.nf:50` defined / `:70` called, and `inst/nextflow/modules/scaffold_join_workflow.nf:87` defined / `:223` called. (`blast_genbank_workflow.nf:7`/`:18` are script-level `def`s too, but they are only called from script-level string interpolation at `:75`/`:81`.)
- `assemble.nf`'s shell preamble is `:22-24` (`workingDir`, `outDir`); the MapToRef branch is `:103-112`, and the `Rscript -e` call is `:105`. The input tuple is `:16` and the outputs are `:19-20`; none of those change.
- The in-process default pattern for the local BLAST database is `inst/nextflow/modules/blast_genbank.nf:79-83`, with the reason in the comment at `:79-81`. `export BLASTDB='!{db_dir}'` is `blast_genbank.nf:212`; `find_mito.nf:30` does the same. `export NCBI_API_KEY='!{params.ncbi_api_key ?: ""}'` is `blast_ref_fetch.nf:84`.
- **Neither file lints clean today.** Measured with `nextflow lint` (NF 25.10.6 and 26.04.6): `assemble.nf` has **three** warnings - `:8` launchDir, `:22` deprecated `shell:`, `:24` `outDir` declared but not used - and `assemble_workflow.nf` has one pre-existing **error** (`n++` at `:243`). Step 5's edit adds exactly **one** new warning, `Variable was declared but not used` for `mtr_db`, for the same reason `outDir` is already flagged: the linter does not read `!{mtr_db}` inside the `'''` shell string. The pass condition is baseline equivalence plus that one expected warning.

**Files:**
- Modify: `inst/nextflow/modules/assemble_workflow.nf`
- Modify: `inst/nextflow/modules/assemble.nf`

**Interfaces:**
- Consumes: Task 2's `assemble.maptoref_ref` column, Task 6's `map_to_ref(..., ref_value, blast_db)`.
- Produces: a working per-sample reference through WF1. An accession-shaped effective value stages `NO_FILE` and reaches R as `opts.maptoref_value`; anything else stages as a file exactly as today.

- [ ] **Step 1: Capture the lint baseline before touching anything**

```bash
NXF_VER=25.10.6 nextflow lint inst/nextflow/modules/assemble.nf inst/nextflow/modules/assemble_workflow.nf > /tmp/mtr_lint_before.txt 2>&1
cat /tmp/mtr_lint_before.txt
```

Keep the output. Step 6 compares against it; the pass condition is equivalence plus one expected new warning, not "no errors". The baseline is **not** clean: `assemble.nf` has three warnings (launchDir at `:8`, deprecated `shell:` at `:22`, `outDir` declared but not used at `:24`) and `assemble_workflow.nf` has a pre-existing error (`n++` at `:243`). Record the actual numbers you see; they are what Step 6 is judged against.

- [ ] **Step 2: COALESCE the select in place**

In `inst/nextflow/modules/assemble_workflow.nf`, replace this line:

```groovy
                  'opts.maptoref_ref, opts.maptoref, opts.maptoref_consensus, ' +
```

with:

```groovy
                  // Per-sample reference wins when it is set and non-blank; the
                  // parameter set is the default. Position 19 is unchanged.
                  // Same expression as .mtr_warn_missing_refs() in R/map_to_ref_refs.R.
                  "COALESCE(NULLIF(TRIM(a.maptoref_ref), ''), " +
                  "NULLIF(TRIM(opts.maptoref_ref), '')), " +
                  'opts.maptoref, opts.maptoref_consensus, ' +
```

Double-quoted Groovy strings because a single-quoted string cannot hold `''` without escaping; there is no `$` in either line, so they are plain Strings, not GStrings.

- [ ] **Step 3: Add the accession helper**

In the same file, immediately after the `include {assemble} from './assemble.nf'` line at the top:

```groovy

// An NCBI nucleotide accession is resolved inside the task (local BLAST database
// first, NCBI second), so it must not be staged as a file. Keep this pattern
// identical to .mtr_acc_re in R/map_to_ref_refs.R.
def maptorefAccession(v) {
    def s = (v ?: '').toString().trim()
    s ==~ '(?i)^[A-Z]{1,2}_?[0-9]{5,9}(\\.[0-9]{1,3})?$' ? s.toUpperCase() : ''
}
```

A single-quoted Groovy string, not a slashy `/.../` literal, so no `$` can be read as interpolation.

- [ ] **Step 4: Carry the raw value and divert an accession from staging**

In the options map, give `maptoref_topology` a trailing comma and append one key:

```groovy
                        maptoref_topology: (it[23] ?: ""),                      // MapToRef reference topology
                        maptoref_value: ((it[19] ?: "").toString().trim())      // raw reference: path, URL, or accession
```

Replace the staged-file line with:

```groovy
                    file((it[19] != null && it[19].toString().trim() && !maptorefAccession(it[19])) ? it[19] : "${projectDir}/assets/NO_FILE")  // MapToRef reference (accessions resolve in-task)
```

Same element, same tuple position; only the condition changed.

- [ ] **Step 5: Pass the value and the database into the MapToRef branch**

In `inst/nextflow/modules/assemble.nf`, extend the shell preamble (after `outDir = "${workingDir}/${opts_id}"`):

```groovy
    // Local BLAST database for accession references. Defaulted here rather than
    // required in .config, the same way blast_genbank.nf:79-83 does it: an old
    // .config or a saved cluster profile may not carry these keys.
    mtr_db_dir = params.blast_gb?.db_dir ?: '/ref_dbs/mito_metazoa'
    mtr_db = mtr_db_dir + '/' + (params.blast_gb?.db_name ?: 'mito_metazoa')
```

In the MapToRef branch, replace the two lines `mkdir -p !{outDir}` and the `Rscript -e` call with:

```bash
        mkdir -p !{outDir}
        # An accession reference resolves from the bundled BLAST database first,
        # so an air-gapped run still works; BLASTDB is how blastdbcmd finds it.
        export BLASTDB='!{mtr_db_dir}'
        export NCBI_API_KEY='!{params.ncbi_api_key ?: ""}'
        Rscript -e "MitoPilot::map_to_ref('!{id}', '!{ref}', '!{reads[0]}', '!{reads[1]}', '!{opts.maptoref}', '!{opts.maptoref_consensus}', !{opts.maptoref_iter}, '!{opts.maptoref_topology}', !{genetic_code.intValue()}, !{opts.cpus}, '!{outDir}', '!{opts.maptoref_value}', '!{mtr_db}')"
```

Everything below it in the branch (the `opts.txt` echo, the reads tarball, the work-dir note, and the closing `fi`) is unchanged context; do not retype it.

The input tuple at `assemble.nf:16` and both output declarations at `:19-20` are **not** touched.

- [ ] **Step 6: Lint and diff against the baseline**

```bash
NXF_VER=25.10.6 nextflow lint inst/nextflow/modules/assemble.nf inst/nextflow/modules/assemble_workflow.nf > /tmp/mtr_lint_after.txt 2>&1
diff /tmp/mtr_lint_before.txt /tmp/mtr_lint_after.txt
```

Expected: line numbers shift, and **exactly one** new finding is acceptable - `Variable was declared but not used` for `mtr_db` at the new preamble line in `assemble.nf`. That one is not a defect: the linter cannot see `!{mtr_db}` inside the `'''` shell string, which is why `outDir` is already flagged the same way at `:24`. **Any other new warning, and any new error, is real** - most likely an unbalanced quote in the Groovy regex string or a missing comma in the options map.

- [ ] **Step 7: Confirm no index moved**

```bash
grep -n "it\[19\]" inst/nextflow/modules/assemble_workflow.nf
grep -n "it\[1\]\[8\]\|it\[1\]\[9\]" inst/nextflow/modules/assemble_workflow.nf
```

Expected: `it[19]` appears exactly twice now (the opts-map key and the staged-file expression) and nowhere else; `it[1][8]` still appears once (the `.cross` map) and `it[1][9]` does not appear at all. **No line-count check**: the two greps above are what actually protect the tuple indices, and a count is only a proxy that goes stale the moment a comment is reworded.

- [ ] **Step 8: ASCII check**

```bash
grep -nP '[^\x00-\x7F]' inst/nextflow/modules/assemble.nf inst/nextflow/modules/assemble_workflow.nf
```

Expected: no output.

---

### Task 8: App relax, help text, and documentation

The app half of R8 and all of R10. The modal stops refusing an empty reference (the parameter set is now allowed to have none, because samples may bring their own) and stops demanding a topology for an accession. The help text learns about accessions. The docs learn about the `Reference` column, `set_maptoref_refs()`, `maptoref_fetch_accession()`, and the `-resume` trap.

Reference facts this task depends on (verified 2026-09-03 at HEAD `e93c403`):
- `R/app_assemble.R:1002-1081` is the option-set save handler inside `if (input$edit_assemble_opts) {`. `ref_value` is assigned at `:1005` and `topology_value` at `:1006`; both are still read at `:1018`, `:1020`, `:1061`, and `:1073`, so neither assignment may be deleted.
- The "Reference required" alert is `:1007-1016`; the `needs_topology` condition is `:1017-1020` and its alert is `:1021-1031`; the quote alert is `:1032-1044` and today tests only `paste(input$maptoref %||% "", input$maptoref_consensus %||% "")` against `['\"]`; the upsert is `:1045-1078` and writes `maptoref_ref = if (nzchar(ref_value)) ref_value else NA_character_` at `:1061`.
- The modal's reference field is `R/app_assemble_utils.R:376-388`: label at `:378`, help text at `:383-386`.
- The Assemble table hides unknown columns by default (`R/app_assemble.R:246`), so no table change is needed.
- `_pkgdown.yml`: the "Project database" section is `:80-90`; `update_sample_seqdata` is `:88` and the section **ends with `export_db_to_csv` at `:90`** (`backwards_compatibility` is `:89`), so "after `update_sample_seqdata`" means inserting at `:89`, not appending to the end of the section. `map_to_ref` and `maptoref_prepare_ref` are `:155-156`.
- `vignettes/Test-Project-Assemble.Rmd:172-174` is the "MapToRef Reference" bullet in the Assemble-options field list, which describes the field this task relabels.
- `NEWS.md:1-14` is the `# MitoPilot 1.5.5` heading and its `### Map-to-reference assembly` bullets.
- `README.Rmd:139-143` and `README.md:167-172` are the matching "Assembly references" bullets. README.md is generated from README.Rmd but is edited by hand here with matching text, so no knit reflow lands in the diff (ruling C-1).
- `vignettes/Your-Own-Project.Rmd:194-215` is the MapToRef `new_project()` example; `vignettes/custom_dbs.Rmd:149-176` is "Reference mitogenome for MapToRef", ending with the container-mount warning box.
- `R/app_run_pipline.R:262-272` is the `-resume` toggle referenced by D22.

**Files:**
- Modify: `R/app_assemble.R`, `R/app_assemble_utils.R`
- Modify: `R/init_db.R` (one roxygen `@param`), `R/init_project.R` (one roxygen `@param`)
- Modify: `_pkgdown.yml`, `NEWS.md`, `README.Rmd`, `README.md`
- Modify: `vignettes/Your-Own-Project.Rmd`, `vignettes/custom_dbs.Rmd`, `vignettes/Test-Project-Assemble.Rmd`
- Regenerated: `man/`

**Interfaces:**
- Consumes: everything from Tasks 1-7. In particular Task 1's `.mtr_ref_class()` and `.mtr_bad_chars_re`, which `R/app_assemble.R` now calls directly, and Task 4's `set_maptoref_refs()`, which the docs `\link{}` to.
- Produces: an option set can be saved with an empty reference or with an accession; the exported surface is documented and indexed. No new UI.

- [ ] **Step 1: Relax the modal save handler**

In `R/app_assemble.R`, delete the whole "Reference required" block - the `if (identical(input$assembler, "MapToRef") && !nzchar(ref_value)) { ... return() }` including its `show_alert`. **Keep** the two `ref_value <-` and `topology_value <-` assignments above it.

Then narrow the topology condition so an accession is exempt:

```r
        needs_topology <- identical(input$assembler, "MapToRef") &&
          nzchar(ref_value) &&
          !identical(.mtr_ref_class(ref_value), "accession") &&
          !grepl("\\.(gb|gbk|gbff)$", ref_value, ignore.case = TRUE) &&
          !nzchar(topology_value)
```

Then extend the **existing** quote alert (`:1032-1044`) to cover the reference, with the same character set the validator uses. Task 7 Step 5 makes this value shell-interpolated for the first time (`'!{opts.maptoref_value}'` inside the `Rscript -e "..."` line), so a reference typed as `/data/it's/ref.gb` would break the R expression, the task would exit non-zero, and `failOnIgnore = true` would end the whole run:

```r
        if (identical(input$assembler, "MapToRef") &&
            grepl(.mtr_bad_chars_re, paste(ref_value,
                                           input$maptoref %||% "",
                                           input$maptoref_consensus %||% ""))) {
```

and change that alert's `text=` from "The bowtie2 and samtools consensus option strings are passed through a shell call, so they cannot contain a single or double quote." to:

```r
            text = paste("The reference, bowtie2, and samtools consensus values",
                         "are passed through a shell call, so they cannot",
                         "contain a quote, dollar sign, backtick, or backslash."),
```

Nothing else in the handler changes: `:1061` already stores `NA_character_` for an empty value. No reader call and no network call on save - ruling A3 stands, and a Shiny save handler must not block on HTTP or on a path that only exists on the compute node.

- [ ] **Step 2: Update the modal label and help text**

In `R/app_assemble_utils.R`, replace the reference field's label and help text:

```r
          label = "MapToRef Reference (.gb, FASTA, URL, or NCBI accession):",
```

```r
            "Path, URL, or NCBI nucleotide accession (for example NC_002333) of ",
            "one complete mitogenome to map against. An accession is looked up in ",
            "the container's bundled mitogenome BLAST database first and ",
            "downloaded from NCBI only if it is not there. A file ending .gb, ",
            ".gbk, or .gbff is read as GenBank and takes its topology from the ",
            "LOCUS line; anything else is read as FASTA and needs the topology ",
            "set below. Leave this empty if every sample brings its own reference ",
            "(mapping file 'Reference' column, or set_maptoref_refs()).",
```

- [ ] **Step 3: Update the two roxygen `@param` blocks**

`R/init_db.R`, the `@param maptoref_ref` entry:

```r
#' @param maptoref_ref Default MapToRef reference mitogenome for the parameter
#'   set: an absolute file path, a URL, or an NCBI nucleotide accession (for
#'   example NC_002333). A single-record GenBank file (.gb) is preferred; a
#'   FASTA is accepted but then \code{maptoref_topology} must be set. Optional:
#'   samples may instead name their own reference in the mapping file's
#'   \code{Reference} column or through \code{\link{set_maptoref_refs}}.
```

`R/init_project.R`, the `@param mapping_fn` entry, gains the reserved-column sentence:

```r
#'   May include additional columns with other sample metadata, and an optional
#'   `Reference` column naming a per-sample MapToRef reference (file path, URL,
#'   or NCBI accession). `Reference` is a reserved column name: it is never
#'   stored as sample metadata, and its values are checked whatever the
#'   assembler, so rename the column if you use it for something else.
```

Add the same sentences to `@param update_mapping_fn` in `R/add_samples.R` if Task 3 did not already.

- [ ] **Step 4: Index the two new exports**

`_pkgdown.yml`, in the "Project database" section (`:80-90`) immediately after `update_sample_seqdata` (`:88`) and **before** `backwards_compatibility`/`export_db_to_csv` - not at the end of the section:

```yaml
  - set_maptoref_refs
```

and after `maptoref_prepare_ref`:

```yaml
  - maptoref_fetch_accession
```

- [ ] **Step 5: NEWS**

In `NEWS.md`, under the existing `### Map-to-reference assembly` bullets, add:

```markdown
- **Per-sample references.** Each sample can now use its own MapToRef reference. Add an optional `Reference` column to the mapping CSV (an absolute file path, a URL, or an NCBI nucleotide accession such as `NC_002333`), or set them later with `set_maptoref_refs(path, refs)` from a two-column CSV. A sample's own reference wins; the parameter set's reference is the fallback, and it may now be left empty.
- **Accession references resolve offline first.** An accession is pulled from the mitogenome BLAST database bundled in the container before NCBI is contacted, so air-gapped runs work. The reference source (`file`, `url`, `local_db`, or `ncbi`) is recorded in each sample's summary and assembler log. `maptoref_fetch_accession()` exposes the same lookup on its own.
- References are validated when the project is created or when `set_maptoref_refs()` runs: missing files, unreachable URLs, and non-existent accessions are reported together, before anything is written. `Reference` is now a reserved mapping-file column name and is checked whatever the assembler; rename the column if you were using it for something else.
- Upgrading changes the assemble task signature, so the first run after the upgrade re-runs any still-queued assemble task even with `-resume`. Samples already at state 2 are unaffected, because they never enter the channel.
```

- [ ] **Step 6: README**

Edit `README.Rmd` and `README.md` by hand with matching text. In the "Assembly references" bullet, after "MapToRef instead uses a single reference mitogenome you supply yourself", add:

```
Each sample can use a different one: add a `Reference` column to your mapping CSV, or call `MitoPilot::set_maptoref_refs()`. A reference may be a file path, a URL, or an NCBI accession.
```

Re-wrap `README.md` to its existing indentation and line width by hand; do not knit.

- [ ] **Step 7: Vignettes**

`vignettes/Your-Own-Project.Rmd`, after the existing MapToRef example block, add:

````markdown
`maptoref_ref` may also be an NCBI nucleotide accession, which MitoPilot looks up
in the mitogenome BLAST database bundled in the container before falling back to
downloading it:

```r
new_project(assembler = "MapToRef", maptoref_ref = "NC_002333.1")
```

To give each sample its own reference, add a `Reference` column to your mapping
CSV holding a file path, a URL, or an accession; blank cells fall back to
`maptoref_ref`. The column never becomes sample metadata. If your reference list
is not ready at project creation, leave `maptoref_ref` empty (MitoPilot warns
which samples have no reference) and supply the list later:

```r
set_maptoref_refs(refs = "my_refs.csv")   # column 1: sample ID, column 2: reference
```

Every value is checked when it is set: a path must exist and be a readable
single-record GenBank or FASTA mitogenome, a URL must be reachable, and an
accession must exist at NCBI. Every bad value in the file is reported at once.
Note that the existing examples above assume the reference file is really there;
they now fail immediately if it is not.
````

`vignettes/custom_dbs.Rmd`, in "Reference mitogenome for MapToRef", before the container-mount warning box, add:

```markdown
You can skip the download entirely and give the accession instead (for example
`NC_002333.1`). MitoPilot extracts the record from the mitogenome BLAST database
bundled in the container, and only contacts NCBI if it is not there, so a run on
an offline compute node still works. A record taken from the local database has
no topology of its own, so it is treated as circular; the junction-depth check
publishes it as linear if no reads span the seam.

If a sample's reference cannot be resolved at run time, only that sample fails,
with a `[maptoref]` note explaining why. One caveat: because a MapToRef failure
is a normal per-sample outcome rather than a crashed task, Nextflow caches it.
If you fix a transient network problem and re-run with the same reference, untick
`-resume` so the sample is attempted again; changing the reference changes the
task and re-runs on its own.
```

`vignettes/Test-Project-Assemble.Rmd`, replace the "MapToRef Reference" bullet (`:172-174`) - it documents the very modal field Steps 1 and 2 change:

```markdown
- **MapToRef Reference.** One complete mitogenome: a file path (GenBank `.gb`
  preferred, one record per file), a URL, or an NCBI nucleotide accession such as
  `NC_002333`. An accession is taken from the mitogenome BLAST database bundled in
  the container before NCBI is contacted. Keep a reference file inside the project
  folder so the container can see it. A FASTA reference is accepted, but you must
  also set the topology. Leave this empty if every sample brings its own reference
  (the mapping file's `Reference` column, or `set_maptoref_refs()`).
```

(`vignettes/Difficult-Assemblies.Rmd:18-52` was checked and needs no change: it describes MapToRef's output shape, not its reference.)

- [ ] **Step 8: Regenerate, run the full suite, and check the diff is ASCII**

```bash
Rscript -e 'devtools::document()'
Rscript -e 'devtools::test()'
git diff -U0 -- README.Rmd README.md NEWS.md vignettes/ _pkgdown.yml | grep -nP '^\+.*[^\x00-\x7F]'
grep -nP '[^\x00-\x7F]' R/app_assemble.R R/app_assemble_utils.R
```

Expected: `FAIL 0 | WARN 0 | SKIP 23 | PASS >= 2030` plus the new cases; both greps print nothing.

- [ ] **Step 9: Confirm the app still starts and the modal saves**

```bash
Rscript -e 'devtools::load_all(quiet = TRUE); print(exists("assemble_opts_modal"))'
Rscript -e 'devtools::check(document = FALSE, args = c("--no-manual", "--no-tests", "--no-build-vignettes"), error_on = "error")'
```

Expected: `TRUE`, and the check reports no ERROR. Warnings about the pre-existing undocumented-import situation are baseline; compare against `git stash`-ed output if any look new.

---

### Task 9: End-to-end verification in the container (R11)

Nothing in Tasks 1-8 proves the feature works, because the resolver and `map_to_ref()` run **inside** the container, and the accession regex exists twice (R and Groovy) with no test that crosses the boundary. This task rebuilds the image with the new package and makes **ONE** WF1 run on a fresh **four**-sample project whose parameter set has **no** reference, with four per-sample `Reference` values that between them exercise every provenance label (`file`, `local_db`, `ncbi`, `url`), the local-DB circular default, the GenBank LOCUS linear path, and the URL-staging path that no unit test can reach.

One run, not two. There is no bogus-`db_dir` run and no `assemble_switch` juggling: `params.blast_gb.db_dir` is shared with `blast_genbank.nf:82`, so pointing it at a missing directory silently changes what the rest of WF1 does and can end the run `ERR` on a project with `remote_fallback = 0`. The NCBI arm is forced instead by a real accession that is measured absent from the bundled database.

Reference facts this task depends on (verified 2026-09-03 at HEAD `e93c403`, plus `dev/map_to_ref_refs_sdd/probes.md`):
- `docker/deploy-local.sh:23` already runs `rm -f docker/MitoPilot_*.tar.gz` (with the COPY-glob reason in the comment at `:21-22`), and `:13-18` already fails early when `docker/mito_metazoa_blastdb.tar.gz` is missing. The brief's manual "delete stale tarballs first" step is therefore dropped.
- `docker/Dockerfile:82` unpacks the BLAST database to `/ref_dbs/`; `:83-84` smoke-tests it with `BLASTDB=/ref_dbs/mito_metazoa blastdbcmd -db /ref_dbs/mito_metazoa/mito_metazoa -info`.
- **The prior e2e recipe is `dev/map_to_ref_e2e/setup2.R` and its report `dev/map_to_ref_e2e/REPORT.md`.** It subset `inst/test_data/mapping_test.csv` to three IDs, copied each sample's shipped subsampled reads into `<proj>/data/`, copied references into `<proj>/ref/`, and called `new_project(path, mapping_fn, mapping_id = "ID", data_path = file.path(proj, "data", ""), genetic_code = NULL, container = "macguigand/mitopilot:1.5.5", min_depth = 500, executor = "local", Rproj = FALSE, force = TRUE)`. Reuse it verbatim, with a fourth sample and a `Reference` column.
- **The pipeline was launched from the project directory** (the config's JDBC URL is the relative `jdbc:sqlite:.sqlite`):
  `nextflow -log <proj>/.logs/nextflow.log run <repo>/inst/nextflow -c <proj>/.config -entry WF1 -resume`
  on host Nextflow 25.10.6 (`/home/dmacguig/bin/nextflow`).
- **Wall clock, measured in the prior e2e:** project creation ~3 s; a three-sample WF1 GetOrganelle run 5m 20s; a two-sample MapToRef `-resume` run 4m 15s; the `assemble` task itself is 4-5 s per MapToRef sample. Most of a run is `blast_ref_fetch` waiting out NCBI HTTP 429 back-offs (120 s per retry with no API key). Budget 10-15 minutes for the single four-sample run below, plus the image build.
- `new_project()` copies the mapping file to `<proj>/mapping.csv` verbatim (`R/init_project.R:82-85`) and passes `...` straight to `new_db()` (`:125-133`), so `assembler = "MapToRef"` and the extra `Reference` column both survive.
- `assemble` runs with `errorStrategy ... 'ignore'` (`inst/nextflow/modules/assemble.nf:10`) and `workflow.failOnIgnore = true` (`inst/config.local:102`), so a genuine crash makes the whole run exit non-zero. A MapToRef per-sample failure exits 0 and does not.
- An old image's `map_to_ref()` has eleven formals, so the thirteen-argument call raises `unused arguments` and the task exits non-zero. Nothing is silent, but nothing is tested either - hence the version check in Step 2.
- **The four references, and why each one** (all measured in probes.md):
  - `NC_002333` (Danio rerio, 16,596 bp, circular) **is** in the bundled database; `-entry` resolves it in bare, versioned, and lowercase form.
  - `NC_001638` (Chlamydomonas reinhardtii mitochondrion, 15,758 bp) is **not** in the bundled database, is **LINEAR** in GenBank, and is a real record, so it forces the NCBI arm with the real `db_dir` in place and exercises LOCUS-wins topology at the same time. Being non-metazoan it will give a graceful per-sample failure or a heavily divergent product; **either is a pass**, a run crash is not.
  - `https://raw.githubusercontent.com/Smithsonian/MitoPilot/main/ref_dbs/MitoFinder/NC_002333_Danio_rerio.gb` returns HTTP 200 and is a single-record `.gb`, so it is stageable by Nextflow and readable by `maptoref_prepare_ref()`.
- **The four sample IDs** come from `inst/test_data/mapping_test.csv`, preferring the three the prior e2e used plus one more, all with shipped subsampled reads: `SRR22396640` (Xyrichtys novacula), `SRR21844202` (Fundulus majalis), `SRR22396627` (Gigantura indica), and `SRR22396758` (Upeneus parvus).

**Files:** none tracked. Evidence goes under `dev/map_to_ref_refs_sdd/`.

**Interfaces:**
- Consumes: everything.
- Produces: R11's evidence - per-sample `reference_source=` and `accession=` in the published summaries, the `source=<file|url|local_db|ncbi>` field on each `assembler.log.txt` reference line, the resolved reference under `maptoref/`, the `assemble` rows, and a run that does not end `ERR`.

- [ ] **Step 1: Rebuild the image**

```bash
bash docker/deploy-local.sh 1.5.5
docker run --rm mitopilot:1.5.5 Rscript -e 'cat(as.character(utils::packageVersion("MitoPilot")), "\n")'
```

Expected: the build succeeds and the version prints `1.5.5`.

- [ ] **Step 2: Prove the image carries the new code**

```bash
docker run --rm mitopilot:1.5.5 Rscript -e 'cat(names(formals(MitoPilot::map_to_ref)), sep=" ")'
docker run --rm mitopilot:1.5.5 Rscript -e 'cat(is.function(MitoPilot::maptoref_fetch_accession), is.function(MitoPilot::set_maptoref_refs), "\n")'
```

Expected: the formals list ends `out_dir ref_value blast_db`, and both `is.function` results are `TRUE`. If not, the build picked up a stale tarball; stop and rebuild.

- [ ] **Step 3: Re-confirm `blastdbcmd -entry` in THIS image, and record the NCBI bodies**

```bash
for e in NC_002333 NC_002333.2 nc_002333 NC_001638; do
  echo "== -entry $e"
  docker run --rm mitopilot:1.5.5 bash -lc \
    "blastdbcmd -db /ref_dbs/mito_metazoa/mito_metazoa -entry $e -outfmt '%f' 2>&1 | head -1; echo exit=\${PIPESTATUS[0]}"
done
curl -s 'https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi?db=nuccore&id=NC_999999999&retmode=json'
```

Expected, matching `dev/map_to_ref_refs_sdd/probes.md`: the first three forms each print a `>NC_002333.2 ...` defline with `exit=0`, and `NC_001638` prints `Error: [blastdbcmd] Entry not found` with a non-zero exit. This is a re-confirmation in the freshly built image, not a discovery: if any of the first three now fails, the database in this image is not the one that was probed and the local arm must be reported as unusable. Save the `esummary` body verbatim for the report - it is the recorded shape `.mtr_esummary_found()` is written against (C3).

- [ ] **Step 4: Build the four-sample project with no option-set reference**

Create `dev/map_to_ref_refs_sdd/e2e_setup.R` (untracked evidence, not part of the package):

```r
setwd("/home/dmacguig/Documents/GitHub/MitoPilot")
devtools::load_all(quiet = TRUE)

proj <- normalizePath("dev/map_to_ref_refs_sdd/proj", mustWork = FALSE)
dir.create(file.path(proj, "data"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(proj, "ref"), recursive = TRUE, showWarnings = FALSE)

# Three IDs from the prior e2e plus one more; all have shipped subsampled reads.
ids <- c("SRR22396640", "SRR21844202", "SRR22396627", "SRR22396758")
mapping <- utils::read.csv(app_sys(file.path("test_data", "mapping_test.csv")))
mapping <- mapping[match(ids, mapping$ID), ]
for (i in seq_len(nrow(mapping))) {
  file.copy(app_sys(file.path("test_data", mapping$R1[i])), file.path(proj, "data"), overwrite = TRUE)
  file.copy(app_sys(file.path("test_data", mapping$R2[i])), file.path(proj, "data"), overwrite = TRUE)
}
file.copy(app_sys(file.path("test_data", "NC_002333_Danio_rerio.gb")),
          file.path(proj, "ref", "NC_002333_Danio_rerio.gb"), overwrite = TRUE)

# One reference per resolution path. The option set gets NONE.
mapping$Reference <- c(
  normalizePath(file.path(proj, "ref", "NC_002333_Danio_rerio.gb")),  # -> file
  "NC_002333",                                                        # -> local_db
  "NC_001638",                                                        # -> ncbi (absent locally, LINEAR)
  paste0("https://raw.githubusercontent.com/Smithsonian/MitoPilot/",  # -> url
         "main/ref_dbs/MitoFinder/NC_002333_Danio_rerio.gb")
)
readr::write_csv(mapping, file.path(proj, "mapping.csv"), quote = "none", na = "")

new_project(
  path = proj,
  mapping_fn = file.path(proj, "mapping.csv"),
  mapping_id = "ID",
  data_path = file.path(proj, "data", ""),
  genetic_code = NULL,
  container = "macguigand/mitopilot:1.5.5",
  min_depth = 500,
  executor = "local",
  Rproj = FALSE,
  force = TRUE,
  assembler = "MapToRef"        # note: NO maptoref_ref
)
cat("PROJECT CREATED\n")
```

```bash
Rscript dev/map_to_ref_refs_sdd/e2e_setup.R 2>&1 | tee dev/map_to_ref_refs_sdd/e2e_setup.log
sqlite3 dev/map_to_ref_refs_sdd/proj/.sqlite \
  "SELECT ID, maptoref_ref FROM assemble ORDER BY ID; SELECT assemble_opts, assembler, maptoref_ref FROM assemble_opts;"
sqlite3 dev/map_to_ref_refs_sdd/proj/.sqlite "PRAGMA table_info(samples);" | grep -c Reference
```

Expected: creation emits **no** "no reference" warning (every sample has one); the four `assemble.maptoref_ref` values are the absolute path, `NC_002333`, `NC_001638`, and the raw URL; `assemble_opts.maptoref_ref` is empty; and `samples` has no `Reference` column (the grep prints `0`). If `new_project()` errors on `NC_001638`, the ingest check reached NCBI and something is wrong with the record, not with the plan - report it, do not swap in a fake accession, because ingest rejects those before the run starts.

- [ ] **Step 5: One WF1 run**

```bash
cd dev/map_to_ref_refs_sdd/proj
date +%s > ../wf1.start
nextflow -log "$PWD/.logs/nextflow.log" run \
  /home/dmacguig/Documents/GitHub/MitoPilot/inst/nextflow \
  -c "$PWD/.config" -entry WF1 2>&1 | tee ../wf1.log
echo "exit=$?"
```

No `-resume`: this is a fresh project. Expected: the run ends `OK`, not `ERR`; `grep -c 'Missing output file' ../wf1.log` is `0`; and no `assemble` task is ignored. A `[maptoref]` per-sample failure on `SRR22396627` (the `NC_001638` sample) is a **pass**, because R6 says one sample failing must not fail the run.

- [ ] **Step 6: Collect the evidence**

```bash
cd dev/map_to_ref_refs_sdd/proj
for s in SRR22396640 SRR21844202 SRR22396627 SRR22396758; do
  echo "== $s"
  grep -E '^(accession|reference_source|reference_length|reference_topology|published_topology|failure)=' \
    out/$s/assemble/default/${s}_summary.txt
  grep -nE 'reference .* source=(file|url|local_db|ncbi)\)$' \
    out/$s/assemble/default/assembler.log.txt | head -3
  ls out/$s/assemble/default/maptoref/
done
sqlite3 .sqlite "SELECT ID, assemble_switch, topology, length, assemble_notes FROM assemble ORDER BY ID;"
```

Expected, one row per sample:

| sample | `Reference` | `reference_source` | also |
|---|---|---|---|
| `SRR22396640` | absolute path to `ref/NC_002333_Danio_rerio.gb` | `file` | `accession=NC_002333.2`, `maptoref/reference.gb` present |
| `SRR21844202` | `NC_002333` | `local_db` | `reference_topology=circular` (the local-DB default, D16), `maptoref/reference_NC_002333.fasta` present |
| `SRR22396627` | `NC_001638` | `ncbi` | `reference_topology=linear` read from the GenBank LOCUS line, `maptoref/reference_NC_001638.gb` present, and either a `failure=` summary with `<ID>_assembly_0.fasta` **or** a published but heavily divergent product - never a run crash |
| `SRR22396758` | the raw `https://` URL | `url` | the reference was staged by Nextflow (not diverted by `maptorefAccession()`), `accession=NC_002333.2` |

and, across all four:
- every `assembler.log.txt` reference line matches `reference .* source=(file|url|local_db|ncbi)\)$` - so the grep above prints one hit per sample;
- `assemble_notes` carries `[maptoref] ...` only where a genuine warning fired. **No note appears merely because provenance was recorded.** A provenance note on every sample means Task 6 wrote `reference_source` as a `note=` line by mistake.

- [ ] **Step 7: Prove the empty-reference failure path**

Clear one sample's reference and re-run just that sample:

```bash
cd dev/map_to_ref_refs_sdd/proj
sqlite3 .sqlite "UPDATE assemble SET maptoref_ref = NULL, assemble_switch = 1 WHERE ID = 'SRR22396640'; UPDATE assemble SET assemble_switch = 2 WHERE ID != 'SRR22396640';"
nextflow -log "$PWD/.logs/nextflow.log" run \
  /home/dmacguig/Documents/GitHub/MitoPilot/inst/nextflow \
  -c "$PWD/.config" -entry WF1 -resume 2>&1 | tee ../wf1_norefs.log
grep -E '^(assembler|failure)=' out/SRR22396640/assemble/default/SRR22396640_summary.txt
head -1 out/SRR22396640/assemble/default/SRR22396640_assembly_0.fasta
```

Expected: the run completes (not `ERR`); that sample publishes `SRR22396640_assembly_0.fasta` containing `>No assembly found`; its summary carries `failure=no MapToRef reference for this sample; ...`; and the other three are untouched. This is R6.

- [ ] **Step 8: Write the report**

Write `dev/map_to_ref_refs_sdd/E2E_REPORT.md`, same shape as `dev/map_to_ref_e2e/REPORT.md`, with: the image id and build time; the Step 3 `blastdbcmd -entry` results for all four forms and the raw `esummary` body for the known-bad accession; the four-row per-sample table from Step 6; the failure evidence from Step 7; each run's wall clock and exit status; and any deviation from this plan.

- [ ] **Step 9: Confirm the tree is clean of stray files**

```bash
git status --short
```

Expected: only the intended modifications from Tasks 1-8 plus untracked `dev/map_to_ref_refs_sdd/` artefacts. **No commit.**
---

## Self-Review

Run through this list before declaring the plan done. Each line is a defect a reviewer of the three source proposals actually found, or a trap this codebase has sprung before.

**Schema and migration**
- [ ] `assemble.maptoref_ref` was added to BOTH `R/init_db.R` and `R/init_db_userAsmb.R`. A fresh userAsmb project and a migrated one must have identical schemas, or every new userAsmb project reports itself out of date.
- [ ] The "already current" predicate clause reads `names(assemble_table)`, not `names(assemble_opts_table)`. Two tables now have a column called `maptoref_ref`.
- [ ] `schema_gaps()` gained the entry. Without it, an un-migrated project's next WF1 run dies at channel creation on `a.maptoref_ref` instead of telling the user to migrate.
- [ ] The migration has no backfill. NULL is the fallback sentinel; a backfilled `''` would be caught by `NULLIF(TRIM(...), '')` anyway but is still wrong.
- [ ] Both `expect_cols(con, "assemble", ...)` lists in `tests/testthat/test-backwards-compatibility.R` gained `"maptoref_ref"`.

**Validation**
- [ ] `new_db(assembler = "MapToRef")` with no reference **warns** and does not demand a topology. `trimws(NA)` is `NA` and `grepl(p, NA)` is `FALSE`; the guard clause is what stops the FASTA-topology check firing on an absent reference.
- [ ] An accession is exempt from the FASTA-topology demand, in `new_db()` **and** in the modal.
- [ ] The topology-value and quote checks still run **above** the new validation call, so their messages fire first and the three repaired tests keep their expectations.
- [ ] `.mtr_validate_refs()` never stores an error message as a value. `.mtr_check_ref_value()` returns two distinct shapes; there is no `"!"` sentinel and no blanket `tryCatch` around the whole per-value check.
- [ ] Every bad value is listed at once, with its sample ID, **in row order**: `bad` is indexed by row and compacted before the `stop()`, never appended to.
- [ ] The forbidden-character set is ``["'$`\\]`` in all three places - `.mtr_validate_refs()`, the modal's extended quote alert, and `map_to_ref()`'s `.mtr_fail()` refusal - and the message names quote, dollar, backtick, and backslash.
- [ ] An NCBI request failure (offline, 429, 403, unparseable body) is a **warning** and keeps the value. Only a definitive absence from a 200 response is an error, and an `"esummaryresult"` empty-id-list body counts as a definitive absence, not as unreadable.
- [ ] `.mtr_esummary_found()` is a pure function with its own tests against recorded bodies; the top-level `"error"` string is never parsed (it names only one bad id even when several are bad).
- [ ] The accession check makes ONE request per 200 accessions, never one per accession.
- [ ] `.mtr_content_problem()` cleans up its temp directory with `on.exit(unlink(...))`, and `.mtr_url_fetch()` unlinks its download on every failure path.
- [ ] A bad reference leaves no half-built `.sqlite`: the ingest and validation run before `DBI::dbConnect`.
- [ ] `Reference` is stripped **and validated for every assembler**, so `assemble.maptoref_ref` can never hold an unchecked value; the GetOrganelle regression test proves it and the `mapping_fn` roxygen says the name is reserved.
- [ ] `add_samples()` stops **unconditionally** on a project with no `assemble.maptoref_ref`, immediately after `on.exit(DBI::dbDisconnect(con))` and before the `samples` write, naming `MitoPilot::backwards_compatibility()`. The DROP COLUMN test proves nothing was half-applied.

**Classifier**
- [ ] `.mtr_ref_class()` does not call `file.exists()`. It runs inside the container too, where a host path does not exist, and R5's provenance labels are `file|url|local_db|ncbi` - never `path`.
- [ ] The Groovy copy is a single-quoted string, not a slashy literal: a slashy string interpolates `$`.
- [ ] The R and Groovy regexes are character-identical modulo Groovy's `(?i)` and doubled backslash, and each carries a comment naming the other.

**Run time**
- [ ] `map_to_ref()`'s two new arguments are defaulted, and all 14 existing calls in `tests/testthat/test-map-to-ref-loop.R` still pass.
- [ ] The empty-reference guard is conditional on the staged file being missing or 0 bytes. An unconditional `stop()` on `src == "none"` breaks every existing call.
- [ ] The resolver is called from `map_to_ref()`, not from a second `Rscript -e` in the shell branch. A shell-side non-zero exit plus `failOnIgnore = true` fails the run.
- [ ] Every local-database step degrades to NCBI: an empty `blast_db`, no `blastdbcmd` on PATH, and any non-zero exit or empty output from the single `-entry` call (whose stderr goes to the assembler log).
- [ ] There is exactly ONE `blastdbcmd` call: no version-stripped retry and no `-info` probe. Bare, versioned, and lowercase accessions all resolve (measured, `dev/map_to_ref_refs_sdd/probes.md`).
- [ ] The container default for `blast_db` is substituted only when it is `NULL`, so an explicit `""` still skips the local arm.
- [ ] A quote, dollar, backtick, or backslash in `ref_value` is a per-sample `.mtr_fail()`, not a broken `Rscript -e`.
- [ ] A local-DB FASTA defaults to `"circular"` only when `maptoref_topology` is unset.
- [ ] `reference_source=` is a plain summary key, never a `note=` line, and the log line is spelled `reference ... source=<src>)` - matching `reference .* source=(file|url|local_db|ncbi)\)$` - in Task 6 and Task 9 alike.

**Nextflow**
- [ ] `it[19]` still means the effective reference and `it[20..23]` did not move.
- [ ] The staged-file expression is the same tuple element in the same position; only its condition changed.
- [ ] `assemble.nf`'s input tuple and both output declarations are untouched.
- [ ] `BLASTDB` and `NCBI_API_KEY` are exported in the MapToRef branch.
- [ ] `nextflow lint` output equals the baseline captured before the edit plus **exactly one** expected new warning: `mtr_db` declared but not used in `assemble.nf`, for the same reason `outDir` is already flagged. Neither file lints clean today (three warnings in `assemble.nf`, one pre-existing error in `assemble_workflow.nf`), so "no findings" is the wrong gate.
- [ ] Task 7's index check is the two `grep -n` commands on `it[19]` and `it[1][8]`. There is no line-count assertion.

**Helper**
- [ ] `set_maptoref_refs()` takes columns by position, refuses a directory passed as the CSV path, refuses unknown IDs, duplicate IDs, and locked rows, clears on blank, and flips `assemble_switch` only for rows that actually changed - compared NA-safely, never with `%||%`.
- [ ] It returns `invisible(<IDs still without a reference>)`, and its validator call uses `context = "the reference list"`.
- [ ] It does not back up `.sqlite`, and it did not grow a refactor of the two existing inline backup blocks.
- [ ] `.mtr_warn_missing_refs()` returns early **twice**: on a userAsmb project (no `assembler` column in `assemble_opts`) and on an un-migrated project (no `maptoref_ref` column in `assemble`).

**Process**
- [ ] Nothing was committed and nothing was pushed. No Claude attribution anywhere.
- [ ] Every touched file is ASCII: `git diff --name-only | xargs grep -nP '[^\x00-\x7F]'` prints nothing for source files.
- [ ] The full suite is `FAIL 0 | WARN 0` with at least the 2030 baseline passes. Every fixture call that warns by design is wrapped in `suppressWarnings()` inside the test.
- [ ] The e2e was ONE run over four samples covering `file`, `local_db`, `ncbi`, and `url`, with the real `db_dir` throughout. No bogus-`db_dir` run: that key is shared with `blast_genbank.nf:82`.
- [ ] The e2e report re-confirms `blastdbcmd -entry` in the freshly built image (bare, versioned, lowercase, and the `NC_001638` miss) and records the raw `esummary` body for a known-bad accession.
- [ ] The `NC_001638` sample either failed gracefully per-sample or produced a divergent result; the run itself did not end `ERR`.
