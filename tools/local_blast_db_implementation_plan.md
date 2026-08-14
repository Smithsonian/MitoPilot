# Plan: switch the WF1 reference BLAST search from remote `core_nt` to a local metazoan mitogenome database

## 0. What was verified in this environment

Everything in this section was measured here, in `macguigand/mitopilot:1.5.1`, with `/media/dmacguig/Mockra/mitopilot_blastdb_build/db` bind-mounted at `/db`, using the real staged query FASTAs from `/home/dmacguig/Documents/MitoPilot_fish_test/work/blast_select_targets/`. It supersedes any conflicting assumption elsewhere in this document.

### 0.1 `-taxids` silently discards the restriction, and the guard file is NOT `taxdb.btd`

Query: `SRR22396740.1.blast_target.fasta` (a chordate), `-taxids 6656` (Arthropoda). BLASTDB set to a directory holding the `mito_metazoa.*` volume plus the listed taxonomy files.

| taxonomy files present | exit | stderr | hits returned |
|---|---|---|---|
| `taxdb.btd` + `taxdb.bti` + `taxonomy4blast.sqlite3` | 0 | clean | correct (arthropods only) |
| none | 0 | `The -taxids command line option requires additional data files.` | **chordate hits, unrestricted** |
| `taxdb.btd` + `taxdb.bti`, **no** `taxonomy4blast.sqlite3` | 0 | same notice | **chordate hits, unrestricted** |
| `taxonomy4blast.sqlite3` **only** | 0 | clean | correct (arthropods only) |

`taxonomy4blast.sqlite3` is the load-bearing file. A guard on `taxdb.btd` passes in the third row, which is a silently unrestricted search. Also verified: exporting `BLASTDB` is required even when `-db` is an absolute path and all three files sit beside the volume; without it the notice fires and the restriction is discarded.

Verified separately: `-negative_taxids 7711` with no reachable taxonomy files produces **the same stderr notice** and is also silently discarded. So a single unconditional stderr check covers both the `taxids` column and any `-negative_taxids` a user puts in `extra_opts`.

### 0.2 Other measured behaviour

| Check | Result |
|---|---|
| `blastn` in the image | `/opt/conda/bin/blastn`, `2.16.0+`, build Nov 27 2024. **Not installed by `docker/Dockerfile`** (arrives transitively from a bioconda install at `docker/Dockerfile:27-36`) |
| `blastdb_aliastool`, `blastdbcmd`, `makeblastdb` | all present in the image |
| `-taxids 999999999` (absent from DB) | exit **2**, `BLAST Database error: Taxonomy ID(s) not found in the mito_metazoa database.` |
| `-taxids Chordata` (non-numeric) | exit **1**, `BLAST query/options error: Invalid taxidlist file` |
| `-taxids 7711` / `-taxids 6656,7711` | exit 0, correct |
| stale `-remote` in `extra_opts` colliding with `-db` | exit **255**, `NCBI C++ Exception` |
| Runtime, one 16 kb mitogenome, `-num_threads 4`, `-max_target_seqs 5` | ~1.2 s warm, ~3.5 s cold |
| Peak RSS (`VmHWM`), same query, 4 threads | **672 MiB** (the `.nsq` is mmapped and scanned). Succeeds under `docker run --memory=1g` |
| E-values on top hits | all `0.0`. The `pident` tie-break at `blast_genbank_workflow.nf:369` and `:469` is preserved. Do not build mitigations for e-value inflation |
| `blastdbcmd -db mito_metazoa -entry all -outfmt "%a %T %l"` | works, gives accession + taxid + length with no NCBI traffic |
| `blastdb_aliastool -seqidlist` filtered alias | builds, `-taxids` works through it. Rejected for shipping: the generated `.nal` hardcodes `DBLIST "/db/mito_metazoa"` (absolute) and `SEQIDLIST keep.txt` (relative external file) |
| Tarball layout | `tools/build_local_blast_db.py:453` is `tar.add(dbdir, arcname="mito_metazoa")`, matching what `prepare_ref_db.nf:32-41` expects |

### 0.3 Finding A: RefSeq/GenBank duplication, verified on all six staged queries

The local DB carries 16,018 `NC_` records out of 171,915 (9.3%), and for many of them the byte-identical GenBank source is also present. Remote `core_nt` returned only the `NC_` copy. Measured rank 1, remote baseline (`out/*/assemble/default/blast_genbank_*.txt`) versus the local DB:

| query | remote rank 1 | local rank 1 | distinct genomes in the 5 candidate slots |
|---|---|---|---|
| `MULTISCAFF.1.1` | `NC_083028.1` | **`OR546180.1`** (identical dup) | 4 |
| `MULTISCAFF.1.2` | `NC_083079.1` | **`OR546244.1`** (identical dup) | 3 |
| `SRR21844202.1.1` | `OR582709.1` | `OR582709.1` | 5 |
| `SRR21844202.2.1` | `OR582709.1` | `OR582709.1` | 5 |
| `SRR22396740.1.1` | `NC_082563.1` | **`OR482471.1`** (identical dup) | **3** |
| `SRR22396794.1.1` | `OR499733.1` | `OR499733.1` | 5 |
| `SRR22396940.1.1` | `OR482444.1` | `OR482444.1` | **3** |

Three of seven queries flip rank 1 to a redundant accession, and three of seven lose two of five candidate slots to duplicates. This changes `assemble.blast_accession`, which GFF3 gets fetched (curated RefSeq annotation versus raw submitter annotation), the published `blast_ref_<accession>/` directory name, and the diversity of the annotate-details reference picker. Fix it before shipping (Step 1).

---

## 1. Dedup the database (no NCBI refetch needed)

`blastdbcmd` can dump accession, taxid, and sequence straight out of the existing DB, so the whole dedup is local and takes minutes, not a 35-minute rebuild against E-utilities. Doing it this way also avoids record drift between the verified DB and the shipped one.

1.1. Add `tools/dedup_local_blast_db.py` (or a `--dedup-from-db` mode on `tools/build_local_blast_db.py`; a standalone script is simpler and does not disturb the staged-build state machine):

- `blastdbcmd -db mito_metazoa -entry all -outfmt "%a\t%T\t%l"` to get the accession / taxid / length table.
- `blastdbcmd -db mito_metazoa -entry all -outfmt "%a\t%s"` streamed, SHA-256 over the uppercased sequence, grouped by hash.
- For each group with more than one member keep exactly one: prefer an accession matching `^NC_`, else the lexicographically first. Write every drop to `dropped_duplicates.tsv` (kept accession, dropped accession, length, taxid) so the decision is auditable.
- Emit `keep.txt` (kept accessions), `taxid_map.txt` (kept accession -> taxid, the format `makeblastdb -taxid_map` expects, see `tools/build_local_blast_db.py:387`), and `mito_metazoa_dedup.fa` via `blastdbcmd -entry_batch keep.txt`.
- `makeblastdb -in mito_metazoa_dedup.fa -dbtype nucl -parse_seqids -blastdb_version 5 -taxid_map taxid_map.txt -out mito_metazoa -title "..."`.
- Copy `taxdb.btd`, `taxdb.bti`, `taxonomy4blast.sqlite3` into the new db dir unchanged.
- Rewrite `VERSION` with the **new** `sequences` and `bases` counts (the current file at `/media/dmacguig/Mockra/mitopilot_blastdb_build/db/VERSION` reports 171915 / 2813598799) and add a `deduplicated_identical<TAB>True` line plus the drop count. Stale counts here become stale provenance in every project (Step 9).
- Re-tar with `arcname="mito_metazoa"`.

1.2. Do not change the DB name (`mito_metazoa`) or the tarball's top-level directory. Delivery depends on both.

1.3. Re-verify: record count drops by the number of dropped duplicates (upper bound 16,018); re-run the seven-query comparison in Step 10.2 and confirm rank 1 matches the remote baseline in all seven and every candidate list holds five distinct genomes.

SHA-256 catches only byte-identical pairs. A RefSeq copy that differs by one base, or is rotated relative to its GenBank source, survives. Report the actual matched count; do not assume all 16,018 collapse.

---

## 2. Delivery: bake the database into the container image

> **Superseded in part.** The decision to bake into the image stands, but the
> database is now staged locally as `docker/mito_metazoa_blastdb.tar.gz` and
> pulled in with `ADD`, rather than downloaded from a GitHub release asset at
> build time. No release asset is created and no `curl` runs during the build.
> See "Delivery and refresh" in `tools/local_blast_db_design.md` for the
> implemented approach. Steps 2.1 and 2.2 below are kept for the reasoning that
> led here; the `ARG MITO_BLASTDB_URL` block they describe is not what shipped.

Option B (staging the tarball through `prepare_ref_db`) is dropped, not deferred. `prepare_ref_db.nf:9-13` records in-repo that cross-run `storeDir` caching was tried and abandoned under Docker, and `prepare_ref_db.nf:20` hard-codes `executor 'local'`. That means a 336 MB download plus a 989 MB extraction on the driver on **every** run, including every `-resume`, landing on the submit node under SLURM/SGE. It would also break the `-resume` caching requirement in Step 10.6, since a session-UUID-keyed `path(db_dir)` input is a new path every session.

2.1. Upload the post-Step-1 `mito_metazoa_blastdb.tar.gz` as a **GitHub release asset**, under a database-specific tag (`blastdb-2026.08`) decoupled from the package version. It cannot live under `ref_dbs/` the way `ref_dbs/Mitos2/Metazoa_RefSeq235.tar.gz` (47 MB) does: GitHub rejects pushes over 100 MB, and this is 336 MB. Release assets allow up to 2 GB.

There is no CI image build (`.github/workflows/` holds only `pkgdown.yaml` and `R-CMD-check.yaml`; images come from the manual `docker/deploy-*.sh`), so the ordering is a manual step: **upload the asset first, then build the image**. A DB-specific tag means routine patch releases do not need the asset re-uploaded.

2.2. Add to `docker/Dockerfile`, **before** the `COPY docker/MitoPilot_*.tar.gz` at line 70, so R-code edits do not invalidate a 989 MB layer:

```dockerfile
# blastn arrives transitively today (bioconda getorganelle/mitos/trnascan pull it
# in). The local reference search depends on it and on BLAST DB v5 read support,
# so pin it explicitly rather than rely on a transitive solve.
RUN mamba install -y -c bioconda blast=2.16.0

# Local metazoan mitogenome BLAST database for the WF1 reference search.
# BLASTDB must point at this directory at run time or -taxids is silently ignored.
ARG MITO_BLASTDB_URL=https://github.com/Smithsonian/MitoPilot/releases/download/blastdb-2026.08/mito_metazoa_blastdb.tar.gz
RUN mkdir -p /ref_dbs && \
    curl -fsSL "$MITO_BLASTDB_URL" | tar -xz -C /ref_dbs && \
    BLASTDB=/ref_dbs/mito_metazoa blastdbcmd -db /ref_dbs/mito_metazoa -info && \
    test -s /ref_dbs/mito_metazoa/taxonomy4blast.sqlite3
```

`-f` on curl is load-bearing: without it a GitHub 404 or 403 returns exit 0 with an HTML body that gets piped into `tar`, failing with "not in gzip format" instead of the real cause. `blastdbcmd -info` is a real smoke test and is volume-agnostic (see Step 3.5); the `taxonomy4blast.sqlite3` check catches the silent-taxon-filter case from Step 0.1 at build time.

The `ARG` carries a real default, so `docker/deploy-local.sh:17`, `deploy-dockerhub.sh:29`, and `deploy-aws.sh:17` need no `--build-arg` and stay unchanged.

2.3. Add `.dockerignore` at the repo root. The context is currently 1.4 GB with none. Write it as an **allowlist**, because `.gitignore:20` is `/docker/MitoPilot*` and anyone deriving `.dockerignore` from `.gitignore` would exclude the package tarball the build needs (`docker/Dockerfile:70`), reproducing the known stale-tarball build failure:

```
*
!renv.lock
!docker/
```

The Dockerfile copies exactly three things: `renv.lock` (line 41), `docker/arwen/arwen1.2.3.c` (line 52), `docker/MitoPilot_*.tar.gz` (line 70).

2.4. Image size accounting for the release notes: the DB is 989 MiB uncompressed on disk but the registry transfers the **compressed** layer, roughly the tarball's 336 MB. Singularity SIF growth is squashfs-compressed, so also roughly 300 to 400 MB, not 1 GB. The real risk is not transfer, it is conversion: `singularity pull docker://` unpacks the whole image into `SINGULARITY_TMPDIR` (default `/tmp`) before building the SIF, and login nodes often have a small `/tmp`. `vignettes/NOAA-SEDNA.Rmd:139-142` already carries this workaround, commented out. Re-time a Hydra pull against `inst/config.NMNH_Hydra:13` (`pullTimeout = '60 min'`) before release.

---

## 3. Rewrite `inst/nextflow/modules/blast_genbank.nf`

Keep lines 1-4 (executor/container), line 11 (`cache 'lenient'` and its comment at 6-10, whose rationale is unchanged), line 13 (`maxForks`), and lines 23-35 (clusterOptions, publishDir, tag). Replace the rest.

3.1. **Error strategy.** Local compute on a scheduler still fails transiently: node failure, preemption, cgroup OOM kill, shared-FS stall, Singularity image-cache race when many tasks first touch the SIF. Do not drop retries wholesale; gate them on exit code so the loud, deterministic failures verified in Step 0.2 (2, 1, 255) fail immediately.

```groovy
    // Local search: no network, no rate limit, so no backoff. Bad DB path,
    // malformed extra_opts and unknown taxids all fail deterministically and
    // must not be retried. Node/OOM/preemption signals still get two attempts.
    errorStrategy { task.exitStatus in [104,134,137,139,140,143,247] && task.attempt <= 2 ? 'retry' : 'ignore' }
    maxRetries 2
```

Delete line 19 (`maxRetries { params.blast_gb.maxRetries }`) and the 60 s backoff at lines 50-53.

3.2. **Delete** the `NCBI_API_KEY` export at line 55. The independent one at `blast_ref_fetch.nf:58` stays.

3.3. **Input tuple** (line 38), arity 8. `entrez_query` is kept (it drives the legacy block in 3.6) and `taxids` is added:

```groovy
    input:
        tuple val(id), val(path_idx), path(assembly), val(opts_id), val(entrez_query), val(taxids), val(extra_opts), val(max_target_seqs)
```

3.4. **Output**, with named emits so the DB provenance file can be published without changing the existing tuple's shape:

```groovy
    output:
        tuple val(id), val(path_idx), path("${outDir}/${outFile}"), emit: hits
        path "${outDir}/blast_db_VERSION.txt", emit: db_version, optional: true
```

3.5. **Shell body.** `db_dir` and `db_name` default in the module and are only overridden by config (Step 3.8 explains why). Presence is proven with `blastdbcmd -info`, not a `.nsq` stat: the DB is single-volume today (`mito_metazoa.nsq`, 706,656,152 bytes), but a future refresh that crosses `makeblastdb`'s volume threshold yields `mito_metazoa.00.nsq` plus a `.nal` and no plain `.nsq`, which would make every task exit 1.

```groovy
    shell:
    outDir  = "${id}/assemble/${opts_id}"
    outFile = "blast_genbank_${path_idx}.txt"
    db_dir  = params.blast_gb?.db_dir  ?: '/ref_dbs/mito_metazoa'
    db_name = params.blast_gb?.db_name ?: 'mito_metazoa'
    // Omit -taxids entirely when unset: a blank value becomes a literal empty
    // flag, which blastn rejects as an invalid taxid list.
    tax_clean = (taxids ?: '').toString().trim()
    tax_flag  = tax_clean ? "-taxids ${tax_clean}" : ""
    // An Entrez query has no local equivalent. Treat the values that are no-ops
    // against a metazoan-mitogenome-only database as no-ops; anything else with
    // no taxid replacement is a restriction we cannot honour, so refuse to run.
    eq      = (entrez_query ?: '').toString().trim().toLowerCase().replaceAll(/\s+/, ' ')
    eq_noop = eq in ['', 'mitochondrion[location]', 'mitochondrion[filter]', 'biomol_genomic[prop]']
    blocked = (!eq_noop && !tax_clean) ? '1' : '0'
    '''
    mkdir -p !{outDir}

    if [ "!{blocked}" = "1" ]; then
        echo "This BLAST parameter set still carries an Entrez query that the local" >&2
        echo "BLAST database cannot apply, and no taxon restriction to replace it." >&2
        echo "Open BLAST Options, check Edit, optionally enter NCBI taxon IDs, and" >&2
        echo "click Update to clear the legacy Entrez query." >&2
        exit 1
    fi

    # BLAST locates taxonomy4blast.sqlite3 (and taxdb.*) via BLASTDB. Without it,
    # -taxids / -negative_taxids do NOT error: blastn prints a notice to stderr,
    # DISCARDS the restriction, exits 0, and returns hits from every taxon.
    # Verified against this database, including with taxdb.btd/.bti present but
    # taxonomy4blast.sqlite3 absent.
    export BLASTDB='!{db_dir}'
    if ! blastdbcmd -db "${BLASTDB}/!{db_name}" -info > /dev/null 2>&1; then
        echo "local BLAST database not readable: ${BLASTDB}/!{db_name}" >&2
        exit 1
    fi
    if [ ! -s "${BLASTDB}/taxonomy4blast.sqlite3" ]; then
        echo "${BLASTDB}/taxonomy4blast.sqlite3 is missing; taxon restrictions would be" >&2
        echo "silently discarded. Refusing to run." >&2
        exit 1
    fi

    cp "${BLASTDB}/VERSION" !{outDir}/blast_db_VERSION.txt 2>/dev/null || true

    if blastn \
        -db "${BLASTDB}/!{db_name}" \
        -query !{assembly} \
        -outfmt "6 qseqid saccver stitle pident qcovs evalue" \
        -max_target_seqs !{max_target_seqs ?: 5} \
        -max_hsps 1 \
        -task megablast \
        -num_threads !{task.cpus} \
        !{tax_flag} \
        !{extra_opts} \
        > !{outDir}/!{outFile} 2> blast.err; then
        # blastn exits 0 after silently discarding a taxon restriction it could
        # not apply. Unconditional so it also covers -negative_taxids passed
        # through extra_opts, which produces the identical notice.
        if grep -q 'requires additional data files' blast.err; then
            cat blast.err >&2
            echo "taxon restriction was silently discarded by blastn; refusing the result" >&2
            exit 1
        fi
        if [ ! -s !{outDir}/!{outFile} ]; then
            echo "NO_SIGNIFICANT_HITS" > !{outDir}/!{outFile}
        fi
    else
        cat blast.err >&2
        exit 1
    fi
    '''
```

3.6. The legacy-Entrez block lives here, in the task, and not as a driver channel branch. That is deliberate. A driver-side `UPDATE ... WHERE assemble_switch = 4` fed off `channel.fromQuery` cannot work: `fromQuery` snapshots the DB at session ignition, at which point every eligible sample sits at `assemble_switch = 1` (`assemble_workflow.nf:18` selects `IN (1, 4)`), so the UPDATE matches zero rows; and for the rare row already at 4, `params.sqlWriteAssemble` (`assemble_workflow.nf:26-27`, executed at `:203-204`) then overwrites `assemble_switch`, `assemble_notes`, and sets `poor_blast_ref=NULL`, wiping the note. Blocking in the task instead makes the sample fail with no output, which routes it through the **existing, ordering-correct** no-output write at `blast_genbank_workflow.nf:304-308`: state 3, `poor_blast_ref = 'failed'`, tagged `[blast]` note. No new SQL, no new channel branch, no race.

3.7. **`NO_SIGNIFICANT_HITS` stays exactly as is.** It has one writer (this file) and zero readers; it survives only because a one-field line fails the `parts.size() >= 6` filters at `blast_genbank_workflow.nf:207`, `:257`, `:338`. Do not clean it up.

3.8. `db_dir` / `db_name` default in the module rather than requiring a config key, because two supported upgrade paths leave `.config` without the key:
- `backwards_compatibility(update_config = FALSE)` migrates the DB only (`R/backwards_compatibility.R:45-47`), and the app's own stale-project banner tells users to run exactly that (`R/app_server.R:40`, `R/app_server_userAsmb.R:37`).
- `resolve_config()` (`R/generate_config.R:228-243`) prefers a saved user profile at `~/.local/share/MitoPilot/config.<name>` over the package built-in, and `generate_config()` (`:388-407`) froze that profile at creation time. `migrate_config()` (`:139`) regenerates from the same stale profile. `vignettes/Custom-HPC.Rmd:88-96` documents saved profiles as the recommended HPC path.

Without a module default, `params.blast_gb.db_dir` resolves to null and the guard prints `local BLAST database not readable: null/null` for every sample on every project.

---

## 4. Config templates

4.1. Add to the `blast_gb { }` block in all eight of `inst/config.{local,slurm,sge,pbs,lsf,awsbatch,NMNH_Hydra,NOAA_SEDNA}`:

```groovy
        cpus = 4                            // -num_threads for the local search
        memory = 4                          // measured peak RSS 672 MiB (mmapped .nsq)
        db_dir = '/ref_dbs/mito_metazoa'    // override only; the module defaults to this
        db_name = 'mito_metazoa'
```

Keep `maxForks = 10` everywhere. Do not add a `<<PLACEHOLDER>>` for any of these: `R/generate_config.R:192-196` warns and leaves `.config` untouched when a placeholder survives, and adding one means also editing the `fill_config` call sites at `R/generate_config.R:180`, `:388`, `R/init_project.R:145`, and `R/init_project_userAsmb.R:160`.

4.2. `inst/config.slurm:92-99`, `config.sge:93-100`, `config.pbs:92-99`, `config.lsf:92-99` have no `maxRetries` in `blast_gb`. After Step 3.1 nothing reads `params.blast_gb.maxRetries` (`blast_ref_fetch.nf:31` hard-codes `maxRetries 3`), so the key becomes vestigial. Leave it where it exists rather than churn `migrate_config`; do not add it to the four that lack it.

4.3. `inst/config.awsbatch` has **no** `workflow { failOnIgnore = true }` block; the other seven do (`grep -L failOnIgnore inst/config.*` returns exactly that one file). With `errorStrategy 'ignore'` as the terminal state, a bad `db_dir` or missing DB layer would mark every sample failed and still exit 0. Add the block:

```groovy
// pipeline will exit with a non-zero exit code if any failed tasks are ignored using the ignore error strategy
workflow {
  failOnIgnore = true
}
```

4.4. **Hydra.** `inst/config.NMNH_Hydra:20` sets `penv = 'mthread'` globally, and its generic process block at `:22-28` computes `mres = memory * attempt` with `h_data = h_vmem = mres / cpus`. The `blast_gb` override at `:99` is a fixed literal `"-l mres=32G,h_data=32G,h_vmem=32G,himem -S /bin/bash"`, which encodes cpus=1. Raising cpus to 4 against that literal requests 4 slots at 32 GB each versus a 32 GB total, on the scarce himem queue, for a job whose measured peak is 672 MiB. Replace it with a value consistent with the convention and drop `himem`:

```groovy
        clusterOptions = "-l mres=8G,h_data=2G,h_vmem=2G -S /bin/bash"
```

---

## 5. Decouple `blast_ref_fetch` from `blast_gb.cpus`

`inst/nextflow/modules/blast_ref_fetch.nf:14` reads `params.blast_gb.cpus`, and `:12` reads `params.blast_gb.maxForks`. Raising `cpus` to 4 for the local search would give every reference-fetch task (one `Rscript` doing HTTP) four cores, times `maxForks 10`; on `config.local` Nextflow packs the local executor by cpus, so ten concurrent fetches would become roughly two on an 8-core laptop, and on Hydra the fetch would inherit the same scheduler request as the search. That is a straight regression on the step that is still network-bound.

5.1. In `inst/nextflow/modules/blast_ref_fetch.nf`, replace line 14 with a literal:

```groovy
    // One Rscript doing HTTP. Never CPU-bound; deliberately not tied to
    // params.blast_gb.cpus, which now sizes the local blastn search.
    cpus 1
```

5.2. Leave `maxForks params.blast_gb.maxForks` at line 12 alone. It is a genuine NCBI rate limit for this process and 10 remains the right value for both.

5.3. `blast_ref_stamp` (same file, lines 69-111) declares no `cpus` and is unaffected.

---

## 6. New per-project option: `blast_opts.taxids`

Numeric NCBI taxon IDs only. No name resolution, therefore **no network anywhere in the R layer** for this feature. That keeps the save path working on air-gapped systems, keeps the migration from ever blocking on NCBI, and avoids the 5-attempt / `Sys.sleep(120L * attempt)` retry profile at `R/blast_ref_utils.R:216-255` (about 20 minutes worst case) from freezing a single-threaded Shiny session.

Follow the `max_target_seqs` precedent at `R/backwards_compatibility.R:1219-1227`, which uses live `dbListFields` because `blast_opts` is deliberately not snapshotted at `:74-80`.

6.1. **Fresh-project DDL + seed, standard.** `R/init_db.R:351-358`, add `taxids TEXT,` after `entrez_query TEXT,`; `R/init_db.R:362-368`, add `taxids = ""`.

6.2. **Fresh-project DDL + seed, userAsmb.** `R/init_db_userAsmb.R:311-318` and `:322-328`, same two edits.

6.3. **Migration ALTER.** In `R/backwards_compatibility.R`, immediately after the `max_target_seqs` block ending at line 1227:

```r
  # Local BLAST database replaces the remote core_nt search. -entrez_query has no
  # local equivalent; taxon restriction is now a comma-separated NCBI taxid list.
  if ("blast_opts" %in% DBI::dbListTables(con) &&
      !("taxids" %in% DBI::dbListFields(con, "blast_opts"))) {
    message("added taxids column to blast_opts table")
    DBI::dbExecute(con, "ALTER TABLE blast_opts ADD COLUMN taxids TEXT")
    DBI::dbExecute(con, "UPDATE blast_opts SET taxids = '' WHERE taxids IS NULL")
    # Normalize the no-op values so nothing is blocked for them. A non-trivial
    # Entrez query is deliberately LEFT IN PLACE: those samples must stop rather
    # than silently run unrestricted. Warn by name so the user learns at upgrade
    # time, not mid-run.
    DBI::dbExecute(con, paste(
      "UPDATE blast_opts SET entrez_query = 'mitochondrion[Location]'",
      "WHERE entrez_query IS NULL OR TRIM(entrez_query) = ''",
      "OR LOWER(TRIM(entrez_query)) IN ('mitochondrion[location]','mitochondrion[filter]','biomol_genomic[prop]')"
    ))
    legacy <- DBI::dbGetQuery(con, paste(
      "SELECT blast_opts, entrez_query FROM blast_opts",
      "WHERE entrez_query IS NOT NULL AND TRIM(entrez_query) <> ''",
      "AND entrez_query <> 'mitochondrion[Location]'"
    ))
    if (nrow(legacy) > 0) {
      warning(
        "These BLAST parameter sets carry an Entrez query the local BLAST ",
        "database cannot apply: ",
        paste0(legacy$blast_opts, " ('", legacy$entrez_query, "')", collapse = "; "),
        ". Samples using them will fail the BLAST step until you open BLAST ",
        "Options, check Edit, optionally enter NCBI taxon IDs, and click Update. ",
        "Taxon IDs: https://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi",
        call. = FALSE
      )
    }
  }
```

Normalizing no-ops to the literal `mitochondrion[Location]` rather than `''` is a rollback measure: an older MitoPilot reading a migrated DB emits `-entrez_query "mitochondrion[Location]"` and behaves exactly as before. Writing `''` would make a rolled-back remote search run against `core_nt` with no mitochondrion restriction, which is how a nuclear or NUMT hit becomes the reference.

6.4. **Table-absent DDL.** `R/backwards_compatibility.R:1456-1482` creates `blast_opts` from scratch when missing. Add `taxids TEXT` to that DDL and `taxids = ""` to its seed `data.frame`. That branch and 6.3 are mutually exclusive, so both must carry the column.

6.5. **Currency guard, mandatory.** `R/backwards_compatibility.R:140-233` is one monolithic `&&` chain whose `message("nothing to update")` at `:232-233` returns *before* the backup at `:240-257` and before every ALTER. Add a sibling to the `max_target_seqs` probe at `:165-168`:

```r
      isTRUE(tryCatch(
        "taxids" %in% DBI::dbListFields(con, "blast_opts"),
        error = function(e) FALSE
      )) &&
```

Omit this and any project satisfying every other clause is silently left on the old schema, then dies at WF1 ignition when `channel.fromQuery` issues `SELECT ... b.taxids`. `topology` and `ref_based_rc` have each hit this before.

6.6. **`schema_gaps()`.** `R/backwards_compatibility.R:1795-1821` does not probe `blast_opts` at all, so `R/app_server.R:31` and `R/app_server_userAsmb.R:28` would let an unmigrated project open cleanly, and the user would only find out when WF1 dies at session start with a raw SQLite backtrace. Add:

```r
  if (!has("taxids" %in% DBI::dbListFields(con, "blast_opts"))) {
    gaps <- c(gaps, "the blast_opts table lacks the 'taxids' column")
  }
```

6.7. Update the roxygen migration inventory at `R/backwards_compatibility.R:16-26` and run `devtools::document()`.

---

## 7. R-side UI plumbing

The BLAST-options modal is duplicated byte-for-byte across two flavours with no shared helper. Every edit below is two edits.

7.1. **Replace the Entrez input with a taxids input.** `R/app_assemble_utils.R:508-525` and `R/app_assemble_utils_userAsmb.R:228-245`. Rename the wrapper div id from `blast_entrez_group` to `blast_taxids_group` so the next person grepping "entrez" does not find a live code path. The rename touches ten sites total, listed in 7.3.

```r
        div(
          id = ns("blast_taxids_group"),
          tags$label(
            "Restrict search to taxa (optional) -",
            tags$a("NCBI Taxonomy Browser",
              href = "https://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi",
              target = "_blank"
            )
          ),
          textInput(
            ns("taxids"),
            label = NULL,
            value = current$taxids %||% "",
            placeholder = "e.g. 7711 for Chordata, or 7711,6656",
            width = "100%"
          ) |> shinyjs::disabled(),
          opts_help("Comma-separated numeric NCBI taxon IDs. Leave blank to search ",
                    "the whole metazoan mitogenome database. A taxon ID with no ",
                    "mitogenome in the database will fail the search, so prefer a ",
                    "broad clade. To exclude taxa instead, put -negative_taxids in ",
                    "Additional blastn options (it cannot be combined with a value ",
                    "in this field).")
        ),
```

7.2. **Legacy-query notice.** In the same two files, immediately after the div above, render a warning only when the stored `entrez_query` is non-trivial, so blocked users can see what they are losing and how to clear it:

```r
        if (!is.null(current$entrez_query) &&
            !tolower(trimws(current$entrez_query %||% "")) %in%
              c("", "mitochondrion[location]", "mitochondrion[filter]", "biomol_genomic[prop]")) {
          div(
            class = "alert alert-warning",
            style = "margin-top: 8px;",
            tags$b("Legacy Entrez query: "), tags$code(current$entrez_query),
            tags$p(style = "margin: 6px 0 0 0;",
                   "The local BLAST database cannot apply this. Samples using this ",
                   "parameter set will fail the BLAST step. Check Edit and click ",
                   "Update to clear it (enter taxon IDs above first if you want to ",
                   "keep a restriction).")
          )
        },
```

7.3. **Show/hide sites for the renamed div** (ten in total): `R/app_assemble_utils.R:561`, `R/app_assemble_utils_userAsmb.R:281` (the initial hide when `run_blast` is off), `R/app_assemble.R:1071`, `:1075`, `:1121`, `:1125`, and `R/app_assemble_userAsmb.R` at the four mirrors of those.

7.4. **Observers**, three per flavour:
- repopulate on set-switch: `R/app_assemble.R:1068` and `R/app_assemble_userAsmb.R:740` become `updateTextInput(inputId = "taxids", value = cur$taxids %||% "")`.
- enable/disable with Edit: `R/app_assemble.R:1084` and `R/app_assemble_userAsmb.R:756` become `shinyjs::toggleState("taxids", condition = input$edit_blast_opts)`.
- save: `R/app_assemble.R:1134-1145` and `R/app_assemble_userAsmb.R:804-815`.

7.5. **Save observer.** Validate numerically in-process (no network), and always reset `entrez_query` to the default so an Update is the escape hatch out of a blocked state. Note the selectize at `R/app_assemble_utils.R:475-483` has `create = TRUE`, so this upsert can create a new row; it must never write `entrez_query` as NULL.

```r
      tx <- trimws(input$taxids %||% "")
      tx <- paste(trimws(strsplit(tx, ",")[[1]]), collapse = ",")
      if (nzchar(tx) && !grepl("^[0-9]+(,[0-9]+)*$", tx)) {
        shinyWidgets::sendSweetAlert(
          title = "Invalid taxon restriction",
          text = paste0("Enter comma-separated numeric NCBI taxon IDs (e.g. 7711), ",
                        "or leave the field blank. Look IDs up at ",
                        "https://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi"),
          type = "error"
        )
        req(FALSE)
      }
      ...
            data.frame(
              blast_opts      = req(input$blast_opts),
              run_blast       = as.integer(isTRUE(input$run_blast)),
              # Never edited now, never NULL: kept populated with the historical
              # default so an older MitoPilot reading this row still restricts
              # its remote search.
              entrez_query    = "mitochondrion[Location]",
              taxids          = tx,
              max_target_seqs = as.integer(input$max_target_seqs %||% 5L),
              extra_opts      = input$extra_opts %||% ""
            ),
```

7.6. **Copy fixes.** `R/app_assemble_utils.R:501` and `:505-507`, plus the mirrors at `R/app_assemble_utils_userAsmb.R:221-227`: the checkbox reads "Run remote BLAST search using using assembly as query" (also a typo) and the help reads "BLAST each assembly against NCBI GenBank". Change to "Run BLAST reference search using assembly as query" and "BLAST each assembly against a local database of metazoan mitogenomes to find the closest reference (used for orientation and curation). Annotations for the winning reference are still fetched from NCBI." Drop the `href` to `blast.ncbi.nlm.nih.gov`.

7.7. **Stale API-key docs.** `R/init_project.R:37-40` and `R/init_project_userAsmb.R:41-44` both say the key raises limits for "the remote BLAST + GenBank fetch steps". Change to "the GenBank reference fetch step".

---

## 8. Driver changes in `inst/nextflow/modules/blast_genbank_workflow.nf`

8.1. **SELECT** (lines 57-60). Append `b.taxids` at the **end**; the `row[N]` indices are positional:

```groovy
params.sqlReadBlastOpts =
    'SELECT a.ID, b.run_blast, b.entrez_query, b.extra_opts, b.max_target_seqs, b.taxids ' +
    'FROM assemble a ' +
    'JOIN blast_opts b ON a.blast_opts = b.blast_opts'
```

8.2. **Options channel** (lines 79-82), arity 5:

```groovy
        channel.fromQuery(params.sqlReadBlastOpts, db: 'sqlite')
            .filter { row -> row[1] as Integer == 1 }
            .map { row -> tuple(row[0], row[2] ?: '', (row[5] ?: '').toString().trim(),
                                row[3] ?: '', (row[4] == null ? 5 : (row[4] as Integer))) }
            .set { blast_opts_ch }  // (id, entrez_query, taxids, extra_opts, max_target_seqs)
```

`mts_ch` at lines 85-88 is unchanged; it reads `row[4]`, which has not moved.

8.3. **Tuple arity** at lines 155-164. Add `taxids` to the `.map` and `.multiMap` closures, arity 7 to 8:

```groovy
            .combine(blast_opts_ch, by: 0)
            .map{ id, path_idx, asmb, opts_id, entrez_query, taxids, extra_opts, mts ->
                tuple(id, path_idx, asmb, opts_id, entrez_query, taxids, extra_opts, mts)
            }
            .multiMap { id, path_idx, asmb, opts_id, entrez_query, taxids, extra_opts, mts ->
                process: tuple(id, path_idx, asmb, opts_id, entrez_query, taxids, extra_opts, mts)
                ids:     tuple(id, true)
                pathkey: id
            }
```

8.4. **Named output** at line 187, to match Step 3.4:

```groovy
        blast_genbank(blast_in_split.process).hits
            .multiMap { id, path_idx, result_file ->
```

8.5. **Failure messages** (lines 27-28). Both name NCBI, and the no-output message must now also cover the legacy-Entrez block from Step 3.6:

```groovy
params.blastNoOutputMsg = "BLAST produced no output. Most often this is a legacy Entrez query that the local BLAST database cannot apply (open BLAST Options, check Edit, then click Update to clear it), an unknown taxon restriction, bad additional blastn options, or an unreadable local BLAST database. The blast_genbank task's error output is in its work directory (Work Directory browser). To retry, set this sample back to 'Ready to Assemble' (State button) and re-run the pipeline."
params.blastNoHitMsg = "No significant BLAST hits found in the local metazoan mitogenome database. The assembly may be non-target, too fragmented, or from a taxon with no mitogenome in the database."
```

8.6. **Do not touch** lines 201-245, 250-293, or 316-398. They are three independent parse blocks over an unchanged output format, and Step 11.3 depends on them being byte-identical.

---

## 9. Behaviours that must be preserved, and how each is verified

| # | Behaviour | Where | Verification |
|---|---|---|---|
| P1 | Output path `{id}/assemble/{opts_id}/blast_genbank_{path_idx}.txt`, published to `launchDir/out` | `blast_genbank.nf:31,44,45` | 11.3 file-listing diff |
| P2 | Output format: TSV, six columns `qseqid saccver stitle pident qcovs evalue`, best-first per query, one HSP per subject | `blast_genbank.nf` shell | 11.2 field-count and ordering assertions |
| P3 | `NO_SIGNIFICANT_HITS` sentinel: one writer, zero readers, survives via `parts.size() >= 6` at `blast_genbank_workflow.nf:207,257,338` | `blast_genbank.nf` shell | 11.5 forced-no-hit test |
| P4 | State machine: success -> `assemble_switch = 4`; no output -> 3 + `poor_blast_ref='failed'`; all paths NO HIT -> 3 + `'failed'` | `blast_genbank_workflow.nf:297-308, 405-411` | 11.3 DB diff |
| P5 | All nine SQL writes, their statements, their `WHERE assemble_switch = 4` guards, and the time-stamp-gated deletes | `blast_genbank_workflow.nf:32-55` | 11.3 DB diff |
| P6 | `blast_accession` and `blast_accession_auto` written from the same rank-1 value | `blast_genbank_workflow.nf:32, 498-506` | 11.3 |
| P7 | The remote GFF3/FASTA/taxonomy fetch, unchanged | `R/blast_ref_utils.R:152-206, 472-535` | zero diff in that file |
| P8 | `blast_ref_fetch`'s NCBI throttle: `maxForks 10`, 30 s backoff, `NCBI_API_KEY` export | `blast_ref_fetch.nf:12, 52-58` | only line 14 changes in that file |
| P9 | `run_blast = 0` samples never enter BLAST and terminate at state 2 | `blast_genbank_workflow.nf:80`, `assemble_workflow.nf:59, 188-190` | 11.4 |
| P10 | `cache 'lenient'` and its effect on `-resume` | `blast_genbank.nf:11` | 11.6 |

**What cannot be preserved, by construction.** Hit identity. The subject database is different, so `saccver`, the candidate list, and occasionally rank 1 will differ from a remote run. "Unchanged" applies to format, sentinel, state machine, and SQL writes, never to which accession wins. Any test written as "same accessions as before" is a wrong test. 11.2 compares against a local baseline; 11.3 isolates the plumbing by using a synthetic DB.

**Release-note item.** Existing projects do **not** all re-run BLAST on the first `-resume`. `assemble_workflow.nf:18` selects `WHERE a.assemble_switch IN (1, 4)`, so completed samples parked at state 2 never re-enter ASSEMBLE and so never reach `BLAST_GENBANK` (whose input is `ASSEMBLE.out.blast`, `main.nf:43`). The `shell:` hash change re-executes BLAST only for samples still at state 1 or 4. Those samples can get a different `blast_accession` than a remote run would have given. Explicit user overrides are unaffected: `resolve_unit_blast_ref()` (`R/blast_ref_utils.R:22-41`) checks `blast_ref_override` first, and WF1's candidate delete (`sqlDeleteCandidates`, `blast_genbank_workflow.nf:55`) never touches that table.

---

## 10. Documentation

10.1. `vignettes/MitoPilot.Rmd` (already modified on branch `docs-get-started`) is 25 lines and contains no mention of BLAST, Entrez, or NCBI. No vignette anywhere documents `blast_opts`. **Write** a BLAST-options section rather than update one: what the reference search is for, that it now runs against a bundled metazoan mitogenome database, how to enter numeric taxon IDs, and that the winning reference's annotations are still fetched from NCBI.

10.2. `vignettes/Custom-HPC.Rmd:131-135` lists per-process resource blocks. Add that `blast_gb` is now real local compute (`cpus`, `memory`, `db_dir`, `db_name`) and that the reference **fetch** is separate and still network-bound. Add `SINGULARITY_TMPDIR` and `NXF_SINGULARITY_CACHEDIR` guidance for the larger image (Step 2.4).

10.3. `vignettes/NOAA-SEDNA.Rmd`: uncomment and promote the `SINGULARITY_TMPDIR` block at `:139-142`. Note at `:66` that `singularity cache clean` now forces a full re-pull and re-conversion of a larger image.

10.4. State plainly in `Custom-HPC.Rmd` and `NOAA-SEDNA.Rmd` that WF1 still requires outbound NCBI access for the reference fetch regardless of this change. This is not an offline pipeline.

10.5. `NEWS.md` (create if absent): the local search, the `entrez_query` deprecation and its migration warning, the `taxids` column, the reference-flip caveat above, and the image-size change.

10.6. `tools/local_blast_db_design.md`: record Finding A (Step 0.3), the `taxonomy4blast.sqlite3` result (Step 0.1), and resolve the open questions at `:328-332` with the decisions in Section 13.

---

## 11. Test plan

All tests use a **copy** of the populated fish project at `/home/dmacguig/Documents/MitoPilot_fish_test`. Do not mutate the original.

### 11.1 Preflight (run before merging, no code required)

```bash
docker run --rm -v <db>:/db:ro macguigand/mitopilot:1.5.1 bash -lc '
  export BLASTDB=/db
  blastdbcmd -db /db/mito_metazoa -info
  blastn -db /db/mito_metazoa -query <one mitogenome> -taxids 6656 -outfmt 6 | head'
```
Pass: restriction applied, clean stderr. Regression: any hit outside the clade, or exit 0 with `requires additional data files` on stderr.

Accession coverage against the remote baseline (currently 30/30 before dedup):
```bash
cut -f2 <copy>/out/*/assemble/*/blast_genbank_*.txt | sort -u | while read a; do
  blastdbcmd -db /db/mito_metazoa -entry "$a" >/dev/null 2>&1 || echo "MISSING $a"; done
```
Pass: the only misses are accessions listed in `dropped_duplicates.tsv`. Regression: any other miss, which means the build filters removed something the remote search considered a good reference.

### 11.2 Golden comparison, search only

Re-run the new command over the six staged target FASTAs and compare rank 1 (`awk -F'\t' '!seen[$1]++'`) against `out/<id>/assemble/default/blast_genbank_<path>.txt`.

Pass criteria, stated as properties rather than exact-accession equality (sequence-hash dedup only collapses byte-identical records; a RefSeq copy differing by one base or rotated relative to its GenBank source survives and can still take rank 1):

- Every line has exactly 6 tab-separated fields; `qseqid` matches `^[^.]+\.[0-9]+\.[0-9]+$`.
- Rank 1's `stitle` genus matches the sample's `Taxon` column in `inst/test_data/mapping_test.csv`.
- Each query's candidate list holds `max_target_seqs` distinct **species**, not just distinct accessions. Pre-dedup measurement: three of seven queries hold only 3 species in 5 slots.
- Record any surviving RefSeq/GenBank rank-1 flip as accepted, with the pair named. Do not gate the merge on exact equality with the remote baseline.

Run once at `-num_threads 1` and once at `-num_threads 4` and diff. Byte-identical output was observed for one query; BLAST+ does not formally guarantee tie ordering across thread counts, and rank 1 among equal-scoring hits determines the fetched reference.

### 11.3 Plumbing-equality test

Build a throwaway DB from the **remote baseline's own FASTA**, fetched from NCBI by accession, not extracted from the deduplicated local DB (dedup deliberately deletes some of those accessions, so extracting would make the test unpassable):

```bash
efetch -db nuccore -id "$(paste -sd, accs.txt)" -format fasta > golden.fa
makeblastdb -in golden.fa -dbtype nucl -parse_seqids -blastdb_version 5 -out golden
```

Point `blast_gb.db_dir`/`db_name` at it and run WF1 on a fresh copy of the project. Because the subject set is exactly the remote result set, the search should reproduce the remote hits, and then:

- `diff` of the `assemble` / `assemblies` / `assembly_blast` / `blast_ref_candidates` dumps, old versus new, must show no differences other than `time_stamp`.
- The six result files must be identical up to hit ordering within a `pident` tie.

Regression: any difference in `assemble_switch`, `poor_blast_ref`, `blast_accession_auto`, `blast_ref_candidates` row count, or the set of published `blast_ref_*/` directories. This is the test that proves Sections 3 and 8 did not disturb the parse blocks, the SQL writes, or the state machine.

Note this DB will have no `taxonomy4blast.sqlite3`, so copy the three taxonomy files in beside it or the Step 3.5 guard will (correctly) refuse to run.

### 11.4 Full WF1 on the real project

`new_test_project()` into a clean dir, run WF1 end to end with the real local DB, then WF2 to completion.

Pass:
- All 14 samples reach `assemble_switch = 2` or a documented terminal state.
- `SRR21844202` produces 2 paths and both get BLAST rows.
- `MULTISCAFF` produces 2 scaffolds with **different** accessions (remote baseline: `NC_083028.1` and `NC_083079.1`, different genera). This exercises `scaffold_hits_disagree()` at `R/scaffold_join.R:100-106`. Regression: both scaffolds resolving to the same accession, which would let an automatic join proceed unchecked.
- No `[blast]` segment in any `assemble_notes`.
- The `blast_genbank` process wall clock in `.logs/nextflow.log` drops from minutes to seconds.
- `out/<id>/assemble/default/blast_db_VERSION.txt` exists and reports the deduplicated counts.
- WF2 curation, ORF, and `blast_ref_align` all succeed on the local-derived references.

Repeat 11.4 against the Scyphozoa project at `/home/dmacguig/Documents/MitoPilot_Scyphozoa_test`. The fish result is a chordate result; Cnidaria is exactly the clade the build's `min_cds >= 1` and `drop_unverified` filters could hurt.

### 11.5 Negative and edge tests

| Test | Setup | Expected |
|---|---|---|
| Genuine no hit | random 15 kb non-biological target FASTA | exit 0, `NO_SIGNIFICANT_HITS`, sample at `assemble_switch = 3` + `poor_blast_ref = 'failed'` with `blastNoHitMsg` |
| **Missing `taxonomy4blast.sqlite3`** | `BLASTDB` dir with `mito_metazoa.*` + `taxdb.btd` + `taxdb.bti` but no sqlite3; `taxids = '7711'` | task exits 1 with the explicit refusal. **Never** exit 0 with unrestricted hits. Highest-value negative test in the suite |
| `-negative_taxids` with no taxonomy files | `taxids = ''`, `extra_opts = '-negative_taxids 7711'` | task exits 1 via the stderr-notice check, not a silent unrestricted result |
| Only `taxonomy4blast.sqlite3` present | that file, no `taxdb.*`; `taxids = '6656'` | restriction applied correctly, exit 0 (this is why the guard is on the sqlite3 file) |
| Unknown taxid | `taxids = '999999999'` | blastn exit 2, no retry, task ignored, run fails via `failOnIgnore` |
| Non-numeric taxid written directly into `.sqlite` | `taxids = 'Chordata'` | blastn exit 1, task fails loudly |
| Non-numeric taxid via the app | type `Chordata` in the field, click Update | rejected at save with the alert from Step 7.5; nothing written |
| Valid restriction | `taxids = '7711'` on the fish project | identical to unrestricted (all targets are chordates) |
| Wrong-clade restriction | `taxids = '6656'` on the fish project | only arthropod hits, or none; nothing chordate |
| Stale `extra_opts` | `extra_opts = '-remote'` | blastn exit 255, task fails, not a silent wrong result |
| Blocked legacy Entrez | `entrez_query = 'Chordata[Organism] AND mitochondrion[Location]'`, `taxids = ''` | task exits 1; sample ends at `assemble_switch = 3` with `poor_blast_ref = 'failed'` and a `[blast]` note. **Assert on the DB row, not just on "BLAST never ran"** |
| Escape hatch | for that sample, open BLAST Options, check Edit, click Update | `entrez_query` becomes `mitochondrion[Location]`, sample re-runs clean |
| Missing config key | `.config` with a `blast_gb` block containing no `db_dir` | search still runs against `/ref_dbs/mito_metazoa` |

### 11.6 `-resume` and cache

Run WF1, then `-resume` with no changes. Expected: `blast_genbank` tasks cached. Note the correct premise: only samples at `assemble_switch` 1 or 4 ever re-enter this stage at all.

### 11.7 R-side tests

Add to `tests/testthat/test-backwards-compatibility.R`, alongside the existing assertions at `:428`, `:448`, `:486`, `:500`:

- After `backwards_compatibility()` on a fixture lacking the column, `"taxids" %in% DBI::dbListFields(con, "blast_opts")` and the default row has `taxids == ""`.
- **Guard test:** run twice, second call prints `nothing to update` (the file already tests this pattern at `:296`, `:394-397`, `:566-569`). Then drop `taxids` from a fixture and re-run; it must **not** print `nothing to update`. This is the test that catches a forgotten clause in 6.5.
- `schema_gaps()` on a fixture lacking `taxids` returns a non-empty vector naming it.
- Migration classification: `entrez_query` of `NULL`, `''`, `mitochondrion[Location]`, and `MITOCHONDRION[Filter]` all normalize to `mitochondrion[Location]` with no warning; `Chordata[Organism] OR Arthropoda[Organism]` is left untouched and produces a warning naming the parameter set.
- Add a first-ever assertion that `entrez_query`, `extra_opts`, and `max_target_seqs` values survive migration unchanged. None currently exists.
- The taxids validation regex accepts `""`, `"7711"`, `"7711,6656"`, `" 7711 , 6656 "` (normalizing to `7711,6656`) and rejects `"Chordata"`, `"7711;6656"`, `"-7711"`.

### 11.8 Cross-executor smoke test

Run WF1 on Hydra with the new Singularity image. Confirm `blastdbcmd -info` succeeds inside the container, the new `blast_gb` clusterOptions from Step 4.4 schedules without himem, and the ref-fetch step still starts promptly with `cpus 1`.

---

## 12. Rollback

12.1. Do all of this on a branch off `main`. Ask which base before branching.

12.2. **Runtime rollback, no code change.** Pin the project's `.config` `container` back to `macguigand/mitopilot:1.5.1` and check out the pre-change tag. The old image and a migrated `.config`/`.sqlite` work together: the old `SELECT` at `blast_genbank_workflow.nf:57-60` does not name `taxids`, SQLite ignores the extra column, and `entrez_query` is never NULL or empty in a migrated row (Steps 6.3 and 7.5), so the old remote search still carries its mitochondrion restriction.

12.3. **Schema rollback.** The migration is additive (`ALTER TABLE ... ADD COLUMN`) and never removes `entrez_query`. No down-migration exists and none is needed.

12.4. **Project-level rollback.** `backwards_compatibility()` copies `.sqlite` to `.old_sqlite_dbs/.sqlite.<n>` (`R/backwards_compatibility.R:240-257`) and `migrate_config()` copies `.config` to `.config.bak.<ts>` (`R/generate_config.R:199-202`) before writing. Restoring both returns a project to its pre-upgrade state.

12.5. **Bad reference for one sample.** No code rollback needed: the annotate-details picker writes a `blast_ref_override` row (`R/app_annotate_details.R:3109-3176`), which takes precedence over `assemblies.blast_accession` at `R/blast_ref_utils.R:22-41`.

12.6. **Image rollback.** Tags are immutable; `1.5.1` stays pullable. Because the DB is baked in, code and database roll back together, which makes results reproducible per image tag.

---

## 13. Decisions for the maintainer

### D1. RefSeq/GenBank duplicate handling
**Recommendation: dedup by sequence hash, keeping the `NC_` record, rebuilt locally from `blastdbcmd` output (Step 1).** Measured cost: minutes, no NCBI traffic, no record drift from the DB already verified. Measured benefit: rank 1 stops flipping to a redundant accession in 3 of 7 test queries, candidate lists stop losing 2 of 5 slots to duplicates in 3 of 7, the DB shrinks about 9%, and the reference whose GFF3 gets fetched is the curated RefSeq annotation.
The alternative (ship as-is) is not wrong, just worse: the annotate-details picker shows the same genome twice under two accessions, which users will report as a bug.
A `blastdb_aliastool -seqidlist` alias was built and verified working (`-taxids` honoured through it) but is rejected for shipping: its `.nal` hardcodes an absolute `DBLIST` path and references an external `SEQIDLIST` file, adding two runtime failure modes for no benefit over a local rebuild.

### D2. Delivery mechanism
**Recommendation: bake into the image (Step 2). Drop the `prepare_ref_db` staging option entirely.**
For: no per-run download; no head-node work (`prepare_ref_db.nf:20` forces `executor 'local'`); no Singularity bind-mount question (`vignettes/Custom-HPC.Rmd:45-53` notes only `$HOME` is mounted by default); `BLASTDB` is a fixed literal; DB and code version together, which makes rollback atomic.
Against: about +336 MB compressed on pull; a DB refresh needs an image release; the Hydra `pullTimeout` needs re-timing.
The staging option is not left in the plan as a fallback because `prepare_ref_db.nf:9-13` documents that its cross-run cache was tried and abandoned, meaning it pays 336 MB of download plus a 989 MB extraction on the driver on every run and every `-resume`. A specified-but-unverified alternative gets reached for the first time during an incident.

### D3. Remote fallback when the local search finds nothing
**Recommendation: do not implement it.**
A remote fallback cannot honour `-taxids` (incompatible with `-remote`), so a user who restricted to Arthropoda would silently get a chordate reference written into `assemble.blast_accession`, with nothing anywhere in the project database recording that the filter was ignored. It also reintroduces the latency, rate limiting, and irreproducibility this change removes, on exactly the samples the user is least likely to be watching. A genuine no-hit already produces a clear terminal state (`assemble_switch = 3`, `poor_blast_ref = 'failed'`) and an actionable note. If you disagree and want it anyway, it must hard-code `-entrez_query "mitochondrion[Location]"` and must refuse to fire when `taxids` is non-empty.

### D4. `cpus` for the local search
**Recommendation: `cpus = 4` for `blast_gb`, and hard-code `cpus 1` in `blast_ref_fetch.nf` in this change, not a follow-up (Step 5).**
Measured: about 3.5 s cold at 4 threads versus 13 s at 1. The coupling is not optional to fix: `blast_ref_fetch.nf:14` reads `params.blast_gb.cpus` today, so raising it without Step 5 gives a pure-HTTP process 4 cores times `maxForks 10`, throttling the local executor and mis-sizing Hydra jobs. `maxForks` stays shared at 10, which is correct for both.

### D5. Memory
**Recommendation: `memory = 4` (down from 8), and drop `himem` from Hydra's `blast_gb`.**
Measured peak RSS is 672 MiB, dominated by the mmapped 706 MB `.nsq`; the query succeeds under a 1 GB container cap. 8 GB times `maxForks 10` reserves 80 GB for work that peaks under 1 GB, and Hydra currently asks for 32 GB on the scarce himem queue. Do not go to 2 GB; the measured peak leaves too little headroom.

### D6. How much `entrez_query` to translate
**Recommendation: translate nothing. Treat a fixed set of known no-ops as no-ops, block everything else, and let the user retype a taxid.**
No-op set: empty, `mitochondrion[Location]`, `mitochondrion[Filter]`, `biomol_genomic[PROP]`, case- and whitespace-insensitive. Everything else stops the sample.
A partial boolean parser risks producing a *wrong but plausible* restriction, which is the exact silent-taxon-corruption class this whole change exists to eliminate, and nothing in the project database would record it afterwards. A user with `(Chordata[Organism] OR Arthropoda[Organism])` retypes `7711,6656`. That cost is one-time, visible, and correctable, and the migration warning names the affected parameter sets before any run.

### D7. Taxon name resolution in the UI
**Recommendation: numeric taxon IDs only. No name lookup, no network.**
Adding an ESearch call would put a network dependency in the modal's save path and in `backwards_compatibility()`, on a feature whose stated point is making the search work offline. `.blast_ref_efetch` (`R/blast_ref_utils.R:216-255`) does 5 attempts with `Sys.sleep(120L * attempt)`, up to about 20 minutes of blocking, which would freeze the single-threaded Shiny session with no feedback. The field links to the NCBI Taxonomy Browser instead. Revisit only if users actually ask.

### D8. Recording which database produced a `blast_accession`
**Recommendation: implement the one-line VERSION copy (Steps 3.4, 3.5, 8.4).**
The image tag does not record this implicitly: `migrate_config()` at `R/generate_config.R:169-173` deliberately preserves a custom container rather than bumping it, `vignettes/Custom-HPC.Rmd:42` still instructs pulling `1.4.7`, and the DB refresh cadence is independent of the package version. Copying `VERSION` into the published output directory costs one `cp` plus a named `emit:`, needs no schema change, and means a project that mixes `core_nt`-derived and `mito_metazoa`-derived accessions can be told apart. A `blast_db_version` DB column is not needed; defer it.

### D9. Refresh cadence
Not needed to ship. Tag the database asset independently (`blastdb-YYYY.MM`) so a refresh does not require a package release and a package release does not require re-uploading 336 MB.

---

## 14. Risks and unknowns

**Verified risks, mitigated in this plan.**

1. `-taxids` and `-negative_taxids` are silently discarded when `taxonomy4blast.sqlite3` is unreachable, with exit 0 and only a stderr notice. Verified reproducible, including with `taxdb.btd`/`taxdb.bti` present. Mitigated by a hard `exit 1` on the sqlite3 file plus an unconditional stderr grep (Step 3.5). Highest-severity item in the change.
2. RefSeq/GenBank duplication changes rank 1 and shrinks candidate diversity. Verified on all seven test queries (Step 0.3). Addressed in Step 1.
3. Stale `extra_opts` containing `-remote` collides with `-db` (verified exit 255). Fails loudly with no retry, which is correct; no migration message is planned for it.
4. Stale saved cluster profiles and `update_config = FALSE` leave `.config` without the new keys. Mitigated by defaulting in the module (Step 3.8).

**Not verified from the code, and honestly flagged.**

- **Whether `-num_threads > 1` can reorder hits within a score tie in general.** Byte-identical output was observed for one query at 1 versus 4 threads. BLAST+ does not formally guarantee it. Step 1's dedup removes the dominant source of exact ties; Step 11.2 runs the diff across all queries.
- **Singularity behaviour on Hydra and SEDNA.** Not testable from here. Baking into the image should be inert, but the SIF conversion path (`SINGULARITY_TMPDIR`, `/tmp` capacity on the login node) and the 60-minute `pullTimeout` at `inst/config.NMNH_Hydra:13` need a real pull before release.
- **Coverage for non-chordate taxa.** The 30/30 accession-coverage result is a fish result. Cnidaria is exactly the clade the build's `min_cds >= 1` and `drop_unverified` filters could hurt; run 11.4 against the Scyphozoa project before release.
- **Whether removing nuclear subjects weakens NUMT and contaminant detection.** `.score_concordance` / `.top_taxon` (`R/assembly_path_scoring.R:125, 221-253`) and `scaffold_hits_disagree()` (`R/scaffold_join.R:100-106`) all read BLAST output. A mito-only search space plausibly reduces this signal, but the test project has no known NUMT case, so it was not measured. This is a real, unquantified reduction in a safety signal. Worth a follow-up with a deliberately contaminated sample.
- **Whether `.score_blast()`'s 80-100 linear ramp (`R/assembly_path_scoring.R:210-216`) loses discriminating power** against a mito-only DB where most `pident` values sit near 100. Measured fish values were 100.000 / 96.258 / 88.057 / 84.132, still spread across the ramp, so the effect looks smaller than feared. One project is not evidence. Nothing here changes the ramp; watch it.
- **Whether `.best_blast_ref_sequence()` (`R/curate_mito_core.R:908`, all-zero-evalue branch at `:928-933`) changes which reference `ref_based_rc` orients against.** All measured e-values were `0.0`, so the existing branch should still fire. Not exhaustively tested.
- **How many of the 16,018 `NC_` records actually collapse under SHA-256.** Three pairs were confirmed byte-identical and three more inferred from tied hits. The real number is unknown until Step 1 runs; report it rather than assume 16,018.
- **`makeblastdb` volume threshold.** The current DB is single-volume. A future refresh could cross it and produce `.00.nsq` plus a `.nal`. Step 3.5 uses `blastdbcmd -info` rather than a `.nsq` stat specifically so that does not break every task, but the tarball layout under multi-volume was not tested.