# Local metazoan mitogenome BLAST database

Design doc. Status: **built and verified**. Date: 2026-08-13.

## Summary

Replace the remote `blastn -remote -db core_nt` reference search with a local BLAST
database containing only metazoan mitochondrial genomes.

The database described here has been built. Actuals against the estimates that
justified it:

| Property | Estimated | **Actual** |
| --- | --- | --- |
| Records | ~167,600 | **134,560** |
| Total bases | ~2.75 Gbp | **2,201,289,510** |
| BLAST DB v5 on disk | ~736 MB | **557 MiB** |
| `taxdb` (required, see below) | ~350 MB | **278 MiB** |
| Total on disk | - | **835 MiB** |
| Shipped tarball | ~230 MB | **289 MB** |
| Build wall clock | - | **~35 min + 2 min dedup** |

The record count came in below the estimate because the raw build was
deduplicated afterwards, which removed 37,355 byte-identical sequences (see
"Redundancy" below). The pre-dedup build was 171,915 records and 2.81 Gbp, so
the bytes-per-base projection that produced the ~736 MB estimate was accurate to
within 1%.

Sizes above are `du` values (MiB). The bytes-per-base estimate that produced the
~736 MB projection was accurate to within 1%; the tarball estimate was not, since
its compression ratio came from an 862-sequence sample and the shipped archive
also carries `taxdb`.

That is roughly +7% on the current 13.5 GB MitoPilot container image.

Measured search performance: a full mitogenome query against all 171,915
sequences returns in **2.9 seconds** on 4 threads, versus minutes for the remote
search plus queue wait.

## Motivation

`inst/nextflow/modules/blast_genbank.nf` currently runs a remote megablast against
`core_nt`. This is the slowest and least reliable step in WF1:

- NCBI queue latency dominates runtime, and the process already carries a
  60-second-per-attempt backoff plus special handling for silent server errors
  (`blastn` exiting 0 with an empty result and an error on stderr).
- Throughput is rate limited per API key, so large projects serialize on NCBI.
- Results are not reproducible over time, since `core_nt` changes underneath.
- `core_nt` is overwhelmingly irrelevant. We only ever want a mitogenome back.

A local database fixes all four. It also makes the reference search work offline
and on air-gapped HPC systems.

## Inclusion criteria

Base query against `nuccore`:

```
Metazoa[Organism] AND mitochondrion[filter]
```

then four filters. Each is parameterized, with the recommended default in bold.

### 1. Length window

| Parameter | Default | Meaning |
| --- | --- | --- |
| `min_len_any` | **12000** | floor for any record |
| `min_len_complete` | **8000** | floor for records whose title asserts a complete genome |
| `max_len` | **100000** | ceiling |

Evidence for the ceiling. Length distribution of the 202,274 metazoan
mitochondrial records at or above 12 kb:

| Range | Records | Share |
| --- | --- | --- |
| 12-20 kb | 200,335 | 99.0% |
| 20-30 kb | 1,672 | 0.83% |
| 30-40 kb | 183 | 0.09% |
| 40-60 kb | 75 | 0.04% |
| 60-100 kb | 9 | 0.004% |
| over 100 kb | 2 | 0.001% |

The long tail is mostly genuine: `Polypodium hydriforme` at 93,065 bp is the
largest known animal mitogenome, and the 40-60 kb bin is real repeat-expanded
bivalve mitogenomes (`Anadara`, `Arca`) plus ichneumonid wasps. Only two records
exceed 100 kb, both auto-deposited organelle contigs (`OZ178930` Pinctada radiata
at 156,931 bp, `OZ178950` Arca noae at 111,486 bp) that are almost certainly
uncollapsed circular repeats. A 100 kb ceiling excludes exactly those two and
keeps everything defensible. Tail size impact is negligible either way: 269
records sit above 30 kb.

Evidence for the split floor. A flat 12 kb floor silently drops entire phyla with
genuinely reduced mitogenomes. Reading all 99 short records whose title claims a
complete genome:

- **Real, keep.** Ctenophores bottom out the animal kingdom at 9.9-11.0 kb
  (`Mnemiopsis leidyi` 10,326; `Vallicula multiformis` 9,959; `Benthoplana
  meteoris` 9,974; `Beroe`, `Pleurobrachia`, `Lyrocteis`, `Tjalfiella`).
  Chaetognaths run 11.1-11.9 kb (`Spadella`, `Zonosagitta`, `Paraspadella`,
  `Decipisagitta`, `Aidanosagitta`). Also the chewing louse `Anaticola
  crassicornis` at 8,118 and the mite `Pyemotes zhonghuajia` at 10,555.
- **Real but multipartite, exclude.** `Alatina alata` is deposited as 8 separate
  "chromosomes" of 2,578-4,366 bp; `Haematopinus apri` as minicircles of
  2,780-4,212 bp. Each record is one piece of a genome, not a usable reference.
- **Mislabeled, exclude.** About 25 BOLD COI barcodes of 657-658 bp titled
  "mitochondrion, complete genome" (`Agrotis`, `Suinzona`, `Hagnagora`), plus
  short partials such as `Carduelis carduelis` at 7,718 bp and `Pagurus` at
  7,589 bp whose real genomes are ~16 kb.

8,000 sits in a genuine empty gap in the data. The nearest records below it are
7,797 and 7,718, both mislabeled; the nearest above is 8,118, which is real.

Cost of the split floor: 48 additional records, under 1 MB of database. It buys
reference coverage for Ctenophora and Chaetognatha, which otherwise have none.

Accepted downside: roughly 6 of those 48 are over-optimistic partials from
well-sampled groups (`Gadus chalcogrammus` 11,407, catfish `Atopodontus` 11,133,
two termites, a spider). Each competes against hundreds of better references in
its own clade and will not win a megablast.

Title match for the lower floor should be a "complete genome" match that excludes
`nearly complete`, since deposits like `AB080275` (`Varanus komodoensis
mitochondrial DNA, nearly complete genome, region 1/2`) would otherwise pass.

### 2. Drop UNVERIFIED

| Parameter | Default |
| --- | --- |
| `drop_unverified` | **TRUE** |

20,976 records (10.4%) of the 12 kb-plus set carry the `UNVERIFIED:` title prefix,
which is NCBI stating that the submitter's annotation failed validation. This
matters more than it looks: `R/blast_ref_utils.R:156` fetches GFF3 for whichever
accession wins the BLAST, so an unverified reference propagates bad gene
coordinates into curation, not merely a mediocre sequence. Dropping them also
saves ~90 MB.

### 3. Drop unannotated records

| Parameter | Default |
| --- | --- |
| `min_cds` | **1** |

Same reasoning: a reference with no CDS features yields an empty GFF3 and is
useless to MitoPilot regardless of how good its sequence is.

Measured CDS-count distribution over a 3,500-record sample of the verified
12 kb-plus set:

| CDS count | Share |
| --- | --- |
| 0 | 7.6% |
| 1-11 | 0.2% |
| 12 | 1.6% |
| 13 | 89.2% |
| 14 or more | 1.4% |

The distribution is sharply bimodal, so the threshold choice is easy: a record
either has a full protein-coding complement or has nothing at all.

99% of the unannotated records are auto-deposited assemblies titled
`... genome assembly, organelle: mitochondrion`, i.e. Darwin Tree of Life style
organelle contigs submitted without annotation. They are 6.1% of the set.

`min_cds` must stay at 1, not 13. Requiring 13 would drop every flatworm
(`Raillietina`, `Hymenolepis` and relatives genuinely lack atp8, so 12 PCGs) and
would also threaten the reduced ctenophore and chaetognath genomes the split
floor was added to include. The measured gap between 0 and 12 means a threshold
of 1 and a threshold of 10 keep the same records to within 0.1%, so 1 is the
safe choice.

Accepted downside: dropping unannotated contigs loses some taxa that are only
represented by a DToL assembly. Annotating them ourselves is out of scope for
this change. If coverage gaps show up in practice, that is the follow-up.

### 4. Resulting size

Applied cumulatively, projected from the measured sample:

| Filter stage | Records | Gbp | DB size |
| --- | --- | --- | --- |
| 12 kb-100 kb | 202,274 | 3.34 | ~894 MB |
| minus UNVERIFIED | 181,298 | 2.99 | ~800 MB |
| minus unannotated (`min_cds` 1) | 167,571 | 2.75 | ~736 MB |
| plus complete-titled 8-12 kb | ~167,619 | 2.75 | ~736 MB |

### 5. Build results

Implemented in `tools/build_local_blast_db.py`. Actual filter tallies from the
2026-08-13 build:

| Outcome | Records |
| --- | --- |
| Candidates from the bounded query | 204,307 |
| Dropped: UNVERIFIED | 21,784 |
| Dropped: below the applicable floor | 1,190 |
| Dropped: no CDS annotation | 9,418 |
| Dropped: above the ceiling | 0 |
| Dropped: duplicate accession | 0 |
| **Kept** | **171,915** |

The ceiling drops nothing because it is enforced in the Entrez query itself. The
longest sequence in the finished database is 93,065 bp, i.e. `Polypodium
hydriforme`, exactly as the length analysis predicted.

Unannotated records came in at 5.2% of post-UNVERIFIED candidates versus the 7.6%
the sample projected. Everything else landed within a few percent.

Verified after the build:

- `Mnemiopsis leidyi` (10,326 bp), `Beroe cucumis` (10,487 bp), and `Spadella
  cephaloptera` (11,905 bp) are all present, so the split floor did its job and
  Ctenophora and Chaetognatha have references.
- `MN366013.1`, an `UNVERIFIED:` record, is absent.
- A full mitogenome query with the exact flags from `blast_genbank.nf`
  (`-outfmt "6 qseqid saccver stitle pident qcovs evalue" -max_target_seqs 5
  -max_hsps 1 -task megablast`) returns correct top hits in 2.7 s on 4 threads.
- With `BLASTDB` set, `-taxids 6656` on a pangolin query returns only arthropod
  hits and `-taxids 7711` returns only chordates.

### 6. Redundancy: fixed by deduplication

Found by comparing the local database against the remote baselines in the
existing fish test project, using the real staged query FASTAs. Fixed by
`tools/dedup_local_blast_db.py`, results at the end of this section.

The database holds 16,018 `NC_` (RefSeq) records out of 171,915, and for many of
them the byte-identical GenBank source record is also present. Remote `core_nt`
returned only the RefSeq copy. Locally, both compete, and the GenBank duplicate
often wins rank 1:

| Query | Remote rank 1 | Local rank 1 | Distinct genomes in the 5 candidate slots |
| --- | --- | --- | --- |
| MULTISCAFF.1.1 | `NC_083028.1` | `OR546180.1` (identical dup) | 4 |
| MULTISCAFF.1.2 | `NC_083079.1` | `OR546244.1` (identical dup) | 3 |
| SRR21844202.1.1 | `OR582709.1` | `OR582709.1` | 5 |
| SRR21844202.2.1 | `OR582709.1` | `OR582709.1` | 5 |
| SRR22396740.1.1 | `NC_082563.1` | `OR482471.1` (identical dup) | 3 |
| SRR22396794.1.1 | `OR499733.1` | `OR499733.1` | 5 |
| SRR22396940.1.1 | `OR482444.1` | `OR482444.1` | 3 |

Three of seven queries flip rank 1 to a redundant accession, and three of seven
lose two of five candidate slots to duplicates. That changes
`assemble.blast_accession`, which GFF3 gets fetched (curated RefSeq annotation
versus raw submitter annotation), the published `blast_ref_<accession>/`
directory name, and the diversity of the reference picker.

Fix is a local dedupe: `blastdbcmd` dumps accessions and sequences from the
existing database, identical sequences are collapsed by SHA-256 (preferring the
`NC_` accession), and the database is rebuilt without refetching anything from
NCBI. This is exact-duplicate collapsing, a different and much safer operation
than the species-level deduplication considered and deferred below. Every drop
is recorded in `dropped_duplicates.tsv` so the decision is auditable.

**Result.** 37,355 records collapsed, more than twice the 16,018 that the RefSeq
overlap alone predicted:

| Reason a record was dropped | Count |
| --- | --- |
| A RefSeq (`NC_`) copy of the identical sequence was kept | 16,444 |
| An identical GenBank record was kept instead | 20,911 |

The second row was the surprise. Population studies deposit large numbers of
byte-identical mitogenomes: one accession absorbed 228 identical records, and
24,749 kept accessions absorbed at least one duplicate. These were consuming
candidate slots with the same genome under different accessions.

Rank 1 after dedup matches the remote baseline on **7 of 7** test queries,
including the three that previously flipped. Candidate diversity went from
4/5, 3/5, and 3/5 on the three worst queries to 5/5, 4/5, and 5/5.

The one remaining repeat is correct behaviour, not a miss: `MULTISCAFF.1.2`
returns both `NC_002761.2` and `MW788427.1` for *Conger myriaster*, at 90.421%
and 90.414% identity. Those are two different individuals, so they are real
biological variation and both belong in the database.

SHA-256 only catches byte-identical pairs. A RefSeq copy differing by one base,
or rotated relative to its GenBank source, survives on purpose. Merging those
would require alignment, and a wrong guess would silently delete a real
reference.

## Build pipeline

A standalone script, run by maintainers rather than by users:

0. **The length window must be part of the Entrez query, not a post-filter.**
   `Metazoa[Organism] AND mitochondrion[filter]` alone matches 8,027,329 records,
   since every COI barcode in GenBank carries that filter. Adding
   `8000:100000[SLEN]` cuts it to 204,307 candidates and turns a ~16,000-request
   summary stage into ~409. The script hard-fails if the candidate count exceeds
   one million, so this cannot silently regress.
1. `esearch` the bounded query, then page `esummary` in batches of
   500 to collect accession, length, title, and CDS count per record.
2. Apply the four filters above locally. Write the surviving accession list and a
   `taxid_map` file (accession to taxid, straight from the same esummary pass).
   **Deduplicate by accession here.** One pass during development produced 307
   duplicate rows out of 204,307 while a later clean pass over the identical UID
   list produced none, so the duplication is transient rather than a stable
   property of the query. Its exact origin is unconfirmed: most likely an
   esummary response replayed after a connection hiccup. The consequence is
   severe either way. A repeated accession makes `makeblastdb` abort the entire
   build with `Duplicate seq_ids are found`, and it deletes the previous
   database before discovering the problem. Deduplicating UIDs is not sufficient,
   since the collision is at the accession level.
3. `efetch` FASTA by accession in batches, with an NCBI API key.
4. `makeblastdb -dbtype nucl -parse_seqids -blastdb_version 5 -taxid_map ...`
5. Write a `VERSION` file recording build date, record count, base count, and the
   exact filter parameters used.
6. Tar and compress for distribution.

Roughly 340 esummary requests plus 340 efetch requests. With an API key this is
minutes of wall clock in theory and an hour or two in practice given NCBI
throttling. It is a maintainer-side batch job, so that is fine.

FASTA deflines from `efetch` already look like:

```
>OR256923.1 Manis javanica isolate HKU36_3 mitochondrion, complete genome
```

which is the same `stitle` shape the remote search returns, so the downstream
`qseqid saccver stitle pident qcovs evalue` parsing in
`blast_genbank_workflow.nf:311` needs no changes.

## Delivery and refresh

The database ships **inside the container image**, and reaches users through
Docker Hub alongside everything else. There is no hosted copy and no download at
build time.

The tarball is staged into the build context as
`docker/mito_metazoa_blastdb.tar.gz`, exactly like the existing
`docker/MitoPilot_*.tar.gz` package artifact, and is gitignored for the same
reason: at 275 MiB it is over GitHub's 100 MB per-file push limit, and git
history is permanent, so every clone would carry a copy of every refresh
forever. The three `docker/deploy-*.sh` scripts check for it before doing any
work and fail with instructions if it is missing.

`ADD` is used rather than `COPY` plus a `RUN tar`, because `ADD` extracts a local
archive into a single layer and leaves no tarball behind; the two-step form would
keep the 275 MiB archive in its own layer permanently. `--chown=root:root` is
required, since `ADD` otherwise preserves the build machine's uid and gid from
the archive. Do not add a `chmod -R` in a later `RUN`: touching every file's
metadata copies the whole 835 MiB layer again. The archive is already 0644/0755,
so it is readable by the invoking user under Singularity. Verified in a real
build: the layer adds 900 MB uncompressed (13.5 GB image to 14.4 GB), roughly
275 MiB compressed on the wire, and a search run as uid 65534 on a read-only
mount returns the expected top hit with taxon filtering honoured.

A hosted copy was considered and rejected for now. Image builds are manual
(`docker/deploy-*.sh`; the only GitHub workflows are `pkgdown` and
`R-CMD-check`), so a URL buys nothing while adding a build-time network
dependency and a 404 failure mode. Revisit if image builds ever move to CI,
where the build machine would not have the artifact. A separate data-only image
consumed via `COPY --from` is the natural next step if database refreshes start
outpacing package releases.

Refresh quarterly. Record the database `VERSION` string into the project database
at run time so a project can report which reference set produced its assignments.

## Integration with MitoPilot

Changes needed in `inst/nextflow/modules/blast_genbank.nf`:

1. Swap `-remote -db core_nt` for `-db <local path>` and drop the retry backoff,
   the empty-plus-stderr server-error handling, and the `NCBI_API_KEY` export.
   A local search either succeeds or genuinely finds nothing.
2. **`-entrez_query` has no local equivalent.** This is the one real API break.
   The project default is `mitochondrion[Location]`
   (`R/init_db.R:365`), which becomes a no-op since the whole database is
   mitochondrial. But users can set taxon restrictions such as
   `Chordata[Organism] AND mitochondrion[Location]`, and those must be translated
   to `-taxids` / `-negative_taxids`. Plan is to keep the stored option, translate
   the common organism-restriction form, and warn on anything not translatable.

   **`BLASTDB` must be set at run time, and this fails dangerously if it is
   not.** Taxonomic filtering needs three things: taxids inside the database
   (handled by `-taxid_map` at build), the taxonomy files (shipped in the
   tarball), and BLAST actually locating them. If it cannot, `blastn` does
   **not** error. It prints a notice to stderr, silently discards the
   restriction, exits 0, and returns hits from every taxon. Verified on the
   built database: a pangolin query run with `-taxids 6656` (Arthropoda)
   returned pangolin hits and exit code 0. With `BLASTDB` pointed at the
   database directory, the same query correctly returns only arthropod hits, and
   `-taxids 7711` returns only chordates. Exporting `BLASTDB` is required even
   when `-db` is given as an absolute path.

   **The load-bearing file is `taxonomy4blast.sqlite3`, not `taxdb.btd`.**
   Measured behaviour of a restricted search by which taxonomy files are
   reachable:

   | Files present | Exit | Restriction applied |
   | --- | --- | --- |
   | all three | 0 | yes |
   | none | 0 | **no, silently** |
   | `taxdb.btd` + `taxdb.bti` only | 0 | **no, silently** |
   | `taxonomy4blast.sqlite3` only | 0 | yes |

   Consequence for the Nextflow process: export `BLASTDB` alongside `-db`, and
   do **not** guard on `taxdb.btd`, which is present in the silently-unrestricted
   third row. The robust guard is to fail the task if `blastn` emits the
   "requires additional data files" notice on stderr, which also covers a
   `-negative_taxids` passed through `extra_opts`. Silently ignoring a user's
   taxon filter would corrupt reference selection in a way nothing downstream
   could detect.
3. Keep the GFF3 and FASTA fetch in `R/blast_ref_utils.R` remote. Only the search
   moves local. Annotation retrieval for the single winning accession is one
   small request and does not need to be mirrored.

Suggested safety valve: if the local search returns no significant hit, optionally
fall back to the existing remote path. Cheap to implement, and it covers the case
where a sample is from a lineage with no mitogenome in GenBank at all.

## Alternatives considered

**Use NCBI's prebuilt `mito` database.** NCBI publishes
`ftp.ncbi.nlm.nih.gov/blast/db/mito.tar.gz` (194 MB, weekly, 18,034 sequences,
594 Mbp). Zero build and zero maintenance. Rejected because it is RefSeq-only and
spans all kingdoms: only 15,808 records are metazoan complete mitogenomes, i.e.
about 10% of what GenBank actually holds. Coverage of recently sequenced
invertebrates would be poor.

**Include sub-12 kb fragments.** Adding everything from 1-12 kb would bring
741,536 records for +237 MB. Rejected: mean length in that band is 1,196 bp, so
the mass is COI and 16S barcodes, not partial genomes. It would grow the database
27% while making reference selection worse, since a query could best-hit a 650 bp
barcode carrying no mitogenome to use as a reference.

**Deduplicate to one genome per species.** Would cut size substantially, since
GenBank holds heavy per-species redundancy. Deferred rather than rejected: it
needs a defensible representative-picking rule (longest? RefSeq-preferred? most
complete annotation?) and 736 MB is already acceptable. Worth revisiting if the
database is ever shipped to bandwidth-constrained users.

## Open questions

1. Should the fallback to remote BLAST be default-on or opt-in?
2. How much of the `entrez_query` grammar do we translate, versus documenting
   that taxon restriction now takes taxids?
3. Refresh cadence and whether project databases should pin a database version
   for reproducibility rather than tracking the container image.

## Appendix: how the numbers were measured

- Record counts: `esearch` against `nuccore` with the stated terms, 2026-08-13.
- Lengths: `esummary` `Slen` over 3,000-record samples spread across each result
  set at six offsets.
- CDS counts: `esummary` `Statistics` `type="cdregion"` over a 3,500-record
  sample of the verified 12 kb-plus set at seven offsets.
- Bytes per base: 862 real mitogenomes (14,170,902 bp) downloaded from the actual
  result set and built with `makeblastdb 2.16.0+ -blastdb_version 5
  -parse_seqids` inside `mitopilot:1.5.1`. Result 3,790,324 bytes, i.e. **0.2675
  bytes per base**. Compression to tar.gz reached 31% of that.
- Cross-check: NCBI's own `mito` database metadata reports 153,848,740 bytes for
  594,169,998 letters, i.e. 0.259 bytes per base. Consistent.
