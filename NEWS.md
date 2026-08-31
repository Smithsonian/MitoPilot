# MitoPilot 1.5.4

Released 2026-08-31. Container: `macguigand/mitopilot:1.5.4`

## New Features

This release is about **user-supplied assemblies**. MitoPilot can now take a whole draft genome rather than a finished mitogenome, find the mitochondrial contig(s) in it, and circularize them if evidence is present.

### Finding the mitogenome in a whole assembly

- Point MitoPilot at a FASTA holding a whole assembly, thousands or millions of contigs, and it will **locate the mitochondrial contigs before anything else runs**. Off by default; enable with `new_project_userAsmb(find_mitogenome = TRUE)`. You must also supply a MitoFinder database for your clade.
- Contigs are screened against a bundled metazoan mitogenome database, the best candidates are confirmed by annotating them, and only confirmed contigs continue through the pipeline. A sample with no confirmed mitochondrial contig is flagged and stops there; the rest of the batch carries on.
- **Nuclear insertions (NUMTs) are filtered out** by requiring the match to cover a large fraction of the contig.
- A sample carrying **more than one species' mitogenome** can return each mitogenome it finds, so you can review them and decide which to drop.
- A new **Mito Search** column reports the outcome, and the evidence behind each contig it kept or dropped.
- The search is built for large inputs: a full genome assembly of 1.4 million contigs reduces to its single mitochondrial contig in under four minutes.

### Circularizing user-supplied assemblies

- Assemblers often report a circular mitogenome as a linear contig whose end repeats its start. MitoPilot can now **trim that redundant overlap and circularize the sequence** during the Assemble module. Off by default; enable with `new_project_userAsmb(attempt_circularization = TRUE)`.
- When raw reads are available, **reads must span the new junction** before the assembly is called circular, so a repeat is not mistaken for a real circle.
- Circularization is attempted for each contig when a sample has multiple mitochondrial contigs.
- A new **Circularization** column reports the outcome. Clicking it shows the overlap alignment alongside the read depth across the seam.

### Scaffold joining for user-supplied assemblies

- A fragmented single-path assembly can now be **ordered against its BLAST reference into one joined sequence**, the same step the regular pipeline already had. Off by default; enable with `new_project_userAsmb(join_scaffolds = TRUE)`.
- Samples whose contigs match **different** reference mitogenomes are left separate for review, so a contaminated sample is never spliced together.
- A new **Scaffold Join Notes** column reports what the join did, or why it was refused.
- **Redo join in pipeline**, a button in a sample's assembly details window, queues a fresh join for the next pipeline update. It becomes **Cancel queued pipeline join** while one is waiting, and says why if a sample cannot be queued.
- Both the column and the button appear in every project, not just user-assembly ones.

### Fragmented assemblies are annotated contig by contig

- A user assembly split across several contigs could not be locked for annotation until you ignored all but one contig, and only that contig was ever annotated. **Every contig is now its own annotation unit** and is annotated, curated, and exported like any other sequence.

### Ambiguous bases are counted and shown

- The Assemble table of a user-assembly project has a new **Ambig. Bases** column, right after the assembly length, counting the bases that are not A, C, G, or T. It is filled in for samples with a single active contig; a sample with several contigs shows the count per contig in the assembly details window instead.
- The annotation alignment viewer warns when the gene on screen contains ambiguous bases, for protein-coding genes and rRNAs alike. The count updates as you nudge the gene ends with the +/- buttons.

### Topology is now optional

- The `Topology` column in the mapping file is **optional**. Leave it out, or leave a cell blank, and a single-contig assembly is treated as linear. An earlier release refused to build the project without the column.
- It applies only to single-contig assemblies. An assembly holding more than one contig is recorded as **`multi`** and whatever you declared is ignored; declaring such a sample `circular` prints a warning. Each contig then carries its own topology, which stays `unknown` unless circularization is switched on, and an `unknown` contig is handled as linear.
- The same rules apply to samples you add to an existing project.

### Test project for user assemblies

- **`new_test_project_userAsmb()`** builds a ready-to-run project from nine user-supplied assemblies cut from real data, with examples of linear, circular, not-yet-circularized, and multi-contig assemblies.

### Default MitoFinder reference database

- New projects now assemble against a **ten-species fish mitogenome sampler** rather than a single zebrafish record. This applies to every new project, regular and user-assembly alike. Existing projects keep the database they were created with. Pass `mitofinder_db` to use a database for your own clade.

## Bug Fixes

- **A gene containing ambiguous bases no longer kills the sample or the app.** A protein-coding gene holding bases that are not A, C, G, or T was refused outright by translation, so curation died partway through with an unreadable error, and clicking the +/- START or STOP codon buttons in the annotation editor hung the app behind a spinner that never cleared. Ambiguous bases arrive both from the `N` spacers MitoPilot inserts when joining scaffolds and from consensus assemblies called against a reference, which carry IUPAC codes at uncertain sites. Those codons are now translated to an amino acid where only one is possible and to `X` otherwise, and the gene is flagged with an `ambiguous bases in CDS` warning for review.
- **An error while editing codons no longer freezes the annotation window.** Anything that went wrong while walking the START or STOP codon left the "Updating alignment, hold tight..." overlay on screen forever and no other sample would open. The overlay now clears and the problem is reported.
- **The Assemble table reports the lengths of the contigs that are actually active.** The length and scaffold counts were a snapshot taken before any join, so a joined sample kept describing the fragments it replaced, and ignoring or restoring a contig afterwards changed nothing. They are now refreshed whenever a join, an edit, or an ignore toggle changes what is active, and equal-length fragments are listed individually rather than collapsed into one value that looks like a total.
- **Gaps in a joined assembly are declared on export.** Every run of `N`s that MitoPilot inserted when joining fragments is written as a `gap` feature carrying its estimated length, however short the run. Runs of `N`s that arrived with your own sequence are left alone, since they may be ambiguous base calls rather than gaps. Any coding feature containing unknown bases carries a note saying how many.
- **A junction that cannot be sized is no longer padded.** MitoPilot used to insert a fixed 100 `N`s where the reference could not estimate a gap. NCBI expects the number of `N`s to be the estimated length, so such a sample is now left fragmented with a note; its contigs can be submitted as several sequences under one BioSample. This applies to regular projects too, so a sample that joined under an earlier release may now be left in pieces, with the reason given in the **Scaffold Join Notes** column. If you joined a fragmented assembly under an earlier release, redo the join before exporting: MitoPilot cannot recognize the old fixed padding and will not declare those gaps.
- **A contig with no read coverage no longer stops annotation.** Every base of an uncovered contig looked masked, so coverage trimming cut the contig down to 50 bp and annotation then failed with an unreadable error that ended the run. Such contigs are now kept whole and carry a `no read coverage: trim skipped` note.
- **A contig with no genes no longer stops the run.** MITOS2 writes an empty result file for a contig it finds nothing on, and reading that file failed. Now that every contig is annotated on its own, a contig with no genes is normal and simply returns no annotations.
- **The combined export files are written by R rather than shell commands.** `cat` and `cp` are not available everywhere and broke on paths containing a space, so a submission group's combined FASTA and feature table could go missing with no error.
- **The redo scaffold join button is no longer blocked on every sample.** The check for a recorded BLAST reference was looking the sample up by an ID it never read, so it found nothing and reported that no sample had a reference.
- **Export reads each record's own topology** rather than applying one value across a mixed unit.
- **The Annotate update dialog counts sequences, not samples**, which is what it has always queued.

## Documentation

- User-assembly material now has its **own article**, covering the mitogenome search, circularization, and scaffold joining, with the FAQ and reference index updated to match.

**Note**
Projects created with an earlier release should be updated with [`MitoPilot::backwards_compatibility()`](https://smithsonian.github.io/MitoPilot/reference/backwards_compatibility.html). Give it your executor, for example `backwards_compatibility(executor = "slurm")`. It adds the new database fields and rebuilds your project `.config` from that executor's template, so re-apply any hand edits afterwards. The rebuild matters on a cluster: the new mitogenome-search and circularization steps have their own memory and scheduler settings, and a `.config` written by an earlier release does not have them. A project pinned to a custom container image is left alone by the update; run `backwards_compatibility()` first, then edit the `container` line in the project `.config` by hand. The new Assemble steps will not run until you do.

**Full Changelog**: https://github.com/Smithsonian/MitoPilot/compare/1.5.3...1.5.4

# MitoPilot 1.5.3

Released 2026-08-21. Container: `macguigand/mitopilot:1.5.3`

## Bug Fixes

### Origin-spanning features on circular assemblies

A feature that runs across the origin of a circular assembly is stored with its start position after its end position. Nothing downstream expected that, so a single wrapping feature could stop a project outright.

- **Curation no longer crashes on a wrapping feature.** Every boundary adjustment handed those coordinates straight to a sequence-extraction call that refuses a start after an end, so the whole curation step exited with an error and the run stopped. Sequence extraction and length are now wrap-aware, and extension or trimming may cross the origin on a circular contig. Linear assemblies behave exactly as before.
- **Export writes a wrapping feature as two intervals** in the submission table. A single line spanning the origin does not mean "wraps" to table2asn, it means "minus strand", so the feature was being submitted backwards. Also fixed in export: the 3' end of a `transl_except`, minus-strand exon coordinates in the GFF, and a crash in the PCG outlier review when an exon wrapped.
- **Validation checks are circular-aware.** Overlap, containment, and length tests measured a wrapping feature as if it ran the long way around the genome, so genuine overlaps went unreported and non-overlapping features were flagged.
- **The control region keeps its extension when a neighbour wraps.** An origin-adjacent OH call is now bounded by the wrapping feature rather than by the contig edge. Three test samples were finishing with a stunted control region and a spurious "below minimum length" warning.
- **The annotation editor handles wrapping throughout**: nudging boundaries, copy FASTA, rRNA alignment, the boundary viewer, the zoom track, the linearize guard, and merge / un-merge.
- **Rotating an assembly now warns** when the requested start gene is not present, and when the new origin would cut through a feature.
- **MITOS2 annotations use the right contig length.** On a multi-contig unit the first contig's length was applied to every contig, which mis-placed wrap calculations on all the others.

### Database writes

- **Scaffold mappings could be wiped.** The delete and insert halves of a scaffold-mapping update were sent as two independent database operators with no guaranteed ordering, so the delete could land last and leave the table empty. Both now run inside a single transaction from the pipeline driver.

### HPC

- **NOAA SEDNA memory now scales with the retry attempt**, matching every other cluster profile. A retry was re-requesting the same memory that had just failed, so a step that ran out of memory would fail again identically.

### Container builds

- **A stale package archive can no longer be shipped in the image.** The Dockerfile picks up the package archive by name order, so a leftover build of an older version won out over the new one and the container quietly shipped the wrong MitoPilot. All three deploy scripts now clear old archives first.

## Documentation

- The website has been rewritten and reorganized: **Get Started is now a full tutorial** built around the bundled test project, the README is a landing page with installation moved to its own article, and the curation documentation is clade-neutral with the ruleset chosen in the app rather than hand-written.
- The article on handling difficult assemblies replaces the old multiple-assemblies page, the reference index is grouped by topic, screenshots are regenerated for the current app, and a citation file is included with the package.
- A maintainer guide for building the container was added, and the legacy GenBank download script was retired.

## Internal

- `run_app()` is no longer exported. `MitoPilot()` is the documented way to start the app and passes everything through, so nothing changes for users.
- The documentation build is capped at 20 minutes so a stalled runner fails fast instead of hanging for over an hour.

**Note**
To update older MitoPilot projects, please run [`MitoPilot::backwards_compatibility()`](https://smithsonian.github.io/MitoPilot/reference/backwards_compatibility.html). This will add any missing fields to the SQL database and attempt to update the Docker/Singularity container version in your project `.config` file.

**Full Changelog**: https://github.com/Smithsonian/MitoPilot/compare/1.5.2...1.5.3

# MitoPilot 1.5.2

Released 2026-08-14. Container: `macguigand/mitopilot:1.5.2`

## New Features

### Local BLAST database

- MitoPilot now finds each sample's closest reference mitogenome by searching a **local BLAST database packaged in the container**, rather than querying NCBI over the network. The database holds **all annotated metazoan mitogenomes in GenBank** (134,560 sequences).
- The search takes **1-2 seconds per sample** instead of minutes, needs no internet connection and no NCBI API key, and is **reproducible**: a remote search returns whatever GenBank held that day, so the same project could pick a different reference on a re-run.
- Verified against saved remote results: accession, percent identity, and query coverage are **identical** for every sample in the test project.
- Annotations for the winning reference are still fetched from NCBI, so an internet connection is still required for that step.

### New BLAST options

- **Remote BLAST** searches NCBI over the network instead of the bundled database, for reaching sequences the local database does not contain.
- **Fall back to remote BLAST when no local hit** (on by default) retries a sample against NCBI when the local search finds nothing, which covers lineages with no mitogenome in the database. Turn it off for strictly offline, reproducible runs.
- **Restrict search to taxon IDs** limits the search to one or more numeric NCBI taxon IDs (for example `7711` for Chordata). The same restriction is applied if the remote fallback runs.
- The **Entrez query** field now applies only to remote searches and is shown only when Remote BLAST is ticked. Existing projects that set a custom Entrez query are switched to Remote BLAST when the project is upgraded, so they keep behaving as before.

### Version and database provenance

- The BLAST options window shows **when the bundled database was built** and how many sequences it holds, so it is clear which reference set produced a project's assignments. The build date is also recorded alongside each sample's results.
- Opening a project whose container version does not match the installed MitoPilot now shows a **warning** naming both versions. It does not block the app: the project still works, it simply runs the pipeline code baked into the older image.

### Scaffold-join editor

- The base-pair alignment zoom now opens windows up to **1000 bp**, up from the previous cap. This is the widest window that renders on every display.
- **Row labels stay in a fixed panel** in the base-pair zoom, so it remains clear which scaffold each row belongs to while the alignment scrolls sideways.
- **Build joined assembly** now shows a progress spinner while Path 0 is being built, instead of appearing to hang.

## Bug Fixes

### BLAST results

- **BLAST hits could be silently lost.** The assembly record and the BLAST result were written to the project database by two independent processes with no guaranteed ordering, so a hit could be overwritten moments after it was stored. The sample then showed a reference in the Assemble panel but a blank one in Annotate, with nothing reporting a problem. Observed on one sample of fourteen in a single run.
- **A path with no BLAST hit is now scored as one.** A missing hit was treated as missing information and simply left out of an assembly path's score, so a path that matched nothing was never penalized and could outrank a path with a genuine, if distant, match. For the same reason the "possible NUMT/contaminant" flag never fired for those paths, which is exactly where it is most useful.
- **Automatic scaffold joining is no longer cancelled by a no-hit scaffold.** A scaffold that matched nothing was counted as a second, disagreeing reference, so MitoPilot held back the join and left the sample fragmented without explanation.

### Annotation editing

- **The synteny plot no longer mis-registers after editing an assembly.** Linearize left the cached reference alignment untouched, so the plot was silently shifted by the rotation offset, and Trim deleted the alignment with nothing rebuilding it. Both operations now repair the cached alignment exactly, and an alignment that cannot be repaired is recomputed on demand when the window opens.
- **The Trim control re-measures after annotation edits.** Deleting, restoring, merging, or adjusting the boundaries of an annotation now updates the unannotated flanks shown in the Trim control and its confirmation dialog, which previously kept reporting the pre-edit measurement.
- **The PCG outlier review no longer shows the pre-edit alignment** after returning through Back to Review. The review now decides for itself what changed by comparing against the project database, and recomputes only the genes that actually moved.
- Mouse-wheel scrolling is no longer translated into horizontal scrolling on the coverage map and synteny views.

### App

- **Fixed a crash when opening annotation options.** Ignoring every scaffold of a sample and then un-ignoring one left that sample with no parameter sets assigned, showing "undefined" for the annotate, curate, and ORF sets; clicking one closed the app. Affected samples are repaired automatically.

### HPC

- **Fixed the reference fetch step being killed by scheduler memory limits.** On nodes with many cores, R reserved several GB of address space before doing any work, which schedulers that cap address space (such as SGE on Hydra) treated as an overrun, killing the task before it made a single request. Raising the memory request did not help, because the reservation grows with the node's core count. All R pipeline steps are now capped to a single compute thread.
- The NMNH Hydra profile now uses a fixed memory reservation for its annotation steps rather than one that scaled with the retry attempt.

**Note**
To update older MitoPilot projects, please run [`MitoPilot::backwards_compatibility()`](https://smithsonian.github.io/MitoPilot/reference/backwards_compatibility.html). This will add any missing fields to the SQL database (including the new `assembly_backup` table) and attempt to update the Docker/Singularity container version in your project `.config` file.

**Full Changelog**: https://github.com/Smithsonian/MitoPilot/compare/1.5.1...1.5.2

# MitoPilot 1.5.1

Released 2026-07-29. Container: `macguigand/mitopilot:1.5.1`

## New Features

### Trim unannotated assembly ends

- New **Trim unannotated ends** control in the annotate-details window cuts a linear assembly back to its outermost annotated feature. Export already performed this cut in memory for linear units; doing it in the app makes it **visible, undoable, and applied to everything the app shows** rather than only to what is submitted. Because the boundary is a feature boundary, no feature is ever dropped or truncated, so feature counts, spans, and gene order are unchanged.
- New **Restore assembly** control undoes every in-app assembly edit for a unit, returning the sequence, feature model, coverage track, and annotate-stage files to the pipeline's own output. **Linearize** is now covered by the same undo.
- Snapshots are stored in a new **`assembly_backup`** table; a row exists only while a unit carries an un-restored edit.
- Guarded throughout: circular assemblies must be linearized first, the coverage track must line up with the assembly or the trim is refused rather than desynchronizing them, user-supplied multi-contig units are refused outright, and soft-deleted annotations stay tombstoned at position zero. Any edit drops the unit's cached `blast_ref_alignment`, which WF1 regenerates on the next run, so the synteny view can never silently mis-register against an edited sequence.


### Stale assembly-output detection

- ASSEMBLE publishes to `out/<ID>/assemble/<assemble_opts>/`, so the option-set name doubles as a directory name and every downstream stage rebuilds that path. Reassigning a sample to a different parameter set **after** it has assembled left the published output under the old name, and nothing noticed: WF2 annotate / curate / ORF, the coverage viewer, and the output-folder button all pointed somewhere that was never created, so runs either failed obscurely or completed with **no reference hits at all**.
- MitoPilot now detects this and reports it at three points: **on app startup** (a warning, keeping the app open, since the fix requires the app), **at lock time** (the affected samples are held back, the rest are locked, and both remedies are spelled out), and **when the parameter set is reassigned**. The check mirrors the WF2 gate exactly and tests for the per-path assembly FASTA, not merely the directory, so an empty publish directory does not silence it.
- `ANNOTATE` additionally logs a warning naming the parameter-set directories that *do* exist for the sample. The unit is still emitted either way, so `annotate()` fails as loudly as it always has.
- Coverage-details reads and writes are checked **before** anything is written, so a missing output can no longer leave a half-written Path 0 behind. The check reports but never creates: a directory holding only the Path 0 files would look complete while the raw path assemblies stayed orphaned.

### Scaffold-join editor: live verification views

- The mapping plot and base-pair zoom now read a **live layout** that overlays your current reverse-complement and include choices, so manual edits are reflected immediately without re-running the reference mapping.
- **Exclusion reasons are surfaced** ("low reference coverage", "unmapped", "subsumed by scaffold N") in the editor, the mapping plot, and the join note, so it is clear why a scaffold was left out of Path 0.
- **Sparse placements are hatched** in the mapping plot. A bar spans the union extent of a scaffold's alignment blocks, so a sparse placement was previously drawn solid over reference it does not actually cover, which is what made the base-pair zoom look like it disagreed with the overview.
- Scaffolds that are included but have no reference placement are now called out ("appended with N gap") instead of being absent from the plot entirely, and empty zoom rows say *why* they are empty rather than reading as missing data.
- Switching the reference recomputes the layout and snaps back to the last good accession if the recompute fails, so the dropdown can never disagree with the layout the build step actually uses.

## Bug Fixes

### Scaffold placement and joining

- **Origin-wrap and repeat blocks no longer balloon a scaffold's placement.** A scaffold's alignment blocks are reduced to a single colinear chain before the reference extent is taken, so a block on the far side of the origin can no longer stretch the extent across the whole reference and manufacture a genome-scale negative gap at the next junction. Query coverage is still scored over all blocks, so a genuinely rearranged scaffold is not dropped.
- **The sparseness test is now relative to the sample's own scaffolds** rather than an absolute match-density floor. Match density falls with reference divergence, so the old fixed threshold was cleared by everything against a conspecific reference and by nothing against an ~83%-identity one, switching the guard off exactly where fragmented assemblies need it most.
- **Contained scaffolds are excluded from Path 0** instead of forcing a large negative gap that trimmed real bases out of the container. Guarded so a scaffold still carrying substantial unaligned sequence is never discarded, and a scaffold that is itself excluded cannot be cited as another's container.
- **Junction overlaps must now be genuine.** A confirmed overlap has to reach the 3' terminus of the preceding scaffold, have ungapped and near-identical terminal columns, and not be low-complexity. On real data a dragged alignment scored 5/10 at the terminus with 42 indel bases where a true junction scored 100%, so short AT-rich matches were previously confirming junctions that were not there and trimming real bases.
- **Overlap consensus no longer manufactures ambiguity codes.** The coverage-majority consensus pairs the two scaffolds by index, so it now runs only when the overlap is base-alignable (no indels, and the aligned length matches the trim taken). Otherwise the preceding scaffold's bases are kept and the junction note says so, instead of quoting an identity that does not describe what was written.
- **A scaffold fully consumed by overlap trimming is dropped** rather than appended as an empty piece, which previously desynchronized the source-position map. The following junction is re-measured against the scaffold actually present.
- **Origin rotation only happens when a block actually reaches reference position 0.** Extrapolating to the origin from a distant block is wrong across N gaps and non-colinear scaffolds, where the colinear offset is not constant.
- **An unticked Circular box no longer silently emits the origin region twice.** Redundant-end detection always runs; when trimming is not authorized, the join note warns that the duplication is present.
- **Coverage tracks stay in register.** Per-position strings now preserve their position count (empty cells become NA, outlier-mask prefixes are stripped) and are padded or truncated to the sequence length. Numeric mapping columns are also coerced back after SQLite returns them as text, which it does whenever unmapped scaffolds store empty strings.

### Base-pair zoom rendering blank

- The zoom's scaffold sub-region is estimated from a single anchor block, so its error grows with distance from that anchor and with every indel in between; scaffolds that showed a bar in the overview could render blank. The estimate now widens in proportion to the drift, candidate alignments are scored against the reference window and rejected below an identity floor, and a whole-scaffold realignment is tried when the estimate covers less than half the window. A drifted estimate now yields an honest blank instead of confidently wrong bases.

### BLAST reference synteny orientation

- The whole-genome aligner aligns on whichever strand scores higher, so a **sample stored on the opposite strand from its reference came back reverse-complemented**, putting the synteny view in a different frame from the coverage map and the sequence editor: the same sample, with gene order and orientation mirrored between two plots sitting side by side. Alignments are now normalized on load so the **sample is always drawn in its stored orientation** and the **reference** carries the flip, labelled *"shown reverse-complemented"*. Independent of the `ref_based_rc` curation option, which already flips the assembly before alignment.
- **Origin-wrapping features keep their gene label on both arcs.** Labelling only the longer arc left an unidentified arrow at the opposite edge of the linearized layout.

### Path 0 attribution in the join editor

- The joined assembly now inherits its BLAST hit from **the accession the layout was actually built from**, not the raw dropdown value, so a reference whose recompute failed can no longer mislabel Path 0.
- Manual layouts carry their reference coordinates and **recompute junction gaps** rather than blanking them. An all-empty gap vector switched off the overlap probe at *every* junction, turning each one into a blind 100-N spacer with the shared bases duplicated, including junctions the user never touched.
- Per-sample join state is cleared when the window opens, so a stale accession or orientation from a previously viewed sample cannot be stamped onto another sample's Path 0.

### Join-group ids in the annotation editor

- The next join-group id was derived from an unanchored `group=` search over the notes field, and applied a non-vectorized operator to a whole column. A free-text note mentioning `group=` anywhere could steal or inflate the id, and a notes column holding only NULLs raised an error instead of returning the first id. The id is now derived from the anchored `JOIN:` marker, matching the readers used by export and the un-join action.

### Cluster configuration

- **`memory` in `.config` is a plain number of gigabytes, but a bare number is interpreted as *bytes* by Nextflow.** `scaffold_join`, `coverage`, and `coverage_userAsmb` now convert it explicitly and scale the request with the retry attempt, so a scheduler memory kill self-heals on retry instead of failing identically.
- `scaffold_join.clusterOptions` accepts a closure, letting a site configuration grow its own reservation per attempt (schedulers such as SGE take memory from `clusterOptions` rather than the `memory` directive). Plain strings continue to work unchanged.
- Default `scaffold_join` memory raised from 4 GB to 8 GB, and the bundled **NMNH Hydra** configuration from 16 GB to 32 GB with per-attempt scaling.

**Note**
To update older MitoPilot projects, please run [`MitoPilot::backwards_compatibility()`](https://smithsonian.github.io/MitoPilot/reference/backwards_compatibility.html). This will add any missing fields to the SQL database (including the new `assembly_backup` table) and attempt to update the Docker/Singularity container version in your project `.config` file.

**Full Changelog**: https://github.com/Smithsonian/MitoPilot/compare/1.5.0...1.5.1

# MitoPilot 1.5.0

Released 2026-07-16. Container: `macguigand/mitopilot:1.5.0`

## New Features

### Annotate multiple paths/scaffolds per sample

- **A single sample can now carry more than one assembly into annotation.** The annotation unit is promoted from `(ID)` to `(ID, path, scaffold)`: a sample may have several **paths** (alternative graph resolutions) and/or several
**scaffolds** (fragments, separate molecules, or a multipartite mitogenome), and each unit is annotated and validated independently.
- **Annotate table shows one row per unit.** New **Path** and **Scaffold** columns auto-hide when every unit is `(1, 1)`, so single-assembly projects look unchanged. All row actions and the per-unit editor are keyed on the full `(ID, path, scaffold)`
unit.
- **Resolving competing paths**: either ignore the extra paths, or build a consensus **Path 0** (`(ID, 0, 0)`) that merges them. **Fragmented scaffolds**: join them into Path 0, or keep genuinely separate molecules apart.
- WF2 now runs per unit. `userAsmb` projects stay single-sequence and validate all contigs of a path together.
- Per-unit export: `{seqid}` is enforced for multi-unit FASTA headers so deflines match the `.tbl`; multi-path samples are blocked from export (competing resolutions of one genome would submit duplicate records).
- New documentation articles: **Multiple Paths and Scaffolds** (how to resolve, join, and export units) and **Troubleshooting** (error messages moved out of the FAQ).

### Performance

- Reference databases (MITOS + curation) are now extracted **once per run** and symlinked into each task instead of copied and re-extracted (~241 MB/task, now a symlink). Safe because the extracted DB is read-only.

## Bug Fixes

- **Fresh-project seeding**: WF1 seeded per-unit `annotate` rows from a `fromQuery` on the just-written `assemblies` table, but `fromQuery` snapshots the DB at session start, so on a fresh project every unit but `(1, 1)` was silently dropped by WF2's
inner join. Now seeded from the in-run channel.
- **Annotation persistence**: `persist_annotations()` re-inserted keyed on `ID` alone, so a save on a multi-unit sample deleted the unit's annotations and never restored them. Now keyed on the full primary key.
- **Migration fan-out**: migrated multi-scaffold projects only received one unit; now every non-ignored scaffold is seeded.
- **userAsmb multi-contig**: ANNOTATE / CURATE / ORF dropped all contigs but the first; now routed per the `params.userAsmb` contract.
- **Export robustness**: export errors now show an alert instead of crashing the session.
- **Curation remote hits**: split into a task-private DB searched alongside the read-only base (all nine `get_top_hits` sites), so the base database is no longer mutated per task.
- **Synteny on consensus**: `compute_blast_ref_alignment` used `baseOnly=TRUE`, which fails on the `N` gap-spacers in Path 0 assemblies; now `baseOnly=FALSE`.

**Warning**
**1.5.0 is a major database rework, not a routine update.** [`MitoPilot::backwards_compatibility()`](https://smithsonian.github.io/MitoPilot/reference/backwards_compatibility.html) migrates existing projects **in place and irreversibly**: it re-keys the `annotate` table to `(ID, path, scaffold)`, rebuilds `blast_ref_alignment` (dropping legacy per-path alignment rows, which are recomputed on the next WF2 run), drops `samples.export_group`, and adds `export` and `blast_ref_override` tables. A migrated `.sqlite` **cannot be reopened by an older version of MitoPilot** (downgrade is not supported). The migration backs up your database to `.old_sqlite_dbs/` first, and the app now refuses to open an un-migrated project with an actionable message instead of a raw error. **Back up your projects before upgrading.**

**Note**
To update older MitoPilot projects, please run [`MitoPilot::backwards_compatibility()`](https://smithsonian.github.io/MitoPilot/reference/backwards_compatibility.html). This will migrate the SQL database to the new multi-unit schema and attempt to  update the Docker/Singularity container version in your project `.config` file.

**Full Changelog**: https://github.com/Smithsonian/MitoPilot/compare/1.4.12...1.5.0


# MitoPilot 1.4.12

Released 2026-07-14. Container: `macguigand/mitopilot:1.4.12`

## New Features

### Run the user-assembly pipeline without raw reads

- **`new_project_userAsmb()` gains a `no_raw_data` mode** so user-supplied assemblies can be annotated with **no sequencing reads at all**. Read mapping and coverage calculation are skipped, and `RAW_DIR="NA"` is pinned in the project `.config`.
- `coverage()` gains a no-reads branch that **synthesizes a schema-identical `coverageStats.csv` straight from the assembly** (GC computed from the sequence; depth / error left empty), so all downstream WF2 machinery runs unchanged. WF1 branches on `params.noRawData` (derived from `rawDir == 'NA'`) to skip `PREPROCESS` via a new `coverage_userAsmb_noReads` process / workflow, and the assemblies / assemble DB writes are factored into a shared writer.
- In this mode, annotate **coverage trimming is disabled** (`coverage_trim = 0`) and the `R1` / `R2` mapping columns are optional (carried as `NA`).
- The **Assemble table hides the read-derived columns** (**Preprocess Opts.**, **Reads**, **Read Length**) when a project is opened in `no_raw_data` mode, since they carry no data without raw reads.

## Bug Fixes

### scaffold_join workflow stall

- `scaffold_join` now **retries a failed cluster submission before ignoring it**. An ignored SGE / qsub submission failure left the mandatory `mappings` output channel unclosed, hanging all downstream operators and stalling the head process after every task had finished; retrying first prevents transient submission rejections from triggering the stall.

### blast_genbank remote NCBI server errors

- `blastn -remote` can print a server-side error (e.g. an NCBI queue database failure) to stderr while still **exiting `0` with empty output**, which was misclassified as a genuine no-hit. The step now captures stderr and exits `1` to trigger a retry when the output is empty but stderr shows an error signature.

### userAsmb samples table R1/R2

- The `no_raw_data` R1/R2 backfill ran after the samples table was built, so samples lacked `R1` / `R2` and the app's `fetch_export_data()` crashed on open. The backfill now runs **before** the samples / preprocess tables are created, so both carry the columns as `NA`, matching the read-based schema.

### Cluster configuration

- Added a `himem` flag to `scaffold_join` in the bundled **NMNH Hydra** config, whose 16 GB/CPU request exceeds the 8 GB standard-queue threshold.

**Note**
To update older MitoPilot projects, please run [`MitoPilot::backwards_compatibility()`](https://smithsonian.github.io/MitoPilot/reference/backwards_compatibility.html). This will add any missing fields to the SQL database and attempt to update the Docker/Singularity container version in your project `.config` file.

**Full Changelog**: https://github.com/Smithsonian/MitoPilot/compare/1.4.11...1.4.12

# MitoPilot 1.4.11

Released 2026-07-08. Container: `macguigand/mitopilot:1.4.11`

## New Features

### Scyphozoa curation ruleset

- New **`scyphozoa_mito`** ruleset for true jellyfish, drafted from a survey of 39 complete mitogenomes: translation table 4, the 13 standard PCGs, `rrnL`/`rrnS`, only `trnM` + `trnW`, and optional `dpo` / `orf314` terminal ORFs. Registered in the ruleset map and the curation ruleset browser.

### orf314 curation reference & dpo consolidation

- New **`orf314`** protein BLAST reference database built from cnidarian (Medusozoa) GenBank mitogenomes (RefSeq not required) and added to the bundled `Metazoa_RefSeq235` curation database, with provenance recorded in a tarball `MANIFEST.txt`.
- **`dpo` / `polB` / `dnaB` are treated as a single gene** (the medusozoan terminal DNA-polymerase-B ORF; the "replication helicase" label is a misannotation): `polB` and `dnaB` are normalized to `dpo` across annotation, curation, and the reference-DB build, and their reference sequences are merged into `dpo`.

### Reference-based reverse-complement (curation)

- New optional curation step that **reverse-complements a contig to match the orientation of its top BLAST reference**, aligning against the top hit with an ambiguity-aware substitution matrix and propagating the flip to the sequence, annotations, and coverage. Intended for taxa the rRNA / start-gene heuristics cannot orient (e.g. jellyfish with rRNAs on opposite strands). Off by default; toggled in the curation options.

### MITOS2 rRNA / PCG rescue

- New annotation option (**on by default**) that runs MITOS2 a **second pass with tRNA prediction disabled** and merges in any rRNAs / PCGs it uniquely recovers. MITOS2 discards rRNAs whose locus overlaps a predicted tRNA (e.g. a scyphozoan `rrnS` wedged against `nad5`); the tRNA-free pass recovers them (tRNAs still come from the full pass and tRNAscan-SE). The merge is additive, and a checkbox disables it to save runtime.

### Export tracking in the Annotate & Export tabs

- Samples now carry a persistent **`export_time_stamp`**, so you can see at a glance which samples have already been exported. The Annotate and Export tables show an **Exported** date column alongside the sample's **Export Group**, and the Export table **auto-refreshes when an export finishes** so the timestamp populates immediately.
- Both tabs gain an **Exported (yes / no) filter**; on the Export tab, filtered-out rows are pruned from the selection so bulk export only touches visible samples.

### Table filtering & readability

- New **"Updated between" date-range filter** on the Annotate and Assemble tabs, to narrow the table to samples touched in a given window.
- **5'/3' partial badges** now show in the annotation-details feature table **without entering alignment-edit mode**, and refresh live when partiality is toggled with the edit tools.
- The Export table was cleaned up: raw internal columns are hidden and every table header is normalized to readable Title Case (e.g. **Genetic Code**, **BLAST Hit**, **Export Group**, **# PCGs** / **# tRNAs** / **# rRNAs**).

## Bug Fixes

### BLAST reference synteny orientation

- Reference genomes are now **rotated to the start gene only when they are truly circular**. Topology is read from the reference record (`Is_circular`) and stored per reference, so **linear references (e.g. jellyfish mitogenomes) keep their native coordinates** in the whole-genome synteny alignment instead of being spuriously rotated.

### Genetic code

- Opening a project whose `samples.genetic_code` is empty or malformed no longer crashes the app; the value is coerced to a single valid translation-table id (falling back to `2`).

### add_samples

- Newly added samples now inherit **`blast_opts`** and the annotate status flags (`reviewed` / `problematic` / `partial` / `ID_verified`) like `init_db`, so opening the BLAST or annotation options windows for them no longer errors.

### backwards_compatibility

- Migrations add the new `blast_ref_sequences.topology`, `annotate_opts.ref_based_rc` / `rescue_no_trna`, and `samples.export_time_stamp` columns (and their entries in the "nothing to update" guard), so existing projects self-migrate on open.

**Note**
To update older MitoPilot projects, please run [`MitoPilot::backwards_compatibility()`](https://smithsonian.github.io/MitoPilot/reference/backwards_compatibility.html). This will add any missing fields to the SQL database and attempt to update the Docker/Singularity container version in your project `.config` file.

**Full Changelog**: https://github.com/Smithsonian/MitoPilot/compare/1.4.10...1.4.11


# MitoPilot 1.4.10

Released 2026-07-06. Container: `macguigand/mitopilot:1.4.10`

## New Features

### Gene merging & spliced-gene editing

- Reworked gene merging with distinct **span** vs **joined-feature** modes, and join-aware multi-segment editing in the annotate-details window.
- Refined the **spliced-gene editor**: alignment view, junction handling, overlap and nucleotide controls, and a nucleotide junction viewer available for all PCGs.

### Multi-candidate BLAST references

- New schema and workflow for **multiple candidate reference genomes per sample**: the top-N BLAST candidate hits from GenBank are retained, each candidate reference (plus lineage) is fetched, and all are whole-genome-aligned to the sample.
- Candidates are kept **separate per scaffold/path** (never merged across divergent scaffolds), stored in accession-keyed reference tables, with per-sample streaming ref-fetch via a batched fetcher.
- New **"All BLAST Hits"** window shows one ranked candidate list per path/scaffold, and all candidate references are injected into the curation BLAST database to improve auto-curation.
- `max_target_seqs` is now an editable `blast_opts` value in the database.

### Reference genome picker & "Set as reference genome"

- New **synteny reference picker** in the annotate-details window (view-only), plus an explicit **"Set as reference genome"** button that overwrites `assemble.blast_accession` (and metadata) from the chosen candidate.

### Per-ruleset genetic code

- Genetic code now **auto-selects per sample from its curation ruleset** (`curate_opts.target`) instead of one project-wide value, so samples on different rulesets can carry different codes.
- Optional per-ruleset **override** in the curate-options window ("Auto (from ruleset)" or an explicit NCBI translation table); the resolved code flows into the annotate / curate / ORF steps and the annotation-details editor.
- The project-level `genetic_code` was removed from `.config`

### Curation database default

- **`Metazoa_RefSeq235`** is now the default curation reference database (the only bundled DB with rRNA BLAST references), decoupled from the default MITOS2 annotation database, which stays `Chordata`.

## Bug Fixes

### Nextflow progress panel

- The live progress board is now **reconstructed frame-by-frame**: Nextflow reprints the whole board on each redraw and truncates process names unpredictably when task tags are long, which previously piled up stale/garbled rows (and an empty key could clobber a real process). The panel now renders exactly Nextflow's latest board.

### Curation & assembly options UI

- Ruleset **"Target"** dropdown is ordered alphabetically by its displayed label.
- **Free-typing is disabled** on fixed-vocabulary pickers (`target`, `assembler`, `start_gene`) so invalid values cannot be entered or dispatched; a typed `target` previously crashed via `params_<typed>`.
- Fixed a synteny-section crash (NULL passed to `%|NA|%`) that left the BLAST Reference Synteny panel blank, and kept the assembler tool help lines in sync with the assembler selection.

### backwards_compatibility

- New signature `backwards_compatibility(path, executor = NULL, update_config = TRUE)`; the `.config` is regenerated wholesale from the executor template (porting project values and backing up the old config) instead of being patched line by line.
- `genetic_code` is stored as INTEGER (with a legacy TEXT rebuild) so `assemble.nf`'s `intValue()` works; migrations add the new per-scaffold candidate, `blast_accession_auto`, and `curate_opts.genetic_code` columns, guarding the scaffold-candidate seed so migrating an old project without an `assemblies` table no longer crashes.
- The `curate_opts.genetic_code` override is seeded from the project's current code, so migrated projects behave identically (no re-translation on re-run).

**Note**
To update older MitoPilot projects, please run [`MitoPilot::backwards_compatibility()`](https://smithsonian.github.io/MitoPilot/reference/backwards_compatibility.html). This will add any missing fields to the SQL database and attempt to update the Docker/Singularity container version in your project `.config` file.

**Full Changelog**: https://github.com/Smithsonian/MitoPilot/compare/1.4.9...1.4.10

# MitoPilot 1.4.9

Released 2026-06-29. Container: `macguigand/mitopilot:1.4.9`

## New Features

### MitoFinder annotation

- New optional **MitoFinder** annotation tool in WF2, alongside MITOS2, with a new `annotate_mitofinder.R` and supporting Nextflow modules.
- **MITOS2 and tRNAscan-SE are now optional** (both on by default). The MITOS2 checkbox label describes the `--best` flag.
- MitoFinder gene naming normalized: the GFF gene comes from the reference qualifier, with descriptors carried in `Name`.
- Duplicate gene calls sharing the same start are collapsed, and origin-spanning intervals are handled in the overlap helpers.
- Non-standard MitoFinder genes are kept as non-standard PCGs (sanitized fallback name) rather than dropped, inherit the default PCG ruleset via `augment_rules_for_unknown_genes`, are renameable like ORFs in the window, and BLAST against the combined all-gene DB with gene-labeled hits and a non-standard badge.

### rRNA curation editor

- New **rRNA editor** in the annotate-details window, reusing the edit/save framework: a nucleotide-boundary editor with 5'/3' nudge buttons (no codon search). Editing auto-flags the rRNA as partial, partial flag is toggleable per end.
- **Nucleotide alignment view** for the focal rRNA. Alignment against new rRNA database (only available in the Metazoan RefSeq 235 curation database) plus the rRNAs from the best remote BLAST GenBank hit.
- rRNA features in the `.tbl` (group and per-gene) carry the 5'/3' partial markers set in the editor

### General RefSeq curation databases

- New general RefSeq curation-DB builder (`data-raw/build_curation_db.R`) covering PCGs and rRNAs, with non-standard mito gene capture and robust ORIGIN parsing.
- New **`Metazoa_RefSeq235`** curation DB offered in the UI (shipped under `ref_dbs/Mitos2/`), with rRNA availability noted.
- Combined ORF BLAST now works with gene-less curation DBs; the curation-DB header format matches the distributed databases.

### Work Dirs browser

- New **Work Dirs** button in the top control bar opens a popup listing every Nextflow task work directory for a chosen sample, so you can jump straight to a step's scratch dir to troubleshoot a failed process or inspect intermediate files.
- Popup has a searchable sample picker and a table of work dirs: Process | Status | Param set | Modified | Work directory | Copy / Open.
- Source is `.logs/nextflow.log*`, parsed for completed-task records (success **and** failure), so failed processes (never published to the output tree) are captured with **zero pipeline changes** on existing projects.
- Status icons come from the task exit code: green check (success) / red triangle (failed), so a failed attempt and its successful retry both show, distinctly.
- Rerun handling: dedup by path, filter to dirs still on disk, add a Modified timestamp, sort newest-first.
- Pre-selects the sample currently selected in the active table. Environment-aware Open: local OS file browser, RStudio Server Files pane, headless warning + path to copy, missing-dir warning.

### Sample table quality-of-life

- The Assemble, Annotate, and Export sample tables now auto-fit the window height and can be manually resized via a drag handle, replacing the fixed `height = 500`. On window resize the dragged height is cleared so the table re-fits the new window size.
- Added a live **"N selected"** counter beneath each sample table.
- Added an **"All"** option to the page-size dropdowns, re-applied via a MutationObserver after reactable re-renders.
- Add a **Clear Group** button to unassign export groups.

### Annotate details navigation

- **Mousewheel** now scrolls the coverage map, BLAST synteny plot, and MSA alignment horizontally; page scroll is suppressed while hovering these views.
- **Sticky gene-name labels** overlaid on the coverage map, synteny overview, and zoomed synteny plot: each visible annotation keeps its name pinned to the edge of the view while any part of its block is on screen, then hands off to the next gene as blocks scroll past.
- Labels include a directional arrow drawn as a CSS border-triangle, with strand-aware layout (`+` strand: name then right arrow, pinned left; `-` strand: left arrow then name, pinned right). The old in-plot gggenes labels were removed.

### RStudio Server file-open notification

- `open_path()` now shows a brief notification on RStudio Server explaining that the folder opened in the Files pane (bottom-right), since that is not obvious.
- The three "output folder" buttons (assemble, annotate, userAsmb assemble) route through `open_path()`, sharing this behavior plus headless / missing-dir handling, replacing duplicated server-vs-local blocks.

### Multi-scaffold assembly joining

- Scaffold joining reworked with a new `scaffold_join.R` and a dedicated Nextflow workflow, plus a join-quality report.
- Per-scaffold BLAST hits are passed into `scaffold_join` via channel; reference sequences are indexed by name membership rather than atomic `[[`.

## Bug Fixes


### rRNA export gene names

- At request of NCBI GenBank curation team, rRNA gene names are now written as **`rrn12`** (small subunit / s-rRNA / 12S) and **`rrn16`** (large subunit / l-rRNA / 16S) across all export artifacts: main `.tbl` gene qualifier, GFF `Name=`, and per-gene exports (file/directory names, per-gene `.tbl` gene qualifier, FASTA headers). 
- Export-only transformation: internal storage, annotation, curation, and display are unchanged, product fields (`12S/16S ribosomal RNA`) are unchanged, and re-import stays safe because `normalize_rrna()` already maps `rrn12`/`rrn16` back to `rrnS`/`rrnL`.

### User-assembly WF2 crashes from coverage NA/format

- `validate`: `na.rm` plus a zero-row guard on the coverage/error heuristics; strip the outlier `#` prefix and coerce `MeanDepth`/`ErrorRate` to numeric.
- `coverage_trim`: skip the trim when no position clears the depth threshold (avoids `Inf` indices).
- `coverage_userAsmb_workflow`: drop a stray `.take(2)` that truncated the per-position depth/gc/error series.

### BLAST reference fetching

- Deduplicate BLAST reference fetches across samples by accession, with clearer failure flagging when a fetch fails.
- **NCBI taxonomy 200-with-error-body retry:** EFetch can return HTTP 200 with an error payload inside `<TaxaSet>` when its backend times out, which previously wrote `organism`/`lineage` as null and exited successfully. The taxonomy body is now validated for `<ScientificName>`; if absent, the task `stop()`s so the existing Nextflow retry re-runs it. The expensive genome FASTA download happens after the taxonomy step, so it is not re-downloaded by these retries.

### User-assembly table UI

- Filter parity: the userAsmb assemble table gained the Lock / State / Show-columns filters present in normal assemble.
- Removed reactable `columnGroups` from the userAsmb table, which broke horizontal scrolling when combined with sticky columns.
- Scoped `col_css` `display:none` rules to each table's own DOM id, fixing a cross-table filter leak where a filter on one table hid matching rows/columns in the others.
- Shift-click range selection no longer picks up CSS-hidden rows: a read-time mask plus an observer prune hidden rows from reactable's selection.

### Other fixes

- Run the PCG outlier review **before** writing export files, not after.
- `isolate()` the reactive read in the deferred job-submit callback.
- Drop spurious NA warnings from `warnings_details`.
- Harden the work-dir browser for cluster executors.
- Show a "hold tight" message while submitting a cluster job.

**Note**
To update older MitoPilot projects, please run [`MitoPilot::backwards_compatibility()`](https://smithsonian.github.io/MitoPilot/reference/backwards_compatibility.html). This will add any missing fields to the SQL database and attempt to update the Docker/Singularity container version in your project `.config` file.

**Full Changelog**: https://github.com/Smithsonian/MitoPilot/compare/1.4.8...1.4.9

# MitoPilot 1.4.8

Released 2026-06-23. Container: `macguigand/mitopilot:1.4.8`

## New Features

### Partial / incomplete mitogenome support for GenBank submission

- New per-sample **Partial** review flag to mark incomplete mitogenomes. Export titles switch from "complete genome" to "partial genome" via a new `{completeness}` template field.
- Completeness now **auto-derives from topology**: circular assemblies default to "complete", linear to "partial". New project-level **`linear_complete`** setting (in the curation-options window) forces linear assemblies to "complete" for taxa with genuinely linear mitogenomes. A per-sample partial flag always overrides.
- Marking a circular assembly as partial warns and steers users toward the Linearize button (override still allowed).
- Export window warns when an export group mixes complete + partial mitogenomes (not allowed in one GenBank submission), offering to split into `<name>-complete` / `<name>-partial`.

### Manual partial CDS start/stop controls

- New per-gene `partial_start` / `partial_stop` flags with controls to mark CDS ends as partial (exported as NCBI `<` / `>` markers, applied strand-independently).
- Poly-A stop trim: force trim a complete stop codon to "T" or "TA"
- Start/stop codons not in the gene-specific allowed list are highlighted on the 5'/3' partial buttons and flagged, matching export handling.

### CSV export

- New **Export Selected/All to CSV** buttons on the Export tab (matching Assemble/Annotate tabs).
- Every `export_files()` run auto-writes a per-sample summary CSV (`<group>_sample_info.csv`, or `sample_info_<date>.csv` for direct ID calls; gated by new `summary_csv` arg, default on). Now includes assembly length.

### Hydra HPC support

- New `hydra_setup()` replaces the [manual path modification](https://smithsonianworkshops.github.io/MitoPilot_workshop_2025/qmd/setup/05_load.html) required for Hydra R Studio server

## Bug Fixes

### WF2 atomic curate + validate writer

- Replaced four separate WF2 database writes with a **single driver-side JDBC transaction** (`write_curated_result`) that
commits the curated sequence, annotation coordinates, and partial flags together, rolling back on any error. Prevents
assembly and annotation coordinates ending up in different rotation frames (misaligned viewer, wrong exports).
CURATE/VALIDATE now emit files only, gated on the writer's commit.
- The writer runs as a native Nextflow `exec` process pinned to the **`local` executor** (it runs in the driver JVM, so it
is never dispatched to an HPC scheduler like SGE/SLURM), keeping all DB writes single-writer and NFS-safe.
- The SQLite JDBC driver is now loaded from the **`nf-sqldb` plugin classloader** (the plugin the pipeline already uses for
`fromQuery`/`sqlInsert`), instead of a vendored `sqlite-jdbc` jar shipped with the package. This removes the fragile `lib/`
jar (which `R CMD build` could silently strip), decouples the driver from the R package layout for headless cluster runs,
and keeps `write_curated_result` on the same driver version as the rest of the pipeline.

### Per-scaffold BLAST lineage

- Multi-scaffold samples whose scaffolds hit different taxa no longer all inherit the sample top-hit lineage. Lineage is now keyed on the full `(id, path, scaffold, accession)`.

### GUI lifecycle

- App now stops cleanly when the last browser session closes (unblocking the R console) but survives a page refresh via a grace-period idle check.

### Annotate edit gating

- Close / Lock&Close detect unsaved partial 5'/3' and poly-A stop edits (previously only checked translation).
- Partial toggle stores explicit `'no'` (not NA) when turned off and renders it in the table; "complete" badge shown as neutral.
- Auto-generated `EDITED:` notes (e.g. from manual linearization) are stripped on WF2 regeneration; user-typed notes preserved.

**Note**
To update older MitoPilot projects, please run [`MitoPilot::backwards_compatibility()`](https://smithsonian.github.io/MitoPilot/reference/backwards_compatibility.html). This will add any missing fields to the SQL database and attempt to update the Docker/Singularity container version in your project `.config` file.

**Full Changelog**: https://github.com/Smithsonian/MitoPilot/compare/1.4.7..1.4.8

# MitoPilot 1.4.7

Released 2026-06-16. Container: `macguigand/mitopilot:1.4.7`

### Run MitoPilot headless on an HPC cluster

This release lets you run the MitoPilot app **headless** on a high-performance computing (HPC) cluster, without RStudio Server, an X11 display, or a web browser on the cluster itself.

"Headless" means the app runs as a plain web server on a cluster node, with no graphical desktop attached. The MitoPilot app is just a Shiny web server sitting on top of your project's SQLite database and Nextflow, so it does not need a screen of its own. You start the server on the cluster, open an SSH tunnel from your local computer, and use the full assemble / annotate / curate interface in your local browser at `http://localhost:<port>`. The computation still happens on the cluster; only the interface is forwarded to you.

See the documentation for the complete walkthrough: **[Running MitoPilot headless over an SSH tunnel](https://smithsonian.github.io/MitoPilot/articles/Custom-HPC.html)**.

#### New features

- **Headless / SSH-tunnel launch.** `MitoPilot()` now accepts `host`, `port`, and `launch.browser` so you can bind the app to a fixed host and port and tell it not to open a local browser (`launch.browser = FALSE`). On startup the server prints the exact `ssh -N -L <port>:NODE:<port> <user>@<cluster>` tunnel command using the live node hostname.
- **In-app cluster submission script.** When you start a workflow over a tunnel, MitoPilot does not run Nextflow on the app's node. Instead it shows a ready-to-edit cluster submission script, pre-filled with the correct scheduler directives (`#SBATCH`, `#$`, `#PBS`, or `#BSUB`) for your project's executor. Use **"Submit to Cluster"** to submit it directly (`sbatch` / `qsub` / `bsub`), or **"Save Script Only"** to write the script and submit it yourself from a normal cluster shell.
- **Persisted resource edits.** Edits to the submission script's resource block are remembered per project (saved to `.mitopilot_submit.template`) and pre-filled on the next run. Toggling the "Resume previous run?" checkbox adds or removes `-resume` without disturbing your edits.
- **Hydra submission format.** On the NMNH Hydra cluster the submission script is generated in Hydra's expected format automatically.

### Hydra setup helper function
 
- **`hydra_setup()` helper for NMNH Hydra.** RStudio Server sessions on Hydra start with a stripped `PATH` that omits the Univa Grid Engine binaries, so Nextflow cannot find `qsub` and job submission silently fails. Call `MitoPilot::hydra_setup()` once
per session, before launching the app, to prepend the UGE, Java, and `~/bin` directories to `PATH`. Has no effect (and warns) when not running on Hydra.

#### Documentation

- Expanded the HPC vignette with a full headless / SSH-tunnel section, an RStudio Server alternative, and an R-session-before-config setup order.

**Note**
To update older MitoPilot projects, please run [`MitoPilot::backwards_compatibility()`](https://smithsonian.github.io/MitoPilot/reference/backwards_compatibility.html). This will add any missing fields to the SQL database and attempt to update the Docker/Singularity container version in your project `.config` file.

**Full Changelog**: https://github.com/Smithsonian/MitoPilot/compare/1.4.6...1.4.7

# MitoPilot 1.4.6

Released 2026-06-12. Container: `macguigand/mitopilot:1.4.6`

### New Features

#### 19 new invertebrate clade rulesets

Curation and validation support extended to 19 additional metazoan lineages.

New clades: Ascidiacea, Bivalvia, Bryozoa, Crinoidea, Demospongiae, Echinoidea, Gastropoda, Holothuroidea, Homoscleromorpha, Hydrozoa, Malacostraca, Nemertea, Ophiuroidea, Platyhelminthes, Polychaeta, Pycnogonida, Sipuncula, Thaliacea, Thecostraca.

- `curate_*/validate_*` bodies refactored into shared `curate_mito_core()` / `validate_mito_core()`; each clade is a thin wrapper
- Curate/annotate target dropdown now shows `Scientific name (Common name)` labels
- [Curation ruleset browser](https://smithsonian.github.io/MitoPilot/ruleset-browser.html) updated with all 19 new clades

#### Build assembly reference databases in R with `custom_assembly_db()`

New function builds GetOrganelle (seed + label) and/or MitoFinder reference databases for any valid clade name in the NCBI taxonomy, with no external tools required.

- Validates clade against NCBI Taxonomy; single GenBank download via NCBI E-utilities (httr2)
- CDS extracted locally from feature locations; length-based handling of unannotated sequences for seed DB
- MitoFinder DB restricted to complete mitochondrial genomes
- Dated output directory with README/manifest recording GenBank access; overwrite guard
- Documented in README and new `custom_dbs` vignette

#### Multi-path assembly review and consensus builder

New interactive workflow in the assembly coverage-details window for resolving multi-path assemblies.

- **Path scoring:** each assembly path scored on topology, length, coverage, and BLAST identity; paths ranked with a summary table
- **Conflict-block navigation:** guided step-through of alignment columns where paths disagree
- **Consensus builder:** position-by-position review panel shows base-pair level conflicts, allows user to decide how to resolve each conflict (mask with N, ambiguity code, majority rules consensus, or choose one path)
- **Finalize Consensus:** writes a resolved sequence + coverage CSV and records edit positions (JSON) and a human-readable summary in assemble notes

#### Optional ORF-finder annotation step

New optional workflow step after curation/validation finds open reading frames (ORFs) in unannotated regions.

- New `orf_opts` parameter set (window and DB table) with configurable CPUs, memory, min ORF length, max overlap fraction, strand, and nested-ORF toggle
- `orf_finder()`: runs NCBI ORFfinder, filters to unannotated regions (configurable overlap fraction), deduplicates by longest-per-region, names surviving ORFs `ORF.1..N`, and BLASTs each against the combined featureProt database with per-hit candidate gene labels
- Annotate-details window updated: "Assign gene name" button, "Edit/Remove assignment" for assigned ORFs, "Auto-assign ORFs" bulk action (assigns ORFs with >= 60% similarity to a standard PCG)
- `# ORFs` column added to Annotate and Export tables; blank (not 0) when ORF finding is disabled
- ORFs exported as hypothetical-protein CDS in `.tbl`/`.gff`; per-gene FASTA/tbl files skip ORFs
- Docker image updated with NCBI ORFfinder binary and runtime dependencies

#### Alignment-based protein-coding gene outlier review on export

New per-gene review step detects annotation outliers before export.

- Flags PCGs with start/stop codon offsets beyond a configurable amino-acid threshold (default ±10 aa) or low sequence identity
- Gene-by-gene review window with inline MSA, per-sample edit links that jump directly to the annotate-details window (auto-selecting the flagged gene), and a "Mark gene resolved" control
- "Back to Review" button in the details window returns to Export and recomputes only the edited gene's alignment (not all PCGs)
- Outlier flags + offsets included in the HTML alignment report
- Review is on by default; thresholds and toggle persisted across window opens

#### Save FASTA header templates for export

Saved FASTA header templates for mitogenome and gene sequences, persisted in a new `export_opts` table.

- Dropdown menu loads or saves a header pair
- Live validation: unbalanced braces, unknown `{column}` references, and empty templates are caught and blocked from saving
- Output path shown in a popup with a copy button after export completes

### Bug Fixes

#### MITOS2 crash on ambiguous bases in CDS

`Biostrings::translate()` with default `if.fuzzy.codon = "error"` crashed WF2 when MITOS2 annotated a CDS containing IUPAC ambiguity codes (e.g. from low-coverage regions). Fixed by passing `if.fuzzy.codon = "solve"`.

#### Circular wrap-around genes in annotation plots

Coverage map and BLAST-reference synteny plots now correctly render genes that cross the circular origin; such genes are split into two arcs via `split_wrapped_genes()` with the label placed on the longer arc.

#### Consensus assembly coverage format

The multi-path "Build resolved assembly" (Path 0) wrote a raw coverage CSV incompatible with `annotate()`, causing a crash when moving a consensus assembly to the Annotate step. Fixed by sharing the rolling-stats helpers between `coverage()` and `persist_path0()`.

#### Alignment-edit save performance

Per-hit stat recomputation (similarity, pct identity, end gaps) was running ~400 individual pairwise alignments and reloading BLOSUM80 on each call. Replaced with one vectorized `pwalign` call for similarity/pctid and one DECIPHER alignment per target. Result identical; substantially faster.

### Internal

- Default `max_blast_hits` reduced from 100 to 10 (speeds up in-app review and alignment-edit save; existing projects keep their stored value)
- `blast_genbank` workflow switched from `nt` to `core_nt` database
- `maxForks` throttle added to `blast_ref_fetch` Nextflow process

---

**Note**
To update older MitoPilot projects, please run [`MitoPilot::backwards_compatibility()`](https://smithsonian.github.io/MitoPilot/reference/backwards_compatibility.html). This will add any missing fields to the SQL database and attempt to update the Docker/Singularity container version in your project `.config` file.

**Full Changelog**: https://github.com/Smithsonian/MitoPilot/compare/1.4.5...1.4.6

# MitoPilot 1.4.5

Released 2026-06-08. Container: `macguigand/mitopilot:1.4.5`

### New Features
#### Interactive curation ruleset browser

New `ruleset_browser()` function generates a self-contained interactive HTML visualization of MitoPilot's taxon-specific curation rulesets.

- **Left pane:** collapsible taxonomy tree rooted at Metazoa, built from NCBI Taxonomy lineages. Supports scroll, expand/collapse, and live filter. NCBI lineages cached locally; works offline after first run.
- **Right pane:** per-clade genetic code, global thresholds, and collapsible PCG/rRNA/tRNA/ctrl tables showing effective per-gene rules with type defaults overlaid by per-gene overrides. Parameter tooltips throughout.
- Wired into the pkgdown docs as a new **Curation Rulesets** article embedding a committed HTML snapshot (no build-time NCBI dependency).

#### Sample status filters on Assemble and Annotate tables

Lock and State picker dropdowns added to the Assemble and Annotate sample tables.

- Filter by Lock (Unlocked/Locked) and by workflow State without reloading or re-sorting the table.
- Uses CSS-hide so sort order, search, column filters, current page, and row selection survive toggling.

**Known tradeoff:** hidden rows still count toward pagination, so a page may show fewer rows than its page size while filters are active.

#### `transl_except` qualifier for poly-A-completed stop codons

GenBank feature table export now writes `/transl_except=(pos:..,aa:TERM)` alongside the existing poly-A note when a PCG stop codon is completed by 3' A-tail addition. Applies to intron, non-intron, and single-gene export tables.

#### Configurable codon step size

The fixed ±5/±10 step buttons on START and STOP codon controls are replaced by a single numeric step-size box per codon (default 1, max 50). Control layout collapsed to a single row: `START [−][+] [box]  STOP [−][+] [box]  [ ] single codon`.

### Bug Fixes

#### Alignment view not refreshing on repeated codon edits

The annotate-details MSA stopped updating when consecutive edits produced the same codon string. `reactiveValues` uses `identical()` to deduplicate assignments, which cannot detect content changes inside `XStringSet` S4 external pointers, so the rebuilt alignment compared equal and `msaR` never re-rendered. Fix: bump a monotonic `render_nonce` on each edit so the alignment always invalidates.

#### Codon edit handlers consolidated

The eight duplicated `*-simple-5`/`*-simple-10` handler sets are collapsed into one handler each, driven by the configurable step-size input. Manual edits now correctly honor gene-specific codon lists from the taxon-specific curation ruleset (fixed `modifyList` argument order so per-gene rules override type defaults).

#### Start/stop codon button order

The START `-`/`+` buttons were swapped; corrected so order is intuitive (left=decrease, right=increase).

**Note**
To update older MitoPilot projects, please run the R function [`MitoPilot::backwards_compatibility()`](https://smithsonian.github.io/MitoPilot/reference/backwards_compatibility.html). This will add any missing fields to the SQL database and attempt to update the Docker/Singularity container version in your project `.config` file.

**Full Changelog**: https://github.com/Smithsonian/MitoPilot/compare/1.4.4...1.4.5

# MitoPilot 1.4.4

Released 2026-05-29. Container: `macguigand/mitopilot:1.4.4`

### Bug Fixes

#### Issue with low-confidence tRNA annotations

Low-confidence tRNA calls from tRNAscan-SE/ARWEN/ARAGORN with an unresolved `NNN` anticodon could overlap a valid MITOS2 tRNA, suppress it via the overlap filter, and then be removed by the final `NNN` filter, leaving the gene unannotated. `NNN` calls are now excluded from overlap suppression, so valid MITOS2 tRNAs are retained.

### New Features

#### Option to retain low-confidence (`NNN`) tRNAs

A new **"Retain low-confidence (NNN anticodon) tRNAs"** toggle in the annotate-options modal (off by default) keeps `NNN`-anticodon tRNAs in the output when desired. When retained, each such tRNA is flagged during validation with a `low-confidence tRNA (NNN anticodon)` warning, surfaced in the Annotate table's warnings column.

Available as the `retain_low_conf_trna` parameter of `annotate()` and plumbed through the Nextflow annotate workflow. Existing projects gain the new option automatically via a database migration with  [`MitoPilot::backwards_compatibility()`](https://smithsonian.github.io/MitoPilot/reference/backwards_compatibility.html) (default: off).

### Performance

#### Faster, higher-fidelity Annotate details figures

- Coverage map is rendered with `ragg::agg_png`, bypassing Cairo's surface-size limit that previously truncated the right edge of large mitogenome plots.
- Coverage, synteny, and alignment/codon views render substantially faster.
- Coverage and gene-track panels are aligned; synteny identity stripes added.
- The genetic-code lookup table is cached once per session instead of being recomputed on every codon edit.

#### Smoother Annotate details editing

- Editing the Notes field no longer reloads the figures on every keystroke.
- Toggling review state (ID verified / reviewed / problematic) no longer re-renders the coverage and synteny figures.

### Internal / Build

- Added `ragg` and `cowplot` to package dependencies.
- The Docker image now installs `r-ragg` from conda-forge (with its font/graphics dependencies); rebuild the image to pick up the new dependency and the annotation fix.



**Note**
To update older MitoPilot projects, please run the R function [`MitoPilot::backwards_compatibility()`](https://smithsonian.github.io/MitoPilot/reference/backwards_compatibility.html). This will add any missing fields to the SQL database and attempt to update the Docker/Singularity container version in your project `.config` file.

**Full Changelog**: https://github.com/Smithsonian/MitoPilot/compare/1.4.3...1.4.4

# MitoPilot 1.4.3

Released 2026-05-27. Container: `macguigand/mitopilot:1.4.3`

## MitoPilot v1.4.3

### New Features

#### Generic HPC executor support

MitoPilot now ships built-in Nextflow config templates for generic job schedulers: `slurm`, `sge`, `pbs`, and `lsf`. A new `generate_config()` helper saves a named cluster profile (queue, account, container engine, etc.) to the user config directory
for reuse across projects:

```r
## configure once
MitoPilot::generate_config(
  name             = "my_cluster",
  scheduler        = "slurm",
  queue            = "general",
  account          = "my_allocation",
  container_engine = "apptainer"
)

## reuse for any project
MitoPilot::new_project(..., executor = "my_cluster")

## see all available configs
MitoPilot::list_configs()
```

Custom Nextflow configs can still be passed directly via config = "path/to/.config". 

See the new [Custom HPC vignette](https://smithsonian.github.io/MitoPilot/articles/Custom-HPC.html) for details.  

#### CSV export for Assemble and Annotate tables

"Export Selected to CSV" and "Export All to CSV" download buttons are now available at the bottom of the Assemble and Annotate modals. The selected-rows export is disabled when no rows are selected. Files download directly to the browser, including
when running via RStudio Server on a remote cluster.

#### Shift click to select multiple samples

Added ability to shift click and add a range of samples in the Shiny app tables. 

#### Alternating row shading

Subtle striped row shading added to the Assemble and Annotate sample tables for improved readability. Selection highlighting is preserved.

### Bug Fixes

#### `BLAST_REF_FETCH` silent failures

`fetch_blast_ref()` was calling `write_empty_ref()` and returning exitcode 0 on empty or malformed NCBI GFF3 responses. Nextflow's retry and failure-detection machinery therefore never fired, and affected samples were falsely advanced to
`assemble_switch = 2` (WF1 complete) with no reference data. All failure paths now call `stop()`, enabling proper retry behavior and correct terminal failure marking (`poor_blast_ref = 'failed'`, `assemble_switch = 3`).

#### Missing `blast_lineage` on Nextflow `-resume`

On `-resume`, re-running ASSEMBLE wipes `assemblies.blast_lineage` via `INSERT OR REPLACE`. If the cached `blast_ref_fetch` lineage UPDATE fired before `blast_genbank` had re-written `blast_accession`, the accession-matched lineage UPDATE was a no-op,  leaving `blast_lineage` blank in the app's Assemble modal. Fixed by propagating `blast_lineage` from the `assemble` table via a correlated subquery whenever `blast_genbank` writes scaffold BLAST hits, covering both task-ordering scenarios.

#### `ID Verified` column filter not matching `NULL` rows

`NULL` `ID_verified` values were displayed as `"no"` by the badge renderer but stored as `NULL` in the data, so filtering the column for `"no"` missed most rows. Fixed by coercing `NULL` to `"no"` in `fetch_annotate_data()`.


**Note**
To update older MitoPilot projects, please run the R function [`MitoPilot::backwards_compatibility()`](https://smithsonian.github.io/MitoPilot/reference/backwards_compatibility.html). This will add any missing fields to the SQL database and attempt to update the Docker/Singularity container version in your project `.config` file.

**Full Changelog**: https://github.com/Smithsonian/MitoPilot/compare/1.4.2...1.4.3

# MitoPilot 1.4.2

Released 2026-05-26. Container: `macguigand/mitopilot:1.4.2`

### New Features

 ### Optional NCBI API key
- New `ncbi_api_key` argument on `new_project()` and `new_project_userAsmb()`, substituted into all config templates as `params.ncbi_api_key`.
- `blast_genbank` and `blast_ref_fetch` export `NCBI_API_KEY`, raising the remote BLAST and eutils rate limits. The key is redacted from logs. An empty key keeps the existing anonymous behavior, so no migration is needed for existing projects.

### Bug Fixes

#### WF1 state machine hardening
- **Terminal state no longer overwritten:** `state=3` from `ASSEMBLE` is now reserved for truly terminal outcomes (empty output, exceeds path/scaffold limits, all scaffolds below min length). Only `state=4` rows are emitted downstream, so
BLAST/ref-fetch can no longer flip a dead sample back to `state=2` or clobber its assembly warning.
- **Race that stranded `no_blast` samples fixed:** `ASSEMBLE` is now the sole writer for `run_blast=0` samples, finalizing `state=2` directly instead of racing with `BLAST_GENBANK`'s batched skip-write.
- **COVERAGE now runs for `no_blast` samples**, so WF2 ANNOTATE finds the expected `coverageStats.csv`.
- BLAST/ref-fetch notes use tag-and-replace append semantics, so warnings accumulate cleanly across stages and `-resume` retries stay idempotent. All downstream UPDATEs are guarded with `WHERE assemble_switch = 4`.
- `poor_blast_ref` now distinguishes failure source: NULL = assembly, `failed` = BLAST/ref-fetch, `good` = success.

#### BLAST robustness
- Empty BLAST output (genuine no-hit or silent connection failure) now exits non-zero and retries up to 3 times with backoff; exhausted retries write `state=3` with a note instead of silently completing.
- `state=2` is written only once the reference fetch also succeeds, matching the semantics that WF1 is complete only when both steps pass.
- Added `maxForks` to `blast_genbank` to limit concurrent remote calls and reduce NCBI timeouts.

### Other Changes
- Adjusted Hydra controller and SGE job memory requests.

**Note**
To update older MitoPilot projects, please run the R function [`MitoPilot::backwards_compatibility()`](https://smithsonian.github.io/MitoPilot/reference/backwards_compatibility.html). This will add any missing fields to the SQL database and attempt to update the Docker/Singularity container version in your project `.config` file.

**Full Changelog**: https://github.com/Smithsonian/MitoPilot/compare/1.4.1...1.4.2

# MitoPilot 1.4.1

Released 2026-05-20. Container: `macguigand/mitopilot:1.4.1`

**BLAST enhancements**
- Per-scaffold GenBank BLAST search and reference sequence fetching — each scaffold is evaluated independently rather than requiring a single-contig assembly
- BLAST reference alignment status badges in the Assembly table
- BLAST reference fetch timeouts are now surfaced as sample-level failures with a descriptive note rather than silently stalling

**Assembly QC**
- Scaffolds below the `min_assembly_length` threshold are colored and flagged `ignore = 1`
- Multi-scaffold samples with exactly one scaffold above the `min_assembly_length` threshold are now promoted to **Successful** rather than **Problematic** state. A "disconnected contigs" note is still
recorded.

**Multi-scaffold support**
- Fragmented-assembly details modal is now enabled, allowing scaffold-level review
- Scaffold `ignore` flags are now respected throughout the Annotate and Export modules
- Export hardened against multi-scaffold edge cases
- Assembly table auto-promotes to Successful when only one scaffold/path is active; auto-reverts to Problematic if multiple become active

**UI improvements**
- Column-group picker in Assembly and Annotate tables
- Compact status UI in the Annotate module
- BLAST reference alignment status column and badges in Assembly table

**In-app help documentation**
- Bundled `--help` text for getOrganelle, MitoFinder, MITOS2, BLASTN, tRNAscan-SE, ARAGORN, and ARWEN
- "?" icons throughout the UI open tool documentation inline

**Bug fixes**
- Fixed remote BLAST reference path resolution after per-accession subdirectory refactor
- Fixed Nextflow channel deadlock in the ASSEMBLE workflow that caused COVERAGE, BLAST_GENBANK, and BLAST_REF_FETCH process nodes to remain active indefinitely after all tasks completed

**Full Changelog**: https://github.com/Smithsonian/MitoPilot/compare/1.4.0...1.4.1

# MitoPilot 1.4.0

Released 2026-05-12. Container: `macguigand/mitopilot:1.4.0`

- **Remote GenBank BLAST tools**: new BLAST_REF_FETCH / BLAST_REF_ALIGN workflows execute during the Assemble module. For each sample, fetch the top BLAST hit directly from GenBank and compute pairwise mitgenome alignment against your assembly. Remote BLAST results are shown in MitoPilot app (with hyperlinks to GenBank). Best remote BLAST hit is used during the automatic curation of gene annotations. Remote BLAST hit it appended to FASTA header during export as `[note=annotation compared to GenBank accession XXXXX]` to facilitate review by GenBank staff.
- **Synteny/alignment visualization**: interactive 4-track synteny/alignment plot (overview + zoomed bp-level view) comparing sample assembly to remote BLAST reference; features include click-to-zoom, zoom-to-gene, rolling windows of % sequence similarity, and bp-level alignment
- **ARAGORN tRNA rescue**: integrate ARAGORN as an optional tool for tRNA detection 
- **Live per-sample assembly writes**: Assemble table in app updates per-sample as each job completes rather than at the end of the Assemble module; user must still click the `refresh` button
- **max_paths / max_scaffolds thresholds**: set in the `Assembly Opts` to cap ambiguous outputs and reduce the number of coverage stat calculations; samples exceeding thresholds fail gracefully with diagnostic notes
- `Merge` button in Details section of Annotate page can now combine PCGs or rRNAs
- Optimizations to improve execution on Smithsonian Hydra cluster and reduce number of Java OOM crashes
- UX improvements, including gene type and annotation tool column badge colors in annotation details table

**Note**
To update older MitoPilot projects, please run the R function [`MitoPilot::backwards_compatibility()`](https://smithsonian.github.io/MitoPilot/reference/backwards_compatibility.html). This will add any missing fields to the SQL database and attempt to update the Docker/Singularity container version in your project `.config` file.

**Full Changelog**: https://github.com/Smithsonian/MitoPilot/compare/1.3.10...1.4.0

# MitoPilot 1.3.10

Released 2026-04-28. Container: `macguigand/mitopilot:1.3.10`

- added option to use [ARWEN](https://doi.org/10.1093/bioinformatics/btm573) for tRNA prediction alongside tRNAScan and MITOS, may be helpful for samples missing tRNAs
- updated logic to better reconcile overlapping tRNA annotations between tRNAscan, MITOS, and ARWEN
- updated default options for tRNAscan to remove poor tRNA predictions
- added "Merge PCG" button to Details section of Annotate page, allows user to combine split annotations for the same gene
- added "Restore" button to Details section of Annotate page, can now easily restore annotations deleted by the user

**Note**
To update older MitoPilot projects, please run the R function [`MitoPilot::backwards_compatibility()`](https://smithsonian.github.io/MitoPilot/reference/backwards_compatibility.html). This will add any missing fields to the SQL database and attempt to update the Docker/Singularity container version in your project `.config` file.

**Full Changelog**: https://github.com/Smithsonian/MitoPilot/compare/1.3.9...1.3.10

# MitoPilot 1.3.9

Released 2025-12-17. Container: `macguigand/mitopilot:1.3.9`

- added curation parameter rules for annelids
- bugfix for validation step

**Note**
To update older MitoPilot projects, please run the R function [`MitoPilot::backwards_compatibility()`](https://smithsonian.github.io/MitoPilot/reference/backwards_compatibility.html). This will add any missing fields to the SQL database and attempt to update the Docker/Singularity container version in your project `.config` file.

**Full Changelog**: https://github.com/Smithsonian/MitoPilot/compare/1.3.8...1.3.9

# MitoPilot 1.3.8

Released 2025-12-15. Container: `macguigand/mitopilot:1.3.8`

- updated to MITOS v2.1.10 to fix issue with D-loop orientation (see https://gitlab.com/Bernt/MITOS/-/issues/58)
- updated Docker image R version to 4.5.2 
- added curation parameter rules for ctenophores and birds
- minor app bugfixes

**Note**
To update older MitoPilot projects, please run the R function [`MitoPilot::backwards_compatibility()`](https://smithsonian.github.io/MitoPilot/reference/backwards_compatibility.html). This will add any missing fields to the SQL database and attempt to update the Docker/Singularity container version in your project `.config` file.

**Full Changelog**: https://github.com/Smithsonian/MitoPilot/compare/1.3.7...1.3.8

# MitoPilot 1.3.7

Released 2025-12-03. Container: `macguigand/mitopilot:1.3.7`

- improved performance of start/stop codon editing buttons
- added new `tbl_to_gff3` function, converts GenBank feature table (generated by MitoPilot) to GFF3 format
- minor tweak to control region ID in GFF export
- bugfix for validating annotations with introns

**Note**
To update older MitoPilot projects, please run the R function [`MitoPilot::backwards_compatibility()`](https://smithsonian.github.io/MitoPilot/reference/backwards_compatibility.html). This will add any missing fields to the SQL database and attempt to update the Docker/Singularity container version in your project `.config` file.

**Full Changelog**: https://github.com/Smithsonian/MitoPilot/compare/1.3.6...1.3.7

# MitoPilot 1.3.6

Released 2025-11-21. Container: `macguigand/mitopilot:1.3.6`

- new curation ruleset for hexacorals, allows introns in ND5 and CO1
- handle annotation and export of genes with introns
- updated the NOAA SEDNA cluster documentation
 
**Note**
To update older MitoPilot projects, please run the R function [`MitoPilot::backwards_compatibility()`](https://smithsonian.github.io/MitoPilot/reference/backwards_compatibility.html). This will add any missing fields to the SQL database and attempt to update the Docker/Singularity container version in your project `.config` file.

**Full Changelog**: https://github.com/Smithsonian/MitoPilot/compare/1.3.5...1.3.6

# MitoPilot 1.3.5

Released 2025-09-05. Container: `macguigand/mitopilot:1.3.5`

Changed Nextflow implementation to skip samples that fail during the pipeline, allowing good samples to continue running.

**Note**
To update older MitoPilot projects, please run the R function [`MitoPilot::backwards_compatibility()`](https://smithsonian.github.io/MitoPilot/reference/backwards_compatibility.html). This will add any missing fields to the SQL database and attempt to update the Docker/Singularity container version in your project `.config` file.

**Full Changelog**: https://github.com/Smithsonian/MitoPilot/compare/1.3.4...1.3.5

# MitoPilot 1.3.4

Released 2025-08-20. Container: `macguigand/mitopilot:1.3.4`

- added curation ruleset for turtles
- clarified help documentation for `new_project` argument `min_depth`
 
**Note**
To update older MitoPilot projects, please run the R function [`MitoPilot::backwards_compatibility()`](https://smithsonian.github.io/MitoPilot/reference/backwards_compatibility.html). This will add any missing fields to the SQL database and attempt to update the Docker/Singularity container version in your project `.config` file.

**Full Changelog**: https://github.com/Smithsonian/MitoPilot/compare/1.3.3...1.3.4

# MitoPilot 1.3.3

Released 2025-08-15. Container: `macguigand/mitopilot:1.3.3`

- New function custom_curation_db generate a custom curation database from user-supplied table of translated (amino acid) mitochondrial gene sequences
- Bugfix for switching between curate option sets in Shiny app

**Note**
To update older MitoPilot projects, please run the R function [`MitoPilot::backwards_compatibility()`](https://smithsonian.github.io/MitoPilot/reference/backwards_compatibility.html). This will add any missing fields to the SQL database and attempt to update the Docker/Singularity container version in your project `.config` file.

**Full Changelog**: https://github.com/Smithsonian/MitoPilot/compare/1.3.2...1.3.3

# MitoPilot 1.3.2

Released 2025-08-08. Container: `macguigand/mitopilot:1.3.2`

- check for lowercase "circular" or "linear" values in Topology field when user supplies their own assemblies
- bugfix for "add_samples" function

**Note**
To update older MitoPilot projects, please run the R function [`MitoPilot::backwards_compatibility()`](https://smithsonian.github.io/MitoPilot/reference/backwards_compatibility.html). This will add any missing fields to the SQL database and attempt to update the Docker/Singularity container version in your project `.config` file.

**Full Changelog**: https://github.com/Smithsonian/MitoPilot/compare/1.3.1...1.3.2

# MitoPilot 1.3.1

Released 2025-07-31. Container: `macguigand/mitopilot:1.3.1`

- added curation ruleset for lepidosaurs
- fixed bug in curation rulesets and parameters for octocorals and copepods

**Note**
To update older MitoPilot projects, please run the R function [`MitoPilot::backwards_compatibility()`](https://smithsonian.github.io/MitoPilot/reference/backwards_compatibility.html). This will add any missing fields to the SQL database and attempt to update the Docker/Singularity container version in your project `.config` file.

**Full Changelog**: https://github.com/Smithsonian/MitoPilot/compare/1.3.0...1.3.1

# MitoPilot 1.3.0

Released 2025-07-22. Container: `macguigand/mitopilot:1.3.0`

- allow user to provide custom curation databases
- new metazoa RefSeq 231 database is included for curation (old metazoa database was RefSeq 89)
- NMNH users can submit MitoPilot Hydra jobs directly from the app

**Note**
To update older MitoPilot projects, please run the R function [`MitoPilot::backwards_compatibility()`](https://smithsonian.github.io/MitoPilot/reference/backwards_compatibility.html). This will add any missing fields to the SQL database and attempt to update the Docker/Singularity container version in your project `.config` file.

**Full Changelog**: https://github.com/Smithsonian/MitoPilot/compare/1.2.8...1.3.0

# MitoPilot 1.2.8

Released 2025-07-15. Container: `macguigand/mitopilot:1.2.8`

- new curation ruleset for octocorals
- bug fixes for start/stop codon manual editing
- new buttons to manually shift start/stop codon positions in increments of 1, 5, or 10
- "DELETED" message sent to notes instead of warnings field when removing gene annotations
- require sample IDs to only contain alphanumeric characters, dashes, underscores, or colons
- print sample IDs that violate rules when initializing a project
- fixed bug in init_db_userAsmb
- "problematic" field defaults to "no" when initializing a project

**Note**
To update older MitoPilot projects, please run the R function [`MitoPilot::backwards_compatibility()`](https://smithsonian.github.io/MitoPilot/reference/backwards_compatibility.html). This will add any missing fields to the SQL database and attempt to update the Docker/Singularity container version in your project `.config` file.

**Full Changelog**: https://github.com/Smithsonian/MitoPilot/compare/1.2.7...1.2.8

# MitoPilot 1.2.7

Released 2025-06-27. Container: `macguigand/mitopilot:1.2.7`

- adds preliminary curation ruleset for copepods
- fixes error in handling of tRNAs with undetermined anticodons
- fixes bug in curation script when correcting overextended negative strand gene start codon positions
- adds warnings filter to Annnotate GUI
- `warnings` field in the annotate table now reports the total number of warnings rather than the number of genes with warnings
- `extra` field in annotate table now lists duplicated genes by name rather than a count of total duplicated genes

**Important**
The `extra` field in the annotation table will not update unless you rerun the Annotate module.

**Note**
To update older MitoPilot projects, please run the R function [`MitoPilot::backwards_compatibility()`](https://smithsonian.github.io/MitoPilot/reference/backwards_compatibility.html). This will add any missing fields to the SQL database and attempt to update the Docker/Singularity container version in your project `.config` file.

**Full Changelog**: https://github.com/Smithsonian/MitoPilot/compare/1.2.6...1.2.7

# MitoPilot 1.2.6

Released 2025-06-20. Container: `macguigand/mitopilot:1.2.6`

- Fixed a start/stop codon bug when MITOS2 annotation wraps around a circular assembly

**Full Changelog**: https://github.com/Smithsonian/MitoPilot/compare/1.2.5...1.2.6

**Note**
To update older MitoPilot projects, please run the R function [`MitoPilot::backwards_compatibility()`](https://smithsonian.github.io/MitoPilot/reference/backwards_compatibility.html). This will add any missing fields to the SQL database and attempt to update the container version in your project `.config` file.

# MitoPilot 1.2.5

Released 2025-06-17. Container: `macguigand/mitopilot:1.2.5`

- fixed bug with Nextflow progress output ANSI encoding in the Shiny app

**Full Changelog**: https://github.com/Smithsonian/MitoPilot/compare/1.2.4...1.2.5

**Note**
To update older MitoPilot projects for this release, please run the R function [`MitoPilot::backwards_compatibility()`](https://smithsonian.github.io/MitoPilot/reference/backwards_compatibility.html). This will add any missing fields to the SQL database. You should also make sure your project's `.config` file lists the matching container (`container = 'macguigand/mitopilot:1.2.5'`).

# MitoPilot 1.2.4

Released 2025-06-03. Container: `macguigand/mitopilot:1.2.4`

- reject stop codon auto-curation if it introduces an internal stop codon
- fixed bugs with Annotate details stop codon manual editing buttons

**Full Changelog**: https://github.com/Smithsonian/MitoPilot/compare/1.2.3...1.2.4

**Note**
To update older MitoPilot projects for this release, please run the R function [`MitoPilot::backwards_compatibility()`](https://smithsonian.github.io/MitoPilot/reference/backwards_compatibility.html). This will add any missing fields to the SQL database. You should also make sure your project's `.config` file lists the matching container (`container = 'macguigand/mitopilot:1.2.4'`).

# MitoPilot 1.2.3

Released 2025-05-21. Container: `macguigand/mitopilot:1.2.3`

Added alternate start codons for NAD1, NAD3, and NAD4L to curation parameters in `params_starfish_mito.R` based on RefSeq starfish mitogenome annotations.

**Full Changelog**: https://github.com/Smithsonian/MitoPilot/compare/1.2.2...1.2.3

**Note**
To use old projects with the latest MitoPilot release, please run the function [`MitoPilot::backwards_compatibility()`](https://smithsonian.github.io/MitoPilot/reference/backwards_compatibility.html). This will update the SQL database with any missing fields (columns).

# MitoPilot 1.2.2

Released 2025-05-20. Container: `macguigand/mitopilot:1.2.2`

Changed coverage plotting in the Annotate details panel of the Shiny app.

**Full Changelog**: https://github.com/Smithsonian/MitoPilot/compare/1.2.1...1.2.2

**Note**
To use old projects with the latest MitoPilot release, please run the function [`MitoPilot::backwards_compatibility()`](https://smithsonian.github.io/MitoPilot/reference/backwards_compatibility.html). This will update the SQL database with any missing fields (columns).

# MitoPilot 1.2.1

Released 2025-05-08. Container: `macguigand/mitopilot:1.2.1`

Fixed the following R functions to work with user-assembly projects
- `update_sample_metadata()`
- `update_sample_seqdata()`
- `add_samples()`
- `export_db_to_csv()`

**Full Changelog**: https://github.com/Smithsonian/MitoPilot/compare/1.2.0...1.2.1

**Note**
To use old projects with the latest MitoPilot release, please run the function [`MitoPilot::backwards_compatibility()`](https://smithsonian.github.io/MitoPilot/reference/backwards_compatibility.html). This will update the SQL database with any missing fields (columns).

# MitoPilot 1.2.0

Released 2025-05-07. Container: `macguigand/mitopilot:1.2.0`

### What's Changed
* New R function `new_project_userAsmb()` allows users to create a project with their own mitogenome assemblies

**Full Changelog**: https://github.com/Smithsonian/MitoPilot/compare/1.1.0...1.2.0

**Note**
To use old projects with the latest MitoPilot release, please run the function [`MitoPilot::backwards_compatibility()`](https://smithsonian.github.io/MitoPilot/reference/backwards_compatibility.html). This will update the SQL database with any missing fields (columns).

# MitoPilot 1.1.0

Released 2025-05-05. Container: `macguigand/mitopilot:1.1.0`

### What's Changed
* New assembler option: MitoFinder by @dmacguigan in https://github.com/Smithsonian/MitoPilot/pull/36

**Full Changelog**: https://github.com/Smithsonian/MitoPilot/compare/1.0.0...1.1.0

**Note**
To use old projects with the latest MitoPilot release, please run the function [`MitoPilot::backwards_compatibility()`](https://smithsonian.github.io/MitoPilot/reference/backwards_compatibility.html). This will update the SQL database with any missing fields (columns).

# MitoPilot 1.0.0

Released 2025-04-28. Container: `macguigand/mitopilot:1.0.0`

First release

