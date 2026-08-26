include { scaffold_join } from './scaffold_join.nf'

// Upsert the joined Path 0 row; ignore the original scaffold rows; mirror the
// consensus metadata into the annotate row (as the app's persist_path0 does).
// The joined row inherits the BLAST hit of the reference used for the join (the
// per-ID top hit in `assemble`) via subqueries, but blanks %ident/%cov/evalue:
// those described a single scaffold and are meaningless for the joined assembly.
params.sqlUpsertJoined = '''INSERT OR REPLACE INTO assemblies
    (ID, path, scaffold, topology, length, length_raw, sequence, depth, gc, errors, ignore, edited, time_stamp,
     blast_accession, blast_species, blast_lineage, blast_pident, blast_qcovs, blast_evalue)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?,
     (SELECT blast_accession FROM assemble WHERE ID = ?),
     (SELECT blast_species FROM assemble WHERE ID = ?),
     (SELECT blast_lineage FROM assemble WHERE ID = ?),
     NULL, NULL, NULL)'''

params.sqlIgnoreOriginals = 'UPDATE assemblies SET ignore = 1 WHERE ID = ? AND path > 0'

// annotate is keyed (ID, path, scaffold); the joined consensus is unit (ID,0,0).
// Upsert that unit (inheriting the sample's option sets from an existing row) so
// it becomes the single non-ignored annotation unit. The original path>0 units'
// annotate rows are left in place but never selected (their assemblies rows are
// set to ignore=1 above and the app filters children on ignore=0).
params.sqlSyncAnnotateJoin = '''INSERT OR REPLACE INTO annotate
    (ID, path, scaffold, scaffolds, topology, length, partial,
     annotate_opts, curate_opts, orf_opts, annotate_switch, annotate_lock, reviewed, time_stamp)
    VALUES (?, 0, 0, 1, ?, ?, ?,
     (SELECT annotate_opts FROM annotate WHERE ID = ? ORDER BY path, scaffold LIMIT 1),
     (SELECT curate_opts FROM annotate WHERE ID = ? ORDER BY path, scaffold LIMIT 1),
     (SELECT orf_opts FROM annotate WHERE ID = ? ORDER BY path, scaffold LIMIT 1),
     1, 0, "no", ?)'''

// Sample state written by the join itself. Before this existed the reference
// fetch promoted every sample to state 2 before the join had run, so a crashed
// join was reported as a success.
//   joined           -> state 2, no note (a stale note from an earlier run is
//                       cleared, not left to mislead).
//   declined         -> state 2, note explaining why nothing was joined.
//   skipped          -> see sqlWriteJoinSkipped below.
//   crashed / dropped-> state 3, note naming what went wrong.
// join_switch is cleared either way so a queued redo does not repeat forever.
// No assemble_switch guard: joined and declined are real verdicts from a task
// that ran, and a redo deliberately re-reports a sample at 2 or 3.
params.sqlWriteJoinDone   = 'UPDATE assemble SET assemble_switch = 2, join_notes = ?, join_switch = NULL WHERE ID = ?'
params.sqlWriteJoinFailed = 'UPDATE assemble SET assemble_switch = 3, join_notes = ?, join_switch = NULL WHERE ID = ?'

// "skipped" means the join_scaffolds toggle is off, so the task deliberately did
// no work and has NO opinion about the sample. It therefore may not overwrite an
// opinion something else already recorded:
//   assemble_switch - promoted 4 -> 2 exactly as before (a normal run reaching
//     this point with the toggle off is a success), but a sample already at 3
//     stays at 3. Without the CASE, a redo queued on a crashed sample whose
//     toggle happens to be off would silently promote it to done.
//   join_notes      - cleared only on that same 4 -> 2 promotion, so a normal
//     run with the toggle off finishes clean instead of wearing a stale note
//     from an earlier run. On any other state (a redo of a sample already at 3)
//     the note is left alone, so a toggled-off redo cannot launder a failure.
// join_switch is still cleared, so the redo resolves rather than looping.
params.sqlWriteJoinSkipped = 'UPDATE assemble SET ' +
    'assemble_switch = CASE WHEN assemble_switch = 4 THEN 2 ELSE assemble_switch END, ' +
    'join_notes = CASE WHEN assemble_switch = 4 THEN NULL ELSE join_notes END, ' +
    'join_switch = NULL WHERE ID = ?'

// A redo whose inputs are not on disk. Same shape as sqlWriteJoinSkipped: the
// note is always recorded, but the state may not be demoted from 2. A
// fragmented sample with BLAST switched off legitimately sits at 2 with no
// reference accession, is join-eligible, and has published output, so a stale
// or mistaken queued redo would otherwise turn a healthy sample into a failure.
params.sqlWriteJoinRedoMissing = 'UPDATE assemble SET ' +
    'assemble_switch = CASE WHEN assemble_switch = 2 THEN 2 ELSE 3 END, ' +
    'join_notes = ?, join_switch = NULL WHERE ID = ?'

params.joinRedoMissingMsg = "Scaffold join redo did not run: this sample's published assembly output is not on disk, so there was nothing to join from. Missing: "

params.joinCrashedMsg = "Scaffold join failed. The task produced no outcome file, so it died rather than declining to join. Check .command.log in the task's work directory (empty except for a signal message means the scheduler killed it: exit 137 = OOM, exit 140 = SGE runtime limit). To retry, re-run the scaffold join for this sample."

// Nextflow's file() returns a single Path for one match and a List for many.
// The redo path below always wants a list, empty when nothing matched.
def asFileList(x) {
    if (x == null) return []
    return (x instanceof List) ? x : [x]
}

// Precomputed scaffold->reference mappings (one row per ID/scaffold/ref) so the
// in-app manual join editor needs no minimap2. The clear + insert must run in
// one driver-side transaction: two independent sqlInsert operators batch and
// commit on separate connections with no ordering guarantee, so the DELETE
// batch can commit after the INSERT batch and empty the table.
process write_scaffold_mappings {

    executor 'local'
    maxForks 1
    errorStrategy 'ignore'
    tag "${id}"

    input:
    tuple val(id), val(csv_fn)

    output:
    val(id)

    exec:
    def dbPath = "${workflow.launchDir}/.sqlite"
    // Resolve org.sqlite.JDBC from the nf-sqldb plugin classloader, falling back
    // to the app classpath (same approach as write_curated_result).
    def driverClass = null
    try {
        def pcl = nextflow.plugin.Plugins.manager?.getPluginClassLoader('nf-sqldb')
        if (pcl) driverClass = pcl.loadClass("org.sqlite.JDBC")
    } catch (Throwable ignored) {}
    if (driverClass == null) {
        try { driverClass = Class.forName("org.sqlite.JDBC") } catch (Throwable ignored) {}
    }
    if (driverClass == null)
        throw new RuntimeException("Could not load org.sqlite.JDBC from the nf-sqldb plugin or the classpath")
    def drv = driverClass.getDeclaredConstructor().newInstance()
    def conn = drv.connect("jdbc:sqlite:${dbPath}", new java.util.Properties())

    try {
        conn.autoCommit = false
        def pragma = conn.prepareStatement("PRAGMA busy_timeout=30000"); pragma.execute(); pragma.close()

        def del = conn.prepareStatement("DELETE FROM scaffold_mappings WHERE ID = ?")
        del.setString(1, id.toString()); del.executeUpdate(); del.close()

        def lines = new File(csv_fn.toString()).readLines()
        def hi = [:]; lines[0].split(',', -1).eachWithIndex { h, i -> hi[h.trim()] = i }
        def cols = ['ID', 'ref_accession', 'scaffold', 'ref_start', 'ref_end',
                    'strand', 'nmatch', 'qcov', 'qstart', 'mapped']
        def ins = conn.prepareStatement(
            "INSERT OR REPLACE INTO scaffold_mappings (${cols.join(', ')}) " +
            "VALUES (${cols.collect { '?' }.join(', ')})")
        lines.drop(1).each { line ->
            if (line == null || line.length() == 0) return
            def f = line.split(',', -1)
            cols.eachWithIndex { c, k ->
                def v = f[hi[c]]
                if (v == null || v.length() == 0) ins.setNull(k + 1, java.sql.Types.VARCHAR)
                else ins.setString(k + 1, v)
            }
            ins.addBatch()
        }
        ins.executeBatch(); ins.close()
        conn.commit()
    } catch (Exception e) {
        try { conn.rollback() } catch (Exception ignored) {}
        throw new RuntimeException("write_scaffold_mappings failed for ${id} (rolled back): ${e.message}", e)
    } finally {
        conn.close()
    }
}

workflow SCAFFOLD_JOIN {
    take:
        // tuple(id, assembly_fasta, opts_id, auto_join, cov_csvs, ref_seq_file, scaffold_hits)
        input
        // tuple(id, missing_inputs) - join-eligible samples that never reached
        // the join because an upstream step failed for them.
        dropped
        // tuple(id, assemble_opts, join_scaffolds, blast_accession) - samples the
        // app queued for a join-only redo (assemble.join_switch = 1).
        redo

    main:
        // Redo path: rebuild the join's inputs from the PUBLISHED outputs of the
        // run that assembled the sample, never from the assembly channel (which
        // this sample is not in) and never from Nextflow's cache. -resume would
        // probably cache-hit the assembly, but that stops being true the moment
        // work/ is cleaned or the run is launched without -resume, whereas
        // out/<ID>/assemble/<assemble_opts>/ is the pipeline's durable record.
        //
        // assemble_opts doubles as that directory name, so a sample moved to a
        // different option set after it assembled has its output under the OLD
        // name and resolves to nothing here. That is reported below, not skipped.
        redo
            .map { id, opts, join_scaffolds, blast_accession ->
                def dir = "${workflow.launchDir}/${params.publishDir}/${id}/assemble/${opts}"
                def base = file(dir)
                def fastas = []
                def covs = []
                def ref_fa = null
                // Globbing a directory that is not there throws, and a missing
                // directory is precisely the case this has to report, so nothing
                // below runs until the directory is known to exist.
                if (base.exists()) {
                    // assembly_0 is a PREVIOUS join's output; the redo re-joins
                    // the original fragmented assembly, so both globs exclude it.
                    fastas = asFileList(file("${dir}/${id}_assembly_*.fasta"))
                        .findAll { f -> f.name != "${id}_assembly_0.fasta" }
                        .sort { f -> f.name }
                    covs = asFileList(file("${dir}/${id}_assembly_*_coverageStats.csv"))
                        .findAll { f -> f.name != "${id}_assembly_0_coverageStats.csv" }
                        .sort { f -> f.name }
                    // The published reference cache is the per-accession JSON
                    // bundle (blast_ref_sequence.txt never leaves the task work
                    // directory), so the reference FASTA the join wants is
                    // rebuilt from it here.
                    if (blast_accession) {
                        def json = file("${dir}/blast_ref_${blast_accession}/remote_blast_ref.json")
                        if (json.exists()) {
                            def seq = null
                            try {
                                seq = new groovy.json.JsonSlurper().parseText(json.text)?.sequence
                            } catch (Exception e) {
                                seq = null
                            }
                            if (seq) {
                                ref_fa = file("${workflow.workDir}/scaffold_join_redo/${id}_${blast_accession}_ref.fa")
                                ref_fa.parent.mkdirs()
                                ref_fa.text = ">${blast_accession}\n${seq}\n"
                            }
                        }
                    }
                }
                def missing = []
                if (!fastas) missing << 'the assembly FASTA'
                if (!covs)   missing << 'the coverage statistics'
                // No accession is a different fault from an accession whose
                // cached reference is gone: nothing is missing from disk, the
                // sample simply never got a reference to join against.
                if (!ref_fa) {
                    missing << (blast_accession
                        ? 'the cached BLAST reference'
                        : 'a BLAST reference (none was ever selected for this sample)')
                }
                // Empty scaffold_hits: run_scaffold_join() then reads the
                // per-scaffold hits from the assemblies table, which a redo can
                // trust because BLAST committed them in an earlier run.
                tuple(id, fastas ? fastas[0] : null, opts, join_scaffolds,
                      covs, ref_fa, '', missing.join(', '), dir)
            }
            .branch { row ->
                ready:   row[7] == ''
                missing: true
            }
            .set { redo_rows }

        redo_rows.ready
            .map { id, fasta, opts, join_scaffolds, covs, ref_fa, hits, missing, dir ->
                tuple(id, fasta, opts, join_scaffolds, covs, ref_fa, hits) }
            .set { redo_input }

        // Missing published output is reported as a failure with a note, never
        // dropped: a redo request that silently disappears is the same bug this
        // workflow's state writes exist to fix. The write also clears
        // join_switch, so the request does not queue itself forever.
        redo_rows.missing
            .map { id, fasta, opts, join_scaffolds, covs, ref_fa, hits, missing, dir ->
                tuple(params.joinRedoMissingMsg + missing + ' (expected under ' + dir +
                      '). The assembly parameter set may have been renamed or ' +
                      'reassigned, or the output directory moved. Re-run Assembly ' +
                      'for this sample.', id)
            }
            .sqlInsert(statement: params.sqlWriteJoinRedoMissing, db: 'sqlite')

        // One channel from here on, so nothing downstream (the join process, the
        // crash detection, the state writes) knows which route a sample took.
        input.mix(redo_input).set { join_in }

        scaffold_join(join_in)

        // Always-present mappings: clear stale rows and insert the fresh set
        // in one driver-side transaction (see write_scaffold_mappings).
        write_scaffold_mappings(
            scaffold_join.out.mappings.map { id, csv -> tuple(id, csv.toString()) }
        )

        // Joined Path 0 row is emitted only when auto-join built it; the channel
        // is simply empty otherwise, so these branches are no-ops in that case.
        scaffold_join.out.row
            .map { id, row -> row }
            .splitCsv(header: true)
            .set { joined_rows }

        joined_rows
            .map { r -> tuple(r.ID, r.path, r.scaffold, r.topology, r.length, r.length_raw,
                              r.sequence, r.depth, r.gc, r.errors, r.ignore, r.edited, r.time_stamp,
                              r.ID, r.ID, r.ID) }
            .sqlInsert(statement: params.sqlUpsertJoined, db: 'sqlite')

        // path>0 update never touches the path=0 joined row, so order vs the
        // upsert above does not matter. Driven off the (optional) joined row so
        // originals are ignored ONLY when a Path 0 was actually built.
        scaffold_join.out.row
            .map { id, row -> tuple(id) }
            .sqlInsert(statement: params.sqlIgnoreOriginals, db: 'sqlite')

        joined_rows
            .map { r -> tuple(r.ID, r.topology, r.length as Integer,
                              (r.topology == 'circular') ? 'no' : 'yes',
                              r.ID, r.ID, r.ID, params.ts) }
            .sqlInsert(statement: params.sqlSyncAnnotateJoin, db: 'sqlite')

        // Outcome -> state. Mapped by the status VALUE, never by the note text.
        scaffold_join.out.outcome
            .map { id, status_file, note_file ->
                def status = status_file.text.trim()
                def note   = note_file.text.trim()
                // Only a decline carries a reason; clear any stale note otherwise.
                tuple(id, status, (status == 'declined' && note) ? note : null)
            }
            .set { join_outcome }

        // "skipped" is split off because it is the one outcome that carries no
        // verdict about the sample, so it gets the non-destructive write.
        join_outcome
            .branch { id, status, note ->
                skipped: status == 'skipped'
                verdict: true
            }
            .set { outcome_ch }

        outcome_ch.verdict
            .map { id, status, note -> tuple(note, id) }
            .sqlInsert(statement: params.sqlWriteJoinDone, db: 'sqlite')

        outcome_ch.skipped
            .map { id, status, note -> tuple(id) }
            .sqlInsert(statement: params.sqlWriteJoinSkipped, db: 'sqlite')

        // Entered but produced no outcome file: the task crashed (errorStrategy
        // 'ignore' emits nothing). Same idiom as the ref-fetch failure detection.
        join_in
            .map { it -> tuple(it[0], true) }
            .join(join_outcome.map { id, status, note -> tuple(id, true) }, remainder: true)
            .filter { id, entered, reported -> entered != null && reported == null }
            .map    { id, entered, reported -> tuple(params.joinCrashedMsg, id) }
            .sqlInsert(statement: params.sqlWriteJoinFailed, db: 'sqlite')

        // Never reached the join at all: an upstream step failed for the sample.
        // Without this the sample simply vanished from the channel and kept the
        // state 2 that the reference fetch had already written.
        dropped
            .map { id, missing ->
                tuple('Scaffold join did not run. Missing input: ' + missing +
                      '. An earlier step failed for this sample; check its ' +
                      'assemble notes, then re-run.', id)
            }
            .sqlInsert(statement: params.sqlWriteJoinFailed, db: 'sqlite')
}
