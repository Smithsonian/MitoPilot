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
//   joined / skipped -> state 2, no note (a stale note from an earlier run is
//                       cleared, not left to mislead).
//   declined         -> state 2, note explaining why nothing was joined.
//   crashed / dropped-> state 3, note naming what went wrong.
// join_switch is cleared either way so a queued redo does not repeat forever.
// No assemble_switch guard: the samples reaching these writes were withheld at 4
// by BLAST_REF_FETCH, and a redo deliberately re-reports a sample at 2 or 3.
params.sqlWriteJoinDone   = 'UPDATE assemble SET assemble_switch = 2, join_notes = ?, join_switch = NULL WHERE ID = ?'
params.sqlWriteJoinFailed = 'UPDATE assemble SET assemble_switch = 3, join_notes = ?, join_switch = NULL WHERE ID = ?'

params.joinCrashedMsg = "Scaffold join failed. The task produced no outcome file, so it died rather than declining to join. Check .command.log in the task's work directory (empty except for a signal message means the scheduler killed it: exit 137 = OOM, exit 140 = SGE runtime limit). To retry, re-run the scaffold join for this sample."

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

    main:
        scaffold_join(input)

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
                tuple(id, (status == 'declined' && note) ? note : null)
            }
            .set { join_outcome }

        join_outcome
            .map { id, note -> tuple(note, id) }
            .sqlInsert(statement: params.sqlWriteJoinDone, db: 'sqlite')

        // Entered but produced no outcome file: the task crashed (errorStrategy
        // 'ignore' emits nothing). Same idiom as the ref-fetch failure detection.
        input
            .map { it -> tuple(it[0], true) }
            .join(join_outcome.map { id, note -> tuple(id, true) }, remainder: true)
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
