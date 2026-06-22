process validate {

    executor params.curate.executor
    container params.curate.container
    clusterOptions {
        def opts = [
            (params.curate.executor == 'sge') ? '-S /bin/bash' : '',
            (params.curate.clusterOptions instanceof String) ? params.curate.clusterOptions : ''
        ].findAll { it }.join(' ')
        opts ?: null
    }

    publishDir "${launchDir}/${params.publishDir}", overwrite: true, mode: 'copy'

    errorStrategy 'ignore'

    tag "${id}"

    input:
    tuple val(id), val(path), path(annotations), path(coverage), val(opts)

    output:
    tuple val(id), val(path), path("${id}/annotate/${id}_annotations_*.tsv"), path("${id}/annotate/${id}_summary_*.csv"), path("${id}/annotate/NF_work_dir_validate.txt")

    shell:
    dir = "${id}/annotate"
    '''
    export OMP_NUM_THREADS=1 # fix for OpenBLAS blas_thread_init error
    mkdir -p !{dir}
    Rscript -e "MitoPilot::validate_!{opts.target}( \
        annotations_fn = '!{annotations}', \
        coverage_fn = '!{coverage}', \
        params = '!{opts.params}', \
        out_dir = '!{dir}'
    )"
    ### work dir info for troubleshooting ####
    echo "Nextflow validate working directory:" > !{dir}/NF_work_dir_validate.txt
    echo "$PWD" >> !{dir}/NF_work_dir_validate.txt
    '''
}

// Single, serialized, transactional commit of the curated + validated result for
// one (id, path). Native exec process: the body runs in the Nextflow driver JVM
// on the head node (no executor task, no container), the same locus where the
// nf-sqldb operators already write the project .sqlite reliably over NFS. One
// JDBC transaction writes the assembly sequence (assemblies), the validated
// annotation coordinates (annotations), and the summary (annotate) atomically;
// any row-count mismatch or error rolls back ALL of it and fails the task, so a
// sample is never left half-written / desynced. maxForks 1 makes it the single
// such writer at a time. errorStrategy 'ignore' drops a failed sample so the
// gated downstream stages skip it.
//
// The SQLite JDBC driver is loaded from inst/nextflow/lib/ (auto-added to the
// main classpath). We instantiate the driver and call .connect() directly rather
// than via DriverManager, whose caller-classloader filtering does not see jars
// loaded from lib/ ("No suitable driver found").
process write_curated_result {

    maxForks 1
    errorStrategy 'ignore'
    tag "${id}"

    input:
    tuple val(id), val(path), val(coverage_fn), val(annotations_fn), val(summary_fn), val(assembly_fn)

    output:
    tuple val(id), val(path)

    exec:
    def ts = params.ts as String
    def dbPath = "${workflow.launchDir}/.sqlite"
    def drv = Class.forName("org.sqlite.JDBC").getDeclaredConstructor().newInstance()
    def conn = drv.connect("jdbc:sqlite:${dbPath}", new java.util.Properties())

    // set a string-or-NULL parameter (lets SQLite column affinity coerce numeric
    // strings into INTEGER columns; empty -> NULL rather than the text "")
    def setStr = { java.sql.PreparedStatement st, int idx, Object v ->
        if (v == null || v.toString().length() == 0) st.setNull(idx, java.sql.Types.VARCHAR)
        else st.setString(idx, v.toString())
    }

    try {
        conn.autoCommit = false
        def pragma = conn.prepareStatement("PRAGMA busy_timeout=30000"); pragma.execute(); pragma.close()

        // ---- assemblies: reconstruct per-scaffold sequence from coverageStats ----
        def covLines = new File(coverage_fn.toString()).readLines()
        def ch = covLines[0].split(',', -1); def ci = [:]; ch.eachWithIndex { h, i -> ci[h.trim()] = i }
        def groups = [:]  // SeqId -> rows
        covLines.drop(1).each { line ->
            if (!line) return
            def f = line.split(',', -1)
            (groups[f[ci['SeqId']]] = (groups[f[ci['SeqId']]] ?: [])) << f
        }
        def updAsm = conn.prepareStatement(
            "UPDATE assemblies SET sequence = ?, length = ?, depth = ?, gc = ?, errors = ?, " +
            "time_stamp = ? WHERE ID = ? AND path = ? AND scaffold = ?")
        groups.each { sid, rows ->
            rows.sort { a, b -> (a[ci['Position']] as int) <=> (b[ci['Position']] as int) }
            def seq    = rows.collect { it[ci['Call']] }.join('')
            def depth  = rows.collect { it[ci['MeanDepth']] }.join(' ')
            def gc     = rows.collect { it[ci['GC']] }.join(' ')
            def errors = rows.collect { it[ci['ErrorRate']] }.join(' ')
            def key = sid.split('\\.')  // ID.path.scaffold
            updAsm.setString(1, seq); updAsm.setInt(2, seq.length())
            updAsm.setString(3, depth); updAsm.setString(4, gc); updAsm.setString(5, errors)
            updAsm.setString(6, ts)
            updAsm.setString(7, key[0]); updAsm.setInt(8, key[1] as int); updAsm.setInt(9, key[2] as int)
            def n = updAsm.executeUpdate()
            if (n != 1) throw new RuntimeException("assemblies UPDATE matched ${n} rows (expected 1) for SeqId '${sid}'")
        }
        updAsm.close()

        // ---- annotations: clear stale rows, insert validated coordinates ----
        def del = conn.prepareStatement("DELETE FROM annotations WHERE ID = ? AND path = ? AND time_stamp != ?")
        del.setString(1, id.toString()); del.setInt(2, path as int); del.setString(3, ts)
        del.executeUpdate(); del.close()

        def annLines = new File(annotations_fn.toString()).readLines()
        def ah = annLines[0].split('\t', -1); def ai = [:]; ah.eachWithIndex { h, i -> ai[h.trim()] = i }
        def insCols = ['ID','path','scaffold','type','gene','product','pos1','pos2','length','direction',
                       'start_codon','stop_codon','anticodon','tool','notes','warnings','translation','refHits',
                       'partial_start','partial_stop','time_stamp','edited']
        def ins = conn.prepareStatement(
            "INSERT OR REPLACE INTO annotations (${insCols.join(', ')}) " +
            "VALUES (${insCols.collect { '?' }.join(', ')})")
        // columns sourced from the TSV (contig is split into ID/path/scaffold)
        def tsvCols = ['type','gene','product','pos1','pos2','length','direction','start_codon','stop_codon',
                       'anticodon','tool','notes','warnings','translation','refHits','partial_start','partial_stop']
        annLines.drop(1).each { line ->
            if (line == null || line.length() == 0) return
            def f = line.split('\t', -1)
            def contig = f[ai['contig']].split('\\.')  // ID, path, scaffold
            setStr(ins, 1, contig[0]); setStr(ins, 2, contig[1]); setStr(ins, 3, contig[2])
            tsvCols.eachWithIndex { c, k ->
                setStr(ins, 4 + k, ai.containsKey(c) ? f[ai[c]] : null)
            }
            setStr(ins, 21, ts)           // time_stamp
            ins.setInt(22, 0)             // edited
            ins.addBatch()
        }
        ins.executeBatch(); ins.close()

        // ---- annotate: summary (from TSV) + assembly stats (from FASTA) ----
        def descs = []; def lengths = []; def cur = new StringBuilder(); def started = false
        new File(assembly_fn.toString()).readLines().each { l ->
            if (l.startsWith('>')) {
                if (started) lengths << cur.length()
                descs << l.substring(1); cur = new StringBuilder(); started = true
            } else { cur.append(l.trim()) }
        }
        if (started) lengths << cur.length()
        def scaffolds = descs.size()
        def totLen = (lengths.sum() ?: 0) as int
        def topology = descs.collect { d -> def p = d.split(/\s+/, 2); p.length > 1 ? p[1] : '' }.join(';')

        // summary is CSV; its fields never contain commas (structure joins with
        // '|', missing/extra with ';', the rest are integers)
        def sumLines = new File(summary_fn.toString()).readLines()
        def sh = sumLines[0].split(',', -1); def si = [:]; sh.eachWithIndex { h, i -> si[h.trim()] = i }
        def sr = sumLines[1].split(',', -1)
        def g = { c -> si.containsKey(c) ? sr[si[c]] : null }

        // Auto-set the partial flag from topology: linear -> "yes",
        // circular -> "no", unless the curation "complete mitogenomes are
        // linear" option (curate_opts.linear_complete) is set, which makes
        // linear assemblies complete ("no").
        def linComplete = false
        try {
            def lq = conn.prepareStatement(
                "SELECT d.linear_complete FROM annotate c " +
                "JOIN curate_opts d ON c.curate_opts = d.curate_opts WHERE c.ID = ?")
            lq.setString(1, id.toString())
            def lrs = lq.executeQuery()
            if (lrs.next()) linComplete = (lrs.getInt(1) == 1)
            lrs.close(); lq.close()
        } catch (Exception ignored) {}
        def partial = (topology == 'circular' || linComplete) ? 'no' : 'yes'

        // Drop auto-generated "EDITED:" notes (e.g. the manual linearization
        // note) now that WF2 has regenerated the assembly; keep user-typed notes
        // (the other "; "-delimited segments). NULL when nothing remains.
        String cleanedNotes = null
        try {
            def nq = conn.prepareStatement("SELECT annotate_notes FROM annotate WHERE ID = ?")
            nq.setString(1, id.toString())
            def nrs = nq.executeQuery()
            if (nrs.next()) {
                def raw = nrs.getString(1)
                if (raw != null) {
                    def kept = raw.split('; ', -1).findAll { !it.trim().startsWith('EDITED:') }
                    cleanedNotes = kept.join('; ')
                    if (cleanedNotes.trim().length() == 0) cleanedNotes = null
                }
            }
            nrs.close(); nq.close()
        } catch (Exception ignored) {}

        def updAnn = conn.prepareStatement(
            "UPDATE annotate SET path = ?, scaffolds = ?, topology = ?, length = ?, structure = ?, " +
            "PCGCount = ?, tRNACount = ?, rRNACount = ?, missing = ?, extra = ?, warnings = ?, " +
            "partial = ?, annotate_notes = ?, annotate_switch = 2, time_stamp = ? WHERE ID = ?")
        updAnn.setInt(1, path as int)
        updAnn.setInt(2, scaffolds)
        setStr(updAnn, 3, topology)
        updAnn.setInt(4, totLen)
        setStr(updAnn, 5, g('structure'))
        setStr(updAnn, 6, g('PCGCount'))
        setStr(updAnn, 7, g('tRNACount'))
        setStr(updAnn, 8, g('rRNACount'))
        setStr(updAnn, 9, g('missing'))
        setStr(updAnn, 10, g('extra'))
        setStr(updAnn, 11, g('warnings'))
        updAnn.setString(12, partial)
        setStr(updAnn, 13, cleanedNotes)
        updAnn.setString(14, ts)
        updAnn.setString(15, id.toString())
        def na = updAnn.executeUpdate()
        if (na != 1) throw new RuntimeException("annotate UPDATE matched ${na} rows (expected 1) for ID '${id}'")
        updAnn.close()

        conn.commit()
    } catch (Exception e) {
        try { conn.rollback() } catch (Exception ignored) {}
        throw new RuntimeException("write_curated_result failed for ${id} (rolled back): ${e.message}", e)
    } finally {
        conn.close()
    }
}
