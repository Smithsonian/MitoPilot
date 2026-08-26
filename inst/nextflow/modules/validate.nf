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
    tuple val(id), val(path), val(scaffold), path(annotations), path(coverage), val(opts)

    output:
    tuple val(id), val(path), val(scaffold), path("${id}/annotate/${id}_annotations_*.tsv"), path("${id}/annotate/${id}_summary_*.csv"), path("${id}/annotate/NF_work_dir_validate.txt")

    shell:
    dir = "${id}/annotate"
    scafArg = "${scaffold}"
    outCsv  = "${id}_annotations_${path}.${scaffold}.csv"
    '''
    export OMP_NUM_THREADS=1 # fix for OpenBLAS blas_thread_init error
    mkdir -p !{dir}
    # Subset the per-path curated annotations to this unit, so gene-count
    # validation never pools separate genomes. The output filename propagates to
    # the validate outputs (annotations tsv + summary csv).
    Rscript -e "ann <- utils::read.csv('!{annotations}'); sc <- '!{scafArg}'; keep <- sub('^.*[.]', '', as.character(ann[['contig']])) == sc; utils::write.csv(ann[keep, , drop = FALSE], '!{outCsv}', row.names = FALSE)"
    Rscript -e "MitoPilot::validate_!{opts.target}( \
        annotations_fn = '!{outCsv}', \
        coverage_fn = '!{coverage}', \
        params = '!{opts.params}', \
        out_dir = '!{dir}'
    )"
    ### work dir info for troubleshooting ####
    echo "Nextflow validate working directory:" > !{dir}/NF_work_dir_validate.txt
    echo "$PWD" >> !{dir}/NF_work_dir_validate.txt
    '''
}

// Single serialized transactional commit of the curated + validated result for one
// (id, path). Native exec: runs in the Nextflow driver JVM (no executor/container),
// where nf-sqldb already writes the .sqlite reliably over NFS. One JDBC transaction
// writes assemblies + annotations + annotate atomically; any row-count mismatch or
// error rolls back all of it and fails the task, so a sample is never left
// half-written. maxForks 1 = single writer; errorStrategy 'ignore' drops a failure.
//
// org.sqlite.JDBC is already loaded by the nf-sqldb plugin (a hard dependency), so we
// resolve it from that plugin's classloader and call driver.connect() directly
// (DriverManager's caller-classloader filtering wouldn't see a plugin class).
process write_curated_result {

    // Native (exec) task: writes the .sqlite driver-side via JDBC. Pin to the
    // local executor so it isn't routed to sge/slurm (which can't run native
    // tasks and emits a "cannot be executed by ... Using 'local' instead" warn).
    executor 'local'
    maxForks 1
    errorStrategy 'ignore'
    tag "${id}"

    input:
    tuple val(id), val(path), val(scaffold), val(coverage_fn), val(annotations_fn), val(summary_fn), val(assembly_fn)

    output:
    tuple val(id), val(path), val(scaffold)

    exec:
    def ts = params.ts as String
    def dbPath = "${workflow.launchDir}/.sqlite"
    // Resolve org.sqlite.JDBC from the nf-sqldb plugin classloader, falling back
    // to the app classpath.
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
        // This writer runs per (id, path, scaffold); the curate coverage is
        // per-path, so update only this unit's scaffold row.
        def targetSid = "${id}.${path}.${scaffold}".toString()
        def updAsm = conn.prepareStatement(
            "UPDATE assemblies SET sequence = ?, length = ?, depth = ?, gc = ?, errors = ?, " +
            "time_stamp = ? WHERE ID = ? AND path = ? AND scaffold = ?")
        groups.each { sid, rows ->
            if (sid != targetSid) return  // only this unit's row
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
        // Scoped to this scaffold unit so a sibling unit of the same path is not
        // wiped (the validated TSV holds only this scaffold's rows).
        def del = conn.prepareStatement(
            "DELETE FROM annotations WHERE ID = ? AND path = ? AND scaffold = ? AND time_stamp != ?")
        del.setString(1, id.toString()); del.setInt(2, path as int); del.setInt(3, scaffold as int); del.setString(4, ts)
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
        // Restrict the summary to this scaffold unit (assembly FASTA is per-path).
        def selIdx = (0..<descs.size()).findAll { descs[it].split(/\s+/, 2)[0] == targetSid }
        def scaffolds = selIdx.size()
        def totLen = (selIdx.collect { lengths[it] }.sum() ?: 0) as int
        // A unit is normally one record with one topology. If it ever spans
        // several with mixed topologies, collapse to the 'fragmented' sentinel
        // rather than joining values with ';': the joined string reads as a
        // topology downstream and would reach a submission defline.
        def topoVals = selIdx.collect { def p = descs[it].split(/\s+/, 2); p.length > 1 ? p[1] : '' }
                             .findAll { it.length() > 0 }.unique()
        def topology = topoVals.size() == 1 ? topoVals[0] : (topoVals.size() > 1 ? 'fragmented' : '')

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
                "JOIN curate_opts d ON c.curate_opts = d.curate_opts " +
                "WHERE c.ID = ? AND c.path = ? AND c.scaffold = ?")
            lq.setString(1, id.toString()); lq.setInt(2, path as int); lq.setInt(3, scaffold as int)
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
            def nq = conn.prepareStatement("SELECT annotate_notes FROM annotate WHERE ID = ? AND path = ? AND scaffold = ?")
            nq.setString(1, id.toString()); nq.setInt(2, path as int); nq.setInt(3, scaffold as int)
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

        // Per-unit summary write; keyed by (ID, path, scaffold) so sibling units
        // of the same path are not clobbered (path/scaffold are the key, not SET).
        def updAnn = conn.prepareStatement(
            "UPDATE annotate SET scaffolds = ?, topology = ?, length = ?, structure = ?, " +
            "PCGCount = ?, tRNACount = ?, rRNACount = ?, missing = ?, extra = ?, warnings = ?, " +
            "partial = ?, annotate_notes = ?, annotate_switch = 2, time_stamp = ? " +
            "WHERE ID = ? AND path = ? AND scaffold = ?")
        updAnn.setInt(1, scaffolds)
        setStr(updAnn, 2, topology)
        updAnn.setInt(3, totLen)
        setStr(updAnn, 4, g('structure'))
        setStr(updAnn, 5, g('PCGCount'))
        setStr(updAnn, 6, g('tRNACount'))
        setStr(updAnn, 7, g('rRNACount'))
        setStr(updAnn, 8, g('missing'))
        setStr(updAnn, 9, g('extra'))
        setStr(updAnn, 10, g('warnings'))
        updAnn.setString(11, partial)
        setStr(updAnn, 12, cleanedNotes)
        updAnn.setString(13, ts)
        updAnn.setString(14, id.toString())
        updAnn.setInt(15, path as int)
        updAnn.setInt(16, scaffold as int)
        def na = updAnn.executeUpdate()
        if (na != 1) throw new RuntimeException("annotate UPDATE matched ${na} rows (expected 1) for unit '${id}.${path}.${scaffold}'")
        updAnn.close()

        conn.commit()
    } catch (Exception e) {
        try { conn.rollback() } catch (Exception ignored) {}
        throw new RuntimeException("write_curated_result failed for ${id} (rolled back): ${e.message}", e)
    } finally {
        conn.close()
    }
}
