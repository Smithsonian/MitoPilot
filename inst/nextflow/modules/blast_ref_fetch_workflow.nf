import groovy.json.JsonSlurper

include {blast_ref_fetch} from './blast_ref_fetch.nf'

// SQL fragment: assemble_notes with any segment starting at `tag` stripped
// (and a preceding '; ' if present). Mirrors the helper in
// blast_genbank_workflow.nf; duplicated here so this module is self-contained.
def stripTagSql(String tag) {
    def lit = tag.replace("'", "''")
    return "RTRIM(" +
        "CASE WHEN INSTR(COALESCE(assemble_notes,''), '${lit}') > 0 " +
            "THEN SUBSTR(COALESCE(assemble_notes,''), 1, INSTR(COALESCE(assemble_notes,''), '${lit}') - 1) " +
            "ELSE COALESCE(assemble_notes,'') END" +
    ", '; ')"
}

def appendTaggedNoteSql(String tag, String msg) {
    def stripped = stripTagSql(tag)
    def tagged = (tag + ' ' + msg).replace("'", "''")
    return "CASE WHEN ${stripped} = '' THEN '${tagged}' ELSE ${stripped} || '; ${tagged}' END"
}

// Strip BOTH [blast] and [ref] tagged segments from assemble_notes. Used on
// the success path so stale failure messages from prior -resume attempts are
// removed when a sample reaches state=2.
def stripBlastAndRefTagsSql() {
    def stripBlast = "RTRIM(" +
        "CASE WHEN INSTR(COALESCE(assemble_notes,''), '[blast]') > 0 " +
            "THEN SUBSTR(COALESCE(assemble_notes,''), 1, INSTR(COALESCE(assemble_notes,''), '[blast]') - 1) " +
            "ELSE COALESCE(assemble_notes,'') END" +
    ", '; ')"
    return "RTRIM(" +
        "CASE WHEN INSTR(${stripBlast}, '[ref]') > 0 " +
            "THEN SUBSTR(${stripBlast}, 1, INSTR(${stripBlast}, '[ref]') - 1) " +
            "ELSE ${stripBlast} END" +
    ", '; ')"
}

params.refFetchFailedMsg = 'BLAST reference fetch timed out after all retries. Rerun pipeline with -resume to retry.'

params.sqlWriteBlastLineage = 'UPDATE assemble SET blast_lineage = ? WHERE ID = ?'
// Per-scaffold lineage: matched on accession so each scaffold row inherits the
// lineage of whichever ref was fetched for its BLAST hit. Relies on the
// blast_records writer in BLAST_GENBANK having already populated
// assemblies.blast_accession; with SQLite sqlInsert being synchronous and
// remote NCBI EFetch dominating wall time, this ordering is reliable in practice.
params.sqlWriteBlastLineageScaffold = 'UPDATE assemblies SET blast_lineage = ? WHERE ID = ? AND blast_accession = ?'

params.sqlWriteBlastRef = '''INSERT OR REPLACE INTO blast_ref_annotations
    (ID, gene, type, pos1, pos2, direction, ref_length, time_stamp)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?)'''

params.sqlWriteRefSeq = '''INSERT OR REPLACE INTO blast_ref_sequences
    (accession, sequence, ref_length, genetic_code, time_stamp)
    VALUES (?, ?, ?, ?, ?)'''

// Written when the top-hit ref fetch fails after all retries (NCBI timeout / transient error).
// blast_ref_fetch uses errorStrategy 'ignore' so the pipeline keeps running for other samples.
// 'ignore' does NOT cache the failure as a successful task, so -resume will retry the step.
// Guarded WHERE assemble_switch = 4 so terminal state=3 rows cannot be touched.
// The [ref] message is embedded; the UPDATE strips any prior [ref] segment then appends.
params.sqlWriteBlastRefFetchFailed = "UPDATE assemble SET " +
    "assemble_switch = 3, " +
    "assemble_notes = ${appendTaggedNoteSql('[ref]', params.refFetchFailedMsg)}, " +
    "poor_blast_ref = 'failed' " +
    "WHERE ID = ? AND assemble_switch = 4"

// Mark state=2 (WF1 complete) and poor_blast_ref = 'good' when the top-hit ref fetch + parse succeeds.
// Strips any [blast]/[ref] stale failure segments from prior -resume attempts; preserves
// assembly-stage warnings (e.g. 'Output contains disconnected contigs').
// Guarded WHERE assemble_switch = 4 so terminal state=3 rows cannot be touched.
params.sqlWriteBlastRefGood = "UPDATE assemble SET " +
    "assemble_switch = 2, " +
    "poor_blast_ref = 'good', " +
    "assemble_notes = ${stripBlastAndRefTagsSql()} " +
    "WHERE ID = ? AND assemble_switch = 4"

workflow BLAST_REF_FETCH {
    take:
        // input: tuple(id, blast_accession, blast_species, blast_evalue, opts_id, is_top)
        input

    main:
        // Track top-hit IDs entering the workflow so failures can be detected below
        input
            .filter { id, accession, species, evalue, opts_id, is_top -> is_top }
            .map    { id, accession, species, evalue, opts_id, is_top -> tuple(id, true) }
            .set { all_top_ids }

        blast_ref_fetch(input).set { ref_out }
        // ref_out: tuple(id, accession, is_top, csv_file, seq_file, gc_file, json_file)

        // Parse annotations CSV; only write for the per-ID top hit so the
        // per-ID-keyed blast_ref_annotations table stays unambiguous downstream.
        ref_out
            .filter { id, accession, is_top, csv_file, seq_file, gc_file, json_file -> is_top }
            .flatMap { id, accession, is_top, csv_file, seq_file, gc_file, json_file ->
                def rows = []
                def lines = csv_file.readLines()
                if (lines.size() <= 1) return rows   // empty or header-only
                def ts = java.time.Instant.now().getEpochSecond()
                lines.drop(1).each { line ->
                    if (line.trim()) {
                        // CSV columns: gene,type,pos1,pos2,direction,ref_length
                        // Strip surrounding quotes written by R's write.csv
                        def parts = line.split(',').collect { it.replaceAll('^"|"$', '') }
                        if (parts.size() >= 6 && parts[2].isLong() && parts[3].isLong() && parts[5].isLong()) {
                            rows << tuple(
                                id,
                                parts[0],              // gene
                                parts[1],              // type
                                parts[2].toLong(),     // pos1
                                parts[3].toLong(),     // pos2
                                parts[4],              // direction
                                parts[5].toLong(),     // ref_length
                                ts                     // time_stamp
                            )
                        }
                    }
                }
                rows
            }
            .sqlInsert(statement: params.sqlWriteBlastRef, db: 'sqlite')

        // Store reference nucleotide sequence (one row per accession; dedup by PK)
        ref_out
            .map { id, accession, is_top, csv_file, seq_file, gc_file, json_file ->
                def seq = seq_file.text.trim()
                if (!seq) return null
                def gc_str = gc_file.text.trim()
                def gc = gc_str.isInteger() ? gc_str.toInteger() : 2
                def ts = java.time.Instant.now().getEpochSecond()
                tuple(accession, seq, seq.length() as Long, gc, ts)
            }
            .filter { it != null }
            .sqlInsert(statement: params.sqlWriteRefSeq, db: 'sqlite')

        // Lineage: assemble (per-ID) row gets the top-hit lineage; assemblies
        // (per-scaffold) rows get lineage matched on accession so every scaffold
        // hit inherits the right lineage even when multiple accessions are fetched
        // per sample.
        ref_out
            .map { id, accession, is_top, csv_file, seq_file, gc_file, json_file ->
                def json = new JsonSlurper().parse(json_file)
                def lineage = json?.lineage ?: null
                lineage ? tuple(id, accession, is_top, lineage) : null
            }
            .filter { it != null }
            .set { lineage_records }

        lineage_records
            .filter { id, accession, is_top, lineage -> is_top }
            .map    { id, accession, is_top, lineage -> tuple(lineage, id) }
            .sqlInsert(statement: params.sqlWriteBlastLineage, db: 'sqlite')

        lineage_records
            .map { id, accession, is_top, lineage -> tuple(lineage, id, accession) }
            .sqlInsert(statement: params.sqlWriteBlastLineageScaffold, db: 'sqlite')

        // Detect top-hit fetch failures: IDs that entered but produced no output after all retries
        ref_out
            .filter { id, accession, is_top, csv_file, seq_file, gc_file, json_file -> is_top }
            .map    { id, accession, is_top, csv_file, seq_file, gc_file, json_file -> tuple(id, true) }
            .set { succeeded_top_ids }

        // Mark poor_blast_ref = 'good' for IDs whose top-hit ref fetch succeeded
        ref_out
            .filter { id, accession, is_top, csv_file, seq_file, gc_file, json_file -> is_top }
            .map    { id, accession, is_top, csv_file, seq_file, gc_file, json_file -> tuple(id) }
            .sqlInsert(statement: params.sqlWriteBlastRefGood, db: 'sqlite')

        // Failure message is embedded in params.sqlWriteBlastRefFetchFailed (with [ref] tag)
        // and the UPDATE is guarded WHERE assemble_switch = 4.
        all_top_ids
            .join(succeeded_top_ids, remainder: true)
            .filter { id, all_flag, success_flag -> success_flag == null }
            .map    { id, all_flag, success_flag -> tuple(id) }
            .sqlInsert(statement: params.sqlWriteBlastRefFetchFailed, db: 'sqlite')
}
