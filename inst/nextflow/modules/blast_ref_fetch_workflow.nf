import groovy.json.JsonSlurper

include {blast_ref_fetch; blast_ref_stamp} from './blast_ref_fetch.nf'

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

params.refFetchFailedMsg = "BLAST reference fetch failed. Common causes: an NCBI connection or rate-limit problem, or, on a cluster, the scheduler killing the task for exceeding a resource limit (exit 140 = SIGUSR2 from SGE, exit 137 = OOM kill; check .command.log in the task's work directory, which is empty except for a signal message in that case). To retry, set this sample back to 'Ready to Assemble' (State button) and re-run the pipeline."

params.sqlWriteBlastLineage = 'UPDATE assemble SET blast_lineage = ? WHERE ID = ?'
// Per-scaffold lineage: keyed on (ID, path, scaffold). The scaffold->accession
// map comes from BLAST_GENBANK in-memory, so we don't read the deferred-commit
// assemblies.blast_accession column. Each scaffold gets the lineage of the ref
// fetched for ITS OWN accession (not the sample top hit), so multi-scaffold
// samples with hits to different taxa keep distinct per-scaffold lineages.
params.sqlWriteBlastLineageScaffold = 'UPDATE assemblies SET blast_lineage = ? WHERE ID = ? AND path = ? AND scaffold = ?'

params.sqlWriteBlastRef = '''INSERT OR REPLACE INTO blast_ref_annotations
    (accession, gene, type, pos1, pos2, direction, ref_length, time_stamp)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?)'''

params.sqlWriteRefSeq = '''INSERT OR REPLACE INTO blast_ref_sequences
    (accession, sequence, ref_length, genetic_code, lineage, topology, time_stamp)
    VALUES (?, ?, ?, ?, ?, ?, ?)'''

// Written when the top-hit ref fetch fails after all retries. Guarded WHERE
// assemble_switch = 4 so terminal state=3 rows are untouched. Strips any prior
// [ref] note segment then appends the new one. (-resume retries the fetch.)
params.sqlWriteBlastRefFetchFailed = "UPDATE assemble SET " +
    "assemble_switch = 3, " +
    "assemble_notes = ${appendTaggedNoteSql('[ref]', params.refFetchFailedMsg)}, " +
    "poor_blast_ref = 'failed' " +
    "WHERE ID = ? AND assemble_switch = 4"

// Mark state=2 (WF1 complete), poor_blast_ref='good' on top-hit ref fetch success.
// Strips stale [blast]/[ref] failure note segments but preserves assembly warnings
// (e.g. 'Output contains disconnected contigs'). Guarded WHERE assemble_switch = 4.
params.sqlWriteBlastRefGood = "UPDATE assemble SET " +
    "assemble_switch = 2, " +
    "poor_blast_ref = 'good', " +
    "assemble_notes = ${stripBlastAndRefTagsSql()} " +
    "WHERE ID = ? AND assemble_switch = 4"

// Same, minus the state promotion, for IDs withheld because a later step (the
// scaffold join) owns their final state. Same guard, so a terminal state=3 row
// is untouched.
params.sqlWriteBlastRefGoodHeld = "UPDATE assemble SET " +
    "poor_blast_ref = 'good', " +
    "assemble_notes = ${stripBlastAndRefTagsSql()} " +
    "WHERE ID = ? AND assemble_switch = 4"

workflow BLAST_REF_FETCH {
    take:
        // input: tuple(id, blast_accession, blast_species, blast_evalue, opts_id, is_top)
        input
        // scaffold_map: tuple(id, path, scaffold, accession) for every real scaffold hit
        scaffold_map
        // ref_batches: tuple(id, opts_id, [accessions]) - one per-sample fetch batch
        ref_batches
        // withhold_ids: IDs whose state 2 promotion is owned by a later step (the
        // scaffold join). They stay at 4 here so the join can report the real
        // outcome. Pass channel.empty() when nothing follows the fetch.
        withhold_ids

    main:
        // Track top-hit IDs entering the workflow so failures can be detected below
        input
            .filter { id, accession, species, evalue, opts_id, is_top -> is_top }
            .map    { id, accession, species, evalue, opts_id, is_top -> tuple(id, true) }
            .set { all_top_ids }

        // Per-sample reference fetch. ref_batches is (id, opts_id, [accessions]),
        // streaming per sample from BLAST_GENBANK, so a sample's fetch starts as
        // soon as ITS blast finishes and re-runs independently. Accessions are
        // deduped WITHIN a sample; a reference shared by two samples is fetched
        // once per sample (idempotent DB writes) - the trade for per-sample rerun.
        // Each task emits its per-accession ref_<accession>/ dirs, keyed by
        // (id, accession) so the stamp step fans back to the right sample.
        blast_ref_fetch(ref_batches)
            .flatMap { id, opts_id, dirs ->
                def list = (dirs == null) ? [] : ((dirs instanceof List) ? dirs : [dirs])
                list.findAll { it != null }.collect { d ->
                    def acc = d.name.replaceFirst(/^ref_/, '')
                    tuple(tuple(id, acc),
                          file("${d}/blast_ref_annotations.csv"),
                          file("${d}/blast_ref_sequence.txt"),
                          file("${d}/blast_ref_genetic_code.txt"),
                          file("${d}/remote_blast_ref.json"))
                }
            }
            .set { fetched }
        // fetched: tuple([id, accession], csv_file, seq_file, gc_file, json_file)

        // Join each sample's fetched files back to its per-(sample, accession) BLAST
        // metadata (species, evalue, is_top) for blast_ref_stamp.
        input
            .map { id, accession, species, evalue, opts_id, is_top ->
                tuple(tuple(id, accession), species, evalue, opts_id, is_top) }
            .combine(fetched, by: 0)
            .map { key, species, evalue, opts_id, is_top, csv_file, seq_file, gc_file, json_file ->
                tuple(key[0], key[1], species, evalue, opts_id, is_top, csv_file, seq_file, gc_file, json_file) }
            .set { stamp_input }

        blast_ref_stamp(stamp_input).set { ref_out }
        // ref_out: tuple(id, accession, is_top, csv_file, seq_file, gc_file, json_file)

        // Parse annotations CSV for EVERY fetched accession (not just the top hit),
        // keyed by accession. The picker in the annotate-details synteny plot loads
        // annotations for whichever candidate the user selects. Writes for the same
        // accession across samples are idempotent (INSERT OR REPLACE by accession).
        ref_out
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
                                accession,
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

        // Store reference nucleotide sequence + per-accession lineage (one row per
        // accession; dedup by PK). Lineage is read from the same JSON the fetch
        // wrote, so every candidate accession keeps its lineage for the app's
        // "All BLAST Hits" view (not just the top / per-scaffold hits).
        ref_out
            .map { id, accession, is_top, csv_file, seq_file, gc_file, json_file ->
                def seq = seq_file.text.trim()
                if (!seq) return null
                def gc_str = gc_file.text.trim()
                def gc = gc_str.isInteger() ? gc_str.toInteger() : 2
                def lineage = null
                def topology = null
                try {
                    def j = new JsonSlurper().parse(json_file)
                    lineage = j?.lineage ?: null
                    topology = j?.topology ?: null
                } catch (ignored) { lineage = null; topology = null }
                def ts = java.time.Instant.now().getEpochSecond()
                tuple(accession, seq, seq.length() as Long, gc, lineage, topology, ts)
            }
            .filter { it != null }
            .sqlInsert(statement: params.sqlWriteRefSeq, db: 'sqlite')

        // Lineage: assemble (per-ID) row gets the top-hit lineage; assemblies
        // (per-scaffold) rows get the lineage of their OWN accession via an
        // in-memory join with scaffold_map, so multi-scaffold samples whose
        // scaffolds hit different taxa keep distinct per-scaffold lineages.
        ref_out
            .map { id, accession, is_top, csv_file, seq_file, gc_file, json_file ->
                def lineage = null
                try {
                    def json = new JsonSlurper().parse(json_file)
                    lineage = json?.lineage ?: null
                } catch (ignored) { lineage = null }
                lineage ? tuple(id, accession, is_top, lineage) : null
            }
            .filter { it != null }
            .set { lineage_records }

        lineage_records
            .filter { id, accession, is_top, lineage -> is_top }
            .map    { id, accession, is_top, lineage -> tuple(lineage, id) }
            .sqlInsert(statement: params.sqlWriteBlastLineage, db: 'sqlite')

        // Per-scaffold: join each accession's lineage to every scaffold that hit it
        // (top AND dup), keyed on (id, accession). combine(by:0) fans the single
        // lineage out across all matching scaffolds.
        lineage_records
            .map { id, accession, is_top, lineage -> tuple(tuple(id, accession), lineage) }
            .combine(
                scaffold_map.map { id, path, scaffold, accession -> tuple(tuple(id, accession), path, scaffold) },
                by: 0
            )
            .map { key, lineage, path, scaffold -> tuple(lineage, key[0], path, scaffold) }
            .sqlInsert(statement: params.sqlWriteBlastLineageScaffold, db: 'sqlite')

        // Detect top-hit fetch failures: IDs that entered but produced no output after all retries
        ref_out
            .filter { id, accession, is_top, csv_file, seq_file, gc_file, json_file -> is_top }
            .map    { id, accession, is_top, csv_file, seq_file, gc_file, json_file -> tuple(id, true) }
            .set { succeeded_top_ids }

        // Mark poor_blast_ref = 'good' for IDs whose top-hit ref fetch succeeded.
        // Withheld IDs are skipped: for them WF1 is not finished when the
        // reference lands, and promoting here is what used to report a failed
        // scaffold join as a success.
        succeeded_top_ids
            .join(withhold_ids.map { id -> tuple(id, true) }, remainder: true)
            .filter { id, success_flag, withhold_flag -> success_flag != null }
            .branch { id, success_flag, withhold_flag ->
                held:  withhold_flag != null
                ready: true
            }
            .set { ref_good }

        ref_good.ready
            .map { id, success_flag, withhold_flag -> tuple(id) }
            .sqlInsert(statement: params.sqlWriteBlastRefGood, db: 'sqlite')

        // Withheld IDs still get poor_blast_ref and the note cleanup; only the
        // state promotion is left to the step that owns it.
        ref_good.held
            .map { id, success_flag, withhold_flag -> tuple(id) }
            .sqlInsert(statement: params.sqlWriteBlastRefGoodHeld, db: 'sqlite')

        // Failure message is embedded in params.sqlWriteBlastRefFetchFailed (with [ref] tag)
        // and the UPDATE is guarded WHERE assemble_switch = 4.
        all_top_ids
            .join(succeeded_top_ids, remainder: true)
            .filter { id, all_flag, success_flag -> success_flag == null }
            .map    { id, all_flag, success_flag -> tuple(id) }
            .sqlInsert(statement: params.sqlWriteBlastRefFetchFailed, db: 'sqlite')

    emit:
        // Per-ID top-hit reference sequence file, for reference-guided scaffold join.
        ref_seq = ref_out
            .filter { id, accession, is_top, csv_file, seq_file, gc_file, json_file -> is_top }
            .map    { id, accession, is_top, csv_file, seq_file, gc_file, json_file ->
                        tuple(id, seq_file) }
}
