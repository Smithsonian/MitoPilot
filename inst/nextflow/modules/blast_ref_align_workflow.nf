include {blast_ref_align} from './blast_ref_align.nf'

// SQL fragment: assemble_notes with any segment starting at `tag` stripped (and a
// preceding '; '). Mirrors the helpers in the blast_* workflows; duplicated here so
// this module is self-contained.
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

// Reference sequence and rotation per candidate reference for newly-curated
// samples (gated on curate_out). One row per (ID, candidate accession). rank=1 is
// the top hit. Rotation is computed from that accession's own annotations.
// Assembly sequence comes from the curate output FASTA (already rotated to start_gene).
params.sqlReadRef =
    'SELECT c.ID, c.accession, c.rank, s.sequence, ' +
        'COALESCE((SELECT MIN(r.pos1) - 1 FROM blast_ref_annotations r ' +
                  'WHERE r.accession = c.accession AND r.gene = d.start_gene), 0) ' +
    'FROM (SELECT ID, accession, MIN(rank) AS rank FROM blast_ref_candidates ' +
         'GROUP BY ID, accession) c ' +
    'JOIN annotate a     ON c.ID = a.ID ' +
    'JOIN annotate_opts d ON a.annotate_opts = d.annotate_opts ' +
    'JOIN blast_ref_sequences s ON c.accession = s.accession'

// Backfill: samples already validated in a prior run (assemblies.sequence is the
// rotated post-curate sequence) that are missing an alignment row. annotate_switch = 2
// is the post-VALIDATE state and is the only safe signal that the stored sequence
// has been rotated to start_gene — for userAsmb projects, assemblies.sequence is
// pre-populated with the unrotated user FASTA in WF1_userAsmb and only becomes
// rotated after CURATE runs in WF2.
params.sqlBackfill =
    'SELECT b.ID, cd.accession, cd.rank, a.sequence, s.sequence, ' +
        'COALESCE((SELECT MIN(r.pos1) - 1 FROM blast_ref_annotations r ' +
                  'WHERE r.accession = cd.accession AND r.gene = d.start_gene), 0) ' +
    'FROM assemble b ' +
    'JOIN (SELECT ID, accession, MIN(rank) AS rank FROM blast_ref_candidates ' +
         'GROUP BY ID, accession) cd ON cd.ID = b.ID ' +
    'JOIN assemblies a   ON b.ID = a.ID AND a.ignore = 0 ' +
        'AND a.scaffold = (SELECT MIN(scaffold) FROM assemblies a2 WHERE a2.ID = a.ID AND a2.ignore = 0) ' +
    'JOIN annotate c     ON b.ID = c.ID ' +
    'JOIN annotate_opts d ON c.annotate_opts = d.annotate_opts ' +
    'JOIN blast_ref_sequences s ON cd.accession = s.accession ' +
    'WHERE a.sequence IS NOT NULL ' +
      'AND c.annotate_switch = 2 ' +
      'AND NOT EXISTS (SELECT 1 FROM blast_ref_alignment al WHERE al.ID = b.ID AND al.accession = cd.accession)'

params.sqlWriteAlignment = '''INSERT OR REPLACE INTO blast_ref_alignment
    (ID, accession, aligned_sample, aligned_ref, rotation, ref_length, time_stamp)
    VALUES (?, ?, ?, ?, ?, ?, ?)'''

// Mark the 'BLAST Ref Align' field (poor_blast_ref) = 'failed' and add an [align]
// note when blast_ref_align fails for a sample (errorStrategy 'ignore' suppresses
// task failure; we detect missing output downstream). This is a WF2 step that runs
// after the sample is already complete/locked in WF1, so the assemble_switch state
// is intentionally left unchanged: the assembly and annotation are unaffected, only
// the whole-genome alignment view is unavailable.
params.refAlignFailedMsg = "Whole-genome BLAST reference alignment failed. Annotation and curation are unaffected; the alignment view in the Assemble details will be unavailable."
params.sqlWriteBlastRefAlignFailed = "UPDATE assemble SET " +
    "poor_blast_ref = 'failed', " +
    "assemble_notes = ${appendTaggedNoteSql('[align]', params.refAlignFailedMsg)} " +
    "WHERE ID = ?"

workflow BLAST_REF_ALIGN {
    take:
        validated   // (id, path) — gates timing: only fires after VALIDATE completes
        curate_out  // (id, path, annotations, assembly_fasta, coverage, work_dir)

    main:
        // Reference data (sequence + rotation) available at WF2 startup from WF1.
        // combine(by:0) below buffers this finite fromQuery side, so no deadlock.
        channel.fromQuery(params.sqlReadRef, db: 'sqlite')
            .map { row -> tuple(row[0], row[1], (row[2] as Integer) == 1, row[3], row[4] as Long) }
            .filter { id, accession, is_top, ref_seq, rotation -> ref_seq }
            .set { ref_ch }  // (id, accession, is_top, ref_seq, rotation)

        // Newly-validated samples: gate on `validated` so alignment runs after
        // VALIDATE completes. Pull the rotated assembly FASTA from curate_out.
        // combine(by:0) fans each sample's assembly across all its candidate refs.
        validated
            .join(curate_out, by: [0, 1])
            .map { id, path, annotations, assembly_fasta, coverage, work_dir ->
                tuple(id, path, assembly_fasta)
            }
            .combine(ref_ch, by: 0)
            .map { id, path, assembly_fasta, accession, is_top, ref_seq, rotation ->
                def seq = assembly_fasta.readLines()
                    .findAll { !it.startsWith('>') }
                    .join('')
                tuple(id, accession, is_top, seq, ref_seq, rotation)
            }
            .set { new_align_in }

        // Backfill: already-curated samples missing an alignment row for a candidate.
        channel.fromQuery(params.sqlBackfill, db: 'sqlite')
            .map { row ->
                def accession = row[1]?.toString()
                def is_top    = (row[2] as Integer) == 1
                def asm_seq   = row[3]?.toString()
                def ref_seq   = row[4]?.toString()
                if (!accession || !asm_seq || !ref_seq) return null
                if (!ref_seq.matches('[ACGTNacgtnRYSWKMBDHVryswkmbdhv]+')) return null
                tuple(row[0], accession, is_top, asm_seq, ref_seq, row[5] as Long)
            }
            .filter { it != null }
            .set { backfill_ch }

        new_align_in.mix(backfill_ch).set { align_in }

        // Track top-hit (id, accession) entering align so a failed TOP-ref alignment
        // can flag the sample. Non-top candidate failures only lose that candidate's
        // 3-track view and are not surfaced as sample-level failures.
        align_in
            .filter { id, accession, is_top, assembly_seq, ref_seq, rotation -> is_top }
            .map    { id, accession, is_top, assembly_seq, ref_seq, rotation -> tuple("${id}|${accession}".toString(), id) }
            .set { top_align_keys }

        blast_ref_align(
            align_in.map { id, accession, is_top, assembly_seq, ref_seq, rotation ->
                tuple(id, accession, assembly_seq, ref_seq, rotation)
            }
        ).set { align_out }

        // Parse the one-row CSV and write to blast_ref_alignment table
        align_out
            .map { id, accession, csv_file ->
                def lines = csv_file.readLines()
                if (lines.size() < 2) return null
                // CSV columns: aligned_sample, aligned_ref, rotation, ref_length
                def parts = lines[1].split(',', -1).collect { it.replaceAll('^"|"$', '') }
                if (parts.size() < 4) return null
                def ts = java.time.Instant.now().getEpochSecond()
                tuple(
                    id,
                    accession,
                    parts[0],           // aligned_sample
                    parts[1],           // aligned_ref
                    parts[2].toLong(),  // rotation
                    parts[3].toLong(),  // ref_length
                    ts
                )
            }
            .filter { it != null }
            .sqlInsert(statement: params.sqlWriteAlignment, db: 'sqlite')

        // Detect TOP-ref align failures: top (id, accession) that entered but produced
        // no CSV output. Keyed on "id|accession" so only the top candidate counts.
        align_out
            .map { id, accession, csv_file -> tuple("${id}|${accession}".toString(), true) }
            .set { succeeded_align_keys }

        top_align_keys
            .join(succeeded_align_keys, remainder: true)
            .filter { key, id, success_flag -> success_flag == null }
            .map    { key, id, success_flag -> tuple(id) }
            .sqlInsert(statement: params.sqlWriteBlastRefAlignFailed, db: 'sqlite')
}
