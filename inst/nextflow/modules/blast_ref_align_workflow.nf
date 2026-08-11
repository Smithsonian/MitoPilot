include {blast_ref_align} from './blast_ref_align.nf'
include {appendTaggedNoteSql} from './sql_notes.nf'

// Reference sequence and rotation per candidate reference for newly-curated
// samples (gated on curate_out). One row per (ID, candidate accession). rank=1 is
// the top hit. Rotation is computed from that accession's own annotations, but
// ONLY for references that are truly circular (s.topology = 'circular'); a linear
// GenBank reference keeps its native coordinates (rotation 0) so the synteny plot
// is not artificially rotated to the start gene.
// Assembly sequence comes from the curate output FASTA (already rotated to start_gene).
params.sqlReadRef =
    'SELECT c.ID, c.accession, c.rank, s.sequence, ' +
        'CASE WHEN s.topology = \'circular\' THEN ' +
            'COALESCE((SELECT MIN(r.pos1) - 1 FROM blast_ref_annotations r ' +
                      'WHERE r.accession = c.accession AND r.gene = d.start_gene), 0) ' +
        'ELSE 0 END ' +
    'FROM (SELECT ID, accession, MIN(rank) AS rank FROM blast_ref_candidates ' +
         'GROUP BY ID, accession) c ' +
    'JOIN annotate_opts d ON d.annotate_opts = (SELECT an.annotate_opts FROM annotate an WHERE an.ID = c.ID ORDER BY an.path, an.scaffold LIMIT 1) ' +
    'JOIN blast_ref_sequences s ON c.accession = s.accession'

// Backfill: samples already validated in a prior run (assemblies.sequence is the
// rotated post-curate sequence) that are missing an alignment row. annotate_switch = 2
// is the post-VALIDATE state and is the only safe signal that the stored sequence
// has been rotated to start_gene - for userAsmb projects, assemblies.sequence is
// pre-populated with the unrotated user FASTA in WF1_userAsmb and only becomes
// rotated after CURATE runs in WF2.
params.sqlBackfill =
    'SELECT b.ID, cd.accession, cd.rank, a.sequence, s.sequence, ' +
        'CASE WHEN s.topology = \'circular\' THEN ' +
            'COALESCE((SELECT MIN(r.pos1) - 1 FROM blast_ref_annotations r ' +
                      'WHERE r.accession = cd.accession AND r.gene = d.start_gene), 0) ' +
        'ELSE 0 END, ' +
    'a.path, a.scaffold ' +
    'FROM assemble b ' +
    'JOIN (SELECT ID, accession, MIN(rank) AS rank FROM blast_ref_candidates ' +
         'GROUP BY ID, accession) cd ON cd.ID = b.ID ' +
    'JOIN assemblies a   ON b.ID = a.ID AND a.ignore = 0 ' +
    'JOIN annotate c     ON c.ID = a.ID AND c.path = a.path AND c.scaffold = a.scaffold ' +
    'JOIN annotate_opts d ON c.annotate_opts = d.annotate_opts ' +
    'JOIN blast_ref_sequences s ON cd.accession = s.accession ' +
    'WHERE a.sequence IS NOT NULL ' +
      'AND c.annotate_switch = 2 ' +
      'AND NOT EXISTS (SELECT 1 FROM blast_ref_alignment al WHERE al.ID = b.ID AND al.path = a.path AND al.scaffold = a.scaffold AND al.accession = cd.accession)'

params.sqlWriteAlignment = '''INSERT OR REPLACE INTO blast_ref_alignment
    (ID, path, scaffold, accession, aligned_sample, aligned_ref, rotation, ref_length, ref_start, strand, time_stamp)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)'''

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
        validated   // (id, path, scaffold) - gates timing: fires after VALIDATE completes
        curate_out  // (id, path, scaffold, annotations, assembly_fasta, coverage, work_dir) [per unit]

    main:
        // Reference data (sequence + rotation) available at WF2 startup from WF1.
        // combine(by:0) below buffers this finite fromQuery side, so no deadlock.
        channel.fromQuery(params.sqlReadRef, db: 'sqlite')
            .map { row -> tuple(row[0], row[1], (row[2] as Integer) == 1, row[3], row[4] as Long) }
            .filter { id, accession, is_top, ref_seq, rotation -> ref_seq }
            .set { ref_ch }  // (id, accession, is_top, ref_seq, rotation)

        // Newly-validated units: gate on `validated` so alignment runs after
        // VALIDATE completes, then align EACH scaffold's own curated assembly to
        // every candidate reference. blast_ref_alignment is keyed
        // (ID, path, scaffold, accession).
        curate_out
            .map { tuple(it[0], it[1], it[2], it[4]) }   // (id, path, scaffold, assembly_fasta)
            .set { curate_assembly }
        validated
            .map { tuple(it[0], it[1], it[2]) }          // (id, path, scaffold)
            .unique()
            .join(curate_assembly, by: [0, 1, 2])
            .map { id, path, scaffold, assembly_fasta ->
                tuple(id, path, scaffold, assembly_fasta)
            }
            .combine(ref_ch, by: 0)                       // ref_ch keyed by id -> broadcast to every scaffold
            .map { id, path, scaffold, assembly_fasta, accession, is_top, ref_seq, rotation ->
                def seq = assembly_fasta.readLines()
                    .findAll { !it.startsWith('>') }
                    .join('')
                tuple(id, path, scaffold, accession, is_top, seq, ref_seq, rotation)
            }
            .set { new_align_in }

        // Backfill: already-validated units missing an alignment row for a candidate.
        channel.fromQuery(params.sqlBackfill, db: 'sqlite')
            .map { row ->
                def accession = row[1]?.toString()
                def is_top    = (row[2] as Integer) == 1
                def asm_seq   = row[3]?.toString()
                def ref_seq   = row[4]?.toString()
                if (!accession || !asm_seq || !ref_seq) return null
                if (!ref_seq.matches('[ACGTNacgtnRYSWKMBDHVryswkmbdhv]+')) return null
                tuple(row[0], row[6], row[7], accession, is_top, asm_seq, ref_seq, row[5] as Long)
            }
            .filter { it != null }
            .set { backfill_ch }

        new_align_in.mix(backfill_ch).set { align_in }

        // Track top-hit (id, path, scaffold, accession) entering align so a failed
        // TOP-ref alignment can flag the sample. Scaffold is in the key so one
        // scaffold's success does not mask a sibling's top-ref failure.
        align_in
            .filter { id, path, scaffold, accession, is_top, assembly_seq, ref_seq, rotation -> is_top }
            .map    { id, path, scaffold, accession, is_top, assembly_seq, ref_seq, rotation -> tuple("${id}|${path}|${scaffold}|${accession}".toString(), id) }
            .set { top_align_keys }

        blast_ref_align(
            align_in.map { id, path, scaffold, accession, is_top, assembly_seq, ref_seq, rotation ->
                tuple(id, path, scaffold, accession, assembly_seq, ref_seq, rotation)
            }
        ).set { align_out }

        // Parse the one-row CSV and write to blast_ref_alignment table
        align_out
            .map { id, path, scaffold, accession, csv_file ->
                def lines = csv_file.readLines()
                if (lines.size() < 2) return null
                // CSV columns: aligned_sample, aligned_ref, rotation, ref_length, ref_start, strand
                def parts = lines[1].split(',', -1).collect { it.replaceAll('^"|"$', '') }
                if (parts.size() < 6) return null
                def ts = java.time.Instant.now().getEpochSecond()
                tuple(
                    id,
                    path,
                    scaffold,
                    accession,
                    parts[0],           // aligned_sample
                    parts[1],           // aligned_ref
                    parts[2].toLong(),  // rotation
                    parts[3].toLong(),  // ref_length
                    parts[4].toLong(),  // ref_start
                    parts[5],           // strand
                    ts
                )
            }
            .filter { it != null }
            .sqlInsert(statement: params.sqlWriteAlignment, db: 'sqlite')

        // Detect TOP-ref align failures: top (id, path, scaffold, accession) that
        // entered but produced no CSV output.
        align_out
            .map { id, path, scaffold, accession, csv_file -> tuple("${id}|${path}|${scaffold}|${accession}".toString(), true) }
            .set { succeeded_align_keys }

        top_align_keys
            .join(succeeded_align_keys, remainder: true)
            .filter { key, id, success_flag -> success_flag == null }
            .map    { key, id, success_flag -> tuple(id) }
            .sqlInsert(statement: params.sqlWriteBlastRefAlignFailed, db: 'sqlite')
}
