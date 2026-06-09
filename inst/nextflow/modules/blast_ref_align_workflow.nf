include {blast_ref_align} from './blast_ref_align.nf'

// Reference sequence and rotation for newly-curated samples (gated on curate_out).
// Assembly sequence comes from the curate output FASTA (already rotated to start_gene).
params.sqlReadRef =
    'SELECT b.ID, s.sequence, ' +
        'COALESCE((SELECT MIN(r.pos1) - 1 FROM blast_ref_annotations r ' +
                  'WHERE r.ID = b.ID AND r.gene = d.start_gene), 0) ' +
    'FROM assemble b ' +
    'JOIN annotate a     ON b.ID = a.ID ' +
    'JOIN annotate_opts d ON a.annotate_opts = d.annotate_opts ' +
    'JOIN blast_ref_sequences s ON b.blast_accession = s.accession ' +
    'WHERE b.blast_accession IS NOT NULL'

// Backfill: samples already validated in a prior run (assemblies.sequence is the
// rotated post-curate sequence) that are missing an alignment row. annotate_switch = 2
// is the post-VALIDATE state and is the only safe signal that the stored sequence
// has been rotated to start_gene — for userAsmb projects, assemblies.sequence is
// pre-populated with the unrotated user FASTA in WF1_userAsmb and only becomes
// rotated after CURATE runs in WF2.
params.sqlBackfill =
    'SELECT b.ID, a.sequence, s.sequence, ' +
        'COALESCE((SELECT MIN(r.pos1) - 1 FROM blast_ref_annotations r ' +
                  'WHERE r.ID = b.ID AND r.gene = d.start_gene), 0) ' +
    'FROM assemble b ' +
    'JOIN assemblies a   ON b.ID = a.ID AND a.ignore = 0 ' +
        'AND a.scaffold = (SELECT MIN(scaffold) FROM assemblies a2 WHERE a2.ID = a.ID AND a2.ignore = 0) ' +
    'JOIN annotate c     ON b.ID = c.ID ' +
    'JOIN annotate_opts d ON c.annotate_opts = d.annotate_opts ' +
    'JOIN blast_ref_sequences s ON b.blast_accession = s.accession ' +
    'WHERE b.blast_accession IS NOT NULL ' +
      'AND a.sequence IS NOT NULL ' +
      'AND c.annotate_switch = 2 ' +
      'AND NOT EXISTS (SELECT 1 FROM blast_ref_alignment al WHERE al.ID = b.ID)'

params.sqlWriteAlignment = '''INSERT OR REPLACE INTO blast_ref_alignment
    (ID, aligned_sample, aligned_ref, rotation, ref_length, time_stamp)
    VALUES (?, ?, ?, ?, ?, ?)'''

// Mark poor_blast_ref = 'failed' when blast_ref_align fails for a sample
// (errorStrategy 'ignore' suppresses task failure; we detect missing output downstream).
params.sqlWriteBlastRefAlignFailed = "UPDATE assemble SET poor_blast_ref = 'failed' WHERE ID = ?"

workflow BLAST_REF_ALIGN {
    take:
        validated   // (id, path) — gates timing: only fires after VALIDATE completes
        curate_out  // (id, path, annotations, assembly_fasta, coverage, work_dir)

    main:
        // Reference data (sequence + rotation) available at WF2 startup from WF1.
        // TODO: ref_ch (fromQuery) closes at WF2 startup but the left side of the
        //   .join(ref_ch) closes much later (after ANNOTATE→CURATE→VALIDATE);
        //   this asymmetric-lifetime join risks a deadlock. Fix by carrying
        //   ref_seq/rotation through upstream channels or a per-item DB lookup.
        channel.fromQuery(params.sqlReadRef, db: 'sqlite')
            .map { row -> tuple(row[0], row[1], row[2] as Long) }
            .filter { id, ref_seq, rotation -> ref_seq }
            .set { ref_ch }  // (id, ref_seq, rotation)

        // Newly-validated samples: gate on `validated` so alignment runs after
        // VALIDATE completes. Pull the rotated assembly FASTA from curate_out.
        validated
            .join(curate_out, by: [0, 1])
            .map { id, path, annotations, assembly_fasta, coverage, work_dir ->
                tuple(id, path, assembly_fasta)
            }
            .join(ref_ch, by: 0)
            .map { id, path, assembly_fasta, ref_seq, rotation ->
                def seq = assembly_fasta.readLines()
                    .findAll { !it.startsWith('>') }
                    .join('')
                tuple(id, seq, ref_seq, rotation)
            }
            .set { new_align_in }

        // Backfill: already-curated samples with no alignment row.
        channel.fromQuery(params.sqlBackfill, db: 'sqlite')
            .map { row ->
                def asm_seq = row[1]?.toString()
                def ref_seq = row[2]?.toString()
                if (!asm_seq || !ref_seq) return null
                if (!ref_seq.matches('[ACGTNacgtnRYSWKMBDHVryswkmbdhv]+')) return null
                tuple(row[0], asm_seq, ref_seq, row[3] as Long)
            }
            .filter { it != null }
            .set { backfill_ch }

        new_align_in.mix(backfill_ch).set { align_in }

        // Track all IDs entering align so failures (no output) can be detected
        align_in
            .map { id, assembly_seq, ref_seq, rotation -> tuple(id, true) }
            .set { all_align_ids }

        blast_ref_align(align_in).set { align_out }

        // Parse the one-row CSV and write to blast_ref_alignment table
        align_out
            .map { id, csv_file ->
                def lines = csv_file.readLines()
                if (lines.size() < 2) return null
                // CSV columns: aligned_sample, aligned_ref, rotation, ref_length
                def parts = lines[1].split(',', -1).collect { it.replaceAll('^"|"$', '') }
                if (parts.size() < 4) return null
                def ts = java.time.Instant.now().getEpochSecond()
                tuple(
                    id,
                    parts[0],           // aligned_sample
                    parts[1],           // aligned_ref
                    parts[2].toLong(),  // rotation
                    parts[3].toLong(),  // ref_length
                    ts
                )
            }
            .filter { it != null }
            .sqlInsert(statement: params.sqlWriteAlignment, db: 'sqlite')

        // Detect align failures: IDs that entered but produced no CSV output
        align_out
            .map { id, csv_file -> tuple(id, true) }
            .set { succeeded_align_ids }

        all_align_ids
            .join(succeeded_align_ids, remainder: true)
            .filter { id, all_flag, success_flag -> success_flag == null }
            .map    { id, all_flag, success_flag -> tuple(id) }
            .sqlInsert(statement: params.sqlWriteBlastRefAlignFailed, db: 'sqlite')
}
