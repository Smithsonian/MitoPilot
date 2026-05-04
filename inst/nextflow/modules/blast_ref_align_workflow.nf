include {blast_ref_align} from './blast_ref_align.nf'

// Query only reference sequence and rotation — all data from WF1 (BLAST_REF_FETCH),
// available at WF2 startup. Assembly sequence is read directly from the curate output
// FASTA, avoiding the timing issue where annotate.path is NULL when fromQuery runs
// at pipeline start (before CURATE writes it to the DB).
// The rotation is pos1 - 1 of the start_gene in blast_ref_annotations; if the
// gene is absent (no BLAST ref data) the subquery returns NULL → COALESCE to 0.
params.sqlReadAlign =
    'SELECT b.ID, s.sequence, ' +
        'COALESCE((SELECT MIN(r.pos1) - 1 FROM blast_ref_annotations r ' +
                  'WHERE r.ID = b.ID AND r.gene = d.start_gene), 0) ' +
    'FROM assemble b ' +
    'JOIN annotate a   ON b.ID = a.ID ' +
    'JOIN annotate_opts d ON a.annotate_opts = d.annotate_opts ' +
    'JOIN blast_ref_sequences s ON b.blast_accession = s.accession ' +
    'WHERE b.blast_accession IS NOT NULL'

params.sqlWriteAlignment = '''INSERT OR REPLACE INTO blast_ref_alignment
    (ID, aligned_sample, aligned_ref, rotation, ref_length, time_stamp)
    VALUES (?, ?, ?, ?, ?, ?)'''

workflow BLAST_REF_ALIGN {
    take:
        validated   // (id, path) per newly validated sample; gates timing
        curate_out  // (id, path, annotations, assembly_fasta, coverage, work_dir)

    main:
        // Reference sequence + rotation; all from WF1 data, available at WF2 startup.
        // Filter out rows where the stored sequence is not a valid nucleotide string
        // (e.g. an NCBI API error JSON stored by a failed BLAST_REF_FETCH run).
        channel.fromQuery(params.sqlReadAlign, db: 'sqlite')
            .map { row -> tuple(row[0], row[1], row[2] as Long) }
            .filter { id, ref_seq, rotation ->
                ref_seq && ref_seq.matches('[ACGTNacgtnRYSWKMBDHVryswkmbdhv]+')
            }
            .set { ref_ch }  // (id, ref_seq, rotation)

        // Gate on validated, pull assembly FASTA from curate_out, join with ref data
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
            .set { align_in }

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
}
