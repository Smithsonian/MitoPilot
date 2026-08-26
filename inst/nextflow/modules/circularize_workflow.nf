include {circularize; circularize_noReads} from './circularize.nf'

// Result note for the Assemble table. Kept in its own column because the
// coverage workflow overwrites assemble_notes with its own summary.
params.sqlWriteCircNotes = 'UPDATE assemble SET circularize_notes = ? WHERE ID = ?'

// Upsert keyed on the primary key rather than a DELETE + INSERT pair, which
// nf-sqldb may commit in either order.
params.sqlWriteCircOverlap = '''INSERT INTO circularize_overlap
    (ID, qstart, qend, sstart, send, length, pident, mismatches,
     aln_query, aln_subject, q_ctx_left, q_ctx_right, s_ctx_left, s_ctx_right,
     accepted, reason, trimmed,
     junction_reads, min_junction_reads, window_bp, min_overhang, time_stamp)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    ON CONFLICT(ID) DO UPDATE SET
      qstart = excluded.qstart, qend = excluded.qend,
      sstart = excluded.sstart, send = excluded.send,
      length = excluded.length, pident = excluded.pident,
      mismatches = excluded.mismatches,
      aln_query = excluded.aln_query, aln_subject = excluded.aln_subject,
      q_ctx_left = excluded.q_ctx_left, q_ctx_right = excluded.q_ctx_right,
      s_ctx_left = excluded.s_ctx_left, s_ctx_right = excluded.s_ctx_right,
      accepted = excluded.accepted, reason = excluded.reason,
      trimmed = excluded.trimmed,
      junction_reads = excluded.junction_reads,
      min_junction_reads = excluded.min_junction_reads,
      window_bp = excluded.window_bp, min_overhang = excluded.min_overhang,
      time_stamp = excluded.time_stamp'''

params.sqlWriteCircDepth = '''INSERT INTO circularize_depth
    (ID, position, rel_position, depth, depth_spanning, time_stamp)
    VALUES (?, ?, ?, ?, ?, ?)
    ON CONFLICT(ID, position) DO UPDATE SET
      rel_position = excluded.rel_position,
      depth = excluded.depth,
      depth_spanning = excluded.depth_spanning,
      time_stamp = excluded.time_stamp'''

// Samples eligible for circularization: the option is switched on and the user
// declared the assembly linear. Multi-contig assemblies are passed through
// untouched by circularize_asmb() itself, which records why in its note.
def circ_eligible(topology, opts) {
    opts?.attempt?.toString() == '1' && topology == 'linear'
}

// Read-based projects. Input/output tuple: (id, reads, assembly, topology, opts_id).
workflow CIRCULARIZE_userAsmb {
    take:
        circ_in     // tuple(id, reads, assembly, topology, opts_id, circ_opts)

    main:
        circ_in
            .branch { id, reads, assembly, topology, opts_id, copts ->
                run:  circ_eligible(topology, copts)
                skip: true
            }
            .set { br }

        circularize(
            br.run.map { id, reads, assembly, topology, opts_id, copts ->
                tuple(id, reads, assembly, opts_id, copts)
            }
        ).set { circ_out }

        circ_out
            .map { id, fasta, topo_f, note_f, ov_f, dp_f, opts_id, log_f -> tuple(note_f.text.trim(), id) }
            .sqlInsert(statement: params.sqlWriteCircNotes, db: 'sqlite')

        circ_out
            .map { id, fasta, topo_f, note_f, ov_f, dp_f, opts_id, log_f -> ov_f }
            .splitCsv(header: true, quote: '"')
            .map { row -> tuple(row.ID, row.qstart, row.qend, row.sstart, row.send,
                                row.length, row.pident, row.mismatches,
                                row.aln_query, row.aln_subject,
                                row.q_ctx_left ?: '', row.q_ctx_right ?: '',
                                row.s_ctx_left ?: '', row.s_ctx_right ?: '',
                                row.accepted,
                                row.reason ?: '', row.trimmed,
                                row.junction_reads ?: null,
                                row.min_junction_reads, row.window_bp ?: null,
                                row.min_overhang, params.ts) }
            .sqlInsert(statement: params.sqlWriteCircOverlap, db: 'sqlite')

        circ_out
            .map { id, fasta, topo_f, note_f, ov_f, dp_f, opts_id, log_f -> dp_f }
            .splitCsv(header: true, quote: '"')
            .map { row -> tuple(row.ID, row.position, row.rel_position,
                                row.depth, row.depth_spanning, params.ts) }
            .sqlInsert(statement: params.sqlWriteCircDepth, db: 'sqlite')

        // Re-join the circularized samples with the reads they came in with, so
        // the downstream coverage tuple keeps its shape.
        br.run
            .map { id, reads, assembly, topology, opts_id, copts -> tuple(id, reads) }
            .join(circ_out.map { id, fasta, topo_f, note_f, ov_f, dp_f, opts_id, log_f ->
                tuple(id, fasta, topo_f.text.trim(), opts_id)
            })
            .map { id, reads, fasta, topology, opts_id ->
                tuple(id, reads, fasta, topology, opts_id)
            }
            .set { circularized }

        br.skip
            .map { id, reads, assembly, topology, opts_id, copts ->
                tuple(id, reads, assembly, topology, opts_id)
            }
            .mix(circularized)
            .set { out_ch }

    emit:
        coverage_in = out_ch
}

// No-reads projects. Input/output tuple: (id, assembly, topology, opts_id).
workflow CIRCULARIZE_userAsmb_noReads {
    take:
        circ_in     // tuple(id, assembly, topology, opts_id, circ_opts)

    main:
        circ_in
            .branch { id, assembly, topology, opts_id, copts ->
                run:  circ_eligible(topology, copts)
                skip: true
            }
            .set { br }

        circularize_noReads(
            br.run.map { id, assembly, topology, opts_id, copts ->
                tuple(id, assembly, opts_id, copts)
            }
        ).set { circ_out }

        circ_out
            .map { id, fasta, topo_f, note_f, ov_f, dp_f, opts_id, log_f -> tuple(note_f.text.trim(), id) }
            .sqlInsert(statement: params.sqlWriteCircNotes, db: 'sqlite')

        circ_out
            .map { id, fasta, topo_f, note_f, ov_f, dp_f, opts_id, log_f -> ov_f }
            .splitCsv(header: true, quote: '"')
            .map { row -> tuple(row.ID, row.qstart, row.qend, row.sstart, row.send,
                                row.length, row.pident, row.mismatches,
                                row.aln_query, row.aln_subject,
                                row.q_ctx_left ?: '', row.q_ctx_right ?: '',
                                row.s_ctx_left ?: '', row.s_ctx_right ?: '',
                                row.accepted,
                                row.reason ?: '', row.trimmed,
                                row.junction_reads ?: null,
                                row.min_junction_reads, row.window_bp ?: null,
                                row.min_overhang, params.ts) }
            .sqlInsert(statement: params.sqlWriteCircOverlap, db: 'sqlite')

        circ_out
            .map { id, fasta, topo_f, note_f, ov_f, dp_f, opts_id, log_f -> dp_f }
            .splitCsv(header: true, quote: '"')
            .map { row -> tuple(row.ID, row.position, row.rel_position,
                                row.depth, row.depth_spanning, params.ts) }
            .sqlInsert(statement: params.sqlWriteCircDepth, db: 'sqlite')

        circ_out
            .map { id, fasta, topo_f, note_f, ov_f, dp_f, opts_id, log_f ->
                tuple(id, fasta, topo_f.text.trim(), opts_id)
            }
            .set { circularized }

        br.skip
            .map { id, assembly, topology, opts_id, copts ->
                tuple(id, assembly, topology, opts_id)
            }
            .mix(circularized)
            .set { out_ch }

    emit:
        coverage_in = out_ch
}
