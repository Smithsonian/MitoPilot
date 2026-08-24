include {circularize; circularize_noReads} from './circularize.nf'

// Result note for the Assemble table. Kept in its own column because the
// coverage workflow overwrites assemble_notes with its own summary.
params.sqlWriteCircNotes = 'UPDATE assemble SET circularize_notes = ? WHERE ID = ?'

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
            .map { id, fasta, topo_f, note_f, opts_id, log_f -> tuple(note_f.text.trim(), id) }
            .sqlInsert(statement: params.sqlWriteCircNotes, db: 'sqlite')

        // Re-join the circularized samples with the reads they came in with, so
        // the downstream coverage tuple keeps its shape.
        br.run
            .map { id, reads, assembly, topology, opts_id, copts -> tuple(id, reads) }
            .join(circ_out.map { id, fasta, topo_f, note_f, opts_id, log_f ->
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
            .map { id, fasta, topo_f, note_f, opts_id, log_f -> tuple(note_f.text.trim(), id) }
            .sqlInsert(statement: params.sqlWriteCircNotes, db: 'sqlite')

        circ_out
            .map { id, fasta, topo_f, note_f, opts_id, log_f ->
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
