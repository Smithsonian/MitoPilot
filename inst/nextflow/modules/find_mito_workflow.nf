include {find_mito_screen; find_mito_pick} from './find_mito.nf'

// Result summary for the Assemble table. Its own column for the same reason
// circularize_notes is: coverage overwrites assemble_notes later in WF1.
params.sqlWriteFindNotes = 'UPDATE assemble SET find_mito_notes = ? WHERE ID = ?'

// A sample with no confirmed mitochondrial contig fails here: its assembly never
// reaches the assemblies table, and the note says how far the search got.
params.sqlFailFindMito =
    'UPDATE assemble SET assemble_switch = 3, find_mito_notes = ?, time_stamp = ? WHERE ID = ?'

// Evidence behind every call. Upsert keyed on (ID, contig) rather than a
// DELETE + INSERT pair, which nf-sqldb may commit in either order.
params.sqlWriteCandidates = '''INSERT INTO mito_candidates
    (ID, contig, length, accession, pident, aligned_length, aligned_fraction,
     genes, rank, selected, reason, time_stamp)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    ON CONFLICT(ID, contig) DO UPDATE SET
      length           = excluded.length,
      accession        = excluded.accession,
      pident           = excluded.pident,
      aligned_length   = excluded.aligned_length,
      aligned_fraction = excluded.aligned_fraction,
      genes            = excluded.genes,
      rank             = excluded.rank,
      selected         = excluded.selected,
      reason           = excluded.reason,
      time_stamp       = excluded.time_stamp'''

// Samples eligible for the search: the option is switched on for them.
def find_eligible(opts) {
    opts?.attempt?.toString() == '1'
}

// The confirm stage cannot run without a MitoFinder reference database.
def find_has_db(opts) {
    opts?.mitofinder_db?.toString()?.trim()
}

def missing_db_note() {
    'mitogenome search skipped: no MitoFinder reference database configured; ' +
        'build one with custom_assembly_db(db_type = "mitofinder") and set it in the search options'
}

// Locate the mitochondrial contigs inside each sample's assembly.
//
// Input and output tuples share the shape the coverage workflow already uses
// for sample info, (id, assembly, topology, opts_id, circ_opts), with the
// assembly swapped for the confirmed contigs. Samples with the option off pass
// through untouched; samples where nothing is confirmed are failed and dropped.
workflow FIND_MITO_userAsmb {
    take:
        find_in     // tuple(id, assembly, topology, opts_id, circ_opts, genetic_code, find_opts)

    main:
        find_in
            .branch { id, assembly, topology, opts_id, copts, gcode, fopts ->
                run:   find_eligible(fopts) && find_has_db(fopts)
                no_db: find_eligible(fopts)
                skip:  true
            }
            .set { br }

        // Switched on but unusable: fail the sample up front with the fix,
        // rather than letting it run and die at the confirm stage.
        br.no_db
            .map { id, assembly, topology, opts_id, copts, gcode, fopts ->
                tuple(missing_db_note(), params.ts, id)
            }
            .sqlInsert(statement: params.sqlFailFindMito, db: 'sqlite')

        // Chunked fan-out: the FASTA is split into files of chunk_size contigs
        // so a draft genome is searched in parallel and never read whole.
        br.run
            .map { id, assembly, topology, opts_id, copts, gcode, fopts ->
                tuple(id, assembly, fopts)
            }
            .splitFasta(by: params.find_mito.chunk_size, file: true, elem: 1)
            .set { chunks }

        find_mito_screen(chunks)
            .groupTuple()
            .set { hits_by_id }

        br.run
            .map { id, assembly, topology, opts_id, copts, gcode, fopts ->
                tuple(id, assembly, opts_id, gcode, fopts)
            }
            .join(hits_by_id)
            .map { id, assembly, opts_id, gcode, fopts, hits ->
                // The reference database is staged as a task input so it is
                // visible inside the container on every executor.
                tuple(id, assembly, hits, file(fopts.mitofinder_db), opts_id, gcode, fopts)
            }
            .set { pick_in }

        find_mito_pick(pick_in).set { pick_out }

        pick_out
            .map { id, fasta, status_f, note_f, cand_f, opts_id, log_f ->
                tuple(note_f.text.trim(), id)
            }
            .sqlInsert(statement: params.sqlWriteFindNotes, db: 'sqlite')

        pick_out
            .map { id, fasta, status_f, note_f, cand_f, opts_id, log_f -> tuple(id, cand_f) }
            .splitCsv(header: true, quote: '"', elem: 1)
            .map { id, row ->
                tuple(
                    id,
                    row.contig,
                    row.length,
                    row.accession,
                    row.pident,
                    row.aligned_length,
                    row.aligned_fraction,
                    row.genes ?: null,
                    row.rank ?: null,
                    row.selected,
                    row.reason ?: '',
                    params.ts
                )
            }
            .sqlInsert(statement: params.sqlWriteCandidates, db: 'sqlite')

        pick_out
            .branch { id, fasta, status_f, note_f, cand_f, opts_id, log_f ->
                found: status_f.text.trim() == 'ok'
                empty: true
            }
            .set { picked }

        // Nothing confirmed: fail the sample, and it goes no further.
        picked.empty
            .map { id, fasta, status_f, note_f, cand_f, opts_id, log_f ->
                tuple(note_f.text.trim(), params.ts, id)
            }
            .sqlInsert(statement: params.sqlFailFindMito, db: 'sqlite')

        // Confirmed: swap the trimmed FASTA in and carry on with the rest of WF1.
        br.run
            .map { id, assembly, topology, opts_id, copts, gcode, fopts ->
                tuple(id, topology, copts)
            }
            .join(picked.found.map { id, fasta, status_f, note_f, cand_f, opts_id, log_f ->
                tuple(id, fasta, opts_id)
            })
            .map { id, topology, copts, fasta, opts_id ->
                tuple(id, fasta, topology, opts_id, copts)
            }
            .set { found_ch }

        br.skip
            .map { id, assembly, topology, opts_id, copts, gcode, fopts ->
                tuple(id, assembly, topology, opts_id, copts)
            }
            .mix(found_ch)
            .set { out_ch }

    emit:
        sample_info = out_ch
}
