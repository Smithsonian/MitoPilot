include {coverage_userAsmb; coverage_userAsmb_noReads} from './coverage_userAsmb.nf'

params.sqlRead =  'SELECT s.ID, s.assembly, s.topology, ' +
                  'a.assemble_opts, opts.min_assembly_length ' +
                  'FROM samples s ' +
                  'JOIN assemble a ON s.ID = a.ID ' +
                  'JOIN assemble_opts opts ON a.assemble_opts = opts.assemble_opts ' +
                  'WHERE a.assemble_switch IN (1, 4) AND a.assemble_lock = 0'


params.sqlWrite =   'UPDATE assemblies SET depth = ?, gc = ?, errors = ?, time_stamp = ? ' +
                    'WHERE ID=? and path=? and scaffold=?'

params.sqlDeleteAssemblies =  'DELETE FROM assemblies WHERE ID = ? AND time_stamp != ?'

// Upsert for the same reason as assemble_workflow.nf: `INSERT OR REPLACE` is
// delete-then-insert and nulls every unlisted column (all six blast_*, plus
// depth/gc/errors/edit_positions), which races the per-scaffold BLAST UPDATE
// because nf-sqldb batches and commits each channel independently.
params.sqlWriteAssemblies = '''INSERT INTO assemblies
    (ID, path, scaffold, length, length_raw, topology, time_stamp, sequence, ignore, edited)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, 0)
    ON CONFLICT(ID, path, scaffold) DO UPDATE SET
      length     = excluded.length,
      length_raw = excluded.length_raw,
      topology   = excluded.topology,
      time_stamp = excluded.time_stamp,
      sequence   = excluded.sequence,
      ignore     = excluded.ignore,
      edited     = 0,
      depth          = NULL,
      gc             = NULL,
      errors         = NULL,
      edit_positions = NULL,
      blast_lineage  = NULL,
      blast_accession = CASE WHEN assemblies.time_stamp = excluded.time_stamp
                             THEN assemblies.blast_accession ELSE NULL END,
      blast_species   = CASE WHEN assemblies.time_stamp = excluded.time_stamp
                             THEN assemblies.blast_species ELSE NULL END,
      blast_pident    = CASE WHEN assemblies.time_stamp = excluded.time_stamp
                             THEN assemblies.blast_pident ELSE NULL END,
      blast_qcovs     = CASE WHEN assemblies.time_stamp = excluded.time_stamp
                             THEN assemblies.blast_qcovs ELSE NULL END,
      blast_evalue    = CASE WHEN assemblies.time_stamp = excluded.time_stamp
                             THEN assemblies.blast_evalue ELSE NULL END'''

params.sqlWriteAssemble =   'UPDATE assemble SET paths=?, scaffolds=?, length=?, topology=?, ' +
                            'assemble_switch=?, assemble_notes=?, time_stamp=?, poor_blast_ref=NULL WHERE ID=?'


// Shared writer: parse coverageStats -> assemblies/assemble DB writes + emit the
// BLAST input. Lives in one place so the (data-loss-sensitive) DB write logic is
// identical for the read-based and no-reads coverage workflows.
workflow COVERAGE_userAsmb_WRITE {
    take:
        coverage_out
        min_len_lookup
        min_len_summary

    main:
        // Coverage output -> depth/gc/errors
        coverage_out
            .flatten()
            .filter{ it =~ /(.*coverageStats.csv)$/ }
            .splitCsv(header: true, sep: ',')
            .map { it ->
                tuple(
                    it.SeqId,
                    it.MeanDepth,
                    it.GC,
                    it.ErrorRate
                )
            }
            .groupTuple()
            .map { it ->
                tuple(
                    it[1].join(' '),                   // mean depth
                    it[2].join(' '),                   // gc
                    it[3].join(' '),                   // error rate
                    params.ts,                         // timestamp
                    it[0].split('\\.'),                // id, path, scaffold
                ).flatten()
            }
            .sqlInsert(statement: params.sqlWrite, db: 'sqlite')

       // Clear old assemblies from db
        coverage_out
          .map { it ->
            tuple(
              it[2],
              params.ts
            )
          }
          .sqlInsert( statement: params.sqlDeleteAssemblies, db: 'sqlite')

        // Write to assemblies table
        coverage_out
            .map { it -> it[3] }.flatten()
            .splitFasta(record: [id: true, desc: true, seqString: true])
            .map { record ->
                tuple(
                    record.id.split('\\.'),             // ID, path, scaffold
                    record.seqString.length(),          // length
                    record.seqString.length(),          // length_raw (initially equal to length; curate may later trim length)
                    record.desc,                        // topology
                    params.ts,                          // time stamp
                    record.seqString                    // sequence
                ).flatten()
            }
            .combine(min_len_lookup, by: 0)             // append per-sample min_assembly_length
            .map { it ->                                // mark short assemblies
                def min_len = it[8] as Integer
                it[8] = (it[3] < min_len) ? 1 : 0     // replace min_len slot with ignore flag
                return it
            }
            .set { assemblies_ch }
        assemblies_ch.sqlInsert( statement: params.sqlWriteAssemblies, db: 'sqlite')

        // Update DB assemble table
        assemblies_ch
            .map { it ->
                tuple(
                    it[0],                                          // ID
                    it[1].toInteger(),                              // paths
                    it[2].toInteger(),                              // scaffold
                    it[3].toInteger(),                              // length
                    it[5]                                           // topology (shifted after adding length_raw at it[4])
                )
            }
            .groupTuple()
            .combine(min_len_summary, by: 0)        // append per-sample min_assembly_length
            .map { id, paths_list, scaffolds_list, lengths_list, topos_list, min_assembly_length ->
                def max_paths      = paths_list.max()
                def max_scaffolds  = scaffolds_list.max()
                def length_str     = lengths_list.unique().sort().reverse().join(";")
                def topo_str       = topos_list.unique().sort().join(";")
                def max_len        = lengths_list.max() ?: 0
                def status         = '4'
                def notes          = ''
                if (max_scaffolds > 1) {
                    notes = 'Output contains disconnected contigs'
                    def n_passing = lengths_list.count { it >= (min_assembly_length as Integer) }
                    if (n_passing != 1) {
                        topo_str = 'fragmented'
                        status   = '3'
                    }
                }
                if (max_paths > 1) {
                    status = '3'
                    notes  = 'Unable to resolve single assembly from reads'
                }
                if (max_len < (min_assembly_length as Integer)) {
                    status = '3'
                    notes  = "All scaffolds below min assembly length (${min_assembly_length} bp)"
                }
                tuple(max_paths, max_scaffolds, length_str, topo_str, status, notes, params.ts, id)
            }
            .sqlInsert(statement: params.sqlWriteAssemble , db: 'sqlite')

    emit:
        // tuple(id, assembly, opts_id) for single-contig BLAST search
        blast_in = coverage_out.map{ it -> tuple(it[2], it[3], it[4]) }
}


workflow COVERAGE_userAsmb {
    take:
        input

    main:
        // sample info channel from DB
        channel.fromQuery(params.sqlRead, db: 'sqlite')
            .multiMap { it ->
                info: tuple(
                    it[0],                                          // ID
                    file(params.asmbDir + "/" + it[1]),             // assembly
                    it[2],                                          // topology
                    it[3]                                          // assemble opts dummy var
                )
                min_len_scaffolds: tuple(it[0], it[4] == null ? 500 : (it[4] as Integer)) // ID, min_assembly_length (for per-scaffold ignore flag)
                min_len_summary:   tuple(it[0], it[4] == null ? 500 : (it[4] as Integer)) // ID, min_assembly_length (for per-sample all-short check)
            }
            .set { query_ch }

        query_ch.info.set { sample_info }
        query_ch.min_len_scaffolds.set { min_len_lookup }
        query_ch.min_len_summary.set { min_len_summary }

        // Coverage Input Channel
        input
            // cross with sample info
            .cross(sample_info)
            .map{ it ->
                tuple(
                    it[0][0],                                                   // ID
                    it[0][1],                                                   // trimmed reads in
                    it[1][1],                                                   // assembly
                    it[1][2],                                                   // topology
                    it[1][3],                                                   // assemble opts dummy var
                )
            }
            .set { coverage_in }

        // Coverage
        coverage_userAsmb(coverage_in).set { coverage_out }

        COVERAGE_userAsmb_WRITE(coverage_out, min_len_lookup, min_len_summary)

    emit:
        blast_in = COVERAGE_userAsmb_WRITE.out.blast_in
}


// No-reads variant: user-provided assembly, no raw data. Samples come straight
// from the DB (no PREPROCESS) and coverage() runs in its no-reads mode, deriving
// GC from the sequence and leaving depth/error stats empty.
workflow COVERAGE_userAsmb_noReads {
    main:
        channel.fromQuery(params.sqlRead, db: 'sqlite')
            .multiMap { it ->
                info: tuple(
                    it[0],                                          // ID
                    file(params.asmbDir + "/" + it[1]),             // assembly
                    it[2],                                          // topology
                    it[3]                                          // assemble opts dummy var
                )
                min_len_scaffolds: tuple(it[0], it[4] == null ? 500 : (it[4] as Integer))
                min_len_summary:   tuple(it[0], it[4] == null ? 500 : (it[4] as Integer))
            }
            .set { query_ch }

        query_ch.info.set { sample_info }
        query_ch.min_len_scaffolds.set { min_len_lookup }
        query_ch.min_len_summary.set { min_len_summary }

        // No reads to cross in; feed the assembly straight to the no-reads process
        sample_info
            .map{ it -> tuple(it[0], it[1], it[2], it[3]) }
            .set { coverage_in }

        coverage_userAsmb_noReads(coverage_in).set { coverage_out }

        COVERAGE_userAsmb_WRITE(coverage_out, min_len_lookup, min_len_summary)

    emit:
        blast_in = COVERAGE_userAsmb_WRITE.out.blast_in
}
