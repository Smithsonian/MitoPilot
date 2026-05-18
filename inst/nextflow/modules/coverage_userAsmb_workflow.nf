include {coverage_userAsmb} from './coverage_userAsmb.nf'

params.sqlRead =  'SELECT s.ID, s.assembly, s.topology, ' +
                  'a.assemble_opts, opts.min_assembly_length ' +
                  'FROM samples s ' +
                  'JOIN assemble a ON s.ID = a.ID ' +
                  'JOIN assemble_opts opts ON a.assemble_opts = opts.assemble_opts ' +
                  'WHERE a.assemble_switch IN (1, 4) AND a.assemble_lock = 0'


params.sqlWrite =   'UPDATE assemblies SET depth = ?, gc = ?, errors = ?, time_stamp = ? ' +
                    'WHERE ID=? and path=? and scaffold=?'

params.sqlDeleteAssemblies =  'DELETE FROM assemblies WHERE ID = ? AND time_stamp != ?'

params.sqlWriteAssemblies = 'INSERT OR REPLACE INTO assemblies ' +
                            '(ID, path, scaffold, length, length_raw, topology, time_stamp, sequence, ignore, edited) ' +
                            'VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, 0)'

params.sqlWriteAssemble =   'UPDATE assemble SET paths=?, scaffolds=?, length=?, topology=?, ' +
                            'assemble_switch=?, assemble_notes=?, time_stamp=? WHERE ID=?'


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

        // Coverage output
        coverage_out
            .flatten()
            .filter{ it =~ /(.*coverageStats.csv)$/ }
            .splitCsv(header: true, sep: ',')
            .take(2)
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
                    topo_str = 'fragmented'
                    status   = '3'
                    notes    = 'Output contains disconnected contigs'
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
