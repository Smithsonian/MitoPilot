include {coverage_userAsmb} from './coverage_userAsmb.nf'

params.sqlRead =  'SELECT s.ID, s.assembly, s.topology, ' +
                  'a.assemble_opts ' +
                  'FROM samples s ' +
                  'JOIN assemble a ' +
                  'ON s.ID = a.ID ' +
                  'WHERE a.assemble_switch = 1 AND a.assemble_lock = 0'


params.sqlWrite =   'UPDATE assemblies SET depth = ?, gc = ?, errors = ?, time_stamp = ? ' +
                    'WHERE ID=? and path=? and scaffold=?'

params.sqlDeleteAssemblies =  'DELETE FROM assemblies WHERE ID = ? AND time_stamp != ?'

params.sqlWriteAssemblies = 'INSERT OR REPLACE INTO assemblies ' +
                            '(ID, path, scaffold, length, topology, time_stamp, sequence, ignore, edited) ' +
                            'VALUES (?, ?, ?, ?, ?, ?, ?, ?, 0)'

params.sqlWriteAssemble =   'UPDATE assemble SET paths=?, scaffolds=?, length=?, topology=?, ' +
                            'assemble_switch=?, assemble_notes=?, time_stamp=? WHERE ID=?'


workflow COVERAGE_userAsmb {
    take:
        input

    main:
        // sample info channel from DB
        channel.fromQuery(params.sqlRead, db: 'sqlite')
            .map{ it ->
                tuple(
                    it[0],                                          // ID
                    file(params.asmbDir + "/" + it[1]),             // assembly
                    it[2],                                          // topology
                    it[3]                                          // assemble opts dummy var
                )
            }
            .set { sample_info }

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
                    record.desc,                        // topology
                    params.ts,                          // time stamp
                    record.seqString                    // sequence
                ).flatten()
            }
            .map { it ->                                            // add ignore flag for short assemblies
                if(it[3] < params.minAssemblyLength){
                    it[7] = 1
                }else{
                    it[7] = 0
                }
                return it
            }
            .set { assemblies_ch }
        assemblies_ch.sqlInsert( statement: params.sqlWriteAssemblies, db: 'sqlite')

        // Update DB assemble table
        assemblies_ch
            // Add summary stats
            .map { it ->
                tuple(
                    it[0],                                          // ID
                    it[1].toInteger(),                              // paths
                    it[2].toInteger(),                              // scaffold
                    it[3].toInteger(),                              // length
                    it[4]                                           // topology
                )
            }
            .groupTuple()
            .map { it ->
                tuple(
                    it[1].max(),                                    // # paths
                    it[2].max(),                                    // # scaffolds
                    it[3].unique().sort().reverse().join(";"),      // length(s)
                    it[4].unique().sort().join(";"),                // topology(s)
                    '2',                                            // assembly status
                    '',                                             // assembly notes
                    params.ts,                                      // time stamp
                    it[0]                                           // ID
                ).flatten()
            }
            .map { it ->
                if(it[1] > 1){                      // mark fragmented assemblies
                    it[2] = 'fragmented'
                    it[4] = '3'
                    it[5] = 'Output contains disconnected contigs'
                }
                if(it[0] > 1){                      // mark unresolved assemblies
                    it[4] = '3'
                    it[5] = 'Unable to resolve single assembly from reads'
                }
                return it
            }
            .sqlInsert(statement: params.sqlWriteAssemble , db: 'sqlite')

    emit:
        // tuple(id, assembly, opts_id) for single-contig BLAST search
        blast_in = coverage_out.map{ it -> tuple(it[2], it[3], it[4]) }

}
