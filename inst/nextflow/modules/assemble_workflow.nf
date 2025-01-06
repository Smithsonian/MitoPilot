include {assemble} from './assemble.nf'

params.sqlRead =  'SELECT a.ID, a.assemble_opts, opts.cpus, opts.memory, ' +
                  'opts.seeds_db, opts.labels_db, opts.getOrganelle ' +
                  'FROM assemble a ' +
                  'JOIN assemble_opts opts ' +
                  'ON a.assemble_opts = opts.assemble_opts ' +
                  'WHERE a.assemble_switch = 1 AND a.assemble_lock = 0'

params.sqlDeleteAssemblies =  'DELETE FROM assemblies WHERE ID = ? AND time_stamp != ?'

params.sqlWriteAssemblies = 'INSERT OR REPLACE INTO assemblies ' +
                            '(ID, path, scaffold, length, topology, time_stamp, sequence, ignore, edited) ' +
                            'VALUES (?, ?, ?, ?, ?, ?, ?, ?, 0)'

params.sqlWriteAssemble =   'UPDATE assemble SET paths=?, scaffolds=?, length=?, topology=?, ' +
                            'assemble_switch=?, assemble_notes=?, time_stamp=? WHERE ID=?'



workflow ASSEMBLE {
    take:
        input

    main:
        // Assembly Options Channel from DB
        channel.fromQuery(params.sqlRead, db: 'sqlite')
            .map{ it ->
                tuple(
                    it[0],                                                      // ID
                    it[1],                                                      // options id
                    [                                                           //## assembly options ##//
                        cpus: it[2],                                            // cpus
                        memory: it[3],                                          // memory
                        seeds_db: file(it[4]),                                        // getOrganelle seeds
                        labels_db: file(it[5]),                                       // getOrganelle labels
                        getOrganelle: it[6]                                     // getOrganelle options
                    ]
                )
            }
            .set { assemble_opts }

        channel.fromQuery(params.sqlRead, db: 'sqlite')
            .map{ it ->
                tuple(
                    [                                                           //## getOrganelle dbs ##//
                        seeds_db: file(it[4]),                                        // getOrganelle seeds
                        labels_db: file(it[5]),                                       // getOrganelle labels
                    ]
                )
            }
            .set { getOrg_dbs }

        // Assemble Input Channel
        input
            // filter on min seq depth
            .filter{
                try {
                    it[2].toInteger() >= params.minDepth
                } catch (Exception e) {
                    return false
                }
            }
            // cross with assembly options
            .cross(assemble_opts)
            .map{ it ->
                tuple(
                    it[0][0],                                                   // ID
                    it[1][1],                                                   // assembly options id
                    it[0][1],                                                   // trimmed reads in
                    it[1][2],                                                   // assembly options
                )
            }
            .set { assemble_in }

        // Assemble
        assemble(assemble_in).set { assemble_out }

        // Clear old assemblies from db
        assemble_out[0]
          .map { it ->
            tuple(
              it[0],
              params.ts
            )
          }
          .sqlInsert( statement: params.sqlDeleteAssemblies, db: 'sqlite')

        // Write to assemblies table
        assemble_out[0]
            .filter{ it[1] ==~ /^(?!.*assembly_0\.fasta$).*$/ }         // exclude empty assemblies
            .map { it -> it[1] }.flatten()
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
                    it[5] = 'Output contains distonnected contigs'
                }
                if(it[0] > 1){                      // mark unresolved assemblies
                    it[4] = '3'
                    it[5] = 'Unable to resolve single assembly from reads'
                }
                return it
            }
            .sqlInsert(statement: params.sqlWriteAssemble , db: 'sqlite')

        // Update assemble table for failed assemblies
        assemble_out[0]
            .filter{ it[1] ==~ /(.*assembly_0\.fasta)$/ }
            .map { it ->
                tuple(
                    null,                   // # paths
                    null,                   // # scaffolds
                    null,                   // length(s)
                    null,                   // topology(s)
                    '3',                    // assembly status
                    'failed assembly',      // assembly notes
                    params.ts,              // time stamp
                    it[0]                   // ID
                )
            }
            .sqlInsert(statement: params.sqlWriteAssemble , db: 'sqlite')

        // Mark samples with too few reads
        input
            // filter on min seq depth
            .filter{
                try {
                    it[2].toInteger() < params.minDepth
                } catch (Exception e) {
                    return false
                }
            }.cross(assemble_opts)
            .map { it ->
                tuple(
                    null,                                     // # paths
                    null,                                     // # scaffolds
                    null,                                     // length(s)
                    null,                                     // topology(s)
                    '3',                                      // assembly status
                    'Insufficient sequencing depth',          // assembly notes
                    params.ts,                                // time stamp
                    it[0][0]                                  // ID
                )
            }
            .sqlInsert(statement: params.sqlWriteAssemble , db: 'sqlite')

        // Reset annotation data for updated assemblies
        assemble_out[0]
          .map { it ->
            tuple(
              it[0]
            )
          }
          .set { update_ids }
        channel.fromQuery('SELECT ID, annotate_opts, curate_opts FROM annotate;', db: 'sqlite')
            .join(update_ids)
            .sqlInsert(statement: 'INSERT OR REPLACE INTO annotate (ID, annotate_opts, curate_opts, annotate_switch, annotate_lock, reviewed) VALUES (?, ?, ?, 1, 0, "no")', db: 'sqlite')

        emit:
           ch = assemble_out[0]

}
