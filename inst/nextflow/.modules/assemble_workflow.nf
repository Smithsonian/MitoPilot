include {assemble} from './assemble.nf'

params.sqlRead =  'SELECT a.ID, a.assemble_opts, opts.cpus, opts.memory, opts.seeds_db, opts.labels_db, opts.getOrganelle ' +
                  'FROM assemble a ' +
                  'JOIN assemble_opts opts ON a.assemble_opts = opts.assemble_opts ' +
                  'WHERE a.assemble_switch = 1 AND a.lock = 0 '




params.sqlWriteAssemblies = 'INSERT OR REPLACE INTO assemblies (ID, target, hash, path, scaffold, length, topology, sequence, ignore, edited) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, 0)'
params.sqlWriteAssemble = 'UPDATE assemble SET paths=?, scaffolds=?, length=?, topology=?, assemble_switch=?, assembly_notes=? WHERE ID=? and target=? and hash=?'
params.sqlWriteAssembleTS = 'UPDATE assemble SET time_stamp = ? WHERE ID = ? and target = ? and hash = ?'

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
                        seeds_db: it[4],                                        // getOrganelle seeds
                        labels_db: it[5],                                       // getOrganelle labels
                        getOrganelle: it[6]                                     // getOrganelle options
                    ]
                )
            }
            .set { assemble_opts }
assemble_opts.view()
        // // Assemble Input Channel
        // input
        //     // filter on min seq depth
        //     .filter{
        //         try {
        //             it[2].toInteger() >= params.minDepth
        //         } catch (Exception e) {
        //             return false
        //         }
        //     }
        //     // cross with assembly options
        //     .cross(assemble_opts)
        //     .map{ it ->
        //         tuple(
        //             it[0][0],                                                   // ID
        //             it[1][1],                                                   // assembly options id
        //             it[0][1],                                                   // trimmed reads in
        //             it[1][2],                                                   // assembly options
        //         )
        //     }
        //     .take(params.assemble.take) // optional subsetting for development
        //     .set { assemble_in }

        // // Assemble
        // assemble(assemble_in).set { assemble_out }

        // // Update DB with assemblies
        // assemble_out[0]
        //     .filter{ it[3] ==~ /^(?!.*assembly_0\.fasta$).*$/ }                 // exclude empty assembly files
        //     .map { it -> it[3] }.flatten()
        //     .splitFasta(record: [id: true, desc: true, seqString: true])
        //     .map { record ->
        //         tuple(
        //             record.id.split('\\.'),                        // ID, target, hash, # paths, # scaffolds
        //             record.seqString.length(),                     // length
        //             record.desc.split('-'),                        // topology
        //             record.seqString                               // sequence
        //         ).flatten()
        //     }
        //     .map { it ->                                            // add ignore flag for short assemblies
        //         if(it[5] < params.minAssemblyLength[it[1]]){
        //             it[8] = 1
        //         }else{
        //             it[8] = 0
        //         }
        //         return it
        //     }
        //     .set { assemblies_ch }
        // assemblies_ch.sqlInsert( statement: params.sqlWriteAssemblies, db: 'sqlite')

        // // Update DB assemble table
        // assemblies_ch
        //     // Add summary stats
        //     .map { it ->
        //         tuple(
        //             it[0]+'-'+it[1]+'-'+it[2],                      // ID-target-hash (concatenate for grouping)
        //             it[3].toInteger(),                                          // # paths
        //             it[4].toInteger(),                                          // # scaffolds
        //             it[5].toInteger(),                                          // length
        //             it[6]                                           // topology
        //         )
        //     }
        //     .groupTuple()
        //     .map { it ->
        //         tuple(
        //             it[1].max(),                                    // # paths
        //             it[2].max(),                                    // # scaffolds
        //             it[3].unique().sort().reverse().join(";"),      // length(s)
        //             it[4].unique().sort().join(";"),                // topology(s)
        //             '2',                                            // assembly status
        //             '',                                             // assembly notes
        //             it[0].split('-')                                // ID, target, hash (split back out)
        //         ).flatten()
        //     }
        //     .map { it ->                                // mark fragmented assemblies
        //         if(it[1] > 1){
        //             it[3] = 'fragmented'
        //             it[4] = '3'
        //             it[5] = 'Output contains distonnected contigs'
        //         }
        //         return it
        //     }
        //     .map { it ->                                // mark unresolved assemblies
        //         if(it[0] > 1){
        //             it[4] = '3'
        //             it[5] = 'Unable to resolve single assembly from reads'
        //         }
        //         return it
        //     }
        //     .sqlInsert(statement: params.sqlWriteAssemble , db: 'sqlite')

        // // Clear db records for failed assemblies
        // assemble_out[0]
        //     .filter{ it[3] ==~ /(.*assembly_0\.fasta)$/ }
        //     .set { failed_assemblies }
        // failed_assemblies
        //     .map { it ->
        //         tuple(
        //             null, null, null, null, '3',
        //             it[0],                  // ID
        //             it[1],                  // target
        //             it[2]                   // hash
        //         )
        //     }
        //     .sqlInsert(statement: 'UPDATE assemble SET paths=?, scaffolds=?, length=?, topology=?, assemble_switch=? WHERE ID=? and target=? and hash=?' , db: 'sqlite')
        // failed_assemblies
        //     .map { it ->
        //         tuple(
        //             it[0],                  // ID
        //             it[1],                  // target
        //             it[2]                   // hash
        //         )
        //     }
        //     .sqlInsert(statement: 'DELETE FROM assemblies WHERE ID = ? AND target = ? AND hash = ?' , db: 'sqlite')

        // // Mark samples with too few reads
        // input
        //     // filter on min seq depth
        //     .filter{
        //         try {
        //             it[2].toInteger() < params.minDepth
        //         } catch (Exception e) {
        //             return false
        //         }
        //     }.cross(assemble_opts)
        //     .map { it ->
        //         tuple(
        //             '3',                                        // assemble state
        //             'Insufficient sequencing depth',            // assemble notes
        //             it[0][0],                                   // ID
        //             it[1][1]                                    // target
        //         )
        //     }
        //     .sqlInsert(statement: 'UPDATE assemble SET assemble_switch=?, assembly_notes=? WHERE ID=? and target=?' , db: 'sqlite')

        // // Add time stamp to assemble table
        // assemble_out[0]
        //     .map { it ->
        //         tuple(
        //             it[6],                                          // time stamp
        //             it[0],                                          // ID
        //             it[1],                                          // target
        //             it[2]                                           // hash
        //         )
        //     }
        //     .sqlInsert(statement: params.sqlWriteAssembleTS , db: 'sqlite')

        // emit:
        //     ch = assemble_out[0]

}
