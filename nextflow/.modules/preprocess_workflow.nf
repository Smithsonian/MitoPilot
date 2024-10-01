include { preprocess } from './.modules/preprocess.nf'

// Database interactions
params.sqlRead =    'SELECT p.ID, p.R1, p.R2, opts.cpus, opts.memory, opts.fastp ' +
                    'FROM preprocess p ' +
                    'JOIN pre_opts opts ON p.pre_opts = opts.pre_opts ' +
                    'WHERE EXISTS (SELECT 1 FROM assemble a WHERE p.ID = a.ID AND a.assemble_switch = 1)'

params.sqlWrite = 'UPDATE preprocess SET reads = ?, trimmed_reads = ?, mean_length = ?, time_stamp = ? WHERE ID = ?'

workflow PREPROCESS {
    main:
        // Prepare input
        channel.fromQuery(params.sqlRead, db: 'sqlite')
            .map{ it ->
                tuple(
                    it[0],                                                      // ID
                    file(params.rawDir + "/" + it[1]),                          // fwd
                    file(params.rawDir + "/" + it[2]),                          // rev
                    [
                      cpus: it[3],                                              // cpus
                      memory: it[4],                                            // memory
                      fastp: it[5]                                              // fastp args
                    ]
                )
            }
            .take(params.preprocess.take) // optional subsetting for development
            //.view()
            .set { preprocess_in }

        // Run trim
        preprocess(preprocess_in).set { preprocess_out }

        // Update DB
        preprocess_out[1]
            .sqlInsert(statement: params.sqlWrite, db: 'sqlite')

    emit:
        ch = preprocess_out[0]

}
