include {annotate} from './annotate.nf'

params.sqlRead =    'SELECT DISTINCT a.ID, a.path, b.assemble_opts, ' +
                        'd.cpus, d.memory, d.ref_db, d.ref_dir, d.mitos_opts, d.trnaScan_opts, d.start_gene ' +
                    'FROM assemblies a ' +
                    'JOIN assemble b ON a.ID = b.ID ' +
                    'JOIN annotate c ON a.ID = c.ID ' +
                    'JOIN annotate_opts d ON c.annotate_opts = d.annotate_opts ' +
                    'WHERE c.annotate_switch = 1 AND c.annotate_lock = 0 AND b.assemble_lock = 1 AND a.ignore = 0'

workflow ANNOTATE {

    channel.fromQuery(params.sqlRead, db: 'sqlite')
        .map{ it ->
            tuple(
                it[0],                                          // ID
                it[1],                                          // path
                file(                                           // Assembly
                    params.publishDir + '/' +
                    it[0] + '/assemble/' + it[2] + '/' +
                    it[0] + '_assembly_' + it[1] + '.fasta'
                ),
                file(                                           // Coverage
                    params.publishDir + '/' +
                    it[0] + '/assemble/' + it[2] + '/' +
                    it[0] + '_assembly_' + it[1] + '_coverageStats.csv'
                ),
                [
                    cpus:  it[3],                                      // cpus
                    memory: it[4],                                     // memory
                    ref_db: it[5],                                     // mitos_ref_db
                    ref_dir: it[6],                                    // mitos_ref_dir
                    mitos: it[7],                                      // mitos_opts
                    trnaScan: it[8],                                    // trnaScan_opts
                    start_gene: it[9]                                  // starting gene for rotation
                ],
                file(it[6] + "/" + it[5])                                   // MITOS ref dir + clade
            )
        }
        .set { annotate_in }

    annotate(annotate_in).set { annotate_out }

    emit:
           ch = annotate_out[0]

}
