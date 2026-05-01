include {annotate} from './annotate.nf'

params.sqlRead =    'SELECT DISTINCT a.ID, a.path, b.assemble_opts, ' +
                        'd.cpus, d.memory, d.ref_db, d.ref_dir, d.mitos_opts, d.use_mitos_best, d.trnaScan_opts, d.start_gene, d.arwen_opts, d.use_arwen, d.aragorn_opts, d.use_aragorn ' +
                    'FROM assemblies a ' +
                    'JOIN assemble b ON a.ID = b.ID ' +
                    'JOIN annotate c ON a.ID = c.ID ' +
                    'JOIN annotate_opts d ON c.annotate_opts = d.annotate_opts ' +
                    'WHERE c.annotate_switch = 1 AND c.annotate_lock = 0 AND b.assemble_lock = 1 AND a.ignore = 0'

workflow ANNOTATE {

    channel.fromQuery(params.sqlRead, db: 'sqlite')
        .map{ it ->

            // Check if refDir is a GitHub link
            if (it[6].contains('githubusercontent')) {
                if (!it[5].endsWith('.tar.gz')) {
                    it[5] = it[5] + '.tar.gz'
                }
            }

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
                    use_mitos_best: it[8],                             // use_mitos_best toggle
                    trnaScan: it[9],                                   // trnaScan_opts
                    start_gene: it[10],                                // starting gene for rotation
                    arwen: it[11],                                     // arwen_opts
                    use_arwen: it[12],                                 // use_arwen toggle
                    aragorn: it[13],                                   // aragorn_opts
                    use_aragorn: it[14]                                // use_aragorn toggle
                ],
                file(it[6] + "/" + it[5]),                              // curation ref dir + clade
                it[5].replaceFirst(/\.tar\.gz$/, '')                // ref_db without ".tar.gz"
            )
        }
        .set { annotate_in }

    annotate(annotate_in).set { annotate_out }

    emit:
           ch = annotate_out[0]

}
