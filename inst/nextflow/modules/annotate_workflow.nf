include {annotate} from './annotate.nf'

params.sqlRead =    'SELECT a.ID, a.path, b.assemble_opts, ' +
                        'd.cpus, d.memory, d.ref_db, d.ref_dir, d.mitos_opts, d.use_mitos_best, d.trnaScan_opts, d.start_gene, d.arwen_opts, d.use_arwen, d.aragorn_opts, d.use_aragorn, ' +
                        'd.use_mitofinder, d.mitofinder_db, d.mitofinder_new_genes, d.mitofinder_allow_introns, d.mitofinder_opts, ' +
                        "GROUP_CONCAT(CASE WHEN a.ignore = 1 THEN a.scaffold END, ',') AS ignore_scaffolds, " +
                        'd.coverage_trim, d.retain_low_conf_trna, d.use_mitos, d.use_trnaScan ' +
                    'FROM assemblies a ' +
                    'JOIN assemble b ON a.ID = b.ID ' +
                    'JOIN annotate c ON a.ID = c.ID ' +
                    'JOIN annotate_opts d ON c.annotate_opts = d.annotate_opts ' +
                    'WHERE c.annotate_switch = 1 AND c.annotate_lock = 0 AND b.assemble_lock = 1 ' +
                    'GROUP BY a.ID, a.path, b.assemble_opts, d.cpus, d.memory, d.ref_db, d.ref_dir, d.mitos_opts, d.use_mitos_best, d.trnaScan_opts, d.start_gene, d.arwen_opts, d.use_arwen, d.aragorn_opts, d.use_aragorn, d.use_mitofinder, d.mitofinder_db, d.mitofinder_new_genes, d.mitofinder_allow_introns, d.mitofinder_opts, d.coverage_trim, d.retain_low_conf_trna, d.use_mitos, d.use_trnaScan ' +
                    'HAVING SUM(CASE WHEN a.ignore = 0 THEN 1 ELSE 0 END) > 0'

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
                    use_aragorn: it[14],                               // use_aragorn toggle
                    use_mitofinder: it[15] != null ? it[15] as Integer : 0,  // use_mitofinder toggle (default off)
                    mitofinder_new_genes: it[17] != null ? it[17] as Integer : 0,   // --new-genes toggle
                    mitofinder_allow_introns: it[18] != null ? it[18] as Integer : 0, // --allow-intron toggle
                    mitofinder_opts: it[19] ?: '',                     // free-form MitoFinder options
                    ignore_scaffolds: it[20] ?: '',                    // comma-separated scaffold numbers to drop
                    coverage_trim: it[21] != null ? it[21] as Integer : 1,  // coverage trimming toggle (default on)
                    retain_low_conf_trna: it[22] != null ? it[22] as Integer : 0,  // retain low-conf (NNN) tRNAs (default off)
                    use_mitos: it[23] != null ? it[23] as Integer : 1,      // use_mitos toggle (default on)
                    use_trnaScan: it[24] != null ? it[24] as Integer : 1    // use_trnaScan toggle (default on)
                ],
                file(it[6] + "/" + it[5]),                              // curation ref dir + clade
                it[5].replaceFirst(/\.tar\.gz$/, ''),               // ref_db without ".tar.gz"
                file((it[16] != null && it[16].toString().trim()) ? it[16] : "${projectDir}/assets/NO_FILE")  // MitoFinder reference .gb (or placeholder)
            )
        }
        .set { annotate_in }

    annotate(annotate_in).set { annotate_out }

    emit:
           ch = annotate_out[0]

}
