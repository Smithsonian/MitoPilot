import java.util.Base64
include {curate} from './curate.nf'

params.sqlRead =    'SELECT DISTINCT a.ID, a.path, c.curate_opts, ' +
                    'd.cpus, d.memory, d.target, d.params, d.max_blast_hits ' +
                    'd.ref_db, d.ref_dir ' +
                    'FROM assemblies a ' +
                    'JOIN assemble b ON a.ID = b.ID ' +
                    'JOIN annotate c ON a.ID = c.ID ' +
                    'JOIN curate_opts d ON c.curate_opts = d.curate_opts ' +
                    'WHERE c.annotate_switch = 1 AND c.annotate_lock = 0 AND b.assemble_lock = 1 AND a.ignore = 0'

params.sqlWriteAssemblies =  'UPDATE assemblies SET sequence = ?, length = ?, depth = ?, gc = ?, errors = ?, time_stamp = ? ' +
                            'WHERE ID=? and path=? and scaffold=?'

params.sqlWriteAnnotate =   'UPDATE annotate SET path = ?, scaffolds = ?, topology = ?, length = ?, time_stamp = ? WHERE ID = ?'

workflow CURATE {
    take:
        input

    main:

        channel.fromQuery(params.sqlRead, db: 'sqlite')
            .join(input, by: [0, 1])
            .map { it ->
                def jsonParams = it[6].toString()
                def encodedParams = Base64.encoder.encodeToString(jsonParams.bytes)

                tuple(
                    it[0],                                          // ID
                    it[1],                                          // path
                    it[10],                                          // Annotations
                    it[11],                                          // Assembly
                    it[12],                                          // Coverage
                    [
                        cpus:  it[3],                                      // cpus
                        memory: it[4],                                     // memory
                        target: it[5],                                     // target
                        params: encodedParams,                              // params
                        max_blast_hits: it[7]                             // maximum retained blast hits
                    ],
                    file(it[8] + "/" + it[9]),                              // curation ref dir + clade
                    val(it[9])                                              // ref clade
                )
            }
            .set { curate_in }

        curate(curate_in).set { curate_out }

        // Update assemblies table
        curate_out
            .flatten()
            .filter{ it =~ /(.*_coverageStats_.*.csv)$/ }
            .splitCsv(header: true, sep: ',')
            .map { it ->
                tuple(
                    it.SeqId,
                    it.Call,
                    it.MeanDepth,
                    it.GC,
                    it.ErrorRate
                )
            }
            .groupTuple()
            .map { it ->
                def sequence = it[1].join('')
                tuple(
                    sequence,                           // Assembly sequence
                    sequence.size(),                    // sequence length
                    it[2].join(' '),                    // mean depth
                    it[3].join(' '),                    // gc
                    it[4].join(' '),                    // error rate
                    params.ts,                          // timestamp
                    it[0].split('\\.')                  // id, path, scaffold
                ).flatten()
            }
            .sqlInsert(statement: params.sqlWriteAssemblies, db: 'sqlite')

        // Update assemble table
        curate_out
            .map { it ->
                def ID = it[0]
                def path = it[1]
                def seqs = file(it[3]).splitFasta(record: [seqString: true, desc: true])
                def scaffolds = seqs.size()
                def length = seqs.collect { record -> record.seqString.size() }.sum()
                def topology = seqs.collect { record -> record.desc }.join(';')
                tuple(
                    path,
                    scaffolds,
                    topology,
                    length,
                    time_stamp = params.ts,
                    ID
                )
            }
            .sqlInsert(statement: params.sqlWriteAnnotate, db: 'sqlite')

    emit:
           ch = curate_out[0]

}
