params.sqlRead =    'SELECT t.ID, t.R1, t.R1, t.cpus, t.memory, t.fastp ' +
                    'FROM preprocess t ' +
                    'WHERE EXISTS (SELECT 1 FROM assemble a WHERE t.ID = a.ID AND a.assemble_switch = 1)'

params.sqlWrite = 'UPDATE preprocess SET reads = ?, trimmed_reads = ?, mean_length = ?, time_stamp = ? WHERE ID = ?'

process preprocess {

    executor params.preprocess.executor
    container params.preprocess.container

    publishDir params.publishDir, overwrite: true, pattern: "${id}/${id}_preprocess.json", mode: 'copy'

    cpus opts.cpus
    memory opts.memory

    tag "${id}"

    input:
    tuple val(id), path(fwd), path(rev), val(opts)

    output:
    tuple val("${id}"), path("${id}/${id}_preprocess_*"), env(after)                          // output for processing
    tuple env(before), env(after), env(meanLen), val("${params.ts}"), val("${id}")            // output for DB
    path("${id}/${id}_preprocess.json")                                                       // outout to publishDir

    shell:
    json_out = "${id}/${id}_preprocess.json"
    out1 = "${id}/${id}_preprocess_R1.fastq.gz"
    out2 = "${id}/${id}_preprocess_R2.fastq.gz"
    '''
    mkdir !{id}
    fastp --in1 !{fwd} --in2 !{rev} --out1 !{out1} --out2 !{out2} --json !{json_out} --thread !{task.cpus} !{opts.fastp}
    before=$(jq '.summary.before_filtering.total_reads' !{json_out})
    after=$(jq '.summary.after_filtering.total_reads' !{json_out})
    meanLen=$(jq '.summary.after_filtering.read1_mean_length' !{json_out})
    '''

}

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
                        cpus: it[3],                                            // cpus
                        memory: it[4],                                          // memory
                        fastp: it[5]                                            // fastp args
                    ]
                )
            }
            .take(params.trim.take) // optional subsetting for development
            .set { preprocess_in }

        // Run trim
        preprocess(preprocess_in).set { preprocess_out }

        // Update DB
        preprocess_out[1]
            .sqlInsert(statement: params.sqlWrite, db: 'sqlite')

    emit:
        ch = preprocess_out[0]

}
