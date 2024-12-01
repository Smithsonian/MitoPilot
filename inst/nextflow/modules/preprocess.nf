process preprocess {

    executor params.preprocess.executor
    container params.preprocess.container

    publishDir "$launchDir/${params.publishDir}", overwrite: true, pattern: "${id}/${id}_preprocess.json", mode: 'copy'

    errorStrategy 'finish'
    cpus {opts.cpus}
    memory {opts.memory.GB}

    tag "${id}"

    input:
    tuple val(id), path(R1), path(R2), val(opts)

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
    fastp --in1 !{R1} --in2 !{R2} --out1 !{out1} --out2 !{out2} --json !{json_out} --thread !{task.cpus} !{opts.fastp}
    before=$(jq '.summary.before_filtering.total_reads' !{json_out})
    after=$(jq '.summary.after_filtering.total_reads' !{json_out})
    meanLen=$(jq '.summary.after_filtering.read1_mean_length' !{json_out})
    '''

}


