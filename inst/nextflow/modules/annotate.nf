process annotate {
    
    executor params.annotate.executor
    container params.annotate.container
    
    publishDir params.publishDir, overwrite: true, mode: 'copy'

    errorStrategy 'finish'
    cpus {params.annotate.cpus}
    memory {params.annotate.memory.GB}

    tag "${opts.assemblyID}"

    input:
        tuple val(ID), path(assembly), path(coverage), val(opts)

    output:
    // tuple val(opts.ID), val(opts.target), val(opts.hash), val(opts.path), env(time_stamp),
    //     path("${opts.ID}/${opts.target}/${opts.hash}/annotations/${opts.assemblyID}.annotations.tab")
    // path("${opts.ID}/${opts.target}/${opts.hash}/annotations/${opts.assemblyID}.annotations.fasta")
    // path("${opts.ID}/${opts.target}/${opts.hash}/annotations/${opts.assemblyID}.fasta")
    // path("${opts.ID}/${opts.target}/$ccsdfdsf{opts.hash}/annotations/${opts.assemblyID}.coverageStats.csv")
    // path("${opts.ID}/${opts.target}/${opts.hash}/annotations/${opts.assemblyID}.blastp*")
    // path("${opts.ID}/${opts.target}/${opts.hash}/annotations/${opts.assemblyID}.log")

    shell:
    dir = "${ID}/annotate"
    """
    mkdir -p ${dir}
    mito_annotate.R \
        --id ${opts.assemblyID} \
        --fasta ${fasta_working} \
        --coverage ${coverage} \
        --topology ${opts.topology} \
        --outdir ${dir} \
        --seqids ${opts.seqids} \
        --cpus ${task.cpus} \
        --refDB ${opts.refDB} \
        --time_stamp \${time_stamp} \
        > ${dir}/${opts.assemblyID}.log
    """

}