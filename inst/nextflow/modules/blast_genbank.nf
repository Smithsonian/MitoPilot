process blast_genbank {

    executor params.blast_gb.executor
    container params.blast_gb.container

    cpus { (params.blast_gb.cpus instanceof Integer) ? params.blast_gb.cpus : 1 }
    memory = (params.blast_gb.memory instanceof Number) ? "${params.blast_gb.memory}.GB" : null
    clusterOptions {
        def opts = [
            (params.blast_gb.executor == 'sge') ? '-S /bin/bash' : '',
            (params.blast_gb.clusterOptions instanceof String) ? params.blast_gb.clusterOptions : ''
        ].findAll { it }.join(' ')
        opts ?: null
    }

    publishDir "${launchDir}/${params.publishDir}", overwrite: true, mode: 'copy'

    errorStrategy 'ignore'

    tag "${id}.${path_idx}"

    input:
        tuple val(id), val(path_idx), path(assembly), val(opts_id), val(entrez_query), val(extra_opts)

    output:
        tuple val(id), val(path_idx), path("${outDir}/${outFile}")

    shell:
    outDir = "${id}/assemble/${opts_id}"
    outFile = "blast_genbank_${path_idx}.txt"
    '''
    mkdir -p !{outDir}
    blastn \
        -remote \
        -db nt \
        -query !{assembly} \
        -outfmt "6 qseqid saccver stitle pident qcovs evalue" \
        -max_target_seqs 1 \
        -max_hsps 1 \
        -task megablast \
        -entrez_query "!{entrez_query}" \
        !{extra_opts} \
        > !{outDir}/!{outFile}
    '''
}
