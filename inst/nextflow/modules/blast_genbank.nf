process blast_genbank {

    executor params.blast_gb.executor
    container params.blast_gb.container

    publishDir "${launchDir}/${params.publishDir}", overwrite: true, mode: 'copy'

    errorStrategy 'ignore'

    tag "${id}"

    input:
        tuple val(id), path(assembly), val(opts_id)

    output:
        tuple val(id), path("${outDir}/blast_genbank.txt")

    shell:
    outDir = "${id}/assemble/${opts_id}"
    // use entrez filter to only retain mitochondrial hits
    '''
    mkdir -p !{outDir}
    blastn \
        -remote \
        -db nt \
        -query !{assembly} \
        -outfmt "6 saccver stitle pident qcovs" \
        -max_target_seqs 1 \
        -max_hsps 1 \
        -task megablast \
        -entrez_query "mitochondrion[Location]" \
        > !{outDir}/blast_genbank.txt
    '''
}
