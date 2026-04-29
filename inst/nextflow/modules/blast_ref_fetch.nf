process blast_ref_fetch {

    executor params.blast_gb.executor
    container params.blast_gb.container

    cpus { (params.blast_gb.cpus instanceof Integer) ? params.blast_gb.cpus : 1 }
    memory = (params.blast_gb.memory instanceof Number) ? "${params.blast_gb.memory}.GB" : null
    clusterOptions = (params.blast_gb.clusterOptions instanceof String) ? params.blast_gb.clusterOptions : null

    errorStrategy 'ignore'

    tag "${id}"

    input:
        tuple val(id), val(blast_accession), val(opts_id)

    output:
        tuple val(id), path("${outDir}/blast_ref_annotations.csv")

    shell:
    outDir = "${id}/assemble/${opts_id}"
    '''
    mkdir -p !{outDir}
    Rscript -e "MitoPilot::fetch_blast_ref('!{blast_accession}', '!{outDir}/blast_ref_annotations.csv')"
    '''
}
