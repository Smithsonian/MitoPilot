process blast_ref_fetch {

    executor params.blast_gb.executor
    container params.blast_gb.container

    publishDir "${launchDir}/${params.publishDir}", overwrite: true, pattern: "${id}/assemble/${opts_id}/remote_blast_ref.json", mode: 'copy'

    cpus { (params.blast_gb.cpus instanceof Integer) ? params.blast_gb.cpus : 1 }
    memory = (params.blast_gb.memory instanceof Number) ? "${params.blast_gb.memory}.GB" : null
    clusterOptions = (params.blast_gb.clusterOptions instanceof String) ? params.blast_gb.clusterOptions : null

    errorStrategy 'ignore'

    tag "${id}"

    input:
        tuple val(id), val(blast_accession), val(blast_species), val(blast_evalue), val(opts_id)

    output:
        tuple val(id), val(blast_accession), path("${outDir}/blast_ref_annotations.csv"), path("${outDir}/blast_ref_sequence.txt"), path("${outDir}/blast_ref_genetic_code.txt"), path("${outDir}/remote_blast_ref.json")

    shell:
    outDir = "${id}/assemble/${opts_id}"
    '''
    mkdir -p !{outDir}
    Rscript -e "MitoPilot::fetch_blast_ref('!{blast_accession}', '!{outDir}/blast_ref_annotations.csv', '!{outDir}/blast_ref_sequence.txt', '!{outDir}/blast_ref_genetic_code.txt', '!{outDir}/remote_blast_ref.json', '!{blast_species}', !{blast_evalue})"
    '''
}
