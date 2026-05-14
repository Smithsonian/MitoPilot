process blast_ref_fetch {

    executor params.blast_gb.executor
    container params.blast_gb.container

    publishDir "${launchDir}/${params.publishDir}", overwrite: true, pattern: "${id}/assemble/${opts_id}/blast_ref_${blast_accession}/remote_blast_ref.json", mode: 'copy'

    cpus { (params.blast_gb.cpus instanceof Integer) ? params.blast_gb.cpus : 1 }
    memory = (params.blast_gb.memory instanceof Number) ? "${params.blast_gb.memory}.GB" : null
    clusterOptions {
        def opts = [
            (params.blast_gb.executor == 'sge') ? '-S /bin/bash' : '',
            (params.blast_gb.clusterOptions instanceof String) ? params.blast_gb.clusterOptions : ''
        ].findAll { it }.join(' ')
        opts ?: null
    }

    errorStrategy { task.attempt <= 3 ? 'retry' : 'ignore' }
    maxRetries 3

    tag "${id}"

    input:
        tuple val(id), val(blast_accession), val(blast_species), val(blast_evalue), val(opts_id), val(is_top)

    output:
        tuple val(id), val(blast_accession), val(is_top), path("${outDir}/blast_ref_annotations.csv"), path("${outDir}/blast_ref_sequence.txt"), path("${outDir}/blast_ref_genetic_code.txt"), path("${outDir}/remote_blast_ref.json")

    shell:
    // Per-accession subdir keeps multi-accession-per-id runs from clobbering each other
    outDir = "${id}/assemble/${opts_id}/blast_ref_${blast_accession}"
    '''
    mkdir -p !{outDir}
    # Back off on retries to give NCBI EFetch time to recover from rate limits
    if [ "!{task.attempt}" -gt 1 ]; then
        sleep $(( (!{task.attempt} - 1) * 30 ))
    fi
    Rscript -e "MitoPilot::fetch_blast_ref('!{blast_accession}', '!{outDir}/blast_ref_annotations.csv', '!{outDir}/blast_ref_sequence.txt', '!{outDir}/blast_ref_genetic_code.txt', '!{outDir}/remote_blast_ref.json', '!{blast_species}', !{blast_evalue})"
    '''
}
