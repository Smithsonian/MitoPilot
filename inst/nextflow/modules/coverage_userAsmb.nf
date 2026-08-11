process coverage_userAsmb {
    executor params.coverage.executor
    container params.coverage.container

    publishDir "$launchDir/${params.publishDir}", overwrite: true, mode: 'copy'

    errorStrategy 'ignore'
    cpus {params.coverage.cpus}
    // GB from config; a bare number would be read as BYTES.
    memory { (params.coverage.memory instanceof Number) ? params.coverage.memory.GB * task.attempt : null }
    clusterOptions {
        def opts = [
            (params.coverage.executor == 'sge') ? '-S /bin/bash' : '',
            params.coverage.clusterOptions ?: ''
        ].findAll { it }.join(' ')
        opts ?: null
    }

    tag "${id}"

    input:
        tuple val(id), path(reads), file(assembly), val(topology), val(assembler)

    output:
        tuple path("${outDir}/*"),     // output files
            path("${id}/assemble/${assembler}/NF_work_dir_coverage.txt"), // troubleshooting file
            val(id),  // ID
            path("${outDir}/${id}_assembly_1.fasta"),  // assembly fasta
            val(assembler)  // opts_id (assemble_opts key)

    shell:
    outDir = "${id}/assemble/${assembler}"
    // No-raw-data projects stage a placeholder instead of trimmed reads; coverage()
    // then synthesizes its stats from the assembly (GC only, no read mapping).
    r1 = params.noRawData ? 'NA' : reads[0]
    r2 = params.noRawData ? 'NA' : reads[1]
    '''
    mkdir -p !{outDir}

    # rename assembly file and contig(s)
    awk -v topo="!{topology}" '/^>/ {print ">!{id}.1." ++count[">"] " " topo} !/^>/ {print}' !{assembly} > !{outDir}/!{id}_assembly_1.fasta

    # calculate coverage
    Rscript -e "MitoPilot::coverage('!{outDir}/!{id}_assembly_1.fasta', '!{r1}', '!{r2}', 'NA', !{task.cpus}, '!{outDir}')"

    # cleanup
    rm -f !{outDir}/*_working.fasta*

    ### work dir info for troubleshooting ####
    echo "Nextflow coverage working directory:" > !{outDir}/NF_work_dir_coverage.txt
    echo "$PWD" >> !{outDir}/NF_work_dir_coverage.txt
    '''

}
