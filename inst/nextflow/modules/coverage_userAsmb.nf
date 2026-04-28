process coverage_userAsmb {
    executor params.coverage.executor
    container params.coverage.container

    publishDir "$launchDir/${params.publishDir}", overwrite: true, mode: 'copy'

    errorStrategy 'ignore'
    cpus {params.coverage.cpus}
    memory = params.coverage.memory ?: null
    clusterOptions = params.coverage.clusterOptions ?: null

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
    output_name = assembly.baseName
    '''
    mkdir -p !{outDir}

    # rename assembly file and contig(s)
    awk -v topo="!{topology}" '/^>/ {print ">!{id}.1." ++count[">"] " " topo} !/^>/ {print}' !{assembly} > !{outDir}/!{id}_assembly_1.fasta

    # calculate coverage
    Rscript -e "MitoPilot::coverage('!{outDir}/!{id}_assembly_1.fasta', '!{reads[0]}', '!{reads[1]}', 'NA', !{task.cpus}, '!{outDir}')"
    
    # cleanup
    rm !{outDir}/*_working.fasta*

    ### work dir info for troubleshooting ####
    echo "Nextflow coverage working directory:" > !{outDir}/NF_work_dir_coverage.txt
    echo "$PWD" >> !{outDir}/NF_work_dir_coverage.txt
    '''

}