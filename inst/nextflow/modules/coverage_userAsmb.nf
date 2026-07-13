process coverage_userAsmb {
    executor params.coverage.executor
    container params.coverage.container

    publishDir "$launchDir/${params.publishDir}", overwrite: true, mode: 'copy'

    errorStrategy 'ignore'
    cpus {params.coverage.cpus}
    memory = params.coverage.memory ?: null
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
    output_name = assembly.baseName
    '''
    mkdir -p !{outDir}

    # rename assembly file and contig(s)
    awk -v topo="!{topology}" '/^>/ {print ">!{id}.1." ++count[">"] " " topo} !/^>/ {print}' !{assembly} > !{outDir}/!{id}_assembly_1.fasta

    # calculate coverage
    Rscript -e "MitoPilot::coverage('!{outDir}/!{id}_assembly_1.fasta', '!{reads[0]}', '!{reads[1]}', 'NA', !{task.cpus}, '!{outDir}')"

    # cleanup
    rm -f !{outDir}/*_working.fasta*

    ### work dir info for troubleshooting ####
    echo "Nextflow coverage working directory:" > !{outDir}/NF_work_dir_coverage.txt
    echo "$PWD" >> !{outDir}/NF_work_dir_coverage.txt
    '''

}

// No-reads variant: same staging + DB-facing outputs, but coverage() is invoked
// with 'NA' reads so it synthesizes the coverageStats table from the assembly
// (GC only; depth/error empty). No read mapping is performed.
process coverage_userAsmb_noReads {
    executor params.coverage.executor
    container params.coverage.container

    publishDir "$launchDir/${params.publishDir}", overwrite: true, mode: 'copy'

    errorStrategy 'ignore'
    cpus {params.coverage.cpus}
    memory = params.coverage.memory ?: null
    clusterOptions {
        def opts = [
            (params.coverage.executor == 'sge') ? '-S /bin/bash' : '',
            params.coverage.clusterOptions ?: ''
        ].findAll { it }.join(' ')
        opts ?: null
    }

    tag "${id}"

    input:
        tuple val(id), file(assembly), val(topology), val(assembler)

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

    # derive coverage stats from the assembly (no reads)
    Rscript -e "MitoPilot::coverage('!{outDir}/!{id}_assembly_1.fasta', 'NA', 'NA', 'NA', !{task.cpus}, '!{outDir}')"

    # cleanup
    rm -f !{outDir}/*_working.fasta*

    ### work dir info for troubleshooting ####
    echo "Nextflow coverage working directory:" > !{outDir}/NF_work_dir_coverage.txt
    echo "$PWD" >> !{outDir}/NF_work_dir_coverage.txt
    '''

}