process coverage {
    executor params.coverage.executor
    container params.coverage.container

    publishDir "$launchDir/${params.publishDir}", overwrite: true, mode: 'copy'

    errorStrategy 'finish'
    cpus {params.coverage.cpus}
    memory = params.coverage.memory ?: null
    clusterOptions = params.coverage.clusterOptions ?: null

    tag "${id}"

    input:
        tuple val(id), val(opt_id), path(reads), path(assembly)

    output:
        tuple path("${outDir}/*"),
            path("${id}/assemble/${opt_id}/NF_work_dir_coverage.txt")

    shell:
    outDir = "${id}/assemble/${opt_id}"
    output_name = assembly.baseName
    '''
    # Unzip reads
    tar -xzf !{reads} --strip-components=2  
    # Concatenate unpaired reads
    cat extended_*_unpaired.fq >> unpaired.fq  
    Rscript -e "MitoPilot::coverage('!{assembly}', 'extended_1_paired.fq', 'extended_2_paired.fq', 'unpaired.fq', !{task.cpus}, '!{outDir}')"
    ### work dir info for troubleshooting ####
    echo "Nextflow coverage working directory:" > !{outDir}/NF_work_dir_coverage.txt
    echo "$PWD" >> !{outDir}/NF_work_dir_coverage.txt
    '''

}