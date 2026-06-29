process coverage {
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
        tuple val(id), val(opt_id), path(reads), path(assembly), val(assembler)

    output:
        tuple val(id), path("${outDir}/*"),
            path("${id}/assemble/${opt_id}/NF_work_dir_coverage.txt")

    shell:
    outDir = "${id}/assemble/${opt_id}"
    output_name = assembly.baseName
    '''
    # Unzip reads
    if [ !{assembler} == "GetOrganelle" ]; then
        tar -xzf !{reads} --strip-components=2  
        # Concatenate unpaired reads
        cat extended_*_unpaired.fq >> unpaired.fq  
        Rscript -e "MitoPilot::coverage('!{assembly}', 'extended_1_paired.fq', 'extended_2_paired.fq', 'unpaired.fq', !{task.cpus}, '!{outDir}')"
    elif [ !{assembler} == "MitoFinder" ]; then   
        tar -xzf !{reads}    
        Rscript -e "MitoPilot::coverage('!{assembly}', '!{id}_preprocess_R1.fastq.gz', '!{id}_preprocess_R2.fastq.gz', 'NA', !{task.cpus}, '!{outDir}')"
    fi   
    
    ### work dir info for troubleshooting ####
    echo "Nextflow coverage working directory:" > !{outDir}/NF_work_dir_coverage.txt
    echo "$PWD" >> !{outDir}/NF_work_dir_coverage.txt
    '''

}