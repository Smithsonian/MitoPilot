// set memory and cpus based on values in config file
opts.cpus = {params.coverage.cpus}
opts.memory = {params.coverage.memory}

process coverage {

    executor params.coverage.executor
    container params.coverage.container

    publishDir params.publishDir, overwrite: true, mode: 'copy'

    errorStrategy 'finish'
    //cpus {params.coverage.cpus}
    //memory {params.coverage.memory.GB}

    tag "${id}"

    input:
        tuple val(id), val(opt_id), path(reads), path(assembly)

    output:
        path("${outDir}/*")

    shell:
    outDir = "${id}/assemble/${opt_id}"
    output_name = assembly.baseName
    '''
    # Unzip reads
    tar -xzf !{reads} --strip-components=2  
    # Concatenate unpaired reads
    cat extended_*_unpaired.fq >> unpaired.fq  
    Rscript -e "MitoPilot::coverage('!{assembly}', 'extended_1_paired.fq', 'extended_2_paired.fq', 'unpaired.fq', !{task.cpus}, '!{outDir}')"
    '''

}