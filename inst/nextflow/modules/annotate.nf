process annotate {

    executor params.annotate.executor
    container params.annotate.container

    publishDir "$launchDir/${params.publishDir}", overwrite: true, pattern: "${id}/annotate/NF_work_dir_annotate.txt", mode: 'copy'

    errorStrategy 'finish'

    // cpus { opts.cpus }
    // memory { opts.memory.GB }

    tag "${id}"

    input:
        tuple val(id), val(path), path(assembly), path(coverage), val(opts), path(ref_dir_full)

    output:
    tuple val(id), val(path),
        path("${id}/annotate/${id}_annotations_*.csv"),
        path("${id}/annotate/${id}_assembly_*.fasta"),
        path("${id}/annotate/${id}_coverageStats_*.csv"),
        path("${id}/annotate/NF_work_dir_annotate.txt")                 // Nextflow working directory, for troubleshooting

    shell:
    dir = "${id}/annotate/"
    '''
    mkdir -p !{dir}
    Rscript -e "MitoPilot::annotate( \
        assembly_fn = '!{assembly}', \
        coverage_fn = '!{coverage}', \
        cpus = !{task.cpus}, \
        genetic_code = '!{params.genetic_code}', \
        ref_db = '!{opts.ref_db}', \
        ref_dir = ".", \
        mitos_opts = '!{opts.mitos}', \
        mitos_condaenv = '!{params.mitos_condaenv}', \
        trnaScan_opts = '!{opts.trnaScan}', \
        trnaScan_condaenv = '!{params.trnaScan_condaenv}', \
        start_gene = '!{opts.start_gene}', \
        out_dir = '!{dir}'
    )"
    ### work dir info for troubleshooting ####
    echo "Nextflow annotate working directory:" > !{dir}/NF_work_dir_annotate.txt
    echo "$PWD" >> !{dir}/NF_work_dir_annotate.txt
    '''
}
