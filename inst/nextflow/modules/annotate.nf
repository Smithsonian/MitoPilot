process annotate {

    executor params.annotate.executor
    container params.annotate.container
    
    // publishDir "$launchDir/${params.publishDir}", overwrite: true

    errorStrategy 'finish'
    cpus { opts.cpus }
    memory { opts.memory.GB }
    tag "${id}"

    input:
        tuple val(id), val(path), path(assembly), path(coverage), val(opts)

    output: 
    tuple val(id), val(path), 
        path("${id}/annotate/${id}_annotations_*.csv"),
        path("${id}/annotate/${id}_assembly_*.fasta"),
        path("${id}/annotate/${id}_coverageStats_*.csv")

    shell:
    dir = "${id}/annotate"
    '''
    mkdir -p !{dir}
    Rscript -e "MitoPilot::annotate( \
        assembly_fn = '!{assembly}', \
        coverage_fn = '!{coverage}', \
        cpus = !{task.cpus}, \
        genetic_code = '!{params.genetic_code}', \
        ref_db = '!{opts.ref_db}', \
        ref_dir = '!{opts.ref_dir}', \
        mitos_opts = '!{opts.mitos}', \
        mitos_condaenv = '!{params.mitos_condaenv}', \
        trnaScan_opts = '!{opts.trnaScan}', \
        trnaScan_condaenv = '!{params.trnaScan_condaenv}', \
        out_dir = '!{dir}'
    )"
    '''
}