process validate {

    executor params.curate.executor
    container params.curate.container
    
    publishDir "$launchDir/${params.publishDir}", overwrite: true

    errorStrategy 'finish'
    cpus { opts.cpus }
    memory { opts.memory.GB }

    tag "${id}"

    input:
        tuple val(id), val(path), path(annotations), path(coverage), val(opts)

    output: 
    tuple val(id), val(path),
        path("${id}/annotate/${id}_annotations_*.csv"),
        path("${id}/annotate/${id}_summary_*.csv")

    shell:
    dir = "${id}/annotate"
    '''
    mkdir -p !{dir}
    Rscript -e "MitoPilot::validate_!{opts.target}( \
        annotations_fn = '!{annotations}', \
        coverage_fn = '!{coverage}', \
        params = '!{opts.params}', \
        out_dir = '!{dir}'
    )"
    '''
}