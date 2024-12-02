process curate {

    executor params.curate.executor
    container params.curate.container
    
    publishDir "$launchDir/${params.publishDir}", overwrite: true, pattern: "${id}/annotate/*", mode: 'copy'

    errorStrategy 'finish'
    // cpus { opts.cpus }
    // memory { opts.memory.GB }

    tag "${id}"

    input:
        tuple val(id), val(path), path(annotations), path(assembly), path(coverage), val(opts)

    output: 
    tuple val(id), val(path),
        path("${id}/${id}_annotations_*.csv"),
        path("${id}/annotate/${id}_assembly_*.fasta"),
        path("${id}/annotate/${id}_coverageStats_*.csv")

    shell:
    dir = "${id}/annotate"
    '''
    export OMP_NUM_THREADS=1 # fix for OpenBLAS blas_thread_init error
    mkdir -p !{dir}
    Rscript -e "MitoPilot::curate_!{opts.target}( \
        annotations_fn = '!{annotations}', \
        assembly_fn = '!{assembly}', \
        coverage_fn = '!{coverage}', \
        genetic_code = !{params.genetic_code}, \
        params = '!{opts.params}', \
        out_dir = '!{dir}'
    )"
    mv !{dir}/*_annotations_*.csv !{id}/
    '''
}