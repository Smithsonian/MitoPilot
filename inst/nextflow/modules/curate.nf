process curate {

    executor params.curate.executor
    container params.curate.container

    publishDir "$launchDir/${params.publishDir}", overwrite: true, pattern: "${id}/annotate/*", mode: 'copy'

    errorStrategy 'finish'
    // cpus { opts.cpus }
    // memory { opts.memory.GB }

    tag "${id}"

    input:
        tuple val(id), val(path), path(annotations), path(assembly), path(coverage), val(opts), path(ref_dir_full), val(ref_clade)

    output:
    tuple val(id), val(path),
        path("${id}/${id}_annotations_*.csv"),
        path("${id}/annotate/${id}_assembly_*.fasta"),
        path("${id}/annotate/${id}_coverageStats_*.csv"),
        path("${id}/annotate/NF_work_dir_curate.txt")                 // Nextflow working directory, for troubleshooting

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
        out_dir = '!{dir}', \
        max_blast_hits = '!{opts.max_blast_hits}', \
        ref_dir = './!{ref_clade}'
        )"
    mv !{dir}/*_annotations_*.csv !{id}/
    ### work dir info for troubleshooting ####
    echo "Nextflow curate working directory:" > !{dir}/NF_work_dir_curate.txt
    echo "$PWD" >> !{dir}/NF_work_dir_curate.txt
    '''
}
