include { clusterOpts } from './cluster_opts.nf'

process curate {

    // symlink, not copy: the ref DB is a shared pre-extracted directory that is
    // never written (remote hits go to a task-private DB), and the other inputs
    // are read-only too, so nothing needs a private copy.
    stageInMode 'symlink'

    executor params.curate.executor
    container params.curate.container
    clusterOptions { clusterOpts(params.curate) }

    publishDir "${launchDir}/${params.publishDir}", overwrite: true, pattern: "${id}/annotate/*", mode: 'copy'

    errorStrategy 'ignore'

    tag "${id}.${path}.${scaffold}"

    input:
    tuple val(id), val(path), val(scaffold), path(annotations), path(assembly), path(coverage), val(opts), path(ref_dir_full), val(ref_clade), val(ref_db_clean), path(blast_ref_files, stageAs: 'blast_ref_*.json')

    output:
    tuple val(id), val(path), val(scaffold), path("${id}/${id}_annotations_*.csv"), path("${id}/annotate/${id}_assembly_*.fasta"), path("${id}/annotate/${id}_coverageStats_*.csv"), path("${id}/annotate/NF_work_dir_curate.txt")

    shell:
    dir = "${id}/annotate"
    '''
    export OMP_NUM_THREADS=1 # fix for OpenBLAS blas_thread_init error
    mkdir -p !{dir}

    Rscript -e "MitoPilot::curate_!{opts.target}( \
        annotations_fn = '!{annotations}', \
        assembly_fn = '!{assembly}', \
        coverage_fn = '!{coverage}', \
        genetic_code = !{opts.genetic_code}, \
        params = '!{opts.params}', \
        out_dir = '!{dir}', \
        max_blast_hits = '!{opts.max_blast_hits}', \
        ref_dir = '!{ref_db_clean}', \
        blast_ref_file = '!{(blast_ref_files instanceof List ? blast_ref_files : [blast_ref_files]).join(" ")}', \
        feature_trim = !{opts.feature_trim == 1 ? "TRUE" : "FALSE"}, \
        ref_based_rc = !{opts.ref_based_rc == 1 ? "TRUE" : "FALSE"}, \
        blast_accession = '!{opts.blast_accession}' \
        )"
    mv !{dir}/*_annotations_*.csv !{id}/
    ### work dir info for troubleshooting ####
    echo "Nextflow curate working directory:" > !{dir}/NF_work_dir_curate.txt
    echo "$PWD" >> !{dir}/NF_work_dir_curate.txt
    '''
}
