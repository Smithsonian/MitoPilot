process orf {

    stageInMode 'copy'

    executor params.orf.executor
    container params.orf.container
    clusterOptions {
        def opts = [
            (params.orf.executor == 'sge') ? '-S /bin/bash' : '',
            (params.orf.clusterOptions instanceof String) ? params.orf.clusterOptions : ''
        ].findAll { it }.join(' ')
        opts ?: null
    }

    publishDir "${launchDir}/${params.publishDir}", overwrite: true, pattern: "${id}/annotate/*", mode: 'copy'

    errorStrategy 'ignore'

    tag "${id}"

    input:
    tuple val(id), val(path), path(annotations), path(assembly), val(opts), path(ref_dir_full), val(ref_clade), val(ref_db_clean), path(blast_ref_file)

    output:
    tuple val(id), val(path), path("${id}/annotate/${id}_ORFannotations_*.tsv"), path("${id}/annotate/NF_work_dir_orf.txt")

    shell:
    dir = "${id}/annotate"
    '''
    export OMP_NUM_THREADS=1 # fix for OpenBLAS blas_thread_init error
    mkdir -p !{dir}

    # Check if ref database is gzip-compressed file
    MIME_TYPE=$(file --mime-type -b "!{ref_clade}")
    if [[ "$MIME_TYPE" == "application/gzip" || "$MIME_TYPE" == "application/x-gzip" ]]; then
        echo "Decompressing !{ref_clade}..."
        tar -xzf "!{ref_clade}"
        echo "Decompression complete."
    else
        echo "Input ref_db not .tar.gz"
    fi

    Rscript -e "MitoPilot::orf_finder( \
        annotations_fn = '!{annotations}', \
        assembly_fn = '!{assembly}', \
        genetic_code = '!{opts.genetic_code ?: params.genetic_code}', \
        orffinder_opts = '!{opts.orffinder_opts}', \
        orffinder_condaenv = '!{params.orffinder_condaenv}', \
        orf_min_len = !{opts.orf_min_len}, \
        orf_max_overlap = !{opts.orf_max_overlap}, \
        orf_nested = !{opts.orf_nested ? "TRUE" : "FALSE"}, \
        ref_dir = '!{ref_db_clean}', \
        max_blast_hits = '!{opts.max_blast_hits}', \
        blast_ref_file = '!{blast_ref_file}', \
        out_dir = '!{dir}' \
        )"
    ### work dir info for troubleshooting ####
    echo "Nextflow orf working directory:" > !{dir}/NF_work_dir_orf.txt
    echo "$PWD" >> !{dir}/NF_work_dir_orf.txt
    '''
}
