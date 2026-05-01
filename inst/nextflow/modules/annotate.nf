process annotate {

    stageInMode 'copy'

    executor params.annotate.executor
    container params.annotate.container

    publishDir "${launchDir}/${params.publishDir}", overwrite: true, pattern: "${id}/annotate/NF_work_dir_annotate.txt", mode: 'copy'

    errorStrategy 'ignore'

    tag "${id}"

    input:
    tuple val(id), val(path), path(assembly), path(coverage), val(opts), path(ref_dir_full), val(ref_db_clean)

    output:
    tuple val(id), val(path), path("${id}/annotate/${id}_annotations_*.csv"), path("${id}/annotate/${id}_assembly_*.fasta"), path("${id}/annotate/${id}_coverageStats_*.csv"), path("${id}/annotate/NF_work_dir_annotate.txt")

    shell:
    dir = "${id}/annotate/"
    '''
    mkdir -p !{dir}

    # Check if ref database is gzip-compressed file
    MIME_TYPE=$(file --mime-type -b "!{opts.ref_db}")
    if [[ "$MIME_TYPE" == "application/gzip" ]]; then
        echo "Decompressing !{opts.ref_db}..."
        tar -xzf "!{opts.ref_db}"
        echo "Decompression complete."
    else
        echo "Input ref_db not .tar.gz"
    fi

    Rscript -e "MitoPilot::annotate( \
        assembly_fn = '!{assembly}', \
        coverage_fn = '!{coverage}', \
        cpus = !{task.cpus}, \
        genetic_code = '!{params.genetic_code}', \
        ref_db = '!{ref_db_clean}', \
        ref_dir = '.', \
        mitos_opts = '!{opts.mitos}', \
        use_mitos_best = !{opts.use_mitos_best == 1 ? "TRUE" : "FALSE"}, \
        mitos_condaenv = '!{params.mitos_condaenv}', \
        trnaScan_opts = '!{opts.trnaScan}', \
        trnaScan_condaenv = '!{params.trnaScan_condaenv}', \
        arwen_opts = '!{opts.arwen}', \
        use_arwen = !{opts.use_arwen == 1 ? "TRUE" : "FALSE"}, \
        aragorn_opts = '!{opts.aragorn}', \
        aragorn_condaenv = '!{params.aragorn_condaenv}', \
        use_aragorn = !{opts.use_aragorn == 1 ? "TRUE" : "FALSE"}, \
        start_gene = '!{opts.start_gene}', \
        out_dir = '!{dir}'
    )"
    ### work dir info for troubleshooting ####
    echo "Nextflow annotate working directory:" > !{dir}/NF_work_dir_annotate.txt
    echo "$PWD" >> !{dir}/NF_work_dir_annotate.txt
    '''
}
