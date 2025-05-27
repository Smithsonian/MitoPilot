process annotate {

    stageInMode 'copy'

    executor params.annotate.executor
    container params.annotate.container

    publishDir "$launchDir/${params.publishDir}", overwrite: true, pattern: "${id}/annotate/NF_work_dir_annotate.txt", mode: 'copy'

    errorStrategy 'finish'

    // cpus { opts.cpus }
    // memory { opts.memory.GB }

    tag "${id}"

    input:
        tuple val(id), val(path), path(assembly), path(coverage), val(opts), path(ref_dir_full), val(ref_db_clean)

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
