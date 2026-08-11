include { clusterOpts } from './cluster_opts.nf'

process annotate {

    // symlink, not copy: MITOS never writes into its ref DB (it uses MITOS2_temp*),
    // so the shared pre-extracted directory can be symlinked read-only.
    stageInMode 'symlink'

    executor params.annotate.executor
    container params.annotate.container
    clusterOptions { clusterOpts(params.annotate) }

    publishDir "${launchDir}/${params.publishDir}", overwrite: true, pattern: "${id}/annotate/NF_work_dir_annotate.txt", mode: 'copy'

    errorStrategy 'ignore'

    tag "${id}.${path}.${scaffold}"

    input:
    tuple val(id), val(path), val(scaffold), path(assembly), path(coverage), val(opts), path(ref_dir_full), val(ref_db_clean), path(mitofinder_db)

    output:
    tuple val(id), val(path), val(scaffold), path("${id}/annotate/${id}_annotations_*.csv"), path("${id}/annotate/${id}_assembly_*.fasta"), path("${id}/annotate/${id}_coverageStats_*.csv"), path("${id}/annotate/NF_work_dir_annotate.txt")

    shell:
    dir = "${id}/annotate/"
    // Per-unit FASTA name so annotate() derives per-scaffold output filenames
    // (ID_annotations_<path>_<scaffold>.csv, ...). ignore_scaffolds drops every
    // other scaffold of the path, so only this unit's scaffold is annotated.
    unit_assembly = "${id}_assembly_${path}_${scaffold}.fasta"
    '''
    mkdir -p !{dir}

    # Name the assembly per unit; annotate() isolates this scaffold via ignore_scaffolds.
    cp !{assembly} !{unit_assembly}

    # Resolve MitoFinder reference db (decompress if gzip/tar.gz)
    MF_DB="!{mitofinder_db}"
    if [ "!{opts.use_mitofinder}" == "1" ]; then
        MF_MIME=$(file --mime-type -b "$MF_DB")
        if [[ "$MF_MIME" == "application/gzip" ]]; then
            if tar -tzf "$MF_DB" >/dev/null 2>&1; then
                tar -xzf "$MF_DB"
                MF_DB=$(find . -maxdepth 2 -name '*.gb' | head -n1)
            else
                gunzip -c "$MF_DB" > mitofinder_ref.gb
                MF_DB="mitofinder_ref.gb"
            fi
        fi
    fi

    Rscript -e "MitoPilot::annotate( \
        assembly_fn = '!{unit_assembly}', \
        coverage_fn = '!{coverage}', \
        cpus = !{task.cpus}, \
        genetic_code = '!{opts.genetic_code}', \
        ref_db = '!{ref_db_clean}', \
        ref_dir = '.', \
        use_mitos = !{opts.use_mitos == 1 ? "TRUE" : "FALSE"}, \
        mitos_opts = '!{opts.mitos}', \
        use_mitos_best = !{opts.use_mitos_best == 1 ? "TRUE" : "FALSE"}, \
        rescue_no_trna = !{opts.rescue_no_trna == 1 ? "TRUE" : "FALSE"}, \
        mitos_condaenv = '!{params.mitos_condaenv}', \
        use_trnaScan = !{opts.use_trnaScan == 1 ? "TRUE" : "FALSE"}, \
        trnaScan_opts = '!{opts.trnaScan}', \
        trnaScan_condaenv = '!{params.trnaScan_condaenv}', \
        arwen_opts = '!{opts.arwen}', \
        use_arwen = !{opts.use_arwen == 1 ? "TRUE" : "FALSE"}, \
        aragorn_opts = '!{opts.aragorn}', \
        aragorn_condaenv = '!{params.aragorn_condaenv}', \
        use_aragorn = !{opts.use_aragorn == 1 ? "TRUE" : "FALSE"}, \
        use_mitofinder = !{opts.use_mitofinder == 1 ? "TRUE" : "FALSE"}, \
        mitofinder_db = '$MF_DB', \
        mitofinder_new_genes = !{opts.mitofinder_new_genes == 1 ? "TRUE" : "FALSE"}, \
        mitofinder_allow_introns = !{opts.mitofinder_allow_introns == 1 ? "TRUE" : "FALSE"}, \
        mitofinder_opts = '!{opts.mitofinder_opts}', \
        start_gene = '!{opts.start_gene}', \
        ignore_scaffolds = '!{opts.ignore_scaffolds}', \
        coverage_trim = !{opts.coverage_trim == 1 ? "TRUE" : "FALSE"}, \
        retain_low_conf_trna = !{opts.retain_low_conf_trna == 1 ? "TRUE" : "FALSE"}, \
        out_dir = '!{dir}'
    )"
    ### work dir info for troubleshooting ####
    echo "Nextflow annotate working directory:" > !{dir}/NF_work_dir_annotate.txt
    echo "$PWD" >> !{dir}/NF_work_dir_annotate.txt
    '''
}
