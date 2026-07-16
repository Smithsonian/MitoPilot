process orf {

    // symlink, not copy: the ref DB is a shared pre-extracted, read-only directory
    // and the other inputs are read-only too (see curate.nf).
    stageInMode 'symlink'

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

    tag "${id}.${path}.${scaffold}"

    input:
    tuple val(id), val(path), val(scaffold), path(annotations), path(assembly), val(opts), path(ref_dir_full), val(ref_clade), val(ref_db_clean), path(blast_ref_file)

    output:
    tuple val(id), val(path), val(scaffold), path("${id}/annotate/${id}_ORFannotations_*.tsv"), path("${id}/annotate/NF_work_dir_orf.txt")

    shell:
    dir = "${id}/annotate"
    '''
    export OMP_NUM_THREADS=1 # fix for OpenBLAS blas_thread_init error
    mkdir -p !{dir}

    # Reference DB: a pre-extracted, shared directory named for the clade is
    # normally staged (symlinked). Fall back to extracting a tarball if only that
    # is present.
    if [ ! -d "!{ref_db_clean}" ]; then
        for tb in "!{ref_db_clean}.tar.gz" "!{ref_clade}"; do
            if [ -f "$tb" ]; then
                echo "Decompressing $tb..."
                tar -xzf "$tb"
                break
            fi
        done
    fi

    # Merge the per-scaffold validated annotation TSVs into one per-path file so
    # ORFfinder avoids overlaps against every existing annotation on the path.
    merged_annotations=merged_validated_annotations.tsv
    first=1
    for f in !{annotations}; do
        if [ "$first" = "1" ]; then
            cat "$f" > "$merged_annotations"
            first=0
        else
            tail -n +2 "$f" >> "$merged_annotations"
        fi
    done

    Rscript -e "MitoPilot::orf_finder( \
        annotations_fn = '$merged_annotations', \
        assembly_fn = '!{assembly}', \
        genetic_code = '!{opts.genetic_code}', \
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
