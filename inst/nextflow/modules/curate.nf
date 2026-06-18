process curate {

    stageInMode 'copy'

    executor params.curate.executor
    container params.curate.container
    clusterOptions {
        def opts = [
            (params.curate.executor == 'sge') ? '-S /bin/bash' : '',
            (params.curate.clusterOptions instanceof String) ? params.curate.clusterOptions : ''
        ].findAll { it }.join(' ')
        opts ?: null
    }

    publishDir "${launchDir}/${params.publishDir}", overwrite: true, pattern: "${id}/annotate/*", mode: 'copy'

    errorStrategy 'ignore'

    tag "${id}"

    input:
    tuple val(id), val(path), path(annotations), path(assembly), path(coverage), val(opts), path(ref_dir_full), val(ref_clade), val(ref_db_clean), path(blast_ref_file)

    output:
    tuple val(id), val(path), path("${id}/${id}_annotations_*.csv"), path("${id}/annotate/${id}_assembly_*.fasta"), path("${id}/annotate/${id}_coverageStats_*.csv"), path("${id}/annotate/NF_work_dir_curate.txt")

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

    Rscript -e "MitoPilot::curate_!{opts.target}( \
        annotations_fn = '!{annotations}', \
        assembly_fn = '!{assembly}', \
        coverage_fn = '!{coverage}', \
        genetic_code = !{params.genetic_code}, \
        params = '!{opts.params}', \
        out_dir = '!{dir}', \
        max_blast_hits = '!{opts.max_blast_hits}', \
        ref_dir = '!{ref_db_clean}', \
        blast_ref_file = '!{blast_ref_file}', \
        feature_trim = !{opts.feature_trim == 1 ? "TRUE" : "FALSE"} \
        )"
    mv !{dir}/*_annotations_*.csv !{id}/
    ### work dir info for troubleshooting ####
    echo "Nextflow curate working directory:" > !{dir}/NF_work_dir_curate.txt
    echo "$PWD" >> !{dir}/NF_work_dir_curate.txt
    '''
}

// Verified, committed write of the curated assembly sequence to the assemblies
// table. Native (exec) process: runs in the Nextflow driver on the head node
// (never containerized) and shells out to the host R, so it can reach the
// project .sqlite directly. maxForks 1 makes it the single DB writer at any
// instant, avoiding sqlite lock contention (incl. over NFS). Replaces the old
// fire-and-forget nf-sqldb operator whose deferred commit could be lost while
// annotations still committed, leaving the two tables in different frames.
process write_assembly {

    executor 'local'
    maxForks 1
    errorStrategy 'ignore'
    tag "${id}"

    input:
    tuple val(id), val(path), path(coverage)

    output:
    tuple val(id), val(path)

    exec:
    def cmd = [
        'Rscript', '-e',
        "MitoPilot::write_curated_assembly(" +
            "db_path = '${launchDir}/.sqlite', " +
            "coverage_fn = '${coverage}', " +
            "time_stamp = '${params.ts}')"
    ]
    def proc = cmd.execute()
    proc.waitForProcessOutput(System.out, System.err)
    if (proc.exitValue() != 0) {
        throw new RuntimeException("write_curated_assembly failed for ${id} (see log above)")
    }
}
