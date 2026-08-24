// Optional WF1 stage for user-provided assemblies: locate the mitochondrial
// contigs inside a large multi-contig assembly. The screen fans out over chunks
// of the FASTA so a draft genome stays tractable; the pick job then selects,
// extracts and confirms. See R/find_mito.R.
process find_mito_screen {
    executor params.find_mito.executor
    container params.find_mito.container

    errorStrategy 'ignore'
    clusterOptions {
        def opts_str = [
            (params.find_mito.executor == 'sge') ? '-S /bin/bash' : '',
            params.find_mito.clusterOptions ?: ''
        ].findAll { it }.join(' ')
        opts_str ?: null
    }

    tag "${id}.${task.index}"

    input:
        tuple val(id), path(chunk), val(opts)

    output:
        tuple val(id), path("hits_${task.index}.txt")

    shell:
    db_dir = params.blast_gb?.db_dir ?: '/ref_dbs/mito_metazoa'
    db_name = params.blast_gb?.db_name ?: 'mito_metazoa'
    '''
    export BLASTDB=!{db_dir}

    # Drop the short tail before searching: on a draft genome most contigs are
    # far too short to carry a usable piece of the mitogenome, and they dominate
    # the contig count.
    awk -v min=!{opts.min_contig_length} '
        /^>/ { if (seq != "" && length(seq) >= min) print hdr ORS seq; hdr = $0; seq = ""; next }
              { seq = seq $0 }
        END  { if (seq != "" && length(seq) >= min) print hdr ORS seq }
    ' !{chunk} > screened.fasta

    # The screened-contig count rides along as a comment line: the R reader
    # skips comments, so one output file carries both facts.
    n_screened=$(grep -c '^>' screened.fasta || true)
    echo "# screened=${n_screened}" > hits_!{task.index}.txt

    if [ "${n_screened}" -eq 0 ]; then
        exit 0
    fi

    blastn \
        -task megablast \
        -db !{db_dir}/!{db_name} \
        -query screened.fasta \
        -outfmt "6 qseqid saccver pident length bitscore qlen" \
        -max_target_seqs 5 \
        -evalue 1e-20 \
        -num_threads !{task.cpus} \
        >> hits_!{task.index}.txt
    '''
}

// Per sample: merge the chunk hits, select candidate contigs, pull them out of
// the original FASTA, and confirm them with MitoFinder.
process find_mito_pick {
    executor params.find_mito.executor
    container params.find_mito.container

    publishDir "$launchDir/${params.publishDir}", overwrite: true, mode: 'copy',
        pattern: "${outDir}/find_mito*"

    errorStrategy 'ignore'
    clusterOptions {
        def opts_str = [
            (params.find_mito.executor == 'sge') ? '-S /bin/bash' : '',
            params.find_mito.clusterOptions ?: ''
        ].findAll { it }.join(' ')
        opts_str ?: null
    }

    tag "${id}"

    input:
        tuple val(id), file(assembly), path(hits), file(mitofinder_db), val(assembler), val(genetic_code), val(opts)

    output:
        tuple val(id),
            path("${outDir}/${id}_mito_contigs.fasta"),
            path("${outDir}/status.txt"),
            path("${outDir}/note.txt"),
            path("${outDir}/find_mito_candidates.csv"),
            val(assembler),
            path("${outDir}/find_mito.log")

    shell:
    outDir = "${id}/assemble/${assembler}"
    '''
    export OMP_NUM_THREADS=1 # fix for OpenBLAS blas_thread_init error
    mkdir -p !{outDir}

    Rscript -e 'MitoPilot::find_mito(
        assembly_fn = "!{assembly}",
        hits_fn = list.files(".", pattern = "^hits_.*txt$", full.names = TRUE),
        id = "!{id}",
        mitofinder_db = "!{mitofinder_db}",
        genetic_code = !{genetic_code ?: 2},
        min_identity = !{opts.min_identity},
        min_aligned_length = !{opts.min_aligned_length},
        min_aligned_fraction = !{opts.min_aligned_fraction},
        max_candidates = !{opts.max_candidates},
        min_genes = !{opts.min_genes},
        cpus = !{task.cpus},
        out_dir = "!{outDir}")'
    '''
}
