process blast_genbank {

    executor params.blast_gb.executor
    container params.blast_gb.container

    maxForks params.blast_gb.maxForks

    // Retry up to 3 times (default) before ignoring (empty output = possible connection failure).
    // 'ignore' after retries keeps other samples running; failed tasks are NOT cached as
    // successful, so -resume will re-execute this step for the affected sample.
    errorStrategy { task.attempt <= 3 ? 'retry' : 'ignore' }
    maxRetries { params.blast_gb.maxRetries }

    cpus { (params.blast_gb.cpus instanceof Integer) ? params.blast_gb.cpus : 1 }
    memory = (params.blast_gb.memory instanceof Number) ? "${params.blast_gb.memory}.GB" : null
    clusterOptions {
        def opts = [
            (params.blast_gb.executor == 'sge') ? '-S /bin/bash' : '',
            (params.blast_gb.clusterOptions instanceof String) ? params.blast_gb.clusterOptions : ''
        ].findAll { it }.join(' ')
        opts ?: null
    }

    publishDir "${launchDir}/${params.publishDir}", overwrite: true, mode: 'copy'



    tag "${id}.${path_idx}"

    input:
        tuple val(id), val(path_idx), path(assembly), val(opts_id), val(entrez_query), val(extra_opts), val(max_target_seqs)

    output:
        tuple val(id), val(path_idx), path("${outDir}/${outFile}")

    shell:
    outDir = "${id}/assemble/${opts_id}"
    outFile = "blast_genbank_${path_idx}.txt"
    // Omit -entrez_query entirely when unset (a literal "null"/"" filters out all hits)
    entrez = entrez_query ? "-entrez_query \"${entrez_query}\"" : ""
    '''
    mkdir -p !{outDir}
    # Back off on retries to give NCBI BLAST time to recover from rate limits
    if [ "!{task.attempt}" -gt 1 ]; then
        sleep $(( (!{task.attempt} - 1) * 60 ))
    fi
    # Optional NCBI API key raises remote BLAST rate limit. BLAST+ honors NCBI_API_KEY env var.
    export NCBI_API_KEY='!{params.ncbi_api_key ?: ""}'
    # Distinguish a genuine zero-hit result from a connection/tool failure:
    #   - blastn non-zero exit  -> connection/tool error; exit 1 so Nextflow retries.
    #   - blastn exit 0, empty  -> genuine "no significant hits"; retrying is pointless,
    #                              so write a sentinel and succeed. The workflow flags it
    #                              with a distinct note (vs the no-output connection path).
    if blastn \
        -remote \
        -db core_nt \
        -query !{assembly} \
        -outfmt "6 qseqid saccver stitle pident qcovs evalue" \
        -max_target_seqs !{max_target_seqs ?: 5} \
        -max_hsps 1 \
        -task megablast \
        !{entrez} \
        !{extra_opts} \
        > !{outDir}/!{outFile}; then
        if [ ! -s !{outDir}/!{outFile} ]; then
            echo "NO_SIGNIFICANT_HITS" > !{outDir}/!{outFile}
        fi
    else
        exit 1
    fi
    '''
}
