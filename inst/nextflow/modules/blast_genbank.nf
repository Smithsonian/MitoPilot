process blast_genbank {

    executor params.blast_gb.executor
    container params.blast_gb.container

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

    // Retry up to 3 times before ignoring (empty output = possible connection failure).
    // 'ignore' after retries keeps other samples running; failed tasks are NOT cached as
    // successful, so -resume will re-execute this step for the affected sample.
    errorStrategy { task.attempt <= 3 ? 'retry' : 'ignore' }
    maxRetries 3

    tag "${id}"

    input:
        tuple val(id), path(assembly), val(opts_id), val(entrez_query), val(extra_opts)

    output:
        tuple val(id), path("${outDir}/blast_genbank.txt")

    shell:
    outDir = "${id}/assemble/${opts_id}"
    '''
    mkdir -p !{outDir}
    # Back off on retries to give NCBI BLAST time to recover from rate limits
    if [ "!{task.attempt}" -gt 1 ]; then
        sleep $(( (!{task.attempt} - 1) * 60 ))
    fi
    # Optional NCBI API key raises remote BLAST rate limit. BLAST+ honors NCBI_API_KEY env var.
    export NCBI_API_KEY='!{params.ncbi_api_key ?: ""}'
    blastn \
        -remote \
        -db nt \
        -query !{assembly} \
        -outfmt "6 qseqid saccver stitle pident qcovs evalue" \
        -max_target_seqs 1 \
        -max_hsps 1 \
        -task megablast \
        -entrez_query "!{entrez_query}" \
        !{extra_opts} \
        > !{outDir}/blast_genbank.txt
    # Fail if output is empty; triggers retry so transient connection failures are retried
    [ -s !{outDir}/blast_genbank.txt ] || exit 1
    '''
}
