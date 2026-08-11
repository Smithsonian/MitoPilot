include { clusterOpts } from './cluster_opts.nf'

process blast_genbank {

    executor params.blast_gb.executor
    container params.blast_gb.container

    // The BLAST target FASTA is a staged path input but is regenerated on every
    // run (deterministic content), so its last-modified time changes each time.
    // Default caching keys staged files on mtime, so -resume always re-ran this
    // slow remote BLAST. Lenient caching hashes staged inputs by name + size only
    // (ignoring mtime), so an unchanged target caches across resumes.
    cache 'lenient'

    maxForks params.blast_gb.maxForks

    // Retry up to 3 times (default) before ignoring (empty output = possible connection failure).
    // 'ignore' after retries keeps other samples running; failed tasks are NOT cached as
    // successful, so -resume will re-execute this step for the affected sample.
    errorStrategy { task.attempt <= 3 ? 'retry' : 'ignore' }
    maxRetries { params.blast_gb.maxRetries }

    cpus { (params.blast_gb.cpus instanceof Integer) ? params.blast_gb.cpus : 1 }
    memory = (params.blast_gb.memory instanceof Number) ? "${params.blast_gb.memory}.GB" : null
    clusterOptions { clusterOpts(params.blast_gb) }

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
    #   - blastn non-zero exit          -> connection/tool error; exit 1 so Nextflow retries.
    #   - blastn exit 0, empty + stderr -> remote server error (e.g. NCBI queue DB
    #                                      failure) that does NOT set a non-zero exit;
    #                                      exit 1 so Nextflow retries.
    #   - blastn exit 0, empty, clean   -> genuine "no significant hits"; retrying is
    #                                      pointless, so write a sentinel and succeed.
    #                                      The workflow flags it with a distinct note.
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
        > !{outDir}/!{outFile} 2> blast.err; then
        if [ ! -s !{outDir}/!{outFile} ]; then
            if grep -qiE 'error|bad_request|could not queue|failed|exception' blast.err; then
                cat blast.err >&2
                exit 1
            fi
            echo "NO_SIGNIFICANT_HITS" > !{outDir}/!{outFile}
        fi
    else
        cat blast.err >&2
        exit 1
    fi
    '''
}
