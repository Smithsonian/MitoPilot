process blast_ref_fetch {

    executor params.blast_gb.executor
    container params.blast_gb.container

    maxForks params.blast_gb.maxForks

    publishDir "${launchDir}/${params.publishDir}", overwrite: true, pattern: "${id}/assemble/${opts_id}/blast_ref_${blast_accession}/remote_blast_ref.json", mode: 'copy'

    cpus { (params.blast_gb.cpus instanceof Integer) ? params.blast_gb.cpus : 1 }
    memory = (params.blast_gb.memory instanceof Number) ? "${params.blast_gb.memory}.GB" : null
    clusterOptions {
        def opts = [
            (params.blast_gb.executor == 'sge') ? '-S /bin/bash' : '',
            (params.blast_gb.clusterOptions instanceof String) ? params.blast_gb.clusterOptions : ''
        ].findAll { it }.join(' ')
        opts ?: null
    }

    // 'ignore' keeps other samples running when this one times out. Failed tasks are NOT
    // cached as successful, so -resume will re-execute this step for the affected sample.
    errorStrategy { task.attempt <= 3 ? 'retry' : 'ignore' }
    maxRetries 3

    tag "${id}"

    input:
        tuple val(id), val(blast_accession), val(blast_species), val(blast_evalue), val(opts_id), val(is_top)

    output:
        tuple val(id), val(blast_accession), val(is_top), path("${outDir}/blast_ref_annotations.csv"), path("${outDir}/blast_ref_sequence.txt"), path("${outDir}/blast_ref_genetic_code.txt"), path("${outDir}/remote_blast_ref.json")

    shell:
    // Per-accession subdir keeps multi-accession-per-id runs from clobbering each other
    outDir = "${id}/assemble/${opts_id}/blast_ref_${blast_accession}"
    '''
    mkdir -p !{outDir}

    ann="!{outDir}/blast_ref_annotations.csv"
    seq="!{outDir}/blast_ref_sequence.txt"
    gc="!{outDir}/blast_ref_genetic_code.txt"
    json="!{outDir}/remote_blast_ref.json"

    # Per-accession shared cache to dedupe NCBI EFetch calls across samples whose top
    # hit is the same accession. The fetched data is accession-derived; the only
    # per-sample field (blast_evalue, plus blast_species) is re-stamped into the
    # copied JSON. Concurrency: an atomic `mkdir` lock makes the first task fetch the
    # accession while later tasks wait and then copy the cached result.
    CACHE_DIR="!{launchDir}/!{params.publishDir}/.blast_ref_cache/!{blast_accession}"
    DONE="$CACHE_DIR/.done"
    LOCK="$CACHE_DIR/.lock"
    MAX_WAIT=600   # seconds to wait for an in-progress fetch before fetching independently
    # The cache lives under the publishDir, which on some HPC setups is not
    # bind-mounted writable inside the container (read-only filesystem). Treat
    # the cache as best-effort: if we cannot create it, skip caching entirely
    # and fetch directly into the always-writable task work dir.
    CACHE_OK=1
    mkdir -p "$CACHE_DIR" 2>/dev/null || CACHE_OK=0

    copy_from_cache() {
        cp "$CACHE_DIR/blast_ref_annotations.csv"  "$ann" &&
        cp "$CACHE_DIR/blast_ref_sequence.txt"     "$seq" &&
        cp "$CACHE_DIR/blast_ref_genetic_code.txt" "$gc"  &&
        cp "$CACHE_DIR/remote_blast_ref.json"      "$json" || return 1
        # Re-stamp this sample's BLAST hit metadata onto the shared JSON (best-effort:
        # a stale evalue is preferable to discarding a valid cached copy)
        Rscript -e "MitoPilot::patch_blast_ref_meta('$json', '!{blast_species}', !{blast_evalue})" || true
    }

    HOLD=0
    if [ "$CACHE_OK" -eq 1 ]; then
        # Fast path: accession already cached by a prior task / run
        if [ -f "$DONE" ]; then
            copy_from_cache && exit 0
        fi

        # Try to become the fetcher; otherwise wait for the in-progress fetch to finish.
        waited=0
        while true; do
            if mkdir "$LOCK" 2>/dev/null; then
                HOLD=1
                trap 'rmdir "$LOCK" 2>/dev/null || true' EXIT
                break
            fi
            if [ -f "$DONE" ]; then
                copy_from_cache && exit 0
            fi
            if [ "$waited" -ge "$MAX_WAIT" ]; then
                # In-progress fetch did not finish in time (holder may have died);
                # fall back to fetching independently without touching the cache.
                break
            fi
            sleep 30
            waited=$(( waited + 30 ))
        done

        # It may have completed between our last check and acquiring the lock
        if [ -f "$DONE" ]; then
            copy_from_cache && exit 0
        fi
    fi

    # Back off on retries to give NCBI EFetch time to recover from rate limits
    if [ "!{task.attempt}" -gt 1 ]; then
        sleep $(( (!{task.attempt} - 1) * 30 ))
    fi
    # Optional NCBI API key raises EFetch rate limit (3 -> 10 req/s).
    # Read inside R via Sys.getenv("NCBI_API_KEY").
    export NCBI_API_KEY='!{params.ncbi_api_key ?: ""}'
    Rscript -e "MitoPilot::fetch_blast_ref('!{blast_accession}', '$ann', '$seq', '$gc', '$json', '!{blast_species}', !{blast_evalue})"

    # Populate the cache for other samples, but only if we hold the lock. Write the
    # data files first and the .done marker last so readers never see a partial cache.
    # Cache population is best-effort: its failure must not fail an otherwise-good
    # sample (|| true), and .done is only touched after all files are copied.
    if [ "$HOLD" -eq 1 ]; then
        { cp "$ann"  "$CACHE_DIR/blast_ref_annotations.csv"  &&
          cp "$seq"  "$CACHE_DIR/blast_ref_sequence.txt"     &&
          cp "$gc"   "$CACHE_DIR/blast_ref_genetic_code.txt" &&
          cp "$json" "$CACHE_DIR/remote_blast_ref.json"      &&
          touch "$DONE"; } || true
    fi
    '''
}
