// Fetch the NCBI reference for one accession exactly once. Cross-sample dedup is
// done at the channel level (see blast_ref_fetch_workflow.nf): the workflow feeds
// this process the set of UNIQUE accessions, and the per-sample fan-out / metadata
// stamping happens downstream in blast_ref_stamp. This removes the previous
// filesystem cache, which required a shared writable mount (the publishDir), and
// broke on HPC setups where that path is read-only inside the container.
process blast_ref_fetch {

    executor params.blast_gb.executor
    container params.blast_gb.container

    maxForks params.blast_gb.maxForks

    cpus { (params.blast_gb.cpus instanceof Integer) ? params.blast_gb.cpus : 1 }
    memory = (params.blast_gb.memory instanceof Number) ? "${params.blast_gb.memory}.GB" : null
    clusterOptions {
        def opts = [
            (params.blast_gb.executor == 'sge') ? '-S /bin/bash' : '',
            (params.blast_gb.clusterOptions instanceof String) ? params.blast_gb.clusterOptions : ''
        ].findAll { it }.join(' ')
        opts ?: null
    }

    // 'ignore' keeps other accessions running when this one times out. Failed tasks are
    // NOT cached as successful, so -resume re-executes this step. A dropped accession
    // produces no fan-out, so every sample whose top hit was that accession is flagged
    // as a fetch failure downstream (see the all_top_ids join in the workflow).
    errorStrategy { task.attempt <= 3 ? 'retry' : 'ignore' }
    maxRetries 3

    tag "${blast_accession}"

    input:
        val(blast_accession)

    output:
        tuple val(blast_accession), path("${outDir}/blast_ref_annotations.csv"), path("${outDir}/blast_ref_sequence.txt"), path("${outDir}/blast_ref_genetic_code.txt"), path("${outDir}/remote_blast_ref.json")

    shell:
    outDir = "ref_${blast_accession}"
    '''
    mkdir -p !{outDir}

    ann="!{outDir}/blast_ref_annotations.csv"
    seq="!{outDir}/blast_ref_sequence.txt"
    gc="!{outDir}/blast_ref_genetic_code.txt"
    json="!{outDir}/remote_blast_ref.json"

    # Back off on retries to give NCBI EFetch time to recover from rate limits
    if [ "!{task.attempt}" -gt 1 ]; then
        sleep $(( (!{task.attempt} - 1) * 30 ))
    fi
    # Optional NCBI API key raises EFetch rate limit (3 -> 10 req/s).
    # Read inside R via Sys.getenv("NCBI_API_KEY").
    export NCBI_API_KEY='!{params.ncbi_api_key ?: ""}'
    # Accession-derived data only; per-sample blast_species/blast_evalue are stamped
    # later by blast_ref_stamp via MitoPilot::patch_blast_ref_meta.
    Rscript -e "MitoPilot::fetch_blast_ref('!{blast_accession}', '$ann', '$seq', '$gc', '$json')"
    '''
}

// Per-sample fan-out for one fetched accession. Copies the accession-derived files
// into this sample's publish path and re-stamps the per-sample BLAST hit metadata
// (blast_species, blast_evalue) onto the shared JSON. Pure local work (no network),
// so it never hits NCBI rate limits and cannot fail an otherwise-good fetch.
process blast_ref_stamp {

    executor params.blast_gb.executor
    container params.blast_gb.container

    clusterOptions {
        def opts = [
            (params.blast_gb.executor == 'sge') ? '-S /bin/bash' : '',
            (params.blast_gb.clusterOptions instanceof String) ? params.blast_gb.clusterOptions : ''
        ].findAll { it }.join(' ')
        opts ?: null
    }

    publishDir "${launchDir}/${params.publishDir}", overwrite: true, pattern: "${id}/assemble/${opts_id}/blast_ref_${blast_accession}/remote_blast_ref.json", mode: 'copy'

    tag "${id}"

    input:
        tuple val(id), val(blast_accession), val(blast_species), val(blast_evalue), val(opts_id), val(is_top), path(base_ann), path(base_seq), path(base_gc), path(base_json)

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

    cp "!{base_ann}"  "$ann"
    cp "!{base_seq}"  "$seq"
    cp "!{base_gc}"   "$gc"
    cp "!{base_json}" "$json"

    # Stamp this sample's BLAST hit metadata onto the shared JSON (best-effort:
    # a stale evalue is preferable to discarding a valid reference copy)
    Rscript -e "MitoPilot::patch_blast_ref_meta('$json', '!{blast_species}', !{blast_evalue})" || true
    '''
}
