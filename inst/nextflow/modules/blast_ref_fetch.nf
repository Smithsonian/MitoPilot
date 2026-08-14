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

    // One Rscript doing HTTP: network-bound, never CPU-bound. Deliberately NOT
    // tied to params.blast_gb.cpus, which now sizes the local blastn search;
    // inheriting that would give every fetch task the search's cores (times
    // maxForks) and throttle the local executor for no gain.
    cpus 1
    // Method-call form, NOT `memory = ...`: the assignment form is not a process
    // directive, so the value is dropped and the generic process block in .config
    // wins instead (1 byte on config.local, a hard task failure on the scheduler
    // templates, whose closure dereferences an `opts` input this process lacks).
    memory { (params.blast_gb.memory instanceof Number) ? params.blast_gb.memory.GB : null }
    clusterOptions {
        // This process runs at 1 CPU, so it does not inherit a clusterOptions
        // string sized for the multi-CPU local blastn search. Sites that encode
        // memory there (Hydra) set blast_gb.ref_fetch_clusterOptions instead;
        // where that key is absent the shared string is used, as before.
        def co = (params.blast_gb.ref_fetch_clusterOptions instanceof String)
            ? params.blast_gb.ref_fetch_clusterOptions
            : ((params.blast_gb.clusterOptions instanceof String) ? params.blast_gb.clusterOptions : '')
        def opts = [
            (params.blast_gb.executor == 'sge') ? '-S /bin/bash' : '',
            co
        ].findAll { it }.join(' ')
        opts ?: null
    }

    // 'ignore' keeps other batches running when this one times out. Failed tasks are
    // NOT cached as successful, so -resume re-executes this step. Within a batch,
    // fetch_blast_refs writes one ref_<accession>/ dir per accession it can fetch and
    // simply omits the rest, so a dropped accession produces no fan-out and every
    // sample whose top hit was that accession is flagged as a fetch failure downstream
    // (see the all_top_ids join in the workflow).
    // Exit 140 (128+12, SIGUSR2) is an SGE resource kill, and 137 (128+9) an OOM
    // kill. Both are deterministic: the same task on the same node fails the same
    // way, so retrying only burns queue time (0+30+60+90 s of backoff here).
    errorStrategy { (task.exitStatus in [137, 140]) ? 'ignore'
                    : (task.attempt <= 3 ? 'retry' : 'ignore') }
    maxRetries 3

    // No task tag: with a tag, Nextflow's ANSI progress truncates this process's
    // name differently in the tagged (running) frame vs the untagged placeholder
    // frame, which the app's progress parser then keys as two separate lines.
    // Omitting the tag keeps a single, consistently-named progress line.

    input:
        tuple val(id), val(opts_id), val(accessions)

    output:
        tuple val(id), val(opts_id), path("ref_*", type: 'dir', optional: true)

    shell:
    // One Rscript per SAMPLE: fetch_blast_refs makes 1 GFF3 + 1 FASTA request for
    // the sample's accessions (+ one taxonomy request per unique taxid), writing
    // one ref_<accession>/ dir each. Per-sample batching keeps each sample's fetch
    // independent (streams + re-runs cleanly). Any accession the batch can't fetch
    // falls back to a per-accession request inside fetch_blast_refs.
    acc_csv = accessions.join(',')
    '''
    export OMP_NUM_THREADS=1 # fix for OpenBLAS blas_thread_init error
    # Not just the blas_thread_init error: the pthread OpenBLAS in the image sizes
    # its per-thread buffers by core count, so R reserves GBs of VIRTUAL address
    # space before running a line of code (measured: 268 MB with this set, 2.2 G on
    # 16 cores, 6.2 G on 48). Schedulers that cap address space (SGE h_vmem) then
    # kill the task with SIGUSR2 (exit 140) before the first HTTP request, and
    # raising the cap does not help because the reservation scales with the node.
    # Back off on retries to give NCBI EFetch time to recover from rate limits
    if [ "!{task.attempt}" -gt 1 ]; then
        sleep $(( (!{task.attempt} - 1) * 30 ))
    fi
    # Optional NCBI API key raises EFetch rate limit (3 -> 10 req/s).
    # Read inside R via Sys.getenv("NCBI_API_KEY").
    export NCBI_API_KEY='!{params.ncbi_api_key ?: ""}'
    # Accession-derived data only; per-sample blast_species/blast_evalue are stamped
    # later by blast_ref_stamp via MitoPilot::patch_blast_ref_meta.
    Rscript -e "MitoPilot::fetch_blast_refs(strsplit('!{acc_csv}', ',')[[1]], '.')"
    '''
}

// Per-sample fan-out for one fetched accession. Copies the accession-derived files
// into this sample's publish path and re-stamps the per-sample BLAST hit metadata
// (blast_species, blast_evalue) onto the shared JSON. Pure local work (no network),
// so it never hits NCBI rate limits and cannot fail an otherwise-good fetch.
process blast_ref_stamp {

    executor params.blast_gb.executor
    container params.blast_gb.container

    cpus 1
    // Declared even though this process is trivial: without a memory directive the
    // generic process block in .config supplies one, and on the scheduler templates
    // that closure dereferences an `opts` process input this process does not have,
    // which fails the task at submission.
    memory { (params.blast_gb.memory instanceof Number) ? params.blast_gb.memory.GB : null }
    clusterOptions {
        // This process runs at 1 CPU, so it does not inherit a clusterOptions
        // string sized for the multi-CPU local blastn search. Sites that encode
        // memory there (Hydra) set blast_gb.ref_fetch_clusterOptions instead;
        // where that key is absent the shared string is used, as before.
        def co = (params.blast_gb.ref_fetch_clusterOptions instanceof String)
            ? params.blast_gb.ref_fetch_clusterOptions
            : ((params.blast_gb.clusterOptions instanceof String) ? params.blast_gb.clusterOptions : '')
        def opts = [
            (params.blast_gb.executor == 'sge') ? '-S /bin/bash' : '',
            co
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
    export OMP_NUM_THREADS=1 # fix for OpenBLAS blas_thread_init error
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
