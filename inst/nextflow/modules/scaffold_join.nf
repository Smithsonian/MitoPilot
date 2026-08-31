process scaffold_join {

    stageInMode 'copy'

    executor params.scaffold_join.executor
    container params.scaffold_join.container
    cpus { params.scaffold_join.cpus }
    // Config memory is a plain number of GB. A bare number is BYTES to Nextflow,
    // so it must be converted; scaling by task.attempt lets a scheduler memory
    // kill (SGE exit 137-140) self-heal on retry instead of dying identically.
    memory { (params.scaffold_join.memory instanceof Number) ? params.scaffold_join.memory.GB * task.attempt : null }
    clusterOptions {
        // A Closure lets a site config scale its own reservation with the retry
        // attempt (schedulers like SGE take memory from clusterOptions, not from
        // the memory directive).
        def co = params.scaffold_join.clusterOptions
        def co_str = (co instanceof Closure) ? co.call(task.attempt) : ((co instanceof String) ? co : '')
        def opts = [
            (params.scaffold_join.executor == 'sge') ? '-S /bin/bash' : '',
            co_str
        ].findAll { it }.join(' ')
        opts ?: null
    }

    publishDir "${launchDir}/${params.publishDir}", overwrite: true, mode: 'copy'

    // Retry transient failures (notably SGE submission rejections during bursts)
    // before ignoring. A submission failure that is ignored on the first attempt
    // leaves this process's mandatory 'mappings' output channel unclosed, which
    // hangs every downstream operator and stalls the whole workflow. Retrying
    // absorbs the transient qsub errors that trigger it.
    errorStrategy { task.attempt <= (params.scaffold_join.maxRetries instanceof Integer ? params.scaffold_join.maxRetries : 3) ? 'retry' : 'ignore' }
    maxRetries { params.scaffold_join.maxRetries instanceof Integer ? params.scaffold_join.maxRetries : 3 }

    tag "${id}"

    input:
        tuple val(id), path(assembly), val(opts), val(auto_join), path(cov_csvs), path(ref_seq), val(scaffold_hits)

    output:
        // Joined Path 0 outputs are produced only when auto-join builds it
        // (toggle on AND scaffold BLAST hits agree); optional otherwise.
        tuple val(id),
            path("${id}/assemble/${opts}/${id}_assembly_0.fasta"),
            path("${id}/assemble/${opts}/${id}_assembly_0_coverageStats.csv"),
            optional: true, emit: fasta
        tuple val(id),
            path("${id}/assemble/${opts}/${id}_scaffold_mappings.csv"), emit: mappings
        // Always written (header only when nothing was joined), so the write
        // process can clear stale rows for a sample that no longer joins.
        tuple val(id),
            path("${id}/assemble/${opts}/${id}_scaffold_junctions.csv"), emit: junctions
        tuple val(id),
            path("${id}/assemble/${opts}/${id}_joined_row.csv"), optional: true, emit: row
        // Written on EVERY non-crash exit of run_scaffold_join(): status is one
        // of joined / declined / skipped, note is zero bytes unless there is a
        // reason. Not optional, so absence of this pair means the task died.
        tuple val(id),
            path("${id}/assemble/${opts}/join_status.txt"),
            path("${id}/assemble/${opts}/join_note.txt"), emit: outcome

    shell:
    dir = "${id}/assemble/${opts}"
    cov_list = (cov_csvs instanceof List ? cov_csvs : [cov_csvs]).collect { "'" + it + "'" }.join(', ')
    auto = ((auto_join as Integer) == 1) ? 'TRUE' : 'FALSE'
    '''
    export OMP_NUM_THREADS=1 # fix for OpenBLAS blas_thread_init error
    mkdir -p !{dir}

    # The reference fetch stores a bare sequence; wrap it as FASTA if needed.
    if head -c1 "!{ref_seq}" | grep -q '>'; then
        cp "!{ref_seq}" ref.fa
    else
        printf ">ref\\n" > ref.fa
        cat "!{ref_seq}" >> ref.fa
    fi

    Rscript -e "MitoPilot::run_scaffold_join('!{assembly}', c(!{cov_list}), 'ref.fa', '!{id}', '!{dir}', db='!{launchDir}/.sqlite', auto_join=!{auto}, scaffold_hits='!{scaffold_hits}')"
    '''
}
