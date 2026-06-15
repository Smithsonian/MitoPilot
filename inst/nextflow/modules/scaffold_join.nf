process scaffold_join {

    stageInMode 'copy'

    executor params.scaffold_join.executor
    container params.scaffold_join.container
    cpus { params.scaffold_join.cpus }
    memory = params.scaffold_join.memory ?: null
    clusterOptions {
        def opts = [
            (params.scaffold_join.executor == 'sge') ? '-S /bin/bash' : '',
            (params.scaffold_join.clusterOptions instanceof String) ? params.scaffold_join.clusterOptions : ''
        ].findAll { it }.join(' ')
        opts ?: null
    }

    publishDir "${launchDir}/${params.publishDir}", overwrite: true, mode: 'copy'

    errorStrategy 'ignore'

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
        tuple val(id),
            path("${id}/assemble/${opts}/${id}_joined_row.csv"), optional: true, emit: row

    shell:
    dir = "${id}/assemble/${opts}"
    cov_list = (cov_csvs instanceof List ? cov_csvs : [cov_csvs]).collect { "'" + it + "'" }.join(', ')
    auto = ((auto_join as Integer) == 1) ? 'TRUE' : 'FALSE'
    '''
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
