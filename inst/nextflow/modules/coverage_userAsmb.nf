process coverage_userAsmb {
    executor params.coverage.executor
    container params.coverage.container

    publishDir "$launchDir/${params.publishDir}", overwrite: true, mode: 'copy'

    errorStrategy 'ignore'
    cpus {params.coverage.cpus}
    // GB from config; a bare number would be read as BYTES.
    memory { (params.coverage.memory instanceof Number) ? params.coverage.memory.GB * task.attempt : null }
    clusterOptions {
        def opts = [
            (params.coverage.executor == 'sge') ? '-S /bin/bash' : '',
            params.coverage.clusterOptions ?: ''
        ].findAll { it }.join(' ')
        opts ?: null
    }

    tag "${id}"

    input:
        tuple val(id), path(reads), file(assembly), val(topology_map), val(assembler)

    output:
        tuple path("${outDir}/*"),     // output files
            path("${id}/assemble/${assembler}/NF_work_dir_coverage.txt"), // troubleshooting file
            val(id),  // ID
            path("${outDir}/${id}_assembly_1.fasta"),  // assembly fasta
            val(assembler)  // opts_id (assemble_opts key)

    shell:
    outDir = "${id}/assemble/${assembler}"
    output_name = assembly.baseName
    '''
    export OMP_NUM_THREADS=1 # fix for OpenBLAS blas_thread_init error
    mkdir -p !{outDir}

    # Materialize the per-contig topology map in this task's own directory. A
    # quoted heredoc means its newlines never pass through shell quoting.
    cat > topology_map.txt <<'TOPOLOGY_MAP_EOF'
!{topology_map}
TOPOLOGY_MAP_EOF

    # Rename assembly file and contig(s), stamping each record with its OWN
    # topology. The map branch is keyed on FILENAME, not the NR==FNR idiom,
    # which would swallow the assembly's first header if the map were empty.
    # Records are renamed to id.1.N as we go, so the lookup uses the incoming
    # contig name; "*" is the default for a sample that was never circularized
    # and so has no contig names of its own.
    awk -v mapf=topology_map.txt 'FILENAME == mapf {topo[$1] = $2; next}
         /^>/ {key = substr($1, 2)
               t = (key in topo) ? topo[key] : topo["*"]
               if (t == "") {t = "linear"}
               print ">!{id}.1." ++count[">"] " " t
               next}
         {print}' topology_map.txt !{assembly} > !{outDir}/!{id}_assembly_1.fasta

    # calculate coverage
    Rscript -e "MitoPilot::coverage('!{outDir}/!{id}_assembly_1.fasta', '!{reads[0]}', '!{reads[1]}', 'NA', !{task.cpus}, '!{outDir}')"

    # cleanup
    rm -f !{outDir}/*_working.fasta*

    ### work dir info for troubleshooting ####
    echo "Nextflow coverage working directory:" > !{outDir}/NF_work_dir_coverage.txt
    echo "$PWD" >> !{outDir}/NF_work_dir_coverage.txt
    '''

}

// No-reads variant: same staging + DB-facing outputs, but coverage() is invoked
// with 'NA' reads so it synthesizes the coverageStats table from the assembly
// (GC only; depth/error empty). No read mapping is performed.
process coverage_userAsmb_noReads {
    executor params.coverage.executor
    container params.coverage.container

    publishDir "$launchDir/${params.publishDir}", overwrite: true, mode: 'copy'

    errorStrategy 'ignore'
    cpus {params.coverage.cpus}
    // GB from config; a bare number would be read as BYTES.
    memory { (params.coverage.memory instanceof Number) ? params.coverage.memory.GB * task.attempt : null }
    clusterOptions {
        def opts = [
            (params.coverage.executor == 'sge') ? '-S /bin/bash' : '',
            params.coverage.clusterOptions ?: ''
        ].findAll { it }.join(' ')
        opts ?: null
    }

    tag "${id}"

    input:
        tuple val(id), file(assembly), val(topology_map), val(assembler)

    output:
        tuple path("${outDir}/*"),     // output files
            path("${id}/assemble/${assembler}/NF_work_dir_coverage.txt"), // troubleshooting file
            val(id),  // ID
            path("${outDir}/${id}_assembly_1.fasta"),  // assembly fasta
            val(assembler)  // opts_id (assemble_opts key)

    shell:
    outDir = "${id}/assemble/${assembler}"
    output_name = assembly.baseName
    '''
    export OMP_NUM_THREADS=1 # fix for OpenBLAS blas_thread_init error
    mkdir -p !{outDir}

    # Materialize the per-contig topology map in this task's own directory. A
    # quoted heredoc means its newlines never pass through shell quoting.
    cat > topology_map.txt <<'TOPOLOGY_MAP_EOF'
!{topology_map}
TOPOLOGY_MAP_EOF

    # Rename assembly file and contig(s), stamping each record with its OWN
    # topology. The map branch is keyed on FILENAME, not the NR==FNR idiom,
    # which would swallow the assembly's first header if the map were empty.
    # Records are renamed to id.1.N as we go, so the lookup uses the incoming
    # contig name; "*" is the default for a sample that was never circularized
    # and so has no contig names of its own.
    awk -v mapf=topology_map.txt 'FILENAME == mapf {topo[$1] = $2; next}
         /^>/ {key = substr($1, 2)
               t = (key in topo) ? topo[key] : topo["*"]
               if (t == "") {t = "linear"}
               print ">!{id}.1." ++count[">"] " " t
               next}
         {print}' topology_map.txt !{assembly} > !{outDir}/!{id}_assembly_1.fasta

    # derive coverage stats from the assembly (no reads)
    Rscript -e "MitoPilot::coverage('!{outDir}/!{id}_assembly_1.fasta', 'NA', 'NA', 'NA', !{task.cpus}, '!{outDir}')"

    # cleanup
    rm -f !{outDir}/*_working.fasta*

    ### work dir info for troubleshooting ####
    echo "Nextflow coverage working directory:" > !{outDir}/NF_work_dir_coverage.txt
    echo "$PWD" >> !{outDir}/NF_work_dir_coverage.txt
    '''

}
