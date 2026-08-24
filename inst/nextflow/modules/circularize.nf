// Optional WF1 step for user-provided assemblies: trim a redundant end-to-start
// overlap from a linear contig and, when reads are available, require reads
// spanning the new junction before calling the assembly circular.
// See R/circularize_asmb.R.
process circularize {
    executor params.circularize.executor
    container params.circularize.container

    publishDir "$launchDir/${params.publishDir}", overwrite: true, mode: 'copy',
        pattern: "${id}/assemble/${assembler}/circularize.log"

    errorStrategy 'ignore'
    clusterOptions {
        def opts_str = [
            (params.circularize.executor == 'sge') ? '-S /bin/bash' : '',
            params.circularize.clusterOptions ?: ''
        ].findAll { it }.join(' ')
        opts_str ?: null
    }

    tag "${id}"

    input:
        tuple val(id), path(reads), file(assembly), val(assembler), val(opts)

    output:
        tuple val(id),
            path("${outDir}/${id}_circularized.fasta"),
            path("${outDir}/topology.txt"),
            path("${outDir}/note.txt"),
            val(assembler),
            path("${outDir}/circularize.log")

    shell:
    outDir = "${id}/assemble/${assembler}"
    '''
    export OMP_NUM_THREADS=1 # fix for OpenBLAS blas_thread_init error
    mkdir -p !{outDir}

    Rscript -e 'res <- MitoPilot::circularize_asmb(
        "!{assembly}", "!{reads[0]}", "!{reads[1]}",
        min_overlap = !{opts.min_overlap},
        min_identity = !{opts.min_identity},
        min_junction_reads = !{opts.min_junction_reads},
        min_overhang = !{opts.min_overhang},
        cpus = !{task.cpus},
        out_fn = "!{outDir}/!{id}_circularized.fasta",
        log_fn = "!{outDir}/circularize.log");
      writeLines(if (res$circular) "circular" else "linear", "!{outDir}/topology.txt");
      writeLines(res$note, "!{outDir}/note.txt")'
    '''
}

// No-reads variant: overlap trimming only, nothing to confirm the junction with.
process circularize_noReads {
    executor params.circularize.executor
    container params.circularize.container

    publishDir "$launchDir/${params.publishDir}", overwrite: true, mode: 'copy',
        pattern: "${id}/assemble/${assembler}/circularize.log"

    errorStrategy 'ignore'
    clusterOptions {
        def opts_str = [
            (params.circularize.executor == 'sge') ? '-S /bin/bash' : '',
            params.circularize.clusterOptions ?: ''
        ].findAll { it }.join(' ')
        opts_str ?: null
    }

    tag "${id}"

    input:
        tuple val(id), file(assembly), val(assembler), val(opts)

    output:
        tuple val(id),
            path("${outDir}/${id}_circularized.fasta"),
            path("${outDir}/topology.txt"),
            path("${outDir}/note.txt"),
            val(assembler),
            path("${outDir}/circularize.log")

    shell:
    outDir = "${id}/assemble/${assembler}"
    '''
    export OMP_NUM_THREADS=1 # fix for OpenBLAS blas_thread_init error
    mkdir -p !{outDir}

    Rscript -e 'res <- MitoPilot::circularize_asmb(
        "!{assembly}", "NA", "NA",
        min_overlap = !{opts.min_overlap},
        min_identity = !{opts.min_identity},
        cpus = !{task.cpus},
        out_fn = "!{outDir}/!{id}_circularized.fasta",
        log_fn = "!{outDir}/circularize.log");
      writeLines(if (res$circular) "circular" else "linear", "!{outDir}/topology.txt");
      writeLines(res$note, "!{outDir}/note.txt")'
    '''
}
