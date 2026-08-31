// Optional WF1 step for user-provided assemblies: trim a redundant end-to-start
// overlap from a linear contig and, when reads are available, require reads
// spanning the new junction before calling that contig circular. Every contig
// of the assembly is attempted independently, so the process emits a topology
// MAP (topology_map.txt: "<contig> circular|linear", one contig per line,
// space separated) rather than one topology value for the whole sample.
// See R/circularize_asmb.R.
process circularize {
    executor params.circularize?.executor ?: params.assemble.executor
    container params.circularize?.container ?: params.assemble.container

    publishDir "$launchDir/${params.publishDir}", overwrite: true, mode: 'copy',
        pattern: "${id}/assemble/${assembler}/circularize*"

    errorStrategy 'ignore'
    clusterOptions {
        def opts_str = [
            (params.circularize?.executor ?: params.assemble.executor) == 'sge' ? '-S /bin/bash' : '',
            params.circularize?.clusterOptions ?: ''
        ].findAll { it }.join(' ')
        opts_str ?: null
    }

    tag "${id}"

    input:
        tuple val(id), path(reads), file(assembly), val(assembler), val(opts)

    output:
        tuple val(id),
            path("${outDir}/${id}_circularized.fasta"),
            path("${outDir}/topology_map.txt"),
            path("${outDir}/note.txt"),
            path("${outDir}/circularize_overlap.csv"),
            path("${outDir}/circularize_depth.csv"),
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
        id = "!{id}",
        evidence_dir = "!{outDir}",
        out_fn = "!{outDir}/!{id}_circularized.fasta",
        log_fn = "!{outDir}/circularize.log");
      writeLines(vapply(res$contigs, function(ctg)
        paste(ctg$contig, if (ctg$circular) "circular" else "linear"),
        character(1)), "!{outDir}/topology_map.txt");
      writeLines(res$note, "!{outDir}/note.txt")'

    # No map means nothing was examined (too many contigs, or none at all), so
    # no contig gets a topology claimed for it.
    [ -s !{outDir}/topology_map.txt ] || echo "* unknown" > !{outDir}/topology_map.txt
    '''
}

// No-reads variant: overlap trimming only, nothing to confirm the junction with.
process circularize_noReads {
    executor params.circularize?.executor ?: params.assemble.executor
    container params.circularize?.container ?: params.assemble.container

    publishDir "$launchDir/${params.publishDir}", overwrite: true, mode: 'copy',
        pattern: "${id}/assemble/${assembler}/circularize*"

    errorStrategy 'ignore'
    clusterOptions {
        def opts_str = [
            (params.circularize?.executor ?: params.assemble.executor) == 'sge' ? '-S /bin/bash' : '',
            params.circularize?.clusterOptions ?: ''
        ].findAll { it }.join(' ')
        opts_str ?: null
    }

    tag "${id}"

    input:
        tuple val(id), file(assembly), val(assembler), val(opts)

    output:
        tuple val(id),
            path("${outDir}/${id}_circularized.fasta"),
            path("${outDir}/topology_map.txt"),
            path("${outDir}/note.txt"),
            path("${outDir}/circularize_overlap.csv"),
            path("${outDir}/circularize_depth.csv"),
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
        min_junction_reads = !{opts.min_junction_reads},
        min_overhang = !{opts.min_overhang},
        cpus = !{task.cpus},
        id = "!{id}",
        evidence_dir = "!{outDir}",
        out_fn = "!{outDir}/!{id}_circularized.fasta",
        log_fn = "!{outDir}/circularize.log");
      writeLines(vapply(res$contigs, function(ctg)
        paste(ctg$contig, if (ctg$circular) "circular" else "linear"),
        character(1)), "!{outDir}/topology_map.txt");
      writeLines(res$note, "!{outDir}/note.txt")'

    # No map means nothing was examined (too many contigs, or none at all), so
    # no contig gets a topology claimed for it.
    [ -s !{outDir}/topology_map.txt ] || echo "* unknown" > !{outDir}/topology_map.txt
    '''
}
