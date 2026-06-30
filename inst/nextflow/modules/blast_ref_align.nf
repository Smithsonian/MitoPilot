process blast_ref_align {

    executor params.blast_ref_align.executor
    container params.blast_ref_align.container

    cpus { (params.blast_ref_align.cpus instanceof Integer) ? params.blast_ref_align.cpus : 1 }
    memory = (params.blast_ref_align.memory instanceof Number) ? "${params.blast_ref_align.memory}.GB" : null
    clusterOptions {
        def opts = [
            (params.blast_ref_align.executor == 'sge') ? '-S /bin/bash' : '',
            (params.blast_ref_align.clusterOptions instanceof String) ? params.blast_ref_align.clusterOptions : ''
        ].findAll { it }.join(' ')
        opts ?: null
    }

    errorStrategy 'ignore'

    tag "${id}.${accession}"

    input:
        // assembly_seq and ref_seq are plain nucleotide strings (no headers)
        // rotation is the 0-based offset into the reference for the anchor gene
        tuple val(id), val(accession), val(assembly_seq), val(ref_seq), val(rotation)

    output:
        tuple val(id), val(accession), path("${id}/annotate/blast_ref_alignment_${accession}.csv")

    shell:
    outDir = "${id}/annotate"
    outFile = "blast_ref_alignment_${accession}.csv"
    '''
    mkdir -p !{outDir}
    # Write sequences to files — avoids Rscript -e expression length limits
    printf '%s\n' '!{assembly_seq}' > _assembly.txt
    printf '%s\n' '!{ref_seq}'      > _ref.txt
    Rscript -e "MitoPilot::compute_blast_ref_alignment( \
        assembly_seq = paste(readLines('_assembly.txt'), collapse = ''), \
        ref_seq      = paste(readLines('_ref.txt'),      collapse = ''), \
        rotation     = !{rotation}, \
        output_file  = '!{outDir}/!{outFile}' \
    )"
    '''
}
