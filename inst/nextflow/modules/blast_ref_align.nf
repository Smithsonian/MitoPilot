include { clusterOpts } from './cluster_opts.nf'

process blast_ref_align {

    executor params.blast_ref_align.executor
    container params.blast_ref_align.container

    cpus { (params.blast_ref_align.cpus instanceof Integer) ? params.blast_ref_align.cpus : 1 }
    memory = (params.blast_ref_align.memory instanceof Number) ? "${params.blast_ref_align.memory}.GB" : null
    clusterOptions { clusterOpts(params.blast_ref_align) }

    errorStrategy 'ignore'

    tag "${id}.${path}.${scaffold}.${accession}"

    input:
        // assembly_seq and ref_seq are plain nucleotide strings (no headers)
        // rotation is the 0-based offset into the reference for the anchor gene
        tuple val(id), val(path), val(scaffold), val(accession), val(assembly_seq), val(ref_seq), val(rotation)

    output:
        tuple val(id), val(path), val(scaffold), val(accession), path("${id}/annotate/blast_ref_alignment_${path}_${scaffold}_${accession}.csv")

    shell:
    outDir = "${id}/annotate"
    outFile = "blast_ref_alignment_${path}_${scaffold}_${accession}.csv"
    '''
    mkdir -p !{outDir}
    # blast_ref_align output contract v2: CSV has 6 columns
    # (aligned_sample, aligned_ref, rotation, ref_length, ref_start, strand).
    # This version string is part of the task script, so bumping it invalidates
    # the Nextflow -resume cache: a changed aligner output format cannot be
    # silently reused across package upgrades (downstream parser needs 6 cols).
    # Write sequences to files - avoids Rscript -e expression length limits
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
