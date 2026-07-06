include {orf} from './orf.nf'

// max_blast_hits, ref_dir and ref_db are shared with curation (sourced from
// curate_opts via annotate.curate_opts), not stored in orf_opts.
// assemble_opts + blast_accession (cols 12, 13) locate the staged remote BLAST
// reference so ORFs can also BLAST against the closest-relative reference genes.
params.sqlRead =    'SELECT DISTINCT a.ID, a.path, ' +
                    'd.use_orffinder, ' +
                    'd.cpus, d.memory, d.orffinder_opts, d.orf_min_len, d.orf_max_overlap, d.orf_nested, e.max_blast_hits, ' +
                    'e.ref_dir, e.ref_db, b.assemble_opts, b.blast_accession, f.genetic_code ' +
                    'FROM assemblies a ' +
                    'JOIN assemble b ON a.ID = b.ID ' +
                    'JOIN annotate c ON a.ID = c.ID ' +
                    'JOIN orf_opts d ON c.orf_opts = d.orf_opts ' +
                    'JOIN curate_opts e ON c.curate_opts = e.curate_opts ' +
                    'JOIN samples f ON a.ID = f.ID ' +
                    'WHERE c.annotate_switch = 1 AND c.annotate_lock = 0 AND b.assemble_lock = 1 AND a.ignore = 0'

// Append ORFs to the annotations table (rows produced by the orf process). Uses
// the same INSERT OR REPLACE statement and run timestamp as VALIDATE so ORFs are
// added to the validated set rather than removed by VALIDATE's stale-row delete.
params.sqlWriteAnnotations =    'INSERT OR REPLACE INTO annotations ' +
                                '(ID, path, scaffold, type, gene, product, pos1, pos2, length, direction, start_codon, ' +
                                    'stop_codon, anticodon, tool, notes, warnings, translation, refHits, time_stamp, edited) ' +
                                'VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, 0)'

workflow ORF {
    take:
        validatedAnnotations   // (id, path, validated annotations tsv)
        curateOut              // curate output tuple (id, path, annotations, assembly, coverage, workdir)

    main:

        // post-curate assembly per sample
        curateOut
            .map { it -> tuple(it[0], it[1], it[3]) }
            .set { curate_assembly }

        channel.fromQuery(params.sqlRead, db: 'sqlite')
            .join(validatedAnnotations, by: [0, 1])   // append validated annotations tsv (index 15)
            .join(curate_assembly, by: [0, 1])         // append assembly (index 16)
            .branch { it ->
                on:  (it[2] as Integer) == 1
                off: true
            }
            .set { routed }

        routed.on
            .map { it ->

                // Check if refDir is a GitHub link
                if (it[10]?.contains('githubusercontent')) {
                    if (!it[11].endsWith('.tar.gz')) {
                        it[11] = it[11] + '.tar.gz'
                    }
                }

                // Locate the staged remote BLAST reference for this sample (same
                // layout CURATE uses); fall back to an empty stub when absent.
                def blastAccession = it[13] ?: ""
                def blastRefRel = blastAccession
                    ? "${params.publishDir}/${it[0]}/assemble/${it[12]}/blast_ref_${blastAccession}/remote_blast_ref.json"
                    : "${params.publishDir}/${it[0]}/assemble/${it[12]}/remote_blast_ref.json"
                def blastRefAbs = new File(launchDir.toString(), blastRefRel)
                def blastRefFile = blastRefAbs.exists()
                    ? file(blastRefRel)
                    : file("${baseDir}/modules/empty_remote_blast_ref.json")

                tuple(
                    it[0],                                          // ID
                    it[1],                                          // path
                    it[15],                                         // validated annotations tsv
                    it[16],                                         // post-curate assembly
                    [
                        cpus:  it[3],                                      // cpus
                        memory: it[4],                                     // memory
                        orffinder_opts: it[5],                             // ORFfinder options
                        orf_min_len: it[6],                                // min ORF length (nt)
                        orf_max_overlap: it[7],                            // max overlap fraction
                        orf_nested: it[8],                                 // include nested ORFs
                        max_blast_hits: it[9],                             // max BLAST hits per ORF
                        genetic_code: it[14]                               // per-sample genetic code (from samples table)
                    ],
                    file(it[10] + "/" + it[11]),                           // curation ref dir + clade
                    it[11],                                                // ref clade
                    it[11].replaceFirst(/\.tar\.gz$/, ''),             // ref_db without ".tar.gz"
                    blastRefFile                                           // staged remote BLAST ref json
                )
            }
            .set { orf_in }

        orf(orf_in).set { orf_out }

        // Append new ORF annotations to the db
        orf_out
            .map { it -> it[2] }
            .flatten()
            .splitCsv(header: true, sep: '\t')
            .map { record ->
                tuple(
                    record.contig.split('\\.'),  // ID, path, scaffold
                    record.type,
                    record.gene,
                    record.product,
                    record.pos1,
                    record.pos2,
                    record.length,
                    record.direction,
                    record.start_codon,
                    record.stop_codon,
                    record.anticodon,
                    record.tool,
                    record.notes,
                    record.warnings,
                    record.translation,
                    record.refHits,
                    params.ts
                ).flatten()
            }
            .sqlInsert(statement: params.sqlWriteAnnotations, db: 'sqlite')

}
