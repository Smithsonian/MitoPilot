include {orf} from './orf.nf'

// max_blast_hits, ref_dir and ref_db are shared with curation (sourced from
// curate_opts via annotate.curate_opts), not stored in orf_opts.
params.sqlRead =    'SELECT DISTINCT a.ID, a.path, ' +
                    'd.use_orffinder, ' +
                    'd.cpus, d.memory, d.orffinder_opts, d.orf_min_len, d.orf_max_overlap, e.max_blast_hits, ' +
                    'e.ref_dir, e.ref_db ' +
                    'FROM assemblies a ' +
                    'JOIN assemble b ON a.ID = b.ID ' +
                    'JOIN annotate c ON a.ID = c.ID ' +
                    'JOIN orf_opts d ON c.orf_opts = d.orf_opts ' +
                    'JOIN curate_opts e ON c.curate_opts = e.curate_opts ' +
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
            .join(validatedAnnotations, by: [0, 1])   // append validated annotations tsv (index 11)
            .join(curate_assembly, by: [0, 1])         // append assembly (index 12)
            .branch { it ->
                on:  (it[2] as Integer) == 1
                off: true
            }
            .set { routed }

        routed.on
            .map { it ->

                // Check if refDir is a GitHub link
                if (it[9].contains('githubusercontent')) {
                    if (!it[10].endsWith('.tar.gz')) {
                        it[10] = it[10] + '.tar.gz'
                    }
                }

                tuple(
                    it[0],                                          // ID
                    it[1],                                          // path
                    it[11],                                         // validated annotations tsv
                    it[12],                                         // post-curate assembly
                    [
                        cpus:  it[3],                                      // cpus
                        memory: it[4],                                     // memory
                        orffinder_opts: it[5],                             // ORFfinder options
                        orf_min_len: it[6],                                // min ORF length (nt)
                        orf_max_overlap: it[7],                            // max overlap fraction
                        max_blast_hits: it[8]                              // max BLAST hits per ORF
                    ],
                    file(it[9] + "/" + it[10]),                            // curation ref dir + clade
                    it[10],                                                // ref clade
                    it[10].replaceFirst(/\.tar\.gz$/, '')              // ref_db without ".tar.gz"
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
