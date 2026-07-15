include {orf} from './orf.nf'

// max_blast_hits, ref_dir and ref_db are shared with curation (sourced from
// curate_opts via annotate.curate_opts), not stored in orf_opts.
// assemble_opts + blast_accession locate the staged remote BLAST reference so
// ORFs can also BLAST against the closest-relative reference genes.
// ORF runs per (ID, path, scaffold): each unit's ORF search sees only its own
// validated annotations and its own assembly. Option sets come from the unit's
// annotate row; the unit is gated on annotate_switch=1 AND annotate_lock=0.
params.sqlRead =    'SELECT DISTINCT a.ID, a.path, a.scaffold, ' +
                    'd.use_orffinder, ' +
                    'd.cpus, d.memory, d.orffinder_opts, d.orf_min_len, d.orf_max_overlap, d.orf_nested, e.max_blast_hits, ' +
                    'e.ref_dir, e.ref_db, b.assemble_opts, a.blast_accession, f.genetic_code ' +
                    'FROM assemblies a ' +
                    'JOIN assemble b ON a.ID = b.ID ' +
                    'JOIN annotate an ON an.ID = a.ID AND an.path = a.path AND an.scaffold = a.scaffold ' +
                    'JOIN orf_opts d ON d.orf_opts = an.orf_opts ' +
                    'JOIN curate_opts e ON e.curate_opts = an.curate_opts ' +
                    'JOIN samples f ON a.ID = f.ID ' +
                    'WHERE b.assemble_lock = 1 AND a.ignore = 0 ' +
                    'AND an.annotate_switch = 1 AND an.annotate_lock = 0'

// Append ORFs to the annotations table (rows produced by the orf process). Uses
// the same INSERT OR REPLACE statement and run timestamp as VALIDATE so ORFs are
// added to the validated set rather than removed by VALIDATE's stale-row delete.
params.sqlWriteAnnotations =    'INSERT OR REPLACE INTO annotations ' +
                                '(ID, path, scaffold, type, gene, product, pos1, pos2, length, direction, start_codon, ' +
                                    'stop_codon, anticodon, tool, notes, warnings, translation, refHits, time_stamp, edited) ' +
                                'VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, 0)'

workflow ORF {
    take:
        validatedAnnotations   // (id, path, scaffold, validated annotations tsv) [per unit]
        curateOut              // curate output tuple (id, path, scaffold, annotations, assembly, coverage, workdir)

    main:

        // post-curate assembly per (id, path, scaffold)
        curateOut
            .map { it -> tuple(it[0], it[1], it[2], it[4]) }
            .set { curate_assembly }

        // ORF runs per unit; each unit's own validated tsv (no cross-scaffold grouping).
        validatedAnnotations
            .map { it -> tuple(it[0], it[1], it[2], it[3]) }
            .set { validated_by_unit }

        channel.fromQuery(params.sqlRead, db: 'sqlite')
            .join(validated_by_unit, by: [0, 1, 2])    // append validated tsv (index 16)
            .join(curate_assembly, by: [0, 1, 2])      // append assembly (index 17)
            .branch { it ->
                on:  (it[3] as Integer) == 1
                off: true
            }
            .set { routed }

        routed.on
            .map { it ->

                // Check if refDir is a GitHub link
                if (it[11]?.contains('githubusercontent')) {
                    if (!it[12].endsWith('.tar.gz')) {
                        it[12] = it[12] + '.tar.gz'
                    }
                }

                // Locate the staged remote BLAST reference for this unit (same
                // layout CURATE uses); fall back to an empty stub when absent.
                def blastAccession = it[14] ?: ""
                def blastRefRel = blastAccession
                    ? "${params.publishDir}/${it[0]}/assemble/${it[13]}/blast_ref_${blastAccession}/remote_blast_ref.json"
                    : "${params.publishDir}/${it[0]}/assemble/${it[13]}/remote_blast_ref.json"
                def blastRefAbs = new File(launchDir.toString(), blastRefRel)
                def blastRefFile = blastRefAbs.exists()
                    ? file(blastRefRel)
                    : file("${baseDir}/modules/empty_remote_blast_ref.json")

                tuple(
                    it[0],                                          // ID
                    it[1],                                          // path
                    it[2],                                          // scaffold
                    it[16],                                         // validated annotations tsv
                    it[17],                                         // post-curate assembly
                    [
                        cpus:  it[4],                                      // cpus
                        memory: it[5],                                     // memory
                        orffinder_opts: it[6],                             // ORFfinder options
                        orf_min_len: it[7],                                // min ORF length (nt)
                        orf_max_overlap: it[8],                            // max overlap fraction
                        orf_nested: it[9],                                 // include nested ORFs
                        max_blast_hits: it[10],                            // max BLAST hits per ORF
                        genetic_code: it[15]                               // per-sample genetic code (from samples table)
                    ],
                    file(it[11] + "/" + it[12]),                           // curation ref dir + clade
                    it[12],                                                // ref clade
                    it[12].replaceFirst(/\.tar\.gz$/, ''),             // ref_db without ".tar.gz"
                    blastRefFile                                           // staged remote BLAST ref json
                )
            }
            .set { orf_in }

        orf(orf_in).set { orf_out }

        // Append new ORF annotations to the db
        orf_out
            .map { it -> it[3] }
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
