import java.util.Base64
include {validate} from './validate.nf'

params.sqlRead =    'SELECT DISTINCT a.ID, a.path, c.curate_opts, ' +
                        'd.cpus, d.memory, d.target, d.params ' +
                    'FROM assemblies a ' +
                    'JOIN assemble b ON a.ID = b.ID ' +
                    'JOIN annotate c ON a.ID = c.ID ' +
                    'JOIN curate_opts d ON c.curate_opts = d.curate_opts ' +
                    'WHERE c.annotate_switch = 1 AND c.annotate_lock = 0 AND b.assemble_lock = 1 AND a.ignore = 0'

params.sqlWriteAnnotate =   'UPDATE annotate SET structure = ?, PCGCount = ?, tRNACount = ?, ' +
                                'rRNACount = ?, missing = ?, extra = ?, warnings = ?, annotate_switch = 2 ' +
                            'WHERE ID = ?'

params.sqlDeleteAnnotations =  'DELETE FROM annotations WHERE ID = ? AND time_stamp != ?'

params.sqlWriteAnnotations =    'INSERT OR REPLACE INTO annotations ' +
                                '(ID, path, scaffold, type, gene, product, pos1, pos2, length, direction, start_codon, ' +
                                    'stop_codon, anticodon, tool, notes, warnings, translation, refHits, time_stamp, edited) ' +
                                'VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, 0)'

workflow VALIDATE {
    take:
        input

    main:

        channel.fromQuery(params.sqlRead, db: 'sqlite')
            .join(input, by: [0, 1])
            .map { it ->
                def jsonParams = it[6].toString()
                def encodedParams = Base64.encoder.encodeToString(jsonParams.bytes)

                tuple(
                    it[0],                                          // ID
                    it[1],                                          // path
                    it[7],                                          // Annotations
                    it[9],                                          // Coverage
                    [
                        cpus:  it[3],                                      // cpus
                        memory: it[4],                                     // memory
                        target: it[5],                                     // target
                        params: encodedParams                              // params
                    ]
                )
            }
            .set { validate_in }

        validate(validate_in).set { validate_out }

    // Write summary to annotate table
    validate_out
        .map { it ->
            def ID = it[0]
            def csvData = file(it[3]).splitCsv(header: true)
            def row = csvData[0]
            tuple(
                row.structure,
                row.PCGCount,
                row.tRNACount,
                row.rRNACount,
                row.missing,
                row.extra,
                row.warnings,
                ID
            )
        }
        .sqlInsert(statement: params.sqlWriteAnnotate, db: 'sqlite')

    // Clear old annotations from db
    validate_out
        .map { it ->
            tuple(
                ID = it[0],
                time_stamp = params.ts
            )
        }
        .sqlInsert( statement: params.sqlDeleteAnnotations, db: 'sqlite')

    // Write new annotations
    validate_out
        .map { it -> it[2] }
        .flatten()
        .splitCsv(header: true, sep:'\t')
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
        .sqlInsert( statement: params.sqlWriteAnnotations, db: 'sqlite')

    emit:
        // Emit (id, path) for each validated sample so downstream workflows
        // (e.g. BLAST_REF_ALIGN) can use it as an ordering trigger
        validated = validate_out.map { it -> tuple(it[0], it[1]) }
        // Emit the final, validated annotations TSV (id, path, tsv) so the
        // optional ORF step can scan for ORFs in the finalized gene models
        annotations = validate_out.map { it -> tuple(it[0], it[1], it[2]) }

}
