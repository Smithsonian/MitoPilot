import java.util.Base64
include {validate} from './validate.nf'
include {write_curated_result} from './validate.nf'

params.sqlRead =    'SELECT DISTINCT a.ID, a.path, c.curate_opts, ' +
                        'd.cpus, d.memory, d.target, d.params ' +
                    'FROM assemblies a ' +
                    'JOIN assemble b ON a.ID = b.ID ' +
                    'JOIN annotate c ON a.ID = c.ID ' +
                    'JOIN curate_opts d ON c.curate_opts = d.curate_opts ' +
                    'WHERE c.annotate_switch = 1 AND c.annotate_lock = 0 AND b.assemble_lock = 1 AND a.ignore = 0'

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

        // Single atomic commit of assemblies + annotations + annotate. Join the
        // validate output (annotations TSV + summary CSV) with the curate output
        // (`input`: coverageStats CSV + assembly FASTA) by (id, path), then write
        // everything in one driver-side JDBC transaction. File paths are passed
        // as values (read directly in the driver) to avoid native-exec staging.
        validate_out
            .join(input, by: [0, 1])
            .map { it ->
                tuple(
                    it[0],                 // ID
                    it[1],                 // path
                    it[7].toString(),      // coverageStats CSV   (input[4])
                    it[2].toString(),      // validated annotations TSV (validate[2])
                    it[3].toString(),      // summary CSV               (validate[3])
                    it[6].toString()       // assembly FASTA      (input[3])
                )
            }
            .set { writer_in }

        write_curated_result(writer_in).set { write_done }

    emit:
        // Only samples whose atomic commit succeeded flow downstream.
        // (id, path) ordering trigger for BLAST_REF_ALIGN, etc.
        validated = write_done
        // Final validated annotations TSV (id, path, tsv) for the optional ORF
        // step, gated on a successful commit.
        annotations = validate_out.join(write_done, by: [0, 1]).map { it -> tuple(it[0], it[1], it[2]) }

}
