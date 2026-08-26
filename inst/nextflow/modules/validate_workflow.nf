import java.util.Base64
include {validate} from './validate.nf'
include {write_curated_result} from './validate.nf'

// VALIDATE runs per (ID, path, scaffold): each unit is validated, warned, and
// summarized on its own. CURATE now emits per (ID, path, scaffold) too, so its
// files join the validate query directly by (id, path, scaffold).
// User-supplied assemblies follow the same contract: each contig is its own unit.
params.sqlRead = 'SELECT DISTINCT a.ID, a.path, a.scaffold, c.curate_opts, ' +
                     'd.cpus, d.memory, d.target, d.params ' +
                 'FROM assemblies a ' +
                 'JOIN assemble b ON a.ID = b.ID ' +
                 'JOIN annotate c ON a.ID = c.ID AND a.path = c.path AND a.scaffold = c.scaffold ' +
                 'JOIN curate_opts d ON c.curate_opts = d.curate_opts ' +
                 'WHERE c.annotate_switch = 1 AND c.annotate_lock = 0 AND b.assemble_lock = 1 AND a.ignore = 0'

workflow VALIDATE {
    take:
        input

    main:

        channel.fromQuery(params.sqlRead, db: 'sqlite')
            .join(input, by: [0, 1, 2])
            .map { it ->
                def jsonParams = it[7].toString()
                def encodedParams = Base64.encoder.encodeToString(jsonParams.bytes)

                tuple(
                    it[0],                                          // ID
                    it[1],                                          // path
                    it[2],                                          // scaffold
                    it[8],                                          // Annotations (curate)
                    it[10],                                         // Coverage    (curate)
                    [
                        cpus:  it[4],                                      // cpus
                        memory: it[5],                                     // memory
                        target: it[6],                                     // target
                        params: encodedParams                              // params
                    ]
                )
            }
            .set { validate_in }

        validate(validate_in).set { validate_out }

        // Single atomic commit of assemblies + annotations + annotate per unit.
        // Fan the per-unit validate output (annotations TSV + summary CSV) back
        // across the per-path curate output (`input`: coverageStats CSV +
        // assembly FASTA) by (id, path), then write in one driver-side JDBC
        // transaction. File paths are passed as values (read directly in the
        // driver) to avoid native-exec staging.
        validate_out
            .join(input, by: [0, 1, 2])
            .map { it ->
                tuple(
                    it[0],                 // ID
                    it[1],                 // path
                    it[2],                 // scaffold
                    it[8].toString(),      // coverageStats CSV   (curate coverage)
                    it[3].toString(),      // validated annotations TSV (validate[3])
                    it[4].toString(),      // summary CSV               (validate[4])
                    it[7].toString()       // assembly FASTA      (curate assembly)
                )
            }
            .set { writer_in }

        write_curated_result(writer_in).set { write_done }

    emit:
        // Only units whose atomic commit succeeded flow downstream.
        // (id, path, scaffold) ordering trigger for BLAST_REF_ALIGN, etc.
        validated = write_done
        // Final validated annotations TSV (id, path, scaffold, tsv) for the
        // optional ORF step, gated on a successful commit.
        annotations = validate_out.join(write_done, by: [0, 1, 2]).map { it -> tuple(it[0], it[1], it[2], it[3]) }

}
