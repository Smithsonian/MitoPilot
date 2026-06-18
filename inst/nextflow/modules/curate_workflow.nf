import java.util.Base64
include {curate} from './curate.nf'
include {write_assembly} from './curate.nf'

params.sqlRead =    'SELECT DISTINCT a.ID, a.path, b.assemble_opts, c.curate_opts, ' +
                    'd.cpus, d.memory, d.target, d.params, d.max_blast_hits, ' +
                    'd.ref_dir, d.ref_db, e.feature_trim, b.blast_accession ' +
                    'FROM assemblies a ' +
                    'JOIN assemble b ON a.ID = b.ID ' +
                    'JOIN annotate c ON a.ID = c.ID ' +
                    'JOIN curate_opts d ON c.curate_opts = d.curate_opts ' +
                    'JOIN annotate_opts e ON c.annotate_opts = e.annotate_opts ' +
                    'WHERE c.annotate_switch = 1 AND c.annotate_lock = 0 AND b.assemble_lock = 1 AND a.ignore = 0'

params.sqlWriteAnnotate =   'UPDATE annotate SET path = ?, scaffolds = ?, topology = ?, length = ?, time_stamp = ? WHERE ID = ?'

workflow CURATE {
    take:
        input

    main:

        channel.fromQuery(params.sqlRead, db: 'sqlite')
            .join(input, by: [0, 1])
            .map { it ->

                // Check if refDir is a GitHub link
                if (it[9].contains('githubusercontent')) {
                    if (!it[10].endsWith('.tar.gz')) {
                        it[10] = it[10] + '.tar.gz'
                    }
                }

                def jsonParams = it[7].toString()
                def encodedParams = Base64.encoder.encodeToString(jsonParams.bytes)
                def blastAccession = it[12] ?: ""
                def blastRefRel = blastAccession
                    ? "${params.publishDir}/${it[0]}/assemble/${it[2]}/blast_ref_${blastAccession}/remote_blast_ref.json"
                    : "${params.publishDir}/${it[0]}/assemble/${it[2]}/remote_blast_ref.json"
                def blastRefAbs = new File(launchDir.toString(), blastRefRel)
                def blastRefFile = blastRefAbs.exists()
                    ? file(blastRefRel)
                    : file("${baseDir}/modules/empty_remote_blast_ref.json")

                tuple(
                    it[0],                                          // ID
                    it[1],                                          // path
                    it[13],                                          // Annotations
                    it[14],                                          // Assembly
                    it[15],                                          // Coverage
                    [
                        cpus:  it[4],                                      // cpus
                        memory: it[5],                                     // memory
                        target: it[6],                                     // target
                        params: encodedParams,                              // params
                        max_blast_hits: it[8],                             // maximum retained blast hits
                        feature_trim: it[11] != null ? it[11] as Integer : 1   // trim un-annotated ends (default on)
                    ],
                    file(it[9] + "/" + it[10]),                              // curation ref dir + clade
                    it[10],                                                   // ref clade
                    it[10].replaceFirst(/\.tar\.gz$/, ''),                // ref_db without ".tar.gz"
                    blastRefFile

                )
            }
            .set { curate_in }

        curate(curate_in).set { curate_out }

        // Verified, serialized write of the curated assembly sequence (id, path,
        // coverageStats). Only samples whose write commits flow downstream, so
        // VALIDATE never writes annotations against an uncommitted sequence.
        curate_out
            .map { tuple(it[0], it[1], it[4]) }
            .set { write_in }

        write_assembly(write_in).set { write_done }

        curate_out
            .join(write_done, by: [0, 1])
            .set { curate_gated }

        // Update annotate table (only for samples whose assembly write committed)
        curate_gated
            .map { it ->
                def ID = it[0]
                def path = it[1]
                def seqs = file(it[3]).splitFasta(record: [seqString: true, desc: true])
                def scaffolds = seqs.size()
                def length = seqs.collect { record -> record.seqString.size() }.sum()
                def topology = seqs.collect { record -> record.desc }.join(';')
                tuple(
                    path,
                    scaffolds,
                    topology,
                    length,
                    time_stamp = params.ts,
                    ID
                )
            }
            .sqlInsert(statement: params.sqlWriteAnnotate, db: 'sqlite')

    emit:
           ch = curate_gated

}
