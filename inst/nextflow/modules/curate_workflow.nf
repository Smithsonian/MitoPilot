import java.util.Base64
include {curate} from './curate.nf'

params.sqlRead =    'SELECT DISTINCT a.ID, a.path, b.assemble_opts, c.curate_opts, ' +
                    'd.cpus, d.memory, d.target, d.params, d.max_blast_hits, ' +
                    'd.ref_dir, d.ref_db, e.feature_trim, b.blast_accession, f.genetic_code ' +
                    'FROM assemblies a ' +
                    'JOIN assemble b ON a.ID = b.ID ' +
                    'JOIN annotate c ON a.ID = c.ID ' +
                    'JOIN curate_opts d ON c.curate_opts = d.curate_opts ' +
                    'JOIN annotate_opts e ON c.annotate_opts = e.annotate_opts ' +
                    'JOIN samples f ON a.ID = f.ID ' +
                    'WHERE c.annotate_switch = 1 AND c.annotate_lock = 0 AND b.assemble_lock = 1 AND a.ignore = 0'

workflow CURATE {
    take:
        input

    main:

        channel.fromQuery(params.sqlRead, db: 'sqlite')
            .join(input, by: [0, 1])
            .map { it ->

                // Check if refDir is a GitHub link
                if (it[9]?.contains('githubusercontent')) {
                    if (!it[10].endsWith('.tar.gz')) {
                        it[10] = it[10] + '.tar.gz'
                    }
                }

                def jsonParams = it[7].toString()
                def encodedParams = Base64.encoder.encodeToString(jsonParams.bytes)
                // Gather every fetched candidate-reference JSON for this sample
                // (blast_ref_<accession>/remote_blast_ref.json published by
                // blast_ref_stamp). All are injected into the curation BLAST DB so
                // the extra retained BLAST hits enrich curation. Falls back to the
                // legacy single-hit path, then an empty stub.
                def assembleRel = "${params.publishDir}/${it[0]}/assemble/${it[2]}"
                def assembleAbs = new File(launchDir.toString(), assembleRel)
                def blastRefFiles = []
                if (assembleAbs.exists()) {
                    assembleAbs.listFiles()?.findAll {
                        d -> d.isDirectory() && d.name.startsWith('blast_ref_')
                    }?.sort { a, b -> a.name <=> b.name }?.each { d ->
                        def j = new File(d, 'remote_blast_ref.json')
                        if (j.exists()) blastRefFiles << file("${assembleRel}/${d.name}/remote_blast_ref.json")
                    }
                }
                if (blastRefFiles.isEmpty()) {
                    def legacy = new File(assembleAbs, 'remote_blast_ref.json')
                    if (legacy.exists()) blastRefFiles << file("${assembleRel}/remote_blast_ref.json")
                }
                if (blastRefFiles.isEmpty()) {
                    blastRefFiles << file("${baseDir}/modules/empty_remote_blast_ref.json")
                }

                tuple(
                    it[0],                                          // ID
                    it[1],                                          // path
                    it[14],                                          // Annotations
                    it[15],                                          // Assembly
                    it[16],                                          // Coverage
                    [
                        cpus:  it[4],                                      // cpus
                        memory: it[5],                                     // memory
                        target: it[6],                                     // target
                        params: encodedParams,                              // params
                        max_blast_hits: it[8],                             // maximum retained blast hits
                        genetic_code: it[13],                              // per-sample genetic code (from samples table)
                        feature_trim: it[11] != null ? it[11] as Integer : 1   // trim un-annotated ends (default on)
                    ],
                    file(it[9] + "/" + it[10]),                              // curation ref dir + clade
                    it[10],                                                   // ref clade
                    it[10].replaceFirst(/\.tar\.gz$/, ''),                // ref_db without ".tar.gz"
                    blastRefFiles                                            // candidate-reference JSONs

                )
            }
            .set { curate_in }

        curate(curate_in).set { curate_out }

        // No DB writes here: the curated assembly sequence and the annotation
        // coordinates are committed together (with the validate summary) in a
        // single atomic transaction at the end of VALIDATE
        // (write_curated_result). curate_out carries the files VALIDATE needs:
        // (id, path, annotations.csv, assembly.fasta, coverageStats.csv, workdir).

    emit:
           ch = curate_out

}
