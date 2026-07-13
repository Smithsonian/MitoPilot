import java.util.Base64
include {curate} from './curate.nf'

// CURATE runs per (ID, path) (hybrid model). annotate is keyed
// (ID, path, scaffold), so curate_opts/annotate_opts are sourced via a per-path
// subquery and the path is gated by EXISTS (any unit wants annotation).
params.sqlRead =    'SELECT DISTINCT a.ID, a.path, b.assemble_opts, d.curate_opts, ' +
                    'd.cpus, d.memory, d.target, d.params, d.max_blast_hits, ' +
                    'd.ref_dir, d.ref_db, e.feature_trim, b.blast_accession, f.genetic_code, e.ref_based_rc ' +
                    'FROM assemblies a ' +
                    'JOIN assemble b ON a.ID = b.ID ' +
                    'JOIN curate_opts d ON d.curate_opts = (SELECT an.curate_opts FROM annotate an WHERE an.ID = a.ID AND an.path = a.path ORDER BY an.scaffold LIMIT 1) ' +
                    'JOIN annotate_opts e ON e.annotate_opts = (SELECT an.annotate_opts FROM annotate an WHERE an.ID = a.ID AND an.path = a.path ORDER BY an.scaffold LIMIT 1) ' +
                    'JOIN samples f ON a.ID = f.ID ' +
                    'WHERE b.assemble_lock = 1 AND a.ignore = 0 ' +
                    'AND EXISTS (SELECT 1 FROM annotate an WHERE an.ID = a.ID AND an.path = a.path AND an.annotate_switch = 1 AND an.annotate_lock = 0)'

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
                    it[15],                                          // Annotations
                    it[16],                                          // Assembly
                    it[17],                                          // Coverage
                    [
                        cpus:  it[4],                                      // cpus
                        memory: it[5],                                     // memory
                        target: it[6],                                     // target
                        params: encodedParams,                              // params
                        max_blast_hits: it[8],                             // maximum retained blast hits
                        genetic_code: it[13],                              // per-sample genetic code (from samples table)
                        feature_trim: it[11] != null ? it[11] as Integer : 1,  // trim un-annotated ends (default on)
                        ref_based_rc: it[14] != null ? it[14] as Integer : 0,   // reference-based RC (default off)
                        blast_accession: it[12] ?: ''                           // top BLAST hit (orientation ref)
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
