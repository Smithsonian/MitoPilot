import java.util.Base64
include {curate} from './curate.nf'

// CURATE runs per (ID, path, scaffold): each unit is curated independently using
// its own curate_opts/annotate_opts and its own per-scaffold BLAST reference.
params.sqlRead =    'SELECT DISTINCT a.ID, a.path, a.scaffold, b.assemble_opts, an.curate_opts, ' +
                    'd.cpus, d.memory, d.target, d.params, d.max_blast_hits, ' +
                    'd.ref_dir, d.ref_db, e.feature_trim, a.blast_accession, f.genetic_code, e.ref_based_rc ' +
                    'FROM assemblies a ' +
                    'JOIN assemble b ON a.ID = b.ID ' +
                    'JOIN annotate an ON an.ID = a.ID AND an.path = a.path AND an.scaffold = a.scaffold ' +
                    'JOIN curate_opts d ON d.curate_opts = an.curate_opts ' +
                    'JOIN annotate_opts e ON e.annotate_opts = an.annotate_opts ' +
                    'JOIN samples f ON a.ID = f.ID ' +
                    'WHERE b.assemble_lock = 1 AND a.ignore = 0 ' +
                    'AND an.annotate_switch = 1 AND an.annotate_lock = 0'

workflow CURATE {
    take:
        input

    main:

        channel.fromQuery(params.sqlRead, db: 'sqlite')
            .join(input, by: [0, 1, 2])
            .map { it ->

                // Check if refDir is a GitHub link
                if (it[10]?.contains('githubusercontent')) {
                    if (!it[11].endsWith('.tar.gz')) {
                        it[11] = it[11] + '.tar.gz'
                    }
                }

                def jsonParams = it[8].toString()
                def encodedParams = Base64.encoder.encodeToString(jsonParams.bytes)
                // Gather every fetched candidate-reference JSON for this sample
                // (blast_ref_<accession>/remote_blast_ref.json published by
                // blast_ref_stamp). All are injected into the curation BLAST DB so
                // the extra retained BLAST hits enrich curation. Falls back to the
                // legacy single-hit path, then an empty stub.
                def assembleRel = "${params.publishDir}/${it[0]}/assemble/${it[3]}"
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
                    it[2],                                          // scaffold
                    it[16],                                          // Annotations
                    it[17],                                          // Assembly
                    it[18],                                          // Coverage
                    [
                        cpus:  it[5],                                      // cpus
                        memory: it[6],                                     // memory
                        target: it[7],                                     // target
                        params: encodedParams,                              // params
                        max_blast_hits: it[9],                             // maximum retained blast hits
                        genetic_code: it[14],                              // per-sample genetic code (from samples table)
                        feature_trim: it[12] != null ? it[12] as Integer : 1,  // trim un-annotated ends (default on)
                        ref_based_rc: it[15] != null ? it[15] as Integer : 0,   // reference-based RC (default off)
                        blast_accession: it[13] ?: ''                           // this scaffold's BLAST hit (orientation ref)
                    ],
                    file(it[10] + "/" + it[11]),                              // curation ref dir + clade
                    it[11],                                                   // ref clade
                    it[11].replaceFirst(/\.tar\.gz$/, ''),                // ref_db without ".tar.gz"
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
