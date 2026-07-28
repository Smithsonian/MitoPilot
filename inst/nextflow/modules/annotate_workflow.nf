include {annotate} from './annotate.nf'
include {prepare_ref_db} from './prepare_ref_db.nf'

// ANNOTATE runs per (ID, path, scaffold): each non-ignored scaffold is its own
// annotation unit, annotated independently through the original MitoPilot flow
// (its own MITOS/tRNAscan run + its own output files ID_annotations_<path>_<scaffold>.csv).
// annotate() processes only this unit's scaffold by ignoring every OTHER scaffold
// of the path (ignore_scaffolds subquery). annotate_opts is the unit's own set;
// the unit is gated on annotate_switch=1 AND annotate_lock=0.
// userAsmb projects are the exception: a user-supplied assembly is ONE genome, so
// all of its contigs are annotated together as the single seeded unit (scaffold
// literal 1), ignoring only the scaffolds actually flagged ignore = 1. Mirrors
// VALIDATE's userAsmb branch, which already documents the same contract. Without
// it the per-scaffold ignore_scaffolds drops every contig but the target, and the
// seeded (ID, 1, 1) row does not even join when the FASTA's first passing contig
// is not scaffold 1 (contigs are numbered by FASTA order).
def scafSel   = params.userAsmb ? '1 AS scaffold' : 'a.scaffold'
def scafJoin  = params.userAsmb ? 'an.scaffold = 1' : 'an.scaffold = a.scaffold'
def ignoreSub = params.userAsmb ?
                  "(SELECT GROUP_CONCAT(a2.scaffold, ',') FROM assemblies a2 WHERE a2.ID = a.ID AND a2.path = a.path AND a2.ignore = 1)" :
                  "(SELECT GROUP_CONCAT(a2.scaffold, ',') FROM assemblies a2 WHERE a2.ID = a.ID AND a2.path = a.path AND a2.scaffold != a.scaffold)"

params.sqlRead =    'SELECT DISTINCT a.ID, a.path, ' + scafSel + ', b.assemble_opts, ' +
                        'd.cpus, d.memory, d.ref_db, d.ref_dir, d.mitos_opts, d.use_mitos_best, d.trnaScan_opts, d.start_gene, d.arwen_opts, d.use_arwen, d.aragorn_opts, d.use_aragorn, ' +
                        'd.use_mitofinder, d.mitofinder_db, d.mitofinder_new_genes, d.mitofinder_allow_introns, d.mitofinder_opts, ' +
                        ignoreSub + ' AS ignore_scaffolds, ' +
                        'd.coverage_trim, d.retain_low_conf_trna, d.use_mitos, d.use_trnaScan, f.genetic_code, d.rescue_no_trna ' +
                    'FROM assemblies a ' +
                    'JOIN assemble b ON a.ID = b.ID ' +
                    'JOIN annotate an ON an.ID = a.ID AND an.path = a.path AND ' + scafJoin + ' ' +
                    'JOIN annotate_opts d ON d.annotate_opts = an.annotate_opts ' +
                    'JOIN samples f ON a.ID = f.ID ' +
                    'WHERE b.assemble_lock = 1 ' +
                    'AND a.ignore = 0 ' +
                    'AND an.annotate_switch = 1 AND an.annotate_lock = 0'

workflow ANNOTATE {

    channel.fromQuery(params.sqlRead, db: 'sqlite')
        .map{ it ->

            // Check if refDir is a GitHub link
            if (it[7]?.contains('githubusercontent')) {
                if (!it[6].endsWith('.tar.gz')) {
                    it[6] = it[6] + '.tar.gz'
                }
            }

            // params.publishDir is relative, so resolve against launchDir (as CURATE does).
            def assembleRel = params.publishDir + '/' + it[0] + '/assemble/' + it[3]
            def assemblyRel = assembleRel + '/' + it[0] + '_assembly_' + it[1] + '.fasta'
            // Only judged when the project output tree is local; the unit is still
            // emitted either way so annotate() fails as loudly as it always has.
            def outRoot = new File(launchDir.toString(), params.publishDir)
            if (outRoot.isDirectory() && !new File(launchDir.toString(), assemblyRel).exists()) {
                def assembleBase = new File(outRoot, it[0] + '/assemble')
                def onDisk = assembleBase.isDirectory() ?
                    assembleBase.listFiles()?.findAll { d -> d.isDirectory() && !d.name.startsWith('.') }?.collect { d -> d.name }?.sort() : null
                log.warn "ANNOTATE: ${it[0]} (path ${it[1]}, scaffold ${it[2]}): expected assembly ${assemblyRel} not found. " +
                    (onDisk ?
                        "Assembly parameter set directories present for this sample: ${onDisk.join(', ')}. " +
                        "assemble_opts may have been changed after this sample was assembled, or the published output may have been moved or deleted." :
                        "No assembly output is present for this sample at all; it may have been moved or deleted.")
            }

            tuple(
                it[0],                                          // ID
                it[1],                                          // path
                it[2],                                          // scaffold
                file(assemblyRel),                              // Assembly (per-path; annotate() isolates this scaffold)
                file(                                           // Coverage (per-path; subset to this scaffold)
                    assembleRel + '/' +
                    it[0] + '_assembly_' + it[1] + '_coverageStats.csv'
                ),
                [
                    cpus:  it[4],                                      // cpus
                    memory: it[5],                                     // memory
                    ref_db: it[6],                                     // mitos_ref_db
                    ref_dir: it[7],                                    // mitos_ref_dir
                    mitos: it[8],                                      // mitos_opts
                    use_mitos_best: it[9],                             // use_mitos_best toggle
                    trnaScan: it[10],                                  // trnaScan_opts
                    start_gene: it[11],                                // starting gene for rotation
                    arwen: it[12],                                     // arwen_opts
                    use_arwen: it[13],                                 // use_arwen toggle
                    aragorn: it[14],                                   // aragorn_opts
                    use_aragorn: it[15],                               // use_aragorn toggle
                    use_mitofinder: it[16] != null ? it[16] as Integer : 0,  // use_mitofinder toggle (default off)
                    mitofinder_new_genes: it[18] != null ? it[18] as Integer : 0,   // --new-genes toggle
                    mitofinder_allow_introns: it[19] != null ? it[19] as Integer : 0, // --allow-intron toggle
                    mitofinder_opts: it[20] ?: '',                     // free-form MitoFinder options
                    ignore_scaffolds: it[21] ?: '',                    // every OTHER scaffold of the path (isolate this unit)
                    coverage_trim: it[22] != null ? it[22] as Integer : 1,  // coverage trimming toggle (default on)
                    retain_low_conf_trna: it[23] != null ? it[23] as Integer : 0,  // retain low-conf (NNN) tRNAs (default off)
                    use_mitos: it[24] != null ? it[24] as Integer : 1,      // use_mitos toggle (default on)
                    use_trnaScan: it[25] != null ? it[25] as Integer : 1,   // use_trnaScan toggle (default on)
                    genetic_code: it[26],                                   // per-sample genetic code (from samples table)
                    rescue_no_trna: it[27] != null ? it[27] as Integer : 1  // second MITOS2 pass without tRNA prediction (default on)
                ],
                file(it[7] + "/" + it[6]),                              // MITOS ref dir + clade (tarball)
                it[6].replaceFirst(/\.tar\.gz$/, ''),               // ref_db without ".tar.gz"
                file((it[17] != null && it[17].toString().trim()) ? it[17] : "${projectDir}/assets/NO_FILE")  // MitoFinder reference .gb (or placeholder)
            )
        }
        .set { annotate_pre }

    // Extract each unique MITOS ref DB tarball once (shared, read-only), then hand
    // every unit the extracted directory in place of the tarball.
    prepare_ref_db(
        annotate_pre.map { t -> tuple(t[7], t[6]) }.unique { it[0] }
    ).set { annotate_refdirs }

    annotate_pre
        .map { t -> tuple(t[7], t) }
        .combine(annotate_refdirs, by: 0)
        .map { clade, t, refdir ->
            tuple(t[0], t[1], t[2], t[3], t[4], t[5], refdir, t[7], t[8])
        }
        .set { annotate_in }

    annotate(annotate_in).set { annotate_out }

    emit:
           ch = annotate_out[0]

}
