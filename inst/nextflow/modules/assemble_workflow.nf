include {assemble} from './assemble.nf'

params.sqlRead =  'SELECT a.ID, a.assemble_opts, opts.cpus, opts.memory, ' +
                  'opts.seeds_db, opts.labels_db, opts.getOrganelle, opts.assembler, ' +
                  'opts.mitofinder_db, opts.mitofinder, s.genetic_code, ' +
                  'opts.max_paths, opts.max_scaffolds ' +
                  'FROM assemble a ' +
                  'JOIN assemble_opts opts ' +
                  'ON a.assemble_opts = opts.assemble_opts ' +
                  'JOIN samples s ' +
                  'ON a.ID = s.ID ' +
                  'WHERE a.assemble_switch IN (1, 4) AND a.assemble_lock = 0'

params.sqlDeleteAssemblies =  'DELETE FROM assemblies WHERE ID = ? AND time_stamp != ?'

params.sqlWriteAssemblies = 'INSERT OR REPLACE INTO assemblies ' +
                            '(ID, path, scaffold, length, topology, time_stamp, sequence, ignore, edited) ' +
                            'VALUES (?, ?, ?, ?, ?, ?, ?, ?, 0)'

params.sqlWriteAssemble =   'UPDATE assemble SET paths=?, scaffolds=?, length=?, topology=?, ' +
                            'assemble_switch=?, assemble_notes=?, time_stamp=? WHERE ID=?'


workflow ASSEMBLE {
    take:
        input

    main:
        // Assembly Options Channel from DB (includes max_paths/max_scaffolds thresholds)
        channel.fromQuery(params.sqlRead, db: 'sqlite')
            .map{ it ->
                tuple(
                    it[0],                                                      // ID
                    it[1],                                                      // options id
                    [                                                           //## assembly options ##//
                        cpus: it[2],                                            // cpus
                        memory: it[3],                                          // memory
                        getOrganelle: it[6],                                    // getOrganelle options
                        mitofinder: it[9],                                      // mitofinder options
                        assembler: it[7]                                        // assembler
                    ],
                    [
                        it[4],                                                  // getOrganelle seeds_db
                        it[5]                                                   // getOrganelle labels_db
                    ],
                    it[8],                                                      // mitofinder .gb reference database
                    it[10],                                                     // genetic code
                    (it[11] == null ? Integer.MAX_VALUE : (it[11] as Integer)), // max_paths
                    (it[12] == null ? Integer.MAX_VALUE : (it[12] as Integer))  // max_scaffolds
                )
            }
            .set { assemble_opts }

        // Assemble Input Channel
        input
            // filter on min seq depth
            .filter{
                try {
                    it[2].toInteger() >= params.minDepth
                } catch (Exception e) {
                    return false
                }
            }
            // cross with assembly options
            .cross(assemble_opts)
            .map{ it ->
                tuple(
                    it[0][0],                                                   // ID
                    it[1][1],                                                   // assembly options id
                    it[0][1],                                                   // trimmed reads in
                    it[1][2],                                                   // assembly options
                    it[1][3],                                                   // getOrganelle databases
                    it[1][4],                                                   // mitofinder .gb reference db
                    it[1][5],                                                   // genetic code
                    it[1][6],                                                   // max_paths
                    it[1][7]                                                    // max_scaffolds
                )
            }
            .set { assemble_in_full }

        assemble_in_full.set { assemble_in }

        // Assemble
        assemble(assemble_in).set { assemble_out }

        // Clear old assemblies from db (per-sample, fires as each completes)
        assemble_out[0]
          .map { it ->
            tuple(
              it[0],
              params.ts
            )
          }
          .sqlInsert( statement: params.sqlDeleteAssemblies, db: 'sqlite')

        // Per-sample summary: count paths, max scaffolds-per-path, lengths, topologies
        // (replaces the splitFasta + groupTuple aggregation that previously blocked the batch)
        assemble_out[0]
            .filter{ it[1] ==~ /^(?!.*assembly_0\.fasta$).*$/ }    // exclude empty assemblies
            .map { it ->
                def files = (it[1] instanceof List) ? it[1] : [it[1]]
                def n_paths = files.size()
                def scaffold_counts = []
                def lengths = []
                def topologies = []
                files.each { f ->
                    def n = 0
                    def currentSeq = new StringBuilder()
                    def currentTopo = null
                    def closeSeq = {
                        if (currentTopo != null) {
                            lengths << currentSeq.length()
                            topologies << currentTopo
                        }
                    }
                    f.eachLine { line ->
                        if (line.startsWith('>')) {
                            closeSeq()
                            n++
                            def parts = line.substring(1).split(/\s+/, 2)
                            currentTopo = parts.size() > 1 ? parts[1] : ''
                            currentSeq = new StringBuilder()
                        } else {
                            currentSeq.append(line.trim())
                        }
                    }
                    closeSeq()
                    scaffold_counts << n
                }
                def n_scaffolds = scaffold_counts ? scaffold_counts.max() : 0
                def length_str  = lengths.unique().sort().reverse().join(';')
                def topo_str    = topologies.unique().sort().join(';')
                tuple(it[0], n_paths, n_scaffolds, length_str, topo_str, it, it[8], it[9])
            }
            .set { summarized }

        // Apply user-configured thresholds: split into pass / fail branches
        summarized
            .branch { id, n_paths, n_scaffolds, length_str, topo_str, raw, max_paths, max_scaffolds ->
                fail: (n_paths > max_paths) || (n_scaffolds > max_scaffolds)
                pass: true
            }
            .set { branched }

        // PASS: write per-sample summary live, then propagate downstream
        branched.pass
            .multiMap { id, n_paths, n_scaffolds, length_str, topo_str, raw, max_paths, max_scaffolds ->
                def status = '4'
                def notes  = ''
                if (n_scaffolds > 1) {
                    notes  = 'Output contains disconnected contigs'
                    topo_str = 'fragmented'
                    status = '3'
                }
                if (n_paths > 1) {
                    notes  = 'Unable to resolve single assembly from reads'
                    status = '3'
                }
                db_write:   tuple(n_paths, n_scaffolds, length_str, topo_str, status, notes, params.ts, id)
                fasta:      tuple(id, raw[1])
                downstream: raw
            }
            .set { pass_ch }

        // Live per-sample UPDATE of the assemble table
        pass_ch.db_write
            .sqlInsert(statement: params.sqlWriteAssemble, db: 'sqlite')

        // Per-scaffold INSERT into assemblies table (still live; no groupTuple)
        pass_ch.fasta
            .map { id, files -> files }
            .flatten()
            .splitFasta(record: [id: true, desc: true, seqString: true])
            .map { record ->
                tuple(
                    record.id.split('\\.'),             // ID, path, scaffold
                    record.seqString.length(),          // length
                    record.desc,                        // topology
                    params.ts,                          // time stamp
                    record.seqString                    // sequence
                ).flatten()
            }
            .map { it ->                                            // mark short assemblies
                if(it[3] < params.minAssemblyLength){
                    it[7] = 1
                }else{
                    it[7] = 0
                }
                return it
            }
            .sqlInsert(statement: params.sqlWriteAssemblies, db: 'sqlite')

        // FAIL (too many paths/scaffolds): write status=3 with reason, drop from downstream
        branched.fail
            .map { id, n_paths, n_scaffolds, length_str, topo_str, raw, max_paths, max_scaffolds ->
                def msg
                if (n_paths > max_paths && n_scaffolds > max_scaffolds) {
                    msg = "${n_paths} assembly paths, exceeds limit (${max_paths}); ${n_scaffolds} scaffolds, exceeds limit (${max_scaffolds})"
                } else if (n_paths > max_paths) {
                    msg = "${n_paths} assembly paths, exceeds limit (${max_paths})"
                } else {
                    msg = "${n_scaffolds} scaffolds, exceeds limit (${max_scaffolds})"
                }
                tuple(n_paths, n_scaffolds, length_str, topo_str, '3', msg, params.ts, id)
            }
            .sqlInsert(statement: params.sqlWriteAssemble, db: 'sqlite')

        // Empty assemblies (assembly_0.fasta)
        assemble_out[0]
            .filter{ it[1] ==~ /(.*assembly_0\.fasta)$/ }
            .map { it ->
                tuple(
                    null, null, null, null,
                    '3',
                    'failed assembly',
                    params.ts,
                    it[0]
                )
            }
            .sqlInsert(statement: params.sqlWriteAssemble , db: 'sqlite')

        // Samples with too few reads
        input
            .filter{
                try {
                    it[2].toInteger() < params.minDepth
                } catch (Exception e) {
                    return false
                }
            }.cross(assemble_opts)
            .map { it ->
                tuple(
                    null, null, null, null,
                    '3',
                    'Insufficient sequencing depth',
                    params.ts,
                    it[0][0]
                )
            }
            .sqlInsert(statement: params.sqlWriteAssemble , db: 'sqlite')

        // Reset annotation data for updated assemblies (all that produced output)
        assemble_out[0]
          .map { it ->
            tuple(
              it[0]
            )
          }
          .set { update_ids }
        channel.fromQuery('SELECT ID, annotate_opts, curate_opts FROM annotate;', db: 'sqlite')
            .join(update_ids)
            .sqlInsert(statement: 'INSERT OR REPLACE INTO annotate (ID, annotate_opts, curate_opts, annotate_switch, annotate_lock, reviewed) VALUES (?, ?, ?, 1, 0, "no")', db: 'sqlite')

    emit:
        // Only samples passing thresholds proceed to COVERAGE / BLAST_GENBANK
        ch = pass_ch.downstream

}
