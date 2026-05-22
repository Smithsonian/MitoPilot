include {assemble} from './assemble.nf'

// run_blast comes from blast_opts via LEFT JOIN so a missing/NULL blast_opts
// still flows through (treated as run_blast=1 by the default-null coalesce
// in the multiMap below). This is read by ASSEMBLE so it can finalize
// state=2 directly for run_blast=0 samples, eliminating the race with
// BLAST_GENBANK's filtered-out write.
params.sqlRead =  'SELECT a.ID, a.assemble_opts, opts.cpus, opts.memory, ' +
                  'opts.seeds_db, opts.labels_db, opts.getOrganelle, opts.assembler, ' +
                  'opts.mitofinder_db, opts.mitofinder, s.genetic_code, ' +
                  'opts.max_paths, opts.max_scaffolds, opts.min_assembly_length, ' +
                  'b.run_blast ' +
                  'FROM assemble a ' +
                  'JOIN assemble_opts opts ' +
                  'ON a.assemble_opts = opts.assemble_opts ' +
                  'JOIN samples s ' +
                  'ON a.ID = s.ID ' +
                  'LEFT JOIN blast_opts b ' +
                  'ON a.blast_opts = b.blast_opts ' +
                  'WHERE a.assemble_switch IN (1, 4) AND a.assemble_lock = 0'

params.sqlDeleteAssemblies =  'DELETE FROM assemblies WHERE ID = ? AND time_stamp != ?'

params.sqlWriteAssemblies = 'INSERT OR REPLACE INTO assemblies ' +
                            '(ID, path, scaffold, length, length_raw, topology, time_stamp, sequence, ignore, edited) ' +
                            'VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, 0)'

params.sqlWriteAssemble =   'UPDATE assemble SET paths=?, scaffolds=?, length=?, topology=?, ' +
                            'assemble_switch=?, assemble_notes=?, time_stamp=?, poor_blast_ref=NULL WHERE ID=?'


workflow ASSEMBLE {
    take:
        input

    main:
        // Assembly Options Channel from DB (includes max_paths/max_scaffolds/min_assembly_length)
        channel.fromQuery(params.sqlRead, db: 'sqlite')
            .multiMap { it ->
                opts: tuple(
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
                min_len_scaffolds: tuple(it[0], it[13] == null ? 500 : (it[13] as Integer)) // ID, min_assembly_length (for per-scaffold ignore flag)
                min_len_summary:   tuple(it[0], it[13] == null ? 500 : (it[13] as Integer)) // ID, min_assembly_length (for per-sample all-short check)
                run_blast_lookup:  tuple(it[0], it[14] == null ? 1 : (it[14] as Integer))   // ID, run_blast (NULL → 1, i.e. BLAST by default)
            }
            .set { query_ch }

        query_ch.opts.set { assemble_opts }
        query_ch.min_len_scaffolds.set { min_len_lookup }
        query_ch.min_len_summary.set { min_len_summary }
        query_ch.run_blast_lookup.set { run_blast_lookup }

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
                def lengths_all = lengths.sort().reverse().join(';')
                def topo_str    = topologies.unique().sort().join(';')
                tuple(it[0], n_paths, n_scaffolds, length_str, topo_str, lengths_all, it, it[8], it[9])
            }
            .set { summarized }

        // Apply user-configured thresholds: split into pass / fail branches
        // Combine with min_len_summary so the all-short check can fire per-sample,
        // and with run_blast_lookup so ASSEMBLE can finalize state=2 directly for
        // run_blast=0 samples (single-writer fix for the BLAST_GENBANK race).
        summarized
            .combine(min_len_summary, by: 0)
            .combine(run_blast_lookup, by: 0)
            .branch { id, n_paths, n_scaffolds, length_str, topo_str, lengths_all, raw, max_paths, max_scaffolds, min_assembly_length, run_blast ->
                fail: (n_paths > max_paths) || (n_scaffolds > max_scaffolds)
                pass: true
            }
            .set { branched }

        // PASS: classify each sample's status/notes and emit per channel.
        // Only state=4 outcomes are propagated downstream; state=3 (terminal
        // failures) and state=2 (run_blast=0 success) do not consume
        // downstream BLAST/EFetch resources.
        branched.pass
            .map { id, n_paths, n_scaffolds, length_str, topo_str, lengths_all, raw, max_paths, max_scaffolds, min_assembly_length, run_blast ->
                def status = '4'
                def notes  = ''
                if (n_scaffolds > 1) {
                    def all_lengths_list = lengths_all ? lengths_all.split(';').collect { it as Integer } : []
                    def n_passing = all_lengths_list.count { it >= min_assembly_length }
                    notes = (n_passing == 1)
                        ? 'Output contains disconnected contigs'
                        : 'Output contains disconnected contigs (fragmented)'
                }
                if (n_paths > 1) {
                    notes = 'Unable to resolve single assembly from reads'
                }
                def max_len = length_str ? length_str.split(';').collect { it as Integer }.max() : 0
                if (max_len < min_assembly_length) {
                    status = '3'
                    notes  = "All scaffolds below min assembly length (${min_assembly_length} bp)"
                }
                // Single-writer finalization for no_blast samples: if assembly
                // succeeded (status still '4') AND BLAST is not requested,
                // write state=2 directly so BLAST_GENBANK never has to race
                // with ASSEMBLE's UPDATE on this row.
                if (status == '4' && (run_blast as Integer) == 0) {
                    status = '2'
                }
                tuple(id, n_paths, n_scaffolds, length_str, topo_str, raw, status, notes)
            }
            .multiMap { id, n_paths, n_scaffolds, length_str, topo_str, raw, status, notes ->
                db_write:   tuple(n_paths, n_scaffolds, length_str, topo_str, status, notes, params.ts, id)
                fasta:      tuple(id, raw[1])
                downstream: tuple(raw, status)
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
                    record.seqString.length(),          // length_raw (preserved; curate updates length only)
                    record.desc,                        // topology
                    params.ts,                          // time stamp
                    record.seqString                    // sequence
                ).flatten()
            }
            .combine(min_len_lookup, by: 0)             // append per-sample min_assembly_length
            .map { it ->                                // mark short assemblies
                def min_len = it[8] as Integer
                it[8] = (it[3] < min_len) ? 1 : 0     // replace min_len slot with ignore flag
                return it
            }
            .sqlInsert(statement: params.sqlWriteAssemblies, db: 'sqlite')

        // FAIL (too many paths/scaffolds): write status=3 with reason, drop from downstream
        branched.fail
            .map { id, n_paths, n_scaffolds, length_str, topo_str, lengths_all, raw, max_paths, max_scaffolds, min_assembly_length, run_blast ->
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
        // Only state=4 samples (PASS branch with no terminal failure) proceed
        // to COVERAGE / BLAST_GENBANK. State=3 outcomes (e.g. all scaffolds
        // below min_assembly_length) are terminal here and intentionally do
        // NOT consume downstream BLAST/EFetch resources. This is also what
        // guarantees the unguarded BLAST_REF_FETCH UPDATEs can never overwrite
        // a terminal state=3 row written by ASSEMBLE.
        ch = pass_ch.downstream
                .filter { raw, status -> status == '4' }
                .map    { raw, status -> raw }

}
