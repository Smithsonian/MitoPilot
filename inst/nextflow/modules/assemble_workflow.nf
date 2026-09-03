include {assemble} from './assemble.nf'

// LEFT JOIN so a missing/NULL blast_opts still flows through (coalesced to
// run_blast=1 below). ASSEMBLE uses run_blast to finalize state=2 directly
// for run_blast=0 samples.
params.sqlRead =  'SELECT a.ID, a.assemble_opts, opts.cpus, opts.memory, ' +
                  'opts.seeds_db, opts.labels_db, opts.getOrganelle, opts.assembler, ' +
                  'opts.mitofinder_db, opts.mitofinder, s.genetic_code, ' +
                  'opts.max_paths, opts.max_scaffolds, opts.min_assembly_length, ' +
                  'b.run_blast, opts.join_scaffolds, ' +
                  'a.join_switch, a.assemble_switch, a.blast_accession, ' +
                  'opts.maptoref_ref, opts.maptoref, opts.maptoref_consensus, ' +
                  'opts.maptoref_iter, opts.maptoref_topology ' +
                  'FROM assemble a ' +
                  'JOIN assemble_opts opts ' +
                  'ON a.assemble_opts = opts.assemble_opts ' +
                  'JOIN samples s ' +
                  'ON a.ID = s.ID ' +
                  'LEFT JOIN blast_opts b ' +
                  'ON a.blast_opts = b.blast_opts ' +
                  'WHERE (a.assemble_switch IN (1, 4) OR a.join_switch = 1) ' +
                  'AND a.assemble_lock = 0'

// A join-only redo the redo path deliberately does not service (the sample is
// being re-assembled by this same run, so the normal route runs its join). The
// flag has to be resolved anyway: nothing else clears it if the re-assembly ends
// single-scaffold or fails, and a stuck 1 makes every later run admit a no-op
// row and the app's Update modal report work that can never be done.
//
// This is NOT the second competing clearing write the spec warns against. That
// warning is about a second write racing the outcome write for the SAME sample:
// this statement touches one column, writes the same NULL that the outcome write
// would, is therefore order-independent and idempotent, and never touches
// assemble_switch or join_notes.
params.sqlClearJoinSwitch = 'UPDATE assemble SET join_switch = NULL WHERE ID = ?'

params.sqlDeleteAssemblies =  'DELETE FROM assemblies WHERE ID = ? AND time_stamp != ?'

// Upsert, NOT `INSERT OR REPLACE`: REPLACE is delete-then-insert, so every column
// missing from the list below is reset to NULL, including all six blast_* plus
// depth/gc/errors/edit_positions. That is a live data-loss race, not a
// theoretical one: nf-sqldb's InsertHandler batches (default 10 rows) and commits
// per batch with autocommit off, so this channel and the per-scaffold BLAST
// UPDATE in blast_genbank_workflow.nf commit in an order nothing guarantees. When
// a sample's assemblies row lands in a later batch than its BLAST update, the
// REPLACE silently wipes the committed hit. Observed on one sample of 14.
// DO UPDATE touches only the columns this channel owns, so commit order stops
// mattering. Same placeholder arity as before, so callers are unchanged.
// Column notes for the DO UPDATE below:
//   depth/gc/errors/edit_positions/blast_lineage describe the OLD sequence this
//   row is replacing, so they are cleared. INSERT OR REPLACE cleared them as a
//   side effect; an upsert has to say so. Stale edit_positions is the worst of
//   them, since the app would highlight edits at coordinates in a sequence that
//   no longer exists. depth/gc/errors are recomputed by coverage, and
//   blast_lineage is rewritten by BLAST_REF_FETCH later in the run.
//   The blast_* hit is kept ONLY when the existing row carries this run's
//   time_stamp, which is the point of the upsert: the BLAST channel may commit
//   first. A row still holding a previous run's time_stamp describes a
//   superseded assembly, so its hit is cleared exactly as REPLACE used to.
// No SQL `--` comments inside the statement: if the string is ever flattened,
// everything after one would be silently commented out.
params.sqlWriteAssemblies = '''INSERT INTO assemblies
    (ID, path, scaffold, length, length_raw, topology, time_stamp, sequence, ignore, edited)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, 0)
    ON CONFLICT(ID, path, scaffold) DO UPDATE SET
      length     = excluded.length,
      length_raw = excluded.length_raw,
      topology   = excluded.topology,
      time_stamp = excluded.time_stamp,
      sequence   = excluded.sequence,
      ignore     = excluded.ignore,
      edited     = 0,
      depth          = NULL,
      gc             = NULL,
      errors         = NULL,
      edit_positions = NULL,
      blast_lineage  = NULL,
      blast_accession = CASE WHEN assemblies.time_stamp = excluded.time_stamp
                             THEN assemblies.blast_accession ELSE NULL END,
      blast_species   = CASE WHEN assemblies.time_stamp = excluded.time_stamp
                             THEN assemblies.blast_species ELSE NULL END,
      blast_pident    = CASE WHEN assemblies.time_stamp = excluded.time_stamp
                             THEN assemblies.blast_pident ELSE NULL END,
      blast_qcovs     = CASE WHEN assemblies.time_stamp = excluded.time_stamp
                             THEN assemblies.blast_qcovs ELSE NULL END,
      blast_evalue    = CASE WHEN assemblies.time_stamp = excluded.time_stamp
                             THEN assemblies.blast_evalue ELSE NULL END'''

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
                        assembler: it[7],                                       // assembler
                        maptoref: it[20],                                       // MapToRef bowtie2 options
                        maptoref_consensus: it[21],                             // MapToRef samtools consensus options
                        maptoref_iter: (it[22] == null ? 5 : (it[22] as Integer)),
                        maptoref_topology: (it[23] ?: "")
                    ],
                    [
                        it[4],                                                  // getOrganelle seeds_db
                        it[5]                                                   // getOrganelle labels_db
                    ],
                    it[8],                                                      // mitofinder .gb reference database
                    it[10],                                                     // genetic code
                    (it[11] == null ? Integer.MAX_VALUE : (it[11] as Integer)), // max_paths
                    (it[12] == null ? Integer.MAX_VALUE : (it[12] as Integer)), // max_scaffolds
                    file((it[19] != null && it[19].toString().trim()) ? it[19] : "${projectDir}/assets/NO_FILE")  // MapToRef reference
                )
                min_len_scaffolds: tuple(it[0], it[13] == null ? 500 : (it[13] as Integer)) // ID, min_assembly_length (for per-scaffold ignore flag)
                min_len_summary:   tuple(it[0], it[13] == null ? 500 : (it[13] as Integer)) // ID, min_assembly_length (for per-sample all-short check)
                run_blast_lookup:  tuple(it[0], it[14] == null ? 1 : (it[14] as Integer))   // ID, run_blast (NULL -> 1, i.e. BLAST by default)
                join_lookup:       tuple(it[0], it[15] == null ? 0 : (it[15] as Integer))   // ID, join_scaffolds toggle (NULL -> 0, off)
                // Redo request: join_switch = 1 means "redo only the scaffold
                // join for this sample". Carried with the state it must be
                // filtered against below.
                join_redo:         tuple(
                    it[0],                                                                  // ID
                    it[1],                                                                  // assemble_opts (also the published directory name)
                    it[15] == null ? 0 : (it[15] as Integer),                               // join_scaffolds toggle
                    it[16] == null ? 0 : (it[16] as Integer),                               // join_switch
                    it[17] == null ? 0 : (it[17] as Integer),                               // assemble_switch
                    it[18]                                                                  // blast_accession (top hit)
                )
            }
            .set { query_ch }

        query_ch.opts.set { assemble_opts }
        query_ch.min_len_scaffolds.set { min_len_lookup }
        query_ch.min_len_summary.set { min_len_summary }
        query_ch.run_blast_lookup.set { run_blast_lookup }
        query_ch.join_lookup.set { join_lookup }

        // Samples queued for a join-only redo. The WHERE clause above admits
        // them on join_switch alone, so they arrive here even at state 2 or 3.
        // States 1 and 4 are excluded deliberately: those samples are being
        // (re-)assembled by this same run and reach the join by the normal
        // route, so admitting them here would feed the join twice.
        query_ch.join_redo
            .filter { id, opts, join_scaffolds, join_switch, assemble_switch, blast_accession ->
                join_switch == 1 }
            .branch { id, opts, join_scaffolds, join_switch, assemble_switch, blast_accession ->
                // States 1 and 4 are being (re-)assembled by this same run and
                // reach the join by the normal route, so servicing them here
                // would feed the join twice for one sample.
                moot: assemble_switch == 1 || assemble_switch == 4
                redo: true
            }
            .set { redo_branch }

        redo_branch.redo
            .map { id, opts, join_scaffolds, join_switch, assemble_switch, blast_accession ->
                tuple(id, opts, join_scaffolds, blast_accession) }
            .set { join_redo_ch }

        // Resolve the flag for the requests this workflow will not service, so
        // it cannot stick at 1 forever.
        redo_branch.moot
            .map { id, opts, join_scaffolds, join_switch, assemble_switch, blast_accession ->
                tuple(id) }
            .sqlInsert(statement: params.sqlClearJoinSwitch, db: 'sqlite')

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
                    it[1][7],                                                   // max_scaffolds
                    it[1][8]                                                    // MapToRef reference
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
                // Every contig is listed: de-duplicating hid a fragmented
                // assembly whose pieces were the same size behind one
                // value that read like a total.
                def length_str  = lengths.sort().reverse().join(';')
                def lengths_all = lengths.sort().reverse().join(';')
                def topo_str    = topologies.unique().sort().join(';')
                tuple(it[0], n_paths, n_scaffolds, length_str, topo_str, lengths_all, it, it[8], it[9])
            }
            .set { summarized }

        // Apply user-configured thresholds: split into pass / fail branches.
        // Combine with min_len_summary (per-sample all-short check) and
        // run_blast_lookup (state=2 finalization for run_blast=0 samples).
        summarized
            .combine(min_len_summary, by: 0)
            .combine(run_blast_lookup, by: 0)
            .branch { id, n_paths, n_scaffolds, length_str, topo_str, lengths_all, raw, max_paths, max_scaffolds, min_assembly_length, run_blast ->
                fail: (n_paths > max_paths) || (n_scaffolds > max_scaffolds)
                pass: true
            }
            .set { branched }

        // PASS: classify each sample's status/notes and emit per channel.
        // Only state=4 propagates downstream; state=3 (failed) and state=2
        // (run_blast=0 success) are terminal here.
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
                // no_blast samples: if assembly succeeded (status '4') and BLAST is
                // not requested, write state=2 directly here.
                if (status == '4' && (run_blast as Integer) == 0) {
                    status = '2'
                }
                tuple(id, n_paths, n_scaffolds, length_str, topo_str, raw, status, notes)
            }
            .multiMap { id, n_paths, n_scaffolds, length_str, topo_str, raw, status, notes ->
                db_write:   tuple(n_paths, n_scaffolds, length_str, topo_str, status, notes, params.ts, id)
                fasta:      tuple(id, raw[1])
                downstream: tuple(raw, status)
                join_meta:  tuple(id, n_paths, n_scaffolds, status, raw[1], raw[4])
            }
            .set { pass_ch }

        // Live per-sample UPDATE of the assemble table
        pass_ch.db_write
            .sqlInsert(statement: params.sqlWriteAssemble, db: 'sqlite')

        // Per-scaffold rows (still live; no groupTuple). Kept in a channel because
        // the annotate seed below needs the same per-unit records: they are written
        // to assemblies by this run, and channel.fromQuery snapshots the database at
        // session start, so a query would not see them until the NEXT run.
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
            .set { scaffold_rows }

        // INSERT into assemblies table
        scaffold_rows
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

        // Seed one annotate row PER non-ignored unit (ID, path, scaffold). Each
        // retained scaffold is its own annotation unit; topology/partial come
        // from that scaffold (partial rule matches validate: circular or
        // linear_complete -> "no", else "yes"). VALIDATE later recomputes
        // topology/partial per unit.
        //
        // The unit list comes from scaffold_rows (this run's own records), NOT from
        // a query on assemblies: fromQuery runs once at session ignition, before any
        // task, so a query cannot see rows this run writes and the seed would lag a
        // run behind, leaving every unit but the init-seeded (1,1) with no annotate
        // row and silently dropped by WF2's inner join.
        //
        // Options (annotate/curate/orf) and linear_complete ARE safe to query: they
        // are user-set before launch, so the session-start snapshot is current. They
        // are inherited from the sample's existing annotate rows (min-path row) so a
        // re-assembly preserves the user's option choices.
        channel.fromQuery(
                'SELECT an.ID, an.annotate_opts, an.curate_opts, an.orf_opts, ' +
                'COALESCE(co.linear_complete, 0) ' +
                'FROM (SELECT ID, annotate_opts, curate_opts, orf_opts, MIN(path) ' +
                      'FROM annotate GROUP BY ID) an ' +
                'LEFT JOIN curate_opts co ON co.curate_opts = an.curate_opts;', db: 'sqlite')
            .set { unit_opts }

        scaffold_rows
            .filter { it[8] == 0 }                          // non-ignored units only
            .map { tuple(it[0], it[1], it[2], it[5]) }      // ID, path, scaffold, topology
            .combine(unit_opts, by: 0)                      // + opts, linear_complete
            .map { id, path, scaffold, topology, annotate_opts, curate_opts, orf_opts, linear_complete ->
                def partial = (topology == 'circular' || (linear_complete as Integer) == 1) ? 'no' : 'yes'
                tuple(id, path, scaffold, topology, partial, annotate_opts, curate_opts, orf_opts)
            }
            .sqlInsert(statement: 'INSERT OR REPLACE INTO annotate (ID, path, scaffold, topology, partial, annotate_opts, curate_opts, orf_opts, annotate_switch, annotate_lock, reviewed) VALUES (?, ?, ?, ?, ?, ?, ?, ?, 1, 0, "no")', db: 'sqlite')

        // Single-path multi-scaffold samples eligible for scaffold joining,
        // carrying the per-sample join_scaffolds toggle. Built once here so the
        // two emits below cannot drift into two definitions of "eligible".
        // tuple(id, assembly_fasta, opts_id, status, join_scaffolds)
        pass_ch.join_meta
            .filter { id, n_paths, n_scaffolds, status, fasta, opts ->
                (status == '4' || status == '2') && n_paths == 1 && n_scaffolds > 1 }
            .map    { id, n_paths, n_scaffolds, status, fasta, opts ->
                tuple(id, fasta, opts, status) }
            .join(join_lookup)
            .set { join_eligible_meta }

    emit:
        // Two named channels with different gating:
        //   cov   - usable assemblies (status 4, or status 2 from run_blast=0).
        //           COVERAGE runs for no_blast samples too so depth/gc/errors and
        //           coverageStats.csv get populated for ANNOTATE.
        //   blast - status=4 only (excludes no_blast and failed assemblies).
        cov   = pass_ch.downstream
                    .filter { raw, status -> status == '4' || status == '2' }
                    .map    { raw, status -> raw }
        blast = pass_ch.downstream
                    .filter { raw, status -> status == '4' }
                    .map    { raw, status -> raw }
        // Single-path multi-scaffold samples eligible for scaffold joining. The
        // mapping precompute runs for ALL eligible samples; the join_scaffolds
        // toggle (joined in here) only gates the automatic Path 0 BUILD downstream.
        // tuple(id, assembly_fasta, opts_id, join_scaffolds)
        join_eligible = join_eligible_meta
                    .map { id, fasta, opts, status, join_scaffolds ->
                        tuple(id, fasta, opts, join_scaffolds) }
        // IDs that are expected to reach the scaffold join in THIS run: eligible
        // and still heading through BLAST (status 4). status 2 samples are
        // run_blast = 0, already finalized by this workflow, and never get a
        // fetched reference, so they must not be withheld from state 2 nor
        // reported as missing an input.
        join_expected = join_eligible_meta
                    .filter { id, fasta, opts, status, join_scaffolds -> status == '4' }
                    .map    { id, fasta, opts, status, join_scaffolds -> id }
        // Join-only redo requests, for SCAFFOLD_JOIN to resolve against the
        // published outputs of the run that assembled the sample.
        // tuple(id, assemble_opts, join_scaffolds, blast_accession)
        join_redo = join_redo_ch

}
