include {coverage_userAsmb; coverage_userAsmb_noReads} from './coverage_userAsmb.nf'
include {CIRCULARIZE_userAsmb; CIRCULARIZE_userAsmb_noReads} from './circularize_workflow.nf'
include {FIND_MITO_userAsmb} from './find_mito_workflow.nf'

params.sqlRead =  'SELECT s.ID, s.assembly, s.topology, ' +
                  'a.assemble_opts, opts.min_assembly_length, ' +
                  'copts.attempt, copts.min_overlap, copts.min_identity, ' +
                  'copts.min_junction_reads, copts.min_overhang, copts.cpus, copts.memory, ' +
                  's.genetic_code, ' +
                  'fopts.attempt, fopts.mitofinder_db, fopts.min_contig_length, ' +
                  'fopts.min_identity, fopts.min_aligned_length, fopts.min_aligned_fraction, ' +
                  'fopts.max_candidates, fopts.min_genes, fopts.cpus, fopts.memory, ' +
                  'opts.join_scaffolds, bopts.run_blast ' +
                  'FROM samples s ' +
                  'JOIN assemble a ON s.ID = a.ID ' +
                  'JOIN assemble_opts opts ON a.assemble_opts = opts.assemble_opts ' +
                  'LEFT JOIN circularize_opts copts ON a.circularize_opts = copts.circularize_opts ' +
                  'LEFT JOIN find_mito_opts fopts ON a.find_mito_opts = fopts.find_mito_opts ' +
                  'LEFT JOIN blast_opts bopts ON a.blast_opts = bopts.blast_opts ' +
                  'WHERE a.assemble_switch IN (1, 4) AND a.assemble_lock = 0'


params.sqlWrite =   'UPDATE assemblies SET depth = ?, gc = ?, errors = ?, time_stamp = ? ' +
                    'WHERE ID=? and path=? and scaffold=?'

params.sqlDeleteAssemblies =  'DELETE FROM assemblies WHERE ID = ? AND time_stamp != ?'

// Upsert for the same reason as assemble_workflow.nf: `INSERT OR REPLACE` is
// delete-then-insert and nulls every unlisted column (all six blast_*, plus
// depth/gc/errors/edit_positions), which races the per-scaffold BLAST UPDATE
// because nf-sqldb batches and commits each channel independently.
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

// Samples the app queued for a join-only redo (assemble.join_switch = 1). Read
// separately from params.sqlRead because a redo sample sits at state 2 or 3 and
// that query only admits 1 and 4.
params.sqlReadJoinRedoUserAsmb =
    'SELECT a.ID, a.assemble_opts, COALESCE(opts.join_scaffolds, 0), ' +
    'COALESCE(a.join_switch, 0), COALESCE(a.assemble_switch, 0), a.blast_accession ' +
    'FROM assemble a ' +
    'JOIN assemble_opts opts ON a.assemble_opts = opts.assemble_opts ' +
    'WHERE a.join_switch = 1 AND a.assemble_lock = 0'

// A redo this run will not service must not leave the request queued forever.
params.sqlClearJoinSwitchUserAsmb = 'UPDATE assemble SET join_switch = NULL WHERE ID = ?'


// Shared writer: parse coverageStats -> assemblies/assemble DB writes + emit the
// BLAST input. Lives in one place so the (data-loss-sensitive) DB write logic is
// identical for the read-based and no-reads coverage workflows.
workflow COVERAGE_userAsmb_WRITE {
    take:
        coverage_out
        min_len_lookup
        min_len_summary
        join_lookup
        run_blast_lookup

    main:
        // Coverage output -> depth/gc/errors
        coverage_out
            .flatten()
            .filter{ it =~ /(.*coverageStats.csv)$/ }
            .splitCsv(header: true, sep: ',')
            .map { it ->
                tuple(
                    it.SeqId,
                    it.MeanDepth,
                    it.GC,
                    it.ErrorRate
                )
            }
            .groupTuple()
            .map { it ->
                tuple(
                    it[1].join(' '),                   // mean depth
                    it[2].join(' '),                   // gc
                    it[3].join(' '),                   // error rate
                    params.ts,                         // timestamp
                    it[0].split('\\.'),                // id, path, scaffold
                ).flatten()
            }
            .sqlInsert(statement: params.sqlWrite, db: 'sqlite')

       // Clear old assemblies from db
        coverage_out
          .map { it ->
            tuple(
              it[2],
              params.ts
            )
          }
          .sqlInsert( statement: params.sqlDeleteAssemblies, db: 'sqlite')

        // Write to assemblies table
        coverage_out
            .map { it -> it[3] }.flatten()
            .splitFasta(record: [id: true, desc: true, seqString: true])
            .map { record ->
                tuple(
                    record.id.split('\\.'),             // ID, path, scaffold
                    record.seqString.length(),          // length
                    record.seqString.length(),          // length_raw (initially equal to length; curate may later trim length)
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
            .set { assemblies_ch }
        assemblies_ch.sqlInsert( statement: params.sqlWriteAssemblies, db: 'sqlite')

        // Update DB assemble table
        assemblies_ch
            .map { it ->
                tuple(
                    it[0],                                          // ID
                    it[1].toInteger(),                              // paths
                    it[2].toInteger(),                              // scaffold
                    it[3].toInteger(),                              // length
                    it[5]                                           // topology (shifted after adding length_raw at it[4])
                )
            }
            .groupTuple()
            .combine(min_len_summary, by: 0)        // append per-sample min_assembly_length
            .map { id, paths_list, scaffolds_list, lengths_list, topos_list, min_assembly_length ->
                def max_paths      = paths_list.max()
                def max_scaffolds  = scaffolds_list.max()
                // Every contig is listed: de-duplicating hid a fragmented
                // assembly whose pieces were the same size behind one
                // value that read like a total.
                def length_str     = lengths_list.sort().reverse().join(";")
                def topo_str       = topos_list.unique().sort().join(";")
                def max_len        = lengths_list.max() ?: 0
                def status         = '4'
                def notes          = ''
                // Fragmented assemblies are a normal outcome: each contig is its own
                // unit downstream, so note it and keep status 4 (matches
                // assemble_workflow.nf).
                if (max_scaffolds > 1) {
                    def n_passing = lengths_list.count { it >= (min_assembly_length as Integer) }
                    notes = (n_passing == 1)
                        ? 'Output contains disconnected contigs'
                        : 'Output contains disconnected contigs (fragmented)'
                }
                if (max_paths > 1) {
                    status = '3'
                    notes  = 'Unable to resolve single assembly from reads'
                }
                if (max_len < (min_assembly_length as Integer)) {
                    status = '3'
                    notes  = "All scaffolds below min assembly length (${min_assembly_length} bp)"
                }
                tuple(max_paths, max_scaffolds, length_str, topo_str, status, notes, params.ts, id)
            }
            .set { assemble_summary }

        assemble_summary.sqlInsert(statement: params.sqlWriteAssemble , db: 'sqlite')

        // Join eligibility comes off the same summary the assemble row is built
        // from, so the table and the join can never disagree about how many
        // paths and scaffolds a sample has.
        assemble_summary
            .map { paths, scaffolds, length_str, topo_str, status, notes, ts, id ->
                tuple(id, paths as Integer, scaffolds as Integer, status)
            }
            .filter { id, n_paths, n_scaffolds, status ->
                status == '4' && n_paths == 1 && n_scaffolds > 1
            }
            .join(coverage_out.map { files, wd, id, fasta, opts -> tuple(id, fasta, opts) })
            .join(join_lookup)
            .join(run_blast_lookup)
            .set { join_eligible_meta }

        // Join-only redo requests. States 1 and 4 are being reprocessed by this
        // same run and reach the join by the normal route, so servicing them
        // here would feed the join twice for one sample.
        channel.fromQuery(params.sqlReadJoinRedoUserAsmb, db: 'sqlite')
            .filter { id, opts, join_scaffolds, join_switch, assemble_switch, blast_accession ->
                join_switch == 1
            }
            .branch { id, opts, join_scaffolds, join_switch, assemble_switch, blast_accession ->
                moot: assemble_switch == 1 || assemble_switch == 4
                redo: true
            }
            .set { redo_branch }

        redo_branch.moot
            .map { id, opts, join_scaffolds, join_switch, assemble_switch, blast_accession ->
                tuple(id)
            }
            .sqlInsert(statement: params.sqlClearJoinSwitchUserAsmb, db: 'sqlite')

        // Seed one annotate row PER non-ignored unit (ID, path, scaffold), the same
        // way assemble_workflow.nf does for the regular pipeline. WF2 reads its work
        // list from assemblies JOIN annotate on (ID, path, scaffold), so a contig
        // with no annotate row is silently invisible.
        //
        // The unit list comes from assemblies_ch (this run's own records), NOT from a
        // query on assemblies: fromQuery runs once at session ignition, before any
        // task, so it cannot see rows this run writes.
        //
        // Options (annotate/curate/orf) and linear_complete ARE safe to query: they
        // are user-set before launch. They are inherited from the sample's existing
        // annotate rows (min-path row) so a re-run preserves the user's choices.
        channel.fromQuery(
                'SELECT an.ID, an.annotate_opts, an.curate_opts, an.orf_opts, ' +
                'COALESCE(co.linear_complete, 0) ' +
                'FROM (SELECT ID, annotate_opts, curate_opts, orf_opts, MIN(path) ' +
                      'FROM annotate GROUP BY ID) an ' +
                'LEFT JOIN curate_opts co ON co.curate_opts = an.curate_opts;', db: 'sqlite')
            .set { unit_opts }

        assemblies_ch
            .filter { row -> row[8] == 0 }                          // non-ignored units only
            .map    { row -> tuple(row[0], row[1], row[2], row[5]) } // ID, path, scaffold, topology
            .combine(unit_opts, by: 0)                      // + opts, linear_complete
            .map { id, path, scaffold, topology, annotate_opts, curate_opts, orf_opts, linear_complete ->
                def partial = (topology == 'circular' || (linear_complete as Integer) == 1) ? 'no' : 'yes'
                tuple(id, path, scaffold, topology, partial, annotate_opts, curate_opts, orf_opts)
            }
            // Idempotent: an existing unit keeps its row and all of its state. Only
            // topology/partial are refreshed, and only on a unit nobody has worked on
            // yet (still switch 1, unlocked), because circularization can change a
            // contig's topology between runs.
            .sqlInsert(statement: '''INSERT INTO annotate
                (ID, path, scaffold, topology, partial, annotate_opts, curate_opts, orf_opts,
                 annotate_switch, annotate_lock, reviewed)
                VALUES (?, ?, ?, ?, ?, ?, ?, ?, 1, 0, 'no')
                ON CONFLICT(ID, path, scaffold) DO UPDATE SET
                  topology = excluded.topology,
                  partial  = excluded.partial
                WHERE annotate.annotate_switch = 1 AND annotate.annotate_lock = 0''', db: 'sqlite')

    emit:
        // tuple(id, assembly, opts_id) for single-contig BLAST search
        blast_in = coverage_out.map{ it -> tuple(it[2], it[3], it[4]) }

        // Per-ID coverageStats CSV files, for the scaffold join to stitch.
        cov_files = coverage_out
            .map { files, wd, id, fasta, opts ->
                def fl = (files instanceof List) ? files : [files]
                tuple(id, fl.findAll { it.name ==~ /.*coverageStats\.csv/ })
            }
            .filter { id, csvs -> csvs.size() > 0 }
            .groupTuple()
            .map { id, lists -> tuple(id, lists.flatten()) }

        // Single-path multi-scaffold samples eligible for the join, carrying the
        // per-sample join_scaffolds toggle. The mapping precompute runs for ALL
        // eligible samples; the toggle only gates the automatic Path 0 build.
        join_eligible = join_eligible_meta
            .map { id, np, ns, status, fasta, opts, join_scaffolds, run_blast ->
                tuple(id, fasta, opts, join_scaffolds)
            }

        // IDs expected to reach the join in THIS run, withheld from the
        // reference fetch's 4 -> 2 promotion so the join owns their final state.
        // Samples with BLAST switched off never get a fetched reference, so they
        // are excluded: withholding them would strand them, and reporting them
        // would call a missing input a failure.
        join_expected = join_eligible_meta
            .filter { id, np, ns, status, fasta, opts, join_scaffolds, run_blast ->
                (run_blast == null ? 1 : (run_blast as Integer)) == 1
            }
            .map { id, np, ns, status, fasta, opts, join_scaffolds, run_blast -> id }

        // tuple(id, assemble_opts, join_scaffolds, blast_accession)
        join_redo = redo_branch.redo
            .map { id, opts, join_scaffolds, join_switch, assemble_switch, blast_accession ->
                tuple(id, opts, join_scaffolds, blast_accession)
            }
}


workflow COVERAGE_userAsmb {
    take:
        input

    main:
        // sample info channel from DB
        channel.fromQuery(params.sqlRead, db: 'sqlite')
            .multiMap { it ->
                info: tuple(
                    it[0],                                          // ID
                    file(params.asmbDir + "/" + it[1]),             // assembly
                    it[2],                                          // topology
                    it[3],                                          // assemble opts dummy var
                    [ attempt:            it[5],                    // circularization opts
                      min_overlap:        it[6],
                      min_identity:       it[7],
                      min_junction_reads: it[8],
                      min_overhang:       it[9],
                      cpus:               it[10],
                      memory:             it[11] ]
                )
                gcode: tuple(it[0], it[12] == null ? 2 : (it[12] as Integer))
                find_opts: tuple(it[0], [ attempt:              it[13],
                      mitofinder_db:        it[14],
                      min_contig_length:    it[15],
                      min_identity:         it[16],
                      min_aligned_length:   it[17],
                      min_aligned_fraction: it[18],
                      max_candidates:       it[19],
                      min_genes:            it[20],
                      cpus:                 it[21],
                      memory:               it[22] ])
                min_len_scaffolds: tuple(it[0], it[4] == null ? 500 : (it[4] as Integer)) // ID, min_assembly_length (for per-scaffold ignore flag)
                min_len_summary:   tuple(it[0], it[4] == null ? 500 : (it[4] as Integer)) // ID, min_assembly_length (for per-sample all-short check)
                join_lookup:       tuple(it[0], it[23] == null ? 0 : (it[23] as Integer))
                run_blast_lookup:  tuple(it[0], it[24] == null ? 1 : (it[24] as Integer))
            }
            .set { query_ch }

        // Optional mitogenome search runs first, so a multi-contig assembly is
        // cut down before anything is written to the database.
        query_ch.info
            .join(query_ch.gcode)
            .join(query_ch.find_opts)
            .set { find_in }
        FIND_MITO_userAsmb(find_in)
        FIND_MITO_userAsmb.out.sample_info.set { sample_info }
        query_ch.min_len_scaffolds.set { min_len_lookup }
        query_ch.min_len_summary.set { min_len_summary }

        // Coverage Input Channel
        input
            // cross with sample info
            .cross(sample_info)
            .map{ it ->
                tuple(
                    it[0][0],                                                   // ID
                    it[0][1],                                                   // trimmed reads in
                    it[1][1],                                                   // assembly
                    it[1][2],                                                   // topology
                    it[1][3],                                                   // assemble opts dummy var
                    it[1][4],                                                   // circularization opts
                )
            }
            .set { circ_in }

        // Optional circularization (pass-through when switched off)
        CIRCULARIZE_userAsmb(circ_in)
        CIRCULARIZE_userAsmb.out.coverage_in.set { coverage_in }

        // Coverage
        coverage_userAsmb(coverage_in).set { coverage_out }

        COVERAGE_userAsmb_WRITE(coverage_out, min_len_lookup, min_len_summary,
                                query_ch.join_lookup, query_ch.run_blast_lookup)

    emit:
        blast_in      = COVERAGE_userAsmb_WRITE.out.blast_in
        cov_files     = COVERAGE_userAsmb_WRITE.out.cov_files
        join_eligible = COVERAGE_userAsmb_WRITE.out.join_eligible
        join_expected = COVERAGE_userAsmb_WRITE.out.join_expected
        join_redo     = COVERAGE_userAsmb_WRITE.out.join_redo
}


// No-reads variant: user-provided assembly, no raw data. Samples come straight
// from the DB (no PREPROCESS) and coverage() runs in its no-reads mode, deriving
// GC from the sequence and leaving depth/error stats empty.
workflow COVERAGE_userAsmb_noReads {
    main:
        channel.fromQuery(params.sqlRead, db: 'sqlite')
            .multiMap { it ->
                info: tuple(
                    it[0],                                          // ID
                    file(params.asmbDir + "/" + it[1]),             // assembly
                    it[2],                                          // topology
                    it[3],                                          // assemble opts dummy var
                    [ attempt:            it[5],                    // circularization opts
                      min_overlap:        it[6],
                      min_identity:       it[7],
                      min_junction_reads: it[8],
                      min_overhang:       it[9],
                      cpus:               it[10],
                      memory:             it[11] ]
                )
                gcode: tuple(it[0], it[12] == null ? 2 : (it[12] as Integer))
                find_opts: tuple(it[0], [ attempt:              it[13],
                      mitofinder_db:        it[14],
                      min_contig_length:    it[15],
                      min_identity:         it[16],
                      min_aligned_length:   it[17],
                      min_aligned_fraction: it[18],
                      max_candidates:       it[19],
                      min_genes:            it[20],
                      cpus:                 it[21],
                      memory:               it[22] ])
                min_len_scaffolds: tuple(it[0], it[4] == null ? 500 : (it[4] as Integer))
                min_len_summary:   tuple(it[0], it[4] == null ? 500 : (it[4] as Integer))
                join_lookup:       tuple(it[0], it[23] == null ? 0 : (it[23] as Integer))
                run_blast_lookup:  tuple(it[0], it[24] == null ? 1 : (it[24] as Integer))
            }
            .set { query_ch }

        // Optional mitogenome search runs first, so a multi-contig assembly is
        // cut down before anything is written to the database.
        query_ch.info
            .join(query_ch.gcode)
            .join(query_ch.find_opts)
            .set { find_in }
        FIND_MITO_userAsmb(find_in)
        FIND_MITO_userAsmb.out.sample_info.set { sample_info }
        query_ch.min_len_scaffolds.set { min_len_lookup }
        query_ch.min_len_summary.set { min_len_summary }

        // No reads to cross in; feed the assembly straight to the no-reads process
        CIRCULARIZE_userAsmb_noReads(sample_info)
        CIRCULARIZE_userAsmb_noReads.out.coverage_in.set { coverage_in }

        coverage_userAsmb_noReads(coverage_in).set { coverage_out }

        COVERAGE_userAsmb_WRITE(coverage_out, min_len_lookup, min_len_summary,
                                query_ch.join_lookup, query_ch.run_blast_lookup)

    emit:
        blast_in      = COVERAGE_userAsmb_WRITE.out.blast_in
        cov_files     = COVERAGE_userAsmb_WRITE.out.cov_files
        join_eligible = COVERAGE_userAsmb_WRITE.out.join_eligible
        join_expected = COVERAGE_userAsmb_WRITE.out.join_expected
        join_redo     = COVERAGE_userAsmb_WRITE.out.join_redo
}
