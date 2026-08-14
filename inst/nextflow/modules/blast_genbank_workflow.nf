include {blast_genbank} from './blast_genbank.nf'

// SQL fragment that returns assemble_notes with any segment starting with the
// given tag (and a preceding '; ' if present) stripped. Used so each stage's
// failure message can be idempotently replaced across re-runs instead of
// accumulating duplicates on -resume.
def stripTagSql(String tag) {
    def lit = tag.replace("'", "''")
    return "RTRIM(" +
        "CASE WHEN INSTR(COALESCE(assemble_notes,''), '${lit}') > 0 " +
            "THEN SUBSTR(COALESCE(assemble_notes,''), 1, INSTR(COALESCE(assemble_notes,''), '${lit}') - 1) " +
            "ELSE COALESCE(assemble_notes,'') END" +
    ", '; ')"
}

// SQL fragment that appends a tagged message to assemble_notes, after first
// stripping any prior segment with the same tag.
def appendTaggedNoteSql(String tag, String msg) {
    def stripped = stripTagSql(tag)
    def tagged = (tag + ' ' + msg).replace("'", "''")
    return "CASE WHEN ${stripped} = '' THEN '${tagged}' ELSE ${stripped} || '; ${tagged}' END"
}

// Two distinct failure modes, distinguished by blast_genbank.nf:
//   - blastNoOutputMsg: blast produced no output after all retries (connection / tool error).
//   - blastNoHitMsg:    blast ran cleanly but found no significant hits (sentinel output).
params.blastNoOutputMsg = "BLAST produced no output. Most often this is a legacy Entrez query that the local BLAST database cannot apply (open BLAST Options, check Edit, and tick Remote BLAST, which both applies the query and reveals the Entrez query field so it can be cleared, then click Update), bad additional blastn options, an unreadable local BLAST database, or, for a remote search, an NCBI connection or rate-limit failure. The blast_genbank task's error output is in its work directory (Work Directory browser). To retry, set this sample back to 'Ready to Assemble' (State button) and re-run the pipeline."
params.blastNoHitMsg = "No significant BLAST hits found for this assembly. The assembly may be non-target, too fragmented, or from a taxon with no mitogenome in the reference database. If BLAST Options carries a taxon ID restriction, check that it names a clade the database actually contains."

// blast_accession_auto mirrors the automatic rank-1 hit so a later user override
// of blast_accession can be flagged in the tables; set both from the same values.
params.sqlWriteBlastHit = 'UPDATE assemble SET blast_accession = ?, blast_accession_auto = ?, blast_species = ?, blast_pident = ?, blast_qcovs = ?, blast_evalue = ? WHERE ID = ?'
// blast_lineage is NOT set here: ref fetch hasn't run yet so the subquery would
// resolve to NULL, and this deferred commit could clobber the lineage written
// later by BLAST_REF_FETCH. Lineage is handled solely in blast_ref_fetch_workflow.nf.
// Upsert, NOT a plain UPDATE. This channel and the assemblies write in
// assemble_workflow.nf are separate nf-sqldb handlers with autocommit off,
// batching 10 rows each, so nothing orders their commits. A plain UPDATE run
// before the assemblies row is committed matches ZERO rows and the hit is lost
// silently: observed on 1 sample of 14, whose BLAST update fell in the first
// (early-committed) batch while its assemblies row fell in the second.
// Making BOTH statements upserts on the same primary key makes them
// commutative, so either commit order produces the same final row. If this
// lands first it creates the row carrying only the hit, and the assemblies
// upsert then fills in length/sequence/topology without disturbing it.
// Placeholder order is unchanged (accession, species, pident, qcovs, evalue,
// id, path, scaffold), so the feeding channel needs no edit.
params.sqlWriteBlastHitScaffold = '''INSERT INTO assemblies
    (blast_accession, blast_species, blast_pident, blast_qcovs, blast_evalue, ID, path, scaffold)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?)
    ON CONFLICT(ID, path, scaffold) DO UPDATE SET
      blast_accession = excluded.blast_accession,
      blast_species   = excluded.blast_species,
      blast_pident    = excluded.blast_pident,
      blast_qcovs     = excluded.blast_qcovs,
      blast_evalue    = excluded.blast_evalue'''
params.sqlWriteAssembleSwitch = 'UPDATE assemble SET assemble_switch = ? WHERE ID = ? AND assemble_switch = 4'
// Connection / tool failure: blast produced no output file after all retries.
params.sqlWriteBlastNoOutput = "UPDATE assemble SET " +
    "assemble_switch = 3, " +
    "assemble_notes = ${appendTaggedNoteSql('[blast]', params.blastNoOutputMsg)}, " +
    "poor_blast_ref = 'failed' " +
    "WHERE ID = ? AND assemble_switch = 4"
// Genuine no-hit: blast ran cleanly but every path returned no significant hit.
params.sqlWriteBlastNoHit = "UPDATE assemble SET " +
    "assemble_switch = 3, " +
    "assemble_notes = ${appendTaggedNoteSql('[blast]', params.blastNoHitMsg)}, " +
    "poor_blast_ref = 'failed' " +
    "WHERE ID = ? AND assemble_switch = 4"
params.sqlWriteAssemblyBlast = 'INSERT OR REPLACE INTO assembly_blast (ID, path, blast_opts, blast_accession, blast_species, blast_pident, blast_qcovs, blast_evalue, time_stamp) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)'
params.sqlDeleteAssemblyBlast = 'DELETE FROM assembly_blast WHERE ID = ? AND time_stamp != ?'
params.sqlWriteCandidate = 'INSERT OR REPLACE INTO blast_ref_candidates (ID, path, scaffold, rank, accession, species, pident, qcovs, evalue, time_stamp) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)'
params.sqlDeleteCandidates = 'DELETE FROM blast_ref_candidates WHERE ID = ? AND time_stamp != ?'

// New columns are appended at the END: the row[N] indices below are positional.
params.sqlReadBlastOpts =
    'SELECT a.ID, b.run_blast, b.entrez_query, b.extra_opts, b.max_target_seqs, ' +
    'b.taxids, b.remote_blast, b.remote_fallback ' +
    'FROM assemble a ' +
    'JOIN blast_opts b ON a.blast_opts = b.blast_opts'

params.sqlReadMinLen =
    'SELECT a.ID, opts.min_assembly_length ' +
    'FROM assemble a ' +
    'JOIN assemble_opts opts ON a.assemble_opts = opts.assemble_opts'

workflow BLAST_GENBANK {
    take:
        // input: tuple(id, assembly_file_or_list, opts_id)
        // WF1:          from ASSEMBLE.out.blast mapped to (id, it[1], it[4])
        // WF1_userAsmb: from COVERAGE_userAsmb.out.blast_in as (id, assembly, opts_id)
        input

    main:
        // Read per-sample BLAST opts from DB; filter to run_blast == 1.
        // max_target_seqs is now a per-parameter-set DB value (was a .config
        // param); it sets both the blastn -max_target_seqs flag and the number
        // of candidate references retained (N_CAND below). Default 5.
        // taxids / remote_blast / remote_fallback are defaulted here as well as in
        // blast_genbank.nf, so a NULL from an older project database is harmless.
        channel.fromQuery(params.sqlReadBlastOpts, db: 'sqlite')
            .filter { row -> row[1] as Integer == 1 }
            .map { row -> tuple(row[0], row[2],
                                (row[5] ?: '').toString().trim(),
                                (row[6] == null ? 0 : (row[6] as Integer)),
                                (row[7] == null ? 1 : (row[7] as Integer)),
                                row[3] ?: '', (row[4] == null ? 5 : (row[4] as Integer))) }
            .set { blast_opts_ch }  // (id, entrez_query, taxids, remote_blast, remote_fallback, extra_opts, max_target_seqs)

        // Per-sample max_target_seqs, used to bound the retained candidate list.
        channel.fromQuery(params.sqlReadBlastOpts, db: 'sqlite')
            .filter { row -> row[1] as Integer == 1 }
            .map { row -> tuple(row[0], (row[4] == null ? 5 : (row[4] as Integer))) }
            .set { mts_ch }  // (id, max_target_seqs)

        input
            // Normalize: wrap single Path in a list so downstream logic is uniform
            .map{ id, asmbs, opts_id ->
                def asmb_list = (asmbs instanceof List) ? asmbs : [asmbs]
                tuple(id, asmb_list, opts_id)
            }
            .set { normalized_input }

        // Read per-sample min_assembly_length from DB
        channel.fromQuery(params.sqlReadMinLen, db: 'sqlite')
            .map { row -> tuple(row[0], row[1] == null ? 500 : (row[1] as Integer)) }
            .set { min_len_ch }  // (id, min_assembly_length)

        // Per-path stream with min-length scaffold filtering. For each assembly path,
        // keep only scaffolds with length >= min_assembly_length and write a target
        // FASTA preserving original headers ">{id}.{path}.{scaffold} topology" so qseqid
        // (the first whitespace-delimited token) parses back to (path, scaffold)
        // downstream. One BLAST job per qualifying path (carries path_idx).
        //
        // Built PER SAMPLE (one emit per sample, targets bundled in a list) rather
        // than flat per-path, so the sample's BLAST-path count is known in the same
        // step and can be streamed to path_count_ch without a cohort-wide groupTuple
        // (see below). ASSEMBLE.out.blast already delivers one item per sample.
        normalized_input
            .join(min_len_ch, by: 0)
            .map { id, asmb_list, opts_id, min_len ->
                def realFiles = asmb_list.findAll { !(it.name =~ /assembly_0\.fasta$/) }
                def targets = []
                for (def f : realFiles) {
                    def m = (f.name =~ /assembly_(\d+)\.fasta$/)
                    def path_idx = m ? (m[0][1] as Integer) : 0
                    def qualifying = []
                    def header = null
                    def seq = new StringBuilder()
                    f.eachLine { line ->
                        if (line.startsWith('>')) {
                            if (header != null && seq.length() >= min_len) {
                                qualifying << [header: header, seq: seq.toString()]
                            }
                            header = line
                            seq = new StringBuilder()
                        } else {
                            seq.append(line.trim())
                        }
                    }
                    if (header != null && seq.length() >= min_len) {
                        qualifying << [header: header, seq: seq.toString()]
                    }
                    if (qualifying.size() < 1) continue
                    def targetDirStr = "${workflow.workDir}/blast_select_targets"
                    new File(targetDirStr).mkdirs()
                    def targetFasta = new File("${targetDirStr}/${id}.${path_idx}.blast_target.fasta")
                    targetFasta.text = qualifying.collect { "${it.header}\n${it.seq}" }.join('\n') + '\n'
                    targets << [path_idx, targetFasta.toPath()]
                }
                tuple(id, opts_id, targets)
            }
            .set { per_sample_blast_targets }  // (id, opts_id, [[path_idx, fasta], ...])

        // Expand to one entry per qualifying path, then join blast opts; samples
        // with run_blast = 0 have no blast_opts entry and are dropped here.
        per_sample_blast_targets
            .flatMap { id, opts_id, targets ->
                targets.collect { t -> tuple(id, t[0], t[1], opts_id) }
            }
            .combine(blast_opts_ch, by: 0)
            .map{ id, path_idx, asmb, opts_id, entrez_query, taxids, remote_blast, remote_fallback, extra_opts, mts ->
                tuple(id, path_idx, asmb, opts_id, entrez_query, taxids, remote_blast, remote_fallback, extra_opts, mts)
            }
            .multiMap { id, path_idx, asmb, opts_id, entrez_query, taxids, remote_blast, remote_fallback, extra_opts, mts ->
                process: tuple(id, path_idx, asmb, opts_id, entrez_query, taxids, remote_blast, remote_fallback, extra_opts, mts)
                ids:     tuple(id, true)
                pathkey: id
            }
            .set { blast_in_split }

        // Number of BLAST paths per sample, emitted ONCE PER SAMPLE as soon as its
        // assembly is processed (target build above is local/fast). The previous
        // bare groupTuple over blast_in_split could not emit until the whole
        // cohort's assemblies had finished, so the per-sample ref-fetch batch
        // (sample_agg, sized by groupKey(id, n_paths)) stalled behind the slowest
        // assembly. Streaming the count per sample lets each sample's ref fetch
        // start as soon as ITS own paths finish BLAST. blast_opts_ch is one row per
        // run_blast=1 sample, so this pre-join count equals the post-join blast task
        // count for every sample that reaches sample_agg (run_blast=0 samples never
        // do, so a spurious count for them is harmless).
        per_sample_blast_targets
            .map { id, opts_id, targets -> tuple(id, targets.size()) }
            .set { path_count_ch }  // (id, n_paths)

        // Clear stale per-path rows before re-inserting. Time-stamp gating mirrors
        // the assemblies-table delete pattern so this is safe regardless of channel
        // emission order vs the insert below.
        normalized_input
            .map { id, asmb_list, opts_id -> tuple(id, params.ts) }
            .sqlInsert(statement: params.sqlDeleteAssemblyBlast, db: 'sqlite')

        // .hits is the (id, path_idx, result_file) tuple; the process also emits
        // db_version (the local database's VERSION file), published but unused here.
        blast_genbank(blast_in_split.process).hits
            .multiMap { id, path_idx, result_file ->
                state:      tuple(id, result_file)
                parse:      tuple(id, path_idx, result_file)
                perpath:    tuple(id, path_idx, result_file)
                candidates: tuple(id, path_idx, result_file)
                succeeded:  tuple(id, true)
            }
            .set { blast_out }

        // Per-SCAFFOLD candidate references: for each scaffold (BLAST query), its
        // own top-N hits, ranked and keyed by (ID, path, scaffold). Hits are NEVER
        // merged across scaffolds/paths, so a divergent scaffold keeps its own
        // reference list. blastn already caps hits per query at max_target_seqs.
        blast_out.candidates
            .flatMap { id, path_idx, result_file ->
                def lines = result_file.readLines().findAll { it.trim() }
                def per_scaffold = [:]
                lines.each { line ->
                    def parts = line.split('\t')
                    if (parts.size() >= 6) {
                        def rec = [
                            parts[1],
                            parts[2],
                            Math.round(parts[3].toFloat() * 100) / 100.0,
                            Math.round(parts[4].toFloat() * 100) / 100.0,
                            parts[5].toDouble()
                        ]
                        if (!per_scaffold.containsKey(parts[0])) per_scaffold[parts[0]] = []
                        per_scaffold[parts[0]] << rec
                    }
                }
                def out = []
                per_scaffold.each { qseqid, hits ->
                    def toks = qseqid.split(/\./)
                    if (toks.size() < 3) return
                    def path = toks[-2]; def scaffold = toks[-1]
                    // Dedup accessions within THIS scaffold, keeping BLAST's own
                    // ordering: blastn returns hits best-first per query, so the
                    // first occurrence of an accession is its best HSP and rank 1 is
                    // the hit the search itself preferred. Re-sorting on a derived
                    // score (e.g. pident*qcovs) would promote a different accession
                    // than the search's top hit, which is what assemblies.
                    // blast_accession records.
                    def byacc = [:]
                    hits.each { v ->
                        def acc = v[0]
                        if (acc != null && acc != 'NO HIT' && !byacc.containsKey(acc)) {
                            byacc[acc] = v
                        }
                    }
                    byacc.values().toList().eachWithIndex { v, i ->
                        out << tuple(id, path as Integer, scaffold as Integer, i + 1,
                                     v[0], v[1], v[2], v[3], v[4], params.ts)
                    }
                }
                out
            }
            .set { scaffold_candidates }  // (id, path, scaffold, rank, acc, species, pident, qcovs, evalue, ts)

        // Per-path candidate + scaffold-accession summary (exactly one tuple per
        // blasted path, empty lists allowed) parsed from the same BLAST result.
        // Aggregated per-sample below with groupKey so ref fetch streams.
        blast_out.perpath
            .map { id, path_idx, result_file ->
                def opts_id = result_file.parent.name
                def lines = result_file.readLines().findAll { it.trim() }
                def per_scaffold = [:]
                lines.each { line ->
                    def parts = line.split('\t')
                    if (parts.size() >= 6) {
                        def rec = [
                            parts[1],
                            parts[2],
                            Math.round(parts[3].toFloat() * 100) / 100.0,
                            Math.round(parts[4].toFloat() * 100) / 100.0,
                            parts[5].toDouble()
                        ]
                        if (!per_scaffold.containsKey(parts[0])) per_scaffold[parts[0]] = []
                        per_scaffold[parts[0]] << rec
                    }
                }
                // Candidate list: every distinct accession across all hits, keeping
                // its best pident*qcovs (matches the old per-path 'cand' rows).
                def byacc = [:]
                per_scaffold.values().each { hits ->
                    hits.each { v ->
                        def acc = v[0]
                        if (acc != null && acc != 'NO HIT') {
                            def score = (v[2] ?: 0) * (v[3] ?: 0)
                            if (!byacc.containsKey(acc) || score > byacc[acc].score) {
                                byacc[acc] = [rec: v, score: score]
                            }
                        }
                    }
                }
                def candList = byacc.values().collect {
                    [opts_id, it.rec[0], it.rec[1], it.rec[2], it.rec[3], it.rec[4]]
                }
                // Each scaffold's best-hit accession (for per-scaffold lineage fetch).
                def scaffAccs = per_scaffold.values()
                    .collect { it[0][0] }
                    .findAll { it != null && it != 'NO HIT' }
                    .unique()
                tuple(id, opts_id, candList, scaffAccs)
            }
            .set { perpath_ch }  // (id, opts_id, [candRec...], [scaffAcc...])

        // Write state=4 (BLAST done, ref fetch pending); BLAST_REF_FETCH writes
        // state=2 once the ref fetch completes. Redundant per-id updates are safe.
        blast_out.state
            .map { id, result_file -> tuple('4', id) }
            .sqlInsert(statement: params.sqlWriteAssembleSwitch, db: 'sqlite')

        // Connection/tool failures: IDs that entered BLAST but produced no output after
        // all retries. UPDATE guarded WHERE assemble_switch = 4 so it can't overwrite a
        // terminal state=3 row.
        blast_in_split.ids
            .join(blast_out.succeeded, remainder: true)
            .filter { id, blast_flag, success_flag -> success_flag == null }
            .map { id, blast_flag, success_flag -> tuple(id) }
            .sqlInsert(statement: params.sqlWriteBlastNoOutput, db: 'sqlite')

        // Parse each per-path BLAST result. blast outfmt is
        //   qseqid saccver stitle pident qcovs evalue
        // qseqid encodes scaffold identity as "{id}.{path}.{scaffold}". Emits:
        //   'scaffold' rows  -> one per qualifying scaffold (incl. no-hit), assemblies table
        //   'path'     rows  -> per-path best hit, assembly_blast table
        //   'reffetch_*' rows-> per-path deduped accessions for BLAST_REF_FETCH
        blast_out.parse
            .flatMap{ id, path_idx, result_file ->
                def opts_id = result_file.parent.name
                def lines = result_file.readLines().findAll{ it.trim() }
                // Collect ALL queried scaffolds from this path's target FASTA so
                // no-hit scaffolds still get a row written to the assemblies table.
                def targetFasta = new File("${workflow.workDir}/blast_select_targets/${id}.${path_idx}.blast_target.fasta")
                def queried = []
                if (targetFasta.exists()) {
                    targetFasta.eachLine { line ->
                        if (line.startsWith('>')) {
                            queried << line.substring(1).split(/\s+/)[0]
                        }
                    }
                }
                // qseqid -> ordered list of hits [accession, species, pident, qcovs, evalue]
                // (blastn outfmt 6 returns hits best-first per query). With
                // -max_target_seqs > 1 each scaffold can return multiple hits; the
                // extra hits widen the per-sample candidate-reference pool.
                def per_scaffold = [:]
                lines.each { line ->
                    def parts = line.split('\t')
                    if (parts.size() >= 6) {
                        def rec = [
                            parts[1],
                            parts[2],
                            Math.round(parts[3].toFloat() * 100) / 100.0,
                            Math.round(parts[4].toFloat() * 100) / 100.0,
                            parts[5].toDouble()
                        ]
                        if (!per_scaffold.containsKey(parts[0])) per_scaffold[parts[0]] = []
                        per_scaffold[parts[0]] << rec
                    }
                }
                def out = []
                // Per-scaffold rows (assemblies table): each scaffold's BEST hit only
                queried.each { qseqid ->
                    def hits = per_scaffold[qseqid]
                    def hit  = hits ? hits[0] : null
                    def toks = qseqid.split(/\./)
                    if (toks.size() < 3) return
                    def path     = toks[-2]
                    def scaffold = toks[-1]
                    if (hit) {
                        out << tuple('scaffold', id, opts_id, path, scaffold, hit[0], hit[1], hit[2], hit[3], hit[4])
                    } else {
                        out << tuple('scaffold', id, opts_id, path, scaffold, 'NO HIT', null, null, null, null)
                    }
                }
                // Per-path best hit (assembly_blast): best hit per scaffold, then
                // lowest evalue, tie-broken by highest pident
                def allBest = per_scaffold.values().collect { it[0] }
                def top = allBest
                    .sort { a, b -> (a[4] <=> b[4]) ?: -(a[2] <=> b[2]) }
                    .find { true }
                if (top) {
                    out << tuple('path', id, opts_id, path_idx, null, top[0], top[1], top[2], top[3], top[4])
                } else {
                    out << tuple('path', id, opts_id, path_idx, null, 'NO HIT', null, null, null, null)
                }
                // Per-path candidate rows: every distinct accession in this path
                // (across ALL hits, not just the best per scaffold), keeping its best
                // pident*qcovs. Aggregated per-sample downstream into the top-N
                // candidate reference list. scaffold='cand' marker uses arity 10.
                def byacc = [:]
                per_scaffold.values().each { hits ->
                    hits.each { v ->
                        def acc = v[0]
                        if (acc != null && acc != 'NO HIT') {
                            def score = (v[2] ?: 0) * (v[3] ?: 0)
                            if (!byacc.containsKey(acc) || score > byacc[acc].score) {
                                byacc[acc] = [rec: v, score: score]
                            }
                        }
                    }
                }
                byacc.each { acc, info ->
                    def v = info.rec
                    out << tuple('cand', id, opts_id, path_idx, null, acc, v[1], v[2], v[3], v[4])
                }
                return out
            }
            .set { blast_records }

        // Genuine no-hit: every path for this ID returned 'NO HIT' (blast ran cleanly
        // but found nothing significant). Distinct from the no-output connection failure
        // above. Guarded WHERE assemble_switch = 4. NOTE: an ID with a mix of no-hit
        // paths and connection-failed paths is treated as no-hit here (connection-failed
        // paths emit no 'path' row); such mixed cases are rare and are flagged either way.
        blast_records
            .filter { kind, id, opts_id, path, scaffold, accession, species, pident, qcovs, evalue -> kind == 'path' }
            .map    { kind, id, opts_id, path, scaffold, accession, species, pident, qcovs, evalue -> tuple(id, accession) }
            .groupTuple()
            .filter { id, accessions -> accessions.every { it == 'NO HIT' } }
            .map    { id, accessions -> tuple(id) }
            .sqlInsert(statement: params.sqlWriteBlastNoHit, db: 'sqlite')

        // Per-scaffold rows: update assemblies table
        blast_records
            .filter { kind, id, opts_id, path, scaffold, accession, species, pident, qcovs, evalue -> kind == 'scaffold' }
            .map { kind, id, opts_id, path, scaffold, accession, species, pident, qcovs, evalue ->
                tuple(accession, species, pident, qcovs, evalue, id, path as Integer, scaffold as Integer)
            }
            .sqlInsert(statement: params.sqlWriteBlastHitScaffold, db: 'sqlite')

        // Per-path rows: insert into assembly_blast (one row per path)
        blast_records
            .filter { kind, id, opts_id, path, scaffold, accession, species, pident, qcovs, evalue -> kind == 'path' }
            .map { kind, id, opts_id, path_idx, scaffold, accession, species, pident, qcovs, evalue ->
                tuple(id, path_idx, opts_id, accession, species, pident, qcovs, evalue, params.ts)
            }
            .sqlInsert(statement: params.sqlWriteAssemblyBlast, db: 'sqlite')

        // Per-scaffold (id, path, scaffold) -> accession map for real hits. Lets
        // BLAST_REF_FETCH write each scaffold its own ref lineage keyed on the same
        // (ID, path, scaffold) tuple used for the accession write above, without
        // depending on the deferred commit of assemblies.blast_accession.
        blast_records
            .filter { kind, id, opts_id, path, scaffold, accession, species, pident, qcovs, evalue ->
                kind == 'scaffold' && accession != 'NO HIT' && accession != null
            }
            .map { kind, id, opts_id, path, scaffold, accession, species, pident, qcovs, evalue ->
                tuple(id, path as Integer, scaffold as Integer, accession)
            }
            .set { scaffold_accession }

        // Per-sample aggregation of the per-path summaries. groupKey(id, n_paths)
        // makes groupTuple emit a sample as soon as all *its* paths finish BLAST
        // (streaming) instead of waiting for the whole cohort. Candidates are
        // deduped by accession (best pident*qcovs), ranked, and capped at the
        // per-sample blast_opts.max_target_seqs (default 5, joined from mts_ch).
        perpath_ch
            .combine(path_count_ch, by: 0)
            .map { id, opts_id, candList, scaffAccs, n_paths ->
                tuple(groupKey(id, n_paths), [id, opts_id, candList, scaffAccs]) }
            .groupTuple()
            .map { gk, items ->
                def id = items[0][0]
                def opts_id = items[0][1]
                def allCands = items.collectMany { it[2] ?: [] }
                def allScaffs = items.collectMany { it[3] ?: [] }.unique()
                tuple(id, opts_id, allCands, allScaffs)
            }
            .join(mts_ch, by: 0)
            .map { id, opts_id, allCands, allScaffs, mts ->
                def n_cand = (mts ?: 5) as Integer
                allCands = allCands ?: []
                // Keep each accession's best hit, ranked the way BLAST ranks:
                // lowest evalue first, ties broken by higher pident (the same
                // ordering the per-path top hit uses above). A derived pident*qcovs
                // score would make rank 1 an accession the search itself did not
                // prefer, disagreeing with assemblies.blast_accession (= blastn's
                // first row for that query).
                def better = { a, b -> (a[5] <=> b[5]) ?: -(a[3] <=> b[3]) }
                def byacc = [:]
                allCands.each { r ->
                    def acc = r[1]
                    if (acc != null && acc != 'NO HIT' &&
                        (!byacc.containsKey(acc) || better(r, byacc[acc]) < 0)) {
                        byacc[acc] = r
                    }
                }
                // All candidate accessions for this sample (union across paths).
                // NOT capped: every per-scaffold candidate must get a fetched
                // reference (+lineage), and rank-1 is the sample's representative
                // hit. Per-scaffold separation lives in the blast_ref_candidates
                // table (written from scaffold_candidates).
                def ranked = byacc.values().toList().sort(false, better)
                tuple(id, opts_id, ranked, (allScaffs ?: []).unique())
            }
            .set { sample_agg }  // (id, opts_id, ranked, [scaffAcc...])

        // Candidate list for the SQL writes (samples with at least one candidate).
        sample_agg
            .filter { id, opts_id, ranked, scaffs -> ranked != null && ranked.size() > 0 }
            .map { id, opts_id, ranked, scaffs -> tuple(id, opts_id, ranked) }
            .set { candidates_ch }  // (id, opts_id, [ [opts_id, acc, species, pident, qcovs, evalue], ... ])

        // Representative hit for the assemble table = the rank-1 candidate. Derived
        // from the same ranking as the candidate list and is_top below, so
        // assemble.blast_accession, the fetched top hit, and the picker default all
        // agree on one accession.
        candidates_ch
            .map { id, opts_id, ranked ->
                def r = ranked ? ranked[0] : null
                // acc bound twice: blast_accession and blast_accession_auto (the
                // remembered rank-1 top hit, used to flag user overrides).
                r ? tuple(r[1], r[1], r[2], r[3], r[4], r[5], id) : null
            }
            .filter { it != null }
            .sqlInsert(statement: params.sqlWriteBlastHit, db: 'sqlite')

        // Clear stale candidate rows before re-inserting (time-stamp gated, mirrors
        // the assembly_blast delete pattern).
        normalized_input
            .map { id, asmb_list, opts_id -> tuple(id, params.ts) }
            .sqlInsert(statement: params.sqlDeleteCandidates, db: 'sqlite')

        // Per-scaffold candidate rows (kept separate per ID/path/scaffold).
        scaffold_candidates
            .sqlInsert(statement: params.sqlWriteCandidate, db: 'sqlite')

        // BLAST_REF_FETCH input: fetch the union of (a) the top-N candidate accessions
        // and (b) every scaffold's best-hit accession (needed for per-scaffold lineage
        // even when that hit ranks below N). is_top marks the per-sample rank-1 hit.
        // Streams per-sample from sample_agg (no cohort-wide groupTuple).
        sample_agg
            .flatMap { id, opts_id, ranked, scaffaccs ->
                ranked = ranked ?: []
                scaffaccs = (scaffaccs ?: []).unique()
                def topAcc = ranked ? ranked[0][1] : null
                def meta = [:]
                ranked.each { r -> meta[r[1]] = [species: r[2], evalue: r[5]] }
                def out = []
                (ranked.collect { it[1] } + scaffaccs).unique().each { acc ->
                    if (acc != null && acc != 'NO HIT') {
                        def m = meta[acc] ?: [species: null, evalue: null]
                        out << tuple(id, acc, m.species, m.evalue, opts_id, acc == topAcc)
                    }
                }
                out
            }
            .set { ref_fetch_input }

        // Per-sample accession BATCH for BLAST_REF_FETCH: this sample's unique real
        // hit accessions (top-N candidates + per-scaffold best hits). Streams per
        // sample (from sample_agg's groupKey) so each sample's reference fetch is
        // batched on its own and starts / re-runs independently of other samples.
        sample_agg
            .map { id, opts_id, ranked, scaffaccs ->
                def accs = ((ranked ?: []).collect { it[1] } + (scaffaccs ?: []))
                    .findAll { it != null && it != 'NO HIT' }.unique()
                tuple(id, opts_id, accs)
            }
            .filter { id, opts_id, accs -> accs.size() > 0 }
            .set { ref_fetch_batches }

        // Per-id list of "scaffold|accession|pident" for the assembled scaffolds,
        // passed to SCAFFOLD_JOIN so the reference choice / multi-ref mapping /
        // conflicting-hit check use reliably-available channel data rather than a
        // racy read of the per-scaffold assemblies.blast_accession (written above
        // by an async sqlInsert with no happens-before vs scaffold_join).
        blast_records
            .filter { kind, id, opts_id, path, scaffold, accession, species, pident, qcovs, evalue -> kind == 'scaffold' }
            .map { kind, id, opts_id, path, scaffold, accession, species, pident, qcovs, evalue ->
                tuple(id, "${scaffold}|${accession}|${pident == null ? '' : pident}")
            }
            .groupTuple()
            .map { id, items -> tuple(id, items.join(';')) }
            .set { scaffold_hits_ch }

    emit:
        // Downstream BLAST_REF_FETCH consumes this; filtered to real hits only.
        ref_input = ref_fetch_input
            .filter{ id, accession, species, evalue, opts_id, is_top -> accession != 'NO HIT' && accession != null }
        // Per-sample accession batch (id, opts_id, [accessions]) for the fetch step.
        ref_batches = ref_fetch_batches
        // (id, path, scaffold, accession) for per-scaffold lineage assignment.
        scaffold_map = scaffold_accession
        // Consumed by SCAFFOLD_JOIN (per-scaffold hits as a ';'-joined string).
        scaffold_hits = scaffold_hits_ch
}
