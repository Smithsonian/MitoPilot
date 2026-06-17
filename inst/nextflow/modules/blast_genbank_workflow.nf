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

params.blastNoHitMsg = 'BLAST returned no hits after all retries. Possible connection failure. Use -resume to retry.'

params.sqlWriteBlastHit = 'UPDATE assemble SET blast_accession = ?, blast_species = ?, blast_pident = ?, blast_qcovs = ?, blast_evalue = ? WHERE ID = ?'
// blast_lineage is NOT set here: ref fetch hasn't run yet so the subquery would
// resolve to NULL, and this deferred commit could clobber the lineage written
// later by BLAST_REF_FETCH. Lineage is handled solely in blast_ref_fetch_workflow.nf.
params.sqlWriteBlastHitScaffold = '''UPDATE assemblies
    SET blast_accession = ?, blast_species = ?, blast_pident = ?, blast_qcovs = ?, blast_evalue = ?
    WHERE assemblies.ID = ? AND path = ? AND scaffold = ?'''
params.sqlWriteAssembleSwitch = 'UPDATE assemble SET assemble_switch = ? WHERE ID = ? AND assemble_switch = 4'
params.sqlWriteBlastNoHit = "UPDATE assemble SET " +
    "assemble_switch = 3, " +
    "assemble_notes = ${appendTaggedNoteSql('[blast]', params.blastNoHitMsg)}, " +
    "poor_blast_ref = 'failed' " +
    "WHERE ID = ? AND assemble_switch = 4"
params.sqlWriteAssemblyBlast = 'INSERT OR REPLACE INTO assembly_blast (ID, path, blast_opts, blast_accession, blast_species, blast_pident, blast_qcovs, blast_evalue, time_stamp) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)'
params.sqlDeleteAssemblyBlast = 'DELETE FROM assembly_blast WHERE ID = ? AND time_stamp != ?'

params.sqlReadBlastOpts =
    'SELECT a.ID, b.run_blast, b.entrez_query, b.extra_opts ' +
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
        // Read per-sample BLAST opts from DB; filter to run_blast == 1
        channel.fromQuery(params.sqlReadBlastOpts, db: 'sqlite')
            .filter { row -> row[1] as Integer == 1 }
            .map { row -> tuple(row[0], row[2], row[3] ?: '') }
            .set { blast_opts_ch }  // (id, entrez_query, extra_opts)

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
        normalized_input
            .join(min_len_ch, by: 0)
            .flatMap { id, asmb_list, opts_id, min_len ->
                def realFiles = asmb_list.findAll { !(it.name =~ /assembly_0\.fasta$/) }
                def out = []
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
                    out << tuple(id, path_idx, targetFasta.toPath(), opts_id)
                }
                return out
            }
            // Join with blast opts; samples with run_blast = 0 have no entry and are dropped
            .combine(blast_opts_ch, by: 0)
            .map{ id, path_idx, asmb, opts_id, entrez_query, extra_opts ->
                tuple(id, path_idx, asmb, opts_id, entrez_query, extra_opts)
            }
            .multiMap { id, path_idx, asmb, opts_id, entrez_query, extra_opts ->
                process: tuple(id, path_idx, asmb, opts_id, entrez_query, extra_opts)
                ids:     tuple(id, true)
            }
            .set { blast_in_split }

        // Clear stale per-path rows before re-inserting. Time-stamp gating mirrors
        // the assemblies-table delete pattern so this is safe regardless of channel
        // emission order vs the insert below.
        normalized_input
            .map { id, asmb_list, opts_id -> tuple(id, params.ts) }
            .sqlInsert(statement: params.sqlDeleteAssemblyBlast, db: 'sqlite')

        blast_genbank(blast_in_split.process)
            .multiMap { id, path_idx, result_file ->
                state:     tuple(id, result_file)
                parse:     tuple(id, path_idx, result_file)
                succeeded: tuple(id, true)
            }
            .set { blast_out }

        // Write state=4 (BLAST done, ref fetch pending); BLAST_REF_FETCH writes
        // state=2 once the ref fetch completes. Redundant per-id updates are safe.
        blast_out.state
            .map { id, result_file -> tuple('4', id) }
            .sqlInsert(statement: params.sqlWriteAssembleSwitch, db: 'sqlite')

        // NO HIT failures: IDs that entered BLAST but produced no output after all
        // retries. UPDATE guarded WHERE assemble_switch = 4 so it can't overwrite a
        // terminal state=3 row.
        blast_in_split.ids
            .join(blast_out.succeeded, remainder: true)
            .filter { id, blast_flag, success_flag -> success_flag == null }
            .map { id, blast_flag, success_flag -> tuple(id) }
            .sqlInsert(statement: params.sqlWriteBlastNoHit, db: 'sqlite')

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
                def per_scaffold = [:] // qseqid -> [accession, species, pident, qcovs, evalue]
                lines.each { line ->
                    def parts = line.split('\t')
                    if (parts.size() >= 6 && !per_scaffold.containsKey(parts[0])) {
                        per_scaffold[parts[0]] = [
                            parts[1],
                            parts[2],
                            Math.round(parts[3].toFloat() * 100) / 100.0,
                            Math.round(parts[4].toFloat() * 100) / 100.0,
                            parts[5].toDouble()
                        ]
                    }
                }
                def out = []
                // Per-scaffold rows (assemblies table)
                queried.each { qseqid ->
                    def hit = per_scaffold[qseqid]
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
                // Per-path best hit (assembly_blast): lowest evalue, tie-broken by highest pident
                def top = per_scaffold.values().toList()
                    .sort { a, b -> (a[4] <=> b[4]) ?: -(a[2] <=> b[2]) }
                    .find { true }
                if (top) {
                    out << tuple('path', id, opts_id, path_idx, null, top[0], top[1], top[2], top[3], top[4])
                } else {
                    out << tuple('path', id, opts_id, path_idx, null, 'NO HIT', null, null, null, null)
                }
                // Per-path deduped ref-fetch rows (one per unique accession in this path).
                // kind = 'reffetch_top' marks the accession matching the path top hit
                // (drives is_top downstream); 'reffetch_dup' marks the rest.
                def top_accession = top ? top[0] : null
                def seen = [:] as LinkedHashMap
                per_scaffold.values().each { v ->
                    def acc = v[0]
                    if (acc != null && acc != 'NO HIT' && !seen.containsKey(acc)) {
                        seen[acc] = [species: v[1], evalue: v[4]]
                    }
                }
                seen.each { acc, info ->
                    def kind_str = (acc == top_accession) ? 'reffetch_top' : 'reffetch_dup'
                    out << tuple(kind_str, id, opts_id, path_idx, null, acc, info.species, null, null, info.evalue)
                }
                return out
            }
            .set { blast_records }

        // Per-scaffold rows: update assemblies table
        blast_records
            .filter { kind, id, opts_id, path, scaffold, accession, species, pident, qcovs, evalue -> kind == 'scaffold' }
            .map { kind, id, opts_id, path, scaffold, accession, species, pident, qcovs, evalue ->
                tuple(accession, species, pident, qcovs, evalue, id, path as Integer, scaffold as Integer)
            }
            .sqlInsert(statement: params.sqlWriteBlastHitScaffold, db: 'sqlite')

        // Per-path rows: insert into assembly_blast + carry forward for id-level rollup
        blast_records
            .filter { kind, id, opts_id, path, scaffold, accession, species, pident, qcovs, evalue -> kind == 'path' }
            .multiMap { kind, id, opts_id, path_idx, scaffold, accession, species, pident, qcovs, evalue ->
                db_path: tuple(id, path_idx, opts_id, accession, species, pident, qcovs, evalue, params.ts)
                group:   tuple(id, [path_idx, opts_id, accession, species, pident, qcovs, evalue])
            }
            .set { blast_path }

        // Per-path insert into assembly_blast
        blast_path.db_path
            .sqlInsert(statement: params.sqlWriteAssemblyBlast, db: 'sqlite')

        // Group per-id to compute "representative" hit for the assemble table (best by
        // pident*qcovs across paths) and write a single row per id.
        blast_path.group
            .groupTuple()
            .map { id, rows ->
                def best = rows
                    .findAll { it[2] != 'NO HIT' && it[2] != null }
                    .max { (it[4] ?: 0) * (it[5] ?: 0) }
                if (best == null) best = rows[0]
                tuple(id, best[0], best[1], best[2], best[3], best[4], best[5], best[6])
            }
            .map { id, path_idx, opts_id, accession, species, pident, qcovs, evalue ->
                tuple(accession, species, pident, qcovs, evalue, id)
            }
            .set { blast_rep_assemble }

        // Representative hit into assemble table (one row per id)
        blast_rep_assemble
            .sqlInsert(statement: params.sqlWriteBlastHit, db: 'sqlite')

        // Per-path ref-fetch inputs already deduped inside the flatMap above. Filter to
        // the reffetch_* rows and map to the shape BLAST_REF_FETCH expects. No
        // groupTuple here, so items flow through as each path's BLAST_GENBANK completes.
        blast_records
            .filter { kind, id, opts_id, path, scaffold, accession, species, pident, qcovs, evalue ->
                kind == 'reffetch_top' || kind == 'reffetch_dup'
            }
            .map { kind, id, opts_id, path, scaffold, accession, species, pident, qcovs, evalue ->
                tuple(id, accession, species, evalue, opts_id, kind == 'reffetch_top')
            }
            .set { ref_fetch_input }

    emit:
        // Downstream BLAST_REF_FETCH consumes this; filtered to real hits only.
        ref_input = ref_fetch_input
            .filter{ id, accession, species, evalue, opts_id, is_top -> accession != 'NO HIT' && accession != null }
}
