include {blast_genbank} from './blast_genbank.nf'

params.sqlWriteBlastHit = 'UPDATE assemble SET blast_accession = ?, blast_species = ?, blast_pident = ?, blast_qcovs = ?, blast_evalue = ? WHERE ID = ?'
params.sqlWriteBlastHitScaffold = 'UPDATE assemblies SET blast_accession = ?, blast_species = ?, blast_pident = ?, blast_qcovs = ?, blast_evalue = ? WHERE ID = ? AND path = ? AND scaffold = ?'
params.sqlWriteAssembleSwitch = 'UPDATE assemble SET assemble_switch = ? WHERE ID = ? AND assemble_switch = 4'
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
        // WF1:          from ASSEMBLE.out[0] mapped to (id, it[1], it[4])
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

        // Track all IDs entering this workflow so we can mark skipped samples as complete
        normalized_input
            .map { id, asmb_list, opts_id -> tuple(id, true) }
            .set { all_ids }

        // Read per-sample min_assembly_length from DB
        channel.fromQuery(params.sqlReadMinLen, db: 'sqlite')
            .map { row -> tuple(row[0], row[1] == null ? 500 : (row[1] as Integer)) }
            .set { min_len_ch }  // (id, min_assembly_length)

        // Per-path stream: filter qualifying scaffolds within each path FASTA by
        // min_assembly_length, write a per-path multi-FASTA, and emit one task per
        // (id, path_idx) for BLAST. Headers are preserved so qseqid
        // ("{id}.{path}.{scaffold}") can be parsed back downstream.
        normalized_input
            .join(min_len_ch, by: 0)
            .flatMap { id, asmb_list, opts_id, min_len ->
                def realFiles = asmb_list.findAll { !(it.name =~ /assembly_0\.fasta$/) }
                def emitted = []
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
                    def targetFasta = new File("${targetDirStr}/${id}.path${path_idx}.blast_target.fasta")
                    targetFasta.text = qualifying.collect { "${it.header}\n${it.seq}" }.join('\n') + '\n'
                    emitted << tuple(id, path_idx, targetFasta.toPath(), opts_id)
                }
                return emitted
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
                state:  tuple(id, result_file)
                parse:  tuple(id, path_idx, result_file)
            }
            .set { blast_out }

        // Write state=2 (WF1 complete) for each (id, path) result; redundant updates
        // on the same id are safe (the WHERE clause is keyed on assemble_switch = 4)
        blast_out.state
            .map { id, result_file -> tuple('2', id) }
            .sqlInsert(statement: params.sqlWriteAssembleSwitch, db: 'sqlite')

        // Write state=2 for samples that were filtered out before BLAST
        // (no qualifying scaffold >= min_assembly_length in any path, or run_blast = 0)
        all_ids
            .join(blast_in_split.ids, remainder: true)
            .filter { id, all_flag, blast_flag -> blast_flag == null }
            .map { id, all_flag, blast_flag -> tuple('2', id) }
            .sqlInsert(statement: params.sqlWriteAssembleSwitch, db: 'sqlite')

        // Parse each per-path result file. blast outfmt is
        //   qseqid saccver stitle pident qcovs evalue
        // qseqid encodes scaffold identity as "{id}.{path}.{scaffold}".
        // Emits:
        //   kind='scaffold' rows per qualifying scaffold (including no-hit) for the assemblies table
        //   kind='path' row per (id, path_idx) holding the path's top hit for the assembly_blast table
        blast_out.parse
            .flatMap{ id, path_idx, result_file ->
                def opts_id = result_file.parent.name
                def lines = result_file.readLines().findAll{ it.trim() }
                def targetFasta = new File("${workflow.workDir}/blast_select_targets/${id}.path${path_idx}.blast_target.fasta")
                def queried = []
                if (targetFasta.exists()) {
                    targetFasta.eachLine { line ->
                        if (line.startsWith('>')) {
                            def tok = line.substring(1).split(/\s+/)[0]
                            queried << tok
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
                // Path-level top hit: lowest evalue, tie-broken by highest pident
                def top = per_scaffold.values().toList()
                    .sort { a, b -> (a[4] <=> b[4]) ?: -(a[2] <=> b[2]) }
                    .find { true }
                if (top) {
                    out << tuple('path', id, opts_id, path_idx, null, top[0], top[1], top[2], top[3], top[4])
                } else {
                    out << tuple('path', id, opts_id, path_idx, null, 'NO HIT', null, null, null, null)
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

        // Per-path rows: insert into assembly_blast (path_idx stored in 'path' slot)
        blast_records
            .filter { kind, id, opts_id, path_idx, scaffold, accession, species, pident, qcovs, evalue -> kind == 'path' }
            .map { kind, id, opts_id, path_idx, scaffold, accession, species, pident, qcovs, evalue ->
                tuple(id, path_idx, opts_id, accession, species, pident, qcovs, evalue, params.ts)
            }
            .sqlInsert(statement: params.sqlWriteAssemblyBlast, db: 'sqlite')

        // Pick id-level representative across all paths (best pident*qcovs) for the
        // assemble table and as the "top" marker for ref-fetch dedup.
        blast_records
            .filter { kind, id, opts_id, path_idx, scaffold, accession, species, pident, qcovs, evalue -> kind == 'path' }
            .map { kind, id, opts_id, path_idx, scaffold, accession, species, pident, qcovs, evalue ->
                tuple(id, [opts_id, accession, species, pident, qcovs, evalue])
            }
            .groupTuple()
            .map { id, rows ->
                def real = rows.findAll { it[1] != 'NO HIT' && it[1] != null }
                def best = real ? real.max { (it[3] ?: 0) * (it[4] ?: 0) } : rows[0]
                tuple(id, best[0], best[1], best[2], best[3], best[4], best[5])
            }
            .multiMap { id, opts_id, accession, species, pident, qcovs, evalue ->
                db_assemble: tuple(accession, species, pident, qcovs, evalue, id)
                ref_input:   tuple(id, accession, species, evalue, opts_id)
            }
            .set { blast_rep }

        // Representative hit into assemble table (one row per id)
        blast_rep.db_assemble
            .sqlInsert(statement: params.sqlWriteBlastHit, db: 'sqlite')

        // Per-id deduped ref-fetch rows: one entry per unique accession across all
        // paths of this sample, with is_top set on the row whose accession matches
        // the id-level top hit. Per-id groupTuple here doesn't impose a global
        // barrier — each sample's ref fetch can start as its BLAST_GENBANK tasks
        // for that id finish.
        blast_records
            .filter { kind, id, opts_id, path_idx, scaffold, accession, species, pident, qcovs, evalue -> kind == 'path' }
            .map { kind, id, opts_id, path_idx, scaffold, accession, species, pident, qcovs, evalue ->
                tuple(id, [opts_id, accession, species, pident, qcovs, evalue])
            }
            .groupTuple()
            .flatMap { id, rows ->
                def real = rows.findAll { it[1] != 'NO HIT' && it[1] != null }
                if (real.isEmpty()) return []
                def top = real.max { (it[3] ?: 0) * (it[4] ?: 0) }
                def top_accession = top[1]
                def opts_id = rows[0][0]
                def seen = [:] as LinkedHashMap
                def emitted = []
                real.each { r ->
                    def acc = r[1]
                    if (!seen.containsKey(acc)) {
                        seen[acc] = true
                        emitted << tuple(id, acc, r[2], r[5], opts_id, acc == top_accession)
                    }
                }
                return emitted
            }
            .set { ref_fetch_input }

    emit:
        // Downstream BLAST_REF_FETCH consumes this; one entry per unique (id, accession)
        ref_input = ref_fetch_input
}
