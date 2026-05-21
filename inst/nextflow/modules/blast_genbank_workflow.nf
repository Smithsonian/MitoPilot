include {blast_genbank} from './blast_genbank.nf'

params.sqlWriteBlastHit = 'UPDATE assemble SET blast_accession = ?, blast_species = ?, blast_pident = ?, blast_qcovs = ?, blast_evalue = ? WHERE ID = ?'
params.sqlWriteBlastHitScaffold = 'UPDATE assemblies SET blast_accession = ?, blast_species = ?, blast_pident = ?, blast_qcovs = ?, blast_evalue = ? WHERE ID = ? AND path = ? AND scaffold = ?'
params.sqlWriteAssembleSwitch = 'UPDATE assemble SET assemble_switch = ? WHERE ID = ? AND assemble_switch = 4'
params.sqlWriteBlastNoHit = "UPDATE assemble SET assemble_switch = 3, assemble_notes = ?, poor_blast_ref = 'failed' WHERE ID = ?"

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

        normalized_input
            // Join with per-sample min_assembly_length, then collect every qualifying scaffold
            // (length >= min_assembly_length) across all assembly paths into a single
            // multi-FASTA for BLAST input. Header is preserved so qseqid (the first
            // whitespace-delimited token, e.g. "{id}.{path}.{scaffold}") can be parsed
            // back to (path, scaffold) downstream.
            .join(min_len_ch, by: 0)
            .map { id, asmb_list, opts_id, min_len ->
                def qualifying = []
                def realFiles = asmb_list.findAll { !(it.name =~ /assembly_0\.fasta$/) }
                for (def f : realFiles) {
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
                }
                if (qualifying.size() < 1) return null
                def targetDirStr = "${workflow.workDir}/blast_select_targets"
                new File(targetDirStr).mkdirs()
                def targetFasta = new File("${targetDirStr}/${id}.blast_target.fasta")
                targetFasta.text = qualifying.collect { "${it.header}\n${it.seq}" }.join('\n') + '\n'
                tuple(id, targetFasta.toPath(), opts_id)
            }
            .filter { it != null }
            // Join with blast opts; samples with run_blast = 0 have no entry and are dropped
            .join(blast_opts_ch, by: 0)
            .map{ id, asmb, opts_id, entrez_query, extra_opts ->
                tuple(id, asmb, opts_id, entrez_query, extra_opts)
            }
            // Split so we can track which IDs ran BLAST without consuming the channel
            .multiMap { id, asmb, opts_id, entrez_query, extra_opts ->
                process: tuple(id, asmb, opts_id, entrez_query, extra_opts)
                ids:     tuple(id, true)
            }
            .set { blast_in_split }

        blast_genbank(blast_in_split.process)
            .multiMap { id, result_file ->
                state:     tuple(id, result_file)
                parse:     tuple(id, result_file)
                succeeded: tuple(id, true)
            }
            .set { blast_out }

        // Write state=4 (BLAST done, ref fetch pending) for samples that ran BLAST successfully;
        // state=2 is written by BLAST_REF_FETCH once the reference fetch also completes
        blast_out.state
            .map { id, result_file -> tuple('4', id) }
            .sqlInsert(statement: params.sqlWriteAssembleSwitch, db: 'sqlite')

        // Detect NO HIT failures: IDs that entered BLAST but produced no output after all retries
        blast_in_split.ids
            .join(blast_out.succeeded, remainder: true)
            .filter { id, blast_flag, success_flag -> success_flag == null }
            .map { id, blast_flag, success_flag ->
                tuple('BLAST returned no hits after all retries. Possible connection failure. Use -resume to retry.', id)
            }
            .sqlInsert(statement: params.sqlWriteBlastNoHit, db: 'sqlite')

        // Write state=2 for samples that were filtered out before BLAST
        // (no single qualifying scaffold >= min_assembly_length across all paths, or run_blast = 0)
        all_ids
            .join(blast_in_split.ids, remainder: true)
            .filter { id, all_flag, blast_flag -> blast_flag == null }
            .map { id, all_flag, blast_flag -> tuple('2', id) }
            .sqlInsert(statement: params.sqlWriteAssembleSwitch, db: 'sqlite')

        // Parse all hits, round numeric fields. blast outfmt is now
        //   qseqid saccver stitle pident qcovs evalue
        // qseqid encodes scaffold identity as "{id}.{path}.{scaffold}".
        // Emits one record per qualifying scaffold (including no-hit scaffolds derived
        // from the BLAST query FASTA) plus a single "top hit" record per ID.
        blast_out.parse
            .flatMap{ id, result_file ->
                def opts_id = result_file.parent.name
                def lines = result_file.readLines().findAll{ it.trim() }
                // Collect ALL queried scaffolds from the target FASTA so no-hit
                // scaffolds still get a row written to the assemblies table.
                def targetFasta = new File("${workflow.workDir}/blast_select_targets/${id}.blast_target.fasta")
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
                // Pick top hit for ID-level row: lowest evalue, tie-broken by highest pident
                def top = per_scaffold.values().toList()
                    .sort { a, b -> (a[4] <=> b[4]) ?: -(a[2] <=> b[2]) }
                    .find { true }
                if (top) {
                    out << tuple('id', id, opts_id, null, null, top[0], top[1], top[2], top[3], top[4])
                } else {
                    out << tuple('id', id, opts_id, null, null, 'NO HIT', null, null, null, null)
                }
                // Per-id deduped ref-fetch rows (one per unique accession across all
                // scaffolds of this sample). Emitting here instead of via a downstream
                // groupTuple avoids a global channel-close barrier — BLAST_REF_FETCH
                // can start for sample N as soon as BLAST_GENBANK for N finishes,
                // rather than waiting for every sample's BLAST_GENBANK to complete.
                // kind = 'reffetch_top' marks the accession matching the id-level top
                // hit (drives is_top downstream); 'reffetch_dup' marks the rest.
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
                    out << tuple(kind_str, id, opts_id, null, null, acc, info.species, null, null, info.evalue)
                }
                return out
            }
            .set { blast_records }

        // ID-level row: update assemble table + feed ref_fetch
        blast_records
            .filter { kind, id, opts_id, path, scaffold, accession, species, pident, qcovs, evalue -> kind == 'id' }
            .multiMap { kind, id, opts_id, path, scaffold, accession, species, pident, qcovs, evalue ->
                db_insert:  tuple(accession, species, pident, qcovs, evalue, id)
                ref_input:  tuple(id, accession, species, evalue, opts_id)
            }
            .set { blast_parsed }

        blast_parsed.db_insert
            .sqlInsert(statement: params.sqlWriteBlastHit, db: 'sqlite')

        // Per-scaffold rows: update assemblies table
        blast_records
            .filter { kind, id, opts_id, path, scaffold, accession, species, pident, qcovs, evalue -> kind == 'scaffold' }
            .map { kind, id, opts_id, path, scaffold, accession, species, pident, qcovs, evalue ->
                tuple(accession, species, pident, qcovs, evalue, id, path as Integer, scaffold as Integer)
            }
            .sqlInsert(statement: params.sqlWriteBlastHitScaffold, db: 'sqlite')

        // Per-id ref-fetch inputs already deduped inside the flatMap above.
        // Filter to just the reffetch_* rows and map to the shape BLAST_REF_FETCH
        // expects. No groupTuple here, so items flow through as each sample's
        // BLAST_GENBANK task completes.
        blast_records
            .filter { kind, id, opts_id, path, scaffold, accession, species, pident, qcovs, evalue ->
                kind == 'reffetch_top' || kind == 'reffetch_dup'
            }
            .map { kind, id, opts_id, path, scaffold, accession, species, pident, qcovs, evalue ->
                tuple(id, accession, species, evalue, opts_id, kind == 'reffetch_top')
            }
            .set { ref_fetch_input }

    emit:
        // Downstream BLAST_REF_FETCH consumes this; one entry per unique (id, accession)
        ref_input = ref_fetch_input
}
