include {blast_genbank} from './blast_genbank.nf'

params.sqlWriteBlastHit = 'UPDATE assemble SET blast_accession = ?, blast_species = ?, blast_pident = ?, blast_qcovs = ?, blast_evalue = ? WHERE ID = ?'
params.sqlWriteAssembleSwitch = 'UPDATE assemble SET assemble_switch = ? WHERE ID = ? AND assemble_switch = 4'
params.sqlWriteAssemblyBlast = 'INSERT OR REPLACE INTO assembly_blast (ID, path, blast_opts, blast_accession, blast_species, blast_pident, blast_qcovs, blast_evalue, time_stamp) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)'
params.sqlDeleteAssemblyBlast = 'DELETE FROM assembly_blast WHERE ID = ? AND time_stamp != ?'

params.sqlReadBlastOpts =
    'SELECT a.ID, b.run_blast, b.entrez_query, b.extra_opts ' +
    'FROM assemble a ' +
    'JOIN blast_opts b ON a.blast_opts = b.blast_opts'

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

        // Per-path stream: flatten to one task per (id, path_idx, fasta).
        // Skip failed empty assemblies (assembly_0.fasta) and join with BLAST opts;
        // samples with run_blast == 0 have no entry in blast_opts_ch and are dropped here.
        normalized_input
            .flatMap{ id, asmb_list, opts_id ->
                asmb_list
                    .findAll { f -> !(f.name =~ /assembly_0\.fasta$/) }
                    .collect { f ->
                        def m = (f.name =~ /assembly_(\d+)\.fasta$/)
                        def path_idx = m ? (m[0][1] as Integer) : 0
                        tuple(id, path_idx, f, opts_id)
                    }
            }
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

        // Write state=2 for samples that never entered BLAST (run_blast = 0)
        all_ids
            .join(blast_in_split.ids, remainder: true)
            .filter { id, all_flag, blast_flag -> blast_flag == null }
            .map { id, all_flag, blast_flag -> tuple('2', id) }
            .sqlInsert(statement: params.sqlWriteAssembleSwitch, db: 'sqlite')

        // Parse top hit per (id, path). Carry opts_id from result file parent dir.
        blast_out.parse
            .map{ id, path_idx, result_file ->
                def opts_id = result_file.parent.name
                def lines = result_file.readLines().findAll{ it.trim() }
                def blast_accession, blast_species, blast_pident, blast_qcovs, blast_evalue
                if (lines) {
                    def parts = lines[0].split('\t')
                    if (parts.size() >= 5) {
                        blast_accession = parts[0]
                        blast_species   = parts[1]
                        blast_pident    = Math.round(parts[2].toFloat() * 100) / 100.0
                        blast_qcovs     = Math.round(parts[3].toFloat() * 100) / 100.0
                        blast_evalue    = parts[4].toDouble()
                    } else {
                        blast_accession = 'NO HIT'
                        blast_species   = null
                        blast_pident    = null
                        blast_qcovs     = null
                        blast_evalue    = null
                    }
                } else {
                    blast_accession = 'NO HIT'
                    blast_species   = null
                    blast_pident    = null
                    blast_qcovs     = null
                    blast_evalue    = null
                }
                tuple(id, path_idx, opts_id, blast_accession, blast_species, blast_pident, blast_qcovs, blast_evalue)
            }
            .set { blast_parsed }

        // Group per-id to compute "representative" hit for the assemble table (best
        // by pident*qcovs across paths) and to gate the single ref fetch
        blast_parsed
            .map { id, path_idx, opts_id, accession, species, pident, qcovs, evalue ->
                tuple(id, [path_idx, opts_id, accession, species, pident, qcovs, evalue])
            }
            .groupTuple()
            .map { id, rows ->
                def best = rows
                    .findAll { it[2] != 'NO HIT' && it[2] != null }
                    .max { (it[4] ?: 0) * (it[5] ?: 0) }
                if (best == null) best = rows[0]
                tuple(id, best[0], best[1], best[2], best[3], best[4], best[5], best[6])
            }
            .multiMap { id, path_idx, opts_id, accession, species, pident, qcovs, evalue ->
                db_assemble: tuple(accession, species, pident, qcovs, evalue, id)
                ref_input:   tuple(id, accession, species, evalue, opts_id)
            }
            .set { blast_rep }

        // Per-path insert into assembly_blast
        blast_parsed
            .map { id, path_idx, opts_id, accession, species, pident, qcovs, evalue ->
                tuple(id, path_idx, opts_id, accession, species, pident, qcovs, evalue, params.ts)
            }
            .sqlInsert(statement: params.sqlWriteAssemblyBlast, db: 'sqlite')

        // Representative hit into assemble table (one row per id)
        blast_rep.db_assemble
            .sqlInsert(statement: params.sqlWriteBlastHit, db: 'sqlite')

    emit:
        // Downstream BLAST_REF_FETCH consumes this; filtered to real hits only
        ref_input = blast_rep.ref_input
            .filter{ id, accession, species, evalue, opts_id -> accession != 'NO HIT' && accession != null }
}
