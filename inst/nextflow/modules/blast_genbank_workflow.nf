include {blast_genbank} from './blast_genbank.nf'

params.sqlWriteBlastHit = 'UPDATE assemble SET blast_accession = ?, blast_species = ?, blast_pident = ?, blast_qcovs = ?, blast_evalue = ? WHERE ID = ?'
params.sqlWriteAssembleSwitch = 'UPDATE assemble SET assemble_switch = ? WHERE ID = ? AND assemble_switch = 4'

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
            // Join with per-sample min_assembly_length, then find the single qualifying scaffold
            // across all assembly paths; write it to a temp FASTA for BLAST input
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
                    if (qualifying.size() > 1) break
                }
                if (qualifying.size() != 1) return null
                def targetDirStr = "${workflow.workDir}/blast_select_targets"
                new File(targetDirStr).mkdirs()
                def targetFasta = new File("${targetDirStr}/${id}.blast_target.fasta")
                targetFasta.text = "${qualifying[0].header}\n${qualifying[0].seq}\n"
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
                state:  tuple(id, result_file)
                parse:  tuple(id, result_file)
            }
            .set { blast_out }

        // Write state=2 (WF1 complete) for samples that ran BLAST
        blast_out.state
            .map { id, result_file -> tuple('2', id) }
            .sqlInsert(statement: params.sqlWriteAssembleSwitch, db: 'sqlite')

        // Write state=2 for samples that were filtered out before BLAST
        // (no single qualifying scaffold >= min_assembly_length across all paths, or run_blast = 0)
        all_ids
            .join(blast_in_split.ids, remainder: true)
            .filter { id, all_flag, blast_flag -> blast_flag == null }
            .map { id, all_flag, blast_flag -> tuple('2', id) }
            .sqlInsert(statement: params.sqlWriteAssembleSwitch, db: 'sqlite')

        // Parse top hit, round numeric fields; carry opts_id from result file path
        blast_out.parse
            .map{ id, result_file ->
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
                tuple(id, opts_id, blast_accession, blast_species, blast_pident, blast_qcovs, blast_evalue)
            }
            // Fan out: one branch for DB insert, one for reference fetch
            .multiMap { id, opts_id, accession, species, pident, qcovs, evalue ->
                db_insert:  tuple(accession, species, pident, qcovs, evalue, id)
                ref_input:  tuple(id, accession, species, evalue, opts_id)
            }
            .set { blast_parsed }

        blast_parsed.db_insert
            .sqlInsert(statement: params.sqlWriteBlastHit, db: 'sqlite')

    emit:
        // Downstream BLAST_REF_FETCH consumes this; filtered to real hits only
        ref_input = blast_parsed.ref_input
            .filter{ id, accession, species, evalue, opts_id -> accession != 'NO HIT' }
}
