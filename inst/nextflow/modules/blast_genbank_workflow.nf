include {blast_genbank} from './blast_genbank.nf'

params.sqlWriteBlastHit = 'UPDATE assemble SET blast_accession = ?, blast_species = ?, blast_pident = ?, blast_qcovs = ?, blast_evalue = ? WHERE ID = ?'

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
            // Keep only single-path assemblies (exactly 1 file, not a failed assembly)
            .filter{ id, asmb_list, opts_id ->
                asmb_list.size() == 1 &&
                !(asmb_list[0].name =~ /assembly_0\.fasta$/)
            }
            .map{ id, asmb_list, opts_id ->
                tuple(id, asmb_list[0], opts_id)
            }
            // Keep only single-scaffold assemblies (exactly 1 sequence in the FASTA)
            // Use early-exit reader to avoid loading large multi-contig files into master JVM heap
            .filter{ id, asmb, opts_id ->
                asmb.withReader { reader ->
                    int count = 0
                    String line
                    while ((line = reader.readLine()) != null) {
                        if (line.startsWith('>') && ++count > 1) return false
                    }
                    count == 1
                }
            }
            // Join with blast opts; samples with run_blast = 0 have no entry and are dropped
            .join(blast_opts_ch, by: 0)
            .map{ id, asmb, opts_id, entrez_query, extra_opts ->
                tuple(id, asmb, opts_id, entrez_query, extra_opts)
            }
            .set { blast_in }

        blast_genbank(blast_in).set { blast_out }

        // Parse top hit, round numeric fields; carry opts_id from result file path
        blast_out
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
