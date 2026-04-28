include {blast_genbank} from './blast_genbank.nf'

params.sqlWriteBlastHit = 'UPDATE assemble SET blast_accession = ?, blast_species = ?, blast_pident = ?, blast_qcovs = ? WHERE ID = ?'

workflow BLAST_GENBANK {
    take:
        // input: tuple(id, assembly_file_or_list, opts_id)
        // WF1:          from ASSEMBLE.out[0] mapped to (id, it[1], it[4])
        // WF1_userAsmb: from COVERAGE_userAsmb.out.blast_in as (id, assembly, opts_id)
        input

    main:
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
            .filter{ id, asmb, opts_id ->
                asmb.readLines().count{ it.startsWith('>') } == 1
            }
            .set { blast_in }

        blast_genbank(blast_in).set { blast_out }

        // Parse top hit, round numeric fields, and write to DB
        blast_out
            .map{ id, result_file ->
                def lines = result_file.readLines().findAll{ it.trim() }
                def blast_accession, blast_species, blast_pident, blast_qcovs
                if (lines) {
                    def parts = lines[0].split('\t')
                    if (parts.size() >= 4) {
                        blast_accession = parts[0]
                        blast_species   = parts[1]
                        blast_pident    = Math.round(parts[2].toFloat() * 100) / 100.0
                        blast_qcovs     = Math.round(parts[3].toFloat() * 100) / 100.0
                    } else {
                        blast_accession = 'NO HIT'
                        blast_species   = null
                        blast_pident    = null
                        blast_qcovs     = null
                    }
                } else {
                    blast_accession = 'NO HIT'
                    blast_species   = null
                    blast_pident    = null
                    blast_qcovs     = null
                }
                tuple(blast_accession, blast_species, blast_pident, blast_qcovs, id)
            }
            .sqlInsert(statement: params.sqlWriteBlastHit, db: 'sqlite')
}
