import groovy.json.JsonSlurper

include {blast_ref_fetch} from './blast_ref_fetch.nf'

params.sqlWriteBlastLineage = 'UPDATE assemble SET blast_lineage = ? WHERE ID = ?'

params.sqlWriteBlastRef = '''INSERT OR REPLACE INTO blast_ref_annotations
    (ID, gene, type, pos1, pos2, direction, ref_length, time_stamp)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?)'''

params.sqlWriteRefSeq = '''INSERT OR REPLACE INTO blast_ref_sequences
    (accession, sequence, ref_length, genetic_code, time_stamp)
    VALUES (?, ?, ?, ?, ?)'''

workflow BLAST_REF_FETCH {
    take:
        // input: tuple(id, blast_accession, blast_species, blast_evalue, opts_id)
        input

    main:
        blast_ref_fetch(input).set { ref_out }

        // Parse CSV rows and insert one row per gene into the DB
        ref_out
            .flatMap { id, accession, csv_file, seq_file, gc_file, json_file ->
                def rows = []
                def lines = csv_file.readLines()
                if (lines.size() <= 1) return rows   // empty or header-only
                def ts = java.time.Instant.now().getEpochSecond()
                lines.drop(1).each { line ->
                    if (line.trim()) {
                        // CSV columns: gene,type,pos1,pos2,direction,ref_length
                        // Strip surrounding quotes written by R's write.csv
                        def parts = line.split(',').collect { it.replaceAll('^"|"$', '') }
                        if (parts.size() >= 6 && parts[2].isLong() && parts[3].isLong() && parts[5].isLong()) {
                            rows << tuple(
                                id,
                                parts[0],              // gene
                                parts[1],              // type
                                parts[2].toLong(),     // pos1
                                parts[3].toLong(),     // pos2
                                parts[4],              // direction
                                parts[5].toLong(),     // ref_length
                                ts                     // time_stamp
                            )
                        }
                    }
                }
                rows
            }
            .sqlInsert(statement: params.sqlWriteBlastRef, db: 'sqlite')

        // Store reference nucleotide sequence (one row per accession)
        ref_out
            .map { id, accession, csv_file, seq_file, gc_file, json_file ->
                def seq = seq_file.text.trim()
                if (!seq) return null
                def gc_str = gc_file.text.trim()
                def gc = gc_str.isInteger() ? gc_str.toInteger() : 2
                def ts = java.time.Instant.now().getEpochSecond()
                tuple(accession, seq, seq.length() as Long, gc, ts)
            }
            .filter { it != null }
            .unique { it[0] }  // deduplicate by accession in case of reruns
            .sqlInsert(statement: params.sqlWriteRefSeq, db: 'sqlite')

        // Write lineage to assemble table
        ref_out
            .map { id, accession, csv_file, seq_file, gc_file, json_file ->
                def json = new JsonSlurper().parseText(json_file.text)
                def lineage = json?.lineage ?: null
                lineage ? tuple(lineage, id) : null
            }
            .filter { it != null }
            .sqlInsert(statement: params.sqlWriteBlastLineage, db: 'sqlite')
}
