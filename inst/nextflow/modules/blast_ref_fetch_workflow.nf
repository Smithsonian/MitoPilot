include {blast_ref_fetch} from './blast_ref_fetch.nf'

params.sqlWriteBlastRef = '''INSERT OR REPLACE INTO blast_ref_annotations
    (ID, gene, type, pos1, pos2, direction, ref_length, time_stamp)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?)'''

workflow BLAST_REF_FETCH {
    take:
        // input: tuple(id, blast_accession, opts_id)
        input

    main:
        blast_ref_fetch(input).set { ref_out }

        // Parse CSV rows and insert one row per gene into the DB
        ref_out
            .flatMap { id, csv_file ->
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
                                parts[0],       // gene
                                parts[1],       // type
                                parts[2].toLong(), // pos1
                                parts[3].toLong(), // pos2
                                parts[4],       // direction
                                parts[5].toLong(), // ref_length
                                ts              // time_stamp
                            )
                        }
                    }
                }
                rows
            }
            .sqlInsert(statement: params.sqlWriteBlastRef, db: 'sqlite')
}
