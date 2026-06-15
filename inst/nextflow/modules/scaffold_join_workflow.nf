include { scaffold_join } from './scaffold_join.nf'

// Upsert the joined Path 0 row; ignore the original scaffold rows; mirror the
// consensus metadata into the annotate row (as the app's persist_path0 does).
// The joined row inherits the BLAST hit of the reference used for the join (the
// per-ID top hit in `assemble`) via subqueries, but blanks %ident/%cov/evalue:
// those described a single scaffold and are meaningless for the joined assembly.
params.sqlUpsertJoined = '''INSERT OR REPLACE INTO assemblies
    (ID, path, scaffold, topology, length, length_raw, sequence, depth, gc, errors, ignore, edited, time_stamp,
     blast_accession, blast_species, blast_lineage, blast_pident, blast_qcovs, blast_evalue)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?,
     (SELECT blast_accession FROM assemble WHERE ID = ?),
     (SELECT blast_species FROM assemble WHERE ID = ?),
     (SELECT blast_lineage FROM assemble WHERE ID = ?),
     NULL, NULL, NULL)'''

params.sqlIgnoreOriginals = 'UPDATE assemblies SET ignore = 1 WHERE ID = ? AND path > 0'

params.sqlSyncAnnotateJoin = 'UPDATE annotate SET path = ?, scaffolds = ?, topology = ?, length = ?, time_stamp = ? WHERE ID = ?'

// Precomputed scaffold->reference mappings (one row per ID/scaffold/ref) so the
// in-app manual join editor needs no minimap2. Replace any stale rows first.
params.sqlClearMappings  = 'DELETE FROM scaffold_mappings WHERE ID = ?'
params.sqlInsertMappings = '''INSERT OR REPLACE INTO scaffold_mappings
    (ID, ref_accession, scaffold, ref_start, ref_end, strand, nmatch, qcov, qstart, mapped)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)'''

workflow SCAFFOLD_JOIN {
    take:
        // tuple(id, assembly_fasta, opts_id, auto_join, cov_csvs, ref_seq_file)
        input

    main:
        scaffold_join(input)

        // Always-present mappings: clear stale rows, then insert the fresh set.
        scaffold_join.out.mappings
            .map { id, csv -> tuple(id) }
            .sqlInsert(statement: params.sqlClearMappings, db: 'sqlite')

        scaffold_join.out.mappings
            .map { id, csv -> csv }
            .splitCsv(header: true)
            .map { r -> tuple(r.ID, r.ref_accession, r.scaffold, r.ref_start, r.ref_end,
                              r.strand, r.nmatch, r.qcov, r.qstart, r.mapped) }
            .sqlInsert(statement: params.sqlInsertMappings, db: 'sqlite')

        // Joined Path 0 row is emitted only when auto-join built it; the channel
        // is simply empty otherwise, so these branches are no-ops in that case.
        scaffold_join.out.row
            .map { id, row -> row }
            .splitCsv(header: true)
            .set { joined_rows }

        joined_rows
            .map { r -> tuple(r.ID, r.path, r.scaffold, r.topology, r.length, r.length_raw,
                              r.sequence, r.depth, r.gc, r.errors, r.ignore, r.edited, r.time_stamp,
                              r.ID, r.ID, r.ID) }
            .sqlInsert(statement: params.sqlUpsertJoined, db: 'sqlite')

        // path>0 update never touches the path=0 joined row, so order vs the
        // upsert above does not matter. Driven off the (optional) joined row so
        // originals are ignored ONLY when a Path 0 was actually built.
        scaffold_join.out.row
            .map { id, row -> tuple(id) }
            .sqlInsert(statement: params.sqlIgnoreOriginals, db: 'sqlite')

        joined_rows
            .map { r -> tuple('0', 1, r.topology, r.length as Integer, params.ts, r.ID) }
            .sqlInsert(statement: params.sqlSyncAnnotateJoin, db: 'sqlite')
}
