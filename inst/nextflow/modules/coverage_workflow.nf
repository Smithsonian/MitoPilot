include {coverage} from './coverage.nf'

params.sqlWrite =   'UPDATE assemblies SET depth = ?, gc = ?, errors = ?, time_stamp = ? ' +
                    'WHERE ID=? and path=? and scaffold=?'

workflow COVERAGE {
    take:
        input
    
    main:
        
        input
            .filter{ it[1] ==~ /(?!.*assembly_0\.fasta$).*$/ }      // skip empty assemblies
            .map{ it ->
                tuple( 
                    it[0],                                          // ID
                    it[4],                                          // assemble opt_id
                    it[2],                                          // reads
                    (it[1] instanceof List) ? it[1] : [it[1]],       // assembly
                    it[7]                                             // assembler
                )                      
            }
            .transpose( by: 3 )                                     // transpose by assembly (process each assembly separately)                                       
            .set { coverage_in }

        coverage(coverage_in).set { coverage_out }

        // Coverage
        coverage_out
            .flatten()
            .filter{ it =~ /(.*coverageStats.csv)$/ }
            .splitCsv(header: true, sep: ',')
            .set { coverage_rows }
        coverage_rows
            .map { it ->
                tuple(
                    it.SeqId,
                    it.MeanDepth,
                    it.GC,
                    it.ErrorRate
                )
            }
            .groupTuple()
            .map { it -> 
                tuple(
                    it[1].join(' '),                   // mean depth  
                    it[2].join(' '),                   // gc
                    it[3].join(' '),                   // error rate
                    params.ts,                         // timestamp
                    it[0].split('\\.'),                // id, path, scaffold
                ).flatten()
            }
            .sqlInsert(statement: params.sqlWrite, db: 'sqlite')

    emit:
        // Per-ID coverageStats CSV files, for the scaffold-join step to stitch.
        cov_files = coverage_out
            .map { id, files, wd ->
                def fl = (files instanceof List) ? files : [files]
                tuple(id, fl.findAll { it.name ==~ /.*coverageStats\.csv/ })
            }
            .filter { id, csvs -> csvs.size() > 0 }
            .groupTuple()
            .map { id, lists -> tuple(id, lists.flatten()) }
}