include {annotate} from './annotate.nf'

params.sqlRead =    'SELECT DISTINCT b.assemble_opts, a.ID, a.path, d.cpus, d.memory, d.mitos_refDB ' +
                    'FROM assemblies a ' +
                    'JOIN assemble b ON a.ID = b.ID ' +
                    'JOIN annotate c ON a.ID = c.ID ' +
                    'JOIN annotate_opts d ON c.annotate_opts = d.annotate_opts ' +
                    'WHERE c.annotate_switch = 1 AND c.annotate_lock = 0 AND b.assemble_lock = 1 AND a.ignore = 0'

workflow ANNOTATE {

    channel.fromQuery(params.sqlRead, db: 'sqlite')
        .map{ it ->
            tuple(
                it[1],                                          // ID   
                file(                                           // Assembly
                    params.publishDir + '/' + 
                    it[1] + '/assemble/' + it[0] + '/' + 
                    it[1] + '_assembly_' + it[2] + '.fasta'
                ),
                file(                                           // Coverage
                    params.publishDir + '/' + 
                    it[1] + '/assemble/' + it[0] + '/' + 
                    it[1] + '_assembly_' + it[2] + '_coverageStats.csv'
                ),
                [
                    it[3],                                      // cpus
                    it[4],                                      // memory
                    it[5]                                       // mitos_refDB
                ]
                
            )
        }
        .set { annotate_in }

}