import java.util.Base64
include {curate} from './curate.nf'

params.sqlRead =    'SELECT DISTINCT a.ID, a.path, c.curate_opts, ' +
                        'd.cpus, d.memory, d.target, d.params ' +
                    'FROM assemblies a ' +
                    'JOIN assemble b ON a.ID = b.ID ' +
                    'JOIN annotate c ON a.ID = c.ID ' +
                    'JOIN curate_opts d ON c.curate_opts = d.curate_opts ' +
                    'WHERE c.annotate_switch = 1 AND c.annotate_lock = 0 AND b.assemble_lock = 1 AND a.ignore = 0'

workflow CURATE {
    take:
        input

    main:
    
        channel.fromQuery(params.sqlRead, db: 'sqlite')
            .join(input, by: [0, 1])
            .map { it ->
                def jsonParams = it[6].toString()
                def encodedParams = Base64.encoder.encodeToString(jsonParams.bytes) 

                tuple(
                    it[0],                                          // ID   
                    it[1],                                          // path
                    it[7],                                          // Annotations
                    it[8],                                          // Assembly
                    it[9],                                          // Coverage
                    [
                        cpus:  it[3],                                      // cpus
                        memory: it[4],                                     // memory
                        target: it[5],                                     // target
                        params: encodedParams                              // params
                    ]
                )
            }
            .set { curate_in }

        curate(curate_in).set { curate_out }

    // TODO - write to database

    emit:
           ch = curate_out

}