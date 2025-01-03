process getOrgBindPaths {
    
    executor params.assemble.executor

    input:
    tuple val(id), val(opts_id), path(reads), val(opts)

    // Dynamically determine Singularity bind paths needed for custom get organelle databases
    // check if we're using Singularity
    if (workflow.containerEngine == 'singularity') {
        // get base paths for databases
        def seeds = opts.seeds_db
        def labels = opts.labels_db
        def seeds_path = java.nio.file.Paths.get(seeds).parent.toString()
        def labels_path = java.nio.file.Paths.get(labels).parent.toString()
        println seeds_path
        println labels_path
    }
/*
        // check if the bind paths were actually changed
        if (seeds_db_path != "/ref_dbs/getOrganelle/seeds" || labels_db_path != "/ref_dbs/getOrganelle/seeds") {
            if (seeds_db_path != "/ref_dbs/getOrganelle/seeds"){
                bindPathsList << seeds_db_path
            }
            if (labels_db_path != "/ref_dbs/getOrganelle/seeds"){
                bindPathsList << labels_db_path
            }
            // Combine the paths into a comma-separated string
            def dynamicBindPaths = bindPathsList.join(',')
            // Print the bind paths for debugging
            println "Dynamic Singularity bind paths set to: $dynamicBindPaths"
            // Update the Singularity configuration at runtime
            session.config.singularity.bindPaths = dynamicBindPaths 
        } else {
            println "Using default databases, no custom bind paths are needed"
        }
    } else {
        println "Singularity is NOT enabled"
    }
*/
}