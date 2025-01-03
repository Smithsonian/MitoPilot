process assemble {

    debug true

    executor params.assemble.executor
    container params.assemble.container
 
    errorStrategy { task.exitStatus in 137..140 ? 'retry' : 'finish' }
    maxRetries { params.assemble.maxRetries }
    // cpus { opts.cpus }
    // memory { opts.memory.GB * task.attempt }

    tag "${id}"

    input:
    tuple val(id), val(opts_id), path(reads), val(opts)

    output:
    tuple val("${id}"), 
        path("${id}/assemble/${opts_id}/${id}_assembly_*.fasta"),             // Assemblies Output
        path("${id}/assemble/${opts_id}/${id}_reads.tar.gz"),                 // Trimmed Reads Out
        path("${id}/assemble/${opts_id}/${id}_summary.txt"),                  // getOrganelle summary
        val("${opts_id}"),                                                    // options id
        path("${id}/assemble/${opts_id}/get_org.log.txt")                     // getOrganelle log

    shell:
    workingDir = "${id}/assemble"
    outDir = "${workingDir}/${opts_id}"
    println "${opts}"
    println "${opts.cpus}"
    println "${opts.seeds_db}"
/*     if (workflow.containerEngine == 'singularity') {
        // get base paths for databases
        def seeds = ${opts.seeds_db}
        def labels = ${opts.labels_db}
        def seeds_path = java.nio.file.Paths.get(seeds).parent.toString()
        def labels_path = java.nio.file.Paths.get(labels).parent.toString()
        println seeds_path
        println labels_path
        if (seeds_db_path != "/ref_dbs/getOrganelle/seeds" || labels_db_path != "/ref_dbs/getOrganelle/seeds") {
            def bindPathsList = []
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
            // set bind path env variable
            //SINGULARITY_BIND = dynamicBindPaths
        } else {
            println "Using default databases, no custom bind paths are needed"
        }
    } else {
        println "Singularity is NOT enabled"
    } */
    '''
    echo "bind path is ${SINGULARITY_BIND}"       
    mkdir -p !{workingDir}
    get_organelle_from_reads.py \
        -1 !{reads[0]} \
        -2 !{reads[1]} \
        -o !{workingDir}/ --overwrite \
        -s !{opts.seeds_db} \
        --genes !{opts.labels_db} \
        -t !{task.cpus} \
        !{opts.getOrganelle}
    mkdir -p !{outDir}
    ### LOGS ####
    cp !{workingDir}/get_org.log.txt !{outDir}/get_org.log.txt
    echo "!{opts.getOrganelle}" > !{outDir}/opts.txt
    summary_get_organelle_output.py !{workingDir} -o !{outDir}/!{id}_summary.txt
    ### work dir info for troubleshooting ####
    echo "Nextflow assemble working directory:" > !{outDir}/NF_work_dir_assemble.txt
    echo "$PWD" >> !{outDir}/NF_work_dir_assemble.txt
    ### ARCHIVE READS ###
    tar -czvf !{outDir}/!{id}_reads.tar.gz !{workingDir}/extended*.fq
    ### FORMAT ASSEMBLIES ###
    export topology=$(awk -F'\t' 'NR==1{for(i=1;i<=NF;i++){if($i=="circular"){col=i;break;}}} NR>1{if($col=="yes") print "circular"; else if($col=="no") print "linear";}' !{outDir}/!{id}_summary.txt)
    shopt -s nullglob
    files=(!{workingDir}/*.fasta)
    if [ ${#files[@]} -eq 0 ]; then
        echo ">No assembly found" > !{outDir}/!{id}_assembly_0.fasta
    else
        parallel -j !{task.cpus} 'awk -v topo=$topology "/^>/ {print \\">!{id}.{#}.\\" ++count[\\">\\"] \\" \\" topo} !/^>/ {print}" {} > !{outDir}/!{id}_assembly_{#}.fasta' ::: "${files[@]}"
    fi
    '''
}
