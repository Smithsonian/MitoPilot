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

    beforeScript:
    '''
    export SINGULARITY_BIND="/test/dir"
    echo "bind path = ${SINGULARITY_BIND}"       
    '''

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
