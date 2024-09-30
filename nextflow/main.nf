nextflow.enable.dsl=2
include { fromQuery; sqlInsert } from 'plugin/nf-sqldb'
import groovy.transform.*
import groovy.util.*
import java.time.Instant

// Time stamp
params.ts = workflow.start.toInstant().getEpochSecond().toString()

// Modules
include {TRIM} from 'nextflow/.modules/preprocess.nf'
//include {ASSEMBLE} from './.modules/assemble.nf'
//include {COVERAGE} from './.modules/coverage.nf'
//include {ANNOTATE} from './.modules/annotate.nf'
//include {AMPLICONS} from './.modules/amplicons.nf'

// ASSEMBLY WORKFLOW
workflow WF1 {

    PREPROCESS()
    //ASSEMBLE(PREPROCESS.out[0])
    //COVERAGE(ASSEMBLE.out[0])

}

// ANNOTATION WORKFLOW
//workflow WF2 {

   // ANNOTATE()

//}

// EXTRACT AMPLICONS
//workflow WF3 {

    //AMPLICONS()

//}
