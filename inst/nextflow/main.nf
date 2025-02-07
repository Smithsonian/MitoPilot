nextflow.enable.dsl=2
include { fromQuery; sqlInsert } from 'plugin/nf-sqldb'
import groovy.transform.*
import groovy.util.*
import java.time.Instant

// Time stamp
params.ts = workflow.start.toInstant().getEpochSecond().toString()

// Modules
include {PREPROCESS} from './modules/preprocess_workflow.nf'
include {ASSEMBLE} from './modules/assemble_workflow.nf'
include {COVERAGE} from './modules/coverage_workflow.nf'
include {ANNOTATE} from './modules/annotate_workflow.nf'
include {CURATE} from './modules/curate_workflow.nf'
include {VALIDATE} from './modules/validate_workflow.nf'

// ASSEMBLY WORKFLOW
workflow WF1 {

    PREPROCESS()
    ASSEMBLE(PREPROCESS.out[0])
    COVERAGE(ASSEMBLE.out[0])

}

// ANNOTATION WORKFLOW
workflow WF2 {

   ANNOTATE()
   CURATE(ANNOTATE.out[0])
   VALIDATE(CURATE.out[0])

}

