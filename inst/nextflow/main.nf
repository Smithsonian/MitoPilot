nextflow.enable.dsl=2
include { fromQuery; sqlInsert } from 'plugin/nf-sqldb'
import groovy.transform.*
import groovy.util.*
import java.time.Instant

// Time stamp
params.ts = workflow.start.toInstant().getEpochSecond().toString()

// User-supplied-assembly projects with no raw reads: skip PREPROCESS + read
// mapping and derive coverage stats from the assembly itself. Signalled by the
// project .config setting rawDir = 'NA' (see new_project_userAsmb(no_raw_data)).
params.noRawData = (params.rawDir == 'NA')

// Modules
include {PREPROCESS} from './modules/preprocess_workflow.nf'
include {ASSEMBLE} from './modules/assemble_workflow.nf'
include {COVERAGE} from './modules/coverage_workflow.nf'
include {ANNOTATE} from './modules/annotate_workflow.nf'
include {CURATE} from './modules/curate_workflow.nf'
include {VALIDATE} from './modules/validate_workflow.nf'
include {ORF} from './modules/orf_workflow.nf'
include {COVERAGE_userAsmb; COVERAGE_userAsmb_noReads} from './modules/coverage_userAsmb_workflow.nf'
include {BLAST_GENBANK} from './modules/blast_genbank_workflow.nf'
include {BLAST_REF_FETCH} from './modules/blast_ref_fetch_workflow.nf'
include {BLAST_REF_ALIGN} from './modules/blast_ref_align_workflow.nf'
include {SCAFFOLD_JOIN} from './modules/scaffold_join_workflow.nf'

// ASSEMBLY WORKFLOW
workflow WF1 {

    PREPROCESS()
    ASSEMBLE(PREPROCESS.out[0])
    // COVERAGE runs for samples with a usable assembly regardless of whether
    // BLAST is requested; BLAST_GENBANK only runs for samples that still need
    // BLAST (status=4). See ASSEMBLE emit comments.
    COVERAGE(ASSEMBLE.out.cov)
    BLAST_GENBANK(ASSEMBLE.out.blast.map{ it -> tuple(it[0], it[1], it[4]) })
    BLAST_REF_FETCH(BLAST_GENBANK.out.ref_input, BLAST_GENBANK.out.scaffold_map, BLAST_GENBANK.out.ref_batches)

    // Auto-join single-path multi-scaffold assemblies. Joined here (not just in
    // the app) so a reference-ordered Path 0 is ready before annotation. Gated to
    // eligible IDs that also have coverage + a fetched reference (inner joins).
    ASSEMBLE.out.join_eligible
        .join(COVERAGE.out.cov_files)
        .join(BLAST_REF_FETCH.out.ref_seq)
        .join(BLAST_GENBANK.out.scaffold_hits)
        .set { scaffold_join_in }
    SCAFFOLD_JOIN(scaffold_join_in)

}

// ASSEMBLY WORKFLOW - user provided assemblies
workflow WF1_userAsmb {

    // No-reads projects skip PREPROCESS entirely and pull samples straight from
    // the DB; read-based projects preprocess then map reads for coverage. Either
    // path emits the same blast_in, so BLAST is invoked once.
    if (params.noRawData) {
        COVERAGE_userAsmb_noReads()
        blast_in = COVERAGE_userAsmb_noReads.out.blast_in
    } else {
        PREPROCESS()
        COVERAGE_userAsmb(PREPROCESS.out[0])
        blast_in = COVERAGE_userAsmb.out.blast_in
    }
    BLAST_GENBANK(blast_in)
    BLAST_REF_FETCH(BLAST_GENBANK.out.ref_input, BLAST_GENBANK.out.scaffold_map, BLAST_GENBANK.out.ref_batches)

}

// ANNOTATION WORKFLOW
workflow WF2 {

   ANNOTATE()
   CURATE(ANNOTATE.out[0])
   VALIDATE(CURATE.out[0])
   // Optional ORF-finder step: runs after validation on the finalized gene
   // models and appends any ORFs (in still-unannotated regions) to the db.
   ORF(VALIDATE.out.annotations, CURATE.out[0])
   BLAST_REF_ALIGN(VALIDATE.out.validated, CURATE.out[0])

}
