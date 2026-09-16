# Codebase map: how GetOrganelle and MitoFinder are plumbed through MitoPilot

Repo: /home/dmacguig/Documents/GitHub/MitoPilot, branch map-to-ref-assembly (HEAD 112d178, tree clean). Read-only investigation. Every fact carries a file:line reference; snippets are verbatim. "INFERENCE" marks conclusions I drew rather than things the code states.

Working name for the third assembler throughout: "MapToRef".

---

## 0. One-page summary of the seams

The assembler choice is a single TEXT column, `assemble_opts.assembler`, on a NAMED OPTION SET (not per sample). Samples point at an option set through `assemble.assemble_opts` (init_db.R:268, :298), and that option-set name is also the on-disk output directory `out/<ID>/assemble/<assemble_opts>/` (assemble.nf:22-23, project_consistency.R:1-15). Both assemblers live inside ONE Nextflow process, `assemble`, as an `if/elif` on `opts.assembler` (assemble.nf:27, :56). Downstream, exactly one other place branches on the assembler string: the `coverage` process (coverage.nf:35, :40). Everything after coverage (BLAST, reference fetch, scaffold join, annotate, curate, validate, export, the app) is assembler-agnostic and keys only on (a) the published directory `out/<ID>/assemble/<opts>/`, (b) the file names `<ID>_assembly_<path>.fasta` and `<ID>_assembly_<path>_coverageStats.csv`, (c) the FASTA header contract `>ID.path.scaffold topology`, and (d) rows written to `assemble`, `assemblies` and `annotate`.

So a third assembler is, minimally: new columns on `assemble_opts` + defaults + validation + migration; a new `elif` in assemble.nf that emits the same five output files; a new `elif` in coverage.nf; the SELECT and opts map in assemble_workflow.nf; the options modal (five touch points in two R files); tool help; docs; tests.

---

## 1. Project init and options

### 1.1 Where the assembler choice lives

`new_db()` (R/init_db.R:61-109) is the schema authority. Defaults for the assembly option set (verbatim, init_db.R:67-86):

```r
    # Default assembly options
    assemble_cpus = 6,
    assemble_memory = 24,
    assembler = "GetOrganelle",
    seeds_db = "https://raw.githubusercontent.com/smithsonian/MitoPilot/main/ref_dbs/getOrganelle/seeds/fish_mito_seeds.fasta",
    labels_db = "https://raw.githubusercontent.com/smithsonian/MitoPilot/main/ref_dbs/getOrganelle/labels/fish_mito_labels.fasta",
    getOrganelle = paste(
      "-F 'anonym'",
      "-R 10 -k '21,45,65,85,105,115'",
      "--larger-auto-ws",
      "--expected-max-size 20000",
      "--target-genome-size 16500"
    ),
    mitofinder_db = "https://raw.githubusercontent.com/Smithsonian/MitoPilot/refs/heads/main/ref_dbs/MitoFinder/fish_mito_sampler.gb",
    mitofinder = paste(
      "--megahit"
    ),
    max_paths = 10,
    max_scaffolds = 10,
    min_assembly_length = 500,
```

Validation gate (init_db.R:130-133), which a third assembler must extend:

```r
  # Validate assembler choice
  if (assembler %nin% c("GetOrganelle", "MitoFinder")) {
    stop("Assembler not supported, valid options: [GetOrganelle, MitoFinder]")
  }
```

Roxygen for the arg (init_db.R:45) says `Assembler, choice of "GetOrgnalle" (default) or "MitoFinder"` (typo in source).

### 1.2 The `assemble_opts` table, every column (init_db.R:311-329)

```sql
    CREATE TABLE assemble_opts (
      assemble_opts TEXT NOT NULL,
      cpus INTEGER,
      memory INTEGER,
      getOrganelle TEXT,
      seeds_db TEXT,
      labels_db TEXT,
      assembler TEXT,
      mitofinder_db TEXT,
      mitofinder TEXT,
      max_paths INTEGER,
      max_scaffolds INTEGER,
      min_assembly_length INTEGER,
      join_scaffolds INTEGER,
      PRIMARY KEY (assemble_opts)
    );
```

The seed row is named `"default"` (init_db.R:333) and carries `join_scaffolds = 0L` (init_db.R:345). Tool-specific option strings are free-form TEXT appended verbatim to the command line (`!{opts.getOrganelle}` at assemble.nf:36; `!{opts.mitofinder}` at assemble.nf:60). Database paths are TEXT that may be a local path or a URL (init_db.R:14-17, :46-47).

The userAsmb variant deliberately carries a minimal `assemble_opts` (init_db_userAsmb.R:358-371): only `assemble_opts`, `min_assembly_length`, `join_scaffolds`, seed row named `"user"` (init_db_userAsmb.R:375). Comment at init_db_userAsmb.R:359-362: "The regular pipeline schema carries assembler/getOrganelle/etc. fields that don't apply when assemblies are user-provided."

### 1.3 Per project or per sample

Per OPTION SET. `assemble.assemble_opts` is a TEXT pointer per sample (init_db.R:268), seeded `"default"` for every sample (init_db.R:298); `add_samples()` also seeds `"default"` (add_samples.R:183). Different samples can point at different sets (the app lets you create a set by typing a new name, app_assemble.R:962-986). There is no per-sample column for a reference. The closest per-sample precedents are:

- userAsmb projects: `samples.assembly` and `samples.topology` come from mapping-file columns `Assembly` / `Topology` (init_db_userAsmb.R:182-220), and the workflow stages the file with `file(params.asmbDir + "/" + it[1])` (coverage_userAsmb_workflow.nf:319).
- Regular projects: `assemble.blast_accession` (per sample, init_db.R:271) plus the `blast_ref_sequences` table keyed by accession (init_db.R:743-753); the fetched reference FASTA is emitted per ID as `BLAST_REF_FETCH.out.ref_seq` (blast_ref_fetch_workflow.nf:272-274) and consumed by scaffold_join.nf:38 as `path(ref_seq)`.

### 1.4 The option-set name is a directory name

project_consistency.R:1-15:

```r
#' The ASSEMBLE workflow publishes to `out/<ID>/assemble/<assemble_opts>/`
#' (`inst/nextflow/modules/assemble.nf`), so the option-set name doubles as a
#' directory name. Every downstream stage rebuilds this path from the current
#' `assemble.assemble_opts` value.
assemble_out_dir <- function(dir_out, ID, opts) {
  file.path(dir_out, ID, "assemble", opts)
}
```

`stale_assemble_dirs()` (project_consistency.R:63) warns when a sample's pointer names a directory that was never published; it is invoked at app start (app_server.R:132-147), on lock (app_assemble.R:597-612), and after an options edit (app_assemble.R:1009-1054). The Nextflow annotate driver also logs a warning listing the directories actually present (annotate_workflow.nf:39-54). Consequence for MapToRef: a user who creates a set called e.g. `maptoref` gets `out/<ID>/assemble/maptoref/`.

### 1.5 cpus / memory

`assemble_opts.cpus` / `memory` (GB) feed the per-process directives through the `opts` input map: every config template has `cpus = { opts.cpus }` and `memory = { opts.memory.GB * task.attempt }` (config.slurm:19-20; config.local:12-13 uses `(opts?.memory?.GB ?: 1) * task.attempt`). NMNH_Hydra computes `-l mres=... h_data=...` from `opts.memory` and `opts.cpus` (config.NMNH_Hydra:23-29). Inside the assemble script GetOrganelle takes `-t !{opts.cpus}` (assemble.nf:35) and MitoFinder takes `-p !{opts.cpus} -m !{opts.memory}` (assemble.nf:67-68). Contract: the `opts` map handed to `assemble` MUST have `cpus` and `memory` keys or the config closures fail.

### 1.6 Custom assembly databases: build, register, ship

`custom_assembly_db()` (R/custom_assembly_db.R:60, exported at NAMESPACE:49) downloads GenBank records with httr2 and writes, into a dated directory, `getorganelle_seed.fasta`, `getorganelle_label.fasta`, and/or `mitofinder_<clade>_<src>_<date>.gb` (custom_assembly_db.R:226-237), plus `manifest.json` and `README.txt` (:253-254). It does NOT touch any project database: registration is the user pasting the path/URL into `new_project(custom_seeds_db=, custom_labels_db=)` (init_project.R:55-56, forwarded at :130-131) or `new_db(mitofinder_db=)` via `...` (init_project.R:132), or into the app modal text fields (app_assemble_utils.R:323-365). `.cadb_print_instructions()` (custom_assembly_db.R:753) prints those instructions.

Shipping:
- `ref_dbs/` is EXCLUDED from the R package (`.Rbuildignore` line `^ref_dbs$`) and NOT copied into the image (Dockerfile:120-123 are commented out: "not needed for >= v1.3.0, since dbs are downloaded directly from GitHub").
- The defaults are GitHub raw URLs (init_db.R:71-72, :80) which Nextflow stages as remote files because they arrive at `path(dbs)` / `path(mf_db)` inputs (assemble.nf:16) from plain strings (assemble_workflow.nf:109-113). backwards_compatibility.R:1276 backfills the same URL for old projects.
- The only reference data baked into the image is the BLAST database: `ADD --chown=root:root docker/mito_metazoa_blastdb.tar.gz /ref_dbs/` (Dockerfile:82), referenced by `params.blast_gb.db_dir = '/ref_dbs/mito_metazoa'` (config.local:93-94).
- The package DOES ship `inst/test_data/fish_mito_sampler.gb` (10 LOCUS records) used by `new_test_project_userAsmb()` (init_test_project_userAsmb.R:99-100). `ref_dbs/MitoFinder/NC_002333_Danio_rerio.gb` is a single-record GenBank file (LOCUS NC_002333 16596 bp DNA circular VRT) that is NOT in the package.
- Staging rule the maintainers wrote down: find_mito_workflow.nf:103-105 "The reference database is staged as a task input so it is visible inside the container on every executor." Same pattern at annotate_workflow.nf:92 (`file(it[17] ... : "${projectDir}/assets/NO_FILE")`) with the `inst/nextflow/assets/NO_FILE` placeholder for "no file".

### 1.7 Config generation

`new_project()` fills `<<CONTAINER_ID>>`, `<<RAW_DIR>>`, `<<ASMB_DIR>>` (= "NA" for read projects), `<<MIN_DEPTH>>`, `<<NCBI_API_KEY>>` (init_project.R:144-152) into a template resolved by `resolve_config()` (generate_config.R:228). Container default: `paste0("macguigand/mitopilot:", utils::packageVersion("MitoPilot"))` (init_project.R:54). Templates: inst/config.{local,awsbatch,slurm,sge,pbs,lsf,NMNH_Hydra,NOAA_SEDNA}. The `assemble` block in every template is just `maxRetries = 1`, `container`, `executor` (config.local:40-44); there is no per-tool param in the config. `migrate_config()` regenerates `.config` wholesale on `backwards_compatibility()` (generate_config.R:134-216). A new assembler needs NO config change unless it wants its own `params.maptoref { ... }` block (pattern: `params.find_mito?.executor ?: params.assemble.executor`, find_mito.nf:6-7).

Test project: `new_test_project()` copies 15 fish read pairs from inst/test_data (mapping_test.csv) and calls `new_project(min_depth = 500, ...)` (init_test_project.R:121-132). Extra args pass through `...` to `new_db()`, so `new_test_project(assembler = "MapToRef", maptoref_ref = ...)` would work once `new_db()` accepts them.

---

## 2. Nextflow

### 2.1 Entry and WF1 order

`nextflow_cmd()` builds `nextflow -log .logs/nextflow.log run <pkg>/nextflow -c .config -entry WF1|WF1_userAsmb|WF2 [-resume]` (update_mitopilot.R:11-42). WF1 (main.nf:31-86):

```
    PREPROCESS()
    ASSEMBLE(PREPROCESS.out[0])
    COVERAGE(ASSEMBLE.out.cov)
    BLAST_GENBANK(ASSEMBLE.out.blast.map{ it -> tuple(it[0], it[1], it[4]) })
    BLAST_REF_FETCH(BLAST_GENBANK.out.ref_input, ..., ASSEMBLE.out.join_expected)
    ... SCAFFOLD_JOIN(join_rows.complete, join_dropped, ASSEMBLE.out.join_redo)
```

`params.ts` is the run's epoch-second time stamp (main.nf:8) stamped onto every row written in the run.

### 2.2 PREPROCESS output consumed by ASSEMBLE

preprocess.nf:16: `tuple val("${id}"), path("${id}/${id}_preprocess_*"), env(after)`, i.e. (ID, [R1,R2] named `<ID>_preprocess_R1.fastq.gz` / `_R2.fastq.gz` (preprocess.nf:26-27), trimmed read count). ASSEMBLE filters on `it[2].toInteger() >= params.minDepth` (assemble_workflow.nf:174-180) and writes `'Insufficient sequencing depth'` with state 3 for the rest (:369-387).

### 2.3 The `assemble` process contract (assemble.nf)

Directives (assemble.nf:5-11): `executor params.assemble.executor`, `container params.assemble.container`, `publishDir "${launchDir}/${params.publishDir}", overwrite: true, mode: 'copy'` (no pattern, so every declared output is published), `errorStrategy { task.exitStatus in 137..140 ? 'retry' : 'ignore' }`, `maxRetries { params.assemble.maxRetries }`.

Input (assemble.nf:16):

```
    tuple val(id), val(opts_id), path(reads), val(opts), path(dbs), path(mf_db), val(genetic_code), val(max_paths), val(max_scaffolds)
```

Output (assemble.nf:19), the contract every consumer relies on:

```
    tuple val("${id}"), path("${id}/assemble/${opts_id}/${id}_assembly_*.fasta"), path("${id}/assemble/${opts_id}/${id}_reads.tar.gz"), path("${id}/assemble/${opts_id}/${id}_summary.txt"), val("${opts_id}"), path("${id}/assemble/${opts_id}/assembler.log.txt"), path("${id}/assemble/${opts_id}/NF_work_dir_assemble.txt"), val("${opts.assembler}"), val(max_paths), val(max_scaffolds)
```

Positional meaning used downstream: [0] ID, [1] assembly FASTA(s) (a single path or a List), [2] reads tarball, [3] summary, [4] opts_id (= directory name), [5] log, [6] work-dir note, [7] assembler string, [8] max_paths, [9] max_scaffolds.

The switch (assemble.nf:27 and :56): `if [ "!{opts.assembler}" = "GetOrganelle" ]; then ... elif [ "!{opts.assembler}" = "MitoFinder" ]; then ... fi`. There is NO else: an unknown assembler string produces no files and the task fails on missing outputs.

GetOrganelle branch (assemble.nf:29-55): `get_organelle_from_reads.py -1 -2 -o ID/assemble/ --overwrite -s dbs[0] --genes dbs[1] -t cpus <opts.getOrganelle>`; copies `get_org.log.txt` to `assembler.log.txt` (:39); writes `opts.txt` (:40, NOT a declared output so not published); runs `summary_get_organelle_output.py` to make `<ID>_summary.txt` (:41); archives `extended*.fq` (the recruited reads) into `<ID>_reads.tar.gz` (:46); reads topology from the summary's `circular` column (:48); then renames every `*.fasta` GetOrganelle produced:

```
            parallel -j !{opts.cpus} 'awk -v topo=$topology "/^>/ {print \\">!{id}.{#}.\\" ++count[\\">\\"] \\" \\" topo} !/^>/ {print}" {} > !{outDir}/!{id}_assembly_{#}.fasta' ::: "${files[@]}"
```

i.e. file N (GNU parallel `{#}` job number, 1-based) becomes `<ID>_assembly_N.fasta` and each record inside becomes `>ID.N.k topology` with k = 1..scaffolds. If no FASTA exists: `echo ">No assembly found" > <ID>_assembly_0.fasta` (:52).

MitoFinder branch (assemble.nf:57-101): `mitofinder <opts.mitofinder> --ignore -j ID -1 -2 -r mf_db -o <genetic_code> -p cpus -m memory` run from inside `ID/assemble`; `*_MitoFinder.log` becomes `assembler.log.txt` (:72); summary is `touch`ed as an empty placeholder (:75-77, "TO DO"); archives the staged `*.fastq.gz` preprocess reads (:80); collects `*_Final_Results/*mtDNA_contig*.fasta` (:88); multiple files are concatenated into ONE path with `topology="linear"` (:91-97), a single file takes topology from `.infos` `Circularization: Yes` (:99); same rename/awk as above (:100).

Genetic code arrives as `val(genetic_code)` and is used as `!{genetic_code.intValue()}` (assemble.nf:66); it comes from `samples.genetic_code` (assemble_workflow.nf:8, :114).

### 2.4 The ASSEMBLE workflow driver (assemble_workflow.nf)

SELECT (assemble_workflow.nf:6-20), verbatim:

```
params.sqlRead =  'SELECT a.ID, a.assemble_opts, opts.cpus, opts.memory, ' +
                  'opts.seeds_db, opts.labels_db, opts.getOrganelle, opts.assembler, ' +
                  'opts.mitofinder_db, opts.mitofinder, s.genetic_code, ' +
                  'opts.max_paths, opts.max_scaffolds, opts.min_assembly_length, ' +
                  'b.run_blast, opts.join_scaffolds, ' +
                  'a.join_switch, a.assemble_switch, a.blast_accession ' +
                  'FROM assemble a ' +
                  'JOIN assemble_opts opts ' +
                  'ON a.assemble_opts = opts.assemble_opts ' +
                  'JOIN samples s ' +
                  'ON a.ID = s.ID ' +
                  'LEFT JOIN blast_opts b ' +
                  'ON a.blast_opts = b.blast_opts ' +
                  'WHERE (a.assemble_switch IN (1, 4) OR a.join_switch = 1) ' +
                  'AND a.assemble_lock = 0'
```

The opts map (assemble_workflow.nf:99-117) is positional, so any new column must be appended to the SELECT AND to this map:

```
                    [                                                           //## assembly options ##//
                        cpus: it[2],                                            // cpus
                        memory: it[3],                                          // memory
                        getOrganelle: it[6],                                    // getOrganelle options
                        mitofinder: it[9],                                      // mitofinder options
                        assembler: it[7]                                        // assembler
                    ],
                    [
                        it[4],                                                  // getOrganelle seeds_db
                        it[5]                                                   // getOrganelle labels_db
                    ],
                    it[8],                                                      // mitofinder .gb reference database
```

Note the database paths are handed over OUTSIDE the opts map, as separate tuple elements, so they reach `path()` qualifiers and get staged (assemble.nf:16 `path(dbs), path(mf_db)`). A MapToRef reference file must follow the same route (a separate `path(...)` element, not a string inside `opts`), or it will not be visible inside Singularity on HPC (find_mito_workflow.nf:103-105).

Post-assembly logic, all assembler-agnostic:
- Delete previous-run rows: `DELETE FROM assemblies WHERE ID = ? AND time_stamp != ?` (:35, fired at :204-211).
- Summarize each sample by parsing the FASTA files it emitted (:213-255): n_paths = number of files, n_scaffolds = max records per file, lengths, topologies from the header description. Files matching `assembly_0.fasta` are excluded (:215).
- Threshold branch (:260-267): `fail: (n_paths > max_paths) || (n_scaffolds > max_scaffolds)`, else pass.
- Status and notes (:272-297): default `'4'`; `n_scaffolds > 1` sets notes `'Output contains disconnected contigs'` or `'... (fragmented)'`; `n_paths > 1` sets `'Unable to resolve single assembly from reads'`; `max_len < min_assembly_length` sets `'3'` and `"All scaffolds below min assembly length (...)"`; `run_blast == 0` and status 4 becomes `'2'`.
- `assemble` write (:87-88): `UPDATE assemble SET paths=?, scaffolds=?, length=?, topology=?, assemble_switch=?, assemble_notes=?, time_stamp=?, poor_blast_ref=NULL WHERE ID=?` with length = `"len1;len2"` descending and topology = unique sorted `;`-joined (:250-252).
- `assemblies` write (:60-85): an UPSERT, NOT `INSERT OR REPLACE`; the comment at :37-46 explains the data-loss race with nf-sqldb batches. Per-record tuple built by `splitFasta(record: [id: true, desc: true, seqString: true])` where `record.id.split('\\.')` yields (ID, path, scaffold) and `record.desc` is the topology (:314-334); `ignore = 1` when length < min_assembly_length (:328-333).
- Empty assemblies (`assembly_0.fasta`): `'3'`, `'failed assembly'` (:355-367).
- Annotate seeding (:405-421): one row per non-ignored unit, `INSERT OR REPLACE INTO annotate (ID, path, scaffold, topology, partial, annotate_opts, curate_opts, orf_opts, annotate_switch, annotate_lock, reviewed) VALUES (?, ?, ?, ?, ?, ?, ?, ?, 1, 0, "no")`. `partial = 'no'` iff topology == 'circular' or curate_opts.linear_complete == 1 (:418). Because it is INSERT OR REPLACE, every annotate column not in that list is reset to NULL (the userAsmb variant switched to `ON CONFLICT DO UPDATE` limited to topology/partial, coverage_userAsmb_workflow.nf:259-266).
- Emits (:435-466): `cov` (status 4 or 2), `blast` (status 4 only), `join_eligible`, `join_expected`, `join_redo`.

State codes (app_assemble.R:27-33): 0 Pre-Assembly, 1 Ready, 4 In Progress (assembled, awaiting BLAST), 2 Success, 3 Failed. BLAST promotes 4 -> 2 (`UPDATE assemble SET assemble_switch = ? WHERE ID = ? AND assemble_switch = 4`, blast_genbank_workflow.nf:65) or writes 3 with `[blast]`-tagged notes (:73-80).

### 2.5 Failure semantics

- `errorStrategy 'ignore'` on assemble (non-OOM exits) means a crashed task writes NOTHING to the DB: the sample keeps state 1/4 and the run exits non-zero at the end because every config sets `workflow { failOnIgnore = true }` (config.local:100-103). The only path to a recorded assembly failure is the process succeeding and emitting the `>No assembly found` sentinel in `<ID>_assembly_0.fasta` (assemble.nf:52, :90 -> assemble_workflow.nf:355-367). INFERENCE: a MapToRef branch should never `exit 1` on "no consensus"; it should emit the sentinel.
- OOM/resource kills (exit 137..140) retry once (assemble.nf:10-11; `maxRetries = 1` in every config) with `memory * task.attempt`.

### 2.6 The `coverage` process contract (coverage.nf)

Input (coverage.nf:23): `tuple val(id), val(opt_id), path(reads), path(assembly), val(assembler)`; COVERAGE builds it from the assemble tuple as (it[0], it[4], it[2], it[1] as List, it[7]) and `transpose(by: 3)` so ONE task per path file (coverage_workflow.nf:12-24). Output: `tuple val(id), path("${outDir}/*"), path(".../NF_work_dir_coverage.txt")` with `outDir = "${id}/assemble/${opt_id}"` (coverage.nf:26-30). Directives: `params.coverage.cpus/memory/clusterOptions` from config (coverage.nf:9-18), `errorStrategy 'ignore'`.

The assembler switch (coverage.nf:35-43), verbatim:

```
    if [ "!{assembler}" = "GetOrganelle" ]; then
        tar -xzf !{reads} --strip-components=2
        # Concatenate unpaired reads
        cat extended_*_unpaired.fq >> unpaired.fq
        Rscript -e "MitoPilot::coverage('!{assembly}', 'extended_1_paired.fq', 'extended_2_paired.fq', 'unpaired.fq', !{task.cpus}, '!{outDir}')"
    elif [ !{assembler} == "MitoFinder" ]; then
        tar -xzf !{reads}
        Rscript -e "MitoPilot::coverage('!{assembly}', '!{id}_preprocess_R1.fastq.gz', '!{id}_preprocess_R2.fastq.gz', 'NA', !{task.cpus}, '!{outDir}')"
    fi
```

So the reads tarball contract is assembler-specific: GetOrganelle's archive holds `ID/assemble/extended_1_paired.fq`, `extended_2_paired.fq`, `extended_*_unpaired.fq` (two leading path components, hence `--strip-components=2`); MitoFinder's holds `<ID>_preprocess_R1.fastq.gz` / `_R2.fastq.gz` at top level. Again no else branch: an unknown assembler yields no coverageStats, so `assemblies.depth/gc/errors` stay NULL and annotate.nf later fails to stage the missing `_coverageStats.csv` (annotate_workflow.nf:61-64 builds `file(...)` unconditionally; annotate.nf:24 declares it `path(coverage)`).

The userAsmb variant (coverage_userAsmb.nf:22) takes `tuple val(id), path(reads), file(assembly), val(topology_map), val(assembler)` where `assembler` is documented as `opts_id (assemble_opts key)` (:29) and always calls `coverage(fasta, R1, R2, 'NA', ...)` (:59); the no-reads variant calls `coverage(fasta, 'NA','NA','NA', ...)` (:132). Naming confusion to be aware of: in coverage_userAsmb.nf, circularize.nf:27 and find_mito.nf:95 the variable named `assembler` is actually the option-set name used for `outDir`.

### 2.7 COVERAGE DB write and emit

`UPDATE assemblies SET depth = ?, gc = ?, errors = ?, time_stamp = ? WHERE ID=? and path=? and scaffold=?` (coverage_workflow.nf:3-4), fed by `splitCsv(header: true)` of every `*coverageStats.csv`, selecting `SeqId, MeanDepth, GC, ErrorRate`, grouped by SeqId and joined with spaces (:28-53). `SeqId.split('\\.')` yields (ID, path, scaffold) (:50). Emits `cov_files` = per-ID list of `*coverageStats.csv` (:56-64) for the scaffold join.

### 2.8 What BLAST, reference fetch, scaffold join, annotate need from an assembler

- BLAST_GENBANK takes `tuple(id, assembly_file_or_list, opts_id)` (blast_genbank_workflow.nf:101-106), parses `assembly_(\d+)\.fasta$` for the path index (:160), filters records by min_assembly_length and preserves headers so `qseqid` parses back to (path, scaffold) (:143-147). Publishes `blast_genbank_<path>.txt` into `out/<ID>/assemble/<opts_id>/` (blast_genbank.nf:77-78).
- SCAFFOLD_JOIN takes `tuple val(id), path(assembly), val(opts), val(auto_join), path(cov_csvs), path(ref_seq), val(scaffold_hits)` (scaffold_join.nf:38) and writes `<ID>_assembly_0.fasta` + `<ID>_assembly_0_coverageStats.csv` into the same directory (scaffold_join.nf:44-45); redo mode reads them back from `${launchDir}/${params.publishDir}/${id}/assemble/${opts}` (scaffold_join_workflow.nf:205-227).
- ANNOTATE (WF2) reads from the DB only `a.ID, a.path, a.scaffold, b.assemble_opts, ...` (annotate_workflow.nf:13-25; no `assembler` column) and stages `out/<ID>/assemble/<opts>/<ID>_assembly_<path>.fasta` and `..._coverageStats.csv` (:40-64). annotate.nf:24-27 then runs `MitoPilot::annotate(assembly_fn, coverage_fn, ...)`.

### 2.9 Container / tool paths in Nextflow

Everything runs in `process.container` (`<<CONTAINER_ID>>`), and each stage gets `params.<stage>.container = process.container` (config.local:36-97). Conda-env tools are referenced by env name params `mitos_condaenv`, `trnaScan_condaenv`, `aragorn_condaenv`, `orffinder_condaenv` (config.local:32-35) and invoked as `conda run -n ...` (coverage.R:81) or `reticulate::conda_run2` (annotate_mitofinder.R:104-109). Tools on PATH (get_organelle_from_reads.py, mitofinder, bowtie2, samtools, minimap2, blastn) are called bare.

---

## 3. Coverage, path scoring, circularization: what an assembly must look like

### 3.1 `coverage()` (R/coverage.R:12-229)

Inputs: FASTA path, R1, R2, unpaired ("NA" for none), cpus, outDir. Topology is read PER RECORD from the header description containing the word `circular` (:26, :241-243). Circular records get their first 500 bp appended before mapping (:52, :250-259) and positions folded back (:124, :267-287). Mapping: `bowtie2-build`, then `bowtie2 --very-sensitive-local --no-unal -x index -1 -2 [-U] --threads | samtools view -bS - | samtools sort - > <basename>.bam` (:62-77); per-base counts via `conda run -n bam-readcount bam-readcount -w1 -f <working.fasta> <bam>` (:80-82). If `paired_reads_1 == "NA"` the per-base table is synthesized from the sequence with NA depth (:40-48).

Outputs in outDir, all named from the FASTA basename: `<base>_coverage.csv` (per-base: SeqId, Position, Call, Depth, Correct, ErrorRate; :148), `<base>_<scaffoldNum>_coverage.pdf` per scaffold (:216-217, skipped with no reads), `<base>.bam`, and `<base>_coverageStats.csv` (:221-226). The stats columns are: `SeqId, Position, Call, Depth, Correct, ErrorRate, MeanDepth, GC` where `MeanDepth`/`ErrorRate` are 5-bp rolling means, prefixed with `#` where the outlier mask fires (:295-350). `.coverage_rolling_stats()` is shared with the scaffold-join consensus writer so both produce "an identical *_coverageStats.csv layout" (:289-293).

Consumers of the CSV: coverage_workflow.nf:34-41 (SeqId, MeanDepth, GC, ErrorRate), annotate.R:101-102 (`read.csv(coverage_fn)`), `coverage_trim()` (annotate_coverage_trim.R:8, needs `Position`, `MeanDepth`, and fills `Depth`, `Correct`, `ErrorRate`, `mask`; skips contigs < 152 rows, :33), the details modal (app_assemble_coverage_details.R:104-117 reads `Depth`/`ErrorRate` for path scoring; :618-626; :2062-2065), and the scaffold join (`stitch_coverage`, scaffold_join.R:1187).

### 3.2 Path scoring (R/assembly_path_scoring.R)

Pure functions; needs `paths_df` with `path, scaffold, topology, length, sequence` and optional BLAST columns plus per-path CSVs with `Depth`/`ErrorRate` (:62-96). Prefers circular topology, one scaffold, length near consensus, even depth, low error, BLAST concordance, few ambiguities (:22-32). Not assembler-specific despite the docstring naming both tools (:4).

### 3.3 Circularization (R/circularize_asmb.R) and the userAsmb WF1 stage pattern

Only in WF1_userAsmb (main.nf:89-139). `circularize_asmb()` (:205) self-BLASTs each contig for an end overlap (`find_end_overlap`, :345-380, `blastn -query -subject`) and, with reads, confirms with `bowtie2 ... | samtools view/sort` on a junction construct and parses SAM fields in R (`count_junction_reads`, :522-599; `cigar_ref_length`, :611). The Nextflow process writes a `topology_map.txt` (`contig circular|linear` per line, circularize.nf:56-63) that the coverage awk stamps into headers (coverage_userAsmb.nf:50-56). This is the existing precedent for "a WF1 stage that maps reads to a sequence and records evidence" (tables `circularize_overlap`, `circularize_depth`, circularize_workflow.nf:9-37).

### 3.4 find_mito (R/find_mito.R)

userAsmb-only; BLAST screen + `samtools faidx` extraction (:429-436) + MitoFinder confirmation (:449). Relevant to MapToRef only as precedent for a chunked BLAST fan-out and for `extract_contigs()`.

### 3.5 Every assembler-specific string in R/ and inst/

`grep -rin "getorganelle\|mitofinder"` hits (ignoring the MitoFinder ANNOTATION tool, which is a different feature in annotate_opts and is unrelated to the assembler):

- R/init_db.R:45-47, :70-83, :131-132, :151-160 (defaults, validation, DB fallback).
- R/init_project.R:27-28, :55-56, :130-131 (custom_seeds_db / custom_labels_db args).
- R/backwards_compatibility.R:14, :176-178, :1253-1313 (assembler / mitofinder_db / mitofinder migrations), :1276 (default URL), :1298 (default `--megahit`).
- R/app_assemble_utils.R:276-366 (modal UI), :441-448 (initial show/hide).
- R/app_assemble.R:125-126 (tool help registration), :847-885 (repopulate on set change), :889-900 (toggleState), :944-957 (show/hide on assembler change), :963-983 (rows_upsert of the option set).
- R/app_assemble_coverage_details.R:2285 (note text "multi-path getOrganelle output trimmed for consensus").
- R/assembly_path_scoring.R:4, R/export.R:3, R/scaffold_join.R:18, :660, R/circularize_asmb.R:166 (comments only).
- R/custom_assembly_db.R (whole file builds the two DB kinds).
- inst/nextflow/modules/assemble.nf:27, :56 (the switch); coverage.nf:35, :40 (the switch); assemble_workflow.nf:7-9, :105-113 (SELECT/opts).
- inst/tool_help/getOrganelle.txt, mitofinder.txt; tools/capture_tool_help.sh:34-43 (TOOLS list).
- vignettes/Test-Project-Assemble.Rmd:142-168, Difficult-Assemblies.Rmd:16-23, :43-47, custom_dbs.Rmd:31-147, Your-Own-Project.Rmd:182-192; README.md:29-31, :168.

---

## 4. Shiny app

### 4.1 Where the assembler is chosen

Only inside the "Assembly Opts." modal. The Assemble table shows the option-set NAME as a link (`assemble_opts` colDef, app_assemble.R:315-320, `cell = rt_link(ns("set_assemble_opts"))`); there is no assembler column in the table. `fetch_assemble_data()` selects only `assemble_opts, min_assembly_length` from the opts table (app_assemble_utils.R:18-19).

### 4.2 The options modal, and the five places to add a MapToRef field

`assemble_opts_modal()` (app_assemble_utils.R:222-457). The assembler picker (verbatim, :290-300):

```r
            selectizeInput(
              ns("assembler"),
              label = NULL,
              choices = c("GetOrganelle", "MitoFinder"),
              selected = current$assembler %||% character(0),
              width = "100%",
              options = list(
                create = FALSE,
                maxItems = 1
              )
            ) |> shinyjs::disabled()
```

Tool fields are `textInput`s (:313-365) with help appended INSIDE the input container (`tagAppendChild(opts_help(..., nested = TRUE))`) so that `shinyjs::hide(id)` hides the help too (:310-312, :438-440). Field ids: `mitofinder`, `mf_db` (note: the input is `mf_db`, the column is `mitofinder_db`), `getOrganelle`, `seeds_db`, `labels_db`. The `?` icon is `tool_help_icon("mitofinder")` (:315) reading `inst/tool_help/mitofinder.txt` (help_utils.R:41-51, :67-102), toggled by `register_tool_help("mitofinder", input, ...)` (app_assemble.R:126, help_utils.R:113-120).

The show/hide logic is duplicated three times and all three need the new branch:
1. Initial state after modal build (app_assemble_utils.R:441-448).
2. When the user picks a different existing set in the dropdown (app_assemble.R:873-885), preceded by the `update*Input` repopulation (:847-870).
3. When the assembler selector changes (app_assemble.R:944-958).

Edit gating: `shinyjs::toggleState(<id>, condition = input$edit_assemble_opts)` per field (app_assemble.R:889-900).

Save (app_assemble.R:960-986), verbatim core:

```r
        dplyr::tbl(session$userData$con, "assemble_opts") |>
          dplyr::rows_upsert(
            data.frame(
              assemble_opts = req(input$assemble_opts),
              cpus = req(input$assemble_opts_cpus),
              memory = req(input$assemble_opts_memory),
              getOrganelle = req(input$getOrganelle),
              seeds_db = req(input$seeds_db),
              labels_db = req(input$labels_db),
              assembler = req(input$assembler),
              mitofinder_db = req(input$mf_db),
              mitofinder = req(input$mitofinder),
              max_paths = as.integer(req(input$max_paths)),
              max_scaffolds = as.integer(req(input$max_scaffolds)),
              min_assembly_length = as.integer(req(input$min_assembly_length)),
              join_scaffolds = as.integer(isTRUE(input$join_scaffolds))
            ),
            in_place = TRUE, copy = TRUE, by = "assemble_opts")
```

Then it points the selected samples at the set and resets `assemble_switch = 1` (:991-1003), and warns if the new set has no output directory on disk (:1009-1054). Note `req()` on every field: an empty text field blocks the save (INFERENCE: a MapToRef reference field must either be non-empty or be wrapped like app_annotate.R:1134 `if (nzchar(...)) ... else NA_character_`).

### 4.3 Coverage / details views that read assembler output

`app_assemble_coverage_details.R` builds `rv$asmb_dir <- file.path(dir_out, ID, "assemble", assemble_opts)` (:96-99) and reads `<ID>_assembly_<p>_coverageStats.csv` per path (:104-117, :618-626, :2201-2209) and `<ID>_assembly_0_coverageStats.csv` (:2062-2065, :2246), plus `..._<scaffold>_coverage.pdf` (:449-452). It never reads `assembler.log.txt`, `opts.txt` or `<ID>_summary.txt`. The "output" folder button opens the same directory (app_assemble.R:457-466, :1262 `open_path`). The Work Dirs browser (app_workdir_browser.R) lists task work directories from `.logs/nextflow.log*`.

### 4.4 File-upload patterns

None. `grep -rn "fileInput\|shinyFiles\|shinyFileChoose\|shinyDirChoose" R/` returns nothing; shinyFiles is not in DESCRIPTION Imports (DESCRIPTION Imports block, lines 17-64). Every path (seeds_db, labels_db, mf_db, find_mitofinder_db, annotate mitofinder_db, MITOS ref_dir) is a plain `textInput` holding a local absolute path or URL (app_assemble_utils.R:323-365; app_assemble_utils_userAsmb.R:734-742; app_annotate_utils.R:427-431). The vignette tells users to give absolute paths, not `~` (Your-Own-Project.Rmd:185-186). INFERENCE: the house pattern for a reference .gb/FASTA is a textInput path/URL stored in `assemble_opts`, staged by Nextflow; per-sample references would follow the mapping-file column pattern (`samples.assembly`) instead.

### 4.5 Run modal

`app_run_pipline.R:93-101` counts samples with `assemble_switch %in% c(1, 4) | (join_switch == 1 & assemble_lock == 0)`; the progress board keys processes by leaf name (`MITOPILOT_PROCESS_ORDER`, :8-15) but by frame position for identity (:5-7). A new PROCESS name would need adding to that vector for ordering only; a new branch inside `assemble` needs nothing.

---

## 5. Backwards compatibility, export, annotation

### 5.1 Migration pattern (R/backwards_compatibility.R)

Two-part idempotent pattern. (a) Column adds, e.g. :1253-1271:

```r
  # if assembler column doesn't exist, add it
  if(!("assembler" %in% names(assemble_opts_table))){
    message("added 'assembler' column to assemble_opts table")
    assemble_opts_table$assembler <- rep("GetOrganelle", nrow(assemble_opts_table))
    glue::glue_sql("ALTER TABLE assemble_opts ADD COLUMN assembler TEXT", .con = con) |> DBI::dbExecute(con, statement = _)
    dplyr::tbl(con, "assemble_opts") |> dplyr::rows_upsert(assemble_opts_table, in_place = TRUE, copy = TRUE, by = "assemble_opts")
  }
```

The shorter modern form is :435-440 (`ALTER TABLE ... ADD COLUMN join_scaffolds INTEGER` then `UPDATE ... SET join_scaffolds = 0 WHERE join_scaffolds IS NULL`). (b) The "already current" early-exit gate at :170-276 lists every column/table the current schema needs (`"assembler" %in% names(assemble_opts_table)` at :176, `"mitofinder_db"` :177, `"mitofinder"` :178, `max_paths/max_scaffolds` :206-207). A new column must be added to BOTH the add-step and this gate, else old projects print "nothing to update" (:274) and skip it. The DB is backed up to `.old_sqlite_dbs/.sqlite.N` before any write (:278-299). `.config` is regenerated wholesale via `migrate_config()` (:36-45).

Tests: `expect_cols(con, "assemble_opts", c("assembler", "mitofinder_db", "mitofinder", "max_paths", "max_scaffolds"))` (test-backwards-compatibility.R:431-433) against fixtures `create_v100_db()` / v1.3.10 (:402, :465); legacy fixture tables at :71-75 and :126-133.

The app server also patches `assemble` BLAST columns on start (app_server.R:90-103) but nothing for `assemble_opts`.

### 5.2 Export (R/export.R)

Assembler-agnostic. `check_single_path()` refuses to export a sample with more than one path regardless of assembler (:12-28; the comment at :3 explains GetOrganelle paths as the motivation). Gaps (`N` runs) are handled generically "whatever put it there: a reference-guided join, an assembler, or a sequence the user supplied" (:98-102, `find_sequence_gaps` :110). No `assembler` column is read anywhere in export (grep in Section 3.5).

### 5.3 Annotation (R/annotate*.R)

Does not know the assembler. `annotate()` reads the FASTA and the coverageStats CSV (annotate.R:85, :101-102), decides rotation/wrap-around from the word `circular` in the header (annotate.R:113-114, :128, :150, :167, :258, :380), extracts scaffold numbers from the header with `(?<=\.)\d+(?=\s|$)` (annotate.R:93), and runs `coverage_trim()` on linear contigs (annotate_coverage_trim.R). The `annotations.tool` column ("MitoFinder", "MITOS2", ...) refers to the ANNOTATION tool (annotate.R:372, constants.R:73-77), not the assembler.

---

## 6. Container (docker/Dockerfile, 123 lines)

Base: `condaforge/mambaforge:24.9.2-0` (:1). apt: `jq parallel default-jre python2.7 build-essential automake autoconf` (:8-16). conda: `r-base=4.5.2 r-reticulate r-remotes r-ragg` (:24). Pinned bioconda installs (:27-36):

| Tool | Version | Where |
|---|---|---|
| fastp | 0.23.4 | base env |
| spades | 4.1.0 | base env |
| getorganelle | 1.7.7.1 | base env |
| bam-readcount | 1.0.1 | env `bam-readcount` |
| bowtie2 | 2.5.4 | base env |
| samtools | 1.21 | base env |
| minimap2 | 2.28 | base env |
| trnascan-se | 2.0.12 | env `trnascan-se` |
| mitos | 2.1.10 | env `mitos` |
| aragorn | 1.2.41 | env `aragorn` |
| BLAST+ (blastn/blastdbcmd/makeblastdb) | >= 2.16, transitive | base env, asserted at :48-57 |
| MitoFinder | git HEAD of RemiAllio/MitoFinder, `./install.sh` | /opt/MitoFinder on PATH (:91-94) |
| ARWEN 1.2.3 | compiled from bundled C (:97-99) | /opt/arwen |
| NCBI ORFfinder | binary download | env `orffinder` (:109-113) |
| R packages | renv.lock via `renv::restore()` (:86-88) | includes Biostrings, pwalign, DECIPHER, IRanges, XVector, ape, msaR, reticulate |

NOT present in the Dockerfile: bwa, bwa-mem2, bcftools, seqkit, seqtk, bbmap, pilon, ivar, mafft, muscle, pysam, biopython (unknown: MitoFinder's install.sh and the bioconda getorganelle solve may pull Python deps transitively; not verifiable from the repo). Python 3 exists in the mambaforge base; python2.7 is for MitoFinder. INFERENCE (not verifiable from the repo): samtools 1.21 includes `samtools consensus`, `samtools depth`, `samtools mpileup`, `samtools ampliconclip`, so a BAM-to-consensus step needs no bcftools.

Existing in-repo mapping/consensus code paths: bowtie2 local mapping + bam-readcount per-base counts (coverage.R:62-82), bowtie2 + SAM parsing in R (circularize_asmb.R:550-571), minimap2 PAF for scaffold-vs-reference (scaffold_join.R:357-373, args `-x asm20 -k 13`, findable via `getOption("MitoPilot.minimap2")`), pwalign whole-genome alignment (`compute_blast_ref_alignment`, blast_ref_utils.R:1389), `samtools faidx` extraction (find_mito.R:433).

Image size hints: the BLAST DB layer alone is 835 MiB unpacked (:60, docker/README.md:84); an explicit `mamba install blast` cost "a 1.08 GB layer" and was removed (:44-46); a full rebuild is 30-60 min (docker/README.md:83). Tag pinning: image tag must equal `DESCRIPTION Version:` (docker/README.md:5-8); `new_project()` writes `macguigand/mitopilot:<version>` into `.config` `process.container` (init_project.R:54, :146); `container_version_gap()` warns on mismatch (generate_config.R:429-440); deploy scripts `docker/deploy-{local,dockerhub,aws}.sh` delete stale `docker/MitoPilot_*.tar.gz` before `devtools::build(path="docker")` and `docker build -f docker/Dockerfile` (deploy-local.sh:23-28). `.github/workflows` has only R-CMD-check and pkgdown; images are built by hand.

Tool help capture: `tools/capture_tool_help.sh` runs each tool's `--help` inside the image and writes `inst/tool_help/<tool>.txt` (TOOLS array at :34-43; README at inst/tool_help/README.md).

---

## 7. Tests and docs

### 7.1 Tests touching assembler plumbing (tests/testthat/)

No test executes assemble.nf or coverage.nf. What exists:
- test-backwards-compatibility.R: legacy fixtures and `expect_cols()` on `assemble_opts` (:71-75, :126-133, :431-433, :493), idempotency (:285, :581), config regeneration (:678).
- test-generate-config.R: profiles/templates (:8-78).
- test-coverage-per-scaffold.R: pure helpers (:5-80) and a full `coverage()` run using a fake aligner bin on PATH (`fake_aligner_bin`, :140-145) so bowtie2/samtools/bam-readcount are not required; also the coverage awk (:216-262).
- test-assemble-summary-refresh.R: `refresh_assemble_summary()` and the WF1 length-string convention (:66).
- test-find-mito.R, test-scaffold_join.R, test-circularize-asmb.R: pure R units with fake binaries where needed (:239-268 shows a fake `mitofinder` script on PATH).
- test-init-db-userasmb.R:1 builds a DB and checks it; there is no equivalent `new_db()` schema test for the regular project (INFERENCE from grep: no test calls `new_db(` outside userAsmb).
- test-project-consistency.R: stale-directory logic.

Plans in the repo require `Rscript -e 'devtools::test()'` (baseline `FAIL 0 | PASS 1665 | SKIP 23` at the time) and `nextflow lint` with one pre-existing error in main.nf:3 (tools/userasmb_scaffold_join_implementation_plan.md:22-23).

### 7.2 Vignettes documenting assembler options

- vignettes/Test-Project-Assemble.Rmd:142-168: the modal walkthrough, one bullet per control ("Assembler.", "Seeds and Labels databases.", "MitoFinder reference database.", "Max assembly paths / Max scaffolds.", "Automatically join ...").
- vignettes/Difficult-Assemblies.Rmd:16-23: "GetOrganelle explores an assembly graph and can return several alternative paths ... MitoFinder returns a single path, but can still report several scaffolds"; :43-47 "Multiple paths come only from GetOrganelle".
- vignettes/custom_dbs.Rmd:31-35 ("What parts of the MitoPilot pipeline use reference databases?"), :37-147 build sections per tool.
- vignettes/Your-Own-Project.Rmd:167-192 ("Arguments worth setting": `custom_seeds_db`/`custom_labels_db`, `assembler` and `mitofinder_db`).
- README.md:29-31, :168. Articles are listed in `_pkgdown.yml:42-54`. NEWS.md uses `# MitoPilot x.y.z` / `## New Features` / `### <feature>` with user-facing bullets (NEWS.md:1-50).

### 7.3 Planning-document house style

Design docs (tools/*_design.md, e.g. tools/userasmb_scaffold_join_design.md, 226 lines): `# Title`, `## Problem`, `## What is already in place`, `## What is missing`, `## Design decisions` (sub-headed per decision), `## Changes` (numbered: Schema, Channels, Wiring, App plumbing, Test sample), `## Testing`, `## Out of scope`. The larger tools/local_blast_db_design.md adds `## Summary`, `## Motivation`, `## Inclusion criteria`, `## Build pipeline`, `## Delivery and refresh`, `## Integration with MitoPilot`, `## Alternatives considered`, `## Open questions`, `## Appendix`.

Implementation plans (tools/*_implementation_plan.md, dev/plans/*.md): a header block (`> For agentic workers: ...`, `**Goal:**`, `**Architecture:**`, `**Tech Stack:**`, `**Spec:**`), `## Global Constraints` (ASCII only, minimal comments, never push, no attribution, branch, test baseline, nextflow lint), `## File Structure` (Modified / Created lists with one-line purposes), then `### Task N: ...` each with **Files:** (with line ranges), **Interfaces:**, checkbox steps, and a closing `## Self-Review` (tools/userasmb_scaffold_join_implementation_plan.md:1-60, :949). tools/local_blast_db_implementation_plan.md adds `## 0. What was verified in this environment`, `## 9. Behaviours that must be preserved`, `## 12. Rollback`, `## 13. Decisions for the maintainer` (D1..D9), `## 14. Risks and unknowns`.

dev/specs/*.md carry a `Date:` / `Branch:` / `Status:` header, then `## Goal`, `## Tool survey`, `## Pipeline` or `## Algorithm`, `## Data model`, `## App`, `## Testing`, `## Out of scope` (dev/specs/2026-08-24-userasmb-find-mitogenome-design.md:1-50). dev/ is gitignored (.gitignore line `dev`), tools/ is tracked.

---

## 8. DATA CONTRACT a new assembler process must satisfy

Everything below is what the unchanged rest of WF1/WF2/app requires. If a MapToRef branch produces exactly this, nothing after `coverage` changes.

A. Directory: every output lives under `<ID>/assemble/<opts_id>/` relative to the task dir (assemble.nf:22-23); publishDir copies it to `out/<ID>/assemble/<opts_id>/`.

B. Files declared by the `assemble` output block (assemble.nf:19), all mandatory or the task fails on missing outputs:
1. `<ID>_assembly_<N>.fasta`, N >= 1, one file per assembly PATH (alternative reconstruction). N=1 for a single reference. `<ID>_assembly_0.fasta` is reserved: it means "no assembly" when it contains `>No assembly found` (assemble.nf:52), and it is the joined/edited Path 0 elsewhere (scaffold_join.nf:44). Files matching `assembly_0.fasta` are dropped by the summary (assemble_workflow.nf:215) and by COVERAGE (coverage_workflow.nf:13).
2. `<ID>_reads.tar.gz`: whatever the matching coverage.nf branch will untar. Free to define for a new branch; existing branches expect `extended_{1,2}_paired.fq` + `extended_*_unpaired.fq` (strip 2) or `<ID>_preprocess_R{1,2}.fastq.gz`.
3. `<ID>_summary.txt`: free-form; MitoFinder writes an empty file (assemble.nf:77). Only GetOrganelle's is parsed, and only inside the GetOrganelle branch (:48).
4. `assembler.log.txt`: the tool log (assemble.nf:39, :72).
5. `NF_work_dir_assemble.txt`: two lines, "Nextflow assemble working directory:" and `$PWD` (assemble.nf:43-44).
(`opts.txt` is written but not declared, so not published.)

C. FASTA record naming, the one contract used everywhere: header `>ID.path.scaffold topology` with `path` = the N in the file name, `scaffold` = 1..k within the file, and `topology` exactly `circular` or `linear` (assemble.nf:54, :96, :100). Parsed by `record.id.split('\\.')` and `record.desc` (assemble_workflow.nf:320-323), by `SeqId.split('\\.')` (coverage_workflow.nf:50), by `str_detect(names, "circular")` (coverage.R:242, annotate.R:113-380), by `(?<=\.)\d+(?=\s|$)` (annotate.R:93), by BLAST qseqid parsing (blast_genbank_workflow.nf:143-147), and by `get_assembly()` when rebuilding names from the DB (app_assemble_utils.R:741-745). Sample IDs are restricted to `[a-zA-Z0-9_:-]` and 18 chars (init_db.R:136-149), so `.` is safe as the separator. Sequence letters: anything Biostrings accepts; `N`/IUPAC are counted as ambiguities (assembly_path_scoring.R:56, annotate_utils.R:872) and `N` runs >= 10 become gap features on export (export.R:110).

D. Tuple element [7] `val("${opts.assembler}")` must equal the string the coverage.nf branch tests for (coverage.nf:35, :40). Elements [8]/[9] pass max_paths/max_scaffolds through unchanged.

E. Coverage outputs (produced by `MitoPilot::coverage()` if the new coverage.nf branch calls it): `<ID>_assembly_<N>_coverageStats.csv` with columns `SeqId, Position, Call, Depth, Correct, ErrorRate, MeanDepth, GC` (`#`-prefixed values allowed in ErrorRate/MeanDepth), one row per base per scaffold, `SeqId` = `ID.path.scaffold`; plus `<ID>_assembly_<N>_coverage.csv`, `.bam`, `_<k>_coverage.pdf`. WF2 stages `<ID>_assembly_<path>_coverageStats.csv` by name (annotate_workflow.nf:61-64).

F. DB rows the driver writes for you (no change needed if A-D hold): `assemble` (paths, scaffolds, length "l1;l2", topology, assemble_switch 4/2/3, assemble_notes, time_stamp), `assemblies` (ID, path, scaffold, length, length_raw, topology, time_stamp, sequence, ignore, edited=0), `annotate` (one row per non-ignored unit). Coverage then fills `assemblies.depth/gc/errors`. BLAST fills `assemble.blast_*` and `assemblies.blast_*`.

G. Failure signalling: emit `<ID>_assembly_0.fasta` containing `>No assembly found` and exit 0 to get state 3 / "failed assembly"; a non-zero exit is ignored (no DB write) and only surfaces as the run's final exit code.

H. Resource keys: the `opts` map must carry `cpus` and `memory` (config closures), and the process must be given `path()` inputs for any reference file so it is staged into the container.

---

## 9. TOUCH-POINT CHECKLIST for adding "MapToRef"

Ordered roughly as data flows. "Must" = the pipeline or app breaks without it.

Schema and defaults
1. R/init_db.R:67-86 (must): new `new_db()` args, e.g. `maptoref_ref` (path/URL to the reference .gb or FASTA) and `maptoref` (free-form option string, mirrors `getOrganelle`/`mitofinder`), with roxygen at :45-48.
2. R/init_db.R:130-133 (must): extend the validation vector to include `"MapToRef"`.
3. R/init_db.R:311-350 (must): new columns in `CREATE TABLE assemble_opts` (e.g. `maptoref_ref TEXT, maptoref TEXT`, plus any numeric knobs) and in the seed `data.frame`.
4. R/init_project.R (optional): a `custom_*` convenience arg like `custom_seeds_db` (:27-28, :55-56, :130-131), or rely on `...` passthrough to `new_db()` (:132).
5. R/backwards_compatibility.R:1253-1313 (must): add-column blocks for each new column, and R/backwards_compatibility.R:170-272 (must): extend the "already current" gate; :11-15 roxygen list.
6. R/init_db_userAsmb.R:358-371: NOT needed (userAsmb has no assembler); leave alone.

Nextflow
7. inst/nextflow/modules/assemble_workflow.nf:6-20 (must): add the new columns to `params.sqlRead`; :99-117 (must): add them to the opts map and, for the reference FILE, as a separate tuple element; :183-195 (must): thread the new element through `assemble_in_full`.
8. inst/nextflow/modules/assemble.nf:16 (must): add a `path(ref)` input (use `${projectDir}/assets/NO_FILE` when unset, as annotate_workflow.nf:92 does); :56-101 (must): a new `elif [ "!{opts.assembler}" = "MapToRef" ]` branch that writes the five files of Section 8B, stamps headers per 8C, and emits the sentinel on failure.
9. inst/nextflow/modules/coverage.nf:35-43 (must): a new `elif` that untars the MapToRef reads archive and calls `MitoPilot::coverage(...)` with the right read names.
10. inst/config.* (optional): nothing required; add a `params.maptoref { }` block only if the branch needs its own executor/cpus (pattern find_mito.nf:6-7, config.local:55-59). If added, all eight templates plus `migrate_config()` placeholders must stay consistent (generate_config.R:180-190 does not need changes for a plain block).
11. inst/nextflow/main.nf: no change if MapToRef is a branch inside `assemble`. Needed only if it becomes its own process/workflow (then also R/app_run_pipline.R:8-15 for board ordering).

R helper(s)
12. New R function(s) (e.g. R/map_to_ref.R) if the branch calls `Rscript -e "MitoPilot::..."` like coverage/circularize/find_mito do; export in NAMESPACE via roxygen `@export`. Reusable pieces listed in Section 10.

App
13. R/app_assemble_utils.R:293 (must): `choices = c("GetOrganelle", "MitoFinder", "MapToRef")`; :303-309 help sentence; new `textInput`s after :365 following the `mf_db` pattern (nested help, `shinyjs::disabled()`); :441-448 (must): initial show/hide for the third case.
14. R/app_assemble.R:847-870 (must): `updateTextAreaInput` for the new fields; :873-885 and :944-958 (must): show/hide branches; :889-900 (must): `toggleState` lines; :963-983 (must): new columns in the `rows_upsert` data.frame (fields missing here would be NULL on newly created sets); :125-126 (optional): `register_tool_help("<tool>", ...)` if a `?` icon is wanted.
15. inst/tool_help/<tool>.txt + tools/capture_tool_help.sh:34-43 (optional): captured `--help` for whatever mapper is used.
16. R/app_assemble_coverage_details.R: no change required. Optional cosmetic: :2285 note text names getOrganelle.

Docs and tests
17. vignettes/Test-Project-Assemble.Rmd:150-163, vignettes/Difficult-Assemblies.Rmd:16-23 and :43-47 ("Multiple paths come only from GetOrganelle" stays true), vignettes/custom_dbs.Rmd:31-35 (+ a section on preparing a reference), vignettes/Your-Own-Project.Rmd:188-192, README.md:29-31 and :168, NEWS.md.
18. tests/testthat/test-backwards-compatibility.R:431-433 (`expect_cols` for the new columns) and the v1.0.0/v1.3.10 fixture flows; a new test file for the R helper with fake binaries on PATH (pattern test-coverage-per-scaffold.R:140-145, test-find-mito.R:239-268); optionally a `new_db()` schema test.
19. docker/Dockerfile: only if a new tool is needed. Bowtie2, minimap2, samtools 1.21, bam-readcount, BLAST+ and SPAdes are already there (Section 6). docker/README.md and DESCRIPTION Version bump follow the normal release flow.
20. Test data: `ref_dbs/MitoFinder/NC_002333_Danio_rerio.gb` (single-record, circular) exists in the repo but is not packaged; `inst/test_data/fish_mito_sampler.gb` (10 records) is packaged. A packaged single-record reference for the fish test project would need adding under inst/test_data (or a URL default like the other DBs).

---

## 10. Existing helpers reusable for MapToRef

GenBank / reference parsing (pure R, no deps beyond Biostrings/stringr):
- `.cadb_parse_gb(gb_file)` (custom_assembly_db.R:487-552): splits a flat file on `//`, keeps `/organelle="mitochondrion"` records, extracts VERSION accession, DEFINITION, feature keys, first `/product`, and the ORIGIN sequence; also every CDS via `.cadb_record_cds()` (:560) and `.cadb_parse_location()` (:623-649, handles complement/join/order and `<`/`>`). `.cadb_grab_version()` (:696), `.cadb_grab_definition()` (:682). Note: it does NOT read the LOCUS `circular`/`linear` flag; that is a one-line addition (LOCUS line at ref_dbs/MitoFinder/NC_002333_Danio_rerio.gb:1 reads `LOCUS NC_002333 16596 bp DNA circular VRT ...`).
- `.cadb_write_mitofinder_db()` (:657-678): filters record blocks; template for "extract one record from a .gb".
- `.parse_ref_gff3()` (blast_ref_utils.R:~280-362): NCBI GFF3 -> typed feature table with `normalize_mito_gene()` (:978), topology from `Is_circular`, genetic code from `transl_table` (:333-334). Useful if the reference's annotations should seed `blast_ref_annotations` (init_db.R:708-718) so the synteny/reference views work with a user reference.
- `fetch_blast_ref()` / `fetch_blast_refs()` (blast_ref_utils.R:146, :554): NCBI efetch of GFF3 + FASTA + taxonomy for an accession, writes `blast_ref_annotations.csv`, `blast_ref_sequence.txt`, `blast_ref_genetic_code.txt`, `remote_blast_ref.json` (blast_ref_fetch_workflow.nf:124-127). Precedent for "reference by accession" instead of by file.
- `.parse_ref_fasta()` (blast_ref_utils.R:392), `.split_fasta_by_accession()` (:444).

FASTA IO and headers: `Biostrings::readDNAStringSet`/`writeXStringSet` everywhere (coverage.R:19, :56; annotate_mitofinder.R:84); header stamping awk (assemble.nf:54; coverage_userAsmb.nf:50-56 with a topology map); `get_assembly()` rebuilds `ID.path.scaffold topology` names from the DB (app_assemble_utils.R:732-746); `extract_contigs()` via `samtools faidx` (find_mito.R:429-436); `rc_seq()` (scaffold_join.R:823).

Mapping and per-base evidence: `coverage()` (coverage.R) is the canonical bowtie2 + bam-readcount runner and CSV writer; `.coverage_extend_circular()` / `.coverage_reform_circular()` (:250-287) implement the circular junction construct; `count_junction_reads()` (circularize_asmb.R:522-599) shows bowtie2 + `samtools view` + R-side SAM/CIGAR parsing (`cigar_ref_length`, :611); `run_minimap2_paf()` / `parse_paf()` (scaffold_join.R:357-433) for contig-vs-reference placement; `compute_blast_ref_alignment()` (blast_ref_utils.R:1389) for pwalign whole-genome alignment; `find_end_overlap()` (circularize_asmb.R:345) for self-BLAST.

Consensus building in R: `build_resolved_sequence()` / `.majority_row()` / `iupac_code()` (assembly_path_scoring.R:409-468) already implement per-column majority with IUPAC fallback over an alignment matrix; `overlap_consensus()` (scaffold_join.R:946) does depth-weighted base picking; `find_sequence_gaps()` (export.R:110) finds N runs.

File staging / options: the `path()`-input + `NO_FILE` placeholder pattern (annotate_workflow.nf:92, inst/nextflow/assets/NO_FILE); Groovy null-defaulting idioms (`it[13] == null ? 500 : (it[13] as Integer)`, assemble_workflow.nf:118); the per-stage `params.<stage>?.executor ?: params.assemble.executor` fallback (find_mito.nf:6-7); topology-map heredoc (coverage_userAsmb.nf:40-42).

DB migrations: the `ALTER TABLE ... ADD COLUMN` + `UPDATE ... WHERE ... IS NULL` idiom (backwards_compatibility.R:435-440) and the upsert-with-defaults idiom (:1253-1271); `expect_cols()` in tests.

Sequence tools already in the image and called from R/nf: bowtie2 (coverage.R:63-76), samtools view/sort/faidx (coverage.R:67, find_mito.R:433), bam-readcount via `conda run -n bam-readcount` (coverage.R:81), minimap2 (scaffold_join.R:373), blastn (circularize_asmb.R:358, find_mito.nf:50), spades (installed, not called directly by MitoPilot code; used by GetOrganelle).

---

## 11. Gotchas and open questions surfaced by the map

1. `assemble_opts` is per option set, so "one reference per project, possibly per sample" has two homes: option-set column (shared) vs a `samples`/`assemble` column (per sample). The only per-sample file precedent is `samples.assembly` from the mapping CSV (userAsmb). Mixing both (set-level default, per-sample override column read with COALESCE in `params.sqlRead`) has no precedent but fits the SELECT/opts-map pattern.
2. `INSERT OR REPLACE INTO annotate` in the regular ASSEMBLE driver (assemble_workflow.nf:421) nulls unlisted annotate columns; the userAsmb driver already uses `ON CONFLICT DO UPDATE` (coverage_userAsmb_workflow.nf:259-266). Not MapToRef-specific, but any change near the seeding should not copy the older form.
3. The reads tarball: MitoFinder's `tar -czvf ... *.fastq.gz` archives the STAGED preprocess inputs (assemble.nf:80, after `cd ../..` at :69). Nextflow stages inputs as symlinks on local executors; tar without `-h` stores symlinks. INFERENCE: this works today only because the symlink targets are reachable when coverage runs; a MapToRef branch archiving mapped reads (e.g. a name-sorted BAM or the mapped FASTQ subset) would sidestep the question and shrink the archive.
4. `coverage.nf` re-maps ALL reads with bowtie2 for depth. For MapToRef the mapper already produced a BAM; re-mapping is redundant but keeps `coverage()` and its CSV contract untouched. Alternative with no new tool: archive the mapped-read FASTQ subset (like GetOrganelle's `extended_*.fq`) so coverage stays cheap.
5. Topology for a MapToRef consensus must be decided by the branch (reference LOCUS flag, user option, or read-spanning evidence like circularize_asmb.R's junction test) and stamped as `circular`/`linear`. `partial` in `annotate` derives from it (assemble_workflow.nf:418).
6. `new_db()` roxygen typo "GetOrgnalle" (init_db.R:45) and the `mf_db` input id vs `mitofinder_db` column (app_assemble_utils.R:324, app_assemble.R:973) are existing inconsistencies to mirror deliberately or fix in passing.
7. `req()` on every modal field blocks saving a set whose hidden fields are empty (app_assemble.R:966-974); today every set carries all five tool fields with defaults, so adding a field whose default is empty needs the `nzchar`/`NA_character_` guard.
8. Nothing in the app displays `assembler.log.txt` or `<ID>_summary.txt`; MapToRef diagnostics (mapped read count, reference coverage %, mean identity) would need either a new `assemble` column (pattern: `find_mito_notes`, find_mito_workflow.nf:3-5) or a place in `assemble_notes`, which coverage/BLAST overwrite (comment at find_mito_workflow.nf:3-4).
9. `_pkgdown.yml` article list and `NEWS.md` are the doc registration points; `dev/` is gitignored while `tools/` is tracked, so a design doc meant to persist belongs in tools/.
