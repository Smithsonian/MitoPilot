# Codebase map: how GetOrganelle and MitoFinder are plumbed through MitoPilot

Repo: /home/dmacguig/Documents/GitHub/MitoPilot, branch map-to-ref-assembly, package version 1.5.4 (DESCRIPTION:3).
All line numbers are from the working tree on 2026-09-02. Read-only investigation; nothing was changed.

Legend used below: "SAYS" = verbatim or near-verbatim from the source; "INFER" = my reading of it.

---

## 0. One-paragraph orientation

An assembler in MitoPilot is nothing more than (a) a string value in `assemble_opts.assembler`, (b) an `if/elif` branch in the shell block of ONE Nextflow process (`inst/nextflow/modules/assemble.nf`), (c) a matching `if/elif` branch in ONE coverage process (`inst/nextflow/modules/coverage.nf`), and (d) a handful of option columns on `assemble_opts` that the app modal shows or hides depending on the selected assembler. Everything downstream (coverage stats, BLAST, reference fetch, scaffold join, annotation, curation, export) is assembler-agnostic and keys only on the published file layout `out/<ID>/assemble/<assemble_opts>/<ID>_assembly_<path>.fasta` plus `<ID>_assembly_<path>_coverageStats.csv`, the FASTA header convention `>ID.path.scaffold circular|linear`, and the rows written to `assemblies` / `assemble` / `annotate`. A third assembler therefore touches a short, well-defined list of files (Section 9) and must satisfy one data contract (Section 10).

---

## 1. Project init and options (R side)

### 1.1 Where the assembler choice lives

- Table `assemble_opts`, column `assembler TEXT` (R/init_db.R:313-328). It is PER PARAMETER SET, not per sample. Samples point at a set through `assemble.assemble_opts TEXT` (R/init_db.R:268), default `"default"` (R/init_db.R:298). Any sample can be moved to a different set in the app, so "per sample" assembler choice = one parameter set per sample.
- `new_db()` argument `assembler = "GetOrganelle"` (R/init_db.R:70) and the validator (R/init_db.R:130-133):

```r
  # Validate assembler choice
  if (assembler %nin% c("GetOrganelle", "MitoFinder")) {
    stop("Assembler not supported, valid options: [GetOrganelle, MitoFinder]")
  }
```

- Roxygen for the arg (R/init_db.R:45): `#' @param assembler Assembler, choice of "GetOrgnalle" (default) or "MitoFinder"` (typo in source).
- `new_project()` (R/init_project.R:46-62) forwards `custom_seeds_db`/`custom_labels_db` as `seeds_db`/`labels_db` and everything else via `...` to `new_db()` (R/init_project.R:125-133). So `assembler = "MitoFinder", mitofinder_db = "..."` is passed straight through `...`. Vignette confirms this usage (vignettes/Your-Own-Project.Rmd:188-192).
- `new_test_project()` (R/init_test_project.R:28-36) also forwards `...` to `new_project()` (R/init_test_project.R:121-132) with `min_depth = 500`.

### 1.2 assemble_opts schema (every column)

R/init_db.R:311-329, verbatim:

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

Default row (R/init_db.R:330-350): `assemble_opts = "default"`, `cpus = assemble_cpus` (6, R/init_db.R:68), `memory = assemble_memory` (24 GB, R/init_db.R:69), `join_scaffolds = 0L`, plus the values below.

### 1.3 Default option strings

- GetOrganelle (R/init_db.R:73-79):
  `"-F 'anonym' -R 10 -k '21,45,65,85,105,115' --larger-auto-ws --expected-max-size 20000 --target-genome-size 16500"`
- seeds_db (R/init_db.R:71): `https://raw.githubusercontent.com/smithsonian/MitoPilot/main/ref_dbs/getOrganelle/seeds/fish_mito_seeds.fasta`
- labels_db (R/init_db.R:72): `.../ref_dbs/getOrganelle/labels/fish_mito_labels.fasta`
- MitoFinder (R/init_db.R:81-83): `"--megahit"`
- mitofinder_db (R/init_db.R:80): `https://raw.githubusercontent.com/Smithsonian/MitoPilot/refs/heads/main/ref_dbs/MitoFinder/fish_mito_sampler.gb`
- max_paths 10, max_scaffolds 10, min_assembly_length 500 (R/init_db.R:84-86).
- If `seeds_db`/`labels_db` come in NULL they are re-defaulted to the fish URLs (R/init_db.R:151-160).

INFER: databases are URLs by default; they reach the container only because Nextflow `path()` inputs accept http URLs (Section 2.2). No R-side download happens.

### 1.4 Option-set key is also a directory name

SAYS (R/project_consistency.R:1-15): "The ASSEMBLE workflow publishes to `out/<ID>/assemble/<assemble_opts>/` (`inst/nextflow/modules/assemble.nf`), so the option-set name doubles as a directory name. Every downstream stage rebuilds this path from the current `assemble.assemble_opts` value." Helper `assemble_out_dir(dir_out, ID, opts)` (R/project_consistency.R:13-15); `assemble_dirs_on_disk()` (:25-35); `stale_assemble_dirs()` (:63) drives the app warning on open (R/app_server.R:132-160) and the "No assembly output for this parameter set" alert after re-assigning a sample (R/app_assemble.R:1010-1052). Also read by R/app_workdir_browser.R:253 (recovers opts_id from `<sample>/assemble/*` inside a Nextflow work dir) and scaffold_join_workflow.nf:205-212 (redo path).

### 1.5 userAsmb variants

- `new_db_userAsmb()` creates a MINIMAL `assemble_opts` (R/init_db_userAsmb.R:358-370): only `assemble_opts, min_assembly_length, join_scaffolds`; single row named `"user"` (:371-381). SAYS comment: "The regular pipeline schema carries assembler/getOrganelle/etc. fields that don't apply when assemblies are user-provided." The published directory is therefore `out/<ID>/assemble/user/` (coverage_userAsmb.nf:32).
- MitoFinder .gb lives elsewhere for userAsmb: `find_mito_opts.mitofinder_db` (R/init_db_userAsmb.R:465-479, :490).
- `new_project_userAsmb()` validates the .gb exists when `find_mitogenome = TRUE` (R/init_project_userAsmb.R:127-140) and normalizes the path.
- `new_test_project_userAsmb()` copies the packaged `inst/test_data/fish_mito_sampler.gb` into the project and passes it as `mitofinder_db` (R/init_test_project_userAsmb.R:96-115). INFER: this is the precedent for "ship a small reference inside the R package and copy it into the project".
- The userAsmb `samples` table carries arbitrary mapping columns; the pipeline reads `s.assembly` and `s.topology` from it (coverage_userAsmb_workflow.nf:5, :319-320) and resolves the file as `file(params.asmbDir + "/" + it[1])`. INFER: this is the only existing precedent for a PER-SAMPLE file path (a mapping-file column plus a `.config` directory param). The regular `new_db()` also stores every extra mapping column in `samples` (R/init_db.R:178-198), so a per-sample `Reference` column would already land in the DB with no schema change.

### 1.6 Config generation and container pin

- `new_project()` fills `<<CONTAINER_ID>>`, `<<RAW_DIR>>`, `<<ASMB_DIR>>` (= "NA" for regular projects), `<<MIN_DEPTH>>`, `<<NCBI_API_KEY>>` (R/init_project.R:144-152) via `fill_config()` (R/generate_config.R:50-58). Container default `paste0("macguigand/mitopilot:", utils::packageVersion("MitoPilot"))` (R/init_project.R:54).
- `resolve_config()` looks for a saved profile then `inst/config.<executor>` (R/generate_config.R:228-243). `generate_config()` (:323-412) writes cluster profiles with per-project tokens intact.
- `migrate_config()` regenerates `.config` from the template and ports rawDir/asmbDir/minDepth/ncbi_api_key/queue/penv/clusterOptions/container engine (R/generate_config.R:134-216). INFER: any NEW `params.*` block a third assembler needs (e.g. a `map_to_ref { }` block) must be added to EVERY template in `inst/config.*` (8 files: local, awsbatch, slurm, sge, pbs, lsf, NMNH_Hydra, NOAA_SEDNA) or the process must fall back with `params.x?.y ?: params.assemble.y` the way find_mito.nf:6-7 and circularize.nf:9-10 do.
- `container_version_gap()` (R/generate_config.R:429-440) warns in-app when `.config` container != installed package version (R/app_server.R:55-).

### 1.7 Custom assembly DBs (R/custom_assembly_db.R) and shipping

- `custom_assembly_db(clade, db_path, db_type = c("both","getorganelle","mitofinder"), ...)` (R/custom_assembly_db.R:60-74) downloads GenBank flat files via E-utilities (`.cadb_esearch` :321, `.cadb_efetch` :352) and builds:
  - GetOrganelle seed + label FASTAs (:160-231);
  - MitoFinder `.gb` = GenBank record blocks filtered to `/organelle="mitochondrion"` and DEFINITION containing "complete genome" (`.cadb_write_mitofinder_db` :657-678).
- REUSABLE GENBANK PARSER (pure R, no deps beyond Biostrings): `.cadb_parse_gb(gb_file)` (:487-552) splits on `//`, keeps mitochondrion records, counts features, pulls `/product`, extracts the ORIGIN sequence (`toupper(gsub("[^A-Za-z]", "", ...))` :524), and extracts every CDS via `.cadb_record_cds` (:560-616) using `.cadb_parse_location` (:623-649, handles `complement()`, `join()`, `order()`, `<`/`>`). `.cadb_grab_definition` (:682-692), `.cadb_grab_version` (:696-700). INFER: this is the in-house route to turn a user `.gb` reference into a FASTA (and gene intervals) inside the container without Biopython. Gap: it does not read the LOCUS line topology token ("circular"/"linear"); that would be a 2-line addition.
- The R function prints how to register the DBs (:753-769): paste paths into `new_project()` args or into the app options modal fields.
- Registration is a plain TEXT path/URL in `assemble_opts.seeds_db/labels_db/mitofinder_db`; there is NO upload mechanism. `grep -rn "fileInput\|shinyFiles" R/` returns nothing. The modal fields are `textInput`s (R/app_assemble_utils.R:323-332 for `mf_db`).
- Shipping: `ref_dbs/` at repo root (getOrganelle seeds/labels, MitoFinder/fish_mito_sampler.gb and NC_002333_Danio_rerio.gb, Mitos2 tarballs, validate BLAST db) is EXCLUDED from the R package (`^ref_dbs$` in .Rbuildignore) and NOT copied into the image (docker/Dockerfile:120-123, commented out: "not needed for >= v1.3.0, since dbs are downloaded directly from GitHub"). Defaults are fetched by URL at run time by Nextflow. The only reference shipped inside the package is `inst/test_data/fish_mito_sampler.gb` (for the userAsmb test project).
- Image build: docker/deploy-local.sh and deploy-dockerhub.sh run `devtools::build(path="docker")` then `docker build -f docker/Dockerfile .`; the Dockerfile `COPY docker/MitoPilot_*.tar.gz /pkg.tar.gz` (Dockerfile:116) with the "stale tarball" gotcha called out in the scripts (deploy-local.sh:21-24). `.dockerignore` is an allowlist (renv.lock + docker/ only).

---

## 2. Nextflow

### 2.1 Entry points and channel shapes

- `inst/nextflow/main.nf:31-86` WF1: `PREPROCESS()` -> `ASSEMBLE(PREPROCESS.out[0])` -> `COVERAGE(ASSEMBLE.out.cov)` (:38) -> `BLAST_GENBANK(ASSEMBLE.out.blast.map{ it -> tuple(it[0], it[1], it[4]) })` (:39) -> `BLAST_REF_FETCH(...)` (:43) -> `SCAFFOLD_JOIN(...)` (:84). `WF1_userAsmb` (:89-139) replaces ASSEMBLE with COVERAGE_userAsmb. `WF2` (:142-152) is shared.
- Launch command (R/update_mitopilot.R:11-41): `nextflow -log <proj>/.logs/nextflow.log run <inst/nextflow> -c <proj>/.config -entry WF1|WF1_userAsmb|WF2 [-resume]`; app runs it with processx and pins `NXF_VER` (R/app_run_pipline.R:295-320).
- PREPROCESS output tuple (preprocess.nf:15): `tuple val(id), path("${id}/${id}_preprocess_*"), env(after)`; files are `<ID>_preprocess_R1.fastq.gz` and `_R2` (preprocess.nf:25-26). Only samples with `assemble_switch IN (1, 4)` are preprocessed (preprocess_workflow.nf:4-7).

### 2.2 ASSEMBLE workflow (assemble_workflow.nf) : the DB read and the input tuple

sqlRead (assemble_workflow.nf:6-20), verbatim column order (positional indices matter everywhere below):

```
SELECT a.ID, a.assemble_opts, opts.cpus, opts.memory,
       opts.seeds_db, opts.labels_db, opts.getOrganelle, opts.assembler,
       opts.mitofinder_db, opts.mitofinder, s.genetic_code,
       opts.max_paths, opts.max_scaffolds, opts.min_assembly_length,
       b.run_blast, opts.join_scaffolds,
       a.join_switch, a.assemble_switch, a.blast_accession
FROM assemble a JOIN assemble_opts opts ... JOIN samples s ... LEFT JOIN blast_opts b ...
WHERE (a.assemble_switch IN (1, 4) OR a.join_switch = 1) AND a.assemble_lock = 0
```

multiMap (assemble_workflow.nf:98-133) builds `opts:` as
`tuple(ID, opts_id, [cpus: it[2], memory: it[3], getOrganelle: it[6], mitofinder: it[9], assembler: it[7]], [it[4], it[5]], it[8], it[10], max_paths, max_scaffolds)`.
Then reads are crossed in (:172-196) giving the process input:

```
tuple val(id), val(opts_id), path(reads), val(opts), path(dbs), path(mf_db), val(genetic_code), val(max_paths), val(max_scaffolds)
```
(assemble.nf:16). `dbs` is a 2-element list (seeds, labels); `mf_db` is a single path. Both are staged by Nextflow from a URL or local path because they are `path` qualifiers. INFER: adding a reference for a third assembler = add a column to sqlRead, a slot in the `opts:` tuple, a slot in the cross map, and a `path(ref)` in the process input; when the assembler is not MapToRef pass the `${projectDir}/assets/NO_FILE` placeholder, the pattern annotate_workflow.nf:92 uses for an absent MitoFinder db.

GOTCHA (config closure): every config template sets `process { cpus = { opts.cpus }; memory = { (opts?.memory?.GB ?: 1) * task.attempt } }` (inst/config.local:12-13; config.slurm:19-20 has NO null guard: `memory = { opts.memory.GB * task.attempt }`). `opts` is the PROCESS INPUT named `opts`. A new process without an input called `opts` carrying `cpus`/`memory` must set its own `cpus {}`/`memory {}` directives, as blast_genbank.nf:45-56 and coverage.nf:8-10 do (with the comment "a bare number would be read as BYTES").

### 2.3 The assemble process (assemble.nf)

- Directives: `executor params.assemble.executor`, `container params.assemble.container` (:5-6), `publishDir "${launchDir}/${params.publishDir}", overwrite: true, mode: 'copy'` (:8), `errorStrategy { task.exitStatus in 137..140 ? 'retry' : 'ignore' }` (:10), `maxRetries { params.assemble.maxRetries }` (:11; templates set 1).
- Output tuple (:19), verbatim:

```
tuple val("${id}"), path("${id}/assemble/${opts_id}/${id}_assembly_*.fasta"), path("${id}/assemble/${opts_id}/${id}_reads.tar.gz"), path("${id}/assemble/${opts_id}/${id}_summary.txt"), val("${opts_id}"), path("${id}/assemble/${opts_id}/assembler.log.txt"), path("${id}/assemble/${opts_id}/NF_work_dir_assemble.txt"), val("${opts.assembler}"), val(max_paths), val(max_scaffolds)
```
Positions: [0] id, [1] fasta or list, [2] reads tarball, [3] summary, [4] opts_id, [5] log, [6] workdir note, [7] assembler string, [8] max_paths, [9] max_scaffolds. Downstream consumers index these positions (coverage_workflow.nf:13-21 uses it[0], it[1], it[2], it[4], it[7]; main.nf:39 uses it[0], it[1], it[4]).
- `workingDir = "${id}/assemble"`, `outDir = "${workingDir}/${opts_id}"` (:22-23).
- Switch: `if [ "!{opts.assembler}" = "GetOrganelle" ]; then ... elif [ "!{opts.assembler}" = "MitoFinder" ]; then ... fi` (:27, :56, :102). There is NO else branch: an unknown assembler value runs nothing and the task fails on missing outputs.

GetOrganelle branch (:27-55):
- `get_organelle_from_reads.py -1 reads[0] -2 reads[1] -o workingDir/ --overwrite -s dbs[0] --genes dbs[1] -t opts.cpus opts.getOrganelle` (:29-36).
- Logs: `cp get_org.log.txt outDir/assembler.log.txt`, `echo opts > outDir/opts.txt`, `summary_get_organelle_output.py workingDir -o outDir/${id}_summary.txt` (:39-41).
- Reads archive: `tar -czvf outDir/${id}_reads.tar.gz workingDir/extended*.fq` (:46), i.e. the recruited read subset `extended_1_paired.fq`, `extended_2_paired.fq`, `extended_*_unpaired.fq`.
- Topology parsed from the summary's `circular` column (:48) as one value for the whole sample.
- FASTA formatting (:49-55): every `*.fasta` in workingDir becomes `outDir/${id}_assembly_{#}.fasta` with headers rewritten by awk to `>${id}.{#}.{scaffoldCounter} ${topology}` (:54). Empty result: `echo ">No assembly found" > outDir/${id}_assembly_0.fasta` (:52).

MitoFinder branch (:56-101):
- `cd workingDir; mitofinder opts.mitofinder --ignore -j id -1 ../../reads[0] -2 ../../reads[1] -r ../../mf_db -o genetic_code.intValue() -p opts.cpus -m opts.memory` (:57-68). NOTE `genetic_code.intValue()` requires `samples.genetic_code` to be INTEGER (backwards_compatibility.R:118-121 comment).
- Logs: `cp workingDir/*_MitoFinder.log outDir/assembler.log.txt` (:72); `touch outDir/${id}_summary.txt # temporary placeholder summary file` (:77; the TODO at :75-76 says no summary exists for MitoFinder).
- Reads archive: `tar -czvf outDir/${id}_reads.tar.gz *.fastq.gz` (:80), i.e. the full preprocessed pair.
- FASTA (:87-101): finds `*_Final_Results/*mtDNA_contig*.fasta` (not `*genes*`); 0 files -> assembly_0 sentinel; >1 files -> concatenated into ONE path, all scaffolds forced `linear`, headers `>${id}.1.N linear` (:91-97); exactly 1 file -> topology from `.infos` line `Circularization: Yes` (:99), header `>${id}.1.1 topology` (:100). So MitoFinder always yields path 1 only (vignettes/Difficult-Assemblies.Rmd:45-46 documents this).

### 2.4 What ASSEMBLE does with the outputs (assemble_workflow.nf)

- Deletes previous rows: `DELETE FROM assemblies WHERE ID = ? AND time_stamp != ?` (:35, :204-211).
- Summarizes each sample by reading the published FASTAs in Groovy (:214-255): `n_paths = files.size()`, scaffolds per file counted from `>` lines, topology = header text after the first whitespace (:236-237), lengths from sequence text.
- Threshold branch (:260-267): `fail: (n_paths > max_paths) || (n_scaffolds > max_scaffolds)`.
- Pass status rules (:272-296): status `'4'` (BLAST pending) by default; `n_scaffolds > 1` -> note "Output contains disconnected contigs[ (fragmented)]"; `n_paths > 1` -> note "Unable to resolve single assembly from reads"; `max_len < min_assembly_length` -> status `'3'`; `run_blast == 0` -> status `'2'`.
- `assemble` row write (:87-88, :307-308): `UPDATE assemble SET paths=?, scaffolds=?, length=?, topology=?, assemble_switch=?, assemble_notes=?, time_stamp=?, poor_blast_ref=NULL WHERE ID=?`.
- `assemblies` rows via `splitFasta(record: [id: true, desc: true, seqString: true])` (:314-327): `record.id.split('\\.')` -> ID, path, scaffold; `record.desc` -> topology; `ignore = length < min_assembly_length` (:328-333). Writer is an UPSERT (:60-85) with the long comment (:37-59) explaining why `INSERT OR REPLACE` must not be used (it nulls every unlisted column and races the BLAST channel). VERBATIM head: `INSERT INTO assemblies (ID, path, scaffold, length, length_raw, topology, time_stamp, sequence, ignore, edited) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, 0) ON CONFLICT(ID, path, scaffold) DO UPDATE SET ...`.
- Failure records: threshold fail -> status `'3'` with message "N assembly paths, exceeds limit (M)" (:341-353); `assembly_0.fasta` sentinel -> `'3'`, "failed assembly" (:356-367); too few reads -> `'3'`, "Insufficient sequencing depth" (:370-387).
- Annotate seed per non-ignored unit (:405-421): `INSERT OR REPLACE INTO annotate (ID, path, scaffold, topology, partial, annotate_opts, curate_opts, orf_opts, annotate_switch, annotate_lock, reviewed) VALUES (..., 1, 0, "no")` with `partial = (topology == 'circular' || linear_complete == 1) ? 'no' : 'yes'` (:418). The userAsmb writer uses an upsert instead (coverage_userAsmb_workflow.nf:259-266). NOTE the comment at :311-313 and :395-399: `channel.fromQuery` snapshots the DB at session start, so this seed is built from the run's own channel, never from a query.
- Emits (:435-466): `cov` (status 4 or 2), `blast` (status 4 only), `join_eligible`, `join_expected`, `join_redo`.
- State codes (R/app_assemble.R:26-32): 0 Pre-Assembly, 1 Ready, 4 In Progress, 2 Success, 3 Failed. BLAST_REF_FETCH promotes 4 -> 2 (blast_ref_fetch_workflow.nf:74-78) or 3 on fetch failure (:65-69); BLAST no-hit -> 3 (blast_genbank_workflow.nf:70-80).
- failOnIgnore: every template ends with `workflow { failOnIgnore = true }` (inst/config.local:100-103; config.slurm:115-118), so an ignored task failure makes the whole run exit non-zero even though other samples finish (memory note "assemble_opts is a directory name" agrees).

### 2.5 Coverage (coverage.nf + coverage_workflow.nf + R/coverage.R)

- coverage_workflow.nf: filters out `assembly_0.fasta` (:12), maps to `tuple(ID, opts_id=it[4], reads=it[2], assembly list=it[1], assembler=it[7])` (:13-21), `.transpose(by: 3)` so each path is a separate task (:22).
- coverage.nf input (:23): `tuple val(id), val(opt_id), path(reads), path(assembly), val(assembler)`; output (:26-27): `tuple val(id), path("${outDir}/*"), path("${id}/assemble/${opt_id}/NF_work_dir_coverage.txt")`; `outDir = "${id}/assemble/${opt_id}"` (:30). Resources: `cpus {params.coverage.cpus}`, `memory { params.coverage.memory.GB * task.attempt }` (:8-10), i.e. NOT from `opts` (config.local:45-50 sets 4 cpus / 8 GB).
- The assembler switch (:35-42), verbatim:

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
No else: a third assembler value silently produces nothing and the task fails on the missing `NF_work_dir_coverage.txt`.

- `MitoPilot::coverage(assembly_fn, paired_reads_1, paired_reads_2, unpaired_reads, cpus, outDir)` (R/coverage.R:12-229):
  - Reads FASTA; scaffold ids = first token of header; circular ids = headers containing "circular" (:19-27; `.coverage_circular_ids` :241-243 uses `str_detect(seq_ids, "circular")`).
  - No-reads mode when `paired_reads_1 == "NA"` (:40-48) synthesizes the table (Depth/Correct/ErrorRate NA, GC only).
  - Circular scaffolds get their first 500 bp appended (:52, `.coverage_extend_circular` :250-259) and written to `<assembly stem>_working.fasta` (:53-56).
  - Mapping: `bowtie2-build ... index`; `bowtie2 --very-sensitive-local --no-unal -x index -1 R1 -2 R2 [-U unpaired] --threads cpus | samtools view -bS - | samtools sort - > <outDir>/<stem>.bam` (:58-77).
  - `conda run -n bam-readcount bam-readcount -w1 -f working.fasta bam > <outDir>/<stem>_coverage.csv` (:80-82), then parsed into per-base `SeqId, Position, Call, Depth, Correct, ErrorRate` (:85-105) and re-written as CSV (:148).
  - Seam depth folded back for circular scaffolds (:124, :267-287).
  - Rolling stats `.coverage_rolling_stats` (:295-333): `MeanDepth` (window 5), `MeanDepth_mask` (MAD outlier), `ErrorRate` (window 5), `ErrorRate_mask` (>0.05), `GC` (window 200, NA at edges).
  - Per-scaffold PDF `<stem>_<scaffold>_coverage.pdf` (:216-217).
  - FINAL `<stem>_coverageStats.csv` (:221-226) via `.coverage_stats_to_output` (:337-350): columns `SeqId, Position, Call, Depth, Correct, ErrorRate, MeanDepth, GC`; `MeanDepth` and `ErrorRate` are CHARACTER, prefixed with `#` when masked.
- coverage_workflow.nf then reads `*coverageStats.csv` rows (:28-53), keeps `SeqId, MeanDepth, GC, ErrorRate`, groups by SeqId, joins each series with spaces and writes `UPDATE assemblies SET depth = ?, gc = ?, errors = ?, time_stamp = ? WHERE ID=? and path=? and scaffold=?` (:3-4), the ID/path/scaffold coming from `SeqId.split('\\.')` (:47). It emits `cov_files` = per-ID list of coverageStats CSVs for the scaffold join (:56-63).

### 2.6 Downstream readers of the assembler's files (what a new assembler must keep working)

- BLAST (blast_genbank_workflow.nf:102-190): input `tuple(id, assembly_file_or_list, opts_id)`; path index from the file name regex `assembly_(\d+)\.fasta$` (:159-160); rewrites a target FASTA "preserving original headers `>{id}.{path}.{scaffold} topology` so qseqid ... parses back to (path, scaffold)" (:145-147); drops scaffolds shorter than `min_assembly_length`. Output published to `${id}/assemble/${opts_id}/blast_genbank_<path>.txt` (blast_genbank.nf:76-77).
- Reference fetch publishes `blast_ref_<accession>/{blast_ref_annotations.csv, blast_ref_sequence.txt, blast_ref_genetic_code.txt, remote_blast_ref.json}` (blast_ref_fetch_workflow.nf:124-127) into the same assemble dir; WF2 curate and ORF look for them there (curate_workflow.nf:46-56, orf_workflow.nf:74-75).
- Scaffold join (scaffold_join.nf:38, :43-60): consumes the path FASTA, the per-path coverageStats CSVs, the fetched reference; writes `${id}_assembly_0.fasta`, `${id}_assembly_0_coverageStats.csv`, `${id}_scaffold_mappings.csv`, `${id}_scaffold_junctions.csv`, `join_status.txt`, `join_note.txt`. R side `run_scaffold_join()` (R/scaffold_join.R:1581-1614) reads MeanDepth/GC/ErrorRate keyed on the scaffold field of SeqId and strips the `#` mask prefix.
- Annotation (annotate_workflow.nf:13-25, :40-64): sqlRead joins `assemblies` x `assemble` x `annotate` x `annotate_opts` x `samples`, gated on `assemble_lock = 1 AND ignore = 0 AND annotate_switch = 1 AND annotate_lock = 0`; the unit's files are `${publishDir}/${ID}/assemble/${assemble_opts}/${ID}_assembly_${path}.fasta` and `..._assembly_${path}_coverageStats.csv`. annotate.nf:24 input; `unit_assembly = "${id}_assembly_${path}_${scaffold}.fasta"` (:34). R `annotate()` reads `coverage_fn` (R/annotate.R:100-120) and `coverage_trim()` uses `MeanDepth > 10` / `> 15` thresholds (R/annotate_coverage_trim.R:39-63); circular handling keys on the header containing "circular" (R/annotate.R:113-114, :128, :150, :167, :258, :380).
- App: `assembly_coverage_details_server()` reads `<dir_out>/<ID>/assemble/<opts>/<ID>_assembly_<p>_coverageStats.csv` for path scoring (R/app_assemble_coverage_details.R:96-121) and MSA stats (:618-634); Path 0 writers put `_assembly_0.fasta` + `_assembly_0_coverageStats.csv` in the same dir (:1489-1497, :2236-2262). "Open output folder" opens that dir (R/app_assemble.R:1255-1262).

### 2.7 Other WF1 modules that follow the same seams (patterns to copy)

- `find_mito.nf` / `find_mito_workflow.nf`: an optional stage with its own `find_mito_opts` table, master toggle `attempt`, `params.find_mito?.executor ?: params.assemble.executor` fallback (find_mito.nf:6-7), publishDir `${id}/assemble/${assembler}` (:95), status/note files read back into SQL (`params.sqlFailFindMito`, find_mito_workflow.nf:9-10), evidence CSV upserted into `mito_candidates` (:22-36).
- `circularize.nf`: emits a `topology_map.txt` ("<contig> circular|linear") per sample (:1-66) which coverage_userAsmb.nf stamps into the headers (:44-56).
- `prepare_ref_db.nf`: extract-once-per-run pattern for a reference tarball (:296-343 of the combined listing; file lines 1-48).

---

## 3. Coverage and path scoring: what an assembly must look like

- Paths: `assemblies.path` integer >= 1 from the assembler; path 0 is reserved for the app/pipeline-built consensus or joined sequence (R/app_assemble_coverage_details.R:128 "Path 0 is the edited/consensus sequence, not a raw assembler path"; scaffold_join.nf:44). Export refuses >1 non-ignored path per sample (R/export.R:12-29; comment :3 "GetOrganelle paths are competing resolutions of one tangled graph").
- Scaffolds: each `(ID, path, scaffold)` is an annotation unit (annotate_workflow.nf:4-11). Multi-scaffold single path is "join eligible" (assemble_workflow.nf:427-433; R/scaffold_join.R:28 `scaffold_join_eligible`).
- Topology: header token must be literally `circular` or `linear` (coverage.R:242 `str_detect(..., "circular")`; assemble_workflow.nf:418; annotate.R:114). `unknown` is treated as linear by the userAsmb path (coverage_userAsmb.nf:53).
- Scoring (R/assembly_path_scoring.R): `score_assembly_paths(paths_df, cov_by_path, expected_len)` (:75-163) needs columns `path, scaffold, topology, length, sequence` plus optional `blast_*`; coverage frames need `Depth` and optional `ErrorRate`. Weights (:22-32): topology 2.0 (circular=1, else 0.4 :122), scaffolds 1.5, length 1.5, depth_even 1.5, depth_mean 1.0, error 1.0, blast 1.0, blast_conc 1.5, ambiguity 0.5 (`count_ambiguities` :56-60 counts non-ACGT). INFER: a reference-mapped consensus with IUPAC codes and Ns is scored, not rejected; NEWS.md (1.5.4 Bug Fixes) already says ambiguous bases "arrive ... from consensus assemblies called against a reference, which carry IUPAC codes at uncertain sites" and are translated to X with a warning.
- `grep -in "getorganelle|mitofinder"` across R/ and inst/ (411 hits, 33 files): in the ASSEMBLY sense, only assemble.nf, assemble_workflow.nf, coverage.nf, coverage_workflow.nf (passes the string), init_db.R, backwards_compatibility.R, app_assemble.R, app_assemble_utils.R, export.R:3 (comment), app_assemble_coverage_details.R:2285 (note text "multi-path getOrganelle output trimmed for consensus"), app_assemble_utils.R:727 (doc "assembly getOrganelle path"), vignettes, and inst/test_data/make_scaffjoin.sh:8. Every other hit is the MitoFinder ANNOTATOR (R/annotate_mitofinder.R, annotate.R:332-372, annotate_opts.use_mitofinder, constants.R:73-77 `tool %in% "MitoFinder"`), the find-mito confirm step (R/find_mito.R), or the custom DB builder. None of those care which assembler made the sequence.

---

## 4. Shiny app

- Module wiring: `assemble_ui("assemble")` under `input.mode == 'Assemble'` (R/app_ui.R:119-122); `assemble_server("assemble")` (R/app_server.R:257); userAsmb has its own pair (R/app_ui_userAsmb.R:113, R/app_server_userAsmb.R:217).
- The table (R/app_assemble.R:107-290) shows `assemble_opts` as "Assembly Opts." with a link that opens the modal (:176-181). The assembler NAME is not a table column; only the parameter-set name is visible. Data comes from `fetch_assemble_data()` (R/app_assemble_utils.R:7-130) which joins `assemble`, `preprocess`, `samples`, `assemble_opts(min_assembly_length)`, `assemblies`.
- Options modal `assemble_opts_modal()` (R/app_assemble_utils.R:221-460):
  - set picker `selectizeInput(ns("assemble_opts"), ..., create = TRUE)` (:229-241) + Edit checkbox (:246-252);
  - CPUs / Memory numeric (:258-273);
  - "Assembler" box (:276-366): `selectizeInput(ns("assembler"), choices = c("GetOrganelle", "MitoFinder"), ...)` (:288-300); `textInput(ns("mitofinder"))` with `tool_help_icon("mitofinder")` (:313-322); `textInput(ns("mf_db"), label = "MitoFinder Database:")` (:323-332); `textInput(ns("getOrganelle"))` (:333-343); `seeds_db` (:344-354); `labels_db` (:355-365). Help text is appended INSIDE each input container so hide/show carries it (:309-312 comment).
  - max_paths / max_scaffolds (:367-390), min_assembly_length (:396-410), join_scaffolds checkbox (:416-433).
  - Initial hide by assembler (:438-447): GetOrganelle hides `mitofinder`, `mf_db`; MitoFinder hides `getOrganelle`, `seeds_db`, `labels_db`.
- Server (R/app_assemble.R):
  - `register_tool_help("getOrganelle"/"mitofinder", input, reopen = ...)` (:125-126) wires the `?` popovers to `inst/tool_help/<tool>.txt` (R/help_utils.R:40-50, :58-); the txt files are generated by tools/capture_tool_help.sh (TOOLS array :31-40).
  - Open modal (:802-813); on set change repopulate every input and hide/show by assembler (:814-887); Edit toggles `toggleState` for each input id (:888-900); `observeEvent(input$assembler, ...)` hide/show (:944-956).
  - Save (:958-1000): `rows_upsert` into `assemble_opts` with an explicit data.frame of ALL columns (:963-976: assemble_opts, cpus, memory, getOrganelle, seeds_db, labels_db, assembler, mitofinder_db, mitofinder, max_paths, max_scaffolds, min_assembly_length, join_scaffolds); then `assemble` rows get the new set + `assemble_switch = 1` (:991-1001). INFER: a new column not listed here is written NULL for a NEW set.
  - Post-save warning when the new set has no directory on disk (:1010-1052).
- Coverage/details modal (R/app_assemble_coverage_details.R) is assembler-agnostic; reads from the published dir (Section 2.6).
- Progress board: `MITOPILOT_PROCESS_ORDER` (R/app_run_pipline.R:8-15) lists leaf process names for display ordering; a NEW process name would need adding there (a third branch inside `assemble` would not).
- File upload: none. All reference paths are typed. `session$userData$dir_out` is derived from `.config` `publishDir` (R/app_server.R:128-130).

---

## 5. Backwards compatibility, export, annotation

- `backwards_compatibility()` (R/backwards_compatibility.R:64-): reads tables (:84-90), builds a giant "already current" predicate that includes `"assembler" %in% names(assemble_opts_table) && "mitofinder_db" ... && "mitofinder" ...` (:176-178) and `max_paths`/`max_scaffolds` (:206-207). Migration pattern for an assemble_opts column, verbatim (:1253-1271):

```r
  # if assembler column doesn't exist, add it
  if(!("assembler" %in% names(assemble_opts_table))){
    message("added 'assembler' column to assemble_opts table")
    assemble_opts_table$assembler <- rep("GetOrganelle", nrow(assemble_opts_table))
    # add new columns to database
    glue::glue_sql(
      "ALTER TABLE assemble_opts
       ADD COLUMN assembler TEXT",
      .con = con
    ) |> DBI::dbExecute(con, statement = _)

    dplyr::tbl(con, "assemble_opts") |> # update SQL database
      dplyr::rows_upsert(
        assemble_opts_table,
        in_place = TRUE,
        copy = TRUE,
        by = "assemble_opts"
      )
  }
```
Same shape for `mitofinder_db` (:1273-1292, default = the fish sampler URL), `mitofinder` (:1295-1313, default `--megahit`), `max_paths` (:1316-1332), `max_scaffolds` (:1335-1351), `min_assembly_length` (:1354-1370). The terser two-statement form is used for `join_scaffolds` (:435-440): `ALTER TABLE ... ADD COLUMN join_scaffolds INTEGER` then `UPDATE ... SET join_scaffolds = 0 WHERE ... IS NULL`. `schema_gaps()` (:2130-2170) is the hard-stop list checked on app open (R/app_server.R:31-50); it does NOT currently include any assemble_opts column, so a missing new column would not block the app, it would just read NULL. Test fixture for migrations: tests/testthat/test-backwards-compatibility.R:100-139 (`create_v1310_db`) and the column expectations at :431-433.
- Export (R/export.R): assembler-agnostic. `check_single_path()` (:12-29) blocks multi-path export; `find_sequence_gaps()` (:110) declares N-runs "whatever put it there: a reference-guided join, an assembler, or a sequence the user supplied" (:101-103). Export reads topology per `assemblies` record (NEWS.md 1.5.4).
- Annotation (R/annotate.R, R/annotate_*.R, curate/validate cores): no dependence on `assemble_opts.assembler`. The only MitoFinder references are the optional MitoFinder ANNOTATOR (`annotate_opts.use_mitofinder`, R/annotate.R:332-372) and "non-standard MitoFinder gene" handling keyed on `annotations.tool == "MitoFinder"` (R/constants.R:73-77).

---

## 6. Container (docker/Dockerfile) and what is already installed

- Base `condaforge/mambaforge:24.9.2-0` (:1); apt: jq, parallel, default-jre, python2.7, build-essential, automake, autoconf (:8-17); channels defaults/conda-forge/bioconda (:19-22); `r-base=4.5.2 r-reticulate r-remotes r-ragg` (:24).
- Bioconda pins (:27-36): `fastp=0.23.4`, `spades=4.1.0`, `getorganelle=1.7.7.1`, `bam-readcount=1.0.1` (own env `bam-readcount`), `bowtie2=2.5.4`, `samtools=1.21`, `minimap2=2.28`, `trnascan-se=2.0.12` (env), `mitos=2.1.10` (env), `aragorn=1.2.41` (env).
- BLAST+ arrives transitively; build asserts `>= 2.16` (:40-57). Local BLAST DB `ADD docker/mito_metazoa_blastdb.tar.gz /ref_dbs/` (:82-84; 289 MB tarball, 835 MiB unpacked).
- `renv::restore()` from renv.lock (:86-88). MitoFinder `git clone https://github.com/RemiAllio/MitoFinder.git /opt/MitoFinder; ./install.sh` (:91-94) which bundles megahit, metaspades, idba, mitfi, arwen, blast under /opt/MitoFinder (verified by `ls /opt/MitoFinder` in the image). ARWEN compiled from source (:97-99). ORFfinder in env `orffinder` (:109-113). MitoPilot package last (:116-118).
- Image size: `docker images` shows `macguigand/mitopilot:1.5.4` at 15.3 GB.
- Probe of the local 1.5.4 image (docker run, 2026-09-02):
  - PRESENT: `bowtie2 2.5.4`, `samtools 1.21` (which includes `samtools consensus` and `samtools depth`), `minimap2 2.28-r1209`, `spades.py 4.1.0`, `blastn`, `python 3.12.7` (conda base), `python2.7`, GetOrganelle 1.7.7.1, MitoFinder 1.4.2, `/opt/MitoFinder/megahit/megahit`, `/opt/MitoFinder/metaspades/bin`.
  - MISSING: bwa, bwa-mem2, bcftools, seqkit, seqtk, megahit (on PATH), mafft, muscle, bbmap/bbduk, pilon, freebayes, ivar, vcfutils.pl, bedtools, mosdepth.
  - Python libs: Biopython NOT in base python3 and NOT in python2.7; Biopython 1.81 IS in the `mitos` conda env (`/opt/conda/envs/mitos/bin/python`). pysam absent.
  - R (renv.lock and in-image check): `Biostrings`, `pwalign`, `DECIPHER`, `msaR` present; `Rsamtools`, `GenomicAlignments` ABSENT.
- How the image tag is pinned: `.config` `process.container = '<<CONTAINER_ID>>'` (inst/config.local:11) filled with `macguigand/mitopilot:<packageVersion>` at init (R/init_project.R:54, :146); every `params.<stage>.container = process.container` (config.local:36-97). Version mismatch is only warned about (R/generate_config.R:429-440).
- In-container tool invocations already in R (reusable): bowtie2 + samtools sort (R/coverage.R:58-77; R/circularize_asmb.R:550-554), `samtools view` SAM parsing with a CIGAR reference-length helper (R/circularize_asmb.R:556-611), `samtools faidx -r names -o out` (R/find_mito.R:429-436), `minimap2 -x asm20 -k 13` PAF + parser (R/scaffold_join.R:357-432), blastn self-overlap (R/circularize_asmb.R:345), `conda run -n <env> <tool>` (R/coverage.R:81; R/annotate_utils.R:416), `mitofinder -a` assembly mode (R/find_mito.R:474-482).

---

## 7. Tests and docs

### 7.1 Tests touching assembler plumbing (tests/testthat)

- No test executes assemble.nf or coverage.nf; Nextflow is exercised only end-to-end via `new_test_project()` (13 SRA samples + MULTISCAFF + SCAFFJOIN, inst/test_data/mapping_test.csv) and `new_test_project_userAsmb()` (9 fixtures in inst/test_data/assemblies/).
- test-backwards-compatibility.R: `create_v1310_db()` fixture with `assembler/mitofinder_db/mitofinder` columns (:100-139); `expect_cols(con, "assemble_opts", c("assembler","mitofinder_db","mitofinder","max_paths","max_scaffolds"))` (:431-433). A new column needs a line here.
- test-coverage-per-scaffold.R: contract tests for `.coverage_circular_ids`, `.coverage_extend_circular`, `.coverage_reform_circular` (:1-60+), i.e. the header/topology convention.
- test-assembly_path_scoring.R: scoring on synthetic frames.
- test-project-consistency.R: `stale_assemble_dirs` fixtures (:1-40).
- test-find-mito.R: shows how an external binary is STUBBED with a shell script on PATH (:248-255) and how MitoFinder output layouts are faked (:188-236). Good template for testing a wrapper around a mapper without running it.
- test-generate-config.R: profile generation (:1-30).
- Fixtures dir: tests/testthat/fixtures/{mitos2_empty, mitos2_genes}.

### 7.2 Vignettes that document assembler options

- vignettes/Test-Project-Assemble.Rmd:142-176: the options modal walk-through (Assembler, Seeds/Labels, MitoFinder db, max paths/scaffolds, join toggle, named sets).
- vignettes/Difficult-Assemblies.Rmd:16-23, :45-47: paths vs scaffolds; "Multiple paths come only from GetOrganelle; MitoFinder always returns a single path."
- vignettes/Your-Own-Project.Rmd:182-192 (`custom_seeds_db`/`custom_labels_db`, `assembler`/`mitofinder_db`), :219-226 (passing option strings to `new_project()`).
- vignettes/custom_dbs.Rmd:33-147 (building DBs; :139-147 MitoFinder db section).
- vignettes/Installation.Rmd:74 lists the bundled tools (GetOrganelle, MitoFinder, bowtie2, BLAST+, MITOS2, tRNAscan-SE, ARWEN, ARAGORN, ...).
- NEWS.md top entry (1.5.4) is the house style for release notes: "## New Features" with "### <feature>" bullets written for users, "## Bug Fixes" in bold-lead sentences.

### 7.3 Planning-doc house style (tools/*.md, dev/specs, dev/plans)

Design docs (e.g. dev/specs/2026-08-24-userasmb-find-mitogenome-design.md, tools/userasmb_scaffold_join_design.md, tools/local_blast_db_design.md):
- Header block: `# Title`, then `Date:`, `Branch:`, `Status:` lines.
- Sections in order: `## Goal` (or `## Problem`), `## Tool survey` (what existing tools can/cannot do, with the decision at the end), `## What is already in place` / `## What is missing` (a table with columns Piece | Regular pipeline | User-assembly path), `## Design decisions` (subsections per decision, stating the default and why), `## Pipeline` (numbered stages), `## Selection algorithm` / algorithm section, `## Data model` (a markdown table: column | meaning | default; plus which tables gain FK/notes columns), `## App` (which modal, which table column, which guards), `## Testing` (pure-function unit tests listed as bullets, then e2e), `## Out of scope`, sometimes `## Open questions` and `## Alternatives considered`.
Implementation plans (tools/*_implementation_plan.md, dev/plans/*.md):
- `# <Feature> Implementation Plan`, `## Global Constraints` (ASCII only, no new deps, narrow scope), `## File Structure` (create/modify list), then `### Task N: <name>` blocks each with Files, Step 1..n containing the exact code/diff, a verification command, and a commit message; ends with `## Self-Review`.
- Sizes: designs 90-460 lines; plans 130-1437 lines. Line references are given as `path:line` inline.

---

## 8. Reusable helpers inventory

- GenBank flat-file parsing (pure R): `.cadb_parse_gb`, `.cadb_record_cds`, `.cadb_parse_location`, `.cadb_grab_definition`, `.cadb_grab_version`, `.cadb_write_mitofinder_db`, `.cadb_trim_partial_gb` (R/custom_assembly_db.R:425-700). GFF3-based reference feature fetch for NCBI accessions: `fetch_blast_ref()` (R/blast_ref_utils.R:146) writes `blast_ref_annotations.csv` + sequence; gene-name normalizers `normalize_mito_gene/normalize_pcg/normalize_rrna/normalize_trna` (:978-1145).
- FASTA IO: `Biostrings::readDNAStringSet/writeXStringSet` everywhere; `get_assembly(ID, path, scaffold, con)` rebuilds `>ID.path.scaffold topology` records from the DB (R/app_assemble_utils.R:727-744); `extract_contigs()` via samtools faidx (R/find_mito.R:429-436); `write_joined_files()` writes a Path-0 FASTA + coverageStats pair (R/scaffold_join.R:1501); `joined_assemblies_row()` (:1531).
- Coverage: `coverage()` and the `.coverage_*` helpers (R/coverage.R); `stitch_coverage()` (R/scaffold_join.R:1187); `parse_cov_string()` (:1214); `refresh_assemble_summary()` (R/app_assemble_utils.R:757-778).
- Read mapping: bowtie2 local mode + samtools pipeline (R/coverage.R:58-77, R/circularize_asmb.R:550-554); `count_junction_reads()` reads SAM and computes spanning depth (R/circularize_asmb.R:522-600); `cigar_ref_length()` (:611).
- Rotation/circularity: `rotate_asmb()` (R/rotate_asmb.R:9), `rotate_to_reference()` (R/scaffold_join.R:1305), `circularize_sequence()` (:1271), `trim_end_overlap()`/`find_end_overlap()` (R/circularize_asmb.R:303, :345).
- Pairwise/MSA in R: `pwalign` and `DECIPHER::AlignSeqs` (used by the consensus modal; R/assembly_path_scoring.R:394 doc), `compute_blast_ref_alignment()` (R/blast_ref_utils.R:1389).
- File staging: Nextflow `path()` inputs accept URLs (seeds/labels/mf_db); `${projectDir}/assets/NO_FILE` placeholder (annotate_workflow.nf:92); `prepare_ref_db` extract-once (prepare_ref_db.nf); MitoFinder db gunzip/untar handling (annotate.nf:55-68).
- Options plumbing: `opts_help()`, `tool_help_icon()`, `register_tool_help()` (R/help_utils.R); the modal + observer pattern in R/app_assemble.R:802-1000; DB migration snippet (Section 5); `fill_config()` / `migrate_config()` for config params.
- DB writes: upsert statements (assemble_workflow.nf:60-85; coverage_userAsmb_workflow.nf:32-57); per-sample `UPDATE assemble ...` (assemble_workflow.nf:87-88).

---

## 9. TOUCH-POINT CHECKLIST for a third assembler ("MapToRef")

Assumed shape (minimal, mirrors MitoFinder): a third `elif` branch in `assemble` + a third branch (or an `else`) in `coverage`, two new `assemble_opts` columns (working names `maptoref_ref TEXT` = path/URL to a .gb or FASTA, `maptoref TEXT` = free-form options string), and `assembler = 'MapToRef'`.

R package
1. R/init_db.R
   - :45 roxygen for `assembler`; new `@param maptoref_ref`, `@param maptoref`.
   - :70 default remains GetOrganelle; add args `maptoref_ref = <default or NA>`, `maptoref = "<default opts>"` near :80-83.
   - :131-132 validator: add `"MapToRef"` to the allowed vector and the error text.
   - :313-328 DDL: add the two columns; :330-350 default row: add the two values.
2. R/init_project.R: nothing required (`...` forwards), but roxygen :27-28 style implies documenting new args if exposed as named params; optionally add `custom_ref = NULL` mirroring `custom_seeds_db`.
3. R/init_test_project.R: nothing required; optionally a MapToRef test set (see 15).
4. R/backwards_compatibility.R
   - :176-178 add the new column names to the "already current" predicate.
   - after :1313 add two migration blocks (copy :1295-1313 shape; defaults NA / "").
   - optional: :2130-2170 `schema_gaps()` if the app must refuse a DB lacking the columns (current precedent: assemble_opts columns are NOT gaps).
5. R/app_assemble_utils.R `assemble_opts_modal()`
   - :293 `choices = c("GetOrganelle", "MitoFinder", "MapToRef")`.
   - after :365 add `textInput(ns("maptoref"), label = tagList("MapToRef options", tool_help_icon("maptoref")))` and `textInput(ns("maptoref_ref"), label = "Reference (.gb or FASTA):")` following the mf_db block (:323-332), with nested `opts_help(...)`.
   - :438-447 hide logic: hide the two new inputs for GetOrganelle/MitoFinder; add a `MapToRef` branch hiding the other five.
   - :301-308 help sentence listing the assemblers.
6. R/app_assemble.R
   - :125-126 `register_tool_help("maptoref", input, reopen = function() assemble_opts_modal(rv))` (needs inst/tool_help/maptoref.txt, or the fallback text at R/help_utils.R:43-48 shows).
   - :847-870 add `updateTextAreaInput` for the two inputs; :873-885 add a `MapToRef` show/hide branch.
   - :888-900 add `toggleState` lines for the two inputs.
   - :944-956 add a `MapToRef` branch.
   - :963-976 add the two columns to the `rows_upsert` data.frame (otherwise a new set stores NULL).
7. R/custom_assembly_db.R: optional `db_type = "maptoref"` is NOT needed; a single `.gb` is already what the MitoFinder builder emits. Consider exposing a small exported helper `gb_to_fasta()` built on `.cadb_parse_gb` so the container can convert a `.gb` reference (see contract item C).
8. inst/tool_help/maptoref.txt + tools/capture_tool_help.sh TOOLS entry (:31-40) if a real CLI tool is wrapped.
9. tests/testthat: test-backwards-compatibility.R :431-433 expected columns; a new unit test for any R helper (gb-to-fasta, consensus post-processing, header stamping) using the stub-binary pattern from test-find-mito.R:248-255.
10. Docs: NEWS.md new "### MapToRef assembler" bullets; vignettes/Test-Project-Assemble.Rmd:150-162 (Assembler bullet + new reference bullet); Difficult-Assemblies.Rmd:16-23 (say MapToRef returns one path); Your-Own-Project.Rmd:188-192; custom_dbs.Rmd (how to obtain a reference .gb); Installation.Rmd:74 (tool list) if a new tool is added; man/ regenerated by roxygen.

Nextflow
11. inst/nextflow/modules/assemble_workflow.nf
   - :6-20 sqlRead: append `opts.maptoref_ref, opts.maptoref` at the END of the select list (positional indices; comment at blast_genbank_workflow.nf:88 "New columns are appended at the END" is the house rule).
   - :102-108 add `maptoref: it[19]` to the opts map; add `it[20]`-style ref slot to the tuple (:99-117) and to the cross map (:183-195); use `file(x ?: "${projectDir}/assets/NO_FILE")` for the ref when absent.
   - No change to summary/status/DB logic if the process emits the standard files.
12. inst/nextflow/modules/assemble.nf
   - :16 input: add `path(ref)`.
   - :19 output unchanged (opts.assembler flows through as it[7]).
   - after :101 add `elif [ "!{opts.assembler}" = "MapToRef" ]; then ... fi` producing: `${outDir}/${id}_assembly_1.fasta` with `>${id}.1.N circular|linear` headers (or `${id}_assembly_0.fasta` sentinel), `${outDir}/${id}_reads.tar.gz` (archive `*.fastq.gz` exactly like the MitoFinder branch :80 so the coverage branch can be shared), `${outDir}/${id}_summary.txt` (may be a `touch` placeholder as :77), `assembler.log.txt`, `opts.txt`, `NF_work_dir_assemble.txt`.
   - cpus/memory: inherited from `opts.cpus`/`opts.memory` via the config closure (no change needed as long as the input is still named `opts`).
13. inst/nextflow/modules/coverage.nf :35-42: either add `elif [ "!{assembler}" = "MapToRef" ]` duplicating the MitoFinder lines, or change `elif ... MitoFinder` to `else` (any assembler that archives the preprocess pair). Read names inside the tarball must be `<ID>_preprocess_R1.fastq.gz` / `_R2` (:42).
14. inst/config.* (8 templates): only if a new `params.<stage>` block or new tool env var is needed; a branch inside `assemble` needs nothing. If a separate process is created, add it to `MITOPILOT_PROCESS_ORDER` (R/app_run_pipline.R:8-15) and give it `params.map_to_ref?.executor ?: params.assemble.executor` fallbacks (find_mito.nf:6-7 pattern), plus its own `cpus {}`/`memory {}` (Section 2.2 gotcha).
15. Container (docker/Dockerfile:27-36): add whatever mapper/consensus tool is chosen if not already present (present today: bowtie2, minimap2, samtools 1.21 incl. `samtools consensus`, spades, blastn, Biostrings/pwalign/DECIPHER in R; absent: bwa, bcftools, seqkit, seqtk, mafft, Biopython in base python). Then rebuild via docker/deploy-*.sh, bump DESCRIPTION Version, re-run tools/capture_tool_help.sh.
16. Test data: optional MapToRef parameter set in the test project (`new_test_project(assembler = "MapToRef", maptoref_ref = ...)` already works through `...`); a shipped small reference could reuse `inst/test_data/fish_mito_sampler.gb` or `ref_dbs/MitoFinder/NC_002333_Danio_rerio.gb` (single record, 16.6 kb; not in the package tarball, would need copying under inst/).

Per-sample reference (if wanted): the only existing per-sample-file precedent is the userAsmb `samples.assembly` column + `params.asmbDir` (coverage_userAsmb_workflow.nf:5, :319). Regular `new_db()` stores every mapping column in `samples` (R/init_db.R:178-198), so a `Reference` mapping column is available to sqlRead as `s.Reference` with no schema change; the sqlRead would `COALESCE(s.Reference, opts.maptoref_ref)`.

---

## 10. DATA CONTRACT a new assembler process must satisfy

A. Published directory: `${launchDir}/${params.publishDir}/${ID}/assemble/${assemble_opts}/` (assemble.nf:8, :22-23). Nothing else may choose the directory name; `assemble_opts` is the key.

B. FASTA files: `${ID}_assembly_<path>.fasta`, `<path>` an integer >= 1 (blast_genbank_workflow.nf:159-160 regex `assembly_(\d+)\.fasta$`); one file per path; each record header EXACTLY `>${ID}.<path>.<scaffold> <topology>` with `<topology>` in {`circular`, `linear`} (assemble.nf:54/:96/:100; assemble_workflow.nf:236-237, :317-327; coverage.R:242; annotate.R:114). Scaffold numbering starts at 1 and is contiguous. Sequence: uppercase A/C/G/T with N and IUPAC allowed (scored as ambiguity, R/assembly_path_scoring.R:56-60; translated as X in curation per NEWS 1.5.4). "No assembly" = a single file `${ID}_assembly_0.fasta` whose content is `>No assembly found` (assemble.nf:52, :90; assemble_workflow.nf:356-367). Path 0 is RESERVED for the app/join consensus; never emit `_assembly_0.fasta` as a real assembly.

C. Reference handling: nothing in the pipeline converts `.gb` to FASTA at assembly time; MitoFinder consumes `.gb` directly. A MapToRef branch that needs FASTA must convert (R `.cadb_parse_gb` in the package inside the container, or Biopython from the `mitos` env). The reference may arrive as a URL or local path through a `path()` input (Section 2.2).

D. Reads tarball: `${ID}_reads.tar.gz` (assemble.nf:19) whose contents match a branch in coverage.nf:35-42: either GetOrganelle layout (`<workingDir>/extended_1_paired.fq`, `extended_2_paired.fq`, `extended_*_unpaired.fq`, extracted with `--strip-components=2`) or MitoFinder layout (`<ID>_preprocess_R1.fastq.gz`, `<ID>_preprocess_R2.fastq.gz` at the archive root).

E. Other mandatory outputs (assemble.nf:19): `${ID}_summary.txt` (content free; MitoFinder ships an empty file), `assembler.log.txt`, `NF_work_dir_assemble.txt` ("Nextflow assemble working directory:" + `$PWD`, :43-44). `opts.txt` is written but not declared.

F. Output tuple positions (assemble.nf:19): [0] id, [1] fasta(s), [2] reads tar, [3] summary, [4] opts_id, [5] log, [6] workdir txt, [7] `opts.assembler` string, [8] max_paths, [9] max_scaffolds.

G. Coverage CSV (produced by `MitoPilot::coverage()` when the coverage branch works; must be identical if produced otherwise): `${ID}_assembly_<path>_coverageStats.csv` with header `SeqId,Position,Call,Depth,Correct,ErrorRate,MeanDepth,GC` (R/coverage.R:145-148, :221-226, :295-350); `SeqId = ID.path.scaffold`; `Position` 1-based per scaffold covering EVERY base; `MeanDepth`/`ErrorRate` are strings optionally prefixed `#` (mask); `GC` NA at the 200-bp edges; `na = ""`. Readers: coverage_workflow.nf:28-53 (writes `assemblies.depth/gc/errors` as space-joined strings), annotate_workflow.nf:61-64 + R/annotate.R:100-120 (coverage trim on MeanDepth > 10/15), scaffold_join.R:1590-1614, app_assemble_coverage_details.R:102-115/:618-634. Side files also expected in the dir: `<stem>_coverage.csv` (per-base), `<stem>_<scaffold>_coverage.pdf`, `<stem>.bam`, `NF_work_dir_coverage.txt`.

H. DB rows (written by assemble_workflow.nf, not by the process): `assemblies(ID, path, scaffold, length, length_raw, topology, time_stamp, sequence, ignore, edited)` via the upsert :60-85; `assemble(paths, scaffolds, length ";"-joined, topology ";"-joined unique, assemble_switch, assemble_notes, time_stamp)` via :87-88; `annotate` seed per non-ignored unit :405-421. Status semantics: 4 = awaiting BLAST, 2 = done (run_blast=0), 3 = failed with note. A new assembler needs NO new columns on `assemblies`/`assemble`/`annotate`.

I. Resources: the process input map must be named `opts` and carry `cpus` and `memory` (GB, integer) or the config closure at inst/config.*:12-13 / :19-20 fails.

J. Genetic code: `genetic_code` input is an Integer; `.intValue()` is called on it (assemble.nf:66).

K. Failure signalling: exit non-zero -> `errorStrategy 'ignore'` (after OOM retries) -> sample silently has no row update, and the run ends non-zero because `failOnIgnore = true`. Prefer writing the `assembly_0.fasta` sentinel and exiting 0 so the sample is recorded as "failed assembly" (assemble_workflow.nf:356-367).

---

## 11. Gotchas and traps observed (all with sources)

1. `INSERT OR REPLACE` nulls unlisted columns (assemble_workflow.nf:37-59); the annotate seed still uses it (:421) by design (units are re-seeded fresh).
2. `channel.fromQuery` snapshots the DB at session start; rows written during the run are invisible to queries in the same run (assemble_workflow.nf:311-313, :395-399).
3. nf-sqldb `sqlInsert` operators batch and commit independently; paired DELETE/INSERT can land in either order (blast_genbank_workflow.nf:36-58 comment). Use upserts keyed on the PK.
4. `assemble_opts` rename/reassignment orphans published output (project_consistency.R:37-49; app_server.R:132-160).
5. `memory` directive traps: bare number = bytes; `directive = {closure}` form is silently ignored (blast_genbank.nf:49-53; coverage.nf:9).
6. `-resume` cache: staged inputs regenerated each run break caching unless `cache 'lenient'` (blast_genbank.nf:6-11). A reference FASTA generated per run from a .gb should be produced ONCE (prepare_ref_db pattern) or in the assemble task itself.
7. Symlinked inputs are not visible inside the container; `stageInMode 'copy'` was needed for the ref DB tarball (prepare_ref_db.nf:9-14 comment) and scaffold_join.nf:3.
8. `samples.genetic_code` must be INTEGER (assemble.nf:66; backwards_compatibility.R:118-121).
9. Test-project reads are filtered/small (`inst/test_data/*_R?.fastq.gz`, `min_depth = 500` at R/init_test_project.R:128); full-size fetch from ENA optional (:58-110).
10. Nextflow supported range 24.10.x - 25.10.x (README.md:69; R/nextflow_version.R:117-127 pins NXF_VER).

---

## 12. Open questions surfaced by the map

- Does the maintainer want the third assembler as a third `elif` inside `assemble.nf` (smallest diff, shares resources/publishDir/DB logic) or as a separate process/workflow (cleaner but touches config templates, process ordering, and the `opts` closure)? Every existing seam favours the `elif`.
- Where should the reference live: `assemble_opts.maptoref_ref` (per parameter set, matches `mitofinder_db`) vs a per-sample `samples.<column>` from mapping.csv (matches userAsmb `samples.assembly`)? Both are already supported by the schema; a COALESCE in sqlRead would allow both.
- `.gb` to FASTA conversion location: in R inside the container via a small exported wrapper around `.cadb_parse_gb` (no new deps) vs Biopython from the `mitos` env (present but not a declared dependency) vs asking the user for FASTA only.
- Which mapper/consensus tool: bowtie2/minimap2 + `samtools consensus` (all present) vs adding bcftools/bwa (absent; needs Dockerfile change and image rebuild).
- Topology of the result: a mapped consensus inherits the reference's topology; the LOCUS line token is not parsed today (needs a tiny addition to the GenBank parser) and a FASTA reference has no topology, so a per-set or per-sample topology option (like userAsmb `Topology` column, R/sample_topology.R:12-24) may be needed.
- Whether coverage should be recomputed by `coverage()` (bowtie2 again) or derived from the mapper's own BAM; the contract (Section 10.G) is satisfied either way only if the CSV layout is reproduced exactly.
