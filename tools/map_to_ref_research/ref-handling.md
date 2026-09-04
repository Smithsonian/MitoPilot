# Lane: ref-handling. How MitoPilot already handles reference sequences and GenBank files, and what to reuse for a user-supplied single reference in a map-to-reference assembler

Repo: /home/dmacguig/Documents/GitHub/MitoPilot, branch map-to-ref-assembly (git status clean, HEAD 112d178).
Method: read-only repo inspection (Read/grep/sed), probes of the locally built container image macguigand/mitopilot:1.5.4 (docker run --rm ... read-only commands), and light web checks. Every fact carries file:line or a URL. "SAYS" = what a source states; "INFER" = my inference.

Section map
1. Existing reference / GenBank handling in the package
2. How per-project files reach Nextflow and the container
3. Recommended minimal reuse path for a user-supplied .gb / FASTA reference
4. Risks, with evidence
5. Web checks: LOCUS topology field, R GenBank parsers, Python in the container
6. Evidence index (file:line quick list)

--------------------------------------------------------------------------------
## 1. Existing reference / GenBank handling in the package

### 1.1 Remote NCBI reference fetch (R/blast_ref_utils.R) is GFF3 + FASTA, NOT GenBank flat files

- fetch_blast_ref() (R/blast_ref_utils.R:146-206) downloads, per accession, three things from NCBI EFetch with httr2 (no rentrez, no genbankr, no Biopython):
  - GFF3: `?db=nuccore&id=<acc>&rettype=gff3&retmode=text` (R/blast_ref_utils.R:155-158)
  - taxonomy XML for the record's taxid (R/blast_ref_utils.R:162-175)
  - FASTA: `rettype=fasta` (R/blast_ref_utils.R:181-191)
- .parse_ref_gff3() (R/blast_ref_utils.R:262-362) derives from the GFF3:
  - ref_length from `##sequence-region` (266-270)
  - feature table with `gene, type, pos1, pos2, direction` where type is PCG / tRNA / rRNA / ctrl (336-346, 355-357)
  - topology from the region feature's `Is_circular=true` attribute (313-319). Verbatim comment: "NCBI GFF3 marks circular genomes with `Is_circular=true`; linear records omit it. Used to gate reference rotation: only truly circular references may be rotated to a start gene".
  - genetic code from the first CDS `transl_table` attribute, default 2 (333-334)
  - gene names normalized by normalize_mito_gene() (352; definition 978-1002, with normalize_pcg 1013-1067, normalize_rrna 1107-1141, normalize_trna 1145-1206)
- .parse_ref_fasta() (R/blast_ref_utils.R:392-398) strips headers, joins lines, and validates the string with the IUPAC regex `^[ACGTNacgtnRYSWKMBDHVryswkmbdhv]+$`, returning "" on failure. This is the package's existing "is this a nucleotide sequence" check.
- .write_ref_files() (R/blast_ref_utils.R:401-423) writes the four per-accession artifacts consumed downstream:
  - `blast_ref_annotations.csv` (columns gene,type,pos1,pos2,direction,ref_length) via write.csv (406)
  - `blast_ref_sequence.txt` (bare sequence string, no header) (407)
  - `blast_ref_genetic_code.txt` (405)
  - `remote_blast_ref.json` with keys accession, blast_species, blast_evalue, organism, lineage, sequence, genetic_code, topology, pcg (409-421)
- fetch_blast_refs() (R/blast_ref_utils.R:554-596) batches many accessions and writes one `ref_<accession>/` dir each (560-569); called by the blast_ref_fetch Nextflow process (inst/nextflow/modules/blast_ref_fetch.nf:87).

INFER: a user-supplied .gb reference can be turned into exactly these four files by a small R function, after which every downstream consumer (DB ingestion, curation refHits, synteny alignment, start-gene rotation, scaffold join) works unchanged. See section 3.

### 1.2 Where the fetched reference goes in the database and who consumes it

- DB ingestion (inst/nextflow/modules/blast_ref_fetch_workflow.nf):
  - `INSERT OR REPLACE INTO blast_ref_annotations (accession, gene, type, pos1, pos2, direction, ref_length, time_stamp)` (49-51) fed by parsing blast_ref_annotations.csv in Groovy (147-177)
  - `INSERT OR REPLACE INTO blast_ref_sequences (accession, sequence, ref_length, genetic_code, lineage, topology, time_stamp)` (53-55) fed from blast_ref_sequence.txt + genetic_code.txt + JSON lineage/topology (183-200)
- Schema (userAsmb variant, identical shape in init_db.R):
  - blast_ref_annotations PK (accession, gene, pos1) (R/init_db_userAsmb.R:881-892)
  - blast_ref_sequences with a `topology TEXT` column, PK accession (R/init_db_userAsmb.R:913-923)
  - blast_ref_override table (R/init_db_userAsmb.R:947) used by resolve_unit_blast_ref() (R/blast_ref_utils.R:22-41): precedence = user override, else assemblies.blast_accession, else NA.
- Consumers of the reference:
  - Curation refHits: prepend_blast_hit_to_refhits() translates each `pcg` region of the JSON `sequence` with the JSON `genetic_code` and inserts them at the top of each PCG's refHits (R/blast_ref_utils.R:649-763); called at R/curate_mito_core.R:387-388 and 835-836.
  - Curation BLAST DB injection: inject_remote_hits_into_blast_db() writes per-gene protein FASTAs + makeblastdb (R/blast_ref_utils.R:803-949); called at R/curate_mito_core.R:140-144.
  - Reference-based strand flip: `ref_based_rc` option in curate_mito_core() uses .best_blast_ref_sequence(blast_ref_file, prefer_accession = blast_accession) (R/curate_mito_core.R:233-239, 984-986).
  - Whole-genome synteny alignment: compute_blast_ref_alignment() (R/blast_ref_utils.R:1389-1487) sanitizes with `gsub("[^ACGTNacgtnRYSWKMBDHVryswkmbdhv]", "N", s)` (1413), rotates a circular reference by `rotation` (1419-1424), and runs pwalign global-local on both strands (1440-1452).
  - Rotation offset: unit_ref_rotation() (R/blast_ref_utils.R:1268-1292) only rotates when blast_ref_sequences.topology == 'circular' (1274) and uses `MIN(pos1) - 1` of the start_gene row in blast_ref_annotations (1283-1290). The same SQL is in inst/nextflow/modules/blast_ref_align_workflow.nf:29-38 ("a linear GenBank reference keeps its native coordinates (rotation 0)").
  - Scaffold join: rotate_to_reference() rotates a joined circular sequence to reference position 1 via minimap2 PAF (R/scaffold_join.R:1296-1323); the join reads `blast_ref_<accession>/remote_blast_ref.json` from the published output (inst/nextflow/modules/scaffold_join_workflow.nf:234).
  - Curate workflow locates the per-sample reference JSON under `out/<ID>/assemble/<opts>/blast_ref_<acc>/remote_blast_ref.json`, with a legacy single-file fallback and, if none, `modules/empty_remote_blast_ref.json` whose content is `{}` (inst/nextflow/modules/curate_workflow.nf:42-62; empty file content verified).
  - Staging into curate: `path(blast_ref_files, stageAs: 'blast_ref_*.json')` (inst/nextflow/modules/curate.nf:25) and passed as a whitespace-separated list (curate.nf:58; parsed at R/curate_mito_core.R:140-141).

### 1.3 The only GenBank flat-file parser in the package: R/custom_assembly_db.R (internal, pure R)

custom_assembly_db() (R/custom_assembly_db.R:60-261) downloads GenBank records via EFetch `rettype = "gb"` (152-153) and parses them with hand-written helpers. Its roxygen SAYS it "Replaces the external Entrez Direct / python / biopython workflow with a pure-R implementation, so no external command-line tools are required" (3-6). Helpers, all `@noRd` (internal, reachable via MitoPilot:::):

- .cadb_parse_gb(gb_file) (487-552):
  - `lines <- readLines(gb_file, warn = FALSE); ends <- which(lines == "//")` (488-489); record blocks split on exact `//` lines (493)
  - keeps only records containing `/organelle="mitochondrion"` (503)
  - accession from VERSION (.cadb_grab_version 696-700: `^VERSION\s+(\S+)`)
  - "complete genome" test on DEFINITION (.cadb_grab_definition 682-692; 508)
  - feature keys detected as 5-space-indented tokens `^ {5}\S+ +` (510-512)
  - sequence: lines after `ORIGIN` up to the record end, `toupper(gsub("[^A-Za-z]", "", ...))` (520-525). Note this strips digits/spaces and uppercases, so lowercase ORIGIN text is already handled.
- .cadb_record_cds(block, origin_idx, seq, accession) (560-616): walks FEATURES, collects multi-line locations until a `/qualifier` line (578-583), reads `/gene=` else `/product=` (586-593), extracts with Biostrings::subseq / reverseComplement / xscat (600-610).
- .cadb_parse_location(loc) (623-649): handles complement(), join(), order(), and fuzzy `<`/`>` markers.
- .cadb_write_mitofinder_db(gb_file, out_file) (657-678): the MitoFinder database is just the raw record blocks re-written verbatim (`writeLines(block, con)`, 674) for mitochondrion + "complete genome" records. INFER: this is the practical definition of "MitoFinder-format .gb" in this repo: an unmodified GenBank flat file, one or more records, `//` terminated.
- .cadb_trim_partial_gb(path) (425-461) counts records by the byte pattern `\n//\n` (428).

No LOCUS-line parsing exists anywhere in R/ (grep for `LOCUS` across R/, inst/, tests/ hits only the .gb data files; see the grep in section 6). Topology is never read from a GenBank file today; the only topology source for references is the GFF3 Is_circular attribute (1.1).

### 1.4 MitoFinder's .gb reference: how it is validated and delivered to the container

- Validation is `file.exists` only, everywhere:
  - annotate_mitofinder(): `if (is.null(mitofinder_db) || !nzchar(mitofinder_db) || !file.exists(mitofinder_db))` warn + skip (R/annotate_mitofinder.R:60-63); then `normalizePath(mitofinder_db, mustWork = TRUE)` with a comment about Nextflow-staged relative paths such as "NC_002333_Danio_rerio.gb" (67-69).
  - mitofinder_gene_counts(): same check, stop() with "Build one with custom_assembly_db(db_type = \"mitofinder\")" (R/find_mito.R:454-461).
  - new_project_userAsmb(): refuses `find_mitogenome = TRUE` without an existing file, then normalizePath (R/init_project_userAsmb.R:129-140).
  - App save guard: `if (isTRUE(input$find_mitogenome) && (!nzchar(db_path) || !file.exists(db_path)))` -> sweet alert "MitoFinder database not found" (R/app_assemble_userAsmb.R:790-802).
  - The WF1 assembler modal saves `mitofinder_db = req(input$mf_db)` with NO existence check (R/app_assemble.R:960-975), because the value may be a URL.
  - The test suite passes a file containing the text "fake genbank" as a MitoFinder db (tests/testthat/test-find-mito.R:246, 264), confirming no content validation.
- Delivery to Nextflow / container:
  - WF1: assemble_opts.mitofinder_db is read by the fromQuery SQL (inst/nextflow/modules/assemble_workflow.nf:6-8), placed at tuple slot it[8] "mitofinder .gb reference database" (113), then it[1][4] (190), and received as `path(mf_db)` in the assemble process (inst/nextflow/modules/assemble.nf:16) and used as `-r ../../!{mf_db}` (65).
  - Default value is a GitHub raw URL: `https://raw.githubusercontent.com/Smithsonian/MitoPilot/refs/heads/main/ref_dbs/MitoFinder/fish_mito_sampler.gb` (R/init_db.R:46-47, 80; backfilled for old projects at R/backwards_compatibility.R:1276). Roxygen SAYS "must be GenBank format (.gb), can be a URL" (R/init_db.R:46). INFER: Nextflow's `path()` staging of an https URL is what makes the URL default work.
  - userAsmb find_mito: `tuple(id, assembly, hits, file(fopts.mitofinder_db), ...)` with the comment "The reference database is staged as a task input so it is visible inside the container on every executor." (inst/nextflow/modules/find_mito_workflow.nf:103-105); process input `file(mitofinder_db)` (inst/nextflow/modules/find_mito.nf:83).
  - WF2 annotate: `file((it[17] != null && it[17].toString().trim()) ? it[17] : "${projectDir}/assets/NO_FILE")` placeholder pattern for an optional file input (inst/nextflow/modules/annotate_workflow.nf:92; inst/nextflow/assets/NO_FILE is an empty file). annotate.nf resolves gzip / tar.gz MitoFinder dbs in the shell: `file --mime-type`, `tar -xzf` then `find . -maxdepth 2 -name '*.gb' | head -n1`, else `gunzip -c "$MF_DB" > mitofinder_ref.gb` (inst/nextflow/modules/annotate.nf:55-68).
- MitoFinder gene naming: MitoFinder takes the gene from the reference `/gene` qualifier split on "_" and Name strings can carry descriptors (R/annotate_mitofinder.R:370-375); normalize_mitofinder_gene() maps them (309-377).
- MitoFinder README (https://github.com/RemiAllio/MitoFinder) SAYS: `-r` is "Reference mitochondrial genome in GenBank format (.gb)"; several references may be used ("If several references are used, make sure the non-standard genes have the same names in the several references"); NCBI download via `efetch -format gbwithparts > reference.gb`; the tool "is mainly written in python 2.7". It does not list required qualifiers.
- Container probe (docker run --rm macguigand/mitopilot:1.5.4): `/opt/MitoFinder/mitofinder` shebang is `#!/usr/bin/python2.7`; MitoFinder ships its own vendored Biopython at /opt/MitoFinder/Bio with `__version__ = "1.63"`; `python2.7 -c "import Bio"` outside that dir fails (`ImportError: No module named Bio`); `mitofinder --version` reports 1.4.2. Dockerfile installs python2.7 via apt (docker/Dockerfile:13) and clones MitoFinder (docker/Dockerfile:83-86).

### 1.5 Shipped GenBank data and test fixtures

- ref_dbs/MitoFinder/fish_mito_sampler.gb (429,648 bytes) and ref_dbs/MitoFinder/NC_002333_Danio_rerio.gb (55,541 bytes). fish_mito_sampler.gb holds 10 records (10 `//` lines), every LOCUS line reads `... bp    DNA     circular VRT ...`, e.g. line 1: `LOCUS       NC_002333              16596 bp    DNA     circular VRT 03-APR-2023`. `file` reports "ASCII text" (LF endings).
- inst/test_data/fish_mito_sampler.gb is the same file shipped with the package and is copied into the userAsmb test project as the MitoFinder db (R/init_test_project_userAsmb.R:99-100; asserted at tests/testthat/test-new-test-project-userAsmb.R:33).
- inst/test_data/assemblies/ holds nine UA_*.fasta user-assembly fixtures (17 KB to 218 KB) described in inst/test_data/mapping_test_userAsmb.csv (columns ID,Taxon,R1,R2,Assembly,Topology,Donors,Expected; e.g. UA_CIRCULAR "single mitogenome contig - already circular", UA_LINEAR, UA_UNCIRC "400 bp duplicated end -> circularized").
- inst/test_data/ also holds paired FASTQs for 14 SRR samples plus MULTISCAFF/SCAFFJOIN pairs and filter_reads.sh / make_scaffjoin.sh.
- Other ref_dbs: ref_dbs/getOrganelle/{seeds,labels}/*.fasta and GenBankDownload/parseGB.py (a Biopython `SeqIO.parse(..., "genbank")` script kept for reference, not used at runtime); ref_dbs/Mitos2/*.tar.gz curation DBs; ref_dbs/validate/fish_mito.* protein BLAST DB.
- tests/testthat/fixtures/ contains only mitos2_empty and mitos2_genes; there is no GenBank parsing test (no test references .cadb_ or custom_assembly_db).

### 1.6 Other user-supplied-file validation patterns

- custom_curation_db(): per-file `file.exists` stop (R/custom_curation_db.R:192-197), Biostrings::readAAStringSet inside tryCatch with a friendly stop (199-206), and a "must contain only one sequence" check (207-214). This is the closest existing "single-record FASTA" validation.
- userAsmb assemblies: `Assembly` column required (R/init_db_userAsmb.R:182-186), optional `Topology` column validated to lowercase "circular"/"linear" (R/sample_topology.R:12-24), contig count via `Biostrings::fasta.seqlengths(fn)` returning NA when the file is unreachable (R/sample_topology.R:39-56; comment: "the assembly directory is not always reachable from wherever the project is being set up"), multi-contig assemblies recorded as "multi" (72-97). Missing assembly files are surfaced later by project_consistency.R:111 (`missing <- unset | !file.exists(fasta)`).
- No shiny::fileInput or shinyFiles usage anywhere: grep across R/app_*.R for `fileInput|shinyFiles|shinyFileChoose` returns nothing; every path field is a `textInput` (R/app_assemble_utils.R:334-345 "MitoFinder Database:", 344-353 seeds/labels; R/app_assemble_utils_userAsmb.R:734-742 "MitoFinder reference database (.gb):"; R/app_annotate_utils.R:426-430).

### 1.7 The userAsmb no-reads sentinel and synthetic coverage (for parity with a mapping assembler)

- new_project_userAsmb(no_raw_data = TRUE) pins `data_path <- "NA"` (R/init_project_userAsmb.R:110-115) and fills `RAW_DIR`/`ASMB_DIR` placeholders in .config (212-220).
- main.nf: `params.noRawData = (params.rawDir == 'NA')` (inst/nextflow/main.nf:13); WF1_userAsmb branches on it (94-101).
- The app reads the same sentinel back: `session$userData$no_raw_data <- readLines(.config) |> str_detect("rawDir\\s*=\\s*['\"]NA['\"]")` (R/app_server_userAsmb.R:125-128); init disables coverage_trim when no reads (R/init_db_userAsmb.R:746).
- coverage(): `no_reads <- identical(as.character(paired_reads_1), "NA")` then builds the per-base table straight from the assembly with NA depth/error (R/coverage.R:37-48); invoked with 'NA' reads by coverage_userAsmb_noReads (inst/nextflow/modules/coverage_userAsmb.nf:132). The read-based process calls the same function with reads (coverage_userAsmb.nf:59), and WF1 coverage.nf does the same for GetOrganelle/MitoFinder outputs (inst/nextflow/modules/coverage.nf:33-42).

--------------------------------------------------------------------------------
## 2. How per-project files reach Nextflow and the container

- Project directory = `<path>/.sqlite`, `<path>/.config`, `<path>/mapping.csv`, outputs under `out/` (`publishDir = 'out'`, inst/config.local:26). The app launches `nextflow` with `wd = dirname(getOption("MitoPilot.db"))`, i.e. the project directory (R/app_run_pipline.R:298-316), so `launchDir` == project dir and relative paths in .config resolve there.
- Two channels for paths:
  1. Nextflow params in .config, filled from `<<PLACEHOLDER>>` tokens by fill_config() (R/generate_config.R:44-56): `rawDir`, `asmbDir`, `publishDir`, `minDepth`, `ncbi_api_key` (inst/config.local:24-29). Workflows join them at channel level: `file(params.rawDir + "/" + it[1])` (inst/nextflow/modules/preprocess_workflow.nf:18-19), `file(params.asmbDir + "/" + it[1])` (inst/nextflow/modules/coverage_userAsmb_workflow.nf:319, 403).
  2. Option tables in the SQLite DB read by `channel.fromQuery`: assemble_opts.seeds_db/labels_db/mitofinder_db (R/init_db.R:313-328; inst/nextflow/modules/assemble_workflow.nf:6-20), find_mito_opts.mitofinder_db (R/init_db_userAsmb.R:470-483), annotate_opts.ref_db/ref_dir/mitofinder_db (R/init_db_userAsmb.R:693-720). Per-sample selection is already possible because each sample row points at an option-set name (assemble.assemble_opts, R/init_db.R:268; assemble.find_mito_opts, R/init_db_userAsmb.R:311).
- Containers and mounts:
  - Docker template: `docker { enabled = true }` and nothing else (inst/config.local:5-7). Singularity templates: `singularity { enabled = true ... runOptions = '-B /scratch02/' }` on Hydra (inst/config.NMNH_Hydra:11-15), plain `singularity { enabled = true }` on SEDNA (inst/config.NOAA_SEDNA:12-14), and a `<<CONTAINER_ENGINE>>` block for generic clusters built by container_engine_block(engine, cache, run_options) (R/generate_config.R:23-37; inst/config.slurm:12).
  - The repo's own guidance on getting a file into the container is "stage it as a task input": find_mito_workflow.nf:103-105 (quoted in 1.4) and prepare_ref_db.nf:15-19, which SAYS "Copy the tarball in so it is a real, readable file (a symlinked input's target is not mounted into the container)" and uses `stageInMode 'copy'`.
  - INFER (risk): a user reference file living outside the project tree and outside any bound path may be staged as a symlink whose target is invisible in the container on Singularity sites without autoMounts or a matching `-B`. The safest minimal move is to copy the validated reference (and its derived FASTA) into the project directory at option-save time, so it sits under launchDir like `.sqlite` and `out/`.
- Existing upload UI: none. All references are typed paths or URLs in `textInput` fields (1.6). The only path validation in the app is `file.exists` at save time for find_mito (R/app_assemble_userAsmb.R:790-802).

--------------------------------------------------------------------------------
## 3. Recommended minimal reuse path (planning only, no code)

Goal: accept one reference per project (optionally per sample via option sets), .gb preferred, FASTA accepted; validate; derive a FASTA for mapping; keep the annotations for later use. Reuse, in order of leverage:

### 3.1 Storage and UI: copy the MitoFinder db plumbing

- Add one TEXT column to assemble_opts (schema R/init_db.R:313-328; backfill pattern for old projects at R/backwards_compatibility.R:1274-1290 which does `ALTER TABLE assemble_opts ADD COLUMN mitofinder_db TEXT` and upserts a default).
- Add one `textInput` in the assembler modal next to `mf_db` (R/app_assemble_utils.R:334-345), shown/hidden by assembler like the existing shinyjs toggles (R/app_assemble.R:875-884, 947-956), saved in the same rows_upsert (R/app_assemble.R:960-975).
- Per-sample references fall out for free: samples select an assemble_opts set (R/init_db.R:268, 298), so "one option set per reference" gives per-sample references without new schema.
- Read it in the fromQuery SQL and tuple (inst/nextflow/modules/assemble_workflow.nf:6-20, 99-117, 183-195) and add a `path(ref)` input next to `path(mf_db)` (inst/nextflow/modules/assemble.nf:16), plus a third `elif` branch in the shell (assemble.nf:27, 56).

### 3.2 Validation: one small R helper at option-save time, built from existing pieces

Mirror the find_mito save guard (R/app_assemble_userAsmb.R:790-802: trim, nzchar, file.exists, sweet alert, `req(F)`), then, for a file that exists, read it once (a mitogenome .gb is 50 KB; this is not "heavy head-node work"):

- Detect format by content, not extension: first non-blank line starts with `>` => FASTA; starts with `LOCUS` => GenBank. (.gb vs .gbff vs .gbk is only a name; the record grammar is identical.)
- GenBank path, reusing R/custom_assembly_db.R internals: count records with the `//` split logic from .cadb_parse_gb (489-493) and require exactly one (multi-record = reject with a message, or take the first with a warning; decision below); accession from .cadb_grab_version (696-700); DEFINITION from .cadb_grab_definition (682-692); sequence from the ORIGIN block exactly as lines 520-525 (already strips digits/spaces and uppercases); features via .cadb_record_cds / .cadb_parse_location (560-649) generalized from CDS to CDS/tRNA/rRNA/D-loop (the feature-key detection at 510-512 already sees all keys).
- Topology from the LOCUS line: new code, but tiny. Parse token-wise per NCBI's own advice (section 5.1): `tokens <- strsplit(trimws(locus_line), "\\s+")[[1]]; topology <- if ("circular" %in% tokens) "circular" else "linear"`. Positions 56-63 of the LOCUS line hold `'linear' followed by two spaces, or 'circular'` (gbrel.txt 3.4.4.2), but NCBI recommends tokens over columns (3.4.4.1).
- Sequence checks, reusing R/blast_ref_utils.R: IUPAC-only regex from .parse_ref_fasta (397); length range sanity (INFER: the repo's own working range hints are MitoFinder's NCBI query 12000-20000 bp and custom_assembly_db's `nogene_min_length = 12000` default at R/custom_assembly_db.R:69; a plausible guard is warn outside 10-25 kb, reject outside 5-50 kb, but this should be a parameter, not a constant); N/gap content reported as a warning (compute_blast_ref_alignment already tolerates Ns by masking non-IUPAC to N, 1413).
- FASTA path: Biostrings::readDNAStringSet inside tryCatch, exactly like custom_curation_db (R/custom_curation_db.R:199-214), require length 1, uppercase, take the first header token as the name (the package already truncates names this way: R/annotate_mitofinder.R:82, R/coverage.R:22). Topology for a FASTA reference: default "linear" unless the header carries a `circular` token (the pipeline's own FASTA convention is `>ID.path.scaffold circular|linear`, assemble.nf:54, 96, 100; coverage_userAsmb.nf:50-56), or a UI checkbox; the mapping CSV already uses a `Topology` column validated by validate_declared_topology (R/sample_topology.R:12-24) which could be reused verbatim for an option-modal value.
- Genetic code: take the first CDS `/transl_table=` qualifier when present, default 2, exactly as .parse_ref_gff3 does with the GFF3 attribute (R/blast_ref_utils.R:333-334); compare against samples.genetic_code and warn on mismatch.

### 3.3 Derivation: write the same four files the NCBI fetch writes, plus the mapping FASTA

- Call .write_ref_files(accession, result, genetic_code_num, organism, lineage, seq_str, NULL, NULL, ann, seq, gc, json, topology = topology) (R/blast_ref_utils.R:401-423) into a project-local directory (INFER: `<project>/ref/<accession>/`), where `result` is the normalized feature table (gene/type/pos1/pos2/direction/ref_length) produced by running normalize_mito_gene() over the .gb `/gene` or `/product` values (R/blast_ref_utils.R:352, 978). This yields blast_ref_annotations.csv, blast_ref_sequence.txt, blast_ref_genetic_code.txt and remote_blast_ref.json in the exact shapes ingested by blast_ref_fetch_workflow.nf:147-200 and consumed by curation (R/curate_mito_core.R:140-144, 387-388), the synteny view, and rotation (R/blast_ref_utils.R:1268-1292).
- Write `<accession>.fasta` with Biostrings::writeXStringSet (single record, uppercase) next to them; this is the mapper input. INFER: because the package already requires plain-string reference sequences everywhere (blast_ref_sequence.txt is header-less, compute_blast_ref_alignment takes a string), keeping both a FASTA and the bare .txt costs nothing.
- Copying into the project directory also resolves the container-mount risk in section 2 and lets Nextflow stage it with `path()` like every other input.

### 3.4 Bonus: using the .gb annotations later

- Origin / start_gene: the pipeline rotates a circular reference to the sample's start_gene using `MIN(pos1) - 1` of that gene in blast_ref_annotations (R/blast_ref_utils.R:1283-1290; blast_ref_align_workflow.nf:29-38), and rotates assemblies to start_gene after annotation with rotate_asmb() (R/rotate_asmb.R:9-113; called via annotate() start_gene, R/annotate.R:74, 384). INFER: if the user reference is registered in blast_ref_sequences/blast_ref_annotations under its accession (rows shaped as in 1.2), then (a) the synteny view and (b) start-gene rotation logic work for it without changes, and the scaffold-join `rotate_to_reference()` (R/scaffold_join.R:1296-1323) can canonicalize a mapped consensus to reference position 1 with minimap2, which is in the container.
- Registering it as "the" reference for a sample: blast_ref_override(ID, path, scaffold, accession) is the existing user-override mechanism (R/blast_ref_utils.R:22-29; R/init_db_userAsmb.R:947), honored by resolve_unit_blast_ref and unit_ref_facts. INFER: writing an override row for map-to-reference units (or setting assemblies.blast_accession when BLAST is off) is the smallest way to make the app treat the user reference as the comparison reference.
- MitoFinder piggyback: a single-record annotated .gb is already a valid MitoFinder `-r` database (1.4; MitoFinder README allows one or many records; custom_assembly_db writes MitoFinder dbs as raw record blocks, R/custom_assembly_db.R:657-678). INFER: the same file can be offered as the default for annotate_opts.mitofinder_db (R/app_annotate_utils.R:426-430) and find_mito_opts.mitofinder_db, giving MitoFinder gap-fill annotation (annotate_mitofinder.R:1-13) against the user's own reference. annotate.nf's gzip/tar handling (55-68) shows the accepted packaging.
- Do not reinvent the GenBank -> feature-table conversion: R/tbl_to_gff3.R is documented as only tested on MitoPilot-produced tables ("It likely will not work for a feature table downloaded directly from GenBank", R/tbl_to_gff3.R:11-12), so it is not a candidate.

--------------------------------------------------------------------------------
## 4. Risks, with evidence

- Multi-record .gb: .cadb_parse_gb happily returns many records (R/custom_assembly_db.R:493-541); MitoFinder accepts many; a mapper needs one. Decide: reject, or take the first record and warn. INFER: reject unless exactly one record, since "single reference (one sequence)" is the stated design and a silent first-record pick would hide user error.
- .gbff vs .gb vs .gbk: same grammar; detect by content (`LOCUS` first token), not extension. annotate.nf's tar path searches only `*.gb` (`find . -maxdepth 2 -name '*.gb'`, inst/nextflow/modules/annotate.nf:62), so a .gbff inside a tarball would already be missed there; a plain file path is unaffected.
- Windows line endings: readLines keeps `\r`, and the record splitter tests `lines == "//"` exactly (R/custom_assembly_db.R:489, 659), so a CRLF file yields "No GenBank records found" (491, 662). The ORIGIN cleanup would survive (`gsub("[^A-Za-z]", "")`, 524). Any new helper should `sub("\r$", "", lines)` first. Same for FASTA: Biostrings handles CRLF, but a `circular` token check on the header should trimws.
- Lowercase sequence: ORIGIN text is lowercase in GenBank; .cadb_parse_gb uppercases (524); .parse_ref_fasta's regex accepts both cases (397); compute_blast_ref_alignment and normalize_blast_ref_alignment uppercase (1243, 1413). Fine as long as the new helper uppercases once.
- Gaps / Ns / IUPAC in the reference: accepted by the IUPAC regex (397) and masked to N for alignment (1413). A mapper treats N as mismatch; report N count and long-run count as a warning. Reference `assembly_gap` features would be visible in the feature table if kept.
- Reference not starting at the conventional start gene / on the opposite strand: no assumption needed. The existing rotation is gene-driven (start_gene MIN(pos1) on the reference; rotate_asmb on the sample, which also reverse-complements when start_gene is on the minus strand, R/rotate_asmb.R:60-69), and `ref_based_rc` can flip a sample to the reference strand (R/curate_mito_core.R:233-239). INFER: for a mapped consensus the natural coordinate system is the reference's own, so keep the reference unrotated for mapping and let WF2 rotate as it does today.
- Linear vs circular reference: only the LOCUS topology says so for .gb; a FASTA has no flag. The pipeline gates rotation and origin handling on the stored topology (R/blast_ref_utils.R:1274; blast_ref_align_workflow.nf:29-33). A wrong flag means a wrong synteny rotation, not a crash.
- Genetic code mismatch: reference `/transl_table` vs samples.genetic_code (resolved per curate_target, R/init_db_userAsmb.R:203-205). Warn, do not block.
- URL values: assemble_opts.mitofinder_db defaults to a URL (R/init_db.R:80). If the reference field also accepts URLs, save-time `file.exists` must be skipped for `^https?://` (as the WF1 modal already implicitly does by not checking, R/app_assemble.R:960-975) and validation deferred to the task.
- Container visibility of an external path: see section 2; copying into the project directory removes the question.
- The reference gene names must normalize: normalize_mito_gene flags unknowns with a "?" prefix (R/blast_ref_utils.R:1006-1009), so odd `/gene` values in a user .gb surface in the synteny labels rather than failing.

--------------------------------------------------------------------------------
## 5. Web checks

### 5.1 LOCUS line topology field

- NCBI GenBank release notes (https://ftp.ncbi.nih.gov/genbank/gbrel.txt, fetched today with curl; section 3.4.4) SAY:
  - 3.4.4.1: "Users who process the data elements of the LOCUS line should use a token-based parsing approach rather than parsing its content based on fixed column positions." and "we recommend that users parse the LOCUS line based on whitespace-separated tokens."
  - 3.4.4.2 column table: `56-63      Molecule Topology : 'linear' followed by two spaces, or 'circular'`; also `13-28 Locus Name`, `30-40 Sequence Length, right-justified`, `42-43 'bp'`, `48-53 Molecule Type`, then division at 65-67 and date at 69-79 (the table continues past the excerpt I printed; positions 56-63 and the token advice are the load-bearing facts).
  - Example line in the notes: `LOCUS       CP032762             5868661 bp    DNA     circular BCT 15-OCT-2018`.
- The NCBI sample record page (https://www.ncbi.nlm.nih.gov/genbank/samplerecord/) lists locus name, length, molecule type, division and date but does not document the topology token (WebFetch summary), so gbrel.txt is the primary source.
- Repo evidence matches: all ten LOCUS lines in inst/test_data/fish_mito_sampler.gb carry `circular VRT`.

### 5.2 R packages that can parse GenBank, and what is actually installed

- DESCRIPTION Imports (DESCRIPTION:18-63) include Biostrings, BiocGenerics, DECIPHER, pwalign, httr2, jsonlite, reticulate, readr, stringr; Suggests are knitr, testthat, withr. None of genbankr, seqinr, ape, rentrez is declared.
- renv.lock: ape 5.x is present only as a transitive dependency of msaR (renv.lock:705-706; msaR Imports "ape" at renv.lock:2718). seqinr, genbankr, rentrez are absent from renv.lock (grep).
- Container probe (Rscript requireNamespace): seqinr FALSE, ape TRUE, genbankr FALSE, rentrez FALSE, Biostrings TRUE, pwalign TRUE, DECIPHER TRUE, Rsamtools FALSE, GenomicAlignments FALSE.
- Biostrings readDNAStringSet (https://rdrr.io/bioc/Biostrings/man/XStringSet-io.html) SAYS format is `Either "fasta" (the default) or "fastq"` and "Only FASTA and FASTQ files are supported for now." No GenBank reader.
- genbankr was removed: it appears under "Packages removed with Bioconductor 3.19 release" (http://bioconductor.org/about/removed-packages/). Not an option.
- ape::read.GenBank (https://rdrr.io/cran/ape/man/read.GenBank.html) SAYS "This function connects to the GenBank database, and reads nucleotide sequences using accession numbers given as arguments." It downloads; it does not parse a local file.
- seqinr::gb2fasta (https://rdrr.io/cran/seqinr/man/gb2fasta.html) SAYS "Converts a single entry in GenBank format into a fasta file." and "Multiple entries in GenBank file are not supported." seqinr is not installed anyway.
- Conclusion: the repo's own pure-R parser in R/custom_assembly_db.R is the only GenBank reader available without a new dependency, and it already covers what is needed (records, VERSION, DEFINITION, features, locations, ORIGIN). Adding a LOCUS token check is a few lines.

### 5.3 Python in the container (docker/Dockerfile plus probe of macguigand/mitopilot:1.5.4)

- Base env: `/opt/conda/bin/python3` = Python 3.12.7 (mambaforge base, docker/Dockerfile:1, 19); `import Bio` fails (`ModuleNotFoundError: No module named 'Bio'`). numpy 2.4.6 and scipy 1.16.0 are present (GetOrganelle deps).
- `mitos` conda env (docker/Dockerfile:30): Python 3.12.13 with Biopython 1.81 (`conda run -n mitos python -c "import Bio"` prints 1.81). So Biopython SeqIO is reachable via `conda run -n mitos python`, but it is a separate env, not the base.
- MitoFinder: python2.7 (docker/Dockerfile:13) with its vendored Biopython 1.63 in /opt/MitoFinder/Bio (probe).
- Mappers and helpers in the base env (docker/Dockerfile:27-32; probe): minimap2 2.28-r1209, bowtie2 2.5.4, samtools 1.21, spades 4.1.0, tabix/bgzip; bam-readcount 1.0.1 lives in its own env (`conda run -n bam-readcount`, R/coverage.R:81). Absent: bwa, bcftools, seqkit, seqtk, bedtools, freebayes, ivar, pilon.
- INFER: a map-to-reference process can be built from bowtie2/minimap2 + samtools, and consensus/coverage from the existing coverage() + bam-readcount path, with no new container dependency. GenBank parsing should stay in R (already there) rather than Python.

--------------------------------------------------------------------------------
## 6. Evidence index (quick list)

Repo, GenBank/reference handling
- R/blast_ref_utils.R:146-206 fetch_blast_ref (GFF3+FASTA+taxonomy via httr2)
- R/blast_ref_utils.R:262-362 .parse_ref_gff3 (topology 313-319; genetic code 333-334; normalize 352)
- R/blast_ref_utils.R:392-398 .parse_ref_fasta (IUPAC regex 397)
- R/blast_ref_utils.R:401-423 .write_ref_files (CSV/txt/gc/JSON shapes)
- R/blast_ref_utils.R:554-596 fetch_blast_refs (ref_<acc>/ dirs)
- R/blast_ref_utils.R:649-763 prepend_blast_hit_to_refhits; 803-949 inject_remote_hits_into_blast_db
- R/blast_ref_utils.R:978-1206 normalize_mito_gene and helpers
- R/blast_ref_utils.R:1268-1292 unit_ref_rotation; 1389-1487 compute_blast_ref_alignment
- R/custom_assembly_db.R:425-461 .cadb_trim_partial_gb; 487-552 .cadb_parse_gb; 560-616 .cadb_record_cds; 623-649 .cadb_parse_location; 657-678 .cadb_write_mitofinder_db; 682-700 DEFINITION/VERSION grabbers
- R/annotate_mitofinder.R:60-69 file.exists + normalizePath; 309-377 normalize_mitofinder_gene
- R/find_mito.R:454-481 mitofinder_gene_counts (file.exists, -r db)
- R/custom_curation_db.R:192-214 file/single-record validation pattern
- R/init_db.R:46-47, 71-80, 313-349 assemble_opts schema + URL defaults
- R/init_db_userAsmb.R:182-222 mapping/topology; 470-503 find_mito_opts; 693-745 annotate_opts; 881-925 blast_ref_* tables; 947 blast_ref_override
- R/init_project_userAsmb.R:110-140, 212-220 no_raw_data sentinel, mitofinder_db check, config fill
- R/sample_topology.R:12-24, 39-56, 72-97, 106-117
- R/coverage.R:37-48 synthetic coverage when reads == "NA"
- R/rotate_asmb.R:9-113; R/scaffold_join.R:1296-1323 rotate_to_reference
- R/curate_mito_core.R:140-144, 233-239, 387-388, 984-986
- R/app_assemble_utils.R:334-345 mf_db textInput; R/app_assemble.R:875-884, 960-975 toggle/save; R/app_assemble_userAsmb.R:790-802 save guard; R/app_annotate_utils.R:426-430
- R/app_run_pipline.R:298-316 nextflow launched with wd = project dir
- R/generate_config.R:23-37 container_engine_block; 44-56 fill_config
- R/backwards_compatibility.R:1274-1290 add-column + default backfill pattern
- R/tbl_to_gff3.R:11-12 not usable for downloaded GenBank tables
- tests/testthat/test-find-mito.R:246, 264 "fake genbank" db accepted

Nextflow / config
- inst/nextflow/main.nf:13 noRawData sentinel; 31-86 WF1; 89-139 WF1_userAsmb
- inst/nextflow/modules/assemble_workflow.nf:6-20 SQL; 99-117 tuple; 183-195 assemble_in
- inst/nextflow/modules/assemble.nf:16 inputs incl. path(mf_db); 27, 56 assembler branches; 65 -r db; 54, 96, 100 header topology convention
- inst/nextflow/modules/annotate_workflow.nf:92 NO_FILE placeholder; inst/nextflow/modules/annotate.nf:55-68 gz/tar .gb handling
- inst/nextflow/modules/find_mito_workflow.nf:103-105 stage db as task input; find_mito.nf:83
- inst/nextflow/modules/blast_ref_fetch_workflow.nf:49-55 INSERT statements; 147-200 CSV/JSON -> rows
- inst/nextflow/modules/blast_ref_align_workflow.nf:29-38 rotation SQL gated on topology
- inst/nextflow/modules/curate_workflow.nf:42-62 JSON lookup + empty fallback; curate.nf:25, 58
- inst/nextflow/modules/prepare_ref_db.nf:15-19 symlink-target mount gotcha; stageInMode 'copy'
- inst/nextflow/modules/coverage_userAsmb_workflow.nf:319, 403 file(params.asmbDir + ...); coverage_userAsmb.nf:50-56, 132
- inst/config.local:5-7, 24-29; inst/config.NMNH_Hydra:11-15; inst/config.NOAA_SEDNA:12-14; inst/config.slurm:12
- docker/Dockerfile:1, 13, 19, 27-32, 83-86

Data
- ref_dbs/MitoFinder/fish_mito_sampler.gb (10 records, all LOCUS "circular VRT"); ref_dbs/MitoFinder/NC_002333_Danio_rerio.gb
- inst/test_data/fish_mito_sampler.gb; inst/test_data/assemblies/UA_*.fasta; inst/test_data/mapping_test_userAsmb.csv
- ref_dbs/getOrganelle/GenBankDownload/parseGB.py (Biopython script, not used at runtime)

Web
- https://ftp.ncbi.nih.gov/genbank/gbrel.txt section 3.4.4 (LOCUS token advice; positions 56-63 topology)
- https://www.ncbi.nlm.nih.gov/genbank/samplerecord/ (LOCUS fields, no topology documented)
- https://rdrr.io/bioc/Biostrings/man/XStringSet-io.html (fasta/fastq only)
- http://bioconductor.org/about/removed-packages/ (genbankr removed in Bioc 3.19)
- https://rdrr.io/cran/ape/man/read.GenBank.html (downloads by accession)
- https://rdrr.io/cran/seqinr/man/gb2fasta.html (single entry only)
- https://github.com/RemiAllio/MitoFinder (reference "in GenBank format (.gb)", multi-record allowed, python 2.7)
