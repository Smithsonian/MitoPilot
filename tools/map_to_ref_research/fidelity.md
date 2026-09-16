# Design proposal: MapToRef assembler, fidelity-first

Date: 2026-09-03
Branch: map-to-ref-assembly
Status: proposal (planning only, no code)
Angle: maximise faithfulness to Geneious Prime "Map to Reference" semantics (geneious-advisor.md sections 6-8 and 10) while touching MitoPilot at the seams the codebase map identified.

Evidence base: geneious-advisor.md, alt-tools.md, codebase-map.md (+ v1), ref-handling.md. Every file:line below was re-checked in the working tree on 2026-09-03; the samtools 1.21 filter-expression, `samtools consensus`, bam-readcount 1.0.1 and Biostrings 2.78 facts were probed live in the macguigand/mitopilot:1.5.4 image the same day.

---

## 1. Summary

1. Add a third `assemble_opts.assembler` value, `MapToRef`, that maps the preprocessed reads to ONE user-supplied reference (.gb preferred, FASTA accepted) and iterates Geneious-style: map, call a reads-only consensus, make that consensus the next reference, extend past linear ends from overhanging reads, wrap the origin for circular references, stop when no new reads map or the cap is hit.
2. Core mapper: bowtie2 2.5.4 (already in the image) in `--local` mode, one flag set per Geneious sensitivity preset (seed length = Geneious index word length, seed mismatch = Geneious "single mismatch in the seed"), followed by `samtools view -e` filter expressions that enforce the Geneious per-read caps (max mismatches %, max gap size, min overlap, min overlap identity, min mapping quality, multi-best policy) exactly, not approximately.
3. Core consensus: a small custom R step (new `R/map_to_ref.R`, Biostrings only) over `bam-readcount` per-base counts (already in the image, already used by `R/coverage.R:81`). It reproduces the documented Geneious rules verbatim: percentage threshold with cumulative IUPAC codes and all-or-none ties, "Highest Quality" as summed quality with a 60% share, low-coverage call below depth 3, no-coverage call policy (N / Ref / gap), trim-to-reference off, consensus of reads only. No existing open-source caller has these semantics (alt-tools.md section 4), so this is the one place a custom step is justified.
4. Circularity and extension are custom too, but tiny: the elongate-and-fold trick already in `R/circularize_asmb.R:466-555`, and a column consensus of soft-clipped overhangs at linear reference ends.
5. Why: zero new container dependencies, every knob a Geneious user recognises has a named equivalent with an honest fidelity label, and the integration is the minimal shape the codebase map recommends (third `elif` in `assemble.nf`, two `assemble_opts` columns, coverage `elif` becomes `else`).

---

## 2. Algorithm

### 2.1 One sample, end to end

Runs inside the existing `assemble` process (container, `opts.cpus` / `opts.memory`), as `Rscript -e "MitoPilot::map_to_ref(...)"` in a new `elif` branch. Shell-like pseudo commands; `$W` = task work dir, `$OUT` = `${id}/assemble/${opts_id}`.

Step 0. Trim. None inside MapToRef. PREPROCESS already ran fastp (`inst/nextflow/modules/preprocess.nf:27`, default opts `--trim_poly_g --correction --detect_adapter_for_pe --dont_eval_duplication`, `R/init_db.R:248`). This is the Geneious "Do not trim (already trimmed with BBDuk)" recipe (advisor 6.6, 10.2). Adapter read-through is removed by fastp's PE overlap analysis, which is also what Geneious "Trim paired read overhangs" removes.

Step 1. Prepare the reference (R, `maptoref_prepare_ref()`; details in section 3).
- .gb -> `ref.fasta` (one record, uppercase, header `>ACC topology`), `ref_meta.json` (accession, definition, topology, length, transl_table, n_ambiguous).
- FASTA -> same, topology from `--topology` or header token, default linear.
- Fail early (sentinel, section 5.4) if 0 or >1 records, non-IUPAC characters, or length outside `[5000, 50000]`.

Step 2. Pass loop, k = 1 .. cap (`--iterations`, default 10; `0` = Geneious "Fine Tuning: None", i.e. a single pass).
```
ref_k   = (k == 1) ? ref.fasta : cons_{k-1}.fasta
L_k     = length(ref_k)
if circular: map_ref = ref_k + first F bases of ref_k, F = min(500, L_k %/% 2)   # R/circularize_asmb.R:529 pattern
else:        map_ref = ref_k
bowtie2-build -q map_ref idx_k
bowtie2 --local <PRESET FLAGS> -I $MIN_INS -X $MAX_INS [--no-mixed --no-discordant] \
        --no-unal -p $CPUS -x idx_k -1 R1.fastq.gz -2 R2.fastq.gz 2>> assembler.log.txt \
  | samtools view -u -e '<PER-READ CAP EXPRESSION>' - \
  | samtools sort -@ $CPUS -o pass_k.bam -
samtools index pass_k.bam
n_k = samtools view -c -F 0x904 pass_k.bam                  # primary, mapped
conda run -n bam-readcount bam-readcount -w1 -q 0 -b 0 -f map_ref pass_k.bam > counts_k.tsv
Rscript: fold (circular) -> call consensus (section 2.3) -> extend ends (linear, section 2.4) -> cons_k.fasta, mask_k.tsv, stats row
```
Stopping rule after pass k (section 2.5): stop if `n_k <= n_{k-1}` (no new reads mapped; the documented Geneious behaviour, advisor 5.15 and 9.2) or `cons_k == cons_{k-1}` (fixed point) or `k == cap` or `n_k < --min-mapped-reads` (default 100; failure, section 5.4).

Step 3. Final pass. The consensus of the last pass IS the product (Geneious does the same: the consensus is read from the last iteration's contig). No extra mapping inside MapToRef; the existing `coverage` process re-maps all reads to the published FASTA with `bowtie2 --very-sensitive-local` (`R/coverage.R:63-75`) and produces the contract coverage CSV, which doubles as the "final clean pass" QC that the Geneious staff recipe recommends (advisor 5.15).

Step 4. Publish (contract, codebase-map.md section 10 / v1 section 8):
- `$OUT/${id}_assembly_1.fasta`, header `>${id}.1.1 circular|linear` (topology inherited from the reference), sequence uppercase A/C/G/T/N/IUPAC. Rotation: reference coordinates, never rotated (WF2 rotates to `start_gene` as it does for every assembler).
- `$OUT/${id}_reads.tar.gz` containing `${id}_preprocess_R1.fastq.gz` and `_R2` at the archive root (MitoFinder layout, `assemble.nf:80`), so `coverage.nf` needs no new branch.
- `$OUT/${id}_summary.txt` (human-readable key metrics, section 5), `assembler.log.txt` (bowtie2 stderr + loop log), `opts.txt`, `NF_work_dir_assemble.txt`.
- `$OUT/maptoref/` : `ref.fasta`, `ref_meta.json`, `iterations.tsv`, `mask.tsv`, `final.bam` + `.bai` (last pass; optional, see 7.3).
- Failure: `$OUT/${id}_assembly_0.fasta` with `>No assembly found` and exit 0 (the sentinel route, `assemble_workflow.nf:356-367`).

### 2.2 Mapper presets: Geneious sensitivity -> bowtie2 flags + samtools filters

Common flags (all presets): `--local --ma 2 --np 1 --n-ceil L,0,0.15 --rdg 5,3 --rfg 5,3 --score-min G,20,8 --no-unal`. Geneious values are the recovered ones (advisor 8.1 Medium is exact; Medium-Low mismatch/gap are second-hand; Low is derived from the SARS-CoV-2 custom set; Medium-High and Highest are INFERENCE, marked *).

| Preset | Geneious (index word / expansion word / max mismatch % / max gap % / max gap size / repeat filter) | bowtie2 seed and penalty flags | Per-read caps (samtools `-e`) |
|---|---|---|---|
| Low | 14 / 24 / 10 / 10 / 10* / 8 | `-L 22 -N 0 -i S,1,1.15 -D 10 -R 2 --mp 6,6` | f=0.10, g=0.10, G=10 |
| Medium-Low | ? / ? / 20 / 10 / 30* / ? | `-L 16 -N 0 -i S,1,0.75 -D 15 -R 2 --mp 6,6` | f=0.20, g=0.10, G=30 |
| Medium (default) | 12 / 14 / 30 / 15 / 50 / 20 | `-L 12 -N 1 -i S,1,0.50 -D 20 -R 3 --mp 4,4` | f=0.30, g=0.15, G=50 |
| Medium-High* | 11 / 13 / 35 / 15 / 50 / 20 | `-L 11 -N 1 -i S,1,0.50 -D 25 -R 3 --mp 4,4` | f=0.35, g=0.15, G=50 |
| Highest* | 10 / 12 / 40 / 20 / 50 / 20 | `-L 10 -N 1 -i C,1,0 -D 30 -R 4 --mp 3,3` | f=0.40, g=0.20, G=50 |
| Custom | user values | derived from `--index-word` (`-L`), `--seed-mismatch` (`-N`), and the caps | user f, g, G |

Why local mode with a low mismatch penalty: a read must still align THROUGH a stretch at the preset's divergence, so the per-base expected score `2*(1-f) - mp*f` must stay positive (Medium: 1.4 - 1.2 = +0.2 with `--mp 4,4`). That is what makes bowtie2 behave like a Geneious end-to-end read placement at 30% divergence instead of clipping it. Local mode is required anyway for reads that overhang a linear reference end (section 2.4).

Per-read cap expression (Medium, linear reference of length REFLEN; `[NM]` counts mismatches plus inserted/deleted bases, `sclen` = soft-clipped bases, `qlen` = read length; all verified to work in samtools 1.21 `view -e`):
```
( [NM] + sclen <= 0.30*qlen                                   # Geneious "Maximum Mismatches Per Read (%)"; clipped bases count as differences
  || ((pos == 1 || endpos == REFLEN) && [NM] <= 0.30*(qlen - sclen)) )   # overhangs at real ends are allowed to hang (extension)
&& cigar !~ "(5[1-9]|[6-9][0-9]|[1-9][0-9]{2,})[ID]"           # Geneious "Maximum Gap Size" 50 (regex for any I/D op > 50)
&& (qlen - sclen) >= MINOVL                                    # Geneious "Minimum Overlap" (only when applyMinOverlap)
&& [NM] <= (1 - MINOVLID/100)*(qlen - sclen)                   # Geneious "Minimum Overlap Identity (%)" (only when applied)
&& (mapq >= MINMAPQ || <dup-block exemption>)                  # Geneious "Minimum mapping quality" (only when applied)
&& ([XS] == null || [AS] > [XS] || <dup-block exemption>)      # Geneious "Map multiple best matches: None" (only when chosen)
```
For a circular reference the end exemption is dropped (the elongated reference has no real ends) and `<dup-block exemption>` is `((pos >= 1 && endpos <= F) || pos > REFLEN)`: reads lying entirely inside either copy of the duplicated block are ambiguous by construction and must not be MAPQ- or XS-filtered (the same reason `count_junction_reads()` refuses a `-q` filter, `R/circularize_asmb.R:466-471`).

The gap % cap (g) cannot be expressed in a samtools expression (no CIGAR arithmetic). It is not enforced separately: gap bases already count inside the mismatch budget through `[NM]`, and the bowtie2 gap penalties (`--rdg 5,3 --rfg 5,3`) bound gaps inside the score budget. It is labelled approximate in the table (section 2.6).

### 2.3 Consensus caller (custom R, exact Geneious semantics)

Input: `counts_k.tsv` from `bam-readcount -q 0 -b 0` over the (possibly elongated) mapping reference. Verified format (probe 2026-09-03): one row per position with depth > 0 (zero-depth positions are omitted, exactly as `R/coverage.R:104-110` already compensates); per-allele fields `base:count:avg_mapq:avg_baseq:...`; a deletion `-ACT:count:...` is reported on the FIRST deleted base and the deleting reads are excluded from the depth of the following deleted bases; an insertion `+TTT:count:...` is reported on the base PRECEDING the insertion. `mtr_consensus()`:

1. Fold (circular only). For rows with `pos > L_k`: `pos <- pos - L_k`; merge with the original row: counts add, `avg_baseq` and `avg_mapq` become count-weighted means. This is `contig_depth()`'s fold (`R/circularize_asmb.R:480-495`) applied to allele counts instead of intervals. Deletions/insertions anchored in the appended block fold the same way. Rows with `pos > L_k + F` cannot exist.
2. Per-position allele table: `A, C, G, T` (N in reads ignored, Geneious ignores ambiguous read bases in the count), `-` (gap) with count = sum of deletion alleles covering this position (a `-XYZ` with count c reported at p contributes c to the gap allele at p, p+1, ..., p+len-1), insertions kept aside.
3. Weight per allele:
   - `percent` mode: weight = count.
   - `highest-quality` mode: weight = count * avg_baseq (this equals the SUMMED base quality, which is the manual's definition: "sums the total quality for each potential base call"). If `--use-mq yes` (default, mirrors XML `mapQuality=true`): avg_baseq is first combined with avg_mapq as a joint error probability, `Q = -10*log10(pB + pM - pB*pM)`; the combination formula is not published by Geneious (approximate). MQ is NOT used for positions `1..F` of a circular reference (duplicated block, MAPQ meaningless).
   - gap allele weight in HQ mode: count * (mean avg_baseq of the base alleles at that position, or 20 if none). Geneious "low-quality-gap halving" is not reproduced (INFER).
4. Depth gate, in this order:
   - `depth == 0` (no row): emit `--no-cov-call` = `N` (default; Geneious `?`), or the reference base (`ref`; Geneious "Ref"), or nothing (`gap`; Geneious "-"). Status `no_cov`.
   - `depth < --min-depth` (default 3, XML `coverageThreshold=3` for reference assemblies): emit `N` (Geneious `?`). Status `low_cov`. (MitoPilot's FASTA contract forbids `?`; N carries the same meaning, and the mask file keeps the low/no distinction that Geneious shows by colour.)
5. Threshold call (manual 6.5, reproduced literally). Sort alleles by weight, descending. Walk down accumulating `cum = sum(weights so far) / total`. Stop at the first allele where `cum >= T/100` (T = `--threshold`, default 60 in HQ mode, 65 in percent mode, the XML `thresholdPercent=weighted_60` / `thresholdPercentNoQuality=65` defaults). Tie rule: if the stopping allele ties with the next one(s), include all tied alleles ("either all or none of the involved residues will be selected"). Worked check against the manual example (6 A, 3 G, 1 T): T <= 60 -> A; 60 < T <= 90 -> R; T > 90 -> D. T = 0 is the plain plurality call; T = 100 is "Identical".
   - Called set without `-`: emit `Biostrings::mergeIUPACLetters(set)` (verified: `AG -> R`, `ACGT -> N`, `CT -> Y`). Status `called` (1 allele) or `ambiguous`.
   - Called set == {`-`}: emit nothing (deletion). Status `deleted`.
   - Called set contains `-` and bases: emit the IUPAC of the bases (gap-vs-base ambiguity has no IUPAC symbol; Geneious behaviour here is undocumented, INFER). Status `ambiguous`.
6. Insertions after position p: ins_weight = sum of all `+SEQ` allele weights at p; if `ins_weight / total_p >= max(T/100, 0.5)` emit the most common inserted string after the base called at p (Geneious columns where the reference has a gap; the >50% guard avoids emitting an insertion that fewer than half the reads carry). Status `inserted` rows (consensus positions with `ref_pos = NA`).
7. `noConsensusEndGaps=true`: leading/trailing `no_cov` positions are still emitted as N (Geneious only suppresses GAP calls at ends, not `?`); with `--no-cov-call gap` they are dropped.
8. Write `cons_k.fasta` (uppercase) and `mask_k.tsv` with columns `ref_pos, cons_pos, ref_base, call, depth, status, top_fraction` (the coordinate map between reference and consensus that `--mark-ins`-style tools provide; here it is a side table).

Why not `samtools consensus`: probed in 1.21; `--mode simple -c/-H` is "top base share >= c else N, second base share >= H else drop", not the cumulative-IUPAC rule; Bayesian mode has no percentage semantics; neither offers Ref/gap no-coverage policies (`-T` arrives in 1.22 only). It stays a debugging cross-check, not the caller.

### 2.4 Extending past linear reference ends (Geneious "reads can extend a bit further past the ends on each iteration")

Only when topology is linear and `--trim-to-ref no` (the XML default `trimToReference=false`). Per pass, one extra BAM scan:
```
samtools view -e 'pos == 1 && cigar =~ "^[0-9]+S"' pass_k.bam | cut -f 6,10,11      # left overhangs
samtools view -e 'endpos == REFLEN && cigar =~ "[0-9]+S$"' pass_k.bam | cut -f 6,10,11   # right overhangs
```
In R: take the clipped prefix (right-justified against position 0) / clipped suffix (left-justified at REFLEN+1) with its qualities, build per-column allele weights (same percent / HQ weighting, base quality only), call each column with the section 2.3 threshold rule, and stop at the first column whose depth is below `--min-depth`. Prepend/append the result to `cons_k`. Each pass can grow by at most one read length; later passes map more reads onto the new ends and grow again, until a pass adds no new reads. This is the documented Geneious mechanism (manual 6.2) and the manual "Skimming for barcodes" / Scyphozoa loop (advisor 9.3). The extension is capped by `--max-extension` (default 20000 bp total) as a runaway guard for a repetitive end.

### 2.5 Iteration and stopping

- Semantics: `--iterations N` = "Iterate up to N times" = at most N mapping passes in total, pass 1 against the user reference. `--iterations 0` = "None" (one pass, no re-mapping).
- Geneious converts read placements back to the ORIGINAL reference coordinates each iteration; MapToRef reports in the evolving consensus coordinates and keeps `mask.tsv` as the map back to the reference (`ref_pos` column). Same information, different bookkeeping.
- Stop conditions (first that fires): (a) `n_k <= n_{k-1}` mapped primary reads (Geneious "only perform as many iterations as required", Kemp "discontinued once no additional sequence reads are aligned"); (b) `cons_k` identical to `cons_{k-1}`; (c) `k == N`; (d) `n_k < --min-mapped-reads` -> failure. The stop reason is written to `iterations.tsv` and `${id}_summary.txt`.
- Default N = 10: a real Geneious dropdown value ("Iterate up to 10 times"), the saturation point for the worst reference in the trevally test (advisor 9.2), and free when convergence arrives earlier (usually 3-5 passes). 5 (Geneious shipped default) and 25 (distant-reference papers) are one option away.

### 2.6 Geneious option -> MapToRef parameter table

Parameters live in the `assemble_opts.maptoref` option string (section 4), parsed by `map_to_ref()`. Fidelity: EXACT = same rule and same numbers; APPROX = same intent, different mechanism or unpublished numbers; NOT = not replicable / deliberately not offered.

| Geneious option (XML key / dialog label) | Geneious values | MapToRef parameter | Default | Fidelity |
|---|---|---|---|---|
| Mapper | Geneious | (bowtie2 local + custom caller) | n/a | APPROX: bowtie2 is a seed-and-extend FM-index mapper; Geneious is a hash-index seed-and-expand mapper; same family, different heuristics |
| `sensitivity` preset | Low / Medium-Low / Medium / Medium-High / Highest / Custom | `--sensitivity` | `medium` | Medium EXACT for the caps (30%/15%/50), APPROX for seed heuristics; Low, Medium-Low APPROX (partial numbers); Medium-High, Highest APPROX (numbers inferred) |
| `indexWordLength` (Index Word Length) | 12 (Medium) | `--index-word` -> bowtie2 `-L` | 12 | APPROX (a bowtie2 seed is an FM-index seed with `-N` mismatches, not a hash word) |
| `expansionWordLength` (Word Length) | 14 (Medium) | none; seed interval `-i` scaled per preset | n/a | NOT (no bowtie2 analogue; the DP extension is unbounded by word size) |
| "allowing a single mismatch in the seed" (white paper 7.1) | on | `--seed-mismatch` -> `-N 0/1` | 1 (Medium and above) | EXACT in meaning |
| `filterRepeatsReference` / `filterRepeatsSizeReference` (Ignore words repeated more than n times) | true / 20 | none | n/a | NOT (17 kb reference has almost no 12-mer repeated >20 times; no bowtie2 equivalent) |
| `allowGaps` | true | always on (`--rdg/--rfg`) | on | EXACT |
| `maxGapsPerRead` (Maximum Per Read %) | 15 | `--max-gap-pct` -> gap penalties only | 15 | APPROX (gap bases count inside the mismatch budget via `[NM]`; no separate cap) |
| `maxGapSize` (Maximum Gap Size) | 50 | `--max-gap-size` -> CIGAR regex filter | 50 | EXACT (post-filter; verified) |
| `maxMismatches` (Maximum Mismatches Per Read %) | 30 | `--max-mismatch-pct` -> `[NM] + sclen <= f*qlen` | 30 | EXACT as a per-read cap; APPROX in that Geneious would align a clipped tail through while bowtie2 leaves it unvoting |
| `maxAmbiguity` (Maximum Ambiguity) | 4 | none; `--n-ceil L,0,0.15` fixed | n/a | NOT (Geneious limits IUPAC expansions of words; bowtie2 treats IUPAC as N) |
| `applyMinOverlap` / `minOverlap` (Minimum Overlap) | false / 25 | `--min-overlap` -> `(qlen - sclen) >= x` | 0 (off) | EXACT |
| `applyMinOverlapPercentageIdentical` / `minOverlapPercentageIdentical` | false / 80 | `--min-overlap-identity` | 0 (off) | EXACT (NM-based) |
| `doMoreThoroughSearching` (Search more thoroughly for poor matching reads) | false | folded into `-D/-R` effort per preset | n/a | APPROX |
| `accuratelyMapReadsWithErrorsToRepeatRegions` | true (Medium) | none | n/a | NOT (bowtie2 has no such re-check; iteration provides the same effect per the white paper) |
| `multipleBestMatches` (Map multiple best matches) | Randomly / None / All | `--multi-best random|none` | `random` | EXACT for Randomly (bowtie2 default picks uniformly among equal-best); EXACT-in-effect for None (`[AS] > [XS]` filter); All NOT offered (YAGNI) |
| `applyMinimumMappingQuality` / `minimumMappingQuality` | false / 30 | `--min-mapq` | 0 (off) | APPROX (MAPQ scales differ: bowtie2 0-42; Geneious unpublished) |
| `trimPairedOverhangs` (Trim paired read overhangs) | true | fastp adapter trimming in PREPROCESS | on | APPROX (overhang = adapter read-through, removed upstream; overlapping mates are counted twice in depth, as in Geneious) |
| `onlyMapPairedHitsReference` (Only map paired reads which map nearby) | false / mapNearby | `--paired-nearby yes|no` -> `--no-mixed --no-discordant` | `no` | EXACT in effect |
| Paired-read distance (Set Paired Reads, expected distance) | user value, used as a soft penalty | `--min-insert` / `--max-insert` -> bowtie2 `-I/-X`; concordant pairs preferred | 0 / 1000 | APPROX (window plus concordance preference instead of a distance penalty) |
| `fineTune` (Fine Tuning) | None / iterate_3 / 5 / 10 / 25 / integer | `--iterations` | 10 | EXACT for the loop (previous consensus becomes the reference, all reads re-mapped, early stop when no new reads); APPROX for the intra-iteration effect ("shuffles the gaps around so reads align better to each other": no read-vs-read realignment here) |
| `findStructuralVariants`, `findDeletions` / `maximumDeletionSize`, `includeInsertionsInStructuralVariants`, `minimumJunctionSupport` | false / false, 1000 / true / 2 | none | off | NOT (two-pass junction discovery not replicated; default off in every public workflow) |
| `reanalyzeSequencesThreshold` | 8 | none | n/a | NOT (undocumented internal) |
| `trimOptions.method` (Trim Before Mapping) | Do not trim (recipe) | none; fastp upstream | n/a | EXACT in effect |
| Consensus threshold: `thresholdPercent` = weighted_50/60/75 ("Highest Quality") | weighted_60 | `--consensus highest-quality --threshold 60` | HQ 60 | EXACT for the 60% share rule; APPROX for BQ+MQ combination and gap quality (unpublished) |
| Consensus threshold: percentage (0% ... 100% Identical), `thresholdPercentNoQuality` | 65 | `--consensus percent --threshold T` | 65 when chosen | EXACT (cumulative IUPAC rule and all-or-none ties per manual 6.5) |
| `applyLowCoverageOrQualityCall` + `coverageOrQuality=coverage` + `coverageThreshold` | true / coverage / 3 | `--min-depth` | 3 | EXACT rule; character N instead of `?` |
| `lowCoverageOrQualityCharacter` | `?` | fixed N + `status=low_cov` in mask.tsv | N | APPROX (character), EXACT (meaning) |
| `coverageOrQuality=quality` + `qualityThreshold` (Call N if Quality below) | 20 | none in v1 | n/a | NOT in v1 (deferred; trivial to add to the caller) |
| `noCoverageCharacterReference` (If no coverage call) | `?` / N / - / Ref | `--no-cov-call N|gap|ref` | N | EXACT for all three (Ref fill possible because the caller sees the reference) |
| `trimToReference` (Trim to reference sequence) | false | `--trim-to-ref no|yes` | no | EXACT (yes = skip extension and clip to reference extent) |
| `noConsensusGaps` | false | always call gaps | off | EXACT |
| `noConsensusEndGaps` | true | ends never emitted as gap when no-cov-call is N | on | EXACT |
| `mapQuality` / `mapQualityMethod` (use mapping quality, Total) | true / mapSummed | `--use-mq yes|no` | yes | APPROX (formula) |
| `ignoreReadsMappedToMultipleLocations` | false | see `--multi-best` | off | EXACT |
| `splitAroundQuestionMarks` | false | none | n/a | NOT (a mitogenome consensus must stay one record) |
| Consensus "of the reads only, never the reference" | always | always (reference enters only via `--no-cov-call ref`) | n/a | EXACT |
| Circular reference (index words spanning the origin, expansion wraps) | native | elongate-and-fold (F = min(500, L/2)) | auto | EXACT in effect for reads and fragments shorter than F; pairs straddling the seam with insert > F lose pairing only |
| Reads-weighted-by-mismatches during consensus (white paper 7.1) | on | none | n/a | NOT (undocumented weighting) |
| Deterministic results (manual 6.4) | yes | bowtie2 `--seed 0` fixed; ties broken deterministically in R | yes | EXACT |

---

## 3. Reference input

### 3.1 Accepted forms
- GenBank flat file (`.gb`, `.gbk`, `.gbff`; detected by content: first non-blank line starts with `LOCUS`). Exactly ONE record required (count of `//` lines == 1); multi-record files are rejected with the message "MapToRef needs a single-record reference; the MitoFinder database format is not accepted here" (ref-handling.md 4 recommends reject over silent first-record pick).
- FASTA (first non-blank line starts with `>`): exactly one record.
- Optional gzip (`.gz`), handled with `gzfile()` in R.
- Path or URL (same as `mitofinder_db`; `assemble.nf` `path()` inputs stage both, `R/init_db.R:46` documents the URL precedent).

### 3.2 Validation (R, `maptoref_prepare_ref(ref, out_dir, topology = "auto")`, reused in two places)
Reuses the pure-R GenBank pieces in `R/custom_assembly_db.R`: record split on `//` (`:488-493`), `.cadb_grab_version` (`:696-700`), `.cadb_grab_definition` (`:682-692`), ORIGIN extraction `toupper(gsub("[^A-Za-z]", "", ...))` (`:520-525`). New code, all small:
- strip `\r` from every line first (CRLF gotcha, ref-handling.md 4);
- LOCUS topology by token (`"circular" %in% strsplit(trimws(locus), "\\s+")[[1]]`, per NCBI gbrel.txt 3.4.4.1), else `linear`;
- first `/transl_table=` qualifier -> `transl_table` (default 2), warn if it differs from `samples.genetic_code`;
- sequence must match the package IUPAC regex `^[ACGTNRYSWKMBDHV]+$` (`R/blast_ref_utils.R:397`); count N and IUPAC; warn when > 1% or any run > 100 bp;
- length must be within `[5000, 50000]` (reject) and warn outside `[12000, 25000]`;
- FASTA: `Biostrings::readDNAStringSet` inside `tryCatch` (the `R/custom_curation_db.R:199-214` pattern), one record, header first token = accession, topology from `--topology` if given, else `circular` token in the header, else `linear` with a warning "FASTA carries no topology; assuming linear (set --topology circular to wrap the origin)".
- `--topology circular|linear` in the option string always overrides the file.

Where it runs:
1. In the assemble task (container) at the top of `map_to_ref()`, every run; failures write the sentinel (section 5.4) with the reason in `assembler.log.txt`.
2. In the app at option save (`R/app_assemble.R:958-1000`), only when the value is a local path that exists: `maptoref_prepare_ref(path, out_dir = NULL)` in check-only mode, surfaced as a `shinyWidgets::sendSweetAlert` warning (the `R/app_assemble_userAsmb.R:790-802` guard shape). URLs skip the check (as `mf_db` does today). A 55 KB file parse is not head-node work.

### 3.3 Per-project vs per-sample
Per parameter set: `assemble_opts.maptoref_ref` (mirrors `mitofinder_db`). A sample uses a different reference by pointing at a different parameter set (`assemble.assemble_opts`, `R/init_db.R:268`), which the app already supports via the set picker. That is per-sample selection with zero schema beyond the two columns. A `samples.Reference` mapping column with `COALESCE` in the sqlRead is possible later (codebase-map.md section 9, last paragraph) but is deferred: `samples` columns vary by project and a missing column breaks the SQL.

### 3.4 Storage and container reach
- The user keeps the file wherever they like; recommended `<project>/ref/` so it sits under `launchDir` next to `.sqlite` and `out/`, which every container engine mounts (ref-handling.md 2, INFER risk on Singularity symlink staging). Documented, not enforced.
- Nextflow stages it as `path(ref)` into the task dir (URL or path), the same way `mf_db` reaches MitoFinder (`assemble.nf:16`, `:65`). When the assembler is not MapToRef the placeholder `${projectDir}/assets/NO_FILE` is passed (pattern `annotate_workflow.nf:92`; file exists at `inst/nextflow/assets/NO_FILE`).
- The derived `ref.fasta` + `ref_meta.json` are produced inside the task and published to `$OUT/maptoref/`; nothing is generated per run outside the task, so `-resume` caching is unaffected (codebase-map.md gotcha 6).

### 3.5 What we keep from the .gb
Kept in v1: the sequence, LOCUS topology, VERSION accession, DEFINITION (organism string), first `/transl_table`, and a verbatim copy of the record in `$OUT/maptoref/ref.gb` for provenance. Not used in v1 but preserved by that copy: the feature table. Phase 3 (section 7) registers the reference's genes into `blast_ref_annotations` / `blast_ref_sequences` through `.write_ref_files()` (`R/blast_ref_utils.R:401-423`) plus a `blast_ref_override` row, which lights up the synteny view and start-gene rotation against the user's own reference (ref-handling.md 3.3-3.4). BLAST against GenBank still runs by default (status 4 -> 2), giving an independent reference for curation; users can turn it off per BLAST option set.

---

## 4. Integration (minimal diff)

Naming: assembler string `MapToRef`; columns `maptoref_ref TEXT` (path/URL) and `maptoref TEXT` (option string). Default option string:
```
--sensitivity medium --iterations 10 --consensus highest-quality --threshold 60 --min-depth 3 --no-cov-call N --trim-to-ref no --paired-nearby no --min-insert 0 --max-insert 1000 --multi-best random --min-mapq 0 --topology auto
```
(Every value is a Geneious default from advisor 8.1/8.3 except `--iterations 10`, section 2.5, and `--max-insert 1000`.)

### 4.1 R package
| File | Change |
|---|---|
| `R/init_db.R:45` | roxygen: assembler choices add `"MapToRef"`; new `@param maptoref_ref`, `@param maptoref` |
| `R/init_db.R:70-83` | new args `maptoref_ref = NA_character_`, `maptoref = <default string above>` next to `mitofinder_db`/`mitofinder` |
| `R/init_db.R:131-133` | validator vector and message add `MapToRef`; if `assembler == "MapToRef"` require `nzchar(maptoref_ref)` |
| `R/init_db.R:313-328`, `:330-350` | DDL adds the two TEXT columns; default row adds the two values |
| `R/init_project.R` | none required (`...` forwards, `:125-133`); document in the roxygen example |
| `R/backwards_compatibility.R:176-178` | add `"maptoref_ref" %in% ...` and `"maptoref" %in% ...` to the already-current predicate |
| `R/backwards_compatibility.R` after `:1313` | two migration blocks copied from the `mitofinder` block (`:1295-1313`): `ALTER TABLE assemble_opts ADD COLUMN maptoref_ref TEXT` (default NA) and `... maptoref TEXT` (default string) |
| `R/app_assemble_utils.R:293` | `choices = c("GetOrganelle", "MitoFinder", "MapToRef")` |
| `R/app_assemble_utils.R` after `:332` | `textInput(ns("maptoref_ref"), label = "Reference (.gb or FASTA):")` and `textInput(ns("maptoref"), label = tagList("MapToRef options", tool_help_icon("maptoref")))`, each with a nested `opts_help(...)` inside the container (the comment at `:436-438` explains why) |
| `R/app_assemble_utils.R:438-447` | hide the two new inputs for GetOrganelle/MitoFinder; add a `MapToRef` branch hiding `mitofinder, mf_db, getOrganelle, seeds_db, labels_db` |
| `R/app_assemble.R:125-126` | `register_tool_help("maptoref", input, reopen = function() assemble_opts_modal(rv))` |
| `R/app_assemble.R:873-885`, `:944-956` | add the `MapToRef` show/hide branch in both observers; `updateTextInput` for the two inputs where the others are repopulated (`:847-870`) |
| `R/app_assemble.R:888-900` | two `toggleState` lines |
| `R/app_assemble.R:963-976` | add `maptoref_ref = input$maptoref_ref %||% NA_character_`, `maptoref = input$maptoref %||% default` to the `rows_upsert` data.frame (otherwise a new set stores NULL) ; before the upsert, the save-time check from section 3.2 |
| `R/map_to_ref.R` (NEW) | exported `map_to_ref(ref, reads_1, reads_2, id, opts_string, cpus, out_dir)` (driver: option parsing, preset table, pass loop, stop rule, outputs, sentinel), exported `maptoref_prepare_ref()`; internal `.mtr_preset_flags()`, `.mtr_filter_expr()`, `.mtr_readcounts()` (parse bam-readcount), `.mtr_fold()`, `.mtr_call_column()` (threshold rule), `.mtr_consensus()`, `.mtr_extend_ends()`, `.mtr_write_outputs()`. Shell calls via `system()`/`system2()` exactly as `R/coverage.R:58-82` does; Biostrings only |
| `NAMESPACE`, `man/` | roxygen regenerate |
| `inst/tool_help/maptoref.txt` | hand-written option reference (there is no external CLI to capture); note in `inst/tool_help/README.md` and skip the `tools/capture_tool_help.sh` TOOLS entry |

### 4.2 DB schema and migration
Two nullable TEXT columns on `assemble_opts`; no change to `assemblies`, `assemble`, `annotate`, `samples`, `blast_ref_*`. `schema_gaps()` (`R/backwards_compatibility.R:2130-2170`) stays untouched (assemble_opts columns are not hard-stop gaps today). Old projects get the columns backfilled on open (NA reference, default option string) so existing GetOrganelle/MitoFinder sets are unaffected.

### 4.3 Nextflow
| File | Change |
|---|---|
| `inst/nextflow/modules/assemble_workflow.nf:6-20` | append `opts.maptoref_ref, opts.maptoref` at the END of the select list (house rule, positional indices) -> `it[19]`, `it[20]` |
| `assemble_workflow.nf:99-117` | opts map gains `maptoref: it[20]`; tuple gains an 8th element `file((it[19] != null && it[19].toString().trim()) ? it[19] : "${projectDir}/assets/NO_FILE")` |
| `assemble_workflow.nf:183-195` | cross map forwards it as `it[1][8]` |
| `inst/nextflow/modules/assemble.nf:16` | input tuple gains `path(ref)` after `path(mf_db)` |
| `assemble.nf` after `:101` | new branch: `elif [ "!{opts.assembler}" = "MapToRef" ]; then mkdir -p !{outDir}; Rscript -e "MitoPilot::map_to_ref('!{ref}', '!{reads[0]}', '!{reads[1]}', '!{id}', '!{opts.maptoref}', !{opts.cpus}, '!{outDir}')" ; echo "!{opts.maptoref}" > !{outDir}/opts.txt; tar -czvf !{outDir}/!{id}_reads.tar.gz *.fastq.gz; echo "Nextflow assemble working directory:" > !{outDir}/NF_work_dir_assemble.txt; echo "$PWD" >> ...; fi`. `map_to_ref()` itself writes `${id}_assembly_1.fasta` or the `_0` sentinel, `${id}_summary.txt`, `assembler.log.txt`, and `maptoref/` |
| `assemble.nf:19` output | unchanged (`opts.assembler` flows as `[7]`) |
| `inst/nextflow/modules/coverage.nf:40` | `elif [ !{assembler} == "MitoFinder" ]` -> `else` (any assembler that archives the preprocessed pair) |
| `inst/config.*` (8 templates), `R/app_run_pipline.R:8-15` | no change (no new process, no new params block; resources come from the `opts` input as today) |

Contract check against codebase-map.md section 10 / v1 section 8: A (dir) yes; B (single path 1, header token, sentinel) yes; C (.gb -> FASTA inside the task, in R) yes; D (MitoFinder tarball layout) yes; E (summary/log/workdir files) yes; F (tuple positions untouched) yes; G (coverage CSV produced by the unchanged `coverage()` from the published FASTA) yes; H (DB rows written by `assemble_workflow.nf`, nothing new) yes; I (`opts` input carries cpus/memory) yes; J (`genetic_code` unused by this branch) yes; K (sentinel + exit 0 on failure) yes.

### 4.4 Container
`docker/Dockerfile:27-36`: no change. Everything used is present and probed: bowtie2 2.5.4, samtools 1.21 (`view -e` with `[NM]`, `sclen`, `qlen`, `pos`, `endpos`, `cigar =~`; `sort`, `index`, `faidx`), bam-readcount 1.0.1 in env `bam-readcount` (`conda run -n bam-readcount`, `R/coverage.R:81`), R 4.5.2 with Biostrings 2.78 (`mergeIUPACLetters`). No pysam, no bcftools, no samtools bump needed (the custom caller supplies the Ref-fill behaviour that would otherwise need 1.22's `-T`). A rebuild happens only because the R package changes (DESCRIPTION version bump, `docker/deploy-*.sh`, stale tarball gotcha).

### 4.5 App UI
Only the options modal changes (section 4.1): third assembler choice, two text inputs, show/hide, save. The coverage/details modal, path scoring, BLAST, scaffold join, WF2 and export are untouched and work on the published files. Optional v1.1: render `maptoref/iterations.tsv` as a small table in the coverage-details modal (`R/app_assemble_coverage_details.R`).

### 4.6 Export
None. `R/export.R` is assembler-agnostic; `find_sequence_gaps()` (`:101-110`) already reports N-runs "whatever put it there". IUPAC codes translate to X with a warning in curation (NEWS 1.5.4).

### 4.7 Docs
`NEWS.md` "### MapToRef assembler" bullets; `vignettes/Test-Project-Assemble.Rmd:150-162` (Assembler bullet + reference field); `vignettes/Difficult-Assemblies.Rmd:16-23` (MapToRef returns one path, one scaffold); `vignettes/Your-Own-Project.Rmd:188-192` (`assembler = "MapToRef", maptoref_ref = "ref/NC_002333.gb"`); `vignettes/custom_dbs.Rmd` (how to fetch a single-record .gb from NCBI, and why the MitoFinder sampler is not a MapToRef reference); `inst/tool_help/maptoref.txt` = the option table of section 2.6 in plain text.

### 4.8 Tests
Pure-function unit tests (no mapper run), `tests/testthat/test-map-to-ref.R`:
- `.mtr_call_column()`: the manual's 6A/3G/1T example at T = 50, 60, 70, 90, 95, 100 and T = 0; tie cases (3A/3G at T = 50 -> R; 2A/2G/2T at T = 40 -> D); gap-only set -> deletion; gap+base set -> base IUPAC; depth gates (0 -> N/ref/gap by policy; 2 < 3 -> N).
- `.mtr_readcounts()` + `.mtr_fold()`: a synthetic bam-readcount table for a 200 bp "reference" with F = 50, including a deletion spanning the fold and an insertion in the appended block.
- `.mtr_extend_ends()`: synthetic SAM rows with left/right soft clips; extension stops at depth < 3.
- `.mtr_preset_flags()` / `.mtr_filter_expr()`: exact strings for each preset, linear vs circular, with/without min-mapq and multi-best none.
- Stop rule: sequences of (n_k, cons_k) -> stop reason.
- `maptoref_prepare_ref()`: single-record .gb (topology circular from LOCUS, transl_table 2, accession), CRLF .gb, multi-record .gb rejected, FASTA with/without `circular` token, non-IUPAC rejected, length bounds.
- `test-backwards-compatibility.R:431-433`: add `maptoref_ref`, `maptoref` to `expect_cols`; migration from the v1.3.10 fixture.
- Stubbed-binary test of `map_to_ref()` using the `test-find-mito.R:248-255` PATH-stub pattern (fake `bowtie2`, `samtools`, `conda` scripts that emit canned files) to exercise the loop, sentinel and output layout without a mapper.
End to end: `new_test_project(assembler = "MapToRef", maptoref_ref = system.file("test_data/NC_002333_Danio_rerio.gb", package = "MitoPilot"))` on a fish sample; copy `ref_dbs/MitoFinder/NC_002333_Danio_rerio.gb` (single record, 16.6 kb, 55 KB) to `inst/test_data/` for that purpose. Compare against the GetOrganelle assembly of the same sample (identity > 99% expected for a same-family reference; differences must fall in `low_cov`/`ambiguous` mask positions).

---

## 5. Outputs and QC

### 5.1 Files (all under `out/<ID>/assemble/<opts>/`)
- `<ID>_assembly_1.fasta` (contract), `<ID>_reads.tar.gz`, `<ID>_summary.txt`, `assembler.log.txt`, `opts.txt`, `NF_work_dir_assemble.txt`.
- `maptoref/ref.fasta`, `maptoref/ref.gb` (verbatim copy when .gb), `maptoref/ref_meta.json`.
- `maptoref/iterations.tsv`: `pass, reference_len, mapped_reads, new_reads, mean_depth, n_called, n_ambiguous, n_low_cov, n_no_cov, n_deleted, n_inserted, ext_left, ext_right, consensus_len, changed_vs_prev, stop_reason` (the aITE script's per-iteration log plus the Geneious coverage-CSV idea).
- `maptoref/mask.tsv`: per final-consensus position `ref_pos, cons_pos, ref_base, call, depth, status, top_fraction` (snippy's `.aligned.fa` idea as a table: zero coverage, sub-threshold coverage, ambiguity and indels are visibly distinct, and the reference-to-consensus coordinate map is explicit).
- `maptoref/final.bam` + `.bai` (last pass, filtered; optional, default kept; ~1-5 MB for a skim).
- Then, from the unchanged `coverage` process: `<ID>_assembly_1_coverageStats.csv`, `<ID>_assembly_1.bam`, per-scaffold PDF (contract G).

### 5.2 DB columns populated
Only the existing ones, by `assemble_workflow.nf` and `coverage_workflow.nf`: `assemble.paths=1, scaffolds=1, length, topology, assemble_switch (4 or 2), assemble_notes, time_stamp`; `assemblies` row `(ID, 1, 1, length, topology, sequence, ignore)` then `depth/gc/errors`; `annotate` seed with `partial = (topology == 'circular' ...) ? 'no' : 'yes'`.

### 5.3 Metrics shown to the user
- Existing surfaces, for free: assembly length and topology in the Assemble table; per-base depth, error-rate and GC tracks plus path scoring (ambiguity count, depth evenness, BLAST concordance) in the coverage-details modal; N-run report at export.
- `<ID>_summary.txt` (opened via "Open output folder"): reference accession/definition/topology/length; passes run and stop reason; reads mapped in pass 1 and final; consensus length and delta vs reference; counts of called / ambiguous / low-coverage / no-coverage / deleted / inserted positions; substitutions vs reference (from mask.tsv, a reference-bias tripwire: identity to the reference far above what the mapping rate suggests, or far below it, both warrant a de novo cross-check).

### 5.4 Failure modes and how they surface
| Condition | Behaviour |
|---|---|
| Reference unreadable, multi-record, non-IUPAC, out of length bounds | `assembler.log.txt` gets the reason; `<ID>_assembly_0.fasta` sentinel; exit 0 -> status 3 "failed assembly" (`assemble_workflow.nf:356-367`). Caught earlier in the app when the path is local |
| Pass 1 maps fewer than `--min-mapped-reads` (100) | same sentinel; log says "N reads mapped to the reference; try a closer reference or --sensitivity highest" |
| bowtie2 OOM | exit 137..140 -> existing retry (`assemble.nf:10-11`) |
| Cap reached without convergence | consensus still emitted; `stop_reason = cap`; summary says so |
| Consensus mostly N (called fraction below 50%) | emitted; summary flags it; the annotate seed still runs; low `MeanDepth` shows in the coverage tracks and `coverage_trim` (`R/annotate_coverage_trim.R:39-63`) trims the ends as it does for any assembler |
| Extension runaway | capped at `--max-extension`; noted in summary |
| Unknown option key in the option string | fail fast before mapping, sentinel + log (a typo should not silently fall back to defaults) |

---

## 6. Risks and mitigations

| Risk | Mitigation in this design |
|---|---|
| Reference bias (Culicoides benchmark: Geneious consensus carried 2x the differences of Bowtie2 on a distant reference; Westbury: reference swap changes the consensus) | Reads-only consensus; N (never Ref) below depth and at zero coverage by default; iteration so the sample's own bases replace the reference's; `mask.tsv` and the substitutions count in the summary make the reference contribution auditable; BLAST/reference fetch still run so curation compares against an independent GenBank record; docs recommend a GetOrganelle or MitoFinder run on the same sample as the cross-check (Winn 2025, Kemp) |
| NUMTs (nuclear copies recruit reads, inflate ambiguity or depth) | Geneious defaults are reproduced (no MAPQ filter, random multi-best), so NUMT behaviour matches Geneious; users can set `--min-mapq 20` (with the duplicated-block exemption) and the HQ mode already downweights low-MQ bases; depth outliers are masked by the existing `MeanDepth_mask` (`R/coverage.R:295-333`) |
| Divergent reference (different genus/family) | Medium allows 30% differences per read and iterates; the log reports the pass-1 mapping rate and suggests `--sensitivity highest --iterations 25` (the settings the distant-reference papers used) when it is low; `--multi-best random` plus iteration follows the white paper's own 89%-identity validation |
| Rearrangements / duplications absent from the reference | Cannot be discovered by mapping (Winn 2025, Kemp); they appear as blocks of `no_cov`/`low_cov` or as a truncated consensus, both visible in the tracks and the summary; the vignette says so and points at the de novo assemblers |
| Low coverage | N below 3 (Geneious default), never Ref; `annotate` and `coverage_trim` already handle N-rich ends; `--min-depth` exposed |
| Repeats and the control region (tandem repeat arrays, origin-spanning D-loop) | Origin is handled by elongate-and-fold so the D-loop is not split; tandem-repeat copy number collapses to the reference's (same as Geneious mapping, documented); random placement mirrors Geneious; the region shows as low `top_fraction` in mask.tsv |
| Runtime | One pass = one `coverage()`-sized bowtie2 run over all reads (a known cost in this pipeline); early stop usually ends at 3-5 passes; index build on a 17 kb reference is negligible; bam-readcount over 17 kb is seconds. Worst case (cap 10, 30 M pairs, 6 cpus) is on the order of an hour, comparable to GetOrganelle. If needed later: pass 1 with all reads, passes 2..N with reads that mapped in any earlier pass plus their mates (a fidelity trade-off, therefore deferred and off) |
| Container size | Zero new binaries; image grows only by the R package delta |
| Local mode leaves clipped tails unvoting where Geneious would align through | Preset-specific `--mp` keeps reads aligned through at the preset's divergence; clipped bases count against the mismatch budget so heavily clipped reads are dropped rather than half-used; iteration moves the consensus toward the sample so later passes align through |
| MAPQ artefacts in the duplicated block of a circular reference | MQ weighting and MQ/XS filters are exempted inside the block by construction (section 2.2, 2.3) |
| Deterministic reruns | `bowtie2 --seed 0`, sorted allele order and fixed tie-breaking in R |

---

## 7. Effort

### 7.1 Size
- New: `R/map_to_ref.R` ~650 lines including roxygen (option parser ~60, presets/filters ~80, reference prep ~120, readcount parse + fold ~90, column caller ~70, consensus + indels ~120, extension ~80, driver/outputs/sentinel ~130); `tests/testthat/test-map-to-ref.R` ~300; `inst/tool_help/maptoref.txt` ~80; `inst/test_data/NC_002333_Danio_rerio.gb` (copied).
- Modified: `R/init_db.R` (~15 lines), `R/backwards_compatibility.R` (~40), `R/app_assemble_utils.R` (~35), `R/app_assemble.R` (~30), `inst/nextflow/modules/assemble_workflow.nf` (~6), `assemble.nf` (~15), `coverage.nf` (1), `tests/testthat/test-backwards-compatibility.R` (1), `NEWS.md`, 4 vignettes (~60), `DESCRIPTION` version, `NAMESPACE`/`man` regenerated.
- Total: ~15 files, roughly 1,300 new lines, no new dependencies.

### 7.2 Phases
1. Core (1 PR): `R/map_to_ref.R` with reference prep, Medium preset, HQ and percent consensus, circular fold, linear extension, stop rule, outputs; unit tests; `assemble.nf`/`assemble_workflow.nf`/`coverage.nf` plumbing; `init_db` + migration. Validate on the Danio reference against two shipped fish samples and the Scyphozoa linear project.
2. UI and docs (1 PR): modal inputs, save-time reference check, tool help, vignettes, NEWS, e2e test project set, remaining presets.
3. Later (separate PRs, only if asked): register the .gb features as the curation/synteny reference (`.write_ref_files()` + `blast_ref_override`); iterations table in the coverage-details modal.

### 7.3 Deferred (YAGNI list)
- Per-sample `samples.Reference` column with `COALESCE` (one option set per reference already covers it).
- Quality-based N call (`coverageOrQuality=quality`, "Call N if quality below 20").
- `--multi-best all`, structural-variant discovery, repeat-word filter, expansion word length, Geneious mismatch-weighted consensus.
- Read normalisation / bait pre-filter for speed (Geneious staff recipe); keep all reads every pass for fidelity.
- Partial references (COI seed grown to a mitogenome): extension exists, but a 700 bp seed needs 25+ passes and MITObim-grade recruitment; document "full-length or near-full-length references only" and revisit if users ask.
- MIA / MITObim comparator spike; `samtools consensus` or bcftools as alternative callers; samtools 1.22 bump.
- Multiple references per project ("map to each, keep the best", Westbury) : the existing multi-candidate BLAST machinery could feed it later.
- Keeping `final.bam` could be dropped if disk becomes a complaint; `coverage` writes its own BAM anyway.
