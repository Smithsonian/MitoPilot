# Lane: alt-tools

Survey of open-source tools that could stand in for Geneious Prime "Map to Reference"
inside a MitoPilot Nextflow process (Docker/Singularity), for mitogenome recovery from
Illumina genome skims.

All statements are marked SAYS (sourced, with URL or file:line) or INFER (my reasoning
from sourced facts). ASCII only. Versions, package sizes, and repo activity re-checked
2026-09-02 against the live bioconda / quay.io / GitHub APIs.

This is a completed second pass. Everything load-bearing in the first draft was
re-verified; corrections and new findings are flagged with NEW or CORRECTION.

---

## 0. Baseline: what the MitoPilot image already ships

Source: `/home/dmacguig/Documents/GitHub/MitoPilot/docker/Dockerfile`

| Tool | Version | Dockerfile line |
|---|---|---|
| fastp | 0.23.4 | docker/Dockerfile:27 |
| SPAdes | 4.1.0 | docker/Dockerfile:28 |
| GetOrganelle | 1.7.7.1 | docker/Dockerfile:29 |
| bam-readcount | 1.0.1 (own env) | docker/Dockerfile:30 |
| bowtie2 | 2.5.4 | docker/Dockerfile:31 |
| samtools | 1.21 | docker/Dockerfile:32 |
| minimap2 | 2.28 | docker/Dockerfile:33 |
| tRNAscan-SE / MITOS2 / ARAGORN | 2.0.12 / 2.1.10 / 1.2.41 (own envs) | docker/Dockerfile:34-36 |
| BLAST+ | >= 2.16 asserted, arrives transitively | docker/Dockerfile:39-58 |
| MitoFinder | git HEAD of RemiAllio/MitoFinder | docker/Dockerfile:84-87 |
| default-jre (JVM already paid for) | apt | docker/Dockerfile:13 |

NOT present today (verified by `grep -rn` over `docker/`, `inst/`, `R/`): `bcftools`,
`bwa`, `bwa-mem2`, `ivar`, `freebayes`, `vcfutils`. The grep returns nothing.

INFER: the image already contains a complete short-read mapping stack (bowtie2 +
minimap2 + samtools). `samtools consensus` was introduced in samtools 1.15
(https://github.com/samtools/samtools/releases/tag/1.15), so a map-to-reference-and-
iterate loop can be built today with ZERO new binaries.

**NEW / CORRECTION - the samtools 1.21 vs 1.22 line matters.** I checked the 1.21
tagged man page (https://raw.githubusercontent.com/samtools/samtools/1.21/doc/samtools-consensus.1)
against the current one (https://www.htslib.org/doc/samtools-consensus.html). The
1.21 man page has no `-T`/`--reference` option. The 1.22 release notes say:

- SAYS: "Add `samtools consensus -T ref.fa` functionality. This reports the reference
  value if a consensus value cannot be calculated. (PR #2153)"
- SAYS: "`samtools consensus` now supports proper multi-threading. Previously this was
  restricted to decompression only, but it should now scale better. (PR #2174)"
- SAYS: "`samtools consensus` without `-a` previously still padded with leading Ns in
  some cases. It now consistently removes both leading and trailing Ns."
  (all three: https://github.com/samtools/samtools/releases/tag/1.22)

INFER: if MitoPilot wants a Geneious-style "call the reference base where coverage is
too low" option (as opposed to always N), or threaded consensus, the image needs
samtools >= 1.22. bioconda latest is samtools 1.24
(https://api.anaconda.org/package/bioconda/samtools). This is a version bump of an
already-installed package, not a new dependency, and is the single cheapest capability
upgrade available for this feature.

**NEW - in-repo prior art for the circular-mapping trick.** MitoPilot already does the
CircularMapper "elongate and fold" manoeuvre:

- `R/circularize_asmb.R:522` `count_junction_reads()`; `R/circularize_asmb.R:529`
  `flank <- min(500L, len %/% 2L)`; `R/circularize_asmb.R:546` writes
  `paste0(seq, substr(seq, 1L, flank))` as the mapping reference; then maps with
  bowtie2 (line 550-555) and folds the appended block back
  (`contig_depth()` at `R/circularize_asmb.R:480`).
- Verbatim comment at `R/circularize_asmb.R:466-471`: "The mapping reference is the
  contig followed by a copy of its own first `flank` bases, so a read landing in that
  first `flank` bases aligns equally well to either copy and bowtie2 places it
  arbitrarily. Folding the appended block back onto the contig start recombines the two
  copies into the contig's real coverage".

INFER: the hardest-looking Geneious-parity item (circular reference where mapping wraps
the origin) is already solved in this codebase, with a tested helper, and the map-to-ref
feature can copy that pattern rather than invent one.

**NEW - in-repo prior art for the mapping command itself.** `R/coverage.R:63-75` already
runs `bowtie2-build` then
`bowtie2 --very-sensitive-local --no-unal -x index -1 ... -2 ... | samtools view -bS - | samtools sort -`.
INFER: one iteration of a map-to-ref loop costs approximately what the existing coverage
step already costs per sample, which is a known, accepted runtime in this pipeline.

---

## 1. Taxonomy used below

Following the 2023 human-mitogenome benchmark's framing
(https://bmcbioinformatics.biomedcentral.com/articles/10.1186/s12859-023-05445-3,
Table 1), extended with two categories it does not need:

- **A. Reference-GUIDED mapping + consensus (Geneious-like).** Reads are aligned to a
  reference; the output sequence is derived from the pileup. Reference coordinates are
  the backbone. Single pass.
- **B. Iterative map/bait -> consensus or reassembly -> new reference.** The product
  replaces the reference and the process repeats. This is the category that reproduces
  Geneious "map to reference + fine tuning + extension past reference ends".
- **C. Seed-and-extend de novo.** A seed only recruits or starts; the sequence is built
  from read overlaps, not from reference coordinates.
- **D. Polishing.** Corrects an existing assembly using reads. No recruitment.
- **E. Scaffolding / ordering.** Rearranges existing contigs against a reference,
  does not change base content.

---

## 2. Master comparison tables

### 2A. Serious map-to-reference candidates (categories A and B)

| Tool | Cat | Iterates built in | Extends past ref ends | Circular ref | Reads | Consensus knobs | License | Runtime deps | bioconda (ver, size) | biocontainer | Last push | DOI |
|---|---|---|---|---|---|---|---|---|---|---|---|---|
| **Compose: bowtie2/minimap2 + samtools consensus** | A -> B by looping | you write the loop | no (needs an added step) | via elongate+fold, already in repo | PE/SE Illumina, long | full: `-d`, `-c`, `-H`, `-A`, `--min-MQ`, `--min-BQ`, `-m simple\|bayesian` | MIT/BSD-ish | none new | already installed | n/a | samtools 2026-08-27 | 10.1093/gigascience/giab008 |
| **MITObim 1.9.1** | B | YES (`-start`/`-end`) | YES (default; `--trimoverhang` disables) | no | interleaved FASTQ, `--pair` | `--mismatch`, `--kbait`, `--min_cov`, `--min_len`; no per-base rules, no IUPAC | MIT | Perl + MIRA 4.0.2 | mitobim 1.9.1 (noarch, <0.1 MB); mira 5.0.0rc2 latest, 4.0.2 available | yes | 2020-12-29 | 10.1093/nar/gkt371 |
| **MIA (mapping-iterative-assembler)** NEW | B | YES (`-i` "iterate assembly until convergence") | partial (consensus can grow past ref as reads extend it) | YES (`-c` "reference/assembly is circular") | FASTA/FASTQ fragments; no PE pairing model | `-p 0/1/2` consensus code, kmer filter `-k`, score cutoffs `-H/-S/-N`, `-D` distant-reference mode; N when no base wins | Artistic-2.0 | C, autotools | `mapping-iterative-assembler` 1.0 (0.09 MB linux-64, build _7 rebuilt 2025-08-14) | yes | 2013-07-29 | 10.1126/science.1113485 lineage; MIA introduced in Green et al. 2008, 10.1016/j.cell.2008.06.021 |
| **Westbury aITE script** NEW | B | YES (bash loop, converges on read count) | no | no | SE (aDNA merged) | via kindel `--min-depth` | none stated | bash + BWA + samtools + kindel | n/a (scripts) | n/a | 2024-01-23 | 10.1111/2041-210x.13990 |
| **snippy 4.6.0** NEW | A (iterating is manual) | no (README suggests doing it by hand) | no | no | PE/SE Illumina, or contigs (`--ctgs`) | `--mincov` (10), `--minfrac`, `--minqual` (100); `.aligned.fa` gives `-` at depth 0 and `N` below `--mincov` | GPL-2.0 | Perl + bwa + samtools + freebayes + snpEff (JVM) + vt | snippy 4.6.0 (noarch, 19.9 MB + deps) | yes | 2025-12-12 | none (tool has no paper) |
| **MToolBox** | A/B | yes (internal) | no | no | Illumina | human-specific | GPL-3.0 | Python 2 | not on bioconda | no | 2024-03-05 | 10.1093/bioinformatics/btu483 |
| **IOGA** | B | YES (`--maxrounds`) | yes (reassembly) | no | PE Illumina | none exposed | AGPL-3.0 | Python 2 + bbmap + soapdenovo2 + spades + ALE + picard | no | no | 2016 | 10.1111/bij.12642 |
| **ARC** | B | YES | yes | no | PE FASTQ (unzipped) | none exposed | Apache-2.0 | Python 2 | no | no | 2016-03-29 | 10.1101/014662 |
| **GRAbB** | B | YES | yes | no | Illumina | none exposed | MIT | C + Perl + an assembler | NOT on bioconda | no | 2020-11-06 | 10.1371/journal.pcbi.1004753 |
| **aTRAM 2** | B (per locus) | YES | yes | n/a | Illumina | n/a | BSD-3 | Python + BLAST + assemblers | NOT on bioconda | no | 2026-04-13 | 10.1177/1176934318774546 |
| **MITGARD** | B | YES | yes | no | RNA-seq | none exposed | NOASSERTION | Python + bowtie2 + Trinity/SPAdes | mitgard 1.1 | yes | 2024-02-21 | 10.1093/bib/bbaa429 |
| **MetaCompass** NEW | A/B (metagenome) | YES (reference-guided then de novo for uncovered) | yes | no | Illumina | none mitogenome-shaped | NOASSERTION | Python + snakemake + bowtie2 | metacompass 1.12 | yes | 2026-08-19 | arXiv:2403.01578 |
| **shiver** NEW | A (contig-informed remap) | no | no | no | PE Illumina | via its own consensus caller, virus-shaped | GPL-3.0 | Python + smalt/bowtie + samtools | shiver 1.7.3 | yes | 2024-05-03 | 10.1093/ve/vey007 |
| **Tanoti** NEW | A (aligner only) | no | no | no | PE/SE Illumina | none (emits SAM) | GPL-3.0 | C + BLAST | NOT on bioconda | no | 2021-06-18 | none |
| **mtDNA-Server 2 / mutserve** | A | no | no | acknowledges origin break | Illumina | heteroplasmy >= 2% | MIT | Java | not for this purpose | n/a | 2024-12-23 | 10.1093/nar/gkw247 |
| **viral-ngs refine_assembly** | A (run twice) | no (pipeline runs it 2x) | no | no | Illumina | `--min_coverage` (3), `--major_cutoff` (0.5) | NOASSERTION, repo ARCHIVED | Novoalign (proprietary) + GATK3 + Picard | no | no | archived | 10.1186/s13059-014-0571-3 lineage |

### 2B. De novo organelle assemblers (category C) - not map-to-reference

| Tool | What it does | License | bioconda | Last push | DOI | Note |
|---|---|---|---|---|---|---|
| GetOrganelle 1.7.7.1 | seed used as a probe to recruit reads in successive rounds, then SPAdes graph traversal | GPL-3.0 | getorganelle 1.7.7.1 | 2025-02-13 | 10.1186/s13059-020-02154-5 | already in MitoPilot |
| MitoFinder 1.4.1 | MEGAHIT/IDBA/metaSPAdes then annotate vs reference GenBank | no LICENSE file | mitofinder 1.4.1 | 2025-09-02 | 10.1111/1755-0998.13160 | already in MitoPilot |
| NOVOPlasty 4.3.5 | seed-and-extend from a single seed | bespoke non-commercial EULA | novoplasty 4.3.5 (0.2 MB) | 2024-02-04 | 10.1093/nar/gkw955 | LICENSE HAZARD, see 3.7 |
| MitoZ 3.6 | de novo + annotation | GPL-3.0 | mitoz 3.6 | 2024-11-06 | 10.1093/nar/gkz173 | |
| MitoFlex | de novo, fastest in benchmark | GPL-3.0 | not on bioconda | 2023-05-10 | 10.1093/bioinformatics/btab111 | |
| MEANGS | seed-free de novo | GPL-3.0 | not on bioconda | - | 10.1093/bib/bbab538 | worst accuracy in benchmark |
| ORG.Asm | seed-and-extend | CeCILL | not on bioconda | - | - | GitLab-hosted |
| mtGrasp 1.1.10 | ABySS + Sealer + Pilon + ntJoin + MITOS | GPL-3.0 | mtgrasp 1.1.10 (163.6 MB noarch) | 2025 | 10.1111/2041-210X.14506 | newest maintained metazoan pipeline |
| Norgal | k-mer frequency, reference-free | - | not on bioconda | - | 10.1186/s12859-017-1927-y | failed benchmark testing |
| mitoMaker | SOAPdenovo/MIRA/blast wrapper | - | not on bioconda | - | preprint only | failed benchmark testing |
| IVA 1.0.11 NEW | iterative seed-and-extend de novo virus assembler; explicitly reference-free | GPL-3.0 | iva 1.0.11 | 2021-05-17, repo carries an "Unmaintained" badge | 10.1093/bioinformatics/btv120 | name is misleading, it does NOT map to a reference |
| MitoHiFi / mitoVGP / Organelle_PBA | long reads | MIT / - / - | mitohifi not on bioconda | - | 10.1186/s12859-023-05385-y etc | out of scope |
| MitoGeneExtractor 1.9.6 NEW | exonerate-based reference-guided assembly of individual protein-coding mito genes (COI etc) from reads | AGPL-3.0 | mitogeneextractor 1.9.6 | 2026-04-05 | 10.1111/1755-0998.13918 | per-gene, amino-acid guided; not whole-mitogenome |

### 2C. Consensus callers and polishers (the decisive layer)

Full head-to-head in section 4. Package facts:

| Tool | bioconda version | linux-64 size | License | Repo last push |
|---|---|---|---|---|
| samtools (consensus subcommand) | 1.24 (image has 1.21) | already installed | MIT/Expat | 2026-08-27 |
| bcftools (consensus subcommand) | 1.24 | 0.97 MB | MIT/Expat | 2026-09-02 |
| ivar | 1.4.4 | 1.97 MB | GPL-3.0 | 2026-09-02 |
| kindel | NOT on bioconda (pip only) | - | GPL-3.0 | 2025-08-16 |
| VarScan | 2.4.6 | JVM jar | non-OSI academic | - |
| Pilon | 1.24 | JVM jar | GPL-2.0 | 2022-04-10 |
| ANGSD (`-doFasta`) NEW | 0.940 | - | GPL-2.0 | - |
| Racon / Medaka | 1.5.0 / 2.2.2 | - | MIT / Oxford Nanopore | - |
| snippy | 4.6.0 | 19.9 MB noarch + heavy deps | GPL-2.0 | 2025-12-12 |

### 2D. Scaffolding / other (category E, and near misses)

| Tool | Why it is not the answer |
|---|---|
| RagTag 2.1.0 | SAYS "RagTag performs contig-to-reference alignment only. It does not conduct read mapping or generate consensus sequences ... RagTag explicitly does not alter contig sequences". Already measured and rejected in this project (memory `ragtag-not-adopted`). |
| Circlator 1.5.5 | needs corrected PacBio/nanopore reads. |
| AlignGraph | extends/joins existing contigs using a related reference; unmaintained. |
| TASR | SSAKE-based targeted assembly for short targets (fusions, HLA); no per-base consensus rules; not on bioconda. |
| SPAdes `--trusted-contigs` | SAYS "These options are not intended for contigs of the related species. Only contigs of the same genome should be specified." The manual forbids exactly our use. |
| CircularMapper 1.93.5 | not an assembler; it is the elongate-and-fold trick as a Java tool. Copy the trick (already in this repo), not the tool. |
| ReferenceSeeker | picks which reference genome to use (bacterial); irrelevant to mapping/consensus. |
| NOVOWrap | chloroplast NOVOPlasty wrapper; AGPL, not on bioconda; irrelevant. |
| bbmap/bbduk 40.02, seqtk 1.5 | useful as read-recruitment/baiting utilities if a baiting step is wanted; neither is a map-to-ref method by itself. |
| "IterMap" | no bioinformatics tool by that name exists; searches return JavaScript/Rust iterator libraries. |
| "IMRA" | no such bioinformatics tool found. IMR/DENOM (below) is the real thing. |

---

## 3. Tool-by-tool detail

### 3.1 MITObim (category B) - the best-known published analogue

- **What it does.** SAYS: "The script is performing three steps and iteratively
  repeating them: (i) Deriving reference sequence from previous mapping assembly,
  (ii) in silico baiting using the newly derived reference (iii) previously fished
  reads are mapped to the newly derived reference leading to an extension of the
  reference sequence."
  (https://github.com/chrishah/MITObim/blob/master/README.md, README line 73)
- **Iterates:** YES, `-start` / `-end`.
- **Extends past ends:** YES; README line 122 `--trimoverhang` "trim overhang up- and
  downstream of reference, i.e. don't extend the bait, just re-assemble (default: no)".
- **Partial-reference seeding (NEW detail).** README line 80: "Tutorial III achieves the
  same goal using solely a ~700 bp barcoding sequence as initial seed reference", and
  the tutorial itself uses `Tthymallus-COI-partial-HQ961018.fasta` with `-end 100`
  (README line 300). SAYS: this is a supported, documented use case.
- **`--quick` mode (NEW detail).** README line 268: memory can be bypassed "via an
  initial in-silico baiting step using mirabait ... This strategy can be performed by
  using the --quick flag, together with providing a reference sequence in fasta format."
  README line 273 gives an "approximate runtime: 4 min" for the tutorial dataset.
- **Consensus rules exposed:** `--kbait <int>` (default 31), `--mismatch <int>`
  ("number of allowed mismatches in mapping - only for illumina data (default: 15% of
  avg. read length)"), `--min_cov <int>` (minimum AVERAGE coverage of contigs to be
  retained, default 0 = off, i.e. a whole-contig filter and NOT a per-base rule),
  `--min_len`, `--split`. No IUPAC, no per-base ambiguity threshold, no N-below-coverage.
- **Proofreading is off.** README line 65: "The proofreading option described in the
  paper is at the moment disabled in MITObim 1.8 and will be enabled once I got a chance
  to thoroughly test its behavior with MIRA 4. If you are planning to use the
  proofreading functunality please refer to MITObim 1.6".
- **NFS hostility.** README lines 127-128: `--redirect_tmp` "useful in case you are
  running MITObim on an NFS mount"; `--NFS_warn_only` "allow MIRA to run on NFS mount
  without aborting". INFER: on an HPC cluster with NFS scratch this is a real
  operational tax.
- **Single-threaded (NEW, decision-relevant).** SAYS: "NOVOPlasty, ORG.Asm and MITObim
  do not support multithreading" (https://pmc.ncbi.nlm.nih.gov/articles/PMC10498642/).
- **License:** MIT. **bioconda:** `mitobim` 1.9.1 noarch, tiny (it is one Perl script);
  `mira` bioconda latest 5.0.0rc2, with 4.0.2 among the available files, and MITObim
  1.9.1 is "stable - relies on MIRA 4.0.2" (README line 9, line 44). Both have
  biocontainers (quay.io/biocontainers/mitobim and .../mira both return HTTP 200).
- **Maintenance:** last push 2020-12-29, 118 stars, MIT, not archived
  (https://api.github.com/repos/chrishah/MITObim). MIRA upstream pushed 2025-01-02,
  GPL-2.0, 35 stars.
- **MIRA scope caution.** SAYS: "Illumina projects with more than 40 to 60 million reads
  start to be so resource intensive that you might be better served with other
  assemblers or mapping programs"
  (https://raw.githubusercontent.com/DrMicrobit/mira/master/README.md).
- **Publication:** Hahn, Bachmann & Chevreux 2013, NAR 41:e129, DOI 10.1093/nar/gkt371.
- **Accuracy evidence (verified quote).** SAYS: "Overall, the most accurate results were
  obtained with MITObim using mismatch values of 3 or 5, and the phylogenetically closest
  bait reference sequence. Accuracy could be further improved by combining results from
  multiple bait references."
  (Westbury & Lorenzen, https://www.biorxiv.org/content/10.1101/2021.12.16.472923v1.full ;
  published Methods Ecol Evol, DOI 10.1111/2041-210x.13990)
- SAYS: "Overall, there was no obvious relationship between PWD and phylogenetic distance
  of the bait reference when using MITObim, regardless of mismatch para[meter]" (same
  source). INFER: MITObim degrades gracefully with a divergent reference; a plain
  BWA/bowtie2 loop does not, which is the main argument in MITObim's favour.

### 3.2 MIA - mapping-iterative-assembler (category B) - NEW, and the closest feature match to Geneious

This tool was missing from the first draft and is the single most important addition.

- **What it does.** SAYS: "The basic idea of this program is to align DNA sequencing
  fragments (shotgun or targeted resequencing) to a reference, then call a consensus.
  Then the consensus is used as new reference and the process is repeated until
  convergence. Since it was originally designed to be used on ancient DNA, it supports a
  position specific substitution matrix, which improves both alignment and consensus
  calling on chemically damaged aDNA."
  (https://raw.githubusercontent.com/mpieva/mapping-iterative-assembler/master/README.md)
- SAYS: "MIA has been used to assemble a number of Neandertal and early modern human
  mitochondria. Occasionally it has been used on smallish nuclear regions, but it will
  probably not scale to a genome wide analysis." (same)
- **Man page options** (https://raw.githubusercontent.com/mpieva/mapping-iterative-assembler/master/man/mia.1):
  - `-c` "means reference/assembly is circular" <- direct Geneious parity for circular refs
  - `-i` "iterate assembly until convergence" <- built-in convergence stop
  - `-F` "only output the FINAL assembly, not each iteration"
  - `-D` "reference sequence is only distantly related. Low scoring reads will NOT be
    removed after each iteration" <- a divergent-reference mode
  - `-k LENGTH` "use kmer filter with kmers of this length. The kmer filter requires that
    a sequence fragment have at least one kmer of the specified length in common with the
    reference sequence in order to align it. For 36nt Solexa data, a value of 12 works
    well." <- this is Geneious's "word length" knob, by another name
  - `-p <consensus calling code>` "specifies how the new consensus assembly sequence is
    called at each iteration (default: 1)"; code 1 = "Any base whose aggregate score is
    MIN_SC_DIFF_CONS better than all [o]thers is the assembly base. If none is, then N is
    the assembly base."; code 2 = "The best scoring base whose aggregate score is better
    than MIN_SCORE_CONS is the assembly base. If none is, then N is the assembly base."
  - `-H SCORE` hard score cutoff, `-S SLOPE` / `-N INTERCEPT` length-vs-score cutoff line
    <- a sensitivity ladder, though expressed in raw scores
  - `-s` substitution matrix, with shipped aDNA matrices, default "flat matrix" with
    "MATCH=200, MISMATCH=-600, N=-100 for all positions"
- **The catch, stated in its own man page.** SAYS: "All default parameters, like
  SCORE_CUTOFF_BUFFER or MIN_SCORE_CONS can be changed by modifying the source file
  params.h in the mia source directory and afterwards recompiling." INFER: the
  Geneious-shaped thresholds we would most want to expose (the consensus score
  difference) are compile-time constants, not CLI flags. That is a hard ceiling on what
  a MitoPilot UI could offer over MIA.
- **No paired-end model.** The man page takes `-f fragment reads` (fasta or fastq) with
  no mate file and no insert-size handling. INFER: MIA treats reads as independent
  fragments, so the Geneious "expected distance between paired reads" penalty has no
  analogue. For merged/overlapping aDNA reads that is fine; for standard Illumina PE
  skims it discards pairing information.
- **License:** Artistic-2.0. **Language:** C, autotools build.
- **Packaging (the surprise):** on bioconda as `mapping-iterative-assembler` **1.0**,
  0.09 MB for linux-64, and the recipe is still being REBUILT: build `_7` uploaded
  2025-08-14, with linux-64, linux-aarch64, osx-64 and osx-arm64 artifacts
  (https://api.anaconda.org/package/bioconda/mapping-iterative-assembler).
  quay.io/biocontainers/mapping-iterative-assembler exists (HTTP 200).
- **Maintenance:** upstream https://api.github.com/repos/mpieva/mapping-iterative-assembler
  last pushed 2013-07-29, 19 stars. INFER: frozen upstream but alive in packaging. A
  0.09 MB C binary with no runtime deps is about as low-risk as a frozen tool gets.
- **Publication:** the method is from Green et al. 2008, "A complete Neandertal
  mitochondrial genome sequence determined by high-throughput sequencing", Cell,
  DOI 10.1016/j.cell.2008.06.021.
- **Known weakness, sourced.** SAYS: "when using a more distant bait reference, MIA
  requires much more memory and CPU time than MITObim (Hahn et al., 2013), and is
  therefore not as suitable for species when only relatively divergent [bait references
  are available]" (Westbury & Lorenzen, westbury_clean.txt; same biorxiv URL as above).
- **INFER (bottom line).** On paper MIA matches more Geneious semantics than anything
  else surveyed: iterate-to-convergence, circular reference, kmer seed length, distant-
  reference mode, and an explicit consensus-calling rule with N as the fallback. Against
  it: no PE model, thresholds hidden in params.h, upstream dead since 2013, a
  human-aDNA-shaped substitution-matrix design, and a documented cost blow-up on
  divergent references. It belongs on the shortlist as the "wrap an existing tool"
  alternative to MITObim, and it is a better shape for a FULL-length reference than
  MITObim is, while MITObim is the better shape for a PARTIAL seed.

### 3.3 The Westbury aITE script (category B) - NEW - published prior art for the composed loop

The aDNA benchmark paper built exactly the loop MitoPilot would write, and deposited it.

- SAYS: "As BWA is not specifically designed for iterative mapping, we created a pipeline
  using bash tools for this study, which we called 'ancient ITErative mapper' (aITE
  mapper). In short, this method aligns reads to a bait reference using BWA aln, filters
  the output, and removes duplicates using SAMtools v1.9, creates a consensus fasta
  sequence using ANGSD v0.921, and uses the output consensus fasta sequence as a new
  reference sequence in subsequent mappings. This process is repeated until either no new
  reads map, or for a maximum of 100 iterations."
  (https://www.biorxiv.org/content/10.1101/2021.12.16.472923v1.full)
- The deposited script is at
  https://raw.githubusercontent.com/Mvwestbury/Iterative_mapping/main/BWA/aITE_mapper.sh
  (repo https://github.com/Mvwestbury/Iterative_mapping, last push 2024-01-23, no
  license file). Its actual loop body, verbatim structure:
  1. `bwa aln -l 999 -n 0.04 ... | bwa samse ... | samtools view -F 4 -q $MAPQ -uS - | samtools sort`
  2. `samtools rmdup -S`
  3. `kindel consensus --min-depth 3 <bam> > con.N.fa`
  4. `bwa index con.N.fa`, remap, repeat, `for num in {1..100}`
  5. convergence test: compare `samtools view -c` between iteration N and N+1; on
     equality print "Finished after ${num} iterations with $N1 reads mapping", write the
     final consensus, and `break`.
  6. per-iteration logging of read count, mean depth, and zero-coverage base count via
     `samtools depth -a | awk`.
- **CORRECTION / discrepancy worth knowing:** the PAPER says the consensus is called with
  ANGSD; the DEPOSITED SCRIPT calls `kindel consensus --min-depth 3`. The repo README
  attributes kindel only to the non-iterative "BWA default" script, but aITE_mapper.sh
  itself uses kindel. INFER: treat the exact consensus caller in that study as
  ambiguous; the loop STRUCTURE is the reusable part.
- **INFER (why this matters most).** This gives MitoPilot a published, peer-reviewed,
  citable precedent for the exact composed loop, including a concrete convergence rule
  ("stop when the number of mapped reads stops changing") and a cap (100). It is roughly
  30 lines of shell. It also confirms the per-iteration QC outputs worth emitting
  (read count, mean depth, N count), which map directly onto MitoPilot's existing
  coverage/QC surface.

### 3.4 snippy (category A) - NEW - the "reference is a GenBank file" precedent

- **Why it is relevant to our exact requirement.** MitoPilot wants the user to supply a
  GenBank (.gb) reference. snippy is the mainstream tool that already does this:
  SAYS usage `snippy --cpus 16 --outdir mysnps --ref Listeria.gbk --R1 ... --R2 ...`, and
  "If you supply a Genbank file as the `--reference` rather than a FASTA file, Snippy
  will use it to annotate the variants"
  (https://raw.githubusercontent.com/tseemann/snippy/master/README.md, README lines 22
  and 114).
- **Outputs that match Geneious semantics.** README line 87-89:
  - `.aligned.fa` = "A version of the reference but with `-` at position with `depth=0`
    and `N` for `0 < depth < --mincov` (**does not have variants**)"
  - `.consensus.fa` = "A version of the reference genome with *all* variants instantiated"
  - `.consensus.subs.fa` = "A version of the reference genome with *only substitution*
    variants instantiated"
  INFER: `.consensus.subs.fa` is a same-length-as-reference product (no indels applied),
  which is exactly what a coordinate-preserving map-to-reference output looks like.
- **Thresholds.** README lines 158-160: `--mincov` "the minimum number of reads covering a
  site to be considered (default=10)", `--minfrac` "the minimum proportion of those reads
  which must differ from the reference", `--minqual` "the minimum VCF variant call
  'quality' (default=100)".
- **Iteration is explicitly a manual afterthought.** README: "You may wish to _iterate_
  this process by using `corrected.fa` as a new `--ref` for a repeated run of Snippy.
  Sometimes correcting one error allows BWA to align things it couldn't before, and new
  errors are uncovered." Followed immediately by "Snippy may not be the best way to
  correct assemblies - you should consider dedicated tools such as PILON or iCorn2".
- **Cost.** bioconda `snippy` 4.6.0 noarch is 19.9 MB BEFORE dependencies, and it drags in
  bwa, samtools, freebayes, vt, snpEff (JVM), and a Perl stack. It is designed for
  haploid bacterial genomes; no circular handling; no extension past ends; no IUPAC in
  the consensus.
- **INFER.** snippy is the wrong tool to adopt (too heavy, wrong organism model, no
  iteration, no circularity) but it is the right tool to IMITATE for the output contract:
  emit both a "reference with variants applied" FASTA and an "aligned/masked" FASTA where
  zero coverage and sub-threshold coverage are visibly distinct. That two-file output is
  a good UI answer to "did we actually recover this region, or did we just inherit the
  reference?"
- License GPL-2.0, 591 stars, last push 2025-12-12
  (https://api.github.com/repos/tseemann/snippy).

### 3.5 MIRA mapping mode (category A/B, the engine inside MITObim)

- SAYS: "MIRA can also be used for mapping assemblies and automatic tagging of difference
  site (SNPs, insertions or deletions) of mutant strains against a reference sequence"
  (https://raw.githubusercontent.com/DrMicrobit/mira/master/README.md).
- GPL-2.0; bioconda `mira` (latest 5.0.0rc2; 4.0.2 present for the MITObim pin); upstream
  pushed 2025-01-02.
- INFER: MIRA is algorithmically the closest cousin to the Geneious mapper (seed/expand
  plus an integrated editor that re-aligns reads to each other around indels, which is
  functionally Geneious "Fine Tuning"). But its configuration surface is a bespoke
  manifest language and it is the heaviest, least standard piece of any option here.

### 3.6 GRAbB, ARC, IOGA, aTRAM (category B, all disqualified on packaging or Python 2)

- **GRAbB.** SAYS: "GRAbB identifies reads corresponding to a target region by using exact
  31-mer matching"; "GRAbB is shown to be more efficient than MITObim in terms of speed,
  memory and disk usage" (https://github.com/b-brankovics/grabb ;
  DOI 10.1371/journal.pcbi.1004753). MIT, last push 2020-11-06, 15 stars.
  **NOT on bioconda** (re-verified 2026-09-02: `grabb` -> "could not be found").
- **ARC.** SAYS: stages that "align reads to reference targets, distribute reads into
  target-specific bins, perform assemblies for each bin, and iterate by replacing
  reference targets with assembled contigs"
  (https://www.biorxiv.org/content/10.1101/014662v2). Apache-2.0, Python 2, last push
  2016-03-29, not on bioconda. Disqualified.
- **IOGA.** Loop verified in source: `IOGA_loop()` at
  https://raw.githubusercontent.com/holmrenser/IOGA/master/IOGA.py:305, `--maxrounds/-m`
  at line 426, bbmap per round (line 98), SOAPdenovo2 (210), SPAdes (262), ALE scoring
  (292) to pick the best round. Header line 6 lists deps
  "bbmap,bbduk,seqtk,soapdenovo2,spades.py,ALE,BioPython,picardtools,samtools".
  Python 2, AGPL-3.0. Benchmark: SAYS "IOGA took the longest execution time of 1278 s"
  and "IOGA utilized the highest computational memory of 11.858 GB" (16 threads, 1000X
  simulated data); at 4000X it was "approximately 39 min" and 11.87 GB
  (https://pmc.ncbi.nlm.nih.gov/articles/PMC10498642/). Disqualified for adoption; keep
  as prior art for the ALE-style "score each round, keep the best" stopping rule.
- **aTRAM 2.** BSD-3, actively maintained (last push 2026-04-13, 38 stars), but not on
  bioconda and designed for many independent loci with a BLAST DB over the whole read
  library. Wrong shape for one circular molecule per sample.

### 3.7 NOVOPlasty (category C) - and the license hazard, re-verified

- SAYS: "NOVOPlasty developed a seed-and-extend algorithm that assembles organelle genomes
  from whole genome sequencing (WGS) data, starting from a related or distant single seed
  sequence" (https://academic.oup.com/nar/article/45/4/e18/2290925).
- **LICENSE (re-read in full 2026-09-02,
  https://raw.githubusercontent.com/ndierckx/NOVOPlasty/master/LICENSE).** It is a bespoke
  EULA titled "NOVOPlasty - Terms and conditions", not an OSI licence. Verbatim clauses:
  - "Non-Commercial: Licensee may not use Software for commercial purposes. for the
    purpose of this license, commercial purposes means that a 3rd party has to pay in
    order to access Software or that the Website that runs Software is behind a paywall."
  - "Including the Right to Create Derivative Works: Licensee may create derivative works
    based on Software ... as long as no distribution of the derivative works is made"
  - "Payment: In consideration of the License granted under clause 2, Licensee shall pay
    Licensor a fee, via Credit-Card, PayPal or any other mean which Licensor may deem
    adequate. Failure to perform payment shall construe as material breach of this
    Agreement."
  - "Binary Restricted: Licensee may sublicense Software as a part of a larger work
    containing more than Software, distributed solely in Object or Binary form ..."
  GitHub reports the licence as NOASSERTION.
  INFER: shipping NOVOPlasty inside a public MitoPilot image is a legal question for the
  maintainers, not a technical one. Combined with the fact that it is not map-to-reference
  at all, it stays off the shortlist.
- Performance (for completeness): SAYS "NOVOPlasty utilized the least computational memory
  of approximately 0.098 GB" at 1000X and 0.17 GB at 4000X, and is single-threaded
  (https://pmc.ncbi.nlm.nih.gov/articles/PMC10498642/).

### 3.8 GetOrganelle (already in MitoPilot) - why map-to-ref is genuinely additive

- SAYS: "`-s` GetOrganelle takes the seed (fasta format; if this was not provided, the
  default is `GetOrganelleLib/SeedDatabase/*.fasta`) as probe, the script would recruit
  target reads in successive rounds (extending process). The default seed works for most
  samples, but using a complete organelle genome sequence of a related species as the seed
  would help the assembly in many cases"
  (https://raw.githubusercontent.com/Kinggerm/GetOrganelle/master/README.md, README
  lines 118-122).
- INFER: the seed drives RECRUITMENT; the sequence itself comes from a SPAdes graph. So
  GetOrganelle cannot produce a reference-coordinate consensus, cannot honour a per-base
  consensus threshold, and cannot produce a same-length-as-reference product. A
  map-to-reference method adds capability that GetOrganelle structurally cannot provide,
  even in its reference-seeded mode. This is the strongest argument that the feature is
  not redundant with what MitoPilot already ships.
- GPL-3.0, last push 2025-02-13, 397 stars, bioconda 1.7.7.1.
  DOI 10.1186/s13059-020-02154-5.

### 3.9 Other reference-guided pipelines checked and set aside

- **MToolBox** (A/B): human rCRS/RSRS + haplogroups + Python 2; GPL-3.0; not on bioconda;
  last push 2024-03-05. SAYS the benchmark's overall winner: "Based on the overall
  performance metrics and consistency in assembly quality for all sequencing data,
  MToolBox performed the best" (https://pmc.ncbi.nlm.nih.gov/articles/PMC10498642/).
  Disqualified: hard-wired to the human reference.
- **mtDNA-Server 2 / mutserve** (A): human heteroplasmy caller. Useful only for its
  statement of the origin problem: SAYS the control region "spans the artificial break in
  the circular genome (coordinates chrM:16024-16569 and chrM:1-576), which can make it
  challenging to call variants in this region"
  (https://mitoverse.readthedocs.io/mtdna-server/mtdna-server/).
- **MITGARD** (B, RNA-seq): "The pipeline takes RNA-seq data as input and assembles a
  mitochondrial genome using as a reference a mitogenome provided by the user"
  (https://academic.oup.com/bib/article/22/5/bbaa429/6123950). Right shape, wrong data
  type; 17 stars; NOASSERTION licence.
- **MetaCompass** (NEW, A/B for metagenomes): reference-guided assembly then de novo for
  uncovered regions; bioconda 1.12; repo active (2026-08-19); NOASSERTION licence;
  arXiv:2403.01578 / PMC11188144. INFER: architecturally the right idea (reference-guided
  first, de novo to fill), but its unit of work is a metagenome with a reference
  database, not one mitogenome with one reference. Wrong granularity, heavy.
- **shiver** (NEW, A): GPL-3.0, bioconda 1.7.3, last push 2024-05-03. HIV-specific
  contig-corrected reference construction then remapping. INFER: the "build a
  sample-specific reference, then map to it" idea is exactly right, but the
  implementation is soaked in HIV assumptions (primer/adapter handling, HIV BLAST DBs).
- **Tanoti** (NEW, A): SAYS "Tanoti is a BLAST guided reference based short read aligner.
  It is developed for maximising alignment in highly variable next generation sequence
  data sets (Illumina)"; "Tanoti's read alignment performance is superior to BWA and
  Bowtie in our comparisons on small viral genomes, giving greater depth with highly
  variable reads without losing accuracy of alignment"
  (https://raw.githubusercontent.com/vbsreenu/Tanoti/master/README.md). GPL-3.0, 4 stars,
  last push 2021-06-18, source-build only, NOT on bioconda. INFER: interesting as a
  divergent-reference aligner (the one axis where bowtie2 is weakest), but 4 stars, no
  packaging, and no paper make it an unacceptable dependency. If divergence turns out to
  be the binding constraint, the cheaper answers are bowtie2 `--very-sensitive-local`
  with a relaxed `--score-min`, minimap2 with a lower `-k`, or MITObim/MIA.
- **IVA** (NEW, C): despite the name "Iterative Virus Assembler", SAYS it is a "de novo
  virus assembler of Illumina paired reads" and the repo carries an explicit
  "Unmaintained" badge plus "we currently do not have the resources to provide support"
  (https://raw.githubusercontent.com/sanger-pathogens/iva/master/README.md). The
  "iterative" refers to contig extension, not to reference re-mapping. GPL-3.0, bioconda
  1.0.11, DOI 10.1093/bioinformatics/btv120. Not a candidate.
- **viral-ngs `assembly.py refine_assembly`** (A): semantics verified from source
  (https://raw.githubusercontent.com/broadinstitute/viral-assemble/master/assembly.py):
  "we take a crude assembly, align all reads back to it, and modify the assembly to the
  majority allele at each position based on read pileups"; `--min_coverage` (argparse
  default 3) "Minimum read coverage required to call a position unambiguous";
  `--major_cutoff` (default 0.5) "If the major allele is present at a frequency higher
  than this cutoff, we will call an unambiguous base at that position. If it is equal to
  or below this cutoff, we will call an ambiguous base representing all possible alleles
  at that position". Disqualified for adoption: Novoalign is proprietary, GATK3 is
  required, and the repo is ARCHIVED
  (https://api.github.com/repos/broadinstitute/viral-assemble -> archived: true).
  ADOPT THE SEMANTICS: min_coverage + major_cutoff + IUPAC-for-ties is the Geneious
  consensus contract expressed in two numbers.
- **ngs_mapper (VDBWRAIR)**: single-pass viral mapping pipeline, Python 2 era, not
  iterative. Reference only for consensus thresholds.
- **IMR/DENOM**: SAYS "At each iteration, reads are aligned to the current version of a
  consensus sequence for a genome, high-confidence SNPs and indels are called, and
  incorporated into a new consensus, with this process repeated until additional rounds
  of iteration produce few or alternating changes in the consensus sequence"
  (http://mtweb.cs.ucl.ac.uk/mus/www/19genomes/IMR-DENOM/description.html). 2011-era
  Arabidopsis/mouse-specific package, not packaged, not maintained. Adopt the convergence
  wording, not the code.

### 3.10 nf-core prior art (the Nextflow-native answer)

- **nf-core/viralmetagenome** (MIT, 40 stars, last push 2026-09-02; preprint
  https://www.biorxiv.org/content/10.1101/2025.06.27.661954). Verified from
  https://nf-co.re/viralmetagenome/1.1.3/docs/usage/workflow/5_variant_and_refinement :
  - SAYS the refinement loop is literally: "Uses current consensus as reference" ->
    "Maps reads back to this reference" -> "Variants are called from the mappings" ->
    "A consensus genome is generated based on the variant calls" -> "Repeats steps 1-4
    for specified number of iterations (default: 2)".
  - SAYS "The mapping tool can be specified with the `--mapper` parameter, the default is
    `bwamem2`" with `--intermediate_mapper` for intermediate cycles.
  - SAYS "The variant caller can be specified with the `--variant_caller` parameter, the
    default is `ivar`", and "The consensus caller can be specified with the
    `--consensus_caller` parameter, the default is `ivar`. The intermediate consensus
    caller ... is by default `bcftools`."
  - SAYS "Variant calling is done with BCFtools and iVar, here a SNP will need to have at
    least a depth of 5 and a base quality of 20."
  - SAYS "Variant filtering: filter out variants with an allelic depth of less than 75% of
    the average depth of the sample."
  - SAYS "Areas of low frequency are more easily deleted and not carried along with iVar,
    this can be a bad thing during the iterative improvement of the consensus but is a
    good thing at the final consensus step." INFER: this is why they use bcftools for
    intermediate rounds and ivar for the final one; worth copying if we ever offer both.
  - Reference choice is by Mash k-mer distance against a multi-FASTA of candidates. INFER:
    MitoPilot's existing multi-candidate BLAST reference machinery (memory
    `multi-blast-ref-candidates`) plays the same role.
- **nf-core/viralrecon**: offers "VarScan 2, BCFTools, BEDTools, and iVar variants and
  consensus" (https://github.com/nf-core/viralrecon). INFER: confirms bcftools and ivar
  are the two mainstream consensus recipes in Nextflow-land.
- **NEW - nf-core/modules already exist for every piece.** All of these returned HTTP 200
  from the GitHub contents API on 2026-09-02:
  `modules/nf-core/samtools/consensus/main.nf`, `.../bcftools/consensus/main.nf`,
  `.../ivar/consensus/main.nf`, `.../bowtie2/align/main.nf`, `.../minimap2/align/main.nf`,
  `.../samtools/depth/main.nf`. INFER: even the process bodies do not have to be invented;
  they can be read as reference implementations (MitoPilot writes its own processes, but
  the module source documents the exact flags and output contracts the community settled
  on).

### 3.11 Circular-reference handling

- **CircularMapper (EAGER).** SAYS: "A method to improve mappings on circular genomes,
  using the BWA mapper"; "Reads that have a starting position within the unmodified
  reference genome and simultaneously have an end position in the modified region are
  considered as overlapping reads, spanning the circular overlap region of the reference.
  These reads are split according to their overlap and are afterward placed at their
  correct positions"; and the motivation: "Using the CircularMapper enables researchers to
  apply mitochondrial (mtDNA) haplogroup assignment methods ... with higher certainty, as
  many phylogenetically informative positions can be found at the beginning and the end of
  the mtDNA reference sequence" (https://github.com/apeltzer/CircularMapper ;
  https://pmc.ncbi.nlm.nih.gov/articles/PMC4815194/ ; DOI 10.1186/s13059-016-0918-z).
  GPL-3.0, Java, 11 stars, last push 2022-03-04; bioconda `circularmapper` 1.93.5 exists.
- INFER: do not add the tool. MitoPilot already implements the same elongate-and-fold
  manoeuvre in `R/circularize_asmb.R:466-555`, and the published method is what that code
  does. Reuse the in-repo pattern; cite CircularMapper as the published justification.
- **MIA `-c`** is the only surveyed tool with first-class circular-reference support built
  into the mapper itself.

---

## 4. Consensus callers head to head (the decisive comparison)

This determines whether Geneious consensus semantics can be reproduced at all.

| Capability | `samtools consensus` (1.15+) | `bcftools consensus` | `ivar consensus` | `kindel` | Pilon |
|---|---|---|---|---|---|
| In the MitoPilot image today | YES (samtools 1.21) | no (+0.97 MB) | no (+1.97 MB) | no (pip only) | no (JVM jar) |
| Input | BAM directly | needs mpileup + call VCF + reference FASTA | needs `samtools mpileup -aa -A -d 0 -Q 0` piped in | BAM directly | BAM + FASTA |
| IUPAC ambiguity codes | YES: `-A, --ambig` "Enables IUPAC ambiguity codes in the consensus output. Without this the output will be limited to A, C, G, T, N and *" | YES: `-I, --iupac-codes` "output variants in the form of IUPAC ambiguity codes determined from FORMAT/GT fields" | YES, automatic: "If one base is not enough to match a given frequency, then an ambigious nucleotide is called at that position" | no (majority only) | no |
| N below depth | YES: `-d, --min-depth D` (default 1), "Failing this depth check will produce consensus 'N', or absent if it is an insertion. Note this check is performed after filtering by flags and mapping/base quality." | NOT natively; needs `-m/--mask FILE` BED plus `--mask-with CHAR`, and `-a/--absent`, `-M/--missing` for uncalled positions | YES: `-m` (default 10), `-n` char to print below it (default N), `-k` to drop those regions | YES: `--min-depth` (default 1) | `--mindepth` default 10% of mean coverage or 5 |
| Majority / threshold rule | `--mode simple` + `-c, --call-fract` (default **0.75**) "Require at least C fraction of bases agreeing with the most likely consensus call to emit that base type ... Failing this check will output 'N'"; `-H, --het-fract` for the ambiguity trigger. Default mode is Bayesian, "derived from the 'Gap5' consensus algorithm" | via `bcftools call` genotype calling; not a direct frequency threshold | `-t` frequency threshold (default 0 = plain majority); documented ladder: 0 majority, 0.2, 0.5 strict, 0.9 very strict; `-q` min base quality (default 20); `-c` min insertion frequency (default 0.8) | fixed majority | no user frequency knob |
| Quality-aware ("Highest Quality" analogue) | YES: Bayesian mode uses base AND mapping qualities, with `--no-adj-qual`, `--no-use-MQ`, `--no-adj-MQ`, `--scale-MQ`, `--low-MQ`/`--high-MQ` controls; simple mode gets `-q/--use-qual` | YES via mpileup genotype likelihoods (BAQ) | partially (`-q` filter only) | no | yes |
| Indels vs reference | `--show-ins yes/no` (default yes), `--show-del yes/no` (default no), `--mark-ins` (adds an underscore before inserted bases so consensus-to-reference coordinates stay derivable) | applies VCF indels to the reference, so output length changes; `--mark-del`, `--mark-ins` available | insertions gated by `-c` (default 0.8) | "reconciles substitutions and CIGAR-described indels"; `--realign` reassembles clip-dominant regions | `--fix indels`, `gaps`, `local` |
| Local realignment around indels | no | partially (mpileup BAQ) | no | yes (`--realign`) | yes (`--fix local`) |
| Fill uncovered positions with the reference base | YES but only in **samtools >= 1.22**: `-T ref.fa` "reports the reference value if a consensus value cannot be calculated" | that is its default behaviour (it edits the reference) | no (prints `-` or N) | no | n/a |
| Full-length-as-reference output | `-a` "Outputs all bases, from start to end of reference, even when the aligned data does not extend to the ends. This is most useful for construction of a full length reference sequence." | inherently | `-aa` mpileup needed | no | n/a |
| Can extend past reference ends | NO | NO | NO | PARTIALLY (`--realign` uses soft-clipped bases, adjacent to the aligned region only) | NO |
| Output formats | FASTA / FASTQ / pileup (`-f`) | FASTA | FASTA + per-base quality file | FASTA | FASTA + VCF + BED/WIG |

Sources: https://www.htslib.org/doc/samtools-consensus.html and the 1.21 tagged man page
https://raw.githubusercontent.com/samtools/samtools/1.21/doc/samtools-consensus.1 ;
https://samtools.github.io/bcftools/bcftools.html#consensus ;
https://andersen-lab.github.io/ivar/html/manualpage.html ;
https://github.com/bede/kindel (JOSS DOI 10.21105/joss.00282) ;
https://github.com/broadinstitute/pilon/wiki/Requirements-&-Usage .

**Bottom line.** `samtools consensus -A -d <mindepth> --mode simple -c <callfract>
-H <hetfract> --min-MQ <mq> --min-BQ <bq> -a` is a single command, already in the image,
and covers five of the six consensus behaviours a Geneious-shaped UI needs: majority
threshold, N below coverage, IUPAC ambiguity, quality weighting, and full-length output.
The sixth (fill-with-reference where uncovered) needs a bump to samtools >= 1.22.
Its real gap versus Geneious is indel handling: no local realignment, so indels are read
straight off CIGAR strings. `bcftools mpileup | bcftools call --ploidy 1 -m |
bcftools consensus -I -m lowcov.bed` gives better indel calls but needs three commands,
+0.97 MB, and an explicit low-depth mask BED because bcftools consensus has no depth
concept. `ivar consensus` is the cleanest threshold-and-N interface but needs the awkward
mpileup incantation and has a crude insertion model.

---

## 5. Extension past reference ends and circular wrap

Geneious does two things no single standard tool does:

1. **Extends the consensus past the ends of the reference** (its "Fine Tuning" with more
   iterations lets reads extend further past reference ends).
2. **Indexes circular references with origin-spanning words so expansion wraps.**

For (1), ranked by simplicity:
- **Iterate and re-recruit.** Each round, take the consensus, re-recruit reads (including
  mates whose partner mapped), assemble the soft-clipped tails, append. MITObim, IOGA,
  ARC, IMR and MIA all do a version of this.
- **INFER, and this is the key scoping point:** with a FULL-LENGTH circular reference
  there are no ends to extend past. Extension only matters when the user supplies a
  PARTIAL reference (a COI barcode, a mitogenome missing the control region). If MitoPilot
  scopes map-to-reference to full-length references, the whole extension problem
  disappears and with it MITObim's main advantage.
- If partial references must work: MITObim's documented COI-seed tutorial is the
  precedent, or a micro-assembly of the recruited read subset with SPAdes (already in the
  image) followed by re-alignment.
- kindel `--realign` closes gaps using clip-dominant regions but is pip-only, 21 stars.
  Not worth the dependency.

For (2): the elongate-and-fold trick, already implemented in this repo at
`R/circularize_asmb.R:466-555` (`count_junction_reads()` / `contig_depth()`), and
published as CircularMapper. Elongate the reference by its first N bases (the repo uses
`flank <- min(500L, len %/% 2L)`), map, fold coordinates past the true length back to the
origin, clip the consensus to true length.

---

## 6. Top 5 candidates, ranked

Criteria: mimic Geneious map-to-ref for mitogenomes from Illumina genome skims, run inside
the existing container, minimal new dependencies, robust, maintained.

**1. Compose our own loop from tools already in the image: bowtie2 (or minimap2) ->
samtools sort -> samtools consensus -> repeat N times.**
- New binaries: ZERO. Optional: bump samtools 1.21 -> 1.22+ for `-T` and threading
  (a version change to an installed package, not a new dependency); optional +0.97 MB
  bcftools if better indel calling is wanted later.
- Only option that lets the Shiny UI expose Geneious-shaped knobs: sensitivity preset
  (bowtie2 `--very-fast` .. `--very-sensitive`, `--local` vs `--end-to-end`, `-N`, `-L`,
  `--score-min`, `-X`), min depth for N, consensus call fraction, ambiguity on/off, min MQ
  and BQ, iteration count.
- Prior art is strong, current, and citable: the Westbury aITE script is the published
  bash implementation with a convergence rule; nf-core/viralmetagenome is the maintained
  Nextflow implementation with default 2 iterations; IMR/DENOM wrote the convergence
  criterion; viral-ngs defined the min_coverage/major_cutoff semantics; nf-core/modules
  has ready process bodies for every step.
- Circularity: reuse `R/circularize_asmb.R`'s elongate-and-fold.
- Reuses MitoPilot's existing bowtie2 invocation shape from `R/coverage.R:63-75`, so
  per-iteration cost is a known quantity in this pipeline.
- Risk: we own ~50 lines of shell/Nextflow. It is boring, testable, and every component is
  among the most maintained software in the field (samtools pushed 2026-08-27, bcftools
  2026-09-02, ivar 2026-09-02).

**2. MIA (`mapping-iterative-assembler` 1.0 on bioconda).** NEW at this rank.
- The only surveyed tool with circular-reference support (`-c`), iterate-to-convergence
  (`-i`), a distant-reference mode (`-D`), and a kmer seed-length knob (`-k`) in one
  binary. That is more Geneious semantics in one tool than anything else.
- 0.09 MB conda package, no runtime dependencies, biocontainer available, and bioconda is
  still rebuilding it (build _7, 2025-08-14) despite upstream being frozen since 2013.
- Against it: no paired-end model (fragments only), the consensus thresholds that matter
  live in `params.h` at compile time, aDNA-shaped design, and a sourced warning that it
  costs much more memory and CPU than MITObim on divergent references.
- INFER: worth a half-day spike as a comparator against option 1 on real MitoPilot test
  samples. If it works, it is a legitimate "one small binary, one command" answer. If the
  compile-time thresholds prove blocking, it still makes a good validation oracle.

**3. MITObim 1.9.1 + MIRA 4.0.2 (bioconda), wrapped as an external tool.**
- The only well-cited tool whose stated purpose is exactly "iterative mapping to a
  user-supplied mitochondrial reference, extending it", with published evidence that it
  tolerates divergent baits better than a BWA loop.
- Genuinely the best option IF partial-reference seeding (a ~700 bp COI barcode) is a
  requirement, because that is its documented Tutorial III use case.
- Costs: MIRA in its own conda env, Perl, uncompressed interleaved FASTQ preparation,
  NFS hostility on HPC, large `.maf` project directories, single-threaded, upstream last
  touched 2020-12-29, proofreading disabled since 1.8, and no exposable per-base consensus
  rules at all.

**4. Consensus-caller swap-ins for option 1: `bcftools` (+0.97 MB) or `ivar` (+1.97 MB).**
- bcftools: best indel calling (BAQ), `-I/--iupac-codes`, `--absent`/`--missing`/
  `--mark-del`; but low-coverage masking must be supplied as a BED (`samtools depth` +
  awk). Use as an option, not the default.
- ivar: cleanest threshold + N-below-depth + auto-IUPAC interface (`-t`, `-m`, `-n`, `-q`),
  GPL-3.0, extremely active. Downside: the `samtools mpileup -aa -A -d 0 -Q 0` pipe and a
  weak insertion model (fixed `-c 0.8`, quality faked to the threshold).
- nf-core/viralmetagenome's split (bcftools for intermediate rounds, ivar for the final
  consensus) is the informed default if we ever support both.

**5. mtGrasp 1.1.10** (as a possible future FOURTH de novo assembler, not as the
map-to-ref answer).
- Only genuinely new, maintained metazoan short-read mitogenome pipeline with bioconda +
  biocontainer + a 2025 MEE paper (DOI 10.1111/2041-210X.14506).
- But it is de novo with reference-guided joining, its noarch conda package is 163.6 MB,
  and its MITOS/Pilon/ABySS stack duplicates what MitoPilot already ships.

**Explicitly NOT recommended, with reasons.** GRAbB, aTRAM, Tanoti, MitoFlex, MEANGS,
MToolBox, mitoMaker, Norgal, ORG.Asm (no bioconda package, or Python 2, or both);
NOVOPlasty (non-commercial EULA that also forbids distributing derivative works, and it is
not map-to-ref); ARC / IOGA (Python 2, unmaintained, slowest and most memory-hungry in the
benchmark); IVA (unmaintained badge, and it is de novo despite the name); shiver /
MetaCompass (right ideas, wrong organism/granularity); snippy (19.9 MB plus freebayes and
a JVM, bacterial model, no iteration, no circularity - imitate its output contract
instead); viral-ngs refine_assembly (Novoalign licence, GATK3, archived repo);
mtDNA-Server / mutserve / MToolBox (human rCRS); Organelle_PBA / MitoHiFi / mitoVGP /
Circlator / Racon / Medaka (long reads); RagTag / AlignGraph (never touch reads);
SPAdes `--trusted-contigs` (manual forbids related-species contigs); CircularMapper (copy
the trick, it is already in this repo).

---

## 7. "Wrap MITObim" vs "compose standard tools", head to head

MIA added as a third column because it changes the answer on two rows.

| Dimension | Wrap MITObim (+MIRA) | Wrap MIA | Compose bowtie2/minimap2 + samtools (/bcftools/ivar) |
|---|---|---|---|
| **Seed-and-expand mapping fidelity** | MIRA is a genuine seed-and-extend mapper with an integrated read-vs-read editor; closest algorithmic cousin to the Geneious mapper. Best. | kmer seed filter (`-k`) plus banded alignment with a substitution matrix. Close in spirit, and `-k` is literally Geneious's word length. | bowtie2 is seed-and-extend (FM-index seeds, SIMD extension); minimap2 is minimizer chain-align. Same family, not identical. |
| **Sensitivity presets for a UI** | ONE knob: `--mismatch` (default 15% of read length), plus `--kbait` (31). Cannot express a Low/Medium/High grid. | `-k`, `-H`, `-S`, `-N`, `-D` - expressive, but in raw alignment-score units that no user will understand. | `--very-fast` .. `--very-sensitive`, `--local`/`--end-to-end`, `-N`, `-L`, `--score-min`, `-X`. A 3-to-5 level preset ladder maps on cleanly. **Win.** |
| **Majority/quality consensus with a threshold** | Not exposed; MIRA's rules are internal; proofreading disabled since 1.8. | `-p 0/1/2` picks the RULE, but the thresholds behind it are `params.h` compile-time constants. | Fully exposed: `--mode simple -c <fract>`, `-H <het>`, `-q`, or the Bayesian default with `--min-MQ`/`--min-BQ`. **Win.** |
| **N below coverage** | `--min_cov` filters whole contigs, not per base. | N is the fallback when no base wins the score test, but there is no depth knob. | `samtools consensus -d N`, `ivar -m/-n`, or a bcftools mask BED. **Win.** |
| **IUPAC ambiguity** | No. | No. | `samtools consensus -A`, `bcftools consensus -I`, ivar automatic. **Win.** |
| **Extension past reference ends** | Built in; it is MITObim's whole point (`--trimoverhang` disables). **Win.** | Consensus can grow as reads extend it, but this is not an advertised feature. | Not native; needs an added recruit-and-micro-assemble step, or accept "no extension". **Loss** (moot for full-length references). |
| **Circular reference** | Not handled. | `-c` handled natively. **Win.** | Not handled by the tools, but the elongate-and-fold code already exists at `R/circularize_asmb.R:466-555`. Tie-to-win. |
| **Paired-end reads** | `--pair` extends the readpool to full pairs; MIRA understands pairs. | No PE model at all (fragments only). **Loss.** | Native in bowtie2/minimap2, including insert-size limits (`-X`), which is the closest analogue to the Geneious paired-distance penalty. **Win.** |
| **GenBank (.gb) reference input** | FASTA only. | FASTA only. | Also FASTA only, but MitoPilot already parses GenBank elsewhere, and `--mark-ins` keeps consensus-to-reference coordinates derivable so reference annotations can be carried across. **Win.** |
| **Container cost** | +MIRA in its own conda env + Perl. The Dockerfile already uses the `-m -n <env>` isolation pattern five times, so the shape exists. | +0.09 MB. **Win.** | +0 MB minimal; +0.97 MB (bcftools) or +1.97 MB (ivar) optional. **Win.** |
| **Maintenance risk** | Upstream frozen 2020-12-29; proofreading disabled with an 8-year-old "will test soon" note; MIRA's author caps its recommended scope. High. | Upstream dead since 2013; but the bioconda recipe is rebuilt (build _7, 2025-08-14) and it is a dependency-free C binary. Medium. | samtools/bcftools/bowtie2/minimap2 are the most maintained tools in the field. **Win.** |
| **Speed on a genome skim** | MIRA is slow; MITObim runs many MIRA assemblies; and it is single-threaded (benchmark: "NOVOPlasty, ORG.Asm and MITObim do not support multithreading"). | Single binary, but sourced warning of "much more memory and CPU time than MITObim" on distant references. | bowtie2 on a mitogenome-sized reference is minutes per iteration at most; the existing `R/coverage.R` step is the same operation. 2-5 iterations is comfortably bounded. **Win.** |
| **Debuggability in Nextflow** | Large `.maf` project trees, NFS-hostile, needs uncompressed interleaved FASTQ. | Its own `.maln` format, needs a converter to get FASTA/SAM out. | Standard BAM/FASTA at every step; MitoPilot's existing coverage/QC helpers read them directly. **Win.** |
| **Partial reference (COI barcode seed)** | Yes, documented tutorial. **Win.** | Not designed for it. | No, without an added de novo step. **Loss.** |

**Verdict (INFER).** For the stated goal (single full-length reference per project or per
sample, user supplies GenBank or FASTA, mimic Geneious map-to-reference), compose standard
tools. It is the only option that lets the Shiny UI expose Geneious-shaped knobs, it adds
zero or one small binary, it reuses the mapper and samtools already installed and the
circular-fold code already written, and every dependency is actively maintained. MITObim
wins on exactly one axis that matters only if partial references are in scope. MIA wins on
circularity and convergence but loses on paired-end reads and on threshold exposure, and
is best used as a comparator or a validation oracle rather than the shipped path.

Suggested minimal shape of the composed loop (INFER, no code written):
1. Optional: elongate a circular reference by ~500 bp using the existing
   `R/circularize_asmb.R` pattern.
2. `bowtie2-build` + `bowtie2` with a sensitivity preset -> `samtools sort` -> BAM.
3. `samtools consensus -a -A -d <min_depth> --min-MQ <mq> --min-BQ <bq>` (or
   `--mode simple -c <call_fract> -H <het_fract>`) -> new FASTA.
4. Fold circular coordinates, clip to true length.
5. Compare to the previous iteration; stop on convergence (mapped-read count unchanged,
   per the aITE script, or "few or alternating changes", per IMR/DENOM) or at
   `max_iterations` (nf-core default: 2; aITE cap: 100).
6. Emit BAM + consensus FASTA + a per-base depth CSV (`samtools depth -a`), plus a
   snippy-style masked FASTA distinguishing zero coverage from sub-threshold coverage.

---

## 8. Numbers worth reusing as defaults

All sourced, with the source named so a maintainer can argue with the choice:

| Parameter | Value | Source |
|---|---|---|
| Iterations | 2 | nf-core/viralmetagenome default |
| Iteration cap with a convergence test | 100 | Westbury aITE script `for num in {1..100}` |
| Convergence test | mapped-read count unchanged between rounds | Westbury aITE script; alternative wording "until additional rounds of iteration produce few or alternating changes" (IMR/DENOM) |
| Min depth to call a base | 3 | viral-ngs `--min_coverage` argparse default; aITE `kindel --min-depth 3` |
| Min depth (stricter alternatives) | 5 / 10 | nf-core/viralmetagenome "at least a depth of 5"; ivar `-m` and snippy `--mincov` both default 10 |
| Major-allele cutoff | 0.5 | viral-ngs `--major_cutoff` |
| Call fraction (samtools simple mode) | 0.75 | `samtools consensus -c` default |
| Min base quality | 20 | nf-core/viralmetagenome; ivar `-q` default |
| Min mapping quality | 20-30 | Westbury tested 10/20/30 and found "lower PWD with increasing minimum mapping quality" |
| Mismatch tolerance (MITObim-style knob) | 3 or 5 | Westbury et al. 2022 recommendation |
| Bait / word size | 31 | MITObim `--kbait` default; GRAbB exact 31-mer matching |
| MIA kmer filter | 12 for 36 nt reads | MIA man page (INFER: scale up for 150 nt reads) |
| Circular flank for elongate-and-fold | min(500, len/2) | `R/circularize_asmb.R:529`, already this project's choice |

---

## 9. Known weaknesses that will bite regardless of tool choice

- **Reference bias.** Every category A/B method pulls the consensus toward the reference.
  Sourced mitigation: use "the phylogenetically closest bait reference sequence" and note
  that "Accuracy could be further improved by combining results from multiple bait
  references" (Westbury et al.). MitoPilot already has multi-candidate reference machinery
  (memory `multi-blast-ref-candidates`) that is directly reusable.
- **Under-recovery of length.** Sourced and specific: in the Westbury palaeognath dataset,
  "all bait references excluding Dromaius resulted in a total sequence length of ~14,886
  bp, as opposed to the expected linear length of 16,740 bp, regardless of mismatch value
  or damage patterns". INFER: expect an iterative map-to-ref product to be SHORTER than the
  reference on divergent baits, and design the QC display to make that obvious rather than
  silently emitting a truncated mitogenome.
- **Divergent references break plain mappers first.** The same study found "PWD to the
  reference conspecific mitogenome ... was consistently higher with BWA than MITObim" while
  "there was no obvious relationship between PWD and phylogenetic distance of the bait
  reference when using MITObim". INFER: a bowtie2-based loop sits closer to the BWA end of
  that spectrum, so a permissive `--local` preset and >= 2 iterations matter more for us
  than they would for a MIRA-based tool. This is the strongest technical argument for
  keeping MITObim or MIA available as a fallback for hard samples.
- **NUMTs.** Nuclear mitochondrial insertions in a genome skim map to the reference and
  inflate false heteroplasmy or ambiguity. Human tools build NUMT filtering in; a generic
  metazoan tool cannot. Mitigations (INFER): a non-zero `--min-MQ` to drop multi-mapping
  reads, and coverage-outlier flagging.
- **The control region / D-loop.** It spans the artificial origin break and is the least
  conserved region, so expect the lowest-confidence consensus there. The human field flags
  this explicitly (mtDNA-Server docs, quoted in 3.9).
- **Indels.** `samtools consensus` derives indels from CIGAR without local realignment;
  this is the single biggest fidelity gap versus Geneious "Fine Tuning". Mitigations:
  offer the bcftools route as an alternative consensus caller, or accept and document it.
- **Insertions break coordinate identity.** If insertions are applied, the output is no
  longer the same length as the reference and annotation transfer needs a coordinate map.
  `samtools consensus --mark-ins` exists precisely for this: "this permits an easy
  derivation of the consensus to reference coordinate mapping". Alternatively emit a
  substitutions-only product alongside, as snippy does with `.consensus.subs.fa`.

---

## 10. Source list (every URL used, re-checked 2026-09-02)

Repo (file:line facts)
- docker/Dockerfile:13, 27-36, 39-58, 84-87
- R/coverage.R:63-75
- R/circularize_asmb.R:466-471, 480, 522, 529, 546, 550-555

Tools
- MITObim: https://github.com/chrishah/MITObim/blob/master/README.md ;
  https://api.github.com/repos/chrishah/MITObim ;
  https://api.anaconda.org/package/bioconda/mitobim ; DOI 10.1093/nar/gkt371
- MIRA: https://raw.githubusercontent.com/DrMicrobit/mira/master/README.md ;
  https://api.github.com/repos/DrMicrobit/mira ;
  https://api.anaconda.org/package/bioconda/mira
- MIA: https://raw.githubusercontent.com/mpieva/mapping-iterative-assembler/master/README.md ;
  https://raw.githubusercontent.com/mpieva/mapping-iterative-assembler/master/man/mia.1 ;
  https://api.github.com/repos/mpieva/mapping-iterative-assembler ;
  https://api.anaconda.org/package/bioconda/mapping-iterative-assembler ;
  https://quay.io/api/v1/repository/biocontainers/mapping-iterative-assembler ;
  DOI 10.1016/j.cell.2008.06.021
- Westbury aITE: https://www.biorxiv.org/content/10.1101/2021.12.16.472923v1.full ;
  https://github.com/Mvwestbury/Iterative_mapping ;
  https://raw.githubusercontent.com/Mvwestbury/Iterative_mapping/main/BWA/aITE_mapper.sh ;
  DOI 10.1111/2041-210x.13990
- snippy: https://raw.githubusercontent.com/tseemann/snippy/master/README.md ;
  https://api.github.com/repos/tseemann/snippy ;
  https://api.anaconda.org/package/bioconda/snippy
- GRAbB: https://github.com/b-brankovics/grabb ; DOI 10.1371/journal.pcbi.1004753
- ARC: https://github.com/ibest/ARC ; https://www.biorxiv.org/content/10.1101/014662v2
- IOGA: https://raw.githubusercontent.com/holmrenser/IOGA/master/IOGA.py ; DOI 10.1111/bij.12642
- aTRAM: https://github.com/juliema/aTRAM ; DOI 10.1177/1176934318774546
- NOVOPlasty: https://academic.oup.com/nar/article/45/4/e18/2290925 ;
  https://raw.githubusercontent.com/ndierckx/NOVOPlasty/master/LICENSE ; DOI 10.1093/nar/gkw955
- GetOrganelle: https://raw.githubusercontent.com/Kinggerm/GetOrganelle/master/README.md ;
  DOI 10.1186/s13059-020-02154-5
- MitoFinder: https://api.github.com/repos/RemiAllio/MitoFinder ; DOI 10.1111/1755-0998.13160
- MitoZ: https://github.com/linzhi2013/MitoZ/wiki/The--assemble--subcommand ; DOI 10.1093/nar/gkz173
- MitoFlex: DOI 10.1093/bioinformatics/btab111 ; MEANGS: DOI 10.1093/bib/bbab538
- MToolBox: https://api.github.com/repos/mitoNGS/MToolBox ; DOI 10.1093/bioinformatics/btu483
- mtDNA-Server 2 / mutserve: https://mitoverse.readthedocs.io/mtdna-server/mtdna-server/ ;
  https://api.github.com/repos/seppinho/mutserve ; DOI 10.1093/nar/gkw247
- MITGARD: https://academic.oup.com/bib/article/22/5/bbaa429/6123950 ; DOI 10.1093/bib/bbaa429
- MetaCompass: https://raw.githubusercontent.com/marbl/MetaCompass/master/README.md ;
  arXiv:2403.01578 / PMC11188144
- shiver: https://github.com/ChrisHIV/shiver ; https://api.anaconda.org/package/bioconda/shiver ;
  DOI 10.1093/ve/vey007
- Tanoti: https://raw.githubusercontent.com/vbsreenu/Tanoti/master/README.md
- IVA: https://raw.githubusercontent.com/sanger-pathogens/iva/master/README.md ;
  DOI 10.1093/bioinformatics/btv120
- MitoGeneExtractor: https://github.com/cmayer/MitoGeneExtractor ;
  https://api.anaconda.org/package/bioconda/mitogeneextractor
- Norgal: DOI 10.1186/s12859-017-1927-y ; mitoMaker: https://github.com/gavieira/mitomaker
- Organelle_PBA: DOI 10.1186/s12864-016-3412-9 ; MitoHiFi: DOI 10.1186/s12859-023-05385-y ;
  mitoVGP: DOI 10.1186/s13059-021-02336-9
- mtGrasp: https://github.com/bcgsc/mtGrasp ; https://api.anaconda.org/package/bioconda/mtgrasp ;
  DOI 10.1111/2041-210X.14506
- TASR: https://github.com/warrenlr/TASR ; AlignGraph: https://github.com/baoe/AlignGraph ,
  DOI 10.1093/bioinformatics/btu291
- IMR/DENOM: http://mtweb.cs.ucl.ac.uk/mus/www/19genomes/IMR-DENOM/description.html ;
  https://chi.mpipz.mpg.de/imrdenom/
- ngs_mapper: https://github.com/VDBWRAIR/ngs_mapper
- viral-ngs: https://raw.githubusercontent.com/broadinstitute/viral-assemble/master/assembly.py ;
  https://api.github.com/repos/broadinstitute/viral-assemble (archived)
- nf-core/viralmetagenome:
  https://nf-co.re/viralmetagenome/1.1.3/docs/usage/workflow/5_variant_and_refinement ;
  https://www.biorxiv.org/content/10.1101/2025.06.27.661954
- nf-core/viralrecon: https://github.com/nf-core/viralrecon
- nf-core/modules: https://github.com/nf-core/modules/tree/master/modules/nf-core/{samtools/consensus,bcftools/consensus,ivar/consensus,bowtie2/align,minimap2/align,samtools/depth}
- CircularMapper: https://github.com/apeltzer/CircularMapper ;
  https://pmc.ncbi.nlm.nih.gov/articles/PMC4815194/ ; DOI 10.1186/s13059-016-0918-z
- Circlator: https://github.com/sanger-pathogens/circlator/wiki ; DOI 10.1186/s13059-015-0849-0
- RagTag: https://github.com/malonge/RagTag ; DOI 10.1186/s13059-022-02823-7
- SPAdes manual: https://ablab.github.io/spades/running.html
- Pilon: https://github.com/broadinstitute/pilon/wiki/Requirements-&-Usage ;
  DOI 10.1371/journal.pone.0112963
- samtools consensus: https://www.htslib.org/doc/samtools-consensus.html ;
  https://raw.githubusercontent.com/samtools/samtools/1.21/doc/samtools-consensus.1 ;
  https://github.com/samtools/samtools/releases/tag/1.15 ;
  https://github.com/samtools/samtools/releases/tag/1.17 ;
  https://github.com/samtools/samtools/releases/tag/1.22
- bcftools: https://samtools.github.io/bcftools/bcftools.html#consensus ; DOI 10.1093/gigascience/giab008
- ivar: https://andersen-lab.github.io/ivar/html/manualpage.html ; DOI 10.1186/s13059-018-1618-7
- kindel: https://github.com/bede/kindel ; DOI 10.21105/joss.00282
- Benchmark: https://bmcbioinformatics.biomedcentral.com/articles/10.1186/s12859-023-05445-3 ;
  https://pmc.ncbi.nlm.nih.gov/articles/PMC10498642/ ; DOI 10.1186/s12859-023-05445-3
- Package/version checks: https://api.anaconda.org/package/bioconda/<pkg> ;
  https://quay.io/api/v1/repository/biocontainers/<pkg>

---

## 11. Open questions for the planning phase

1. **Is the user-supplied reference always FULL LENGTH?** If partial references (a COI
   barcode) must work, MITObim moves to the top, because a pure mapping loop cannot grow a
   700 bp seed into a 16.5 kb mitogenome. If full-length only, extension past ends is moot
   and the composed loop wins outright.
2. **Is a samtools bump 1.21 -> 1.22+ acceptable?** It buys `-T ref.fa` (call the reference
   base where consensus fails, i.e. the Geneious "trust the reference at low coverage"
   behaviour), proper threading, and the leading/trailing-N fix. It is a version change to
   an installed package, so the cost is a rebuild, not a new dependency.
3. **Bayesian (Gap5) default or `--mode simple -c`?** Bayesian is quality-aware and closest
   to Geneious "Highest Quality"; `--mode simple -c` is the one that maps onto a
   user-visible "consensus threshold" percentage. Possibly expose "Highest Quality"
   (Bayesian) and "Majority with threshold" (simple) as two named options, which is exactly
   how Geneious presents it.
4. **Is +0.97 MB of bcftools acceptable to get BAQ-based indel calling, or do we accept
   CIGAR-only indels for v1?**
5. **Convergence or fixed count?** Fixed count is one integer in the DB (nf-core uses 2).
   Convergence needs a metric; the cheapest published one is "stop when the mapped-read
   count stops changing" (aITE), which is two `samtools view -c` calls per round.
6. **Does the output need to stay in reference coordinates?** If yes, either suppress
   insertions or use `--mark-ins` to keep the coordinate map, and consider emitting a
   substitutions-only FASTA alongside the full consensus (snippy's `.consensus.subs.fa`
   pattern).
7. **Where does per-sample vs per-project reference selection live** in the existing
   `blast_accession` / reference-override model (memory `reference-override-model`), and can
   map-to-ref just reuse it plus the multi-candidate machinery?
8. **How is the GenBank (.gb) input parsed, and do its annotations need to be carried
   through** to the output, or only its sequence? snippy is the precedent for GenBank-in,
   annotated-variants-out.
9. **Does the coverage CSV Geneious exports need an equivalent artifact?**
   `samtools depth -a` gives it for free, and the aITE script logs read count, mean depth,
   and zero-coverage base count per iteration, which would make a good per-iteration
   progress table in the Shiny app.
10. **Should MIA be spiked as a comparator?** It is a 0.09 MB dependency-free binary with
    native circular and convergence support; a half-day test against the existing MitoPilot
    test samples would settle whether it beats the composed loop on divergent references,
    where the composed loop is weakest.
