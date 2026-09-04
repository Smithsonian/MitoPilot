# Lane: geneious-advisor. Geneious Map-to-Reference assembly advisor: decision tree, validation table, and mitogenome implications

Date of retrieval: 2026-09-02. All quotes transliterated to ASCII (arrows written as "->", curly quotes straightened).
Raw fetched material (HTML, JSON, PNG, PDF, XML, text dumps) is under:
/tmp/claude-1000/-home-dmacguig-Documents-GitHub-MitoPilot/262c98c1-eb27-4a70-9ae5-a3680cd89047/scratchpad/research/raw/

Convention used throughout: "SOURCE SAYS" = verbatim or close paraphrase of a fetched document; "INFERENCE" = my reading of what the sources imply. Anything not marked INFERENCE is sourced.

---------------------------------------------------------------------------------------------------

## 0. Retrieval status (the article WAS recovered in full)

The article "Geneious Map-to-Reference assembly advisor decision tree & validation table" (article id 21749604628372) was retrieved in full, not reconstructed.

How:
- Direct HTML: `https://help.geneious.com/hc/en-us/articles/21749604628372-...` returns HTTP 403 (Cloudflare "Just a moment..." challenge) even with a browser User-Agent. Confirmed 2026-09-02.
- Zendesk JSON API works with a browser UA: `https://help.geneious.com/api/v2/help_center/en-us/articles/21749604628372.json` -> HTTP 200, 65,676 bytes, full `body` HTML. Saved as raw/advisor_api.json, body as raw/advisor_body.html, text dump raw/advisor_body.txt.
- Attachments API: `https://help.geneious.com/api/v2/help_center/en-us/articles/21749604628372/attachments.json` lists two PNGs:
  - 21749925836308 `Figure_Quizzard_MTR_decision_tree.png` (174,092 bytes, created 2023-12-11, older wide layout)
  - 21750387024276 `Quizzard_MTR_decision_tree-3.png` (172,267 bytes, updated 2024-08-28, current tall layout; this is the one embedded in the article body). Both downloaded (raw/tree_attachment = current, raw/tree_attachment_old.png = older) and read visually. The two images carry IDENTICAL node text and recommendations; only the layout differs.
- Wayback Machine has 7 snapshots of the article HTML (2024-02-25, 2024-06-12, 2024-10-15, 2025-02-07, 2025-03-28, 2025-05-19, 2025-09-07) per the CDX API; a direct fetch hit HTTP 429 (rate limit) so they were not used. The API copy is the live version anyway.

Article metadata (from the API JSON): title "Geneious Map-to-Reference assembly advisor decision tree & validation table"; created 2023-12-11T02:00:53Z; updated 2023-12-11T03:33:10Z; section 360009331791 ("Alignment and Assembly", category 360003482792); labels: none; author_id 7266124366100.

The same API trick recovered 19 sibling articles from the "Alignment and Assembly" section (28 articles total in the section), the manual pages, the Geneious Read Mapper white paper PDF, and two Geneious workflow XML files attached to help-center articles, which expose the exact option keys and preset values of the Geneious mapper dialog. Details in Sections 5 to 8.

Verification pass (second run, same day): the decision-tree image (raw/tree_attachment.png, 1018x1284 px) was re-read visually and matches the transcription in Section 2 node for node; the validation table in Section 3 was re-checked against raw/advisor_body.txt row by row (all 15 nodes, all URLs and summaries match); the Medium-preset numbers in Section 8.1 were re-parsed from the `Geneious.reference` block of all 11 public workflow files plus the 2 help-center attachments (17 blocks, Geneious 9.1.0 to 2025.1.3, all identical). Release notes for Geneious 2024.0 (05 December 2023) confirm the advisor's debut: "Map to Reference(s): Interactive quiz to provide guidance and assist in deciding which algorithm to try" (raw/release_notes.txt, Version 2024.0 block). New material added in this pass: a Europe PMC full-text sweep of ~120 open-access papers (Section 9.8), the Culicoides six-mapper mitogenome benchmark (Section 9.9), a Geneious staff answer on iteration counts (Section 5.15), and three teaching/lab protocols (Section 9.10). Wayback Machine and biostars.org remained blocked (HTTP 429 / 403) on retry.

---------------------------------------------------------------------------------------------------

## 1. The article, verbatim text portion

SOURCE SAYS (article body, only two prose paragraphs exist; everything else is the image and the table):

> "In Geneious 2024 or later, the Map-to-Reference assembly advisor can recommend the most appropriate mapping algorithm for your data. Click "Let us Help!" in the Map-to-Reference assembly window to start an interactive questionnaire that will guide you though the following decision tree."
>
> [image: Quizzard_MTR_decision_tree-3.png]
>
> "Support for each decision in the tree can be found in the validation table below."

Then the table. Nothing else. There is no text on sensitivity, iterations, consensus, coverage, divergence, or circular genomes anywhere in the article. The advisor only picks a MAPPER.

---------------------------------------------------------------------------------------------------

## 2. The decision tree, transcribed from the image (complete)

Title in image: "Map to reference decision tree". Rounded boxes = questions; blue boxes = recommended mappers, first (bold) name is the primary recommendation, the following names are the alternatives shown in the same box.

```
Q0  Did you perform DNA-seq or RNA-seq?
 |
 +-- DNA-seq --> Q1  What type of reads are you trying to map?
 |                |
 |                +-- Sanger      --> [Geneious]
 |                |
 |                +-- Short reads --> Q2  Does your reference sequence consist of complex
 |                |                       elements such as tandem repeats?
 |                |                    +-- Yes (Complex reference)     --> [BBMap (primary); minimap2]
 |                |                    +-- No  (Not complex reference) --> [Geneious (primary); Bowtie2]
 |                |
 |                +-- Long reads  --> Q3  Are you interested in identifying structural variants?
 |                                     +-- Yes (Identifying structural variants)     --> [minimap2 (primary); Geneious]
 |                                     +-- No  (Not identifying structural variants) --> [minimap2 (primary); Geneious; BBMap]
 |
 +-- RNA-seq --> Q4  Did you perform cDNA or Direct RNA sequencing?
                  |
                  +-- Direct RNA sequencing --> [minimap2]
                  |
                  +-- cDNA sequencing --> Q5  What type of reads are you trying to map?
                                          |
                                          +-- Short reads --> Q6  Are you interested in predicting and
                                          |                       annotating splice junctions?
                                          |                    +-- Yes (Predict and annotate splice junctions)      --> [STAR (primary); Geneious RNA]
                                          |                    +-- No  (Not predicting and annotating splice junctions) --> [Geneious RNA (primary); STAR]
                                          |
                                          +-- Long reads  --> Q7  Are you interested in mapping full-length transcripts?
                                                               +-- Yes (No read splitting)   --> [minimap2]
                                                               +-- No  (Reads can be split)  --> [minimap2 (primary); Geneious RNA; BBMap]
```

Facts about what the tree does NOT ask (important for the mitogenome case):
- No question about read length in bp, paired vs single, expected divergence from the reference, genome type / circularity, coverage, or purpose (variant calling vs consensus vs assembly). "Structural variants" is asked only in the long-read branch.
- No question about sensitivity, fine-tuning iterations, or consensus settings; the advisor sets only the "Mapper" dropdown.
- INFERENCE: For an Illumina genome skim mapped to a mitogenome the path is DNA-seq -> Short reads -> "complex elements such as tandem repeats?" For a typical vertebrate mitogenome (control-region tandem repeats are the only common repeat structure) the intended answer is "No", giving Geneious (primary) with Bowtie2 as the alternative. If the user answers "Yes" (e.g., a mitogenome with large control-region repeat arrays) the advisor says BBMap (primary) or minimap2.

---------------------------------------------------------------------------------------------------

## 3. The validation table, complete (every row)

Table layout in the article: for each question+answer there is a header row "<question> <answer>", then a row "Mapper | <mapper> | Summary", then one row per reference: "References | <URL> | <one-line summary>". Reproduced here as: node -> mapper -> list of (URL; summary). Summaries are verbatim including the source's typos ("eads", "Bord's", "access" for "assess").

Note: the table contains NO metrics (no accuracy, % mapped, or run time). The "validation" is purely a list of literature citations in which each mapper was used for that data type. There are no datasets or benchmarks run by Geneious in this article. (The only actual benchmark numbers published by Geneious are in the 2012-era Read Mapper white paper; see Section 7.)

### 3.1 "What type of reads are you trying to map? Sanger" -> Mapper: Geneious
- https://www.nature.com/articles/s41598-021-01589-5 ; "Geneious mapper was used to map raw foraminiferal COI Sanger sequences"
- https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6221210/ ; "Geneious mapper was used to map Plasmodium vivax 18S rRNA Sanger sequences"
- https://www.frontiersin.org/articles/10.3389/fmicb.2021.770787/full ; "Geneious RNA mapper was used to map Sanger sequences to mycovirus genome"
- https://www.sciencedirect.com/science/article/pii/S2666979X2200057X ; "Long range PCR of LINE (Long interspersed nuclear elements) sequences were aligned using Geneious mapper"

### 3.2 "Does your reference sequence consist of complex elements such as tandem repeats? Yes" -> Mapper: BBMap
- https://www.nature.com/articles/s41586-022-05256-1 ; "BBMap was used to align Illumina reads to Bord's genome where tandem direct repeat sequences are prevalent"
- https://www.frontiersin.org/articles/10.3389/fmicb.2017.01448/full ; "BBMap was used to alignment of PacBio and Illumina reads to Burkholderia pseudomallei genome, a bacterium which contains two large circular chromosomes, very high G+C content of 68-69%, highly repetitive regions and substantial genomic diversity"

### 3.3 "Does your reference sequence consist of complex elements such as tandem repeats? Yes" -> Mapper: minimap2
- https://genomebiology.biomedcentral.com/articles/10.1186/s13059-022-02831-7 ; "Benchmarking paper showed that minimap2 have the highest accuracy when aligning a repetitive dataset with high diversity generated with Illumina sequencing technology"

### 3.4 "Does your reference sequence consist of complex elements such as tandem repeats? No" -> Mapper: Geneious
- https://academic.oup.com/gbe/article/5/9/1661/557212 ; "Mapping of Illumina HiSeq reads to Polytomella magna mitochondrial DNA made up entirely palindromes via Geneious mapper resulting in complete linear-mapping mitochondrial genome with telomeres"
- https://www.nature.com/articles/s41598-022-05706-w ; "Mapping of Illumina reads using Geneious mapper helped identify insertion of viral and human origin in the Hepatitis E virus genome"
- https://www.nature.com/articles/s41598-022-10720-z ; "Mapping of Illumina MiSeq with Geneious mapper eads resulting in the first whole genome sequences of Vibrio harveyi isolated from European sea bass"

  (This is the ONLY mitochondrial citation in the whole map-to-reference validation table, and it sits under the short-read / non-complex-reference / Geneious node.)

### 3.5 "Does your reference sequence consist of complex elements such as tandem repeats? No" -> Mapper: Bowtie2
- https://www.cell.com/cell/pdf/S0092-8674(21)00585-7.pdf ; "Bowtie2 was utilised to map Illumina reads to human genome hg38 and microbial genome in a metagenomic study"
- https://www.frontiersin.org/articles/10.3389/fevo.2020.00105/full#S10 ; "This paper showed that Bowtie2 aligner substantially reduce computational times and increase sensitivity when mapping HiSeq ancient DNA reads"

### 3.6 "Are you interested in identifying structural variants? Yes/No" -> Mapper: minimap2
- https://www.nature.com/articles/s41467-023-35996-1 ; "PacBio CLR, HiFi, and Nanopore reads were aligned to human GRCh37 genome via minimap2 to help detect structural variants"
- https://www.nature.com/articles/s41467-018-07271-1 ; "minimap2 was used to map Nanopore reads to access structural integrity of large and repeat-rich plant genomes"
- https://www.biorxiv.org/content/10.1101/2022.04.04.487055v1.full.pdf ; "minimap2 was used to map PacBio and Oxford Nanopore reads to the human genome to help identify mosaic structural variants"
- https://www.microbiologyresearch.org/content/journal/mgen/10.1099/mgen.0.000682 ; "Nanopore reads were mapped to Escherichia coli genome with the minimap2 whereby routine rearrangements occur in the genome resulting in structural variants"
- https://academic.oup.com/bioinformatics/article/37/23/4572/6384570 ; "Original paper - Minimap v2.22 accurately maps long reads to highly repetitive regions and align through indels up to 100 kb"

### 3.7 "Are you interested in identifying structural variants? Yes/No" -> Mapper: Geneious
- https://academic.oup.com/ve/article/6/1/vez060/5716172 ; "PacBio reads were mapped to AcMNPV virus genome via Geneious mapper to detect structural variants"
- https://www.microbiologyresearch.org/content/journal/mgen/10.1099/mgen.0.000682 ; "PacBio and Illumina reads were mapped to Escherichia coli genome with the Geneious mapper whereby routine rearrangements occur in the genome resulting in structural variants"

### 3.8 "Are you interested in identifying structural variants? Yes/No" -> Mapper: Geneious RNA
- https://www.cell.com/iscience/pdf/S2589-0042(23)00857-X.pdf ; "Geneious RNA mapper was used to map HiSeq reads to the MERS-CoV genome to study the complex genome organization and expression network of MERS-CoV"

### 3.9 "Are you interested in identifying structural variants? No" -> Mapper: BBMap
- https://academic.oup.com/ve/article/6/1/vez060/5716172 ; "BBMap was used to map metagenome-assembled HiFi reads to provide insights into metabolic potential of uncultivated members of Armatimonadota"
- https://www.microbiologyresearch.org/content/journal/mgen/10.1099/mgen.0.000682 ; "BBMap was used to alignment of PacBio and Illumina reads to Burkholderia pseudomallei genome, a bacterium with highly repetitive regions and substantial genomic diversity"

### 3.10 "Are you interested in annotating splice junctions? Yes/No" -> Mapper: STAR
- https://www.nature.com/articles/nmeth.2722 ; "This study showed that STAR achieved high accuracy of splice-detection when mapping real human and mouse transcriptome data, and simulated RNA-seq data"
- https://www.nature.com/articles/nmeth.4106 ; "Benchmarking of RNA-seq aligners showed that STAR aligner is consistently accurate in identifying intron boundaries in human and Plasmodium falciparum genomes"
- https://onlinelibrary.wiley.com/doi/full/10.1111/tpj.13312 ; "Novel splice junctions were discovered with STAR when mapping RNA-seq reads the Arabidopsis thaliana genome"
- https://www.sciencedirect.com/science/article/pii/S0888754319307517 ; "STAR was used to map RNA-seq reads to hg19 and hg38, to identify splice junctions"

### 3.11 "Are you interested in annotating splice junctions? No" -> Mapper: Geneious RNA
- https://www.nature.com/articles/s41598-018-35654-3 ; "Illumina generated RNA-seq libraries were mapped to chloroplast genomes from a variety of organisms via Geneious RNA to show that RNA-seq data can be used to assemble chloroplast genomes"
- https://www.cell.com/iscience/pdf/S2589-0042(23)00857-X.pdf ; "Geneious RNA mapper was used to map HiSeq reads to the MERS-CoV genome to study the complex genome organization and expression network of MERS-CoV"
- https://journals.asm.org/doi/full/10.1128/mbio.01610-20 ; "Geneious RNA mapper was used to map MiSeq reads to the SARS-CoV-2 genome to detect a variety of genomic changes"
- https://journals.asm.org/doi/10.1128/mra.01224-22 ; "This study showed that high number of Novaseq 6000 reads mapped to Cowpea mild mottle virus isolate DSMZ PV-0090 using Geneious RNA mapper"

### 3.12 "Are you interested in mapping full-length transcripts? Yes/No" -> Mapper: minimap2
- https://www.nature.com/articles/s41592-019-0617-2 ; "minimap2 was used to map direct RNA and cDNA ONT reads to the human genome to identify plausible RNA isoforms"
- https://www.nature.com/articles/s41598-020-70794-5 ; "Minimap2 was used to map ONT and PacBio ROI cDNA reads to the virus genome to access the transcriptomic profile of the viral pathogen"
- https://www.nature.com/articles/s41586-023-05896-x ; "minimap2 in splice-aware mode was used to align PacBio to the Human GRCh38 genome to create a draft human pangenome"
- https://www.nature.com/articles/s41467-020-15171-6 ; "Nanopore reads were aligned to human hg38 genome in the spliced alignment mode with marked improvement in splice-site mapping"

### 3.13 "Are you interested in mapping full-length transcripts? No" -> Mapper: Geneious RNA
- https://www.cell.com/iscience/pdf/S2589-0042(23)00857-X.pdf ; "Geneious RNA mapper was used to map ONT reads to the MERS-CoV genome to study the complex genome organization and expression network of MERS-CoV"

### 3.14 "Are you interested in mapping full-length transcripts? No" -> Mapper: BBMap
- https://academic.oup.com/bioinformatics/article/34/5/748/4562330 ; "Evaluation of mappers for long read RNA-seq showed that BBMap, performed quite well with PacBio ROI reads and on simpler organisms with less multi-exonic genes."

### 3.15 "Did you perform cDNA or Direct RNA sequencing? Direct RNA sequecing" [sic] -> Mapper: minimap2
- https://www.nature.com/articles/s41592-022-01633-w ; "This study align RNA strands generated by ONT software using minimap2 via Nanopolish to analyze cellular mRNA and noncoding RNA, and numerous RNA viruses"
- https://www.nature.com/articles/s41592-019-0617-2 ; "Minimap2 was used to map direct RNA and cDNA ONT reads to the human genome to identify plausible RNA isoforms"
- https://www.nature.com/articles/s41598-020-70794-5 ; "Minimap2 was used to map direct RNA and cDNA ONT reads to the virus genome to access the transcriptomic profile of the viral pathogen"
- https://www.nature.com/articles/s41467-021-27393-3 ; "Nanopore direct RNA sequencing reads were mapped using minimap2 to detect RNA modifications"

End of table. (The table has no rows for the Sanger alternatives, and none for "complex reference" other than BBMap and minimap2.)

---------------------------------------------------------------------------------------------------

## 4. Sibling article: the de novo assembly advisor (article 20548441000468), the only organelle-relevant node

Retrieved the same way (raw/art_20548441000468.txt). Its structure mirrors the map-to-reference article (image tree + citation table). Relevant to mitogenomes:

SOURCE SAYS (validation table row): "Do you expect your coverage to be low or biased? Yes" -> Assembler: Geneious -> reference "The (non) accuracy of mitochondrial genomes for family-level phylogenetics in Erebidae (Lepidoptera)" with summary "Assembly of low coverage Erebidae mitochondrial genomes".

INFERENCE: Geneious positions its own assembler/mapper, not SPAdes/Tadpole, for low-coverage organelle data. This matches the map-to-reference table's only mitochondrial example (Polytomella magna mtDNA, Geneious mapper).

---------------------------------------------------------------------------------------------------

## 5. Related help-center articles (all fetched via the JSON API; text dumps in raw/art_<id>.txt)

### 5.1 "Which map to reference assembly algorithm is best for my data?" (360044628612, updated 2023-12-13)
SOURCE SAYS, Geneious mapper advantages (verbatim list): "Fast"; "High sensitivity"; "Iterative mode to extend past ends of reference sequence and map ends of reads correctly around indels"; "Can discover structural variants"; "Supports circular reference sequences (maps correctly around the origin)"; "Supports soft trimmed reads"; "Can map existing alignments and de novo assembled contigs to reference sequences"; "Provides progress during mapping".
Other mappers: Geneious for RNA Seq (introns, novel introns, fusion genes; "Novel intron and fusion gene discovery is a little slow"); BBMap ("Fast", "High sensitivity"); Minimap2 ("Handles noisy long read data (PacBio, Minion)", splice-aware, "Widely used", "Fast"); Bowtie2 ("Low memory usage", "Widely used"); STAR ("Annotates splice variants"; "Mammal genomes require at least 16GB of RAM, ideally 32G").
INFERENCE: the two Geneious-mapper properties that matter for a mitogenome consensus workflow and that Bowtie2/BBMap/minimap2 lack out of the box are (a) the iterative consensus re-mapping that extends beyond reference ends, and (b) native circular-reference handling.

### 5.2 "Minimap2 Map to Reference Advanced Options" (39567510932500, updated 2025-07-23)
SOURCE SAYS: presets exposed in Geneious as "Data Type": map-ont (default), lr:hq, map-hifi, map-pb, map-iclr, asm5, asm10, asm20, splice, splice:hq, splice:sr, sr. Divergence guidance for the asm presets: asm5 "Use this preset if the average divergence is not much higher than 0.1%"; asm10 "Use this if the average divergence is around 1%"; asm20 "Use this if the average divergence is around several percent"; and "Minimap2 can also be used as a fast whole genome alignment tool for genomes with ~15% max divergence". Short reads: sr = "-k21 -w11 --sr --frag=yes -A2 -B8 -O12,32 -E2,1 -r100 -p.5 -N20 -f1000,5000 -n2 -m25 -s40 -g100 -2K50m --heap-sort=yes --secondary=no".
INFERENCE: This is the only place in the Geneious help center that quantifies divergence tolerance, and it is about minimap2, not the Geneious mapper.

### 5.3 "How do I use the advanced assembly settings?" (360045073331)
SOURCE SAYS: advanced settings appear under "More options" after choosing "Custom Sensitivity"; "it is a good idea to select a standard sensitivity option which fills in appropriate default values for the advanced options, i.e. if you select "Low Sensitivity" then select "Custom Sensitivity" the advanced settings are pre-filled with the settings used for low sensitivity."

### 5.4 "How do I assemble paired reads?" (360045073151)
SOURCE SAYS: pairs must be set via Sequence > Set Paired Reads; "the paired distance you set does not need to be exact ... set it on the mid point of the range. Geneious will still assemble reads that are under or over the expected distance, but the pairing information will be used to help the assembler resolve complex placement issues."

### 5.5 "Can I use multiple reference sequences in my assembly?" (360044629172, updated 2026-01-21)
SOURCE SAYS: multiple references allowed; "Geneious will then try all reads against all references in a single operation"; per-reference batch via workflow "Map reads to each reference sequence"; many short references can be concatenated "with N spacers".

### 5.6 "Can Geneious Prime assemble PacBio or Minion data?" (360044627532)
SOURCE SAYS: Geneious mapper handles PacBio CCS "reasonably well"; CLR/ONT "cannot be reliably assembled using the Geneious assembler"; use Minimap2 plugin for noisy long reads.

### 5.7 "Assembly of SARS-CoV-2 genomes from tiled amplicon Illumina sequencing using Geneious Prime" (360045070991, updated 2026-05-04). The most concrete "recipe" article in the help center.
SOURCE SAYS (mapping): "set Sensitivity to Low, and Fine tuning iterations to 3. Then open the More Options tab, and change the sensitivity to Custom. Tick "only map reads which map nearby" and increase the "Allow gaps" setting to 50."
SOURCE SAYS (consensus): "The preferred method for consensus sequence generation is to call variants on the assembled reads and then apply those variant bases to the reference sequence, rather than generating a consensus directly from the assembly." Variant calling: "Set Minimum Coverage to 10, and turn off Minimum Strand Bias P-value." Low-coverage masking: "Find High/Low Coverage ... Turn on the Low Coverage finder and set number of sequences to 10" then "sites where the coverage is less than 10 will be masked with an N."
SOURCE SAYS (chimeras): if PCR chimeras are present, "use the Minimap2 plugin ... set the data type to Short Reads and turn off secondary alignments."
The attached workflow file (attachment 7069270589204, "Coronavirus_assembly_protocol_2022.2", saved raw/covid_workflow.xml) is the source of the exact option keys in Section 8.
INFERENCE: for an amplicon-to-same-species reference, Geneious staff deliberately choose LOW sensitivity, 3 iterations, and a variant-then-apply consensus. This is the opposite regime from a divergent-reference mitogenome (which needs higher sensitivity, more iterations, and a reads-only consensus), so do not copy these numbers.

### 5.8 "Best practice for preprocessing NGS reads in Geneious Prime" (360044626852, updated 2025-12-18)
SOURCE SAYS: "We recommend trimming Illumina data with a minimum quality (Q) of 13, preferably 30"; BBDuk for adapter and quality trimming; normalization (BBNorm) only for de novo, and "do not normalize data prior to mapping for variant discovery"; "Never normalize read data prior to a quantitative analysis".

### 5.9 "What's the difference between Pairwise/Multiple alignment, de novo Assembly, and Map to Reference?" (360045072591)
SOURCE SAYS: "De novo assembly is generally more computationally intensive than Map to Reference and can require large amounts of RAM."

### 5.10 "Can I call SNPs on individual sequences aligned to a reference?" (360044627872)
SOURCE SAYS: "Be sure to specify the Geneious Mapper in the Map to reference setup, most other mappers available in Geneious Prime will not handle large input "read" sequences." (i.e., the Geneious mapper can map a whole consensus/contig as a single read.) Attached "SNPs per sample" workflow (raw/snps_workflow.xml) is the second source of option keys (Geneious 11.0.3, Medium sensitivity, fine tuning None).

### 5.11 "Which de novo assembly algorithm is best for my data?" (360045072691)
SOURCE SAYS: Geneious de novo assembler "Can produce circular contigs"; Tadpole/SPAdes/Velvet/Flye "Produces only consensus sequences"; SPAdes "Doesn't work with low coverage".

### 5.12 "How do I circularize my linear sequence?" (360046973691, updated 2026-01-23)
SOURCE SAYS: Sequence -> Circular Sequence toggles topology; no mapping-specific content.

### 5.13 "What's the difference between soft trimming and hard trimming" (360044629252)
SOURCE SAYS: soft trims are annotations ignored by the assembler; "Only the Geneious assembler supports the use of trimmed annotations. Sequences should be hard trimmed if using other assembly algorithms, such as SPAdes, Tadpole, Bowtie etc." (this sentence is from the manual page, Section 6).

### 5.14 Help-center search coverage
Searched the Zendesk search API for: mitochondrial, iterate, fine tuning, sensitivity, consensus threshold, organelle, chloroplast, extend reference, reference bias, circular, map to reference, genome skim, iterative mapping, Highest Quality consensus. There is NO help-center article dedicated to mitochondrial/organelle mapping, iterative mapping to extend a reference, reference bias, or consensus thresholds. The de novo advisor (Erebidae mitogenomes) and the map-to-reference advisor (Polytomella mtDNA, RNA-seq chloroplast assembly) are the only organelle mentions. The only "consensus threshold" content is in the manual (Section 6.5) and the geneious.com tutorial (Section 6.6).

Old support-forum threads (support.geneious.com/entries/...) now redirect to help.geneious.com/entries/... which 403s; Wayback has no capture of the one thread found ("Geneious Assembler - loss of annotations with fine tuning", entry 22610388) nor of "Assembly of 454 and Sanger Reads" (entry 22580731). Not recoverable. Biostars thread 9592309 ("Geneious Prime (Consense Sequence)") 403s to WebFetch and curl, and the Wayback copy (20240520121801) returned HTTP 429 on two attempts; not recovered.

### 5.15 Community post "Geneious workflow to grow contigs` extremities" (post 360068924391, 2019-03-22; comment 360011033651, 2019-03-25; raw/post_grow.json, raw/postc_grow.json)
The only official-side statement found on how many fine-tuning iterations to use for extending past a reference and on early stopping. The original poster wanted a workflow that re-maps to the previous consensus "ad infinitum" and claimed that raising iterations "doesn`t work like that at all".
SOURCE SAYS (reply from a Geneious help-center account, author_id 390577310912): "Increasing the Geneious Prime mapper "Iterations" setting should "grow" your assembly beyond the bounds of the reference sequence, provided there are reads that overlap in a contiguous series beyond the reference bounds. How many iterations did you try? Try setting iterations to to a higher number, say 1000 and see if that works. Geneious will only perform as many iterations as required. If the assembly is to slow with a high iterative value, consider using menu Sequence -> Error correct and Normalize reads to Normalize (don't error correct) your reads. This may substantially reduce the size of your data set and allow assembly to proceed faster with a high iterative setting. Once you have generated an extended consensus you can map against that with the full read set to generate a final consensus."
INFERENCE: this settles two of the earlier open questions from the Geneious side: (a) the iteration count is a free integer, not limited to the 3/5/10/25 presets, and (b) iteration stops early once nothing more maps ("only perform as many iterations as required"), which is exactly what Kemp observed in 11.1.5 (Section 9.2). The suggested two-stage recipe (normalized reads with a high cap to extend, then one pass with all reads against the extended consensus for the final call) is a cheap pattern to copy in a re-implementation.

---------------------------------------------------------------------------------------------------

## 6. The user manual (manual.geneious.com/en/latest/AssemblyMapping.html and Alignments.html), the parts that carry settings semantics

Dumps: raw/manual_AssemblyMapping.txt (54,989 chars), raw/manual_Alignments.txt.

### 6.1 Map to reference, mapper choice
SOURCE SAYS: "In the Methods panel, you can choose between the standard Geneious assembler, Geneious for RNAseq, STAR (Geneious Prime 2023+) or the BBMap, Bowtie, Minimap2 (Geneious Prime 2020+) mappers if you have these plugins installed." and "If multiple reference sequences are selected, each read will be mapped to the sequence with the best match only, and will produce one contig per reference."

### 6.2 Fine tuning (verbatim, this is the entire manual section)
SOURCE SAYS: "When aligning to reference the sequences are not aligned to each other, each of them is instead aligned to the reference sequence independently and the pairwise alignments are combined into a contig. However, an iterative fine tuning step can be enabled, which makes reads that overlap from the initial assembly stage align better to each other. Fine tuning causes reads to align better to each other around indels which improves the accuracy of consensus and variant calling. For more information, click the help (question mark) button next to the fine tuning options in the Map to Reference setup dialog."
"If you just wish to use a reference sequence to help construction of the contig where the reads extend beyond the length of the reference then you have two options. With iterative fine tuning, reads can extend a bit further past the ends of the reference sequence on each iteration so make sure you set the number of iterations high enough. Or you could select all sequences including the reference and use the De Novo assembler."
(The in-app help text behind the "?" button is not published anywhere I could find.)

### 6.3 Structural variant discovery
SOURCE SAYS: "Geneious makes two passes during mapping. On the first pass each read mapped will generate candidate junctions ... The second pass involves mapping reads using the discovered junctions." "By default, at least 2 reads must support the discovery of a junction"; "Annotations are only created for variants which are at least 3 bp in size"; Deletion vs Rearrangement boundary is 1000 bp; "For deletions under 1,000 bp, the deletion is represented as a gap in the read. This gap contributes towards calling a gap in the consensus sequence."; discovered insertions "must be less than the read length".

### 6.4 The map to reference algorithm (verbatim bullet list already summarized in context.md; two extra facts)
SOURCE SAYS: "Both the Geneious de novo and reference assemblers use a deterministic method (even when spreading the work cross multiple CPUs) such that if you rerun the assembler using the same settings and same input data it will always produce the same results." and "The final optional fine tuning step at the end, shuffles the gaps around so that they reads better align to each other rather than the reference sequence."

### 6.5 Consensus semantics (Alignments.html, "Threshold settings" and "Other settings")
SOURCE SAYS: "The Threshold determines which base in called in the consensus, and can be set to a percentage, or by using the quality scores on the reads." Worked example: column with 6 A, 3 G, 1 T: "If the consensus threshold is set to 60% or below, then the consensus will be A. If the consensus threshold is set to between 60% and 90%, then the consensus will be R. If the consensus threshold is set to over 90%, then the consensus will be D." Ties: "either all or none of the involved residues will be selected".
"Highest Quality ... sums the total quality for each potential base call, and if the total for a base exceeds 60% of the total quality for all bases, then that base is called." Variants "Highest Quality (50% or 60% or 75%)". Homopolymer quality symmetrisation and low-quality-gap halving are applied in Highest Quality mode. "When reads have mapping qualities ... the mapping quality is combined with the base pair quality to form the quality used during consensus calling."
"If no coverage call: For alignments or contigs with a reference sequence, this setting can be used to control what character the consensus sequence should use when the reference sequence has no coverage. Options available are - , X/N, ? or Ref. ... If Ref is selected, then the consensus is assigned whatever character the reference sequence has at that position."
"Call N if Quality below: Enables you to change consensus bases to N's if the quality is below the threshold that you set."
"Ignore Gaps (alignment documents only)".
Contig viewer: "The consensus sequence ... is the consensus of the reads only and does not include the reference sequence if one is present. ... If the sequences in the contig have quality information attached we recommend selecting the Highest Quality consensus type."
Coverage graph: "The data underlying these graphs can be exported in CSV format by clicking the Export option under the Graphs tab."
INFERENCE: A "0% majority" threshold is simply the plain plurality call (the percentage is the minimum support fraction; 0% means the most frequent base always wins, ties become ambiguity codes). Sources never use the literal phrase "0% majority"; I could not fetch the dropdown label list.

### 6.6 geneious.com official tutorial "How to Map NGS Reads to a Reference and Call Variants" (https://www.geneious.com/tutorials/map-to-reference, fetched 2026-09-02, raw/tutorial_mtr.txt)
SOURCE SAYS (Step 3 settings, verbatim list): "Mapper: Geneious (recommended for most NGS data)"; "Sensitivity: Medium Sensitivity/Fast"; "Fine Tuning: Iterate up to 5 times"; Trim panel "Select Do not trim (already trimmed with BBDuk)"; Results "Save assembly report", "Save contigs".
SOURCE SAYS (consensus): "Go to Display tab -> Threshold; Select Highest Quality 60% (recommended for NGS data with quality scores). Why this matters: The "Highest Quality" setting uses quality scores to call the most accurate consensus. Other thresholds like "100% - Identical" can introduce false ambiguities from sequencing errors."
SOURCE SAYS (coverage): Find Low/High Coverage with "Standard deviations from mean: 2"; "minimum 10x coverage, but 20-30x is recommended"; "Higher coverage (50-100x) is needed" for rare variants.
SOURCE SAYS (troubleshooting): "Low Mapping Rate ... Try: Increase sensitivity to Medium-High"; "Slow Processing ... Is sensitivity set too high? (Use Medium for NGS) ... Reduce iteration count in Fine Tuning".
SOURCE SAYS (BBDuk pre-trim): "Minimum Quality: 20", "Minimum length: 20", "Use Q20 for Illumina data".
The tutorial's screenshot of the dialog (raw/MTR_settings.png, read visually) shows: Sensitivity dropdown "Medium Sensitivity / Fast"; checkboxes "Find structural variants, short insertions, and deletions of any size" (off) and "Find short insertions and large deletions up to [1,000] bp" (off); "Fine Tuning: Iterate up to 5 times"; Trim Before Mapping radio buttons "Use existing trim regions / Remove existing trim regions from sequences / Trim sequences [Options] / Do not trim"; Results checkboxes "Save assembly report / Save list of unused reads / Save list of used reads [Include mates] / Save in sub-folder / Save contigs / Save consensus sequences [Options]".
The manual's own dialog screenshot (raw/manual_Map_to_Ref.png) shows the same dialog with "Highest Sensitivity / Slow" selected, "Iterate up to 5 times", and the trim radio labels "Use existing trim regions / Remove existing trim regions from sequences / Re-trim sequences / Do not trim (discard trim annotations)".
The SARS-CoV-2 article screenshot (raw/sarscov2_mapping.png) shows "Custom Sensitivity", "Fine Tuning: Iterate 3 times" and the full Advanced panel (transcribed in Section 8).
The 2012 white paper Figure 4 (Geneious 6 dialog) shows "Sensitivity: Medium-Low Sensitivity / Fast" and "Fine Tuning: Iterate up to 5 times" as the defaults of that era (raw/wp-05.png).
Dropdown labels, consolidated from every source that quotes them verbatim (help center, manual/tutorial screenshots, and the published-methods sweep in Section 9.8):
- Sensitivity (mapper): "Low Sensitivity / Fastest" (PMC7782976, PMC13022508), "Medium-Low Sensitivity / Fast" (white paper Geneious 6 default; PMC10381751, PMC11034133, PMC11091489, PMC12243559, PMC13299176), "Medium Sensitivity / Fast" (current default; tutorial; PMC10287144, PMC12418279), "Highest Sensitivity / Medium" (PMC7369027, PMC11821457, PMC12551961), "Custom Sensitivity". "Medium-High" is named only in the tutorial troubleshooting text; its speed suffix was never quoted. "Highest Sensitivity / Slow" appears in the manual's dialog screenshot (raw/manual_Map_to_Ref.png); INFERENCE: either an older label or the de novo assembler's label, since the mapper papers consistently say "/ Medium".
- Fine Tuning: "None" (XML `fineTuningNone`; PMC7782976 "Fine Tuning(None)"), "Iterate 3 times" (`iterate_3`; PMC12243559 "three iterations"), "Iterate up to 5 times" (`iterate_5`, default), "Iterate up to 10 times" (PMC6979410, PMC7694756, PMC9359134, verbatim "iterate up to 10 times"), "Iterate up to 25 times" (PMC7369027, PMC7694756, PMC11821457, PMC12551961, verbatim "iterate up to 25 times"). Kemp (Section 9.2) ran 1000, 10000 and 100000 and a Geneious staff reply suggests "say 1000" (Section 5.15), so the count is also free-text editable. INFERENCE: the dropdown is None / 3 / 5 / 10 / 25 plus a custom number; no public source lists the menu itself, but every value above is a verbatim quote of the label a user saw.

---------------------------------------------------------------------------------------------------

## 7. The Geneious Read Mapper white paper (the only real Geneious-run benchmark), https://desktop-links.geneious.com/assets/documentation/geneious/GeneiousReadMapper.pdf (10 pages, Geneious 6.0.3 era; linked from the current manual as "Geneious Mapper white paper"; raw/GeneiousReadMapper.txt)

Authors: Matthew Kearse (developer), Shane Sturrock, Peter Meintjes.

### 7.1 Algorithm facts not in the manual
SOURCE SAYS: index word length "depends on the sensitivity chosen, but is typically in the range 10 to 15 bases"; "Reads that map equally well to multiple locations can either be mapped to a random best location, not mapped at all, or mapped to all locations at the discretion of the user"; paired-read example: a pair mapping perfectly 5000 bp apart loses to a placement with one mismatch at ~500 bp when the expected insert is 500.
Iteration (verbatim): "The results are significantly improved by the use of an iterative system (new in Geneious 6), where the Geneious Read Mapper maps reads to the consensus sequence from the previous iteration. The reads are converted back to mappings relative to the original reference sequence and the process is repeated. This allows more reads to be mapped to variable regions, makes reads better align to each other in INDEL regions (important for downstream analyses such as variant calling), and reduces the likelihood of reads mapping to an incorrect location in near perfect repeat regions."
Heuristics: "allowing a single mismatch in the seed, correct handling of circular genomes, consistently choosing the same one of many equally optimal results and weighting reads differently during consensus calling based on the number of mismatches to the reference."
Memory: "Geneious requires ~14 GB (10 GB for single iteration mapping) compared to about 2.5 GB for Bowtie1" (E. coli whole-genome, 5.4 M reads).

### 7.2 Validation dataset and metrics (Table 1, Illumina HiSeq 2000, 90 bp paired, E. coli K-12 reads mapped to the yghJ gene of E. coli IAI1; the two genes are "89% identical ... four short INDELs"; 5,060 paired reads)
| Algorithm | # Mapped | % Mapped | % Mapped and correctly aligned to consensus | Consensus accuracy |
|---|---|---|---|---|
| Bowtie 1 (default) | 470 | 9.3% | 9.2% | 28.8% |
| Bowtie 2 (default) | 2,226 | 44.0% | 43.2% | 84.0% |
| Bowtie 2 (very-sensitive-local) | 4,320 | 85.4% | 74.7% | 96.5% |
| SOAP2 (default) | 1,316 | 26.0% | 26.0% | 48.4% |
| BWA (default) | 2,878 | 56.9% | 53.1% | 89.0% |
| SMALT (default) | 4,633 | 91.6% | 89.6% | 96.5% |
| Geneious (single iteration, default sensitivity) | 4,543 | 89.8% | 85.6% | 97.1% |
| Geneious (single iteration, highest sensitivity) | 5,060 | 100.0% | 96.1% | 99.7% |
| Geneious (default settings) | 5,060 | 100.0% | 100.0% | 100.0% |

Table 2 (Ion Torrent, SRR515927, 2,535 reads to yghJ): Bowtie1 0.2% mapped / 3.6% consensus accuracy; SOAP2 0.1% / 3.6%; Bowtie2 default 43.0% / 84.3%; Bowtie2 very sensitive 80.0% / 99.2%; BWA 1.7% / 21.3%; SMALT 85.1% / 99.3%; Geneious single iteration default 90.9% / 99.6%; Geneious single iteration highest 99.8% / 99.7%; Geneious default (iterative) 99.96% / 100.0%.

Table 3 (time, 5.4 M Illumina reads to E. coli IAI1, i7-2600, 16 GB): Bowtie1 3:39; Bowtie2 6:42; BWA 5:50; SOAP2 7:11; SMALT 3:19; Geneious single iteration 1:31; "Geneious (default settings - 5 iterations) 5:34".

Conclusion (verbatim): "the Geneious Read Mapper in version 6.0 produces reliable and accurate alignments through regions of relatively low identity (89%) where two major types of polymorphisms, SNPs and INDELs, are present".

INFERENCE for the mitogenome case: 89% identity between sample and reference is squarely inside the "1-15% divergence" band of interest, and the white paper's own result is that (a) single-pass mapping at default sensitivity leaves ~10% of reads unmapped and the consensus imperfect, (b) iterating 5 times to the evolving consensus recovers everything. This is the mechanistic justification for an iterate-to-consensus loop in a re-implementation; the specific mapper matters less than the loop.

---------------------------------------------------------------------------------------------------

## 8. Exact Geneious mapper option keys, preset values, and consensus option keys (recovered from Geneious workflow XML files)

Sources: raw/covid_workflow.xml (Geneious 2022.2.0, attachment 7069270589204 of help article 360045070991), raw/snps_workflow.xml (Geneious 11.0.3, attachment 360059852031 of article 360044627872), plus eight public .geneiousWorkflow files on GitHub (raw/ghwf/wf_1..8.xml; repos clinical-genomics-uppsala/Geneious_typing, CDCgov/MaRS, clinical-genomics-uppsala/Geneious_SARS-CoV-2, cipres-repo/cipres-mrbayes-plugin geneious-9.1.2-devkit sampleWorkflows, madalphadiallo/alpha-mars; Geneious versions 9.1.0 to 2024.0.7). The workflow element is `com.biomatters.plugins.alignment.AssemblyOperation_Reference`, mapper block `<childOption name="Geneious.reference">`.

### 8.1 Geneious mapper option keys and the two presets recovered
All ten public files that use the standard preset carry IDENTICAL "medium" values from Geneious 9.1.0 through 2024.0.7, so the Medium preset has been stable for a decade. The SARS-CoV-2 file is "custom" derived from Low (per the article text) with two edits ("only map reads which map nearby" ticked; "Maximum Gap Size" raised to 50). Dialog labels are from the SARS-CoV-2 screenshot (raw/sarscov2_mapping.png).

| XML key | Dialog label (Advanced panel) | Medium preset | SARS-CoV-2 custom (Low-derived) |
|---|---|---|---|
| sensitivity | Sensitivity | medium | custom |
| fineTune | Fine Tuning | iterate_5 (default in 8 of 10 files; fineTuningNone in the SNP workflows; iterate_3 in one 9.1 sample) | iterate_3 |
| findStructuralVariants | Find structural variants, short insertions, and deletions of any size | false | false |
| findDeletions / maximumDeletionSize | Find short insertions and large deletions up to [bp] | false / 1000 | false / 1000 |
| indexWordLength | Index Word Length | 12 | 14 |
| expansionWordLength | Word Length | 14 | 24 |
| filterRepeatsReference / filterRepeatsSizeReference | Ignore words repeated more than [n] times | true / 20 | true / 8 |
| allowGaps | Allow Gaps | true | true |
| maxGapsPerRead | Maximum Per Read (%) | 15 | 10 |
| maxGapSize | Maximum Gap Size | 50 | 50 (raised from the Low default by the article's author) |
| maxMismatches | Maximum Mismatches Per Read (%) | 30 | 10 |
| maxAmbiguity | Maximum Ambiguity | 4 | 4 |
| applyMinOverlap / minOverlap | Minimum Overlap | false / 25 | false / 25 |
| applyMinOverlapPercentageIdentical / minOverlapPercentageIdentical | Minimum Overlap Identity (%) | false / 80 | false / 80 |
| doMoreThoroughSearching | Search more thoroughly for poor matching reads | false | false |
| accuratelyMapReadsWithErrorsToRepeatRegions | Accurately map reads with errors to repeat regions | true | false |
| multipleBestMatches | Map multiple best matches | mapRandomly ("Randomly") | mapRandomly |
| applyMinimumMappingQuality / minimumMappingQuality | Minimum mapping quality | false / 30 | false / 30 |
| trimPairedOverhangs | Trim paired read overhangs | true | true |
| onlyMapPairedHitsReference / onlyMapPairedHitsReferenceCombobox | Only map paired reads which [map nearby / ...] | false / mapNearby | true / mapNearby |
| includeInsertionsInStructuralVariants | Include insertions in structural variants | true | true |
| minimumJunctionSupport | Minimum support for structural variant discovery | 2 | 2 |
| reanalyzeSequencesThreshold | (no visible label; INFERENCE: internal re-analysis threshold) | 8 | 16 |

INFERENCE on what "sensitivity" actually changes (comparing the two columns): higher sensitivity = shorter index/expansion words (12/14 vs 14/24), more mismatches allowed per read (30% vs 10%), more gaps per read (15% vs 10%), a larger repeat-word filter (20 vs 8), and the repeat-accuracy heuristic on. Medium-Low, Medium-High and Highest presets could NOT be recovered from any public workflow file (all 11 public files, including three more found in this pass at FHU-Bioinformatics/workflows, Geneious 2025.1.3, carry the identical Medium block). The white paper says index length spans "10 to 15" across presets, so Highest is presumably index 10 or 11 with an even higher mismatch allowance (INFERENCE).

Second-hand Medium-Low numbers: one paper reports the dialog values it saw. SOURCE SAYS (PMC10507443, Habromys deer mice, Geneious Prime 2021.2.2): "with default parameters (Medium-Low sensitivity, Maximum mismatches = 20%, Maximum gaps = 10%)". Combined with the Medium (30% / 15%) and Low-derived (10% / 10%) columns above this gives a consistent ladder for the two advanced values that matter most for a divergent reference: Low 10% mismatches / 10% gaps; Medium-Low 20% / 10%; Medium 30% / 15%. Word lengths for Medium-Low remain unknown. Two other papers quote custom settings that look like presets but cannot be assigned: PMC11476305 (Eimeria, 2023.1.2: "maximum gaps per read of 10%, maximum gap size of 5, word length of 25, index word length of 15, maximum mismatches per read of 25%, and maximum ambiguity of 4") and the same paper's second run ("minimum overlap of 16, word length of 10, maximum mismatches per read of 2%").

Observed enum values: `fineTune` in {fineTuningNone, iterate_3, iterate_5}; `multipleBestMatches` = mapRandomly (dialog shows a "Randomly" dropdown; white paper says the alternatives are "not mapped at all, or mapped to all locations"); `onlyMapPairedHitsReferenceCombobox` = mapNearby.

### 8.2 Trim panel keys
`trimOptions.method` = useExistingTrims in both help-center workflows (dialog labels: "Use existing trim regions" / "Remove existing trim regions from sequences" / "Trim sequences" or "Re-trim sequences" / "Do not trim"). When "Trim sequences" is used the sub-options are the Trim Ends options (errorProbability true, errorLimit 0.05, vector screening off, primer screening off, ambiguity trim off, minLength 20 unused).

### 8.3 Consensus option keys (block `consensusOptionsReference`, values from the SARS-CoV-2 workflow, Geneious 2022.2)
| XML key | Value | INFERENCE label / meaning |
|---|---|---|
| thresholdPercent | weighted_60 | "Highest Quality (60%)" threshold |
| thresholdPercentNoQuality | 65 | percentage threshold used when reads carry no quality |
| noConsensusGaps | false | do not suppress gap calls in consensus |
| noConsensusEndGaps | true | do not call gaps at the ends |
| mapQuality / mapQualityMethod | true / mapSummed | include mapping quality, summed ("Assign Quality: Total") |
| noCoverageCharacterReference | unknown | "If no coverage call: ?" (options per manual: -, N, ?, Ref) |
| applyLowCoverageOrQualityCall | true (reference) / false (de novo) | enable the low coverage/quality call |
| coverageOrQuality | coverage | threshold type |
| coverageThreshold | 3 (reference) / 2 (de novo) | call the low-coverage character below this depth |
| lowCoverageOrQualityCharacter | unknown | character used below threshold ("?") |
| qualityThreshold | 20 | "Call N if Quality below" value when coverageOrQuality = quality |
| trimToReference | false | "Trim to reference sequence": consensus is NOT clipped to the reference extent |
| ignoreReadsMappedToMultipleLocations | false | |
| splitAroundQuestionMarks | false | "Split into separate sequences around '?' calls" |
| callChromatogramHeterozygotes / chromatogramHeterozygotePercentage | true / 50 | Sanger only |

SOURCE (XML) vs INFERENCE: the keys and values are verbatim from the file; the labels in the third column are my mapping to the manual's option descriptions (Section 6.5), except "trimToReference" whose existence as a consensus-level key is itself a sourced fact. This is the only place any "trim to reference" setting was found; it lives in consensus generation, not in mapping, and defaults to false.

---------------------------------------------------------------------------------------------------

## 9. Other content on assembling a mitogenome by iterative mapping in Geneious (outside the help center)

### 9.1 Winn, Bester-van der Merwe, Maduna (2025) "Annotated Bioinformatic Pipelines for Genome Assembly and Annotation of Mitochondrial Genomes", Bio-protocol 15(5): e5231 (PMC11896769; PDF at https://en.bio-protocol.org/pdf/Bio-protocol5231.pdf, raw/bioprotocol5231.txt)
SOURCE SAYS: "Assemble the reads to the reference mitogenome using the Geneious read mapper with medium sensitivity settings and five iterations (Figure 1). These are the default settings for the Geneious read mapper and, for most situations, using the default sensitivity is recommended. The highest sensitivity is intended for use with smaller numbers of Sanger reads (1,000 or less), and medium or medium-low sensitivity is usually the best option for large numbers (e.g., 100,000 or more) of next-generation sequencing reads [19]. Iterative fine-tuning maps reads to the consensus sequence from the previous iteration and converts the reads back to mappings relative to the reference sequence, repeating the process until the maximum number of iterations is reached. Iterative assembly greatly improves results around regions that differ from the reference sequence. Later iterations generally map a higher fraction of reads as the mapping extends into regions where reads were previously un-mappable. Geneious recommends using five iterations. Decreasing this will increase the assembly speed."
(Reference [19] is Geneious; the sentence about Sanger 1,000 / NGS 100,000 is, INFERENCE, the text of the in-app Sensitivity help button that I could not fetch directly.)
Also: "Use a high-quality assembly as the reference mitogenome and make sure it is as closely related to your study species as possible" (Galeorhinus galeus mapped to Mustelus mustelus NC_039629, a different genus of the same family; Ion Torrent reads; "all other parameters were left at their default settings"). Their Table 1: reference assembly mapped 1,152 reads into a 16,758 bp contig for G. galeus. Their key caveat: reference-based assembly "may also collapse duplicated regions and fail to detect structural deviations from the reference"; a duplicated Cytb/D-loop segment found by de novo SPAdes was "missing" from the reference and hybrid assemblies and was confirmed by Sanger.

### 9.2 Kemp, L. (thesis chapter, Victoria University of Wellington), "Assembly and description of the P. georgianus mitogenome", https://bookdown.org/leahmhkemp/welly-trevally-html/mitogenome.html (raw/trevally.txt). The most systematic public test of Geneious iteration counts on fish genome-skim data.
SOURCE SAYS: Illumina 125 bp paired reads ("insert size: 125" set at import), whole-genome data, Geneious 11.1.5; "mapped to a reference mitogenome using Geneious mapper, with medium-low sensitivity and no trimming before mapping"; references chosen by discontiguous megaBLAST of COI: Carangoides equula KX373635 (92.1% COI identity), C. equula KM201334 (92.4%), Trachurus japonicus AP003092 (89.7%). Iterations tested: "2, 3, 5, 10, 25, 1000, 10000 and 100000 iterations".
Results: "Increasing the number of mapping iterations ... reduced the frequency of regions with no coverage, low contig depth and nucleotide positions with a low percentage identity to the reference genome ... However, a saturation point appears to be reached"; saturation "around ten mapping iterations" for the worst reference, "after only three mapping iterations" (depth) and "after five mapping iterations" (identity) for the best reference, "after 5 mapping iterations" for T. japonicus. Final assembly: "18,611 of 36,114,914 whole genome sequence reads were assembled to C. equula 2. Four mapping iterations were performed before no more whole genome sequences were aligned to the assembly." (with the run configured for 1000 iterations). "For the map to reference algorithm (Geneious version 11.1.5) there does not appear to be a downside associated with using many mapping iterations such as 100 or 10,000. This is because the mapping iterations are discontinued once no additional sequence reads are aligned to the reference." Caveat: "using large numbers of mapping iterations could introduce mistakes or bias in the consensus sequence as more sequencing errors are included". Three of six uncertain regions (ND2, ATP6, control region) "were not fully resolved by increasing the number of mapping iterations". A reference from a more distant genus (T. japonicus, 89.7%) outperformed one of the two same-genus references, so "a reference mitogenome from a taxonomically closely related species does not guarantee it will perform as well".
Recommendations (verbatim headings): "Use a high quality reference mitogenome of a close relative"; "Ensure enough mapping iterations are used"; "Investigate the quality of the assembly" (map to several references, compare identity along the genome); "Report data confidence" (plot depth and identity, "omitting poorly resolved regions").
INFERENCE: Geneious's "Iterate up to N" stops early when an iteration adds no reads, so N is a cap, not a fixed count; for fish skims at ~8-10% divergence, 5-10 iterations reached saturation.

### 9.3 Scyphozoan linear mitogenomes (Cephea cephea, Mastigias albipunctata), Mitochondrial DNA Part B 2024, doi 10.1080/23802359.2024.2429644, PMC11565657 (raw/scyphozoa_pmc.xml). Relevant to MitoPilot's Scyphozoa test project.
SOURCE SAYS: NovaSeq 6000 reads, BBDuk trimmed; "we used the 'Map to Reference' function and built-in mapper of Geneious Prime 2022, with sensitivity set to 'medium/low' and iterations set to 3 or 5, starting with GenBank published sequences for mitochondrial genes 16S rRNA (KY610618) and COX 1 (KU900928) ... Results of the pending mitochondrial genome assemblies were inspected and ends trimmed (up to 50 bp) where coverage was low (<5X). Consensus sequences were generated and used as subsequent reference seeds, and the 'Map to Reference' step was repeated until assemblies ceased to increase in size." Yields: 101,434 and 182,567 reads mapped, 860X and 1568X coverage.
INFERENCE: this is a manual bait-and-extend loop (single-gene seed, medium-low, 3-5 fine-tuning iterations per round, trim low-coverage (<5X) ends, re-seed with consensus, repeat until no growth). It is the closest published analogue of "assemble a mitogenome from a seed by iterative mapping in Geneious" and the source of the "N/trim below coverage 5" style rule mentioned in the task prompt.

### 9.4 Westbury et al. 2022 (Methods Ecol Evol 13:2151, doi 10.1111/2041-210X.13990; preprint bioRxiv 10.1101/2021.12.16.472923, fetched via WebFetch of the bioRxiv full text). Not Geneious, but the definitive test of iterative mapping vs bait divergence.
SOURCE SAYS: tools compared MITObim v1.8 (MIRA 4.0.2 wrapper) and a BWA-based "ancient ITErative mapper (aITE mapper)"; MITObim mismatch values tested "0 / 1 / 3 / 5 / 10 / 15"; consensus "-dofasta 3 -minq 30 -minmapq 30 -setMinDepth 3"; aITE mapper mapping quality "10 / 20 / 30", mismatch "-n 0.04 / -n 0.01 / -n 0.001 -o 2", max "100 iterations"; targets spotted hyena and southern cassowary with five bait references each of increasing phylogenetic distance. Findings: "MITObim using mismatch values of 3 or 5, and the phylogenetically closest bait reference sequence" gave the most accurate results; BWA error rate "was consistently higher ... than MITObim"; "PWD and number of inserted bp in general increased with phylogenetic distance to the bait reference, and total sequence length declined"; all aITE runs "converged prior to 100 iterations" except one; "Caution should be applied when only considering a single bait reference, as reference-specific biases can occur. Therefore, multiple bait references may be necessary".
(Exact percent divergences per bait were not in the extracted text; the Wiley full text 403s.)

### 9.5 Hahn, Bachmann, Chevreux 2013, MITObim, NAR 41(13):e129 (PMC3711436)
SOURCE SAYS: MIRA mapping + MIRAbait k-mer baiting (k=31) loop; iterations needed: Gyrodactylus 15 and 12 (K2P distance 0.26 between the two species' mitogenomes, "comparable to human-macaque"), teleost hosts with heterologous salmonid references 8 and 5 iterations (K2P ~0.13); from a COI barcode seed alone: 26 iterations (de novo mode) or 115 (mapping mode); worked at 18x to 2112x coverage; MIA was 15x slower.
INFERENCE: 5-15 iterations is the typical convergence range for a whole-mitogenome heterologous reference at 10-25% distance; a single-gene seed needs 25+.

### 9.6 Fonseca et al. 2018 "A new strategy to infer circularity applied to four new complete frog mitogenomes" (PMC5916287)
SOURCE SAYS: assembled with MIRA/MITObim from distantly related references (different families); circularity inferred with the AWA tool: find identical k-mers at the two ends, then "flipped and rewritten, so the ends are adjacent to each other in the middle of the sequence" and re-map the original paired reads with Bowtie2, scoring coverage and "connectivity" across the junction; permutation tests with 1-5% mutation showed ~2% false positives.
INFERENCE: this is the standard open-source way to validate the origin junction of a mapped mitogenome when the mapper does not natively handle circular references.

### 9.7 Geneious marketing page https://www.geneious.com/features/assembly-mapping
SOURCE SAYS: "The exclusive Geneious Read mapper with its iterative approach produces superior results when compared to other popular mapping algorithms and can correctly align structural variants."

### 9.8 Published-methods sweep: what people actually set when they assemble a mitogenome by Geneious map-to-reference (Europe PMC full-text search, ~120 open-access papers fetched to research/ft/PMC*.xml, 2016-2026; queries in research/q1.json)

Every paper below is an organelle (mostly mitogenome) study whose Methods name a Geneious mapper sensitivity and/or iteration count. Quotes are verbatim from the full text; taxon and version as stated. This is the empirical answer to "what settings do practitioners use", independent of Geneious's own documentation.

| PMC id | Year | Taxon / data | Sensitivity | Iterations | Other stated settings |
|---|---|---|---|---|---|
| PMC5240118 | 2016 | millipedes, Geneious 8.1 | medium-low / fast | up to 5 | "maximum of 2% mismatches, a maximum gap size of 3 bp ... minimum overlap of 100 bp; do not trim" |
| PMC7800636 | 2016 | Australian smelt, 9.1.5 | (not stated) | "iterative map to reference" | congeneric reference |
| PMC7707411 | 2019 | ladybird, custom | custom, max mismatch 30% | 25 | reference = different family (Henosepilachna) |
| PMC7782976 | 2020 | Sacalia turtles, 2020.0.3 | "Low Sensitivity/Fastest" | "Fine Tuning(None)" | BBDuk-trimmed |
| PMC6979410 / PMC7694756 | 2020 | crickets, ladybird (seed-and-grow) | custom, max mismatches 30 (extract), then 10 (grow) | "iterate up to 10 times" then "iterate up to 25 times" | consensus "Threshold: Highest quality; Assign quality: Highest" |
| PMC7369027 | 2020 | jerboa, 577,554 reads to a different-genus reference | "Highest sensitivity/Medium" | "iterate up to 25 times" | |
| PMC7899645 | 2021 | mongoose | Medium-Low | (not stated) | four congeneric references |
| PMC8079034 | 2021 | blue runner (Carangidae), 11.0.2 | medium-low | (not stated) | |
| PMC8519520 | 2021 | hydrozoan | custom | (not stated) | "minimum 100 bp overlap and maximum 2% mismatch" |
| PMC9022641 | 2022 | Saturniidae moths, 2021.1.1 | "low to medium" | up to 5 | |
| PMC9359134 | 2022 | 24 marine skims (Skimming for barcodes) | "medium/low" | "up to 10 times" | COI seed, "trimmed at the ends (up to 50 bp) where coverage was low (<5X)", re-seed "until the assemblies stopped increasing in size and identical stretches ... at the 5' and 3' ends"; whole-mitogenome congeneric/confamilial reference "single set of up to 10 iterations" |
| PMC9665073 | 2022 | polecat, 2022.1 | medium-low | 5 | |
| PMC9904314 | 2023 | Poecilia, 2022.2.2 | "Medium/Fast" | "iterative fine-tuning" | |
| PMC9946298 / PMC10283032 | 2023 | Gempylidae fishes, 2020.2.5 | "medium/low" | up to 5 | "followed by manual curation" |
| PMC10048463 | 2023 | treeshrew, 2019.1 | default | default | reference = consensus of six Tupaia species |
| PMC10130705 | 2023 | historical hutia (aDNA) | custom, "2% allowed mismatch" | "iterate up to five times" | |
| PMC10287144 | 2023 | surfperch (long reads then short-read polish), 2022.1.1 | "Medium Sensitivity/Fast" | default | |
| PMC10295106 | 2023 | bird lice (fragmented mitogenomes) | custom | (not stated) | "80 bp minimum overlap and 95% minimum identity; maximum 5% gaps per read with 2 bp maximum gap size; maximum mismatches per read 5% and maximum ambiguity 2" |
| PMC10349043 | 2023 | bison aDNA, 2021.1.1 | default | default | BBDuk default trimming |
| PMC10381751 | 2023 | lichen fungus | "medium-low sensitivity/fast" | 5 | |
| PMC10403706 | 2023 | sweetpotato weevil x40, 2022.0.2 | medium | 5 | same-species reference |
| PMC10507443 | 2023 | Habromys deer mice, 2021.2.2 | "Medium-Low sensitivity, Maximum mismatches = 20%, Maximum gaps = 10%" (stated as defaults) | (not stated) | "using 5X as the lowest coverage to call a base" |
| PMC10531248 | 2023 | rock scallops (re-mapping check) | custom, "minimum mapping quality of 95%, maximum mismatch of 5%" | 3 | |
| PMC10576146 | 2023 | Caribbean parrots aDNA | "medium sensitivity/fast" | "iterate up to five times" | |
| PMC11034133 | 2024 | myxozoans, 2023.2.1 | "Medium-Low Sensitivity / Fast" | default | "Map multiple best matches: Randomly" used to size repeats by coverage |
| PMC11091489 | 2024 | corals skims, 2023.1.2 | "Medium-Low Sensitivity/Fast" | 5 | consensus "threshold was set to 90% identity ... a "?" was called if the coverage was less than 10 mapped reads ... quality ... highest quality from any single base. Each consensus sequence was trimmed to its reference" |
| PMC11104678 | 2024 | Rubiaceae plastomes | medium-low | up to 5 | "restricting paired reads to map nearby"; iterative mapping to extend contigs |
| PMC11202683 | 2024 | Coturnix quails, 2023.2.1 | Medium | 25 | "highest quality threshold" |
| PMC11220847 / PMC11896769 | 2024/2025 | sharks (Ion Torrent), 2019.1.3 / 2024.0.2 | medium | 5 | (Section 9.1) |
| PMC11387203 | 2024 | leafhoppers, 2019.2.1 | Medium-Low | 5 | 600 bp COI seeds as references |
| PMC11384887 | 2024 | Oikopleura, 2023.2.1 | medium-low | (not stated) | "randomly map multiple best matches, only map paired reads which both map, and trim paired read overhangs" |
| PMC11560288 | 2024 | stingless bee (nuclear probes) | "medium sensitivity and fast" | up to 25 | |
| PMC11565657 | 2024 | Scyphozoa (Section 9.3), 2022 | "medium/low" | 3 or 5 | single-gene seeds, trim ends <5X, re-seed until no growth |
| PMC11758796 | 2025 | Notropis minnows, 2024.0.7 | "Medium/Fast" (default) | "iterative fine-tuning" (default) | |
| PMC11764287 / PMC12669778 | 2025 | rye chloroplast, R7 | "default medium-low" | (not stated) | |
| PMC11766006 | 2024 | olive fruit fly (Ion Torrent), 2023.1.2 | "low to medium" | up to 5 | |
| PMC11821457 | 2025 | glirids and squirrels | "highest sensitivity/medium" | "iterate up to 25 times" | reads also GetOrganelle-assembled; re-mapped with BBMap "Normal" |
| PMC11871894 / PMC11882780 | 2025 | krill, shrimp skims, 2019.0.4 | "Medium-Low" | up to 5 | compared against SPAdes de novo |
| PMC12243559 | 2025 | Mallocybe fungus | "Medium-Low Sensitivity/Fast" | 3 | |
| PMC12349903 | 2025 | goats, 2024.0 | (Geneious mapper, not stated) | (not stated) | BBDuk Q25, min length 50 |
| PMC12418279 | 2025 | Strongyloides nematode | "Medium Sensitivity/Fast" then custom "zero alignment gaps, a minimum mapping similarity of 95%" | (not stated) | two-stage: mapping consensus, then combined map+de novo workflow, then strict re-map |
| PMC12486443 | 2025 | Amami rabbit, R9 | "High" | default | |
| PMC12551961 | 2025 | Amblyomma tick, ~485,000 reads | "Highest Sensitivity/Medium" | up to 25 | |
| PMC12948867 | 2026 | Haemaphysalis tick, R9 | "medium-low" | "iterate up to five times" | four congeneric RefSeq references at once |
| PMC13093438 | 2026 | Adonis plastomes | (not stated) | 20 | "to connect the gaps present between contigs" when GetOrganelle failed |
| PMC13299176 | 2026 | sheep ked, 2025.0.2 | "Medium-low Sensitivity/Fast" | (not stated) | BBDuk Q20 |

Tally (papers stating a sensitivity, n = 36): Medium-Low / "low to medium" 19; Medium (incl. "default") 9; Highest/High 4; Low 2; custom-only 2 (plus several customs layered on a preset). Papers stating an iteration count (n = 27): 5 (or "up to 5") 17; 25 5; 10 2; 3 2; 20 1; None 1.
INFERENCE: (1) Medium-Low with "iterate up to 5" is the community default for skims and it is what Geneious itself shipped as the default in the Geneious 6 era; Medium/5 is the current shipped default and equally common. (2) The papers that chose Highest or 25 iterations are those with a distant reference (jerboa: different genus; ladybird: different family; ticks; glirids) or a seed-and-grow design, matching the white paper's mechanism (higher sensitivity and more iterations recover reads in divergent regions). (3) Practitioners layer three kinds of custom edits on the presets: stricter mismatch caps (2-5%) when the reference IS the sample (re-mapping for coverage checks or aDNA), looser caps (30%) when extracting reads from a distant reference, and paired-read restrictions ("only map paired reads which both map", "map nearby") for cleaner coverage. (4) Consensus rules quoted in the wild: Highest Quality threshold (PMC6979410, PMC11202683); 90% identity with "?" below 10 reads and trim-to-reference ON for a downstream MSA (PMC11091489); base called only at >=5X (PMC10507443); trim ends <5X (PMC9359134, PMC11565657). No paper reports a "0%" threshold.

### 9.9 The only head-to-head mapper benchmark on mitogenome consensus from skims: Bilgin Sagalkin et al. 2022, BMC Genomics 23:584, "Mitochondrial genome sequencing, mapping, and assembly benchmarking for Culicoides species" (PMC9375341, doi 10.1186/s12864-022-08743-x; research/ft/PMC9375341.xml)

Design (SOURCE SAYS): Illumina reads from single midges (some after mitochondrial isolation, very low read counts) mapped with "BWA v0.7.17 ... BWA-MEM ... Bowtie2 v2.4.4, Bowtie v1.3.1, Minimap2 v2.17, BBMap v38.84, and Geneious ... as implemented in Geneious Prime v2021.2.2"; "The settings of all mappers were kept at default except for the mapping sensitivity, which was set to "highest" in all but the Geneious mapper ("high") to reduce the runtime"; consensus terminals with missing data trimmed; results only counted when >= 5 reads mapped. Metrics: identical sites (IS) and % pairwise identity (PI) of the consensus vs the reference, plus "differences in non-overlapping sequences" as a third metric. Two reference situations:
- Same-species reference (C. sonorensis reads vs C. sonorensis scaffold710 LN484060.1): "BWA produced the best results ... 15,398 ... identical sites (IS) and ... 99.97% ... pairwise identity"; "Bowtie2 was the second-best mapper with 99.97% PI, 15,391 IS"; "The consensus sequences derived from the Geneious mapper were among the lowest in percentages of pairwise genetic identity and identical sites for C. sonorensis".
- Different-species reference (C. biguttatus reads vs C. arakawae NC_009809.1, the "most closely related non-specific reference"): "Bowtie2 resulted in the highest number of identical sites and percentage of pairwise identity ... 7,418 IS and 96.74% PI"; "Geneious ranked second, with 3,217 IS, and 85.73% PI ... and 4,002 IS and 86.89% PI ... followed by BBMap (2,062 IS and 82.11% PI) and BWA (1,354 IS and 84.73% PI), while Bowtie did not generate consensus sequences or good alignments in any of the tests." "Despite Geneious ranking second in terms of IS, the mapper consistently recovered the highest number of differences for C. biguttatus mitogenomes when compared to the reference mitochondrial genome, doubling the number of differences seen in the second-best mapper (e.g. ... 983 [Geneious] vs 371 [Bowtie2] nucleotide differences)." Geneious also produced the longest consensus ("18,708 bp" vs Bowtie2 "18,140 bp"). Swapping the reference (C. sonorensis instead of C. arakawae) made the Bowtie2 consensus "more similar to that of C. sonorensis than that of C. arakawae, which indicated a high dependency on the used reference genome in absence of a species-specific reference (Supplementary Figure S2). Due to this strong mapping bias, we selected a de novo assembled consensus sequence for C. biguttatus". Also: "More stringent quality trimming at PHRED 20 resulted in the best results for both the mapper and de novo assembly strategies".
The paper does not state which Geneious iteration count or consensus threshold was used (default Medium/5 is implied by "kept at default" but the sensitivity was raised to "high"), nor the percent divergence between C. biguttatus and C. arakawae (the 96.7% PI of the Bowtie2 consensus is an upper bound on similarity in the mappable regions).
INFERENCE for MitoPilot: this is the one dataset where Geneious's more permissive seed-and-expand (high sensitivity, 30% mismatches allowed) was compared with Bowtie2 defaults on a divergent reference. Geneious mapped more (longer consensus) but the extra sequence carried twice as many differences from the reference, which the authors read as noise or mis-mapping rather than true divergence. Their metric (identity to the REFERENCE) penalizes any true divergence too, so the result is ambiguous, but it is a warning that high sensitivity plus a distant reference inflates the consensus with low-confidence bases, and that the reference used shapes the consensus (reference bias) in both mappers. The Geneious design goal for a divergent reference (Section 7 white paper: 89% identity, iterate to consensus) and the Culicoides result together argue for: iterate, but keep a per-site confidence (depth, agreement, mapping quality) and mask or flag sites rather than trusting the longest consensus.

### 9.10 Three teaching or lab protocols that spell out the Geneious seed-and-grow recipe
- Hartnell College "General Botany Genomics Research Project" (https://www.hartnell.edu/faculty/general-botany-genomics-research-project.html, raw/hartnell.txt), Fucus spiralis mitogenome (~36.4 kb) from a Fucus vesiculosus reference. SOURCE SAYS: "Under the "Fine Tuning" line, select "Iterate up to 5 times" ... click the "Do not trim" button ... "Save contigs" ... Under "Sensitivity" ... select "Low Sensitivity/Fast"." Then: copy the consensus, look for "??? or ambiguous letters such as "N" (N = regions of the sequence that Geneious was unable to map)"; if gaps remain, use the draft "as the seed (bait) for an additional mapping of 5 iterations"; if a gap still does not close, "Select about 250 bp of the sequence to the left of the ???" as a new bait named "gap1growtoright", map again "(=5 iterations)", paste the extension over the gap, repeat, then "confirm that there are no errors in your sequence by mapping one final time."
- GitHub README of a Hylomys (gymnure) mitogenome phylogeny project (raw/hylomys_readme.md; historical museum samples, Geneious Prime 2023.0.4). SOURCE SAYS: lineage references were built "by mapping the PE reads iteratively to the published reference AM905041.1 (17290pb), using the Geneious mapper with Medium-Low sensitivity, and up to 5 iterations", picking per lineage "the sample with most mitochondrial reads (as previously determined by BWA mapping)", then generating "lineage-specific consensus sequences" as the references for the remaining (low-coverage) samples.
- Winn et al. 2025 Bio-protocol (Section 9.1) and Kemp (Section 9.2) complete the set.
INFERENCE: all published recipes converge on the same loop MitoPilot would implement: map (Medium-Low or Medium, 5 iterations, no in-mapper trimming), call a reads-only consensus with explicit unknown characters where coverage is missing, re-use the consensus as the next reference or bait, stop when the sequence stops growing, and finish with one clean pass of all reads against the final consensus.

---------------------------------------------------------------------------------------------------

## 10. What the advisor and companion sources imply for the MitoPilot case (short-read Illumina genome skim, single related-species reference at roughly 1-15% divergence, circular genome, goal = complete consensus)

### 10.1 Advisor path
DNA-seq -> Short reads -> tandem-repeat question. SOURCE: for "No" the advisor returns Geneious (primary), Bowtie2 (alternative); for "Yes" BBMap (primary), minimap2. INFERENCE: answer "No" for typical vertebrate mitogenomes; the advisor itself gives no further guidance, so everything below comes from the manual, tutorial, white paper, workflow files, and the three mitogenome papers.

### 10.2 Concrete Geneious settings, with provenance
| Setting | Recommended value for this case | Basis |
|---|---|---|
| Mapper | Geneious | SOURCE: advisor (short reads, non-complex reference); help article 360044628612 lists circular-origin handling and iterative extension as Geneious-only advantages |
| Sensitivity | Medium-Low / Fast to Medium / Fast; go to Medium-High or Highest only if the mapping rate is low or the reference is a different genus/family | SOURCE: Winn 2025 quotes Geneious help: "medium or medium-low sensitivity is usually the best option for large numbers (e.g., 100,000 or more) of next-generation sequencing reads"; 19 of 36 published mitogenome studies used Medium-Low, 9 Medium, 4 Highest (Section 9.8); tutorial troubleshooting: "Low Mapping Rate ... Increase sensitivity to Medium-High". INFERENCE: Medium-Low allows 20% mismatches / 10% gaps per read (PMC10507443) and Medium 30% / 15% (Section 8.1), both cover 15% divergence; the Culicoides benchmark (Section 9.9) shows that Highest on a distant reference buys length at the cost of doubtful bases |
| Fine Tuning iterations | "Iterate up to" 10 as a cap for a whole-mitogenome reference (5 is the Geneious default and the community mode; 10 covered the worst reference in the trevally test; 25 is what the distant-reference and seed-and-grow papers used; a higher cap costs nothing because iteration stops when no new reads map) | SOURCE: white paper and manual (5 = default); Section 9.8 tally (17 of 27 papers use 5, 5 use 25, 2 use 10); Kemp thesis saturation at 3-10; Kemp: "there does not appear to be a downside associated with using many mapping iterations"; Geneious staff: "Try setting iterations to a higher number, say 1000 ... Geneious will only perform as many iterations as required" (Section 5.15); MITObim 5-15 for heterologous references |
| Find structural variants | off for the main run; optional second run on if a gene-order rearrangement is suspected | SOURCE: default off in every public workflow; manual: discovery needs >=2 supporting reads and only finds insertions shorter than a read. INFERENCE: with a related-species reference the "structural variants" mode mainly helps for control-region indels; Winn 2025 warns reference mapping "may also collapse duplicated regions" regardless |
| Trim Before Mapping | Do not trim (trim with BBDuk beforehand, Q20) | SOURCE: tutorial ("Do not trim (already trimmed with BBDuk)"), preprocessing article (Q13, preferably Q30), Kemp ("no trimming before mapping") |
| Paired reads | Set Paired Reads with the library's mean insert; leave "Only map paired reads which map nearby" OFF | SOURCE: paired reads article; default onlyMapPairedHitsReference=false in every public workflow; the "map nearby" restriction was a deliberate amplicon-specific choice in the SARS-CoV-2 recipe. INFERENCE: turning it on would discard pairs that straddle divergent regions in a skim |
| Map multiple best matches | Randomly (default) | SOURCE: all public workflows; white paper |
| Circular reference | keep the reference document circular | SOURCE: manual: "The mapper handles circular reference sequences by indexing reference sequence words spanning the origin and allowing the expansion step to wrap past the ends"; help article: "maps correctly around the origin". INFERENCE: with a circular reference the "extend past the ends" behaviour of fine tuning is not needed; with a LINEAR (partial or single-gene) reference it IS needed and iterations must be high enough (manual, Section 6.2), which is how the Scyphozoa authors grew a genome from a single gene |
| Consensus threshold | Highest Quality (60%) when reads carry quality; consensus is "of the reads only", never the reference | SOURCE: manual Contig Viewer; tutorial; XML default weighted_60 |
| If no coverage call | "?" (XML default `unknown`) or N; NOT "Ref" | SOURCE: manual lists "- , X/N, ? or Ref". INFERENCE: choosing "Ref" would silently paste reference bases into gaps of the sample consensus (reference bias); N/? keeps gaps honest |
| Low coverage call | call ? / N below depth 3 (XML default coverageThreshold=3 for reference assemblies); the Scyphozoa and "Skimming for barcodes" authors trimmed ends below 5X; Habromys authors called bases only at >=5X; corals authors used "?" below 10 reads; the SARS-CoV-2 recipe masks below 10 | SOURCE: XML; PMC11565657; PMC9359134; PMC10507443; PMC11091489; SARS-CoV-2 article. INFERENCE: for a skim consensus, N below 3-5x is the documented range; 10 is a variant-calling / MSA-grade threshold |
| Read trimming and normalization | BBDuk Q20 (Q13 minimum, Q30 preferred); for a seed-and-grow run on a huge read set, normalize (not error-correct) first, then one final pass with all reads | SOURCE: preprocessing article; Culicoides benchmark ("More stringent quality trimming at PHRED 20 resulted in the best results"); Geneious staff (Section 5.15) |
| Reference-bias check | compare the consensus against the reference and against a de novo assembly; flag runs where the consensus carries many more differences than the mapping rate suggests; where possible try a second reference | SOURCE: Culicoides benchmark (reference swap changed the consensus; Geneious consensus had 2x the differences of Bowtie2); Kemp; Westbury |
| Trim to reference | off (default false) | SOURCE: XML key trimToReference=false. INFERENCE: keep off so a consensus can extend past the ends of a linear/partial reference |
| Call N if quality below | 20 (XML qualityThreshold default) if quality masking is wanted | SOURCE: XML; manual |
| Coverage report | export the coverage graph CSV; annotate low coverage at 2 SD below mean or an absolute depth | SOURCE: manual; tutorial |
| Post-run checks | inspect for unresolved regions; consider a second reference | SOURCE: Kemp recommendations; Westbury ("multiple bait references may be necessary") |

### 10.3 Things the Geneious approach cannot give you (all SOURCE-backed)
- Divergence tolerance is never stated numerically for the Geneious mapper; the only numbers are the white paper's 89% identity test (works, with iterations) and minimap2's asm20 "several percent" / "~15% max divergence" guidance.
- Reference-mapping cannot see duplications or rearrangements absent from the reference (Winn 2025: a duplicated Cytb/D-loop block was missed; Kemp: results "should be verified ... to ensure the assemblies produced ... are not simply reflecting the gene arrangements of the reference"). A de novo cross-check (MitoPilot already has GetOrganelle/MitoFinder) remains necessary.
- Iterations cannot rescue a poor reference (Kemp: "A reduction in quality ... could not be mitigated by simply increasing the number of mapping iterations").
- Reference bias is real and measured: with a different-species reference the Culicoides benchmark found the Geneious consensus (high sensitivity) carried "983 [Geneious] vs 371 [Bowtie2] nucleotide differences" from the reference and that swapping the reference changed the Bowtie2 consensus itself; the authors abandoned all mapped consensuses for that species in favour of de novo (Section 9.9). No Geneious source addresses reference bias at all.
- The "validation table" in the advisor article contains no measurements; the only Geneious-run numbers anywhere are the 2012 white paper's (Section 7), on a single 89%-identity bacterial gene.

### 10.4 Which of the task's example recommendations are actually supported
- "iterate 5-25 times": 5 is the documented default (manual/tutorial/white paper/Winn) and the community mode (17 of 27 papers); 10 (Kemp saturation for the worst reference; two seed-and-grow papers) and 25 (five papers, all distant-reference or seed-and-grow) are real dropdown values ("Iterate up to 10 times", "Iterate up to 25 times"); Kemp ran up to 100000 and Geneious staff suggest 1000 with no downside because iteration stops early. SUPPORTED: 5-10 for a whole-mitogenome reference, 25 for a distant reference or single-gene seed.
- "Medium-Low sensitivity": SUPPORTED, it is the single most common published choice (19 of 36 papers) and the Geneious 6 era default; Medium is the current default and equally supported. Second-hand preset numbers: Medium-Low = 20% max mismatches, 10% max gaps per read.
- "trim to reference off": SUPPORTED as the default (trimToReference=false in the consensus options); the one paper that turned it on did so to feed an MSA (PMC11091489), not to assemble.
- "consensus: 0% majority": NOT found in any source, including 36 published methods sections. Sources recommend "Highest Quality 60%" when qualities exist (tutorial, XML default, PMC6979410, PMC11202683); the manual's percentage semantics imply a 0% threshold is a plain plurality call. RECONSTRUCTION only.
- "N below coverage 5": supported as a practice, not as a Geneious default. Scyphozoa and "Skimming for barcodes" authors trimmed ends below 5X; Habromys authors called bases only at >=5X; Geneious's own default low-coverage call is depth 3 with "?" (XML); corals authors and the SARS-CoV-2 recipe use 10. No Geneious document says "N below 5".

---------------------------------------------------------------------------------------------------

## 11. Open questions (not resolvable from public sources)

1. The exact numeric presets for Low, Medium-Low, Medium-High and Highest sensitivity (index/expansion word length, max mismatch %, max gaps %, repeat-word filter). Recovered: Medium in full (XML), a Low-derived custom set (XML), and Medium-Low's two headline numbers second-hand (20% mismatches, 10% gaps; PMC10507443). Word lengths for Medium-Low, and everything for Medium-High and Highest, remain unknown; the white paper gives only "index length 10 to 15".
2. The exact "Fine Tuning" dropdown menu. Now confirmed by verbatim user quotes: None, Iterate 3 times, Iterate up to 5 times, Iterate up to 10 times, Iterate up to 25 times, plus a free integer (1000 suggested by staff; 100000 run by Kemp). What is still unconfirmed is whether the free integer is a dropdown "custom" entry or a separate field, and the menu for Geneious 2024+ specifically.
3. The in-app help text behind the Sensitivity and Fine Tuning "?" buttons (partly quoted second-hand by Winn 2025). Not published anywhere public.
4. Whether the consensus "Threshold" dropdown literally offers "0% (Majority)"; the manual only documents the percentage semantics and no published methods section mentions it.
5. Per-bait percent divergences in Westbury 2022 (Wiley full text blocked); only the qualitative trend was recoverable.
6. Early stopping of iterations: now supported by a Geneious staff statement ("Geneious will only perform as many iterations as required", 2019, Section 5.15) and by Kemp for 11.1.5; still not in the manual. RESOLVED for practical purposes.
7. Which Geneious iteration count and consensus threshold the Culicoides benchmark used (it says "default" except sensitivity "high"), and the actual C. biguttatus vs C. arakawae divergence; needed to weigh its reference-bias result against the 89%-identity white-paper result.
8. Biostars thread 9592309 ("Geneious Prime (Consense Sequence)") content; blocked at the source and rate-limited at the Wayback Machine.

---------------------------------------------------------------------------------------------------

## 12. Source index (URL -> local raw file)

- Advisor article JSON: https://help.geneious.com/api/v2/help_center/en-us/articles/21749604628372.json -> raw/advisor_api.json, raw/advisor_body.html, raw/advisor_body.txt
- Advisor attachments: https://help.geneious.com/hc/article_attachments/21750387024276 (current tree) -> raw/tree_attachment ; https://help.geneious.com/hc/article_attachments/21749925836308 (older tree) -> raw/tree_attachment_old.png
- Section listing: https://help.geneious.com/api/v2/help_center/en-us/sections/360009331791/articles.json -> raw/section_articles.json
- Sibling articles (JSON API, ids in Section 5): raw/art_<id>.json and raw/art_<id>.txt
- SARS-CoV-2 workflow attachment: https://help.geneious.com/hc/article_attachments/7069270589204 -> raw/covid_workflow.xml
- SNPs per sample workflow attachment: https://help.geneious.com/hc/article_attachments/360059852031 -> raw/snps_workflow.xml
- SARS-CoV-2 dialog screenshot: https://help.geneious.com/hc/article_attachments/5729491623572 -> raw/sarscov2_mapping.png
- Manual: https://manual.geneious.com/en/latest/AssemblyMapping.html -> raw/manual_AssemblyMapping.html/.txt ; https://manual.geneious.com/en/latest/Alignments.html -> raw/manual_Alignments.txt ; dialog figure https://manual.geneious.com/en/latest/figures/Map_to_Ref.png -> raw/manual_Map_to_Ref.png
- White paper: https://desktop-links.geneious.com/assets/documentation/geneious/GeneiousReadMapper.pdf -> raw/GeneiousReadMapper.pdf/.txt, page renders raw/wp-03..05.png
- Tutorial: https://www.geneious.com/tutorials/map-to-reference -> raw/tutorial_mtr.txt ; screenshot https://images.ctfassets.net/ifq1osqu2ba1/2MFOyUMFxb8cU1WPzflWbf/815a38e4dc0028aca6e8f2bca2aebf39/MTR_settings.png -> raw/MTR_settings.png
- Video series page: https://www.geneious.com/series/map-to-reference -> raw/series_mtr.txt ; features page https://www.geneious.com/features/assembly-mapping -> raw/features_mtr.txt
- Winn et al. 2025: https://en.bio-protocol.org/pdf/Bio-protocol5231.pdf -> raw/bioprotocol5231.pdf/.txt ; https://pmc.ncbi.nlm.nih.gov/articles/PMC11896769/
- Kemp thesis chapter: https://bookdown.org/leahmhkemp/welly-trevally-html/mitogenome.html -> raw/trevally.txt
- Scyphozoa paper: https://www.ebi.ac.uk/europepmc/webservices/rest/PMC11565657/fullTextXML -> raw/scyphozoa_pmc.xml (doi 10.1080/23802359.2024.2429644)
- Westbury et al. preprint: https://www.biorxiv.org/content/10.1101/2021.12.16.472923v1.full (WebFetch summary only)
- Hahn et al. 2013: https://pmc.ncbi.nlm.nih.gov/articles/PMC3711436/ (WebFetch summary only)
- Fonseca et al. 2018: https://pmc.ncbi.nlm.nih.gov/articles/PMC5916287/ (WebFetch summary only)
- Public workflow files (GitHub, via gh api): raw/ghwf/wf_1..8.xml (repos listed in Section 8) and raw/ghwf/fhu_FHU-Bioinfo*.xml (https://github.com/FHU-Bioinformatics/workflows, Geneious 2025.1.3)
- Community post on iterations: https://help.geneious.com/hc/en-us/community/posts/360068924391-Geneious-workflow-to-grow-contigs-extremities -> raw/post_grow.json, raw/postc_grow.json (via https://help.geneious.com/api/v2/help_center/community/posts/360068924391.json and .../comments.json)
- Release notes: https://www.geneious.com/updates (all versions) -> raw/release_notes.txt (Version 2024.0 block = advisor debut; Version 6.0 block = iterative mapping debut and circular-origin sensitivity fix)
- Europe PMC full-text sweep: https://www.ebi.ac.uk/europepmc/webservices/rest/search (queries in research/q1.json) and .../rest/PMC<id>/fullTextXML -> research/ft/PMC*.xml (about 120 files; the ~45 with usable Geneious settings are tabulated in Section 9.8)
- Culicoides benchmark: https://pmc.ncbi.nlm.nih.gov/articles/PMC9375341/ (doi 10.1186/s12864-022-08743-x) -> research/ft/PMC9375341.xml
- Hartnell College Fucus mitogenome workshop: https://www.hartnell.edu/faculty/general-botany-genomics-research-project.html -> raw/hartnell.txt
- Hylomys mitogenome project README (GitHub; repository URL not recorded by the first run, not re-findable by code search) -> raw/hylomys_readme.md
- Blocked / not recovered: assets.geneious.com old manual pages (403; Wayback 429), help.geneious.com/entries/22610388 and /22580731 (403, no Wayback capture), biostars.org/p/9592309 (403; Wayback capture exists but returned 429 twice), Wiley full text of Westbury 2022 (403)
