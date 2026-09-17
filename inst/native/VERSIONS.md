# Native install pins

Mirrors docker/Dockerfile for MitoPilot 1.5.5. Update both together.

| Component | Version | Env | Source |
|---|---|---|---|
| R | 4.5.2 | mitopilot | conda-forge r-base |
| Java | 21 | mitopilot | conda-forge openjdk |
| Nextflow | 25.10.4 | mitopilot | bioconda |
| fastp | 0.23.4 | mitopilot | bioconda |
| SPAdes | 4.1.0 | mitopilot | bioconda |
| GetOrganelle | 1.7.7.1 | mitopilot | bioconda |
| bowtie2 | 2.5.4 | mitopilot | bioconda |
| bwa | 0.7.19 | mitopilot | bioconda |
| samtools | 1.24 | mitopilot | bioconda |
| minimap2 | 2.28 | mitopilot | bioconda |
| BLAST+ | 2.16.0 | mitopilot | bioconda |
| MITOS2 | 2.1.10 | mitos | bioconda |
| tRNAscan-SE | 2.0.12 | trnascan | bioconda |
| ARAGORN | 1.2.41 | aragorn | bioconda |
| bam-readcount | 1.0.1 | bamreadcount | bioconda |
| NCBI ORFfinder | current linux-i64 | orffinder | ftp.ncbi.nlm.nih.gov (bootstrap) |
| MitoFinder | upstream master (1.4.2) | mitofinder | github.com/RemiAllio/MitoFinder (bootstrap) |
| ARWEN | 1.2.3 | opt/arwen | docker/arwen/arwen1.2.3.c (bootstrap) |
| mito_metazoa BLAST DB | see tag below | ref_dbs | GitHub Release asset |

BLAST DB release tag: `blastdb-2026-08-14` (asset `mito_metazoa_blastdb.tar.gz`, 289 MB).

Note: nextflow=25.10.6 is not published on bioconda as of 2026-09-17; pinned to
25.10.4, the highest 25.10.x build available at solve time.

MITOS entry point: runmitos (confirmed via micromamba smoke build, 2026-09-17;
no runmitos.py shipped by this bioconda build).
