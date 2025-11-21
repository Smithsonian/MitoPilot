# Map reads to assembly

Map reads to assembly

## Usage

``` r
coverage(
  assembly_fn = "22030FL-06-02-140_assembly_1.fasta",
  paired_reads_1 = "extended_1_paired.fq",
  paired_reads_2 = "extended_2_paired.fq",
  unpaired_reads = "unpaired.fq",
  cpus = 4,
  outDir = NULL
)
```

## Arguments

- assembly_fn:

  Path to the input assembly file (fasta)

- paired_reads_1:

  path to the raw forward input reads (fastq)

- paired_reads_2:

  path to the raw reverse input reads (fastq)

- unpaired_reads:

  path to the raw unpaired input reads (fastq)

- cpus:

  Number of CPUs to use

- outDir:

  Path to the output directory
