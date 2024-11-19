#!/bin/bash

# Script used to prepare pre-filtered test datasets.
# Usage: ./process_reads.sh <R1_IN> <R2_IN> <R1_OUT> <R2_OUT> <assembly>

# Input arguments
RAW_R1=$1
RAW_R2=$2
OUT_R1=$3
OUT_R2=$4
ASSEMBLY=$5

# Check if all inputs are provided
if [[ -z "$RAW_R1" || -z "$RAW_R2" || -z "$RAW_R1" || -z "$RAW_R2" || -z "$ASSEMBLY" ]]; then
    echo "Usage: $0 <R1_IN> <R2_IN> <R1_OUT> <R2_OUT> <assembly>"
    exit 1
fi

# Define output filenames
INDEX_PREFIX="mitogenome_index"
TRIMMED_R1="trimmed_reads_R1.fastq"
TRIMMED_R2="trimmed_reads_R2.fastq"
MAPPED_READS_BAM="aligned_reads.sorted.bam"
MAPPED_NAMES="mapped_read_names.txt"
RAW_MAPPED_R1="mapped_raw_reads_R1.fastq"
RAW_MAPPED_R2="mapped_raw_reads_R2.fastq"

# Step 1: Adapter trimming with fastp
echo "Running fastp for adapter trimming..."
fastp --detect_adapter_for_pe --thread 8 --trim_poly_g --correction -i "$RAW_R1" -I "$RAW_R2" -o "$TRIMMED_R1" -O "$TRIMMED_R2"

# Step 2: Build Bowtie2 index for the mitogenome assembly
echo "Building Bowtie2 index for the assembly..."
bowtie2-build --quiet --threads 8 "$ASSEMBLY" "$INDEX_PREFIX"

# Step 3: Map trimmed reads to the mitogenome using Bowtie2
echo "Mapping trimmed reads to the assembly using Bowtie2..."
bowtie2 --quiet --threads 16 -x "$INDEX_PREFIX" -1 "$TRIMMED_R1" -2 "$TRIMMED_R2" -S aligned_reads.sam

# Step 4: Convert SAM to sorted BAM and extract mapped read names
echo "Converting SAM to BAM and extracting mapped read names..."
samtools view --threads 8 -b -F 4 aligned_reads.sam | samtools sort -o "$MAPPED_READS_BAM"
samtools view "$MAPPED_READS_BAM" | awk '{print $1}' | sort | uniq > "$MAPPED_NAMES"

# Step 5: Extract raw reads corresponding to mapped read names
echo "Extracting raw reads corresponding to mapped reads..."
seqtk subseq "$RAW_R1" "$MAPPED_NAMES" > "$OUT_R1"
seqtk subseq "$RAW_R2" "$MAPPED_NAMES" > "$OUT_R2"

# Cleanup intermediate files
echo "Cleaning up intermediate files..."
rm aligned_reads.sam aligned_reads.sorted.bam "$TRIMMED_R1" "$TRIMMED_R2" "$MAPPED_NAMES" "$INDEX_PREFIX"*

# Final output
echo "Done! Extracted raw mapped reads:"
echo "  - $RAW_MAPPED_R1"
echo "  - $RAW_MAPPED_R2"

