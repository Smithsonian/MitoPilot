#!/bin/bash
# Build a fragmented (multi-scaffold) test sample by removing read pairs that
# cover a few windows of a donor sample's assembly.
#
# Usage: make_scaffjoin.sh <ASSEMBLY> <R1> <R2> <OUT_PREFIX> <WIN_BP> <FRAC1> [FRAC2 ...]
#
# The shipped SCAFFJOIN sample was built inside the MitoPilot container from the
# packaged Conger oceanicus reads and their GetOrganelle assembly:
#
#   bash make_scaffjoin.sh donor_asm.fasta \
#     SRR22396843_R1.fastq.gz SRR22396843_R2.fastq.gz SCAFFJOIN 800 0.20 0.50 0.80
#
# Three 800 bp holes in an 18,059 bp circular donor break it into three
# scaffolds of one path, which is what the scaffold join editor is for.
set -euo pipefail

ASM=$1; R1=$2; R2=$3; OUT=$4; WIN=$5; shift 5
FRACS=("$@")

SEQID=$(head -1 "$ASM" | sed 's/^>//' | awk '{print $1}')
LEN=$(awk 'NR>1{n+=length($0)} END{print n}' "$ASM")
echo "donor $SEQID length $LEN"

bowtie2-build --quiet --threads 6 "$ASM" idx
bowtie2 --quiet --threads 6 -x idx -1 "$R1" -2 "$R2" -S aln.sam
samtools sort -@ 6 -o aln.bam aln.sam
samtools index aln.bam

: > drop_regions.txt
for f in "${FRACS[@]}"; do
  START=$(awk -v l="$LEN" -v f="$f" 'BEGIN{printf "%d", l*f}')
  END=$((START + WIN))
  echo "${SEQID}:${START}-${END}" >> drop_regions.txt
done
cat drop_regions.txt

: > drop_names.txt
while read -r reg; do
  samtools view aln.bam "$reg" | awk '{print $1}' >> drop_names.txt
done < drop_regions.txt
sort -u drop_names.txt -o drop_names.txt
echo "dropping $(wc -l < drop_names.txt) read pairs"

samtools view aln.bam | awk '{print $1}' | sort -u > all_names.txt
comm -23 all_names.txt drop_names.txt > keep_names.txt
echo "keeping $(wc -l < keep_names.txt) read pairs"

for side in 1 2; do
  IN=$([ "$side" = 1 ] && echo "$R1" || echo "$R2")
  zcat "$IN" | awk -v keep=keep_names.txt '
    BEGIN{ while ((getline n < keep) > 0) k[n]=1 }
    NR%4==1 { name=substr($1,2); sub(/\/[12]$/,"",name); want = (name in k) }
    want
  ' | gzip -c > "${OUT}_R${side}.fastq.gz"
done

rm -f aln.sam aln.bam aln.bam.bai idx*.bt2
echo "wrote ${OUT}_R1.fastq.gz ${OUT}_R2.fastq.gz"
