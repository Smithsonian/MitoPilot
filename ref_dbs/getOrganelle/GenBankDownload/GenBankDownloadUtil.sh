#!/bin/bash
# script to download all mitochondrial GenBank records
# according to user supplied search term(s)
# and prepare two FASTA files as custom databases for GetOrganelle 
#
# author: Dan MacGuigan
#
# dependencies:
# Entrez Direct tools (https://www.ncbi.nlm.nih.gov/books/NBK179288/) - tested with v22.8
# python - tested with v3.12.2
# biopython - tested with v1.84
#
# user must pass the script an advanced GenBank search term 
# search term could be taxonomic (.e.g "Percidae"[Organism])
# or a specific project (.e.g "PRJNA720393"[BioProject])
# or any other valid option
# see https://www.ncbi.nlm.nih.gov/nuccore/advanced
# 
# user can also provide multiple search terms, e.g.
# "Percidae"[Organism] AND "PRJNA720393"[BioProject]
#
# script will download records from GenBank
# filter to include only true singleton mitochondrial sequences
# and create two FASTA files:
#    singlegene.dedup.fasta -- sequences containing one or fewer genes
#                              product name + record ID as a sequence header
#    multigene.dedup.fasta -- sequences containing multiple genes
#                             often whole mitogenomes
#                             record ID as sequence header
#    nogene.dedup.fasta -- sequences containing no genes
#                          record ID as sequence header
# 
# example usage:
# bash GenBankDownloadUtil.sh '"my query"[QueryType]'
# (NOTE: search term(s) must be in single quotes)

# INPUT
USER_QUERY=${1}

# SCRIPT
QUERY="\"mitochondrion\"[All Fields] OR \"mitochondrial\"[All Fields] AND ${USER_QUERY}"

echo "Downloading nucleotide records from NCBI GenBank using the following query:"
echo "${QUERY}"
echo ""

N_REC=$( esearch -db nucleotide -query "${QUERY}" | grep "Count" | cut -f2 -d">" | cut -f1 -d"<" )

echo "Found ${N_REC} records matching this query, downloading now"
echo "This may take a few minutes..."

esearch -db nucleotide -query "${QUERY}" | efetch -format gb > genbank.gb

echo ""
echo "Matching records downloaded to genbank.gb"
echo ""
echo "Removing non-mitochondrial records,"
echo "removing duplicate records with exact sequence matches,"
echo "and extracting sequences into two FASTA files:"
echo "  singlegene.dedup.fasta -- sequences containing one or fewer genes"
echo "  multigene.dedup.fasta -- sequences containing multiple genes, often whole mitogenomes"
echo "  nogene.dedup.fasta -- sequences containing no genes"
echo ""

python parseGB.py genbank.gb
rm test.fasta

N_SING=$( grep -c ">" singlegene.dedup.fasta )
N_MULT=$( grep -c ">" multigene.dedup.fasta )
N_NO=$( grep -c ">" nogene.dedup.fasta )

echo ""
echo "After filtering, retained:"
echo "${N_NO} no-gene sequences - nogene.dedup.fasta"
echo "${N_SING} single-gene sequences - singlegene.dedup.fasta"
echo "${N_MULT} multi-gene sequences - multigene.dedup.fasta"
