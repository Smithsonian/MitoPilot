# Converts a GenBank file to a FASTA file, extracting sequences
# with the organelle "mitochondrion"
# 
# Also splits GenBank records into three categories:
# singlegene.fasta -- sequences containing one or fewer genes
# multigene.fasta -- sequences containing multiple genes, often whole mitogenomes
# nogene.fasta -- sequences containing no annotated genes
# 
# Lastly, removes duplicate sequences and writes unique sequences 
# to a new "dedup" fasta file.
#
# Example usage:
# python parseGB.py myDatabase.gb

from Bio import SeqIO
import sys

def genbank_to_mitochondrial_fasta(genbank_file):
	"""
	Parameters:
	genbank_file (str): Path to the input GenBank file.
	"""
	multigene_seqs = [] 
	nogene_seqs = []
	singlelocus_seqs = []
	
	with open(genbank_file, "r") as gb_file:
		for record in SeqIO.parse(gb_file, "genbank"):
			# check if record has valid sequence
			if record.seq and len(record.seq) > 0:
				# Check if the source feature has the organelle "mitochondrion"
				for feature in record.features:
					if feature.type == "source" and "organelle" in feature.qualifiers:
						if feature.qualifiers["organelle"][0].lower() == "mitochondrion":
							try:
								record
								with open("test.fasta", "w") as fa_file:
									SeqIO.write(record, fa_file, "fasta")
							except:
								print("WARNING - skipping " + record.name + ", contains an invalid sequence")
								continue
							# Extract all gene features in the mitochondrial sequence
							gene_count = 0
							CDS_count = 0
							# check if record has more than one gene feature
							for feature in record.features:
								if feature.type == "gene":
									gene_count += 1
								elif feature.type == "rRNA":
									gene_count += 1
								elif feature.type == "tRNA":
									gene_count += 1
								elif feature.type == "CDS":
									CDS_count += 1
							if gene_count > 1:
								multigene_seqs.append(record)
							elif gene_count == 0 and CDS_count == 0:
								nogene_seqs.append(record)						
							else:
								product_name = get_product_name(record)
								if product_name:
									record.id = product_name.replace(" ", "_") + " " + record.name # Replace spaces with underscores
									record.description = ""  # Clear the description to only keep the name
								else:
									record.id = "no_product " + record.name
									record.description = ""  # Clear the description to only keep the name
								singlelocus_seqs.append(record)

	print("")
	print("Consider manually reviewing GenBank records flagged as invalid sequences,")
	print("they may contain useful information")

	# Write the mitochondrial sequences to FASTA files
	with open("multigene.fasta", "w") as fa_file:
		SeqIO.write(multigene_seqs, fa_file, "fasta")
	
	with open("nogene.fasta", "w") as fa_file:
		SeqIO.write(nogene_seqs, fa_file, "fasta")

	with open("singlegene.fasta", "w") as fa_file:
		SeqIO.write(singlelocus_seqs, fa_file, "fasta")

def get_product_name(record):
	for feature in record.features:
		if "product" in feature.qualifiers:
			return feature.qualifiers["product"][0]  # Return the first product name
			break
	return None  # Return None if no product name is found

def remove_duplicate_sequences(fasta_file, output_file):
	"""
	Parameters:
	fasta_file (str): Path to the input FASTA file.
	output_file (str): Path to the output FASTA file with duplicates removed.
	"""
	unique_sequences = {}  # Dictionary to store unique sequences
	duplicates = 0
	# Parse the input FASTA file
	with open(fasta_file, "r") as input_handle:
		for record in SeqIO.parse(input_handle, "fasta"):
			sequence_str = str(record.seq)	# Convert sequence to string
			if sequence_str not in unique_sequences:
				# Store unique sequence in a dictionary with the record id
				unique_sequences[sequence_str] = record
			else:
				duplicates += 1
	# Write unique sequences to the output FASTA file
	with open(output_file, "w") as output_handle:
		SeqIO.write(unique_sequences.values(), output_handle, "fasta")

# Example usage
genbank_to_mitochondrial_fasta(sys.argv[1])
remove_duplicate_sequences("singlegene.fasta", "singlegene.dedup.fasta")
remove_duplicate_sequences("multigene.fasta", "multigene.dedup.fasta")
remove_duplicate_sequences("nogene.fasta", "nogene.dedup.fasta")
