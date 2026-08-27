# Builds inst/test_data/assemblies/UA_MULTI_FRAG.fasta: the UA_MULTI_ONE decoy
# scaffolds plus one mitogenome cut into three pieces, written out of order with
# the middle piece reverse-complemented. Out of order and flipped on purpose: a
# clean in-order split would pass even if the join only concatenated.
# Run from the repo root: Rscript tools/make_ua_multi_frag.R

src <- Biostrings::readDNAStringSet("inst/test_data/assemblies/UA_MULTI_ONE.fasta")
mito <- src[names(src) == "mito_contig"]
decoys <- src[names(src) != "mito_contig"]
stopifnot(length(mito) == 1L)

n <- BiocGenerics::width(mito)[1]
cuts <- round(seq(0, n, length.out = 4))
pieces <- Biostrings::DNAStringSet(lapply(seq_len(3), function(i) {
  Biostrings::subseq(mito[[1]], start = cuts[i] + 1, end = cuts[i + 1])
}))
names(pieces) <- paste0("mito_contig_", seq_len(3))

# Middle piece flipped, so the join has to detect and correct the orientation.
pieces[[2]] <- Biostrings::reverseComplement(pieces[[2]])

# Written 3, 1, 2 so file order carries no information about assembly order.
out <- c(pieces[3], decoys[1:50], pieces[1], decoys[51:100], pieces[2])
Biostrings::writeXStringSet(out, "inst/test_data/assemblies/UA_MULTI_FRAG.fasta")

cat("wrote", length(out), "contigs; piece widths:",
    paste(BiocGenerics::width(pieces), collapse = ", "), "\n")
