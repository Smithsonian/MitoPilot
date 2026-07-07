# Build reference protein BLAST databases for novel clade genes that have no
# existing ref_dbs/.../featureProt/<gene>.fas. Fetches RefSeq proteins from NCBI
# (via the `edirect` conda env), reformats headers to the featureProt contract,
# de-duplicates, and builds a BLAST db (via the `blast` conda env).
#
# REVIEW GATE: sequences are staged under data-raw/novel_gene_refdbs/ and a
# provenance table (data-raw/novel_gene_refdb_provenance.csv) is written for
# maintainer review. Nothing is copied into ref_dbs/ automatically -- after
# review, move the <gene>.fas (+ BLAST index) into
# ref_dbs/Mitos2/Metazoa/featureProt/ and rebuild the distributed tarball.
#
#   Rscript data-raw/build_novel_gene_refdbs.R
#
# Header contract (see R/annotate_utils.R get_top_hits / get_top_hits_orf):
#   >{accession}:{gene}-1-1-{len} {Species}
# The gene token must be hyphen-free.

suppressMessages({library(Biostrings)})

EDIRECT <- "edirect"   # conda env with esearch/efetch
BLASTENV <- "blast"    # conda env with makeblastdb
STAGE <- "data-raw/novel_gene_refdbs"
MAX_PER_GENE <- 50L
dir.create(STAGE, showWarnings = FALSE, recursive = TRUE)

# Per-gene fetch spec. `query` is an Entrez protein query; taxids restrict to the
# clades that declare the gene. status "auto" = fetch; "manual" = no clean RefSeq
# family, leave for the maintainer to supply sequences.
specs <- list(
  rvt = list(
    status = "auto",
    taxids = c(Bryozoa = 10205, Demospongiae = 6042, Polychaeta = 6341, Sipuncula = 6519),
    name = '"reverse transcriptase"'
  ),
  dnaB = list(
    status = "auto",
    taxids = c(Hydrozoa = 6074),
    name = '("DnaB"[Protein Name] OR "replicative DNA helicase"[Protein Name])'
  ),
  im = list(
    status = "manual",
    taxids = c(Bryozoa = 10205, Sipuncula = 6519),
    name = NA  # ambiguous gene symbol; needs maintainer-supplied sequences
  ),
  orf167 = list(status = "manual", taxids = c(Demospongiae = 6042), name = NA),
  orf1535 = list(status = "manual", taxids = c(Demospongiae = 6042), name = NA),
  # orf314: built separately from cnidarian mito CDS features, see
  # data-raw/build_orf314_refdb.R (by-name protein query does not work for it).
  orf314 = list(status = "manual", taxids = c(Hydrozoa = 6074), name = NA),
  orf = list(status = "manual", taxids = c(Nemertea = 6217, Pycnogonida = 57294), name = NA)
)

conda_run <- function(env, cmd) {
  system(sprintf("conda run -n %s bash -lc %s", env, shQuote(cmd)),
         intern = TRUE, ignore.stderr = TRUE)
}

fetch_gene <- function(gene, spec) {
  if (spec$status != "auto") {
    return(data.frame(gene = gene, accession = NA, url = NA, taxon = NA,
                      length = NA, source_clade = paste(names(spec$taxids), collapse = ";"),
                      query = NA, status = "manual_needed", stringsAsFactors = FALSE))
  }
  rows <- list()
  seqs <- AAStringSet()
  for (cl in names(spec$taxids)) {
    tx <- spec$taxids[[cl]]
    q <- sprintf('%s AND txid%d[Organism:exp] AND srcdb_refseq[PROP]', spec$name, tx)
    cmd <- sprintf("esearch -db protein -query %s | efetch -format fasta | head -n 2000",
                   shQuote(q))
    fa <- tryCatch(conda_run(EDIRECT, cmd), error = function(e) character(0))
    if (length(fa) == 0) next
    tmp <- tempfile(fileext = ".fa"); writeLines(fa, tmp)
    s <- tryCatch(readAAStringSet(tmp), error = function(e) AAStringSet())
    if (length(s) == 0) next
    acc <- sub("\\s.*$", "", names(s))
    sp  <- sub("^\\S+\\s+", "", names(s))
    sp  <- sub("\\[", "", sub("\\].*$", "", regmatches(sp, regexpr("\\[[^]]+\\]", sp)) %||% sp))
    for (i in seq_along(s)) {
      rows[[length(rows) + 1L]] <- data.frame(
        gene = gene, accession = acc[i],
        url = paste0("https://www.ncbi.nlm.nih.gov/protein/", acc[i]),
        taxon = sp[i], length = width(s)[i], source_clade = cl,
        query = q, status = "fetched", stringsAsFactors = FALSE
      )
    }
    seqs <- c(seqs, s)
  }
  if (length(seqs) == 0) {
    return(data.frame(gene = gene, accession = NA, url = NA, taxon = NA, length = NA,
                      source_clade = paste(names(spec$taxids), collapse = ";"),
                      query = NA, status = "no_hits", stringsAsFactors = FALSE))
  }
  # de-duplicate identical sequences, cap count
  seqs <- seqs[!duplicated(as.character(seqs))]
  if (length(seqs) > MAX_PER_GENE) seqs <- seqs[seq_len(MAX_PER_GENE)]
  prov <- do.call(rbind, rows)
  prov <- prov[match(sub("\\s.*$", "", names(seqs)), prov$accession), ]
  # featureProt header contract: >{acc}:{gene}-1-1-{len} {Species}
  names(seqs) <- sprintf("%s:%s-1-1-%d %s",
                         prov$accession, gene, width(seqs),
                         ifelse(is.na(prov$taxon), "unknown", prov$taxon))
  fas <- file.path(STAGE, paste0(gene, ".fas"))
  writeXStringSet(seqs, fas)
  conda_run(BLASTENV, sprintf("makeblastdb -in %s -dbtype prot", shQuote(fas)))
  message(sprintf("  %s: %d sequences -> %s", gene, length(seqs), fas))
  prov
}

`%||%` <- function(a, b) if (length(a) == 0 || is.null(a)) b else a

prov_all <- list()
for (g in names(specs)) {
  message("Fetching ", g, " (", specs[[g]]$status, ")")
  prov_all[[g]] <- fetch_gene(g, specs[[g]])
}
prov_df <- do.call(rbind, prov_all)
prov_df$date_downloaded <- as.character(Sys.Date())
write.csv(prov_df, "data-raw/novel_gene_refdb_provenance.csv", row.names = FALSE)

cat("\nSummary:\n")
print(table(prov_df$gene, prov_df$status))
cat("\nProvenance: data-raw/novel_gene_refdb_provenance.csv\n")
cat("Staged FASTAs/DBs: ", STAGE, "/ (REVIEW before moving into ref_dbs/)\n", sep = "")
