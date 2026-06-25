# Build reference NUCLEOTIDE BLAST databases for the mitochondrial rRNA genes
# (rrnL = 16S, rrnS = 12S) from RefSeq mitogenomes. rRNAs do not translate, so
# they have no featureProt entry; the annotate-details rRNA alignment uses these
# featureNuc/<gene>.fas databases when present (and otherwise falls back to the
# per-sample BLAST reference genome).
#
# This streams every RefSeq metazoan mitogenome GenBank record in a single bulk
# efetch (edirect batches + retries internally) to a file, parses it locally in
# record-aligned chunks (so the multi-GB file is never fully in memory), slices
# out each rRNA feature from the ORIGIN sequence, normalizes the gene name
# (rrnL / rrnS), de-duplicates, and builds a nucleotide BLAST db (via the `blast`
# conda env).
#
# REVIEW GATE: sequences are staged under data-raw/rrna_refdbs/ with a provenance
# table (data-raw/rrna_refdb_provenance.csv) for maintainer review. Nothing is
# copied into ref_dbs/ automatically -- after review, move <gene>.fas (+ BLAST
# index) into ref_dbs/Mitos2/<clade>/featureNuc/ and rebuild the distributed
# tarball.
#
#   Rscript data-raw/build_rrna_refdbs.R
#
# Header contract (see R/annotate_utils.R get_top_hits_nuc):
#   >{accession}:{gene}-1-1-{len} {Species}
# The gene token must be hyphen-free (rrnL / rrnS satisfy this).

suppressMessages({
  library(Biostrings)
  devtools::load_all(".", quiet = TRUE)  # for normalize_mito_gene()
})

EDIRECT <- "edirect"   # conda env with esearch/efetch
BLASTENV <- "blast"    # conda env with makeblastdb
STAGE <- "data-raw/rrna_refdbs"

# Retention cap. Set to Inf to keep everything (no cap) -> all RefSeq metazoan
# mito rRNAs. An NCBI API key (env var NCBI_API_KEY) lets edirect run faster; it
# is picked up automatically by esearch/efetch.
MAX_PER_GENE <- Inf    # cap retained references per rRNA gene (Inf = no cap)
dir.create(STAGE, showWarnings = FALSE, recursive = TRUE)

`%||%` <- function(a, b) if (length(a) == 0 || is.null(a)) b else a

# All of Metazoa. Replace with a finer taxid breakdown to partition a very large
# fetch into resumable chunks.
clades <- c(Metazoa = 33208)

conda_run <- function(env, cmd, stdout = TRUE) {
  system(sprintf("conda run -n %s bash -lc %s", env, shQuote(cmd)),
         intern = isTRUE(stdout), ignore.stderr = TRUE)
}

# Parse a vector of GenBank flat-file lines (one or more records, "//"-delimited)
# and return rRNA rows: gene / accession / taxon / length / sequence.
parse_gb_records <- function(lines) {
  ends <- which(lines == "//")
  if (length(ends) == 0) return(NULL)
  starts <- c(1L, head(ends, -1L) + 1L)
  out <- list()
  for (k in seq_along(ends)) {
    rec <- lines[starts[k]:ends[k]]

    acc <- sub("^VERSION\\s+(\\S+).*", "\\1", grep("^VERSION", rec, value = TRUE)[1])
    if (is.na(acc) || !nzchar(acc)) {
      acc <- sub("^ACCESSION\\s+(\\S+).*", "\\1", grep("^ACCESSION", rec, value = TRUE)[1])
    }
    if (is.na(acc) || !nzchar(acc)) next
    org <- grep("^  ORGANISM", rec, value = TRUE)[1]
    organism <- if (!is.na(org)) sub("^  ORGANISM\\s+", "", org) else acc

    fi <- grep("^FEATURES", rec)[1]
    oi <- grep("^ORIGIN", rec)[1]
    if (is.na(fi) || is.na(oi) || oi <= fi) next

    seqlines <- rec[(oi + 1L):length(rec)]
    seqlines <- seqlines[!grepl("^//", seqlines)]
    seqstr <- toupper(gsub("[^A-Za-z]", "", paste(seqlines, collapse = "")))
    if (!nzchar(seqstr)) next
    seq <- DNAString(seqstr)

    feat <- rec[(fi + 1L):(oi - 1L)]
    key_idx <- which(grepl("^ {5}\\S", feat))         # feature key lines (col 6)
    if (length(key_idx) == 0) next
    for (j in seq_along(key_idx)) {
      ki <- key_idx[j]
      key <- sub("^ {5}(\\S+).*", "\\1", feat[ki])
      if (toupper(key) != "RRNA") next
      end <- if (j < length(key_idx)) key_idx[j + 1L] - 1L else length(feat)
      block <- feat[ki:end]
      loc <- sub("^ {5}\\S+\\s+", "", block[1])
      cont <- if (length(block) > 1L) block[-1] else character(0)
      ql <- grep("^ +/", cont)                         # qualifier lines
      if (length(ql) > 0 && ql[1] > 1) {
        loc <- paste0(loc, gsub("\\s+", "", paste(cont[seq_len(ql[1] - 1L)], collapse = "")))
      }
      quals <- if (length(ql) > 0) cont[ql[1]:length(cont)] else character(0)

      gene_q <- sub('.*/gene="?([^"/]+)"?.*', "\\1", grep("/gene=", quals, value = TRUE)[1])
      prod_q <- sub('.*/product="?([^"]+)"?.*', "\\1", grep("/product=", quals, value = TRUE)[1])
      raw <- if (!is.na(gene_q) && nzchar(gene_q)) gene_q else prod_q
      if (is.na(raw) || !nzchar(raw)) next
      gene <- tryCatch(normalize_mito_gene(raw, "rRNA", prod_q %||% raw),
                       error = function(e) NA_character_)
      if (is.na(gene) || !gene %in% c("rrnL", "rrnS")) next

      comp <- grepl("complement", loc)
      nums <- as.integer(unlist(regmatches(loc, gregexpr("[0-9]+", loc))))
      nums <- nums[!is.na(nums)]
      if (length(nums) < 2) next
      p1 <- min(nums); p2 <- max(nums)
      if (p1 < 1L || p2 > length(seq)) next
      s <- subseq(seq, p1, p2)
      if (comp) s <- reverseComplement(s)
      out[[length(out) + 1L]] <- data.frame(
        gene = gene, accession = acc, taxon = organism,
        length = length(s), sequence = as.character(s),
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(out) == 0) NULL else do.call(rbind, out)
}

# Stream-parse a (possibly multi-GB) GenBank flat file in record-aligned chunks
# so the whole file is never held in memory at once.
parse_gb_file <- function(path, chunk = 20000L) {
  con <- file(path, "r"); on.exit(close(con))
  carry <- character(); rows <- list()
  repeat {
    block <- readLines(con, n = chunk, warn = FALSE)
    if (length(block) == 0) {
      r <- parse_gb_records(carry)
      if (!is.null(r)) rows[[length(rows) + 1L]] <- r
      break
    }
    lines <- c(carry, block)
    ends <- which(lines == "//")
    if (length(ends) == 0) { carry <- lines; next }
    last <- ends[length(ends)]
    r <- parse_gb_records(lines[1:last])
    if (!is.null(r)) rows[[length(rows) + 1L]] <- r
    carry <- if (last < length(lines)) lines[(last + 1L):length(lines)] else character()
  }
  if (length(rows)) do.call(rbind, rows) else NULL
}

# One bulk fetch per clade: stream all RefSeq mitogenome GenBank records to a
# file (edirect batches + retries internally), then parse rRNA features locally.
all_rows <- list()
prov <- list()
for (cl in names(clades)) {
  message("Fetching ", cl, " RefSeq mitogenomes (one bulk efetch)...")
  q <- sprintf(
    'txid%d[Organism:exp] AND mitochondrion[Filter] AND refseq[Filter] AND "complete genome"[Title]',
    clades[[cl]]
  )
  gb_file <- file.path(STAGE, paste0("_", cl, ".gb"))
  conda_run(EDIRECT,
            sprintf("esearch -db nuccore -query %s | efetch -format gb > %s",
                    shQuote(q), shQuote(gb_file)),
            stdout = FALSE)
  if (!file.exists(gb_file) || file.size(gb_file) == 0) {
    message("  no records fetched for ", cl); next
  }
  message("  parsing ", round(file.size(gb_file) / 1e6, 1), " MB of GenBank records...")
  df <- parse_gb_file(gb_file)
  unlink(gb_file)
  if (is.null(df)) { message("  no rRNA features found"); next }
  df$source_clade <- cl
  all_rows[[length(all_rows) + 1L]] <- df
  message("  ", nrow(df), " rRNA features")
}
dat <- if (length(all_rows)) do.call(rbind, all_rows) else
  data.frame(gene = character(), accession = character(), taxon = character(),
             length = integer(), sequence = character(), source_clade = character())

for (gene in c("rrnL", "rrnS")) {
  sub <- dat[dat$gene == gene, , drop = FALSE]
  if (nrow(sub) == 0) {
    message(gene, ": no sequences")
    next
  }
  sub <- sub[!duplicated(sub$sequence), , drop = FALSE]
  if (nrow(sub) > MAX_PER_GENE) sub <- sub[seq_len(MAX_PER_GENE), , drop = FALSE]
  seqs <- DNAStringSet(sub$sequence)
  names(seqs) <- sprintf("%s:%s-1-1-%d %s",
                         sub$accession, gene, sub$length,
                         ifelse(is.na(sub$taxon) | !nzchar(sub$taxon), "unknown", sub$taxon))
  fas <- file.path(STAGE, paste0(gene, ".fas"))
  writeXStringSet(seqs, fas)
  conda_run(BLASTENV, sprintf("makeblastdb -in %s -dbtype nucl", shQuote(fas)))
  message(sprintf("  %s: %d sequences -> %s", gene, length(seqs), fas))
  prov[[gene]] <- data.frame(
    gene = gene, accession = sub$accession,
    url = paste0("https://www.ncbi.nlm.nih.gov/nuccore/", sub$accession),
    taxon = sub$taxon, length = sub$length, source_clade = sub$source_clade,
    status = "fetched", stringsAsFactors = FALSE
  )
}

prov_df <- if (length(prov)) do.call(rbind, prov) else
  data.frame(gene = character(), accession = character(), url = character(),
             taxon = character(), length = integer(), source_clade = character(),
             status = character())
prov_df$date_downloaded <- as.character(Sys.Date())
write.csv(prov_df, "data-raw/rrna_refdb_provenance.csv", row.names = FALSE)

cat("\nSummary:\n")
if (nrow(prov_df)) print(table(prov_df$gene)) else cat("(no sequences fetched)\n")
cat("\nProvenance: data-raw/rrna_refdb_provenance.csv\n")
cat("Staged FASTAs/DBs: ", STAGE,
    "/ (REVIEW before moving into ref_dbs/Mitos2/<clade>/featureNuc/)\n", sep = "")
