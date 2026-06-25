# Build a MitoPilot curation reference database (one per clade) from RefSeq
# mitogenomes. The distributed ref DBs (ref_dbs/Mitos2/<name>.tar.gz) are plain
# BLAST databases:
#   <name>/featureProt/<gene>.fas   protein BLAST per PCG  (curate get_top_hits,
#                                    orf get_top_hits_orf)
#   <name>/featureNuc/<gene>.fas    nucleotide BLAST per rRNA (curate
#                                    get_top_hits_nuc; rrnL = 16S, rrnS = 12S)
# Header contract (see R/annotate_utils.R get_top_hits / get_top_hits_nuc):
#   >{accession} {Species_with_underscores}
# The gene is implicit in the file name, so no gene token is needed in the header.
#
# This streams every RefSeq mitogenome GenBank record for a clade in a single
# bulk efetch (edirect batches + retries internally) to a file, parses it locally
# in record-aligned chunks (so the multi-GB file is never fully in memory), takes
# each PCG protein from the CDS /translation qualifier and each rRNA nucleotide
# slice from the ORIGIN sequence, normalizes gene names, de-duplicates, and builds
# the BLAST databases. Re-run for each new RefSeq release by bumping REFSEQ_VER.
#
# Distributed in the repo (data-raw/ is in .Rbuildignore, so it is NOT part of the
# installed package). Requires the `edirect` and `blast` conda envs.
#
# REVIEW GATE: output is staged under data-raw/curation_dbs/ with a provenance
# table. Nothing is copied into ref_dbs/ automatically -- after review, move
# <name>.tar.gz into ref_dbs/Mitos2/ and point the DB config / GitHub-raw URL at
# the new version.
#
#   Rscript data-raw/build_curation_db.R

suppressMessages({
  library(Biostrings)
  devtools::load_all(".", quiet = TRUE)  # normalize_mito_gene, MITO_*_GENES
})

# ---- configuration ---------------------------------------------------------
REFSEQ_VER <- "235"                       # NCBI RefSeq release; sets the db name
EDIRECT <- "edirect"                      # conda env with esearch/efetch
BLASTENV <- "blast"                       # conda env with makeblastdb
STAGE <- "data-raw/curation_dbs"
MAX_PER_GENE <- Inf                       # cap retained refs per gene (Inf = all)

# One entry per clade db to build. `name` becomes the tarball root + file name;
# match it to the ref_db value the project config points at. Enable Chordata (or
# add finer clades) as needed.
clades <- list(
  Metazoa = list(taxid = 33208, name = sprintf("Metazoa_RefSeq%s", REFSEQ_VER))
  # Chordata = list(taxid = 7711, name = sprintf("Chordata_RefSeq%s", REFSEQ_VER))
)

# PCG genes to retain in featureProt (normalized names). Mirrors the standard 13
# plus the accessory PCGs the rulesets recognize.
PCG_KEEP <- unique(c(MITO_PCG_GENES, MITO_EXTRA_PCG_GENES))

dir.create(STAGE, showWarnings = FALSE, recursive = TRUE)
`%||%` <- function(a, b) if (length(a) == 0 || is.null(a)) b else a

conda_run <- function(env, cmd, stdout = TRUE) {
  system(sprintf("conda run -n %s bash -lc %s", env, shQuote(cmd)),
         intern = isTRUE(stdout), ignore.stderr = TRUE)
}

# ---- GenBank parsing -------------------------------------------------------

# Pull a (possibly multi-line, quoted) feature qualifier value from a feature's
# qualifier lines. Continuation lines are indented; /translation has no internal
# spaces so concatenation reconstructs the protein.
get_qualifier <- function(quals, key) {
  idx <- grep(sprintf("/%s=", key), quals)
  if (length(idx) == 0) return(NA_character_)
  i <- idx[1]
  val <- sub(sprintf('.*?/%s="?', key), "", quals[i])
  if (grepl(sprintf('/%s="', key), quals[i]) && !grepl('"\\s*$', quals[i])) {
    j <- i + 1L
    while (j <= length(quals) && !grepl('"\\s*$', quals[j])) {
      val <- paste0(val, sub("^\\s+", "", quals[j])); j <- j + 1L
    }
    if (j <= length(quals)) val <- paste0(val, sub("^\\s+", "", quals[j]))
  }
  gsub('"', "", sub('"\\s*$', "", val))
}

# Parse one or more "//"-delimited GenBank records; return feature rows:
#   kind (prot/nuc), gene, accession, taxon, length, sequence
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
    organism <- gsub("\\s+", "_", trimws(organism))

    fi <- grep("^FEATURES", rec)[1]
    oi <- grep("^ORIGIN", rec)[1]
    if (is.na(fi)) next

    # ORIGIN nucleotide sequence (counter lines only); needed for rRNA slices.
    seq <- NULL
    if (!is.na(oi) && oi > fi) {
      sl <- rec[(oi + 1L):length(rec)]
      sl <- sl[grepl("^\\s*[0-9]+ ", sl)]
      ss <- gsub("[^ACGTNRYKMSWBDHV]", "N", toupper(gsub("[^A-Za-z]", "", paste(sl, collapse = ""))))
      if (nzchar(ss)) seq <- tryCatch(DNAString(ss), error = function(e) NULL)
    }

    feat_end <- if (!is.na(oi)) oi - 1L else length(rec)
    feat <- rec[(fi + 1L):feat_end]
    key_idx <- which(grepl("^ {5}\\S", feat))
    if (length(key_idx) == 0) next
    for (j in seq_along(key_idx)) {
      ki <- key_idx[j]
      key <- toupper(sub("^ {5}(\\S+).*", "\\1", feat[ki]))
      if (!key %in% c("CDS", "RRNA")) next
      end <- if (j < length(key_idx)) key_idx[j + 1L] - 1L else length(feat)
      block <- feat[ki:end]
      loc <- sub("^ {5}\\S+\\s+", "", block[1])
      cont <- if (length(block) > 1L) block[-1] else character(0)
      ql <- grep("^ +/", cont)
      if (length(ql) > 0 && ql[1] > 1) {
        loc <- paste0(loc, gsub("\\s+", "", paste(cont[seq_len(ql[1] - 1L)], collapse = "")))
      }
      quals <- if (length(ql) > 0) cont[ql[1]:length(cont)] else character(0)

      gene_q <- get_qualifier(quals, "gene")
      prod_q <- get_qualifier(quals, "product")
      raw <- if (!is.na(gene_q) && nzchar(gene_q)) gene_q else prod_q
      if (is.na(raw) || !nzchar(raw)) next

      if (key == "CDS") {
        gene <- tryCatch(normalize_mito_gene(raw, "PCG", prod_q %||% raw),
                         error = function(e) NA_character_)
        if (is.na(gene) || !gene %in% PCG_KEEP) next
        prot <- get_qualifier(quals, "translation")
        if (is.na(prot) || !nzchar(prot)) next
        prot <- gsub("[^A-Za-z*]", "", prot)
        prot <- sub("\\*+$", "", prot)
        if (!nzchar(prot)) next
        out[[length(out) + 1L]] <- data.frame(
          kind = "prot", gene = gene, accession = acc, taxon = organism,
          length = nchar(prot), sequence = prot, stringsAsFactors = FALSE
        )
      } else { # RRNA
        if (is.null(seq)) next
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
          kind = "nuc", gene = gene, accession = acc, taxon = organism,
          length = length(s), sequence = as.character(s), stringsAsFactors = FALSE
        )
      }
    }
  }
  if (length(out) == 0) NULL else do.call(rbind, out)
}

# Stream-parse a (possibly multi-GB) GenBank file in record-aligned chunks.
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

# ---- build one db per clade ------------------------------------------------

write_gene_db <- function(sub, gene, dir, dbtype) {
  sub <- sub[!duplicated(sub$sequence), , drop = FALSE]
  if (is.finite(MAX_PER_GENE) && nrow(sub) > MAX_PER_GENE) {
    sub <- sub[seq_len(MAX_PER_GENE), , drop = FALSE]
  }
  seqs <- if (dbtype == "prot") AAStringSet(sub$sequence) else DNAStringSet(sub$sequence)
  names(seqs) <- sprintf("%s %s", sub$accession,
                         ifelse(is.na(sub$taxon) | !nzchar(sub$taxon), "unknown", sub$taxon))
  fas <- file.path(dir, paste0(gene, ".fas"))
  writeXStringSet(seqs, fas)
  conda_run(BLASTENV, sprintf("makeblastdb -in %s -dbtype %s", shQuote(fas), dbtype))
  nrow(sub)
}

prov <- list()
for (cl in names(clades)) {
  spec <- clades[[cl]]
  name <- spec$name
  message("== ", name, " (txid", spec$taxid, ") ==")

  gb_file <- file.path(STAGE, paste0("_", name, ".gb"))
  if (file.exists(gb_file) && file.size(gb_file) > 0) {
    message("  reusing existing ", gb_file)
  } else {
    q <- sprintf(
      'txid%d[Organism:exp] AND mitochondrion[Filter] AND refseq[Filter] AND "complete genome"[Title]',
      spec$taxid
    )
    message("  bulk efetch...")
    conda_run(EDIRECT,
              sprintf("esearch -db nuccore -query %s | efetch -format gb > %s",
                      shQuote(q), shQuote(gb_file)),
              stdout = FALSE)
  }
  if (!file.exists(gb_file) || file.size(gb_file) == 0) {
    message("  no records fetched; skipping"); next
  }
  message("  parsing ", round(file.size(gb_file) / 1e6, 1), " MB...")
  dat <- parse_gb_file(gb_file)
  if (is.null(dat)) { message("  no features parsed"); next }

  root <- file.path(STAGE, name)
  prot_dir <- file.path(root, "featureProt")
  nuc_dir  <- file.path(root, "featureNuc")
  dir.create(prot_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(nuc_dir, showWarnings = FALSE, recursive = TRUE)

  rows <- list()
  for (gene in sort(unique(dat$gene[dat$kind == "prot"]))) {
    n <- write_gene_db(dat[dat$kind == "prot" & dat$gene == gene, ], gene, prot_dir, "prot")
    rows[[length(rows) + 1L]] <- data.frame(db = name, set = "featureProt", gene = gene, n = n)
  }
  for (gene in sort(unique(dat$gene[dat$kind == "nuc"]))) {
    n <- write_gene_db(dat[dat$kind == "nuc" & dat$gene == gene, ], gene, nuc_dir, "nucl")
    rows[[length(rows) + 1L]] <- data.frame(db = name, set = "featureNuc", gene = gene, n = n)
  }

  tarball <- file.path(STAGE, paste0(name, ".tar.gz"))
  conda_run(BLASTENV,
            sprintf("tar -czf %s -C %s %s", shQuote(tarball), shQuote(STAGE), shQuote(name)),
            stdout = FALSE)
  message("  -> ", tarball)
  prov[[cl]] <- do.call(rbind, rows)
}

prov_df <- if (length(prov)) do.call(rbind, prov) else
  data.frame(db = character(), set = character(), gene = character(), n = integer())
prov_df$refseq_version <- REFSEQ_VER
prov_df$date_downloaded <- as.character(Sys.Date())
write.csv(prov_df, "data-raw/curation_db_provenance.csv", row.names = FALSE)

cat("\nSummary (sequences per gene):\n")
if (nrow(prov_df)) print(prov_df[, c("db", "set", "gene", "n")]) else cat("(nothing built)\n")
cat("\nProvenance: data-raw/curation_db_provenance.csv\n")
cat("Staged tarball(s): ", STAGE,
    "/<name>.tar.gz (REVIEW before moving into ref_dbs/Mitos2/)\n", sep = "")
