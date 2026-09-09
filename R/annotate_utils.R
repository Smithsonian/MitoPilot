#' Codon length treating NA/"" as 0
#'
#' `nchar(NA_character_)` returns 2, which would silently trim 2 bp when a
#' start/stop codon is unknown. This returns 0 for NA or empty strings.
#'
#' @noRd
.codon_len <- function(x) {
  n <- nchar(x)
  n[is.na(x) | x == ""] <- 0L
  as.integer(n)
}

#' Pairwise comparison of AA sequences
#'
#' @param query The focal sequence
#' @param target The target sequence
#' @param type The type of comparison to perform. Options are 'pctId' and 'similarity'
#' @param subMx The substitution matrix to use for the comparison. Default is BLOSUM80
#'
#' @export
#'
compare_aa <- function(query, target, type = c("pctId", "similarity"), subMx = "BLOSUM80") {
  s1 <- Biostrings::AAString(query)
  s2 <- Biostrings::AAString(target)
  alignment <- pwalign::pairwiseAlignment(subject = s1, pattern = s2, substitutionMatrix = subMx)

  # Return query-centric percent identity
  if (type[1] == "pctId") {
    return(100 * pwalign::nmatch(alignment) / nchar(query))
  }

  if (type[1] == "similarity") {
    data(list = subMx, package = "pwalign", envir = environment())
    mx <- get(subMx, envir = environment())
    max_score <- sum(diag(mx)[match(strsplit(query, NULL)[[1]], rownames(mx))])
    res <- 100 * BiocGenerics::score(alignment) / max_score
    return(res)
  }
  return(alignment)
}

#' Recompute reference-hit stats for one focal sequence (vectorized)
#'
#' Computes the same four per-hit metrics as [compare_aa()] /
#' [count_end_gaps()] - `similarity`, `pctid`, `gap_leading`, `gap_trailing` -
#' against a vector of `targets`, but aligns each pair only once instead of
#' four times and runs the pwalign step as a single vectorized call. Used by the
#' annotate-details save handler, which previously called the per-metric helpers
#' row-by-row (up to 4 alignments per hit x100 hits).
#'
#' Results are identical to the per-metric helpers: pwalign global alignment with
#' focal as subject for similarity/pctid, and a per-target DECIPHER alignment for
#' the terminal-gap counts.
#'
#' @param focal The focal amino-acid sequence (single string).
#' @param targets Character vector of target amino-acid sequences.
#' @param subMx Substitution matrix name (default "BLOSUM80").
#'
#' @return A data.frame with columns `similarity`, `pctid`, `gap_leading`,
#'   `gap_trailing` (one row per target).
#'
#' @noRd
recompute_hit_stats <- function(focal, targets, subMx = "BLOSUM80") {
  n <- length(targets)
  if (n == 0) {
    return(data.frame(
      similarity = numeric(0), pctid = numeric(0),
      gap_leading = integer(0), gap_trailing = integer(0)
    ))
  }
  s_focal <- Biostrings::AAString(focal)
  # One vectorized global alignment: focal (subject) vs every target (pattern).
  aln <- pwalign::pairwiseAlignment(
    subject = s_focal,
    pattern = Biostrings::AAStringSet(targets),
    substitutionMatrix = subMx
  )
  pctid <- 100 * pwalign::nmatch(aln) / nchar(focal)
  # Self-score of focal under subMx, loaded once (was reloaded per hit).
  data(list = subMx, package = "pwalign", envir = environment())
  mx <- get(subMx, envir = environment())
  focal_chars <- strsplit(focal, NULL)[[1]]
  max_score <- sum(diag(mx)[match(focal_chars, rownames(mx))])
  similarity <- 100 * BiocGenerics::score(aln) / max_score
  # Terminal gap counts: one DECIPHER alignment per target (both ends from it).
  gap_leading <- integer(n)
  gap_trailing <- integer(n)
  for (i in seq_len(n)) {
    a <- DECIPHER::AlignSeqs(
      Biostrings::AAStringSet(list(s_focal, Biostrings::AAString(targets[i]))),
      verbose = FALSE
    )
    a1 <- as.character(a[1]); a2 <- as.character(a[2])
    gap_leading[i]  <- nchar(stringr::str_extract(a1, "^-*")) - nchar(stringr::str_extract(a2, "^-*"))
    gap_trailing[i] <- nchar(stringr::str_extract(a1, "-*$")) - nchar(stringr::str_extract(a2, "-*$"))
  }
  data.frame(
    similarity = as.numeric(similarity), pctid = as.numeric(pctid),
    gap_leading = gap_leading, gap_trailing = gap_trailing
  )
}

#' Circular-aware interval overlap test
#'
#' Returns a logical vector: does the (possibly wrap-around) interval [p1, p2]
#' overlap each of the intervals given by q1_vec/q2_vec? A wrap-around interval
#' (p1 > p2) spans the origin of a circular sequence.
#'
#' @param p1,p2 start/end of the focal interval (p1 > p2 indicates wrap-around)
#' @param q1_vec,q2_vec vectors of start/end positions to test against
#'
#' @noRd
#'
circ_overlap <- function(p1, p2, q1_vec, q2_vec) {
  # Either interval may span the origin (start > end). Decompose the cases so an
  # origin-spanning *compared* interval (q1 > q2) is handled too, not just a
  # wrapping focal interval. Length-free: a wrapping arc covers the origin point.
  q_wrap <- q1_vec > q2_vec
  if (p1 <= p2) {
    ifelse(
      q_wrap,
      p1 <= q2_vec | p2 >= q1_vec,   # q wraps: focal hits its [start,q2] or [q1,end] arm
      p1 <= q2_vec & q1_vec <= p2    # neither wraps
    )
  } else {
    ifelse(
      q_wrap,
      TRUE,                          # both wrap -> both cover the origin
      q1_vec <= p2 | q2_vec >= p1    # only focal wraps
    )
  }
}

#' Length of a possibly wrap-around interval on a circular sequence of length L
#'
#' @param p1,p2 start/end of the interval (p1 > p2 indicates wrap-around)
#' @param L length of the contig
#'
#' @noRd
#'
circ_len <- function(p1, p2, L) {
  if (p1 <= p2) p2 - p1 + 1L else L - p1 + p2 + 1L
}

#' Overlap length between [p1,p2] (may wrap) and one normal interval [q1,q2]
#'
#' @param p1,p2 start/end of the focal interval (p1 > p2 indicates wrap-around)
#' @param q1,q2 start/end of the (non-wrapping) interval to compare against
#' @param L length of the contig
#'
#' @noRd
#'
circ_overlap_len <- function(p1, p2, q1, q2, L) {
  # Decompose each (possibly origin-spanning) interval into non-wrapping segments
  # and sum the pairwise linear overlaps, so a wrapping q (q1 > q2) is handled.
  segs <- function(a, b) if (a <= b) list(c(a, b)) else list(c(a, L), c(1L, b))
  total <- 0L
  for (s in segs(p1, p2)) {
    for (t in segs(q1, q2)) {
      total <- total + max(0L, min(s[2], t[2]) - max(s[1], t[1]) + 1L)
    }
  }
  total
}

#' Extend an OH call to the (putative) full-length control region
#'
#' MITOS2 reports the origin of replication (OH) as a short locus; the control
#' region is taken to fill the gap between its neighbours. The first/last row on
#' a contig is normally bounded by the contig edges - but a feature spanning the
#' origin sorts last by `pos1` while also occupying `[1, pos2]`, so it, not the
#' edge, is what bounds an OH sitting at either end of the table.
#'
#' @param annotations annotation table, sorted by contig then pos1
#' @param contig_lens named vector of contig lengths
#'
#' @return `annotations` with each OH row renamed "ctrl" and extended
#'
#' @noRd
extend_oh_to_ctrl <- function(annotations, contig_lens) {
  for (idx in which(annotations$gene == "OH")) {
    ctg <- annotations$contig[idx]
    L <- contig_lens[[ctg]]
    ctg_rows <- which(annotations$contig == ctg)
    # every origin-spanning feature on this contig, the OH itself excluded
    wrapped <- setdiff(
      ctg_rows[annotations$pos1[ctg_rows] > annotations$pos2[ctg_rows]], idx
    )

    pos1 <- if (idx != min(ctg_rows)) {
      annotations$pos2[idx - 1] + 1
    } else if (length(wrapped) > 0) {
      # clear ALL of them: stopping after the first leaves the ctrl region
      # overlapping any feature that reaches further past the origin
      max(annotations$pos2[wrapped]) + 1
    } else {
      1
    }

    pos2 <- if (idx != max(ctg_rows)) {
      annotations$pos1[idx + 1] - 1
    } else if (length(wrapped) > 0) {
      min(annotations$pos1[wrapped]) - 1
    } else {
      L
    }

    # The bounds cross when the OH itself overlaps a neighbour. Keep the called
    # coordinates rather than emitting an inverted region, which every
    # downstream consumer would read as a wrap around the origin.
    if (pos1 > pos2) {
      pos1 <- annotations$pos1[idx]
      pos2 <- annotations$pos2[idx]
    }
    annotations$pos1[idx] <- pos1
    annotations$pos2[idx] <- pos2
    annotations$length[idx] <- circ_len(pos1, pos2, L)
    annotations$gene[idx] <- "ctrl"
  }
  annotations
}

#' Wrap a coordinate onto a circular sequence of length L
#'
#' Maps any integer (including 0, negatives, and values past L) onto [1, L].
#'
#' @param p coordinate(s)
#' @param L length of the contig
#'
#' @noRd
wrap_pos <- function(p, L) {
  as.integer(((p - 1L) %% L) + 1L)
}

#' Extract a possibly wrap-around region [p1, p2] from a sequence
#'
#' When p1 > p2 the region spans the circular origin, so it is reconstructed as
#' `[p1, end] + [1, p2]`. Returns the same type as `subseq(seq, p1, p2)` would.
#'
#' @param seq a DNAString/DNAStringSet
#' @param p1,p2 start/end (p1 > p2 indicates wrap-around)
#'
#' @noRd
extract_circ_region <- function(seq, p1, p2) {
  if (p1 <= p2) {
    return(Biostrings::subseq(seq, start = p1, end = p2))
  }
  w <- if (methods::is(seq, "XStringSet")) Biostrings::width(seq)[1] else length(seq)
  Biostrings::xscat(
    Biostrings::subseq(seq, start = p1, end = w),
    Biostrings::subseq(seq, start = 1L, end = p2)
  )
}

#' Format one or more BLAST databases for a `-db` argument.
#'
#' BLAST+ takes several databases as a single space-separated, quoted value. Used
#' so a read-only base DB and a small task-private DB of remote-derived sequences
#' can be searched together without rewriting the base.
#'
#' @param ref_db character vector of database paths
#'
#' @noRd
blast_db_arg <- function(ref_db) {
  if (length(ref_db) > 1L) {
    paste0("'", paste(ref_db, collapse = " "), "'")
  } else {
    as.character(ref_db)
  }
}

#' Get top BLASTP hits
#'
#' @param ref_db reference database; may be several paths, all searched together
#' @param query query sequeencs
#' @param condaenv Conda environment to use for running blastp
#' @param max_blast_hits Maximum number of top BLAST hits to retain (default = 10)
#'
#' @noRd
#'
get_top_hits <- function(
    ref_db,
    query,
    max_blast_hits = 10,
    condaenv = "base") {
  ref_db <- ref_db[nzchar(ref_db) & file.exists(ref_db)]
  if (length(ref_db) == 0L) {
    stop("get_top_hits: no reference database found", call. = FALSE)
  }
  ref_seqs <- Biostrings::readAAStringSet(ref_db)
  db_arg <- blast_db_arg(ref_db)

  if (!is.null(condaenv)) {
    hits_refSeq <- stringr::str_glue(
      "run -n {condaenv}",
      "echo -e '{query}' |",
      "blastp ",
      "-db {db_arg}",
      "-best_hit_score_edge 0.01",
      "-max_hsps 1",
      #"-qcov_hsp_perc 80",
      "-max_target_seqs 1000",
      "-outfmt '6 salltitles evalue'",
      "-query -",
      .sep = " "
    ) |>
      system2(reticulate::conda_binary(), args = _, stdout = TRUE)
  } else {
    hits_refSeq <- stringr::str_glue(
      "-db {db_arg}",
      "-best_hit_score_edge 0.01",
      "-max_hsps 1",
      #"-qcov_hsp_perc 50",
      "-max_target_seqs 1000",
      "-outfmt '6 salltitles evalue'",
      "-query -",
      .sep = " "
    ) |>
      system2("blastp", args = _, input = query, stdout = TRUE)
  }

  if(length(hits_refSeq) == 0) {
    return({
      data.frame(
        acc = character(),
        Taxon = character(),
        eval = numeric(),
        target = character(),
        pctid = numeric(),
        similarity = numeric(),
        gap_leading = numeric(),
        gap_trailing = numeric()
      )
    })
  }

  hits_refSeq <- hits_refSeq |>
    purrr::map_dfr(~ {
      df <- data.frame(stringr::str_split(.x, "\\t", simplify = T))
      colnames(df) <- c("hit", "eval")
      df |>
        dplyr::mutate(across(!hit, as.numeric))
    }) |>
    dplyr::arrange(eval) |>
    dplyr::transmute(
      acc = stringr::str_extract(hit, "^\\S+"),
      Taxon = stringr::str_remove(hit, "^\\S+ "),
      eval = eval
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      target = as.character(ref_seqs[stringr::str_extract(names(ref_seqs), "^\\S+") == acc])[1]
    ) |>
    dplyr::mutate(
      pctid = compare_aa(query, target, "pctId"),
      similarity = compare_aa(query, target, "similarity"),
      gap_leading = count_end_gaps(query, target, "leading"),
      gap_trailing = count_end_gaps(query, target, "trailing"),
      .after = "eval"
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(dplyr::desc(similarity)) |>
    dplyr::slice_head(n = as.numeric(max_blast_hits))


}

#' Build a combined, gene-labeled protein BLAST database for ORF search
#'
#' The distributed per-gene `featureProt/<gene>.fas` files use gene-less headers
#' (`>{accession} {Species}`), so the gene is implicit in the file name. Simply
#' concatenating them would make the same accession non-unique across genes in
#' the combined DB (each genome's cox1/nad1/... share one accession id), which
#' breaks per-hit target retrieval and gene recovery. This reads every per-gene
#' file and relabels each sequence as `>{accession}:{gene}-{i} {Species}` (gene
#' from the file name, `i` a per-gene counter that also disambiguates two copies
#' of a gene in one genome), writes the combined FASTA, and builds its index, so
#' [get_top_hits_orf()] can recover the gene and a unique target per hit.
#'
#' @param feature_dir directory of per-gene `featureProt/*.fas` files. May be
#'   several directories, e.g. the read-only base DB plus a task-private one of
#'   remote-derived sequences; a gene's files are merged across them so the
#'   per-gene hit numbering stays unique.
#' @param out_fasta path to write the combined FASTA (index built alongside)
#' @param condaenv conda env with makeblastdb (NULL = on PATH)
#'
#' @return `out_fasta` on success, or NULL when no per-gene files are present
#'
#' @noRd
build_combined_orf_db <- function(feature_dir, out_fasta, condaenv = "base") {
  feature_dir <- feature_dir[nzchar(feature_dir) & dir.exists(feature_dir)]
  fas <- unlist(lapply(
    feature_dir,
    function(d) list.files(d, pattern = "\\.fas$", full.names = TRUE)
  ), use.names = FALSE)
  if (length(fas) == 0) return(NULL)
  genes <- sub("\\.fas$", "", basename(fas))
  all_seqs <- Biostrings::AAStringSet()
  for (gene in unique(genes)) {
    s <- Biostrings::AAStringSet()
    for (f in fas[genes == gene]) {
      part <- tryCatch(Biostrings::readAAStringSet(f), error = function(e) NULL)
      if (!is.null(part) && length(part) > 0) s <- c(s, part)
    }
    if (length(s) == 0) next
    nm  <- names(s)
    acc <- sub("\\s.*$", "", nm)               # accession (first token)
    sp  <- sub("^\\S+\\s*", "", nm)             # species (rest; may be empty)
    names(s) <- sprintf("%s:%s-%d %s", acc, gene, seq_along(s), sp)
    all_seqs <- c(all_seqs, s)
  }
  if (length(all_seqs) == 0) return(NULL)
  Biostrings::writeXStringSet(all_seqs, out_fasta)
  mk_args <- c("-in", out_fasta, "-dbtype", "prot")
  if (!is.null(condaenv)) {
    system2(reticulate::conda_binary(), c("run", "-n", condaenv, "makeblastdb", mk_args),
            stdout = NULL, stderr = NULL)
  } else {
    system2("makeblastdb", mk_args, stdout = NULL, stderr = NULL)
  }
  out_fasta
}

#' Get top BLASTP hits for an ORF against a combined gene database
#'
#' Like [get_top_hits()], but BLASTs a query against a single combined protein
#' database holding sequences from every gene (built by [build_combined_orf_db()]
#' from the per-gene `featureProt/*.fas` files). Because the gene of an ORF is
#' unknown, the gene name is recovered per hit from the combined-DB header format
#' `>{accession}:{gene}-{i} {species}` that builder injects, and returned in a
#' `gene` column.
#'
#' @param ref_db combined reference database (FASTA with a makeblastdb index)
#' @param query query (amino acid) sequence
#' @param max_blast_hits Maximum number of top BLAST hits to retain (default = 10)
#' @param condaenv Conda environment to use for running blastp (NULL = on PATH)
#'
#' @noRd
#'
get_top_hits_orf <- function(
    ref_db,
    query,
    max_blast_hits = 10,
    condaenv = "base") {
  ref_seqs <- Biostrings::readAAStringSet(ref_db)

  if (!is.null(condaenv)) {
    hits <- stringr::str_glue(
      "run -n {condaenv}",
      "echo -e '{query}' |",
      "blastp ",
      "-db {ref_db}",
      "-best_hit_score_edge 0.01",
      "-max_hsps 1",
      "-max_target_seqs 1000",
      "-outfmt '6 salltitles evalue'",
      "-query -",
      .sep = " "
    ) |>
      system2(reticulate::conda_binary(), args = _, stdout = TRUE)
  } else {
    hits <- stringr::str_glue(
      "-db {ref_db}",
      "-best_hit_score_edge 0.01",
      "-max_hsps 1",
      "-max_target_seqs 1000",
      "-outfmt '6 salltitles evalue'",
      "-query -",
      .sep = " "
    ) |>
      system2("blastp", args = _, input = query, stdout = TRUE)
  }

  if (length(hits) == 0) {
    return({
      data.frame(
        acc = character(),
        gene = character(),
        Taxon = character(),
        eval = numeric(),
        target = character(),
        pctid = numeric(),
        similarity = numeric(),
        gap_leading = numeric(),
        gap_trailing = numeric()
      )
    })
  }

  ref_ids <- stringr::str_extract(names(ref_seqs), "^\\S+")

  hits |>
    purrr::map_dfr(~ {
      df <- data.frame(stringr::str_split(.x, "\\t", simplify = T))
      colnames(df) <- c("hit", "eval")
      df |>
        dplyr::mutate(across(!hit, as.numeric))
    }) |>
    dplyr::arrange(eval) |>
    dplyr::transmute(
      seqid = stringr::str_extract(hit, "^\\S+"),
      acc = stringr::str_extract(hit, "^[^:]+"),
      gene = stringr::str_match(hit, "^[^:]+:([^-]+)-")[, 2],
      Taxon = stringr::str_remove(hit, "^\\S+ "),
      eval = eval
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      target = as.character(ref_seqs[ref_ids == seqid])[1]
    ) |>
    dplyr::mutate(
      pctid = compare_aa(query, target, "pctId"),
      similarity = compare_aa(query, target, "similarity"),
      gap_leading = count_end_gaps(query, target, "leading"),
      gap_trailing = count_end_gaps(query, target, "trailing"),
      .after = "eval"
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-seqid) |>
    dplyr::arrange(dplyr::desc(similarity)) |>
    # Keep the top hits per candidate gene (not just overall) so a later ORF
    # gene assignment can restrict the alignment to the assigned gene's hits
    # without re-running BLAST. The single combined-DB search thus covers every
    # per-gene DB at once.
    dplyr::group_by(gene) |>
    dplyr::slice_head(n = as.numeric(max_blast_hits)) |>
    dplyr::ungroup()
}

#' Pairwise comparison of nucleotide sequences
#'
#' Nucleotide analog of [compare_aa()] for rRNA (which does not translate). Uses
#' a simple match/mismatch nucleotide substitution matrix.
#'
#' @param query,target nucleotide sequences (character)
#' @param type "pctId" (query-centric) or "similarity" (over aligned columns)
#' @noRd
compare_nt <- function(query, target, type = c("pctId", "similarity")) {
  s1 <- Biostrings::DNAString(query)
  s2 <- Biostrings::DNAString(target)
  submx <- pwalign::nucleotideSubstitutionMatrix(match = 1, mismatch = -1, baseOnly = FALSE)
  aln <- pwalign::pairwiseAlignment(
    subject = s1, pattern = s2, substitutionMatrix = submx,
    gapOpening = 5, gapExtension = 2
  )
  if (type[1] == "similarity") {
    return(100 * pwalign::nmatch(aln) / pwalign::nchar(aln))
  }
  100 * pwalign::nmatch(aln) / nchar(query)
}

#' Get top BLASTN hits for a nucleotide query against a per-gene reference DB
#'
#' Nucleotide analog of [get_top_hits()] for rRNA features. The reference DB is a
#' featureNuc/<gene>.fas BLAST database with the same header contract
#' (`>{accession}:{gene}-1-1-{len} {Species}`). Returns the same columns as
#' [get_top_hits()] so the refHits JSON / annotate-details alignment are shared.
#'
#' @param ref_db nucleotide BLAST database (FASTA with a makeblastdb index)
#' @param query nucleotide query sequence
#' @param max_blast_hits maximum hits to retain (default 10)
#' @param condaenv conda env with blastn (NULL = on PATH)
#' @noRd
get_top_hits_nuc <- function(
    ref_db,
    query,
    max_blast_hits = 10,
    condaenv = "base") {
  ref_seqs <- Biostrings::readDNAStringSet(ref_db)

  if (!is.null(condaenv)) {
    hits_refSeq <- stringr::str_glue(
      "run -n {condaenv}",
      "echo -e '{query}' |",
      "blastn ",
      "-task blastn",
      "-db {ref_db}",
      "-max_hsps 1",
      "-max_target_seqs 1000",
      "-outfmt '6 salltitles evalue'",
      "-query -",
      .sep = " "
    ) |>
      system2(reticulate::conda_binary(), args = _, stdout = TRUE)
  } else {
    hits_refSeq <- stringr::str_glue(
      "-task blastn",
      "-db {ref_db}",
      "-max_hsps 1",
      "-max_target_seqs 1000",
      "-outfmt '6 salltitles evalue'",
      "-query -",
      .sep = " "
    ) |>
      system2("blastn", args = _, input = query, stdout = TRUE)
  }

  empty <- data.frame(
    acc = character(), Taxon = character(), eval = numeric(),
    target = character(), pctid = numeric(), similarity = numeric(),
    gap_leading = numeric(), gap_trailing = numeric()
  )
  if (length(hits_refSeq) == 0) return(empty)

  hits_refSeq |>
    purrr::map_dfr(~ {
      df <- data.frame(stringr::str_split(.x, "\\t", simplify = T))
      colnames(df) <- c("hit", "eval")
      df |> dplyr::mutate(across(!hit, as.numeric))
    }) |>
    dplyr::arrange(eval) |>
    dplyr::transmute(
      acc = stringr::str_extract(hit, "^[^:]+"),
      Taxon = stringr::str_remove(hit, "^\\S+ "),
      eval = eval
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      target = as.character(ref_seqs[stringr::str_extract(names(ref_seqs), "^[^:]+") == acc])[1]
    ) |>
    dplyr::mutate(
      pctid = compare_nt(query, target, "pctId"),
      similarity = compare_nt(query, target, "similarity"),
      gap_leading = NA_real_,
      gap_trailing = NA_real_,
      .after = "eval"
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(dplyr::desc(similarity)) |>
    dplyr::slice_head(n = as.numeric(max_blast_hits))
}

#' Inject synthetic ruleset entries for genes absent from the params `rules`
#'
#' Non-standard MitoFinder genes (and any other gene not listed in a clade's
#' params `rules`) would otherwise be skipped entirely by curation and
#' validation, which only iterate `names(rules)`. Give each such gene a rule
#' that inherits the type's `default_rules` (so start/stop codon, overlap and
#' length checks apply) with `count = c(0, Inf)` so it is never flagged as a
#' missing or duplicated gene. Called by both `curate_mito_core()` and
#' `validate_mito_core()` after the rules/defaults merge.
#'
#' @param rules merged rules list (gene -> rule list)
#' @param annotations annotations data frame (needs `gene`, `type` columns)
#' @param default_rules per-type default rules list from params
#'
#' @return `rules` with synthetic entries added for unknown genes
#'
#' @noRd
augment_rules_for_unknown_genes <- function(rules, annotations, default_rules) {
  if (is.null(annotations) || nrow(annotations) == 0L) return(rules)
  unknown <- setdiff(unique(annotations$gene), names(rules))
  unknown <- unknown[!is.na(unknown) & nzchar(unknown)]
  for (g in unknown) {
    g_type <- annotations$type[annotations$gene == g][1]
    base <- default_rules[[g_type]] %||% default_rules[["PCG"]] %||% list()
    rules[[g]] <- utils::modifyList(base, list(type = g_type, count = c(0, Inf)))
  }
  rules
}

#' Count end gaps in a pairwise alignment
#'
#' @param query The focal sequence
#' @param target The target sequence
#' @param end The end to count gaps. Options are 'leading' and 'trailing'
#' @param subMx The substitution matrix to use for the comparison. Default is BLOSUM80
#'
#' @noRd
#'
count_end_gaps <- function(query, target, end = c("leading", "trailing"), subMx = "BLOSUM80") {
  end <- end[1]
  s1 <- Biostrings::AAString(query)
  s2 <- Biostrings::AAString(target)
  #aln <- pwalign::pairwiseAlignment(subject = s1, pattern = s2, substitutionMatrix = subMx)
  # MSA algorithm seems to do better than pwalign
  seqs <- Biostrings::AAStringSet(list(s1, s2))
  aln <- DECIPHER::AlignSeqs(seqs, verbose = FALSE)
  if (end == "leading") {
    return(
      nchar(stringr::str_extract(as.character(aln[1]), "^-*")) -
        nchar(stringr::str_extract(as.character(aln[2]), "^-*"))
    )
  }
  if (end == "trailing") {
    return(
      nchar(stringr::str_extract(as.character(aln[1]), "-*$")) -
        nchar(stringr::str_extract(as.character(aln[2]), "-*$"))
    )
  }
}

#' Guess an ORF's mitochondrial gene from its stored BLAST hits
#'
#' Reads the `refHits` JSON of an ORF annotation (as produced by
#' [get_top_hits_orf()]), restricts candidates to standard PCG genes, and
#' returns the single best hit by `similarity` if it clears `threshold`.
#'
#' @param refHits_json the ORF's `refHits` value (JSON string, may be NA/empty)
#' @param threshold minimum similarity (%) to return a guess
#'
#' @return `list(gene, similarity, taxon)` for the best candidate, or `NULL`
#'   when there are no usable hits or none clear the threshold.
#' @noRd
guess_orf_gene <- function(refHits_json, threshold = ORF_ASSIGN_SIM_THRESHOLD) {
  hits <- json_parse(refHits_json, TRUE)
  if (!is.data.frame(hits) || nrow(hits) == 0L) {
    return(NULL)
  }
  if (!all(c("gene", "similarity") %in% names(hits))) {
    return(NULL)
  }
  hits <- hits[hits$gene %in% MITO_PCG_GENES & !is.na(hits$similarity), , drop = FALSE]
  if (nrow(hits) == 0L) {
    return(NULL)
  }
  best <- hits[which.max(hits$similarity), , drop = FALSE]
  if (best$similarity < threshold) {
    return(NULL)
  }
  list(
    gene = best$gene,
    similarity = as.numeric(best$similarity),
    taxon = if ("Taxon" %in% names(hits)) best$Taxon else NA_character_
  )
}

#' Next unused join-group id for a set of annotations
#'
#' Join groups are tagged in the `notes` field with a `JOIN: mode=... group=N`
#' marker, so the next id is one past the largest `group=` already present.
#'
#' @param notes the annotation `notes` column (may be NA, empty, or an all-NA
#'   logical vector when the database column holds only NULLs).
#'
#' @return integer group id, 1 when no join markers are present.
#' @noRd
next_join_group <- function(notes) {
  grps <- as.integer(
    stringr::str_match(
      dplyr::coalesce(as.character(notes), ""),
      "^JOIN: mode=\\w+ group=(\\d+)"
    )[, 2]
  )
  if (all(is.na(grps))) 1L else max(grps, na.rm = TRUE) + 1L
}

# Biostrings::translate() warns "last base(s) was ignored" whenever a CDS length
# is not a multiple of 3, which is expected for partial genes and poly-A stops.
# Mute only that message; any other warning still surfaces.
mute_partial_codon_warning <- function(expr) {
  withCallingHandlers(expr, warning = function(w) {
    if (grepl("last (\\d+ )?bases? (was|were) ignored", conditionMessage(w))) {
      invokeRestart("muffleWarning")
    }
  })
}

#' Splice a joined gene's segments into one CDS
#'
#' Concatenates a join group's segment (exon) sequences in 5'->3' order and
#' translates them, matching the export-time splice logic so the manual editor
#' and the exported feature table agree. Used both by [export()] and by the
#' annotate-details editor's spliced-CDS preview / alignment.
#'
#' @param members data frame of the group's annotation rows. Requires columns
#'   pos1, pos2, direction, start_codon, stop_codon, partial_start, partial_stop.
#' @param seq the scaffold assembly sequence (DNAString/DNAStringSet) the members
#'   belong to.
#' @param genetic_code a resolved Biostrings genetic-code vector (e.g.
#'   `session$userData$gcode` or `Biostrings::getGeneticCode("2")`).
#'
#' @return list with `dna`, `translation` (terminal stop stripped), `start_codon`,
#'   `stop_codon`, `partial_start`, `partial_stop`, `pos1`, `pos2`, `length`,
#'   `direction`, and `segments`: a data frame (one row per exon, ordered 5'->3')
#'   with `member_row` (index into `members`), `pos1`, `pos2`, `aa_start`, `aa_end`.
#' @keywords internal
splice_join_cds <- function(members, seq, genetic_code) {
  members$.__row <- seq_len(nrow(members))
  exons <- members[order(members$pos1), ]            # ascending coordinate
  direction <- exons$direction[1]
  n <- nrow(exons)

  # Single DNAString + contig length, for circular-origin-aware extraction.
  s <- if (methods::is(seq, "XStringSet")) seq[[1]] else seq
  L <- length(s)
  wraps <- exons$pos1 > exons$pos2            # exon crosses the origin (pos1 > pos2)
  if (any(wraps) && n > 1L) {
    # A multi-exon gene straddling the origin has ambiguous 5'->3' ordering; the
    # ascending-pos1 sort may be wrong. Extract each exon correctly but warn.
    warning("splice_join_cds: multi-exon gene crosses the circular origin; exon order may be approximate")
  }
  read_exon <- function(p1, p2) {
    if (p1 <= p2) {
      as.character(Biostrings::subseq(s, start = p1, end = p2))
    } else {
      paste0(
        as.character(Biostrings::subseq(s, start = p1, end = L)),
        as.character(Biostrings::subseq(s, start = 1L, end = p2))
      )
    }
  }
  exon_seqs <- vapply(seq_len(n), function(i) {
    read_exon(exons$pos1[i], exons$pos2[i])
  }, character(1))
  merged <- Biostrings::DNAStringSet(paste(exon_seqs, collapse = ""))
  if (direction == "-") merged <- Biostrings::reverseComplement(merged)

  translation <- mute_partial_codon_warning(
    Biostrings::translate(merged, genetic.code = genetic_code,
                          if.fuzzy.codon = "solve")
  ) |>
    as.character()
  translation <- sub("\\*$", "", translation)
  aa_len <- nchar(translation)

  # biological 5'->3' order: + strand ascending pos1, - strand descending
  fivep <- if (direction == "-") rev(seq_len(n)) else seq_len(n)
  exons_53 <- exons[fivep, ]
  seg_len <- mapply(circ_len, exons_53$pos1, exons_53$pos2, MoreArgs = list(L = L))
  cum_end <- cumsum(seg_len)
  cum_start <- cum_end - seg_len + 1
  segments <- data.frame(
    member_row = exons_53$.__row,
    pos1 = exons_53$pos1,
    pos2 = exons_53$pos2,
    aa_start = pmin(floor((cum_start - 1) / 3) + 1, aa_len),
    aa_end = pmin(ceiling(cum_end / 3), aa_len),
    stringsAsFactors = FALSE
  )

  if (direction == "-") {
    start_codon <- exons$start_codon[n]
    stop_codon <- exons$stop_codon[1]
    partial_start <- exons$partial_start[n]
    partial_stop <- exons$partial_stop[1]
  } else {
    start_codon <- exons$start_codon[1]
    stop_codon <- exons$stop_codon[n]
    partial_start <- exons$partial_start[1]
    partial_stop <- exons$partial_stop[n]
  }

  # Gene span. min/max is right while every exon sits on one arc, but an exon
  # that crosses the origin is stored pos1 > pos2, and min/max then reports a
  # span that excludes it entirely. Fall back to the shortest circular arc that
  # covers every exon.
  if (any(wraps)) {
    # arc[i, j] = arc from exon i's start round to exon j's end
    arc <- outer(exons$pos1, exons$pos2, Vectorize(circ_len, c("p1", "p2")), L = L)
    k <- which.min(apply(arc, 1, max))
    gene_pos1 <- exons$pos1[k]
    gene_pos2 <- exons$pos2[which.max(arc[k, ])]
  } else {
    gene_pos1 <- min(exons$pos1)
    gene_pos2 <- max(exons$pos2)
  }

  list(
    dna = as.character(merged),
    translation = translation,
    start_codon = start_codon,
    stop_codon = stop_codon,
    partial_start = partial_start,
    partial_stop = partial_stop,
    pos1 = gene_pos1,
    pos2 = gene_pos2,
    length = sum(seg_len),
    direction = direction,
    segments = segments
  )
}

#' Count bases that are not A, C, G or T
#'
#' A sequence can legitimately hold bases that do not translate to a single
#' amino acid: a gap spacer, a consensus built in iupac mode, or an assembly
#' that arrived with ambiguity codes already in it.
#'
#' @param seq a sequence (character or XString)
#'
#' @return integer count, 0 for an empty or missing sequence
#'
#' @noRd
ambiguous_base_count <- function(seq) {
  s <- toupper(as.character(seq))
  if (length(s) != 1L || is.na(s) || !nzchar(s)) {
    return(0L)
  }
  nchar(gsub("[ACGT]", "", s))
}
