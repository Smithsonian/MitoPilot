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
  #data(list=subMx, package = "pwalign")
  alignment <- pwalign::pairwiseAlignment(subject = s1, pattern = s2, substitutionMatrix = subMx)

  # Return query-centric percent identity
  if (type[1] == "pctId") {
    return(100 * pwalign::nmatch(alignment) / nchar(query))
  }

  if (type[1] == "similarity") {
    data(list=subMx, package = "pwalign")
    max_score <- sum(diag(BLOSUM80)[match(strsplit(query, NULL)[[1]], rownames(BLOSUM80))])
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

#' Get top BLASTP hits
#'
#' @param ref_db reference database
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
  ref_seqs <- Biostrings::readAAStringSet(ref_db)

  if (!is.null(condaenv)) {
    hits_refSeq <- stringr::str_glue(
      "run -n {condaenv}",
      "echo -e '{query}' |",
      "blastp ",
      "-db {ref_db}",
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
      "-db {ref_db}",
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
      acc = stringr::str_extract(hit, "^[^:]+"),
      Taxon = stringr::str_remove(hit, "^\\S+ "),
      eval = eval
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      target = as.character(ref_seqs[stringr::str_extract(names(ref_seqs), "^[^:]+") == acc])[1]
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
#' @param feature_dir directory of per-gene `featureProt/*.fas` files
#' @param out_fasta path to write the combined FASTA (index built alongside)
#' @param condaenv conda env with makeblastdb (NULL = on PATH)
#'
#' @return `out_fasta` on success, or NULL when no per-gene files are present
#'
#' @noRd
build_combined_orf_db <- function(feature_dir, out_fasta, condaenv = "base") {
  fas <- list.files(feature_dir, pattern = "\\.fas$", full.names = TRUE)
  if (length(fas) == 0) return(NULL)
  all_seqs <- Biostrings::AAStringSet()
  for (f in fas) {
    gene <- sub("\\.fas$", "", basename(f))
    s <- tryCatch(Biostrings::readAAStringSet(f), error = function(e) NULL)
    if (is.null(s) || length(s) == 0) next
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
    return({
      nchar(stringr::str_extract(as.character(aln[1]), "^-*")) -
        nchar(stringr::str_extract(as.character(aln[2]), "^-*"))
      # old code for pwalign::pairwiseAlignment() results
      #nchar(stringr::str_extract(as.character(pwalign::alignedSubject(aln)), "^-*")) -
      #  nchar(stringr::str_extract(as.character(pwalign::alignedPattern(aln)), "^-*"))
    })
  }
  if (end == "trailing") {
    return({
      nchar(stringr::str_extract(as.character(aln[1]), "-*$")) -
        nchar(stringr::str_extract(as.character(aln[2]), "-*$"))
      # old code for pwalign::pairwiseAlignment() results
      #nchar(stringr::str_extract(as.character(pwalign::alignedSubject(aln)), "-*$")) -
      #  nchar(stringr::str_extract(as.character(pwalign::alignedPattern(aln)), "-*$"))
    })
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
