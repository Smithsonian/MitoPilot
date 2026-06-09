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
  if (p1 <= p2) {
    p1 <= q2_vec & q1_vec <= p2
  } else {
    q2_vec >= p1 | q1_vec <= p2
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
  if (p1 <= p2) {
    max(0L, min(p2, q2) - max(p1, q1) + 1L)
  } else {
    max(0L, q2 - max(p1, q1) + 1L) + # [p1, L] ∩ [q1, q2]
      max(0L, min(p2, q2) - q1 + 1L) # [1, p2] ∩ [q1, q2]
  }
}

#' Get top BLASTP hits
#'
#' @param ref_db reference database
#' @param query query sequeencs
#' @param condaenv Conda environment to use for running blastp
#' @param max_blast_hits Maximum number of top BLAST hits to retain (default = 100)
#'
#' @noRd
#'
get_top_hits <- function(
    ref_db,
    query,
    max_blast_hits = 100,
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

#' Get top BLASTP hits for an ORF against a combined gene database
#'
#' Like [get_top_hits()], but BLASTs a query against a single combined protein
#' database holding sequences from every gene (built by concatenating the
#' per-gene `featureProt/*.fas` files). Because the gene of an ORF is unknown,
#' the gene name is recovered per hit from the reference header format
#' `>{accession}:{gene}-{idx}-{pos1}-{pos2} {species}` and returned in a `gene`
#' column.
#'
#' @param ref_db combined reference database (FASTA with a makeblastdb index)
#' @param query query (amino acid) sequence
#' @param max_blast_hits Maximum number of top BLAST hits to retain (default = 100)
#' @param condaenv Conda environment to use for running blastp (NULL = on PATH)
#'
#' @noRd
#'
get_top_hits_orf <- function(
    ref_db,
    query,
    max_blast_hits = 100,
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
    dplyr::slice_head(n = as.numeric(max_blast_hits))
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
