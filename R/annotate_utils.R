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
    data(list=subMx, package = "pwalign")
    max_score <- sum(diag(BLOSUM80)[match(strsplit(query, NULL)[[1]], rownames(BLOSUM80))])
    res <- 100 * BiocGenerics::score(alignment) / max_score
    return(res)
  }
  return(alignment)
}

#' Get top BLASTP hits
#'
#' @param ref_db reference database
#' @param query query sequeencs
#' @param condaenv Conda environment to use for running blastp
#'
#' @noRd
#'
get_top_hits <- function(
    ref_db,
    query,
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
    dplyr::arrange(dplyr::desc(similarity))
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
  aln <- pwalign::pairwiseAlignment(subject = s1, pattern = s2, substitutionMatrix = subMx)
  if (end == "leading") {
    return({
      nchar(stringr::str_extract(as.character(pwalign::alignedSubject(aln)), "^-*")) -
        nchar(stringr::str_extract(as.character(pwalign::alignedPattern(aln)), "^-*"))
    })
  }
  if (end == "trailing") {
    return({
      nchar(stringr::str_extract(as.character(pwalign::alignedSubject(aln)), "-*$")) -
        nchar(stringr::str_extract(as.character(pwalign::alignedPattern(aln)), "-*$"))
    })
  }
}
