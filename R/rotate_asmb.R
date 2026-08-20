#' Rotate circular mitogenome to start at given gene
#'
#' @param assembly Biostring containing mitogenome assembly
#' @param annotations tibble containing mitogenome annotation, produced by `annotate.R`
#' @param start_gene name of gene (PCG, rRNA, or tRNA) to start circular assembly (default = "trnF")
#'
#' @export
#'
rotate_asmb <- function(
    assembly = NULL,
    annotations = NULL,
    start_gene = "trnF"
){
  # assembly length
  seq_length <- Biostrings::width(assembly)

  # check if gene exists in annotation
  # if not return unaltered assembly and annotation
  if(start_gene %nin% annotations$gene){
    warning(
      "rotate_asmb: start gene '", start_gene, "' is not annotated on this ",
      "assembly, so it was not rotated. Any feature spanning the current ",
      "origin stays split across it. Set a different start_gene (one of: ",
      paste(utils::head(unique(annotations$gene), 10), collapse = ", "),
      if (length(unique(annotations$gene)) > 10) ", ..." else "", ").",
      call. = FALSE
    )
    return(list(assembly, annotations))
  }

  # Find start position for rotation
  start <- dplyr::filter(annotations, gene %in% start_gene) |>
    dplyr::select(pos1, pos2, direction) |>
    dplyr::slice(1)

  # Warn if the new origin falls inside another feature: rotating there splits
  # that feature across the origin, which every downstream consumer then has to
  # handle as a wrap-around (pos1 > pos2) annotation.
  cut <- if (start$direction == "+") start$pos1 else start$pos2 + 1L
  cut <- ((cut - 1L) %% seq_length) + 1L
  before_cut <- ((cut - 2L) %% seq_length) + 1L
  split <- circ_overlap(cut, cut, annotations$pos1, annotations$pos2) &
    circ_overlap(before_cut, before_cut, annotations$pos1, annotations$pos2)
  if (any(split, na.rm = TRUE)) {
    warning(
      "rotate_asmb: rotating to '", start_gene, "' cuts through ",
      paste(unique(annotations$gene[which(split)]), collapse = ", "),
      ", which will span the origin of the rotated assembly.",
      call. = FALSE
    )
  }

  # Rotate sequence
  if (start$direction == "+") {
    assembly <- Biostrings::xscat(
      Biostrings::subseq(assembly, start$pos1, seq_length),
      Biostrings::subseq(assembly, 1, start$pos1 - 1)
    ) |>
      setNames(names(assembly))
    assembly@metadata["rotate_to"] <- start$pos1
  }

  # Rotate sequence and reverse complement if start_gene is on negative strand
  if (start$direction == "-") {
    assembly <- Biostrings::xscat(
      Biostrings::subseq(assembly, start$pos2 + 1, seq_length),
      Biostrings::subseq(assembly, 1, start$pos2)
    ) |>
      setNames(names(assembly)) |>
      Biostrings::reverseComplement()
    assembly@metadata["rotate_to"] <- -start$pos2
  }

  # update annotation table to reflect rotation
  if (start$direction == "+"){
    for(i in 1:nrow(annotations)){
      # logic to handle wrap around
      annotations$pos1[i] <- if((annotations$pos1[i] - start$pos1 + 1) > 0){
        annotations$pos1[i] - start$pos1 + 1
      } else {
        annotations$pos1[i] - start$pos1 + 1 + seq_length
      }
      annotations$pos2[i] <- if((annotations$pos2[i] - start$pos1 + 1) > 0){
        annotations$pos2[i] - start$pos1 + 1
      } else {
        annotations$pos2[i] - start$pos1 + 1 + seq_length
      }
    }
  }
  else { # if gene is neg strand, need to rc annotation
    for(i in 1:nrow(annotations)){
      # logic to handle wrap around
      old_pos1 <- annotations$pos1[i]
      old_pos2 <- annotations$pos2[i]
      annotations$pos1[i] <- if((start$pos2 - old_pos2 + 1)  > 0){
        start$pos2 - old_pos2 + 1
      } else {
        start$pos2 - old_pos2 + 1 + seq_length
      }
      annotations$pos2[i] <- if((start$pos2 - old_pos1 + 1) > 0){
        start$pos2 - old_pos1 + 1
      } else {
        start$pos2 - old_pos1 + 1 + seq_length
      }
      # update direction too
      if(annotations$direction[i] == "+"){
        annotations$direction[i] <- "-"
      } else {
        annotations$direction[i] <- "+"
      }
    }
  }

  # return rotated assembly and annotation table
  return(list(assembly, annotations))
}
