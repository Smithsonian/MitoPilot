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
  if(gene %nin% annotations){
    return(list(assembly, annotations))
  }

  # Find start position for rotation
  start <- dplyr::filter(annotations, gene %in% start_gene) |>
    dplyr::select(pos1, pos2, direction) |>
    dplyr::slice(1)

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
