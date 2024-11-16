#' Title
#'
#' @param assembly
#' @param topology
#' @param genetic_code
#' @param ref_db
#' @param ref_dir
#' @param mitos_opts
#' @param out
#' @param condaenv
#'
#' @export
#'
annotate_mitos2 <- function(
    assembly = NULL,
    topology = "circular",
    genetic_code = "2",
    ref_db = "Chordata",
    ref_dir = "/home/harpua/Jonah/MitoPilot/ref_dbs/Mitos2",
    mitos_opts = "--best --intron 0 --oril 0 --trna 0",
    out = NULL,
    condaenv = "mitos") {
  genetic_code <- as.character(genetic_code)
  out <- out %||% tempdir()

  fasta <- tempfile(fileext = ".fa")
  names(assembly) <- stringr::str_extract(names(assembly), "^\\S+")
  Biostrings::writeXStringSet(assembly, fasta)

  process_args <- list(
    cmd = "runmitos.py",
    args = stringr::str_glue(
      "--input {fasta}",
      "--outdir {out}",
      "--code {genetic_code}",
      "--refseqver {ref_db}",
      "--refdir {ref_dir}",
      "{ifelse(topology != 'circular', '--linear', '')}",
      "{mitos_opts}",
      "--noplots",
      .sep = " "
    ) |>
      stringr::str_squish()
  )
  if (!is.null(condaenv)) {
    require(reticulate)
    process <- "conda_run2"
    process_args$envname <- condaenv
    process_args$echo <- FALSE
  } else {
    process <- "system2"
  }

  do.call(process, process_args)

  # Format Mitos Output ----
  annotations_mitos <- list.files(out, recursive = T, full.names = T, pattern = "result.fas") |>
    purrr::map_dfr(~ {
      annotations <- Biostrings::readDNAStringSet(.x) |>
        {
          \(x)
          data.frame(
            contig = stringr::str_extract(names(x), "^(.*?)(?=;)"),
            gene = stringr::str_extract(stringr::str_extract(names(x), "\\S+$"), "^[a-zA-Z0-9]+"),
            geneId = stringr::str_extract(names(x), "\\S+$"),
            pos1 = stringr::str_split(names(x), " *; *", simplify = T)[, 2] |>
              stringr::str_extract("^[0-9]+") |> as.numeric(),
            pos2 = stringr::str_split(names(x), " *; *", simplify = T)[, 2] |>
              stringr::str_extract("[0-9]+$") |> as.numeric(),
            direction = stringr::str_split(names(x), " *; *", simplify = T)[, 3],
            row.names = NULL
          ) |>
            dplyr::mutate(
              type = dplyr::case_when(
                stringr::str_detect(gene, "^rrn[L|S]") ~ "rRNA",
                stringr::str_detect(gene, "^OH") ~ "ctrl",
                .default = "PCG"
              ),
              .before = "gene"
            ) |>
            dplyr::mutate(
              product = dplyr::case_when(
                stringr::str_detect(gene, "rrnL") ~ "16S ribosomal RNA",
                stringr::str_detect(gene, "rrnS") ~ "12S ribosomal RNA",
                stringr::str_detect(gene, "OH") ~ "d-loop",
                type == "PCG" ~ CDS_key[gene],
                .default = NA_character_
              ),
              .after = "gene"
            ) |>
            dplyr::mutate(
              length = 1 + abs(pos2 - pos1),
              .before = "direction"
            ) |>
            dplyr::rowwise() |>
            dplyr::mutate(
              start_codon = dplyr::case_when(
                type != "PCG" ~ NA_character_,
                direction == "+" ~ Biostrings::subseq(assembly[contig], pos1, pos1 + 2) |> as.character(),
                direction == "-" ~ Biostrings::subseq(assembly[contig], pos2 - 2, pos2) |>
                  Biostrings::reverseComplement() |> as.character()
              ),
              stop_codon = dplyr::case_when(
                type != "PCG" ~ NA_character_,
                direction == "+" ~ {
                  len <- dplyr::if_else(length %% 3 == 0L, 3, length %% 3)
                  Biostrings::subseq(assembly[contig], pos2 - len + 1, pos2) |>
                    as.character()
                },
                direction == "-" ~ {
                  len <- dplyr::if_else(length %% 3 == 0L, 3, length %% 3)
                  Biostrings::subseq(assembly[contig], pos1, pos1 + len - 1) |>
                    Biostrings::reverseComplement() |>
                    as.character()
                }
              ),
              translation = dplyr::case_when(
                type == "PCG" && direction == "+" ~ suppressWarnings({
                  Biostrings::subseq(assembly[contig], pos1, pos2 - nchar(stop_codon)) |>
                    Biostrings::translate(genetic.code = Biostrings::getGeneticCode(genetic_code)) |>
                    as.character()
                }),
                type == "PCG" && direction == "-" ~ suppressWarnings({
                  Biostrings::subseq(assembly[contig], pos1 + nchar(stop_codon), pos2) |>
                    Biostrings::reverseComplement() |>
                    Biostrings::translate(genetic.code = Biostrings::getGeneticCode(genetic_code)) |>
                    as.character()
                }),
                .default = NA_character_
              )
            )
        }()

      # Filter spurious OH annotations
      if (sum(annotations$gene == "OH") > 1) {
        oh_idx <- which(annotations$gene == "OH") |> rev()
        for (idx in oh_idx) {
          # Check if contained in other gene
          containing <- annotations |>
            dplyr::filter(!idx) |>
            dplyr::filter(pos1 >= annotations$pos1[idx] & pos2 <= annotations$pos2[idx])
          if (nrow(containing) > 0L) {
            annotations <- annotations[-idx, ]
            next
          }
          # remove if not best
          if (stringr::str_detect(annotations$geneId[idx], "OH_0") | stringr::str_detect(annotations$geneId[idx], "OH$")) next
          annotations <- annotations[-idx, ]
        }
      }
      # Extend OH annotations to (putative) full length ctrl region
      oh_idx <- which(annotations$gene == "OH")
      for (idx in oh_idx) {
        if (idx == min(which(annotations$contig == annotations$contig[idx]))) {
          annotations$pos1[idx] <- 1
        } else {
          annotations$pos1[idx] <- annotations$pos2[idx - 1] + 1
        }

        if (idx == max(which(annotations$contig == annotations$contig[idx]))) {
          annotations$pos2[idx] <- assembly[annotations$contig[idx]]@ranges@width
        } else {
          annotations$pos2[idx] <- annotations$pos1[idx + 1] - 1
        }
        annotations$length[idx] <- abs(annotations$pos2[idx] - annotations$pos1[idx]) + 1
        annotations$gene[idx] <- "ctrl"
      }

      ###################
      return(annotations)
      ###################
    })
}

# PCG key ----
CDS_key <- c(
  nad1 = "NADH dehydrogenase subunit 1",
  nad2 = "NADH dehydrogenase subunit 2",
  cox1 = "cytochrome c oxidase subunit 1",
  cox2 = "cytochrome c oxidase subunit 2",
  atp8 = "ATP synthase F0 subunit 8",
  atp6 = "ATP synthase F0 subunit 6",
  cox3 = "cytochrome c oxidase subunit 3",
  nad3 = "NADH dehydrogenase subunit 3",
  nad4l = "NADH dehydrogenase subunit 4L",
  nad4 = "NADH dehydrogenase subunit 4",
  nad5 = "NADH dehydrogenase subunit 5",
  nad6 = "NADH dehydrogenase subunit 6",
  cob = "cytochrome b"
)
