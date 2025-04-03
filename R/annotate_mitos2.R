#' Annotate mitochondrial genomes using MITOS2
#'
#' @param assembly a DNAString object
#' @param topology "circular" or "linear"
#' @param genetic_code NCBI genetic code number (default: 2)
#' @param ref_db Mitos2 reference database (default: "Chordata")
#' @param ref_dir Path to Mitos2 reference database
#' @param mitos_opts Additional command line options for MITOS2
#' @param out output directory
#' @param condaenv Conda environment to run MITOS2 (default: "mitos")
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

  # debugging
  out <- "MITOS2_temp"
  dir.create(out)
  ###

  fasta <- tempfile(fileext = ".fa")

  # debugging
  fasta <- "temp_asmb.fa"
  ###

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
    process <- reticulate::conda_run2
    process_args$envname <- condaenv
    process_args$echo <- FALSE
  } else {
    process <- "system2"
  }

  message("starting MITOS2")
  message(paste("MITOS2 out dir:", out))

  do.call(process, process_args)

  message("finished MITOS2")

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
                stringr::str_detect(gene, "^Intron") ~ "intron",
                stringr::str_detect(gene, "^OL") ~ "OL",
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
                type == "PCG" ~ çƒ[gene],
                .default = NA_character_
              ),
              .after = "gene"
            ) |>
            dplyr::mutate(
              length = 1 + abs(pos2 - pos1),
              .before = "direction"
            ) |>
            dplyr::filter(
              gene != "OH" | stringr::str_detect(geneId, "OH_0") | stringr::str_detect(geneId, "OH$")
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

      annotations <- annotations |>
        dplyr::select(-dplyr::any_of('geneId'))

      ###################
      return(annotations)
      ###################

    })
}

# PCG key ----
# CDS_key <- c(
#   nad1 = "NADH dehydrogenase subunit 1",
#   nad2 = "NADH dehydrogenase subunit 2",
#   cox1 = "cytochrome c oxidase subunit 1",
#   cox2 = "cytochrome c oxidase subunit 2",
#   atp8 = "ATP synthase F0 subunit 8",
#   atp6 = "ATP synthase F0 subunit 6",
#   cox3 = "cytochrome c oxidase subunit 3",
#   nad3 = "NADH dehydrogenase subunit 3",
#   nad4l = "NADH dehydrogenase subunit 4L",
#   nad4 = "NADH dehydrogenase subunit 4",
#   nad5 = "NADH dehydrogenase subunit 5",
#   nad6 = "NADH dehydrogenase subunit 6",
#   cob = "cytochrome b"
# )

# PCG key for full metazoan dataset ----
CDS_key <- c(
  nad1 = "NADH dehydrogenase subunit 1",
  nad2 = "NADH dehydrogenase subunit 2",
  cox1 = "cytochrome c oxidase subunit 1",
  cox2 = "cytochrome c oxidase subunit 2",
  cox3 = "cytochrome c oxidase subunit 3",
  atp8 = "ATP synthase F0 subunit 8",
  atp6 = "ATP synthase F0 subunit 6",
  atp9 = "ATP synthase F0 subunit 9",
  cox3 = "cytochrome c oxidase subunit 3",
  nad3 = "NADH dehydrogenase subunit 3",
  nad4l = "NADH dehydrogenase subunit 4L",
  nad4 = "NADH dehydrogenase subunit 4",
  nad5 = "NADH dehydrogenase subunit 5",
  nad6 = "NADH dehydrogenase subunit 6",
  cob = "cytochrome b",
  dpo = "DNA-polymerase",
  lagli = "homing endonuclease",
  msh1 = "MutS mismatch DNA repair protein",
  mttb = "trimethylamine methyltransferase"
)
