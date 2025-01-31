#' Mitogenome Annotation Wrapper
#'
#' Uses Mito2 and tRNAscan-SE to annotate a mitogenome assembly.
#'
#' @param assembly_fn Path to the mitogenome assembly FASTA file.
#' @param coverage_fn Path to the mitogenome assembly coverage stats CSV file.
#' @param cpus Number of CPUs to use.
#' @param genetic_code Genetic code to use for annotation (default: 2).
#' @param ref_db Reference Mitos2 database to use for annotation (default:
#'   "Chordata").
#' @param ref_dir Path to the Mitos2 reference database.
#' @param mitos_opts Additional command line options for MITOS2.
#' @param mitos_condaenv Conda environment to run MITOS2 (default: "mitos").
#' @param trnaScan_opts Additional command line options for tRNAscan-SE.
#' @param trnaScan_condaenv Conda environment to run tRNAscan-SE (default:
#'   "base").
#' @param out_dir Output directory.
#'
#' @export
#'
annotate <- function(
    assembly_fn = NULL,
    coverage_fn = NULL,
    cpus = 4,
    genetic_code = "2",
    ref_db = "Chordata",
    ref_dir = "/home/harpua/Jonah/MitoPilot/ref_dbs/Mitos2",
    mitos_opts = "--intron 0 --oril 0 --trna 0",
    mitos_condaenv = "mitos",
    trnaScan_opts = "-M vert",
    trnaScan_condaenv = "base",
    out_dir = NULL) {

  assembly <- Biostrings::readDNAStringSet(assembly_fn)

  # Coverage trimming (optional)
  # TODO? Move coverage trimming opt to dynamic params
  if (length(coverage_fn) == 1L && file.exists(coverage_fn)) {
    coverage <- read.csv(coverage_fn) |>
      dplyr::arrange(SeqId, Position) |>
      dplyr::mutate(
        mask = stringr::str_detect(MeanDepth, "^#") |
          stringr::str_detect(ErrorRate, "^#"),
        MeanDepth = as.numeric(stringr::str_remove(MeanDepth, "^#")),
        ErrorRate = as.numeric(stringr::str_remove(ErrorRate, "^#"))
      )
    coverage_trimmed <- assembly |> purrr::imap(~ {
      stats <- coverage[coverage$SeqId == stringr::str_extract(.y, "\\S+"), ]
      # Skip for circular assemblies
      if (stringr::str_detect("circular", .y)) {
        return({
          list(
            assembly = .x,
            stats = stats
          )
        })
      }
      # Run coverage trimming
      coverage_trim(assembly = .x, stats = stats)
    })
    assembly <- purrr::map(coverage_trimmed, ~ .x$assembly) |> Biostrings::DNAStringSet()
    coverage <- purrr::map(coverage_trimmed, ~ .x$stats) |> dplyr::bind_rows()
  }

  # tRNA annotation ----
  trnaScan_out <- annotate_trnaScan(
    assembly = assembly,
    rotate = stringr::str_detect(names(assembly), "circular"),
    trnaScan_opts = trnaScan_opts,
    cpus = cpus,
    condaenv = trnaScan_condaenv
  )
  assembly <- trnaScan_out$assembly
  annotations_trnaScan <- trnaScan_out$annotations

  # Update coverage if rotated ----
  rotate <- assembly@metadata[["rotate_to"]]
  if (!is.null(rotate) && rotate > 0) {
    coverage <- dplyr::bind_rows(
      coverage[rotate:nrow(coverage), ],
      coverage[1:(rotate - 1), ]
    ) |>
      dplyr::mutate(
        Position = dplyr::row_number()
      )
  }
  if (!is.null(rotate) && rotate < 0) {
    coverage <- dplyr::bind_rows(
      coverage[abs(rotate):1, ],
      coverage[nrow(coverage):(abs(rotate) + 1), ]
    ) |>
      dplyr::mutate(
        Position = dplyr::row_number(),
        Call = as.character(assembly) |> stringr::str_split("") |> unlist()
      )
  }

  # Mitos2 annotation ----
  annotations_mitos <- annotate_mitos2(
    assembly = assembly,
    topology = ifelse(all(stringr::str_detect(names(assembly), "circular")), "circular", "linear"),
    genetic_code = genetic_code,
    ref_db = ref_db,
    ref_dir = ref_dir,
    mitos_opts = mitos_opts,
    condaenv = mitos_condaenv
  )

  # Combine annotations ----
  annotations <- dplyr::bind_rows(
    annotations_trnaScan,
    annotations_mitos
  ) |>
    dplyr::arrange(contig, pos1)

  ## Fix D-loop annotations ----
  # Filter spurious OH annotations
  oh_idx <- which(annotations$gene == "OH") |> rev()
  to_remove <- NULL
  for (idx in oh_idx) {
    # Check if overlapping other gene
    containing <- annotations |>
      dplyr::filter(!idx) |>
      dplyr::filter(pos1 >= annotations$pos1[idx] | pos2 <= annotations$pos2[idx])
    if (nrow(containing) > 0L) {
      to_remove <- c(to_remove, idx)
    }
  }
  if(length(to_remove) > 0) {
    annotations <- annotations[-to_remove, ]
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
      annotations$pos2[idx] <- assembly[stringr::str_detect(names(assembly), paste(annotations$contig[idx], "\\w.*"))]@ranges@width
    } else {
      annotations$pos2[idx] <- annotations$pos1[idx + 1] - 1
    }
    annotations$length[idx] <- abs(annotations$pos2[idx] - annotations$pos1[idx]) + 1
    annotations$gene[idx] <- "ctrl"
  }

  # Write outputs
  file.path(
    out_dir,
    stringr::str_replace(basename(assembly_fn), "\\w+$", "csv") |>
      stringr::str_replace("assembly", "annotations")
  ) |>
    readr::write_csv(annotations, file = _, na = "")
  Biostrings::writeXStringSet(assembly, file.path(out_dir, basename(assembly_fn)))
  if (!is.null(coverage_fn)) {
    file.path(
      out_dir,
      stringr::str_replace(basename(assembly_fn), "\\w+$", "csv") |>
        stringr::str_replace("assembly", "coverageStats")
    ) |>
      readr::write_csv(coverage, file = _, quote = "none", na = "")
  }

  return(invisible(annotations))
}
