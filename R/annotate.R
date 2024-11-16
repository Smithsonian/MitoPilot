#' Title
#'
#' @param assembly
#' @param coverage
#' @param cpus
#' @param memory
#' @param genetic_code
#' @param ref_db
#' @param ref_dir
#' @param mitos_condaenv
#' @param trnaScan
#' @param trnaScan_condaenv
#' @param out
#'
#' @export
#'
annotate <- function(
    assembly_fn = "/home/harpua/Jonah/MitoPilot-test/out/SRR22396740/assemble/default/SRR22396740_assembly_1.fasta",
    coverage_fn = "/home/harpua/Jonah/MitoPilot-test/out/SRR22396740/assemble/default/SRR22396740_assembly_1_coverageStats.csv",
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
    rotate = TRUE,
    trnaScan_opts = trnaScan_opts,
    cpus = cpus,
    condaenv = trnaScan_condaenv
  )
  assembly <- trnaScan_out$assembly
  annotations_trnaScan <- trnaScan_out$annotations

  # Mitos2 annotation ----
  annotations_mitos <- annotate_mitos2(
    assembly = assembly,
    topology = ifelse(all(stringr::str_detect("circular", names(assembly))), "circular", "linear"),
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

  # Write outputs
  file.path(
    out_dir,
    stringr::str_replace(basename(assembly_fn), "\\w+$", "csv") |>
      stringr::str_replace("assembly", "annotations")
  ) |>
    write.csv(annotations, file = _, row.names = F)
  Biostrings::writeXStringSet(assembly, file.path(out_dir, basename(assembly_fn)))
  if (!is.null(coverage_fn)) {
    file.path(
      out_dir,
      stringr::str_replace(basename(assembly_fn), "\\w+$", "csv") |>
        stringr::str_replace("assembly", "coverageStats")
    ) |>
      write.csv(coverage, file = _, row.names = F)
  }

  return(invisible(annotations))
}
