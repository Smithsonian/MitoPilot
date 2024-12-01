#' Title
#'
#' @param assembly text string of the assembly to annotate
#' @param rotate should the assembly be rotated? default = TRUE
#' @param trnaScan_opts command line options for tRNAscan-SE (defatult = `-M vert`)
#' @param cpus number of cpus to use (default = 4)
#' @param out output file name
#'
#' @export
#'
annotate_trnaScan <- function(
    assembly = NULL,
    rotate = TRUE,
    trnaScan_opts = "-M vert",
    cpus = 4,
    out = NULL,
    condaenv = "base") {
  out <- out %||% tempfile()

  fasta <- tempfile(fileext = ".fa")
  Biostrings::writeXStringSet(assembly, fasta)

  process_args <- list(
    cmd = "tRNAscan-SE",
    args = stringr::str_glue(
      "{trnaScan_opts} -o {out} --thread {cpus} --forceow --quiet {fasta}"
    )
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

  if (rotate && length(assembly) == 1L) {

    seq_length <- Biostrings::width(assembly)

    # Find start position for rotation
    start <- read.delim(
      out,
      skip = 3,
      header = F,
      col.names = c(
        "seq", "idx", "begin", "end",
        "type", "anticodon", "intron_beign",
        "intron_end", "score", "notes"
      )
    ) |>
      dplyr::filter(type %in% c("Phe", "Val")) |>
      dplyr::arrange(type, desc(score)) |>
      dplyr::select(begin, end) |>
      dplyr::slice(1)

    # Rotate sequence (heavy strand)
    if (nrow(start) == 1 && start$begin < start$end && start$begin > 1) {
      assembly <- Biostrings::xscat(
        Biostrings::subseq(assembly, start$begin, seq_length),
        Biostrings::subseq(assembly, 1, start$begin - 1)
      ) |>
        setNames(names(assembly))
      assembly@metadata["rotate_to"] <- start$begin
    }

    # Rotate sequence (light strand)
    if (nrow(start) == 1 && start$begin > start$end && start$begin < seq_length) {
      assembly <- Biostrings::xscat(
        Biostrings::subseq(assembly, start$begin + 1, seq_length),
        Biostrings::subseq(assembly, 1, start$begin)
      ) |>
        setNames(names(assembly)) |>
        Biostrings::reverseComplement()
      assembly@metadata["rotate_to"] <- -start$begin
    }

    unlink(out)
    Biostrings::writeXStringSet(assembly, fasta)
    do.call(process, process_args)
  }

  # Format output
  annotations <- read.delim(
    out,
    skip = 3,
    header = F,
    col.names = c(
      "seq", "idx", "begin", "end",
      "type", "anticodon", "intron_beign",
      "intron_end", "score", "notes"
    )
  ) |>
    purrr::pmap_dfr(function(...) {
      cur <- list(...)
      data.frame(
        contig = stringr::str_squish(cur$seq),
        type = "tRNA",
        gene = .trnA_key[[cur$type]],
        product = paste("tRNA", cur$type, sep = "-"),
        pos1 = min(c(cur$begin, cur$end)),
        pos2 = max(c(cur$begin, cur$end)),
        direction = ifelse(cur$begin < cur$end, "+", "-"),
        anticodon = cur$anticodon
      ) |>
        dplyr::mutate(length = 1 + abs(pos2 - pos1), .before = "direction")
    })

  return({
    list(
      assembly = assembly,
      annotations = annotations
    )
  })
}

# Key for translating tRNA codes to Amino Acid codes
.trnA_key <- list(
  Phe = "trnF",
  Val = "trnV",
  Leu = "trnL",
  Ile = "trnI",
  Gln = "trnQ",
  Met = "trnM",
  Trp = "trnW",
  Ala = "trnA",
  Asn = "trnN",
  Cys = "trnC",
  Tyr = "trnY",
  Ser = "trnS",
  Asp = "trnD",
  Lys = "trnK",
  Gly = "trnG",
  Arg = "trnR",
  His = "trnH",
  Glu = "trnE",
  Thr = "trnT",
  Pro = "trnP"
)
