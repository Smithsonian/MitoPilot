#' Use tRNAscan-SE to annotate tRNAs in a mitochondrial genome assembly
#'
#' @param assembly a DNAStringSet of the assembly to annotate
#' @param rotate unused; rotation now runs after tRNAscan and MITOS2 complete
#' @param trnaScan_opts command line options for tRNAscan-SE (default = `-M vert -X 20`)
#' @param cpus number of cpus to use (default = 4)
#' @param out output file name
#' @param condaenv conda environment to use (default = "base")
#'
#' @export
#'
annotate_trnaScan <- function(
  assembly = NULL,
  rotate = TRUE,
  trnaScan_opts = "-M vert -X 20",
  cpus = 4,
  out = NULL,
  condaenv = "base"
) {
  # write tRNAscan-SE output into the local work dir so it is retained for inspection
  out <- out %||% "tRNAscan-SE_out"

  fasta <- tempfile(fileext = ".fa")
  Biostrings::writeXStringSet(assembly, fasta)

  process_args <- list(
    cmd = "tRNAscan-SE",
    args = stringr::str_glue(
      "{trnaScan_opts} -o {out} --thread {cpus} --forceow --quiet {fasta}"
    )
  )
  if (!is.null(condaenv)) {
    process <- reticulate::conda_run2
    process_args$envname <- condaenv
    process_args$echo <- FALSE
  } else {
    process <- "system2"
  }

  do.call(process, process_args)

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
        gene = .trnA_key[[cur$type]] %||% NA_character_,
        product = paste("tRNA", cur$type, sep = "-"),
        pos1 = min(c(cur$begin, cur$end)),
        pos2 = max(c(cur$begin, cur$end)),
        direction = ifelse(cur$begin < cur$end, "+", "-"),
        anticodon = cur$anticodon
      ) |>
        dplyr::mutate(length = 1 + abs(pos2 - pos1), .before = "direction") |>
        dplyr::mutate(tRNA_ID = paste0(product, "-", anticodon), .after = "direction") # create temporary ID to compare with MITOS2 results
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
