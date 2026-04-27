#' Use ARWEN to annotate tRNAs in a mitochondrial genome assembly
#'
#' @param assembly DNAStringSet assembly to annotate
#' @param arwen_opts command line options for ARWEN (default = "-mtx")
#' @param genetic_code NCBI translation table number passed to ARWEN via -gc<N> (default = "2")
#' @param circular logical; TRUE for circular topology (-c), FALSE for linear (-l)
#' @param out output file path
#'
#' @export
#'
annotate_arwen <- function(
    assembly = NULL,
    arwen_opts = "-mtx",
    genetic_code = "2",
    circular = TRUE,
    out = NULL) {

  out <- out %||% "ARWEN.out"

  fasta <- tempfile(fileext = ".fa")
  Biostrings::writeXStringSet(assembly, fasta)
  seq_lens <- setNames(
    Biostrings::width(assembly),
    names(assembly) |> stringr::str_extract("\\S+")
  )

  message("starting ARWEN")
  message(paste("ARWEN output:", out))

  topology_flag <- if (circular) "-c" else "-l"

  process_args <- list(
    command = "arwen",
    args = c(strsplit(arwen_opts, "\\s+")[[1]], "-w", paste0("-gc", genetic_code), topology_flag, fasta),
    stdout = out,
    stderr = FALSE
  )

  do.call("system2", process_args)

  raw <- readLines(out)

  trnA_key <- list(
    Phe = "trnF", Val = "trnV", Leu = "trnL", Ile = "trnI",
    Gln = "trnQ", Met = "trnM", Trp = "trnW", Ala = "trnA",
    Asn = "trnN", Cys = "trnC", Tyr = "trnY", Ser = "trnS",
    Asp = "trnD", Lys = "trnK", Gly = "trnG", Arg = "trnR",
    His = "trnH", Glu = "trnE", Thr = "trnT", Pro = "trnP"
  )

  # Batch mode (-w) format:
  # >seq_name [topology]
  # N genes found
  # N   mtRNA-Type   [c][start,end]   apos   (anticodon)
  tRNA_pattern <- "^\\s*\\d+\\s+mtRNA-(\\S+)\\s+(c?)\\[(\\d+),(\\d+)\\]\\s+\\d+\\s+\\((\\w+)\\)"

  current_seq <- NA_character_
  results <- list()

  for (line in raw) {
    if (startsWith(line, ">")) {
      # Header is ">seq_name [topology]" — take only the sequence name
      current_seq <- stringr::str_extract(line, "(?<=>)\\S+")
      next
    }
    m <- regmatches(line, regexec(tRNA_pattern, line))[[1]]
    if (length(m) == 0) next

    aa_type   <- m[2]
    complement <- m[3]   # "c" = minus strand, "" = plus strand
    start     <- as.integer(m[4])
    end       <- as.integer(m[5])
    anticodon <- m[6]

    aa_clean <- sub("[12]$", "", aa_type)
    if (aa_clean == "iMet") aa_clean <- "Met"
    gene <- trnA_key[[aa_clean]]
    if (is.null(gene)) next

    L <- seq_lens[current_seq]
    gene_len <- if (!is.na(L) && start > end) {
      L - start + end + 1L
    } else {
      1L + abs(end - start)
    }
    results[[length(results) + 1]] <- data.frame(
      contig    = current_seq,
      type      = "tRNA",
      gene      = gene,
      product   = paste("tRNA", aa_clean, sep = "-"),
      pos1      = start,
      pos2      = end,
      length    = gene_len,
      direction = ifelse(complement == "c", "-", "+"),
      tRNA_ID   = paste0("tRNA-", aa_clean, "-", anticodon),
      anticodon = anticodon
    )
  }

  if (length(results) == 0) {
    return(data.frame(
      contig = character(), type = character(), gene = character(),
      product = character(), pos1 = integer(), pos2 = integer(),
      length = integer(), direction = character(),
      tRNA_ID = character(), anticodon = character()
    ))
  }

  return(dplyr::bind_rows(results))
}
