#' Use ARAGORN to annotate tRNAs in a mitochondrial genome assembly
#'
#' @param assembly DNAStringSet assembly to annotate
#' @param aragorn_opts command line options for ARAGORN (default = "-m -gcstd")
#' @param genetic_code NCBI translation table number passed to ARAGORN via -gc<N> (default = "2")
#' @param circular logical; TRUE for circular topology (-c), FALSE for linear (-l)
#' @param condaenv conda environment containing ARAGORN (default = "aragorn")
#'
#' @export
#'
annotate_aragorn <- function(
    assembly = NULL,
    aragorn_opts = "-m -gcstd",
    genetic_code = "2",
    circular = TRUE,
    condaenv = "aragorn") {

  fasta <- tempfile(fileext = ".fa")
  Biostrings::writeXStringSet(assembly, fasta)
  seq_lens <- setNames(
    Biostrings::width(assembly),
    names(assembly) |> stringr::str_extract("\\S+")
  )

  message("starting ARAGORN")

  topology_flag <- if (circular) "-c" else "-l"
  out <- tempfile(fileext = ".txt")

  process_args <- list(
    cmd = "aragorn",
    args = c(
      strsplit(aragorn_opts, "\\s+")[[1]],
      paste0("-gc", genetic_code),
      topology_flag,
      "-w", "-o", out,
      fasta
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

  raw <- readLines(out)

  trnA_key <- list(
    Phe = "trnF", Val = "trnV", Leu = "trnL", Ile = "trnI",
    Gln = "trnQ", Met = "trnM", Trp = "trnW", Ala = "trnA",
    Asn = "trnN", Cys = "trnC", Tyr = "trnY", Ser = "trnS",
    Asp = "trnD", Lys = "trnK", Gly = "trnG", Arg = "trnR",
    His = "trnH", Glu = "trnE", Thr = "trnT", Pro = "trnP",
    SeC = "trnU", fMet = "trnM", iMet = "trnM"
  )

  # ARAGORN batch mode (-w) format:
  # >seq_name [length=N] [topology]
  # N genes found.
  #    N   tRNA-Type(anticodon)   c[start,end]
  tRNA_pattern <- "^\\s*\\d+\\s+tRNA-(\\S+?)\\((\\w+)\\)\\s+(c?)\\[(\\d+),(\\d+)\\]"

  current_seq <- NA_character_
  results <- list()

  for (line in raw) {
    if (startsWith(line, ">")) {
      current_seq <- stringr::str_extract(line, "(?<=>)\\S+")
      next
    }
    m <- regmatches(line, regexec(tRNA_pattern, line))[[1]]
    if (length(m) == 0) next

    aa_type    <- m[2]
    anticodon  <- m[3]
    complement <- m[4]   # "c" = minus strand, "" = plus strand
    start      <- as.integer(m[5])
    end        <- as.integer(m[6])

    aa_clean <- sub("[12]$", "", aa_type)
    if (aa_clean %in% c("iMet", "fMet")) aa_clean <- "Met"
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
