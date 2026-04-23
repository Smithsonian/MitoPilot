#' Use ARWEN to annotate tRNAs in a mitochondrial genome assembly
#'
#' @param assembly DNAStringSet assembly to annotate
#' @param arwen_opts command line options for ARWEN (default = "-m")
#' @param genetic_code NCBI translation table number passed to ARWEN via -gc<N> (default = "2")
#' @param out output file path
#'
#' @export
#'
annotate_arwen <- function(
    assembly = NULL,
    arwen_opts = "-m",
    genetic_code = "2",
    out = NULL) {

  out <- out %||% tempfile()

  fasta <- tempfile(fileext = ".fa")
  Biostrings::writeXStringSet(assembly, fasta)

  system2(
    "arwen",
    args = c(strsplit(arwen_opts, "\\s+")[[1]], paste0("-gc", genetic_code), fasta),
    stdout = out,
    stderr = FALSE
  )

  raw <- readLines(out)

  tRNA_pattern <- "^\\s*\\d+\\s+tRNA-(\\S+)\\s+\\((\\w+)\\)\\s+\\[(\\d+),(\\d+)\\]"

  current_seq <- NA_character_
  results <- list()

  for (line in raw) {
    if (startsWith(line, ">")) {
      current_seq <- trimws(sub("^>", "", line))
      next
    }
    m <- regmatches(line, regexec(tRNA_pattern, line))[[1]]
    if (length(m) == 0) next

    aa_type   <- m[2]
    anticodon <- m[3]
    start     <- as.integer(m[4])
    end       <- as.integer(m[5])

    aa_clean <- sub("[12]$", "", aa_type)
    if (aa_clean == "iMet") aa_clean <- "Met"
    gene <- .trnA_key[[aa_clean]]
    if (is.null(gene)) next

    results[[length(results) + 1]] <- data.frame(
      contig    = current_seq,
      type      = "tRNA",
      gene      = gene,
      product   = paste("tRNA", aa_clean, sep = "-"),
      pos1      = min(start, end),
      pos2      = max(start, end),
      length    = 1L + abs(end - start),
      direction = ifelse(start <= end, "+", "-"),
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

  dplyr::bind_rows(results)
}
