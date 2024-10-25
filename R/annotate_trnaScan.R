#' Title
#'
#' @param seqs text string of the assembly to annotate
#' @param rotate should the assembly be rotated? default = TRUE
#' @param trnaScan_opts command line options for tRNAscan-SE (defatult = `-M vert`)
#' @param cpus number of cpus to use (default = 4)
#' @param out output file name
#'
#' @export
#'
annotate_trnaScan <- function(
  seqs = NULL,
  rotate = TRUE,
  trnaScan_opts = "-M vert",
  cpus = 4,
  out = "test.trnascan"
  ){

  tmp <- tempfile(fileext = ".fasta")
  Biostrings::writeXStringSet(seqs, tmp)

  stringr::str_glue(
    "tRNAscan-SE {opts} -o {out} --thread {cpus} --quiet {tmp}"
  ) |> system()

  if(rotate && length(seqs)==1L){

    seq_length <- Biostrings::width(seqs)

    # Find start position for rotation
    start <- read.delim(
      out,
      skip = 3,
      header = F,
      col.names = c("seq", "idx", "begin", "end",
                    "type", "anticodon", "intron_beign",
                    "intron_end", "score", "notes")
    ) |>
      filter(type %in% c("Phe", "Val")) |>
      arrange(type, desc(score)) |>
      select(begin, end) |>
      dplyr::slice(1)

    # Rotate sequence (heavy strand)
    if (nrow(start) == 1 && start$begin < start$end && start$begin > 1) {
      seqs <- xscat(
        subseq(seqs, start$begin, seq_length),
        subseq(seqs, 1, start$begin - 1)
      ) |>
        setNames(names(seqs))
      seqs@metadata['rotate_to'] <- start$begin
    }

    # Rotate sequence (light strand)
    if (nrow(start) == 1 && start$begin > start$end && start$begin < seq_length) {
      seqs <- Biostrings::xscat(
        Biostrings::subseq(seqs, start$begin + 1, seq_length),
        Biostrings::subseq(seqs, 1, start$begin)
      ) |>
        setNames(names(seqs)) |>
        Biostrings::reverseComplement()
      seqs@metadata['rotate_to'] <- -(start$begin + 1)
    }

    unlink(out)
    Biostrings::writeXStringSet(seqs, tmp)
    stringr::str_glue(
      "tRNAscan-SE {opts} -o {out} --thread {cpus} --quiet {tmp}"
    ) |> system()

  }

  # Format output
  read.delim(
    out,
    skip = 3,
    header = F,
    col.names = c("seq", "idx", "begin", "end",
                  "type", "anticodon", "intron_beign",
                  "intron_end", "score", "notes")
  ) |>
    purrr::pmap_dfr(function(...) {
      cur <- list(...)
      data.frame(
        contig = stringr::str_squish(cur$seq),
        type = "tRNA",
        gene = .trnA_key [[cur$type]],
        product = paste("tRNA", cur$type, sep = "-"),
        pos1 = min(c(cur$begin, cur$end)),
        pos2 = max(c(cur$begin, cur$end)),
        direction = ifelse(cur$begin < cur$end, "+", "-"),
        anticodon = cur$anticodon
      ) |>
        mutate(length = 1 + abs(pos2 - pos1), .before = "direction")
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
