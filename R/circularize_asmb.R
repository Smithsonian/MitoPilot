#' Attempt to circularize a linear mitogenome assembly
#'
#' Many assemblers emit a circular molecule as a linear contig whose end
#' duplicates its start. This function detects that redundant overlap by BLASTing
#' the contig against itself, trims the duplicated copy, and (when raw reads are
#' supplied) requires reads spanning the resulting junction before calling the
#' assembly circular.
#'
#' The overlap detection follows the approach used by MitoHiFi's
#' `circularizationCheck` (MIT licensed, Genome Research Ltd).
#'
#' @param assembly_fn Path to the input assembly (fasta, single contig)
#' @param paired_reads_1 Path to forward reads (fastq), or "NA" for none
#' @param paired_reads_2 Path to reverse reads (fastq), or "NA" for none
#' @param min_overlap Shortest accepted self-overlap (bp, default = 220)
#' @param min_identity Percent identity required for the overlap (default = 99)
#' @param min_junction_reads Reads that must span the junction (default = 5)
#' @param min_overhang Bases a read must extend past the junction on each side
#'   (default = 30)
#' @param offset Tolerance for how far the overlap may sit from the contig ends
#'   (bp, default = 40)
#' @param cpus Number of CPUs for read mapping (default = 4)
#' @param out_fn Path for the output fasta. Defaults to overwriting nothing and
#'   returning the result only.
#' @param log_fn Optional path for a plain-text log
#'
#' @return (invisibly) a list with `circular` (logical), `sequence`
#'   (DNAStringSet), `trimmed` (bp removed) and `note` (human-readable summary)
#'
#' @export
#'
circularize_asmb <- function(
    assembly_fn = NULL,
    paired_reads_1 = "NA",
    paired_reads_2 = "NA",
    min_overlap = 220,
    min_identity = 99,
    min_junction_reads = 5,
    min_overhang = 30,
    offset = 40,
    cpus = 4,
    out_fn = NULL,
    log_fn = NULL) {
  assembly <- Biostrings::readDNAStringSet(assembly_fn)

  log_lines <- character(0)
  add_log <- function(...) {
    log_lines <<- c(log_lines, paste0(...))
  }

  finish <- function(res) {
    if (!is.null(out_fn)) {
      Biostrings::writeXStringSet(res$sequence, out_fn)
    }
    if (!is.null(log_fn)) {
      writeLines(c(log_lines, res$note), log_fn)
    }
    invisible(res)
  }

  if (length(assembly) != 1L) {
    return(finish(list(
      circular = FALSE, sequence = assembly, trimmed = 0L,
      note = "not attempted: assembly contains more than one contig"
    )))
  }

  seq <- as.character(assembly[[1]])
  add_log("input length: ", nchar(seq), " bp")

  trim <- trim_end_overlap(
    seq,
    min_overlap = min_overlap,
    min_identity = min_identity,
    offset = offset
  )
  add_log(trim$log)

  if (trim$trimmed == 0L) {
    return(finish(list(
      circular = FALSE, sequence = assembly, trimmed = 0L,
      note = "linear: no self-overlap found"
    )))
  }

  trimmed_set <- Biostrings::DNAStringSet(trim$sequence) |>
    setNames(names(assembly))

  # No reads to consult: the overlap alone decides.
  if (identical(as.character(paired_reads_1), "NA")) {
    return(finish(list(
      circular = TRUE, sequence = trimmed_set, trimmed = trim$trimmed,
      note = paste0("circular: trimmed ", trim$trimmed, " bp overlap (no reads to confirm)")
    )))
  }

  support <- count_junction_reads(
    trim$sequence,
    paired_reads_1 = paired_reads_1,
    paired_reads_2 = paired_reads_2,
    min_overhang = min_overhang,
    cpus = cpus
  )
  add_log("junction-spanning reads: ", support)

  if (support < min_junction_reads) {
    return(finish(list(
      circular = FALSE, sequence = assembly, trimmed = 0L,
      note = paste0(
        "linear: ", trim$trimmed, " bp overlap found but only ", support,
        " junction read", if (support == 1L) "" else "s",
        " (", min_junction_reads, " required)"
      )
    )))
  }

  finish(list(
    circular = TRUE, sequence = trimmed_set, trimmed = trim$trimmed,
    note = paste0(
      "circular: trimmed ", trim$trimmed, " bp overlap, ", support,
      " junction reads"
    )
  ))
}

#' Trim a redundant end-to-start overlap from a contig
#'
#' Repeatedly self-BLASTs the sequence and removes the duplicated copy at the
#' contig end until no qualifying overlap remains.
#'
#' @param seq contig sequence (character)
#' @param min_overlap,min_identity,offset detection thresholds, see
#'   [circularize_asmb()]
#' @param max_rounds maximum trimming iterations (default = 5)
#'
#' @return list with `sequence`, `trimmed` (total bp removed) and `log`
#'
#' @noRd
trim_end_overlap <- function(seq,
                             min_overlap = 220,
                             min_identity = 99,
                             offset = 40,
                             max_rounds = 5) {
  total <- 0L
  log <- character(0)
  for (i in seq_len(max_rounds)) {
    hit <- find_end_overlap(
      seq,
      min_overlap = min_overlap,
      min_identity = min_identity,
      offset = offset
    )
    if (is.null(hit)) {
      break
    }
    # Drop the duplicated copy at the contig end, keeping the copy at the start.
    seq <- substr(seq, 1L, hit$sstart - 1L)
    total <- total + hit$trimmed
    log <- c(log, paste0(
      "round ", i, ": removed ", hit$trimmed, " bp overlap (",
      round(hit$pident, 1), "% identity), new length ", nchar(seq), " bp"
    ))
  }
  list(sequence = seq, trimmed = total, log = log)
}

#' Find an end-to-start self-overlap with BLAST
#'
#' @param seq contig sequence (character)
#' @param min_overlap,min_identity,offset detection thresholds
#' @param blastn path to the blastn binary
#'
#' @return list with `sstart`, `trimmed` and `pident`, or NULL if no qualifying
#'   overlap was found
#'
#' @noRd
find_end_overlap <- function(seq,
                             min_overlap = 220,
                             min_identity = 99,
                             offset = 40,
                             blastn = getOption("MitoPilot.blastn", "blastn")) {
  len <- nchar(seq)
  if (len < 2 * min_overlap) {
    return(NULL)
  }

  tmp <- tempfile(fileext = ".fasta")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(c(">contig", seq), tmp)

  out <- suppressWarnings(system2(
    blastn,
    c(
      "-query", shQuote(tmp), "-subject", shQuote(tmp),
      "-dust", "no", "-evalue", "1e-10",
      "-outfmt", shQuote("6 qstart qend sstart send length pident")
    ),
    stdout = TRUE, stderr = FALSE
  ))
  if (length(out) == 0L || !is.null(attr(out, "status"))) {
    return(NULL)
  }

  hits <- utils::read.delim(
    text = paste(out, collapse = "\n"),
    header = FALSE,
    col.names = c("qstart", "qend", "sstart", "send", "length", "pident")
  )

  hits <- hits[
    hits$sstart < hits$send &                 # same strand
      hits$qstart <= offset &                 # query end of the alignment sits at the contig start
      hits$send >= len - offset &             # subject end sits at the contig end
      hits$length >= min_overlap &
      hits$length < 0.9 * len &               # drop the trivial full-length self hit
      hits$pident >= min_identity,
  ]
  if (nrow(hits) == 0L) {
    return(NULL)
  }

  hit <- hits[which.max(hits$length), ]
  list(
    sstart = hit$sstart,
    trimmed = len - hit$sstart + 1L,
    pident = hit$pident
  )
}

#' Count reads spanning the junction of a circularized contig
#'
#' Maps reads to the contig with its first `flank` bases appended (the same
#' trick [coverage()] uses for circular assemblies) and counts alignments whose
#' aligned reference block extends `min_overhang` bases past the seam on both
#' sides.
#'
#' @param seq circularized contig sequence (character)
#' @param paired_reads_1,paired_reads_2 paths to reads (fastq)
#' @param min_overhang bases required either side of the junction
#' @param cpus threads for bowtie2
#' @param min_mapq minimum mapping quality (default = 20)
#'
#' @return integer count of spanning reads
#'
#' @noRd
count_junction_reads <- function(seq,
                                 paired_reads_1,
                                 paired_reads_2,
                                 min_overhang = 30,
                                 cpus = 4,
                                 min_mapq = 20) {
  len <- nchar(seq)
  flank <- min(500L, floor(len / 2))
  if (flank <= min_overhang) {
    return(0L)
  }

  wd <- tempfile("junction")
  dir.create(wd)
  on.exit(unlink(wd, recursive = TRUE), add = TRUE)

  ref_fn <- file.path(wd, "junction.fasta")
  writeLines(c(">junction", paste0(seq, substr(seq, 1L, flank))), ref_fn)

  index <- file.path(wd, "index")
  bam <- file.path(wd, "junction.bam")
  stringr::str_glue("bowtie2-build -q {ref_fn} {index}") |> system()
  stringr::str_glue(
    "bowtie2 --very-sensitive-local --no-unal -x {index} ",
    "-1 {paired_reads_1} -2 {paired_reads_2} --threads {cpus} ",
    "| samtools view -bS - | samtools sort - > {bam}"
  ) |> system()

  sam <- suppressWarnings(system2(
    "samtools",
    c("view", "-q", min_mapq, shQuote(bam)),
    stdout = TRUE, stderr = FALSE
  ))
  if (length(sam) == 0L) {
    return(0L)
  }

  fields <- stringr::str_split(sam, "\t", simplify = TRUE)
  starts <- as.integer(fields[, 4])
  ends <- starts + cigar_ref_length(fields[, 6]) - 1L

  sum(
    !is.na(starts) & !is.na(ends) &
      starts <= len - min_overhang &
      ends >= len + min_overhang
  )
}

#' Reference bases consumed by a CIGAR string
#'
#' Soft clips are excluded, so only genuinely aligned blocks count toward the
#' junction overhang.
#'
#' @param cigar character vector of CIGAR strings
#'
#' @return integer vector of reference lengths
#'
#' @noRd
cigar_ref_length <- function(cigar) {
  vapply(cigar, function(x) {
    ops <- stringr::str_match_all(x, "(\\d+)([A-Z=])")[[1]]
    if (nrow(ops) == 0L) {
      return(NA_integer_)
    }
    keep <- ops[, 3] %in% c("M", "D", "N", "=", "X")
    sum(as.integer(ops[keep, 2]))
  }, integer(1), USE.NAMES = FALSE)
}
