#' Write the circularization evidence a project database ingests
#'
#' Both files are always written, header included, because the Nextflow process
#' declares them as outputs and a missing file fails the task.
#'
#' @param dir output directory
#' @param id sample ID
#' @param hit hit list from [find_end_overlap()], or NULL
#' @param trimmed bp actually removed
#' @param junction junction list from [count_junction_reads()], or NULL
#' @param min_junction_reads,min_overhang thresholds in force for this run
#'
#' @noRd
write_circularize_evidence <- function(dir, id, hit, trimmed, junction,
                                       min_junction_reads, min_overhang) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  overlap <- if (is.null(hit)) {
    data.frame(
      ID = character(0), qstart = integer(0), qend = integer(0),
      sstart = integer(0), send = integer(0), length = integer(0),
      pident = numeric(0), mismatches = integer(0),
      aln_query = character(0), aln_subject = character(0),
      accepted = integer(0), reason = character(0), trimmed = integer(0),
      junction_reads = integer(0), min_junction_reads = integer(0),
      window_bp = integer(0), min_overhang = integer(0)
    )
  } else {
    data.frame(
      ID = id,
      qstart = hit$qstart, qend = hit$qend,
      sstart = hit$sstart, send = hit$send,
      length = hit$length, pident = round(hit$pident, 2),
      mismatches = hit$mismatches,
      aln_query = hit$qseq, aln_subject = hit$sseq,
      accepted = as.integer(hit$accepted),
      reason = hit$reason %||% NA_character_,
      trimmed = as.integer(trimmed),
      junction_reads = if (is.null(junction)) NA_integer_ else as.integer(junction$count),
      min_junction_reads = as.integer(min_junction_reads),
      window_bp = if (is.null(junction)) NA_integer_ else as.integer(junction$window_bp),
      min_overhang = as.integer(min_overhang)
    )
  }
  utils::write.csv(overlap, file.path(dir, "circularize_overlap.csv"),
                   row.names = FALSE, na = "")

  depth <- if (is.null(junction) || nrow(junction$depth) == 0L) {
    data.frame(
      ID = character(0), position = integer(0), rel_position = integer(0),
      depth = integer(0), depth_spanning = integer(0)
    )
  } else {
    cbind(ID = id, junction$depth)
  }
  utils::write.csv(depth, file.path(dir, "circularize_depth.csv"),
                   row.names = FALSE, na = "")

  invisible(NULL)
}

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
#' @param cpus Number of CPUs for read mapping (default = 4)
#' @param out_fn Path for the output fasta. Defaults to overwriting nothing and
#'   returning the result only.
#' @param log_fn Optional path for a plain-text log
#' @param id Sample ID, recorded in the evidence CSVs (default = "sample")
#' @param evidence_dir Optional directory to write `circularize_overlap.csv`
#'   and `circularize_depth.csv` into
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
    cpus = 4,
    out_fn = NULL,
    log_fn = NULL,
    id = "sample",
    evidence_dir = NULL) {
  assembly <- Biostrings::readDNAStringSet(assembly_fn)

  log_lines <- character(0)
  add_log <- function(...) {
    log_lines <<- c(log_lines, paste0(...))
  }

  finish <- function(res, hit = NULL, junction = NULL) {
    if (!is.null(out_fn)) {
      Biostrings::writeXStringSet(res$sequence, out_fn)
    }
    if (!is.null(log_fn)) {
      writeLines(c(log_lines, res$note), log_fn)
    }
    if (!is.null(evidence_dir)) {
      write_circularize_evidence(
        evidence_dir, id, hit, res$trimmed, junction,
        min_junction_reads = min_junction_reads, min_overhang = min_overhang
      )
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
    min_identity = min_identity
  )
  add_log(trim$log)

  if (trim$trimmed == 0L) {
    note <- if (is.null(trim$hit)) {
      "linear: no self-overlap found"
    } else {
      paste0("linear: ", trim$hit$reason)
    }
    return(finish(list(
      circular = FALSE, sequence = assembly, trimmed = 0L, note = note
    ), hit = trim$hit))
  }

  trimmed_set <- Biostrings::DNAStringSet(trim$sequence) |>
    setNames(names(assembly))

  # No reads to consult: the overlap alone decides.
  if (identical(as.character(paired_reads_1), "NA")) {
    return(finish(list(
      circular = TRUE, sequence = trimmed_set, trimmed = trim$trimmed,
      note = paste0("circular: trimmed ", trim$trimmed, " bp overlap (no reads to confirm)")
    ), hit = trim$hit))
  }

  junction <- count_junction_reads(
    trim$sequence,
    paired_reads_1 = paired_reads_1,
    paired_reads_2 = paired_reads_2,
    min_overhang = min_overhang,
    cpus = cpus
  )
  support <- junction$count
  add_log("junction-spanning reads: ", support)

  if (support < min_junction_reads) {
    return(finish(list(
      circular = FALSE, sequence = assembly, trimmed = 0L,
      note = paste0(
        "linear: ", trim$trimmed, " bp overlap found but only ", support,
        " junction read", if (support == 1L) "" else "s",
        " (", min_junction_reads, " required)"
      )
    ), hit = trim$hit, junction = junction))
  }

  finish(list(
    circular = TRUE, sequence = trimmed_set, trimmed = trim$trimmed,
    note = paste0(
      "circular: trimmed ", trim$trimmed, " bp overlap, ", support,
      " junction reads"
    )
  ), hit = trim$hit, junction = junction)
}

#' Trim a redundant end-to-start overlap from a contig
#'
#' Self-BLASTs the sequence and removes the duplicated copy at the contig end,
#' but only when the overlap clears the thresholds. The hit is returned either
#' way so the caller can record what was found.
#'
#' @param seq contig sequence (character)
#' @param min_overlap,min_identity detection thresholds, see [circularize_asmb()]
#'
#' @return list with `sequence`, `trimmed` (bp removed), `log`, and `hit`
#'
#' @noRd
trim_end_overlap <- function(seq,
                             min_overlap = 220,
                             min_identity = 99) {
  hit <- find_end_overlap(seq, min_overlap = min_overlap,
                          min_identity = min_identity)
  if (is.null(hit)) {
    return(list(sequence = seq, trimmed = 0L, log = character(0), hit = NULL))
  }
  if (!hit$accepted) {
    return(list(
      sequence = seq, trimmed = 0L,
      log = paste0("overlap not used: ", hit$reason),
      hit = hit
    ))
  }
  # Drop the duplicated copy at the contig end, keeping the copy at the start.
  seq <- substr(seq, 1L, hit$sstart - 1L)
  list(
    sequence = seq,
    trimmed = hit$trimmed,
    log = paste0(
      "removed ", hit$trimmed, " bp overlap (", round(hit$pident, 1),
      "% identity), new length ", nchar(seq), " bp"
    ),
    hit = hit
  )
}

#' Find an end-to-start self-overlap with BLAST
#'
#' Returns the single best end-anchored hit, whether or not it clears the
#' thresholds. The caller trims only when `accepted`; a rejected hit is still
#' reported so the user can see what was found and why it was not used.
#'
#' @param seq contig sequence (character)
#' @param min_overlap,min_identity acceptance thresholds
#' @param blastn path to the blastn binary
#'
#' @return list describing the best hit, or NULL when no end-anchored hit exists
#'
#' @noRd
find_end_overlap <- function(seq,
                             min_overlap = 220,
                             min_identity = 99,
                             blastn = getOption("MitoPilot.blastn", "blastn")) {
  # Tolerance for how far the overlap may sit from the contig ends
  offset <- 40L
  len <- nchar(seq)

  tmp <- tempfile(fileext = ".fasta")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(c(">contig", seq), tmp)

  out <- suppressWarnings(system2(
    blastn,
    c(
      "-query", shQuote(tmp), "-subject", shQuote(tmp),
      "-dust", "no", "-evalue", "1e-10",
      "-outfmt", shQuote("6 qstart qend sstart send length pident qseq sseq")
    ),
    stdout = TRUE, stderr = FALSE
  ))
  if (length(out) == 0L || !is.null(attr(out, "status"))) {
    return(NULL)
  }

  hits <- utils::read.delim(
    text = paste(out, collapse = "\n"),
    header = FALSE,
    colClasses = "character",
    col.names = c("qstart", "qend", "sstart", "send", "length", "pident",
                  "qseq", "sseq")
  )
  for (col in c("qstart", "qend", "sstart", "send", "length")) {
    hits[[col]] <- as.integer(hits[[col]])
  }
  hits$pident <- as.numeric(hits$pident)

  # Structural: what makes a hit a candidate at all. The 0.9 rule drops the
  # trivial full-length self match, which would otherwise always win.
  hits <- hits[
    hits$sstart < hits$send &
      hits$qstart <= offset &
      hits$send >= len - offset &
      hits$length < 0.9 * len,
  ]
  if (nrow(hits) == 0L) {
    return(NULL)
  }

  hit <- hits[which.max(hits$length), ]

  # Thresholds label the winner rather than discard it.
  reason <- if (hit$length < min_overlap) {
    paste0("overlap ", hit$length, " bp below the ", min_overlap, " bp minimum")
  } else if (hit$pident < min_identity) {
    paste0("overlap ", round(hit$pident, 1), "% identical, below ",
           min_identity, "%")
  } else {
    NA_character_
  }

  list(
    sstart = hit$sstart,
    trimmed = len - hit$sstart + 1L,
    pident = hit$pident,
    qstart = hit$qstart,
    qend = hit$qend,
    send = hit$send,
    length = hit$length,
    qseq = hit$qseq,
    sseq = hit$sseq,
    mismatches = sum(strsplit(hit$qseq, "")[[1]] != strsplit(hit$sseq, "")[[1]]),
    accepted = is.na(reason),
    reason = reason
  )
}

#' Per-position depth over a window from alignment intervals
#'
#' Difference array plus cumsum, so no interval library is needed for what is a
#' few hundred positions.
#'
#' @param starts,ends 1-based inclusive alignment intervals on the reference
#' @param win_start,win_end 1-based inclusive window bounds
#'
#' @return integer vector of length `win_end - win_start + 1`
#'
#' @noRd
window_depth <- function(starts, ends, win_start, win_end) {
  n <- win_end - win_start + 1L
  if (length(starts) == 0L) {
    return(integer(n))
  }
  s <- pmax(starts, win_start)
  e <- pmin(ends, win_end)
  keep <- s <= e
  if (!any(keep)) {
    return(integer(n))
  }
  # nbins n+1 so an interval ending on the last position still has a slot for
  # its closing -1.
  opened <- tabulate(s[keep] - win_start + 1L, nbins = n + 1L)
  closed <- tabulate(e[keep] - win_start + 2L, nbins = n + 1L)
  cumsum(opened - closed)[seq_len(n)]
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
#' @return list with `count` (integer spanning reads), `window_bp` (integer),
#'   and `depth` (data frame of position, rel_position, depth, depth_spanning)
#'
#' @noRd
count_junction_reads <- function(seq,
                                 paired_reads_1,
                                 paired_reads_2,
                                 min_overhang = 30,
                                 cpus = 4,
                                 min_mapq = 20) {
  len <- nchar(seq)
  flank <- min(500L, len %/% 2L)
  empty <- list(
    count = 0L, window_bp = 0L,
    depth = data.frame(
      position = integer(0), rel_position = integer(0),
      depth = integer(0), depth_spanning = integer(0)
    )
  )
  if (flank <= min_overhang) {
    return(empty)
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
    return(empty)
  }

  fields <- stringr::str_split(sam, "\t", simplify = TRUE)
  starts <- as.integer(fields[, 4])
  ends <- starts + cigar_ref_length(fields[, 6]) - 1L

  ok <- !is.na(starts) & !is.na(ends)
  spanning <- ok &
    starts <= len - min_overhang &
    ends >= len + min_overhang

  win_start <- len - flank + 1L
  win_end <- len + flank
  position <- win_start:win_end

  list(
    count = sum(spanning),
    window_bp = flank,
    depth = data.frame(
      position = position,
      rel_position = position - len,
      depth = window_depth(starts[ok], ends[ok], win_start, win_end),
      depth_spanning = window_depth(starts[spanning], ends[spanning],
                                    win_start, win_end)
    )
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
