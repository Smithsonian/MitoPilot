#' Write the circularization evidence a project database ingests
#'
#' Both files are always written, header included, because the Nextflow process
#' declares them as outputs and a missing file fails the task.
#'
#' @param dir output directory
#' @param id sample ID
#' @param results list of per-contig results, each with `contig`, `hit`,
#'   `trimmed` and `junction`
#' @param min_junction_reads,min_overhang thresholds in force for this run
#'
#' @noRd
write_circularize_evidence <- function(dir, id, results,
                                       min_junction_reads, min_overhang) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  empty_overlap <- data.frame(
    ID = character(0), contig = character(0), qstart = integer(0), qend = integer(0),
    sstart = integer(0), send = integer(0), length = integer(0),
    pident = numeric(0), mismatches = integer(0),
    aln_query = character(0), aln_subject = character(0),
    q_ctx_left = character(0), q_ctx_right = character(0),
    s_ctx_left = character(0), s_ctx_right = character(0),
    accepted = integer(0), reason = character(0),
    contig_length = integer(0), trimmed = integer(0),
    junction_reads = integer(0), min_junction_reads = integer(0),
    window_bp = integer(0), min_overhang = integer(0)
  )

  overlap_row <- function(res) {
    hit <- res$hit
    if (is.null(hit)) {
      return(NULL)
    }
    data.frame(
      ID = id,
      contig = res$contig,
      qstart = hit$qstart, qend = hit$qend,
      sstart = hit$sstart, send = hit$send,
      length = hit$length, pident = round(hit$pident, 2),
      mismatches = hit$mismatches,
      aln_query = hit$qseq, aln_subject = hit$sseq,
      q_ctx_left = hit$q_ctx_left, q_ctx_right = hit$q_ctx_right,
      s_ctx_left = hit$s_ctx_left, s_ctx_right = hit$s_ctx_right,
      accepted = as.integer(hit$accepted),
      reason = hit$reason %||% NA_character_,
      contig_length = as.integer(hit$contig_length),
      trimmed = as.integer(res$trimmed),
      junction_reads = if (is.null(res$junction)) NA_integer_ else as.integer(res$junction$count),
      min_junction_reads = as.integer(min_junction_reads),
      window_bp = if (is.null(res$junction)) NA_integer_ else as.integer(res$junction$window_bp),
      min_overhang = as.integer(min_overhang)
    )
  }

  overlap <- do.call(rbind, c(list(empty_overlap), lapply(results, overlap_row)))
  utils::write.csv(overlap, file.path(dir, "circularize_overlap.csv"),
                   row.names = FALSE, na = "")

  empty_depth <- data.frame(
    ID = character(0), contig = character(0), position = integer(0), rel_position = integer(0),
    depth = integer(0), depth_spanning = integer(0)
  )
  depth_rows <- function(res) {
    if (is.null(res$junction) || nrow(res$junction$depth) == 0L) {
      return(NULL)
    }
    cbind(ID = id, contig = res$contig, res$junction$depth)
  }

  depth <- do.call(rbind, c(list(empty_depth), lapply(results, depth_rows)))
  utils::write.csv(depth, file.path(dir, "circularize_depth.csv"),
                   row.names = FALSE, na = "")

  invisible(NULL)
}

#' Attempt to circularize one contig
#'
#' Reads are mapped only once an overlap has been accepted and trimmed: a
#' bowtie2 index per contig is the expensive part of this function and there is
#' nothing to confirm when nothing was removed.
#'
#' @param seq contig sequence (character)
#' @param paired_reads_1,paired_reads_2 paths to reads (fastq), or "NA"
#' @param min_overlap,min_identity,min_junction_reads,min_overhang,cpus
#'   thresholds, see [circularize_asmb()]
#'
#' @return list with `circular`, `sequence` (character), `trimmed`, `note`,
#'   `hit`, `junction` and `log`
#'
#' @noRd
circularize_contig <- function(seq,
                               paired_reads_1 = "NA",
                               paired_reads_2 = "NA",
                               min_overlap = 220,
                               min_identity = 99,
                               min_junction_reads = 5,
                               min_overhang = 30,
                               cpus = 4) {
  log_lines <- paste0("input length: ", nchar(seq), " bp")

  trim <- trim_end_overlap(
    seq,
    min_overlap = min_overlap,
    min_identity = min_identity
  )
  log_lines <- c(log_lines, trim$log)

  out <- function(circular, sequence, trimmed, note, junction = NULL) {
    list(circular = circular, sequence = sequence, trimmed = trimmed,
         note = note, hit = trim$hit, junction = junction, log = log_lines)
  }

  if (trim$trimmed == 0L) {
    note <- if (is.null(trim$hit)) {
      "linear: no self-overlap found"
    } else {
      paste0("linear: ", trim$hit$reason)
    }
    return(out(FALSE, seq, 0L, note))
  }

  # No reads to consult: the overlap alone decides.
  if (identical(as.character(paired_reads_1), "NA")) {
    return(out(
      TRUE, trim$sequence, trim$trimmed,
      paste0("circular: trimmed ", trim$trimmed, " bp overlap (no reads to confirm)")
    ))
  }

  junction <- count_junction_reads(
    trim$sequence,
    paired_reads_1 = paired_reads_1,
    paired_reads_2 = paired_reads_2,
    min_overhang = min_overhang,
    cpus = cpus
  )
  support <- junction$count
  log_lines <- c(log_lines, paste0("junction-spanning reads: ", support))

  if (support < min_junction_reads) {
    return(out(
      FALSE, seq, 0L,
      paste0(
        "linear: ", trim$trimmed, " bp overlap found but only ", support,
        " junction read", if (support == 1L) "" else "s",
        " (", min_junction_reads, " required)"
      ),
      junction = junction
    ))
  }

  out(
    TRUE, trim$sequence, trim$trimmed,
    paste0(
      "circular: trimmed ", trim$trimmed, " bp overlap, ", support,
      " junction reads"
    ),
    junction = junction
  )
}

#' Attempt to circularize a linear mitogenome assembly
#'
#' Many assemblers emit a circular molecule as a linear contig whose end
#' duplicates its start. This function detects that redundant overlap by BLASTing
#' the contig against itself, trims the duplicated copy, and (when raw reads are
#' supplied) requires reads spanning the resulting junction before calling the
#' assembly circular.
#'
#' Every contig of a fragmented assembly is attempted independently; a fragment
#' can be a circular molecule reported linearly just as a whole assembly can.
#'
#' The overlap detection follows the approach used by MitoHiFi's
#' `circularizationCheck` (MIT licensed, Genome Research Ltd).
#'
#' @param assembly_fn Path to the input assembly (fasta)
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
#' @return (invisibly) a list with `circular` (logical, TRUE when every contig
#'   is circular), `sequence` (DNAStringSet), `trimmed` (bp removed across all
#'   contigs), `note` (human-readable summary) and `contigs`, a list of
#'   per-contig results each carrying `contig`, `circular`, `trimmed` and `note`
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

  # Scaffold numbers do not exist yet at this point in WF1, so the contig name
  # is the only stable key. First token only, matching BLAST qseqid and the
  # mito_candidates table.
  contigs <- sub("\\s.*$", "", names(assembly))

  results <- lapply(seq_along(assembly), function(i) {
    res <- circularize_contig(
      as.character(assembly[[i]]),
      paired_reads_1 = paired_reads_1,
      paired_reads_2 = paired_reads_2,
      min_overlap = min_overlap,
      min_identity = min_identity,
      min_junction_reads = min_junction_reads,
      min_overhang = min_overhang,
      cpus = cpus
    )
    c(list(contig = contigs[i]), res)
  })

  log_lines <- unlist(lapply(results, function(res) {
    if (length(results) == 1L) res$log else paste0(res$contig, ": ", res$log)
  }), use.names = FALSE)

  sequence <- Biostrings::DNAStringSet(
    vapply(results, function(res) res$sequence, character(1))
  ) |> setNames(names(assembly))

  note <- if (length(results) == 0L) {
    "not attempted: assembly contains no contigs"
  } else if (length(results) == 1L) {
    results[[1]]$note
  } else {
    paste(vapply(results, function(res) paste0(res$contig, ": ", res$note),
                 character(1)), collapse = "; ")
  }

  res <- list(
    circular = length(results) > 0L && all(vapply(results, function(r) r$circular, logical(1))),
    sequence = sequence,
    trimmed = sum(vapply(results, function(r) as.integer(r$trimmed), integer(1))),
    note = note,
    contigs = results
  )

  if (!is.null(out_fn)) {
    Biostrings::writeXStringSet(res$sequence, out_fn)
  }
  if (!is.null(log_fn)) {
    writeLines(c(log_lines, res$note), log_fn)
  }
  if (!is.null(evidence_dir)) {
    write_circularize_evidence(
      evidence_dir, id, results,
      min_junction_reads = min_junction_reads, min_overhang = min_overhang
    )
  }
  invisible(res)
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
#' @param context_bp contig sequence kept either side of each copy, for display
#' @param blastn path to the blastn binary
#'
#' @return list describing the best hit, or NULL when no end-anchored hit exists
#'
#' @noRd
find_end_overlap <- function(seq,
                             min_overlap = 220,
                             min_identity = 99,
                             context_bp = 50L,
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

  # Contig sequence either side of each copy, so the modal can show the overlap
  # in context. Clipped where the copy already sits against a contig end, which
  # is the usual case for the 5' copy's left side and the 3' copy's right side.
  ctx <- function(from, to) {
    if (from > to) "" else substr(seq, max(1L, from), min(len, to))
  }

  list(
    sstart = hit$sstart,
    contig_length = len,
    trimmed = len - hit$sstart + 1L,
    pident = hit$pident,
    qstart = hit$qstart,
    qend = hit$qend,
    send = hit$send,
    length = hit$length,
    qseq = hit$qseq,
    sseq = hit$sseq,
    q_ctx_left = ctx(hit$qstart - context_bp, hit$qstart - 1L),
    q_ctx_right = ctx(hit$qend + 1L, hit$qend + context_bp),
    s_ctx_left = ctx(hit$sstart - context_bp, hit$sstart - 1L),
    s_ctx_right = ctx(hit$send + 1L, hit$send + context_bp),
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

#' Per-contig depth from alignments against the doubled junction reference
#'
#' The mapping reference is the contig followed by a copy of its own first
#' `flank` bases, so a read landing in that first `flank` bases aligns equally
#' well to either copy and bowtie2 places it arbitrarily. Folding the appended
#' block back onto the contig start recombines the two copies into the contig's
#' real coverage, which is why the caller must NOT pre-filter on mapping
#' quality: an ambiguous read is placed once and folding puts it back.
#'
#' @param starts,ends 1-based inclusive alignment intervals on the doubled
#'   reference
#' @param len contig length (the fold point)
#'
#' @return integer vector of length `len`, depth per contig position
#'
#' @noRd
contig_depth <- function(starts, ends, len) {
  if (length(starts) == 0L) {
    return(integer(len))
  }
  # An interval crossing the fold point contributes to both ends, so split it.
  head_keep <- starts <= len
  tail_keep <- ends > len
  s <- c(starts[head_keep], pmax(starts[tail_keep] - len, 1L))
  e <- c(pmin(ends[head_keep], len), ends[tail_keep] - len)
  keep <- s <= e
  if (!any(keep)) {
    return(integer(len))
  }
  s <- pmax(s[keep], 1L)
  e <- pmin(e[keep], len)
  cumsum(tabulate(s, nbins = len + 1L) - tabulate(e + 1L, nbins = len + 1L))[seq_len(len)]
}

#' Count reads spanning the junction of a circularized contig
#'
#' Maps reads to the contig with its first `flank` bases appended (the same
#' trick [coverage()] uses for circular assemblies) and counts alignments whose
#' aligned reference block extends `min_overhang` bases past the seam on both
#' sides.
#'
#' Two depth tracks come back. `depth` is the contig's own coverage either side
#' of the junction, folded so both ends read as one continuous curve. It counts
#' every alignment, because the duplicated block makes reads there ambiguous and
#' a mapping-quality filter would empty it. `depth_spanning` counts only reads
#' that cross the seam with `min_overhang` on both sides, and does apply the
#' filter, since a spanning read has to be placed uniquely to mean anything.
#'
#' @param seq circularized contig sequence (character)
#' @param paired_reads_1,paired_reads_2 paths to reads (fastq)
#' @param min_overhang bases required either side of the junction
#' @param cpus threads for bowtie2
#' @param min_mapq minimum mapping quality for a spanning read (default = 20)
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

  # No -q here: the depth track needs the ambiguous alignments in the duplicated
  # block. The filter is applied in R, to the spanning track only.
  sam <- suppressWarnings(system2(
    "samtools",
    c("view", shQuote(bam)),
    stdout = TRUE, stderr = FALSE
  ))
  if (length(sam) == 0L) {
    return(empty)
  }

  fields <- stringr::str_split(sam, "\t", simplify = TRUE)
  starts <- as.integer(fields[, 4])
  mapq <- as.integer(fields[, 5])
  ends <- starts + cigar_ref_length(fields[, 6]) - 1L

  ok <- !is.na(starts) & !is.na(ends)
  spanning <- ok &
    !is.na(mapq) & mapq >= min_mapq &
    starts <= len - min_overhang &
    ends >= len + min_overhang

  win_start <- len - flank + 1L
  win_end <- len + flank
  position <- win_start:win_end
  rel_position <- position - len

  # Negative offsets are the contig's 3' end running into the seam, positive
  # offsets its 5' start running out of it.
  cov <- contig_depth(starts[ok], ends[ok], len)

  list(
    count = sum(spanning),
    window_bp = flank,
    depth = data.frame(
      position = position,
      rel_position = rel_position,
      depth = cov[ifelse(rel_position <= 0L, len + rel_position, rel_position)],
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
