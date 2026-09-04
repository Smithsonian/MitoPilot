#' On-disk paths for one sample's MapToRef outputs
#'
#' The published layout is `<dir_out>/<ID>/assemble/<assemble_opts>/`, with the
#' MapToRef working files under `maptoref/`. Centralised here because the
#' viewer needs eight of these paths and the convention is otherwise rebuilt
#' inline at every call site.
#'
#' @param dir_out project output root, normally `session$userData$dir_out`
#' @param ID sample id
#' @param assemble_opts assembly parameter set name
#' @return named list of character paths
#'
#' @noRd
maptoref_paths <- function(dir_out, ID, assemble_opts) {
  dir <- file.path(dir_out, ID, "assemble", assemble_opts)
  work <- file.path(dir, "maptoref")
  list(
    dir = dir,
    work = work,
    ref_fasta = file.path(work, "ref.fasta"),
    consensus = file.path(work, "subs_only.fasta"),
    bam = file.path(work, "final.bam"),
    bai = file.path(work, "final.bam.bai"),
    gb = file.path(work, "reference.gb"),
    depth = file.path(work, "maptoref_depth.csv"),
    features = file.path(work, "maptoref_features.csv"),
    summary = file.path(dir, paste0(ID, "_summary.txt"))
  )
}

#' Per-base depth table, or an empty frame
#' @noRd
maptoref_read_depth <- function(path) {
  empty <- data.frame(Position = integer(0), Depth = numeric(0))
  if (length(path) != 1L || is.na(path) || !file.exists(path)) {
    return(empty)
  }
  out <- utils::read.csv(path)
  if (!all(c("Position", "Depth") %in% names(out))) {
    return(empty)
  }
  data.frame(
    Position = as.integer(out$Position),
    Depth = as.numeric(out$Depth)
  )
}

#' Annotation features table, or an empty frame
#' @noRd
maptoref_read_features <- function(path) {
  empty <- data.frame(
    type = character(0), gene = character(0),
    start = integer(0), end = integer(0), strand = character(0),
    stringsAsFactors = FALSE
  )
  if (length(path) != 1L || is.na(path) || !file.exists(path)) {
    return(empty)
  }
  out <- utils::read.csv(path, stringsAsFactors = FALSE)
  if (!all(names(empty) %in% names(out))) {
    return(empty)
  }
  data.frame(
    type = as.character(out$type),
    gene = as.character(out$gene),
    start = as.integer(out$start),
    end = as.integer(out$end),
    strand = as.character(out$strand),
    stringsAsFactors = FALSE
  )
}

#' MapToRef summary block as a named character vector
#'
#' Repeated `note` keys are collapsed rather than overwritten, so a run with
#' several warnings shows all of them.
#' @noRd
maptoref_read_summary <- function(path) {
  if (length(path) != 1L || is.na(path) || !file.exists(path)) {
    return(character(0))
  }
  lines <- readLines(path, warn = FALSE)
  lines <- lines[grepl("=", lines, fixed = TRUE)]
  if (length(lines) == 0L) {
    return(character(0))
  }
  key <- sub("=.*$", "", lines)
  val <- sub("^[^=]*=", "", lines)
  vapply(split(val, key), paste, character(1), collapse = " | ")
}

#' Downsample a depth series, keeping the peak of each bin
#'
#' The whole-reference view would otherwise draw one point per base. Taking the
#' maximum rather than the mean keeps spikes and single-base dropouts visible.
#' @noRd
maptoref_bin_depth <- function(depth, n = 2000L) {
  if (nrow(depth) <= n) {
    return(depth)
  }
  bin <- ceiling(seq_len(nrow(depth)) / (nrow(depth) / n))
  data.frame(
    Position = as.integer(tapply(depth$Position, bin, min)),
    Depth = as.numeric(tapply(depth$Depth, bin, max))
  )
}

#' First FASTA record as one uppercase string
#' @noRd
maptoref_read_seq <- function(path) {
  if (length(path) != 1L || is.na(path) || !file.exists(path)) {
    return(NA_character_)
  }
  lines <- readLines(path, warn = FALSE)
  hdr <- grep("^>", lines)
  if (length(hdr) == 0L) {
    return(NA_character_)
  }
  last <- if (length(hdr) > 1L) hdr[2] - 1L else length(lines)
  toupper(paste(lines[(hdr[1] + 1L):last], collapse = ""))
}

#' Walk one read's CIGAR against the reference
#'
#' Mismatches are derived here rather than read from MD tags: the tags are
#' relative to the converged reference the reads were mapped to, while the
#' viewer displays the original reference, and the walk is needed for indels
#' regardless.
#'
#' @param pos leftmost reference position of the alignment
#' @param cigar CIGAR string
#' @param seq read sequence, including any soft-clipped bases
#' @param ref reference sequence as one string
#' @return list with start, end, mm, del, ins
#'
#' @noRd
.mtr_cigar_walk <- function(pos, cigar, seq, ref) {
  n <- as.integer(regmatches(cigar, gregexpr("[0-9]+", cigar))[[1]])
  op <- regmatches(cigar, gregexpr("[MIDNSHP=X]", cigar))[[1]]
  refv <- strsplit(ref, "", fixed = TRUE)[[1]]
  qv <- strsplit(toupper(as.character(seq)), "", fixed = TRUE)[[1]]
  rp <- as.integer(pos)
  qp <- 1L
  mm <- list()
  del <- list()
  ins <- list()
  for (k in seq_along(op)) {
    o <- op[k]
    L <- n[k]
    if (o %in% c("M", "=", "X")) {
      idx <- seq_len(L)
      rpos <- rp + idx - 1L
      qb <- qv[qp + idx - 1L]
      # A circular fold-back can start an alignment left of position 1, so
      # only positions inside the reference are compared.
      inside <- rpos >= 1L & rpos <= length(refv)
      rb <- rep(NA_character_, length(rpos))
      rb[inside] <- refv[rpos[inside]]
      hit <- which(!is.na(qb) & !is.na(rb) & qb != rb)
      if (length(hit) > 0L) {
        mm[[length(mm) + 1L]] <- data.frame(
          pos = rpos[hit], base = qb[hit], stringsAsFactors = FALSE
        )
      }
      rp <- rp + L
      qp <- qp + L
    } else if (o == "I") {
      ins[[length(ins) + 1L]] <- data.frame(pos = rp - 1L, len = L)
      qp <- qp + L
    } else if (o %in% c("D", "N")) {
      del[[length(del) + 1L]] <- data.frame(start = rp, end = rp + L - 1L)
      rp <- rp + L
    } else if (o == "S") {
      qp <- qp + L
    }
    # H and P consume neither reference nor the sequence scanBam returns.
  }
  list(
    start = as.integer(pos),
    end = rp - 1L,
    mm = if (length(mm) > 0L) do.call(rbind, mm) else NULL,
    del = if (length(del) > 0L) do.call(rbind, del) else NULL,
    ins = if (length(ins) > 0L) do.call(rbind, ins) else NULL
  )
}

#' Greedy interval packing for a stacked read view
#'
#' @param start integer vector of read start positions
#' @param end integer vector of read end positions
#' @param gap minimum bases between two reads sharing a row
#' @return integer vector of row indices, one per read
#'
#' @noRd
.mtr_stack_rows <- function(start, end, gap = 1L) {
  rows <- integer(length(start))
  last <- numeric(0)
  for (i in seq_along(start)) {
    free <- which(last < start[i] - gap)
    r <- if (length(free) > 0L) free[1] else length(last) + 1L
    rows[i] <- r
    last[r] <- end[i]
  }
  rows
}

#' Reads overlapping one reference window, stacked and annotated
#'
#' A circular reference is mapped against a BAM sequence with an extra flank
#' appended after the reference end (see `.mtr_depth_table()`), so a window
#' near position 1 also needs the reads that landed in that flank, folded
#' back by subtracting the reference length.
#'
#' @param bam path to an indexed BAM
#' @param start,end reference window, inclusive
#' @param ref_seq reference sequence as one string
#' @param seqname sequence name in the BAM; always "mapping_ref" for MapToRef
#' @param max_reads maximum stacked rows to return
#' @param ref_len reference length; the BAM sequence may be longer (circular flank)
#' @return list with reads, mm, del, ins, n_shown, n_total
#'
#' @noRd
maptoref_window_reads <- function(bam, start, end, ref_seq,
                                  seqname = "mapping_ref", max_reads = 100L,
                                  ref_len = nchar(ref_seq)) {
  empty <- list(
    reads = data.frame(read = character(0), row = integer(0),
                       start = integer(0), end = integer(0),
                       strand = character(0), stringsAsFactors = FALSE),
    mm = data.frame(row = integer(0), pos = integer(0), base = character(0),
                    stringsAsFactors = FALSE),
    del = data.frame(row = integer(0), start = integer(0), end = integer(0)),
    ins = data.frame(row = integer(0), pos = integer(0), len = integer(0)),
    n_shown = 0L, n_total = 0L
  )
  if (length(bam) != 1L || is.na(bam) || !file.exists(bam)) {
    return(empty)
  }
  if (!file.exists(paste0(bam, ".bai"))) {
    idx <- try(Rsamtools::indexBam(bam), silent = TRUE)
    if (inherits(idx, "try-error")) {
      return(empty)
    }
  }
  scan_range <- function(lo, hi) {
    param <- Rsamtools::ScanBamParam(
      which = IRanges::IRangesList(
        stats::setNames(list(IRanges::IRanges(lo, hi)), seqname)
      ),
      what = c("qname", "pos", "cigar", "seq", "strand")
    )
    h <- try(Rsamtools::scanBam(Rsamtools::BamFile(bam), param = param),
             silent = TRUE)
    if (inherits(h, "try-error") || length(h) == 0L) NULL else h[[1]]
  }
  hit <- scan_range(start, end)

  bam_len <- try({
    hdr <- Rsamtools::scanBamHeader(bam)
    hdr[[1]]$targets[[seqname]]
  }, silent = TRUE)
  if (inherits(bam_len, "try-error")) {
    bam_len <- NA_integer_
  }
  tail_len <- if (!is.na(bam_len) && !is.na(ref_len)) bam_len - ref_len else NA_integer_
  if (!is.na(tail_len) && tail_len > 0L && start <= tail_len) {
    lo2 <- start + ref_len
    hi2 <- min(end + ref_len, bam_len)
    if (lo2 <= hi2) {
      hit2 <- scan_range(lo2, hi2)
      if (!is.null(hit2) && length(hit2$pos) > 0L) {
        hit2$pos <- hit2$pos - ref_len
        hit <- if (is.null(hit)) hit2 else list(
          qname = c(hit$qname, hit2$qname),
          pos = c(hit$pos, hit2$pos),
          cigar = c(hit$cigar, hit2$cigar),
          seq = c(hit$seq, hit2$seq),
          strand = c(hit$strand, hit2$strand)
        )
      }
    }
  }

  if (is.null(hit)) {
    return(empty)
  }
  n_total <- length(hit$pos)
  if (n_total == 0L) {
    return(empty)
  }

  walks <- lapply(seq_len(n_total), function(i) {
    .mtr_cigar_walk(hit$pos[i], hit$cigar[i], hit$seq[i], ref_seq)
  })
  spans <- data.frame(
    read = as.character(hit$qname),
    start = vapply(walks, function(w) w$start, integer(1)),
    end = vapply(walks, function(w) w$end, integer(1)),
    strand = as.character(hit$strand),
    stringsAsFactors = FALSE
  )
  ord <- order(spans$start, spans$end)
  spans <- spans[ord, , drop = FALSE]
  walks <- walks[ord]
  spans$row <- .mtr_stack_rows(spans$start, spans$end)

  shown <- which(spans$row <= max_reads)
  spans <- spans[shown, , drop = FALSE]
  walks <- walks[shown]
  rownames(spans) <- NULL

  bind <- function(field, cols) {
    parts <- lapply(seq_along(walks), function(i) {
      x <- walks[[i]][[field]]
      if (is.null(x)) {
        return(NULL)
      }
      cbind(row = spans$row[i], x)
    })
    parts <- parts[!vapply(parts, is.null, logical(1))]
    if (length(parts) == 0L) {
      return(empty[[field]])
    }
    out <- do.call(rbind, parts)
    rownames(out) <- NULL
    out[, cols, drop = FALSE]
  }

  list(
    reads = spans[, c("read", "row", "start", "end", "strand")],
    mm = bind("mm", c("row", "pos", "base")),
    del = bind("del", c("row", "start", "end")),
    ins = bind("ins", c("row", "pos", "len")),
    n_shown = nrow(spans),
    n_total = n_total
  )
}
