#' Read and validate a MapToRef reference mitogenome
#'
#' Accepts a single-record GenBank file (first non-blank line starts with
#' LOCUS) or a single-record FASTA. Unlike the custom assembly database
#' parser, no organelle qualifier is required.
#'
#' @param ref_file Path to the reference file.
#' @param topology "circular" or "linear". Required for a FASTA reference.
#'   For GenBank, the LOCUS line wins when it states one; otherwise this
#'   value is used and is required.
#' @param genetic_code The sample's genetic code, used only to warn when the
#'   reference disagrees.
#' @param out_dir Directory to write the `maptoref/` working files into.
#'
#' @return A list with seq, length, topology, accession, organism,
#'   transl_table, and notes.
#' @export
maptoref_prepare_ref <- function(ref_file,
                                 topology = NA_character_,
                                 genetic_code = NA_integer_,
                                 out_dir = ".") {
  if (!file.exists(ref_file)) {
    stop("Reference file not found: ", ref_file)
  }
  lines <- gsub("\r", "", readLines(ref_file, warn = FALSE), fixed = TRUE)
  nonblank <- which(nzchar(trimws(lines)))
  if (length(nonblank) == 0L) {
    stop("Reference file is empty: ", ref_file)
  }
  first <- lines[nonblank[1]]

  if (grepl("^LOCUS", first)) {
    ref <- .mtr_read_gb(lines, topology)
    ext <- "gb"
  } else if (grepl("^>", first)) {
    ref <- .mtr_read_fasta(lines, topology)
    ext <- "fasta"
  } else {
    stop("Reference must be a GenBank file (first line starts with LOCUS) ",
         "or a FASTA (first line starts with >)")
  }

  bad <- unique(strsplit(gsub("[ACGTRYSWKMBDHVN]", "", ref$seq), "")[[1]])
  if (length(bad) > 0L) {
    stop("Reference sequence has invalid characters: ", paste(bad, collapse = " "))
  }
  ref$length <- nchar(ref$seq)
  if (ref$length < 5000L || ref$length > 50000L) {
    stop("Reference length ", ref$length, " is outside the accepted range ",
         "[5000, 50000]; this does not look like a mitogenome")
  }

  notes <- character(0)
  if (ref$length < 10000L || ref$length > 25000L) {
    notes <- c(notes, paste0(
      "Reference length ", ref$length,
      " is outside the usual mitogenome range [10000, 25000]."))
  }
  amb <- nchar(gsub("[ACGT]", "", ref$seq))
  if (amb > 0.01 * ref$length) {
    notes <- c(notes, paste0(
      "Reference has ", amb, " ambiguous bases (", round(100 * amb / ref$length, 1),
      "%); mapping is weaker there."))
  }
  gc_int <- suppressWarnings(as.integer(genetic_code))
  if (!is.na(gc_int) && !is.na(ref$transl_table) && gc_int != ref$transl_table) {
    notes <- c(notes, paste0(
      "Reference genetic code ", ref$transl_table, " differs from the sample's ",
      gc_int, "; annotation uses the sample's."))
  }
  ref$notes <- notes

  work <- file.path(out_dir, "maptoref")
  dir.create(work, recursive = TRUE, showWarnings = FALSE)
  writeLines(
    c(paste0(">", ref$accession, " ", ref$topology), ref$seq),
    file.path(work, "ref.fasta")
  )
  file.copy(ref_file, file.path(work, paste0("reference.", ext)), overwrite = TRUE)
  ref
}

#' @noRd
.mtr_read_gb <- function(lines, topology) {
  ends <- which(trimws(lines) == "//")
  if (length(ends) != 1L) {
    stop("Reference must contain exactly one record; this file has ",
         length(ends), ". The MitoFinder database format is not accepted here.")
  }
  block <- lines[1:ends[1]]

  locus <- grep("^LOCUS", block, value = TRUE)[1]
  tokens <- strsplit(trimws(locus), "\\s+")[[1]]
  locus_topology <- if (any(tolower(tokens) == "circular")) {
    "circular"
  } else if (any(tolower(tokens) == "linear")) {
    "linear"
  } else {
    NA_character_
  }
  topology <- if (!is.na(locus_topology)) locus_topology else .mtr_validate_topology(topology)

  accession <- .cadb_grab_version(block)
  if (is.na(accession)) {
    accession <- tokens[2]
  }
  organism <- trimws(sub("^DEFINITION\\s*", "", .cadb_grab_definition(block)))

  tt <- grep("/transl_table=", block, fixed = TRUE, value = TRUE)
  transl_table <- if (length(tt) == 0L) {
    NA_integer_
  } else {
    suppressWarnings(as.integer(sub('.*/transl_table=([0-9]+).*', "\\1", tt[1])))
  }

  origin <- grep("^ORIGIN", block)
  if (length(origin) == 0L || origin[1] >= length(block) - 1L) {
    stop("Reference GenBank record has no ORIGIN sequence")
  }
  seq_lines <- block[(origin[1] + 1L):(length(block) - 1L)]
  seq <- toupper(gsub("[^A-Za-z-]", "", paste(seq_lines, collapse = "")))
  if (!nzchar(seq)) {
    stop("Reference GenBank record has an empty ORIGIN sequence")
  }

  list(seq = seq, topology = topology, accession = accession,
       organism = organism, transl_table = transl_table)
}

#' @noRd
.mtr_validate_topology <- function(topology) {
  if (is.na(topology) || !nzchar(trimws(topology))) {
    stop("Set the reference topology (circular or linear) for a FASTA reference.")
  }
  topology <- tolower(trimws(topology))
  if (!topology %in% c("circular", "linear")) {
    stop("Reference topology must be circular or linear, not: ", topology)
  }
  topology
}

#' @noRd
.mtr_read_fasta <- function(lines, topology) {
  heads <- grep("^>", lines)
  if (length(heads) != 1L) {
    stop("Reference must contain exactly one record; this file has ",
         length(heads), ".")
  }
  topology <- .mtr_validate_topology(topology)
  header <- sub("^>", "", lines[heads[1]])
  accession <- strsplit(trimws(header), "\\s+")[[1]][1]
  seq <- if (heads[1] >= length(lines)) {
    ""
  } else {
    toupper(gsub("[^A-Za-z-]", "",
                 paste(lines[(heads[1] + 1L):length(lines)], collapse = "")))
  }
  if (!nzchar(seq)) {
    stop("Reference FASTA record has no sequence")
  }
  list(seq = seq, topology = topology, accession = accession,
       organism = trimws(header), transl_table = NA_integer_)
}

#' @noRd
.mtr_fill <- function(raw, prev) {
  a <- strsplit(raw, "", fixed = TRUE)[[1]]
  b <- strsplit(prev, "", fixed = TRUE)[[1]]
  if (length(a) != length(b)) {
    stop("consensus and reference must be the same length: ",
         length(a), " vs ", length(b))
  }
  hit <- a %in% c("N", "n", "*")
  a[hit] <- b[hit]
  paste(a, collapse = "")
}

# The first F/2 positions of the reference copy have structurally low depth,
# so their calls are taken from the appended copy instead.
#' @noRd
.mtr_splice <- function(x, len, flank) {
  if (length(x) != len + flank) {
    stop("expected ", len + flank, " positions, got ", length(x))
  }
  if (flank %% 2L != 0L) {
    stop("flank must be even, got ", flank)
  }
  if (flank %/% 2L > len) {
    stop("flank ", flank, " exceeds reference length ", len)
  }
  if (flank == 0L) {
    return(x[seq_len(len)])
  }
  half <- flank %/% 2L
  c(x[(len + 1L):(len + half)], x[(half + 1L):len])
}

# samtools consensus --mark-ins prefixes an inserted base with "_". Lowercase
# letters are base-versus-gap codes and appear at any position, so case must
# not be used to detect insertions.
#' @noRd
.mtr_parse_marked <- function(s) {
  ch <- strsplit(s, "", fixed = TRUE)[[1]]
  n <- length(ch)
  tokens <- character(n)
  k <- 0L
  i <- 1L
  while (i <= n) {
    if (ch[i] == "_") {
      if (k == 0L) {
        stop("consensus begins with an insertion mark")
      }
      if (i == n) {
        stop("consensus ends with an incomplete insertion mark")
      }
      tokens[k] <- paste0(tokens[k], ch[i], ch[i + 1L])
      i <- i + 2L
    } else {
      k <- k + 1L
      tokens[k] <- ch[i]
      i <- i + 1L
    }
  }
  tokens[seq_len(k)]
}

#' @noRd
.mtr_tokens_to_seq <- function(tokens) {
  flat <- strsplit(paste(tokens, collapse = ""), "", fixed = TRUE)[[1]]
  flat <- flat[!flat %in% c("*", "_")]
  half <- grepl("^[a-z]$", flat)
  flat[half] <- "N"
  list(seq = toupper(paste(flat, collapse = "")), half_deletions = sum(half))
}

#' @noRd
.mtr_strip_ends <- function(seq) {
  sub("N+$", "", sub("^N+", "", seq))
}

#' @noRd
.mtr_check_consensus_opts <- function(opts, circular) {
  opts <- if (is.null(opts) || length(opts) == 0L || is.na(opts)) "" else trimws(opts)
  notes <- character(0)
  error <- NA_character_

  if (grepl("['\"]", opts)) {
    return(list(ok = FALSE, notes = notes,
                error = "consensus options must not contain quote characters"))
  }

  tokens <- strsplit(opts, "\\s+")[[1]]
  tokens <- tokens[nzchar(tokens)]

  # samtools uses getopt_long, so "--flag=value" and attached short values
  # ("-ovalue") are both legal; normalize every token to a flag/value pair
  # before matching, rather than matching raw tokens.
  recs <- list()
  i <- 1L
  n <- length(tokens)
  while (i <= n) {
    tok <- tokens[i]
    eq <- regexpr("=", tok, fixed = TRUE)
    if (eq > 0L) {
      flag <- substr(tok, 1L, eq - 1L)
      value <- substr(tok, eq + 1L, nchar(tok))
      i <- i + 1L
    } else if (!grepl("^--", tok) && nchar(tok) > 2L) {
      flag <- substr(tok, 1L, 2L)
      value <- substr(tok, 3L, nchar(tok))
      i <- i + 1L
    } else {
      flag <- tok
      value <- NA_character_
      if (i < n && !grepl("^-", tokens[i + 1L])) {
        value <- tokens[i + 1L]
        i <- i + 2L
      } else {
        i <- i + 1L
      }
    }
    recs[[length(recs) + 1L]] <- list(flag = flag, value = value)
  }
  flags <- vapply(recs, function(r) r$flag, character(1))

  refused <- c("-a", "-A", "-T", "-o", "-f", "-r",
               "--show-del", "--show-ins", "--mark-ins", "--no-use-MQ")
  hit <- refused[refused %in% flags]
  if (length(hit) > 0L) {
    error <- paste0("Consensus options set by MitoPilot cannot be given here: ",
                    paste(hit, collapse = " "))
  }

  mode_only <- c("-c", "-H", "-q")[c("-c", "-H", "-q") %in% flags]
  has_m_simple <- any(vapply(recs, function(r) {
    identical(r$flag, "-m") && identical(r$value, "simple")
  }, logical(1)))
  if (length(mode_only) > 0L && !has_m_simple) {
    notes <- c(notes, paste0(
      "Consensus options ", paste(mode_only, collapse = " "),
      " were ignored; they only apply with -m simple."))
  }

  mq <- Filter(function(r) identical(r$flag, "--min-MQ"), recs)
  if (length(mq) > 0L) {
    value <- suppressWarnings(as.numeric(mq[[1]]$value))
    if (!is.na(value) && value > 0) {
      if (isTRUE(circular)) {
        error <- paste0(
          "--min-MQ above 0 blanks the origin of a circular reference; ",
          "reads inside the duplicated block carry mapping quality 1.")
      } else {
        notes <- c(notes, paste0(
          "--min-MQ ", value, " discards multi-mapping reads; ",
          "mapping quality carries little signal against a mitogenome reference."))
      }
    }
  }

  list(ok = is.na(error), notes = notes, error = error)
}

# Two terms: the sequence has settled AND reads have stopped being recruited.
#' @noRd
.mtr_stop <- function(bases_changed, reads_now, reads_prev) {
  denom <- max(as.numeric(reads_prev), 1)
  isTRUE(bases_changed < 5L &&
           abs(as.numeric(reads_now) - as.numeric(reads_prev)) / denom < 0.001)
}
