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

#' Map-to-reference mitogenome assembly
#'
#' Maps a sample's reads to a reference mitogenome, feeds the consensus back in
#' as the next mapping reference until it stops changing, then calls the
#' published sequence from a final pass over all reads. The reference base never
#' enters the published sequence.
#'
#' @param id Sample ID.
#' @param ref Path to the reference (.gb or FASTA, one record).
#' @param reads_1,reads_2 Preprocessed paired reads.
#' @param bowtie2_opts Flags passed verbatim to bowtie2.
#' @param consensus_opts Flags passed to samtools consensus after validation.
#' @param iter_cap Maximum number of iteration passes.
#' @param topology "circular" or "linear"; required for a FASTA reference,
#'   ignored for GenBank.
#' @param genetic_code The sample's genetic code, used only for a warning.
#' @param cpus Threads.
#' @param out_dir Output directory.
#'
#' @return invisibly TRUE on success, FALSE after writing the failure sentinel.
#' @export
map_to_ref <- function(id, ref, reads_1, reads_2,
                       bowtie2_opts = "--very-sensitive-local",
                       consensus_opts = "-d 3 --min-BQ 20",
                       iter_cap = 5,
                       topology = NA_character_,
                       genetic_code = NA_integer_,
                       cpus = 4,
                       out_dir = ".") {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  log_fn <- file.path(out_dir, "assembler.log.txt")
  if (!file.exists(log_fn)) {
    file.create(log_fn)
  }
  # The option strings are interpolated into an Rscript -e call, so a quote
  # character breaks the R expression. consensus_opts is covered by
  # .mtr_check_consensus_opts().
  bowtie2_opts <- .mtr_opts(bowtie2_opts)
  if (grepl("['\"]", bowtie2_opts)) {
    .mtr_fail(id, out_dir, log_fn,
              "bowtie2 options must not contain quote characters")
    return(invisible(FALSE))
  }
  ok <- tryCatch(
    {
      .mtr_assemble(id, ref, reads_1, reads_2, bowtie2_opts, consensus_opts,
                    as.integer(iter_cap), topology, genetic_code,
                    as.integer(cpus), out_dir, log_fn)
      TRUE
    },
    error = function(e) {
      .mtr_fail(id, out_dir, log_fn, conditionMessage(e))
      FALSE
    }
  )
  invisible(ok)
}

#' @noRd
.mtr_log <- function(log_fn, ...) {
  cat(paste0(..., "\n"), file = log_fn, append = TRUE)
}

# An absent, empty, or NA option string is an empty option list.
#' @noRd
.mtr_opts <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x[1])) "" else as.character(x[1])
}

# bash -o pipefail so a failed bowtie2 stage is not masked by a later stage that
# exits 0. The whole command is grouped so every stage's stderr reaches the log.
#' @noRd
.mtr_run <- function(cmd, log_fn) {
  .mtr_log(log_fn, "+ ", cmd)
  full <- paste0("{ ", cmd, " ; } 2>> ", shQuote(log_fn))
  status <- system2("bash", c("-o", "pipefail", "-c", shQuote(full)))
  if (status != 0L) {
    stop("command failed (exit ", status, "): ", cmd)
  }
  invisible(TRUE)
}

# samtools consensus line-wraps its output, so every read of a consensus FASTA
# unwraps before indexing.
#' @noRd
.mtr_read_seq <- function(fn) {
  lines <- readLines(fn, warn = FALSE)
  paste(lines[!grepl("^>", lines)], collapse = "")
}

#' @noRd
.mtr_extend <- function(seq, flank) {
  if (flank == 0L) seq else paste0(seq, substr(seq, 1L, flank))
}

#' @noRd
.mtr_count_primary <- function(bam) {
  out <- suppressWarnings(system2(
    "samtools", c("view", "-c", "-F", "0x904", shQuote(bam)),
    stdout = TRUE, stderr = FALSE
  ))
  value <- suppressWarnings(as.integer(out[1]))
  if (is.na(value)) 0L else value
}

# Primary alignments whose reference span crosses the seam at position len.
# The region query keeps the whole BAM out of R; only alignments that overlap
# the seam are returned.
#' @noRd
.mtr_junction_depth <- function(bam, len, refname = "mapping_ref",
                                min_overhang = 30L) {
  region <- paste0(refname, ":", len, "-", len)
  sam <- suppressWarnings(system2(
    "samtools", c("view", "-F", "0x904", shQuote(bam), shQuote(region)),
    stdout = TRUE, stderr = FALSE
  ))
  # A failed query must not read as "no reads span the seam".
  status <- attr(sam, "status")
  if (!is.null(status) && status != 0L) {
    stop("samtools view failed (exit ", status, ") for region ", region)
  }
  if (length(sam) == 0L) {
    return(0L)
  }
  fields <- stringr::str_split(sam, "\t", simplify = TRUE)
  starts <- suppressWarnings(as.integer(fields[, 4]))
  ends <- starts + cigar_ref_length(fields[, 6]) - 1L
  ok <- !is.na(starts) & !is.na(ends)
  sum(ok & starts <= len - min_overhang & ends >= len + min_overhang)
}

#' @noRd
.mtr_diff_count <- function(a, b) {
  x <- strsplit(a, "", fixed = TRUE)[[1]]
  y <- strsplit(b, "", fixed = TRUE)[[1]]
  n <- min(length(x), length(y))
  sum(x[seq_len(n)] != y[seq_len(n)]) + abs(length(x) - length(y))
}

#' @noRd
.mtr_fail <- function(id, out_dir, log_fn, reason) {
  .mtr_log(log_fn, "FAILED: ", reason)
  writeLines(">No assembly found",
             file.path(out_dir, paste0(id, "_assembly_0.fasta")))
  writeLines(c("assembler=MapToRef", paste0("failure=", reason)),
             file.path(out_dir, paste0(id, "_summary.txt")))
  invisible(FALSE)
}

#' @noRd
.mtr_assemble <- function(id, ref_file, reads_1, reads_2, bowtie2_opts,
                          consensus_opts, iter_cap, topology, genetic_code,
                          cpus, out_dir, log_fn) {
  ref <- maptoref_prepare_ref(ref_file, topology = topology,
                              genetic_code = genetic_code, out_dir = out_dir)
  work <- file.path(out_dir, "maptoref")
  notes <- ref$notes
  circular <- identical(ref$topology, "circular")
  len <- ref$length
  flank <- if (circular) min(500L, len %/% 2L) else 0L
  .mtr_log(log_fn, "reference ", ref$accession, " ", ref$organism,
           " (", len, " bp, ", ref$topology, ")")

  check <- .mtr_check_consensus_opts(consensus_opts, circular)
  if (!check$ok) {
    stop(check$error)
  }
  notes <- c(notes, check$notes)
  user_cons <- .mtr_opts(consensus_opts)
  fixed_cons <- paste("-a -A --no-use-MQ --show-del yes -@", cpus)

  ref_fa <- file.path(work, "ref_0.fa")
  writeLines(c(">mapping_ref", .mtr_extend(ref$seq, flank)), ref_fa)
  prev_ref <- .mtr_extend(ref$seq, flank)
  prev_cons <- ref$seq

  idx <- file.path(work, "idx")
  bam <- file.path(work, "pass_1.bam")
  .mtr_run(stringr::str_glue(
    "bowtie2-build -q {shQuote(ref_fa)} {shQuote(idx)}"
  ), log_fn)
  # No --no-unal: it would drop the unmapped mate of a half-mapped pair, and
  # recruitment below would then keep only fully mapped pairs.
  .mtr_run(stringr::str_glue(
    "bowtie2 {bowtie2_opts} -x {shQuote(idx)} -1 {shQuote(reads_1)} ",
    "-2 {shQuote(reads_2)} --threads {cpus} 2>> {shQuote(log_fn)} ",
    "| samtools view -b -G 12 - | samtools sort -@ {cpus} -o {shQuote(bam)} -"
  ), log_fn)

  reads_pass_1 <- .mtr_count_primary(bam)
  if (reads_pass_1 < 100L) {
    stop(reads_pass_1, " reads mapped to the reference; use a closer reference ",
         "or a more sensitive preset")
  }
  if (reads_pass_1 < 1000L) {
    notes <- c(notes, paste0(
      "Only ", reads_pass_1, " reads mapped; check that the reference is a ",
      "mitogenome from a related taxon."))
  }

  sub_1 <- file.path(work, "sub_R1.fq")
  sub_2 <- file.path(work, "sub_R2.fq")
  .mtr_run(stringr::str_glue(
    "samtools sort -n {shQuote(bam)} ",
    "| samtools fastq -1 {shQuote(sub_1)} -2 {shQuote(sub_2)} ",
    "-0 /dev/null -s /dev/null -n"
  ), log_fn)

  iters <- data.frame()
  reads_prev <- reads_pass_1
  stop_reason <- "cap"
  passes <- 0L

  for (k in seq_len(max(1L, iter_cap))) {
    passes <- k
    raw <- file.path(work, paste0("raw_", k, ".fa"))
    .mtr_run(stringr::str_glue(
      "samtools consensus {fixed_cons} --show-ins no {user_cons} ",
      "{shQuote(bam)} > {shQuote(raw)}"
    ), log_fn)

    raw_seq <- .mtr_read_seq(raw)
    filled <- .mtr_fill(raw_seq, prev_ref)
    cons <- paste(
      .mtr_splice(strsplit(filled, "", fixed = TRUE)[[1]], len, flank),
      collapse = ""
    )
    writeLines(c(">cons", cons), file.path(work, paste0("cons_", k, ".fa")))

    reads_now <- .mtr_count_primary(bam)
    bases_changed <- .mtr_diff_count(cons, prev_cons)
    done <- .mtr_stop(bases_changed, reads_now, reads_prev) || k >= iter_cap
    if (done) {
      stop_reason <- if (k >= iter_cap &&
                         !.mtr_stop(bases_changed, reads_now, reads_prev)) {
        "cap"
      } else {
        "converged"
      }
    }
    iters <- rbind(iters, data.frame(
      pass = k,
      reads_mapped = reads_now,
      bases_changed = bases_changed,
      n_count = nchar(gsub("[^N]", "", raw_seq)),
      stop_reason = if (done) stop_reason else NA_character_
    ))

    prev_cons <- cons
    prev_ref <- .mtr_extend(cons, flank)
    reads_prev <- reads_now
    if (done) {
      break
    }

    ref_fa <- file.path(work, paste0("ref_", k, ".fa"))
    writeLines(c(">mapping_ref", prev_ref), ref_fa)
    idx <- file.path(work, paste0("idx_", k))
    bam <- file.path(work, paste0("pass_", k + 1L, ".bam"))
    .mtr_run(stringr::str_glue(
      "bowtie2-build -q {shQuote(ref_fa)} {shQuote(idx)}"
    ), log_fn)
    .mtr_run(stringr::str_glue(
      "bowtie2 {bowtie2_opts} --no-unal -x {shQuote(idx)} -1 {shQuote(sub_1)} ",
      "-2 {shQuote(sub_2)} --threads {cpus} 2>> {shQuote(log_fn)} ",
      "| samtools sort -@ {cpus} -o {shQuote(bam)} -"
    ), log_fn)
  }
  utils::write.table(iters, file.path(work, "iterations.tsv"),
                     sep = "\t", row.names = FALSE, quote = FALSE)

  # Final pass: all reads against the converged reference. Reads that only
  # become mappable after the reference has moved are exactly the ones the
  # loop exists to reach.
  final_ref <- file.path(work, "ref_final.fa")
  writeLines(c(">mapping_ref", prev_ref), final_ref)
  final_idx <- file.path(work, "idx_final")
  final_bam <- file.path(work, "final.bam")
  .mtr_run(stringr::str_glue(
    "bowtie2-build -q {shQuote(final_ref)} {shQuote(final_idx)}"
  ), log_fn)
  .mtr_run(stringr::str_glue(
    "bowtie2 {bowtie2_opts} --no-unal -x {shQuote(final_idx)} ",
    "-1 {shQuote(reads_1)} -2 {shQuote(reads_2)} --threads {cpus} ",
    "2>> {shQuote(log_fn)} | samtools sort -@ {cpus} -o {shQuote(final_bam)} -"
  ), log_fn)
  reads_final <- .mtr_count_primary(final_bam)
  # The index exists only to serve the seam query.
  junction_depth <- NA_integer_
  if (circular) {
    .mtr_run(stringr::str_glue("samtools index {shQuote(final_bam)}"), log_fn)
    junction_depth <- .mtr_junction_depth(final_bam, len)
  }

  final_raw <- file.path(work, "final_raw.fa")
  final_subs <- file.path(work, "final_subs.fa")
  .mtr_run(stringr::str_glue(
    "samtools consensus {fixed_cons} --show-ins yes --mark-ins {user_cons} ",
    "{shQuote(final_bam)} > {shQuote(final_raw)}"
  ), log_fn)
  .mtr_run(stringr::str_glue(
    "samtools consensus {fixed_cons} --show-ins no {user_cons} ",
    "{shQuote(final_bam)} > {shQuote(final_subs)}"
  ), log_fn)

  tokens <- .mtr_splice(.mtr_parse_marked(.mtr_read_seq(final_raw)), len, flank)
  product <- .mtr_tokens_to_seq(tokens)
  seq <- product$seq

  published <- ref$topology
  if (circular && !is.na(junction_depth) && junction_depth == 0L) {
    published <- "linear"
    notes <- c(notes, paste0(
      "No reads span the start and end of the sequence, so this assembly is ",
      "published as linear even though the reference is circular. Add reads or ",
      "use a closer reference, or edit the topology if you are confident the ",
      "molecule is circular."))
  }
  # Gated on the published topology, so a downgraded assembly is trimmed too.
  if (!identical(published, "circular")) {
    seq <- .mtr_strip_ends(seq)
  }

  subs <- paste(
    .mtr_splice(strsplit(.mtr_read_seq(final_subs), "", fixed = TRUE)[[1]],
                len, flank),
    collapse = ""
  )
  writeLines(c(paste0(">", id, ".1.1 subs_only"), subs),
             file.path(work, "subs_only.fasta"))

  n_count <- nchar(gsub("[^N]", "", seq))
  n_pct <- round(100 * n_count / nchar(seq), 1)
  if (n_count > 0.50 * nchar(seq)) {
    notes <- c(notes, paste0(
      n_pct, "% of the product is N; the reference may be too divergent for ",
      "this sample."))
  } else if (n_count > 0.02 * nchar(seq)) {
    notes <- c(notes, paste0(
      n_pct, "% of the reference could not be called (N)."))
  }
  if (identical(stop_reason, "cap")) {
    notes <- c(notes, paste0(
      "Still changing after ", passes, " passes; raise the cap (10 to 25) and ",
      "re-run."))
  }
  # Position-wise over the common length, with uncalled sites excluded, so an
  # internal N run cannot shift the comparison frame. Lowercase is a
  # base-versus-gap call, which is uncalled for this diagnostic.
  a <- strsplit(subs, "", fixed = TRUE)[[1]]
  b <- strsplit(ref$seq, "", fixed = TRUE)[[1]]
  n <- min(length(a), length(b))
  a <- a[seq_len(n)]
  b <- b[seq_len(n)]
  keep <- !(a %in% c("N", "*")) & !grepl("[a-z]", a)
  subs_diff <- sum(toupper(a[keep]) != toupper(b[keep]))
  if (subs_diff > 0.10 * ref$length) {
    notes <- c(notes, paste0(
      "Reference is more than 10% divergent; expect reference bias and missing ",
      "regions. Use a closer reference, a more sensitive preset, or compare ",
      "with a de novo set."))
  }

  writeLines(c(paste0(">", id, ".1.1 ", published), seq),
             file.path(out_dir, paste0(id, "_assembly_1.fasta")))
  writeLines(c(
    "assembler=MapToRef",
    paste0("accession=", ref$accession),
    paste0("organism=", ref$organism),
    paste0("reference_length=", len),
    paste0("reference_topology=", ref$topology),
    paste0("published_topology=", published),
    paste0("transl_table=", ref$transl_table),
    paste0("passes_run=", passes),
    paste0("stop_reason=", stop_reason),
    paste0("reads_mapped_pass_1=", reads_pass_1),
    paste0("reads_mapped_final=", reads_final),
    paste0("junction_depth=", ifelse(is.na(junction_depth), "NA", junction_depth)),
    paste0("consensus_length=", nchar(seq)),
    paste0("n_count=", n_count),
    paste0("iupac_count=", nchar(gsub("[ACGTN]", "", seq))),
    paste0("half_deletions=", product$half_deletions),
    paste0("substitutions_vs_reference=", subs_diff),
    paste0("note=", notes)
  ), file.path(out_dir, paste0(id, "_summary.txt")))

  # Reproducible transients, dropped so the published loop record stays the
  # small file set of design 4.11. A failed run keeps everything.
  unlink(list.files(work, pattern = "\\.(bam|bai|bt2|bt2l|fq)$", full.names = TRUE))

  invisible(TRUE)
}
