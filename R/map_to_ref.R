#' Read and validate a MapToRef reference mitogenome
#'
#' Accepts a single-record GenBank file (first non-blank line starts with
#' LOCUS) or a single-record FASTA. Unlike the custom assembly database
#' parser, no organelle qualifier is required.
#'
#' @param ref_file Path to the reference file.
#' @param topology "circular" or "linear". Required for a FASTA reference,
#'   ignored for GenBank, where the LOCUS line wins.
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
    ref <- .mtr_read_gb(lines)
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
  if (!is.na(genetic_code) && !is.na(ref$transl_table) &&
      as.integer(genetic_code) != ref$transl_table) {
    notes <- c(notes, paste0(
      "Reference genetic code ", ref$transl_table, " differs from the sample's ",
      genetic_code, "; annotation uses the sample's."))
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
.mtr_read_gb <- function(lines) {
  ends <- which(trimws(lines) == "//")
  if (length(ends) != 1L) {
    stop("Reference must contain exactly one record; this file has ",
         length(ends), ". The MitoFinder database format is not accepted here.")
  }
  block <- lines[1:ends[1]]

  locus <- grep("^LOCUS", block, value = TRUE)[1]
  tokens <- strsplit(trimws(locus), "\\s+")[[1]]
  topology <- if (any(tolower(tokens) == "circular")) "circular" else "linear"

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
  if (length(origin) == 0L || origin[1] >= length(block)) {
    stop("Reference GenBank record has no ORIGIN sequence")
  }
  seq_lines <- block[(origin[1] + 1L):(length(block) - 1L)]
  seq <- toupper(gsub("[^A-Za-z]", "", paste(seq_lines, collapse = "")))
  if (!nzchar(seq)) {
    stop("Reference GenBank record has an empty ORIGIN sequence")
  }

  list(seq = seq, topology = topology, accession = accession,
       organism = organism, transl_table = transl_table)
}

#' @noRd
.mtr_read_fasta <- function(lines, topology) {
  heads <- grep("^>", lines)
  if (length(heads) != 1L) {
    stop("Reference must contain exactly one record; this file has ",
         length(heads), ".")
  }
  if (is.na(topology) || !nzchar(trimws(topology))) {
    stop("Set the reference topology (circular or linear) for a FASTA reference.")
  }
  topology <- tolower(trimws(topology))
  if (!topology %in% c("circular", "linear")) {
    stop("Reference topology must be circular or linear, not: ", topology)
  }
  header <- sub("^>", "", lines[heads[1]])
  accession <- strsplit(trimws(header), "\\s+")[[1]][1]
  seq <- toupper(gsub("[^A-Za-z-]", "",
                      paste(lines[(heads[1] + 1L):length(lines)], collapse = "")))
  if (!nzchar(seq)) {
    stop("Reference FASTA record has no sequence")
  }
  list(seq = seq, topology = topology, accession = accession,
       organism = trimws(header), transl_table = NA_integer_)
}
