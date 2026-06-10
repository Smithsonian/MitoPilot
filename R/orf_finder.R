#' Find open reading frames in unannotated regions
#'
#' Optional annotation step (WF2) that runs after curation/validation. Uses NCBI
#' ORFfinder to locate open reading frames, retains only those falling in regions
#' without an existing annotation (allowing a user-set degree of overlap), keeps
#' the longest ORF when several overlap the same unannotated region, and BLASTs
#' each surviving ORF against the combined curation protein database so the
#' annotate-details modal can show candidate gene identities. ORFs are written as
#' annotations named `ORF.1`, `ORF.2`, ... with `type = "ORF"`.
#'
#' @param annotations_fn Path to the (final, validated) annotations TSV.
#' @param assembly_fn Path to the curated assembly FASTA.
#' @param genetic_code NCBI translation table number (default: "2").
#' @param orffinder_opts Extra ORFfinder command-line options (default:
#'   "-s 1 -n true"). `-g` and `-ml` are supplied from `genetic_code`/`orf_min_len`.
#' @param orffinder_condaenv Conda environment containing ORFfinder (default:
#'   "orffinder"). Set NULL to use ORFfinder on the PATH.
#' @param orf_min_len Minimal ORF length in nucleotides (default: 300).
#' @param orf_max_overlap Maximum overlap with existing annotations, as a fraction
#'   of the ORF length, before an ORF is discarded (default: 0.1).
#' @param ref_dir Path to the curation reference directory (must contain a
#'   `featureProt/` subdirectory of per-gene protein FASTAs).
#' @param max_blast_hits Maximum number of BLAST hits to retain per ORF (default: 10).
#' @param blast_condaenv Conda environment containing blastp/makeblastdb (default:
#'   "base"). Set NULL to use them on the PATH.
#' @param out_dir Output directory for the ORF annotations TSV.
#'
#' @export
#'
orf_finder <- function(
  annotations_fn = NULL,
  assembly_fn = NULL,
  genetic_code = "2",
  orffinder_opts = "-s 1 -n true",
  orffinder_condaenv = "orffinder",
  orf_min_len = 300,
  orf_max_overlap = 0.1,
  ref_dir = ".",
  max_blast_hits = 10,
  blast_condaenv = "base",
  out_dir = NULL
) {
  orf_min_len <- as.integer(orf_min_len)
  orf_max_overlap <- as.numeric(orf_max_overlap)

  # Output column order must match what VALIDATE writes to the annotations table.
  out_cols <- c(
    "contig", "type", "gene", "product", "pos1", "pos2", "length", "direction",
    "start_codon", "stop_codon", "anticodon", "tool", "notes", "warnings",
    "translation", "refHits"
  )
  empty_out <- function() {
    setNames(
      data.frame(matrix(character(0), ncol = length(out_cols))),
      out_cols
    )
  }
  write_out <- function(df) {
    fn <- file.path(
      out_dir,
      stringr::str_replace(basename(assembly_fn), "\\w+$", "tsv") |>
        stringr::str_replace("assembly", "ORFannotations")
    )
    # quote = "none" so the JSON refHits field is written raw; default CSV
    # quoting doubles its inner quotes and nextflow's splitCsv does not
    # un-escape them, producing invalid JSON in the db (matches validate_*).
    readr::write_tsv(df, fn, na = "", quote = "none")
    invisible(df)
  }

  annotations <- readr::read_tsv(annotations_fn, show_col_types = FALSE) |>
    as.data.frame()

  assembly <- Biostrings::readDNAStringSet(assembly_fn)
  names(assembly) <- stringr::str_extract(names(assembly), "\\S+")
  contig_lens <- setNames(Biostrings::width(assembly), names(assembly))

  # Run ORFfinder ----
  fasta <- tempfile(fileext = ".fa")
  Biostrings::writeXStringSet(assembly, fasta)
  out <- tempfile(fileext = ".faa")

  message("starting ORFfinder")
  orf_args <- c(
    "-in", fasta,
    "-g", as.character(genetic_code),
    "-ml", as.character(orf_min_len),
    strsplit(orffinder_opts, "\\s+")[[1]],
    "-outfmt", "0",
    "-out", out
  )
  if (!is.null(orffinder_condaenv)) {
    reticulate::conda_run2(
      cmd = "ORFfinder", args = orf_args, envname = orffinder_condaenv, echo = FALSE
    )
  } else {
    system2("ORFfinder", args = orf_args)
  }

  if (!file.exists(out) || file.info(out)$size == 0) {
    return(write_out(empty_out()))
  }
  peptides <- Biostrings::readAAStringSet(out)
  if (length(peptides) == 0) {
    return(write_out(empty_out()))
  }

  # Parse ORFfinder deflines: lcl|ORF<n>_<seqid>:<start>:<stop> <desc>
  # start<stop => plus strand; start>stop => minus strand. Range includes the
  # stop codon (for complete ORFs); the peptide excludes it. ORFfinder reports
  # 0-based inclusive coordinates, so add 1 to convert to the 1-based positions
  # used throughout MitoPilot.
  ids <- stringr::str_extract(names(peptides), "^\\S+")
  m <- stringr::str_match(ids, "^lcl\\|ORF\\d+_(.+):(\\d+):(\\d+)$")
  start <- as.integer(m[, 3]) + 1L
  stop <- as.integer(m[, 4]) + 1L
  orfs <- data.frame(
    contig = m[, 2],
    type = "ORF",
    pos1 = pmin(start, stop),
    pos2 = pmax(start, stop),
    direction = ifelse(start < stop, "+", "-"),
    translation = as.character(peptides),
    tool = "ORFfinder",
    stringsAsFactors = FALSE
  )
  orfs <- orfs[!is.na(orfs$contig) & orfs$contig %in% names(assembly), , drop = FALSE]
  orfs$length <- orfs$pos2 - orfs$pos1 + 1L

  if (nrow(orfs) == 0) {
    return(write_out(empty_out()))
  }

  # Filter to unannotated regions ----
  # Drop ORFs whose total overlap with existing annotations exceeds
  # orf_max_overlap of the ORF length. Existing deleted markers (pos1 == 0) are
  # excluded from the comparison.
  existing <- annotations[
    !is.na(annotations$pos1) & annotations$pos1 > 0,
    c("contig", "pos1", "pos2"),
    drop = FALSE
  ]
  keep <- purrr::pmap_lgl(
    list(orfs$contig, orfs$pos1, orfs$pos2),
    function(ctg, p1, p2) {
      L <- contig_lens[[ctg]]
      orf_len <- circ_len(p1, p2, L)
      hits <- existing[
        existing$contig == ctg &
          circ_overlap(p1, p2, existing$pos1, existing$pos2),
        ,
        drop = FALSE
      ]
      if (nrow(hits) == 0L) {
        return(TRUE)
      }
      total_overlap <- sum(purrr::map2_int(
        hits$pos1, hits$pos2, \(q1, q2) circ_overlap_len(p1, p2, q1, q2, L)
      ))
      (total_overlap / orf_len) <= orf_max_overlap
    }
  )
  orfs <- orfs[keep, , drop = FALSE]

  if (nrow(orfs) == 0) {
    return(write_out(empty_out()))
  }

  # Longest-per-region dedup ----
  # Among surviving ORFs, greedily keep the longest, accepting an ORF only if it
  # does not overlap an already-accepted ORF on the same contig.
  orfs <- orfs[order(-orfs$length), , drop = FALSE]
  accepted <- rep(FALSE, nrow(orfs))
  for (i in seq_len(nrow(orfs))) {
    prev <- which(accepted & orfs$contig == orfs$contig[i])
    if (length(prev) == 0L ||
        !any(circ_overlap(orfs$pos1[i], orfs$pos2[i], orfs$pos1[prev], orfs$pos2[prev]))) {
      accepted[i] <- TRUE
    }
  }
  orfs <- orfs[accepted, , drop = FALSE]

  # Order by position and name ORF.1, ORF.2, ...
  orfs <- orfs[order(orfs$contig, orfs$pos1), , drop = FALSE]
  orfs$gene <- paste0("ORF.", seq_len(nrow(orfs)))

  # Genomic start/stop codons (informational; wrap-around ORFs are not produced)
  orfs$start_codon <- NA_character_
  orfs$stop_codon <- NA_character_
  for (i in seq_len(nrow(orfs))) {
    ctg <- orfs$contig[i]
    p1 <- orfs$pos1[i]
    p2 <- orfs$pos2[i]
    L <- contig_lens[[ctg]]
    if (p1 < 1L || p2 > L || (p2 - p1 + 1L) < 3L) next
    first <- as.character(Biostrings::subseq(assembly[ctg], p1, p1 + 2L))
    last <- as.character(Biostrings::subseq(assembly[ctg], p2 - 2L, p2))
    if (orfs$direction[i] == "+") {
      orfs$start_codon[i] <- first
      orfs$stop_codon[i] <- last
    } else {
      orfs$start_codon[i] <- as.character(Biostrings::reverseComplement(Biostrings::DNAString(last)))
      orfs$stop_codon[i] <- as.character(Biostrings::reverseComplement(Biostrings::DNAString(first)))
    }
  }

  orfs$product <- NA_character_
  orfs$anticodon <- NA_character_
  orfs$notes <- NA_character_
  orfs$warnings <- NA_character_

  # BLAST each ORF against the combined gene database ----
  feature_dir <- file.path(ref_dir, "featureProt")
  fas <- list.files(feature_dir, pattern = "\\.fas$", full.names = TRUE)
  orfs$refHits <- NA_character_
  if (length(fas) > 0) {
    combined <- file.path(tempdir(), "_ORF_all.fas")
    if (file.exists(combined)) file.remove(combined)
    file.create(combined)
    for (f in fas) file.append(combined, f)
    mk_args <- c("-in", combined, "-dbtype", "prot")
    if (!is.null(blast_condaenv)) {
      system2(reticulate::conda_binary(), c("run", "-n", blast_condaenv, "makeblastdb", mk_args))
    } else {
      system2("makeblastdb", mk_args)
    }
    orfs$refHits <- orfs$translation |>
      purrr::map_chr(~ {
        get_top_hits_orf(combined, .x, max_blast_hits, condaenv = blast_condaenv) |>
          json_string() %||% "{}"
      })
  } else {
    message("orf_finder: featureProt/ not found in ref_dir; skipping ORF BLAST")
  }

  write_out(orfs[, out_cols, drop = FALSE])
}
