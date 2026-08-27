#' Select mitochondrial contigs from a BLAST screen
#'
#' Given the merged per-contig hits from the WF1 screen (all contigs of one
#' sample BLASTed against the bundled metazoan mitogenome database), pick the
#' reference the sample as a whole matches best and keep the contigs that match
#' it convincingly.
#'
#' The vote runs in rounds. Each round scores only the contigs that hit that
#' round's winning reference, then drops all of them from the pool, pass or
#' fail, so every contig is judged against exactly one reference. Rounds keep
#' the anti-splitting behaviour within a mitogenome while letting a second,
#' unrelated mitogenome (a contaminant, or a mixed sample) win its own round.
#' The search stops at the first round that selects nothing: references are
#' visited strongest first, so a reference carrying only NUMT-grade hits means
#' the weaker ones behind it carry less.
#'
#' The fraction rule is the NUMT filter: a real mitochondrial contig is almost
#' entirely mitochondrial, while a nuclear scaffold carrying a NUMT aligns over
#' a tiny slice of itself.
#'
#' @param hits data frame of BLAST hits with columns `qseqid`, `saccver`,
#'   `pident`, `length` (aligned bases), `bitscore` and `qlen`.
#' @param min_identity Percent identity required against the winning reference
#'   (default = 70)
#' @param min_aligned_length Aligned bases required (default = 300)
#' @param min_aligned_fraction Fraction of the contig the alignment must cover
#'   (default = 0.5)
#' @param max_candidates Most contigs carried into confirmation, counted across
#'   all references (default = 20)
#' @param max_references Most references the vote may award, a safety net on
#'   very messy assemblies (default = 5)
#'
#' @return a list with `accession` (the winning references in the order they
#'   won, NA when there are no hits), `candidates` (character vector of contig
#'   names, best first) and `evidence` (one row per contig scored, with the
#'   numbers behind the call and a `reason` for anything dropped)
#'
#' @export
#'
select_mito_contigs <- function(hits,
                                min_identity = 70,
                                min_aligned_length = 300,
                                min_aligned_fraction = 0.5,
                                max_candidates = 20,
                                max_references = 5) {
  empty <- data.frame(
    contig = character(0), length = integer(0), accession = character(0),
    pident = numeric(0), aligned_length = integer(0),
    aligned_fraction = numeric(0), rank = integer(0),
    selected = integer(0), reason = character(0)
  )
  if (is.null(hits) || nrow(hits) == 0L) {
    return(list(accession = NA_character_, candidates = character(0), evidence = empty))
  }

  # One row per contig/reference pair: a contig may align to a reference in
  # several pieces, and those pieces together are what covers the contig.
  per_ref <- hits |>
    dplyr::group_by(.data$qseqid, .data$saccver) |>
    dplyr::summarise(
      qlen = max(.data$qlen),
      aligned_length = sum(.data$length),
      bitscore = sum(.data$bitscore),
      pident = stats::weighted.mean(.data$pident, .data$length),
      .groups = "drop"
    )

  pool <- per_ref
  rounds <- list()
  candidates <- character(0)
  accessions <- character(0)

  for (i in seq_len(max_references)) {
    if (nrow(pool) == 0L || length(candidates) >= max_candidates) {
      break
    }

    # The contigs still in the pool vote for one reference, so a mitogenome
    # broken across several contigs is not split between near-identical
    # references.
    winner <- pool |>
      dplyr::group_by(.data$saccver) |>
      dplyr::summarise(total = sum(.data$bitscore), .groups = "drop") |>
      dplyr::arrange(dplyr::desc(.data$total)) |>
      dplyr::slice(1) |>
      dplyr::pull(.data$saccver)

    evidence <- score_against_reference(
      pool, winner,
      min_identity = min_identity,
      min_aligned_length = min_aligned_length,
      min_aligned_fraction = min_aligned_fraction,
      max_candidates = max_candidates,
      n_taken = length(candidates)
    )

    rounds[[length(rounds) + 1L]] <- evidence
    accessions <- c(accessions, winner)
    kept <- evidence$contig[evidence$selected == 1L]
    candidates <- c(candidates, kept)

    # Everything judged this round leaves the pool, pass or fail: a NUMT-bearing
    # scaffold left behind would vote for a fresh reference every round.
    pool <- pool[!(pool$qseqid %in% evidence$contig), , drop = FALSE]
    if (length(kept) == 0L) {
      break
    }
  }

  list(
    accession = accessions,
    candidates = candidates,
    evidence = do.call(rbind, rounds)
  )
}

#' Score one round of the reference vote
#'
#' @param pool contig/reference pairs still in play
#' @param winner accession this round awarded
#' @param min_identity,min_aligned_length,min_aligned_fraction,max_candidates
#'   thresholds, see [select_mito_contigs()]
#' @param n_taken candidates already selected by earlier rounds, so `rank` keeps
#'   counting and `max_candidates` stays a budget across the whole search
#'
#' @return evidence rows for every contig that hit `winner`
#'
#' @noRd
score_against_reference <- function(pool,
                                    winner,
                                    min_identity,
                                    min_aligned_length,
                                    min_aligned_fraction,
                                    max_candidates,
                                    n_taken = 0L) {
  scored <- pool |>
    dplyr::filter(.data$saccver == winner) |>
    dplyr::mutate(
      aligned_fraction = pmin(1, .data$aligned_length / .data$qlen)
    ) |>
    dplyr::arrange(dplyr::desc(.data$aligned_length))

  scored$reason <- dplyr::case_when(
    scored$pident < min_identity ~
      paste0("identity ", round(scored$pident, 1), "% below ", min_identity, "%"),
    scored$aligned_length < min_aligned_length ~
      paste0("aligned ", scored$aligned_length, " bp below ", min_aligned_length, " bp"),
    scored$aligned_fraction < min_aligned_fraction ~
      paste0(
        "hit covers ", round(100 * scored$aligned_fraction, 1),
        "% of the contig, below ", round(100 * min_aligned_fraction), "% (possible NUMT)"
      ),
    .default = NA_character_
  )

  keep <- is.na(scored$reason)
  # Rank among the contigs that passed, continuing the count from earlier
  # rounds, then apply the cap.
  scored$rank <- NA_integer_
  scored$rank[keep] <- n_taken + seq_len(sum(keep))
  over_cap <- keep & scored$rank > max_candidates
  scored$reason[over_cap] <- paste0("beyond the ", max_candidates, "-candidate cap")
  scored$selected <- as.integer(keep & !over_cap)
  scored$rank[!as.logical(scored$selected)] <- NA_integer_

  data.frame(
    contig = scored$qseqid,
    length = as.integer(scored$qlen),
    accession = winner,
    pident = round(scored$pident, 2),
    aligned_length = as.integer(scored$aligned_length),
    aligned_fraction = round(scored$aligned_fraction, 4),
    rank = scored$rank,
    selected = scored$selected,
    reason = scored$reason
  )
}

#' Read the merged BLAST screen output
#'
#' @param path file written by the screen processes, or a vector of chunk files
#'
#' @return data frame of hits, NULL if nothing was found
#'
#' @noRd
read_screen_hits <- function(path) {
  cols <- c("qseqid", "saccver", "pident", "length", "bitscore", "qlen")
  rows <- lapply(path[file.exists(path) & file.size(path) > 0L], function(p) {
    utils::read.delim(p, header = FALSE, col.names = cols, comment.char = "#")
  })
  do.call(rbind, rows)
}

#' Count mitochondrial genes per contig in MitoFinder output
#'
#' MitoFinder renames every contig it accepts to `mtDNA_contig[_i]`, so the gene
#' files cannot be mapped back by name alone. Each contig's `.infos` file records
#' its "Initial contig name", and the sibling `_genes_NT.fasta` holds that
#' contig's genes, so the two together give the count per original contig.
#'
#' @param workdir MitoFinder output directory
#'
#' @return named integer vector of gene counts, keyed by the original contig name
#'
#' @noRd
count_mitofinder_genes <- function(workdir) {
  infos <- list.files(workdir, pattern = "\\.infos$", recursive = TRUE, full.names = TRUE)
  if (length(infos) == 0L) {
    return(integer(0))
  }

  # Per-contig gene files only: the "_final_genes_NT" file is the aggregate
  # across contigs and would double-count.
  gene_files <- list.files(
    workdir,
    pattern = "_genes_NT\\.fasta$", recursive = TRUE, full.names = TRUE
  )
  gene_files <- gene_files[!grepl("_final_genes_NT\\.fasta$", gene_files)]

  counts <- integer(0)
  for (info in infos) {
    line <- grep("^Initial contig name:", readLines(info, warn = FALSE), value = TRUE)
    if (length(line) == 0L) {
      next
    }
    contig <- trimws(sub("^Initial contig name:", "", line[1]))
    if (!nzchar(contig)) {
      next
    }

    # Multi-contig runs name the gene file after the .infos file; a single-contig
    # run names the .infos after the job instead, leaving exactly one gene file.
    expected <- sub("\\.infos$", "_genes_NT.fasta", info)
    f <- if (file.exists(expected)) {
      expected
    } else if (length(gene_files) == 1L) {
      gene_files
    } else {
      NA_character_
    }
    counts[contig] <- if (is.na(f)) 0L else length(grep("^>", readLines(f, warn = FALSE)))
  }
  counts
}

#' Confirm screened candidates with MitoFinder gene counts
#'
#' @param candidates character vector of contig names from the screen
#' @param gene_counts named integer vector from [count_mitofinder_genes()]
#' @param min_genes genes a contig must carry to be confirmed
#'
#' @return list with `confirmed` (contig names, best first) and `genes` (counts
#'   aligned to `candidates`)
#'
#' @noRd
confirm_mito_contigs <- function(candidates, gene_counts, min_genes = 3) {
  genes <- as.integer(gene_counts[candidates])
  genes[is.na(genes)] <- 0L
  names(genes) <- candidates
  keep <- candidates[genes >= min_genes]
  # Best first: most genes, ties broken by the screen's original order.
  keep <- keep[order(-genes[keep], match(keep, candidates))]
  list(confirmed = keep, genes = genes)
}

#' Summarize the search for the Assemble table
#'
#' @param n_screened contigs that passed the length filter and were searched
#' @param evidence evidence table from [select_mito_contigs()]
#' @param confirmed contig names surviving MitoFinder
#' @param accession winning reference accession
#'
#' @return single human-readable string
#'
#' @noRd
find_mito_note <- function(n_screened, evidence, confirmed, accession) {
  n_cand <- sum(evidence$selected == 1L)
  if (length(confirmed) > 0L) {
    return(paste0(
      "found ", length(confirmed), " mitochondrial contig",
      if (length(confirmed) == 1L) "" else "s",
      " of ", n_screened, " screened"
    ))
  }
  if (n_cand > 0L) {
    return(paste0(
      "no contig confirmed: ", n_cand, " candidate",
      if (n_cand == 1L) "" else "s",
      " matched ", paste(accession, collapse = ", "),
      " but carried too few mitochondrial genes"
    ))
  }
  if (nrow(evidence) > 0L) {
    top <- evidence[which.max(evidence$aligned_length), ]
    return(paste0(
      "no candidate contig of ", n_screened, " screened; best was ",
      top$contig, " (", top$reason, ")"
    ))
  }
  paste0("no BLAST hits among ", n_screened, " screened contigs")
}

#' Find the mitochondrial contigs in a user-supplied assembly
#'
#' Driver for the WF1 `find_mito_pick` process: merges the BLAST screen output,
#' selects candidate contigs, extracts them from the assembly with
#' `samtools faidx`, confirms them with MitoFinder, and writes the files the
#' pipeline consumes.
#'
#' @param assembly_fn Path to the sample's full assembly (fasta)
#' @param hits_fn Chunk hit files written by the screen
#' @param id Sample ID, used to name the output fasta
#' @param mitofinder_db Path to a MitoFinder GenBank database, built with
#'   [custom_assembly_db()] (`db_type = "mitofinder"`)
#' @param genetic_code NCBI translation table passed to MitoFinder
#' @param min_identity,min_aligned_length,min_aligned_fraction,max_candidates
#'   Screen thresholds, see [select_mito_contigs()]
#' @param min_genes Mitochondrial genes a contig must carry to be confirmed
#' @param cpus Number of CPUs for MitoFinder
#' @param out_dir Directory for the outputs
#'
#' @return (invisibly) a list with `confirmed`, `note` and `evidence`
#'
#' @export
#'
find_mito <- function(
    assembly_fn = NULL,
    hits_fn = NULL,
    id = "sample",
    mitofinder_db = NULL,
    genetic_code = 2,
    min_identity = 70,
    min_aligned_length = 300,
    min_aligned_fraction = 0.5,
    max_candidates = 20,
    min_genes = 3,
    cpus = 4,
    out_dir = ".") {
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  }
  log_lines <- character(0)
  add_log <- function(...) log_lines <<- c(log_lines, paste0(...))

  n_screened <- sum(vapply(hits_fn[file.exists(hits_fn)], function(f) {
    n <- grep("^# screened=", readLines(f, warn = FALSE), value = TRUE)
    if (length(n) == 0L) 0L else as.integer(sub("^# screened=", "", n[1]))
  }, integer(1)))

  hits <- read_screen_hits(hits_fn)
  add_log("screened ", n_screened, " contigs, ", nrow(hits) %||% 0L, " BLAST hits")

  sel <- select_mito_contigs(
    hits,
    min_identity = min_identity,
    min_aligned_length = min_aligned_length,
    min_aligned_fraction = min_aligned_fraction,
    max_candidates = max_candidates
  )
  add_log("references: ", paste(sel$accession %||% NA_character_, collapse = ", "),
          "; candidates: ", length(sel$candidates))

  evidence <- sel$evidence
  # rep(), not a bare NA: a sample with no BLAST hits has no evidence rows, and
  # assigning a length-1 value to a zero-row data frame is an error.
  evidence$genes <- rep(NA_integer_, nrow(evidence))
  confirmed <- character(0)

  if (length(sel$candidates) > 0L) {
    cand_fn <- file.path(out_dir, "candidates.fasta")
    extract_contigs(assembly_fn, sel$candidates, cand_fn)

    genes <- mitofinder_gene_counts(
      cand_fn,
      mitofinder_db = mitofinder_db,
      genetic_code = genetic_code,
      cpus = cpus,
      workdir = file.path(out_dir, "mitofinder")
    )
    conf <- confirm_mito_contigs(sel$candidates, genes, min_genes = min_genes)
    confirmed <- conf$confirmed
    evidence$genes[match(names(conf$genes), evidence$contig)] <- as.integer(conf$genes)
    rejected <- setdiff(sel$candidates, confirmed)
    if (length(rejected) > 0L) {
      idx <- match(rejected, evidence$contig)
      evidence$selected[idx] <- 0L
      evidence$rank[idx] <- NA_integer_
      evidence$reason[idx] <- paste0(
        "only ", evidence$genes[idx], " mitochondrial genes, ", min_genes, " required"
      )
    }
    add_log("MitoFinder confirmed ", length(confirmed), " of ",
            length(sel$candidates), " candidates")
  }

  note <- find_mito_note(n_screened, evidence, confirmed, sel$accession)

  out_fasta <- file.path(out_dir, paste0(id, "_mito_contigs.fasta"))
  if (length(confirmed) > 0L) {
    extract_contigs(assembly_fn, confirmed, out_fasta)
  } else {
    # The pipeline still needs the declared output to exist; the status file is
    # what tells the workflow this sample found nothing.
    file.create(out_fasta)
  }
  writeLines(if (length(confirmed) > 0L) "ok" else "fail", file.path(out_dir, "status.txt"))
  writeLines(note, file.path(out_dir, "note.txt"))
  utils::write.csv(
    evidence[order(-evidence$selected, evidence$rank, -evidence$aligned_length), ],
    file.path(out_dir, "find_mito_candidates.csv"),
    row.names = FALSE, na = ""
  )
  writeLines(c(log_lines, note), file.path(out_dir, "find_mito.log"))

  invisible(list(confirmed = confirmed, note = note, evidence = evidence))
}

#' Pull named contigs out of a (possibly very large) assembly
#'
#' Uses `samtools faidx` so the assembly is indexed and read by offset rather
#' than loaded into memory.
#'
#' @param assembly_fn Path to the assembly fasta
#' @param contigs Character vector of contig names
#' @param out_fn Output fasta
#'
#' @noRd
extract_contigs <- function(assembly_fn, contigs, out_fn) {
  names_fn <- tempfile(fileext = ".txt")
  on.exit(unlink(names_fn), add = TRUE)
  writeLines(contigs, names_fn)
  system2("samtools", c("faidx", shQuote(assembly_fn), "-r", shQuote(names_fn),
                        "-o", shQuote(out_fn)))
  invisible(out_fn)
}

#' Run MitoFinder on candidate contigs and count genes per contig
#'
#' @param candidates_fn fasta of candidate contigs
#' @param mitofinder_db path to the MitoFinder GenBank database
#' @param genetic_code NCBI translation table
#' @param cpus threads for MitoFinder
#' @param workdir MitoFinder working directory
#'
#' @return named integer vector of gene counts
#'
#' @noRd
mitofinder_gene_counts <- function(candidates_fn,
                                   mitofinder_db,
                                   genetic_code = 2,
                                   cpus = 4,
                                   workdir = tempfile("mitofinder_")) {
  if (is.null(mitofinder_db) || !nzchar(mitofinder_db) || !file.exists(mitofinder_db)) {
    stop(
      "MitoFinder reference database not found: '", mitofinder_db, "'. ",
      "Build one with custom_assembly_db(db_type = \"mitofinder\") and set it ",
      "in the Find Mitogenome options.",
      call. = FALSE
    )
  }
  # Resolve both inputs before changing directory: MitoFinder writes its results
  # into the working directory, and a relative path would move out from under it.
  candidates_abs <- normalizePath(candidates_fn, mustWork = TRUE)
  db_abs <- normalizePath(mitofinder_db, mustWork = TRUE)

  dir.create(workdir, recursive = TRUE, showWarnings = FALSE)
  # Absolute, because the counting pass below runs while the working directory
  # IS workdir: a relative path would resolve against itself and find nothing.
  workdir <- normalizePath(workdir, mustWork = TRUE)
  old <- setwd(workdir)
  on.exit(setwd(old), add = TRUE)

  system2("mitofinder", c(
    "-j", "find_mito",
    "-a", shQuote(candidates_abs),
    "-r", shQuote(db_abs),
    "-o", genetic_code,
    "-p", cpus,
    "--ignore"
  ))

  count_mitofinder_genes(workdir)
}
