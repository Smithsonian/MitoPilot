#' Shared annotation-curation core for all clade rulesets
#'
#' Holds the curation logic shared by every `curate_<clade>_mito()` function.
#' The per-clade wrappers differ only in their `genetic_code` default and, for
#' diptera, whether minus-strand rRNAs are reverse-complemented (see
#' `flip_rRNA_minus_strand`).
#'
#' @param annotations_fn Path to the annotations file (csv)
#' @param assembly_fn Path to the assembly file (fasta)
#' @param coverage_fn Path to the coverage file (csv)
#' @param genetic_code Genetic code to use
#' @param out_dir Path to the output directory
#' @param max_blast_hits Maximum number of top BLAST hits to retain (default = 10)
#' @param params Nested list of curation parameters. Can also provided as a
#'   base64 encoded json string.
#' @param ref_dir Path to reference directory for curation
#' @param blast_ref_file Path to a JSON file of remote BLAST reference hits to inject into the local curation database (default = NULL)
#' @param feature_trim Trim feature coordinates to assembly boundaries (default = TRUE)
#' @param flip_rRNA_minus_strand Reverse-complement contigs whose rRNAs are all
#'   on the minus strand so annotations are reported on the plus strand
#'   (default = TRUE). Diptera mitogenomes legitimately carry minus-strand
#'   rRNAs, so the diptera wrapper passes FALSE.
#' @param ref_based_rc Reverse-complement each contig to match the strand of the
#'   top BLAST reference when the RC aligns better than the forward orientation
#'   (default = FALSE). Off by default because the rRNA / start-gene heuristics
#'   usually orient correctly; enable for taxa they cannot resolve (e.g.
#'   scyphozoan genomes with rRNAs on opposite strands). Runs after the
#'   rRNA-strand flip, so it is the final orientation authority.
#'
#' @noRd
#'
curate_mito_core <- function(
    annotations_fn = NULL,
    assembly_fn = NULL,
    coverage_fn = NULL,
    genetic_code = 2,
    out_dir = NULL,
    max_blast_hits = 10,
    params = NULL,
    ref_dir = NULL,
    blast_ref_file = NULL,
    feature_trim = TRUE,
    flip_rRNA_minus_strand = TRUE,
    ref_based_rc = FALSE,
    blast_accession = NULL) {
  # Prepare environment ----

  ## load annotations ----
  annotations <- tryCatch(
    read.csv(annotations_fn),
    error = function(e) {
      stop("Invalid annotations.")
    }
  )
  annotations <- annotations |>
    add_cols(
      list(
        notes = NA_character_,
        warnings = NA_character_,
        refHits = NA_character_
      )
    )

  ## load assembly ----
  assembly <- tryCatch(
    Biostrings::readDNAStringSet(assembly_fn),
    error = function(e) {
      stop("Invalid assembly.")
    }
  )
  contig_key <- names(assembly) |>
    {
      \(x) setNames(x, stringr::str_extract(x, "^\\S+"))
    }()

  ## circular-coordinate helpers ----
  # A feature spanning the origin of a circular contig is stored pos1 > pos2, and
  # the boundary arithmetic below can push a coordinate off either end. These wrap
  # coordinates onto the circle and read across the origin; on linear contigs they
  # are identity / plain subseq, so linear behaviour is unchanged.
  is_circ <- function(seqid) stringr::str_detect(contig_key[[seqid]], "circular")
  ctg_len <- function(seqid) Biostrings::width(assembly[contig_key[seqid]])[[1]]
  circ_pos <- function(seqid, p) {
    if (!is_circ(seqid)) {
      return(p)
    }
    wrap_pos(p, ctg_len(seqid))
  }
  ctg_seq <- function(seqid, p1, p2) {
    extract_circ_region(
      assembly[contig_key[seqid]],
      circ_pos(seqid, p1),
      circ_pos(seqid, p2)
    )
  }
  feat_len <- function(seqid, p1, p2) {
    if (p1 <= p2) {
      return(as.integer(abs(p2 - p1) + 1L))
    }
    as.integer(circ_len(p1, p2, ctg_len(seqid)))
  }

  ## load coverage ----
  if (!is.null(coverage_fn)) {
    coverage <- tryCatch(
      read.csv(coverage_fn),
      error = function(e) {
        stop("Invalid coverage.")
      }
    )
  } else {
    coverage <- NULL
  }

  ## Load params to env ----
  if (!is.null(params) && !is.list(params)) {
    params <- tryCatch(
      jsonlite::fromJSON(rawToChar(base64enc::base64decode(params))),
      error = function(e) {
        stop("Invalid JSON string.")
      }
    )
  }
  list2env(params, envir = environment())


  # set up ref_dbs list
  ref_dbs <- list(
    default = paste0(ref_dir, "/featureProt/{gene}.fas")
  )

  # Augment local BLAST DB with translated remote BLAST hit gene sequences.
  # blast_ref_file may name multiple candidate-reference JSONs (whitespace
  # separated); inject each so all retained BLAST hits enrich the curation DB.
  # The remote sequences land in their own small per-gene DBs and are searched
  # alongside ref_dir, which stays read-only and can therefore be shared by every
  # task rather than copied per task.
  remote_ref_dir <- file.path(tempdir(), "remote_refs")
  unlink(remote_ref_dir, recursive = TRUE)
  if (!is.null(blast_ref_file) && nzchar(blast_ref_file)) {
    ref_files <- strsplit(trimws(blast_ref_file), "\\s+")[[1]]
    for (rf in ref_files) {
      if (nzchar(rf)) {
        inject_remote_hits_into_blast_db(rf, ref_dir, out_dir = remote_ref_dir)
      }
    }
  }

  # Databases to BLAST a gene against: the read-only base DB plus this task's
  # remote-derived sequences, if any were injected for that gene. EVERY refHits
  # computation must go through here. refHits are recomputed after each boundary
  # adjustment, and a call naming only the base silently drops the remote hits for
  # exactly the genes that got adjusted.
  gene_dbs <- function(gene) {
    base <- as.character(stringr::str_glue(ref_dbs[[gene]] %||% ref_dbs[["default"]]))
    remote <- file.path(remote_ref_dir, paste0(gene, ".fas"))
    c(base, if (file.exists(remote)) remote)
  }

  ## Prepare rules ----
  rules <- rules |>
    purrr::map(~ modifyList(default_rules[[.x$type]] %||% list(), .x))
  # Non-standard genes (e.g. MitoFinder CDS with no canonical name) inherit the
  # default PCG ruleset so they are curated like any other PCG.
  rules <- augment_rules_for_unknown_genes(rules, annotations, default_rules)

  ## Set genetic code ----
  genetic_code <- tryCatch(
    Biostrings::getGeneticCode(as.character(genetic_code)),
    error = function(e) {
      stop("Invalid genetic code.")
    }
  )

  # rRNA ----
  ## enforce + strand ----
  if (isTRUE(flip_rRNA_minus_strand)) {
    rRNA_rev <- annotations |>
      dplyr::filter(type == "rRNA") |>
      dplyr::filter(all(direction == "-"), .by = "contig") |>
      dplyr::pull(contig) |>
      unique()
    for (seqid in rRNA_rev) {
      wdth <- assembly[contig_key[seqid]]@ranges@width
      annotations_updated <- annotations |>
        dplyr::filter(contig == !!seqid) |>
        dplyr::mutate(
          pos1_old = pos1,
          pos2_old = pos2,
          pos1 = wdth - pos2_old + 1,
          pos2 = wdth - pos1_old + 1,
          direction = dplyr::case_match(
            direction, "+" ~ "-", "-" ~ "+"
          )
        ) |>
        dplyr::select(-pos1_old, -pos2_old)

      annotations <- annotations |>
        dplyr::filter(contig != seqid) |>
        dplyr::bind_rows(
          annotations_updated
        ) |>
        dplyr::arrange(contig, pos1)

      assembly[contig_key[seqid]] <- assembly[contig_key[seqid]] |>
        Biostrings::reverseComplement()

      if (!is.null(coverage)) {
        coverage_flip <- coverage |>
          dplyr::filter(SeqId == !!seqid) |>
          dplyr::arrange(desc(Position)) |>
          dplyr::mutate(
            Position = dplyr::row_number(),
            Call = as.character(assembly) |> stringr::str_split("") |> unlist()
          )
        coverage <- dplyr::bind_rows(
          coverage |> dplyr::filter(SeqId != seqid),
          coverage_flip
        )
      }
    }
  }

  ## reference-based reverse-complement (optional) ----
  # Ensure each contig matches the strand of the top BLAST reference: align the
  # contig forward and reverse-complemented against the reference and flip the
  # contig (sequence + annotations + coverage) when the RC aligns materially
  # better. Off by default; runs AFTER the rRNA-strand flip so it is the final
  # orientation authority, and it inspects the current (already-flipped) state so
  # it simply converges to the reference orientation without double-flipping.
  # Intended for taxa the rRNA/start-gene heuristics cannot orient (e.g.
  # scyphozoan genomes with rRNAs on opposite strands).
  if (isTRUE(ref_based_rc)) {
    # Orient against the top BLAST hit (blast_accession). This is the designated
    # reference and the highest-similarity hit; using it (rather than the longest
    # candidate) matters when references disagree on strand, which is common for
    # linear mitogenomes where strand choice at deposition is arbitrary.
    ref_seq_str <- tryCatch(
      .best_blast_ref_sequence(blast_ref_file, prefer_accession = blast_accession),
      error = function(e) NULL)
    if (!is.null(ref_seq_str) && nzchar(ref_seq_str)) {
      ref_dna <- Biostrings::DNAString(
        gsub("[^ACGTNacgtnRYSWKMBDHVryswkmbdhv]", "N", toupper(ref_seq_str))
      )
      # baseOnly = FALSE: the full IUPAC matrix scores N / ambiguity codes.
      # baseOnly = TRUE has no key for N and errors ("key 15 not in lookup
      # table") on any reference or contig carrying an ambiguity base, which the
      # tryCatch below would otherwise swallow into a silent no-flip.
      subMx <- pwalign::nucleotideSubstitutionMatrix(match = 1, mismatch = -1, baseOnly = FALSE)
      for (seqid in unique(annotations$contig)) {
        ctg <- tryCatch(assembly[[contig_key[seqid]]], error = function(e) NULL)
        if (is.null(ctg)) next
        score <- function(s) tryCatch(
          pwalign::pairwiseAlignment(s, ref_dna, substitutionMatrix = subMx,
                                     gapOpening = 10, gapExtension = 4,
                                     type = "local", scoreOnly = TRUE),
          error = function(e) NA_real_
        )
        fwd_score <- score(ctg)
        rc_score  <- score(Biostrings::reverseComplement(ctg))
        if (is.na(fwd_score) || is.na(rc_score) || rc_score <= fwd_score) next
        flipped <- .rc_contig_flip(seqid, assembly, annotations, coverage, contig_key)
        assembly    <- flipped$assembly
        annotations <- flipped$annotations
        coverage    <- flipped$coverage
      }
    }
  }

  ## apply punctuation model ----
  gene_idx <- which(annotations$type == "rRNA")
  # Circular neighbours: the table is sorted by pos1, so a feature spanning the
  # origin sorts last while sitting immediately before the first row. On a
  # circular contig it, not the table edge, is the neighbour of the end rows.
  neighbour_row <- function(idx, side) {
    ctg <- annotations$contig[idx]
    rows <- which(annotations$contig == ctg)
    circ <- is_circ(ctg)
    wrap <- setdiff(rows[annotations$pos1[rows] > annotations$pos2[rows]], idx)
    # A wrapping row is not the table-order neighbour it appears to be: it
    # precedes the contig's first row and follows its last.
    ord <- if (circ) setdiff(rows, wrap) else rows
    k <- match(idx, ord)
    if (is.na(k)) {
      # idx is itself a wrapping row: its neighbours are the outer ordinary rows
      if (length(ord) == 0L) return(NA_integer_)
      return(if (side == "before") ord[length(ord)] else ord[1])
    }
    if (side == "before") {
      if (k > 1L) ord[k - 1L] else if (circ && length(wrap) > 0L) wrap[1] else NA_integer_
    } else {
      if (k < length(ord)) ord[k + 1L] else if (circ && length(wrap) > 0L) wrap[1] else NA_integer_
    }
  }

  for (idx in gene_idx) {
    # Apply tRNA punctuation model
    ctg <- annotations$contig[idx]
    before_row <- neighbour_row(idx, "before")
    before <- annotations[if (is.na(before_row)) 0L else before_row, ] |>
      dplyr::filter(type == "tRNA" & contig == ctg)
    if (nrow(before) == 1) {
      if (circ_pos(ctg, before$pos2 + 1) != annotations$pos1[idx]) {
        pos1_new <- circ_pos(ctg, before$pos2 + 1)
        if (feat_len(ctg, pos1_new, annotations$pos2[idx]) <= rules[[annotations$gene[idx]]][["max_len"]]) {
          pos1_change <- pos1_new - annotations$pos1[idx]
          annotations$pos1[idx] <- pos1_new
          annotations$notes[idx] <- semicolon_paste(
            annotations$notes[idx],
            stringr::str_glue("Applied punctuation model- moved pos1 by {pos1_change} bp")
          )
        }
      }
    }
    after_row <- neighbour_row(idx, "after")
    # a single neighbour cannot bound both ends
    if (!is.na(after_row) && isTRUE(after_row == before_row)) after_row <- NA_integer_
    after <- annotations[if (is.na(after_row)) 0L else after_row, ] |>
      dplyr::filter(type == "tRNA" & contig == ctg)
    if (nrow(after) == 1) {
      if (circ_pos(ctg, annotations$pos2[idx] + 1) != after$pos1) {
        pos2_new <- circ_pos(ctg, after$pos1 - 1)
        if (feat_len(ctg, annotations$pos1[idx], pos2_new) <= rules[[annotations$gene[idx]]][["max_len"]]) {
          pos2_change <- pos2_new - annotations$pos2[idx]
          annotations$pos2[idx] <- pos2_new
          annotations$notes[idx] <- semicolon_paste(
            annotations$notes[idx],
            stringr::str_glue("Applied punctuation model - moved pos2 by {pos2_change} bp")
          )
        }
      }
    }
    annotations$length[idx] <- feat_len(ctg, annotations$pos1[idx], annotations$pos2[idx])
  }

  # PCGs ----
  ## Get top ref hits for each PCG ----
  # Combined all-gene DB for non-standard PCGs that have no per-gene featureProt
  # FASTA. Merges the base DB with the remote sequences injected above, which now
  # live in their own directory. Built lazily on first need so the common
  # all-standard-gene case pays nothing.
  combined_ref_db <- local({
    cache <- NULL; built <- FALSE
    function() {
      if (built) return(cache)
      cache <<- tryCatch(
        build_combined_orf_db(c(file.path(ref_dir, "featureProt"), remote_ref_dir),
                              file.path(tempdir(), "_curate_all_genes.fas"),
                              condaenv = "base"),
        error = function(e) NULL
      )
      built <<- TRUE
      cache
    }
  })

  annotations$refHits <- annotations |>
    dplyr::select(type, gene, translation) |>
    purrr::pmap(function(type, gene, translation) {
      if (type != "PCG") {
        return(NA)
      }

      ## Gene ref database ----
      ref_db <- stringr::str_glue(ref_dbs[[gene]] %||% ref_dbs[["default"]])

      # No per-gene reference DB (e.g. a non-standard MitoFinder gene): BLAST the
      # translation against the combined all-gene DB like an ORF so candidate
      # identities still surface in the alignment.
      if (!file.exists(as.character(ref_db))) {
        cdb <- combined_ref_db()
        if (is.null(cdb) || is.na(translation) || !nzchar(translation)) {
          return('{}')
        }
        out <- get_top_hits_orf(cdb, translation, max_blast_hits) |>
          json_string()
        return(out %||% '{}')
      }

      out <- get_top_hits(gene_dbs(gene), translation, max_blast_hits) |>
        json_string()
      out %||% '{}'
    })


  # Prepend remote BLAST top hit to refHits for each PCG
  if (!is.null(blast_ref_file)) {
    annotations <- prepend_blast_hit_to_refhits(annotations, blast_ref_file)
  }

  ## Get top ref hits for each rRNA (nucleotide BLAST) ----
  # Uses per-gene featureNuc/<gene>.fas databases. Inert (no-op) until those
  # reference databases are built (see data-raw/build_curation_db.R); the
  # annotate-details rRNA alignment falls back to the BLAST reference genome.
  nuc_ref_dir <- file.path(ref_dir, "featureNuc")
  for (i in which(annotations$type == "rRNA")) {
    gene <- annotations$gene[i]
    db <- file.path(nuc_ref_dir, paste0(gene, ".fas"))
    if (!file.exists(db)) next
    nt <- tryCatch({
      s <- ctg_seq(
        annotations$contig[i],
        annotations$pos1[i], annotations$pos2[i]
      )
      if (identical(annotations$direction[i], "-")) s <- Biostrings::reverseComplement(s)
      unname(as.character(s))
    }, error = function(e) NA_character_)
    if (is.na(nt) || !nzchar(nt)) next
    annotations$refHits[i] <- tryCatch(
      get_top_hits_nuc(db, nt, max_blast_hits) |> json_string() %||% "{}",
      error = function(e) annotations$refHits[i]
    )
  }

  ## Curate against top hits ----
  annotations <- purrr::pmap_dfr(annotations, function(...) {
    cur <- list(...)
    if (cur$type != "PCG") {
      return(cur)
    }
    list2env(cur, envir = environment())

    # Stop if no hits above threshold
    refHits <- json_parse(refHits[[1]], TRUE)
    if (nrow(refHits) == 0L || !any(refHits$similarity >= hit_threshold, na.rm = TRUE)) {
      return(cur)
    }

    # start / stop codon options
    start_opts <- rules[[gene]][["start_codons"]]
    stop_opts <- rules[[gene]][["stop_codons"]] |>
      {
        \(x) x[order(nchar(x), decreasing = T)]
      }()

    ## Fix TRUNCATION ----
    ### START ----
    if (isTRUE(refHits$gap_leading[1] != 0) && (sum(refHits$gap_leading > 0L, na.rm = TRUE) / nrow(refHits)) > 0.5) {
      gaps_target <- max(refHits$gap_leading)
      if (direction == "+") {
        while (gaps_target > 0) {
          pos1_new <- pos1 - (3 * gaps_target)
          if (pos1_new <= 0 && !is_circ(contig)) {
            gaps_target <- gaps_target - 1
            next
          }
          new_start_codon <- ctg_seq(contig, pos1_new, pos1_new + 2) |>
            as.character()
          if (!new_start_codon %in% start_opts) {
            gaps_target <- gaps_target - 1
            next
          }
          cur$notes <- semicolon_paste(cur$notes, stringr::str_glue("extending start {pos1 - pos1_new} bp"))
          cur$pos1 <- pos1 <- circ_pos(contig, pos1_new)
          cur$length <- length <- feat_len(contig, pos1, pos2)
          cur$start_codon <- start_codon <- new_start_codon
          cur$translation <- translation <- ctg_seq(
            contig,
            pos1,
            pos2 - .codon_len(stop_codon)
          ) |>
            Biostrings::translate(genetic.code = genetic_code) |>
            as.character()
          refHits <- get_top_hits(
            gene_dbs(gene),
            translation,
            max_blast_hits
          )
          break
        }
      }
      if (direction == "-") {
        while (gaps_target > 0) {
          pos2_new <- pos2 + (3 * gaps_target)
          if (pos2_new >= ctg_len(contig) && !is_circ(contig)) {
            gaps_target <- gaps_target - 1
            next
          }
          new_start_codon <- ctg_seq(contig, pos2_new - 2, pos2_new) |>
            Biostrings::reverseComplement() |>
            as.character()
          if (!new_start_codon %in% start_opts) {
            gaps_target <- gaps_target - 1
            next
          }
          cur$notes <- notes <- semicolon_paste(notes, stringr::str_glue("extending start {pos2_new - pos2} bp"))
          cur$pos2 <- pos2 <- circ_pos(contig, pos2_new)
          cur$length <- length <- feat_len(contig, pos1, pos2)
          cur$start_codon <- start_codon <- new_start_codon
          cur$translation <- translation <- ctg_seq(
            contig,
            pos1 + .codon_len(stop_codon),
            pos2
          ) |>
            Biostrings::reverseComplement() |>
            Biostrings::translate(genetic.code = genetic_code) |>
            as.character()
          refHits <- get_top_hits(
            gene_dbs(gene),
            translation,
            max_blast_hits
          )
          break
        }
      }
    }

    ### STOP ----
    if (isTRUE(refHits$gap_trailing[1] != 0) && (sum(refHits$gap_trailing > 0L, na.rm = TRUE) / nrow(refHits)) > 0.5) {
      gaps_target <- max(refHits$gap_trailing)
      if (direction == "+") {
        while (gaps_target > 0) {
          pos2_new <- pos2 - .codon_len(stop_codon) + 3 + (3 * gaps_target)
          if ((pos2_new + 1) > ctg_len(contig) && !is_circ(contig)) {
            gaps_target <- gaps_target - 1
            next
          }
          new_stop_codon <- ctg_seq(
            contig,
            pos2_new - 2,
            if (is_circ(contig)) pos2_new else min(pos2_new, ctg_len(contig))
          ) |>
            as.character()
          while (nchar(new_stop_codon) > 0 && !new_stop_codon %in% stop_opts) {
            new_stop_codon <- stringr::str_remove(new_stop_codon, ".$")
            pos2_new <- pos2_new - 1
          }
          if (nchar(new_stop_codon) == 0L) {
            gaps_target <- gaps_target - 1
            next
          }
          # reject if new translation has internal stop codon
          translation <- ctg_seq(
            contig,
            pos1,
            pos2_new - nchar(new_stop_codon)
          ) |>
            Biostrings::translate(genetic.code = genetic_code) |>
            as.character()
          if (grepl("\\*", translation)) {
            gaps_target <- gaps_target - 1
            next
          }
          cur$notes <- notes <- semicolon_paste(
            notes,
            stringr::str_glue("extending end {abs(pos2_new - pos2)} bp")
          )
          cur$pos2 <- pos2 <- circ_pos(contig, pos2_new)
          cur$length <- length <- feat_len(contig, pos1, pos2)
          cur$stop_codon <- stop_codon <- new_stop_codon
          cur$translation <- translation
          refHits <- get_top_hits(
            gene_dbs(gene),
            translation,
            max_blast_hits
          )
          break
        }
      }
      if (direction == "-") {
        while (gaps_target > 0) {
          pos1_new <- pos1 + .codon_len(stop_codon) - 3 - (3 * gaps_target)
          if ((pos1_new + 2) < 1 && !is_circ(contig)) {
            gaps_target <- gaps_target - 1
            next
          }
          new_stop_codon <- ctg_seq(
            contig,
            if (is_circ(contig)) pos1_new else max(pos1_new, 1),
            pos1_new + 2
          ) |>
            Biostrings::reverseComplement() |>
            as.character()
          if(nchar(new_stop_codon) < 3L) {
            pos1_new <- pos1_new + (3 - nchar(new_stop_codon))
          }
          while (nchar(new_stop_codon) > 0 && !new_stop_codon %in% stop_opts) {
            new_stop_codon <- stringr::str_remove(new_stop_codon, ".$")
            pos1_new <- pos1_new + 1
          }
          if (nchar(new_stop_codon) == 0L) {
            gaps_target <- gaps_target - 1
            next
          }
          # reject if new translation has internal stop codon
          translation <- ctg_seq(
            contig,
            pos1_new + nchar(new_stop_codon),
            pos2
          ) |>
            Biostrings::reverseComplement() |>
            Biostrings::translate(genetic.code = genetic_code) |>
            as.character()
          if (grepl("\\*", translation)) {
            gaps_target <- gaps_target - 1
            next
          }
          cur$notes <- notes <- semicolon_paste(
            notes,
            stringr::str_glue("extending end {abs(pos1_new - pos1)} bp")
          )
          cur$pos1 <- pos1 <- circ_pos(contig, pos1_new)
          cur$length <- length <- feat_len(contig, pos1, pos2)
          cur$stop_codon <- stop_codon <- new_stop_codon
          cur$translation <- translation
          refHits <- get_top_hits(
            gene_dbs(gene),
            translation,
            max_blast_hits
          )
          break
        }
      }
    }

    ## Fix OVER-EXTENSION ----
    ### START ----
    if (isTRUE(refHits$gap_leading[1] != 0) && (sum(refHits$gap_leading < 0L, na.rm = TRUE) / nrow(refHits)) > 0.5) {
      if (direction == "+") {
        alt_starts <- ctg_seq(
          contig,
          pos1 + 3,
          pos1 + 3 + (3 * max(abs(refHits$gap_leading[refHits$gap_leading < 0]))) - 1
        ) |>
          as.character() |>
          stringr::str_extract_all(".{1,3}") |>
          unlist() |>
          purrr::set_names() |>
          purrr::map(~ {
            .x %in% start_opts
          })
        alt_idx <- 1
        while (alt_idx <= length(alt_starts)) {
          if (!alt_starts[[alt_idx]]) {
            alt_idx <- alt_idx + 1
            next
          }
          pos1_new <- pos1 + (3 * alt_idx)
          translation_new <- ctg_seq(contig, pos1_new, pos2 - .codon_len(stop_codon)) |>
            Biostrings::translate(genetic.code = genetic_code) |>
            as.character()
          refHits_new <- get_top_hits(
            gene_dbs(gene),
            translation_new,
            max_blast_hits
          )
          if (isTRUE(refHits_new$gap_leading[1] != 0) && (sum(refHits_new$gap_leading < 0L, na.rm = TRUE) / nrow(refHits_new)) > 0.5) {
            alt_idx <- alt_idx + 1
            next
          }
          cur$notes <- notes <- semicolon_paste(
            notes,
            stringr::str_glue("trimming start {abs(pos1_new - pos1)} bp")
          )
          cur$pos1 <- pos1 <- circ_pos(contig, pos1_new)
          cur$length <- length <- feat_len(contig, pos1, pos2)
          cur$start_codon <- start_codon <- names(alt_starts)[alt_idx]
          cur$translation <- translation <- translation_new
          refHits <- refHits_new
          break
        }
      }
      if (direction == "-") {
        alt_starts <- ctg_seq(
          contig,
          pos2 - 3 - (3 * max(abs(refHits$gap_leading[refHits$gap_leading < 0]))) + 1,
          pos2 - 3
        ) |>
          Biostrings::reverseComplement() |>
          as.character() |>
          stringr::str_extract_all(".{1,3}") |>
          unlist() |>
          purrr::set_names() |>
          purrr::map(~ {
            .x %in% start_opts
          })
        alt_idx <- 1
        while (alt_idx <= length(alt_starts)) {
          if (!alt_starts[[alt_idx]]) {
            alt_idx <- alt_idx + 1
            next
          }
          pos2_new <- pos2 - (3 * alt_idx)
          translation_new <- ctg_seq(contig, pos1 + .codon_len(stop_codon), pos2_new) |>
            Biostrings::reverseComplement() |>
            Biostrings::translate(genetic.code = genetic_code) |>
            as.character()
          refHits_new <- get_top_hits(
            gene_dbs(gene),
            translation_new,
            max_blast_hits
          )
          if (isTRUE(refHits_new$gap_leading[1] != 0) && (sum(refHits_new$gap_leading < 0L, na.rm = TRUE) / nrow(refHits_new)) > 0.5) {
            alt_idx <- alt_idx + 1
            next
          }
          cur$notes <- notes <- semicolon_paste(
            notes,
            stringr::str_glue("trimming start {abs(pos2_new - pos2)} bp")
          )
          cur$pos2 <- pos2 <- circ_pos(contig, pos2_new)
          cur$length <- length <- feat_len(contig, pos1, pos2)
          cur$start_codon <- start_codon <- names(alt_starts)[alt_idx]
          cur$translation <- translation <- translation_new
          refHits <- refHits_new
          break
        }
      }
    }

    ### STOP ----
    if (isTRUE(refHits$gap_trailing[1] != 0) && (sum(refHits$gap_trailing < 0L, na.rm = TRUE) / nrow(refHits)) > 0.5) {
      if (direction == "+") {
        alt_stops <- ctg_seq(
          contig,
          pos2 - .codon_len(stop_codon) - (3 * max(abs(refHits$gap_trailing[refHits$gap_trailing < 0]))) + 1,
          pos2 - .codon_len(stop_codon)
        ) |>
          as.character() |>
          stringr::str_extract_all(".{1,3}") |>
          unlist() |>
          purrr::map(~ {
            s <- stringr::str_extract(.x, paste0("^", stop_opts)) |>
              na.omit()
            s[1] |> unlist()
          }) |>
          rev()
        alt_idx <- 1
        while (alt_idx <= length(alt_stops)) {
          if (is.na(alt_stops[[alt_idx]])) {
            alt_idx <- alt_idx + 1
            next
          }
          pos2_new <- pos2 - .codon_len(stop_codon) + 3 - (3 * alt_idx) - (3 - nchar(alt_stops[[alt_idx]]))
          translation_new <- ctg_seq(
            contig,
            pos1,
            pos2_new - nchar(alt_stops[[alt_idx]])
          ) |>
            Biostrings::translate(genetic.code = genetic_code) |>
            as.character()
          refHits_new <- get_top_hits(
            gene_dbs(gene),
            translation_new,
            max_blast_hits
          )
          if (isTRUE(refHits_new$gap_trailing[1] != 0) && (sum(refHits_new$gap_trailing < 0L, na.rm = TRUE) / nrow(refHits_new)) > 0.5) {
            alt_idx <- alt_idx + 1
            next
          }
          cur$notes <- notes <- semicolon_paste(
            notes,
            stringr::str_glue("trimming end {abs(pos2_new - pos2)} bp")
          )
          cur$pos2 <- pos2 <- circ_pos(contig, pos2_new)
          cur$length <- length <- feat_len(contig, pos1, pos2)
          cur$stop_codon <- stop_codon <- alt_stops[[alt_idx]]
          cur$translation <- translation <- translation_new
          refHits <- refHits_new
          break
        }
      }
      if (direction == "-") {
        alt_stops <- ctg_seq(
          contig,
          pos1 + .codon_len(stop_codon),
          pos1 + .codon_len(stop_codon) + (3 * max(abs(refHits$gap_trailing[refHits$gap_trailing < 0]))) - 1
        ) |>
          Biostrings::reverseComplement() |>
          as.character() |>
          stringr::str_extract_all(".{1,3}") |>
          unlist() |>
          purrr::map(~ {
            s <- stringr::str_extract(.x, paste0("^", stop_opts)) |>
              na.omit()
            s[1] |> unlist()
          }) |>
          rev()
        alt_idx <- 1
        while (alt_idx <= length(alt_stops)) {
          if (is.na(alt_stops[[alt_idx]])) {
            alt_idx <- alt_idx + 1
            next
          }
          pos1_new <- pos1 + .codon_len(stop_codon) - 3 + (3 * alt_idx) + (3 - nchar(alt_stops[[alt_idx]]))
          translation_new <- ctg_seq(
            contig,
            pos1 + nchar(alt_stops[[alt_idx]]),
            pos2
          ) |>
            Biostrings::reverseComplement() |>
            Biostrings::translate(genetic.code = genetic_code) |>
            as.character()
          refHits_new <- get_top_hits(
            gene_dbs(gene),
            translation_new,
            max_blast_hits
          )
          if (isTRUE(refHits_new$gap_trailing[1] != 0) && (sum(refHits_new$gap_trailing < 0L, na.rm = TRUE) / nrow(refHits_new)) > 0.5) {
            alt_idx <- alt_idx + 1
            next
          }
          cur$notes <- notes <- semicolon_paste(
            notes,
            stringr::str_glue("trimming end {abs(pos1_new - pos1)} bp")
          )
          cur$pos1 <- pos1 <- circ_pos(contig, pos1_new)
          cur$length <- length <- feat_len(contig, pos1, pos2)
          cur$stop_codon <- stop_codon <- alt_stops[[alt_idx]]
          cur$translation <- translation <- translation_new
          refHits <- refHits_new
          break
        }
      }
    }

    cur$refHits <- json_string(refHits)

    return(cur)
  })

  # Restore remote BLAST hit rows after any curation step that recomputed refHits
  if (!is.null(blast_ref_file)) {
    annotations <- prepend_blast_hit_to_refhits(annotations, blast_ref_file)
  }

  ## Stop codon trimming ----
  for (idx in seq_len(nrow(annotations))) {
    if (annotations$type[idx] != "PCG") next
    gene <- annotations$gene[idx]
    overlap_rules <- rules[[gene]][["overlap"]]
    stop_opts <- rules[[gene]][["stop_codons"]]
    while (annotations$direction[idx] == "+") {
      if (idx == nrow(annotations)) break
      if (overlap_rules$stop) break
      if (.codon_len(annotations$stop_codon[idx]) < 1) break
      ctg <- annotations$contig[idx]
      codon_positions <- circ_pos(ctg, seq(
        annotations$pos2[idx] - .codon_len(annotations$stop_codon[idx]) + 1,
        annotations$pos2[idx]
      ))
      overlaps <- annotations[(idx + 1):nrow(annotations), ] |>
        dplyr::filter(pos1 %in% codon_positions) |>
        dplyr::filter(direction == annotations$direction[idx])
      if (nrow(overlaps) != 1L) break
      overlap <- sum(vapply(
        codon_positions,
        \(p) circ_overlap(p, p, overlaps$pos1, overlaps$pos2),
        logical(1)
      ))
      new_stop <- stringr::str_sub(annotations$stop_codon[idx], 1, .codon_len(annotations$stop_codon[idx]) - overlap)
      if (nchar(new_stop) < 1) break
      if (!new_stop %in% stop_opts) break
      annotations$stop_codon[idx] <- new_stop
      annotations$pos2[idx] <- circ_pos(ctg, annotations$pos2[idx] - overlap)
      annotations$length[idx] <- annotations$length[idx] - overlap
      annotations$notes[idx] <- semicolon_paste(
        annotations$notes[idx],
        stringr::str_glue("stop codon trimmed  by {overlap} bp")
      )
      break
    }
    while (annotations$direction[idx] == "-") {
      if (idx == 1) break
      if (overlap_rules$stop) break
      if (.codon_len(annotations$stop_codon[idx]) < 1) break
      ctg <- annotations$contig[idx]
      codon_positions <- circ_pos(ctg, seq(
        annotations$pos1[idx],
        annotations$pos1[idx] + .codon_len(annotations$stop_codon[idx]) - 1
      ))
      overlaps <- annotations[1:(idx - 1), ] |>
        dplyr::filter(pos2 %in% codon_positions) |>
        dplyr::filter(direction == annotations$direction[idx])
      if (nrow(overlaps) != 1L) break
      overlap <- sum(vapply(
        codon_positions,
        \(p) circ_overlap(p, p, overlaps$pos1, overlaps$pos2),
        logical(1)
      ))
      new_stop <- stringr::str_sub(annotations$stop_codon[idx], 1, .codon_len(annotations$stop_codon[idx]) - overlap)
      if (nchar(new_stop) < 1) break
      if (!new_stop %in% stop_opts) break
      annotations$stop_codon[idx] <- new_stop
      annotations$pos1[idx] <- circ_pos(ctg, annotations$pos1[idx] + overlap)
      annotations$length[idx] <- annotations$length[idx] - overlap
      annotations$notes[idx] <- semicolon_paste(
        annotations$notes[idx],
        stringr::str_glue("stop codon trimmed by {overlap} bp")
      )
      break
    }
  }

  # End trimming ----
  # Remove un-annotated regions at the beginning or end of linear contigs
  if (isTRUE(feature_trim)) {
  purrr::iwalk(contig_key, ~ {
    # Skip circular contigs
    if (stringr::str_detect(.x, "circular")) {
      return()
    }
    # Nothing to trim against on a contig with no annotations
    ctg_ann <- dplyr::filter(annotations, contig == .y)
    if (nrow(ctg_ann) == 0L) {
      return()
    }
    ## Check beginning ----
    min_ann <- min(pmin(ctg_ann$pos1, ctg_ann$pos2))
    if (min_ann > 1) {
      assembly[.x] <<- Biostrings::subseq(assembly[.x], min_ann, -1)
      annotations <<- annotations |>
        dplyr::mutate(
          pos1 = dplyr::case_when(
            contig == .y ~ pos1 - min_ann + 1,
            .default = pos1
          ),
          pos2 = dplyr::case_when(
            contig == .y ~ pos2 - min_ann + 1,
            .default = pos2
          )
        )
      if (!is.null(coverage)) {
        coverage <<- coverage |>
          dplyr::mutate(
            Position = dplyr::case_when(
              SeqId == .y ~ Position - min_ann + 1,
              .default = Position
            )
          ) |>
          dplyr::filter(Position > 0)
      }
    }
    ## Check end ----
    max_ann <- max(pmax(ctg_ann$pos1, ctg_ann$pos2))
    if (max_ann < assembly[.x]@ranges@width) {
      assembly[.x] <<- Biostrings::subseq(assembly[.x], 1, max_ann)
      if (!is.null(coverage)) {
        coverage <<- coverage |>
          dplyr::filter(
            SeqId != .y | Position <= max_ann
          )
      }
    }
  })

  }

  # Outputs ----
  readr::write_csv(
    annotations,
    file.path(out_dir, basename(annotations_fn)),
    na = ""
  )
  Biostrings::writeXStringSet(assembly, file.path(out_dir, basename(assembly_fn)))
  if (!is.null(coverage_fn)) {
    readr::write_csv(coverage, file.path(out_dir, basename(coverage_fn)), quote = "none", na = "")
  }

  return(invisible(annotations))
}

# Return the nucleotide sequence of the reference to orient against from the
# whitespace-separated `blast_ref_file` JSON list, or NULL if none carry a usable
# sequence. When `prefer_accession` (the top BLAST hit) matches a JSON with a
# usable sequence it wins outright - the top hit is the designated reference and,
# for linear mitogenomes whose candidates disagree on strand, the only one whose
# orientation curation should match. Otherwise falls back to lowest blast_evalue,
# breaking ties by longest sequence (prefer a complete genome over a partial;
# evalues are frequently all 0 for strong hits). Used by the reference-based RC step.
# @noRd
.best_blast_ref_sequence <- function(blast_ref_file, prefer_accession = NULL) {
  if (is.null(blast_ref_file) || !nzchar(blast_ref_file)) return(NULL)
  files <- strsplit(trimws(blast_ref_file), "\\s+")[[1]]
  files <- files[nzchar(files) & file.exists(files)]
  if (length(files) == 0) return(NULL)
  prefer_accession <- if (is.null(prefer_accession) || is.na(prefer_accession) ||
                          !nzchar(prefer_accession)) NULL else trimws(prefer_accession)
  usable <- function(s) is.character(s) && length(s) == 1 && nzchar(s)
  best_seq <- NULL
  best_eval <- Inf
  best_len <- -1L
  for (f in files) {
    j <- tryCatch(jsonlite::fromJSON(f), error = function(e) NULL)
    if (is.null(j)) next
    seq_str <- j$sequence %||% ""
    if (!usable(seq_str)) next
    # Top BLAST hit wins outright.
    if (!is.null(prefer_accession) && identical(as.character(j$accession %||% ""), prefer_accession)) {
      return(seq_str)
    }
    ev <- suppressWarnings(as.numeric(j$blast_evalue %||% NA))
    if (is.na(ev)) ev <- Inf
    len <- nchar(seq_str)
    if (ev < best_eval || (ev == best_eval && len > best_len)) {
      best_eval <- ev
      best_len <- len
      best_seq <- seq_str
    }
  }
  best_seq
}

# Reverse-complement one contig and propagate the flip to its annotations
# (pos1/pos2 mirrored about the contig width, direction toggled) and coverage
# (row order reversed, bases re-called from the flipped sequence). Mirrors the
# rRNA-strand flip block. Returns the updated assembly, annotations, coverage.
# @noRd
.rc_contig_flip <- function(seqid, assembly, annotations, coverage, contig_key) {
  wdth <- assembly[contig_key[seqid]]@ranges@width
  annotations_updated <- annotations |>
    dplyr::filter(contig == !!seqid) |>
    dplyr::mutate(
      pos1_old = pos1,
      pos2_old = pos2,
      pos1 = wdth - pos2_old + 1,
      pos2 = wdth - pos1_old + 1,
      direction = dplyr::case_match(direction, "+" ~ "-", "-" ~ "+")
    ) |>
    dplyr::select(-pos1_old, -pos2_old)
  annotations <- annotations |>
    dplyr::filter(contig != seqid) |>
    dplyr::bind_rows(annotations_updated) |>
    dplyr::arrange(contig, pos1)
  assembly[contig_key[seqid]] <- assembly[contig_key[seqid]] |>
    Biostrings::reverseComplement()
  if (!is.null(coverage)) {
    coverage_flip <- coverage |>
      dplyr::filter(SeqId == !!seqid) |>
      dplyr::arrange(desc(Position)) |>
      dplyr::mutate(
        Position = dplyr::row_number(),
        Call = as.character(assembly[contig_key[seqid]]) |> stringr::str_split("") |> unlist()
      )
    coverage <- dplyr::bind_rows(
      coverage |> dplyr::filter(SeqId != seqid),
      coverage_flip
    )
  }
  list(assembly = assembly, annotations = annotations, coverage = coverage)
}
