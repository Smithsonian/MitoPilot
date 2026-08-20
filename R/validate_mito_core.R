#' Shared annotation-validation core for all clade rulesets
#'
#' Holds the validation logic shared by every `validate_<clade>_mito()`
#' function. Per-clade wrappers differ only by the params they are called with;
#' the body here handles introns (when a gene rule sets `intron = TRUE`),
#' counts, overlaps, length limits, coverage/error heuristics, and PCG codon /
#' reference checks.
#'
#' @param annotations_fn path to annotations file (csv)
#' @param coverage_fn path to coverage file (csv)
#' @param params nested list of curation/validation parameters. Can also be
#'   provided as a base64 encoded JSON string.
#' @param out_dir output directory
#'
#' @noRd
#'
validate_mito_core <- function(
    annotations_fn = NULL,
    coverage_fn = NULL,
    params = list(),
    out_dir = NULL) {
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

  ## load coverage ----
  if (!is.null(coverage_fn)) {
    coverage <- tryCatch(
      read.csv(coverage_fn),
      error = function(e) {
        stop("Invalid coverage.")
      }
    )
    # Outlier-masked values are written with a leading "#"; strip it so the
    # numeric comparisons below don't fall back to string comparison.
    for (col in c("MeanDepth", "ErrorRate")) {
      if (col %in% names(coverage)) {
        coverage[[col]] <- suppressWarnings(
          as.numeric(sub("^#", "", coverage[[col]]))
        )
      }
    }
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

  ## Prepare rules ----
  rules <- rules |>
    purrr::map(~ modifyList(default_rules[[.x$type]] %||% list(), .x))
  # Non-standard genes (e.g. MitoFinder CDS with no canonical name) inherit the
  # default PCG ruleset so they are validated like any other PCG.
  rules <- augment_rules_for_unknown_genes(rules, annotations, default_rules)

  ## contig lengths ----
  # Needed to measure a feature that spans the origin of a circular contig, which
  # is stored pos1 > pos2. The coverage table carries one row per base, so its
  # highest Position is the contig length; without coverage, fall back to the
  # furthest annotated base.
  contig_lens <- if (!is.null(coverage) && all(c("SeqId", "Position") %in% names(coverage))) {
    tapply(coverage$Position, coverage$SeqId, max, na.rm = TRUE)
  } else {
    tapply(
      pmax(annotations$pos1, annotations$pos2), annotations$contig,
      max, na.rm = TRUE
    )
  }
  ctg_len <- function(seqid) {
    L <- suppressWarnings(as.numeric(contig_lens[as.character(seqid)]))
    if (length(L) != 1L || is.na(L)) {
      L <- max(c(annotations$pos1, annotations$pos2), na.rm = TRUE)
    }
    L
  }

  # counter for warnings
  total_warnings <- 0

  # Validate counts ----
  missing <- NA_character_
  extra <- NA_character_
  for (gene in names(rules)) {
    gene_rules <- rules[[gene]]

    # check if gene can have introns
    has_intron <- !(
      is.null(gene_rules$intron) ||
        length(gene_rules$intron) == 0 ||
        is.na(gene_rules$intron[1])
    ) && as.logical(gene_rules$intron[1])

    gene_annotations <- annotations |>
      dplyr::filter(gene == {{ gene }})

    ## Missing ----
    if (nrow(gene_annotations) < min(gene_rules$count %||% 1)) {
      missing <- semicolon_paste(missing, gene)
      next
    }

    ## Duplication ----
    if (nrow(gene_annotations) > max(gene_rules$count %||% 1)) {
      if (!(has_intron)) { # do not issue duplicate warnings for genes that may have introns
        extra <- semicolon_paste(extra, gene)
        annotations$warnings[annotations$gene == gene] <- semicolon_paste(annotations$warnings[annotations$gene == gene], "possible duplicate")
        total_warnings <- total_warnings + 1
      }
    }

    ## Intron check ----
    if (has_intron) {
      target_idx <- which(annotations$gene == gene)
      # check to make sure all CDS regions are in the same orientation
      if (all(gene_annotations$direction == "+")) {
        for (i in 1:nrow(gene_annotations)) {
          annotations$notes[target_idx[i]] <- semicolon_paste(annotations$notes[target_idx[i]], stringr::str_glue(paste0("EXON ", i)))
        }
      } else if (all(gene_annotations$direction == "-")) {
        for (i in 1:nrow(gene_annotations)) {
          annotations$notes[target_idx[nrow(gene_annotations) - i + 1]] <- semicolon_paste(annotations$notes[target_idx[nrow(gene_annotations) - i + 1]], stringr::str_glue(paste0("EXON ", i)))
        }
      } else {
        annotations$warnings[annotations$gene == gene] <- semicolon_paste(annotations$warnings[annotations$gene == gene], "exons on opposite strands")
        total_warnings <- total_warnings + 1
      }
    }
  }

  # Validate individual annotations ----
  for (i in seq_len(nrow(annotations))) {
    list2env(annotations[i, ], envir = environment())
    gene_rules <- rules[[gene]]
    # An origin-spanning feature is stored pos1 > pos2; measure it around the
    # circle rather than trusting the stored length or plain interval maths.
    L_i <- ctg_len(contig)
    feat_length <- if (isTRUE(pos1 > pos2)) circ_len(pos1, pos2, L_i) else length

    ## Overlaps ----
    # logic to handle case when there are no other annotations on the same strand
    if(nrow(dplyr::filter(annotations[-i, ], contig == {{ contig }} & direction == {{ direction }})) > 0L){
      overlapping <- annotations[-i, ] |>
        dplyr::filter(contig == {{ contig }} & direction == {{ direction }}) |>
        dplyr::rowwise() |>
        dplyr::mutate(
          overlap = circ_overlap_len({{ pos1 }}, {{ pos2 }}, pos1, pos2, L_i)
        ) |>
        dplyr::filter(overlap > 0L)
    } else {
      overlapping <- dplyr::filter(annotations[-i, ], contig == {{ contig }} & direction == {{ direction }})
    }
    # Max Overlap
    if (nrow(overlapping) > 0L && any(overlapping$overlap / feat_length > max_overlap)) {
      annotations$warnings[i] <- warnings <- semicolon_paste(warnings, "exceeds max overlap")
      total_warnings <- total_warnings + 1
    } else if (nrow(overlapping) > 0L && !is.null(gene_rules$overlap)) {
      # Gene specific START overlap rules
      while (direction == "+") {
        if (i == 1) break
        start_ol <- overlapping |>
          dplyr::filter(circ_overlap({{ pos1 }}, {{ pos1 }}, pos1, pos2))
        if (nrow(start_ol) == 0L) break
        if (max(start_ol$overlap) > gene_rules$overlap$start) {
          annotations$warnings[i] <- warnings <- semicolon_paste(warnings, "exceeds max start overlap")
          total_warnings <- total_warnings + 1
        }
        break
      }
      while (direction == "-") {
        if (i == nrow(annotations)) break
        start_ol <- overlapping |>
          dplyr::filter(circ_overlap({{ pos2 }}, {{ pos2 }}, pos1, pos2))
        if (nrow(start_ol) == 0L) break
        if (max(start_ol$overlap) > gene_rules$overlap$start) {
          annotations$warnings[i] <- warnings <- semicolon_paste(warnings, "exceeds max start overlap")
          total_warnings <- total_warnings + 1
        }
        break
      }
      # Gene specific STOP overlap rules
      while (direction == "+") {
        if (i == nrow(annotations)) break
        stop_ol <- overlapping |>
          dplyr::filter(circ_overlap({{ pos2 }}, {{ pos2 }}, pos1, pos2))
        if (nrow(stop_ol) == 0L) break
        if (max(stop_ol$overlap) > 1 && !gene_rules$overlap$stop) {
          annotations$warnings[i] <- warnings <- semicolon_paste(warnings, "exceeds max stop overlap")
          total_warnings <- total_warnings + 1
        }
        break
      }
      while (direction == "-") {
        if (i == 1) break
        stop_ol <- overlapping |>
          dplyr::filter(circ_overlap({{ pos1 }}, {{ pos1 }}, pos1, pos2))
        if (nrow(stop_ol) == 0L) break
        if (max(stop_ol$overlap) > 1 && !gene_rules$overlap$stop) {
          annotations$warnings[i] <- warnings <- semicolon_paste(warnings, "exceeds max stop overlap")
          total_warnings <- total_warnings + 1
        }
        break
      }
    }

    ## tRNA within PCG or rRNA ----
    if (type == "tRNA") {
      # circular containment: the whole tRNA arc falls inside the other feature
      containing <- annotations[-i, ] |>
        dplyr::filter(contig == {{ contig }} & type %in% c("PCG", "rRNA")) |>
        dplyr::rowwise() |>
        dplyr::filter(
          circ_overlap_len(pos1, pos2, {{ pos1 }}, {{ pos2 }}, L_i) ==
            circ_len({{ pos1 }}, {{ pos2 }}, L_i)
        ) |>
        dplyr::ungroup()
      if (nrow(containing) > 0L) {
        annotations$warnings[i] <- warnings <- semicolon_paste(warnings, "tRNA within PCG or rRNA")
        total_warnings = total_warnings + 1
      }
      # Flag low-confidence tRNAs whose anticodon could not be resolved ("NNN").
      # These only reach validation when the user enabled retain_low_conf_trna.
      if (isTRUE(annotations$anticodon[i] == "NNN")) {
        annotations$warnings[i] <- warnings <- semicolon_paste(warnings, "low-confidence tRNA (NNN anticodon)")
        total_warnings = total_warnings + 1
      }
    }

    ## Length limits ----
    if (!is.na(gene_rules$max_len %||% NA) && feat_length > gene_rules$max_len) {
      annotations$warnings[i] <- warnings <- semicolon_paste(warnings, "exceeds max length")
      total_warnings <- total_warnings + 1
    }
    if (!is.na(gene_rules$min_len %||% NA) && feat_length < gene_rules$min_len) {
      annotations$warnings[i] <- warnings <- semicolon_paste(warnings, "below min length")
      total_warnings <- total_warnings + 1
    }

    ## Coverage and Error Rate----
    ## TODO! Heuristics need review (maybe put in params)
    if (!is.null(coverage)) {
      gene_coverage <- if (pos1 <= pos2) {
        dplyr::filter(coverage, SeqId == {{ contig }} & Position >= pos1 & Position <= pos2)
      } else {
        # feature wraps the circular origin (pos1 > pos2)
        dplyr::filter(coverage, SeqId == {{ contig }} & (Position >= pos1 | Position <= pos2))
      }
      n_cov <- nrow(gene_coverage)
      if (n_cov > 0L && sum(gene_coverage$MeanDepth <= 10, na.rm = TRUE) / n_cov > 0.05) {
        annotations$warnings[i] <- warnings <- semicolon_paste(warnings, "low coverage region")
        total_warnings <- total_warnings + 1
      }
      if (n_cov > 0L && sum(gene_coverage$ErrorRate >= 0.05, na.rm = TRUE) / n_cov > 0.05) {
        annotations$warnings[i] <- warnings <- semicolon_paste(warnings, "high error region")
        total_warnings <- total_warnings + 1
      }
    }

    ## Stop if not PCG ----
    if (type != "PCG") next
    refHits <- json_parse(refHits[[1]], TRUE)

    ## Internal Stop codons ----
    if (!is.na(translation) && stringr::str_detect(translation, "\\*")) {
      annotations$warnings[i] <- warnings <- semicolon_paste(warnings, "internal stop codon")
      total_warnings <- total_warnings + 1
    }

    ## Improper Stop ----
    if (!is.na(stop_codon) && stop_codon %nin% gene_rules$stop_codons) {
      annotations$warnings[i] <- warnings <- semicolon_paste(warnings, "non-standard stop codon")
      total_warnings <- total_warnings + 1
    }

    ## Improper Start ----
    if (!is.na(start_codon) && start_codon %nin% gene_rules$start_codons) {
      annotations$warnings[i] <- warnings <- semicolon_paste(warnings, "non-standard start codon")
      total_warnings <- total_warnings + 1
    }

    ## Ref Similarity ----
    if (!any(refHits$similarity >= hit_threshold, na.rm = TRUE)) {
      annotations$warnings[i] <- warnings <- semicolon_paste(warnings, "low reference similarity")
      total_warnings <- total_warnings + 1
    }


    ## Ref alignments ----
    if (!any(refHits$gap_leading == 0L, na.rm = TRUE)) {
      annotations$warnings[i] <- warnings <- semicolon_paste(warnings, "check reference start alignment")
      total_warnings <- total_warnings + 1
    }
    if (!any(refHits$gap_trailing == 0L, na.rm = TRUE)) {
      annotations$warnings[i] <- warnings <- semicolon_paste(warnings, "check reference stop alignment")
      total_warnings <- total_warnings + 1
    }
  }

  # de-duplicate the "extra" field
  if (!is.na(extra)) {
    extra <- extra |>
      strsplit(";") |>
      unlist() |>
      unique() |>
      paste(collapse = ";")
  }

  # Final Summary ----
  summary <- data.frame(
    scaffolds = length(unique(annotations$contig)),
    structure = annotations |>
      dplyr::pull(gene) |>
      paste(collapse = "|"),
    PCGCount = sum(annotations$type == "PCG"),
    tRNACount = sum(annotations$type == "tRNA"),
    rRNACount = sum(annotations$type == "rRNA"),
    missing = missing,
    extra = extra,
    warnings = total_warnings
    # warnings = sum(!is.na(annotations$warnings))
  )

  # Outputs ----
  readr::write_tsv(
    annotations,
    file.path(
      out_dir,
      basename(annotations_fn) |> stringr::str_replace(".csv$", ".tsv")
    ),
    na = "", escape = "none"
  )
  readr::write_csv(
    summary,
    file.path(
      out_dir,
      stringr::str_replace(basename(annotations_fn), "_annotations_", "_summary_")
    ),
    quote = "none", na = ""
  )

  return(invisible(list(annotations = annotations, summary = summary)))
}
