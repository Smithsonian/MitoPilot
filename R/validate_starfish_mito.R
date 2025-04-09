#' Title
#'
#' @param annotations_fn path to annotations file (csv)
#' @param coverage_fn path to coverage file (csv)
#' @param params nested list of curation/validation parameters. Can also be
#'   provided as a base64 encoded JSON string.
#' @param out_dir output directory
#'
#' @export
#'
validate_starfish_mito <- function(
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

  # Validate counts ----
  missing <- NA_character_
  extra <- 0
  for (gene in names(rules)) {
    gene_rules <- rules[[gene]]
    gene_annotations <- annotations |>
      dplyr::filter(gene == {{ gene }})

    ## Missing ----
    if (nrow(gene_annotations) < min(gene_rules$count %||% 1)) {
      missing <- semicolon_paste(missing, gene)
      next
    }

    ## Duplication ----
    if (nrow(gene_annotations) > max(gene_rules$count)) {
      extra <- extra + nrow(gene_annotations) - max(gene_rules$count)
      annotations$warnings[annotations$gene == gene] <- semicolon_paste(annotations$warnings[annotations$gene == gene], "possible duplicate")
    }
  }

  # Validate individual annotations ----
  for (i in seq_len(nrow(annotations))) {
    list2env(annotations[i, ], envir = environment())
    gene_rules <- rules[[gene]]

    ## Overlaps ----
    overlapping <- annotations[-i, ] |>
      dplyr::filter(contig == {{ contig }} & direction == {{ direction }}) |>
      dplyr::rowwise() |>
      dplyr::mutate(
        overlap = length(intersect(seq(pos1, pos2), seq({{ pos1 }}, {{ pos2 }})))
      ) |>
      dplyr::filter(overlap > 0L)
    # Max Overlap
    if (nrow(overlapping) > 0L && any(overlapping$overlap / length > max_overlap)) {
      annotations$warnings[i] <- warnings <- semicolon_paste(warnings, "exceeds max overlap")
    } else if (nrow(overlapping) > 0L && !is.null(gene_rules$overlap)) {
      # Gene specific START overlap rules
      while (direction == "+") {
        if (i == 1) break
        start_ol <- overlapping |>
          dplyr::filter({{ pos1 }} %in% seq(pos1, pos2))
        if (nrow(start_ol) == 0L) break
        if (max(start_ol$overlap) > gene_rules$overlap$start) {
          annotations$warnings[i] <- warnings <- semicolon_paste(warnings, "exceeds max start overlap")
        }
        break
      }
      while (direction == "-") {
        if (i == nrow(annotations)) break
        start_ol <- overlapping |>
          dplyr::filter({{ pos2 }} %in% seq(pos1, pos2))
        if (nrow(start_ol) == 0L) break
        if (max(start_ol$overlap) > gene_rules$overlap$start) {
          annotations$warnings[i] <- warnings <- semicolon_paste(warnings, "exceeds max start overlap")
        }
        break
      }
      # Gene specific STOP overlap rules
      while (direction == "+") {
        if (i == nrow(annotations)) break
        stop_ol <- overlapping |>
          dplyr::filter({{ pos2 }} %in% seq(pos1, pos2))
        if (nrow(stop_ol) == 0L) break
        if (max(stop_ol$overlap) > 1 && !gene_rules$overlap$stop) {
          annotations$warnings[i] <- warnings <- semicolon_paste(warnings, "exceeds max stop overlap")
        }
        break
      }
      while (direction == "-") {
        if (i == 1) break
        stop_ol <- overlapping |>
          dplyr::filter({{ pos1 }} %in% seq(pos1, pos2))
        if (nrow(stop_ol) == 0L) break
        if (max(stop_ol$overlap) > 1 && !gene_rules$overlap$stop) {
          annotations$warnings[i] <- warnings <- semicolon_paste(warnings, "exceeds max stop overlap")
        }
        break
      }
    }

    ## Length limits ----
    if (!is.na(gene_rules$max_len %||% NA) && length > gene_rules$max_len) {
      annotations$warnings[i] <- warnings <- semicolon_paste(warnings, "exceeds max length")
    }
    if (!is.na(gene_rules$min_len %||% NA) && length < gene_rules$min_len) {
      annotations$warnings[i] <- warnings <- semicolon_paste(warnings, "below min length")
    }

    ## Coverage and Error Rate----
    ## TODO! Heuristics need review (maybe put in params)
    if (!is.null(coverage)) {
      gene_coverage <- coverage |>
        dplyr::filter(SeqId == {{ contig }}) |>
        dplyr::rowwise() |>
        dplyr::filter(Position %in% pos1:pos2)
      if (sum(gene_coverage$MeanDepth <= 10) / nrow(gene_coverage) > 0.05) {
        annotations$warnings[i] <- warnings <- semicolon_paste(warnings, "low coverage region")
      }
      if (sum(gene_coverage$ErrorRate >= 0.05) / nrow(gene_coverage) > 0.05) {
        annotations$warnings[i] <- warnings <- semicolon_paste(warnings, "high error region")
      }
    }

    ## Stop if not PCG ----
    if (type != "PCG") next
    refHits <- json_parse(refHits[[1]], TRUE)

    ## Internal Stop codons ----
    if (!is.na(translation) && stringr::str_detect(translation, "\\*")) {
      annotations$warnings[i] <- warnings <- semicolon_paste(warnings, "internal stop codon")
    }

    ## Improper Stop ----
    if (!is.na(stop_codon) && stop_codon %nin% gene_rules$stop_codons) {
      annotations$warnings[i] <- warnings <- semicolon_paste(warnings, "non-standard stop codon")
    }

    ## Improper Start ----
    if (!is.na(start_codon) && start_codon %nin% gene_rules$start_codons) {
      annotations$warnings[i] <- warnings <- semicolon_paste(warnings, "non-standard start codon")
    }

    ## Ref Similarity ----
    if (!any(refHits$similarity >= hit_threshold)) {
      annotations$warnings[i] <- warnings <- semicolon_paste(warnings, "low reference similarity")
    }


    ## Ref alignments ----
    if (!any(refHits$gap_leading == 0L)) {
      annotations$warnings[i] <- warnings <- semicolon_paste(warnings, "check reference start alignment")
    }
    if (!any(refHits$gap_trailing == 0L)) {
      annotations$warnings[i] <- warnings <- semicolon_paste(warnings, "check reference stop alignment")
    }
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
    warnings = sum(!is.na(annotations$warnings))
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
