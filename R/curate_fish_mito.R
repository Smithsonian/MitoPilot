#' Title
#'
#' @param annotations_fn
#' @param assembly_fn
#' @param coverage_fn
#' @param genetic_code
#' @param out_dir
#' @param params
#'
#' @export
#'
curate_fish_mito <- function(
    annotations_fn = "~/Jonah/MitoPilot-testing/work/36/1b84646ab90f4dc787453529cd1162/SRR22396740/annotate/SRR22396740_annotations_1.csv",
    assembly_fn = "~/Jonah/MitoPilot-testing/work/36/1b84646ab90f4dc787453529cd1162/SRR22396740/annotate/SRR22396740_assembly_1.fasta",
    coverage_fn = "~/Jonah/MitoPilot-testing/work/36/1b84646ab90f4dc787453529cd1162/SRR22396740/annotate/SRR22396740_coverageStats_1.csv",
    genetic_code = 2,
    out_dir = NULL,
    params = NULL) {
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

  ## Set genetic code ----
  genetic_code <- tryCatch(
    Biostrings::getGeneticCode(as.character(genetic_code)),
    error = function(e) {
      stop("Invalid genetic code.")
    }
  )

  # rRNA ----
  ## enforce + strand ----
  rRNA_rev <- annotations |>
    dplyr::filter(type == "rRNA") |>
    dplyr::filter(all(direction == "-"), .by = "contig") |>
    dplyr::pull(contig) |>
    unique()
  for (seqid in rRNA_rev) {
    annotations_updated <- annotations |>
      dplyr::filter(contig == seqid) |>
      dplyr::mutate(
        pos1_old = pos1,
        pos2_old = pos2,
        pos1 = assembly[seqid]@ranges@width - pos2_old + 1,
        pos2 = assembly[seqid]@ranges@width - pos1_old + 1,
        direction = dplyr::case_match(
          direction, "+" ~ "-", "-" ~ "+"
        )
      ) |>
      select(-pos1_old, -pos2_old)

    annotations <- annotations |>
      dplyr::filter(contig != seqid) |>
      dplyr::bind_rows(
        annotations_updated
      ) |>
      dplyr::arrange(contig, pos1)

    if (!is.null(coverage)) {
      coverage_flip <- coverage |>
        filter(SeqId == seqid) |>
        arrange(desc(Position)) |>
        mutate(Position = row_number())
      coverage <- bind_rows(
        coverage |> filter(SeqId != seqid),
        coverage_flip
      )
    }

    assembly[seqid] <- assembly[seqid] |>
      reverseComplement()
  }

  ## apply punctuation model ----
  gene_idx <- which(annotations$type == "rRNA")
  for (idx in gene_idx) {
    # Apply tRNA punctuation model
    before <- annotations[idx - 1, ] |>
      dplyr::filter(type == "tRNA" & contig == annotations$contig[idx])
    if (nrow(before) == 1) {
      if (annotations$pos1[idx] - before$pos2 != 1) {
        pos1_new <- before$pos2 + 1
        if (annotations$pos2[idx] - pos1_new + 1 <= rules[[annotations$gene[idx]]][["max_len"]]) {
          pos1_change <- pos1_new - annotations$pos1[idx]
          annotations$pos1[idx] <- pos1_new
          annotations$notes[idx] <- semicolon_paste(
            annotations$notes[idx],
            stringr::str_glue("Applied punctutation model- moved pos1 by {pos1_change} bp")
          )
        }
      }
    }
    after <- annotations[idx + 1, ] |>
      dplyr::filter(type == "tRNA" & contig == annotations$contig[idx])
    if (nrow(after) == 1) {
      if (after$pos1 - annotations$pos2[idx] != 1) {
        pos2_new <- after$pos1 - 1
        if (pos2_new - annotations$pos1[idx] + 1 <= rules[[annotations$gene[idx]]][["max_len"]]) {
          pos2_change <- pos2_new - annotations$pos2[idx]
          annotations$pos2[idx] <- pos2_new
          annotations$notes[idx] <- semicolon_paste(
            annotations$notes[idx],
            stringr::str_glue("Applied punctutation model - moved pos2 by {pos2_change} bp")
          )
        }
      }
    }
    annotations$length[idx] <- 1 + abs(annotations$pos2[idx] - annotations$pos1[idx])
  }

  # PCGs ----
  ## Get top ref hits for each PCG ----
  annotations$refHits <- annotations |>
    dplyr::select(type, gene, geneId, contig, translation) |>
    purrr::pmap(function(type, gene, geneId, contig, translation) {
      if (type != "PCG") {
        return(NA)
      }

      ## Gene ref database ----
      ref_db <- ref_dbs[[gene]] %||% ref_dbs[["default"]] |>
        stringr::str_glue()

      get_top_hits(ref_db, translation) |>
        json_string()
    })

  ## Curate against top hits ----
  annotations <- annotations |> purrr::pmap_dfr(function(...) {
    cur <- list(...)
    if (cur$type != "PCG") {
      return(cur)
    }
    list2env(cur, envir = environment())

    # Stop if ho hits above threshold
    refHits <- json_parse(refHits[[1]], TRUE)
    if (nrow(refHits) == 0L || !any(refHits$similarity >= hit_threshold)) {
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
    if (refHits$gap_leading[1] != 0 && (sum(refHits$gap_leading > 0L) / nrow(refHits)) > 0.5) {
      gaps_target <- max(refHits$gap_leading)
      if (direction == "+") {
        while (gaps_target > 0) {
          pos1_new <- pos1 - (3 * gaps_target)
          if (pos1_new <= 0) {
            gaps_target <- gaps_target - 1
            next
          }
          new_start_codon <- Biostrings::subseq(assembly[contig_key[contig]], pos1_new, pos1_new + 2) |>
            as.character()
          if (!new_start_codon %in% start_opts) {
            gaps_target <- gaps_target - 1
            next
          }
          cur$notes <- semicolon_paste(cur$notes, stringr::str_glue("extending start {pos1 - pos1_new} bp"))
          cur$pos1 <- pos1 <- pos1_new
          cur$length <- length <- abs(pos2 - pos1) + 1
          cur$start_codon <- start_codon <- new_start_codon
          cur$translation <- translation <- Biostrings::subseq(
            assembly[contig_key[contig]],
            pos1,
            pos2 - nchar(stop_codon)
          ) |>
            Biostrings::translate(genetic.code = genetic_code) |>
            as.character()
          refHits <- get_top_hits(
            stringr::str_glue(ref_dbs[[gene]] %||% ref_dbs[["default"]]),
            translation
          )
          break
        }
      }
      if (direction == "-") {
        while (gaps_target > 0) {
          pos2_new <- pos2 + (3 * gaps_target)
          if (pos2_new >= assembly[contig_key[contig]]@ranges@width) {
            gaps_target <- gaps_target - 1
            next
          }
          new_start_codon <- Biostrings::subseq(assembly[contig_key[contig]], pos2_new - 2, pos2_new) |>
            Biostrings::reverseComplement() |>
            as.character()
          if (!new_start_codon %in% start_opts) {
            gaps_target <- gaps_target - 1
            next
          }
          cur$notes <- notes <- semicolon_paste(notes, stringr::str_glue("extending start {pos2_new - pos2} bp"))
          cur$pos2 <- pos2 <- pos2_new
          cur$length <- length <- abs(pos2 - pos1) + 1
          cur$start_codon <- start_codon <- new_start_codon
          cur$translation <- translation <- Biostrings::subseq(
            assembly[contig_key[contig]],
            pos1 + nchar(stop_codon),
            pos2
          ) |>
            Biostrings::reverseComplement() |>
            Biostrings::translate(genetic.code = genetic_code) |>
            as.character()
          refHits <- get_top_hits(
            stringr::str_glue(ref_dbs[[gene]] %||% ref_dbs[["default"]]),
            translation
          )
          break
        }
      }
    }

    ### STOP ----
    if (refHits$gap_trailing[1] != 0 && (sum(refHits$gap_trailing > 0L) / nrow(refHits)) > 0.5) {
      gaps_target <- max(refHits$gap_trailing)
      if (direction == "+") {
        while (gaps_target > 0) {
          pos2_new <- pos2 - nchar(stop_codon) + (3 * gaps_target)
          if ((pos2_new + 1) > assembly[contig_key[contig]]@ranges@width) {
            gaps_target <- gaps_target - 1
            next
          }
          new_stop_codon <- Biostrings::subseq(
            assembly[contig_key[contig]],
            pos2_new + 1,
            min(pos2_new + 3, assembly[contig_key[contig]]@ranges@width)
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
          cur$notes <- notes <- semicolon_paste(
            notes,
            stringr::str_glue("extending end {abs(pos2_new - pos2)} bp")
          )
          cur$pos2 <- pos2 <- pos2_new
          cur$length <- length <- abs(pos2 - pos1) + 1
          cur$stop_codon <- stop_codon <- new_stop_codon
          cur$translation <- translation <- Biostrings::subseq(
            assembly[contig_key[contig]],
            pos1,
            pos2 - nchar(stop_codon)
          ) |>
            Biostrings::translate(genetic.code = genetic_code) |>
            as.character()
          refHits <- get_top_hits(
            stringr::str_glue(ref_dbs[[gene]] %||% ref_dbs[["default"]]),
            translation
          )
          break
        }
      }
      if (direction == "-") {
        while (gaps_target > 0) {
          pos1_new <- pos1 + nchar(stop_codon) - (3 * gaps_target)
          if (pos1_new - 1 <= 1) {
            gaps_target <- gaps_target - 1
            next
          }
          new_stop_codon <- Biostrings::subseq(assembly[contig_key[contig]], max(pos1_new - 3, 1), pos1_new - 1) |>
            Biostrings::reverseComplement() |>
            as.character()
          while (nchar(new_stop_codon) > 0 && !new_stop_codon %in% stop_opts) {
            new_stop_codon <- stringr::str_remove(new_stop_codon, ".$")
            pos1_new <- pos1_new + 1
          }
          if (nchar(new_stop_codon) == 0L) {
            gaps_target <- gaps_target - 1
            next
          }
          cur$notes <- notes <- semicolon_paste(
            notes,
            stringr::str_glue("extending end {abs(pos1_new - pos1)} bp")
          )
          cur$pos1 <- pos1 <- pos1_new
          cur$length <- length <- abs(pos2 - pos1) + 1
          cur$stop_codon <- stop_codon <- new_stop_codon
          cur$translation <- translation <- Biostrings::subseq(
            assembly[contig_key[contig]],
            pos1 + nchar(stop_codon),
            pos2
          ) |>
            Biostrings::reverseComplement() |>
            Biostrings::translate(genetic.code = genetic_code) |>
            as.character()
          refHits <- get_top_hits(
            stringr::str_glue(ref_dbs[[gene]] %||% ref_dbs[["default"]]),
            translation
          )
          break
        }
      }
    }

    ## Fix OVER-EXTENSION ----
    ### START ----
    if (refHits$gap_leading[1] != 0 && (sum(refHits$gap_leading < 0L) / nrow(refHits)) > 0.5) {
      if (direction == "+") {
        alt_starts <- Biostrings::subseq(
          assembly[contig_key[contig]],
          pos1 + 3,
          pos1 + (3 * max(abs(refHits$gap_leading[refHits$gap_leading < 0]))) - 1
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
          translation_new <- Biostrings::subseq(assembly[contig_key[contig]], pos1_new, pos2 - nchar(stop_codon)) |>
            Biostrings::translate(genetic.code = genetic_code) |>
            as.character()
          refHits_new <- get_top_hits(
            stringr::str_glue(ref_dbs[[gene]] %||% ref_dbs[["default"]]),
            translation_new
          )
          if (refHits_new$gap_leading[1] != 0 && (sum(refHits_new$gap_leading < 0L) / nrow(refHits_new)) > 0.5) {
            alt_idx <- alt_idx + 1
            next
          }
          cur$notes <- notes <- semicolon_paste(
            notes,
            stringr::str_glue("trimming start {abs(pos1_new - pos1)} bp")
          )
          cur$pos1 <- pos1 <- pos1_new
          cur$length <- length <- abs(pos2 - pos1) + 1
          cur$start_codon <- start_codon <- names(alt_starts)[alt_idx]
          cur$translation <- translation <- translation_new
          refHits <- refHits_new
          break
        }
      }
      if (direction == "-") {
        alt_starts <- Biostrings::subseq(
          assembly[contig_key[contig]],
          pos2 - (3 * max(abs(refHits$gap_leading[refHits$gap_leading < 0]))) + 1,
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
          translation_new <- Biostrings::subseq(assembly[contig_key[contig]], pos1 + nchar(stop_codon), pos2_new) |>
            Biostrings::reverseComplement() |>
            Biostrings::translate(genetic.code = genetic_code) |>
            as.character()
          refHits_new <- get_top_hits(
            stringr::str_glue(ref_dbs[[gene]] %||% ref_dbs[["default"]]),
            translation_new
          )
          if (refHits_new$gap_leading[1] != 0 && (sum(refHits_new$gap_leading < 0L) / nrow(refHits_new)) > 0.5) {
            alt_idx <- alt_idx + 1
            next
          }
          cur$notes <- notes <- semicolon_paste(
            notes,
            stringr::str_glue("trimming start {abs(pos2_new - pos2)} bp")
          )
          cur$pos2 <- pos2 <- pos2_new
          cur$length <- length <- abs(pos2 - pos1) + 1
          cur$start_codon <- start_codon <- new_start_codon
          cur$translation <- translation <- translation_new
          refHits <- refHits_new
        }
      }
    }

    ### STOP ----
    if (refHits$gap_trailing[1] != 0 && (sum(refHits$gap_trailing < 0L) / nrow(refHits)) > 0.5) {
      if (direction == "+") {
        alt_stops <- Biostrings::subseq(
          assembly[contig_key[contig]],
          pos2 - nchar(stop_codon) - (3 * max(abs(refHits$gap_trailing[refHits$gap_trailing < 0]))) + 1,
          pos2 - nchar(stop_codon)
        ) |>
          as.character() |>
          stringr::str_extract_all(".{1,3}") |>
          unlist() |>
          purrr::map(~ {
            s <- stringr::str_extract(.x, paste0("^", stop_opts)) |>
              na.omit()
            s[1] |> unlist()
          })
        alt_idx <- 1
        while (alt_idx <= length(alt_stops)) {
          if (is.na(alt_stops[[alt_idx]])) {
            alt_idx <- alt_idx + 1
            next
          }
          pos2_new <- pos2 - nchar(stop_codon) - (3 * alt_idx) - (3 - nchar(alt_stops[[alt_idx]]))
          translation_new <- Biostrings::subseq(
            assembly[contig_key[contig]],
            pos1,
            pos2_new - nchar(alt_stops[[alt_idx]])
          ) |>
            Biostrings::translate(genetic.code = genetic_code) |>
            as.character()
          refHits_new <- get_top_hits(
            stringr::str_glue(ref_dbs[[gene]] %||% ref_dbs[["default"]]),
            translation_new
          )
          if (refHits_new$gap_trailing[1] != 0 && (sum(refHits_new$gap_trailing < 0L) / nrow(refHits_new)) > 0.5) {
            alt_idx <- alt_idx + 1
            next
          }
          cur$notes <- notes <- semicolon_paste(
            notes,
            stringr::str_glue("trimming end {abs(pos2_new - pos2)} bp")
          )
          cur$pos2 <- pos2 <- pos2_new
          cur$length <- length <- abs(pos2 - pos1) + 1
          cur$stop_codon <- stop_codon <- alt_stops[[alt_idx]]
          cur$translation <- translation <- translation_new
          refHits <- refHits_new
        }
      }
      if (direction == "-") {
        alt_stops <- Biostrings::subseq(
          assembly[contig_key[contig]],
          pos1 + nchar(stop_codon),
          pos1 + nchar(stop_codon) + (3 * max(abs(refHits$gap_trailing[refHits$gap_trailing < 0]))) - 1
        ) |>
          Biostrings::reverseComplement() |>
          as.character() |>
          stringr::str_extract_all(".{1,3}") |>
          unlist() |>
          purrr::map(~ {
            s <- stringr::str_extract(.x, paste0("^", stop_opts)) |>
              na.omit()
            s[1] |> unlist()
          })
        alt_idx <- 1
        while (alt_idx <= length(alt_stops)) {
          if (is.na(alt_stops[[alt_idx]])) {
            alt_idx <- alt_idx + 1
            next
          }
          pos1_new <- pos1 + nchar(stop_codon) + (3 * alt_idx) + (3 - nchar(alt_stops[[alt_idx]]))
          translation_new <- Biostrings::subseq(
            assembly[contig_key[contig]],
            pos1 + nchar(alt_stops[[alt_idx]]),
            pos2
          ) |>
            Biostrings::reverseComplement() |>
            Biostrings::translate(genetic.code = genetic_code) |>
            as.character()
          refHits_new <- get_top_hits(
            stringr::str_glue(ref_dbs[[gene]] %||% ref_dbs[["default"]]),
            translation_new
          )
          if (refHits_new$gap_trailing[1] != 0 && (sum(refHits_new$gap_trailing < 0L) / nrow(refHits_new)) > 0.5) {
            alt_idx <- alt_idx + 1
            next
          }
          cur$notes <- notes <- semicolon_paste(
            notes,
            stringr::str_glue("trimming end {abs(pos1_new - pos1)} bp")
          )
          cur$pos1 <- pos1 <- pos1_new
          cur$length <- length <- abs(pos2 - pos1) + 1
          cur$stop_codon <- stop_codon <- alt_stops[[alt_idx]]
          cur$translation <- translation <- translation_new
          refHits <- refHits_new
        }
      }
    }

    cur$refHits <- json_string(refHits)

    return(cur)
  })

  ## Stop codon trimming ----
  for (idx in seq_len(nrow(annotations))) {
    if (annotations$type[idx] != "PCG") next
    gene <- annotations$gene[idx]
    print(gene)
    overlap_rules <- rules[[gene]][["overlap"]]
    stop_opts <- rules[[gene]][["stop_codons"]]
    while (annotations$direction[idx] == "+") {
      if (idx == nrow(annotations)) break
      if (overlap_rules$stop) break
      codon_positions <- (annotations$pos2[idx] - nchar(annotations$stop_codon[idx]) + 1):annotations$pos2[idx]
      overlaps <- annotations[(idx + 1):nrow(annotations), ] |>
        dplyr::filter(pos1 %in% codon_positions) |>
        dplyr::filter(direction == annotations$direction[idx])
      if (nrow(overlaps) != 1L) break
      overlap <- sum(overlaps$pos1:overlaps$pos2 %in% codon_positions)
      new_stop <- stringr::str_sub(annotations$stop_codon[idx], 1, nchar(annotations$stop_codon[idx]) - overlap)
      if (nchar(new_stop) < 1) break
      if (!new_stop %in% stop_opts) break
      annotations$stop_codon[idx] <- new_stop
      annotations$pos2[idx] <- annotations$pos2[idx] - overlap
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
      codon_positions <- annotations$pos1[idx]:(annotations$pos1[idx] + nchar(annotations$stop_codon[idx]) - 1)
      overlaps <- annotations[1:(idx - 1), ] |>
        filter(pos2 %in% codon_positions) |>
        filter(direction == annotations$direction[idx])
      if (nrow(overlaps) != 1L) break
      overlap <- sum(overlaps$pos1:overlaps$pos2 %in% codon_positions)
      new_stop <- stringr::str_sub(annotations$stop_codon[idx], 1, nchar(annotations$stop_codon[idx]) - overlap)
      if (nchar(new_stop) < 1) break
      if (!new_stop %in% stop_opts) break
      annotations$stop_codon[idx] <- new_stop
      annotations$pos1[idx] <- annotations$pos1[idx] + overlap
      annotations$length[idx] <- annotations$length[idx] - overlap
      annotations$notes[idx] <- semicolon_paste(
        annotations$notes[idx],
        str_glue("stop codon trimmed by {overlap} bp")
      )
      break
    }
  }

  # End trimming ----
  # Remove un-annotated regions at the beginning or end of linear contigs
  purrr::iwalk(contig_key, ~ {
    # Skip circular contigs
    if (stringr::str_detect(.x, "circular")) {
      return()
    }
    ## Check beginning ----
    min_ann <- annotations |>
      dplyr::filter(contig == .y) |>
      dplyr::pull(pos1) |>
      min()
    if (min_annotation > 1) {
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
      coverage <<- coverage |>
        dplyr::mutate(
          Position = dplyr::case_when(
            SeqId == .y ~ Position - min_ann + 1,
            .default = Position
          )
        ) |>
        dplyr::filter(Position > 0)
    }
    ## Check end ----
    max_ann <- annotations |>
      dplyr::filter(contig == .y) |>
      dplyr::pull(pos2) |>
      max()
    if (max_annotation < assembly[.x]@ranges@width) {
      assembly[.x] <<- Biostrings::subseq(assembly[.x], 1, max_ann)
      coverage <<- coverage |>
        dplyr::filter(
          SeqId != .y | Position <= max_ann
        )
    }
  })

  # Outputs ----
  write.csv(annotations, file.path(out_dir, basename(annotations_fn)), row.names = F)
  Biostrings::writeXStringSet(assembly, file.path(out_dir, basename(assembly_fn)))
  if (!is.null(coverage_fn)) {
    write.csv(coverage, file.path(out_dir, basename(coverage_fn)), row.names = F)
  }

  return(invisible(annotations))
}
