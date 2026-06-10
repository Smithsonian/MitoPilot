#' Mitogenome Annotation Wrapper
#'
#' Uses Mitos2 and tRNAscan-SE to annotate a mitogenome assembly.
#'
#' @param assembly_fn Path to the mitogenome assembly FASTA file.
#' @param coverage_fn Path to the mitogenome assembly coverage stats CSV file.
#' @param cpus Number of CPUs to use.
#' @param genetic_code Genetic code to use for annotation (default: 2).
#' @param ref_db Reference Mitos2 database to use for annotation (default:
#'   "Chordata").
#' @param ref_dir Path to the Mitos2 reference database.
#' @param mitos_opts Additional command line options for MITOS2.
#' @param mitos_condaenv Conda environment to run MITOS2 (default: "mitos").
#' @param trnaScan_opts Additional command line options for tRNAscan-SE.
#' @param trnaScan_condaenv Conda environment to run tRNAscan-SE (default:
#'   "base").
#' @param use_arwen logical; whether to run ARWEN tRNA prediction (default: FALSE).
#' @param arwen_opts Additional command line options for ARWEN (default: "-mtx").
#' @param use_aragorn logical; whether to run ARAGORN tRNA prediction (default: FALSE).
#' @param aragorn_opts Additional command line options for ARAGORN (default: "-m -gcstd").
#' @param aragorn_condaenv Conda environment containing ARAGORN (default: "aragorn").
#' @param use_mitos_best logical; whether to pass --best to MITOS2 (default: FALSE).
#' @param start_gene name of gene (PCG, rRNA, or tRNA) to start circular assembly (default = "trnF")
#' @param coverage_trim logical; whether to trim low-coverage ends of linear assemblies (default: TRUE).
#' @param retain_low_conf_trna logical; whether to keep low-confidence tRNAs with an
#'   unresolved ("NNN") anticodon. When FALSE (default) these are dropped and are not
#'   allowed to suppress overlapping MITOS2 tRNA predictions. When TRUE they are
#'   retained (and may suppress overlapping MITOS2 predictions, the original behavior).
#' @param ignore_scaffolds Comma-separated scaffold numbers (e.g. "1,3") to drop
#'   from the assembly before annotation. These correspond to the `scaffold`
#'   column in the assemblies table and reflect the per-scaffold `ignore` flag.
#' @param out_dir Output directory.
#'
#' @export
#'
annotate <- function(
  assembly_fn = NULL,
  coverage_fn = NULL,
  cpus = 4,
  genetic_code = "2",
  ref_db = "Chordata",
  ref_dir = "/home/harpua/Jzonah/MitoPilot/ref_dbs/Mitos2",
  mitos_opts = "--intron 0 --oril 0",
  mitos_condaenv = "mitos",
  trnaScan_opts = "-M vert -X 20",
  trnaScan_condaenv = "base",
  arwen_opts = "-mtx",
  use_arwen = FALSE,
  aragorn_opts = "-m -gcstd",
  aragorn_condaenv = "aragorn",
  use_aragorn = FALSE,
  use_mitos_best = TRUE,
  start_gene = "trnF",
  coverage_trim = TRUE,
  retain_low_conf_trna = FALSE,
  ignore_scaffolds = NULL,
  # ===== TEMP (ORF-finder testing): REMOVE LATER ============================
  # When TRUE, drop MITOS2 PCG annotations so ORF finding has unannotated
  # regions to act on. Wired through annotate.nf + main.nf params.
  disable_mitos_pcg = FALSE,
  # ===== END TEMP ==========================================================
  out_dir = NULL
) {
  assembly <- Biostrings::readDNAStringSet(assembly_fn)

  # Drop scaffolds flagged ignore=1 in the assemblies table. Scaffold IDs in the
  # FASTA are <sample>.<path>.<scaffold>; match on the trailing scaffold number.
  if (!is.null(ignore_scaffolds) && nzchar(ignore_scaffolds)) {
    drop <- stringr::str_split_1(ignore_scaffolds, ",") |> stringr::str_trim()
    drop <- drop[nzchar(drop)]
    if (length(drop) > 0) {
      scaffold_nums <- stringr::str_extract(names(assembly), "(?<=\\.)\\d+(?=\\s|$)")
      keep <- !scaffold_nums %in% drop
      assembly <- assembly[keep]
    }
  }

  # Load coverage stats (always, when available — used for trimming and output)
  if (length(coverage_fn) == 1L && file.exists(coverage_fn)) {
    coverage <- read.csv(coverage_fn) |>
      dplyr::arrange(SeqId, Position) |>
      dplyr::mutate(
        mask = stringr::str_detect(MeanDepth, "^#") |
          stringr::str_detect(ErrorRate, "^#"),
        MeanDepth = as.numeric(stringr::str_remove(MeanDepth, "^#")),
        ErrorRate = as.numeric(stringr::str_remove(ErrorRate, "^#"))
      )
    if (isTRUE(coverage_trim)) {
      coverage_trimmed <- assembly |> purrr::imap(~ {
        stats <- coverage[coverage$SeqId == stringr::str_extract(.y, "\\S+"), ]
        # Skip for circular assemblies
        if (stringr::str_detect(.y, "circular")) {
          return(list(assembly = .x, stats = stats))
        }
        coverage_trim(assembly = .x, stats = stats)
      })
      assembly <- purrr::map(coverage_trimmed, ~ .x$assembly) |> Biostrings::DNAStringSet()
      coverage <- purrr::map(coverage_trimmed, ~ .x$stats) |> dplyr::bind_rows()
    }
  }

  # tRNA annotation ----
  trnaScan_out <- annotate_trnaScan(
    assembly = assembly,
    rotate = stringr::str_detect(names(assembly), "circular"),
    trnaScan_opts = trnaScan_opts,
    cpus = cpus,
    condaenv = trnaScan_condaenv
  )
  assembly <- trnaScan_out$assembly
  annotations_trnaScan <- trnaScan_out$annotations

  # ARWEN tRNA annotation ----
  annotations_arwen <- if (use_arwen) {
    annotate_arwen(
      assembly = assembly,
      arwen_opts = arwen_opts,
      genetic_code = genetic_code,
      circular = all(stringr::str_detect(names(assembly), "circular"))
    )
  } else {
    data.frame(
      contig = character(), type = character(), gene = character(),
      product = character(), pos1 = integer(), pos2 = integer(),
      length = integer(), direction = character(),
      tRNA_ID = character(), anticodon = character()
    )
  }

  # ARAGORN tRNA annotation ----
  annotations_aragorn <- if (use_aragorn) {
    annotate_aragorn(
      assembly = assembly,
      aragorn_opts = aragorn_opts,
      genetic_code = genetic_code,
      circular = all(stringr::str_detect(names(assembly), "circular")),
      condaenv = aragorn_condaenv
    )
  } else {
    data.frame(
      contig = character(), type = character(), gene = character(),
      product = character(), pos1 = integer(), pos2 = integer(),
      length = integer(), direction = character(),
      tRNA_ID = character(), anticodon = character()
    )
  }
  # Contig lengths for circular overlap detection (pos1 > pos2 = wrap-around)
  contig_lens <- setNames(
    Biostrings::width(assembly),
    stringr::str_extract(names(assembly), "\\S+")
  )
  # Circular-aware overlap helpers (circ_overlap / circ_len / circ_overlap_len)
  # are defined at package level in annotate_utils.R and shared with orf_finder().

  # Drop ARWEN predictions that overlap a tRNAscan prediction of the same gene
  # (position overlap is more robust than anticodon string matching)
  annotations_arwen <- annotations_arwen |>
    dplyr::filter(!purrr::pmap_lgl(
      list(gene, contig, pos1, pos2),
      \(g, ctg, p1, p2) any(
        annotations_trnaScan$gene == g &
          annotations_trnaScan$contig == ctg &
          circ_overlap(p1, p2, annotations_trnaScan$pos1, annotations_trnaScan$pos2)
      )
    ))

  # Drop ARWEN predictions where total overlap with any tRNAscan predictions
  # exceeds 10% of the ARWEN prediction length
  annotations_arwen <- annotations_arwen |>
    dplyr::filter(!purrr::pmap_lgl(
      list(contig, pos1, pos2),
      \(ctg, p1, p2) {
        L <- contig_lens[ctg]
        arwen_len <- circ_len(p1, p2, L)
        hits <- annotations_trnaScan[
          annotations_trnaScan$contig == ctg &
            circ_overlap(p1, p2, annotations_trnaScan$pos1, annotations_trnaScan$pos2),
        ]
        if (nrow(hits) == 0L) {
          return(FALSE)
        }
        total_overlap <- sum(purrr::map2_int(
          hits$pos1, hits$pos2, \(q1, q2) circ_overlap_len(p1, p2, q1, q2, L)
        ))
        total_overlap / arwen_len > 0.10
      }
    ))

  # Filter ARAGORN against tRNAscan + ARWEN combined (same logic as ARWEN filter)
  combined_pre_aragorn <- dplyr::bind_rows(annotations_trnaScan, annotations_arwen)
  annotations_aragorn <- annotations_aragorn |>
    dplyr::filter(!purrr::pmap_lgl(
      list(gene, contig, pos1, pos2),
      \(g, ctg, p1, p2) any(
        combined_pre_aragorn$gene == g &
          combined_pre_aragorn$contig == ctg &
          circ_overlap(p1, p2, combined_pre_aragorn$pos1, combined_pre_aragorn$pos2)
      )
    ))
  annotations_aragorn <- annotations_aragorn |>
    dplyr::filter(!purrr::pmap_lgl(
      list(contig, pos1, pos2),
      \(ctg, p1, p2) {
        L <- contig_lens[ctg]
        aragorn_len <- circ_len(p1, p2, L)
        hits <- combined_pre_aragorn[
          combined_pre_aragorn$contig == ctg &
            circ_overlap(p1, p2, combined_pre_aragorn$pos1, combined_pre_aragorn$pos2),
        ]
        if (nrow(hits) == 0L) return(FALSE)
        total_overlap <- sum(purrr::map2_int(
          hits$pos1, hits$pos2, \(q1, q2) circ_overlap_len(p1, p2, q1, q2, L)
        ))
        total_overlap / aragorn_len > 0.10
      }
    ))

  # Mitos2 annotation ----
  effective_mitos_opts <- if (isTRUE(use_mitos_best)) {
    paste("--best", mitos_opts)
  } else {
    mitos_opts
  }
  annotations_mitos <- annotate_mitos2(
    assembly = assembly,
    topology = ifelse(all(stringr::str_detect(names(assembly), "circular")), "circular", "linear"),
    genetic_code = genetic_code,
    ref_db = ref_db,
    mitos_opts = effective_mitos_opts,
    condaenv = mitos_condaenv
  )

  # ===== TEMP (ORF-finder testing): REMOVE LATER ============================
  if (isTRUE(disable_mitos_pcg)) {
    message("[TEMP] disable_mitos_pcg=TRUE: dropping MITOS2 PCG annotations")
    annotations_mitos <- dplyr::filter(annotations_mitos, type != "PCG")
  }
  # ===== END TEMP ==========================================================

  # Combine annotations ----
  # Priority: tRNAscan > ARWEN > ARAGORN > MITOS2
  # Normalize trnL/trnS variants before deduplication so gene names match across tools
  normalize_trna_gene <- function(gene) {
    gene |>
      stringr::str_replace("trnL1|trnL2", "trnL") |>
      stringr::str_replace("trnS1|trnS2", "trnS")
  }
  combined_trna <- dplyr::bind_rows(annotations_trnaScan, annotations_arwen, annotations_aragorn) |>
    dplyr::mutate(gene = normalize_trna_gene(gene))
  # Unless explicitly retaining low-confidence tRNAs, drop "NNN"-anticodon calls so
  # they cannot suppress valid overlapping MITOS2 tRNA predictions (they are removed
  # from the final output below anyway).
  if (!isTRUE(retain_low_conf_trna)) {
    combined_trna <- combined_trna |>
      dplyr::filter(is.na(anticodon) | anticodon != "NNN")
  }
  annotations_mitos <- annotations_mitos |>
    dplyr::mutate(gene = normalize_trna_gene(gene)) |>
    dplyr::filter(
      type != "tRNA" |
        !purrr::pmap_lgl(
          list(contig, pos1, pos2),
          \(ctg, p1, p2) {
            L <- contig_lens[ctg]
            mitos_len <- circ_len(p1, p2, L)
            hits <- combined_trna[
              combined_trna$contig == ctg &
                circ_overlap(p1, p2, combined_trna$pos1, combined_trna$pos2),
            ]
            if (nrow(hits) == 0L) {
              return(FALSE)
            }
            total_overlap <- sum(purrr::map2_int(
              hits$pos1, hits$pos2, \(q1, q2) circ_overlap_len(p1, p2, q1, q2, L)
            ))
            total_overlap / mitos_len > 0.10
          }
        )
    )
  annotations <- dplyr::bind_rows(
    dplyr::mutate(annotations_trnaScan, tool = "tRNAscan-SE"),
    dplyr::mutate(annotations_arwen,    tool = "ARWEN"),
    dplyr::mutate(annotations_aragorn,  tool = "ARAGORN"),
    dplyr::mutate(annotations_mitos,    tool = "MITOS2")
  ) |>
    dplyr::select(-dplyr::any_of("tRNA_ID")) |> # remove temporary tRNA_ID column
    dplyr::mutate(dplyr::across("gene", stringr::str_replace, "trnS1|tnrS2", "trnS")) |> # Rename trnS1 and trnS2 to trnS
    dplyr::mutate(dplyr::across("gene", stringr::str_replace, "trnL1|trnL2", "trnL")) # Rename trnL1 and trnL2 to trnL
  # Drop low-confidence "NNN"-anticodon tRNAs from the final output unless the user
  # chose to retain them.
  if (!isTRUE(retain_low_conf_trna)) {
    annotations <- annotations |>
      dplyr::filter(type != "tRNA" | is.na(anticodon) | anticodon != "NNN")
  }
  annotations <- annotations |>
    dplyr::arrange(contig, pos1)


  # Rotate assembly and annotation if circular
  if (all(stringr::str_detect(names(assembly), "circular"))) {
    rotate_results <- rotate_asmb(
      assembly = assembly,
      annotations = annotations,
      start_gene = start_gene
    )

    assembly <- rotate_results[[1]]
    annotations <- rotate_results[[2]]

    # reorder annotations
    annotations <- dplyr::arrange(annotations, contig, pos1)
  }

  # Update coverage if rotated ----
  rotate <- assembly@metadata[["rotate_to"]]
  if (!is.null(rotate) && rotate > 0) {
    coverage <- dplyr::bind_rows(
      coverage[rotate:nrow(coverage), ],
      coverage[1:(rotate - 1), ]
    ) |>
      dplyr::mutate(
        Position = dplyr::row_number()
      )
  }
  if (!is.null(rotate) && rotate < 0) {
    coverage <- dplyr::bind_rows(
      coverage[abs(rotate):1, ],
      coverage[nrow(coverage):(abs(rotate) + 1), ]
    ) |>
      dplyr::mutate(
        Position = dplyr::row_number(),
        Call = as.character(assembly) |> stringr::str_split("") |> unlist()
      )
  }

  ## Fix D-loop annotations ----
  # Filter spurious OH annotations
  oh_idx <- which(annotations$gene == "OH") |> rev()
  to_remove <- NULL
  for (idx in oh_idx) {
    # Check if overlapping other gene
    containing <- annotations |>
      dplyr::filter(!idx) |>
      dplyr::filter(pos1 >= annotations$pos1[idx] | pos2 <= annotations$pos2[idx])
    if (nrow(containing) > 0L) {
      to_remove <- c(to_remove, idx)
    }
  }
  if (length(to_remove) > 0) {
    annotations <- annotations[-to_remove, ]
  }

  # Extend OH annotations to (putative) full length ctrl region
  oh_idx <- which(annotations$gene == "OH")
  for (idx in oh_idx) {
    if (idx == min(which(annotations$contig == annotations$contig[idx]))) {
      annotations$pos1[idx] <- 1
    } else {
      annotations$pos1[idx] <- annotations$pos2[idx - 1] + 1
    }

    if (idx == max(which(annotations$contig == annotations$contig[idx]))) {
      annotations$pos2[idx] <- assembly[stringr::str_detect(names(assembly), paste(annotations$contig[idx], "\\w.*"))]@ranges@width
    } else {
      annotations$pos2[idx] <- annotations$pos1[idx + 1] - 1
    }
    annotations$length[idx] <- abs(annotations$pos2[idx] - annotations$pos1[idx]) + 1
    annotations$gene[idx] <- "ctrl"
  }

  # Write outputs
  file.path(
    out_dir,
    stringr::str_replace(basename(assembly_fn), "\\w+$", "csv") |>
      stringr::str_replace("assembly", "annotations")
  ) |>
    readr::write_csv(annotations, file = _, na = "")
  Biostrings::writeXStringSet(assembly, file.path(out_dir, basename(assembly_fn)))
  if (!is.null(coverage_fn)) {
    file.path(
      out_dir,
      stringr::str_replace(basename(assembly_fn), "\\w+$", "csv") |>
        stringr::str_replace("assembly", "coverageStats")
    ) |>
      readr::write_csv(coverage, file = _, quote = "none", na = "")
  }

  return(invisible(annotations))
}
