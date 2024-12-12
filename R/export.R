#' Generate export NCBI files
#'
#' @param group (optioanl) exportgroup names
#' @param IDs One or more sample IDs to export. If not provided all samples in
#'   the export group will be exported
#' @param fasta_header Template fot fasta headers. Uses glue syntax (i.e.
#'   `{...}`) to insert values from the samples table
#' @param out_dir directory to save the exported files
#' @param start_codons NCBI recognized start codons
#' @param stop_codons NCBI recognized stop codons
#' @param generateAAalignments Generate group-level amino acid alignments
#'   (default: TRUE)
#'
#' @export
#'
export_files <- function(
    group = NULL,
    IDs = NULL,
    fasta_header = paste(
      "{ID} [organism={Taxon}] [topology={topology}] [mgcode=2]",
      "[location=mitochondrion] {Taxon} mitochondrion, complete genome"
    ),
    out_dir = NULL,
    start_codons = c("ATG", "GTG", "ATA", "ATT", "ATC"),
    stop_codons = c("TAA", "TAG", "AGA", "AGG", "TA", "T"),
    generateAAalignments = T) {
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = file.path(dirname(out_dir), ".sqlite"))
  on.exit(DBI::dbDisconnect(con))

  if (length(group) == 1) {
    IDs <- dplyr::tbl(con, "samples") |>
      dplyr::filter(export_group == !!group) |>
      dplyr::pull("ID")
    group_pth <- file.path(
      out_dir,
      "export",
      group
    )
    dir.create(group_pth, recursive = T, showWarnings = F)
    unlink(group_fasta <- file.path(group_pth, paste0(group, ".fasta")))
    unlink(group_tbl <- file.path(group_pth, paste0(group, ".tbl")))
  }

  if (length(IDs) == 0) {
    stop("No samples selected")
  }

  purrr::walk(IDs, ~ {
    export_path <- file.path(
      out_dir,
      .x,
      "export"
    )
    dir.create(export_path, showWarnings = F)

    annotations <- dplyr::tbl(con, "annotations") |>
      dplyr::filter(ID == !!.x) |>
      dplyr::arrange(path, pos1) |>
      dplyr::filter(pos1 > 0) |>
      dplyr::collect()

    dat <- dplyr::tbl(con, "samples") |>
      dplyr::filter(ID == !!.x) |>
      dplyr::left_join(
        dplyr::tbl(con, "annotate") |>
          dplyr::select(ID, topology, path) |>
          dplyr::distinct(),
        by = "ID"
      ) |>
      dplyr::collect()

    seq <- get_assembly(
      ID = .x,
      path = dat$path,
      con = con
    )
    if (length(seq) > 1) {
      stop("Multiple sequences found. Export not supported for fragmented assemblies.")
    }
    names(seq) <- stringr::str_glue_data(dat, fasta_header)

    # Trim un-annotated ends of linear assemblies
    if (dat$topology == "linear") {
      start <- min(annotations$pos1)
      if (start > 1) {
        seq <- Biostrings::subseq(seq, start, seq@ranges@width)
        annotations <- annotations |>
          dplyr::mutate(
            pos1 = pos1 - start + 1,
            pos2 = pos2 - start + 1
          )
      }
      stop <- max(annotations$pos2)
      if (stop > seq@ranges@width) {
        seq <- Biostrings::subseq(seq, 1, stop)
      }
    }

    # Write FASTA
    fasta_fn <- file.path(export_path, paste0(.x, ".fasta"))
    Biostrings::writeXStringSet(seq, filepath = fasta_fn)

    # MAKE 4 column tab file
    tbl_fn <- file.path(export_path, paste0(.x, ".tbl"))
    if (file.exists(tbl_fn)) {
      file.remove(tbl_fn)
    }
    if ("GenBankAccession" %in% names(dat) && length(dat$GenBankAccession) > 0 && nchar(dat$GenBankAccession) > 4) {
      cat(paste(">Feature", paste0("gb|", dat$GenBankAccession, "|")), file = tbl_fn, sep = "\n")
    } else {
      cat(paste(">Feature", .x), file = tbl_fn, sep = "\n")
    }

    purrr::pwalk(annotations, function(...) {
      cur <- list(...)
      note <- NULL
      pos <- c(cur$pos1, cur$pos2) |> as.character()
      if (cur$direction == "-") {
        pos <- rev(pos)
      }

      if (cur$type == "PCG") {
        if (stringr::str_detect(cur$translation, "\\*")) {
          message(crayon::red(paste("##### Internal stop codon", cur$gene, crayon::bgBlue(cur$stop_codon), "#####")))
        }
        if (cur$stop_codon %nin% stop_codons) {
          message(crayon::red(paste("Non-standard stop codon:", cur$gene, crayon::bgBlue(cur$stop_codon))))
        }
        if (cur$start_codon %nin% start_codons) {
          message(crayon::red(paste("Non-standard start codon:", cur$gene, crayon::bgBlue(cur$start_codon))))
          if (cur$direction == "+") {
            pos[1] <- paste0("<", pos[1])
          } else {
            pos[2] <- paste0(pos[2], ">")
          }
          note <- "start codon not determined"
        }
        if (nchar(cur$stop_codon) < 3) {
          note <- paste(c(note, "TAA stop codon is completed by the addition of 3' A residues to the mRNA"), collapse = "; ")
        }

        paste(c(pos, "gene"), collapse = "\t") |>
          cat(file = tbl_fn, sep = "\n", append = TRUE)
        paste0("\t\t\tgene\t", cur$gene) |>
          cat(file = tbl_fn, sep = "\n", append = TRUE)
        paste(c(pos, "CDS"), collapse = "\t") |>
          cat(file = tbl_fn, sep = "\n", append = TRUE)
        paste("\t\t\tproduct\t", cur$product) |>
          cat(file = tbl_fn, sep = "\n", append = TRUE)
        paste("\t\t\ttransl_table\t", 2) |>
          cat(file = tbl_fn, sep = "\n", append = TRUE)
        if (!cur$start_codon %in% start_codons) {
          paste("\t\t\tcodon_start\t", 1) |>
            cat(file = tbl_fn, sep = "\n", append = TRUE)
        }
        if (length(note) > 0) {
          paste0("\t\t\tnote\t", note) |>
            cat(file = tbl_fn, sep = "\n", append = TRUE)
        }
        return()
      }

      if (cur$type == "tRNA") {
        paste(c(pos, "gene"), collapse = "\t") |>
          cat(file = tbl_fn, sep = "\n", append = TRUE)
        paste0("\t\t\tgene\t", cur$gene) |>
          cat(file = tbl_fn, sep = "\n", append = TRUE)
        paste(c(pos, "tRNA"), collapse = "\t") |>
          cat(file = tbl_fn, sep = "\n", append = TRUE)
        paste("\t\t\tproduct\t", cur$product) |>
          cat(file = tbl_fn, sep = "\n", append = TRUE)
        if (cur$anticodon != "NNN") {
          paste("\t\t\tnote\t", paste0("anticodon:", tolower(cur$anticodon))) |>
            cat(file = tbl_fn, sep = "\n", append = TRUE)
        }
        return()
      }

      if (cur$type == "rRNA") {
        paste(c(pos, "gene"), collapse = "\t") |>
          cat(file = tbl_fn, sep = "\n", append = TRUE)
        paste0("\t\t\tgene\t", ifelse(cur$gene == "rrnS", "s-rRNA", "l-rRNA")) |>
          cat(file = tbl_fn, sep = "\n", append = TRUE)
        paste(c(pos, "rRNA"), collapse = "\t") |>
          cat(file = tbl_fn, sep = "\n", append = TRUE)
        paste("\t\t\tproduct\t", cur$product) |>
          cat(file = tbl_fn, sep = "\n", append = TRUE)
        return()
      }

      if (cur$type == "ctrl") {
        paste(c(pos, "D-loop"), collapse = "\t") |>
          cat(file = tbl_fn, sep = "\n", append = TRUE)
        paste0("\t\t\tnote\tcontrol region") |>
          cat(file = tbl_fn, sep = "\n", append = TRUE)
        return()
      }
    })


    if (length(group) == 1) {
      stringr::str_glue(
        "cat {tbl_fn} >> {group_tbl}"
      ) |> system()
      stringr::str_glue(
        "cat {fasta_fn} >> {group_fasta}"
      ) |> system()
    }
  })

  if (length(group) == 1 && length(IDs) > 1 && generateAAalignments) {
    make_PCG_alignments(
      export_group = group,
      db = file.path(dirname(out_dir), ".sqlite"),
      out_path = group_pth
    )
  }
}

#' Generate HTML report woth PCG alignments
#'
#' @param db path for sqlite database
#' @param out_path path for output files
#' @param export_group Name of the submission group
#'
#' @export
#'
make_PCG_alignments <- function(
    export_group = NULL,
    db = NULL,
    out_path = NULL) {
  rmarkdown::render(
    input = system.file("AA_alignment_report.Rmd", package = "MitoPilot"),
    output_file = stringr::str_glue("AA_alignments_{export_group}.html"),
    output_dir = out_path,
    intermediates_dir = getwd(),
    knit_root_dir = getwd(),
    params = list(
      group = export_group,
      db_path = db
    )
  )
}
