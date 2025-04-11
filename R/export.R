#' Generate export NCBI files
#'
#' @param group (optional) export group names
#' @param IDs One or more sample IDs to export. If not provided all samples in
#'   the export group will be exported
#' @param fasta_header Template for mitogenome fasta headers. Uses glue syntax (i.e.
#'   `{...}`) to insert values from the samples table
#' @param fasta_header_gene Template for gene fasta headers. Uses glue syntax (i.e.
#'   `{...}`) to insert values from the samples table
#' @param out_dir directory to save the exported files
#' @param start_codons NCBI recognized start codons
#' @param stop_codons NCBI recognized stop codons
#' @param generateAAalignments Generate group-level amino acid alignments
#'   (default: TRUE)
#' @param gene_export Export FASTAs and feature tables for individual genes?
#'   (default: FALSE)
#'
#' @export
#'
export_files <- function(
    group = NULL,
    IDs = NULL,
    fasta_header = paste(
      "{ID} [organism={Taxon}] [topology={topology}] [mgcode={genetic_code}]",
      "[location=mitochondrion] {Taxon} mitochondrion, complete genome"
    ),
    fasta_header_gene = paste(
      "{ID} [organism={Taxon}] [mgcode={genetic_code}]",
      "[location=mitochondrion] {Taxon}"
    ),
    out_dir = NULL,
    start_codons = c("ATG", "GTG", "ATA", "ATT", "ATC"), # TODO: dynamic assignment based on curation rules
    stop_codons = c("TAA", "TAG", "AGA", "AGG", "TA", "T"), # TODO: dynamic assignment based on curation rules
    generateAAalignments = T,
    gene_export = F) {
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
    group_gff_pth <- file.path(group_pth, "GFFs")
    dir.create(group_pth, recursive = T, showWarnings = F)
    dir.create(group_gff_pth, recursive = T, showWarnings = F)
    unlink(group_fasta <- file.path(group_pth, paste0(group, ".fasta")))
    unlink(group_tbl <- file.path(group_pth, paste0(group, ".tbl")))
    if(gene_export){
      group_genes_pth <- file.path(group_pth, "genes")
      dir.create(group_genes_pth, recursive = T, showWarnings = F)
    }
  }

  if (length(IDs) == 0) {
    stop("No samples selected")
  }

  if(gene_export){
    # cleanup prior run
    group_allgene_tbl_fn <- file.path(group_pth, "genes", paste0(group, "_PCGs.tbl"))
    if (file.exists(group_allgene_tbl_fn)) {
      file.remove(group_allgene_tbl_fn)
    }
    group_allgene_fasta <- file.path(group_pth, "genes", paste0(group, "_PCGs.fasta"))
    if (file.exists(group_allgene_fasta)) {
      file.remove(group_allgene_fasta)
    }
  }

  purrr::walk(IDs, ~ {
    export_path <- file.path(
      out_dir,
      .x,
      "export"
    )
    dir.create(export_path, showWarnings = F)

    # debugging help
    message(paste0(.x, ":"))

    annotations <- dplyr::tbl(con, "annotations") |>
      dplyr::filter(ID == !!.x) |>
      dplyr::arrange(path, pos1) |>
      dplyr::filter(pos1 > 0) |>
      dplyr::collect()

    # check for duplicate gene names in annotations and rename
    annotations$gene_uniq <- make.unique(annotations$gene)

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

    # sequence name, to be used as first column in GFF
    seq_name <- sapply(strsplit(names(seq)," "), `[`, 1)

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

    # MAKE GFF
    gff_fn <- file.path(export_path, paste0(.x, ".gff"))
    if (file.exists(gff_fn)) {
      file.remove(gff_fn)
    }
    # add GFF header
    "##gff-version 3" |>
      cat(file = gff_fn, sep = "\n", append = TRUE)
    # add GFF region
    # circ = tolower((dat$topology == "circular"))
    #f9 = paste0("ID=",seq_name,":1..",seq@ranges@width,";Is_circular=",circ,";Name=MT;mol_type=genomic DNA") # Is_circular currently bugged in Geneious
    asmb_len = seq@ranges@width
    f9 = paste0("ID=",seq_name,":1..",asmb_len,";Name=MT;mol_type=genomic DNA")
    paste(c(seq_name, "MitoPilot", "region", 1, asmb_len, ".", "+", ".", f9), collapse = "\t") |>
      cat(file = gff_fn, sep = "\n", append = TRUE)

    purrr::pwalk(annotations, function(...) {
      cur <- list(...)
      note <- NULL
      pos <- c(cur$pos1, cur$pos2) |> as.character()
      if (cur$direction == "-") {
        pos <- rev(pos)
      }

      if(cur$pos1 >= cur$pos2){
        message(paste0("Warning: pos1 >= pos2 for ", dat$ID,": ", cur$gene, ", may be an annotation error"))
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

        # write to .tbl
        paste(c(pos, "gene"), collapse = "\t") |>
          cat(file = tbl_fn, sep = "\n", append = TRUE)
        paste0("\t\t\tgene\t", cur$gene) |>
          cat(file = tbl_fn, sep = "\n", append = TRUE)
        paste(c(pos, "CDS"), collapse = "\t") |>
          cat(file = tbl_fn, sep = "\n", append = TRUE)
        paste("\t\t\tproduct\t", cur$product) |>
          cat(file = tbl_fn, sep = "\n", append = TRUE)
        paste("\t\t\ttransl_table\t", dat$genetic_code) |>
          cat(file = tbl_fn, sep = "\n", append = TRUE)
        if (!cur$start_codon %in% start_codons) {
          paste("\t\t\tcodon_start\t", 1) |>
            cat(file = tbl_fn, sep = "\n", append = TRUE)
        }
        if (length(note) > 0) {
          paste0("\t\t\tnote\t", note) |>
            cat(file = tbl_fn, sep = "\n", append = TRUE)
        }

        # write to GFF
        # gene feature
        f9 = paste0("ID=gene-",cur$gene,";Name=",cur$gene,";gbkey=Gene;gene=",cur$gene,";gene_biotype=protein_coding")
        # logic to deal with annotations that wrap around the end of a circular assembly
        if(dat$topology == "circular" && cur$pos1 > cur$pos2 && cur$length != (cur$pos1 - cur$pos2 + 1)){ # annotation wraps
          pos2_fix <- asmb_len + cur$pos2
          paste(c(seq_name, "MitoPilot", "gene", cur$pos1, pos2_fix, ".", cur$direction, ".", f9), collapse = "\t") |>
            cat(file = gff_fn, sep = "\n", append = TRUE)
        } else {
          paste(c(seq_name, "MitoPilot", "gene", cur$pos1, cur$pos2, ".", cur$direction, ".", f9), collapse = "\t") |>
            cat(file = gff_fn, sep = "\n", append = TRUE)
        }

        # CDS feature
        f9 = paste0("ID=cds-",cur$gene,";Parent=gene-",cur$gene,";Name=",cur$gene,";gbkey=CDS;gene=",cur$gene,";product=",cur$product,";transl_table=",dat$genetic_code)
        if (length(note) > 0){
          f9 = paste0(f9, ";Note=", note)
        }
        # logic to deal with annotations that wrap around the end of a circular assembly
        if(dat$topology == "circular" && cur$pos1 > cur$pos2 && cur$length != (cur$pos1 - cur$pos2 + 1)){ # annotation wraps
          pos2_fix <- asmb_len + cur$pos2
          paste(c(seq_name, "MitoPilot", "CDS", cur$pos1, pos2_fix, ".", cur$direction, "0", f9), collapse = "\t") |>
            cat(file = gff_fn, sep = "\n", append = TRUE)
        } else {
          paste(c(seq_name, "MitoPilot", "CDS", cur$pos1, cur$pos2, ".", cur$direction, "0", f9), collapse = "\t") |>
            cat(file = gff_fn, sep = "\n", append = TRUE)
        }

        if(gene_export){
          # EXTRACT GENE FROM ASSEMBLY
          # make directory for gene if it doesn't exist
          group_geneName_pth <- file.path(group_pth, "genes", cur$gene)
          dir.create(group_geneName_pth, recursive = T, showWarnings = F)

          # get gene region from assembly
          gene = Biostrings::subseq(seq, start = cur$pos1, end = cur$pos2)

          # update FASTA header with gene name
          head_split <- strsplit(fasta_header_gene, "\\s+")
          head_split[[1]][1] <- paste0(head_split[[1]][1], "_", cur$gene_uniq)
          head_split[[1]][length(head_split[[1]])] <- paste0(head_split[[1]][length(head_split[[1]])], ", ", cur$product)
          head <- paste(c(head_split[[1]]), sep=" ", collapse=" ")
          names(gene) <- stringr::str_glue_data(dat, head)

          # reverse complement if needed
          if (cur$direction == "-") {
           gene = Biostrings::reverseComplement(gene)
          }

          # write FASTA
          gene_fn <- file.path(export_path, paste0(.x, "_", cur$gene_uniq, ".fasta"))
          Biostrings::writeXStringSet(gene, filepath = gene_fn)

          # fix the start and stop position
          pos1_new = 1
          pos2_new = abs(cur$pos2 - cur$pos1)

          # write gene feature table
          gene_tbl_fn <- file.path(export_path, paste0(.x, "_", cur$gene_uniq, ".tbl"))
          if (file.exists(gene_tbl_fn)) {
            file.remove(gene_tbl_fn)
          }
          cat(paste0(">Feature ", .x, "_", cur$gene_uniq), file = gene_tbl_fn, sep = "\n")
          paste(c(pos1_new, pos2_new, "gene"), collapse = "\t") |>
            cat(file = gene_tbl_fn, sep = "\n", append = TRUE)
          paste0("\t\t\tgene\t", cur$gene_uniq) |>
            cat(file = gene_tbl_fn, sep = "\n", append = TRUE)
          paste(c(pos1_new, pos2_new, "CDS"), collapse = "\t") |>
            cat(file = gene_tbl_fn, sep = "\n", append = TRUE)
          paste("\t\t\tproduct\t", cur$product) |>
            cat(file = gene_tbl_fn, sep = "\n", append = TRUE)
          paste("\t\t\ttransl_table\t", dat$genetic_code) |>
            cat(file = gene_tbl_fn, sep = "\n", append = TRUE)
          if (!cur$start_codon %in% start_codons) {
            paste("\t\t\tcodon_start\t", 1) |>
              cat(file = gene_tbl_fn, sep = "\n", append = TRUE)
          }
          if (length(note) > 0) {
            paste0("\t\t\tnote\t", note) |>
              cat(file = gene_tbl_fn, sep = "\n", append = TRUE)
          }

          # concatenate sequences and tables by gene
          if (length(group) == 1) {
            group_gene_tbl <- file.path(group_geneName_pth, paste0(group, "_", cur$gene, ".tbl"))
            stringr::str_glue(
              "cat {gene_tbl_fn} >> {group_gene_tbl}"
            ) |> system()
            group_gene_fasta <- file.path(group_geneName_pth, paste0(group, "_", cur$gene, ".fasta"))
            stringr::str_glue(
              "cat {gene_fn} >> {group_gene_fasta}"
            ) |> system()
          }

          # concatenate all sequences and tables
          if (length(group) == 1) {
            stringr::str_glue(
              "cat {gene_tbl_fn} >> {group_allgene_tbl_fn}"
            ) |> system()
            stringr::str_glue(
              "cat {gene_fn} >> {group_allgene_fasta}"
            ) |> system()
          }

        }



        return()
      }

      if (cur$type == "tRNA") {
        # write to .tbl
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

        # write to GFF
        # tRNA feature
        f9 = paste0("ID=rna-",seq_name,":",cur$pos1,"..",cur$pos2,";Name=",cur$gene,";gbkey=tRNA;product=",cur$product)
        # logic to deal with annotations that wrap around the end of a circular assembly
        if(dat$topology == "circular" && cur$pos1 > cur$pos2 && cur$length != (cur$pos1 - cur$pos2 + 1)){ # annotation wraps
          pos2_fix <- asmb_len + cur$pos2
          paste(c(seq_name, "MitoPilot", "tRNA", cur$pos1, pos2_fix, ".", cur$direction, ".", f9), collapse = "\t") |>
            cat(file = gff_fn, sep = "\n", append = TRUE)
        } else {
          paste(c(seq_name, "MitoPilot", "tRNA", cur$pos1, cur$pos2, ".", cur$direction, ".", f9), collapse = "\t") |>
            cat(file = gff_fn, sep = "\n", append = TRUE)
        }

        # exon feature
        f9 = paste0("ID=exon-",seq_name,":",cur$pos1,"..",cur$pos2,";Parent=rna-",seq_name,":",cur$pos1,"..",cur$pos2,";gbkey=tRNA;product=",cur$product)
        # logic to deal with annotations that wrap around the end of a circular assembly
        if(dat$topology == "circular" && cur$pos1 > cur$pos2 && cur$length != (cur$pos1 - cur$pos2 + 1)){ # annotation wraps
          pos2_fix <- asmb_len + cur$pos2
          paste(c(seq_name, "MitoPilot", "exon", cur$pos1, pos2_fix, ".", cur$direction, ".", f9), collapse = "\t") |>
            cat(file = gff_fn, sep = "\n", append = TRUE)
        } else {
          paste(c(seq_name, "MitoPilot", "exon", cur$pos1, cur$pos2, ".", cur$direction, ".", f9), collapse = "\t") |>
            cat(file = gff_fn, sep = "\n", append = TRUE)
        }

        return()
      }

      if (cur$type == "rRNA") {
        # write to .tbl
        paste(c(pos, "gene"), collapse = "\t") |>
          cat(file = tbl_fn, sep = "\n", append = TRUE)
        paste0("\t\t\tgene\t", ifelse(cur$gene == "rrnS", "s-rRNA", "l-rRNA")) |>
          cat(file = tbl_fn, sep = "\n", append = TRUE)
        paste(c(pos, "rRNA"), collapse = "\t") |>
          cat(file = tbl_fn, sep = "\n", append = TRUE)
        paste("\t\t\tproduct\t", cur$product) |>
          cat(file = tbl_fn, sep = "\n", append = TRUE)

        # write to GFF
        # rRNA feature
        f9 = paste0("ID=rna-",seq_name,":",cur$pos1,"..",cur$pos2,";Name=",cur$gene,";gbkey=rRNA;product=",cur$product)
        # logic to deal with annotations that wrap around the end of a circular assembly
        if(dat$topology == "circular" && cur$pos1 > cur$pos2 && cur$length != (cur$pos1 - cur$pos2 + 1)){ # annotation wraps
          pos2_fix <- asmb_len + cur$pos2
          paste(c(seq_name, "MitoPilot", "rRNA", cur$pos1, pos2_fix, ".", cur$direction, ".", f9), collapse = "\t") |>
            cat(file = gff_fn, sep = "\n", append = TRUE)
        } else {
          paste(c(seq_name, "MitoPilot", "rRNA", cur$pos1, cur$pos2, ".", cur$direction, ".", f9), collapse = "\t") |>
            cat(file = gff_fn, sep = "\n", append = TRUE)
        }
        # exon feature
        f9 = paste0("ID=exon-",seq_name,":",cur$pos1,"..",cur$pos2,";Parent=rna-",seq_name,":",cur$pos1,"..",cur$pos2,";gbkey=rRNA;product=",cur$product)
        # logic to deal with annotations that wrap around the end of a circular assembly
        if(dat$topology == "circular" && cur$pos1 > cur$pos2 && cur$length != (cur$pos1 - cur$pos2 + 1)){ # annotation wraps
          pos2_fix <- asmb_len + cur$pos2
          paste(c(seq_name, "MitoPilot", "exon", cur$pos1, pos2_fix, ".", cur$direction, ".", f9), collapse = "\t") |>
            cat(file = gff_fn, sep = "\n", append = TRUE)
        } else {
          paste(c(seq_name, "MitoPilot", "exon", cur$pos1, cur$pos2, ".", cur$direction, ".", f9), collapse = "\t") |>
            cat(file = gff_fn, sep = "\n", append = TRUE)
        }

        return()
      }

      if (cur$type == "ctrl") {
        # write to .tbl
        paste(c(pos, "D-loop"), collapse = "\t") |>
          cat(file = tbl_fn, sep = "\n", append = TRUE)
        paste0("\t\t\tnote\tcontrol region") |>
          cat(file = tbl_fn, sep = "\n", append = TRUE)

        # write to GFF
        f9 = paste0("ID=exon-",seq_name,":",cur$pos1,"..",cur$pos2,";gbkey=D-loop;Note=control region")
        # logic to deal with annotations that wrap around the end of a circular assembly
        # with special check for D_loop problem of pos1 and pos2 mixup
        if(dat$topology == "circular" && cur$pos1 > cur$pos2 && cur$length != (cur$pos1 - cur$pos2 + 1)){ # annotation wraps
          pos2_fix <- asmb_len + cur$pos2
          paste(c(seq_name, "MitoPilot", "D_loop", cur$pos1, pos2_fix, ".", cur$direction, ".", f9), collapse = "\t") |>
            cat(file = gff_fn, sep = "\n", append = TRUE)
        } else if(dat$topology == "circular" && cur$pos1 > cur$pos2){ # annotation does not wrap but pos1 and pos2 need to be flipped
          paste(c(seq_name, "MitoPilot", "D_loop", cur$pos2, cur$pos1, ".", cur$direction, ".", f9), collapse = "\t") |>
            cat(file = gff_fn, sep = "\n", append = TRUE)
        } else {
          paste(c(seq_name, "MitoPilot", "D_loop", cur$pos1, cur$pos2, ".", cur$direction, ".", f9), collapse = "\t") |>
            cat(file = gff_fn, sep = "\n", append = TRUE)
        }

        return()
      }
    })


    if (length(group) == 1) {
      stringr::str_glue(
        "cat {tbl_fn} >> {group_tbl}"
      ) |> system()
      stringr::str_glue(
        "cp {gff_fn} {group_gff_pth}"
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
