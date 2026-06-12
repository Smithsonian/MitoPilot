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
#' @param generateAAalignments Generate group-level amino acid alignments
#'   (default: TRUE)
#' @param gene_export Export FASTAs and feature tables for individual genes?
#'   (default: FALSE)
#' @param review Run the PCG annotation outlier review after writing files and
#'   return the flagged results? (default: TRUE)
#' @param start_aa Start-offset threshold (amino acids) passed to
#'   [flag_PCG_outliers()]. Default 10.
#' @param stop_aa Stop-offset threshold (amino acids) passed to
#'   [flag_PCG_outliers()]. Default 10.
#' @param ident_pct Identity threshold (percent) passed to
#'   [flag_PCG_outliers()]. Default 60.
#'
#' @return Invisibly, the list returned by [flag_PCG_outliers()] when `review`
#'   is TRUE (and a group of >1 sample is exported), otherwise `NULL`.
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
    generateAAalignments = T,
    gene_export = F,
    review = TRUE,
    start_aa = 10,
    stop_aa = 10,
    ident_pct = 60) {


  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = file.path(dirname(out_dir), ".sqlite"))
  on.exit(DBI::dbDisconnect(con))

  if (length(group) == 1) {
    IDs <- dplyr::tbl(con, "samples") |>
      dplyr::filter(export_group == !!group) |>
      dplyr::pull("ID")
    group_pth <- file.path(out_dir, "export", group)
    unlink(group_pth, recursive = TRUE)
    group_gff_pth <- file.path(group_pth, "GFFs")
    dir.create(group_pth, recursive = TRUE, showWarnings = FALSE)
    dir.create(group_gff_pth, recursive = TRUE, showWarnings = FALSE)
    group_fasta <- file.path(group_pth, paste0(group, ".fasta"))
    group_tbl   <- file.path(group_pth, paste0(group, ".tbl"))
    if (gene_export) {
      group_genes_pth <- file.path(group_pth, "genes")
      dir.create(group_genes_pth, recursive = TRUE, showWarnings = FALSE)
    }
  }

  if (length(IDs) == 0) {
    stop("No samples selected")
  }

  if (gene_export) {
    group_allgene_tbl_fn <- file.path(group_pth, "genes", paste0(group, "_PCGs.tbl"))
    group_allgene_fasta  <- file.path(group_pth, "genes", paste0(group, "_PCGs.fasta"))
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

    curation_opts <-  dplyr::tbl(con, "annotate") |>
      dplyr::filter(ID == !!.x) |>
      dplyr::pull("curate_opts")

    curate_rules <- dplyr::tbl(con, "curate_opts") |>
      dplyr::filter(curate_opts == !!curation_opts) |>
      dplyr::pull("params") |>
      jsonlite::fromJSON()

    # check for duplicate gene names in annotations and rename
    annotations$gene_uniq <- make.unique(annotations$gene)

    dat <- dplyr::tbl(con, "samples") |>
      dplyr::select(-dplyr::any_of("topology")) |>
      dplyr::filter(ID == !!.x) |>
      dplyr::left_join(
        dplyr::tbl(con, "annotate") |>
          dplyr::select(ID, topology, path) |>
          dplyr::distinct(),
        by = "ID"
      ) |>
      dplyr::left_join(
        dplyr::tbl(con, "assemble") |>
          dplyr::select(ID, blast_accession, dplyr::any_of("poor_blast_ref")),
        by = "ID"
      ) |>
      dplyr::collect()

    kept <- dplyr::tbl(con, "assemblies") |>
      dplyr::filter(ID == !!.x & path == !!dat$path & ignore == 0) |>
      dplyr::select(scaffold, topology) |>
      dplyr::collect()
    if (nrow(kept) == 0) {
      warning(.x, ": No scaffolds remain after ignore filter. Skipping.")
      return()
    }
    if (nrow(kept) > 1) {
      warning(.x, ": Multiple non-ignored scaffolds found. Export not supported for fragmented assemblies. Mark all but one scaffold as ignore to export. Skipping.")
      return()
    }
    seq <- MitoPilot::get_assembly(
      ID = .x,
      path = dat$path,
      scaffold = kept$scaffold,
      con = con
    )
    # Override fragmented topology with the kept scaffold's per-scaffold topology
    if (isTRUE(dat$topology == "fragmented") && !is.na(kept$topology[1])) {
      dat$topology <- kept$topology[1]
    }
    blast_acc <- dat$blast_accession[1]
    blast_note <- if (!is.null(blast_acc) && !is.na(blast_acc) && nzchar(blast_acc) &&
                      blast_acc != "NO HIT" &&
                      !isTRUE(dat$poor_blast_ref[1] %in% c("poor", "failed"))) {
      paste0(" [note=annotation compared to GenBank accession ", blast_acc, "]")
    } else {
      ""
    }
    names(seq) <- paste0(stringr::str_glue_data(dat, fasta_header), blast_note)

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
      transl_except <- NULL
      pos <- c(cur$pos1, cur$pos2) |> as.character()
      if (cur$direction == "-") {
        pos <- rev(pos)
      }

      if(cur$pos1 >= cur$pos2){
        message(paste0("Warning: pos1 >= pos2 for ", dat$ID,": ", cur$gene, ", may be an annotation error"))
      }

      if (cur$type == "PCG") {
        # default product for custom-named ORFs / CDS without a standard product
        if (length(cur$product) == 0 || is.na(cur$product) || !nzchar(trimws(cur$product))) {
          cur$product <- "hypothetical protein"
        }
        # get start and stop codons from the ruleset
        cur_rules <- curate_rules$rules[[cur$gene]]
        if("stop_codons" %in% names(cur_rules)){
          stop_codons <- cur_rules$stop_codons
        } else {
          stop_codons <- curate_rules$default_rules$PCG$stop_codons
        }
        if("start_codons" %in% names(cur_rules)){
          start_codons <- cur_rules$start_codons
        } else {
          start_codons <- curate_rules$default_rules$PCG$start_codons
        }

        if ("intron" %in% names(cur_rules)){
          intron <- cur_rules$intron
        } else {
          if("intron" %in% curate_rules$default_rules$PCG){
            intron <- curate_rules$default_rules$PCG$intron
          } else {
            intron <- FALSE
          }
        }

        if (intron & length(which(annotations$gene == cur$gene)) > 1) {  # logic to merge exons if intron is present
          exons <- annotations[which(annotations$gene == cur$gene), ]
          # skip if not the first exon
          if (cur$pos1 != exons[1,]$pos1) return()
          exon_seqs <- rep(NA, nrow(exons))
          if (all(exons$direction == "+")) {
            # extract exons
            for (i in 1:nrow(exons)) {
              exon_seqs[i] <- Biostrings::subseq(seq, start = exons[i, ]$pos1, end = exons[i, ]$pos2)
            }
            # merge exons
            merged_sequence <- Biostrings::DNAStringSet(paste(exon_seqs, collapse = ""))
            # translate
            cur$translation <- Biostrings::translate(merged_sequence,
                                                     genetic.code = Biostrings::getGeneticCode(as.character(dat$genetic_code))) |>
              as.character()
            cur$translation <- sub("\\*$", "", cur$translation) # remove terminal stop codon
            # set new start and stop codons
            cur$start_codon <- exons[1,]$start_codon
            cur$stop_codon <- exons[nrow(exons),]$stop_codon
            # set new start and stop pos for gene
            pos <- c(exons[1,]$pos1, exons[nrow(exons),]$pos2) |> as.character()
            cur$pos1 <- exons[1,]$pos1
            cur$pos2 <- exons[nrow(exons),]$pos2
            # set new CDS length
            cur$length <- sum(exons$length)
          } else if (all(exons$direction == "-")) {
            # extract exons
            for (i in 1:nrow(exons)) {
              exon_seqs[i] <- Biostrings::subseq(seq, start = exons[i, ]$pos1, end = exons[i, ]$pos2)
            }
            # merge exons and reverse complement
            merged_sequence <- Biostrings::DNAStringSet(paste(exon_seqs, collapse = "")) |>
              Biostrings::reverseComplement()
            # translate
            cur$translation <- Biostrings::translate(merged_sequence,
                                                     genetic.code = Biostrings::getGeneticCode(as.character(dat$genetic_code))) |>
              as.character()
            cur$translation <- sub("\\*$", "", cur$translation) # remove terminal stop codon
            # set new start and stop codons
            cur$start_codon <- exons[nrow(exons),]$start_codon
            cur$stop_codon <- exons[1]$stop_codon
            # set new start and stop pos for gene
            pos <- c(exons[1,]$pos1, exons[nrow(exons),]$pos2) |> as.character() |> rev()
            cur$pos1 <- exons[nrow(exons),]$pos2
            cur$pos2 <- exons[1,]$pos1
            # set new CDS length
            cur$length <- sum(exons$length)
          } else {
            message(crayon::red(
              paste0("Warning: exons on opposite strands for gene ", cur$gene)
              # NEED LOGIC TO DEAL WITH THIS
            ))
          }

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
            # transl_except marks the partial stop codon position(s) completed by poly-A
            n_stop <- nchar(cur$stop_codon)
            if (cur$direction == "+") {
              te_end <- max(cur$pos1, cur$pos2)
              te_start <- te_end - n_stop + 1
            } else {
              te_end <- min(cur$pos1, cur$pos2)
              te_start <- te_end + n_stop - 1
            }
            if (n_stop == 1) {
              transl_except <- paste0("(pos:", te_end, ",aa:TERM)")
            } else {
              transl_except <- paste0("(pos:", te_start, "..", te_end, ",aa:TERM)")
            }
          }

          # write to .tbl
          paste(c(pos, "gene"), collapse = "\t") |>
            cat(file = tbl_fn, sep = "\n", append = TRUE)
          paste0("\t\t\tgene\t", cur$gene) |>
            cat(file = tbl_fn, sep = "\n", append = TRUE)
          # write CDS lines for each exon
          if (all(exons$direction == "+")) {
            for (i in 1:nrow(exons)){
              if (i == 1){
                paste(c(exons[i,]$pos1, exons[i,]$pos2, "CDS"), collapse = "\t") |>
                  cat(file = tbl_fn, sep = "\n", append = TRUE)
              } else {
                paste(c(exons[i,]$pos1, exons[i,]$pos2), collapse = "\t") |>
                  cat(file = tbl_fn, sep = "\n", append = TRUE)
              }
            }
          } else if (all(exons$direction == "-")) {
            for (i in nrow(exons):1){
              if (i == 1){
                paste(c(exons[i,]$pos2, exons[i,]$pos1, "CDS"), collapse = "\t") |>
                  cat(file = tbl_fn, sep = "\n", append = TRUE)
              } else {
                paste(c(exons[i,]$pos2, exons[i,]$pos1), collapse = "\t") |>
                  cat(file = tbl_fn, sep = "\n", append = TRUE)
              }
            }
          }
          paste("\t\t\tproduct\t", cur$product) |>
            cat(file = tbl_fn, sep = "\n", append = TRUE)
          paste("\t\t\ttransl_table\t", dat$genetic_code) |>
            cat(file = tbl_fn, sep = "\n", append = TRUE)
          if (length(transl_except) > 0) {
            paste0("\t\t\ttransl_except\t", transl_except) |>
              cat(file = tbl_fn, sep = "\n", append = TRUE)
          }
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

          # write CDS feature for each exon
          if (all(exons$direction == "+")) {
            for (i in 1:nrow(exons)){
              f9 = paste0("ID=cds-",cur$gene,";Parent=gene-",cur$gene,";Name=",cur$gene,";gbkey=CDS;gene=",cur$gene,";product=",cur$product,";transl_table=",dat$genetic_code)
              if (length(note) > 0){
                f9 = paste0(f9, ";Note=", note)
              }
              # logic to deal with annotations that wrap around the end of a circular assembly
              if(dat$topology == "circular" && exons[i,]$pos1 > exons[i,]$pos2 && exons[i,]$length != (exons[i,]$pos1 - exons[i,]$pos2 + 1)){ # annotation wraps
                pos2_fix <- asmb_len + exons[i,]$pos2
                paste(c(seq_name, "MitoPilot", "CDS", exons[i,]$pos1, pos2_fix, ".", cur$direction, "0", f9), collapse = "\t") |>
                  cat(file = gff_fn, sep = "\n", append = TRUE)
              } else {
                paste(c(seq_name, "MitoPilot", "CDS", exons[i,]$pos1, exons[i,]$pos2, ".", cur$direction, "0", f9), collapse = "\t") |>
                  cat(file = gff_fn, sep = "\n", append = TRUE)
              }
            }
          } else if (all(exons$direction == "-")) {
            for (i in nrow(exons):1){
              f9 = paste0("ID=cds-",cur$gene,";Parent=gene-",cur$gene,";Name=",cur$gene,";gbkey=CDS;gene=",cur$gene,";product=",cur$product,";transl_table=",dat$genetic_code)
              if (length(note) > 0){
                f9 = paste0(f9, ";Note=", note)
              }
              # logic to deal with annotations that wrap around the end of a circular assembly
              if(dat$topology == "circular" && exons[i,]$pos2 > exons[i,]$pos1 && exons[i,]$length != (exons[i,]$pos2 - exons[i,]$pos1 + 1)){ # annotation wraps
                pos1_fix <- asmb_len + exons[i,]$pos2
                paste(c(seq_name, "MitoPilot", "CDS", exons[i,]$pos2, pos1_fix, ".", cur$direction, "0", f9), collapse = "\t") |>
                  cat(file = gff_fn, sep = "\n", append = TRUE)
              } else {
                paste(c(seq_name, "MitoPilot", "CDS", exons[i,]$pos2, exons[i,]$pos1, ".", cur$direction, "0", f9), collapse = "\t") |>
                  cat(file = gff_fn, sep = "\n", append = TRUE)
              }
            }
          }

          if(gene_export){
            # EXTRACT GENE FROM ASSEMBLY
            # make directory for gene if it doesn't exist
            group_geneName_pth <- file.path(group_pth, "genes", cur$gene)
            dir.create(group_geneName_pth, recursive = T, showWarnings = F)

            # get gene region from assembly
            gene = merged_sequence

            # update FASTA header with gene name
            head_split <- strsplit(fasta_header_gene, "\\s+")
            head_split[[1]][1] <- paste0(head_split[[1]][1], "_", cur$gene)
            head_split[[1]][length(head_split[[1]])] <- paste0(head_split[[1]][length(head_split[[1]])], ", ", cur$product)
            head <- paste(c(head_split[[1]]), sep=" ", collapse=" ")
            names(gene) <- stringr::str_glue_data(dat, head)

            # write FASTA
            gene_fn <- file.path(export_path, paste0(.x, "_", cur$gene, ".fasta"))
            Biostrings::writeXStringSet(gene, filepath = gene_fn)

            # fix the start and stop position
            pos1_new = 1
            pos2_new = length(gene[[1]])

            # write gene feature table
            gene_tbl_fn <- file.path(export_path, paste0(.x, "_", cur$gene, ".tbl"))
            if (file.exists(gene_tbl_fn)) {
              file.remove(gene_tbl_fn)
            }
            cat(paste0(">Feature ", .x, "_", cur$gene), file = gene_tbl_fn, sep = "\n")
            paste(c(pos1_new, pos2_new, "gene"), collapse = "\t") |>
              cat(file = gene_tbl_fn, sep = "\n", append = TRUE)
            paste0("\t\t\tgene\t", cur$gene) |>
              cat(file = gene_tbl_fn, sep = "\n", append = TRUE)
            paste(c(pos1_new, pos2_new, "CDS"), collapse = "\t") |>
              cat(file = gene_tbl_fn, sep = "\n", append = TRUE)
            paste("\t\t\tproduct\t", cur$product) |>
              cat(file = gene_tbl_fn, sep = "\n", append = TRUE)
            paste("\t\t\ttransl_table\t", dat$genetic_code) |>
              cat(file = gene_tbl_fn, sep = "\n", append = TRUE)
            if (nchar(cur$stop_codon) < 3) {
              # extracted gene is oriented 5'->3', partial stop is the last base(s)
              n_stop <- nchar(cur$stop_codon)
              if (n_stop == 1) {
                gene_transl_except <- paste0("(pos:", pos2_new, ",aa:TERM)")
              } else {
                gene_transl_except <- paste0("(pos:", pos2_new - n_stop + 1, "..", pos2_new, ",aa:TERM)")
              }
              paste0("\t\t\ttransl_except\t", gene_transl_except) |>
                cat(file = gene_tbl_fn, sep = "\n", append = TRUE)
            }
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

        } else { # normal processing, no introns
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
            # transl_except marks the partial stop codon position(s) completed by poly-A
            n_stop <- nchar(cur$stop_codon)
            if (cur$direction == "+") {
              te_end <- max(cur$pos1, cur$pos2)
              te_start <- te_end - n_stop + 1
            } else {
              te_end <- min(cur$pos1, cur$pos2)
              te_start <- te_end + n_stop - 1
            }
            if (n_stop == 1) {
              transl_except <- paste0("(pos:", te_end, ",aa:TERM)")
            } else {
              transl_except <- paste0("(pos:", te_start, "..", te_end, ",aa:TERM)")
            }
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
          if (length(transl_except) > 0) {
            paste0("\t\t\ttransl_except\t", transl_except) |>
              cat(file = tbl_fn, sep = "\n", append = TRUE)
          }
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
            pos2_new = length(gene[[1]])

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
            if (nchar(cur$stop_codon) < 3) {
              # extracted gene is oriented 5'->3', partial stop is the last base(s)
              n_stop <- nchar(cur$stop_codon)
              if (n_stop == 1) {
                gene_transl_except <- paste0("(pos:", pos2_new, ",aa:TERM)")
              } else {
                gene_transl_except <- paste0("(pos:", pos2_new - n_stop + 1, "..", pos2_new, ",aa:TERM)")
              }
              paste0("\t\t\ttransl_except\t", gene_transl_except) |>
                cat(file = gene_tbl_fn, sep = "\n", append = TRUE)
            }
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

        # export rRNAs individually
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
          paste(c(pos1_new, pos2_new, "rRNA"), collapse = "\t") |>
            cat(file = gene_tbl_fn, sep = "\n", append = TRUE)
          paste("\t\t\tproduct\t", cur$product) |>
            cat(file = gene_tbl_fn, sep = "\n", append = TRUE)
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

      if (cur$type == "ctrl") {
        # write to .tbl
        paste(c(pos, "D-loop"), collapse = "\t") |>
          cat(file = tbl_fn, sep = "\n", append = TRUE)
        paste0("\t\t\tnote\tcontrol region") |>
          cat(file = tbl_fn, sep = "\n", append = TRUE)

        # write to GFF
        f9 = paste0("ID=ctrl-",seq_name,":",cur$pos1,"..",cur$pos2,";gbkey=D-loop;Note=control region")
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

      if (cur$type == "ORF") {
        # Unassigned ORFs: export as a hypothetical-protein CDS. ORFs have NA
        # start/stop codons, so none of the PCG codon logic (partial markers,
        # transl_except, codon membership checks) applies.
        product <- "hypothetical protein"
        note <- "open reading frame predicted by ORFfinder"

        # write to .tbl
        paste(c(pos, "gene"), collapse = "\t") |>
          cat(file = tbl_fn, sep = "\n", append = TRUE)
        paste0("\t\t\tgene\t", cur$gene) |>
          cat(file = tbl_fn, sep = "\n", append = TRUE)
        paste(c(pos, "CDS"), collapse = "\t") |>
          cat(file = tbl_fn, sep = "\n", append = TRUE)
        paste("\t\t\tproduct\t", product) |>
          cat(file = tbl_fn, sep = "\n", append = TRUE)
        paste("\t\t\ttransl_table\t", dat$genetic_code) |>
          cat(file = tbl_fn, sep = "\n", append = TRUE)
        paste("\t\t\tcodon_start\t", 1) |>
          cat(file = tbl_fn, sep = "\n", append = TRUE)
        paste0("\t\t\tnote\t", note) |>
          cat(file = tbl_fn, sep = "\n", append = TRUE)

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
        f9 = paste0("ID=cds-",cur$gene,";Parent=gene-",cur$gene,";Name=",cur$gene,";gbkey=CDS;gene=",cur$gene,";product=",product,";transl_table=",dat$genetic_code,";Note=",note)
        # logic to deal with annotations that wrap around the end of a circular assembly
        if(dat$topology == "circular" && cur$pos1 > cur$pos2 && cur$length != (cur$pos1 - cur$pos2 + 1)){ # annotation wraps
          pos2_fix <- asmb_len + cur$pos2
          paste(c(seq_name, "MitoPilot", "CDS", cur$pos1, pos2_fix, ".", cur$direction, "0", f9), collapse = "\t") |>
            cat(file = gff_fn, sep = "\n", append = TRUE)
        } else {
          paste(c(seq_name, "MitoPilot", "CDS", cur$pos1, cur$pos2, ".", cur$direction, "0", f9), collapse = "\t") |>
            cat(file = gff_fn, sep = "\n", append = TRUE)
        }

        # Per-gene FASTA/tbl export is intentionally skipped for ORFs: ORF.N
        # numbering is per-sample and not orthologous across samples, so the
        # gene-grouped files must not mix ORFs from different samples.

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

  db_path <- file.path(dirname(out_dir), ".sqlite")

  if (length(group) == 1 && length(IDs) > 1 && generateAAalignments) {
    make_PCG_alignments(
      export_group = group,
      db = db_path,
      out_path = group_pth,
      start_aa = start_aa,
      stop_aa = stop_aa,
      ident_pct = ident_pct
    )
  }

  if (review && length(group) == 1 && length(IDs) > 1) {
    return(invisible(flag_PCG_outliers(
      group = group,
      db = db_path,
      start_aa = start_aa,
      stop_aa = stop_aa,
      ident_pct = ident_pct
    )))
  }

  invisible(NULL)
}


#' Generate HTML report woth PCG alignments
#'
#' @param db path for sqlite database
#' @param out_path path for output files
#' @param export_group Name of the submission group
#' @param start_aa Start-offset threshold (amino acids) for the flagged-outlier
#'   section of the report. Default 10.
#' @param stop_aa Stop-offset threshold (amino acids) for the flagged-outlier
#'   section of the report. Default 10.
#' @param ident_pct Identity threshold (percent) for the flagged-outlier section
#'   of the report. Default 60.
#'
#' @export
#'
make_PCG_alignments <- function(
    export_group = NULL,
    db = NULL,
    out_path = NULL,
    start_aa = 10,
    stop_aa = 10,
    ident_pct = 60) {
  rmarkdown::render(
    input = system.file("AA_alignment_report.Rmd", package = "MitoPilot"),
    output_file = stringr::str_glue("AA_alignments_{export_group}.html"),
    output_dir = out_path,
    intermediates_dir = getwd(),
    knit_root_dir = getwd(),
    params = list(
      group = export_group,
      db_path = db,
      start_aa = start_aa,
      stop_aa = stop_aa,
      ident_pct = ident_pct
    )
  )
}


#' Collect PCG annotations for an export group (with exons merged)
#'
#' Pulls all protein-coding gene (PCG) annotations for the samples in an export
#' group and, for genes flagged as intron-containing in the curation rules,
#' merges multi-exon entries into a single translated record. The merged record
#' keeps the first exon's row, has its `translation` recomputed from the spliced
#' sequence, and its `ID` prefixed with `*` to mark the merge. Shared by the
#' AA alignment report (`inst/AA_alignment_report.Rmd`) and
#' [flag_PCG_outliers()] so both use identical sequences.
#'
#' @param con An open SQLite connection to the project database.
#' @param group Name of the export group.
#'
#' @return A data frame of PCG annotations (one row per sample/gene after
#'   merging), including `ID`, `path`, `scaffold`, `gene`, `pos1`, `pos2`,
#'   `direction`, and `translation`.
#'
#' @noRd
get_export_PCG_annotations <- function(con, group) {
  annotations <- dplyr::tbl(con, "samples") |>
    dplyr::filter(export_group == !!group) |>
    dplyr::select(ID) |>
    dplyr::left_join(
      dplyr::tbl(con, "annotations") |>
        dplyr::filter(pos1 > 0 & type == "PCG"),
      by = "ID"
    ) |>
    dplyr::collect()

  # IDs in the group, used to drive per-sample exon merging
  IDs <- dplyr::tbl(con, "samples") |>
    dplyr::filter(export_group == !!group) |>
    dplyr::pull("ID")

  # Row indices of exons removed after being merged into their first exon
  rows_to_remove <- integer(0)

  for (ID in IDs) {
    # Get curation rules for the current ID
    curation_opts <- dplyr::tbl(con, "annotate") |>
      dplyr::filter(ID == !!ID) |>
      dplyr::pull("curate_opts")

    curate_rules <- dplyr::tbl(con, "curate_opts") |>
      dplyr::filter(curate_opts == !!curation_opts) |>
      dplyr::pull("params") |>
      jsonlite::fromJSON()

    # Get sample data
    dat <- dplyr::tbl(con, "samples") |>
      dplyr::select(-dplyr::any_of("topology")) |>
      dplyr::filter(ID == !!ID) |>
      dplyr::left_join(
        dplyr::tbl(con, "annotate") |>
          dplyr::select(ID, topology, path) |>
          dplyr::distinct(),
        by = "ID"
      ) |>
      dplyr::collect()

    # Get mitogenome sequence (respecting per-scaffold ignore flag)
    kept_scaffolds <- dplyr::tbl(con, "assemblies") |>
      dplyr::filter(ID == !!ID & path == !!dat$path & ignore == 0) |>
      dplyr::pull("scaffold")
    seq <- get_assembly(
      ID = ID,
      path = dat$path,
      scaffold = kept_scaffolds,
      con = con
    )
    if (length(seq) > 1) {
      stop("Multiple non-ignored scaffolds found. Export not supported for fragmented assemblies. Mark all but one scaffold as ignore to export.")
    }

    genes_in_id <- unique(annotations$gene[annotations$ID == ID])

    for (current_gene in genes_in_id) {
      exons_idx <- which(annotations$ID == ID & annotations$gene == current_gene)
      if (any(exons_idx %in% rows_to_remove)) next
      if (length(exons_idx) <= 1) next

      cur <- annotations[exons_idx[1], ] # Use the first exon as the reference
      cur_rules <- curate_rules$rules[[cur$gene]]
      intron <- cur_rules$intron %||% curate_rules$default_rules$PCG$intron %||% FALSE

      if (intron) {
        exons <- annotations[exons_idx, ]
        exon_seqs <- character(nrow(exons))

        message(paste("Merged ", length(exon_seqs), " exons for gene ", cur$gene, " (", ID, ")", sep = ""))

        if (all(exons$direction == "+")) {
          for (i in 1:nrow(exons)) {
            exon_seqs[i] <- as.character(Biostrings::subseq(seq, start = exons$pos1[i], end = exons$pos2[i]))
          }
          merged_sequence <- Biostrings::DNAString(paste(exon_seqs, collapse = ""))
        } else if (all(exons$direction == "-")) {
          for (i in 1:nrow(exons)) {
            exon_seqs[i] <- as.character(Biostrings::subseq(seq, start = exons$pos1[i], end = exons$pos2[i]))
          }
          merged_sequence <- Biostrings::DNAString(paste(exon_seqs, collapse = "")) |>
            Biostrings::reverseComplement()
        } else {
          message(crayon::red(paste("Warning: exons on opposite strands for gene", cur$gene)))
          next
        }

        translation <- Biostrings::translate(
          merged_sequence,
          genetic.code = Biostrings::getGeneticCode(as.character(dat$genetic_code))
        ) |> as.character()
        translation <- sub("\\*$", "", translation) # remove terminal stop codon

        annotations[exons_idx[1], "translation"] <- translation
        annotations[exons_idx[1], "ID"] <- paste0("*", annotations[exons_idx[1], "ID"])
        rows_to_remove <- c(rows_to_remove, exons_idx[-1])
      }
    }
  }

  if (length(rows_to_remove) > 0) {
    annotations <- annotations[-rows_to_remove, ]
  }

  annotations
}


#' Flag outlier PCG annotations in an export group
#'
#' For each protein-coding gene in an export group, aligns the amino-acid
#' translations across samples and flags annotations that are likely
#' mis-positioned: those whose start or stop extends past, or falls short of,
#' the alignment's well-occupied core by more than a set number of residues
#' (pointing at a start/stop codon placed too long or too short) and those that
#' align poorly to the rest of the group (a low sequence-identity catch-all for
#' badly annotated regions).
#'
#' @param group Name of the export group.
#' @param db Path to the project SQLite database.
#' @param start_aa Start-offset threshold (amino acids). A sample is flagged
#'   when its start extends past, or falls short of, the alignment core by more
#'   than this many residues. Default 10.
#' @param stop_aa Stop-offset threshold (amino acids). As `start_aa`, but for
#'   the stop end. Default 10.
#' @param ident_pct Identity threshold (percent). A sample is flagged when its
#'   mean pairwise identity to the rest of the group is below this. Default 60.
#' @param genes Optional character vector of gene names. When supplied, only
#'   these genes are aligned and flagged (the rest are skipped), e.g. to
#'   recompute a single gene edited via "Back to Review". Default `NULL` (all
#'   genes).
#'
#' @return A list with two elements:
#'   \describe{
#'     \item{flags}{A tibble with one row per flagged (sample, gene):
#'       `ID`, `label`, `path`, `scaffold`, `gene`, `pct_identity`,
#'       `start_offset`, `stop_offset`, `start_flag`, `stop_flag`,
#'       `identity_flag`, `issue`.}
#'     \item{alignments}{A named list (by gene) of clustering-ordered aligned
#'       `AAStringSet` objects, for every gene that has a flagged sample.}
#'   }
#'
#' @export
flag_PCG_outliers <- function(group, db, start_aa = 10, stop_aa = 10, ident_pct = 60, genes = NULL) {
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = db)
  on.exit(DBI::dbDisconnect(con))

  annotations <- get_export_PCG_annotations(con, group)
  annotations <- annotations[!is.na(annotations$translation) & nzchar(annotations$translation), , drop = FALSE]
  if (nrow(annotations) == 0) {
    return(list(flags = .empty_outlier_flags(), alignments = list()))
  }

  all_genes <- sort(unique(annotations$gene))
  # Optionally restrict the (expensive) per-gene alignment loop to a subset, e.g.
  # recomputing only the single gene edited via "Back to Review".
  loop_genes <- if (!is.null(genes)) intersect(all_genes, genes) else all_genes
  flag_rows <- list()
  alignments <- list()

  for (g in loop_genes) {
    sub <- annotations[annotations$gene == g, , drop = FALSE]
    if (nrow(sub) < 2) next

    seqs <- Biostrings::AAStringSet(stats::setNames(sub$translation, sub$ID))

    aln <- DECIPHER::AlignSeqs(seqs, processors = NULL, verbose = FALSE)
    dst <- DECIPHER::DistanceMatrix(
      aln,
      includeTerminalGaps = TRUE, processors = NULL, type = "dist", verbose = FALSE
    )
    clust <- stats::hclust(dst, "complete")
    aln <- aln[clust$order]
    alignments[[g]] <- aln

    # Mean pairwise identity of each sample to the rest of the group
    dmat <- as.matrix(DECIPHER::DistanceMatrix(
      aln,
      includeTerminalGaps = TRUE, processors = NULL, type = "matrix", verbose = FALSE
    ))
    diag(dmat) <- NA_real_
    pct_identity <- 100 * (1 - rowMeans(dmat, na.rm = TRUE))

    # Character matrix of the alignment (rows = sequences, cols = columns)
    cmat <- do.call(rbind, strsplit(as.character(aln), "", fixed = TRUE))
    rownames(cmat) <- names(aln)
    occupancy <- colMeans(cmat != "-")
    core <- which(occupancy > 0.5)
    if (length(core) == 0) next
    first_core <- min(core)
    last_core <- max(core)

    for (label in names(aln)) {
      row_chars <- cmat[label, ]
      res_cols <- which(row_chars != "-")
      if (length(res_cols) == 0) next

      # Start (N-terminus): missing core columns before this seq starts vs
      # residues extending before the core
      start_short <- sum(core < min(res_cols)) # too short: annotation starts too late
      start_long <- sum(res_cols < first_core) # too long: annotation starts too early
      # Stop (C-terminus)
      stop_short <- sum(core > max(res_cols)) # too short: annotation stops too early
      stop_long <- sum(res_cols > last_core) # too long: annotation stops too late

      start_flag <- max(start_short, start_long) > start_aa
      stop_flag <- max(stop_short, stop_long) > stop_aa

      idx <- match(label, names(aln))
      ident <- unname(pct_identity[idx])
      identity_flag <- isTRUE(ident < ident_pct)

      if (!start_flag && !stop_flag && !identity_flag) next

      # Signed per-end offset (aa) relative to the alignment core:
      # negative = end placed too short, positive = extends too long.
      start_offset <- start_long - start_short
      stop_offset <- stop_long - stop_short

      issues <- character(0)
      if (start_flag) issues <- c(issues, if (start_offset < 0) "start too short" else "start too long")
      if (stop_flag) issues <- c(issues, if (stop_offset < 0) "stop too short" else "stop too long")
      if (identity_flag) issues <- c(issues, "low identity")

      srow <- sub[match(label, sub$ID), ]
      flag_rows[[length(flag_rows) + 1L]] <- dplyr::tibble(
        ID = sub("^\\*", "", label),
        label = label,
        path = srow$path,
        scaffold = srow$scaffold,
        gene = g,
        pct_identity = round(ident, 1),
        start_offset = start_offset,
        stop_offset = stop_offset,
        start_flag = start_flag,
        stop_flag = stop_flag,
        identity_flag = identity_flag,
        issue = paste(issues, collapse = ", ")
      )
    }
  }

  flags <- if (length(flag_rows) > 0) dplyr::bind_rows(flag_rows) else .empty_outlier_flags()
  # Keep alignments only for genes that actually have a flagged sample
  alignments <- alignments[names(alignments) %in% unique(flags$gene)]

  list(flags = flags, alignments = alignments)
}

# Empty flags tibble with the canonical column types
.empty_outlier_flags <- function() {
  dplyr::tibble(
    ID = character(0),
    label = character(0),
    path = integer(0),
    scaffold = integer(0),
    gene = character(0),
    pct_identity = numeric(0),
    start_offset = integer(0),
    stop_offset = integer(0),
    start_flag = logical(0),
    stop_flag = logical(0),
    identity_flag = logical(0),
    issue = character(0)
  )
}
