#' Create a custom curation database
#'
#' Generate a custom curation database from user-supplied table of translated mitochondrial gene sequences.
#' Requires a CSV file containing three columns: "SeqID" = unique name to be used for sequence,
#' "Gene" = name of gene,
#' and "FASTA" = name of fasta file containing the sequence.
#'
#'  Values in "Gene" column of your CSV must only include the following gene abbreviations: \cr
#'     nad1 = "NADH dehydrogenase subunit 1", \cr
#'     nad2 = "NADH dehydrogenase subunit 2", \cr
#'     cox1 = "cytochrome c oxidase subunit 1", \cr
#'     cox2 = "cytochrome c oxidase subunit 2", \cr
#'     cox3 = "cytochrome c oxidase subunit 3", \cr
#'     atp8 = "ATP synthase F0 subunit 8", \cr
#'     atp6 = "ATP synthase F0 subunit 6", \cr
#'     atp9 = "ATP synthase F0 subunit 9", \cr
#'     cox3 = "cytochrome c oxidase subunit 3", \cr
#'     nad3 = "NADH dehydrogenase subunit 3", \cr
#'     nad4l = "NADH dehydrogenase subunit 4L", \cr
#'     nad4 = "NADH dehydrogenase subunit 4", \cr
#'     nad5 = "NADH dehydrogenase subunit 5", \cr
#'     nad6 = "NADH dehydrogenase subunit 6", \cr
#'     cob = "cytochrome b", \cr
#'     dpo = "DNA-polymerase", \cr
#'     lagli = "homing endonuclease", \cr
#'     msh1 = "MutS mismatch DNA repair protein", \cr
#'     mttb = "trimethylamine methyltransferase"
#'
#' @param path Path to the project directory (default = current working directory)
#' @param genes_to_add Full path to CSV file containing three columns: SeqID = unique name to be used for sequence, Gene = name of gene, FASTA = name of fasta file containing the sequence
#' @param gene_fasta_dir Full path to directory containing your gene FASTA files, one file per sequence
#' @param base_db Which base NCBI RefSeq database to use, "Metazoa" or "Chordata"?
#'
#' @export
#'

gen_custom_curation_db <- function(path = ".",
                                   genes_to_add = NULL,
                                   gene_fasta_dir = NULL,
                                   base_db = "Metazoa") {
  # read in user data
  setwd(path)
  data <- read.csv(genes_to_add, header = TRUE)

  # check that makeblastdb is installed and in the PATH
  makeblastdb_path <- Sys.which("makeblastdb")
  # Check if the returned path is an empty string.
  if (makeblastdb_path == "") {
    stop(
      "`makeblastdb` was not found, please ensure NCBI BLAST+ is installed correctly and that its 'bin' directory has been added to your PATH environment variable."
    )
  } else {
    message(paste(
      "`makeblastdb` is installed and located at:",
      makeblastdb_path
    ))
  }

  # make sure user CSV contains the required columns
  if (any(colnames(data) %nin% c("SeqID", "Gene", "FASTA"))) {
    stop("genes_to_add CSV must contain columns \"SeqID\", \"Gene\", and \"FASTA\"")
  }

  # Validate unique ID
  if (any(duplicated(paste0(data$SeqID, data$Gene)))) {
    bad_IDs <- unique(data$SeqID[duplicated(paste0(data$SeqID, data$Gene))])
    message("duplicated IDs:")
    message(paste(bad_IDs, collapse = ", "))
    stop("Duplicate values found in SeqID column")
  }

  # Validate unique FASTA files
  if (any(duplicated(data$FASTA))) {
    bad_IDs <- unique(data$FASTA[duplicated(data$FASTA)])
    message("duplicated FASTA files:")
    message(paste(bad_IDs, collapse = ", "))
    stop("Duplicate values found in FASTA column")
  }

  # check that gene names are correct
  CDS_key <- c(
    nad1 = "NADH dehydrogenase subunit 1",
    nad2 = "NADH dehydrogenase subunit 2",
    cox1 = "cytochrome c oxidase subunit 1",
    cox2 = "cytochrome c oxidase subunit 2",
    cox3 = "cytochrome c oxidase subunit 3",
    atp8 = "ATP synthase F0 subunit 8",
    atp6 = "ATP synthase F0 subunit 6",
    atp9 = "ATP synthase F0 subunit 9",
    cox3 = "cytochrome c oxidase subunit 3",
    nad3 = "NADH dehydrogenase subunit 3",
    nad4l = "NADH dehydrogenase subunit 4L",
    nad4 = "NADH dehydrogenase subunit 4",
    nad5 = "NADH dehydrogenase subunit 5",
    nad6 = "NADH dehydrogenase subunit 6",
    cob = "cytochrome b",
    dpo = "DNA-polymerase",
    lagli = "homing endonuclease",
    msh1 = "MutS mismatch DNA repair protein",
    mttb = "trimethylamine methyltransferase"
  )

  # list of genes in user CSV
  user_genes <- unique(data$Gene)

  # make sure all gene names in CSV are correctly formatted
  if (any(user_genes %nin% names(CDS_key))) {
    bad_gene <- user_genes[user_genes %nin% names(CDS_key)]
    message("unsupported gene names:")
    message(paste(bad_gene, collapse = ", "))
    message("Gene column must contain only the following values:")
    message(paste(names(CDS_key), collapse = ", "))
    stop("Found unsupported values in the Gene column")
  }

  # create directory for custom databases
  dir.create(file.path(path, "custom_curation_dbs"), showWarnings = FALSE)
  cur_dir <- strftime(as.POSIXlt(Sys.time(), "UTC"), "%Y-%m-%dT%H-%M-%S")
  dir.create(file.path(path, "custom_curation_dbs", cur_dir),
             showWarnings = FALSE)
  # download the base curation database
  if (base_db == "Metazoa") {
    URL <- "https://raw.githubusercontent.com/Smithsonian/MitoPilot/refs/heads/main/ref_dbs/Mitos2/Metazoa_RefSeq231.tar.gz"
    download.file(
      url = URL,
      destfile = file.path('./custom_curation_dbs/Metazoa_RefSeq231.tar.gz'),
      method = 'curl'
    )
    untar(
      file.path('./custom_curation_dbs/Metazoa_RefSeq231.tar.gz'),
      exdir = file.path(path, "custom_curation_dbs", cur_dir)
    )
    file.remove(file.path('./custom_curation_dbs/Metazoa_RefSeq231.tar.gz'))
    orig_db_dir_base <- "Metazoa_RefSeq231"
    orig_db_dir <- file.path(path, "custom_curation_dbs", cur_dir, "Metazoa_RefSeq231")
  } else if (base_db == "Chordata") {
    URL <- "https://raw.githubusercontent.com/Smithsonian/MitoPilot/refs/heads/main/ref_dbs/Mitos2/Chordata.tar.gz"
    download.file(
      url = URL,
      destfile = file.path('./custom_curation_dbs/Chordata.tar.gz'),
      method = 'curl'
    )
    untar(
      file.path('./custom_curation_dbs/Chordata.tar.gz'),
      exdir = file.path(path, "custom_curation_dbs", cur_dir)
    )
    file.remove(file.path('./custom_curation_dbs/Chordata.tar.gz'))
    orig_db_dir_base <- "Chordata"
    orig_db_dir <- file.path(path, "custom_curation_dbs", cur_dir, "Chordata")
  } else {
    stop("base_db must be either Metazoa or Chordata")
  }

  # append sequences to existing FASTAs in the curation directory
  setwd(file.path(orig_db_dir, "featureProt"))
  for (gene in user_genes) {
    # read in FASTA
    message(paste0("processing ", gene))
    all_seqs <- Biostrings::readAAStringSet(file.path(orig_db_dir, "featureProt", paste0(gene, ".fas")))
    for (i in 1:nrow(data)) {
      if (data$Gene[i] == gene) {
        fasta_file_path <- file.path(gene_fasta_dir, data$FASTA[i])
        # Check if the file exists before trying to read it
        if (!file.exists(fasta_file_path)) {
          stop(paste(
            "Error: Required file does not exist at path:",
            fasta_file_path
          ))
        }
        new_seq <- Biostrings::readAAStringSet(fasta_file_path)
        names(new_seq)[1] <- data$SeqID[i]
        all_seqs <- c(all_seqs, new_seq)
      }
    }
    # write the new fasta file
    Biostrings::writeXStringSet(all_seqs, file.path(orig_db_dir, "featureProt", paste0(gene, ".fas")))
    # make BLAST database
    system2(
      "makeblastdb",
      args = paste0("-dbtype prot -in ", paste0(gene, ".fas")),
      stdout = NULL,
      stderr = NULL
    )
  }

  # rename directory
  setwd(file.path(path, "custom_curation_dbs", cur_dir))
  file.rename(file.path(orig_db_dir), file.path(paste0(orig_db_dir, "_custom")))

  #tar(paste0(orig_db_dir_base, "_custom.tar.gz"), paste0(orig_db_dir_base, "_custom"), compression = 'gzip', tar="tar")
  #unlink(file.path(paste0(orig_db_dir, "_custom")), recursive = TRUE)

  message("FINISHED creating custom curation database")
  message("In the curation options, specify the following directory as the \'ref_dif\':")
  message(file.path(path, "custom_curation_dbs", cur_dir))
  message(paste0("and select \'", orig_db_dir_base, "\' as the \'ref_db\'."))
}
