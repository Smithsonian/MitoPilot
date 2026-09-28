NCBI_EUTILS <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils"
.ncbi_env <- new.env()

#' Normalize NCBI BioSample or SRA accessions
#'
#' @param x Vector of BioSample accessions (SAMN, SAMEA, SAMD), BioSample
#'   numbers, SRA accessions (SRR/ERR/DRR runs, SRX experiments, SRS samples),
#'   or ncbi.nlm.nih.gov biosample / sra links.
#' @return Character vector of upper-case IDs, NA where the input is blank or
#'   not a BioSample or SRA ID.
#' @export
ncbi_normalize_id <- function(x) {
  x <- toupper(trimws(.meta_chr(x)))
  x <- sub("^HTTPS?://(WWW\\.)?NCBI\\.NLM\\.NIH\\.GOV/(BIOSAMPLE|SRA)/", "", x)
  x <- sub("/+$", "", x)
  ok <- !is.na(x) & grepl("^(SAM(N|EA|D)[0-9]+|[0-9]+|[SED]R[RXS][0-9]+)$", x)
  ifelse(ok, x, NA_character_)
}

.ncbi_is_sra <- function(x) !is.na(x) & grepl("^[SED]R[RXS][0-9]+$", x)
