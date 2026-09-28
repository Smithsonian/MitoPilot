#' Fetch NCBI BioSample and BioProject metadata for project samples
#'
#' Looks up each sample's NCBI BioSample, directly or through an SRA accession
#' (SRR/ERR/DRR run, SRX experiment, or SRS sample), plus the BioProject(s) it
#' belongs to, and stores everything in the project database for viewing in
#' the app and use at export. Set the environment variable `ENTREZ_KEY` to an
#' NCBI API key for faster lookups.
#'
#' @param path Path to the project directory (default = current working directory)
#' @param ids Sample IDs to fetch. Default: every sample with a BioSample value.
#' @param biosamples Optional BioSample or SRA accessions to set for `ids` first
#'   (same length as `ids`). A blank value removes that sample's BioSample and
#'   its NCBI data.
#' @param from_id Use each sample's own ID as its BioSample value when that ID
#'   is a BioSample or SRA accession (for example an SRR run used as the sample
#'   ID). Samples whose ID is not one are left unchanged.
#' @return Invisibly, a data frame of `ID`, `status`, and `message`.
#' @export
fetch_biosample <- function(path = ".", ids = NULL, biosamples = NULL, from_id = FALSE) {
  if (isTRUE(from_id)) {
    con <- DBI::dbConnect(RSQLite::SQLite(), dbname = file.path(path, ".sqlite"))
    on.exit(DBI::dbDisconnect(con))
    .meta_ensure_tables(con)
    s <- DBI::dbGetQuery(con, "SELECT ID FROM samples")$ID
    s <- if (is.null(ids)) s else intersect(s, ids)
    ok <- !is.na(ncbi_normalize_id(s))
    for (id in s[ok]) .meta_set_ref(con, "NCBI", id, id)
    if (any(!ok)) message("Sample ID is not a BioSample or SRA accession, skipped: ", .lst(s[!ok]))
    ids <- s[ok]
    if (!length(ids)) {
      return(invisible(data.frame(ID = character(), status = character(), message = character())))
    }
  }
  .meta_fetch_project(path, "NCBI", ids, biosamples)
}
