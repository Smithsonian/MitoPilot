NCBI_EUTILS <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils"
.ncbi_env <- new.env()

#' Normalize NCBI BioSample or SRA accessions
#'
#' @param x Vector of BioSample accessions (SAMN, SAMEA, SAMD), BioSample
#'   numbers, SRA accessions (SRR/ERR/DRR runs, SRX experiments, SRS samples),
#'   or ncbi.nlm.nih.gov biosample / sra links.
#' @param strict Reject bare BioSample numbers, for values that are really
#'   sample IDs (a sample ID `12345` is not a BioSample).
#' @return Character vector of upper-case IDs, NA where the input is blank or
#'   not a BioSample or SRA ID.
#' @export
ncbi_normalize_id <- function(x, strict = FALSE) {
  x <- toupper(trimws(.meta_chr(x)))
  x <- sub("^HTTPS?://(WWW\\.)?NCBI\\.NLM\\.NIH\\.GOV/(BIOSAMPLE|SRA)/", "", x)
  x <- sub("/+$", "", x)
  ok <- !is.na(x) & grepl(if (strict) "^(SAM(N|EA|D)[0-9]+|[SED]R[RXS][0-9]+)$" else
    "^(SAM(N|EA|D)[0-9]+|[0-9]+|[SED]R[RXS][0-9]+)$", x)
  ifelse(ok, x, NA_character_)
}

.ncbi_is_sra <- function(x) !is.na(x) & grepl("^[SED]R[RXS][0-9]+$", x)

.ncbi_get <- function(endpoint, query) {
  key <- Sys.getenv("ENTREZ_KEY")
  gap <- if (nzchar(key)) 0.11 else 0.34
  wait <- gap - (as.numeric(Sys.time()) - (.ncbi_env$last %||% 0))
  if (wait > 0) Sys.sleep(wait)
  q <- c(query, list(tool = "MitoPilot"), if (nzchar(key)) list(api_key = key))
  req <- httr2::request(paste0(NCBI_EUTILS, "/", endpoint, ".fcgi")) |>
    httr2::req_url_query(!!!q) |>
    httr2::req_user_agent("MitoPilot (https://github.com/Smithsonian/MitoPilot)") |>
    httr2::req_timeout(30) |>
    httr2::req_retry(max_tries = 3,
                     is_transient = function(r) httr2::resp_status(r) %in% c(429, 500, 502, 503, 504)) |>
    httr2::req_error(is_error = function(r) FALSE)
  resp <- tryCatch(httr2::req_perform(req), error = function(e) {
    stop("could not reach NCBI (", conditionMessage(e), ")", call. = FALSE)
  })
  .ncbi_env$last <- as.numeric(Sys.time())
  st <- httr2::resp_status(resp)
  if (st >= 400) stop("NCBI returned HTTP ", st, call. = FALSE)
  httr2::resp_body_string(resp)
}

.ncbi_txt <- function(node, xpath) {
  n <- xml2::xml_find_all(node, xpath)
  v <- trimws(xml2::xml_text(n))
  v <- v[nzchar(v)]
  if (!length(v)) NA_character_ else paste(unique(v), collapse = ", ")
}

.ncbi_att <- function(node, xpath, attr) {
  n <- xml2::xml_find_first(node, xpath)
  if (inherits(n, "xml_missing")) NA_character_ else xml2::xml_attr(n, attr)
}

.ncbi_enum <- function(x) sub("^e(?=[A-Z])", "", x, perl = TRUE)

.ncbi_sra <- function(acc) {
  s <- xml2::read_xml(.ncbi_get("esearch", list(db = "sra", term = paste0(acc, "[accn]"))))
  uid <- xml2::xml_text(xml2::xml_find_first(s, "//IdList/Id"))
  if (is.na(uid)) stop("SRA accession ", acc, " not found", call. = FALSE)
  j <- jsonlite::fromJSON(.ncbi_get("esummary", list(db = "sra", id = uid, retmode = "json")),
                          simplifyVector = FALSE)
  r <- j$result[[uid]]
  x <- xml2::read_xml(paste0("<r>", r$expxml %||% "", "</r>"))
  runs <- xml2::read_xml(paste0("<r>", r$runs %||% "", "</r>"))
  lay <- xml2::xml_find_first(x, ".//LIBRARY_LAYOUT/*")
  f <- list(
    accession = acc,
    experiment = .ncbi_att(x, ".//Experiment", "acc"),
    study = .ncbi_att(x, ".//Study", "acc"),
    study_title = .ncbi_att(x, ".//Study", "name"),
    title = .ncbi_txt(x, ".//Summary/Title"),
    runs = .ncbi_txt(runs, ".//Run/@acc"),
    platform = .ncbi_txt(x, ".//Summary/Platform"),
    instrument = .ncbi_att(x, ".//Summary/Platform", "instrument_model"),
    library_strategy = .ncbi_txt(x, ".//LIBRARY_STRATEGY"),
    library_source = .ncbi_txt(x, ".//LIBRARY_SOURCE"),
    library_selection = .ncbi_txt(x, ".//LIBRARY_SELECTION"),
    library_layout = if (inherits(lay, "xml_missing")) NA_character_ else xml2::xml_name(lay),
    center = .ncbi_att(x, ".//Submitter", "center_name"),
    biosample = .ncbi_txt(x, ".//Biosample"),
    bioproject = .ncbi_txt(x, ".//Bioproject")
  )
  if (is.na(f$biosample)) stop("SRA accession ", acc, " has no linked BioSample", call. = FALSE)
  f
}

.ncbi_biosample <- function(id) {
  x <- xml2::read_xml(.ncbi_get("efetch", list(db = "biosample", id = id, retmode = "xml")))
  b <- xml2::xml_find_first(x, "/BioSampleSet/BioSample")
  if (inherits(b, "xml_missing")) stop("BioSample ", id, " not found", call. = FALSE)
  f <- list(
    accession = xml2::xml_attr(b, "accession"),
    title = .ncbi_txt(b, "Description/Title"),
    organism = .ncbi_txt(b, "Description/Organism/OrganismName") %|NA|%
      .ncbi_att(b, "Description/Organism", "taxonomy_name"),
    taxonomy_id = .ncbi_att(b, "Description/Organism", "taxonomy_id"),
    sample_name = .ncbi_txt(b, "Ids/Id[@db_label='Sample name']"),
    sra_sample = .ncbi_txt(b, "Ids/Id[@db='SRA']"),
    owner = .ncbi_txt(b, "Owner/Name"),
    package = .ncbi_txt(b, "Package"),
    publication_date = xml2::xml_attr(b, "publication_date"),
    last_update = xml2::xml_attr(b, "last_update")
  )
  at <- xml2::xml_find_all(b, "Attributes/Attribute")
  nm <- xml2::xml_attr(at, "harmonized_name")
  nm <- ifelse(is.na(nm), xml2::xml_attr(at, "attribute_name"), nm)
  val <- trimws(xml2::xml_text(at))
  keep <- !duplicated(nm) & !nm %in% names(f)
  f <- c(f, stats::setNames(as.list(val[keep]), nm[keep]))
  ln <- xml2::xml_find_all(b, "Links/Link[@target='bioproject']")
  list(fields = f, projects = data.frame(uid = trimws(xml2::xml_text(ln)),
                                         acc = xml2::xml_attr(ln, "label")))
}

.ncbi_bioproject <- function(uid) {
  x <- xml2::read_xml(.ncbi_get("efetch", list(db = "bioproject", id = uid, retmode = "xml")))
  p <- xml2::xml_find_first(x, "//DocumentSummary")
  if (inherits(p, "xml_missing")) stop("BioProject ", uid, " not found", call. = FALSE)
  rel <- xml2::xml_find_all(p, "Project/ProjectDescr/Relevance/*")
  list(
    accession = .ncbi_att(p, "Project/ProjectID/ArchiveID", "accession"),
    name = .ncbi_txt(p, "Project/ProjectDescr/Name"),
    title = .ncbi_txt(p, "Project/ProjectDescr/Title"),
    description = .ncbi_txt(p, "Project/ProjectDescr/Description"),
    relevance = if (length(rel)) paste(xml2::xml_name(rel), collapse = ", ") else NA_character_,
    material = .ncbi_enum(.ncbi_att(p, "Project/ProjectType//Target", "material")),
    capture = .ncbi_enum(.ncbi_att(p, "Project/ProjectType//Target", "capture")),
    sample_scope = .ncbi_enum(.ncbi_att(p, "Project/ProjectType//Target", "sample_scope")),
    data_type = .ncbi_txt(p, "Project/ProjectType//ProjectDataTypeSet/DataType"),
    organization = .ncbi_txt(p, "Submission//Organization/Name"),
    submitted = .ncbi_att(p, "Submission", "submitted"),
    last_update = .ncbi_att(p, "Submission", "last_update"),
    umbrella = .ncbi_att(p, "ProjectLinks//Hierarchical[@type='TopAdmin']/MemberID", "accession")
  )
}

.ncbi_fetch_chain <- function(ref, cache = new.env()) {
  ref <- ncbi_normalize_id(ref)
  out <- list()
  bs <- ref
  if (.ncbi_is_sra(ref)) {
    sra <- .ncbi_sra(ref)
    out[[1]] <- .meta_flatten(sra, "SRA", 0L, ref)
    bs <- sra$biosample
  }
  b <- .ncbi_biosample(bs)
  out[[length(out) + 1L]] <- .meta_flatten(b$fields, "BioSample", 1L, b$fields$accession)
  for (i in seq_len(nrow(b$projects))) {
    key <- paste0("bp:", b$projects$uid[i])
    p <- tryCatch({
      if (is.null(cache[[key]])) cache[[key]] <- .ncbi_bioproject(b$projects$uid[i])
      cache[[key]]
    }, error = function(e) NULL)
    if (!is.null(p)) {
      out[[length(out) + 1L]] <- .meta_flatten(p, "BioProject", 1L + i,
                                               p$accession %|NA|% b$projects$acc[i])
    }
  }
  do.call(rbind, out)
}

# eutils answers HEAD with 405, so .check_resource() cannot be used
.check_ncbi_reachable <- function(iss) {
  ok <- tryCatch({ .ncbi_get("einfo", list()); TRUE }, error = function(e) FALSE)
  if (!ok) iss$warn("NCBI: not reachable right now: ", NCBI_EUTILS)
  invisible(NULL)
}
