GBIF_API <- "https://api.gbif.org/v1"

#' Normalize GBIF occurrence IDs
#'
#' @param x Vector of gbifIDs: digits, numbers, or a gbif.org / api.gbif.org
#'   occurrence URL. Smithsonian NMNH EZIDs (ark:/65665/3..., n2t.net or
#'   collections.nmnh.si.edu links) are also accepted.
#' @return Character vector of digit-only IDs or canonical NMNH ARKs, NA where
#'   the input is blank or not an occurrence ID.
#' @export
gbif_normalize_id <- function(x) {
  x <- trimws(.meta_chr(x))
  x <- sub("^https?://(www\\.)?gbif\\.org/occurrence/", "", x, ignore.case = TRUE)
  x <- sub("^https?://api\\.gbif\\.org/v1/occurrence/", "", x, ignore.case = TRUE)
  x <- sub("/+$", "", x)
  ok <- !is.na(x) & grepl("^[0-9]+$", x)
  ark <- .nmnh_normalize_ark(x)
  ifelse(ok, x, ark)
}

.nmnh_normalize_ark <- function(x) {
  m <- regmatches(x, regexpr("ark:/65665/3[0-9a-fA-F-]+", x))
  out <- rep(NA_character_, length(x))
  hit <- !is.na(x) & grepl("ark:/65665/3[0-9a-fA-F-]+", x)
  u <- tolower(gsub("-", "", sub("^ark:/65665/3", "", m)))
  u <- ifelse(nchar(u) == 32, u, NA_character_)
  out[hit] <- ifelse(is.na(u), NA_character_, paste0(
    "ark:/65665/3", substr(u, 1, 8), "-", substr(u, 9, 12), "-", substr(u, 13, 16), "-",
    substr(u, 17, 20), "-", substr(u, 21, 32)))
  out
}

.nmnh_resolve_ark <- function(ark) {
  q <- utils::URLencode(paste0("http://n2t.net/", ark), reserved = TRUE)
  res <- .gbif_get(paste0("occurrence/search?limit=2&occurrenceId=", q))$results
  if (length(res) != 1L) {
    stop("no GBIF occurrence found for NMNH EZID ", ark,
         " (GBIF may not have indexed it yet)", call. = FALSE)
  }
  .meta_chr(res[[1]]$key)
}

.gbif_get <- function(path) {
  req <- httr2::request(paste0(GBIF_API, "/", path)) |>
    httr2::req_user_agent("MitoPilot (https://github.com/Smithsonian/MitoPilot)") |>
    httr2::req_timeout(30) |>
    httr2::req_retry(max_tries = 3,
                     is_transient = function(r) httr2::resp_status(r) %in% c(429, 500, 502, 503, 504)) |>
    httr2::req_error(is_error = function(r) FALSE)
  resp <- tryCatch(httr2::req_perform(req), error = function(e) {
    stop("could not reach GBIF (", conditionMessage(e), ")", call. = FALSE)
  })
  st <- httr2::resp_status(resp)
  if (st == 404) {
    stop("GBIF occurrence not found (IDs can change when a dataset is republished)", call. = FALSE)
  }
  if (st >= 400) stop("GBIF returned HTTP ", st, call. = FALSE)
  jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector = FALSE)
}

.gbif_fetch_chain <- function(id, cache = new.env()) {
  if (startsWith(id, "ark:")) id <- .nmnh_resolve_ark(id)
  get <- function(key, path) {
    if (is.null(cache[[key]])) cache[[key]] <- .gbif_get(path)
    cache[[key]]
  }
  occ <- get(paste0("occ:", id), paste0("occurrence/", id))
  occ$issues <- if (length(occ$issues)) paste(unlist(occ$issues), collapse = ",") else NULL
  out <- list(.meta_flatten(occ, "Occurrence", 0L, .meta_chr(occ$key %||% id)))
  org <- occ$publishingOrgKey
  if (!is.null(occ$datasetKey)) {
    tryCatch({
      d <- get(paste0("ds:", occ$datasetKey), paste0("dataset/", occ$datasetKey))
      d$citation <- d$citation$text
      out[[length(out) + 1L]] <- .meta_flatten(d, "Dataset", 1L, occ$datasetKey)
      org <- org %||% d$publishingOrganizationKey
    }, error = function(e) NULL)
  }
  if (!is.null(org)) {
    tryCatch({
      o <- get(paste0("org:", org), paste0("organization/", org))
      o$homepage <- if (length(o$homepage)) paste(unlist(o$homepage), collapse = ", ") else NULL
      out[[length(out) + 1L]] <- .meta_flatten(o, "Organization", 2L, org)
    }, error = function(e) NULL)
  }
  do.call(rbind, out)
}
