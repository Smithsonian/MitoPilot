GBIF_API <- "https://api.gbif.org/v1"

#' Normalize GBIF occurrence IDs
#'
#' @param x Vector of gbifIDs: digits, numbers, or a gbif.org / api.gbif.org
#'   occurrence URL.
#' @return Character vector of digit-only IDs, NA where the input is blank or
#'   not an occurrence ID.
#' @export
gbif_normalize_id <- function(x) {
  x <- trimws(.meta_chr(x))
  x <- sub("^https?://(www\\.)?gbif\\.org/occurrence/", "", x, ignore.case = TRUE)
  x <- sub("^https?://api\\.gbif\\.org/v1/occurrence/", "", x, ignore.case = TRUE)
  x <- sub("/+$", "", x)
  ok <- !is.na(x) & grepl("^[0-9]+$", x)
  ifelse(ok, x, NA_character_)
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
