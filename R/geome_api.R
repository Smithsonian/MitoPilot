GEOME_API <- "https://api.geome-db.org"

#' Normalize GEOME BCIDs to bare ARKs
#'
#' @param x Character vector of BCIDs, optionally with an n2t.net or
#'   geome-db.org URL prefix.
#' @return Character vector of `ark:/NNNNN/...` identifiers, NA where the
#'   input is blank or not an ARK.
#' @export
geome_normalize_bcid <- function(x) {
  x <- trimws(as.character(x))
  x <- sub("^https?://[^/]+/(record/)?", "", x)
  ok <- !is.na(x) & grepl("^ark:/[0-9]+/[A-Za-z0-9._~-]+$", x)
  ifelse(ok, x, NA_character_)
}

.geome_get <- function(path, query = list()) {
  req <- httr2::request(paste0(GEOME_API, "/", path)) |>
    httr2::req_url_query(!!!query) |>
    httr2::req_user_agent("MitoPilot (https://github.com/Smithsonian/MitoPilot)") |>
    httr2::req_timeout(30) |>
    httr2::req_retry(max_tries = 3,
                     is_transient = function(r) httr2::resp_status(r) %in% c(429, 502, 503, 504)) |>
    httr2::req_error(is_error = function(r) FALSE)
  resp <- tryCatch(httr2::req_perform(req), error = function(e) {
    stop("could not reach GEOME (", conditionMessage(e), ")", call. = FALSE)
  })
  st <- httr2::resp_status(resp)
  if (st %in% c(401, 403)) stop("record is private or needs a GEOME login", call. = FALSE)
  if (st == 400) stop("BCID not recognized by GEOME", call. = FALSE)
  if (st %in% c(404, 500)) stop("BCID not found in GEOME", call. = FALSE)
  if (st >= 400) stop("GEOME returned HTTP ", st, call. = FALSE)
  jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector = FALSE)
}

.geome_flatten <- function(x, level, depth, bcid) {
  keep <- vapply(x, function(v) {
    length(v) == 1L && !is.list(v) && !is.na(v) && nzchar(as.character(v))
  }, logical(1))
  x <- x[keep]
  if (!length(x)) return(NULL)
  data.frame(level = level, depth = as.integer(depth), bcid = bcid,
             field = names(x), value = vapply(x, as.character, ""),
             row.names = NULL)
}

.geome_fetch_chain <- function(bcid, cache = new.env()) {
  get <- function(key, path, query = list()) {
    if (is.null(cache[[key]])) cache[[key]] <- .geome_get(path, query)
    cache[[key]]
  }
  out <- list()
  seen <- character()
  cur <- bcid
  depth <- 0L
  top <- NULL
  while (!is.null(cur)) {
    if (cur %in% seen || depth >= 10L) {
      stop("GEOME parent chain loops or is too deep", call. = FALSE)
    }
    seen <- c(seen, cur)
    resp <- get(cur, paste0("records/", cur), list(includeParent = "true"))
    top <- resp$record
    out[[length(out) + 1L]] <- .geome_flatten(top, top$entity %||% "Record", depth, cur)
    cur <- resp$parent$bcid
    depth <- depth + 1L
  }
  pid <- top$projectId
  exp <- top$expeditionCode
  if (!is.null(pid) && !is.null(exp)) {
    tryCatch({
      e <- get(paste0("exp:", pid, ":", exp), paste0("projects/", pid, "/expeditions/", exp))
      out[[length(out) + 1L]] <- .geome_flatten(e, "Expedition", depth, e$identifier %||% NA_character_)
      projects <- get("projects", "projects", list(includePublic = "true"))
      p <- Filter(function(x) identical(as.character(x$projectId), as.character(pid)), projects)
      if (length(p)) {
        out[[length(out) + 1L]] <- .geome_flatten(p[[1]], "Project", depth + 1L, NA_character_)
      }
    }, error = function(e) NULL)
  }
  do.call(rbind, out)
}
