.geome_ensure_tables <- function(con) {
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS geome_records (
    ID TEXT NOT NULL, level TEXT NOT NULL, depth INTEGER NOT NULL, bcid TEXT,
    field TEXT NOT NULL, value TEXT, PRIMARY KEY (ID, depth, field))")
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS geome_status (
    ID TEXT NOT NULL, bcid TEXT, status TEXT NOT NULL, message TEXT,
    fetched_at INTEGER, PRIMARY KEY (ID))")
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS geome_export_fields (
    key TEXT NOT NULL, PRIMARY KEY (key))")
  if (DBI::dbExistsTable(con, "samples") &&
      !"GEOME_BCID" %in% DBI::dbListFields(con, "samples")) {
    DBI::dbExecute(con, "ALTER TABLE samples ADD COLUMN GEOME_BCID TEXT")
  }
  invisible(NULL)
}

.geome_store_value <- function(x) {
  raw <- trimws(as.character(x))
  raw[is.na(raw)] <- ""
  norm <- geome_normalize_bcid(raw)
  ifelse(!is.na(norm), norm, ifelse(nzchar(raw), raw, NA_character_))
}

.geome_drop <- function(con, ids) {
  for (id in ids) {
    DBI::dbExecute(con, "DELETE FROM geome_records WHERE ID = ?", params = list(id))
    DBI::dbExecute(con, "DELETE FROM geome_status WHERE ID = ?", params = list(id))
  }
  invisible(NULL)
}

.geome_set_bcid <- function(con, id, raw) {
  .geome_ensure_tables(con)
  old <- DBI::dbGetQuery(con, "SELECT GEOME_BCID FROM samples WHERE ID = ?", params = list(id))$GEOME_BCID
  old <- if (length(old)) old[1] else NA_character_
  val <- .geome_store_value(raw)
  changed <- xor(is.na(old), is.na(val)) || (!is.na(old) && !is.na(val) && old != val)
  DBI::dbExecute(con, "UPDATE samples SET GEOME_BCID = ? WHERE ID = ?", params = list(val, id))
  if (changed) .geome_drop(con, id)
  val
}

.geome_fetch_into <- function(con, ids, bcids, cache = new.env()) {
  .geome_ensure_tables(con)
  status <- character(length(ids))
  msg <- rep(NA_character_, length(ids))
  for (i in seq_along(ids)) {
    b <- bcids[i]
    res <- if (is.na(geome_normalize_bcid(b))) {
      simpleError(paste0("'", b, "' is not a GEOME BCID (expected ark:/NNNNN/...)"))
    } else {
      tryCatch(.geome_fetch_chain(b, cache), error = function(e) e)
    }
    ok <- !inherits(res, "error")
    status[i] <- if (ok) "ok" else "failed"
    if (!ok) msg[i] <- conditionMessage(res)
    DBI::dbWithTransaction(con, {
      if (ok) {
        DBI::dbExecute(con, "DELETE FROM geome_records WHERE ID = ?", params = list(ids[i]))
        DBI::dbAppendTable(con, "geome_records", cbind(ID = ids[i], res))
      }
      DBI::dbExecute(con, "INSERT OR REPLACE INTO geome_status VALUES (?, ?, ?, ?, ?)",
                     params = list(ids[i], b, status[i], msg[i], as.integer(Sys.time())))
    })
  }
  out <- data.frame(ID = ids, status = status, message = msg)
  bad <- out$status == "failed"
  if (any(bad)) {
    warning("GEOME fetch failed for ", .lst(paste0(out$ID[bad], " (", out$message[bad], ")")),
            call. = FALSE)
  }
  invisible(out)
}

#' Fetch GEOME metadata for project samples
#'
#' Looks up each sample's GEOME BCID, walks up its parent records (e.g.
#' Tissue, Sample, Event) and adds expedition and project details, then stores
#' everything in the project database for viewing in the app and use at export.
#'
#' @param path Path to the project directory (default = current working directory)
#' @param ids Sample IDs to fetch. Default: every sample with a BCID.
#' @param bcids Optional BCIDs to set for `ids` first (same length as `ids`).
#'   A blank value removes that sample's BCID and its GEOME data.
#' @return Invisibly, a data frame of `ID`, `status`, and `message`.
#' @export
fetch_geome <- function(path = ".", ids = NULL, bcids = NULL) {
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = file.path(path, ".sqlite"))
  on.exit(DBI::dbDisconnect(con))
  .geome_ensure_tables(con)
  samples <- DBI::dbGetQuery(con, "SELECT ID, GEOME_BCID FROM samples")
  if (!is.null(ids)) {
    unknown <- setdiff(ids, samples$ID)
    if (length(unknown)) stop("sample(s) not in this project: ", .lst(unknown), call. = FALSE)
  }
  if (!is.null(bcids)) {
    if (length(bcids) != length(ids)) stop("ids and bcids must be the same length", call. = FALSE)
    for (i in seq_along(ids)) .geome_set_bcid(con, ids[i], bcids[i])
    samples <- DBI::dbGetQuery(con, "SELECT ID, GEOME_BCID FROM samples")
  }
  target <- samples[!is.na(samples$GEOME_BCID) & (is.null(ids) | samples$ID %in% ids), ]
  if (!nrow(target)) {
    message("No samples with a GEOME BCID to fetch")
    return(invisible(data.frame(ID = character(), status = character(), message = character())))
  }
  .geome_fetch_into(con, target$ID, target$GEOME_BCID)
}
