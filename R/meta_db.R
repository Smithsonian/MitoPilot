META_SOURCES <- list(
  GEOME = list(
    col = "GEOME_BCID", label = "GEOME", id_label = "BCID", arg = "bcids",
    normalize = function(x) geome_normalize_bcid(x),
    invalid = function(x) paste0("'", x, "' is not a GEOME BCID (expected ark:/NNNNN/...)"),
    chain = function(ref, cache) .geome_fetch_chain(ref, cache)
  ),
  GBIF = list(
    col = "GBIF_ID", label = "GBIF", id_label = "ID", arg = "gbifs",
    normalize = function(x) gbif_normalize_id(x),
    invalid = function(x) paste0("'", x, "' is not a GBIF occurrence ID (expected digits or an NMNH EZID)"),
    chain = function(ref, cache) .gbif_fetch_chain(ref, cache)
  )
)

.meta_chr <- function(x) {
  if (!is.numeric(x)) return(as.character(x))
  vapply(x, function(v) if (is.na(v)) NA_character_ else format(v, scientific = FALSE, digits = 15),
         character(1), USE.NAMES = FALSE)
}

.meta_flatten <- function(x, level, depth, ref) {
  keep <- vapply(x, function(v) {
    length(v) == 1L && !is.list(v) && !is.na(v) && nzchar(as.character(v))
  }, logical(1))
  x <- x[keep]
  if (!length(x)) return(NULL)
  data.frame(level = level, depth = as.integer(depth), ref = ref,
             field = names(x), value = vapply(x, .meta_chr, ""),
             row.names = NULL)
}

.meta_ensure_tables <- function(con) {
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS meta_records (
    ID TEXT NOT NULL, source TEXT NOT NULL, level TEXT NOT NULL, depth INTEGER NOT NULL,
    ref TEXT, field TEXT NOT NULL, value TEXT, PRIMARY KEY (ID, source, depth, field))")
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS meta_status (
    ID TEXT NOT NULL, source TEXT NOT NULL, ref TEXT, status TEXT NOT NULL, message TEXT,
    fetched_at INTEGER, PRIMARY KEY (ID, source))")
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS meta_export_fields (
    key TEXT NOT NULL, PRIMARY KEY (key))")
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS meta_csv_map (
    concept TEXT NOT NULL, column TEXT, PRIMARY KEY (concept))")
  if (DBI::dbExistsTable(con, "samples")) {
    have <- DBI::dbListFields(con, "samples")
    for (s in META_SOURCES) {
      if (!s$col %in% have) DBI::dbExecute(con, paste0("ALTER TABLE samples ADD COLUMN ", s$col, " TEXT"))
    }
  }
  old <- intersect(c("geome_records", "geome_status", "geome_export_fields"), DBI::dbListTables(con))
  if (length(old)) {
    DBI::dbWithTransaction(con, {
      if ("geome_records" %in% old) {
        DBI::dbExecute(con, "INSERT OR IGNORE INTO meta_records
          SELECT ID, 'GEOME', level, depth, bcid, field, value FROM geome_records")
      }
      if ("geome_status" %in% old) {
        DBI::dbExecute(con, "INSERT OR IGNORE INTO meta_status
          SELECT ID, 'GEOME', bcid, status, message, fetched_at FROM geome_status")
      }
      if ("geome_export_fields" %in% old) {
        DBI::dbExecute(con, "INSERT OR IGNORE INTO meta_export_fields
          SELECT 'geome:' || key FROM geome_export_fields")
      }
      for (t in old) DBI::dbExecute(con, paste("DROP TABLE", t))
    })
  }
  invisible(NULL)
}

.meta_store_value <- function(source, x) {
  raw <- trimws(.meta_chr(x))
  raw[is.na(raw)] <- ""
  norm <- META_SOURCES[[source]]$normalize(raw)
  ifelse(!is.na(norm), norm, ifelse(nzchar(raw), raw, NA_character_))
}

.meta_drop <- function(con, source, ids) {
  for (id in ids) {
    DBI::dbExecute(con, "DELETE FROM meta_records WHERE ID = ? AND source = ?", params = list(id, source))
    DBI::dbExecute(con, "DELETE FROM meta_status WHERE ID = ? AND source = ?", params = list(id, source))
  }
  invisible(NULL)
}

.meta_set_ref <- function(con, source, id, raw) {
  .meta_ensure_tables(con)
  col <- META_SOURCES[[source]]$col
  old <- DBI::dbGetQuery(con, paste0("SELECT ", col, " AS v FROM samples WHERE ID = ?"),
                         params = list(id))$v
  old <- if (length(old)) old[1] else NA_character_
  val <- .meta_store_value(source, raw)
  changed <- xor(is.na(old), is.na(val)) || (!is.na(old) && !is.na(val) && old != val)
  DBI::dbExecute(con, paste0("UPDATE samples SET ", col, " = ? WHERE ID = ?"), params = list(val, id))
  if (changed) .meta_drop(con, source, id)
  val
}

.meta_fetch_into <- function(con, source, ids, refs, cache = new.env()) {
  .meta_ensure_tables(con)
  src <- META_SOURCES[[source]]
  status <- character(length(ids))
  msg <- rep(NA_character_, length(ids))
  for (i in seq_along(ids)) {
    r <- refs[i]
    res <- if (is.na(src$normalize(r))) {
      simpleError(src$invalid(r))
    } else {
      tryCatch(src$chain(r, cache), error = function(e) e)
    }
    ok <- !inherits(res, "error")
    status[i] <- if (ok) "ok" else "failed"
    if (!ok) msg[i] <- conditionMessage(res)
    DBI::dbWithTransaction(con, {
      if (ok) {
        DBI::dbExecute(con, "DELETE FROM meta_records WHERE ID = ? AND source = ?",
                       params = list(ids[i], source))
        if (!is.null(res) && nrow(res)) {
          DBI::dbAppendTable(con, "meta_records", data.frame(
            ID = ids[i], source = source, level = res$level, depth = res$depth,
            ref = res$ref, field = res$field, value = res$value))
        }
      }
      DBI::dbExecute(con, "INSERT OR REPLACE INTO meta_status VALUES (?, ?, ?, ?, ?, ?)",
                     params = list(ids[i], source, r, status[i], msg[i], as.integer(Sys.time())))
    })
  }
  out <- data.frame(ID = ids, status = status, message = msg)
  bad <- out$status == "failed"
  if (any(bad)) {
    warning(src$label, " fetch failed for ",
            .lst(paste0(out$ID[bad], " (", out$message[bad], ")")), call. = FALSE)
  }
  invisible(out)
}

.meta_fetch_project <- function(path, source, ids = NULL, refs = NULL) {
  src <- META_SOURCES[[source]]
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = file.path(path, ".sqlite"))
  on.exit(DBI::dbDisconnect(con))
  .meta_ensure_tables(con)
  q <- paste0("SELECT ID, ", src$col, " AS ref FROM samples")
  samples <- DBI::dbGetQuery(con, q)
  if (!is.null(ids)) {
    unknown <- setdiff(ids, samples$ID)
    if (length(unknown)) stop("sample(s) not in this project: ", .lst(unknown), call. = FALSE)
  }
  if (!is.null(refs)) {
    if (length(refs) != length(ids)) stop("ids and ", src$arg, " must be the same length", call. = FALSE)
    for (i in seq_along(ids)) .meta_set_ref(con, source, ids[i], refs[i])
    samples <- DBI::dbGetQuery(con, q)
  }
  target <- samples[!is.na(samples$ref) & (is.null(ids) | samples$ID %in% ids), ]
  if (!nrow(target)) {
    message("No samples with a ", src$label, " ", src$id_label, " to fetch")
    return(invisible(data.frame(ID = character(), status = character(), message = character())))
  }
  .meta_fetch_into(con, source, target$ID, target$ref)
}

.meta_take_cols <- function(mapping, cols) {
  for (src in names(cols)) {
    col <- cols[[src]]
    std <- META_SOURCES[[src]]$col
    if (col %in% colnames(mapping)) {
      mapping[[std]] <- .meta_store_value(src, mapping[[col]])
      if (col != std) mapping[[col]] <- NULL
    }
  }
  mapping
}

.meta_fetch_new <- function(con, mapping, fetch) {
  .meta_ensure_tables(con)
  for (src in names(fetch)) {
    col <- META_SOURCES[[src]]$col
    if (!isTRUE(fetch[[src]]) || !col %in% colnames(mapping)) next
    has <- !is.na(mapping[[col]])
    if (any(has)) .meta_fetch_into(con, src, mapping$ID[has], mapping[[col]][has])
  }
  invisible(NULL)
}

.meta_sync_changed <- function(con, mapping, old, fetch) {
  for (src in names(fetch)) {
    col <- META_SOURCES[[src]]$col
    if (!col %in% colnames(mapping)) next
    .meta_ensure_tables(con)
    new <- mapping[[col]]
    prev <- if (col %in% colnames(old)) {
      unname(stats::setNames(old[[col]], old$ID)[mapping$ID])
    } else {
      rep(NA_character_, length(new))
    }
    changed <- xor(is.na(new), is.na(prev)) | (!is.na(new) & !is.na(prev) & new != prev)
    if (any(changed)) .meta_drop(con, src, mapping$ID[changed])
    refetch <- changed & !is.na(new)
    if (isTRUE(fetch[[src]]) && any(refetch)) {
      .meta_fetch_into(con, src, mapping$ID[refetch], new[refetch])
    }
  }
  invisible(NULL)
}
