# On-screen metadata columns shared by the Assemble, Annotate, and Export
# tables. Separate from meta_export_fields, which drives exported files.

META_VIEW_SOURCES <- c("Map file", "GEOME", "GBIF")
# Columns the metadata block is placed before, first one present wins
META_VIEW_ANCHORS <- c("time_stamp", "assemble_notes", "annotate_notes", "export_time_stamp",
                       "export_group", "output", "view")

.meta_view_ensure <- function(con) {
  .meta_ensure_tables(con)
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS meta_view_fields (
    key TEXT NOT NULL, shown INTEGER NOT NULL, PRIMARY KEY (key))")
  invisible(NULL)
}

.meta_view_stored <- function(con) {
  .meta_view_ensure(con)
  DBI::dbGetQuery(con, "SELECT key, shown FROM meta_view_fields")
}

.meta_view_map_cols <- function(con) {
  cols <- tryCatch(DBI::dbListFields(con, "samples"), error = function(e) character(0))
  export_metadata_cols(cols, character(0))
}

#' Every metadata field with data, in display order, with its shown state
#'
#' @param con database connection
#' @return data.frame: source, key, level, field, n_samples, example, col, shown
#' @noRd
meta_view_fields <- function(con) {
  st <- .meta_view_stored(con)
  mc <- .meta_view_map_cols(con)
  map <- if (length(mc)) {
    s <- DBI::dbGetQuery(con, paste0("SELECT ",
      paste(DBI::dbQuoteIdentifier(con, mc), collapse = ", "), " FROM samples"))
    vals <- lapply(s, function(v) { v <- trimws(as.character(v)); v[!is.na(v) & nzchar(v)] })
    data.frame(source = "Map file", key = paste0("map:", mc), level = "Map file", field = mc,
               n_samples = vapply(vals, length, integer(1)),
               example = vapply(vals, function(v) if (length(v)) v[1] else NA_character_, ""),
               col = paste0("mv_map_", gsub("[^A-Za-z0-9_]", "_", mc)))
  }
  specimen <- lapply(c("GEOME", "GBIF"), function(src) {
    s <- meta_field_summary(con, src)
    if (!nrow(s)) return(NULL)
    data.frame(source = src, key = s$key,
               level = ifelse(s$kind == "combo", "GenBank-ready", s$level),
               field = ifelse(s$kind == "combo", sub(".*:", "", s$key), s$field),
               n_samples = s$n_samples, example = s$example, col = paste0("mv_", s$col))
  })
  out <- do.call(rbind, c(list(map), specimen))
  if (is.null(out)) {
    return(data.frame(source = character(), key = character(), level = character(),
                      field = character(), n_samples = integer(), example = character(),
                      col = character(), shown = logical()))
  }
  out <- out[out$n_samples > 0, ]
  out$col <- make.unique(out$col, sep = "_")
  saved <- stats::setNames(st$shown == 1, st$key)[out$key]
  out$shown <- ifelse(is.na(saved), out$source == "Map file", saved)
  rownames(out) <- NULL
  out
}

#' Save which metadata fields are shown, and the wrap setting
#'
#' Map file fields are stored when hidden, GEOME/GBIF fields when shown, so a
#' new map file column appears and a newly fetched field stays off.
#' @noRd
meta_view_save <- function(con, fields, shown_keys, wrap) {
  .meta_view_ensure(con)
  is_map <- fields$source == "Map file"
  on <- fields$key %in% shown_keys
  keep <- (is_map & !on) | (!is_map & on)
  rows <- data.frame(key = c(fields$key[keep], "opt:wrap"),
                     shown = c(as.integer(on[keep]), as.integer(isTRUE(wrap))))
  DBI::dbWithTransaction(con, {
    DBI::dbExecute(con, "DELETE FROM meta_view_fields")
    DBI::dbAppendTable(con, "meta_view_fields", rows)
  })
  invisible(NULL)
}

meta_view_wrap <- function(con) {
  st <- .meta_view_stored(con)
  isTRUE(st$shown[st$key == "opt:wrap"][1] == 1)
}

#' Add the shown metadata columns to a table's data, before its notes and
#' action columns
#' @noRd
meta_view_join <- function(dat, con, fields = meta_view_fields(con)) {
  f <- fields[fields$shown, ]
  if (!nrow(f)) return(dat)
  ids <- unique(as.character(dat$ID))
  vals <- data.frame(ID = ids)
  m <- f[f$source == "Map file", ]
  if (nrow(m)) {
    s <- DBI::dbGetQuery(con, paste0("SELECT ID, ",
      paste(DBI::dbQuoteIdentifier(con, m$field), collapse = ", "), " FROM samples"))
    s <- s[match(ids, s$ID), m$field, drop = FALSE]
    for (i in seq_len(nrow(m))) vals[[m$col[i]]] <- as.character(s[[i]])
  }
  g <- f[f$source != "Map file", ]
  if (nrow(g)) {
    mv <- meta_export_cols(con, ids, keys = g$key)
    for (i in seq_len(nrow(g))) vals[[g$col[i]]] <- mv[[.meta_key_col(g$key[i])]]
  }
  vals[f$col] <- lapply(vals[f$col], function(x) ifelse(is.na(x), "", x))
  out <- dplyr::left_join(dat[setdiff(names(dat), f$col)], vals, by = "ID")
  anchor <- intersect(names(out), META_VIEW_ANCHORS)
  if (length(anchor)) {
    first <- names(out)[min(match(anchor, names(out)))]
    out <- dplyr::relocate(out, dplyr::all_of(f$col), .before = dplyr::all_of(first))
  }
  out
}

#' Source logo drawn by CSS (mp-meta-logo-geome / -gbif), NULL for map file
#' @noRd
meta_view_logo <- function(src) {
  if (!src %in% c("GEOME", "GBIF")) return(NULL)
  htmltools::tags$span(class = paste0("mp-meta-logo mp-meta-logo-", tolower(src)),
                       role = "img", `aria-label` = src)
}

#' colDefs for the shown metadata columns
#' @noRd
meta_view_col_defs <- function(fields, wrap = FALSE) {
  f <- fields[fields$shown, ]
  cls <- paste(c("mp-grp-Metadata", "mp-meta-col", if (wrap) "mp-meta-wrap"), collapse = " ")
  stats::setNames(lapply(seq_len(nrow(f)), function(i) {
    src <- f$source[i]
    path <- paste(c(src, if (src != "Map file") f$level[i], f$field[i]), collapse = " > ")
    header <- htmltools::tags$span(class = "mp-th-tip", title = path, meta_view_logo(src), f$field[i])
    reactable::colDef(show = TRUE, name = f$field[i], header = header,
                      class = cls, headerClass = "mp-grp-Metadata",
                      html = TRUE, cell = rt_longtext(), minWidth = 140, resizable = TRUE)
  }), f$col)
}

#' Metadata colDefs for the columns present in a table's data
#' @noRd
meta_view_table_defs <- function(con, dat) {
  f <- meta_view_fields(con)
  meta_view_col_defs(f[f$col %in% names(dat), ], meta_view_wrap(con))
}
