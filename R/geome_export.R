.geome_pick <- function(recs, field) {
  hit <- recs[recs$field == field & !is.na(recs$value) & nzchar(recs$value), ]
  if (!nrow(hit)) NA_character_ else hit$value[which.min(hit$depth)]
}

.geome_coord <- function(x, max) {
  if (is.na(x)) return(NULL)
  s <- sub("^\\+", "", trimws(x))
  n <- suppressWarnings(as.numeric(s))
  if (is.na(n) || abs(n) > max) return(NULL)
  list(neg = n < 0, txt = sub("^-", "", s))
}

.geome_passthrough <- function(field, label) {
  list(label = label, sources = field, fn = function(recs) .geome_pick(recs, field))
}

GEOME_COMBOS <- list(
  lat_lon = list(
    label = "lat_lon", sources = c("decimalLatitude", "decimalLongitude"),
    fn = function(recs) {
      la <- .geome_coord(.geome_pick(recs, "decimalLatitude"), 90)
      lo <- .geome_coord(.geome_pick(recs, "decimalLongitude"), 180)
      if (is.null(la) || is.null(lo)) return(NA_character_)
      paste(la$txt, if (la$neg) "S" else "N", lo$txt, if (lo$neg) "W" else "E")
    }
  ),
  collection_date = list(
    label = "collection_date", sources = c("yearCollected", "monthCollected", "dayCollected"),
    fn = function(recs) {
      y <- .geome_pick(recs, "yearCollected")
      if (is.na(y) || !grepl("^[0-9]{4}$", y)) return(NA_character_)
      m <- suppressWarnings(as.integer(.geome_pick(recs, "monthCollected")))
      d <- suppressWarnings(as.integer(.geome_pick(recs, "dayCollected")))
      if (is.na(m) || m < 1 || m > 12) return(y)
      if (is.na(d) || d < 1 || d > 31) return(sprintf("%s-%02d", y, m))
      sprintf("%s-%02d-%02d", y, m, d)
    }
  ),
  geo_loc_name = list(
    label = "geo_loc_name", sources = c("country", "stateProvince", "locality"),
    fn = function(recs) {
      co <- .geome_pick(recs, "country")
      if (is.na(co)) return(NA_character_)
      parts <- c(.geome_pick(recs, "stateProvince"), .geome_pick(recs, "locality"))
      parts <- unique(parts[!is.na(parts) & parts != co])
      if (length(parts)) paste0(co, ": ", paste(parts, collapse = ", ")) else co
    }
  ),
  specimen_voucher = list(
    label = "specimen_voucher", sources = c("institutionCode", "catalogNumber"),
    fn = function(recs) {
      cat <- .geome_pick(recs, "catalogNumber")
      if (is.na(cat)) return(NA_character_)
      inst <- .geome_pick(recs, "institutionCode")
      if (is.na(inst)) cat else paste0(inst, ":", cat)
    }
  ),
  collected_by = .geome_passthrough("collectorList", "collected_by"),
  tissue_type = .geome_passthrough("tissueType", "tissue_type"),
  sex = .geome_passthrough("sex", "sex"),
  dev_stage = .geome_passthrough("lifeStage", "dev_stage")
)

.geome_key_col <- function(key) {
  p <- strsplit(key, ":", fixed = TRUE)[[1]]
  nm <- if (p[1] == "combo") p[2] else paste(p[2], paste(p[-(1:2)], collapse = ":"), sep = "_")
  paste0("geome_", gsub("[^A-Za-z0-9_]", "_", nm))
}

.geome_key_value <- function(recs, key) {
  p <- strsplit(key, ":", fixed = TRUE)[[1]]
  if (p[1] == "combo") {
    spec <- GEOME_COMBOS[[p[2]]]
    return(if (is.null(spec)) NA_character_ else spec$fn(recs))
  }
  field <- paste(p[-(1:2)], collapse = ":")
  v <- recs$value[recs$level == p[2] & recs$field == field]
  if (length(v)) v[1] else NA_character_
}

geome_export_cols <- function(con, ids = NULL) {
  if (!DBI::dbExistsTable(con, "geome_export_fields")) return(NULL)
  keys <- DBI::dbGetQuery(con, "SELECT key FROM geome_export_fields")$key
  if (!length(keys)) return(NULL)
  recs <- DBI::dbGetQuery(con, "SELECT ID, level, depth, field, value FROM geome_records")
  ids <- ids %||% unique(recs$ID)
  out <- data.frame(ID = ids)
  for (k in keys) {
    out[[.geome_key_col(k)]] <- vapply(ids, function(i) {
      .geome_key_value(recs[recs$ID == i, ], k)
    }, character(1), USE.NAMES = FALSE)
  }
  out
}

.geome_join <- function(dat, con) {
  g <- geome_export_cols(con, ids = unique(dat$ID))
  if (is.null(g)) return(dat)
  out <- dplyr::left_join(dat, g, by = "ID")
  cols <- setdiff(names(g), "ID")
  out[cols] <- lapply(out[cols], function(x) ifelse(is.na(x), "", x))
  out
}

geome_field_summary <- function(con) {
  .geome_ensure_tables(con)
  recs <- DBI::dbGetQuery(con, "SELECT ID, level, depth, field, value FROM geome_records")
  sel <- DBI::dbGetQuery(con, "SELECT key FROM geome_export_fields")$key
  ids <- unique(recs$ID)
  combos <- do.call(rbind, lapply(names(GEOME_COMBOS), function(nm) {
    vals <- vapply(ids, function(i) GEOME_COMBOS[[nm]]$fn(recs[recs$ID == i, ]), character(1))
    data.frame(key = paste0("combo:", nm), kind = "combo", level = NA_character_,
               field = paste(GEOME_COMBOS[[nm]]$sources, collapse = " + "),
               n_samples = sum(!is.na(vals)),
               example = if (any(!is.na(vals))) vals[!is.na(vals)][1] else NA_character_)
  }))
  raw <- if (nrow(recs)) {
    g <- unique(recs[, c("level", "depth", "field")])
    g <- g[order(-g$depth, g$level, g$field), ]
    data.frame(
      key = paste0("raw:", g$level, ":", g$field), kind = "raw", level = g$level,
      field = g$field,
      n_samples = vapply(seq_len(nrow(g)), function(j) {
        length(unique(recs$ID[recs$level == g$level[j] & recs$field == g$field[j]]))
      }, integer(1)),
      example = vapply(seq_len(nrow(g)), function(j) {
        recs$value[recs$level == g$level[j] & recs$field == g$field[j]][1]
      }, character(1))
    )
  }
  out <- rbind(combos, raw)
  out <- unique(out[, c("key", "kind", "level", "field", "n_samples", "example")])
  out$n_samples <- as.integer(out$n_samples)
  out$col <- vapply(out$key, .geome_key_col, character(1), USE.NAMES = FALSE)
  out$selected <- out$key %in% sel
  rownames(out) <- NULL
  out
}
