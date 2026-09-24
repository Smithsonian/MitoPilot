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

