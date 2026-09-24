.gbif_occ <- function(recs, field) {
  .geome_pick(recs[recs$level == "Occurrence", , drop = FALSE], field)
}

.gbif_bad_coord_issues <- c("ZERO_COORDINATE", "COORDINATE_INVALID", "COORDINATE_OUT_OF_RANGE")

.gbif_passthrough <- function(field, label, lower = FALSE) {
  list(label = label, sources = field, fn = function(recs) {
    v <- .gbif_occ(recs, field)
    if (lower) tolower(v) else v
  })
}

GBIF_COMBOS <- list(
  lat_lon = list(
    label = "lat_lon", sources = c("decimalLatitude", "decimalLongitude"),
    fn = function(recs) {
      iss <- strsplit(.gbif_occ(recs, "issues") %|NA|% "", ",", fixed = TRUE)[[1]]
      if (any(iss %in% .gbif_bad_coord_issues)) return(NA_character_)
      la <- .geome_coord(.gbif_occ(recs, "decimalLatitude"), 90)
      lo <- .geome_coord(.gbif_occ(recs, "decimalLongitude"), 180)
      if (is.null(la) || is.null(lo)) return(NA_character_)
      paste(la$txt, if (la$neg) "S" else "N", lo$txt, if (lo$neg) "W" else "E")
    }
  ),
  collection_date = list(
    label = "collection_date", sources = c("year", "month", "day", "eventDate"),
    fn = function(recs) {
      out <- .meta_ymd(.gbif_occ(recs, "year"), .gbif_occ(recs, "month"), .gbif_occ(recs, "day"))
      if (!is.na(out)) return(out)
      ed <- .gbif_occ(recs, "eventDate")
      if (is.na(ed) || grepl("/", ed, fixed = TRUE)) return(NA_character_)
      hit <- regmatches(ed, regexec("^([0-9]{4})(-([0-9]{2}))?(-([0-9]{2}))?([T ].*)?$", ed))[[1]]
      if (!length(hit)) return(NA_character_)
      .meta_ymd(hit[2], hit[4], hit[6])
    }
  ),
  geo_loc_name = list(
    label = "geo_loc_name", sources = c("country", "stateProvince", "locality"),
    fn = function(recs) GEOME_COMBOS$geo_loc_name$fn(recs[recs$level == "Occurrence", , drop = FALSE])
  ),
  specimen_voucher = list(
    label = "specimen_voucher", sources = c("institutionCode", "collectionCode", "catalogNumber"),
    fn = function(recs) {
      cat <- .gbif_occ(recs, "catalogNumber")
      if (is.na(cat) || grepl("^[a-z]+://|ark:/", cat, ignore.case = TRUE)) return(NA_character_)
      inst <- .gbif_occ(recs, "institutionCode")
      coll <- .gbif_occ(recs, "collectionCode")
      if (is.na(inst)) return(cat)
      if (is.na(coll)) paste0(inst, ":", cat) else paste(inst, coll, cat, sep = ":")
    }
  ),
  collected_by = .gbif_passthrough("recordedBy", "collected_by"),
  identified_by = .gbif_passthrough("identifiedBy", "identified_by"),
  sex = .gbif_passthrough("sex", "sex", lower = TRUE),
  dev_stage = .gbif_passthrough("lifeStage", "dev_stage", lower = TRUE)
)
