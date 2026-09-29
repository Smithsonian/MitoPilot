.NCBI_MISSING <- c("missing", "not collected", "not applicable", "not provided",
                   "restricted access", "unknown", "na", "n/a", "none", "-",
                   "not recorded", "not available", "unspecified")

.ncbi_blank <- function(v) {
  if (is.na(v) || tolower(trimws(sub(":.*", "", v))) %in% .NCBI_MISSING) NA_character_ else trimws(v)
}

.ncbi_val <- function(recs, field, level = "BioSample") {
  .ncbi_blank(.geome_pick(recs[recs$level == level, , drop = FALSE], field))
}

.ncbi_passthrough <- function(field, lower = FALSE) {
  list(label = field, sources = field, fn = function(recs) {
    v <- .ncbi_val(recs, field)
    if (lower) tolower(v) else v
  })
}

.ncbi_lat_lon <- function(recs) {
  v <- .ncbi_val(recs, "lat_lon")
  if (is.na(v)) return(v)
  h <- regmatches(v, regexec("^([0-9.]+)\\s*([NSns])[ ,;]+([0-9.]+)\\s*([EWew])$", v))[[1]]
  if (length(h)) {
    if (is.null(.geome_coord(h[2], 90)) || is.null(.geome_coord(h[4], 180))) return(NA_character_)
    return(paste(h[2], toupper(h[3]), h[4], toupper(h[5])))
  }
  d <- regmatches(v, regexec("^([+-]?[0-9.]+)\\s*[ ,;]\\s*([+-]?[0-9.]+)$", v))[[1]]
  if (!length(d)) return(NA_character_)
  la <- .geome_coord(d[2], 90)
  lo <- .geome_coord(d[3], 180)
  if (is.null(la) || is.null(lo)) return(NA_character_)
  paste(la$txt, if (la$neg) "S" else "N", lo$txt, if (lo$neg) "W" else "E")
}

# biosample/bioproject are not GenBank defline modifiers, so their chips insert plain tokens
NCBI_COMBOS <- list(
  lat_lon = list(label = "lat_lon", sources = "lat_lon", fn = .ncbi_lat_lon),
  collection_date = .ncbi_passthrough("collection_date"),
  geo_loc_name = .ncbi_passthrough("geo_loc_name"),
  specimen_voucher = list(label = "specimen_voucher",
                          sources = c("specimen_voucher", "genbankSpecimenVoucher"),
                          fn = function(recs) .ncbi_val(recs, "specimen_voucher") %|NA|%
                            .ncbi_val(recs, "genbankSpecimenVoucher")),
  collected_by = .ncbi_passthrough("collected_by"),
  identified_by = .ncbi_passthrough("identified_by"),
  sex = .ncbi_passthrough("sex", lower = TRUE),
  dev_stage = .ncbi_passthrough("dev_stage", lower = TRUE),
  biosample = list(label = "biosample", sources = "accession", plain = TRUE,
                   fn = function(recs) .ncbi_val(recs, "accession")),
  bioproject = list(label = "bioproject", sources = "accession", plain = TRUE,
                    fn = function(recs) .ncbi_val(recs, "accession", level = "BioProject"))
)

.ncbi_concept_value <- function(concept, recs) {
  geo <- NCBI_COMBOS$geo_loc_name$fn(recs)
  switch(concept,
    coordinates = NCBI_COMBOS$lat_lon$fn(recs),
    collection_date = NCBI_COMBOS$collection_date$fn(recs),
    country = if (is.na(geo)) NA_character_ else .ncbi_blank(sub(":.*", "", geo)),
    locality = if (is.na(geo) || !grepl(":", geo, fixed = TRUE)) NA_character_ else
      .ncbi_blank(sub("^[^:]*:", "", geo)),
    voucher = NCBI_COMBOS$specimen_voucher$fn(recs),
    collector = NCBI_COMBOS$collected_by$fn(recs),
    sex = NCBI_COMBOS$sex$fn(recs),
    dev_stage = NCBI_COMBOS$dev_stage$fn(recs),
    taxon = .ncbi_val(recs, "organism")
  )
}
