.spec_env <- new.env()

.spec_norm_name <- function(x) {
  x <- iconv(as.character(x), "UTF-8", "ASCII//TRANSLIT", sub = "")
  x <- gsub("[^a-z0-9]+", " ", tolower(x))
  trimws(x)
}

.spec_countries <- function() {
  if (is.null(.spec_env$countries)) {
    x <- utils::read.csv(app_sys("extdata", "countries.csv"), colClasses = "character",
                         na.strings = character())
    .spec_env$countries <- stats::setNames(x$iso2, x$name)
  }
  .spec_env$countries
}

.spec_country_iso2 <- function(x) {
  if (length(x) != 1L || is.na(x)) return(NA_character_)
  key <- .spec_norm_name(sub(":.*", "", x))
  if (!nzchar(key)) return(NA_character_)
  unname(.spec_countries()[key])
}

.spec_parse_coords <- function(x) {
  if (length(x) != 1L || is.na(x) || !nzchar(trimws(x))) return(NULL)
  x <- trimws(x)
  h <- regmatches(x, regexec("^([0-9.]+)\\s*([NSns])[ ,;]+([0-9.]+)\\s*([EWew])$", x))[[1]]
  if (length(h)) {
    la <- suppressWarnings(as.numeric(h[2])) * if (toupper(h[3]) == "S") -1 else 1
    lo <- suppressWarnings(as.numeric(h[4])) * if (toupper(h[5]) == "W") -1 else 1
  } else {
    d <- regmatches(x, regexec("^([+-]?[0-9.]+)\\s*[ ,;]\\s*([+-]?[0-9.]+)$", x))[[1]]
    if (!length(d)) return(NULL)
    la <- suppressWarnings(as.numeric(d[2]))
    lo <- suppressWarnings(as.numeric(d[3]))
  }
  if (is.na(la) || is.na(lo) || abs(la) > 90 || abs(lo) > 180) return(NULL)
  c(la, lo)
}

.spec_ymd <- function(y, m, d) {
  m <- suppressWarnings(as.integer(m))
  d <- suppressWarnings(as.integer(d))
  if (is.na(m) || m < 1 || m > 12) m <- NA_integer_
  if (is.na(m) || is.na(d) || d < 1 || d > 31) d <- NA_integer_
  c(y = as.integer(y), m = m, d = d)
}

.spec_parse_date <- function(x) {
  if (length(x) != 1L || is.na(x) || !nzchar(trimws(x))) return(NULL)
  x <- trimws(x)
  if (grepl("/", x, fixed = TRUE)) return(NULL)
  iso <- regmatches(x, regexec("^([0-9]{4})(-([0-9]{1,2}))?(-([0-9]{1,2}))?([T ].*)?$", x))[[1]]
  if (length(iso)) return(.spec_ymd(iso[2], iso[4], iso[6]))
  gb <- regmatches(x, regexec("^(([0-9]{1,2})-)?([A-Za-z]{3})-([0-9]{4})$", x))[[1]]
  if (length(gb)) {
    m <- match(tolower(gb[4]), tolower(month.abb))
    if (is.na(m)) return(NULL)
    return(.spec_ymd(gb[5], m, gb[3]))
  }
  NULL
}

.spec_same_date <- function(a, b) {
  if (a[["y"]] != b[["y"]]) return(FALSE)
  if (is.na(a[["m"]]) || is.na(b[["m"]])) return(TRUE)
  if (a[["m"]] != b[["m"]]) return(FALSE)
  if (is.na(a[["d"]]) || is.na(b[["d"]])) return(TRUE)
  a[["d"]] == b[["d"]]
}

.spec_norm_voucher <- function(x) {
  x <- sub(".*:", "", trimws(x))
  x <- sub("^([A-Za-z]+\\s+)+", "", x)
  tolower(gsub("\\s+", "", x))
}

.spec_binomial <- function(x) {
  paste(utils::head(strsplit(tolower(trimws(x)), "\\s+")[[1]], 2), collapse = " ")
}

.spec_squash <- function(x) gsub("\\s+", " ", tolower(trimws(x)))

.spec_compare <- function(concept, a, b) {
  switch(concept,
    coordinates = {
      pa <- .spec_parse_coords(a)
      pb <- .spec_parse_coords(b)
      if (is.null(pa) || is.null(pb)) "note"
      else if (all(abs(pa - pb) <= 0.01 + 1e-9)) "agree" else "conflict"
    },
    collection_date = {
      pa <- .spec_parse_date(a)
      pb <- .spec_parse_date(b)
      if (is.null(pa) || is.null(pb)) "note"
      else if (.spec_same_date(pa, pb)) "agree" else "conflict"
    },
    country = {
      ca <- .spec_country_iso2(a)
      cb <- .spec_country_iso2(b)
      if (!is.na(ca) && !is.na(cb)) {
        if (ca == cb) "agree" else "conflict"
      } else if (.spec_norm_name(sub(":.*", "", a)) == .spec_norm_name(sub(":.*", "", b))) {
        "agree"
      } else {
        "note"
      }
    },
    voucher = if (.spec_norm_voucher(a) == .spec_norm_voucher(b)) "agree" else "conflict",
    taxon = if (.spec_binomial(a) == .spec_binomial(b)) "agree" else "conflict",
    locality = ,
    collector = if (.spec_squash(a) == .spec_squash(b)) "agree" else "note",
    sex = ,
    dev_stage = if (tolower(trimws(a)) == tolower(trimws(b))) "agree" else "note"
  )
}

.spec_status <- function(concept, vals) {
  vals <- vals[!is.na(vals) & nzchar(trimws(vals))]
  if (!length(vals)) return(NA_character_)
  if (length(vals) == 1L) return("single")
  pairs <- utils::combn(length(vals), 2)
  st <- apply(pairs, 2, function(p) .spec_compare(concept, vals[p[1]], vals[p[2]]))
  if ("conflict" %in% st) "conflict" else if ("note" %in% st) "note" else "agree"
}

SPECIMEN_CONCEPTS <- c("coordinates", "collection_date", "country", "locality", "voucher",
                       "collector", "sex", "dev_stage", "taxon")

.SPEC_CSV_NAMES <- list(
  coordinates = "lat_lon",
  collection_date = c("collection_date", "date", "eventDate"),
  country = c("country", "geo_loc_name"),
  locality = "locality",
  voucher = c("specimen_voucher", "voucher", "catalogNumber"),
  collector = c("collected_by", "collector", "recordedBy"),
  sex = "sex",
  dev_stage = c("dev_stage", "life_stage", "lifeStage"),
  taxon = "Taxon"
)
.SPEC_LAT <- c("lat", "latitude", "decimalLatitude")
.SPEC_LON <- c("lon", "long", "longitude", "decimalLongitude")

.spec_detect_csv <- function(cols, overrides = character()) {
  pick <- function(cands) {
    hit <- cols[match(tolower(cands), tolower(cols))]
    hit <- hit[!is.na(hit)]
    if (length(hit)) hit[1] else character()
  }
  out <- lapply(.SPEC_CSV_NAMES, pick)
  if (!length(out$coordinates)) {
    la <- pick(.SPEC_LAT)
    lo <- pick(.SPEC_LON)
    if (length(la) && length(lo)) out$coordinates <- c(la, lo)
  }
  out$taxon <- if ("Taxon" %in% cols) "Taxon" else character()
  for (k in intersect(names(overrides), setdiff(names(out), "taxon"))) {
    v <- overrides[[k]]
    out[[k]] <- if (is.na(v) || !nzchar(v)) character() else strsplit(v, ",", fixed = TRUE)[[1]]
  }
  out
}

specimen_csv_columns <- function(con) {
  .meta_ensure_tables(con)
  m <- DBI::dbGetQuery(con, "SELECT concept, column FROM meta_csv_map")
  .spec_detect_csv(DBI::dbListFields(con, "samples"), stats::setNames(m$column, m$concept))
}

.spec_csv_value <- function(row, cols) {
  if (!length(cols) || !all(cols %in% names(row))) return(NA_character_)
  v <- trimws(vapply(cols, function(cc) .meta_chr(row[[cc]])[1], character(1)))
  if (any(is.na(v) | !nzchar(v))) return(NA_character_)
  paste(v, collapse = ", ")
}

.spec_source_value <- function(concept, source, recs) {
  if (!nrow(recs)) return(NA_character_)
  combos <- .meta_combos(source)
  pick <- if (source == "GBIF") .gbif_occ else .geome_pick
  switch(concept,
    coordinates = combos$lat_lon$fn(recs),
    collection_date = combos$collection_date$fn(recs),
    country = pick(recs, if (source == "GBIF") "countryCode" else "country"),
    locality = pick(recs, "locality"),
    voucher = combos$specimen_voucher$fn(recs),
    collector = combos$collected_by$fn(recs),
    sex = combos$sex$fn(recs),
    dev_stage = combos$dev_stage$fn(recs),
    taxon = pick(recs, "scientificName")
  )
}

specimen_conflicts <- function(con, ids = NULL) {
  .meta_ensure_tables(con)
  s <- DBI::dbReadTable(con, "samples")
  if (!is.null(ids)) s <- s[s$ID %in% ids, , drop = FALSE]
  cols <- specimen_csv_columns(con)
  recs <- DBI::dbGetQuery(con, "SELECT ID, source, level, depth, field, value FROM meta_records")
  by_id <- split(recs, recs$ID)
  empty <- recs[0, ]
  nk <- length(SPECIMEN_CONCEPTS)
  n <- nrow(s) * nk
  csv_v <- geome_v <- gbif_v <- status <- rep(NA_character_, n)
  j <- 0L
  for (i in seq_len(nrow(s))) {
    row <- s[i, , drop = FALSE]
    r <- by_id[[s$ID[i]]] %||% empty
    g <- r[r$source == "GEOME", , drop = FALSE]
    b <- r[r$source == "GBIF", , drop = FALSE]
    for (k in SPECIMEN_CONCEPTS) {
      j <- j + 1L
      v <- c(.spec_csv_value(row, cols[[k]]), .spec_source_value(k, "GEOME", g),
             .spec_source_value(k, "GBIF", b))
      csv_v[j] <- v[1]
      geome_v[j] <- v[2]
      gbif_v[j] <- v[3]
      status[j] <- .spec_status(k, v)
    }
  }
  csv_col <- vapply(SPECIMEN_CONCEPTS, function(k) {
    if (length(cols[[k]])) paste(cols[[k]], collapse = " + ") else NA_character_
  }, character(1), USE.NAMES = FALSE)
  data.frame(ID = rep(s$ID, each = nk), concept = rep(SPECIMEN_CONCEPTS, nrow(s)),
             csv_column = rep(csv_col, nrow(s)), csv_value = csv_v, geome_value = geome_v,
             gbif_value = gbif_v, status = status)
}

.spec_set_csv_map <- function(con, map) {
  if (!length(map)) return(invisible(NULL))
  .meta_ensure_tables(con)
  ok <- setdiff(SPECIMEN_CONCEPTS, "taxon")
  nm <- names(map) %||% rep("", length(map))
  bad <- nm[!nm %in% ok]
  if (length(bad)) {
    stop("unknown concept(s): ", .lst(ifelse(nzchar(bad), bad, "<unnamed>")),
         "; use ", .lst(ok), call. = FALSE)
  }
  cols <- DBI::dbListFields(con, "samples")
  for (k in nm) {
    v <- map[[k]]
    if ((length(v) == 1L && is.na(v)) || (length(v) == 1L && !nzchar(v))) next
    if (length(v) > (if (k == "coordinates") 2L else 1L)) stop(k, ": too many columns", call. = FALSE)
    miss <- setdiff(v, cols)
    if (length(miss)) stop(k, ": column(s) not in the samples table: ", .lst(miss), call. = FALSE)
  }
  for (k in nm) {
    v <- map[[k]]
    if (length(v) == 1L && is.na(v)) {
      DBI::dbExecute(con, "DELETE FROM meta_csv_map WHERE concept = ?", params = list(k))
    } else {
      DBI::dbExecute(con, "INSERT OR REPLACE INTO meta_csv_map VALUES (?, ?)",
                     params = list(k, paste(v, collapse = ",")))
    }
  }
  invisible(NULL)
}

#' Choose which mapping-file columns are compared with GEOME and GBIF
#'
#' MitoPilot compares specimen details (coordinates, collection date, country,
#' locality, voucher, collector, sex, and life stage) between your mapping file,
#' GEOME, and GBIF, and flags disagreements. It finds the mapping-file columns
#' by name; use this function when a column has a name it does not recognize,
#' or to stop comparing one. The Taxon column is always compared.
#'
#' @param path Path to the project directory (default = current working directory)
#' @param ... Named `concept = column` pairs. Concepts: `coordinates`,
#'   `collection_date`, `country`, `locality`, `voucher`, `collector`, `sex`,
#'   `dev_stage`. `coordinates` takes one combined column or two columns
#'   (latitude, then longitude). `NA` returns a concept to automatic detection;
#'   `""` stops comparing it.
#' @return Invisibly, a data frame of each concept and the column(s) now used
#'   (`NA` when none).
#' @export
set_metadata_columns <- function(path = ".", ...) {
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = file.path(path, ".sqlite"))
  on.exit(DBI::dbDisconnect(con))
  .spec_set_csv_map(con, list(...))
  cols <- specimen_csv_columns(con)
  invisible(data.frame(
    concept = names(cols),
    column = vapply(cols, function(x) if (length(x)) paste(x, collapse = " + ") else NA_character_,
                    character(1), USE.NAMES = FALSE)
  ))
}
