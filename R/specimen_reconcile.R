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
