# Builds inst/extdata/countries.csv from GBIF's country enumeration plus common
# aliases. Run from the package root: Rscript data-raw/build_countries.R
devtools::load_all(quiet = TRUE)
x <- jsonlite::fromJSON("https://api.gbif.org/v1/enumeration/country")
aliases <- c(
  "USA" = "US", "United States" = "US", "U.S.A." = "US",
  "UK" = "GB", "United Kingdom" = "GB", "Great Britain" = "GB", "England" = "GB",
  "Scotland" = "GB", "Wales" = "GB",
  "Russia" = "RU", "South Korea" = "KR", "North Korea" = "KP", "Laos" = "LA",
  "Ivory Coast" = "CI", "Cote d'Ivoire" = "CI", "Czech Republic" = "CZ",
  "Cape Verde" = "CV", "Swaziland" = "SZ", "Macedonia" = "MK", "Turkey" = "TR",
  "Taiwan" = "TW", "Vietnam" = "VN", "Bolivia" = "BO", "Iran" = "IR", "Syria" = "SY",
  "Tanzania" = "TZ", "Venezuela" = "VE", "Moldova" = "MD", "Micronesia" = "FM",
  "Brunei" = "BN", "Burma" = "MM", "Democratic Republic of the Congo" = "CD",
  "DR Congo" = "CD", "Republic of the Congo" = "CG", "East Timor" = "TL",
  "Palestine" = "PS", "Vatican City" = "VA", "Falkland Islands" = "FK",
  "US Virgin Islands" = "VI", "British Virgin Islands" = "VG"
)
out <- data.frame(
  iso2 = c(x$iso2, x$iso2, x$iso2, x$iso2, unname(aliases)),
  name = .spec_norm_name(c(x$title, gsub("_", " ", x$enumName), x$iso2, x$iso3, names(aliases)))
)
out <- unique(out[nzchar(out$name), ])
dup <- out$name[duplicated(out$name)]
if (length(dup)) stop("names map to more than one country: ", paste(dup, collapse = ", "))
out <- out[order(out$iso2, out$name), ]
dir.create("inst/extdata", showWarnings = FALSE)
utils::write.csv(out, "inst/extdata/countries.csv", row.names = FALSE, quote = FALSE)
