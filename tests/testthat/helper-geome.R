geome_calls <- new.env()

geome_fixture_get <- function(path, query = list()) {
  geome_calls[[path]] <- (geome_calls[[path]] %||% 0L) + 1L
  f <- testthat::test_path("fixtures", "geome",
                           paste0(gsub("[^A-Za-z0-9.]", "_", path), ".json"))
  if (!file.exists(f)) stop("BCID not found in GEOME", call. = FALSE)
  jsonlite::fromJSON(f, simplifyVector = FALSE)
}

geome_reset_calls <- function() rm(list = ls(geome_calls), envir = geome_calls)
