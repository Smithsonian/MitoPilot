gbif_calls <- new.env()

gbif_fixture_get <- function(path) {
  gbif_calls[[path]] <- (gbif_calls[[path]] %||% 0L) + 1L
  f <- testthat::test_path("fixtures", "gbif",
                           paste0(gsub("[^A-Za-z0-9.]", "_", path), ".json"))
  if (!file.exists(f)) {
    stop("GBIF occurrence not found (IDs can change when a dataset is republished)", call. = FALSE)
  }
  jsonlite::fromJSON(f, simplifyVector = FALSE)
}

gbif_reset_calls <- function() rm(list = ls(gbif_calls), envir = gbif_calls)
