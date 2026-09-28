ncbi_calls <- new.env()

ncbi_fixture_get <- function(endpoint, query) {
  key <- query$id %||% sub("\\[accn\\]$", "", query$term)
  f <- testthat::test_path("fixtures", "ncbi", paste0(endpoint, "_", query$db, "_", key,
                           if (identical(query$retmode, "json")) ".json" else ".xml"))
  ncbi_calls[[basename(f)]] <- (ncbi_calls[[basename(f)]] %||% 0L) + 1L
  if (!file.exists(f)) stop("NCBI returned HTTP 500", call. = FALSE)
  paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

ncbi_reset_calls <- function() rm(list = ls(ncbi_calls), envir = ncbi_calls)
