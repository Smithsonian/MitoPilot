test_that("gbif_normalize_id accepts digits, numbers, and gbif.org URLs", {
  x <- c("6186461308", " 6186461308 ",
         "https://www.gbif.org/occurrence/6186461308",
         "https://gbif.org/occurrence/6186461308/",
         "https://api.gbif.org/v1/occurrence/6186461308",
         "", NA, "abc", "12.5", "https://www.gbif.org/dataset/6186461308")
  expect_equal(gbif_normalize_id(x), c(rep("6186461308", 5), rep(NA, 5)))
  expect_equal(gbif_normalize_id(c(6186461308, NA)), c("6186461308", NA))
})

test_that(".gbif_fetch_chain returns occurrence, dataset, and organization levels", {
  local_mocked_bindings(.gbif_get = gbif_fixture_get)
  out <- .gbif_fetch_chain("6186461308")
  lv <- unique(out[order(out$depth), c("level", "depth")])
  expect_equal(lv$level, c("Occurrence", "Dataset", "Organization"))
  expect_equal(lv$depth, 0:2)
  occ <- out[out$level == "Occurrence", ]
  expect_equal(occ$value[occ$field == "countryCode"], "US")
  expect_equal(occ$value[occ$field == "catalogNumber"], "250399")
  expect_equal(unique(occ$ref), "6186461308")
  expect_match(occ$value[occ$field == "issues"], "GEODETIC_DATUM_ASSUMED_WGS84", fixed = TRUE)
  expect_false(grepl("[", occ$value[occ$field == "issues"], fixed = TRUE))
  ds <- out[out$level == "Dataset", ]
  expect_equal(ds$value[ds$field == "title"], "UF FLMNH Ichthyology")
  expect_match(ds$value[ds$field == "citation"], "UF FLMNH Ichthyology", fixed = TRUE)
  expect_equal(unique(ds$ref), "eccf4b09-f0c8-462d-a48c-41a7ce36815a")
  org <- out[out$level == "Organization", ]
  expect_equal(org$value[org$field == "title"], "Florida Museum of Natural History")
  expect_equal(org$value[org$field == "homepage"], "http://www.flmnh.ufl.edu")
  expect_type(out$depth, "integer")
})

test_that("the GEOME-published record resolves to the GeOMe publisher", {
  local_mocked_bindings(.gbif_get = gbif_fixture_get)
  out <- .gbif_fetch_chain("2336663130")
  expect_equal(out$value[out$level == "Organization" & out$field == "title"],
               "The Genomic Observatories Metadatabase (GeOMe)")
  expect_equal(out$value[out$level == "Dataset" & out$field == "title"],
               "Diversity of the Indo-Pacific (DIPnet)")
})

test_that("dataset and organization are fetched once per cache", {
  local_mocked_bindings(.gbif_get = gbif_fixture_get)
  gbif_reset_calls()
  cache <- new.env()
  .gbif_fetch_chain("6186461308", cache)
  .gbif_fetch_chain("6186461308", cache)
  expect_equal(gbif_calls[["dataset/eccf4b09-f0c8-462d-a48c-41a7ce36815a"]], 1L)
  expect_equal(gbif_calls[["organization/8483a1f0-1032-11db-ae00-b8a03c50a862"]], 1L)
})

test_that("an unknown occurrence stops with the 404 message", {
  local_mocked_bindings(.gbif_get = gbif_fixture_get)
  expect_error(.gbif_fetch_chain("999999999999"), "GBIF occurrence not found")
})

test_that("a failing dataset lookup keeps the occurrence and organization", {
  fake <- function(path) {
    if (startsWith(path, "occurrence/")) {
      return(list(key = 1, datasetKey = "d1", publishingOrgKey = "o1", country = "Peru",
                  issues = list("ZERO_COORDINATE", "COORDINATE_INVALID")))
    }
    if (startsWith(path, "organization/")) return(list(title = "Org", homepage = list("http://a", "http://b")))
    stop("GBIF returned HTTP 503", call. = FALSE)
  }
  local_mocked_bindings(.gbif_get = fake)
  out <- .gbif_fetch_chain("1")
  expect_equal(unique(out$level), c("Occurrence", "Organization"))
  expect_equal(out$value[out$field == "issues"], "ZERO_COORDINATE,COORDINATE_INVALID")
  expect_equal(out$value[out$field == "homepage"], "http://a, http://b")
})

test_that("an occurrence without issues stores no issues field", {
  local_mocked_bindings(.gbif_get = function(path) list(key = 5, issues = list()))
  out <- .gbif_fetch_chain("5")
  expect_false("issues" %in% out$field)
})

test_that(".gbif_get maps HTTP status to readable errors", {
  msg <- function(st) {
    httr2::local_mocked_responses(function(req) httr2::response(status_code = st))
    tryCatch(.gbif_get("occurrence/1"), error = conditionMessage)
  }
  expect_equal(msg(404), "GBIF occurrence not found (IDs can change when a dataset is republished)")
  expect_equal(msg(400), "GBIF returned HTTP 400")
  expect_equal(msg(418), "GBIF returned HTTP 418")
})

test_that("live GBIF lookup works", {
  skip_on_cran()
  skip_if_offline("api.gbif.org")
  out <- .gbif_fetch_chain("6186461308")
  expect_true("Occurrence" %in% out$level)
})
