recs <- function(...) {
  x <- list(...)
  data.frame(level = "Event", depth = 2L, field = names(x), value = unlist(x))
}

test_that("lat_lon formats hemispheres and keeps given precision", {
  f <- GEOME_COMBOS$lat_lon$fn
  expect_equal(f(recs(decimalLatitude = "-17.530", decimalLongitude = "-149.83")),
               "17.530 S 149.83 W")
  expect_equal(f(recs(decimalLatitude = "+38.9", decimalLongitude = "77.03")),
               "38.9 N 77.03 E")
  expect_true(is.na(f(recs(decimalLatitude = "95", decimalLongitude = "10"))))
  expect_true(is.na(f(recs(decimalLatitude = "abc", decimalLongitude = "10"))))
  expect_true(is.na(f(recs(decimalLatitude = "10"))))
})

test_that("collection_date handles partial dates", {
  f <- GEOME_COMBOS$collection_date$fn
  expect_equal(f(recs(yearCollected = "2009", monthCollected = "11", dayCollected = "5")), "2009-11-05")
  expect_equal(f(recs(yearCollected = "2009", monthCollected = "11")), "2009-11")
  expect_equal(f(recs(yearCollected = "2009", dayCollected = "5")), "2009")
  expect_true(is.na(f(recs(yearCollected = "09"))))
  expect_equal(f(recs(yearCollected = "2009", monthCollected = "13")), "2009")
})

test_that("geo_loc_name joins country with region and locality", {
  f <- GEOME_COMBOS$geo_loc_name$fn
  expect_equal(f(recs(country = "French Polynesia", locality = "Moorea")), "French Polynesia: Moorea")
  expect_equal(f(recs(country = "USA", stateProvince = "Maryland", locality = "Solomons")),
               "USA: Maryland, Solomons")
  expect_equal(f(recs(country = "French Polynesia", locality = "French Polynesia")), "French Polynesia")
  expect_true(is.na(f(recs(locality = "Moorea"))))
})

test_that("specimen_voucher needs a catalog number", {
  f <- GEOME_COMBOS$specimen_voucher$fn
  expect_equal(f(recs(institutionCode = "USNM", catalogNumber = "123")), "USNM:123")
  expect_equal(f(recs(catalogNumber = "123")), "123")
  expect_true(is.na(f(recs(institutionCode = "USNM"))))
})

test_that("nearest level wins when a field appears twice", {
  r <- rbind(
    data.frame(level = "Sample", depth = 1L, field = "country", value = "Near"),
    data.frame(level = "Event", depth = 2L, field = "country", value = "Far")
  )
  expect_equal(GEOME_COMBOS$geo_loc_name$fn(r), "Near")
})

test_that("geome_export_cols returns only ticked keys, NULL when none", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbWriteTable(con, "samples", data.frame(ID = c("s1", "s2"), Taxon = "x"))
  .geome_ensure_tables(con)
  expect_null(geome_export_cols(con))
  DBI::dbAppendTable(con, "geome_records", data.frame(
    ID = "s1", level = "Event", depth = 2L, bcid = "ark:/1/E",
    field = c("country", "locality"), value = c("Peru", "Lima")))
  DBI::dbAppendTable(con, "geome_export_fields",
                     data.frame(key = c("combo:geo_loc_name", "raw:Event:country")))
  out <- geome_export_cols(con, ids = c("s1", "s2"))
  expect_equal(names(out), c("ID", "geome_geo_loc_name", "geome_Event_country"))
  expect_equal(out$geome_geo_loc_name, c("Peru: Lima", NA))
  expect_equal(out$geome_Event_country, c("Peru", NA))
})

test_that("geome_export_cols is NULL on a project without GEOME tables", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  expect_null(geome_export_cols(con))
})

test_that("geome_field_summary lists combos and raw fields with counts", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbWriteTable(con, "samples", data.frame(ID = c("s1", "s2"), Taxon = "x"))
  .geome_ensure_tables(con)
  DBI::dbAppendTable(con, "geome_records", data.frame(
    ID = c("s1", "s2"), level = "Event", depth = 2L, bcid = "ark:/1/E",
    field = "country", value = c("Peru", "Chile")))
  DBI::dbAppendTable(con, "geome_export_fields", data.frame(key = "raw:Event:country"))
  s <- geome_field_summary(con)
  raw <- s[s$key == "raw:Event:country", ]
  expect_equal(raw$n_samples, 2L)
  expect_true(raw$selected)
  expect_equal(raw$col, "geome_Event_country")
  expect_true(all(paste0("combo:", names(GEOME_COMBOS)) %in% s$key))
})
