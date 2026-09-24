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

geome_mem_db <- function(ids = c("s1", "s2")) {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(con, "samples", data.frame(ID = ids, Taxon = "x"))
  .meta_ensure_tables(con)
  con
}

add_geome_recs <- function(con, id, field, value, level = "Event", depth = 2L) {
  DBI::dbAppendTable(con, "meta_records", data.frame(
    ID = id, source = "GEOME", level = level, depth = depth, ref = "ark:/1/E",
    field = field, value = value))
}

test_that("meta_export_cols returns only ticked keys, NULL when none", {
  con <- geome_mem_db()
  on.exit(DBI::dbDisconnect(con))
  expect_null(meta_export_cols(con))
  add_geome_recs(con, "s1", c("country", "locality"), c("Peru", "Lima"))
  .meta_save_fields(con, c("geome:combo:geo_loc_name", "geome:raw:Event:country"))
  out <- meta_export_cols(con, ids = c("s1", "s2"))
  expect_equal(names(out), c("ID", "geome_geo_loc_name", "geome_Event_country"))
  expect_equal(out$geome_geo_loc_name, c("Peru: Lima", NA))
  expect_equal(out$geome_Event_country, c("Peru", NA))
})

test_that("meta_export_cols is NULL on a project that never had GEOME tables", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  expect_null(meta_export_cols(con))
})

test_that("meta_field_summary lists GEOME combos and raw fields with counts", {
  con <- geome_mem_db()
  on.exit(DBI::dbDisconnect(con))
  add_geome_recs(con, c("s1", "s2"), "country", c("Peru", "Chile"))
  .meta_save_fields(con, "geome:raw:Event:country")
  s <- meta_field_summary(con, "GEOME")
  raw <- s[s$key == "geome:raw:Event:country", ]
  expect_equal(raw$n_samples, 2L)
  expect_true(raw$selected)
  expect_equal(raw$col, "geome_Event_country")
  expect_true(all(paste0("geome:combo:", names(GEOME_COMBOS)) %in% s$key))
})

test_that("export_metadata_cols treats GEOME_BCID as owned", {
  expect_equal(export_metadata_cols(c("ID", "Taxon", "GEOME_BCID", "site"), character()), "site")
})

test_that(".meta_join adds ticked columns and is a no-op otherwise", {
  con <- geome_mem_db("s1")
  on.exit(DBI::dbDisconnect(con))
  dat <- data.frame(ID = "s1", Taxon = "x")
  expect_identical(.meta_join(dat, con), dat)
  add_geome_recs(con, "s1", "country", "Peru")
  .meta_save_fields(con, "geome:raw:Event:country")
  expect_equal(.meta_join(dat, con)$geome_Event_country, "Peru")
})

test_that("a ticked GEOME column resolves in a header template", {
  dat <- data.frame(ID = "s1", geome_lat_lon = "17.5 S 149.8 W")
  expect_equal(as.character(stringr::str_glue_data(dat, "{ID} [lat_lon={geome_lat_lon}]")),
               "s1 [lat_lon=17.5 S 149.8 W]")
})

test_that("a ticked GEOME field with no value joins as empty, not NA", {
  con <- geome_mem_db()
  on.exit(DBI::dbDisconnect(con))
  add_geome_recs(con, "s1", "country", "Peru")
  .meta_save_fields(con, "geome:combo:lat_lon")
  dat <- .meta_join(data.frame(ID = c("s1", "s2"), Taxon = c("x", NA)), con)
  expect_equal(dat$geome_lat_lon, c("", ""))
  expect_true(is.na(dat$Taxon[2]))
  expect_equal(as.character(stringr::str_glue_data(dat[1, ], "[lat_lon={geome_lat_lon}]")), "[lat_lon=]")
})
