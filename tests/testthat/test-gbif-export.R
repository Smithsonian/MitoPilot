occ <- function(...) {
  x <- list(...)
  data.frame(level = "Occurrence", depth = 0L, field = names(x), value = unlist(x))
}

gbif_recs <- function(id) {
  local_mocked_bindings(.gbif_get = gbif_fixture_get)
  .gbif_fetch_chain(id)
}

test_that("GBIF combos build GenBank values from the fish occurrence", {
  r <- gbif_recs("6186461308")
  v <- function(nm) GBIF_COMBOS[[nm]]$fn(r)
  expect_equal(v("lat_lon"), "28.53783 N 81.33322 W")
  expect_equal(v("collection_date"), "2026-02-23")
  expect_equal(v("geo_loc_name"), "United States of America: Florida, Lake Underhill")
  expect_equal(v("specimen_voucher"), "UF:Fish:250399")
  expect_equal(v("collected_by"), "Robins, Robert; Casteel, Jamie; Lange, Ted")
  expect_true(is.na(v("sex")))
})

test_that("GBIF combos skip an ARK catalog number and lowercase life stage", {
  r <- gbif_recs("2336663130")
  expect_true(is.na(GBIF_COMBOS$specimen_voucher$fn(r)))
  expect_equal(GBIF_COMBOS$dev_stage$fn(r), "adult")
  expect_equal(GBIF_COMBOS$collection_date$fn(r), "2026")
  expect_equal(GBIF_COMBOS$lat_lon$fn(r), "14.27374 S 170.58225 W")
})

test_that("gbif lat_lon is empty when an issue flags the coordinates", {
  f <- GBIF_COMBOS$lat_lon$fn
  expect_equal(f(occ(decimalLatitude = "10.5", decimalLongitude = "-20", issues = "COORDINATE_ROUNDED")),
               "10.5 N 20 W")
  for (iss in c("ZERO_COORDINATE", "COORDINATE_INVALID", "COORDINATE_OUT_OF_RANGE")) {
    expect_true(is.na(f(occ(decimalLatitude = "0", decimalLongitude = "0",
                            issues = paste0("GEODETIC_DATUM_ASSUMED_WGS84,", iss)))))
  }
  expect_true(is.na(f(occ(decimalLatitude = "10"))))
})

test_that("gbif collection_date falls back to a single eventDate and ignores intervals", {
  f <- GBIF_COMBOS$collection_date$fn
  expect_equal(f(occ(eventDate = "2019-07-04T10:00:00")), "2019-07-04")
  expect_equal(f(occ(eventDate = "2019-07")), "2019-07")
  expect_true(is.na(f(occ(eventDate = "2019-07-01/2019-07-09"))))
  expect_equal(f(occ(year = "2019", month = "7", eventDate = "2019-07-01/2019-07-09")), "2019-07")
  expect_true(is.na(f(occ(eventDate = "July 2019"))))
})

test_that("gbif specimen_voucher builds the institution:collection:catalog triplet", {
  f <- GBIF_COMBOS$specimen_voucher$fn
  expect_equal(f(occ(institutionCode = "UF", collectionCode = "Fish", catalogNumber = "250399")), "UF:Fish:250399")
  expect_equal(f(occ(institutionCode = "UF", catalogNumber = "250399")), "UF:250399")
  expect_equal(f(occ(collectionCode = "Fish", catalogNumber = "250399")), "250399")
  expect_true(is.na(f(occ(institutionCode = "UF"))))
  expect_true(is.na(f(occ(catalogNumber = "https://n2t.net/ark:/21547/pm2269.04"))))
  expect_true(is.na(f(occ(catalogNumber = "ark:/21547/pm2269.04"))))
})

test_that("gbif combos read only the Occurrence level", {
  r <- rbind(occ(sex = "Female"),
             data.frame(level = "Dataset", depth = 1L, field = "country", value = "US"))
  expect_equal(GBIF_COMBOS$sex$fn(r), "female")
  expect_true(is.na(GBIF_COMBOS$geo_loc_name$fn(r)))
})

test_that("GEOME collection_date is unchanged by the shared date helper", {
  expect_equal(.meta_ymd("2009", "11", "5"), "2009-11-05")
  expect_equal(.meta_ymd("2009", "13", "5"), "2009")
  expect_equal(.meta_ymd("2009", "11", "40"), "2009-11")
  expect_true(is.na(.meta_ymd("09", NA, NA)))
})

test_that("ticked GBIF keys join as gbif_ columns and appear in the field summary", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbWriteTable(con, "samples", data.frame(ID = c("s1", "s2"), Taxon = "x"))
  .meta_ensure_tables(con)
  DBI::dbAppendTable(con, "meta_records", data.frame(
    ID = "s1", source = "GBIF", level = "Occurrence", depth = 0L, ref = "1",
    field = c("countryCode", "catalogNumber", "institutionCode"), value = c("US", "250399", "UF")))
  .meta_save_fields(con, c("gbif:combo:specimen_voucher", "gbif:raw:Occurrence:countryCode"))
  out <- meta_export_cols(con, c("s1", "s2"))
  expect_equal(out$gbif_specimen_voucher, c("UF:250399", NA))
  expect_equal(out$gbif_Occurrence_countryCode, c("US", NA))
  s <- meta_field_summary(con, "GBIF")
  expect_true(all(paste0("gbif:combo:", names(GBIF_COMBOS)) %in% s$key))
  expect_true(s$selected[s$key == "gbif:raw:Occurrence:countryCode"])
  expect_equal(s$col[s$key == "gbif:combo:lat_lon"], "gbif_lat_lon")
  expect_false(any(grepl("^geome:", s$key)))
})

test_that("export_metadata_cols treats GBIF_ID as owned", {
  expect_equal(export_metadata_cols(c("ID", "Taxon", "GBIF_ID", "GEOME_BCID", "site"), character()), "site")
})
