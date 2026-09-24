spec_db <- function() {
  f <- withr::local_tempfile(fileext = ".sqlite", .local_envir = parent.frame())
  con <- DBI::dbConnect(RSQLite::SQLite(), f)
  withr::defer(DBI::dbDisconnect(con), envir = parent.frame())
  DBI::dbWriteTable(con, "samples", data.frame(
    ID = c("s1", "s2", "s3"),
    Taxon = c("Notemigonus crysoleucas", "Nerita albicilla", "Fish b"),
    Latitude = c("28.5378", "-14.2", NA), Longitude = c("-81.3332", "-170.6", NA),
    Date = c("23-Feb-2026", "2025", NA), geo_loc_name = c("USA: Florida", "American Samoa", NA),
    Voucher = c("UF 250399", NA, NA)))
  .meta_ensure_tables(con)
  add <- function(id, source, level, field, value) {
    DBI::dbAppendTable(con, "meta_records", data.frame(
      ID = id, source = source, level = level, depth = 0L, ref = "r", field = field, value = value))
  }
  add("s1", "GBIF", "Occurrence",
      c("decimalLatitude", "decimalLongitude", "year", "month", "day", "countryCode",
        "catalogNumber", "institutionCode", "collectionCode", "scientificName", "locality"),
      c("28.53783", "-81.33322", "2026", "2", "23", "US", "250399", "UF", "Fish",
        "Notemigonus crysoleucas (Mitchill, 1814)", "Lake Underhill"))
  add("s2", "GEOME", "Event",
      c("yearCollected", "country", "locality", "collectorList", "scientificName"),
      c("2026", "Samoa", "Amouli beach", "E. Crandall", "Nerita albicilla"))
  add("s2", "GBIF", "Occurrence",
      c("countryCode", "locality", "recordedBy", "scientificName", "catalogNumber"),
      c("AS", "Amouli Beach", "Eric Crandall", "Nerita albicilla Linnaeus, 1758",
        "https://n2t.net/ark:/21547/pm2269.04"))
  con
}

st <- function(cf, id, k) cf$status[cf$ID == id & cf$concept == k]

test_that(".spec_detect_csv finds columns case-insensitively, first candidate wins", {
  d <- .spec_detect_csv(c("ID", "Taxon", "LAT", "long", "eventDate", "date", "collection_date"))
  expect_equal(d$coordinates, c("LAT", "long"))
  expect_equal(d$collection_date, "collection_date")
  expect_equal(d$taxon, "Taxon")
  expect_equal(d$sex, character())
  expect_equal(.spec_detect_csv(c("lat_lon", "lat", "lon"))$coordinates, "lat_lon")
  o <- .spec_detect_csv(c("lat", "lon", "Where", "x", "y"),
                        c(country = "Where", coordinates = "x,y", sex = ""))
  expect_equal(o$country, "Where")
  expect_equal(o$coordinates, c("x", "y"))
  expect_equal(o$sex, character())
  expect_equal(names(o), SPECIMEN_CONCEPTS)
})

test_that("specimen_conflicts flags agree, note, conflict, single, and empty", {
  con <- spec_db()
  cf <- specimen_conflicts(con)
  expect_equal(nrow(cf), 3L * length(SPECIMEN_CONCEPTS))
  expect_equal(names(cf), c("ID", "concept", "csv_column", "csv_value", "geome_value", "gbif_value", "status"))
  expect_equal(st(cf, "s1", "coordinates"), "agree")
  expect_equal(cf$csv_column[cf$ID == "s1" & cf$concept == "coordinates"], "Latitude + Longitude")
  expect_equal(cf$csv_value[cf$ID == "s1" & cf$concept == "coordinates"], "28.5378, -81.3332")
  expect_equal(st(cf, "s1", "collection_date"), "agree")
  expect_equal(st(cf, "s1", "country"), "agree")
  expect_equal(st(cf, "s1", "voucher"), "agree")
  expect_equal(st(cf, "s1", "taxon"), "agree")
  expect_equal(st(cf, "s1", "locality"), "single")
  expect_equal(st(cf, "s2", "collection_date"), "conflict")
  expect_equal(st(cf, "s2", "country"), "conflict")
  expect_equal(st(cf, "s2", "locality"), "agree")
  expect_equal(st(cf, "s2", "collector"), "note")
  expect_equal(st(cf, "s2", "voucher"), NA_character_)
  expect_equal(st(cf, "s2", "coordinates"), "single")
  expect_equal(cf$gbif_value[cf$ID == "s2" & cf$concept == "country"], "AS")
  expect_equal(st(cf, "s3", "taxon"), "single")
  expect_true(all(is.na(cf$status[cf$ID == "s3" & cf$concept != "taxon"])))
  expect_equal(unique(specimen_conflicts(con, ids = "s2")$ID), "s2")
})

test_that("samples with no meta_records rows short-circuit to CSV-only single/NA status", {
  con <- spec_db()
  DBI::dbExecute(con, "DELETE FROM meta_records WHERE ID = 's1'")
  cf <- specimen_conflicts(con, ids = "s1")
  csv_only <- cf$concept[!is.na(cf$csv_value) & nzchar(cf$csv_value)]
  expect_true(all(cf$status[cf$concept %in% csv_only] == "single"))
  expect_true(all(is.na(cf$status[!cf$concept %in% csv_only])))
  expect_true(all(is.na(cf$geome_value)))
  expect_true(all(is.na(cf$gbif_value)))
})

test_that("stored CSV overrides change what is compared, NA restores detection", {
  con <- spec_db()
  .spec_set_csv_map(con, list(collection_date = ""))
  cf <- specimen_conflicts(con)
  expect_equal(st(cf, "s2", "collection_date"), "single")
  expect_true(is.na(cf$csv_column[cf$concept == "collection_date"][1]))
  .spec_set_csv_map(con, list(collection_date = NA))
  expect_equal(st(specimen_conflicts(con), "s2", "collection_date"), "conflict")
  .spec_set_csv_map(con, list(coordinates = c("Longitude", "Latitude")))
  expect_equal(st(specimen_conflicts(con), "s1", "coordinates"), "conflict")
})

test_that(".spec_set_csv_map validates before writing anything", {
  con <- spec_db()
  expect_error(.spec_set_csv_map(con, list(country = "geo_loc_name", colour = "x")), "unknown concept")
  expect_error(.spec_set_csv_map(con, list(taxon = "Taxon")), "unknown concept")
  expect_error(.spec_set_csv_map(con, list(country = "Nope")), "not in the samples table")
  expect_error(.spec_set_csv_map(con, list(sex = c("Date", "Voucher"))), "too many columns")
  expect_error(.spec_set_csv_map(con, list(coordinates = c("Date", "Voucher", "Taxon"))), "too many columns")
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM meta_csv_map")$n, 0L)
})

test_that("set_metadata_columns writes overrides and reports the result", {
  d <- withr::local_tempdir()
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(d, ".sqlite"))
  DBI::dbWriteTable(con, "samples", data.frame(ID = "s1", Taxon = "x", Where = "Peru", country = "Chile"))
  DBI::dbDisconnect(con)
  res <- set_metadata_columns(d, country = "Where")
  expect_equal(res$column[res$concept == "country"], "Where")
  res <- set_metadata_columns(d, country = NA)
  expect_equal(res$column[res$concept == "country"], "country")
  res <- set_metadata_columns(d)
  expect_equal(res$column[res$concept == "taxon"], "Taxon")
})

csv_map <- function() {
  list(coordinates = c("Latitude", "Longitude"), collection_date = "Date",
       country = "geo_loc_name", locality = character(), voucher = character(),
       collector = character(), sex = character(), dev_stage = character(), taxon = "Taxon")
}

test_that("specimen_template_concepts maps combo, raw, and CSV tokens to concepts", {
  f <- function(t) specimen_template_concepts(t, csv_map())
  expect_setequal(f("{seqid} [lat_lon={geome_lat_lon}] [geo_loc_name={gbif_geo_loc_name}]"),
                  c("coordinates", "country", "locality"))
  expect_setequal(f("{gbif_Occurrence_decimalLatitude} {geome_Event_yearCollected}"),
                  c("coordinates", "collection_date"))
  expect_setequal(f("[collection_date={Date}] {Taxon}"), c("collection_date", "taxon"))
  expect_equal(f("{seqid} [mgcode={genetic_code}] {completeness}"), character())
  expect_equal(f(c("{seqid}", NA, "[sex={geome_sex}]")), "sex")
  expect_equal(f("{geome_tissue_type} {gbif_identified_by} {gbif_Dataset_title}"), character())
})

test_that("specimen_export_warnings keeps only conflicts on used concepts for exported samples", {
  cf <- data.frame(ID = c("s1", "s1", "s2", "s3"), concept = c("country", "sex", "country", "country"),
                   csv_column = NA, csv_value = c("USA", "m", "Peru", "Chile"),
                   geome_value = c("Canada", "f", "Peru", "Peru"), gbif_value = NA,
                   status = c("conflict", "note", "agree", "conflict"))
  w <- specimen_export_warnings(cf, c("country", "sex"), c("s1", "s2"))
  expect_equal(w$ID, "s1")
  expect_equal(w$concept, "country")
  expect_equal(names(w), c("ID", "concept", "csv_value", "geome_value", "gbif_value"))
  expect_equal(nrow(specimen_export_warnings(cf, "sex", c("s1", "s2", "s3"))), 0L)
  html <- as.character(specimen_warning_html(w))
  expect_match(html, "<td>s1</td><td>country</td><td>USA</td><td>Canada</td><td>-</td>", fixed = TRUE)
  expect_match(html, "1 sample", fixed = TRUE)
})

test_that("a raw-token template triggers the warning end to end", {
  con <- spec_db()
  cf <- specimen_conflicts(con)
  concepts <- specimen_template_concepts("{seqid} [country={geome_Event_country}]", specimen_csv_columns(con))
  w <- specimen_export_warnings(cf, concepts, c("s1", "s2"))
  expect_equal(unique(w$ID), "s2")
  expect_equal(w$concept, "country")
  expect_equal(nrow(specimen_export_warnings(
    cf, specimen_template_concepts("{seqid} {completeness}", specimen_csv_columns(con)), "s2")), 0L)
})
