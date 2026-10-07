test_that("link switch and provenance tables", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbWriteTable(con, "samples", data.frame(ID = "s1", Taxon = "x"))
  .meta_ensure_tables(con)
  expect_false(.meta_link_enabled(con))
  .meta_set_link_enabled(con, TRUE)
  expect_true(.meta_link_enabled(con))
  DBI::dbExecute(con, "INSERT INTO meta_links VALUES ('s1', 'GBIF', '123', 'NCBI BioSample voucherURI', NULL)")
  .meta_set_ref(con, "GBIF", "s1", "456")
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM meta_links")$n, 0L)
})

lrecs <- function(level, field, value, depth = 1L, ref = "r") {
  data.frame(level = level, depth = depth, ref = ref, field = field, value = value)
}
gocc <- function(key, sp, coll = "FISH", basis = "PRESERVED_SPECIMEN") {
  list(key = key, scientificName = sp, collectionCode = coll, basisOfRecord = basis)
}

test_that("NCBI records link to GEOME by bcid and to GBIF by NMNH EZID", {
  r <- lrecs("BioSample", c("bcid", "voucherURI"),
             c("https://n2t.net/ark:/21547/FDZ2UW:157636.1",
               "http://n2t.net/ark:/65665/3dd003c5a-d734-480a-9670-918173b14bd3"))
  c <- .meta_link_candidates("NCBI", r, "Upeneus parvus")
  expect_equal(c$GEOME$ref, "ark:/21547/FDZ2UW:157636.1")
  expect_equal(c$GEOME$via, "NCBI BioSample bcid")
  expect_equal(c$GBIF$ref, "ark:/65665/3dd003c5a-d734-480a-9670-918173b14bd3")
  expect_equal(c$GBIF$via, "NCBI BioSample voucherURI")
})

test_that("a voucher from another museum is found by GBIF catalog number", {
  local_mocked_bindings(.gbif_get = function(path) {
    if (grepl("catalogNumber=UW%20157636", path, fixed = TRUE)) {
      return(list(results = list(gocc(2013211250, "Psychrolutes paradoxus Gunther, 1861", "ADULT COLLECTION"))))
    }
    list(results = list())
  })
  c <- .meta_link_candidates("NCBI", lrecs("BioSample", "specimen_voucher", "UW:157636"), "Psychrolutes paradoxus")
  expect_equal(c$GBIF$ref, "2013211250")
  expect_equal(c$GBIF$via, "NCBI BioSample specimen_voucher UW:157636")
})

test_that("voucher search never links when the species or count is wrong", {
  other <- list(results = lapply(1:3, function(i) gocc(i, "Melamphaes parvus", "Marine Vertebrates")))
  local_mocked_bindings(.gbif_get = function(path) other)
  expect_null(.gbif_find_voucher("SIO:09-320", "Psenes pellucidus"))
  two <- list(results = list(gocc(1, "Psenes pellucidus", "MV"), gocc(2, "Psenes pellucidus", "MV")))
  local_mocked_bindings(.gbif_get = function(path) two)
  expect_match(.gbif_find_voucher("SIO:09-320", "Psenes pellucidus")$note, "2 possible GBIF matches for SIO:09-320")
})

test_that("collection code and preserved specimens narrow USNM hits", {
  hits <- list(results = list(gocc(1321689872, "Fundulus majalis (Walbaum, 1792)"),
                              gocc(3028087651, "Fundulus majalis (Walbaum, 1792)", basis = "MATERIAL_SAMPLE"),
                              gocc(1318885156, "Plethodon cinereus", "HERP")))
  local_mocked_bindings(.gbif_get = function(path) if (grepl("USNM%20419933", path, fixed = TRUE)) hits else list(results = list()))
  expect_equal(.gbif_find_voucher("USNM:FISH:419933", "Fundulus majalis")$ref, "1321689872")
})

test_that("GBIF records link to NCBI and GEOME", {
  r <- lrecs("Occurrence", c("associatedSequences", "occurrenceID"),
             c("PV245865|SRR34274583|SAMN49487040|PRJNA1057917", "ark:/21547/ABC1"), depth = 0L)
  c <- .meta_link_candidates("GBIF", r, "x")
  expect_equal(c$NCBI$ref, "SAMN49487040")
  expect_equal(c$GEOME$ref, "ark:/21547/ABC1")
  c2 <- .meta_link_candidates("GBIF", lrecs("Occurrence", "associatedSequences", "PV1|SRR9", 0L), "x")
  expect_equal(c2$NCBI$ref, "SRR9")
})

test_that("GEOME records link to NCBI through the sequencing record and to GBIF", {
  local_mocked_bindings(.geome_get = function(path, query = list()) {
    expect_equal(query$includeChildren, "true")
    list(children = list(list(entity = "fastqMetadata", bioSample = list(accession = "SAMN1"))))
  })
  r <- rbind(lrecs("Tissue", "bcid", "ark:/21547/T1", 0L, "ark:/21547/T1"),
             lrecs("Sample", "voucherURI", "http://n2t.net/ark:/65665/3dd003c5a-d734-480a-9670-918173b14bd3", 1L))
  c <- .meta_link_candidates("GEOME", r, "x")
  expect_equal(c$NCBI$ref, "SAMN1")
  expect_equal(c$NCBI$via, "GEOME Tissue sequencing record")
  expect_equal(c$GBIF$via, "GEOME Sample voucherURI")
})

test_that("a failing lookup gives no candidate instead of an error", {
  local_mocked_bindings(.geome_get = function(...) stop("could not reach GEOME"),
                        .gbif_get = function(...) stop("could not reach GBIF"))
  r <- rbind(lrecs("Tissue", "bcid", "ark:/21547/T1", 0L, "ark:/21547/T1"),
             lrecs("Sample", "genbankSpecimenVoucher", "UW:1", 1L))
  expect_equal(.meta_link_candidates("GEOME", r, "x"), list())
})

link_con <- function(envir = parent.frame()) {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  withr::defer(DBI::dbDisconnect(con), envir = envir)
  DBI::dbWriteTable(con, "samples", data.frame(ID = "s1", Taxon = "Fundulus majalis"))
  .meta_ensure_tables(con)
  DBI::dbExecute(con, "UPDATE samples SET BioSample = 'SAMN1'")
  con
}
ezid <- "ark:/65665/3dd003c5a-d734-480a-9670-918173b14bd3"
mock_chains <- function(calls, geome_err = FALSE) {
  list(
    .ncbi_fetch_chain = function(ref, cache) {
      calls$NCBI <- (calls$NCBI %||% 0L) + 1L
      data.frame(level = "BioSample", depth = 1L, ref = "SAMN1", field = "bcid",
                 value = "https://n2t.net/ark:/21547/T1")
    },
    .geome_fetch_chain = function(ref, cache) {
      calls$GEOME <- (calls$GEOME %||% 0L) + 1L
      if (geome_err) stop("could not reach GEOME")
      data.frame(level = c("Tissue", "Sample"), depth = 0:1, ref = c("ark:/21547/T1", "ark:/21547/S1"),
                 field = c("bcid", "voucherURI"), value = c("ark:/21547/T1", paste0("http://n2t.net/", ezid)))
    },
    .gbif_fetch_chain = function(ref, cache) {
      calls$GBIF <- (calls$GBIF %||% 0L) + 1L
      data.frame(level = "Occurrence", depth = 0L, ref = "77", field = "associatedSequences", value = "SAMN1")
    },
    .geome_get = function(...) list(children = list())
  )
}

test_that("linking follows NCBI -> GEOME -> GBIF once each and records where IDs came from", {
  con <- link_con()
  calls <- new.env()
  local_mocked_bindings(!!!mock_chains(calls))
  .meta_fetch_into(con, "NCBI", "s1", "SAMN1", link = TRUE)
  s <- DBI::dbGetQuery(con, "SELECT GEOME_BCID, GBIF_ID, BioSample FROM samples")
  expect_equal(s$GEOME_BCID, "ark:/21547/T1")
  expect_equal(s$GBIF_ID, ezid)
  expect_equal(c(calls$NCBI, calls$GEOME, calls$GBIF), c(1L, 1L, 1L))
  l <- DBI::dbGetQuery(con, "SELECT source, via FROM meta_links ORDER BY source")
  expect_equal(l$source, c("GBIF", "GEOME"))
  expect_equal(l$via, c("GEOME Sample voucherURI", "NCBI BioSample bcid"))
})

test_that("linking never overwrites a user ID and leaves a note instead", {
  con <- link_con()
  DBI::dbExecute(con, "UPDATE samples SET GEOME_BCID = 'ark:/21547/MINE'")
  calls <- new.env()
  local_mocked_bindings(!!!mock_chains(calls))
  .meta_fetch_into(con, "NCBI", "s1", "SAMN1", link = TRUE)
  expect_equal(DBI::dbGetQuery(con, "SELECT GEOME_BCID FROM samples")$GEOME_BCID, "ark:/21547/MINE")
  n <- DBI::dbGetQuery(con, "SELECT ref, note FROM meta_links WHERE source = 'GEOME'")
  expect_true(is.na(n$ref))
  expect_match(n$note, "ark:/21547/T1.*kept your ID ark:/21547/MINE")
  expect_null(calls$GEOME)
})

test_that("a failing linked fetch does not fail the original fetch", {
  con <- link_con()
  calls <- new.env()
  local_mocked_bindings(!!!mock_chains(calls, geome_err = TRUE))
  res <- .meta_fetch_into(con, "NCBI", "s1", "SAMN1", link = TRUE)
  expect_equal(res$status, "ok")
  st <- DBI::dbGetQuery(con, "SELECT source, status FROM meta_status ORDER BY source")
  expect_equal(st$status[st$source == "GEOME"], "failed")
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM meta_links WHERE source = 'GEOME'")$n, 1L)
})

test_that("no linking unless asked; fetch_* follow the project switch", {
  con <- link_con()
  calls <- new.env()
  local_mocked_bindings(!!!mock_chains(calls))
  .meta_fetch_into(con, "NCBI", "s1", "SAMN1")
  expect_null(calls$GEOME)
  dir <- withr::local_tempdir()
  db <- DBI::dbConnect(RSQLite::SQLite(), file.path(dir, ".sqlite"))
  DBI::dbWriteTable(db, "samples", data.frame(ID = "s1", Taxon = "Fundulus majalis", BioSample = "SAMN1"))
  .meta_ensure_tables(db)
  .meta_set_link_enabled(db, TRUE)
  DBI::dbDisconnect(db)
  fetch_ncbi(dir)
  expect_equal(calls$GEOME, 1L)
  fetch_ncbi(dir, link_sources = FALSE)
  expect_equal(calls$GEOME, 1L)
})

test_that("new_db stores the link switch", {
  local_mocked_bindings(.ncbi_get = ncbi_fixture_get, .geome_get = function(...) stop("offline"),
                        .gbif_get = function(...) stop("offline"))
  d <- withr::local_tempdir()
  m <- data.frame(ID = "SRR21844202", Taxon = "Fundulus majalis", R1 = "a_1.fq", R2 = "a_2.fq",
                  BioSample = "SRR21844202")
  utils::write.csv(m, file.path(d, "mapping.csv"), row.names = FALSE)
  suppressWarnings(new_db(db_path = file.path(d, ".sqlite"), mapping_fn = file.path(d, "mapping.csv"),
                          link_sources = TRUE))
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(d, ".sqlite"))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_true(.meta_link_enabled(con))
})

test_that("a link to the record already fetched under another ID form is not a disagreement", {
  con <- link_con()
  DBI::dbExecute(con, "UPDATE samples SET BioSample = 'SRR1'")
  calls <- new.env()
  m <- mock_chains(calls)
  m$.ncbi_fetch_chain <- function(ref, cache) {
    data.frame(level = c("SRA", "BioSample"), depth = 0:1, ref = c("SRR1", "SAMN1"),
               field = c("accession", "bcid"), value = c("SRR1", "https://n2t.net/ark:/21547/T1"))
  }
  m$.geome_get <- function(...) list(children = list(list(bioSample = list(accession = "SAMN1"))))
  local_mocked_bindings(!!!m)
  .meta_fetch_into(con, "NCBI", "s1", "SRR1", link = TRUE)
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM meta_links WHERE source = 'NCBI'")$n, 0L)
})

test_that("update_sample_metadata makes a CSV-supplied ID the user's and keeps linked IDs on blanks", {
  dir <- withr::local_tempdir()
  m <- data.frame(ID = c("s1", "s2"), Taxon = "x", R1 = "a", R2 = "b", BioSample = c("", ""))
  utils::write.csv(m, file.path(dir, "m.csv"), row.names = FALSE)
  suppressMessages(new_db(db_path = file.path(dir, ".sqlite"), mapping_fn = file.path(dir, "m.csv"),
                          fetch_ncbi = FALSE))
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(dir, ".sqlite"))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .meta_ensure_tables(con)
  DBI::dbExecute(con, "UPDATE samples SET BioSample = 'SAMN1' WHERE ID IN ('s1', 's2')")
  DBI::dbExecute(con, "INSERT INTO meta_links VALUES ('s1', 'NCBI', 'SAMN1', 'GEOME Tissue sequencing record', NULL)")
  DBI::dbExecute(con, "INSERT INTO meta_links VALUES ('s2', 'NCBI', 'SAMN1', 'GEOME Tissue sequencing record', NULL)")
  m$BioSample <- c("SAMN1", "")
  utils::write.csv(m[c("ID", "Taxon", "BioSample")], file.path(dir, "m2.csv"), row.names = FALSE)
  suppressMessages(update_sample_metadata(dir, file.path(dir, "m2.csv"), fetch_ncbi = FALSE))
  expect_equal(DBI::dbGetQuery(con, "SELECT ID FROM meta_links ORDER BY ID")$ID, "s2")
  expect_equal(DBI::dbGetQuery(con, "SELECT BioSample FROM samples ORDER BY ID")$BioSample, c("SAMN1", "SAMN1"))
  .meta_remove(con)
  expect_equal(DBI::dbGetQuery(con, "SELECT BioSample FROM samples ORDER BY ID")$BioSample, c("SAMN1", NA))
})

test_that("voucher search checks the museum, the total hit count, and odd vouchers", {
  local_mocked_bindings(.gbif_get = function(path) {
    if (grepl("occurrenceID=157636", path, fixed = TRUE)) {
      return(list(count = 1, results = list(c(gocc(9, "Psychrolutes paradoxus", "X"), institutionCode = "KAUM"))))
    }
    list(count = 0, results = list())
  })
  expect_null(.gbif_find_voucher("UW:157636", "Psychrolutes paradoxus"))
  local_mocked_bindings(.gbif_get = function(path) {
    list(count = 120, results = list(c(gocc(5, "Psychrolutes paradoxus", "F"), institutionCode = "UW")))
  })
  expect_match(.gbif_find_voucher("UW:157636", "Psychrolutes paradoxus")$note, "too many GBIF matches")
  n <- 0
  local_mocked_bindings(.gbif_get = function(path) { n <<- n + 1; list(count = 0, results = list()) })
  expect_null(.gbif_find_voucher("http://arctos.database.museum/guid/MVZ:Mamm:1", "x y"))
  expect_equal(n, 0)
  seen <- character()
  local_mocked_bindings(.gbif_get = function(path) {
    seen <<- c(seen, path)
    list(count = 1, results = list(c(gocc(7, "Aus bus", "Fish"), institutionCode = "MCZ")))
  })
  expect_equal(.gbif_find_voucher("MCZ::123", "Aus bus")$ref, "7")
})

test_that("linking skips costly lookups for databases the sample already has", {
  con <- link_con()
  DBI::dbExecute(con, "UPDATE samples SET GEOME_BCID = 'ark:/21547/T1', GBIF_ID = '77'")
  n <- new.env()
  local_mocked_bindings(
    .gbif_get = function(...) { n$gbif <- (n$gbif %||% 0) + 1; list(count = 0, results = list()) },
    .geome_get = function(...) { n$geome <- (n$geome %||% 0) + 1; list(children = list()) })
  r <- data.frame(level = c("Tissue", "Sample"), depth = 0:1, ref = "ark:/21547/T1",
                  field = c("bcid", "genbankSpecimenVoucher"), value = c("ark:/21547/T1", "UW:1"))
  .meta_link_candidates("GEOME", r, "x", want = character())
  expect_null(n$gbif)
  expect_null(n$geome)
})

test_that("old link notes clear when linking runs again", {
  con <- link_con()
  DBI::dbExecute(con, "INSERT INTO meta_links VALUES ('s1', 'GBIF', NULL, NULL, 'old note')")
  calls <- new.env()
  local_mocked_bindings(!!!mock_chains(calls))
  .meta_fetch_into(con, "NCBI", "s1", "SAMN1", link = TRUE)
  expect_false("old note" %in% DBI::dbGetQuery(con, "SELECT note FROM meta_links")$note)
})

test_that("a numeric gbifID and the EZID of the same GBIF record are not a disagreement", {
  con <- link_con()
  DBI::dbExecute(con, "UPDATE samples SET GBIF_ID = '1321689872'")
  DBI::dbAppendTable(con, "meta_records", data.frame(
    ID = "s1", source = "GBIF", level = "Occurrence", depth = 0L, ref = "1321689872",
    field = "occurrenceID", value = paste0("http://n2t.net/", ezid)))
  DBI::dbAppendTable(con, "meta_status", data.frame(ID = "s1", source = "GBIF", ref = "1321689872",
                                                    status = "ok", message = NA_character_, fetched_at = 1L))
  calls <- new.env()
  m <- mock_chains(calls)
  m$.ncbi_fetch_chain <- function(ref, cache) {
    data.frame(level = "BioSample", depth = 1L, ref = "SAMN1", field = "voucherURI",
               value = paste0("http://n2t.net/", ezid))
  }
  local_mocked_bindings(!!!m)
  .meta_fetch_into(con, "NCBI", "s1", "SAMN1", link = TRUE)
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM meta_links WHERE source = 'GBIF'")$n, 0L)
  expect_equal(DBI::dbGetQuery(con, "SELECT GBIF_ID FROM samples")$GBIF_ID, "1321689872")
})

test_that("two sources naming different records for a linked ID leave a note", {
  con <- link_con()
  DBI::dbExecute(con, "UPDATE samples SET GEOME_BCID = 'ark:/21547/T1'")
  calls <- new.env()
  m <- mock_chains(calls)
  m$.ncbi_fetch_chain <- function(ref, cache) {
    data.frame(level = "BioSample", depth = 1L, ref = "SAMN1", field = c("bcid", "voucherURI"),
               value = c("https://n2t.net/ark:/21547/T1", "http://n2t.net/ark:/65665/3bc380cef-b981-48ea-ac5f-0283b239833a"))
  }
  local_mocked_bindings(!!!m)
  .meta_fetch_into(con, "GEOME", "s1", "ark:/21547/T1")
  .meta_fetch_into(con, "NCBI", "s1", "SAMN1", link = TRUE)
  expect_equal(DBI::dbGetQuery(con, "SELECT GBIF_ID FROM samples")$GBIF_ID, ezid)
  l <- DBI::dbGetQuery(con, "SELECT ref, via, note FROM meta_links WHERE source = 'GBIF'")
  expect_equal(l$via, "GEOME Sample voucherURI")
  expect_match(l$note, "NCBI record links to ark:/65665/3bc380cef-b981-48ea-ac5f-0283b239833a.*kept ark:/65665/3dd003c5a")
  .meta_link_sample(con, "s1")
  expect_equal(nrow(DBI::dbGetQuery(con, "SELECT * FROM meta_links WHERE source = 'GBIF' AND note IS NOT NULL")), 1L)
})

test_that("vouchers parse as Darwin Core triplets, doublets, and INST CAT forms", {
  expect_equal(.parse_voucher("USNM:FISH:419933"), list(inst = "USNM", cat = "419933", coll = "FISH"))
  expect_equal(.parse_voucher("UW 157636")$cat, "157636")
  expect_equal(.parse_voucher("urn:catalog:UWFC:ADULT COLLECTION:UW 157636")$coll, "ADULT COLLECTION")
  expect_equal(.parse_voucher("FMNH:Mammal:1234 | MBG 33424")$cat, "1234")
  expect_null(.parse_voucher("http://n2t.net/ark:/65665/3abc"))
  expect_null(.parse_voucher("250399"))
})

test_that("sequence-derived GBIF datasets never link, and INST CAT doublets do", {
  insdc <- gocc(6189916995, "Psychrolutes paradoxus")
  insdc$datasetKey <- "d8cd16ba-bb74-4420-821e-083f2bac17c2"
  mined <- gocc(5860571592, "Psychrolutes paradoxus", basis = "MATERIAL_SAMPLE")
  mined$institutionCode <- "Mined from GenBank, NCBI"
  local_mocked_bindings(.gbif_get = function(path) list(results = list(insdc, mined)))
  expect_null(.gbif_find_voucher("UW:157636", "Psychrolutes paradoxus"))
  local_mocked_bindings(.gbif_get = function(path) {
    if (grepl("catalogNumber=UW%20157636", path, fixed = TRUE)) {
      return(list(results = list(insdc, gocc(2013211250, "Psychrolutes paradoxus", "ADULT COLLECTION"))))
    }
    list(results = list())
  })
  expect_equal(.gbif_find_voucher("UW 157636", "Psychrolutes paradoxus")$ref, "2013211250")
})

test_that("a collection code narrows hits but does not reject the only match", {
  hits <- list(results = list(gocc(1, "Fundulus majalis", "ADULT COLLECTION")))
  local_mocked_bindings(.gbif_get = function(path) hits)
  expect_equal(.gbif_find_voucher("USNM:FISH:419933", "Fundulus majalis")$ref, "1")
})

test_that("GEOME and NCBI records link to GBIF through more voucher fields", {
  local_mocked_bindings(.gbif_get = function(path) {
    if (grepl("UW", path, fixed = TRUE)) list(results = list(gocc(2013211250, "Psychrolutes paradoxus")))
    else list(results = list())
  }, .geome_get = function(path, query = list()) list(children = list()))
  r <- lrecs("Sample", c("institutionID", "voucherCatalogNumber"), c("UW", "157636"))
  c <- .meta_link_candidates("GEOME", r, "Psychrolutes paradoxus")
  expect_equal(c$GBIF$ref, "2013211250")
  expect_equal(c$GBIF$via, "GEOME Sample voucherCatalogNumber UW:157636")
  r <- lrecs("BioSample", "materialSampleID", "UW:157636")
  c <- .meta_link_candidates("NCBI", r, "Psychrolutes paradoxus")
  expect_equal(c$GBIF$via, "NCBI BioSample materialSampleID UW:157636")
})
