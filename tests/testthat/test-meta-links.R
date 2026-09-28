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
