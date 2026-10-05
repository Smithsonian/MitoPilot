spec_ark <- "http://n2t.net/ark:/65665/30f303846-9d23-4069-881e-e43ff718ef9f"
tissue_ark <- "http://n2t.net/ark:/65665/381f83590-5526-4ac4-b3d8-2917bfc4d582"
bird_ark <- "http://n2t.net/ark:/65665/3f7264089-ae3e-4d03-9823-79be7676f8f5"

nmnh_occ <- function(occ_id, basis = "PRESERVED_SPECIMEN", inst = "USNM", coll = "FISH",
                     cat = "USNM 487075", parent = NULL, key = 1) {
  r <- list(key = key, occurrenceID = occ_id, basisOfRecord = basis,
            institutionCode = inst, collectionCode = coll, catalogNumber = cat)
  if (!is.null(parent)) {
    r$extensions <- list(`http://rs.tdwg.org/dwc/terms/ResourceRelationship` = list(list(
      `http://rs.tdwg.org/dwc/terms/relatedResourceID` = paste0(
        "institutionCode=USNM&collectionCode=Birds&catalogNumber=666459&accesspoint=x&guid=", parent))))
  }
  r
}
nmnh_gbif <- function(...) {
  occs <- list(...)
  function(path) {
    for (o in occs) {
      if (grepl(utils::URLencode(o$occurrenceID, reserved = TRUE), path, fixed = TRUE)) {
        return(list(count = 1, results = list(o)))
      }
    }
    list(count = 0, results = list())
  }
}
nmnh_db <- function(samples, recs = NULL) {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(con, "samples", samples)
  .meta_ensure_tables(con)
  if (!is.null(recs)) DBI::dbAppendTable(con, "meta_records", recs)
  con
}

test_that("template add, remove, and conflict detection", {
  t <- "{seqid} [organism={Taxon}] [specimen-voucher={v}] {Taxon} mitochondrion, {completeness}"
  expect_equal(nmnh_existing_mods(t), "[specimen-voucher={v}]")
  expect_equal(nmnh_existing_mods("{seqid} [Voucher URI=x] [specimen_voucher=y]"),
               c("[Voucher URI=x]", "[specimen_voucher=y]"))
  expect_length(nmnh_existing_mods(DEFAULT_FASTA_HEADER), 0)
  a <- nmnh_template_add(t)
  expect_equal(a, paste0("{seqid} [organism={Taxon}]", nmnh_tokens(),
                         " {Taxon} mitochondrion, {completeness}"))
  expect_true(nmnh_template_on(a))
  expect_false(nmnh_template_on(t))
  expect_equal(nmnh_template_add("{seqid} title"), paste0("{seqid} title", nmnh_tokens()))
  expect_equal(nmnh_template_remove(a), "{seqid} [organism={Taxon}] {Taxon} mitochondrion, {completeness}")
  edited <- sub("{nmnh_voucherURI}", "{x}", a, fixed = TRUE)
  expect_equal(nmnh_template_remove(edited), edited)
})

test_that("empty NMNH modifiers are stripped, others kept", {
  h <- "s1 [organism=X] [specimen_voucher=] [voucherURI= ] [note=] X"
  expect_equal(nmnh_strip_empty(h), "s1 [organism=X] [note=] X")
  expect_equal(nmnh_strip_empty("s1 [specimen_voucher=USNM:FISH:1] X"), "s1 [specimen_voucher=USNM:FISH:1] X")
})

test_that("duplicate NMNH modifiers block the header", {
  d <- data.frame(ID = "s1", seqid = "s1", Taxon = "X", v = "USNM:FISH:1")
  r <- validate_fasta_header("{seqid} [specimen_voucher={v}] [specimen-voucher={v}] {Taxon}", d)
  expect_false(r$ok)
  expect_match(r$message, "specimen_voucher")
  r <- validate_fasta_header("{seqid} [note={v}] [note={v}] {Taxon}", d)
  expect_true(r$ok)
  expect_equal(r$level, "warn")
})

test_that("vouchers normalise to NCBI USNM codes", {
  ok <- function(x, hint = NA) nmnh_normalize_voucher(x, hint)$value
  expect_equal(ok("USNM:FISH:487075"), "USNM:FISH:487075")
  for (k in c("FISH", "Birds", "MAMM", "Herp", "IZ", "ENT", "Botany")) {
    expect_equal(ok(paste0("USNM:", k, ":1")), paste0("USNM:", k, ":1"))
  }
  expect_equal(ok("US:3777091"), "US:3777091")
  expect_equal(ok("US:US:US 3777091"), "US:3777091")
  f <- nmnh_normalize_voucher("usnm:fish:USNM 487075")
  expect_equal(f$value, "USNM:FISH:487075")
  expect_true(f$fixed)
  expect_equal(ok("USNM:BIRDS:666459"), "USNM:Birds:666459")
  expect_equal(ok("USNM 487075", "FISH"), "USNM:FISH:487075")
  expect_true(is.na(ok("USNM 487075")))
  expect_match(nmnh_normalize_voucher("USNM 487075")$note, "collection")
  expect_true(is.na(ok("USNM:LAB:12345")))
  expect_match(nmnh_normalize_voucher("USNM:LAB:12345")$note, "bio_material")
  expect_true(is.na(ok("UW:157636")))
  expect_true(is.na(ok("USNM:FOO:1")))
  expect_true(is.na(ok("USNM:FISH:#1")))
  expect_true(is.na(ok(NA)))
})

test_that("URIs normalise from every accepted form", {
  for (x in c("ark:/65665/30f303846-9d23-4069-881e-e43ff718ef9f",
              "https://n2t.net/ark:/65665/30f3038469d234069881ee43ff718ef9f",
              "http://ezid.cdlib.org/id/ark:/65665/30f303846-9d23-4069-881e-e43ff718ef9f",
              "https://arks.org/ark:/65665/30F303846-9D23-4069-881E-E43FF718EF9F",
              "https://collections.nmnh.si.edu/search/fishes/?ark=ark:/65665/30f303846-9d23-4069-881e-e43ff718ef9f",
              "https://collections.nmnh.si.edu/search/fishes/?ark=ark%3A%2F65665%2F30f303846-9d23-4069-881e-e43ff718ef9f")) {
    expect_equal(nmnh_normalize_uri(x)$value, spec_ark, label = x)
  }
  m <- nmnh_normalize_uri("http://n2t.net/ark:/65665/m30f303846-9d23-4069-881e-e43ff718ef9f")
  expect_true(is.na(m$value))
  expect_match(m$note, "media")
  expect_true(is.na(nmnh_normalize_uri("ark:/21547/FDZ2UW")$value))
})

test_that("GBIF check: specimen, tissue to parent, no parent, no hit, offline", {
  con <- nmnh_db(data.frame(ID = "s1", Taxon = "X"))
  on.exit(DBI::dbDisconnect(con))
  local_mocked_bindings(.gbif_get = nmnh_gbif(
    nmnh_occ(spec_ark),
    nmnh_occ(tissue_ark, "MATERIAL_SAMPLE", parent = bird_ark),
    nmnh_occ(bird_ark, coll = "BIRDS", cat = "USNM 666459"),
    nmnh_occ("http://n2t.net/ark:/65665/3aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa", "MATERIAL_SAMPLE")))
  s <- nmnh_gbif_check(spec_ark, con)
  expect_equal(s$basis, "PRESERVED_SPECIMEN")
  expect_equal(s$voucher, "USNM:FISH:487075")
  t <- nmnh_gbif_check(tissue_ark, con)
  expect_equal(t$parent, bird_ark)
  n <- nmnh_gbif_check("http://n2t.net/ark:/65665/3aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa", con)
  expect_equal(n$basis, "MATERIAL_SAMPLE")
  expect_true(is.na(n$parent))
  expect_true(is.na(nmnh_gbif_check("http://n2t.net/ark:/65665/3bbbbbbbb-aaaa-aaaa-aaaa-aaaaaaaaaaaa", con)$basis))
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM nmnh_ark_cache")$n, 4L)
  local_mocked_bindings(.gbif_get = function(path) stop("could not reach GBIF"))
  expect_equal(nmnh_gbif_check(spec_ark, con)$basis, "PRESERVED_SPECIMEN")
  expect_null(nmnh_gbif_check("http://n2t.net/ark:/65665/3cccccccc-aaaa-aaaa-aaaa-aaaaaaaaaaaa", con))
})

test_that("resolve follows source priority and cross-fills", {
  samples <- data.frame(ID = c("s1", "s2", "s3", "s4", "s5"), Taxon = "Fundulus majalis",
                        specimen_voucher = c("USNM:LAB:1", NA, NA, "USNM:FISH:487075", NA),
                        voucherURI = c(NA, tissue_ark, spec_ark, NA, NA))
  recs <- rbind(
    data.frame(ID = "s1", source = "GBIF", level = "Occurrence", depth = 0L, ref = "1",
               field = c("institutionCode", "collectionCode", "catalogNumber", "occurrenceID"),
               value = c("USNM", "FISH", "USNM 487075", spec_ark)),
    data.frame(ID = "s5", source = "NCBI", level = "BioSample", depth = 0L, ref = "SAMN1",
               field = c("specimen_voucher", "note"), value = c("USNM:FISH:487075", paste("see", spec_ark))))
  con <- nmnh_db(samples, recs)
  on.exit(DBI::dbDisconnect(con))
  local_mocked_bindings(.gbif_get = function(path) {
    if (grepl("catalogNumber=", path, fixed = TRUE)) {
      return(list(count = 1, results = list(list(key = 9, scientificName = "Fundulus majalis",
                                                  collectionCode = "FISH",
                                                  basisOfRecord = "PRESERVED_SPECIMEN"))))
    }
    if (path == "occurrence/9") return(list(key = 9, occurrenceID = spec_ark))
    nmnh_gbif(nmnh_occ(spec_ark),
              nmnh_occ(tissue_ark, "MATERIAL_SAMPLE", parent = bird_ark),
              nmnh_occ(bird_ark, coll = "BIRDS", cat = "USNM 666459"))(path)
  })
  r <- nmnh_resolve(con, samples$ID)
  expect_equal(r$ID, samples$ID)
  # s1: mapfile LAB rejected, GBIF supplies both
  expect_equal(r$nmnh_specimen_voucher[1], "USNM:FISH:487075")
  expect_equal(r$voucher_source[1], "GBIF")
  expect_match(r$voucher_note[1], "LAB")
  expect_true(r$ok[1])
  # s2: tissue ARK replaced with parent, voucher derived from it
  expect_equal(r$nmnh_voucherURI[2], bird_ark)
  expect_match(r$uri_note[2], "tissue ARK replaced")
  expect_equal(r$nmnh_specimen_voucher[2], "USNM:Birds:666459")
  expect_equal(r$voucher_source[2], "derived from URI")
  expect_true(r$ok[2] && r$fixed[2])
  # s3: voucher derived from URI
  expect_equal(r$nmnh_specimen_voucher[3], "USNM:FISH:487075")
  # s4: URI derived from voucher
  expect_equal(r$nmnh_voucherURI[4], spec_ark)
  expect_equal(r$uri_source[4], "derived from voucher")
  # s5: NCBI voucher, ARK found in any attribute
  expect_equal(r$voucher_source[5], "NCBI")
  expect_equal(r$nmnh_voucherURI[5], spec_ark)
  expect_true(all(r$ok))
})

test_that("resolve reports missing values and mismatches; offline skips checks", {
  samples <- data.frame(ID = c("s1", "s2"), Taxon = "X",
                        voucher = c("USNM:FISH:1", NA), ark = c(spec_ark, NA))
  con <- nmnh_db(samples)
  on.exit(DBI::dbDisconnect(con))
  local_mocked_bindings(.gbif_get = nmnh_gbif(nmnh_occ(spec_ark)))
  r <- nmnh_resolve(con, samples$ID)
  expect_false(r$ok[1])
  expect_match(r$voucher_note[1], "does not match")
  expect_false(r$ok[2])
  expect_true(is.na(r$nmnh_specimen_voucher[2]))
  expect_match(r$voucher_note[2], "missing")
  local_mocked_bindings(.gbif_get = function(path) stop("offline"))
  DBI::dbExecute(con, "DELETE FROM nmnh_ark_cache")
  r <- nmnh_resolve(con, "s1")
  expect_true(r$ok)
  expect_false(r$checked)
  nmnh_set_columns(con, voucher = "", uri = "ark")
  expect_equal(nmnh_columns(con), list(voucher = NA_character_, uri = "ark"))
})
