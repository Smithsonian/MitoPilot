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

test_that("modifiers with empty, NA, or absent values are left out", {
  d <- data.frame(ID = "s1", Taxon = "X", v = "", u = " ", n = NA, k = "USNM:FISH:1")
  h <- "{ID} [organism={Taxon}] [specimen_voucher={v}] [voucherURI={u}] [note={n}] [country={gone}] {Taxon}"
  expect_equal(as.character(header_fill(d, h)), "s1 [organism=X] X")
  expect_equal(as.character(header_fill(d, "{ID} [specimen_voucher={k}] X")), "s1 [specimen_voucher=USNM:FISH:1] X")
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
  expect_equal(r$voucher_source[2], "GBIF via specimen link")
  expect_true(r$ok[2] && r$fixed[2])
  # s3: voucher derived from URI
  expect_equal(r$nmnh_specimen_voucher[3], "USNM:FISH:487075")
  # s4: URI derived from voucher
  expect_equal(r$nmnh_voucherURI[4], spec_ark)
  expect_equal(r$uri_source[4], "GBIF via catalog number")
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

test_that("export writes NMNH values and drops the empty modifier", {
  d <- withr::local_tempdir()
  out_dir <- file.path(d, "out")
  dir.create(out_dir)
  withr::with_seed(1, sq <- paste(sample(c("A", "C", "G", "T"), 1200, TRUE), collapse = ""))
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(d, ".sqlite"))
  DBI::dbWriteTable(con, "annotations", data.frame(
    ID = "s1", path = 1L, scaffold = 1L, contig = "s1.1.1", type = "PCG", gene = "cox1",
    product = "cox1", pos1 = 100L, pos2 = 700L, length = 601L, direction = "+",
    start_codon = "ATG", stop_codon = "TAA", translation = strrep("M", 30L),
    anticodon = NA_character_, partial_start = 0L, partial_stop = 0L, notes = NA_character_,
    refHits = "{}", warnings = NA_character_))
  DBI::dbWriteTable(con, "assemblies", data.frame(ID = "s1", path = 1L, scaffold = 1L, ignore = 0L,
                                                  topology = "circular", sequence = sq))
  DBI::dbWriteTable(con, "export", data.frame(ID = "s1", path = 1L, scaffold = 1L,
                                              export_group = "g1", export_time_stamp = NA_integer_))
  DBI::dbWriteTable(con, "annotate", data.frame(ID = "s1", path = 1L, scaffold = 1L, topology = "circular",
                                                partial = "no", curate_opts = "default"))
  DBI::dbWriteTable(con, "curate_opts", data.frame(curate_opts = "default", params = "{}", linear_complete = 0L))
  DBI::dbWriteTable(con, "samples", data.frame(ID = "s1", Taxon = "Testus testus", genetic_code = 2L,
                                               specimen_voucher = "usnm:fish:1"))
  DBI::dbWriteTable(con, "assemble", data.frame(ID = "s1", blast_accession = "NC_000001",
                                                blast_accession_auto = 0L, poor_blast_ref = "ok"))
  DBI::dbDisconnect(con)
  local_mocked_bindings(.gbif_get = function(path) stop("offline"))
  suppressMessages(export_files(
    group = "g1", out_dir = out_dir, generateAAalignments = FALSE, gene_export = TRUE,
    review = FALSE, summary_csv = FALSE,
    fasta_header = nmnh_template_add(DEFAULT_FASTA_HEADER),
    fasta_header_gene = nmnh_template_add(DEFAULT_FASTA_HEADER_GENE)))
  h <- grep("^>", readLines(file.path(out_dir, "export", "g1", "g1.fasta")), value = TRUE)
  expect_match(h, "[location=mitochondrion] [specimen_voucher=USNM:FISH:1] Testus", fixed = TRUE)
  expect_false(grepl("voucherURI", h))
  g <- list.files(file.path(out_dir, "s1", "export"), "_cox1[.]fasta$", full.names = TRUE)
  expect_match(readLines(g)[1], "[specimen_voucher=USNM:FISH:1] Testus testus, cox1", fixed = TRUE)
})

test_that("missing_fields leaves NMNH tokens to the NMNH status line", {
  d <- data.frame(ID = "a", nmnh_voucherURI = "")
  expect_null(missing_fields("{ID} [voucherURI={nmnh_voucherURI}]", d))
})

test_that("user values live in nmnh_vouchers and win over automatic ones", {
  samples <- data.frame(ID = c("s1", "s2"), Taxon = "Fundulus majalis",
                        catalog_no = c("USNM:FISH:1", NA))
  con <- nmnh_db(samples)
  on.exit(DBI::dbDisconnect(con))
  nmnh_set_columns(con, "catalog_no", "")
  r <- nmnh_resolve(con, samples$ID, online = FALSE)
  expect_equal(r$voucher_source[1], "mapfile:catalog_no")
  st <- DBI::dbReadTable(con, "nmnh_vouchers")
  expect_equal(st$specimen_voucher_source[st$ID == "s1"], "mapfile:catalog_no")

  expect_null(nmnh_edit_value(con, "s1", "voucher", "USNM:FISH:99"))
  expect_match(nmnh_edit_value(con, "s2", "voucher", "nonsense"), "not a voucher")
  r <- nmnh_resolve(con, samples$ID, online = FALSE)
  expect_equal(r$nmnh_specimen_voucher[1], "USNM:FISH:99")
  expect_equal(r$voucher_source[1], "entered")
  # the mapping-file column is read, never written
  expect_equal(DBI::dbGetQuery(con, "SELECT catalog_no FROM samples WHERE ID = 's1'")[[1]], "USNM:FISH:1")

  # clearing goes back to automatic
  expect_null(nmnh_edit_value(con, "s1", "voucher", ""))
  r <- nmnh_resolve(con, samples$ID, online = FALSE)
  expect_equal(r$voucher_source[1], "mapfile:catalog_no")
})

test_that("an edited voucher CSV loads into nmnh_vouchers only", {
  samples <- data.frame(ID = c("s1", "s2"), Taxon = "Fundulus majalis")
  con <- nmnh_db(samples)
  on.exit(DBI::dbDisconnect(con))
  f <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(data.frame(ID = c("s1", "s2", "zz"), Taxon = "x",
                              specimen_voucher = c("USNM:FISH:5", "bad", "USNM:FISH:6"),
                              voucherURI = ""), f, row.names = FALSE)
  res <- nmnh_upload_csv(con, f)
  expect_equal(res$ids, "s1")
  expect_length(res$bad, 2)
  st <- DBI::dbReadTable(con, "nmnh_vouchers")
  expect_equal(st$specimen_voucher_source[st$ID == "s1"], "upload")
  expect_false(any(c("specimen_voucher", "voucherURI") %in% DBI::dbListFields(con, "samples")))
})

test_that("not-NMNH samples skip checks and chosen fields replace automatic sources", {
  con <- nmnh_db(data.frame(ID = c("s1", "s2"), Taxon = "X", voucher = c("MCZ:Ich:1", "USNM:FISH:1"),
                            alt = c("KU:5", "USNM:FISH:2"), uri = NA_character_))
  on.exit(DBI::dbDisconnect(con))
  nmnh_set_not_nmnh(con, "s1", TRUE)
  r <- nmnh_resolve(con, c("s1", "s2"), online = FALSE, save = FALSE)
  expect_true(r$ok[1])
  expect_true(r$not_nmnh[1])
  expect_equal(r$nmnh_specimen_voucher[1], "MCZ:Ich:1")
  expect_true(is.na(r$nmnh_voucherURI[1]))
  expect_false(r$ok[2])
  expect_null(nmnh_edit_value(con, "s1", "uri", "https://example.org/1"))
  nmnh_set_field(con, "s1", "voucher", "map:alt")
  nmnh_set_field(con, "s2", "voucher", "map:alt")
  r <- nmnh_resolve(con, c("s1", "s2"), online = FALSE, save = FALSE)
  expect_equal(r$nmnh_specimen_voucher, c("KU:5", "USNM:FISH:2"))
  expect_equal(r$nmnh_voucherURI[1], "https://example.org/1")
  expect_equal(r$voucher_source[2], "field:map:alt")
  expect_true("map:alt" %in% nmnh_field_choices(con))
  nmnh_set_not_nmnh(con, "s1", FALSE)
  r <- nmnh_resolve(con, "s1", online = FALSE, save = FALSE)
  expect_false(r$ok)
})
