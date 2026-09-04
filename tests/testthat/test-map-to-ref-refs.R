mtr_ref_fasta <- function(dir, name = "ref.fasta", reps = 3000L) {
  fn <- file.path(dir, name)
  writeLines(c(">TESTREF", strrep("ACGT", reps)), fn)
  fn
}

test_that(".mtr_ref_class separates urls, accessions, paths, and blanks", {
  expect_equal(.mtr_ref_class("https://example.org/ref.gb"), "url")
  expect_equal(.mtr_ref_class("http://example.org/ref.gb"), "url")
  expect_equal(.mtr_ref_class("ftp://ftp.ncbi.nlm.nih.gov/x.gb"), "url")
  expect_equal(.mtr_ref_class("NC_002333"), "accession")
  expect_equal(.mtr_ref_class("NC_002333.1"), "accession")
  expect_equal(.mtr_ref_class("nc_002333.1"), "accession")
  expect_equal(.mtr_ref_class("AB123456"), "accession")
  expect_equal(.mtr_ref_class("MN908947.3"), "accession")
  expect_equal(.mtr_ref_class("U12345"), "accession")
  expect_equal(.mtr_ref_class("NC_002333.gb"), "file")
  expect_equal(.mtr_ref_class("ref/NC_002333.gb"), "file")
  expect_equal(.mtr_ref_class("/data/refs/NC_002333.gb"), "file")
  expect_equal(.mtr_ref_class("auto"), "file")
  expect_equal(.mtr_ref_class(""), "none")
  expect_equal(.mtr_ref_class("   "), "none")
  expect_equal(.mtr_ref_class(NA_character_), "none")
  expect_equal(.mtr_ref_class(NULL), "none")
})

test_that(".mtr_validate_refs normalises a real file to an absolute path", {
  d <- withr::local_tempdir()
  fa <- mtr_ref_fasta(d)
  rel <- file.path(basename(d), "ref.fasta")
  withr::local_dir(dirname(d))
  out <- .mtr_validate_refs(c(S1 = rel), ids = "S1")
  expect_equal(out, normalizePath(fa, winslash = "/"))
})

test_that(".mtr_validate_refs stores blanks as NA and leaves them alone", {
  out <- .mtr_validate_refs(c("", "   ", NA_character_), ids = c("S1", "S2", "S3"))
  expect_equal(out, rep(NA_character_, 3L))
})

test_that(".mtr_validate_refs reports every bad value at once, with sample IDs", {
  d <- withr::local_tempdir()
  testthat::local_mocked_bindings(
    .mtr_ncbi_known = function(accs, ...) list(ok = TRUE, found = character(0))
  )
  err <- expect_error(
    .mtr_validate_refs(
      c(file.path(d, "nope.gb"), "NC_999999", d),
      ids = c("S1", "S2", "S3")
    ),
    "problems \\(3\\)"
  )
  msg <- conditionMessage(err)
  expect_match(msg, "S1 \\[.*nope\\.gb\\]: file not found")
  expect_match(msg, "S2 \\[NC_999999\\]: no such nucleotide record at NCBI")
  expect_match(msg, "S3 \\[.*\\]: is a directory, not a file")
  # Row order, not the order the class batches run in.
  expect_match(msg, "S1 \\[[^\n]*\n  S2 \\[[^\n]*\n  S3 \\[")
})

test_that(".mtr_validate_refs refuses shell metacharacters in a reference value", {
  # The value lands inside Rscript -e "..." in a bash double-quoted string, so a
  # quote ends the R expression and $ / backtick / backslash reach bash.
  expect_error(.mtr_validate_refs("/data/it's/ref.gb", ids = "S1"), "not allowed")
  expect_error(.mtr_validate_refs("/data/\"q\"/ref.gb", ids = "S1"), "not allowed")
  expect_error(.mtr_validate_refs("/data/$HOME/ref.gb", ids = "S1"), "not allowed")
  expect_error(.mtr_validate_refs("/data/a`id`b/ref.gb", ids = "S1"), "not allowed")
  expect_error(.mtr_validate_refs("/data/a\\b/ref.gb", ids = "S1"), "not allowed")
})

test_that(".mtr_esummary_found reads a hit, a miss, and a non-esummary body", {
  # Body shapes recorded from the live endpoint.
  hit <- paste0(
    '{"header":{},"result":{"uids":["15079186"],',
    '"15079186":{"uid":"15079186","caption":"NC_002333",',
    '"accessionversion":"NC_002333.2","slen":16596,"topology":"circular"}}}'
  )
  expect_equal(.mtr_esummary_found(hit), "NC_002333")
  # A nonexistent accession: HTTP 200, a top-level "error", an empty uid list.
  expect_equal(
    .mtr_esummary_found(paste0(
      '{"header":{"type":"esummary","version":"0.3"},',
      '"error":"Invalid uid NC_999999999 at position= 0","result":{"uids":[]}}'
    )),
    character(0)
  )
  expect_equal(.mtr_esummary_found('{"esummaryresult":["Empty id list - nothing todo"]}'),
               character(0))
  expect_null(.mtr_esummary_found("<html>502 Bad Gateway</html>"))
  # Rate limited: 200, no result and no esummaryresult, so unreadable not a miss.
  expect_null(.mtr_esummary_found(
    '{"error":"API rate limit exceeded","api-key":"1.2.3.4","count":"4","limit":"3"}'
  ))
  expect_null(.mtr_esummary_found("502"))
})

test_that(".mtr_validate_refs rejects a file whose content is not a reference", {
  d <- withr::local_tempdir()
  bad <- file.path(d, "notes.txt")
  writeLines(c("hello", "world"), bad)
  expect_error(.mtr_validate_refs(bad, ids = "S1"), "LOCUS")
})

test_that(".mtr_validate_refs rejects an unreadable file", {
  skip_on_os("windows")
  skip_if(identical(Sys.info()[["effective_user"]], "root"), "running as root")
  d <- withr::local_tempdir()
  fa <- mtr_ref_fasta(d)
  Sys.chmod(fa, "0000")
  on.exit(Sys.chmod(fa, "0600"), add = TRUE)
  expect_error(.mtr_validate_refs(fa, ids = "S1"), "not readable")
})

test_that(".mtr_validate_refs uppercases an accession that exists", {
  testthat::local_mocked_bindings(
    .mtr_ncbi_known = function(accs, ...) list(ok = TRUE, found = "NC_002333")
  )
  expect_equal(.mtr_validate_refs("nc_002333.1", ids = "S1"), "NC_002333.1")
})

test_that("an unreachable NCBI is a warning and the value is kept", {
  testthat::local_mocked_bindings(
    .mtr_ncbi_known = function(accs, ...) list(ok = FALSE, reason = "timed out")
  )
  expect_warning(out <- .mtr_validate_refs("NC_002333", ids = "S1"),
                 "resolved when the pipeline runs")
  expect_equal(out, "NC_002333")
})

test_that("a checked accession list is batched into one request", {
  calls <- 0L
  testthat::local_mocked_bindings(
    .mtr_ncbi_known = function(accs, ...) {
      calls <<- calls + 1L
      list(ok = TRUE, found = c("NC_002333", "AB123456"))
    }
  )
  .mtr_validate_refs(c("NC_002333", "AB123456", "NC_002333"),
                     ids = c("S1", "S2", "S3"))
  expect_equal(calls, 1L)
})

test_that("a url is fetched, content-checked, and stored as the url", {
  d <- withr::local_tempdir()
  fa <- mtr_ref_fasta(d)
  testthat::local_mocked_bindings(
    .mtr_url_fetch = function(url, ...) list(ok = TRUE, file = fa)
  )
  expect_equal(.mtr_validate_refs("https://example.org/r.fa", ids = "S1"),
               "https://example.org/r.fa")
})

test_that("an unreachable url is an error naming the reason", {
  testthat::local_mocked_bindings(
    .mtr_url_fetch = function(url, ...) list(ok = FALSE, reason = "HTTP 404")
  )
  expect_error(.mtr_validate_refs("https://example.org/r.fa", ids = "S1"),
               "not reachable: HTTP 404")
})

test_that("an ftp url is refused with the escape hatch in the message", {
  expect_error(
    .mtr_validate_refs("ftp://ftp.ncbi.nlm.nih.gov/x.gb", ids = "S1"),
    "https"
  )
})

test_that("a distinct file value is content-checked only once", {
  d <- withr::local_tempdir()
  fa <- mtr_ref_fasta(d)
  calls <- 0L
  testthat::local_mocked_bindings(
    .mtr_content_problem = function(file) {
      calls <<- calls + 1L
      NA_character_
    }
  )
  .mtr_validate_refs(c(fa, fa, fa), ids = c("S1", "S2", "S3"))
  expect_equal(calls, 1L)
})

mtr_refs_mapping <- function(dir, refs = NULL, ids = c("S1", "S2")) {
  m <- data.frame(
    ID = ids,
    Taxon = "Danio rerio",
    R1 = paste0(ids, "_R1.fastq.gz"),
    R2 = paste0(ids, "_R2.fastq.gz")
  )
  if (!is.null(refs)) m$Reference <- refs
  fn <- file.path(dir, "mapping.csv")
  utils::write.csv(m, fn, row.names = FALSE)
  fn
}

test_that("new_db warns instead of demanding a reference or a topology", {
  # The trap: deleting the reference-required stop must not leave the
  # FASTA-topology check firing on an NA reference.
  d <- withr::local_tempdir()
  mapping <- mtr_refs_mapping(d)
  db <- file.path(d, ".sqlite")
  expect_warning(
    new_db(db_path = db, mapping_fn = mapping, assembler = "MapToRef"),
    "no reference"
  )
  expect_true(file.exists(db))
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_equal(DBI::dbGetQuery(con, "SELECT maptoref_ref FROM assemble")$maptoref_ref,
               c(NA_character_, NA_character_))
})

test_that("new_db seeds assemble.maptoref_ref from the Reference column", {
  d <- withr::local_tempdir()
  fa <- mtr_ref_fasta(d)
  mapping <- mtr_refs_mapping(d, refs = c(fa, ""))
  db <- file.path(d, ".sqlite")
  expect_warning(
    new_db(db_path = db, mapping_fn = mapping, assembler = "MapToRef"),
    "S2"
  )
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_false("Reference" %in% DBI::dbListFields(con, "samples"))
  a <- DBI::dbGetQuery(con, "SELECT ID, maptoref_ref FROM assemble ORDER BY ID")
  expect_equal(a$maptoref_ref,
               c(normalizePath(fa, winslash = "/"), NA_character_))
})

test_that("new_db does not warn when every sample has a reference", {
  d <- withr::local_tempdir()
  fa <- mtr_ref_fasta(d)
  mapping <- mtr_refs_mapping(d, refs = c(fa, fa))
  expect_no_warning(
    new_db(db_path = file.path(d, ".sqlite"), mapping_fn = mapping,
           assembler = "MapToRef")
  )
})

test_that("the option-set reference covers samples that have none of their own", {
  d <- withr::local_tempdir()
  fa <- mtr_ref_fasta(d)
  mapping <- mtr_refs_mapping(d, refs = c("", ""))
  expect_no_warning(
    new_db(db_path = file.path(d, ".sqlite"), mapping_fn = mapping,
           assembler = "MapToRef", maptoref_ref = fa,
           maptoref_topology = "circular")
  )
})

test_that("a bad Reference value and a bad option-set value are reported together", {
  d <- withr::local_tempdir()
  mapping <- mtr_refs_mapping(d, refs = c(file.path(d, "a.gb"), ""))
  err <- expect_error(
    new_db(db_path = file.path(d, ".sqlite"), mapping_fn = mapping,
           assembler = "MapToRef", maptoref_ref = file.path(d, "b.gb")),
    "problems \\(2\\)"
  )
  expect_match(conditionMessage(err), "assemble options \\[.*b\\.gb\\]")
  expect_match(conditionMessage(err), "S1 \\[.*a\\.gb\\]")
  expect_false(file.exists(file.path(d, ".sqlite")))
})

test_that("new_db still demands a topology for a FASTA option-set reference", {
  d <- withr::local_tempdir()
  fa <- mtr_ref_fasta(d)
  mapping <- mtr_refs_mapping(d)
  expect_error(
    new_db(db_path = file.path(d, ".sqlite"), mapping_fn = mapping,
           assembler = "MapToRef", maptoref_ref = fa),
    "maptoref_topology"
  )
})

test_that("new_db does not demand a topology for an accession", {
  d <- withr::local_tempdir()
  mapping <- mtr_refs_mapping(d)
  testthat::local_mocked_bindings(
    .mtr_ncbi_known = function(accs, ...) list(ok = TRUE, found = "NC_002333")
  )
  expect_no_error(
    new_db(db_path = file.path(d, ".sqlite"), mapping_fn = mapping,
           assembler = "MapToRef", maptoref_ref = "NC_002333")
  )
})

test_that("add_samples seeds the reference and never adds a samples column", {
  d <- withr::local_tempdir()
  fa <- mtr_ref_fasta(d)
  new_db(db_path = file.path(d, ".sqlite"),
         mapping_fn = mtr_refs_mapping(d, ids = c("S1", "S2")))
  add_fn <- mtr_refs_mapping(file.path(d), refs = fa, ids = c("S3", "S4"))
  add_samples(path = d, update_mapping_fn = add_fn)

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(d, ".sqlite"))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_false("Reference" %in% DBI::dbListFields(con, "samples"))
  a <- DBI::dbGetQuery(con, "SELECT ID, maptoref_ref FROM assemble ORDER BY ID")
  expect_equal(a$maptoref_ref[a$ID %in% c("S3", "S4")],
               rep(normalizePath(fa, winslash = "/"), 2L))
})

test_that("a Reference column is reserved for every assembler", {
  # Stripped and validated whatever the assembler, so assemble.maptoref_ref can
  # never hold garbage a later assembler switch would pick up.
  d <- withr::local_tempdir()
  testthat::local_mocked_bindings(
    .mtr_ncbi_known = function(accs, ...) list(ok = TRUE, found = "NC_002333")
  )
  mapping <- mtr_refs_mapping(d, refs = c("NC_002333", "NC_002333"))
  db <- file.path(d, ".sqlite")
  expect_no_warning(
    new_db(db_path = db, mapping_fn = mapping, assembler = "GetOrganelle")
  )
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_false("Reference" %in% DBI::dbListFields(con, "samples"))
  expect_equal(DBI::dbGetQuery(con, "SELECT maptoref_ref FROM assemble")$maptoref_ref,
               c("NC_002333", "NC_002333"))
})

test_that("add_samples refuses a project that predates the reference column", {
  d <- withr::local_tempdir()
  new_db(db_path = file.path(d, ".sqlite"),
         mapping_fn = mtr_refs_mapping(d, ids = c("S1", "S2")))
  con0 <- DBI::dbConnect(RSQLite::SQLite(), file.path(d, ".sqlite"))
  DBI::dbExecute(con0, "ALTER TABLE assemble DROP COLUMN maptoref_ref")
  DBI::dbDisconnect(con0)

  add_fn <- mtr_refs_mapping(d, ids = c("S3", "S4"))
  expect_error(add_samples(path = d, update_mapping_fn = add_fn),
               "backwards_compatibility")

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(d, ".sqlite"))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  # Nothing was half-applied: the guard runs before the samples write.
  expect_false(any(c("S3", "S4") %in%
                     DBI::dbGetQuery(con, "SELECT ID FROM samples")$ID))
})

test_that("update_sample_metadata strips a Reference column with a message", {
  d <- withr::local_tempdir()
  fa <- mtr_ref_fasta(d)
  new_db(db_path = file.path(d, ".sqlite"), mapping_fn = mtr_refs_mapping(d))
  upd <- file.path(d, "upd.csv")
  utils::write.csv(
    data.frame(ID = c("S1", "S2"), Taxon = "Danio rerio", Reference = fa),
    upd, row.names = FALSE
  )
  expect_message(update_sample_metadata(path = d, update_mapping_fn = upd),
                 "set_maptoref_refs")
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(d, ".sqlite"))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_false("Reference" %in% DBI::dbListFields(con, "samples"))
  expect_equal(DBI::dbGetQuery(con, "SELECT maptoref_ref FROM assemble")$maptoref_ref,
               c(NA_character_, NA_character_))
})

mtr_refs_project <- function(dir, ids = c("S1", "S2"), ...) {
  new_db(db_path = file.path(dir, ".sqlite"),
         mapping_fn = mtr_refs_mapping(dir, ids = ids), ...)
  file.path(dir, ".sqlite")
}

test_that("set_maptoref_refs writes the column and flips the switch", {
  d <- withr::local_tempdir()
  fa <- mtr_ref_fasta(d)
  db <- mtr_refs_project(d)
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "UPDATE assemble SET assemble_switch = 2")

  set_maptoref_refs(d, data.frame(a = "S1", b = fa))

  a <- DBI::dbGetQuery(con, "SELECT ID, maptoref_ref, assemble_switch FROM assemble ORDER BY ID")
  expect_equal(a$maptoref_ref, c(normalizePath(fa, winslash = "/"), NA_character_))
  expect_equal(a$assemble_switch, c(1, 2))
})

test_that("set_maptoref_refs reads a CSV by position, ignoring header names", {
  d <- withr::local_tempdir()
  fa <- mtr_ref_fasta(d)
  db <- mtr_refs_project(d)
  csv <- file.path(d, "refs.csv")
  utils::write.csv(data.frame(sample = c("S1", "S2"), whatever = fa),
                   csv, row.names = FALSE)
  set_maptoref_refs(d, csv)

  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_equal(DBI::dbGetQuery(con, "SELECT maptoref_ref FROM assemble")$maptoref_ref,
               rep(normalizePath(fa, winslash = "/"), 2L))
})

test_that("set_maptoref_refs does not re-queue an unchanged row", {
  d <- withr::local_tempdir()
  fa <- mtr_ref_fasta(d)
  db <- mtr_refs_project(d)
  set_maptoref_refs(d, data.frame(a = "S1", b = fa))
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "UPDATE assemble SET assemble_switch = 2")

  expect_message(set_maptoref_refs(d, data.frame(a = "S1", b = fa)), "No changes")
  expect_equal(DBI::dbGetQuery(con, "SELECT assemble_switch FROM assemble WHERE ID = 'S1'")$assemble_switch, 2)
})

test_that("a blank value clears the per-sample reference", {
  d <- withr::local_tempdir()
  fa <- mtr_ref_fasta(d)
  db <- mtr_refs_project(d)
  set_maptoref_refs(d, data.frame(a = "S1", b = fa))
  set_maptoref_refs(d, data.frame(a = "S1", b = ""))

  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_true(is.na(DBI::dbGetQuery(
    con, "SELECT maptoref_ref FROM assemble WHERE ID = 'S1'")$maptoref_ref))
})

test_that("set_maptoref_refs refuses unknown IDs, duplicates, and locked rows", {
  d <- withr::local_tempdir()
  fa <- mtr_ref_fasta(d)
  db <- mtr_refs_project(d)
  expect_error(set_maptoref_refs(d, data.frame(a = "NOPE", b = fa)), "NOPE")
  expect_error(set_maptoref_refs(d, data.frame(a = c("S1", "S1"), b = fa)),
               "Duplicate")
  expect_error(set_maptoref_refs(d, d), "refs CSV not found")

  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "UPDATE assemble SET assemble_lock = 1 WHERE ID = 'S2'")
  expect_error(set_maptoref_refs(d, data.frame(a = "S2", b = fa)), "locked")
})

test_that("a locked row the call would not change is left alone", {
  d <- withr::local_tempdir()
  fa <- mtr_ref_fasta(d)
  db <- mtr_refs_project(d)
  set_maptoref_refs(d, data.frame(a = c("S1", "S2"), b = fa))
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "UPDATE assemble SET assemble_lock = 1 WHERE ID = 'S2'")

  expect_message(set_maptoref_refs(d, data.frame(a = c("S1", "S2"), b = fa)),
                 "No changes")
})

test_that("set_maptoref_refs validates values before writing anything", {
  d <- withr::local_tempdir()
  db <- mtr_refs_project(d)
  expect_error(
    set_maptoref_refs(d, data.frame(a = "S1", b = file.path(d, "nope.gb"))),
    "file not found"
  )
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_true(is.na(DBI::dbGetQuery(
    con, "SELECT maptoref_ref FROM assemble WHERE ID = 'S1'")$maptoref_ref))
})

test_that("set_maptoref_refs warns about samples still without a reference", {
  d <- withr::local_tempdir()
  fa <- mtr_ref_fasta(d)
  # Creating a MapToRef project with no reference warns by design; an uncaught
  # warning inside test_that() would be counted in the WARN column.
  db <- suppressWarnings(mtr_refs_project(d, assembler = "MapToRef"))
  expect_warning(still <- set_maptoref_refs(d, data.frame(a = "S1", b = fa)), "S2")
  expect_equal(still, "S2")
})

test_that("maptoref_fetch_accession downloads the GenBank record", {
  skip_on_os("windows")
  d <- withr::local_tempdir()
  testthat::local_mocked_bindings(
    .mtr_efetch_gb = function(acc) paste0("LOCUS  ", acc, "  16596 bp DNA circular VRT\n//\n")
  )

  got <- maptoref_fetch_accession("nc_002333.1", out_dir = d)
  expect_equal(got$source, "ncbi")
  expect_equal(got$accession, "NC_002333.1")
  expect_true(file.exists(got$file))
  expect_true(grepl("^LOCUS", readLines(got$file)[1]))
  expect_equal(basename(dirname(got$file)), "maptoref")
})

test_that("an unresolvable accession is an error naming the accession", {
  skip_on_os("windows")
  d <- withr::local_tempdir()
  testthat::local_mocked_bindings(
    .mtr_efetch_gb = function(acc) stop("network unreachable")
  )
  log_fn <- file.path(d, "assembler.log.txt")
  file.create(log_fn)
  expect_error(
    maptoref_fetch_accession("ZZ999999", out_dir = d, log_fn = log_fn),
    "ZZ999999"
  )
  expect_true(any(grepl("NCBI fetch failed for ZZ999999: network unreachable",
                        readLines(log_fn), fixed = TRUE)))
})

test_that("the resolver logs its attempt when given a log file", {
  skip_on_os("windows")
  d <- withr::local_tempdir()
  testthat::local_mocked_bindings(
    .mtr_efetch_gb = function(acc) paste0("LOCUS  ", acc, "  16596 bp DNA circular VRT\n//\n")
  )
  log_fn <- file.path(d, "assembler.log.txt")
  file.create(log_fn)
  maptoref_fetch_accession("NC_002333.1", out_dir = d, log_fn = log_fn)
  expect_true(any(grepl("NC_002333.1 from NCBI", readLines(log_fn))))
})
