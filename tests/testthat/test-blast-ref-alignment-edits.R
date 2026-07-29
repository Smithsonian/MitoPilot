
# Assembly edits in the Annotate modal (trim / linearize) rewrite the sequence the
# cached synteny alignment was computed against. These check the two O(L) repairs
# that keep `blast_ref_alignment` in step without realigning.

# Non-gap column pairs of an alignment, as (sample base index, ref base index) in
# the stored strand's own coordinates. This IS the alignment: the synteny plot
# draws nothing else, so preserving this mapping is the correctness property.
aln_pairs <- function(s, r) {
  sc <- strsplit(s, "")[[1]]
  rc <- strsplit(r, "")[[1]]
  ok <- sc != "-" & rc != "-"
  data.frame(s = cumsum(sc != "-")[ok], r = cumsum(rc != "-")[ok])
}

ungapped <- function(x) gsub("-", "", x, fixed = TRUE)

no_rn <- function(df) {
  rownames(df) <- NULL
  df
}

rc <- function(x) {
  as.character(Biostrings::reverseComplement(Biostrings::DNAString(x)))
}

# A genuine aligner-produced row, so the fixtures carry real gap structure.
make_aln <- function(assembly_seq, ref_seq, rotation = 0L) {
  f <- tempfile(fileext = ".csv")
  on.exit(unlink(f))
  compute_blast_ref_alignment(assembly_seq, ref_seq, rotation, f)
  utils::read.csv(f, stringsAsFactors = FALSE)
}

set.seed(42)
.ref <- paste(sample(c("A", "C", "G", "T"), 400, TRUE), collapse = "")
# Sample = reference with a deletion, an insertion and scattered substitutions,
# flanked by unrelated sequence so there is something to trim.
.core <- paste0(
  substr(.ref, 21, 180),
  substr(.ref, 201, 360)
)
.sample <- paste0(
  paste(sample(c("A", "C", "G", "T"), 30, TRUE), collapse = ""),
  .core,
  paste(sample(c("A", "C", "G", "T"), 25, TRUE), collapse = "")
)

test_that("project_alignment_trim keeps ungapped tracks and the base pairing", {
  a <- make_aln(.sample, .ref)
  expect_equal(nrow(a), 1)
  expect_equal(a$strand[1], "+")
  n <- nchar(.sample)
  from <- 31L
  to <- n - 25L

  res <- project_alignment_trim(a$aligned_sample[1], a$aligned_ref[1],
                                a$strand[1], a$rotation[1], n, from, to)
  expect_false(is.null(res))
  # The sample track is exactly the trimmed assembly ...
  expect_equal(ungapped(res$aligned_sample), substr(.sample, from, to))
  # ... and the reference track is still the WHOLE reference, which the plot
  # relies on to place reference genes by counting non-gap bases.
  expect_equal(ungapped(res$aligned_ref), .ref)
  expect_equal(nchar(res$aligned_sample), nchar(res$aligned_ref))

  before <- aln_pairs(a$aligned_sample[1], a$aligned_ref[1])
  after <- aln_pairs(res$aligned_sample, res$aligned_ref)
  kept <- before[before$s >= from & before$s <= to, ]
  kept$s <- kept$s - from + 1L
  expect_equal(no_rn(after), no_rn(kept))
})

test_that("project_alignment_trim mirrors the window on a reverse-strand row", {
  asm <- rc(.sample)
  a <- make_aln(asm, .ref)
  expect_equal(a$strand[1], "-")
  n <- nchar(asm)
  from <- 26L
  to <- n - 30L

  res <- project_alignment_trim(a$aligned_sample[1], a$aligned_ref[1],
                                a$strand[1], a$rotation[1], n, from, to)
  expect_false(is.null(res))
  # A "-" row stores the reverse complement of the assembly, so the retained
  # window must mirror with it.
  expect_equal(ungapped(res$aligned_sample), rc(substr(asm, from, to)))
  expect_equal(ungapped(res$aligned_ref), .ref)
})

test_that("project_alignment_trim refuses a row that is already stale", {
  a <- make_aln(.sample, .ref)
  # n disagrees with the stored sample track: repairing it would cement the error.
  expect_null(project_alignment_trim(a$aligned_sample[1], a$aligned_ref[1],
                                     a$strand[1], a$rotation[1],
                                     nchar(.sample) + 10L, 5L, 100L))
  expect_null(project_alignment_trim(NA_character_, NA_character_, "+", 0L,
                                     nchar(.sample), 5L, 100L))
})

test_that("rotate_alignment_columns rotates both tracks and updates rotation", {
  a <- make_aln(.sample, .ref)
  n <- nchar(.sample)
  ref_len <- as.integer(a$ref_length[1])
  start <- 120L

  res <- rotate_alignment_columns(a$aligned_sample[1], a$aligned_ref[1],
                                  a$strand[1], a$rotation[1], ref_len, n, start)
  expect_false(is.null(res))
  rotated <- paste0(substr(.sample, start, n), substr(.sample, 1, start - 1))
  expect_equal(ungapped(res$aligned_sample), rotated)
  # The reference is rotated by the same cut, so it is still whole and still the
  # same length; only its origin moved, which `rotation` records.
  expect_equal(nchar(ungapped(res$aligned_ref)), ref_len)
  r_off <- (res$rotation - a$rotation[1]) %% ref_len
  expect_equal(
    ungapped(res$aligned_ref),
    paste0(substr(.ref, r_off + 1L, ref_len), substr(.ref, 1L, r_off))
  )

  # Every aligned base pair survives the rotation, just re-indexed.
  before <- aln_pairs(a$aligned_sample[1], a$aligned_ref[1])
  after <- aln_pairs(res$aligned_sample, res$aligned_ref)
  expected <- data.frame(
    s = ((before$s - start) %% n) + 1L,
    r = ((before$r - r_off - 1L) %% ref_len) + 1L
  )
  expect_equal(no_rn(after[order(after$s), ]), no_rn(expected[order(expected$s), ]))
})

test_that("rotate_alignment_columns mirrors the cut on a reverse-strand row", {
  asm <- rc(.sample)
  a <- make_aln(asm, .ref)
  expect_equal(a$strand[1], "-")
  n <- nchar(asm)
  start <- 90L

  res <- rotate_alignment_columns(a$aligned_sample[1], a$aligned_ref[1],
                                  a$strand[1], a$rotation[1],
                                  as.integer(a$ref_length[1]), n, start)
  expect_false(is.null(res))
  rotated <- paste0(substr(asm, start, n), substr(asm, 1, start - 1))
  expect_equal(ungapped(res$aligned_sample), rc(rotated))
})

test_that("rotate_alignment_columns declines out-of-range cut points", {
  a <- make_aln(.sample, .ref)
  n <- nchar(.sample)
  rl <- as.integer(a$ref_length[1])
  # start == 1 is not a rotation; start > n is not a position.
  expect_null(rotate_alignment_columns(a$aligned_sample[1], a$aligned_ref[1],
                                       a$strand[1], a$rotation[1], rl, n, 1L))
  expect_null(rotate_alignment_columns(a$aligned_sample[1], a$aligned_ref[1],
                                       a$strand[1], a$rotation[1], rl, n, n + 1L))
})

# --- DB wrappers -------------------------------------------------------------

aln_test_db <- function(topology = "circular") {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbExecute(con, "CREATE TABLE blast_ref_alignment (
    ID TEXT, path INTEGER, scaffold INTEGER, accession TEXT,
    aligned_sample TEXT, aligned_ref TEXT, rotation INTEGER, ref_length INTEGER,
    ref_start INTEGER, strand TEXT, time_stamp INTEGER,
    PRIMARY KEY (ID, path, scaffold, accession))")
  DBI::dbExecute(con, "CREATE TABLE blast_ref_sequences (
    accession TEXT PRIMARY KEY, sequence TEXT, topology TEXT)")
  DBI::dbExecute(con, "CREATE TABLE assemblies (
    ID TEXT, path INTEGER, scaffold INTEGER, sequence TEXT)")
  DBI::dbExecute(con, "CREATE TABLE blast_ref_annotations (
    accession TEXT, gene TEXT, pos1 INTEGER)")
  DBI::dbExecute(con, "CREATE TABLE annotate (
    ID TEXT, path INTEGER, scaffold INTEGER, annotate_opts TEXT)")
  DBI::dbExecute(con, "CREATE TABLE annotate_opts (
    annotate_opts TEXT, start_gene TEXT)")

  a <- make_aln(.sample, .ref)
  DBI::dbExecute(
    con, "INSERT INTO blast_ref_alignment VALUES ('S1',1,1,'ACC1',?,?,?,?,?,?,0)",
    params = list(a$aligned_sample[1], a$aligned_ref[1], a$rotation[1],
                  a$ref_length[1], a$ref_start[1], a$strand[1])
  )
  DBI::dbExecute(con, "INSERT INTO blast_ref_sequences VALUES ('ACC1', ?, ?)",
                 params = list(.ref, topology))
  DBI::dbExecute(con, "INSERT INTO assemblies VALUES ('S1',1,1,?)",
                 params = list(.sample))
  DBI::dbExecute(con, "INSERT INTO blast_ref_annotations VALUES ('ACC1','COX1',41)")
  DBI::dbExecute(con, "INSERT INTO annotate VALUES ('S1',1,1,'default')")
  DBI::dbExecute(con, "INSERT INTO annotate_opts VALUES ('default','COX1')")
  con
}

test_that("project_ref_alignment_trim rewrites the cached row", {
  con <- aln_test_db()
  on.exit(DBI::dbDisconnect(con))
  n <- nchar(.sample)
  project_ref_alignment_trim(con, "S1", 1, 1, n, 31L, n - 25L)
  row <- DBI::dbGetQuery(con, "SELECT * FROM blast_ref_alignment")
  expect_equal(nrow(row), 1)
  expect_equal(ungapped(row$aligned_sample[1]), substr(.sample, 31L, n - 25L))
  expect_equal(ungapped(row$aligned_ref[1]), .ref)
})

test_that("rotate_ref_alignment rotates against a circular reference", {
  con <- aln_test_db("circular")
  on.exit(DBI::dbDisconnect(con))
  n <- nchar(.sample)
  rotate_ref_alignment(con, "S1", 1, 1, n, 120L)
  row <- DBI::dbGetQuery(con, "SELECT * FROM blast_ref_alignment")
  expect_equal(nrow(row), 1)
  expect_equal(ungapped(row$aligned_sample[1]),
               paste0(substr(.sample, 120L, n), substr(.sample, 1L, 119L)))
})

test_that("rotate_ref_alignment drops the row for a linear reference", {
  con <- aln_test_db("linear")
  on.exit(DBI::dbDisconnect(con))
  # Permuting a linear reference would invent an origin it does not have, so the
  # row is dropped and recomputed on demand instead.
  rotate_ref_alignment(con, "S1", 1, 1, nchar(.sample), 120L)
  expect_equal(nrow(DBI::dbGetQuery(con, "SELECT * FROM blast_ref_alignment")), 0)
})

test_that("ensure_ref_alignment recomputes a missing row", {
  con <- aln_test_db()
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "DELETE FROM blast_ref_alignment")
  expect_true(ensure_ref_alignment(con, "S1", 1, 1, "ACC1"))
  row <- DBI::dbGetQuery(con, "SELECT * FROM blast_ref_alignment")
  expect_equal(nrow(row), 1)
  expect_equal(ungapped(row$aligned_sample[1]), .sample)
  expect_equal(nchar(ungapped(row$aligned_ref[1])), row$ref_length[1])
  # Circular reference + a start_gene at position 41 -> 40 bp rotation, same as
  # the pipeline's own SQL.
  expect_equal(row$rotation[1], 40)
})

test_that("ensure_ref_alignment refuses a pair too large to align in-app", {
  con <- aln_test_db()
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "DELETE FROM blast_ref_alignment")
  # The DP matrix is quadratic; an oversized pair would take the app down.
  expect_message(
    expect_false(ensure_ref_alignment(con, "S1", 1, 1, "ACC1", max_cells = 1e3)),
    "too large to align"
  )
  expect_equal(nrow(DBI::dbGetQuery(con, "SELECT * FROM blast_ref_alignment")), 0)
})

test_that("ensure_ref_alignment is a no-op when the reference sequence is absent", {
  con <- aln_test_db()
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "DELETE FROM blast_ref_alignment")
  DBI::dbExecute(con, "DELETE FROM blast_ref_sequences")
  expect_false(ensure_ref_alignment(con, "S1", 1, 1, "ACC1"))
  expect_equal(nrow(DBI::dbGetQuery(con, "SELECT * FROM blast_ref_alignment")), 0)
})
