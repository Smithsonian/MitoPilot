# Remote reference sequences are injected into their own small per-gene FASTAs
# rather than appended to the shared curation DB, so ref_dir stays read-only and
# can be shared across tasks instead of copied (~194 MB) into each one. The base
# and remote DBs are then searched together.

make_ref_dir <- function() {
  d <- tempfile()
  dir.create(file.path(d, "featureProt"), recursive = TRUE)
  for (gene in c("cox1", "nad2")) {
    Biostrings::writeXStringSet(
      Biostrings::AAStringSet(c("LOCAL1 Genus species" = "MTNLRWLFSTNHKDIGT")),
      file.path(d, "featureProt", paste0(gene, ".fas"))
    )
  }
  d
}

make_ref_json <- function(genes = c("cox1", "nad2")) {
  f <- tempfile(fileext = ".json")
  # cox1 at 1-18, nad2 at 19-36 of a trivial ORF-ish sequence
  pcg <- data.frame(
    gene = genes,
    pos1 = c(1L, 19L)[seq_along(genes)],
    pos2 = c(18L, 36L)[seq_along(genes)],
    direction = "+",
    stringsAsFactors = FALSE
  )
  jsonlite::write_json(
    list(
      accession = "OQ123456", organism = "Refus exampleus", genetic_code = 2L,
      sequence = paste(rep("ATGACAAACCTACGA", 4), collapse = ""),
      pcg = pcg
    ),
    f, auto_unbox = TRUE
  )
  f
}

test_that("injection writes remote sequences to out_dir and leaves ref_dir untouched", {
  ref_dir <- make_ref_dir()
  out_dir <- file.path(tempfile(), "remote")
  on.exit(unlink(c(ref_dir, dirname(out_dir)), recursive = TRUE))

  before <- vapply(
    list.files(file.path(ref_dir, "featureProt"), full.names = TRUE),
    function(f) tools::md5sum(f)[[1]], character(1)
  )

  genes <- inject_remote_hits_into_blast_db(
    make_ref_json(), ref_dir, out_dir = out_dir
  )

  # the shared base DB must be byte-identical afterwards
  after <- vapply(
    list.files(file.path(ref_dir, "featureProt"), full.names = TRUE),
    function(f) tools::md5sum(f)[[1]], character(1)
  )
  expect_equal(before, after)

  # and the remote sequences land in out_dir instead
  expect_true(length(genes) > 0)
  expect_true(all(file.exists(file.path(out_dir, paste0(genes, ".fas")))))
})

test_that("injected remote FASTAs carry the remote tag and accession header", {
  ref_dir <- make_ref_dir()
  out_dir <- file.path(tempfile(), "remote")
  on.exit(unlink(c(ref_dir, dirname(out_dir)), recursive = TRUE))

  inject_remote_hits_into_blast_db(make_ref_json("cox1"), ref_dir, out_dir = out_dir)
  seqs <- Biostrings::readAAStringSet(file.path(out_dir, "cox1.fas"))
  expect_equal(length(seqs), 1L)
  expect_match(names(seqs), "^OQ123456 \\[remote\\] Refus exampleus$")
})

test_that("repeated injection accumulates candidates rather than overwriting", {
  ref_dir <- make_ref_dir()
  out_dir <- file.path(tempfile(), "remote")
  on.exit(unlink(c(ref_dir, dirname(out_dir)), recursive = TRUE))

  inject_remote_hits_into_blast_db(make_ref_json("cox1"), ref_dir, out_dir = out_dir)
  inject_remote_hits_into_blast_db(make_ref_json("cox1"), ref_dir, out_dir = out_dir)
  seqs <- Biostrings::readAAStringSet(file.path(out_dir, "cox1.fas"))
  expect_equal(length(seqs), 2L)
})

test_that("injection is a no-op for genes absent from the base DB", {
  ref_dir <- make_ref_dir()
  out_dir <- file.path(tempfile(), "remote")
  on.exit(unlink(c(ref_dir, dirname(out_dir)), recursive = TRUE))

  genes <- inject_remote_hits_into_blast_db(
    make_ref_json("notagene"), ref_dir, out_dir = out_dir
  )
  expect_length(genes, 0)
})

test_that("every curate refHits computation searches the remote DB too", {
  # The remote sequences live in their own database, so any get_top_hits() call
  # that names only the base silently drops them. refHits are recomputed after
  # each start/stop adjustment, so a missed call site loses remote hits for
  # exactly the genes that got adjusted -- which is what shipped the first time.
  src <- deparse(curate_mito_core)
  calls <- grep("get_top_hits\\(", src)
  expect_true(length(calls) > 0)

  # every get_top_hits( call must be followed by gene_dbs( as its db argument
  # (get_top_hits_orf/_nuc use their own combined/nucleotide DBs and are exempt)
  plain <- grep("[^_]get_top_hits\\(", src)
  for (i in plain) {
    window <- paste(src[i:min(i + 1, length(src))], collapse = " ")
    expect_match(window, "gene_dbs\\(", info = paste("line:", trimws(src[i])))
  }
})

test_that("blast_db_arg quotes only when several databases are searched", {
  expect_equal(blast_db_arg("/db/cox1.fas"), "/db/cox1.fas")
  expect_equal(
    blast_db_arg(c("/db/cox1.fas", "/remote/cox1.fas")),
    "'/db/cox1.fas /remote/cox1.fas'"
  )
})

test_that("build_combined_orf_db merges base and remote dirs with unique numbering", {
  ref_dir <- make_ref_dir()
  out_dir <- file.path(tempfile(), "remote")
  dir.create(out_dir, recursive = TRUE)
  on.exit(unlink(c(ref_dir, dirname(out_dir)), recursive = TRUE))
  Biostrings::writeXStringSet(
    Biostrings::AAStringSet(c("OQ999 [remote] Refus exampleus" = "MTNLRWLFSTNHKDIGT")),
    file.path(out_dir, "cox1.fas")
  )

  out <- file.path(tempfile(fileext = ".fas"))
  # makeblastdb may be absent here; the FASTA merge is what is under test
  res <- suppressWarnings(build_combined_orf_db(
    c(file.path(ref_dir, "featureProt"), out_dir), out, condaenv = NULL
  ))
  skip_if(is.null(res), "combined DB not built")

  seqs <- Biostrings::readAAStringSet(out)
  # cox1 contributes its base + remote sequence, nad2 only its base one
  expect_equal(length(seqs), 3L)
  cox1 <- grep(":cox1-", names(seqs), value = TRUE)
  expect_length(cox1, 2L)
  # numbering is unique within a gene across both directories
  expect_equal(sort(sub("^.*:cox1-(\\d+).*$", "\\1", cox1)), c("1", "2"))
})

test_that("build_combined_orf_db tolerates a missing remote dir", {
  ref_dir <- make_ref_dir()
  on.exit(unlink(ref_dir, recursive = TRUE))
  out <- file.path(tempfile(fileext = ".fas"))
  res <- suppressWarnings(build_combined_orf_db(
    c(file.path(ref_dir, "featureProt"), file.path(tempfile(), "nope")),
    out, condaenv = NULL
  ))
  skip_if(is.null(res), "combined DB not built")
  expect_equal(length(Biostrings::readAAStringSet(out)), 2L)
})
