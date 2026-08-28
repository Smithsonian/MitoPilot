# A CDS crossing a scaffold join contains the N spacer the join inserted.
# Biostrings::translate() defaults to if.fuzzy.codon = "error" and dies on it,
# which killed the whole sample in WF2 with a .Call2() stack trace.

test_that("ambiguous bases are counted, whatever their case or code", {
  expect_equal(ambiguous_base_count("ACGT"), 0L)
  expect_equal(ambiguous_base_count("acgt"), 0L)
  expect_equal(ambiguous_base_count("ACGTNNN"), 3L)
  expect_equal(ambiguous_base_count("ACGTRYKM"), 4L)
  expect_equal(ambiguous_base_count(Biostrings::DNAString("ACGTNN")), 2L)
})

test_that("an empty or missing sequence counts as unambiguous", {
  expect_equal(ambiguous_base_count(""), 0L)
  expect_equal(ambiguous_base_count(NA_character_), 0L)
  expect_equal(ambiguous_base_count(character(0)), 0L)
})

test_that("solving a fuzzy codon yields X instead of an error", {
  # Pins the policy the fix depends on, and the default it had to override.
  cds <- Biostrings::DNAString("ATGNNNTAA")
  expect_error(Biostrings::translate(cds), "not a base")
  expect_equal(
    as.character(Biostrings::translate(cds, if.fuzzy.codon = "solve")),
    "MX*"
  )
  # A codon whose amino acid does not depend on the ambiguous base is resolved.
  # Not the first codon, which Biostrings reads as an initiation codon: CTN
  # could be the alternative start CTG, so only a later CTN resolves to L.
  expect_equal(
    as.character(Biostrings::translate(Biostrings::DNAString("ATGCTN"),
                                       if.fuzzy.codon = "solve")),
    "ML"
  )
})

test_that("no translate call in the package can crash on an ambiguous base", {
  # The original fix reached four call sites and missed the curation path, so
  # the same crash came back from a different function. Guard all of them.
  files <- list.files(testthat::test_path("../..", "R"), pattern = "[.]R$",
                      full.names = TRUE)
  if (!length(files)) skip("package sources not available")

  unguarded <- unlist(lapply(files, function(f) {
    lines <- readLines(f, warn = FALSE)
    hits <- grep("Biostrings::translate(", lines, fixed = TRUE)
    hits <- hits[vapply(hits, function(i) {
      !any(grepl("if.fuzzy.codon", lines[i:min(i + 5L, length(lines))], fixed = TRUE))
    }, logical(1))]
    if (length(hits)) paste0(basename(f), ":", hits) else NULL
  }))

  expect_equal(unguarded, NULL)
})
