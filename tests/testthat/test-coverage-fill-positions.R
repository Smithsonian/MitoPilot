# bam-readcount reports only covered positions. Internal zero-coverage runs
# (map-to-ref N gaps) used to vanish from the coverage table, and the validate
# writer rebuilt the saved sequence from it, silently dropping those bases.

asm <- Biostrings::DNAStringSet(c(
  "s.1.1" = "ACGTNNNNNACGTACGTNNNACGT",
  "s.1.2" = "GGGGCCCC"
))

per_base <- function(id, pos) {
  calls <- strsplit(as.character(asm[[id]]), "")[[1]][pos]
  data.frame(SeqId = id, Position = pos, Call = calls,
             Depth = 10, Correct = 9, ErrorRate = 0.1)
}

test_that(".coverage_fill_positions completes start, end, and internal holes", {
  covered <- c(3:5, 10:17, 21:22)
  cov <- rbind(per_base("s.1.1", covered), per_base("s.1.2", 1:8))
  out <- .coverage_fill_positions(cov, asm)

  one <- out[out$SeqId == "s.1.1", ]
  expect_equal(one$Position, 1:24)
  expect_equal(paste(one$Call, collapse = ""), as.character(asm[["s.1.1"]]))
  filled <- !one$Position %in% covered
  expect_true(all(one$Depth[filled] == 0 & one$Correct[filled] == 0))
  expect_true(all(is.na(one$ErrorRate[filled])))
  expect_true(all(one$Depth[!filled] == 10 & one$Correct[!filled] == 9))
  # complete scaffold is unchanged
  two <- out[out$SeqId == "s.1.2", ]
  expect_equal(two$Position, 1:8)
  expect_true(all(two$Depth == 10 & two$Correct == 9))
})

test_that(".coverage_fill_positions leaves an uncovered scaffold absent", {
  out <- .coverage_fill_positions(per_base("s.1.2", 2:7), asm)
  expect_equal(unique(out$SeqId), "s.1.2")
  expect_equal(out$Position, 1:8)
})

test_that("ambiguity codes with reads keep their NA Correct", {
  cov <- per_base("s.1.1", 1:24)
  cov$Correct[5] <- NA
  out <- .coverage_fill_positions(cov, asm)
  expect_true(is.na(out$Correct[5]))
})

write_stats <- function(cov, fn) {
  stats <- .coverage_stats_to_output(.coverage_rolling_stats(cov))
  readr::write_csv(stats, fn, quote = "none", na = "")
}

test_that(".coverage_read_complete heals a gappy coverageStats file", {
  d <- withr::local_tempdir()
  covered <- c(1:4, 10:17, 21:24)
  gappy_fn <- file.path(d, "gappy.csv")
  full_fn <- file.path(d, "full.csv")
  write_stats(per_base("s.1.1", covered), gappy_fn)
  write_stats(.coverage_fill_positions(per_base("s.1.1", covered), asm), full_fn)

  named <- asm["s.1.1"]
  names(named) <- "s.1.1 circular"
  healed <- .coverage_read_complete(gappy_fn, named)
  expect_equal(healed$Position, 1:24)
  expect_equal(paste(healed$Call, collapse = ""), as.character(asm[["s.1.1"]]))
  expect_equal(as.character(healed$MeanDepth),
               as.character(read.csv(full_fn)$MeanDepth))

  # complete files pass through as read
  expect_equal(.coverage_read_complete(full_fn, named), read.csv(full_fn))
})

test_that("validate writer takes the sequence from the curated FASTA and checks coverage", {
  nf <- readLines(system.file("nextflow", "modules", "validate.nf", package = "MitoPilot"))
  body <- paste(nf, collapse = "\n")
  expect_false(grepl("def seq\\s*=\\s*rows\\.collect", body))
  expect_match(body, "new File(assembly_fn.toString())", fixed = TRUE)
  expect_match(body, "does not match its assembly", fixed = TRUE)
})
