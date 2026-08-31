# A contig no reads mapped to reaches coverage_trim with zero coverage rows.
# Every base then looks masked, and the trailing trim used to eat the whole
# contig down to 50 bp, after which the leading test indexed past the end of the
# shrunken stats and died with "missing value where TRUE/FALSE needed". The
# crash was the visible half; the truncation was the dangerous half.

cov_stats <- function(n, depth = 40) {
  data.frame(
    SeqId = "u.1.1", Position = seq_len(n), Call = rep("A", n),
    Depth = depth, Correct = depth, ErrorRate = 0, MeanDepth = depth,
    GC = NA_real_, mask = FALSE, stringsAsFactors = FALSE
  )
}

empty_stats <- function() {
  data.frame(
    SeqId = character(0), Position = integer(0), Call = character(0),
    Depth = numeric(0), Correct = numeric(0), ErrorRate = numeric(0),
    MeanDepth = numeric(0), GC = numeric(0), mask = logical(0),
    stringsAsFactors = FALSE
  )
}

asm <- function(n) Biostrings::DNAString(strrep("ACGT", ceiling(n / 4)) |> substr(1, n))

test_that("a contig with no coverage keeps every base", {
  a <- asm(5000)
  res <- coverage_trim(assembly = a, stats = empty_stats())
  expect_equal(length(res$assembly), 5000L)
})

test_that("a contig with no coverage does not error", {
  expect_no_error(coverage_trim(assembly = asm(5000), stats = empty_stats()))
  # short enough to hit the existing < 152 guard, long enough to matter
  expect_no_error(coverage_trim(assembly = asm(300), stats = empty_stats()))
})

test_that("a fully covered contig is still left alone", {
  a <- asm(5000)
  res <- coverage_trim(assembly = a, stats = cov_stats(5000))
  expect_equal(length(res$assembly), 5000L)
})

test_that("ragged ends are still trimmed", {
  # 200 bp of dead sequence at each end, good coverage in between
  n <- 5000
  s <- cov_stats(n)
  dead <- c(1:200, (n - 199):n)
  s$MeanDepth[dead] <- 0
  s$Depth[dead] <- 0
  s$mask[dead] <- TRUE
  res <- coverage_trim(assembly = asm(n), stats = s)
  expect_lt(length(res$assembly), n)
  expect_gt(length(res$assembly), 4000L)
})

test_that("the leading window is not indexed past a shrunken stats table", {
  # coverage only in the first 60 bp: the trailing trim shrinks stats below the
  # 100 bp leading window, which is exactly what used to produce the NA
  n <- 4000
  s <- cov_stats(n)
  s$MeanDepth[61:n] <- 0
  s$Depth[61:n] <- 0
  s$mask[61:n] <- TRUE
  expect_no_error(coverage_trim(assembly = asm(n), stats = s))
})
