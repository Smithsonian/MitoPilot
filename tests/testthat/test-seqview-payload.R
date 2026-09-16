# tests/testthat/test-seqview-payload.R
sv_ann <- function(...) {
  base <- data.frame(
    type = "PCG", gene = "nad1", pos1 = 10L, pos2 = 39L, direction = "+",
    partial_start = "no", partial_stop = "no", translation = "MKLIVLLKN",
    notes = "", stringsAsFactors = FALSE
  )
  rows <- list(...)
  if (length(rows) == 0) return(base)
  do.call(rbind, lapply(rows, function(r) { b <- base; b[names(r)] <- r; b }))
}

test_that("payload carries the unit, sequence, and every live feature", {
  a <- sv_ann(list(), list(gene = "trnF", type = "tRNA", pos1 = 40L, pos2 = 108L, translation = NA))
  p <- seqview_payload(a, "acgtacgt", "circular", "S1.1.1", selected = 2L, version = 3L)
  expect_equal(p$unit, "S1.1.1")
  expect_equal(p$len, 8L)
  expect_equal(p$seq, "ACGTACGT")
  expect_equal(p$topology, "circular")
  expect_equal(p$version, 3L)
  expect_equal(p$selected, 2L)
  expect_length(p$features, 2)
  f <- p$features[[1]]
  expect_equal(f[c("row", "gene", "pos1", "pos2", "dir")],
               list(row = 1L, gene = "nad1", pos1 = 10L, pos2 = 39L, dir = "+"))
  expect_equal(f$translation, "MKLIVLLKN")
  expect_false(f$partial5); expect_false(f$partial3)
  expect_null(p$features[[2]]$translation)
})

test_that("soft-deleted rows are dropped and the selection follows the row index", {
  a <- sv_ann(list(), list(gene = "nad2_DELETED_1", pos1 = 0L, pos2 = 0L),
              list(gene = "cox1", pos1 = 100L, pos2 = 200L))
  p <- seqview_payload(a, "ACGT", "linear", "S1.1.1", selected = 3L)
  expect_equal(vapply(p$features, `[[`, integer(1), "row"), c(1L, 3L))
  expect_equal(p$selected, 3L)
  expect_null(seqview_payload(a, "ACGT", "linear", "S1.1.1", selected = 2L)$selected)
})

test_that("partial flags, join markers, and notes are carried", {
  a <- sv_ann(list(partial_start = "yes", notes = "JOIN: mode=exon group=2 extra words that go on and on and on and on and on and on and on"))
  f <- seqview_payload(a, "ACGT", "linear", "S1.1.1")$features[[1]]
  expect_true(f$partial5); expect_false(f$partial3)
  expect_equal(f$joined, "JOIN: mode=exon group=2")
  expect_equal(nchar(f$notes), 80L)
})

test_that("a wrap-around feature is passed through untouched", {
  a <- sv_ann(list(pos1 = 16500L, pos2 = 120L))
  f <- seqview_payload(a, strrep("A", 16600), "circular", "S1.1.1")$features[[1]]
  expect_equal(c(f$pos1, f$pos2), c(16500L, 120L))
})

test_that("seqview_server bumps version only when the sequence string changes", {
  a <- cbind(sv_ann(), data.frame(ID = "S1", path = 1L, scaffold = 1L,
                                   stringsAsFactors = FALSE))
  rv <- shiny::reactiveValues(
    annotations = a,
    updating = list(ID = "S1", path = 1L, scaffold = 1L, topology = "circular"),
    editing = list(assembly = "ACGTACGTAC")
  )
  tick <- shiny::reactiveVal(0L)
  selected <- shiny::reactive(NULL)

  shiny::testServer(
    seqview_server,
    args = list(rv = rv, tick = tick, selected = selected),
    {
      gargoyle::init("annotations_modal", session = session)
      session$flushReact()
      v0 <- version()

      rv$editing <- list(assembly = "ACGTACGTAC", stop_aln = 1)
      session$flushReact()
      expect_equal(version(), v0)

      rv$editing <- list(assembly = "ACGTACGTACGG")
      session$flushReact()
      expect_equal(version(), v0 + 1L)
    }
  )
})

test_that("an edit to a position is resent without bumping the version", {
  a <- cbind(sv_ann(), data.frame(ID = "S1", path = 1L, scaffold = 1L,
                                   stringsAsFactors = FALSE))
  rv <- shiny::reactiveValues(
    annotations = a,
    updating = list(ID = "S1", path = 1L, scaffold = 1L, topology = "circular"),
    editing = list(assembly = "ACGTACGTAC")
  )
  tick <- shiny::reactiveVal(0L)
  selected <- shiny::reactive(NULL)

  shiny::testServer(
    seqview_server,
    args = list(rv = rv, tick = tick, selected = selected),
    {
      gargoyle::init("annotations_modal", session = session)
      # the mock session drops custom messages; capture them on the real one
      sent <- list()
      real <- .subset2(session, "parent")
      real$sendCustomMessage <- function(type, message) {
        if (identical(type, "mpseq")) sent[[length(sent) + 1L]] <<- message
        invisible()
      }
      session$flushReact()
      first <- sent[[length(sent)]]

      rv$annotations$pos1[1] <- rv$annotations$pos1[1] + 3L
      session$flushReact()
      last <- sent[[length(sent)]]
      expect_gt(length(sent), 1L)
      expect_equal(last$features[[1]]$pos1, first$features[[1]]$pos1 + 3L)
      expect_equal(last$version, first$version)
    }
  )
})

test_that("coverage arrays are indexed by position with null gaps", {
  a <- sv_ann()
  cov <- data.frame(Position = c(1L, 2L, 4L), Depth = c(10L, 20L, 40L), ErrorRate = c(0, 0.12345, NA))
  p <- seqview_payload(a, "ACGT", "linear", "S1.1.1", coverage = cov)
  expect_equal(p$depth, c(10L, 20L, NA, 40L))
  expect_equal(p$err, c(0, 0.1235, NA, NA))
  expect_null(seqview_payload(a, "ACGT", "linear", "S1.1.1")$depth)
})

test_that("an assembly without reads carries no coverage tracks", {
  a <- sv_ann()
  cov <- data.frame(Position = 1:4, Depth = NA_real_, ErrorRate = NA_real_)
  p <- seqview_payload(a, "ACGT", "linear", "S1.1.1", coverage = cov)
  expect_null(p$depth); expect_null(p$err)
  expect_null(seqview_payload(a, "ACGT", "linear", "S1.1.1", coverage = NULL)$depth)
})
