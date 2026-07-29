# `unannotated_ends` measures the flanks the Trim control reports and cuts to.
# Deleting an annotation in the modal soft-deletes it (gene suffixed "_DELETED_",
# pos1 = pos2 = 0), so the measurement must ignore those tombstones and move to
# the next real feature.

make_db <- function(len = 1000L, ann = NULL, topology = "linear") {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(
    con, "assemblies",
    data.frame(ID = "S1", path = 1L, scaffold = 1L, length = len,
               topology = topology, stringsAsFactors = FALSE)
  )
  if (is.null(ann)) {
    ann <- data.frame(ID = character(), path = integer(), scaffold = integer(),
                      gene = character(), pos1 = integer(), pos2 = integer(),
                      stringsAsFactors = FALSE)
  }
  DBI::dbWriteTable(con, "annotations", ann)
  con
}

anns <- function(...) {
  rows <- list(...)
  data.frame(
    ID = "S1", path = 1L, scaffold = 1L,
    gene = vapply(rows, function(r) r[[1]], character(1)),
    pos1 = vapply(rows, function(r) as.integer(r[[2]]), integer(1)),
    pos2 = vapply(rows, function(r) as.integer(r[[3]]), integer(1)),
    stringsAsFactors = FALSE
  )
}

test_that("flanks are measured from the outermost live annotations", {
  con <- make_db(ann = anns(list("COX1", 101, 200), list("ND1", 301, 400)))
  on.exit(DBI::dbDisconnect(con))
  e <- unannotated_ends(con, "S1", 1, 1)
  expect_equal(e$from, 101L)
  expect_equal(e$to, 400L)
  expect_equal(e$lead, 100L)
  expect_equal(e$trail, 600L)
})

test_that("a soft-deleted leading annotation moves the trim start inward", {
  con <- make_db(ann = anns(
    list("COX1_DELETED_1700000000", 0, 0),
    list("ND1", 301, 400)
  ))
  on.exit(DBI::dbDisconnect(con))
  e <- unannotated_ends(con, "S1", 1, 1)
  expect_equal(e$from, 301L)
  expect_equal(e$lead, 300L)
  expect_equal(e$trail, 600L)
})

test_that("a soft-deleted trailing annotation moves the trim end inward", {
  con <- make_db(ann = anns(
    list("COX1", 101, 200),
    list("ND1_DELETED_1700000000", 0, 0)
  ))
  on.exit(DBI::dbDisconnect(con))
  e <- unannotated_ends(con, "S1", 1, 1)
  expect_equal(e$to, 200L)
  expect_equal(e$lead, 100L)
  expect_equal(e$trail, 800L)
})

test_that("only tombstones means nothing to trim to", {
  con <- make_db(ann = anns(list("COX1_DELETED_1", 0, 0)))
  on.exit(DBI::dbDisconnect(con))
  e <- unannotated_ends(con, "S1", 1, 1)
  expect_true(is.na(e$from))
  expect_true(is.na(e$lead))
  expect_equal(e$topology, "linear")
})
