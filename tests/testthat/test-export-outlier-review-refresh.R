# The PCG outlier review skips its (expensive) alignment recompute when the user
# returns from the annotate editor without changing anything. That decision is
# made by diffing the focal unit's PCG annotations in the db against a snapshot
# taken on the way out: a wrong "unchanged" verdict silently re-shows the
# pre-edit alignment. These cover the verdict, including the fail-safe cases,
# which must never report "unchanged" when the state is unknown.

ann_row <- function(gene, pos1, pos2, direction = "+", translation = "MPQL",
                    type = "PCG", ID = "S1", path = 1L, scaffold = 1L) {
  data.frame(
    ID = ID, path = as.integer(path), scaffold = as.integer(scaffold),
    type = type, gene = gene, pos1 = as.integer(pos1), pos2 = as.integer(pos2),
    direction = direction, translation = translation, stringsAsFactors = FALSE
  )
}

sig_db <- function(...) {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbWriteTable(con, "annotations", do.call(rbind, list(...)))
  unit_pcg_sig(con, "S1", 1L, 1L)
}

test_that("a 5' shortening of one gene is reported as that gene changing", {
  before <- sig_db(
    ann_row("atp8", 10374, 10541, translation = "MPQLSTNTW"),
    ann_row("cox1", 7894, 9441, translation = "MAITRW")
  )
  after <- sig_db(
    ann_row("atp8", 10425, 10541, translation = "MAFLFIVPL"),
    ann_row("cox1", 7894, 9441, translation = "MAITRW")
  )
  expect_equal(sig_diff(before, after), "atp8")
})

test_that("an untouched unit reports nothing changed", {
  s <- sig_db(ann_row("atp8", 10374, 10541), ann_row("cox1", 7894, 9441))
  expect_length(sig_diff(s, s), 0L)
})

test_that("a position-only edit is caught even when the translation is unchanged", {
  before <- sig_db(ann_row("nad5", 14374, 16212))
  after <- sig_db(ann_row("nad5", 14374, 16215))
  expect_equal(sig_diff(before, after), "nad5")
})

test_that("added and removed genes are both reported", {
  one <- sig_db(ann_row("atp8", 10374, 10541))
  two <- sig_db(ann_row("atp8", 10374, 10541), ann_row("atp6", 10532, 11214))
  expect_equal(sig_diff(one, two), "atp6")
  expect_equal(sig_diff(two, one), "atp6")
})

test_that("multi-exon genes collapse order-independently", {
  a <- sig_db(ann_row("nad5", 100, 200), ann_row("nad5", 400, 500))
  b <- sig_db(ann_row("nad5", 400, 500), ann_row("nad5", 100, 200))
  expect_length(sig_diff(a, b), 0L)
  c3 <- sig_db(ann_row("nad5", 100, 200), ann_row("nad5", 400, 501))
  expect_equal(sig_diff(a, c3), "nad5")
})

test_that("non-PCG rows and deleted tombstones are ignored", {
  before <- sig_db(
    ann_row("atp8", 10374, 10541),
    ann_row("trnF", 1, 70, type = "tRNA")
  )
  after <- sig_db(
    ann_row("atp8", 10374, 10541),
    ann_row("trnF", 5, 74, type = "tRNA"),
    ann_row("cox1_DELETED_1", 0, 0)
  )
  expect_length(sig_diff(before, after), 0L)
})

test_that("only the requested unit is measured", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbWriteTable(con, "annotations", rbind(
    ann_row("atp8", 10374, 10541),
    ann_row("atp8", 999, 1200, scaffold = 2L)
  ))
  expect_equal(unname(unit_pcg_sig(con, "S1", 1L, 1L)), "10374|10541|+|MPQL")
  expect_equal(unname(unit_pcg_sig(con, "S1", 1L, 2L)), "999|1200|+|MPQL")
})

test_that("an unknown snapshot is never mistaken for unchanged", {
  s <- sig_db(ann_row("atp8", 10374, 10541))
  # A failed db read on either side must fall through to a full recompute.
  expect_null(sig_diff(NULL, s))
  expect_null(sig_diff(s, NULL))
  expect_null(sig_diff(NULL, NULL))
  # A unit that has no PCG rows at all is a real (empty) snapshot, not unknown.
  empty <- sig_db(ann_row("trnF", 1, 70, type = "tRNA"))
  expect_length(empty, 0L)
  expect_equal(sig_diff(empty, s), "atp8")
})
