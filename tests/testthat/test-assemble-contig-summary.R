test_that("assemble_contig_summary counts kept and ignored paths and scaffolds", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbWriteTable(con, "assemblies", data.frame(
    ID       = c("a", "a", "a", "b", "b", "c", "c", "c", "c"),
    path     = c(1, 1, 1, 1, 2, 0, 1, 1, 1),
    length   = c(4490, 6741, 4409, 19332, 19330, 18037, 6008, 6008, 6008),
    ignore   = c(0, 1, 1, 0, 0, 0, 1, 1, 1)))
  s <- assemble_contig_summary(con)
  s <- s[order(s$ID), ]
  expect_equal(s$length_per_scaffold, c("6741;4490;4409", "19332;19330", "18037;6008;6008;6008"))
  expect_equal(s$ignore_flags, c("1;0;1", "0;0", "0;1;1;1"))
  expect_equal(s$paths_n, c(1, 2, 1))
  expect_equal(s$paths_ignored, c(0, 0, 0))
  expect_equal(s$scaffolds_n, c(1, 1, 1))
  expect_equal(s$scaffolds_ignored, c(2, 0, 3))
})

test_that("a multi-path consensus Path 0 counts its source paths as ignored paths", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbWriteTable(con, "assemblies", data.frame(
    ID = "d", path = c(0, 1, 2), length = c(19330, 19332, 19332), ignore = c(0, 1, 1)))
  s <- assemble_contig_summary(con)
  expect_equal(c(s$paths_n, s$paths_ignored, s$scaffolds_n, s$scaffolds_ignored), c(1, 2, 1, 0))
})
