test_that(".geome_status_join labels ok, failed, and none", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbWriteTable(con, "samples", data.frame(ID = c("a", "b", "c"), Taxon = "x"))
  .geome_ensure_tables(con)
  DBI::dbAppendTable(con, "geome_status", data.frame(
    ID = c("a", "b"), bcid = "ark:/1/A", status = c("ok", "failed"),
    message = c(NA, "BCID not found in GEOME"), fetched_at = 1L))
  out <- dplyr::tbl(con, "samples") |> .geome_status_join(con) |> dplyr::collect()
  out <- out[order(out$ID), ]
  expect_equal(out$geome, c("ok", "failed", "none"))
  expect_equal(out$geome_message[2], "BCID not found in GEOME")
})

test_that("rt_geome sends the row ID to the given input", {
  js <- as.character(rt_geome("assemble-geome_open"))
  expect_match(js, "assemble-geome_open", fixed = TRUE)
  expect_match(js, "setInputValue", fixed = TRUE)
  expect_match(js, "dataset.id", fixed = TRUE)
})
