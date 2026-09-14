test_that("mtr_display_ref links only MapToRef samples and shows a topology", {
  out <- mtr_display_ref(
    assembler = c("MapToRef", "MapToRef", "MapToRef", "GetOrganelle", NA),
    ref = c("NC_002333", "/r/m.fasta", NA, "NC_002333", NA),
    topology = c(NA, "circular", NA, "linear", NA)
  )
  expect_equal(out, c("NC_002333", "/r/m.fasta (circular)", "set reference", NA, NA))
})
