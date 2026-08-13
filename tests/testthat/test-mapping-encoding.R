# A raw 0x85 byte in a mapping file used to be stored verbatim and only surfaced
# later as a dead Shiny session (the samples table cannot be serialized to the
# client). Ingest must reject it instead.
write_mapping_bytes <- function(txt) {
  fn <- tempfile(fileext = ".csv")
  con <- file(fn, "wb")
  writeBin(charToRaw(txt), con)
  close(con)
  fn
}

# Bare 0x85 (Unicode NEL), the byte from the bug report
nel <- rawToChar(as.raw(0x85))
# Valid two-byte UTF-8, i with an acute accent
i_acute <- rawToChar(as.raw(c(0xc3, 0xad)))

test_that("invalid UTF-8 in a data cell is rejected at ingest", {
  fn <- write_mapping_bytes(paste0(
    "ID,Taxon,R1,R2\n",
    "S1,octopod larva", nel, " id,a_1.fq.gz,a_2.fq.gz\n"
  ))
  expect_error(read_and_validate_mapping(fn), "non-UTF-8")
})

test_that("invalid UTF-8 in a header is rejected at ingest", {
  fn <- write_mapping_bytes(paste0(
    "ID,Ta", nel, "xon,R1,R2\n",
    "S1,squid,a_1.fq.gz,a_2.fq.gz\n"
  ))
  expect_error(read_and_validate_mapping(fn), "non-UTF-8")
})

test_that("valid mapping files still read, including non-ASCII UTF-8", {
  fn <- write_mapping_bytes(paste0(
    "ID,Taxon,depth,R1,R2\n",
    "S1,Sep", i_acute, "a officinalis,42,a_1.fq.gz,a_2.fq.gz\n"
  ))
  mapping <- read_and_validate_mapping(fn)
  expect_equal(nrow(mapping), 1L)
  expect_true(all(validUTF8(mapping$Taxon)))
  # numeric column must not trip the character-only encoding check
  expect_true(is.numeric(mapping$depth))
})
