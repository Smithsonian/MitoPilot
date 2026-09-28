test_that("ncbi_normalize_id accepts BioSample, uid, SRA, and NCBI links", {
  x <- c("SAMN29555051", " samn29555051 ", "https://www.ncbi.nlm.nih.gov/biosample/SAMN29555051/",
         "SAMEA1234567", "SAMD00012345", "29555051", "SRR21844202", "err000001",
         "SRX17832658", "SRS14384543", "https://www.ncbi.nlm.nih.gov/sra/SRR21844202",
         "", NA, "PRJNA720393", "SAMX1", "SRR", "GCA_000001.1")
  expect_equal(ncbi_normalize_id(x), c(
    "SAMN29555051", "SAMN29555051", "SAMN29555051", "SAMEA1234567", "SAMD00012345",
    "29555051", "SRR21844202", "ERR000001", "SRX17832658", "SRS14384543", "SRR21844202",
    rep(NA, 6)))
  expect_equal(.ncbi_is_sra(c("SRR1", "SAMN1", "DRS9", NA)), c(TRUE, FALSE, TRUE, FALSE))
})
