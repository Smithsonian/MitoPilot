# The user-assembly test project only works if its packaged pieces line up: the
# mapping file, the assemblies it names, and the donor read files each sample
# borrows.

test_that("the userAsmb test mapping matches the packaged data", {
  mapping_fn <- app_sys(file.path("test_data", "mapping_test_userAsmb.csv"))
  skip_if(mapping_fn == "", "packaged userAsmb test data not installed")

  mapping <- utils::read.csv(mapping_fn)
  expect_true(all(c("ID", "Taxon", "R1", "R2", "Assembly", "Topology", "Donors")
                  %in% names(mapping)))
  # Blank for the multi-contig samples: a draft assembly has no topology to
  # declare, and MitoPilot works it out per contig instead.
  expect_true(all(mapping$Topology %in% c("linear", "circular", "")))
  expect_true(all(mapping$Topology[grepl("^UA_MULTI_", mapping$ID)] == ""))
  expect_false(any(duplicated(mapping$ID)))

  for (fn in mapping$Assembly) {
    expect_true(file.exists(app_sys(file.path("test_data", "assemblies", fn))),
                info = fn)
  }

  donors <- unique(unlist(strsplit(mapping$Donors, ";")))
  for (acc in donors) {
    for (mate in c("R1", "R2")) {
      expect_true(
        file.exists(app_sys(file.path("test_data", paste0(acc, "_", mate, ".fastq.gz")))),
        info = paste(acc, mate)
      )
    }
  }

  expect_true(file.exists(app_sys(file.path("test_data", "fish_mito_sampler.gb"))))
})
