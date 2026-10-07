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

test_that("a BioSample accession yields BioSample and BioProject levels", {
  local_mocked_bindings(.ncbi_get = ncbi_fixture_get)
  out <- .ncbi_fetch_chain("SAMN63236902")
  lv <- unique(out[order(out$depth), c("level", "depth", "ref")])
  expect_equal(lv$level, c("BioSample", "BioProject"))
  expect_equal(lv$depth, 1:2)
  expect_equal(lv$ref, c("SAMN63236902", "PRJNA1422759"))
  bs <- out[out$level == "BioSample", ]
  v <- function(f) bs$value[bs$field == f]
  expect_equal(v("organism"), "Zoarces americanus")
  expect_equal(v("taxonomy_id"), "8199")
  expect_equal(v("collection_date"), "2024-06-27")
  expect_equal(v("geo_loc_name"), "Canada: Scotian Shelf, Nova Scotia, Atlantic Ocean")
  expect_match(v("identified_by"), "^Ryan Martin")
  expect_equal(v("sex"), "missing")
  expect_equal(v("ToLID"), "fZoaAme1")
  expect_equal(v("sample_name"), "OceanPout01-tissueD")
  expect_false(any(duplicated(bs$field)))
  bp <- out[out$level == "BioProject", ]
  expect_equal(bp$value[bp$field == "umbrella"], "PRJNA1422710")
  expect_equal(bp$value[bp$field == "material"], "Genome")
  expect_match(bp$value[bp$field == "title"], "ocean pout", fixed = TRUE)
  expect_type(out$depth, "integer")
})

test_that("an SRA run resolves through its BioSample", {
  local_mocked_bindings(.ncbi_get = ncbi_fixture_get)
  out <- .ncbi_fetch_chain("SRR21844202")
  expect_equal(unique(out$level[order(out$depth)]), c("SRA", "BioSample", "BioProject"))
  sra <- out[out$level == "SRA", ]
  expect_equal(unique(sra$ref), "SRR21844202")
  expect_equal(sra$value[sra$field == "biosample"], "SAMN29555051")
  expect_equal(sra$value[sra$field == "library_layout"], "PAIRED")
  expect_equal(sra$value[sra$field == "runs"], "SRR21844202")
  expect_equal(unique(out$ref[out$level == "BioSample"]), "SAMN29555051")
})

test_that("unknown IDs stop with a clear message", {
  local_mocked_bindings(.ncbi_get = ncbi_fixture_get)
  expect_error(.ncbi_fetch_chain("SAMN99999999999"), "BioSample SAMN99999999999 not found")
  expect_error(.ncbi_fetch_chain("SRR99999999999"), "SRA accession SRR99999999999 not found")
})

test_that("an SRA record without a BioSample stops", {
  local_mocked_bindings(.ncbi_get = function(endpoint, query) {
    if (endpoint == "esummary") {
      return('{"result":{"uids":["1"],"1":{"expxml":"<Summary><Title>x</Title></Summary>","runs":""}}}')
    }
    "<eSearchResult><Count>1</Count><IdList><Id>1</Id></IdList></eSearchResult>"
  })
  expect_error(.ncbi_fetch_chain("SRR1"), "SRA accession SRR1 has no linked BioSample")
})

test_that("a failing BioProject keeps the BioSample and the other projects", {
  local_mocked_bindings(.ncbi_get = function(endpoint, query) {
    if (query$db == "biosample") {
      x <- ncbi_fixture_get(endpoint, query)
      return(sub("</Links>", '<Link type="entrez" target="bioproject" label="PRJNA1">1</Link></Links>', x,
                 fixed = TRUE))
    }
    ncbi_fixture_get(endpoint, query)
  })
  out <- .ncbi_fetch_chain("SAMN63236902")
  expect_equal(sum(out$level == "BioSample" & out$field == "accession"), 1L)
  expect_equal(out$value[out$level == "BioProject" & out$field == "accession"], "PRJNA1422759")
})

test_that("BioProjects are fetched once per cache", {
  local_mocked_bindings(.ncbi_get = ncbi_fixture_get)
  ncbi_reset_calls()
  cache <- new.env()
  .ncbi_fetch_chain("SAMN63236902", cache)
  .ncbi_fetch_chain("SAMN63236902", cache)
  expect_equal(ncbi_calls[["efetch_bioproject_1422759.xml"]], 1L)
})

test_that(".ncbi_get sends tool and api_key and spaces requests", {
  seen <- list()
  local_mocked_bindings(req_perform = function(req, ...) {
    seen[[length(seen) + 1L]] <<- list(url = req$url, t = Sys.time())
    httr2::response(status_code = 200, body = charToRaw("<x/>"))
  }, .package = "httr2")
  withr::local_envvar(ENTREZ_KEY = "abc", NCBI_API_KEY = "")
  .ncbi_get("efetch", list(db = "biosample", id = "1"))
  .ncbi_get("efetch", list(db = "biosample", id = "2"))
  expect_match(seen[[1]]$url, "tool=MitoPilot", fixed = TRUE)
  expect_match(seen[[1]]$url, "api_key=abc", fixed = TRUE)
  expect_gte(as.numeric(difftime(seen[[2]]$t, seen[[1]]$t, units = "secs")), 0.1)
})

test_that(".ncbi_api_key prefers project key, then NCBI_API_KEY, then ENTREZ_KEY", {
  withr::defer(.ncbi_env$project_key <- NULL)
  withr::local_envvar(NCBI_API_KEY = "nak", ENTREZ_KEY = "ek")
  .ncbi_env$project_key <- "proj"
  expect_equal(.ncbi_api_key(), "proj")
  .ncbi_env$project_key <- NULL
  expect_equal(.ncbi_api_key(), "nak")
  withr::local_envvar(NCBI_API_KEY = "")
  expect_equal(.ncbi_api_key(), "ek")
})

test_that(".ncbi_project_key reads ncbi_api_key from the project .config", {
  d <- withr::local_tempdir()
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(d, ".sqlite"))
  on.exit(DBI::dbDisconnect(con))
  expect_null(.ncbi_project_key(con))
  writeLines("    ncbi_api_key = 'abc123'       // comment", file.path(d, ".config"))
  expect_equal(.ncbi_project_key(con), "abc123")
  writeLines("    ncbi_api_key = ''", file.path(d, ".config"))
  expect_null(.ncbi_project_key(con))
  writeLines("    ncbi_api_key = '<<NCBI_API_KEY>>'", file.path(d, ".config"))
  expect_null(.ncbi_project_key(con))
})
