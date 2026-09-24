test_that("geome_normalize_bcid strips resolver prefixes and whitespace", {
  x <- c("ark:/21547/CYB2REEDY", " ark:/21547/CYB2REEDY ",
         "https://n2t.net/ark:/21547/CYB2REEDY",
         "http://n2t.net/ark:/21547/CYB2REEDY",
         "https://geome-db.org/record/ark:/21547/CYB2REEDY",
         "", NA, "not a bcid", "ark:/abc/X")
  expect_equal(
    geome_normalize_bcid(x),
    c(rep("ark:/21547/CYB2REEDY", 5), NA, NA, NA, NA)
  )
})

test_that(".geome_fetch_chain walks Tissue -> Sample -> Event -> Expedition -> Project", {
  local_mocked_bindings(.geome_get = geome_fixture_get)
  out <- .geome_fetch_chain("ark:/21547/CYC2CMPI38181.1")
  lv <- unique(out[order(out$depth), c("level", "depth")])
  expect_equal(lv$level, c("Tissue", "Sample", "Event", "Expedition", "Project"))
  expect_equal(lv$depth, 0:4)
  expect_equal(out$value[out$level == "Event" & out$field == "country"], "French Polynesia")
  expect_equal(out$value[out$level == "Project" & out$field == "projectTitle"], "Moorea Biocode")
  expect_equal(unique(out$bcid[out$level == "Sample"]), "ark:/21547/CYA2Reedy01")
  expect_false(any(out$field %in% c("user", "project", "entityIdentifiers")))
  expect_type(out$depth, "integer")
})

test_that("shared ancestors are fetched once per cache", {
  local_mocked_bindings(.geome_get = geome_fixture_get)
  geome_reset_calls()
  cache <- new.env()
  .geome_fetch_chain("ark:/21547/CYC2CMPI38181.1", cache)
  .geome_fetch_chain("ark:/21547/CYA2Reedy01", cache)
  expect_equal(geome_calls[["records/ark:/21547/CYB2REEDY"]], 1L)
  expect_equal(geome_calls[["projects"]], 1L)
})

test_that("a missing own record stops with the API message", {
  local_mocked_bindings(.geome_get = geome_fixture_get)
  expect_error(.geome_fetch_chain("ark:/21547/NOPE"), "not found")
})

test_that("a parent loop is refused", {
  fake <- function(path, query = list()) {
    list(record = list(entity = "Sample", bcid = "ark:/1/A"),
         parent = list(entity = "Sample", bcid = "ark:/1/A"))
  }
  local_mocked_bindings(.geome_get = fake)
  expect_error(.geome_fetch_chain("ark:/1/A"), "loops")
})

test_that("a failing expedition/project lookup keeps the record chain", {
  fake <- function(path, query = list()) {
    if (startsWith(path, "records/")) {
      return(list(record = list(entity = "Event", bcid = "ark:/1/E",
                                projectId = "9", expeditionCode = "X", country = "Peru")))
    }
    stop("record is private or needs a GEOME login", call. = FALSE)
  }
  local_mocked_bindings(.geome_get = fake)
  out <- .geome_fetch_chain("ark:/1/E")
  expect_equal(unique(out$level), "Event")
})

test_that("live GEOME walk works", {
  skip_on_cran()
  skip_if_offline("api.geome-db.org")
  out <- .geome_fetch_chain("ark:/21547/CYC2CMPI38181.1")
  expect_true(all(c("Tissue", "Sample", "Event") %in% out$level))
})
