test_that(".spec_norm_name folds case, accents, and punctuation", {
  expect_equal(.spec_norm_name(c("C\u00f4te d\u2019Ivoire", "  United  States of America ", "U.S.A.")),
               c("cote divoire", "united states of america", "u s a"))
})

test_that(".spec_norm_name is stable across platform-dependent transliteration marks", {
  expect_equal(.spec_norm_name("C^ote d'Ivoire"), .spec_norm_name("Cote dIvoire"))
})

test_that("country names, aliases, and codes map to ISO2", {
  x <- c("USA", "United States", "united states of america", "US", "USA: Florida",
         "C\u00f4te d\u2019Ivoire", "Ivory Coast", "Namibia", "NA", "Atlantis", "", NA)
  expect_equal(vapply(x, .spec_country_iso2, "", USE.NAMES = FALSE),
               c(rep("US", 5), "CI", "CI", "NA", "NA", NA, NA, NA))
})

test_that("coordinates parse in decimal and hemisphere forms", {
  expect_equal(.spec_parse_coords("17.5 S 149.8 W"), c(-17.5, -149.8))
  expect_equal(.spec_parse_coords("28.5378, -81.3332"), c(28.5378, -81.3332))
  expect_equal(.spec_parse_coords("28.5 -81.3"), c(28.5, -81.3))
  expect_null(.spec_parse_coords("95 10"))
  expect_null(.spec_parse_coords("near the lake"))
  expect_null(.spec_parse_coords(NA))
})

test_that("dates parse in ISO and GenBank forms", {
  expect_equal(.spec_parse_date("2026-02-23"), c(y = 2026L, m = 2L, d = 23L))
  expect_equal(.spec_parse_date("2026-02-23T10:00:00"), c(y = 2026L, m = 2L, d = 23L))
  expect_equal(.spec_parse_date("23-Feb-2026"), c(y = 2026L, m = 2L, d = 23L))
  expect_equal(.spec_parse_date("Feb-2026"), c(y = 2026L, m = 2L, d = NA))
  expect_equal(.spec_parse_date("2026"), c(y = 2026L, m = NA, d = NA))
  expect_null(.spec_parse_date("2020-01-01/2020-12-31"))
  expect_null(.spec_parse_date("spring 2020"))
})

test_that(".spec_compare applies each concept's rule", {
  cmp <- .spec_compare
  expect_equal(cmp("coordinates", "28.5378, -81.3332", "28.53783 N 81.33322 W"), "agree")
  expect_equal(cmp("coordinates", "28.53, -81.33", "28.54 N 81.33 W"), "agree")
  expect_equal(cmp("coordinates", "28.52, -81.33", "28.54 N 81.33 W"), "conflict")
  expect_equal(cmp("coordinates", "near the lake", "28.5 N 81.3 W"), "note")
  expect_equal(cmp("collection_date", "spring 2020", "2020-04-01"), "note")
  expect_equal(cmp("collection_date", "2026", "2026-02-23"), "agree")
  expect_equal(cmp("collection_date", "23-Feb-2026", "2026-02-23"), "agree")
  expect_equal(cmp("collection_date", "Feb-2026", "2026-03-01"), "conflict")
  expect_equal(cmp("country", "USA: Florida", "US"), "agree")
  expect_equal(cmp("country", "United States of America", "USA"), "agree")
  expect_equal(cmp("country", "Canada", "US"), "conflict")
  expect_equal(cmp("country", "Atlantis", "US"), "note")
  expect_equal(cmp("country", "Atlantis: north", "atlantis"), "agree")
  expect_equal(cmp("voucher", "UF 250399", "UF:Fish:250399"), "agree")
  expect_equal(cmp("voucher", "uf fish 250399", "250399"), "agree")
  expect_equal(cmp("voucher", "250398", "UF:Fish:250399"), "conflict")
  expect_equal(cmp("taxon", "Notemigonus crysoleucas", "Notemigonus crysoleucas (Mitchill, 1814)"), "agree")
  expect_equal(cmp("taxon", "Notemigonus sp.", "Notemigonus crysoleucas"), "conflict")
  expect_equal(cmp("locality", " Lake  Underhill", "lake underhill"), "agree")
  expect_equal(cmp("locality", "Lake Underhill", "Lake Eola"), "note")
  expect_equal(cmp("collector", "R. Robins", "Robins, Robert"), "note")
  expect_equal(cmp("sex", "Female", "female"), "agree")
  expect_equal(cmp("dev_stage", "adult", "juvenile"), "note")
})

test_that(".spec_status reports single, the worst pair, or NA", {
  expect_true(is.na(.spec_status("sex", c(NA, "", NA))))
  expect_equal(.spec_status("sex", c("male", NA, NA)), "single")
  expect_equal(.spec_status("country", c("USA", NA, "US")), "agree")
  expect_equal(.spec_status("country", c("USA", "US", "Canada")), "conflict")
  expect_equal(.spec_status("collector", c(NA, "A. B", "Ann B")), "note")
})
