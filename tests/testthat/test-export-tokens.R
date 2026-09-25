tok_data <- function() {
  data.frame(
    ID = c("s1", "s2"), seqid = c("s1", "s2"), path = 1L, scaffold = 1L,
    Taxon = c("Fish a", "Fish b"), genetic_code = 2L, topology = "circular",
    completeness = "complete genome", site = c("Pond", "Lake"),
    GEOME_BCID = c("ark:/1/A", NA), GBIF_ID = c(NA, "6186461308"),
    geome_lat_lon = c("17.5 S 149.8 W", ""), gbif_Occurrence_countryCode = c("", "US"),
    blast_accession = "NC_1", length = 16500L, annotate_switch = 1L,
    blast_accession_auto = 0L, poor_blast_ref = "ok", export_time_stamp = NA,
    export_group = "g1", specimen = "ok", specimen_message = "GEOME: fetched", specimen_icons = "GEOME:ok"
  )
}
tok_cols <- c("ID", "Taxon", "genetic_code", "R1", "R2", "site", "GEOME_BCID", "GBIF_ID")

test_that("export_token_groups sorts columns into the six groups", {
  g <- export_token_groups(tok_data(), tok_cols,
                           c("geome:combo:lat_lon", "gbif:raw:Occurrence:countryCode"))
  expect_equal(names(g), c("Basics", "Your mapfile columns", "GEOME", "GBIF",
                           "Reference (BLAST)", "Assembly and annotation"))
  expect_equal(g$Basics$tokens$token,
               c("seqid", "ID", "Taxon", "genetic_code", "topology", "completeness", "path", "scaffold"))
  expect_equal(g$Basics$tokens$insert[3], "{Taxon}")
  expect_equal(g$`Your mapfile columns`$tokens$token, "site")
  expect_equal(g$GEOME$tokens$token, c("GEOME_BCID", "geome_lat_lon"))
  expect_equal(g$GEOME$tokens$insert, c("{GEOME_BCID}", "[lat_lon={geome_lat_lon}]"))
  expect_equal(g$GEOME$tokens$example, c("ark:/1/A", "17.5 S 149.8 W"))
  expect_equal(g$GBIF$tokens$insert, c("{GBIF_ID}", "{gbif_Occurrence_countryCode}"))
  expect_equal(g$GBIF$tokens$example, c("", ""))
  expect_equal(g$`Reference (BLAST)`$tokens$token, "blast_accession")
  expect_equal(g$`Assembly and annotation`$tokens$token, "length")
  expect_equal(unname(vapply(g, function(x) x$open, logical(1))),
               c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE))
  listed <- unlist(lapply(g, function(x) x$tokens$token))
  expect_false(any(c("annotate_switch", "blast_accession_auto", "poor_blast_ref", "export_time_stamp",
                     "export_group", "specimen", "specimen_message", "specimen_icons", "R1", "R2") %in% listed))
  expect_null(g$Basics$hint)
})

test_that("groups with nothing ticked or no extra columns say how to add some", {
  d <- tok_data()[, c("ID", "seqid", "Taxon", "GEOME_BCID", "GBIF_ID")]
  g <- export_token_groups(d, c("ID", "Taxon", "GEOME_BCID", "GBIF_ID"), character())
  expect_false(g$GEOME$open)
  expect_false(g$GBIF$open)
  expect_match(g$GEOME$hint, "Metadata Export", fixed = TRUE)
  expect_match(g$GBIF$hint, "Metadata Export", fixed = TRUE)
  expect_true(g$`Your mapfile columns`$open)
  expect_match(g$`Your mapfile columns`$hint, "no extra columns", fixed = TRUE)
  expect_equal(nrow(g$`Your mapfile columns`$tokens), 0L)
  expect_equal(g$GEOME$tokens$token, "GEOME_BCID")
})

test_that("export_token_groups copes with no rows", {
  g <- export_token_groups(tok_data()[0, ], tok_cols, "geome:combo:lat_lon")
  expect_equal(g$GEOME$tokens$example, c("", ""))
})

test_that("export_token_ui renders chips with insert text, a filter box, and picker links", {
  g <- export_token_groups(tok_data(), tok_cols, "geome:combo:lat_lon")
  html <- as.character(export_token_ui(g, target_id = "export-fasta_header", ns = NS("export")))
  expect_match(html, "data-target=\"export-fasta_header\"", fixed = TRUE)
  expect_match(html, "data-insert=\"[lat_lon={geome_lat_lon}]\"", fixed = TRUE)
  expect_match(html, "data-insert=\"{Taxon}\"", fixed = TRUE)
  expect_match(html, "mp-token-filter", fixed = TRUE)
  expect_match(html, "export-token_fields_geome", fixed = TRUE)
  expect_match(html, "export-token_fields_gbif", fixed = TRUE)
  expect_match(html, "First row: Pond", fixed = TRUE)
  expect_equal(lengths(regmatches(html, gregexpr("<details open", html))), 3L)
})
