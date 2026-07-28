# Rebuilds a notes cell exactly as do_join_merge() in R/app_annotate_details.R
# does, so the reader regexes below are exercised against production strings.
make_join_note <- function(join_mode, grp, slip_note = NULL,
                           old_note = NA_character_) {
  marker <- stringr::str_glue("JOIN: mode={join_mode} group={grp}")
  if (identical(join_mode, "frameshift") && !is.null(slip_note) &&
      nzchar(trimws(slip_note))) {
    marker <- paste0(
      marker, " note=", stringr::str_replace_all(trimws(slip_note), ";", ",")
    )
  }
  paste(marker, old_note %|NA|% "", sep = "; ") |> stringr::str_remove("; $")
}

# The export reader (R/export.R, around lines 363-371).
read_join_marker <- function(note) {
  m <- stringr::str_match(
    note %|NA|% "", "^JOIN: mode=(\\w+) group=(\\d+)( note=([^;]*))?"
  )
  list(mode = m[, 2], group = m[, 3], note = m[, 5])
}

# The group member scan (R/export.R around line 377, mirrored in the modal).
join_member_hits <- function(notes, grp) {
  stringr::str_detect(
    dplyr::coalesce(notes, ""),
    paste0("^JOIN: mode=\\w+ group=", grp, "\\b")
  )
}

# The un-join observer's stripping regex (R/app_annotate_details.R line ~4130).
strip_join_marker <- function(notes) {
  stringr::str_remove(
    notes, "^JOIN: mode=\\w+ group=\\d+( note=[^;]*)?(; )?"
  )
}

test_that("next_join_group returns 1 when there are no join markers", {
  expect_equal(next_join_group(c(NA_character_, NA_character_)), 1L)
  expect_equal(next_join_group(character(0)), 1L)
  expect_equal(next_join_group(c("manual edit", "", NA)), 1L)
})

test_that("next_join_group returns 1 for an all-NA logical column", {
  expect_equal(next_join_group(c(NA, NA, NA)), 1L)
})

test_that("next_join_group steps past the largest existing group", {
  expect_equal(next_join_group("JOIN: mode=exon group=1; old note"), 2L)
  expect_equal(
    next_join_group(c(NA, "JOIN: mode=exon group=1", "JOIN: mode=frameshift group=2")),
    3L
  )
})

test_that("next_join_group compares group ids numerically", {
  notes <- c("JOIN: mode=exon group=2", "JOIN: mode=exon group=10")
  expect_equal(next_join_group(notes), 11L)
})

test_that("next_join_group handles a full annotation table's notes column", {
  notes <- rep(NA_character_, 37)
  notes[5] <- "JOIN: mode=exon group=1"
  notes[12] <- "JOIN: mode=exon group=1; manual edit"
  expect_equal(next_join_group(notes), 2L)
})

test_that("next_join_group is vectorized where %|NA|% is not", {
  expect_error(c(NA, "a") %|NA|% "")
  expect_equal(next_join_group(c(NA, "a")), 1L)
})

test_that("next_join_group only counts anchored JOIN markers", {
  expect_equal(next_join_group("read depth drops where group=5 starts"), 1L)
  expect_equal(
    next_join_group(c("group=7", "see group=12 in the notes", NA)),
    1L
  )
  expect_equal(next_join_group("JOIN: mode=exon group=3; see group=99"), 4L)
})

test_that("a marker built by do_join_merge round trips through the export reader", {
  note <- make_join_note("exon", 3, old_note = "manual edit")
  expect_equal(note, "JOIN: mode=exon group=3; manual edit")

  got <- read_join_marker(note)
  expect_equal(got$mode, "exon")
  expect_equal(got$group, "3")
  expect_true(is.na(got$note))
  expect_equal(next_join_group(note), 4L)
})

test_that("a frameshift slippage note survives the round trip", {
  note <- make_join_note(
    "frameshift", 2,
    slip_note = " slip at 4021; confirmed, twice ",
    old_note = "manual edit"
  )
  expect_equal(
    note,
    "JOIN: mode=frameshift group=2 note=slip at 4021, confirmed, twice; manual edit"
  )

  got <- read_join_marker(note)
  expect_equal(got$mode, "frameshift")
  expect_equal(got$group, "2")
  expect_equal(got$note, "slip at 4021, confirmed, twice")
  expect_equal(strip_join_marker(note), "manual edit")
})

test_that("the member scan does not match a group with the same prefix", {
  notes <- c(
    make_join_note("exon", 1),
    make_join_note("exon", 10),
    make_join_note("exon", 1, old_note = "second segment"),
    NA_character_
  )
  expect_equal(join_member_hits(notes, "1"), c(TRUE, FALSE, TRUE, FALSE))
  expect_equal(join_member_hits(notes, "10"), c(FALSE, TRUE, FALSE, FALSE))
})

test_that("un-joining a row with no original note leaves no dangling separator", {
  note <- make_join_note("exon", 1)
  expect_equal(note, "JOIN: mode=exon group=1")
  expect_equal(strip_join_marker(note), "")

  fs <- make_join_note("frameshift", 1, slip_note = "slip at 4021")
  expect_equal(fs, "JOIN: mode=frameshift group=1 note=slip at 4021")
  expect_equal(strip_join_marker(fs), "")
})
