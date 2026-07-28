# Fixture columns follow the real project schema (R/init_db.R): `assemble` has
# assemble_lock, `assemblies` has ignore, and `annotate` carries the WF2 gate.
make_consistency_db <- function(assemble, assemblies = NULL, annotate = NULL) {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbExecute(con, "CREATE TABLE assemble (
    ID TEXT NOT NULL,
    assemble_switch INTEGER,
    assemble_lock INTEGER,
    assemble_opts TEXT,
    PRIMARY KEY (ID)
  )")
  DBI::dbExecute(con, "CREATE TABLE assemblies (
    ID TEXT NOT NULL,
    path INTEGER NOT NULL,
    scaffold INTEGER NOT NULL,
    ignore INTEGER,
    PRIMARY KEY (ID, path, scaffold)
  )")
  DBI::dbExecute(con, "CREATE TABLE annotate (
    ID TEXT NOT NULL,
    path INTEGER NOT NULL DEFAULT 1,
    scaffold INTEGER NOT NULL DEFAULT 1,
    annotate_switch INTEGER,
    annotate_lock INTEGER,
    PRIMARY KEY (ID, path, scaffold)
  )")
  DBI::dbWriteTable(con, "assemble", assemble, append = TRUE)
  if (!is.null(assemblies) && nrow(assemblies) > 0) {
    DBI::dbWriteTable(con, "assemblies", assemblies, append = TRUE)
  }
  if (is.null(annotate)) {
    annotate <- tbl_annotate(unique(assemble$ID))
  }
  if (nrow(annotate) > 0) {
    DBI::dbWriteTable(con, "annotate", annotate, append = TRUE)
  }
  con
}

# Row builders. Defaults describe a sample WF2 would really process: locked,
# not ignored, annotation switched on and still unlocked.
tbl_assemble <- function(ID, assemble_opts = "default", assemble_lock = 1) {
  data.frame(
    ID = ID, assemble_switch = 1, assemble_lock = assemble_lock,
    assemble_opts = assemble_opts, stringsAsFactors = FALSE
  )
}
tbl_assemblies <- function(ID, path = 1, scaffold = 1, ignore = 0) {
  data.frame(
    ID = ID, path = path, scaffold = scaffold, ignore = ignore,
    stringsAsFactors = FALSE
  )
}
tbl_annotate <- function(ID, path = 1, scaffold = 1, annotate_switch = 1,
                         annotate_lock = 0) {
  data.frame(
    ID = ID, path = path, scaffold = scaffold,
    annotate_switch = annotate_switch, annotate_lock = annotate_lock,
    stringsAsFactors = FALSE
  )
}

# Helper: temp output tree. `dirs` maps ID -> option set names and creates the
# directories only; `fasta` maps ID -> list(opts, paths) and additionally writes
# the per-path assembly FASTA that WF2 dereferences.
make_out_tree <- function(dirs = list(), fasta = list()) {
  td <- tempfile()
  dir.create(td, recursive = TRUE)
  for (id in names(dirs)) {
    for (opts in dirs[[id]]) {
      dir.create(file.path(td, id, "assemble", opts), recursive = TRUE)
    }
  }
  for (id in names(fasta)) {
    spec <- fasta[[id]]
    d <- file.path(td, id, "assemble", spec$opts)
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
    for (p in spec$paths) {
      writeLines(">seq", file.path(d, paste0(id, "_assembly_", p, ".fasta")))
    }
  }
  td
}

test_that("assemble_out_dir builds the published path and is vectorized", {
  expect_equal(assemble_out_dir("out", "s1", "default"), "out/s1/assemble/default")
  expect_equal(
    assemble_out_dir("out", c("s1", "s2"), c("default", "fast")),
    c("out/s1/assemble/default", "out/s2/assemble/fast")
  )
})

test_that("a consistent project reports nothing", {
  td <- make_out_tree(fasta = list(
    s1 = list(opts = "default", paths = 1),
    s2 = list(opts = "default", paths = 1)
  ))
  on.exit(unlink(td, recursive = TRUE))

  con <- make_consistency_db(
    assemble = tbl_assemble(c("s1", "s2")),
    assemblies = tbl_assemblies(c("s1", "s2"))
  )
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  expect_equal(nrow(stale_assemble_dirs(con, td)), 0)
})

test_that("a sample reassigned after assembly is reported with what is on disk", {
  td <- make_out_tree(fasta = list(
    s1 = list(opts = "default", paths = 1),
    s2 = list(opts = "default", paths = 1)
  ))
  on.exit(unlink(td, recursive = TRUE))

  # s1 was assembled under 'default' but now points at 'aggressive'
  con <- make_consistency_db(
    assemble = tbl_assemble(c("s1", "s2"), c("aggressive", "default")),
    assemblies = tbl_assemblies(c("s1", "s2"))
  )
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  res <- stale_assemble_dirs(con, td)
  expect_equal(nrow(res), 1)
  expect_equal(res$ID, "s1")
  expect_equal(res$assemble_opts, "aggressive")
  expect_equal(res$expected, paste0(td, "/s1/assemble/aggressive"))
  expect_equal(res$on_disk, "default")
})

test_that("an option-set directory with no assembly FASTA is still reported", {
  # The directory exists (an empty publish dir, or output cleared by hand) but
  # the file WF2 dereferences does not.
  td <- make_out_tree(dirs = list(s1 = "default"))
  on.exit(unlink(td, recursive = TRUE))

  con <- make_consistency_db(
    assemble = tbl_assemble("s1"),
    assemblies = tbl_assemblies("s1")
  )
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  res <- stale_assemble_dirs(con, td)
  expect_equal(nrow(res), 1)
  expect_equal(res$ID, "s1")
  expect_equal(res$assemble_opts, "default")
  expect_equal(res$on_disk, "default")
})

test_that("a multi-path sample is reported once, not once per path", {
  # path 1 published, path 2 did not
  td <- make_out_tree(fasta = list(s1 = list(opts = "default", paths = 1)))
  on.exit(unlink(td, recursive = TRUE))

  con <- make_consistency_db(
    assemble = tbl_assemble("s1"),
    assemblies = tbl_assemblies("s1", path = c(1, 1, 2), scaffold = c(1, 2, 1))
  )
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  res <- stale_assemble_dirs(con, td)
  expect_equal(nrow(res), 1)
  expect_equal(res$ID, "s1")

  # both paths missing collapses to the same single row
  unlink(file.path(td, "s1", "assemble", "default", "s1_assembly_1.fasta"))
  expect_equal(nrow(stale_assemble_dirs(con, td)), 1)
})

test_that("a sample that never assembled is not reported", {
  # s2 is in `assemble` but has no assemblies (e.g. locked out before running),
  # so its missing output directory is expected, not stale
  td <- make_out_tree(fasta = list(s1 = list(opts = "default", paths = 1)))
  on.exit(unlink(td, recursive = TRUE))

  con <- make_consistency_db(
    assemble = tbl_assemble(c("s1", "s2")),
    assemblies = tbl_assemblies("s1")
  )
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  expect_equal(nrow(stale_assemble_dirs(con, td)), 0)
})

test_that("a sample whose scaffolds are all ignored is not reported", {
  td <- make_out_tree()
  on.exit(unlink(td, recursive = TRUE))

  con <- make_consistency_db(
    assemble = tbl_assemble(c("s1", "s2")),
    assemblies = rbind(
      tbl_assemblies("s1", path = c(1, 1), scaffold = c(1, 2), ignore = 1),
      tbl_assemblies("s2", path = c(1, 1), scaffold = c(1, 2), ignore = c(1, 0))
    )
  )
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  res <- stale_assemble_dirs(con, td)
  expect_equal(res$ID, "s2")
})

test_that("a stale sample with no output at all reports an empty on_disk", {
  td <- make_out_tree(fasta = list(s1 = list(opts = "default", paths = 1)))
  on.exit(unlink(td, recursive = TRUE))

  con <- make_consistency_db(
    assemble = tbl_assemble(c("s1", "s2")),
    assemblies = tbl_assemblies(c("s1", "s2"))
  )
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  res <- stale_assemble_dirs(con, td)
  expect_equal(res$ID, "s2")
  expect_equal(res$on_disk, "")
})

test_that("every option set on disk is listed, comma separated", {
  td <- make_out_tree(dirs = list(s1 = c("alpha", "beta")))
  on.exit(unlink(td, recursive = TRUE))

  con <- make_consistency_db(
    assemble = tbl_assemble("s1", "gamma"),
    assemblies = tbl_assemblies("s1")
  )
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  res <- stale_assemble_dirs(con, td)
  expect_equal(nrow(res), 1)
  expect_equal(
    sort(strsplit(res$on_disk, ", ", fixed = TRUE)[[1]]),
    c("alpha", "beta")
  )
})

test_that("pending_only limits the check to samples WF2 would process", {
  td <- make_out_tree()
  on.exit(unlink(td, recursive = TRUE))

  con <- make_consistency_db(
    assemble = rbind(
      tbl_assemble("unlocked", assemble_lock = 0),
      tbl_assemble("done"),
      tbl_assemble("queued"),
      tbl_assemble("switched_off")
    ),
    assemblies = tbl_assemblies(c("unlocked", "done", "queued", "switched_off")),
    annotate = rbind(
      tbl_annotate("unlocked"),
      tbl_annotate("done", annotate_lock = 1),
      tbl_annotate("queued"),
      tbl_annotate("switched_off", annotate_switch = 0)
    )
  )
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  expect_equal(stale_assemble_dirs(con, td)$ID, "queued")
  expect_equal(
    sort(stale_assemble_dirs(con, td, pending_only = FALSE)$ID),
    c("done", "queued", "switched_off", "unlocked")
  )
})

test_that("ids restricts the query", {
  td <- make_out_tree()
  on.exit(unlink(td, recursive = TRUE))

  con <- make_consistency_db(
    assemble = tbl_assemble(c("s1", "s2", "s3")),
    assemblies = tbl_assemblies(c("s1", "s2", "s3"))
  )
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  expect_equal(sort(stale_assemble_dirs(con, td)$ID), c("s1", "s2", "s3"))
  expect_equal(stale_assemble_dirs(con, td, ids = "s2")$ID, "s2")
  expect_equal(sort(stale_assemble_dirs(con, td, ids = c("s1", "s3"))$ID), c("s1", "s3"))
  expect_equal(nrow(stale_assemble_dirs(con, td, ids = character(0))), 0)
  expect_equal(nrow(stale_assemble_dirs(con, td, ids = NA_character_)), 0)
})

test_that("an ID containing an apostrophe does not break the query", {
  td <- make_out_tree()
  on.exit(unlink(td, recursive = TRUE))

  con <- make_consistency_db(
    assemble = tbl_assemble(c("O'Brien_1", "s2")),
    assemblies = tbl_assemblies(c("O'Brien_1", "s2"))
  )
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  res <- stale_assemble_dirs(con, td, ids = "O'Brien_1")
  expect_equal(res$ID, "O'Brien_1")
  expect_equal(nrow(res), 1)
})

test_that("a sample with no assembly option set assigned is reported", {
  td <- make_out_tree(dirs = list(s1 = "default", s2 = "default"))
  on.exit(unlink(td, recursive = TRUE))

  con <- make_consistency_db(
    assemble = tbl_assemble(c("s1", "s2"), c(NA_character_, "")),
    assemblies = tbl_assemblies(c("s1", "s2"))
  )
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  res <- stale_assemble_dirs(con, td)
  expect_equal(sort(res$ID), c("s1", "s2"))
  expect_true(all(is.na(res$assemble_opts)))
  expect_true(all(is.na(res$expected)))
  expect_equal(res$on_disk, c("default", "default"))
})

test_that("an unusable dir_out is skipped without error", {
  con <- make_consistency_db(
    assemble = tbl_assemble("s1"),
    assemblies = tbl_assemblies("s1")
  )
  on.exit(DBI::dbDisconnect(con))

  for (bad in list(NA_character_, "", character(0), NULL,
                   file.path(tempdir(), "no_such_project_dir"))) {
    expect_equal(nrow(stale_assemble_dirs(con, bad)), 0)
  }
})

test_that("a database without the project tables is skipped without error", {
  td <- make_out_tree(dirs = list(s1 = "default"))
  on.exit(unlink(td, recursive = TRUE))

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  expect_equal(nrow(stale_assemble_dirs(con, td)), 0)
  expect_equal(nrow(stale_assemble_dirs(con, td, pending_only = FALSE)), 0)
})

test_that("assemble_dirs_on_disk lists option sets only for known samples", {
  td <- make_out_tree(dirs = list(s1 = c("alpha", "beta")))
  on.exit(unlink(td, recursive = TRUE))

  expect_equal(sort(assemble_dirs_on_disk(td, "s1")), c("alpha", "beta"))
  expect_equal(assemble_dirs_on_disk(td, "nope"), character(0))
  expect_equal(assemble_dirs_on_disk(NA_character_, "s1"), character(0))
  expect_equal(assemble_dirs_on_disk("", "s1"), character(0))
})

test_that("assemble_dirs_on_disk ignores dot-directories", {
  td <- make_out_tree(dirs = list(s1 = c("alpha", ".nextflow", ".cache")))
  on.exit(unlink(td, recursive = TRUE))

  expect_equal(assemble_dirs_on_disk(td, "s1"), "alpha")
})

test_that("stale_assemble_items describes each stale sample", {
  stale <- data.frame(
    ID = c("s1", "s2", "s3"),
    assemble_opts = c("aggressive", NA, "default"),
    expected = c("out/s1/assemble/aggressive", NA, "out/s3/assemble/default"),
    on_disk = c("default", "default, fast", ""),
    stringsAsFactors = FALSE
  )
  txt <- vapply(stale_assemble_items(stale), as.character, character(1))

  expect_length(txt, 3)
  expect_match(txt[1], "<b>s1</b>")
  expect_match(txt[1], "points at parameter set")
  expect_match(txt[1], "<code>aggressive</code>")
  expect_match(txt[1], "<code>default</code>")
  expect_match(txt[2], "has no assembly parameter set assigned")
  expect_match(txt[2], "<code>default, fast</code>")
  expect_match(txt[3], "missing")
  expect_false(grepl("<code></code>", txt[3], fixed = TRUE))
})

test_that("stale_assemble_items caps the list and counts the remainder", {
  stale <- data.frame(
    ID = paste0("s", 1:12),
    assemble_opts = "default",
    expected = "out",
    on_disk = "",
    stringsAsFactors = FALSE
  )
  items <- stale_assemble_items(stale)
  expect_length(items, 11)
  expect_match(as.character(items[[11]]), "... and 2 more", fixed = TRUE)

  expect_length(stale_assemble_items(stale, max_items = 12L), 12)
  expect_length(stale_assemble_items(stale[0, , drop = FALSE]), 0)
})
