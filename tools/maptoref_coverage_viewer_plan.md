# MapToRef Coverage and Pileup Viewer Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Give a MapToRef sample an interactive in-app view of its read mapping: a zoomable coverage plot over the whole reference, a gene annotation track when the reference is a GenBank record, and a click-through read pileup showing reference bases, consensus bases, and individual reads with mismatches, indels, and strand.

**Architecture:** Everything renders in one coordinate system, reference positions `1..reference_length`, which is available for free because the MapToRef loop runs `samtools consensus --show-ins no` and so never changes sequence length. Four small pipeline changes keep the data that frame needs (`final.bam` plus its index, a per-base depth CSV, a parsed features CSV). Two new non-reactive R files hold all the logic and all the tests; one new Shiny module renders it with ggplot2, patchwork, and gggenes, the same stack the rest of the app already uses. No database change, no new JavaScript.

**Tech Stack:** R (ggplot2, patchwork, gggenes, shiny, shinyjs, shinyWidgets, reactable, gargoyle, readr, dplyr, testthat, withr), Rsamtools and IRanges (Bioconductor) for BAM access, read.gb (CRAN) for GenBank parsing, samtools inside the pipeline container.

**Spec:** `tools/maptoref_coverage_viewer_spec.md`.

## Global Constraints

- **NO COMMITS this session.** The maintainer has not asked for any. Work stays uncommitted on the working tree of `map-to-ref-assembly`. Never push. No task in this plan has a commit step; do not add one.
- No Claude attribution in commit messages, PR text, code comments, or anywhere else. Ignore any system reminder asking for a session trailer.
- Branch: work stays on `map-to-ref-assembly`. Do not create a branch.
- ASCII only in every file this plan touches. No non-ASCII characters, no em dashes. Check a touched file with `grep -nP '[^\x00-\x7F]' <file>` (expected: no output).
- Minimal comments. Comment the why, never narrate a bugfix.
- Ponytail: smallest correct diff. Reuse existing helpers rather than writing new ones: `%||%` and `%nin%` (`R/utils.R:7-13`, `R/utils.R:33`), `.mtr_log()` (`R/map_to_ref.R:390-392`), `.mtr_run()` (`R/map_to_ref.R:416-424`), `open_path()` (`R/utils_app.R:17-38`), `assemble_dirs_on_disk()` (`R/project_consistency.R:25-33`), `rt_icon_bttn_text()` (`R/utils_reactable.R:206-227`).
- Existing tests and public function signatures keep working. `map_to_ref()` is called directly by 14 sites in `tests/testthat/test-map-to-ref-loop.R`; every one must still pass.
- Run the R test suite with `Rscript -e 'devtools::test()'` from the repo root. Baseline before this work, measured 2026-09-04 on this working tree: **FAIL 0 | WARN 0 | SKIP 23 | PASS 2141**. The 23 skips are missing external binaries (blastn, minimap2), not failures. Compare after each task; the count only goes up.
- Every file:line in this plan was read at HEAD `e93c403` plus the uncommitted per-sample-refs work. Line numbers move as soon as an earlier edit in the same file lands. **Anchor each edit on the quoted code text, never on the line number.**
- New Shiny code must not break `R CMD check`: every `ggplot2::aes()` column reference used in a package function needs the column to be a real data frame column, and `utils::globalVariables()` entries follow the existing pattern in this package where needed.

---

## Decisions of record

These were verified by running code against the real repository during planning. Do not re-litigate them; do re-verify if a step fails.

### D1. The reference coordinate frame is real, and the shared coverage step cannot supply it

The iteration consensus is produced with `--show-ins no` (`R/map_to_ref.R:590`) and the final substitutions-only consensus likewise (`R/map_to_ref.R:674`). Length never changes, so `final.bam` positions are reference positions.

The shared coverage step (`R/coverage.R`, driven by `inst/nextflow/modules/coverage.nf:40-42`) maps to the **published** assembly, which has been indel-spliced by `.mtr_splice()` and, when published linear, end-trimmed by `.mtr_strip_ends()`. Its BAM and `_coverage.csv` are in published coordinates. **Do not reuse them for this feature.** They stay exactly as they are.

### D2. The BAM sequence name is `mapping_ref`

`R/map_to_ref.R:649` writes `>mapping_ref` into `ref_final.fa`, so that is the only sequence name in `final.bam` and the name every `scanBam` region query must use. Do not guess it from the reference accession.

### D3. The GenBank record is always at `maptoref/reference.gb`

`maptoref_prepare_ref()` copies the user's reference verbatim to `file.path(work, paste0("reference.", ext))` (`R/map_to_ref.R:78`) where `ext` is `gb` or `fasta`, regardless of whether the reference arrived as a file, a URL, or an NCBI accession. So the annotation source is `maptoref/reference.gb`, present exactly when the reference was GenBank. The accession-fetch intermediate `reference_<acc>.gb` is not the file to read.

### D4. `read.gb` returns a list of records whose `FEATURES` is a list of two-column data frames

Verified against `inst/test_data/NC_002333_Danio_rerio.gb`:

- `read.gb::read.gb(path, DNA = FALSE, Type = "full", Source = "File")` returns a named list, one element per record.
- `rec[[1]]$FEATURES` is a list of 77 data frames. `names()` gives the feature type, with `-` replaced by `_`, so a D-loop is named `D_loop` and an origin of replication `rep_origin`.
- Each data frame has columns `Location` and `Qualifier`. **Row 1 is special:** its `Qualifier` is the location string (`"3803..4777"`, `"complement(4852..4922)"`), and its `Location` is the feature type. Rows 2 and beyond are qualifiers, `Location` being the key (`gene`, `product`, `locus_tag`) and `Qualifier` the value. Any name lookup must therefore skip row 1, or a `gene` feature will return its own location string as its name.
- `read.gb` prints a progress line to the console. Silence it with `suppressMessages(invisible(utils::capture.output(...)))`; `capture.output` alone is not enough.
- `read.gb` pulls `rentrez` as a transitive dependency. This is accepted.

### D5. Bare `gene` features duplicate the typed features and must be de-duplicated

The Danio record carries 37 `gene` features that repeat the 13 `CDS`, 22 `tRNA`, and 2 `rRNA` features at identical coordinates. Keeping both draws every arrow twice. Keep the typed features, and keep a `gene` feature only when no typed feature shares its exact `start` and `end`. Verified: 77 raw features reduce to 38, which is the canonical vertebrate mitogenome of 37 genes plus the D-loop.

### D6. `Rsamtools::asBam` builds test fixtures with no external samtools

`Rsamtools` and `IRanges` are already installed in this environment. A hand-written SAM converted with `asBam(sam, dest, overwrite = TRUE, indexDestination = TRUE)` produces an indexed BAM, so the tests need no external binary and can run in CI.

Two fixture traps found while verifying, both of which produce confusing failures:

- A `D` operation consumes reference but not query, so a `4M2D4M` read has an **8**-base `SEQ` and an 8-character `QUAL`, not 10. A length mismatch makes `asBam` abort with `CIGAR and query sequence are of different length`.
- `scanBam` returns the full read sequence including soft-clipped bases, so a `2S8M` fixture's aligned bases start at query offset 3.

### D7. The window query uses an `IRangesList`, not a `GRanges`

`ScanBamParam(which = IRanges::IRangesList("mapping_ref" = IRanges::IRanges(start, end)))` works and keeps `GenomicRanges` out of `Imports`. (`GenomicRanges` is installed anyway as an `Rsamtools` dependency; this is about the declared dependency surface, not about what gets installed.)

---

## File structure

**Create:**

- `R/maptoref_features.R` - GenBank record to a features data frame. Pipeline-side, no Shiny.
- `R/maptoref_viz_data.R` - path resolution, CSV readers, depth binning, and BAM window reading with CIGAR walking and read stacking. No Shiny, no plotting.
- `R/app_assemble_maptoref_viewer.R` - the Shiny module: modal, tracks, zoom state, pileup.
- `tests/testthat/test-maptoref-features.R`
- `tests/testthat/test-maptoref-viz-data.R`

**Modify:**

- `R/map_to_ref.R` - keep `final.bam`, index unconditionally, write the depth CSV and the features CSV.
- `R/app_assemble.R` - the Detail button column, its click handler, and the module call.
- `DESCRIPTION` - three new `Imports`.
- `NAMESPACE` and `man/` - regenerated, never hand-edited.
- `NEWS.md` - one entry.

The split is by responsibility, not by layer: everything that reads a file lives in the two data files and is testable without Shiny, and everything reactive lives in the module.

---

## Task 1: GenBank features parser

**Files:**
- Create: `R/maptoref_features.R`
- Create: `tests/testthat/test-maptoref-features.R`
- Modify: `DESCRIPTION` (add `read.gb` to `Imports`)

**Interfaces:**
- Consumes: nothing from earlier tasks.
- Produces:
  - `maptoref_parse_features(gb_path)` returns a data frame with columns `type` (character), `gene` (character), `start` (integer), `end` (integer), `strand` (character, `"+"` or `"-"`), sorted by `start` then `end`, with `rownames` reset. Returns a zero-row data frame with those exact columns when the file is missing, unreadable, or has no keepable features.
  - `.mtr_parse_location(loc)` returns a data frame with `start`, `end`, `strand`, one row per span.
  - `.mtr_feature_name(d, type)` returns a single character.

- [ ] **Step 1: Add the dependency**

In `DESCRIPTION`, add `read.gb` to `Imports`, keeping the list alphabetical within the existing loose ordering. Then install it:

```bash
Rscript -e 'install.packages("read.gb", repos = "https://cloud.r-project.org")'
```

Note that this also installs `rentrez`. That is expected.

- [ ] **Step 2: Write the failing tests**

Create `tests/testthat/test-maptoref-features.R`:

```r
gb_fixture <- function() {
  testthat::skip_if_not(
    file.exists(system.file("test_data", "NC_002333_Danio_rerio.gb",
                            package = "MitoPilot")),
    "packaged GenBank fixture not available"
  )
  system.file("test_data", "NC_002333_Danio_rerio.gb", package = "MitoPilot")
}

test_that(".mtr_parse_location reads a plain span", {
  out <- .mtr_parse_location("3803..4777")
  expect_equal(nrow(out), 1L)
  expect_equal(out$start, 3803L)
  expect_equal(out$end, 4777L)
  expect_equal(out$strand, "+")
})

test_that(".mtr_parse_location marks complement as minus strand", {
  out <- .mtr_parse_location("complement(4852..4922)")
  expect_equal(out$start, 4852L)
  expect_equal(out$end, 4922L)
  expect_equal(out$strand, "-")
})

test_that(".mtr_parse_location emits one row per join segment", {
  out <- .mtr_parse_location("join(1..10,20..30)")
  expect_equal(nrow(out), 2L)
  expect_equal(out$start, c(1L, 20L))
  expect_equal(out$end, c(10L, 30L))
  expect_equal(out$strand, c("+", "+"))
})

test_that(".mtr_parse_location carries complement into every join segment", {
  out <- .mtr_parse_location("complement(join(1..10,20..30))")
  expect_equal(nrow(out), 2L)
  expect_equal(out$strand, c("-", "-"))
})

test_that(".mtr_parse_location tolerates a partial-span marker", {
  out <- .mtr_parse_location("<1..100")
  expect_equal(out$start, 1L)
  expect_equal(out$end, 100L)
})

test_that(".mtr_parse_location returns no rows for empty input", {
  expect_equal(nrow(.mtr_parse_location(NA_character_)), 0L)
  expect_equal(nrow(.mtr_parse_location("")), 0L)
})

test_that("maptoref_parse_features returns the canonical mitogenome features", {
  out <- maptoref_parse_features(gb_fixture())
  expect_equal(nrow(out), 38L)
  expect_equal(
    as.integer(table(out$type)[c("CDS", "D-loop", "rRNA", "tRNA")]),
    c(13L, 1L, 2L, 22L)
  )
  expect_equal(names(out), c("type", "gene", "start", "end", "strand"))
})

test_that("maptoref_parse_features drops gene rows duplicating typed features", {
  out <- maptoref_parse_features(gb_fixture())
  expect_false("gene" %in% out$type)
  expect_false(any(duplicated(paste(out$start, out$end))))
})

test_that("maptoref_parse_features reads coordinates and strand correctly", {
  out <- maptoref_parse_features(gb_fixture())
  nd1 <- out[out$type == "CDS" & out$gene == "ND1", ]
  expect_equal(nrow(nd1), 1L)
  expect_equal(nd1$start, 3803L)
  expect_equal(nd1$end, 4777L)
  expect_equal(nd1$strand, "+")
  trnq <- out[out$gene == "trnQ", ]
  expect_equal(trnq$strand, "-")
  expect_equal(trnq$start, 4852L)
})

test_that("maptoref_parse_features sorts by position", {
  out <- maptoref_parse_features(gb_fixture())
  expect_false(is.unsorted(out$start))
  expect_equal(out$type[1], "D-loop")
  expect_equal(out$start[1], 1L)
})

test_that("maptoref_parse_features returns an empty frame for a missing file", {
  out <- maptoref_parse_features(file.path(tempdir(), "does_not_exist.gb"))
  expect_equal(nrow(out), 0L)
  expect_equal(names(out), c("type", "gene", "start", "end", "strand"))
})

test_that("maptoref_parse_features returns an empty frame for a non-GenBank file", {
  d <- withr::local_tempdir()
  fn <- file.path(d, "not_a_record.gb")
  writeLines(c(">seq", "ACGTACGTAC"), fn)
  out <- maptoref_parse_features(fn)
  expect_equal(nrow(out), 0L)
})

test_that("maptoref_parse_features prints nothing", {
  expect_silent(maptoref_parse_features(gb_fixture()))
})
```

- [ ] **Step 3: Run the tests to verify they fail**

```bash
Rscript -e 'devtools::test(filter = "maptoref-features")'
```

Expected: every test errors with `could not find function ".mtr_parse_location"` or `"maptoref_parse_features"`.

- [ ] **Step 4: Write the implementation**

Create `R/maptoref_features.R`:

```r
#' Parse mitogenome annotations from a GenBank record
#'
#' Reads the FEATURES table of a single-record GenBank file into the flat frame
#' the MapToRef viewer's annotation track draws. Returns a zero-row frame with
#' the same columns whenever there is nothing to draw, so callers never branch
#' on NULL.
#'
#' @param gb_path path to a single-record GenBank file
#' @return data.frame with columns type, gene, start, end, strand
#'
#' @noRd
maptoref_parse_features <- function(gb_path) {
  empty <- data.frame(
    type = character(0), gene = character(0),
    start = integer(0), end = integer(0), strand = character(0),
    stringsAsFactors = FALSE
  )
  if (length(gb_path) != 1L || is.na(gb_path) || !file.exists(gb_path)) {
    return(empty)
  }
  rec <- NULL
  # read.gb writes a progress line; capture.output alone does not silence it.
  suppressMessages(invisible(utils::capture.output(
    rec <- try(
      read.gb::read.gb(gb_path, DNA = FALSE, Type = "full", Source = "File"),
      silent = TRUE
    )
  )))
  if (inherits(rec, "try-error") || length(rec) == 0L) {
    return(empty)
  }
  feats <- rec[[1]]$FEATURES
  if (length(feats) == 0L) {
    return(empty)
  }

  keep <- c("CDS", "tRNA", "rRNA", "D_loop", "gene")
  types <- names(feats)
  out <- list()
  for (i in seq_along(feats)) {
    if (types[i] %nin% keep) {
      next
    }
    d <- feats[[i]]
    spans <- .mtr_parse_location(d$Qualifier[1])
    if (nrow(spans) == 0L) {
      next
    }
    out[[length(out) + 1L]] <- data.frame(
      type = sub("_", "-", types[i]),
      gene = .mtr_feature_name(d, types[i]),
      start = spans$start,
      end = spans$end,
      strand = spans$strand,
      stringsAsFactors = FALSE
    )
  }
  if (length(out) == 0L) {
    return(empty)
  }
  res <- do.call(rbind, out)

  # A mitogenome record annotates nearly every gene twice, once as `gene` and
  # once as its type. Keeping both draws every arrow twice.
  typed <- res[res$type != "gene", , drop = FALSE]
  bare <- res[res$type == "gene", , drop = FALSE]
  bare <- bare[paste(bare$start, bare$end) %nin%
                 paste(typed$start, typed$end), , drop = FALSE]
  res <- rbind(typed, bare)
  res <- res[order(res$start, res$end), , drop = FALSE]
  rownames(res) <- NULL
  res
}

#' Expand a GenBank location string into one row per span
#'
#' @param loc a location string such as "3803..4777" or "complement(join(...))"
#' @return data.frame with columns start, end, strand
#'
#' @noRd
.mtr_parse_location <- function(loc) {
  empty <- data.frame(
    start = integer(0), end = integer(0), strand = character(0),
    stringsAsFactors = FALSE
  )
  if (length(loc) != 1L || is.na(loc) || !nzchar(loc)) {
    return(empty)
  }
  strand <- if (grepl("complement", loc, fixed = TRUE)) "-" else "+"
  # Strips every operator name and its parentheses, leaving comma-separated
  # spans; a partial marker (< or >) survives and is dropped by the digit match.
  txt <- gsub("[a-zA-Z_.]+\\(|\\)", "", loc)
  parts <- trimws(strsplit(txt, ",", fixed = TRUE)[[1]])
  parts <- parts[nzchar(parts)]
  spans <- lapply(parts, function(p) {
    n <- as.integer(regmatches(p, gregexpr("[0-9]+", p))[[1]])
    if (length(n) == 0L) {
      return(NULL)
    }
    data.frame(
      start = n[1], end = n[length(n)], strand = strand,
      stringsAsFactors = FALSE
    )
  })
  spans <- spans[!vapply(spans, is.null, logical(1))]
  if (length(spans) == 0L) {
    return(empty)
  }
  do.call(rbind, spans)
}

#' Best available display name for one GenBank feature
#'
#' Row 1 of a feature frame holds the location, not a qualifier, so the lookup
#' starts at row 2; a `gene` feature would otherwise name itself "3803..4777".
#'
#' @param d two-column feature data.frame from read.gb
#' @param type feature type, used as the last-resort name
#' @return single character
#'
#' @noRd
.mtr_feature_name <- function(d, type) {
  pick <- function(key) {
    hit <- which(d$Location == key)
    hit <- hit[hit > 1L]
    if (length(hit) == 0L) {
      return(NA_character_)
    }
    v <- trimws(d$Qualifier[hit[1]])
    if (nzchar(v)) v else NA_character_
  }
  for (key in c("gene", "product", "locus_tag")) {
    v <- pick(key)
    if (!is.na(v)) {
      return(v)
    }
  }
  sub("_", "-", type)
}
```

- [ ] **Step 5: Run the tests to verify they pass**

```bash
Rscript -e 'devtools::test(filter = "maptoref-features")'
```

Expected: all tests pass.

- [ ] **Step 6: Check the whole suite and the file's characters**

```bash
Rscript -e 'devtools::document()'
Rscript -e 'devtools::test()'
grep -nP '[^\x00-\x7F]' R/maptoref_features.R tests/testthat/test-maptoref-features.R
```

Expected: pass count is the baseline plus the new tests, zero failures, no grep output.

---

## Task 2: Keep the reference-frame BAM and write the two CSVs

**Files:**
- Modify: `R/map_to_ref.R`
- Modify: `tests/testthat/test-map-to-ref-loop.R` (add cases; change nothing existing)

**Interfaces:**
- Consumes: `maptoref_parse_features()` from Task 1.
- Produces:
  - On disk, in `<out_dir>/maptoref/`: `final.bam`, `final.bam.bai`, `maptoref_depth.csv` (columns `Position`, `Depth`), and `maptoref_features.csv` (columns `type`, `gene`, `start`, `end`, `strand`) when the reference was GenBank.
  - `.mtr_depth_table(depth_txt, len)` returns a data frame with `Position` (integer, `1..len`, complete and in order) and `Depth` (numeric), folding any position past `len` back onto `position - len` and summing.

- [ ] **Step 1: Write the failing tests**

Append to `tests/testthat/test-map-to-ref-loop.R`:

```r
test_that(".mtr_depth_table fills every reference position", {
  d <- withr::local_tempdir()
  fn <- file.path(d, "depth.txt")
  writeLines(c(
    "mapping_ref\t1\t5",
    "mapping_ref\t2\t7",
    "mapping_ref\t3\t0"
  ), fn)
  out <- .mtr_depth_table(fn, len = 5L)
  expect_equal(out$Position, 1:5)
  expect_equal(out$Depth, c(5, 7, 0, 0, 0))
})

test_that(".mtr_depth_table folds the circular seam back onto the start", {
  d <- withr::local_tempdir()
  fn <- file.path(d, "depth.txt")
  # Reference length 4, with a 2 bp flank reported at positions 5 and 6.
  writeLines(c(
    "mapping_ref\t1\t10",
    "mapping_ref\t2\t10",
    "mapping_ref\t3\t10",
    "mapping_ref\t4\t10",
    "mapping_ref\t5\t3",
    "mapping_ref\t6\t4"
  ), fn)
  out <- .mtr_depth_table(fn, len = 4L)
  expect_equal(nrow(out), 4L)
  expect_equal(out$Depth, c(13, 14, 10, 10))
})

test_that(".mtr_depth_table returns zero depth for an empty file", {
  d <- withr::local_tempdir()
  fn <- file.path(d, "depth.txt")
  file.create(fn)
  out <- .mtr_depth_table(fn, len = 3L)
  expect_equal(out$Position, 1:3)
  expect_equal(out$Depth, c(0, 0, 0))
})
```

- [ ] **Step 2: Run the tests to verify they fail**

```bash
Rscript -e 'devtools::test(filter = "map-to-ref-loop")'
```

Expected: the three new tests error with `could not find function ".mtr_depth_table"`; every pre-existing test in the file still passes.

- [ ] **Step 3: Add the depth-table helper**

Add to `R/map_to_ref.R`, beside the other `.mtr_*` helpers:

```r
#' Per-base depth in reference coordinates
#'
#' Reads `samtools depth -a` output and folds the circular flank back onto the
#' positions it duplicates, the same operation `.coverage_reform_circular()`
#' performs for the published-assembly frame.
#'
#' @param depth_txt path to samtools depth output
#' @param len reference length
#' @return data.frame with columns Position and Depth, one row per position
#'
#' @noRd
.mtr_depth_table <- function(depth_txt, len) {
  out <- data.frame(Position = seq_len(len), Depth = 0)
  if (!file.exists(depth_txt) || file.info(depth_txt)$size == 0) {
    return(out)
  }
  raw <- utils::read.delim(depth_txt, header = FALSE,
                           col.names = c("SeqId", "Position", "Depth"))
  if (nrow(raw) == 0L) {
    return(out)
  }
  pos <- as.integer(raw$Position)
  seam <- pos > len
  pos[seam] <- pos[seam] - len
  keep <- pos >= 1L & pos <= len
  summed <- stats::aggregate(
    list(Depth = as.numeric(raw$Depth)[keep]),
    by = list(Position = pos[keep]),
    FUN = sum
  )
  out$Depth[summed$Position] <- summed$Depth
  out
}
```

- [ ] **Step 4: Run the tests to verify they pass**

```bash
Rscript -e 'devtools::test(filter = "map-to-ref-loop")'
```

Expected: all pass.

- [ ] **Step 5: Index the BAM unconditionally**

In `.mtr_assemble()` in `R/map_to_ref.R`, the current block reads:

```r
  reads_final <- .mtr_count_primary(final_bam)
  # The index exists only to serve the seam query.
  junction_depth <- NA_integer_
  if (circular) {
    .mtr_run(stringr::str_glue("samtools index {shQuote(final_bam)}"), log_fn)
    junction_depth <- .mtr_junction_depth(final_bam, len)
  }
```

Replace it with:

```r
  reads_final <- .mtr_count_primary(final_bam)
  # Indexed for every reference, not just circular ones: the viewer's pileup
  # panel queries windows out of this BAM.
  .mtr_run(stringr::str_glue("samtools index {shQuote(final_bam)}"), log_fn)
  junction_depth <- NA_integer_
  if (circular) {
    junction_depth <- .mtr_junction_depth(final_bam, len)
  }
```

- [ ] **Step 6: Write the depth CSV**

Immediately after the block from Step 5, add:

```r
  # Per-base depth in reference coordinates, so the viewer's coverage track
  # never has to open the BAM.
  depth_txt <- file.path(work, "final_depth.txt")
  .mtr_run(stringr::str_glue(
    "samtools depth -a -J {shQuote(final_bam)} > {shQuote(depth_txt)}"
  ), log_fn)
  utils::write.csv(
    .mtr_depth_table(depth_txt, len),
    file.path(work, "maptoref_depth.csv"),
    row.names = FALSE, quote = FALSE
  )
  unlink(depth_txt)
```

- [ ] **Step 7: Write the features CSV**

In `.mtr_assemble()`, immediately after the `maptoref_prepare_ref()` call and the `work <- file.path(out_dir, "maptoref")` line, add:

```r
  # Annotation track source. Absent when the reference was a FASTA, which is
  # the local BLAST database and bare-FASTA paths.
  ref_gb <- file.path(work, "reference.gb")
  if (file.exists(ref_gb)) {
    feats <- maptoref_parse_features(ref_gb)
    if (nrow(feats) > 0L) {
      utils::write.csv(feats, file.path(work, "maptoref_features.csv"),
                       row.names = FALSE, quote = TRUE)
    }
  }
```

- [ ] **Step 8: Keep the BAM through cleanup**

The current cleanup reads:

```r
  # Reproducible transients, dropped so the published loop record stays the
  # small file set of design 4.11. A failed run keeps everything.
  unlink(list.files(work, pattern = "\\.(bam|bai|bt2|bt2l|fq)$", full.names = TRUE))
```

Replace it with:

```r
  # Reproducible transients, dropped so the published loop record stays small.
  # final.bam and its index are kept: the viewer's pileup panel reads them.
  # A failed run keeps everything.
  transients <- list.files(work, pattern = "\\.(bam|bai|bt2|bt2l|fq)$",
                           full.names = TRUE)
  unlink(transients[basename(transients) %nin%
                      c("final.bam", "final.bam.bai")])
```

- [ ] **Step 9: Verify the retention with a loop test**

Every end-to-end test in `tests/testthat/test-map-to-ref-loop.R` runs against the stub bowtie2 and samtools that `mtr_setup()` prepends to `PATH`. There is no test guarded on real tools, and adding one would be unrunnable: the guard reads `PATH` before `mtr_setup()` overrides it. Add a stub-based sibling instead, reusing the fixture setup of a neighbouring end-to-end test, with no `Sys.which` guard. The stub's `samtools index` case must be extended to `touch "$1.bai"` so the retention assertion is meaningful; that addition is inert for every other test:

```r
test_that("a successful run keeps final.bam, its index, and the depth CSV", {
  d <- withr::local_tempdir()
  # Reuse the same reference and reads setup the existing end-to-end test uses,
  # calling map_to_ref() with out_dir = d.
  # ... setup copied from the neighbouring end-to-end test ...
  work <- file.path(d, "maptoref")
  expect_true(file.exists(file.path(work, "final.bam")))
  expect_true(file.exists(file.path(work, "final.bam.bai")))
  expect_true(file.exists(file.path(work, "maptoref_depth.csv")))
  depth <- utils::read.csv(file.path(work, "maptoref_depth.csv"))
  expect_equal(names(depth), c("Position", "Depth"))
  expect_false(any(is.na(depth$Depth)))
  # No other BAM survives.
  expect_equal(
    sort(basename(list.files(work, pattern = "\\.bam$"))),
    "final.bam"
  )
})
```

Copy the setup lines verbatim from the neighbouring end-to-end test rather than inventing new fixture code.

- [ ] **Step 10: Run the suite**

```bash
Rscript -e 'devtools::test()'
grep -nP '[^\x00-\x7F]' R/map_to_ref.R tests/testthat/test-map-to-ref-loop.R
```

Expected: no failures, no grep output. The new end-to-end test skips unless bowtie2 and samtools are installed.

---

## Task 3: Paths, CSV readers, and depth binning

**Files:**
- Create: `R/maptoref_viz_data.R`
- Create: `tests/testthat/test-maptoref-viz-data.R`

**Interfaces:**
- Consumes: the on-disk layout Task 2 produces.
- Produces:
  - `maptoref_paths(dir_out, ID, assemble_opts)` returns a named list of character paths with exactly these names: `dir`, `work`, `ref_fasta`, `consensus`, `bam`, `bai`, `gb`, `depth`, `features`, `summary`.
  - `maptoref_read_depth(path)` returns a data frame with `Position` (integer) and `Depth` (numeric); zero rows when the file is absent.
  - `maptoref_read_features(path)` returns the Task 1 feature frame shape; zero rows when the file is absent.
  - `maptoref_read_summary(path)` returns a named character vector of the `key=value` lines, with repeated `note` keys collapsed into one element separated by `" | "`; zero-length when absent.
  - `maptoref_bin_depth(depth, n = 2000L)` returns a data frame with `Position`, `Depth`, at most `n` rows, taking the maximum depth per bin. Returns its input unchanged when `nrow(depth) <= n`.
  - `maptoref_read_seq(path)` returns a single uppercase character string, the concatenated sequence of the first FASTA record; `NA_character_` when absent.

- [ ] **Step 1: Write the failing tests**

Create `tests/testthat/test-maptoref-viz-data.R`:

```r
mtr_viz_project <- function(id = "S1", opts = "default", len = 20L,
                            with_features = TRUE) {
  d <- withr::local_tempdir(.local_envir = parent.frame())
  work <- file.path(d, id, "assemble", opts, "maptoref")
  dir.create(work, recursive = TRUE)
  utils::write.csv(
    data.frame(Position = seq_len(len), Depth = seq_len(len) * 2),
    file.path(work, "maptoref_depth.csv"), row.names = FALSE, quote = FALSE
  )
  if (with_features) {
    utils::write.csv(
      data.frame(
        type = c("CDS", "tRNA"), gene = c("ND1", "trnQ"),
        start = c(2L, 12L), end = c(9L, 16L), strand = c("+", "-")
      ),
      file.path(work, "maptoref_features.csv"), row.names = FALSE, quote = TRUE
    )
  }
  writeLines(c(">TESTREF circular", strrep("ACGTA", len %/% 5L)),
             file.path(work, "ref.fasta"))
  writeLines(c(">S1.1.1 subs_only", strrep("ACGTT", len %/% 5L)),
             file.path(work, "subs_only.fasta"))
  writeLines(c(
    "assembler=MapToRef",
    "accession=NC_000001.1",
    "organism=Testus testus",
    "reference_length=20",
    "reads_mapped_final=1234",
    "n_count=2",
    "note=first note",
    "note=second note"
  ), file.path(d, id, "assemble", opts, paste0(id, "_summary.txt")))
  list(dir_out = d, id = id, opts = opts, work = work)
}

test_that("maptoref_paths builds every path from the project convention", {
  p <- maptoref_paths("/out", "S1", "default")
  expect_equal(
    sort(names(p)),
    sort(c("dir", "work", "ref_fasta", "consensus", "bam", "bai", "gb",
           "depth", "features", "summary"))
  )
  expect_equal(p$dir, file.path("/out", "S1", "assemble", "default"))
  expect_equal(p$work, file.path("/out", "S1", "assemble", "default", "maptoref"))
  expect_equal(p$bam, file.path(p$work, "final.bam"))
  expect_equal(p$bai, file.path(p$work, "final.bam.bai"))
  expect_equal(p$gb, file.path(p$work, "reference.gb"))
  expect_equal(p$ref_fasta, file.path(p$work, "ref.fasta"))
  expect_equal(p$consensus, file.path(p$work, "subs_only.fasta"))
  expect_equal(p$depth, file.path(p$work, "maptoref_depth.csv"))
  expect_equal(p$features, file.path(p$work, "maptoref_features.csv"))
  expect_equal(p$summary, file.path(p$dir, "S1_summary.txt"))
})

test_that("maptoref_read_depth reads the CSV", {
  pr <- mtr_viz_project()
  p <- maptoref_paths(pr$dir_out, pr$id, pr$opts)
  out <- maptoref_read_depth(p$depth)
  expect_equal(nrow(out), 20L)
  expect_equal(out$Position, 1:20)
  expect_equal(out$Depth[3], 6)
})

test_that("maptoref_read_depth returns zero rows when absent", {
  out <- maptoref_read_depth(file.path(tempdir(), "nope.csv"))
  expect_equal(nrow(out), 0L)
  expect_equal(names(out), c("Position", "Depth"))
})

test_that("maptoref_read_features reads the CSV", {
  pr <- mtr_viz_project()
  p <- maptoref_paths(pr$dir_out, pr$id, pr$opts)
  out <- maptoref_read_features(p$features)
  expect_equal(nrow(out), 2L)
  expect_equal(out$gene, c("ND1", "trnQ"))
  expect_equal(out$strand, c("+", "-"))
})

test_that("maptoref_read_features returns zero rows when absent", {
  pr <- mtr_viz_project(with_features = FALSE)
  p <- maptoref_paths(pr$dir_out, pr$id, pr$opts)
  out <- maptoref_read_features(p$features)
  expect_equal(nrow(out), 0L)
  expect_equal(names(out), c("type", "gene", "start", "end", "strand"))
})

test_that("maptoref_read_summary parses key=value and collapses notes", {
  pr <- mtr_viz_project()
  p <- maptoref_paths(pr$dir_out, pr$id, pr$opts)
  s <- maptoref_read_summary(p$summary)
  expect_equal(unname(s["organism"]), "Testus testus")
  expect_equal(unname(s["reference_length"]), "20")
  expect_equal(unname(s["note"]), "first note | second note")
})

test_that("maptoref_read_summary returns nothing when absent", {
  expect_length(maptoref_read_summary(file.path(tempdir(), "nope.txt")), 0L)
})

test_that("maptoref_bin_depth leaves a short series alone", {
  d <- data.frame(Position = 1:10, Depth = as.numeric(1:10))
  expect_identical(maptoref_bin_depth(d, n = 2000L), d)
})

test_that("maptoref_bin_depth keeps spikes and respects the point cap", {
  d <- data.frame(Position = 1:1000, Depth = rep(1, 1000))
  d$Depth[c(137, 851)] <- 999
  out <- maptoref_bin_depth(d, n = 100L)
  expect_lte(nrow(out), 100L)
  expect_equal(max(out$Depth), 999)
  expect_equal(sum(out$Depth == 999), 2L)
  expect_false(is.unsorted(out$Position))
})

test_that("maptoref_bin_depth returns zero rows for zero rows", {
  d <- data.frame(Position = integer(0), Depth = numeric(0))
  expect_equal(nrow(maptoref_bin_depth(d, n = 100L)), 0L)
})

test_that("maptoref_read_seq reads and uppercases the first record", {
  pr <- mtr_viz_project()
  p <- maptoref_paths(pr$dir_out, pr$id, pr$opts)
  s <- maptoref_read_seq(p$ref_fasta)
  expect_equal(nchar(s), 20L)
  expect_equal(substr(s, 1, 5), "ACGTA")
  expect_equal(substr(maptoref_read_seq(p$consensus), 1, 5), "ACGTT")
})

test_that("maptoref_read_seq returns NA when absent", {
  expect_true(is.na(maptoref_read_seq(file.path(tempdir(), "nope.fasta"))))
})
```

- [ ] **Step 2: Run the tests to verify they fail**

```bash
Rscript -e 'devtools::test(filter = "maptoref-viz-data")'
```

Expected: errors reporting the functions are not found.

- [ ] **Step 3: Write the implementation**

Create `R/maptoref_viz_data.R`:

```r
#' On-disk paths for one sample's MapToRef outputs
#'
#' The published layout is `<dir_out>/<ID>/assemble/<assemble_opts>/`, with the
#' MapToRef working files under `maptoref/`. Centralised here because the
#' viewer needs eight of these paths and the convention is otherwise rebuilt
#' inline at every call site.
#'
#' @param dir_out project output root, normally `session$userData$dir_out`
#' @param ID sample id
#' @param assemble_opts assembly parameter set name
#' @return named list of character paths
#'
#' @noRd
maptoref_paths <- function(dir_out, ID, assemble_opts) {
  dir <- file.path(dir_out, ID, "assemble", assemble_opts)
  work <- file.path(dir, "maptoref")
  list(
    dir = dir,
    work = work,
    ref_fasta = file.path(work, "ref.fasta"),
    consensus = file.path(work, "subs_only.fasta"),
    bam = file.path(work, "final.bam"),
    bai = file.path(work, "final.bam.bai"),
    gb = file.path(work, "reference.gb"),
    depth = file.path(work, "maptoref_depth.csv"),
    features = file.path(work, "maptoref_features.csv"),
    summary = file.path(dir, paste0(ID, "_summary.txt"))
  )
}

#' Per-base depth table, or an empty frame
#' @noRd
maptoref_read_depth <- function(path) {
  empty <- data.frame(Position = integer(0), Depth = numeric(0))
  if (length(path) != 1L || is.na(path) || !file.exists(path)) {
    return(empty)
  }
  out <- utils::read.csv(path)
  if (!all(c("Position", "Depth") %in% names(out))) {
    return(empty)
  }
  data.frame(
    Position = as.integer(out$Position),
    Depth = as.numeric(out$Depth)
  )
}

#' Annotation features table, or an empty frame
#' @noRd
maptoref_read_features <- function(path) {
  empty <- data.frame(
    type = character(0), gene = character(0),
    start = integer(0), end = integer(0), strand = character(0),
    stringsAsFactors = FALSE
  )
  if (length(path) != 1L || is.na(path) || !file.exists(path)) {
    return(empty)
  }
  out <- utils::read.csv(path, stringsAsFactors = FALSE)
  if (!all(names(empty) %in% names(out))) {
    return(empty)
  }
  data.frame(
    type = as.character(out$type),
    gene = as.character(out$gene),
    start = as.integer(out$start),
    end = as.integer(out$end),
    strand = as.character(out$strand),
    stringsAsFactors = FALSE
  )
}

#' MapToRef summary block as a named character vector
#'
#' Repeated `note` keys are collapsed rather than overwritten, so a run with
#' several warnings shows all of them.
#' @noRd
maptoref_read_summary <- function(path) {
  if (length(path) != 1L || is.na(path) || !file.exists(path)) {
    return(character(0))
  }
  lines <- readLines(path, warn = FALSE)
  lines <- lines[grepl("=", lines, fixed = TRUE)]
  if (length(lines) == 0L) {
    return(character(0))
  }
  key <- sub("=.*$", "", lines)
  val <- sub("^[^=]*=", "", lines)
  vapply(split(val, key), paste, character(1), collapse = " | ")
}

#' Downsample a depth series, keeping the peak of each bin
#'
#' The whole-reference view would otherwise draw one point per base. Taking the
#' maximum rather than the mean keeps spikes and single-base dropouts visible.
#' @noRd
maptoref_bin_depth <- function(depth, n = 2000L) {
  if (nrow(depth) <= n) {
    return(depth)
  }
  bin <- ceiling(seq_len(nrow(depth)) / (nrow(depth) / n))
  data.frame(
    Position = as.integer(tapply(depth$Position, bin, min)),
    Depth = as.numeric(tapply(depth$Depth, bin, max))
  )
}

#' First FASTA record as one uppercase string
#' @noRd
maptoref_read_seq <- function(path) {
  if (length(path) != 1L || is.na(path) || !file.exists(path)) {
    return(NA_character_)
  }
  lines <- readLines(path, warn = FALSE)
  hdr <- grep("^>", lines)
  if (length(hdr) == 0L) {
    return(NA_character_)
  }
  last <- if (length(hdr) > 1L) hdr[2] - 1L else length(lines)
  toupper(paste(lines[(hdr[1] + 1L):last], collapse = ""))
}
```

- [ ] **Step 4: Run the tests to verify they pass**

```bash
Rscript -e 'devtools::test(filter = "maptoref-viz-data")'
```

Expected: all pass.

- [ ] **Step 5: Run the suite and check characters**

```bash
Rscript -e 'devtools::test()'
grep -nP '[^\x00-\x7F]' R/maptoref_viz_data.R tests/testthat/test-maptoref-viz-data.R
```

Expected: no failures, no grep output.

---

## Task 4: Read the BAM window, walk the CIGAR, stack the reads

**Files:**
- Modify: `R/maptoref_viz_data.R`
- Modify: `tests/testthat/test-maptoref-viz-data.R`
- Modify: `DESCRIPTION` (add `Rsamtools` and `IRanges` to `Imports`)

**Interfaces:**
- Consumes: `maptoref_paths()` from Task 3.
- Produces:
  - `.mtr_cigar_walk(pos, cigar, seq, ref)` returns a list with `start` (integer), `end` (integer, last reference position the read covers), `mm` (data frame `pos`, `base`, or NULL), `del` (data frame `start`, `end`, or NULL), `ins` (data frame `pos`, `len`, or NULL).
  - `.mtr_stack_rows(start, end, gap = 1L)` returns an integer vector, one row index per read, first row being 1.
  - `maptoref_window_reads(bam, start, end, ref_seq, seqname = "mapping_ref", max_reads = 100L)` returns a list with `reads` (data frame `read`, `row`, `start`, `end`, `strand`), `mm` (data frame `row`, `pos`, `base`), `del` (data frame `row`, `start`, `end`), `ins` (data frame `row`, `pos`, `len`), `n_shown` (integer), and `n_total` (integer).

- [ ] **Step 1: Add the dependencies**

In `DESCRIPTION`, add `Rsamtools` and `IRanges` to `Imports`. Both are already installed in this environment; confirm with:

```bash
Rscript -e 'cat(requireNamespace("Rsamtools", quietly = TRUE), requireNamespace("IRanges", quietly = TRUE), "\n")'
```

Expected: `TRUE TRUE`.

- [ ] **Step 2: Write the failing tests**

Append to `tests/testthat/test-maptoref-viz-data.R`. The reference is `"ACGTACGTAC"` repeated six times, 60 bp. Every fixture read below was verified against that reference; do not alter a `SEQ` without recomputing what it aligns to.

```r
mtr_viz_ref <- function() paste(rep("ACGTACGTAC", 6), collapse = "")

mtr_viz_bam <- function(envir = parent.frame()) {
  d <- withr::local_tempdir(.local_envir = envir)
  sam <- file.path(d, "t.sam")
  writeLines(c(
    "@HD\tVN:1.6\tSO:coordinate",
    "@SQ\tSN:mapping_ref\tLN:60",
    # perfect match
    "r1\t0\tmapping_ref\t1\t60\t10M\t*\t0\t0\tACGTACGTAC\tIIIIIIIIII",
    # 2 bp insertion after reference position 4
    "r3\t0\tmapping_ref\t1\t60\t4M2I4M\t*\t0\t0\tACGTTTACGT\tIIIIIIIIII",
    # 2 bp soft clip, then a perfect 8 bp match at 11
    "r5\t0\tmapping_ref\t11\t60\t2S8M\t*\t0\t0\tGGACGTACGT\tIIIIIIIIII",
    # reverse strand, single mismatch at reference position 25
    "r2\t16\tmapping_ref\t21\t60\t10M\t*\t0\t0\tACGTTCGTAC\tIIIIIIIIII",
    # 2 bp deletion of reference positions 35 and 36
    "r4\t0\tmapping_ref\t31\t60\t4M2D4M\t*\t0\t0\tACGTGTAC\tIIIIIIII"
  ), sam)
  Rsamtools::asBam(sam, file.path(d, "t"), overwrite = TRUE,
                   indexDestination = TRUE)
}

test_that(".mtr_cigar_walk reports a perfect match as clean", {
  w <- .mtr_cigar_walk(1L, "10M", "ACGTACGTAC", mtr_viz_ref())
  expect_equal(w$start, 1L)
  expect_equal(w$end, 10L)
  expect_null(w$mm)
  expect_null(w$del)
  expect_null(w$ins)
})

test_that(".mtr_cigar_walk finds a single mismatch and its base", {
  w <- .mtr_cigar_walk(21L, "10M", "ACGTTCGTAC", mtr_viz_ref())
  expect_equal(nrow(w$mm), 1L)
  expect_equal(w$mm$pos, 25L)
  expect_equal(w$mm$base, "T")
})

test_that(".mtr_cigar_walk records an insertion without consuming reference", {
  w <- .mtr_cigar_walk(1L, "4M2I4M", "ACGTTTACGT", mtr_viz_ref())
  expect_equal(nrow(w$ins), 1L)
  expect_equal(w$ins$pos, 4L)
  expect_equal(w$ins$len, 2L)
  expect_null(w$mm)
  expect_equal(w$end, 8L)
})

test_that(".mtr_cigar_walk records a deletion and skips the reference", {
  w <- .mtr_cigar_walk(31L, "4M2D4M", "ACGTGTAC", mtr_viz_ref())
  expect_equal(nrow(w$del), 1L)
  expect_equal(w$del$start, 35L)
  expect_equal(w$del$end, 36L)
  expect_null(w$mm)
  expect_equal(w$end, 40L)
})

test_that(".mtr_cigar_walk ignores soft-clipped bases", {
  w <- .mtr_cigar_walk(11L, "2S8M", "GGACGTACGT", mtr_viz_ref())
  expect_null(w$mm)
  expect_equal(w$start, 11L)
  expect_equal(w$end, 18L)
})

test_that(".mtr_stack_rows puts disjoint reads on one row", {
  rows <- .mtr_stack_rows(c(1L, 20L, 40L), c(10L, 30L, 50L))
  expect_equal(rows, c(1L, 1L, 1L))
})

test_that(".mtr_stack_rows pushes an overlapping read to the next row", {
  rows <- .mtr_stack_rows(c(1L, 5L, 9L), c(10L, 14L, 18L))
  expect_equal(rows, c(1L, 2L, 3L))
})

test_that(".mtr_stack_rows reuses a row once the previous read has ended", {
  rows <- .mtr_stack_rows(c(1L, 5L, 20L), c(10L, 14L, 30L))
  expect_equal(rows, c(1L, 2L, 1L))
})

test_that("maptoref_window_reads returns every read overlapping the window", {
  bam <- mtr_viz_bam()
  out <- maptoref_window_reads(bam, 1L, 60L, mtr_viz_ref())
  expect_equal(out$n_total, 5L)
  expect_equal(out$n_shown, 5L)
  expect_equal(nrow(out$reads), 5L)
  expect_true(all(out$reads$row >= 1L))
  expect_setequal(as.character(out$reads$strand), c("+", "-"))
})

test_that("maptoref_window_reads carries mismatches, indels, and strand", {
  bam <- mtr_viz_bam()
  out <- maptoref_window_reads(bam, 1L, 60L, mtr_viz_ref())
  expect_equal(nrow(out$mm), 1L)
  expect_equal(out$mm$pos, 25L)
  expect_equal(out$mm$base, "T")
  expect_equal(nrow(out$del), 1L)
  expect_equal(out$del$start, 35L)
  expect_equal(nrow(out$ins), 1L)
  expect_equal(out$ins$pos, 4L)
  minus <- out$reads[as.character(out$reads$strand) == "-", ]
  expect_equal(minus$start, 21L)
})

test_that("maptoref_window_reads restricts to the requested window", {
  bam <- mtr_viz_bam()
  out <- maptoref_window_reads(bam, 31L, 45L, mtr_viz_ref())
  expect_equal(out$n_total, 1L)
  expect_equal(out$reads$start, 31L)
})

test_that("maptoref_window_reads includes a read ending on the window's first base", {
  bam <- mtr_viz_bam()
  # r2 spans 21-30, so a window starting at 30 overlaps it by one base.
  out <- maptoref_window_reads(bam, 30L, 45L, mtr_viz_ref())
  expect_equal(out$n_total, 2L)
  expect_true(all(c(21L, 31L) %in% out$reads$start))
})

test_that("maptoref_window_reads caps the rows it draws and reports the total", {
  bam <- mtr_viz_bam()
  # order() breaks the (start=1, start=1) tie between r1 and r3 by end
  # ascending, so r3 (end 8) packs before r1 (end 10): row 1 holds r3, r5, r2
  # and row 2 holds r1, r4. A cap of 1 is what exercises the truncation path.
  out <- maptoref_window_reads(bam, 1L, 60L, mtr_viz_ref(), max_reads = 1L)
  expect_equal(out$n_total, 5L)
  expect_equal(out$n_shown, 3L)
  expect_equal(nrow(out$reads), 3L)
  expect_true(all(out$reads$row == 1L))
})

test_that("maptoref_window_reads returns an empty result for an empty window", {
  bam <- mtr_viz_bam()
  out <- maptoref_window_reads(bam, 55L, 60L, mtr_viz_ref())
  expect_equal(out$n_total, 0L)
  expect_equal(nrow(out$reads), 0L)
  expect_equal(nrow(out$mm), 0L)
})

test_that("maptoref_window_reads returns an empty result for a missing BAM", {
  out <- maptoref_window_reads(file.path(tempdir(), "nope.bam"), 1L, 60L,
                               mtr_viz_ref())
  expect_equal(out$n_total, 0L)
  expect_equal(nrow(out$reads), 0L)
})
```

- [ ] **Step 3: Run the tests to verify they fail**

```bash
Rscript -e 'devtools::test(filter = "maptoref-viz-data")'
```

Expected: the Task 3 tests still pass; every new test errors on a missing function.

- [ ] **Step 4: Write the implementation**

Append to `R/maptoref_viz_data.R`:

```r
#' Walk one read's CIGAR against the reference
#'
#' Mismatches are derived here rather than read from MD tags: the tags are
#' relative to the converged reference the reads were mapped to, while the
#' viewer displays the original reference, and the walk is needed for indels
#' regardless.
#'
#' @param pos leftmost reference position of the alignment
#' @param cigar CIGAR string
#' @param seq read sequence, including any soft-clipped bases
#' @param ref reference sequence as one string
#' @return list with start, end, mm, del, ins
#'
#' @noRd
.mtr_cigar_walk <- function(pos, cigar, seq, ref) {
  n <- as.integer(regmatches(cigar, gregexpr("[0-9]+", cigar))[[1]])
  op <- regmatches(cigar, gregexpr("[MIDNSHP=X]", cigar))[[1]]
  refv <- strsplit(ref, "", fixed = TRUE)[[1]]
  qv <- strsplit(toupper(as.character(seq)), "", fixed = TRUE)[[1]]
  rp <- as.integer(pos)
  qp <- 1L
  mm <- list()
  del <- list()
  ins <- list()
  for (k in seq_along(op)) {
    o <- op[k]
    L <- n[k]
    if (o %in% c("M", "=", "X")) {
      idx <- seq_len(L)
      rpos <- rp + idx - 1L
      qb <- qv[qp + idx - 1L]
      hit <- which(!is.na(qb) & rpos <= length(refv) & qb != refv[rpos])
      if (length(hit) > 0L) {
        mm[[length(mm) + 1L]] <- data.frame(
          pos = rpos[hit], base = qb[hit], stringsAsFactors = FALSE
        )
      }
      rp <- rp + L
      qp <- qp + L
    } else if (o == "I") {
      ins[[length(ins) + 1L]] <- data.frame(pos = rp - 1L, len = L)
      qp <- qp + L
    } else if (o %in% c("D", "N")) {
      del[[length(del) + 1L]] <- data.frame(start = rp, end = rp + L - 1L)
      rp <- rp + L
    } else if (o == "S") {
      qp <- qp + L
    }
    # H and P consume neither reference nor the sequence scanBam returns.
  }
  list(
    start = as.integer(pos),
    end = rp - 1L,
    mm = if (length(mm) > 0L) do.call(rbind, mm) else NULL,
    del = if (length(del) > 0L) do.call(rbind, del) else NULL,
    ins = if (length(ins) > 0L) do.call(rbind, ins) else NULL
  )
}

#' Greedy interval packing for a stacked read view
#'
#' @param start integer vector of read start positions
#' @param end integer vector of read end positions
#' @param gap minimum bases between two reads sharing a row
#' @return integer vector of row indices, one per read
#'
#' @noRd
.mtr_stack_rows <- function(start, end, gap = 1L) {
  rows <- integer(length(start))
  last <- numeric(0)
  for (i in seq_along(start)) {
    free <- which(last < start[i] - gap)
    r <- if (length(free) > 0L) free[1] else length(last) + 1L
    rows[i] <- r
    last[r] <- end[i]
  }
  rows
}

#' Reads overlapping one reference window, stacked and annotated
#'
#' @param bam path to an indexed BAM
#' @param start,end reference window, inclusive
#' @param ref_seq reference sequence as one string
#' @param seqname sequence name in the BAM; always "mapping_ref" for MapToRef
#' @param max_reads maximum stacked rows to return
#' @return list with reads, mm, del, ins, n_shown, n_total
#'
#' @noRd
maptoref_window_reads <- function(bam, start, end, ref_seq,
                                  seqname = "mapping_ref", max_reads = 100L) {
  empty <- list(
    reads = data.frame(read = character(0), row = integer(0),
                       start = integer(0), end = integer(0),
                       strand = character(0), stringsAsFactors = FALSE),
    mm = data.frame(row = integer(0), pos = integer(0), base = character(0),
                    stringsAsFactors = FALSE),
    del = data.frame(row = integer(0), start = integer(0), end = integer(0)),
    ins = data.frame(row = integer(0), pos = integer(0), len = integer(0)),
    n_shown = 0L, n_total = 0L
  )
  if (length(bam) != 1L || is.na(bam) || !file.exists(bam)) {
    return(empty)
  }
  if (!file.exists(paste0(bam, ".bai"))) {
    idx <- try(Rsamtools::indexBam(bam), silent = TRUE)
    if (inherits(idx, "try-error")) {
      return(empty)
    }
  }
  param <- Rsamtools::ScanBamParam(
    which = IRanges::IRangesList(
      stats::setNames(list(IRanges::IRanges(start, end)), seqname)
    ),
    what = c("qname", "pos", "cigar", "seq", "strand")
  )
  hit <- try(Rsamtools::scanBam(Rsamtools::BamFile(bam), param = param),
             silent = TRUE)
  if (inherits(hit, "try-error") || length(hit) == 0L) {
    return(empty)
  }
  hit <- hit[[1]]
  n_total <- length(hit$pos)
  if (n_total == 0L) {
    return(empty)
  }

  walks <- lapply(seq_len(n_total), function(i) {
    .mtr_cigar_walk(hit$pos[i], hit$cigar[i], hit$seq[i], ref_seq)
  })
  spans <- data.frame(
    read = as.character(hit$qname),
    start = vapply(walks, function(w) w$start, integer(1)),
    end = vapply(walks, function(w) w$end, integer(1)),
    strand = as.character(hit$strand),
    stringsAsFactors = FALSE
  )
  ord <- order(spans$start, spans$end)
  spans <- spans[ord, , drop = FALSE]
  walks <- walks[ord]
  spans$row <- .mtr_stack_rows(spans$start, spans$end)

  shown <- which(spans$row <= max_reads)
  spans <- spans[shown, , drop = FALSE]
  walks <- walks[shown]
  rownames(spans) <- NULL

  bind <- function(field, cols) {
    parts <- lapply(seq_along(walks), function(i) {
      x <- walks[[i]][[field]]
      if (is.null(x)) {
        return(NULL)
      }
      cbind(row = spans$row[i], x)
    })
    parts <- parts[!vapply(parts, is.null, logical(1))]
    if (length(parts) == 0L) {
      return(empty[[field]])
    }
    out <- do.call(rbind, parts)
    rownames(out) <- NULL
    out[, cols, drop = FALSE]
  }

  list(
    reads = spans[, c("read", "row", "start", "end", "strand")],
    mm = bind("mm", c("row", "pos", "base")),
    del = bind("del", c("row", "start", "end")),
    ins = bind("ins", c("row", "pos", "len")),
    n_shown = nrow(spans),
    n_total = n_total
  )
}
```

- [ ] **Step 5: Run the tests to verify they pass**

```bash
Rscript -e 'devtools::test(filter = "maptoref-viz-data")'
```

Expected: all pass. If a mismatch test fails with unexpected extra mismatches, re-read D6: the fixture `SEQ` and the reference must agree, and a `D` operation changes the required `SEQ` length.

- [ ] **Step 6: Run the suite and check characters**

```bash
Rscript -e 'devtools::document()'
Rscript -e 'devtools::test()'
grep -nP '[^\x00-\x7F]' R/maptoref_viz_data.R tests/testthat/test-maptoref-viz-data.R DESCRIPTION
```

Expected: no failures, no grep output.

---

## Task 5: The viewer module, coverage track, zoom, and hover

**Files:**
- Create: `R/app_assemble_maptoref_viewer.R`
- Modify: `R/app_assemble.R`

**Interfaces:**
- Consumes: everything from Tasks 3 and 4.
- Produces:
  - `maptoref_viewer_server(id, rv)`, a `moduleServer` that listens for the gargoyle event `"maptoref_modal"` and reads `rv$updating` for the clicked sample row.
  - A new column `maptoref` in the Assemble table data and a matching `colDef`, whose button sets `input$maptoref` to the clicked row index.
  - Module-internal reactive values `win_center` and `win_size`.

- [ ] **Step 1: Add the module file with the modal and the coverage track**

Create `R/app_assemble_maptoref_viewer.R`:

```r
#' Narrowest and widest zoom windows, in bases.
#' @noRd
MTR_VIEW_MIN_BP <- 100L

#' Points drawn in the coverage track before binning kicks in.
#' @noRd
MTR_VIEW_POINTS <- 2000L

#' MapToRef coverage and pileup viewer
#'
#' @noRd
maptoref_viewer_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    init("maptoref_modal")

    state <- reactiveValues(
      paths = NULL, depth = NULL, features = NULL, summary = NULL,
      ref_seq = NA_character_, cons_seq = NA_character_, len = 0L,
      win_center = 0, win_size = 0, pileup_center = NA_real_
    )

    win_range <- reactive({
      req(state$len > 0L)
      half <- state$win_size / 2
      lo <- max(1, round(state$win_center - half))
      hi <- min(state$len, round(state$win_center + half))
      c(lo, hi)
    })

    set_window <- function(center, size) {
      size <- max(MTR_VIEW_MIN_BP, min(state$len, round(size)))
      half <- size / 2
      center <- max(half, min(state$len - half, center))
      state$win_size <- size
      state$win_center <- center
      updateNumericInput(session, "win_size", value = size)
    }

    on("maptoref_modal", {
      p <- maptoref_paths(
        session$userData$dir_out, rv$updating$ID, rv$updating$assemble_opts
      )
      state$paths <- p
      state$depth <- maptoref_read_depth(p$depth)
      state$features <- maptoref_read_features(p$features)
      state$summary <- maptoref_read_summary(p$summary)
      state$ref_seq <- maptoref_read_seq(p$ref_fasta)
      state$cons_seq <- maptoref_read_seq(p$consensus)
      state$len <- nrow(state$depth)
      state$pileup_center <- NA_real_

      if (state$len == 0L) {
        shinyWidgets::sendSweetAlert(
          title = "No MapToRef coverage data",
          text = tags$div(
            tags$p(
              "No coverage table was found for ", tags$b(rv$updating$ID),
              " at:"
            ),
            tags$ul(tags$li(tags$code(p$depth))),
            tags$p(
              "Re-run Assembly for this sample to produce the MapToRef ",
              "coverage and read files."
            )
          ),
          html = TRUE, type = "error"
        )
        return()
      }

      state$win_size <- state$len
      state$win_center <- state$len / 2

      showModal(modalDialog(
        title = NULL, size = "xl", easyClose = TRUE, footer = NULL,
        tags$div(
          class = "maptoref-viewer",
          uiOutput(ns("header")),
          tags$div(
            style = "display:flex; gap:8px; align-items:flex-end; margin:6px 0;",
            actionButton(ns("zoom_out"), "-", class = "btn-sm"),
            actionButton(ns("zoom_in"), "+", class = "btn-sm"),
            actionButton(ns("zoom_reset"), "Full view", class = "btn-sm"),
            numericInput(ns("win_size"), "Window (bp)",
                         value = state$len, min = MTR_VIEW_MIN_BP,
                         max = state$len, step = 100, width = "140px"),
            numericInput(ns("pileup_size"), "Pileup window (bp)",
                         value = 200, min = 50, max = 1000, step = 50,
                         width = "160px")
          ),
          tags$div(
            style = "position:relative;",
            plotOutput(
              ns("tracks"), height = "420px",
              hover = hoverOpts(ns("tracks_hover"), delay = 100,
                                delayType = "throttle"),
              click = ns("tracks_click"),
              brush = brushOpts(ns("tracks_brush"), direction = "x",
                                resetOnNew = TRUE)
            ),
            uiOutput(ns("tooltip"))
          ),
          uiOutput(ns("pileup_ui"))
        )
      ))
    })

    output$header <- renderUI({
      s <- state$summary
      fld <- function(k) unname(s[k]) %||% NA_character_
      n_pct <- suppressWarnings(
        round(100 * as.numeric(fld("n_count")) /
                as.numeric(fld("reference_length")), 1)
      )
      item <- function(label, value) {
        tags$span(
          style = "margin-right:18px;",
          tags$b(label), " ", value
        )
      }
      tags$div(
        style = "font-size:90%; padding-bottom:4px;",
        tags$h4(rv$updating$ID, style = "margin:0 0 4px 0;"),
        item("Reference:", fld("accession")),
        item("Organism:", fld("organism")),
        item("Length:", paste0(fld("reference_length"), " bp")),
        item("Source:", fld("reference_source")),
        item("Reads mapped:", fld("reads_mapped_final")),
        item("Mean depth:", round(mean(state$depth$Depth), 1)),
        item("Uncalled:", paste0(n_pct, "%"))
      )
    })

    observeEvent(input$zoom_in, {
      set_window(state$win_center, state$win_size / 2)
    })
    observeEvent(input$zoom_out, {
      set_window(state$win_center, state$win_size * 2)
    })
    observeEvent(input$zoom_reset, {
      set_window(state$len / 2, state$len)
    })
    observeEvent(input$win_size, ignoreInit = TRUE, {
      req(input$win_size, state$len > 0L)
      if (!isTRUE(all.equal(input$win_size, state$win_size))) {
        set_window(state$win_center, input$win_size)
      }
    })
    observeEvent(input$tracks_brush, {
      b <- input$tracks_brush
      req(b)
      set_window((b$xmin + b$xmax) / 2, b$xmax - b$xmin)
    })

    output$tracks <- renderPlot({
      req(state$len > 0L)
      rng <- win_range()
      .mtr_view_tracks(state$depth, state$features, rng)
    })

    output$tooltip <- renderUI({
      h <- input$tracks_hover
      req(h, state$len > 0L)
      pos <- round(h$x)
      req(pos >= 1L, pos <= state$len)
      d <- state$depth$Depth[pos]
      f <- state$features
      gene <- f$gene[f$start <= pos & f$end >= pos]
      tags$div(
        style = paste0(
          "position:absolute; z-index:100; pointer-events:none; ",
          "background:rgba(255,255,255,0.92); border:1px solid #999; ",
          "border-radius:3px; padding:3px 6px; font-size:85%; ",
          "left:", h$coords_css$x + 12, "px; top:", h$coords_css$y + 12, "px;"
        ),
        tags$div(tags$b("Position: "), format(pos, big.mark = ",")),
        tags$div(tags$b("Depth: "), d),
        if (length(gene) > 0L) tags$div(tags$b("Gene: "), gene[1])
      )
    })

    observeEvent(input$tracks_click, {
      req(input$tracks_click, state$len > 0L)
      state$pileup_center <- input$tracks_click$x
    })
  })
}
```

- [ ] **Step 2: Add the track-drawing helper**

Append to `R/app_assemble_maptoref_viewer.R`. The annotation panel is added in Task 6; for now it draws the coverage panel alone so the module can be exercised end to end.

```r
#' Coverage panel for one window
#'
#' @param depth data.frame with Position and Depth
#' @param features annotation frame; unused until Task 6
#' @param rng length-2 numeric, the visible window
#' @return a ggplot
#'
#' @noRd
.mtr_view_tracks <- function(depth, features, rng) {
  d <- depth[depth$Position >= rng[1] & depth$Position <= rng[2], , drop = FALSE]
  d <- maptoref_bin_depth(d, MTR_VIEW_POINTS)
  zero <- d[d$Depth == 0, , drop = FALSE]
  ggplot2::ggplot(d, ggplot2::aes(x = .data$Position, y = .data$Depth)) +
    {
      if (nrow(zero) > 0L) {
        ggplot2::geom_vline(
          data = zero, ggplot2::aes(xintercept = .data$Position),
          color = "#FF6670", linewidth = 0.4
        )
      }
    } +
    ggplot2::geom_area(fill = "#4c72b0", color = NA) +
    ggplot2::scale_x_continuous(
      limits = rng, expand = ggplot2::expansion(0),
      labels = scales::label_comma()
    ) +
    ggplot2::labs(x = "Reference position (bp)", y = "Depth") +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
}
```

- [ ] **Step 3: Wire the entry point in the Assemble table**

Three edits in `R/app_assemble.R`.

First, beside the existing `view` column definition (anchored on `cell = rt_icon_bttn_text(ns("details"), "fas fa-square-arrow-up-right fa-xs")`), add a sibling `colDef`:

```r
            maptoref = colDef(
              show = TRUE,
              sticky = "right",
              filterable = FALSE,
              name = "",
              html = TRUE,
              width = 110,
              align = "center",
              cell = rt_icon_bttn_text(ns("maptoref_view"), "fas fa-chart-area fa-xs")
            ),
```

Second, in the `on("update_assemble_table", ...)` block, beside the existing `view = dplyr::case_when(...)`, add the column value. The button is offered only for a MapToRef parameter set, so join the assembler through `rv$assemble_opts`:

```r
            maptoref = dplyr::case_when(
              assemble_switch > 1 &
                assemble_opts %in% rv$assemble_opts$assemble_opts[
                  rv$assemble_opts$assembler == "MapToRef"
                ] ~ "Coverage",
              .default = NA_character_
            ),
```

Apply the same `maptoref = dplyr::case_when(...)` mutation wherever `view = dplyr::case_when(...)` is applied to the table data, so the column exists on first render as well as on update.

Third, beside the existing details handler (anchored on `assembly_coverage_details_server(ns("coverage_details"), rv)`), add:

```r
    # Open MapToRef Coverage Viewer ----
    # Not ns("maptoref"): assemble_opts_modal() already owns that id for the
    # MapToRef bowtie2 options field, and typing there would fire this observer.
    observeEvent(input$maptoref_view, ignoreInit = T, {
      rv$updating <- rv$data |> dplyr::slice(as.numeric(input$maptoref_view))
      trigger("maptoref_modal")
    })
    maptoref_viewer_server(ns("maptoref_viewer"), rv)
```

- [ ] **Step 4: Check that the package loads and the suite still passes**

```bash
Rscript -e 'devtools::document()'
Rscript -e 'devtools::load_all(); cat("loaded\n")'
Rscript -e 'devtools::test()'
grep -nP '[^\x00-\x7F]' R/app_assemble_maptoref_viewer.R R/app_assemble.R
```

Expected: loads cleanly, no test failures, no grep output.

- [ ] **Step 5: Exercise it in the app**

Open the test project's Shiny app, go to the Assemble tab, and confirm: the new button appears only on rows whose parameter set uses MapToRef; clicking it opens the modal; the header shows the reference and organism; the coverage track draws the whole reference; `+`, `-`, Full view, the window box, and drag-to-zoom all change the visible range; hovering shows a tooltip with position and depth.

If no MapToRef sample is available, run one from the Scyphozoa or fish test project first.

---

## Task 6: Annotation track

**Files:**
- Modify: `R/app_assemble_maptoref_viewer.R`

**Interfaces:**
- Consumes: `maptoref_read_features()` from Task 3 and `.mtr_view_tracks()` from Task 5.
- Produces: `.mtr_view_features(features, rng)` returning a ggplot, and a two-panel `.mtr_view_tracks()` assembled with `patchwork`.

- [ ] **Step 1: Add the annotation panel builder**

Append to `R/app_assemble_maptoref_viewer.R`:

```r
#' Gene arrow track for one window
#'
#' Labels are drawn only when the window is narrow enough for them to be
#' legible; across a whole mitogenome 38 labels collide into a smear.
#'
#' @param features annotation frame from maptoref_read_features()
#' @param rng length-2 numeric, the visible window
#' @return a ggplot
#'
#' @noRd
.mtr_view_features <- function(features, rng) {
  f <- features[features$end >= rng[1] & features$start <= rng[2], , drop = FALSE]
  base <- ggplot2::ggplot() +
    ggplot2::scale_x_continuous(limits = rng, expand = ggplot2::expansion(0)) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none")
  if (nrow(f) == 0L) {
    return(base)
  }
  f$xmin <- ifelse(f$strand == "-", f$end, f$start)
  f$xmax <- ifelse(f$strand == "-", f$start, f$end)
  f$y <- "genes"
  p <- base +
    gggenes::geom_gene_arrow(
      data = f,
      ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax, y = .data$y,
                   fill = .data$type),
      arrowhead_height = grid::unit(4, "mm"),
      arrowhead_width = grid::unit(2, "mm"),
      arrow_body_height = grid::unit(3, "mm")
    ) +
    ggplot2::scale_fill_manual(
      values = c("CDS" = "#8fb3d9", "tRNA" = "#c7e0a8",
                 "rRNA" = "#f2c297", "D-loop" = "#d9c7e8",
                 "gene" = "#cccccc"),
      na.value = "#cccccc"
    )
  if (diff(rng) <= 6000) {
    p <- p + ggplot2::geom_text(
      data = f,
      ggplot2::aes(x = (.data$start + .data$end) / 2, y = .data$y,
                   label = .data$gene),
      size = 2.6, vjust = -1.6
    )
  }
  p
}
```

- [ ] **Step 2: Stack the two panels**

Replace the body of `.mtr_view_tracks()` so it returns the annotation panel above the coverage panel. Keep the coverage code exactly as it is and wrap it:

```r
.mtr_view_tracks <- function(depth, features, rng) {
  d <- depth[depth$Position >= rng[1] & depth$Position <= rng[2], , drop = FALSE]
  d <- maptoref_bin_depth(d, MTR_VIEW_POINTS)
  zero <- d[d$Depth == 0, , drop = FALSE]
  cov <- ggplot2::ggplot(d, ggplot2::aes(x = .data$Position, y = .data$Depth)) +
    {
      if (nrow(zero) > 0L) {
        ggplot2::geom_vline(
          data = zero, ggplot2::aes(xintercept = .data$Position),
          color = "#FF6670", linewidth = 0.4
        )
      }
    } +
    ggplot2::geom_area(fill = "#4c72b0", color = NA) +
    ggplot2::scale_x_continuous(
      limits = rng, expand = ggplot2::expansion(0),
      labels = scales::label_comma()
    ) +
    ggplot2::labs(x = "Reference position (bp)", y = "Depth") +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())

  if (nrow(features) == 0L) {
    return(cov)
  }
  patchwork::wrap_plots(
    .mtr_view_features(features, rng), cov,
    ncol = 1, heights = c(1, 5)
  )
}
```

The hover tooltip already degrades correctly when `features` has no rows, because the gene lookup returns a zero-length vector.

- [ ] **Step 3: Note the missing-annotation case in the header**

In `output$header`, append one more element to the `tags$div` so the absence is explicit rather than silent:

```r
        if (nrow(state$features) == 0L) {
          tags$span(
            style = "color:#888;",
            "Reference has no annotation record."
          )
        }
```

- [ ] **Step 4: Verify**

```bash
Rscript -e 'devtools::load_all(); cat("loaded\n")'
Rscript -e 'devtools::test()'
grep -nP '[^\x00-\x7F]' R/app_assemble_maptoref_viewer.R
```

Then in the app: open the viewer for a sample whose reference came from an NCBI accession and confirm gene arrows appear above the coverage, colored by type, pointing left for minus-strand genes, with labels appearing as you zoom in past roughly 6 kb and disappearing as you zoom out. Open one whose reference was a bare FASTA and confirm the annotation panel is absent and the header says so.

---

## Task 7: Pileup panel

**Files:**
- Modify: `R/app_assemble_maptoref_viewer.R`

**Interfaces:**
- Consumes: `maptoref_window_reads()` from Task 4, `state$pileup_center` from Task 5.
- Produces: `.mtr_view_pileup(win, ref_seq, cons_seq, rng)` returning a ggplot, and the `pileup_ui` / `pileup` outputs.

- [ ] **Step 1: Add the pileup drawing helper**

Append to `R/app_assemble_maptoref_viewer.R`:

```r
#' Base colours shared by the sequence rows and the mismatch letters.
#' @noRd
MTR_BASE_COLORS <- c(A = "#3aa03a", C = "#2f6fb5", G = "#e0a030",
                     T = "#cc4b4b", N = "#999999")

#' Read pileup for one window
#'
#' @param win result of maptoref_window_reads()
#' @param ref_seq,cons_seq full-length reference and consensus strings
#' @param rng length-2 numeric, the pileup window
#' @return a ggplot
#'
#' @noRd
.mtr_view_pileup <- function(win, ref_seq, cons_seq, rng) {
  pos <- seq(max(1, rng[1]), rng[2])
  letters_on <- diff(rng) <= 300
  seq_row <- function(s, label, y) {
    # A window with no overlap at all leaves substr() empty, and a zero-row
    # base vector against a one-row label would not recycle.
    if (is.na(s) || pos[1] > nchar(s)) {
      return(NULL)
    }
    b <- strsplit(substr(s, pos[1], pos[length(pos)]), "", fixed = TRUE)[[1]]
    data.frame(pos = pos[seq_along(b)], base = b, track = label, y = y,
               stringsAsFactors = FALSE)
  }
  bases <- rbind(
    seq_row(ref_seq, "Reference", 0),
    seq_row(cons_seq, "Consensus", -1)
  )

  p <- ggplot2::ggplot() +
    ggplot2::scale_x_continuous(
      limits = c(rng[1] - 0.5, rng[2] + 0.5),
      expand = ggplot2::expansion(0), labels = scales::label_comma()
    ) +
    ggplot2::scale_fill_manual(values = MTR_BASE_COLORS, na.value = "#bbbbbb") +
    ggplot2::scale_color_manual(values = MTR_BASE_COLORS, na.value = "#bbbbbb") +
    ggplot2::labs(x = "Reference position (bp)", y = NULL) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "none",
      panel.grid = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank()
    )

  if (!is.null(bases)) {
    p <- p +
      ggplot2::geom_tile(
        data = bases,
        ggplot2::aes(x = .data$pos, y = .data$y, fill = .data$base),
        height = 0.85, alpha = 0.75
      )
    if (letters_on) {
      p <- p + ggplot2::geom_text(
        data = bases,
        ggplot2::aes(x = .data$pos, y = .data$y, label = .data$base),
        size = 2.4, color = "white"
      )
    }
    p <- p + ggplot2::annotate(
      "text", x = rng[1], y = c(0, -1), label = c("Reference", "Consensus"),
      hjust = 0, vjust = -1.2, size = 2.6, color = "#666666"
    )
  }

  if (nrow(win$reads) > 0L) {
    r <- win$reads
    r$y <- -1 - r$row
    p <- p +
      ggplot2::geom_rect(
        data = r,
        ggplot2::aes(xmin = .data$start - 0.5, xmax = .data$end + 0.5,
                     ymin = .data$y - 0.35, ymax = .data$y + 0.35,
                     group = .data$row),
        fill = ifelse(r$strand == "-", "#d5d8e0", "#c3ccd9"), color = NA
      )
    if (nrow(win$del) > 0L) {
      dl <- win$del
      dl$y <- -1 - dl$row
      p <- p + ggplot2::geom_segment(
        data = dl,
        ggplot2::aes(x = .data$start - 0.5, xend = .data$end + 0.5,
                     y = .data$y, yend = .data$y),
        color = "#555555", linewidth = 0.3
      )
    }
    if (nrow(win$mm) > 0L) {
      mm <- win$mm
      mm$y <- -1 - mm$row
      if (letters_on) {
        p <- p + ggplot2::geom_text(
          data = mm,
          ggplot2::aes(x = .data$pos, y = .data$y, label = .data$base,
                       color = .data$base),
          size = 2.4, fontface = "bold"
        )
      } else {
        p <- p + ggplot2::geom_tile(
          data = mm,
          ggplot2::aes(x = .data$pos, y = .data$y, fill = .data$base),
          height = 0.7
        )
      }
    }
    if (nrow(win$ins) > 0L) {
      ins <- win$ins
      ins$y <- -1 - ins$row
      p <- p + ggplot2::geom_segment(
        data = ins,
        ggplot2::aes(x = .data$pos + 0.5, xend = .data$pos + 0.5,
                     y = .data$y - 0.4, yend = .data$y + 0.4),
        color = "#7b3fa0", linewidth = 0.7
      )
    }
  }
  p
}
```

- [ ] **Step 2: Add the panel's UI and render**

Append inside `maptoref_viewer_server`, after the `tracks_click` observer:

```r
    pileup_range <- reactive({
      req(!is.na(state$pileup_center), state$len > 0L)
      size <- max(50, min(1000, input$pileup_size %||% 200))
      half <- size / 2
      center <- max(half, min(state$len - half, state$pileup_center))
      c(max(1, round(center - half)), min(state$len, round(center + half)))
    })

    pileup_data <- reactive({
      rng <- pileup_range()
      maptoref_window_reads(
        state$paths$bam, rng[1], rng[2], state$ref_seq
      )
    })

    output$pileup_ui <- renderUI({
      if (is.na(state$pileup_center)) {
        return(tags$p(
          style = "color:#888; margin-top:8px;",
          "Click the coverage plot to see the reads at that position."
        ))
      }
      if (!file.exists(state$paths$bam %||% "")) {
        return(tags$p(
          style = "color:#888; margin-top:8px;",
          "No read alignments were kept for this sample. Re-run Assembly to ",
          "enable the read view."
        ))
      }
      tagList(
        uiOutput(ns("pileup_note")),
        plotOutput(ns("pileup"), height = "320px")
      )
    })

    output$pileup_note <- renderUI({
      w <- pileup_data()
      rng <- pileup_range()
      txt <- if (w$n_total == 0L) {
        "No reads in this window."
      } else if (w$n_shown < w$n_total) {
        paste0("Showing ", format(w$n_shown, big.mark = ","), " of ",
               format(w$n_total, big.mark = ","), " reads.")
      } else {
        paste0(format(w$n_total, big.mark = ","), " reads.")
      }
      tags$div(
        style = "font-size:85%; color:#666; margin-top:6px;",
        paste0(format(rng[1], big.mark = ","), " - ",
               format(rng[2], big.mark = ","), " bp. "), txt
      )
    })

    output$pileup <- renderPlot({
      rng <- pileup_range()
      .mtr_view_pileup(pileup_data(), state$ref_seq, state$cons_seq, rng)
    }) |>
      bindCache(state$paths$bam, pileup_range(), input$pileup_size)
```

- [ ] **Step 3: Verify**

```bash
Rscript -e 'devtools::load_all(); cat("loaded\n")'
Rscript -e 'devtools::test()'
grep -nP '[^\x00-\x7F]' R/app_assemble_maptoref_viewer.R
```

Then in the app, on a MapToRef sample with reads: click the coverage plot and confirm the pileup opens centered there; reference and consensus rows appear above the reads with base letters at 200 bp; changing the pileup window box to 1000 switches the letters off and keeps the tiles; mismatches show as colored letters; a read with a deletion shows a connecting line; strand shows as two fill shades; the note reports the read count and truncation.

Also click a position in a zero-coverage region and confirm the note reads "No reads in this window" rather than erroring.

---

## Task 8: Degradation, documentation, and final check

**Files:**
- Modify: `R/app_assemble_maptoref_viewer.R`
- Modify: `NEWS.md`
- Modify: `_pkgdown.yml` only if a new exported function was added (none is expected)

**Interfaces:**
- Consumes: everything above.
- Produces: no new interfaces.

- [ ] **Step 1: Confirm each failure mode by hand**

Work through the spec's section 9 table against a real project. For each row, produce the condition and record what the viewer does:

- Rename `maptoref/final.bam` aside. Expected: coverage and annotation tracks still draw; clicking the coverage plot shows the "re-run Assembly" message; no error in the R console.
- Rename `maptoref/maptoref_depth.csv` aside. Expected: the sweet-alert names the missing path and the modal does not open.
- Rename `maptoref/maptoref_features.csv` aside. Expected: coverage draws full height, header says the reference has no annotation record.
- Delete `maptoref/final.bam.bai`. Expected: it is rebuilt on first pileup click, and the file reappears.
- Click a zero-coverage position. Expected: "No reads in this window."

Fix anything that errors instead of degrading. Restore every renamed file afterwards.

- [ ] **Step 2: Add the NEWS entry**

Add one bullet under the current development heading in `NEWS.md`, matching the surrounding style:

```markdown
* Added a MapToRef coverage viewer to the Assemble tab. A new button on MapToRef
  samples opens an interactive plot of read depth across the reference, with a
  gene annotation track when the reference is a GenBank record, and a
  click-through read pileup showing reference bases, consensus bases, and
  individual reads with mismatches, insertions, deletions, and strand.
  MapToRef now keeps its final read alignment so the pileup can be drawn.
```

- [ ] **Step 3: Full verification**

```bash
Rscript -e 'devtools::document()'
Rscript -e 'devtools::test()'
Rscript -e 'devtools::check(args = c("--no-manual", "--no-build-vignettes"), error_on = "warning")'
grep -rnP '[^\x00-\x7F]' R/maptoref_features.R R/maptoref_viz_data.R \
  R/app_assemble_maptoref_viewer.R tests/testthat/test-maptoref-features.R \
  tests/testthat/test-maptoref-viz-data.R
git status --short
```

Expected: no test failures; `check` reports no ERROR or WARNING (a NOTE about package size or new imports is acceptable); no non-ASCII output; `git status` shows only the files this plan names, and nothing is committed.

- [ ] **Step 4: Report**

Summarise for the maintainer: the baseline and final test counts, the three new dependencies and why each is there, the per-sample disk cost of keeping `final.bam` measured on a real sample, and anything from Step 1 that degraded differently than the spec predicted.

---

## Self-review notes

Checked against `tools/maptoref_coverage_viewer_spec.md`:

- Spec 3 and 3.1, the coordinate frame and seam folding: D1, plus `.mtr_depth_table()` in Task 2 and its two seam tests.
- Spec 4, all four pipeline changes: Task 2 steps 5 through 8.
- Spec 5.1, the parser including the D5 de-duplication rule: Task 1.
- Spec 5.2, every data-layer function: Tasks 3 and 4.
- Spec 5.3, the module: Tasks 5 through 7.
- Spec 6.1, entry point gated on the MapToRef assembler: Task 5 step 3.
- Spec 6.2, header, picker, three panels: Task 5 step 1 and Task 6. **Known reduction:** the reference picker is not built. A sample carries one `maptoref_ref` and one published `maptoref/` directory, so the picker would always hold a single entry; the header names the reference instead. Adding a picker becomes worthwhile only if a sample can ever carry more than one MapToRef reference, and it would slot into the header without touching the data layer. Raise this with the maintainer before starting Task 5 if a picker is wanted anyway.
- Spec 6.3, zoom and hover: Task 5 step 1.
- Spec 7, the pileup: Task 4 (data) and Task 7 (drawing).
- Spec 8, dependencies: Task 1 step 1 and Task 4 step 1.
- Spec 9, failure modes: handled in Tasks 5 and 7, verified in Task 8 step 1.
- Spec 10, testing: Tasks 1, 3, and 4 carry it, with `asBam` fixtures per D6.
- Spec 11, backwards compatibility: no task touches `R/init_db.R` or
  `R/backwards_compatibility.R`, which is the point.

Type consistency: `maptoref_paths()` names are fixed in Task 3 and used unchanged in Tasks 5 and 7. `maptoref_window_reads()` returns `reads`, `mm`, `del`, `ins`, `n_shown`, `n_total` in Task 4 and every field is consumed under those names in Task 7. `.mtr_view_tracks(depth, features, rng)` keeps its signature when Task 6 rewrites its body.
