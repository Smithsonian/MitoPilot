# MapToRef Assembly Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a third assembler, "MapToRef", that maps a sample's reads to a user-supplied reference mitogenome, iterates the consensus back into the mapping reference until it stops changing, and publishes a reads-only consensus through the existing assemble/coverage plumbing.

**Architecture:** One new exported R function, `map_to_ref()`, drives bowtie2 and samtools consensus inside the existing `assemble` Nextflow process, in a third `elif` branch, exactly the way `coverage.nf` calls `MitoPilot::coverage()`. The reference is a single-record GenBank or FASTA file staged as a `path()` input. No new Nextflow process, no new tables, no config change. Five passthrough option columns on `assemble_opts` carry the reference, the bowtie2 flags, the samtools consensus flags, the iteration cap, and the FASTA topology.

**Tech Stack:** R (testthat, withr, stringr), bowtie2 2.5.4, samtools 1.24 (bumped from 1.21 by this work), Nextflow DSL2 with the nf-sqldb plugin, SQLite, Shiny.

**Spec:** `tools/map_to_ref_design.md` (decisions of record are section 8.1; the body has been brought in line with them)

## Global Constraints

- ASCII only in R code and in every file this plan touches. No non-ASCII characters, no em dashes. Check with `grep -nP '[^\x00-\x7F]' <file>`.
- Minimal comments. Comment the why, never narrate a bugfix.
- Commit only as the plan's steps say, and only on the `map-to-ref-assembly` branch. Commits on this branch are authorized as checkpoints (ruled 2026-09-03). Never push.
- No Claude attribution in commit messages or anywhere else.
- Branch: work stays on `map-to-ref-assembly`. Do not create a branch.
- Existing tests and public function signatures keep working. New arguments get defaults that preserve current behaviour.
- Nextflow tuple positions: every new element is APPENDED at the end of its tuple, so no existing index moves.
- Option strings are interpolated into an `Rscript -e` shell call, so they must not contain quote characters. The other two assemblers interpolate their options into a plain shell command line, so this rule is specific to MapToRef; `map_to_ref()` refuses option strings containing `'` or `"` (Tasks 2, 3, and 8).
- `assembler` values are exactly `GetOrganelle`, `MitoFinder`, `MapToRef`.
- The published FASTA contract is unchanged: `<ID>_assembly_1.fasta` with header `>ID.1.1 circular` or `>ID.1.1 linear`, plus `<ID>_reads.tar.gz`, `<ID>_summary.txt`, `assembler.log.txt`, `NF_work_dir_assemble.txt`. Failure is `<ID>_assembly_0.fasta` containing `>No assembly found`, and exit status 0.
- Every intermediate file MapToRef writes goes under `maptoref/` inside the task directory, never at the task root, because `assemble.nf` tars `*.fastq.gz` from the task root.
- Run the R test suite with `Rscript -e 'devtools::test()'` from the repo root. Baseline before this work: FAIL 0 | WARN 0 | SKIP 23 | PASS 1807 (measured 2026-09-03).

---

## File Structure

**Created:**

- `R/map_to_ref.R` - the whole feature: reference reader, the iterate-to-consensus loop, the pure helpers it is built from, and the output writer. One file because these pieces are only ever used together and are easier to reason about side by side.
- `tests/testthat/test-map-to-ref.R` - unit tests for the pure helpers and the reference reader.
- `tests/testthat/test-map-to-ref-loop.R` - the stub-binary test that exercises `map_to_ref()` end to end without a real mapper.
- `inst/test_data/NC_002333_Danio_rerio.gb` - the packaged single-record circular reference fixture (copied from `ref_dbs/MitoFinder/`, which is not installed with the package).

**Modified:**

- `R/init_db.R` - five new `assemble_opts` columns, five new `new_db()` arguments, the assembler validator.
- `R/backwards_compatibility.R` - five migration blocks, the "already current" predicate, the roxygen migration list.
- `inst/nextflow/modules/assemble_workflow.nf` - five more selected columns, the opts map, the reference `path()` element, the cross map.
- `inst/nextflow/modules/assemble.nf` - the `path(ref)` input element and the `MapToRef` shell branch.
- `inst/nextflow/modules/coverage.nf` - the MitoFinder branch also serves MapToRef.
- `docker/Dockerfile` - samtools 1.21 to 1.24.
- `docker/README.md`, `NEWS.md`, `DESCRIPTION` - release chores.
- `R/app_assemble_utils.R` - the assembler choice, the five modal inputs, the initial show/hide.
- `R/app_assemble.R` - populate, show/hide, enable/disable, upsert, and the FASTA-topology save check.
- `README.md`, `vignettes/Test-Project-Assemble.Rmd`, `vignettes/Difficult-Assemblies.Rmd`, `vignettes/Your-Own-Project.Rmd`, `vignettes/custom_dbs.Rmd` - docs.
- `NAMESPACE`, `man/` - roxygen regenerate.

---

### Task 1: Reference reader and validation

MapToRef takes one reference mitogenome per parameter set. It cannot reuse `.cadb_parse_gb()`, because that parser silently drops any record without `/organelle="mitochondrion"`, which throws away perfectly good user references. This task builds the reader, its validation rules, and the packaged fixture the rest of the plan tests against.

Reference facts this task depends on (verified 2026-09-03):
- `.cadb_grab_version()` (`R/custom_assembly_db.R:696`) returns the VERSION accession from a block of GenBank lines.
- `.cadb_grab_definition()` (`R/custom_assembly_db.R:682`) returns the (possibly multi-line) DEFINITION text.
- The ORIGIN idiom is `R/custom_assembly_db.R:520-524`.
- `ref_dbs/MitoFinder/NC_002333_Danio_rerio.gb` is a single-record circular zebrafish mitogenome, 16,596 bp, `LOCUS NC_002333 16596 bp DNA circular VRT`. It is NOT installed with the package, so it must be copied into `inst/test_data/`.

**Files:**
- Create: `R/map_to_ref.R`
- Create: `tests/testthat/test-map-to-ref.R`
- Create: `inst/test_data/NC_002333_Danio_rerio.gb` (copy)

**Interfaces:**
- Consumes: nothing from earlier tasks.
- Produces: `maptoref_prepare_ref(ref_file, topology = NA_character_, genetic_code = NA_integer_, out_dir = ".")`, exported, returning a named list with `seq` (uppercase character scalar), `length` (integer), `topology` (`"circular"` or `"linear"`), `accession` (character), `organism` (character), `transl_table` (integer or `NA_integer_`), and `notes` (character vector of warning strings, possibly empty). It creates `<out_dir>/maptoref/`, writes `ref.fasta` there (one record, header `>ACCESSION topology`), and copies the input file verbatim next to it as `reference.gb` or `reference.fasta`. Invalid input is a `stop()`. Task 3 calls it.

- [ ] **Step 1: Copy the fixture**

```bash
cp ref_dbs/MitoFinder/NC_002333_Danio_rerio.gb inst/test_data/NC_002333_Danio_rerio.gb
head -1 inst/test_data/NC_002333_Danio_rerio.gb
```

Expected first line to contain `LOCUS`, `16596 bp`, and `circular`.

- [ ] **Step 2: Write the failing tests**

Create `tests/testthat/test-map-to-ref.R`:

```r
mtr_fixture <- function() {
  p <- system.file("test_data/NC_002333_Danio_rerio.gb", package = "MitoPilot")
  if (!nzchar(p)) {
    p <- testthat::test_path("../..", "inst/test_data/NC_002333_Danio_rerio.gb")
  }
  p
}

mtr_sampler <- function() {
  p <- system.file("test_data/fish_mito_sampler.gb", package = "MitoPilot")
  if (!nzchar(p)) {
    p <- testthat::test_path("../..", "inst/test_data/fish_mito_sampler.gb")
  }
  p
}

mtr_write <- function(dir, name, lines, eol = "\n") {
  fn <- file.path(dir, name)
  con <- file(fn, open = "wb")
  writeLines(lines, con, sep = eol)
  close(con)
  fn
}

test_that("maptoref_prepare_ref reads a single-record circular GenBank reference", {
  skip_if_not(file.exists(mtr_fixture()))
  d <- withr::local_tempdir()
  ref <- maptoref_prepare_ref(mtr_fixture(), out_dir = d)

  expect_equal(ref$topology, "circular")
  expect_equal(ref$length, 16596L)
  expect_equal(nchar(ref$seq), 16596L)
  expect_equal(ref$accession, "NC_002333.2")
  expect_equal(ref$transl_table, 2L)
  expect_true(grepl("Danio rerio", ref$organism))
  expect_true(file.exists(file.path(d, "maptoref", "ref.fasta")))
  expect_true(file.exists(file.path(d, "maptoref", "reference.gb")))
  expect_equal(
    readLines(file.path(d, "maptoref", "ref.fasta"))[1],
    ">NC_002333.2 circular"
  )
})

test_that("maptoref_prepare_ref keeps a record with no organelle qualifier", {
  d <- withr::local_tempdir()
  gb <- mtr_write(d, "plain.gb", c(
    "LOCUS       TEST0001               12000 bp    DNA     linear   INV",
    "DEFINITION  Testus testus mitochondrion, complete genome.",
    "VERSION     TEST0001.1",
    "FEATURES             Location/Qualifiers",
    "     source          1..12000",
    '                     /organism="Testus testus"',
    "     CDS             1..30",
    "                     /transl_table=5",
    "ORIGIN",
    rep(paste("        1", paste(rep("acgt", 15), collapse = " ")), 200L),
    "//"
  ))
  ref <- maptoref_prepare_ref(gb, out_dir = d)
  expect_equal(ref$topology, "linear")
  expect_equal(ref$transl_table, 5L)
  expect_equal(ref$accession, "TEST0001.1")
})

test_that("maptoref_prepare_ref handles CRLF line endings", {
  d <- withr::local_tempdir()
  gb <- mtr_write(d, "crlf.gb", c(
    "LOCUS       TEST0002               12000 bp    DNA     circular INV",
    "DEFINITION  Testus testus mitochondrion, complete genome.",
    "VERSION     TEST0002.1",
    "ORIGIN",
    rep(paste("        1", paste(rep("acgt", 15), collapse = " ")), 200L),
    "//"
  ), eol = "\r\n")
  ref <- maptoref_prepare_ref(gb, out_dir = d)
  expect_equal(ref$topology, "circular")
  expect_equal(nchar(ref$seq), 12000L)
})

test_that("maptoref_prepare_ref rejects a multi-record GenBank file", {
  skip_if_not(file.exists(mtr_sampler()))
  d <- withr::local_tempdir()
  expect_error(
    maptoref_prepare_ref(mtr_sampler(), out_dir = d),
    "exactly one record"
  )
})

test_that("maptoref_prepare_ref requires an explicit topology for FASTA references", {
  d <- withr::local_tempdir()
  fa <- mtr_write(d, "ref.fasta", c(">circular_ref some description",
                                    paste(rep("ACGT", 3000), collapse = "")))
  expect_error(maptoref_prepare_ref(fa, out_dir = d), "topology")

  ref <- maptoref_prepare_ref(fa, topology = "circular", out_dir = d)
  expect_equal(ref$topology, "circular")
  expect_equal(ref$accession, "circular_ref")
  expect_equal(
    readLines(file.path(d, "maptoref", "ref.fasta"))[1],
    ">circular_ref circular"
  )
})

test_that("a GenBank LOCUS line beats the topology option", {
  skip_if_not(file.exists(mtr_fixture()))
  d <- withr::local_tempdir()
  ref <- maptoref_prepare_ref(mtr_fixture(), topology = "linear", out_dir = d)
  expect_equal(ref$topology, "circular")
})

test_that("maptoref_prepare_ref rejects bad sequences, counts, and lengths", {
  d <- withr::local_tempdir()

  two <- mtr_write(d, "two.fasta", c(">a", paste(rep("ACGT", 3000), collapse = ""),
                                     ">b", paste(rep("ACGT", 3000), collapse = "")))
  expect_error(maptoref_prepare_ref(two, topology = "linear", out_dir = d), "exactly one record")

  gappy <- mtr_write(d, "gap.fasta", c(">a", paste(rep("ACG-", 3000), collapse = "")))
  expect_error(maptoref_prepare_ref(gappy, topology = "linear", out_dir = d), "invalid character")

  short <- mtr_write(d, "short.fasta", c(">a", paste(rep("ACGT", 100), collapse = "")))
  expect_error(maptoref_prepare_ref(short, topology = "linear", out_dir = d), "length")
})

test_that("maptoref_prepare_ref warns instead of failing on soft problems", {
  d <- withr::local_tempdir()
  odd <- mtr_write(d, "odd.fasta", c(">a", paste0(paste(rep("ACGT", 1500), collapse = ""),
                                                  paste(rep("N", 400), collapse = ""))))
  ref <- maptoref_prepare_ref(odd, topology = "linear", out_dir = d)
  expect_true(any(grepl("ambiguous", ref$notes)))
  expect_true(any(grepl("outside the usual", ref$notes)))
})

test_that("maptoref_prepare_ref warns when the genetic codes disagree", {
  skip_if_not(file.exists(mtr_fixture()))
  d <- withr::local_tempdir()
  ref <- maptoref_prepare_ref(mtr_fixture(), genetic_code = 5L, out_dir = d)
  expect_true(any(grepl("genetic code", ref$notes)))
})
```

- [ ] **Step 3: Run the tests to verify they fail**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-map-to-ref.R")'`
Expected: FAIL, "could not find function \"maptoref_prepare_ref\"".

- [ ] **Step 4: Write the reader**

The FASTA reader is hand-rolled base R on purpose: a single-record file does not
justify a Biostrings temp-file round trip. This is a deliberate departure from
design 4.5 step 3.

Create `R/map_to_ref.R`:

```r
#' Read and validate a MapToRef reference mitogenome
#'
#' Accepts a single-record GenBank file (first non-blank line starts with
#' LOCUS) or a single-record FASTA. Unlike the custom assembly database
#' parser, no organelle qualifier is required.
#'
#' @param ref_file Path to the reference file.
#' @param topology "circular" or "linear". Required for a FASTA reference,
#'   ignored for GenBank, where the LOCUS line wins.
#' @param genetic_code The sample's genetic code, used only to warn when the
#'   reference disagrees.
#' @param out_dir Directory to write the `maptoref/` working files into.
#'
#' @return A list with seq, length, topology, accession, organism,
#'   transl_table, and notes.
#' @export
maptoref_prepare_ref <- function(ref_file,
                                 topology = NA_character_,
                                 genetic_code = NA_integer_,
                                 out_dir = ".") {
  if (!file.exists(ref_file)) {
    stop("Reference file not found: ", ref_file)
  }
  lines <- gsub("\r", "", readLines(ref_file, warn = FALSE), fixed = TRUE)
  nonblank <- which(nzchar(trimws(lines)))
  if (length(nonblank) == 0L) {
    stop("Reference file is empty: ", ref_file)
  }
  first <- lines[nonblank[1]]

  if (grepl("^LOCUS", first)) {
    ref <- .mtr_read_gb(lines)
    ext <- "gb"
  } else if (grepl("^>", first)) {
    ref <- .mtr_read_fasta(lines, topology)
    ext <- "fasta"
  } else {
    stop("Reference must be a GenBank file (first line starts with LOCUS) ",
         "or a FASTA (first line starts with >)")
  }

  bad <- unique(strsplit(gsub("[ACGTRYSWKMBDHVN]", "", ref$seq), "")[[1]])
  if (length(bad) > 0L) {
    stop("Reference sequence has invalid characters: ", paste(bad, collapse = " "))
  }
  ref$length <- nchar(ref$seq)
  if (ref$length < 5000L || ref$length > 50000L) {
    stop("Reference length ", ref$length, " is outside the accepted range ",
         "[5000, 50000]; this does not look like a mitogenome")
  }

  notes <- character(0)
  if (ref$length < 10000L || ref$length > 25000L) {
    notes <- c(notes, paste0(
      "Reference length ", ref$length,
      " is outside the usual mitogenome range [10000, 25000]."))
  }
  amb <- nchar(gsub("[ACGT]", "", ref$seq))
  if (amb > 0.01 * ref$length) {
    notes <- c(notes, paste0(
      "Reference has ", amb, " ambiguous bases (", round(100 * amb / ref$length, 1),
      "%); mapping is weaker there."))
  }
  if (!is.na(genetic_code) && !is.na(ref$transl_table) &&
      as.integer(genetic_code) != ref$transl_table) {
    notes <- c(notes, paste0(
      "Reference genetic code ", ref$transl_table, " differs from the sample's ",
      genetic_code, "; annotation uses the sample's."))
  }
  ref$notes <- notes

  work <- file.path(out_dir, "maptoref")
  dir.create(work, recursive = TRUE, showWarnings = FALSE)
  writeLines(
    c(paste0(">", ref$accession, " ", ref$topology), ref$seq),
    file.path(work, "ref.fasta")
  )
  file.copy(ref_file, file.path(work, paste0("reference.", ext)), overwrite = TRUE)
  ref
}

#' @noRd
.mtr_read_gb <- function(lines) {
  ends <- which(trimws(lines) == "//")
  if (length(ends) != 1L) {
    stop("Reference must contain exactly one record; this file has ",
         length(ends), ". The MitoFinder database format is not accepted here.")
  }
  block <- lines[1:ends[1]]

  locus <- grep("^LOCUS", block, value = TRUE)[1]
  tokens <- strsplit(trimws(locus), "\\s+")[[1]]
  topology <- if (any(tolower(tokens) == "circular")) "circular" else "linear"

  accession <- .cadb_grab_version(block)
  if (is.na(accession)) {
    accession <- tokens[2]
  }
  organism <- trimws(sub("^DEFINITION\\s*", "", .cadb_grab_definition(block)))

  tt <- grep("/transl_table=", block, fixed = TRUE, value = TRUE)
  transl_table <- if (length(tt) == 0L) {
    NA_integer_
  } else {
    suppressWarnings(as.integer(sub('.*/transl_table=([0-9]+).*', "\\1", tt[1])))
  }

  origin <- grep("^ORIGIN", block)
  if (length(origin) == 0L || origin[1] >= length(block)) {
    stop("Reference GenBank record has no ORIGIN sequence")
  }
  seq_lines <- block[(origin[1] + 1L):(length(block) - 1L)]
  seq <- toupper(gsub("[^A-Za-z]", "", paste(seq_lines, collapse = "")))
  if (!nzchar(seq)) {
    stop("Reference GenBank record has an empty ORIGIN sequence")
  }

  list(seq = seq, topology = topology, accession = accession,
       organism = organism, transl_table = transl_table)
}

#' @noRd
.mtr_read_fasta <- function(lines, topology) {
  heads <- grep("^>", lines)
  if (length(heads) != 1L) {
    stop("Reference must contain exactly one record; this file has ",
         length(heads), ".")
  }
  if (is.na(topology) || !nzchar(trimws(topology))) {
    stop("Set the reference topology (circular or linear) for a FASTA reference.")
  }
  topology <- tolower(trimws(topology))
  if (!topology %in% c("circular", "linear")) {
    stop("Reference topology must be circular or linear, not: ", topology)
  }
  header <- sub("^>", "", lines[heads[1]])
  accession <- strsplit(trimws(header), "\\s+")[[1]][1]
  seq <- toupper(gsub("[^A-Za-z-]", "",
                      paste(lines[(heads[1] + 1L):length(lines)], collapse = "")))
  if (!nzchar(seq)) {
    stop("Reference FASTA record has no sequence")
  }
  list(seq = seq, topology = topology, accession = accession,
       organism = trimws(header), transl_table = NA_integer_)
}
```

- [ ] **Step 5: Run the tests to verify they pass**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-map-to-ref.R")'`
Expected: all PASS.

- [ ] **Step 6: Regenerate documentation and check ASCII**

```bash
Rscript -e 'devtools::document()'
grep -nP '[^\x00-\x7F]' R/map_to_ref.R tests/testthat/test-map-to-ref.R
```

Expected: `document()` adds `maptoref_prepare_ref` to `NAMESPACE` and writes `man/maptoref_prepare_ref.Rd`; the grep prints nothing.

- [ ] **Step 7: Commit**

```bash
git add R/map_to_ref.R tests/testthat/test-map-to-ref.R inst/test_data/NC_002333_Danio_rerio.gb NAMESPACE man/
git commit -m "feat: MapToRef reference reader and validation"
```

---

### Task 2: Pure consensus helpers

The loop is string surgery around two command-line tools. Every piece of that surgery is a pure function here, so it can be tested without a mapper. These are the four places the design says a silent bug would otherwise go unnoticed: the N fill, the circular splice, the `--mark-ins` tokenizer, and the consensus-option validator.

Rules being implemented, from `tools/map_to_ref_design.md` sections 4.6 and 4.7:
- Fill: every `N` and `*` in a raw consensus takes the base at the same position of the previous mapping reference. Fill applies to the mapping reference only, never to the published product.
- Splice (circular only): with L = reference length and F = the appended flank, the output is `x[(L+1):(L+F/2)]` followed by `x[(F/2+1):L]`. Length is preserved AND output position 1 must be reference position 1. A mis-rotation preserves length, so length alone proves nothing.
- `--mark-ins` marks an inserted base with a leading `_`. Lowercase letters are base-versus-gap heterozygous codes and are NOT insertion marks, so tokenize on `_` only, never on case.
- Half-present (lowercase) calls become `N` in the published product and are counted.
- `-c`, `-H`, and `-q` do nothing outside `-m simple`; `--min-MQ` above 0 destroys the circular seam; ten flags are set by the code and must be refused in the user string; quote characters are refused too, because the option string is interpolated into an `Rscript -e` call.

**Files:**
- Modify: `R/map_to_ref.R` (append)
- Modify: `tests/testthat/test-map-to-ref.R` (append)

**Interfaces:**
- Consumes: nothing from Task 1 (these helpers are independent of the reader).
- Produces, all internal (`@noRd`), used by Task 3:
  - `.mtr_fill(raw, prev)` - two equal-length character scalars in, one character scalar out.
  - `.mtr_splice(x, len, flank)` - character VECTOR in (one element per position: a single character, or a token from `.mtr_parse_marked()`), character vector of length `len` out. `flank = 0` returns `x[1:len]`.
  - `.mtr_parse_marked(s)` - character scalar in, character vector out, one token per reference position, each token being the position character plus any `_`-marked inserted run.
  - `.mtr_tokens_to_seq(tokens)` - tokens in, published sequence out: `*` dropped, `_` markers dropped, lowercase half-present calls turned into `N`, everything uppercased. Returns a list with `seq` and `half_deletions` (integer count).
  - `.mtr_strip_ends(seq)` - strips leading and trailing `N` runs, keeps internal ones.
  - `.mtr_check_consensus_opts(opts, circular)` - returns a list with `ok` (logical), `notes` (character), and `error` (character scalar or `NA_character_`).
  - `.mtr_stop(bases_changed, reads_now, reads_prev)` - logical scalar.

- [ ] **Step 1: Write the failing tests**

Append to `tests/testthat/test-map-to-ref.R`:

```r
test_that(".mtr_fill takes N and * from the previous reference", {
  expect_equal(.mtr_fill("ACNT*G", "ACGTAG"), "ACGTAG")
  expect_equal(.mtr_fill("NNNN", "ACGT"), "ACGT")
  expect_equal(.mtr_fill("ACGT", "TTTT"), "ACGT")
  expect_error(.mtr_fill("ACGT", "ACG"), "same length")
})

test_that(".mtr_splice restores the origin and the length", {
  # Truth: 1..200 as distinct three-character position labels.
  truth <- sprintf("%03d", 1:200)
  # The mapping construct is the reference plus its own first F bases.
  flank <- 50L
  construct <- c(truth, truth[1:flank])

  spliced <- .mtr_splice(construct, len = 200L, flank = flank)

  expect_length(spliced, 200L)
  expect_equal(spliced[1], "001")
  expect_equal(spliced[25], "025")
  expect_equal(spliced[200], "200")
  expect_equal(spliced, truth)
})

test_that(".mtr_splice takes the first F/2 positions from the appended copy", {
  truth <- sprintf("%03d", 1:200)
  construct <- c(truth, truth[1:50])
  # Blank the low-depth head of the first copy the way samtools would.
  construct[1:25] <- "NNN"

  spliced <- .mtr_splice(construct, len = 200L, flank = 50L)

  expect_equal(spliced[1:25], truth[1:25])
  expect_false(any(spliced == "NNN"))
})

test_that(".mtr_splice with no flank is a no-op on the reference extent", {
  x <- c("A", "C", "G", "T")
  expect_equal(.mtr_splice(x, len = 4L, flank = 0L), x)
})

test_that(".mtr_parse_marked keys on the underscore, never on case", {
  # A: plain. C: followed by an inserted G. *: a called deletion.
  # t: a half-present (base versus gap) call, NOT an insertion.
  # G: followed by a lowercase inserted base.
  expect_equal(
    .mtr_parse_marked("AC_G*tG_a"),
    c("A", "C_G", "*", "t", "G_a")
  )
  expect_equal(.mtr_parse_marked("ACGT"), c("A", "C", "G", "T"))
  expect_error(.mtr_parse_marked("_AACGT"), "insertion")
})

test_that(".mtr_tokens_to_seq drops deletions and markers and calls half-present N", {
  res <- .mtr_tokens_to_seq(c("A", "C_G", "*", "t", "G_a"))
  expect_equal(res$seq, "ACGNGN")
  expect_equal(res$half_deletions, 2L)
})

test_that(".mtr_strip_ends removes flanking N runs only", {
  expect_equal(.mtr_strip_ends("NNACNNGTNN"), "ACNNGT")
  expect_equal(.mtr_strip_ends("ACGT"), "ACGT")
  expect_equal(.mtr_strip_ends("NNNN"), "")
})

test_that(".mtr_check_consensus_opts warns about mode-specific flags", {
  res <- .mtr_check_consensus_opts("-c 0.65 -H 0.3", circular = FALSE)
  expect_true(res$ok)
  expect_true(any(grepl("-m simple", res$notes)))

  res <- .mtr_check_consensus_opts("-m simple -c 0.65", circular = FALSE)
  expect_true(res$ok)
  expect_length(res$notes, 0L)
})

test_that(".mtr_check_consensus_opts refuses a MAPQ filter on a circular reference", {
  res <- .mtr_check_consensus_opts("--min-MQ 20", circular = TRUE)
  expect_false(res$ok)
  expect_match(res$error, "--min-MQ")

  res <- .mtr_check_consensus_opts("--min-MQ 20", circular = FALSE)
  expect_true(res$ok)
  expect_true(any(grepl("--min-MQ", res$notes)))

  res <- .mtr_check_consensus_opts("--min-MQ 0", circular = TRUE)
  expect_true(res$ok)
})

test_that(".mtr_check_consensus_opts refuses flags the code sets itself", {
  for (flag in c("-a", "-A", "-T ref.fa", "--show-del yes", "--show-ins yes",
                 "--mark-ins", "--no-use-MQ", "-o out.fa", "-f fasta", "-r chr1")) {
    res <- .mtr_check_consensus_opts(flag, circular = FALSE)
    expect_false(res$ok, info = flag)
  }
  expect_true(.mtr_check_consensus_opts("-d 3 --min-BQ 20", circular = TRUE)$ok)
  expect_true(.mtr_check_consensus_opts("", circular = TRUE)$ok)
})

test_that(".mtr_check_consensus_opts refuses quote characters", {
  res <- .mtr_check_consensus_opts("-d 3 --min-BQ 20 --extra 'x'", circular = FALSE)
  expect_false(res$ok)
  expect_match(res$error, "quote characters")

  res <- .mtr_check_consensus_opts("-d 3 --min-BQ \"20\"", circular = TRUE)
  expect_false(res$ok)
})

test_that(".mtr_stop needs both the base term and the read term", {
  expect_true(.mtr_stop(bases_changed = 4L, reads_now = 100000L, reads_prev = 100000L))
  expect_false(.mtr_stop(bases_changed = 40L, reads_now = 100000L, reads_prev = 100000L))
  expect_false(.mtr_stop(bases_changed = 0L, reads_now = 110000L, reads_prev = 100000L))
  expect_true(.mtr_stop(bases_changed = 0L, reads_now = 100050L, reads_prev = 100000L))
})
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-map-to-ref.R")'`
Expected: FAIL, "could not find function \".mtr_fill\"".

- [ ] **Step 3: Write the helpers**

Append to `R/map_to_ref.R`:

```r
#' @noRd
.mtr_fill <- function(raw, prev) {
  a <- strsplit(raw, "", fixed = TRUE)[[1]]
  b <- strsplit(prev, "", fixed = TRUE)[[1]]
  if (length(a) != length(b)) {
    stop("consensus and reference must be the same length: ",
         length(a), " vs ", length(b))
  }
  hit <- a %in% c("N", "n", "*")
  a[hit] <- b[hit]
  paste(a, collapse = "")
}

# The first F/2 positions of the reference copy have structurally low depth,
# so their calls are taken from the appended copy instead.
#' @noRd
.mtr_splice <- function(x, len, flank) {
  if (length(x) != len + flank) {
    stop("expected ", len + flank, " positions, got ", length(x))
  }
  if (flank == 0L) {
    return(x[seq_len(len)])
  }
  half <- flank %/% 2L
  c(x[(len + 1L):(len + half)], x[(half + 1L):len])
}

# samtools consensus --mark-ins prefixes an inserted base with "_". Lowercase
# letters are base-versus-gap codes and appear at any position, so case must
# not be used to detect insertions.
#' @noRd
.mtr_parse_marked <- function(s) {
  ch <- strsplit(s, "", fixed = TRUE)[[1]]
  n <- length(ch)
  tokens <- character(n)
  k <- 0L
  i <- 1L
  while (i <= n) {
    if (ch[i] == "_") {
      if (k == 0L) {
        stop("consensus begins with an insertion mark")
      }
      tokens[k] <- paste0(tokens[k], ch[i], ch[i + 1L])
      i <- i + 2L
    } else {
      k <- k + 1L
      tokens[k] <- ch[i]
      i <- i + 1L
    }
  }
  tokens[seq_len(k)]
}

#' @noRd
.mtr_tokens_to_seq <- function(tokens) {
  flat <- strsplit(paste(tokens, collapse = ""), "", fixed = TRUE)[[1]]
  flat <- flat[!flat %in% c("*", "_")]
  half <- flat %in% c("a", "c", "g", "t")
  flat[half] <- "N"
  list(seq = toupper(paste(flat, collapse = "")), half_deletions = sum(half))
}

#' @noRd
.mtr_strip_ends <- function(seq) {
  sub("N+$", "", sub("^N+", "", seq))
}

#' @noRd
.mtr_check_consensus_opts <- function(opts, circular) {
  opts <- if (is.na(opts)) "" else trimws(opts)
  notes <- character(0)
  error <- NA_character_

  if (grepl("['\"]", opts)) {
    return(list(ok = FALSE, notes = notes,
                error = "consensus options must not contain quote characters"))
  }

  refused <- c("-a", "-A", "-T", "-o", "-f", "-r",
               "--show-del", "--show-ins", "--mark-ins", "--no-use-MQ")
  tokens <- strsplit(opts, "\\s+")[[1]]
  hit <- refused[refused %in% tokens]
  if (length(hit) > 0L) {
    error <- paste0("Consensus options set by MitoPilot cannot be given here: ",
                    paste(hit, collapse = " "))
  }

  mode_only <- c("-c", "-H", "-q")[c("-c", "-H", "-q") %in% tokens]
  if (length(mode_only) > 0L && !("simple" %in% tokens && "-m" %in% tokens)) {
    notes <- c(notes, paste0(
      "Consensus options ", paste(mode_only, collapse = " "),
      " were ignored; they only apply with -m simple."))
  }

  mq <- which(tokens == "--min-MQ")
  if (length(mq) > 0L && length(tokens) > mq[1]) {
    value <- suppressWarnings(as.numeric(tokens[mq[1] + 1L]))
    if (!is.na(value) && value > 0) {
      if (isTRUE(circular)) {
        error <- paste0(
          "--min-MQ above 0 blanks the origin of a circular reference; ",
          "reads inside the duplicated block carry mapping quality 1.")
      } else {
        notes <- c(notes, paste0(
          "--min-MQ ", value, " discards multi-mapping reads; ",
          "mapping quality carries little signal against a mitogenome reference."))
      }
    }
  }

  list(ok = is.na(error), notes = notes, error = error)
}

# Two terms: the sequence has settled AND reads have stopped being recruited.
#' @noRd
.mtr_stop <- function(bases_changed, reads_now, reads_prev) {
  denom <- max(as.numeric(reads_prev), 1)
  bases_changed < 5L && abs(as.numeric(reads_now) - as.numeric(reads_prev)) / denom < 0.001
}
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-map-to-ref.R")'`
Expected: all PASS.

- [ ] **Step 5: Commit**

```bash
git add R/map_to_ref.R tests/testthat/test-map-to-ref.R
git commit -m "feat: MapToRef consensus helpers"
```

---

### Task 3: The `map_to_ref()` driver

This is the loop itself: pass 1 over all reads, recruit the mapped pairs, iterate consensus-into-reference until it stops changing or the cap is hit, then one final pass over all reads against the converged reference, and call the published sequence from that. It also writes every file the assemble contract requires, and the `_assembly_0.fasta` sentinel with a reason when anything fails, so a bad sample never kills the run.

Facts this task depends on (verified 2026-09-03):
- `cigar_ref_length()` (`R/circularize_asmb.R:611-620`) turns a CIGAR string into a reference span; it is package-internal and reusable here.
- The flank formula `flank <- min(500L, len %/% 2L)` is `R/circularize_asmb.R:529`; `.coverage_extend_circular()` (`R/coverage.R:250-259`) is only the elongate pattern being mirrored.
- `coverage()` shells out with `stringr::str_glue(...) |> system()` (`R/coverage.R:58-82`); this task follows the same style.
- `--no-unal` must NOT be used in pass 1: bowtie2 would drop the unmapped mate of a half-mapped pair, and recruitment would then keep only fully mapped pairs. `-G 12` (drop records with both mates unmapped) keeps the BAM small instead.
- `--no-use-MQ` is load-bearing: reads inside the duplicated block get mapping quality 1.

**Files:**
- Modify: `R/map_to_ref.R` (append)
- Create: `tests/testthat/test-map-to-ref-loop.R`

**Interfaces:**
- Consumes: `maptoref_prepare_ref()` (Task 1) and every `.mtr_*` helper (Task 2).
- Produces: `map_to_ref(id, ref, reads_1, reads_2, bowtie2_opts, consensus_opts, iter_cap, topology, genetic_code, cpus, out_dir)`, exported, returning `invisible(TRUE)` on success and `invisible(FALSE)` after writing the sentinel. Task 6 calls it from `assemble.nf` in exactly this argument order.

- [ ] **Step 1: Write the failing test**

Create `tests/testthat/test-map-to-ref-loop.R`:

```r
# Stub binaries standing in for bowtie2 and samtools, so the loop can be tested
# without a mapper. Pattern follows tests/testthat/test-find-mito.R:248-260.
mtr_stub_bin <- function(dir) {
  bin <- file.path(dir, "bin")
  dir.create(bin, showWarnings = FALSE)

  writeLines(c("#!/bin/sh", "exit 0"), file.path(bin, "bowtie2-build"))
  writeLines(c("#!/bin/sh", "exit 0"), file.path(bin, "bowtie2"))
  writeLines(c(
    "#!/bin/sh",
    "cmd=$1; shift",
    "case \"$cmd\" in",
    "  view)",
    "    case \" $* \" in",
    "      *\" -c \"*) echo 5000 ;;",
    "      *) if [ -n \"$MTR_STUB_SAM\" ]; then cat \"$MTR_STUB_SAM\"; fi ;;",
    "    esac",
    "    ;;",
    "  sort)",
    "    out=\"\"",
    "    while [ $# -gt 0 ]; do case \"$1\" in -o) out=$2; shift ;; esac; shift; done",
    "    if [ -p /dev/stdin ]; then cat > /dev/null; fi",
    "    if [ -n \"$out\" ]; then : > \"$out\"; fi",
    "    ;;",
    "  fastq)",
    "    r1=\"\"; r2=\"\"",
    "    while [ $# -gt 0 ]; do case \"$1\" in -1) r1=$2; shift ;; -2) r2=$2; shift ;; esac; shift; done",
    "    cat > /dev/null",
    "    if [ -n \"$r1\" ]; then printf '@r1\\nACGT\\n+\\nIIII\\n' > \"$r1\"; fi",
    "    if [ -n \"$r2\" ]; then printf '@r1\\nACGT\\n+\\nIIII\\n' > \"$r2\"; fi",
    "    ;;",
    "  consensus)",
    "    case \" $* \" in",
    "      *\" --mark-ins \"*) cat \"$MTR_STUB_CONS_INS\" ;;",
    "      *) cat \"$MTR_STUB_CONS\" ;;",
    "    esac",
    "    ;;",
    "esac",
    "exit 0"
  ), file.path(bin, "samtools"))

  Sys.chmod(list.files(bin, full.names = TRUE), "0755")
  bin
}

# A wrapped FASTA, so the test also proves the reader unwraps consensus output.
mtr_write_wrapped <- function(fn, seq) {
  starts <- seq(1L, nchar(seq), by = 60L)
  writeLines(c(">cons", substring(seq, starts, pmin(starts + 59L, nchar(seq)))), fn)
  fn
}

mtr_setup <- function(dir, junction = TRUE) {
  ref_seq <- paste(rep("ACGTACGTTG", 600L), collapse = "")   # 6000 bp
  len <- nchar(ref_seq)
  flank <- min(500L, len %/% 2L)

  ref_fa <- file.path(dir, "ref.fasta")
  writeLines(c(">TESTREF", ref_seq), ref_fa)

  # The consensus the stub returns: the mapping construct with one substitution.
  construct <- paste0(ref_seq, substr(ref_seq, 1L, flank))
  substr(construct, 3000L, 3000L) <- "T"
  mtr_write_wrapped(file.path(dir, "cons.fa"), construct)
  mtr_write_wrapped(file.path(dir, "cons_ins.fa"), construct)

  sam <- file.path(dir, "reads.sam")
  if (junction) {
    # One primary alignment starting 100 bp before the seam with a 200M span.
    writeLines(
      paste("r1", "0", "ref", len - 99L, "42", "200M", "*", "0", "0", "*", "*",
            sep = "\t"),
      sam
    )
  } else {
    writeLines(
      paste("r1", "0", "ref", "10", "42", "100M", "*", "0", "0", "*", "*",
            sep = "\t"),
      sam
    )
  }

  writeLines("@r1", file.path(dir, "R1.fq"))
  writeLines("@r1", file.path(dir, "R2.fq"))

  withr::local_envvar(c(
    PATH = paste(mtr_stub_bin(dir), Sys.getenv("PATH"), sep = ":"),
    MTR_STUB_CONS = file.path(dir, "cons.fa"),
    MTR_STUB_CONS_INS = file.path(dir, "cons_ins.fa"),
    MTR_STUB_SAM = sam
  ), .local_envir = parent.frame())

  list(ref = ref_fa, r1 = file.path(dir, "R1.fq"), r2 = file.path(dir, "R2.fq"),
       len = len, flank = flank)
}

mtr_summary <- function(out_dir, id = "T1") {
  lines <- readLines(file.path(out_dir, paste0(id, "_summary.txt")))
  kv <- lines[grepl("=", lines, fixed = TRUE)]
  stats::setNames(sub("^[^=]*=", "", kv), sub("=.*$", "", kv))
}

test_that("map_to_ref publishes a circular consensus and the loop record", {
  skip_on_os("windows")
  d <- withr::local_tempdir()
  s <- mtr_setup(d)
  out <- file.path(d, "out")

  ok <- map_to_ref("T1", s$ref, s$r1, s$r2,
                   bowtie2_opts = "--very-sensitive-local",
                   consensus_opts = "-d 3 --min-BQ 20",
                   iter_cap = 5, topology = "circular",
                   genetic_code = 2, cpus = 1, out_dir = out)

  expect_true(ok)
  fa <- readLines(file.path(out, "T1_assembly_1.fasta"))
  expect_equal(fa[1], ">T1.1.1 circular")
  expect_equal(nchar(paste(fa[-1], collapse = "")), s$len)

  sm <- mtr_summary(out)
  expect_equal(sm[["reference_topology"]], "circular")
  expect_equal(sm[["published_topology"]], "circular")
  expect_equal(sm[["accession"]], "TESTREF")
  expect_true(as.integer(sm[["junction_depth"]]) > 0L)
  expect_equal(sm[["stop_reason"]], "converged")

  expect_true(file.exists(file.path(out, "maptoref", "subs_only.fasta")))
  expect_true(file.exists(file.path(out, "maptoref", "iterations.tsv")))
  iters <- read.delim(file.path(out, "maptoref", "iterations.tsv"))
  expect_true(nrow(iters) >= 1L)
  expect_true(all(c("pass", "reads_mapped", "bases_changed", "n_count",
                    "stop_reason") %in% names(iters)))
  expect_true(as.integer(sm[["passes_run"]]) < 5L)
})

test_that("a circular reference with no junction reads is published as linear", {
  skip_on_os("windows")
  d <- withr::local_tempdir()
  s <- mtr_setup(d, junction = FALSE)
  out <- file.path(d, "out")

  map_to_ref("T1", s$ref, s$r1, s$r2, "--very-sensitive-local",
             "-d 3 --min-BQ 20", 5, "circular", 2, 1, out)

  fa <- readLines(file.path(out, "T1_assembly_1.fasta"))
  expect_equal(fa[1], ">T1.1.1 linear")

  sm <- mtr_summary(out)
  expect_equal(sm[["reference_topology"]], "circular")
  expect_equal(sm[["published_topology"]], "linear")
  expect_equal(sm[["junction_depth"]], "0")
  notes <- readLines(file.path(out, "T1_summary.txt"))
  expect_true(any(grepl("published as linear", notes)))
})

test_that("map_to_ref writes the sentinel instead of failing the run", {
  skip_on_os("windows")
  d <- withr::local_tempdir()
  s <- mtr_setup(d)
  out <- file.path(d, "out")
  bad <- file.path(d, "bad.fasta")
  writeLines(c(">a", "ACGT"), bad)

  ok <- map_to_ref("T1", bad, s$r1, s$r2, "--very-sensitive-local",
                   "-d 3 --min-BQ 20", 5, "linear", 2, 1, out)

  expect_false(ok)
  expect_equal(readLines(file.path(out, "T1_assembly_0.fasta"))[1],
               ">No assembly found")
  expect_true(any(grepl("outside the accepted range",
                        readLines(file.path(out, "assembler.log.txt")))))
})

test_that("a refused consensus flag is a per-sample failure, not a crash", {
  skip_on_os("windows")
  d <- withr::local_tempdir()
  s <- mtr_setup(d)
  out <- file.path(d, "out")

  ok <- map_to_ref("T1", s$ref, s$r1, s$r2, "--very-sensitive-local",
                   "--min-MQ 20", 5, "circular", 2, 1, out)

  expect_false(ok)
  expect_true(file.exists(file.path(out, "T1_assembly_0.fasta")))
  expect_true(any(grepl("--min-MQ", readLines(file.path(out, "assembler.log.txt")))))
})
```

- [ ] **Step 2: Run the test to verify it fails**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-map-to-ref-loop.R")'`
Expected: FAIL, "could not find function \"map_to_ref\"".

- [ ] **Step 3: Write the driver**

A successful run deletes the reproducible transients under `maptoref/` before it
returns: the BAMs and their indexes, the bowtie2 index files, and the recruited
FASTQ subset. The FASTA, TSV, and log files stay, so what gets published is the
small loop record design 4.11 promises. A failed run deletes nothing, so the
evidence is still there to debug.

Append to `R/map_to_ref.R`:

```r
#' Map-to-reference mitogenome assembly
#'
#' Maps a sample's reads to a reference mitogenome, feeds the consensus back in
#' as the next mapping reference until it stops changing, then calls the
#' published sequence from a final pass over all reads. The reference base never
#' enters the published sequence.
#'
#' @param id Sample ID.
#' @param ref Path to the reference (.gb or FASTA, one record).
#' @param reads_1,reads_2 Preprocessed paired reads.
#' @param bowtie2_opts Flags passed verbatim to bowtie2.
#' @param consensus_opts Flags passed to samtools consensus after validation.
#' @param iter_cap Maximum number of iteration passes.
#' @param topology "circular" or "linear"; required for a FASTA reference,
#'   ignored for GenBank.
#' @param genetic_code The sample's genetic code, used only for a warning.
#' @param cpus Threads.
#' @param out_dir Output directory.
#'
#' @return invisibly TRUE on success, FALSE after writing the failure sentinel.
#' @export
map_to_ref <- function(id, ref, reads_1, reads_2,
                       bowtie2_opts = "--very-sensitive-local",
                       consensus_opts = "-d 3 --min-BQ 20",
                       iter_cap = 5,
                       topology = NA_character_,
                       genetic_code = NA_integer_,
                       cpus = 4,
                       out_dir = ".") {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  log_fn <- file.path(out_dir, "assembler.log.txt")
  if (!file.exists(log_fn)) {
    file.create(log_fn)
  }
  # The option strings are interpolated into an Rscript -e call, so a quote
  # character breaks the R expression. consensus_opts is covered by
  # .mtr_check_consensus_opts().
  if (!is.na(bowtie2_opts) && grepl("['\"]", bowtie2_opts)) {
    .mtr_fail(id, out_dir, log_fn,
              "bowtie2 options must not contain quote characters")
    return(invisible(FALSE))
  }
  ok <- tryCatch(
    {
      .mtr_assemble(id, ref, reads_1, reads_2, bowtie2_opts, consensus_opts,
                    as.integer(iter_cap), topology, genetic_code,
                    as.integer(cpus), out_dir, log_fn)
      TRUE
    },
    error = function(e) {
      .mtr_fail(id, out_dir, log_fn, conditionMessage(e))
      FALSE
    }
  )
  invisible(ok)
}

#' @noRd
.mtr_log <- function(log_fn, ...) {
  cat(paste0(..., "\n"), file = log_fn, append = TRUE)
}

# bash -o pipefail so a failed bowtie2 stage is not masked by a later stage that
# exits 0. Each bowtie2 call carries its own 2>> redirect; the trailing one here
# covers the single-command calls.
#' @noRd
.mtr_run <- function(cmd, log_fn) {
  .mtr_log(log_fn, "+ ", cmd)
  full <- paste0(cmd, " 2>> ", shQuote(log_fn))
  status <- system2("bash", c("-o", "pipefail", "-c", shQuote(full)))
  if (status != 0L) {
    stop("command failed (exit ", status, "): ", cmd)
  }
  invisible(TRUE)
}

# samtools consensus line-wraps its output, so every read of a consensus FASTA
# unwraps before indexing.
#' @noRd
.mtr_read_seq <- function(fn) {
  lines <- readLines(fn, warn = FALSE)
  paste(lines[!grepl("^>", lines)], collapse = "")
}

#' @noRd
.mtr_extend <- function(seq, flank) {
  if (flank == 0L) seq else paste0(seq, substr(seq, 1L, flank))
}

#' @noRd
.mtr_count_primary <- function(bam) {
  out <- suppressWarnings(system2(
    "samtools", c("view", "-c", "-F", "0x904", shQuote(bam)),
    stdout = TRUE, stderr = FALSE
  ))
  value <- suppressWarnings(as.integer(out[1]))
  if (is.na(value)) 0L else value
}

# Primary alignments whose reference span crosses the seam at position len.
#' @noRd
.mtr_junction_depth <- function(bam, len, min_overhang = 30L) {
  sam <- suppressWarnings(system2(
    "samtools", c("view", "-F", "0x904", shQuote(bam)),
    stdout = TRUE, stderr = FALSE
  ))
  if (length(sam) == 0L) {
    return(0L)
  }
  fields <- stringr::str_split(sam, "\t", simplify = TRUE)
  starts <- suppressWarnings(as.integer(fields[, 4]))
  ends <- starts + cigar_ref_length(fields[, 6]) - 1L
  ok <- !is.na(starts) & !is.na(ends)
  sum(ok & starts <= len - min_overhang & ends >= len + min_overhang)
}

#' @noRd
.mtr_diff_count <- function(a, b) {
  x <- strsplit(a, "", fixed = TRUE)[[1]]
  y <- strsplit(b, "", fixed = TRUE)[[1]]
  n <- min(length(x), length(y))
  sum(x[seq_len(n)] != y[seq_len(n)]) + abs(length(x) - length(y))
}

#' @noRd
.mtr_fail <- function(id, out_dir, log_fn, reason) {
  .mtr_log(log_fn, "FAILED: ", reason)
  writeLines(">No assembly found",
             file.path(out_dir, paste0(id, "_assembly_0.fasta")))
  writeLines(c("assembler=MapToRef", paste0("failure=", reason)),
             file.path(out_dir, paste0(id, "_summary.txt")))
  invisible(FALSE)
}

#' @noRd
.mtr_assemble <- function(id, ref_file, reads_1, reads_2, bowtie2_opts,
                          consensus_opts, iter_cap, topology, genetic_code,
                          cpus, out_dir, log_fn) {
  ref <- maptoref_prepare_ref(ref_file, topology = topology,
                              genetic_code = genetic_code, out_dir = out_dir)
  work <- file.path(out_dir, "maptoref")
  notes <- ref$notes
  circular <- identical(ref$topology, "circular")
  len <- ref$length
  flank <- if (circular) min(500L, len %/% 2L) else 0L
  .mtr_log(log_fn, "reference ", ref$accession, " ", ref$organism,
           " (", len, " bp, ", ref$topology, ")")

  check <- .mtr_check_consensus_opts(consensus_opts, circular)
  if (!check$ok) {
    stop(check$error)
  }
  notes <- c(notes, check$notes)
  user_cons <- if (is.na(consensus_opts)) "" else consensus_opts
  fixed_cons <- paste("-a -A --no-use-MQ --show-del yes -@", cpus)

  ref_fa <- file.path(work, "ref_0.fa")
  writeLines(c(">mapping_ref", .mtr_extend(ref$seq, flank)), ref_fa)
  prev_ref <- .mtr_extend(ref$seq, flank)
  prev_cons <- ref$seq

  idx <- file.path(work, "idx")
  bam <- file.path(work, "pass_1.bam")
  .mtr_run(stringr::str_glue("bowtie2-build -q {ref_fa} {idx}"), log_fn)
  # No --no-unal: it would drop the unmapped mate of a half-mapped pair, and
  # recruitment below would then keep only fully mapped pairs.
  .mtr_run(stringr::str_glue(
    "bowtie2 {bowtie2_opts} -x {idx} -1 {reads_1} -2 {reads_2} --threads {cpus} ",
    "2>> {log_fn} | samtools view -b -G 12 - | samtools sort -@ {cpus} -o {bam} -"
  ), log_fn)

  reads_pass_1 <- .mtr_count_primary(bam)
  if (reads_pass_1 < 100L) {
    stop(reads_pass_1, " reads mapped to the reference; use a closer reference ",
         "or a more sensitive preset")
  }
  if (reads_pass_1 < 1000L) {
    notes <- c(notes, paste0(
      "Only ", reads_pass_1, " reads mapped; check that the reference is a ",
      "mitogenome from a related taxon."))
  }

  sub_1 <- file.path(work, "sub_R1.fq")
  sub_2 <- file.path(work, "sub_R2.fq")
  .mtr_run(stringr::str_glue(
    "samtools sort -n {bam} | samtools fastq -1 {sub_1} -2 {sub_2} ",
    "-0 /dev/null -s /dev/null -n"
  ), log_fn)

  iters <- data.frame()
  reads_prev <- reads_pass_1
  stop_reason <- "cap"
  passes <- 0L

  for (k in seq_len(max(1L, iter_cap))) {
    passes <- k
    raw <- file.path(work, paste0("raw_", k, ".fa"))
    .mtr_run(stringr::str_glue(
      "samtools consensus {fixed_cons} --show-ins no {user_cons} {bam} > {raw}"
    ), log_fn)

    filled <- .mtr_fill(.mtr_read_seq(raw), prev_ref)
    cons <- paste(
      .mtr_splice(strsplit(filled, "", fixed = TRUE)[[1]], len, flank),
      collapse = ""
    )
    writeLines(c(">cons", cons), file.path(work, paste0("cons_", k, ".fa")))

    reads_now <- .mtr_count_primary(bam)
    bases_changed <- .mtr_diff_count(cons, prev_cons)
    done <- .mtr_stop(bases_changed, reads_now, reads_prev) || k >= iter_cap
    if (done) {
      stop_reason <- if (k >= iter_cap &&
                         !.mtr_stop(bases_changed, reads_now, reads_prev)) {
        "cap"
      } else {
        "converged"
      }
    }
    iters <- rbind(iters, data.frame(
      pass = k,
      reads_mapped = reads_now,
      bases_changed = bases_changed,
      n_count = nchar(gsub("[^N]", "", cons)),
      stop_reason = if (done) stop_reason else NA_character_
    ))

    prev_cons <- cons
    prev_ref <- .mtr_extend(cons, flank)
    reads_prev <- reads_now
    if (done) {
      break
    }

    ref_fa <- file.path(work, paste0("ref_", k, ".fa"))
    writeLines(c(">mapping_ref", prev_ref), ref_fa)
    idx <- file.path(work, paste0("idx_", k))
    bam <- file.path(work, paste0("pass_", k + 1L, ".bam"))
    .mtr_run(stringr::str_glue("bowtie2-build -q {ref_fa} {idx}"), log_fn)
    .mtr_run(stringr::str_glue(
      "bowtie2 {bowtie2_opts} --no-unal -x {idx} -1 {sub_1} -2 {sub_2} ",
      "--threads {cpus} 2>> {log_fn} | samtools sort -@ {cpus} -o {bam} -"
    ), log_fn)
  }
  utils::write.table(iters, file.path(work, "iterations.tsv"),
                     sep = "\t", row.names = FALSE, quote = FALSE)

  # Final pass: all reads against the converged reference. Reads that only
  # become mappable after the reference has moved are exactly the ones the
  # loop exists to reach.
  final_ref <- file.path(work, "ref_final.fa")
  writeLines(c(">mapping_ref", prev_ref), final_ref)
  final_idx <- file.path(work, "idx_final")
  final_bam <- file.path(work, "final.bam")
  .mtr_run(stringr::str_glue("bowtie2-build -q {final_ref} {final_idx}"), log_fn)
  .mtr_run(stringr::str_glue(
    "bowtie2 {bowtie2_opts} --no-unal -x {final_idx} -1 {reads_1} -2 {reads_2} ",
    "--threads {cpus} 2>> {log_fn} | samtools sort -@ {cpus} -o {final_bam} -"
  ), log_fn)
  reads_final <- .mtr_count_primary(final_bam)
  junction_depth <- if (circular) .mtr_junction_depth(final_bam, len) else NA_integer_

  final_raw <- file.path(work, "final_raw.fa")
  final_subs <- file.path(work, "final_subs.fa")
  .mtr_run(stringr::str_glue(
    "samtools consensus {fixed_cons} --show-ins yes --mark-ins {user_cons} ",
    "{final_bam} > {final_raw}"
  ), log_fn)
  .mtr_run(stringr::str_glue(
    "samtools consensus {fixed_cons} --show-ins no {user_cons} ",
    "{final_bam} > {final_subs}"
  ), log_fn)

  tokens <- .mtr_splice(.mtr_parse_marked(.mtr_read_seq(final_raw)), len, flank)
  product <- .mtr_tokens_to_seq(tokens)
  seq <- product$seq
  if (!circular) {
    seq <- .mtr_strip_ends(seq)
  }

  subs <- paste(
    .mtr_splice(strsplit(.mtr_read_seq(final_subs), "", fixed = TRUE)[[1]],
                len, flank),
    collapse = ""
  )
  writeLines(c(paste0(">", id, ".1.1 subs_only"), subs),
             file.path(work, "subs_only.fasta"))

  published <- ref$topology
  if (circular && !is.na(junction_depth) && junction_depth == 0L) {
    published <- "linear"
    notes <- c(notes, paste0(
      "No reads span the start and end of the sequence, so this assembly is ",
      "published as linear even though the reference is circular. Add reads or ",
      "use a closer reference, or edit the topology if you are confident the ",
      "molecule is circular."))
  }

  n_count <- nchar(gsub("[^N]", "", seq))
  if (n_count > 0.02 * nchar(seq)) {
    notes <- c(notes, paste0(
      round(100 * n_count / nchar(seq), 1),
      "% of the reference could not be called (N)."))
  }
  if (identical(stop_reason, "cap")) {
    notes <- c(notes, paste0(
      "Still changing after ", passes, " passes; raise the cap (10 to 25) and ",
      "re-run."))
  }
  # Position-wise over the common length, with uncalled sites excluded, so an
  # internal N run cannot shift the comparison frame.
  a <- strsplit(subs, "", fixed = TRUE)[[1]]
  b <- strsplit(ref$seq, "", fixed = TRUE)[[1]]
  n <- min(length(a), length(b))
  a <- a[seq_len(n)]
  b <- b[seq_len(n)]
  keep <- !a %in% c("N", "*")
  subs_diff <- sum(a[keep] != b[keep])
  if (subs_diff > 0.10 * ref$length) {
    notes <- c(notes, paste0(
      "Reference is more than 10% divergent; expect reference bias and missing ",
      "regions. Use a closer reference, a more sensitive preset, or compare ",
      "with a de novo set."))
  }

  writeLines(c(paste0(">", id, ".1.1 ", published), seq),
             file.path(out_dir, paste0(id, "_assembly_1.fasta")))
  writeLines(c(
    "assembler=MapToRef",
    paste0("accession=", ref$accession),
    paste0("organism=", ref$organism),
    paste0("reference_length=", len),
    paste0("reference_topology=", ref$topology),
    paste0("published_topology=", published),
    paste0("transl_table=", ref$transl_table),
    paste0("passes_run=", passes),
    paste0("stop_reason=", stop_reason),
    paste0("reads_mapped_pass_1=", reads_pass_1),
    paste0("reads_mapped_final=", reads_final),
    paste0("junction_depth=", ifelse(is.na(junction_depth), "NA", junction_depth)),
    paste0("consensus_length=", nchar(seq)),
    paste0("n_count=", n_count),
    paste0("iupac_count=", nchar(gsub("[ACGTN]", "", seq))),
    paste0("half_deletions=", product$half_deletions),
    paste0("substitutions_vs_reference=", subs_diff),
    paste0("note=", notes)
  ), file.path(out_dir, paste0(id, "_summary.txt")))

  # Reproducible transients, dropped so the published loop record stays the
  # small file set of design 4.11. A failed run keeps everything.
  unlink(list.files(work, pattern = "\\.(bam|bai|bt2|bt2l|fq)$", full.names = TRUE))

  invisible(TRUE)
}
```

- [ ] **Step 4: Run the test to verify it passes**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-map-to-ref-loop.R")'`
Expected: all PASS.

- [ ] **Step 5: Run the whole suite and regenerate docs**

```bash
Rscript -e 'devtools::document()'
Rscript -e 'devtools::test()'
grep -nP '[^\x00-\x7F]' R/map_to_ref.R tests/testthat/test-map-to-ref-loop.R
```

Expected: FAIL 0, PASS at least 1807 plus the new tests; grep prints nothing.

- [ ] **Step 6: Commit**

```bash
git add R/map_to_ref.R tests/testthat/test-map-to-ref-loop.R NAMESPACE man/
git commit -m "feat: MapToRef iterate-to-consensus assembly driver"
```

---

### Task 4: Half-day spike on real data

Everything so far is verified against stubs and synthetic strings. The design rests on five claims about real reads that no unit test can check, plus one open question about duplicates. This task answers them before any schema or Nextflow work locks the shape in. Do not skip it: if the origin claim or the recruitment claim is wrong, the algorithm changes, not the plumbing.

Run inside the container (the tools are not on the host). Samples: three from the shipped test project, chosen for divergence spread. Reference: `inst/test_data/NC_002333_Danio_rerio.gb`.

**Files:**
- Create: `dev/map_to_ref_spike/NOTES.md` (results; `dev/` is gitignored, so nothing here is committed)

**Interfaces:**
- Consumes: `map_to_ref()` from Task 3.
- Produces: a go/no-go on the design, and a decision on the fastp deduplication toggle that Task 10 either ships or documents as a limit.

- [ ] **Step 1: Set up the spike directory**

```bash
mkdir -p dev/map_to_ref_spike
```

- [ ] **Step 2: Run three samples through `map_to_ref()`**

Inside the container, from the repo root:

```r
devtools::load_all()
ids <- c("SRR21843972", "SRR21844202", "SRR22396627")
for (id in ids) {
  map_to_ref(
    id,
    ref = "inst/test_data/NC_002333_Danio_rerio.gb",
    reads_1 = file.path("inst/test_data", paste0(id, "_R1.fastq.gz")),
    reads_2 = file.path("inst/test_data", paste0(id, "_R2.fastq.gz")),
    bowtie2_opts = "--very-sensitive-local",
    consensus_opts = "-d 3 --min-BQ 20",
    iter_cap = 5, topology = NA_character_, genetic_code = 2,
    cpus = 4, out_dir = file.path("dev/map_to_ref_spike", id)
  )
}
```

- [ ] **Step 3: Check the five claims, one command each**

Record each answer in `dev/map_to_ref_spike/NOTES.md`.

1. Origin after the splice. The first 50 bases of the published sequence must be real calls, not a run of N, and must align to the reference's first 50 bases:

```r
seq <- paste(readLines("dev/map_to_ref_spike/SRR21843972/SRR21843972_assembly_1.fasta")[-1], collapse = "")
substr(seq, 1, 50)
```

Expected: no N run at the head. A block of N here means the splice is wrong or `--no-use-MQ` is not taking effect.

2. Recruitment keeps half-mapped pairs. The recruited subset must have equal-length R1 and R2 files:

```bash
for f in dev/map_to_ref_spike/*/maptoref/sub_R?.fq; do echo "$f $(wc -l < "$f")"; done
```

Expected: R1 and R2 line counts match for each sample. Task 3 Step 3 deletes the
recruited FASTQ files at the end of a successful run, so comment out that
`unlink()` line while the spike is running.

3. N fill lets pass 2 map through. Compare `n_count` across passes in `iterations.tsv`:

```bash
cat dev/map_to_ref_spike/*/maptoref/iterations.tsv
```

Expected: `n_count` does not grow pass over pass.

4. The mark-ins parser round-trips. The number of tokens must equal the reference length plus flank:

```r
devtools::load_all()
raw <- .mtr_read_seq("dev/map_to_ref_spike/SRR21843972/maptoref/final_raw.fa")
length(.mtr_parse_marked(raw))   # must equal reference length + flank
```

5. The final all-reads pass recovers more than the subset:

```bash
grep -E "reads_mapped_final|reads_mapped_pass_1" dev/map_to_ref_spike/*/*_summary.txt
for f in dev/map_to_ref_spike/*/maptoref/iterations.tsv; do echo "$f"; tail -1 "$f"; done
```

Expected: `reads_mapped_final` from the summary exceeds the last iteration's `reads_mapped`, which is the second column of the last row of `iterations.tsv`. If it does not, the closing pass is not earning its runtime and the design needs revisiting.

- [ ] **Step 4: Answer the deduplication question**

Re-run one sample with fastp deduplication in place of the shipped flag, then re-run `map_to_ref()` on the deduplicated reads:

```bash
fastp -i inst/test_data/SRR21843972_R1.fastq.gz -I inst/test_data/SRR21843972_R2.fastq.gz \
  -o dev/map_to_ref_spike/dedup_R1.fastq.gz -O dev/map_to_ref_spike/dedup_R2.fastq.gz \
  --dedup --detect_adapter_for_pe --correction
```

Then run `map_to_ref()` on the deduplicated pair:

```r
devtools::load_all()
map_to_ref(
  "SRR21843972",
  ref = "inst/test_data/NC_002333_Danio_rerio.gb",
  reads_1 = "dev/map_to_ref_spike/dedup_R1.fastq.gz",
  reads_2 = "dev/map_to_ref_spike/dedup_R2.fastq.gz",
  bowtie2_opts = "--very-sensitive-local",
  consensus_opts = "-d 3 --min-BQ 20",
  iter_cap = 5, topology = NA_character_, genetic_code = 2,
  cpus = 4, out_dir = "dev/map_to_ref_spike/SRR21843972_dedup"
)
```

Compare against the non-deduplicated run: mean depth, `n_count`, `substitutions_vs_reference`, and whether the two published sequences differ. The two products can differ in length, so report both lengths and get the identity from an alignment, not from an element-wise diff:

```r
a <- paste(readLines("dev/map_to_ref_spike/SRR21843972/SRR21843972_assembly_1.fasta")[-1], collapse = "")
b <- paste(readLines("dev/map_to_ref_spike/SRR21843972_dedup/SRR21843972_assembly_1.fasta")[-1], collapse = "")
c(plain = nchar(a), dedup = nchar(b))
paf <- run_minimap2_paf(c(dedup = b), a, cigar = TRUE)
sum(paf$nmatch) / sum(paf$qend - paf$qstart)   # identity over the aligned length
```

Decision rule, written into NOTES.md: if deduplication changes calls, or removes more than a few percent of reads at the depths these samples run at, Task 10 adds a `dedup` toggle to `preprocess_opts`. If it changes nothing, no toggle ships and the limit stays documented in the vignette.

- [ ] **Step 5: Record the outcome**

Write `dev/map_to_ref_spike/NOTES.md` with one short section per claim: what was run, what came back, and pass or fail. If any claim fails, STOP and take it back to the maintainer before continuing; the remaining tasks assume all five hold.

- [ ] **Step 6: No commit**

`dev/` is gitignored. Nothing from this task is committed.

---

### Task 5: Five option columns on `assemble_opts`

The parameter set carries the reference and the three option strings. `assemble_opts` already holds `assembler`, `mitofinder_db`, and `mitofinder`, and this follows that pattern exactly. Old projects get the columns from `backwards_compatibility.R`; new ones from `init_db.R`.

Facts (verified 2026-09-03):
- The `assemble_opts` DDL is `R/init_db.R:311-329`, ending with `join_scaffolds INTEGER`; the seed row is `R/init_db.R:330-350`.
- The assembler validator is `R/init_db.R:130-133`: `if (assembler %nin% c("GetOrganelle", "MitoFinder")) stop(...)`.
- The `mitofinder` migration block is `R/backwards_compatibility.R:1295-1313`; copy its shape.
- The "already current" predicate is `R/backwards_compatibility.R:176-178`.
- The roxygen migration list is `R/backwards_compatibility.R:14-15`.
- `new_project()` forwards `...` to `new_db()`, so it needs no change.
- Every line number in this task is as of HEAD `112d178` and moves as soon as an earlier edit in the same file lands. Anchor each edit on the quoted code text, not on the line number.

**Files:**
- Modify: `R/init_db.R:45` (roxygen), `:70-83` (arguments), `:130-133` (validator), `:311-329` (DDL), `:330-350` (seed row)
- Modify: `R/backwards_compatibility.R:14-15`, `:176-178`, after `:1313`
- Modify: `tests/testthat/test-backwards-compatibility.R` (the `expect_cols` list around `:431`)
- Modify: `tests/testthat/test-map-to-ref.R` (append)

**Interfaces:**
- Consumes: nothing.
- Produces: `assemble_opts` gains TEXT `maptoref_ref`, TEXT `maptoref`, TEXT `maptoref_consensus`, INTEGER `maptoref_iter`, TEXT `maptoref_topology`, in that order, after `join_scaffolds`. `new_db()` and `new_project()` gain the matching arguments with defaults `NA_character_`, `"--very-sensitive-local"`, `"-d 3 --min-BQ 20"`, `5L`, `NA_character_`. Task 6 reads these column names in SQL; Task 8 writes them from the modal.

- [ ] **Step 1: Write the failing tests**

Append to `tests/testthat/test-map-to-ref.R`:

```r
mtr_test_db <- function(dir, ...) {
  mapping <- file.path(dir, "mapping.csv")
  utils::write.csv(
    data.frame(ID = "S1", Taxon = "Danio rerio",
               R1 = "S1_R1.fastq.gz", R2 = "S1_R2.fastq.gz"),
    mapping, row.names = FALSE
  )
  db <- file.path(dir, ".sqlite")
  new_db(db_path = db, mapping_fn = mapping, ...)
  db
}

test_that("new_db stores the five MapToRef option columns", {
  d <- withr::local_tempdir()
  db <- mtr_test_db(d, assembler = "MapToRef", maptoref_ref = "ref/NC_002333.gb",
                    maptoref_topology = "circular")
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  opts <- DBI::dbGetQuery(con, "SELECT * FROM assemble_opts")
  expect_true(all(c("maptoref_ref", "maptoref", "maptoref_consensus",
                    "maptoref_iter", "maptoref_topology") %in% names(opts)))
  expect_equal(opts$assembler, "MapToRef")
  expect_equal(opts$maptoref_ref, "ref/NC_002333.gb")
  expect_equal(opts$maptoref, "--very-sensitive-local")
  expect_equal(opts$maptoref_consensus, "-d 3 --min-BQ 20")
  expect_equal(opts$maptoref_iter, 5L)
  expect_equal(opts$maptoref_topology, "circular")
})

test_that("new_db refuses MapToRef without a reference and rejects a bad topology", {
  d <- withr::local_tempdir()
  expect_error(mtr_test_db(d, assembler = "MapToRef"), "maptoref_ref")
  expect_error(
    mtr_test_db(d, assembler = "MapToRef", maptoref_ref = "x.gb",
                maptoref_topology = "round"),
    "circular or linear"
  )
})

test_that("new_db still refuses an unknown assembler", {
  d <- withr::local_tempdir()
  expect_error(mtr_test_db(d, assembler = "Nonesuch"), "not supported")
})
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-map-to-ref.R")'`
Expected: FAIL, `unused arguments (maptoref_ref = ...)`.

- [ ] **Step 3: Add the arguments, the validator, the DDL, and the seed row**

In `R/init_db.R`, extend the roxygen at `:45`:

```r
#' @param assembler Assembler, choice of "GetOrganelle" (default), "MitoFinder",
#'   or "MapToRef"
#' @param maptoref_ref Path or URL of the MapToRef reference mitogenome. A
#'   single-record GenBank file (.gb) is preferred; a FASTA is accepted but then
#'   \code{maptoref_topology} must be set. Required when assembler = "MapToRef".
#' @param maptoref Default bowtie2 options for MapToRef
#' @param maptoref_consensus Default samtools consensus options for MapToRef
#' @param maptoref_iter Maximum MapToRef iteration passes (default = 5)
#' @param maptoref_topology Topology of a FASTA MapToRef reference, "circular"
#'   or "linear". Ignored for GenBank references, where the LOCUS line wins.
```

Add the arguments after `mitofinder` in the signature. `mitofinder = paste("--megahit")` spans `:81-83` and `max_paths` is `:84`, so the insertion point is after `:83`:

```r
    maptoref_ref = NA_character_,
    maptoref = "--very-sensitive-local",
    maptoref_consensus = "-d 3 --min-BQ 20",
    maptoref_iter = 5L,
    maptoref_topology = NA_character_,
```

Replace the validator at `:130-133`:

```r
  # Validate assembler choice
  if (assembler %nin% c("GetOrganelle", "MitoFinder", "MapToRef")) {
    stop("Assembler not supported, valid options: [GetOrganelle, MitoFinder, MapToRef]")
  }
  if (assembler == "MapToRef" && (is.na(maptoref_ref) || !nzchar(trimws(maptoref_ref)))) {
    stop("MapToRef requires a reference mitogenome; set maptoref_ref")
  }
  if (!is.na(maptoref_topology) &&
      maptoref_topology %nin% c("circular", "linear")) {
    stop("maptoref_topology must be circular or linear")
  }
```

Add to the DDL after `join_scaffolds INTEGER,` (`:326`):

```r
      maptoref_ref TEXT,
      maptoref TEXT,
      maptoref_consensus TEXT,
      maptoref_iter INTEGER,
      maptoref_topology TEXT,
```

Extend the seed row. `join_scaffolds = 0L` (`:345`) is the last argument of the `data.frame()` call and has no trailing comma today, so it gains one and the last new entry has none:

```r
        join_scaffolds = 0L,
        maptoref_ref = maptoref_ref,
        maptoref = maptoref,
        maptoref_consensus = maptoref_consensus,
        maptoref_iter = as.integer(maptoref_iter),
        maptoref_topology = maptoref_topology
```

- [ ] **Step 4: Add the migration blocks**

In `R/backwards_compatibility.R`, extend the roxygen list at `:14-15`:

```r
#'   \item \code{assemble_opts}: "assembler", "mitofinder_db"/"mitofinder",
#'     "max_paths", "max_scaffolds", "min_assembly_length", "join_scaffolds",
#'     and the MapToRef columns "maptoref_ref", "maptoref",
#'     "maptoref_consensus", "maptoref_iter", "maptoref_topology".
```

Add to the "already current" predicate at `:178`, after the `mitofinder` line:

```r
      "maptoref_ref" %in% names(assemble_opts_table) &&
      "maptoref" %in% names(assemble_opts_table) &&
      "maptoref_consensus" %in% names(assemble_opts_table) &&
      "maptoref_iter" %in% names(assemble_opts_table) &&
      "maptoref_topology" %in% names(assemble_opts_table) &&
```

After the `mitofinder` block (closing brace on `:1313`, immediately above the `# if max_paths column doesn't exist, add it` comment), add five blocks in this shape (repeated once per column, with the column name, SQL type, and backfill value swapped):

```r
  # if maptoref_ref column doesn't exist, add it
  if(!("maptoref_ref" %in% names(assemble_opts_table))){
    message("added 'maptoref_ref' column to assemble_opts table")
    assemble_opts_table$maptoref_ref <- rep(NA_character_, nrow(assemble_opts_table))
    glue::glue_sql(
      "ALTER TABLE assemble_opts
       ADD COLUMN maptoref_ref TEXT",
      .con = con
    ) |> DBI::dbExecute(con, statement = _)

    dplyr::tbl(con, "assemble_opts") |>
      dplyr::rows_upsert(
        assemble_opts_table,
        in_place = TRUE,
        copy = TRUE,
        by = "assemble_opts"
      )
  }
```

The other four backfills: `maptoref` gets `"--very-sensitive-local"` (TEXT), `maptoref_consensus` gets `"-d 3 --min-BQ 20"` (TEXT), `maptoref_iter` gets `5L` (INTEGER), `maptoref_topology` gets `NA_character_` (TEXT).

- [ ] **Step 5: Extend the migration test**

In `tests/testthat/test-backwards-compatibility.R`, add the five names to the `assemble_opts` entry of `expect_cols` (around `:431`).

- [ ] **Step 6: Run the tests**

```bash
Rscript -e 'devtools::document()'
Rscript -e 'devtools::test()'
```

Expected: FAIL 0. The migration test proves an old project gains all five columns.

- [ ] **Step 7: Commit**

```bash
git add R/init_db.R R/backwards_compatibility.R tests/testthat/test-map-to-ref.R tests/testthat/test-backwards-compatibility.R man/
git commit -m "feat: MapToRef option columns and migration"
```

---

### Task 6: Nextflow wiring

Three module files. The rule throughout: every new element is APPENDED to the end of its tuple, so no existing index moves and no other consumer changes.

Facts (verified 2026-09-03):
- `assemble_workflow.nf:6-20` selects 19 columns, `it[0]` to `it[18]`.
- The opts tuple built at `:99-117` has 8 elements: ID, options id, options map, getOrganelle dbs list, mitofinder db, genetic code, max_paths, max_scaffolds.
- The cross map at `:183-195` emits 9 elements.
- `assemble.nf:16` input tuple has 9 elements; the output tuple at `:19` has 10.
- `coverage.nf:35/:40` is the only assembler branch downstream of assemble.
- `NO_FILE` placeholder: pass `file("${projectDir}/assets/NO_FILE")` when the set is not MapToRef, the same way an unused optional path input is staged elsewhere in this pipeline.
- The eleven-argument `map_to_ref()` call in Step 4, passing `!{opts.cpus}`, is authoritative: the `assemble` process declares no `cpus` directive and both existing branches read `!{opts.cpus}`. Design 5.3's row has been updated to match it.

Deviation from the design, deliberate: design section 5.3 says the `assemble.nf` output tuple is unchanged. The loop record (`iterations.tsv`, `subs_only.fasta`, the per-pass consensus files) that section 4.11 promises the user is only published if it is a declared output, so an 11th element `path("${id}/assemble/${opts_id}/maptoref", optional: true)` is APPENDED. It is optional because the other two assemblers never create that directory, and appended so no existing index moves.

**Files:**
- Modify: `inst/nextflow/modules/assemble_workflow.nf:6-20`, `:99-117`, `:183-195`
- Modify: `inst/nextflow/modules/assemble.nf:16`, `:19`, `:101-102`
- Modify: `inst/nextflow/modules/coverage.nf:40`

**Interfaces:**
- Consumes: the five `assemble_opts` columns from Task 5, and `MitoPilot::map_to_ref()` from Task 3.
- Produces: a working `assembler = "MapToRef"` path through WF1, publishing the same file set the other two assemblers publish, plus the `maptoref/` loop record.

- [ ] **Step 1: Select the five columns**

In `assemble_workflow.nf`, append to the `params.sqlRead` select list, after `a.blast_accession` and before `'FROM assemble a '`:

```groovy
                  'a.join_switch, a.assemble_switch, a.blast_accession, ' +
                  'opts.maptoref_ref, opts.maptoref, opts.maptoref_consensus, ' +
                  'opts.maptoref_iter, opts.maptoref_topology ' +
```

They land at `it[19]` (ref), `it[20]` (bowtie2 options), `it[21]` (consensus options), `it[22]` (cap), `it[23]` (topology).

- [ ] **Step 2: Carry them into the opts tuple**

In the `multiMap` at `:99-117`, extend the options map. The existing `assembler: it[7]` line is its last entry today and carries no trailing comma, so it gains one:

```groovy
                        assembler: it[7],                                       // assembler
                        maptoref: it[20],                                       // MapToRef bowtie2 options
                        maptoref_consensus: it[21],                             // MapToRef samtools consensus options
                        maptoref_iter: (it[22] == null ? 5 : (it[22] as Integer)),
                        maptoref_topology: (it[23] ?: "")
```

Then append the reference file as the 9th element of the opts tuple, after `max_scaffolds`:

```groovy
                    (it[12] == null ? Integer.MAX_VALUE : (it[12] as Integer)),  // max_scaffolds
                    file((it[19] != null && it[19].toString().trim()) ? it[19] : "${projectDir}/assets/NO_FILE")  // MapToRef reference
```

- [ ] **Step 3: Pass it through the cross map**

In the `.map` at `:183-195`, append a tenth element after `it[1][7]`:

```groovy
                    it[1][7],                                                   // max_scaffolds
                    it[1][8]                                                    // MapToRef reference
```

- [ ] **Step 4: Take it in `assemble.nf` and add the branch**

`assemble.nf:16` input:

```groovy
    tuple val(id), val(opts_id), path(reads), val(opts), path(dbs), path(mf_db), val(genetic_code), val(max_paths), val(max_scaffolds), path(ref)
```

`assemble.nf:19` output, append the loop record:

```groovy
    tuple val("${id}"), path("${id}/assemble/${opts_id}/${id}_assembly_*.fasta"), path("${id}/assemble/${opts_id}/${id}_reads.tar.gz"), path("${id}/assemble/${opts_id}/${id}_summary.txt"), val("${opts_id}"), path("${id}/assemble/${opts_id}/assembler.log.txt"), path("${id}/assemble/${opts_id}/NF_work_dir_assemble.txt"), val("${opts.assembler}"), val(max_paths), val(max_scaffolds), path("${id}/assemble/${opts_id}/maptoref", optional: true)
```

Insert the third branch between `:101` and the closing `fi` at `:102`:

```bash
    elif [ "!{opts.assembler}" = "MapToRef" ]; then
        mkdir -p !{outDir}
        Rscript -e "MitoPilot::map_to_ref('!{id}', '!{ref}', '!{reads[0]}', '!{reads[1]}', '!{opts.maptoref}', '!{opts.maptoref_consensus}', !{opts.maptoref_iter}, '!{opts.maptoref_topology}', !{genetic_code.intValue()}, !{opts.cpus}, '!{outDir}')"
        echo "!{opts.maptoref} | !{opts.maptoref_consensus} | iterate !{opts.maptoref_iter}" > !{outDir}/opts.txt
        ### ARCHIVE READS ###
        tar -czvf !{outDir}/!{id}_reads.tar.gz *.fastq.gz
        ### work dir info for troubleshooting ####
        echo "Nextflow assemble working directory:" > !{outDir}/NF_work_dir_assemble.txt
        echo "$PWD" >> !{outDir}/NF_work_dir_assemble.txt
    fi
```

The trailing `fi` above is the existing line `:102` shown as context, not part of the insertion; do not paste a second one.

`map_to_ref()` writes the assembly FASTA (or the sentinel), the summary, and the log itself, so the branch only adds the reads archive, the options record, and the working-directory note. The argument order matches the signature from Task 3 exactly.

- [ ] **Step 5: Let coverage serve MapToRef**

`coverage.nf:40`, the MitoFinder branch, also takes MapToRef. The tarball layout and the read names are identical, so `MitoPilot::coverage()` runs unchanged:

```bash
    elif [ "!{assembler}" = "MitoFinder" ] || [ "!{assembler}" = "MapToRef" ]; then
```

- [ ] **Step 6: Lint**

```bash
NXF_VER=25.10.6 nextflow lint inst/nextflow/modules/assemble.nf inst/nextflow/modules/assemble_workflow.nf inst/nextflow/modules/coverage.nf
```

Expected: no errors. `NXF_VER` pins the newest Nextflow release this pipeline supports (`NF_MAX_SUPPORTED` in `R/nextflow_version.R`), because the host may have a newer one installed.

- [ ] **Step 7: Commit**

```bash
git add inst/nextflow/modules/assemble.nf inst/nextflow/modules/assemble_workflow.nf inst/nextflow/modules/coverage.nf
git commit -m "feat: run MapToRef from the assemble process"
```

---

### Task 7: samtools 1.24 and the release chores

Decision 8: bump samtools from 1.21 to 1.24 as part of this feature, and document it. Consensus gains real multi-threading at 1.22, so `-@ cpus` starts paying. `-T` (reference fill) is available in 1.24 and is deliberately NOT used, because filling the product from the reference is the reference bias this design refuses.

**Files:**
- Modify: `docker/Dockerfile:32`
- Modify: `docker/README.md`
- Modify: `DESCRIPTION:3`
- Modify: `NEWS.md`

**Interfaces:**
- Consumes: nothing.
- Produces: an image whose samtools supports threaded consensus. Nothing in `R/map_to_ref.R` requires a 1.24-only flag, so the code still runs on the 1.21 image, just single-threaded in the consensus step.

- [ ] **Step 1: Bump the pin**

`docker/Dockerfile:32`:

```dockerfile
    mamba install -c bioconda samtools=1.24 && \
```

- [ ] **Step 2: Bump the version and write the release notes**

This work ships in 1.5.5. Do this before the image build, so the tag the build produces matches `DESCRIPTION` (`docker/README.md:5` requires that).

`DESCRIPTION:3`: bump `Version:` from 1.5.4 to 1.5.5.

`docker/README.md`: the samtools pin text becomes 1.24, and any mention of the current image tag becomes 1.5.5.

`NEWS.md`: add a NEW top section above `# MitoPilot 1.5.4`, in the shape the file already uses. Do not append under 1.5.4:

```markdown
# MitoPilot 1.5.5

Released TBD. Container: `macguigand/mitopilot:1.5.5`

## New Features

### Map-to-reference assembly

- A third assembler, **MapToRef**, maps your reads to a reference mitogenome you supply and calls the consensus from the reads alone. The reference is used to place reads, never to fill in the answer.
- The consensus is fed back in as the mapping reference and re-mapped until it stops changing (default: up to 5 passes, editable), then every read is mapped once more against the settled reference to produce the published sequence.
- Sites with fewer than 3 reads are called N. Mixed sites get IUPAC codes.
- A circular reference is handled across its origin, and the published sequence is only labelled circular if reads actually span the junction; otherwise it is published as linear with a note saying so.
- References can be GenBank (preferred, one record) or FASTA. A FASTA reference needs its topology set explicitly.

### Container

- **samtools upgraded from 1.21 to 1.24**, which gives the consensus step real multi-threading.
```

- [ ] **Step 3: Build the image and prove the flags exist**

```bash
bash docker/deploy-local.sh 1.5.5
docker run --rm mitopilot:1.5.5 samtools consensus --help 2>&1 | grep -E "mark-ins|no-use-MQ|show-ins|min-BQ"
docker run --rm mitopilot:1.5.5 samtools --version | head -1
```

Expected: all four flags present, version 1.24. No separate `rm` of the stale package tarball is needed: `docker/deploy-local.sh:23` already runs `rm -f docker/MitoPilot_*.tar.gz` before it builds.

- [ ] **Step 4: Commit**

```bash
git add docker/Dockerfile docker/README.md DESCRIPTION NEWS.md
git commit -m "chore: samtools 1.24 and MapToRef release notes"
```

---

### Task 8: Assemble options modal

The modal is the only UI change. Five inputs, shown only when MapToRef is the selected assembler, plus one save-time check: a FASTA reference with no topology is refused, because there is no safe default (a wrong topology silently changes rotation and the `partial` flag).

Facts (verified 2026-09-03):
- The assembler picker is `R/app_assemble_utils.R:288-302`; its help sentence naming two tools is `:303-309`.
- The `mf_db` input at `:323-333` is the shape to copy, including `shinyjs::disabled()` and the nested `opts_help()` that lives INSIDE the input container so it hides with the input.
- The initial show/hide if-block is `R/app_assemble_utils.R:441-448`.
- The observers are `R/app_assemble.R:847-885` (populate and show/hide), `:889-900` (`toggleState`), `:944-958` (assembler change), `:960-986` (upsert).
- MapToRef deliberately gets no `register_tool_help()` "?" icon: there is no single wrapped CLI whose `--help` describes the feature. Each field links to the upstream manual through `opts_help()` instead.

**Files:**
- Modify: `R/app_assemble_utils.R:288-302`, `:303-309`, after `:365`, `:441-448`
- Modify: `R/app_assemble.R:847-885`, `:889-900`, `:944-958`, `:960-986`

**Interfaces:**
- Consumes: the five columns from Task 5.
- Produces: modal inputs `maptoref_ref`, `maptoref_topology`, `maptoref`, `maptoref_consensus`, `maptoref_iter`, written to `assemble_opts` by the existing upsert.

- [ ] **Step 1: Add MapToRef to the picker and its help line**

`R/app_assemble_utils.R:293`:

```r
              choices = c("GetOrganelle", "MitoFinder", "MapToRef"),
```

`:303-309`, the help sentence must name three tools:

```r
        opts_help("Tool used to assemble the mitogenome from reads: ",
                  tags$a(href = "https://github.com/Kinggerm/GetOrganelle",
                         target = "_blank", rel = "noopener", "GetOrganelle"),
                  ", ",
                  tags$a(href = "https://github.com/RemiAllio/MitoFinder",
                         target = "_blank", rel = "noopener", "MitoFinder"),
                  ", or MapToRef, which maps your reads to a reference ",
                  "mitogenome you supply; the relevant tool options appear below."),
```

- [ ] **Step 2: Add the five inputs**

After the `labels_db` input, in the shape of the `mf_db` input at `:323-333`. The `labels_db` input ends at `R/app_assemble_utils.R:365` with `nested = TRUE))` and NO trailing comma, so add a comma there and drop the trailing comma from the last pasted entry:

```r
        textInput(
          ns("maptoref_ref"),
          label = "MapToRef Reference (.gb or FASTA, one complete mitogenome):",
          value = current$maptoref_ref %||% character(0),
          width = "100%"
        ) |> shinyjs::disabled() |>
          tagAppendChild(opts_help(
            "Path or URL of one complete mitogenome to map against. A file ",
            "ending .gb, .gbk, or .gbff is read as GenBank and takes its ",
            "topology from the LOCUS line; anything else is read as FASTA and ",
            "needs the topology set below.",
            href = "https://smithsonian.github.io/MitoPilot/articles/custom_dbs.html",
            id = ns("help_maptoref_ref"), nested = TRUE)),
        selectInput(
          ns("maptoref_topology"),
          label = "Reference topology (required for a FASTA reference):",
          choices = c("", "circular", "linear"),
          selected = current$maptoref_topology %||% "",
          width = "100%"
        ) |> shinyjs::disabled() |>
          tagAppendChild(opts_help(
            "Ignored for a GenBank reference, where the LOCUS line wins. A ",
            "FASTA header carries no topology, so it must be set here.",
            id = ns("help_maptoref_topology"), nested = TRUE)),
        textInput(
          ns("maptoref"),
          label = "MapToRef bowtie2 options",
          value = current$maptoref %||% character(0),
          width = "100%"
        ) |> shinyjs::disabled() |>
          tagAppendChild(opts_help(
            "Flags passed to bowtie2. Presets: --fast-local, ",
            "--sensitive-local, --very-sensitive-local (default), ",
            "--very-sensitive-local -N 1, and, for a distant reference, ",
            "--very-sensitive-local -N 1 -L 15 --score-min G,10,6.",
            href = "https://bowtie-bio.sourceforge.net/bowtie2/manual.shtml",
            id = ns("help_maptoref"), nested = TRUE)),
        textInput(
          ns("maptoref_consensus"),
          label = "MapToRef samtools consensus options",
          value = current$maptoref_consensus %||% character(0),
          width = "100%"
        ) |> shinyjs::disabled() |>
          tagAppendChild(opts_help(
            "Flags passed to samtools consensus. -d sets the depth below which ",
            "a site is called N, --min-BQ the base-quality floor. Options ",
            "MitoPilot sets itself are refused.",
            href = "https://www.htslib.org/doc/samtools-consensus.html",
            id = ns("help_maptoref_consensus"), nested = TRUE)),
        numericInput(
          ns("maptoref_iter"),
          label = "Iterate up to",
          value = current$maptoref_iter %||% 5,
          min = 1, step = 1,
          width = "100%"
        ) |> shinyjs::disabled() |>
          tagAppendChild(opts_help(
            "Maximum passes of consensus-back-into-reference. The loop stops ",
            "early when the sequence stops changing. Try 10 to 25 for a ",
            "distant reference.",
            id = ns("help_maptoref_iter"), nested = TRUE))
```

- [ ] **Step 3: Show only the selected assembler's inputs**

`R/app_assemble_utils.R:441-448` becomes:

```r
    maptoref_ids <- c("maptoref_ref", "maptoref_topology", "maptoref",
                      "maptoref_consensus", "maptoref_iter")
    if(current$assembler == "GetOrganelle"){
      shinyjs::hide(id = "mitofinder")
      shinyjs::hide(id = "mf_db")
      for (i in maptoref_ids) shinyjs::hide(id = i)
    } else if(current$assembler == "MitoFinder"){
      shinyjs::hide(id = "getOrganelle")
      shinyjs::hide(id = "seeds_db")
      shinyjs::hide(id = "labels_db")
      for (i in maptoref_ids) shinyjs::hide(id = i)
    } else if(current$assembler == "MapToRef"){
      shinyjs::hide(id = "getOrganelle")
      shinyjs::hide(id = "seeds_db")
      shinyjs::hide(id = "labels_db")
      shinyjs::hide(id = "mitofinder")
      shinyjs::hide(id = "mf_db")
    }
```

- [ ] **Step 4: Populate, toggle, and show/hide in the server**

`R/app_assemble.R:847-870`, alongside the existing `updateTextAreaInput` / `updateSelectizeInput` calls. Match the local style: no explicit `session` argument. `maptoref_ref` and `maptoref_topology` are `NA` in every parameter set that predates this feature and `%||%` only replaces `NULL`, so both also go through `%|NA|%` (`R/utils.R:21`); otherwise the field renders the literal string "NA":

```r
        updateTextInput(
          inputId = "maptoref_ref",
          value = (cur$maptoref_ref %||% NA_character_) %|NA|% ""
        )
        updateSelectInput(
          inputId = "maptoref_topology",
          selected = (cur$maptoref_topology %||% NA_character_) %|NA|% ""
        )
        updateTextInput(inputId = "maptoref", value = cur$maptoref)
        updateTextInput(inputId = "maptoref_consensus", value = cur$maptoref_consensus)
        updateNumericInput(inputId = "maptoref_iter", value = cur$maptoref_iter)
```

`:873-885` and `:944-958`, both observers, gain a MapToRef branch that shows the five ids and hides the other two tools' ids, mirroring Step 3.

`:889-900`, five more lines:

```r
      for (i in c("maptoref_ref", "maptoref_topology", "maptoref",
                  "maptoref_consensus", "maptoref_iter")) {
        shinyjs::toggleState(i, condition = input$edit_assemble_opts)
      }
```

- [ ] **Step 5: Save with the topology check**

`R/app_assemble.R:960-986`. The block below goes INSIDE the existing `if (input$edit_assemble_opts) {` at `:962`, above the `dplyr::tbl(...) |> rows_upsert(...)` call. Do NOT paste an outer `if`; that guard is already there. Three refusals, all in the shape the file already uses for `show_alert`:

```r
        ref_value <- trimws(input$maptoref_ref %||% "")
        topology_value <- trimws(input$maptoref_topology %||% "")
        if (identical(input$assembler, "MapToRef") && !nzchar(ref_value)) {
          shinyWidgets::show_alert(
            title = "Reference required",
            text = paste("MapToRef needs a reference mitogenome. Give the path",
                         "or URL of a single-record GenBank or FASTA file."),
            type = "error",
            closeOnClickOutside = FALSE
          )
          return()
        }
        needs_topology <- identical(input$assembler, "MapToRef") &&
          nzchar(ref_value) &&
          !grepl("\\.(gb|gbk|gbff)$", ref_value, ignore.case = TRUE) &&
          !nzchar(topology_value)
        if (needs_topology) {
          shinyWidgets::show_alert(
            title = "Reference topology required",
            text = paste("Set the reference topology (circular or linear) for a",
                         "FASTA reference. A GenBank (.gb) reference takes its",
                         "topology from the file."),
            type = "error",
            closeOnClickOutside = FALSE
          )
          return()
        }
        if (grepl("['\"]", paste(input$maptoref %||% "",
                                 input$maptoref_consensus %||% ""))) {
          shinyWidgets::show_alert(
            title = "Quote characters not allowed",
            text = paste("The bowtie2 and samtools consensus option strings are",
                         "passed through a shell call, so they cannot contain a",
                         "single or double quote."),
            type = "error",
            closeOnClickOutside = FALSE
          )
          return()
        }
```

Then add the five columns to the upsert data.frame. Use `%||%` and defaults, NOT `req()`, so a GetOrganelle set with empty MapToRef fields still saves, and store an empty reference or topology as `NA_character_` rather than `""` or the string "NA":

```r
              maptoref_ref = if (nzchar(ref_value)) ref_value else NA_character_,
              maptoref = input$maptoref %||% "--very-sensitive-local",
              maptoref_consensus = input$maptoref_consensus %||% "-d 3 --min-BQ 20",
              maptoref_iter = as.integer(input$maptoref_iter %||% 5L),
              maptoref_topology = if (nzchar(topology_value)) topology_value else NA_character_
```

- [ ] **Step 6: Check it by hand**

Launch the test project, open Assemble options, tick Edit, and confirm: choosing MapToRef shows exactly the five fields and hides the GetOrganelle and MitoFinder fields; switching back hides them again; saving a FASTA reference without a topology raises the alert and does not write; saving with a topology writes all five values.

- [ ] **Step 7: Commit**

```bash
git add R/app_assemble_utils.R R/app_assemble.R
git commit -m "feat: MapToRef options in the assemble modal"
```

---

### Task 9: MapToRef notes in the Assemble table

`map_to_ref()` writes its warnings as `note=` lines in `<ID>_summary.txt`. This surfaces them in the Assemble table's notes column, where the user actually looks.

Deviation from the design, deliberate: design section 5.3 suggests a separate tagged-note write using a copy of `appendTaggedNoteSql`. Do NOT do that here. Paired `sqlInsert` operators commit in any order, and `params.sqlWriteAssemble` SETs `assemble_notes` wholesale for the same row in the same run, so a second racing write would sometimes be clobbered. Instead the notes are folded into the `notes` string that the existing classification map already builds, so there is exactly one write.

**Files:**
- Modify: `inst/nextflow/modules/assemble_workflow.nf` - the `branched.pass` `.map` closure, `:272-297` as of HEAD. Task 6 adds about eight lines above it, so those numbers shift; anchor on the closure text, not on the line number.

**Interfaces:**
- Consumes: `<ID>_summary.txt` from Task 3, reachable as `raw[3]` in the classification map (element 3 of the assemble output tuple).
- Produces: MapToRef warnings appearing in `assemble.assemble_notes`, written by the existing `params.sqlWriteAssemble` statement.

- [ ] **Step 1: Read the notes in the classification map**

In the `branched.pass` `.map` closure, after the existing `notes` assignments and before the `status == '4'` block:

```groovy
                // MapToRef writes its warnings as note= lines in the summary
                // file, tagged [maptoref] per spec 5.3. Folded into the same
                // notes string so there is only one write to assemble_notes.
                def summary = raw[3]
                if (summary && summary.exists()) {
                    def mtr = summary.readLines()
                        .findAll { it.startsWith('note=') && it.length() > 5 }
                        .collect { "[maptoref] " + it.substring(5).trim() }
                    if (mtr) {
                        def msg = mtr.join('; ')
                        notes = notes ? "${notes}; ${msg}" : msg
                    }
                }
```

- [ ] **Step 2: Verify against a real run (deferred: verified in Task 11 Step 4)**

Nothing here blocks Step 3. Task 11 Step 4 checks that a sample whose summary carries a `note=` line shows that text in the Assemble table's notes column, and that a sample with no notes shows nothing new.

- [ ] **Step 3: Commit**

```bash
git add inst/nextflow/modules/assemble_workflow.nf
git commit -m "feat: surface MapToRef warnings in assemble notes"
```

---

### Task 10: Documentation, and the deduplication toggle if the spike earned it

Two things: the user-facing docs for a third assembler, and the fastp deduplication toggle IF AND ONLY IF Task 4's spike showed it changes calls. If the spike said no, skip Steps 4 and 5 and say so in the vignette instead.

Facts (verified 2026-09-03):
- `README.md` is generated from `README.Rmd` (`README.md:2` says so), and nothing in this plan re-knits it, so both files are edited by hand with matching text. The two places that name the assembler set as two tools are `README.Rmd:33` and `:139`, and `README.md:29-32` and `:165-170`.
- `vignettes/Difficult-Assemblies.Rmd:45` says multiple paths come only from GetOrganelle and MitoFinder always returns a single path; MapToRef is a second single-path assembler and must be named there.
- The shipped fastp string sets `--dont_eval_duplication` (`R/init_db.R:248`).

**Files:**
- Modify: `README.Rmd`, `README.md`, `vignettes/Test-Project-Assemble.Rmd:142-176`, `vignettes/Difficult-Assemblies.Rmd:16-23` and `:45`, `vignettes/Your-Own-Project.Rmd:188-192`, `vignettes/custom_dbs.Rmd`
- Conditionally modify: `R/init_db.R` (a `dedup` column on `preprocess_opts`), `R/backwards_compatibility.R`, `R/app_preprocess.R` and its utils, `inst/nextflow/modules/preprocess*.nf`

**Interfaces:**
- Consumes: everything above, plus Task 4's dedup decision.
- Produces: docs that name three assemblers consistently, and (conditionally) `preprocess_opts.dedup`.

- [ ] **Step 1: README**

Both places that name the assembler set gain MapToRef: "GetOrganelle (default), MitoFinder, or MapToRef for mitogenome assembly", and "Assembly references for GetOrganelle, MitoFinder, or MapToRef". Make the edit twice, with matching wording: `README.Rmd:33` and `:139`, then `README.md:29-32` and `:165-170`. `README.md` is generated from `README.Rmd`, so editing only one leaves them out of sync.

- [ ] **Step 2: Test-Project-Assemble.Rmd**

In the Assemble options list (`:142-176`) add:

```markdown
- **Assembler**: `GetOrganelle` (default), `MitoFinder`, or `MapToRef`. MapToRef maps your reads to a reference mitogenome you supply and calls the consensus from the reads; the reference is used to place reads, never to fill in the answer.
- **MapToRef Reference**: one complete mitogenome, GenBank (`.gb`) preferred, one record per file. Keep it inside the project folder so the container can see it. A FASTA reference is accepted, but you must also set the topology.
- **Reference topology**: `circular` or `linear`, required for a FASTA reference and ignored for GenBank.
- **Iterate up to**: how many times the consensus is fed back in as the mapping reference. Default 5; the loop stops early when the sequence stops changing. Try 10 to 25 when the reference is from a distant relative.

If your reference is circular but no reads span the point where the sequence wraps around, the assembly is published as linear and a note says so. That is the data disagreeing with the reference, not an error.

MapToRef does not remove duplicate reads. Mitochondrial depth in a genome skim is usually the limiting resource, so duplicates are kept.
```

If Task 4 shipped the toggle, replace that last paragraph with a pointer to the new Preprocess option instead.

- [ ] **Step 3: Difficult-Assemblies.Rmd**

At `:16-23`, add: "MapToRef, like MitoFinder, returns one path. N runs mark regions the reads did not cover. Mapping cannot see rearrangements, so cross-check a MapToRef result against a de novo assembly before trusting gene order."

At `:45`, correct the single-path claim: "Multiple paths come only from GetOrganelle; MitoFinder and MapToRef always return a single path."

- [ ] **Step 4: Your-Own-Project.Rmd and custom_dbs.Rmd**

`Your-Own-Project.Rmd:188-192`:

```r
new_project(
  assembler = "MapToRef",
  maptoref_ref = "ref/NC_002333.gb"
)

# A FASTA reference needs its topology stated:
new_project(
  assembler = "MapToRef",
  maptoref_ref = "ref/mito.fasta",
  maptoref_topology = "circular"
)
```

`custom_dbs.Rmd`: a short section on fetching one complete mitogenome from NCBI as a single-record `.gb`, and why the MitoFinder sampler database (many records) is not a MapToRef reference. Add the Singularity note: keep the reference under the project folder or bind its directory, because a reference outside the project tree may be an unreadable symlink target (`prepare_ref_db.nf:15-19` explains the trap).

- [ ] **Step 5: The dedup toggle, only if the spike earned it**

If and only if Task 4's NOTES.md says deduplication changed calls: add a `dedup` INTEGER column to `preprocess_opts` (default 0), a migration block in the same shape as Task 5's, a checkbox in the Preprocess options modal, and a swap in the preprocess module that replaces `--dont_eval_duplication` with `--dedup` when it is on. Add a test asserting the default is off and that turning it on changes the stored fastp string. Reuse all five of Task 5's touch points, not two: the DDL, the seed row, the `new_db()` argument, the migration block, and the "already current" predicate, plus the roxygen migration list, the `expect_cols` entry in `tests/testthat/test-backwards-compatibility.R`, and a `NEWS.md` line. Commit it separately as `feat: fastp deduplication toggle`. Do NOT put this option on MapToRef: it belongs upstream, where it serves every assembler.

- [ ] **Step 6: Build the docs and check ASCII**

```bash
Rscript -e 'devtools::document()'
git diff -U0 -- README.Rmd README.md NEWS.md vignettes/ | grep -nP '^\+.*[^\x00-\x7F]'
```

Expected: prints nothing. These files already contain non-ASCII characters, so only the added lines are checked.

- [ ] **Step 7: Commit**

```bash
git add README.Rmd README.md vignettes/ man/
git commit -m "docs: MapToRef assembler"
```

---

### Task 11: End-to-end verification

Nothing above proves the branch runs inside the real pipeline. This does, on the shipped test project, in the container.

**Files:** none modified. This task produces evidence, not code.

**Interfaces:**
- Consumes: every task above.
- Produces: a verified run, or a defect list.

- [ ] **Step 1: Create a MapToRef parameter set**

In a fresh test project, add a parameter set named `maptoref` with `assembler = "MapToRef"` and `maptoref_ref` pointing at a copy of `NC_002333_Danio_rerio.gb` inside the project folder. Assign it to the two cyprinid samples plus one divergent sample.

- [ ] **Step 2: Run WF1 and check the published files**

```bash
ls out/<ID>/assemble/maptoref/
```

Expected: `<ID>_assembly_1.fasta`, `<ID>_reads.tar.gz`, `<ID>_summary.txt`, `assembler.log.txt`, `NF_work_dir_assemble.txt`, `maptoref/`, and the coverage outputs (`*.bam`, `*_coverage.csv`, `*_coverageStats.csv`, the per-scaffold PDF). `opts.txt` is written by the branch but is not a declared output, so it never reaches the publish directory; the other two assemblers have the same gap today.

Inside `maptoref/`, expect only what survives Task 3's end-of-run cleanup: `ref.fasta`, `reference.gb` or `reference.fasta`, the per-pass `cons_*.fa` and `ref_*.fa`, `final_raw.fa`, `final_subs.fa`, `subs_only.fasta`, and `iterations.tsv`. No BAMs, no bowtie2 index files, no recruited FASTQ.

```bash
head -1 out/<ID>/assemble/maptoref/<ID>_assembly_1.fasta
```

Expected: `>ID.1.1 circular` or `>ID.1.1 linear`, exactly one path, one scaffold.

- [ ] **Step 3: Check the result against a de novo assembly**

For a cyprinid sample that already has a GetOrganelle assembly, the two should agree to better than 99% identity, with the differences confined to N and IUPAC sites. `run_minimap2_paf()` takes sequences, not file paths, so read both FASTAs in first:

```r
devtools::load_all()
q <- c(maptoref = paste(readLines(maptoref_fasta)[-1], collapse = ""))
r <- paste(readLines(getorganelle_fasta)[-1], collapse = "")
paf <- run_minimap2_paf(q, r, cigar = TRUE)
sum(paf$nmatch) / sum(paf$qend - paf$qstart)   # identity over the aligned length
```

Expected: better than 0.99.

- [ ] **Step 4: Check the warning paths**

Three checks:

- The divergent sample's summary carries the expected `note=` lines.
- A sample whose summary carries a `note=` line shows `[maptoref] ...` in the Assemble table's notes column, and a sample with no notes shows nothing new (Task 9 Step 2 defers to this).
- On the Scyphozoa project, a linear reference runs and the published sequence has no leading or trailing N run.

- [ ] **Step 5: Run the full test suite one last time**

```bash
Rscript -e 'devtools::test()'
```

Expected: FAIL 0, and PASS at least 1807 plus everything this plan added.

- [ ] **Step 6: Report to the maintainer**

Summarise: which samples ran, the identity numbers from Step 3, any warnings that fired, and anything the spike flagged. Ask before merging. Never push.

---

## Self-Review

Spec coverage, section by section:

- 4.5 reference handling: Task 1 (all six steps, including the CRLF strip, the one-record rule, the IUPAC check, the length bounds, and decision 7's mandatory FASTA topology).
- 4.6 per-sample algorithm, steps 1 to 10: Task 3, with the pure pieces of steps 5 and 9 in Task 2 and the topology downgrade of step 10 in Task 3.
- 4.7 option mapping and consensus-string validation: Task 2 (`.mtr_check_consensus_opts`), surfaced in the modal help in Task 8.
- 4.8 consensus rules: Task 2 and Task 3; the deduplication rule is Task 4's spike and Task 10's conditional toggle.
- 4.9 circular handling and the topology downgrade: Task 3, tested in Task 3 Step 1.
- 4.10 reads: Task 3 (pass 1 all reads, iteration passes on the recruited subset, final pass all reads).
- 4.11 outputs and warnings: Task 3 writes them; Task 6 publishes the `maptoref/` directory; Task 9 surfaces the notes.
- 5.1 R package: Tasks 1, 2, 3, 5, 8.
- 5.2 schema: Task 5.
- 5.3 Nextflow: Task 6, with the two stated deviations (an appended optional output; notes folded into the existing write rather than a second racing write).
- 5.4 container and release chores: Task 7.
- 5.5 app UI: Task 8.
- 5.6 export, annotation, curation: no change needed, verified by Task 11's run.
- 5.7 docs: Tasks 7 and 10.
- 5.8 tests: Tasks 1, 2, 3, 5, plus the manual end-to-end in Task 11.
- 6 defaults: encoded in Task 3's argument defaults and Task 5's column defaults.
- 7 risks: mitigations are in the code (Tasks 1 to 3) and the docs (Task 10).
- 9 phase 1: Tasks 1 to 7 plus the spike in Task 4. Phase 2: Tasks 8 to 10. Phase 3: not planned, by design.

Type consistency: `maptoref_prepare_ref()` returns the same seven fields everywhere it is named; `.mtr_splice()` takes and returns a character VECTOR in both its callers (single characters in the iteration loop, tokens in the final call); `map_to_ref()`'s argument order is identical in Task 3's definition, Task 3's tests, Task 4's spike, and Task 6's `assemble.nf` call.

Both deviations stated in Task 6, the appended optional output element on `assemble.nf` and the MapToRef notes folded into the existing assemble write, were accepted on 2026-09-03 and are recorded in the branch's SDD ledger.
