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
    "  index)",
    "    :",
    "    ;;",
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
    "      *)",
    "        f=\"$MTR_STUB_CONS\"",
    "        if [ -n \"$MTR_STUB_CONS_DIR\" ]; then",
    "          n=1",
    "          if [ -f \"$MTR_STUB_CONS_DIR/n\" ]; then n=$(cat \"$MTR_STUB_CONS_DIR/n\"); fi",
    "          echo $((n + 1)) > \"$MTR_STUB_CONS_DIR/n\"",
    "          if [ -f \"$MTR_STUB_CONS_DIR/cons_$n.fa\" ]; then",
    "            f=\"$MTR_STUB_CONS_DIR/cons_$n.fa\"",
    "          fi",
    "        fi",
    "        cat \"$f\"",
    "        ;;",
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

mtr_setup <- function(dir, junction = TRUE, vary = FALSE) {
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

  # A per-call series, each differing from the last by far more than the
  # convergence threshold, so the loop keeps iterating until the cap.
  vdir <- NA_character_
  if (vary) {
    vdir <- file.path(dir, "cons_var")
    dir.create(vdir, showWarnings = FALSE)
    for (i in 1:6) {
      v <- construct
      substr(v, 300L + 500L * i, 319L + 500L * i) <- strrep("A", 20L)
      mtr_write_wrapped(file.path(vdir, paste0("cons_", i, ".fa")), v)
    }
  }

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
    MTR_STUB_CONS_DIR = vdir,
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

  # A successful run drops the transients and keeps the loop record.
  expect_false(file.exists(file.path(out, "maptoref", "pass_1.bam")))
  expect_false(file.exists(file.path(out, "maptoref", "sub_R1.fq")))
  expect_false(file.exists(file.path(out, "maptoref", "final.bam")))

  expect_true(any(grepl("bowtie2-build",
                        readLines(file.path(out, "assembler.log.txt")))))
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

test_that("a consensus that keeps moving runs to the cap", {
  skip_on_os("windows")
  d <- withr::local_tempdir()
  s <- mtr_setup(d, vary = TRUE)
  out <- file.path(d, "out")

  ok <- map_to_ref("T1", s$ref, s$r1, s$r2, "--very-sensitive-local",
                   "-d 3 --min-BQ 20", 2, "circular", 2, 1, out)

  expect_true(ok)
  iters <- read.delim(file.path(out, "maptoref", "iterations.tsv"))
  expect_equal(nrow(iters), 2L)
  expect_true(all(iters$bases_changed >= 5L))
  expect_equal(iters$stop_reason[2], "cap")

  sm <- mtr_summary(out)
  expect_equal(sm[["passes_run"]], "2")
  expect_equal(sm[["stop_reason"]], "cap")
  expect_true(any(grepl("Still changing after 2 passes",
                        readLines(file.path(out, "T1_summary.txt")))))

  # Written only by the re-map block at the end of a non-final pass.
  expect_true(file.exists(file.path(out, "maptoref", "ref_1.fa")))
  expect_true(file.exists(file.path(out, "maptoref", "cons_2.fa")))
})

test_that("map_to_ref works when out_dir contains a space", {
  skip_on_os("windows")
  d <- withr::local_tempdir()
  s <- mtr_setup(d)
  out <- file.path(d, "out dir")

  ok <- map_to_ref("T1", s$ref, s$r1, s$r2, "--very-sensitive-local",
                   "-d 3 --min-BQ 20", 5, "circular", 2, 1, out)

  expect_true(ok)
  fa <- readLines(file.path(out, "T1_assembly_1.fasta"))
  expect_equal(fa[1], ">T1.1.1 circular")
  expect_equal(nchar(paste(fa[-1], collapse = "")), s$len)
})

test_that("an absent bowtie2 option string is an empty option list", {
  skip_on_os("windows")
  d <- withr::local_tempdir()
  s <- mtr_setup(d)

  for (opts in list(NULL, NA_character_, character(0))) {
    out <- withr::local_tempdir()
    expect_true(map_to_ref("T1", s$ref, s$r1, s$r2, opts,
                           "-d 3 --min-BQ 20", 5, "circular", 2, 1, out))
    expect_true(file.exists(file.path(out, "T1_assembly_1.fasta")))
    expect_false(any(grepl("-x NA",
                           readLines(file.path(out, "assembler.log.txt")),
                           fixed = TRUE)))
  }
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

test_that("a failure inside the loop keeps the transients for debugging", {
  skip_on_os("windows")
  d <- withr::local_tempdir()
  s <- mtr_setup(d)
  out <- file.path(d, "out")
  short <- file.path(d, "short.fa")
  mtr_write_wrapped(short, paste(rep("A", 100L), collapse = ""))
  withr::local_envvar(c(MTR_STUB_CONS = short))

  ok <- map_to_ref("T1", s$ref, s$r1, s$r2, "--very-sensitive-local",
                   "-d 3 --min-BQ 20", 5, "circular", 2, 1, out)

  expect_false(ok)
  expect_true(file.exists(file.path(out, "T1_assembly_0.fasta")))
  expect_true(file.exists(file.path(out, "maptoref", "pass_1.bam")))
  expect_true(file.exists(file.path(out, "maptoref", "sub_R1.fq")))
})
