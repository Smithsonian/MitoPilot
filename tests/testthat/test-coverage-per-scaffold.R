# Topology is per scaffold, not per sample. Before this, a genuinely circular
# scaffold in a multi-scaffold assembly had its coverage computed as linear:
# depth fell off at the seam and nothing warned.

test_that(".coverage_circular_ids reads topology per record", {
  expect_equal(
    .coverage_circular_ids(c("s.1.1 circular", "s.1.2 linear")),
    "s.1.1"
  )
  expect_equal(
    .coverage_circular_ids(c("s.1.1 circular", "s.1.2 circular")),
    c("s.1.1", "s.1.2")
  )
  expect_equal(
    .coverage_circular_ids(c("s.1.1 linear", "s.1.2 linear")),
    character(0)
  )
  expect_equal(.coverage_circular_ids("s.1.1 circular"), "s.1.1")
  expect_equal(.coverage_circular_ids("s.1.1 linear"), character(0))
})

test_that(".coverage_extend_circular extends only the circular scaffolds", {
  asmb <- Biostrings::DNAStringSet(c(
    "s.1.1" = paste(rep("ACGT", 200), collapse = ""),
    "s.1.2" = paste(rep("ACGT", 200), collapse = "")
  ))
  out <- .coverage_extend_circular(asmb, "s.1.1")
  expect_equal(Biostrings::width(out)[1], 800 + 500)
  expect_equal(Biostrings::width(out)[2], 800)
  expect_equal(names(out), names(asmb))
  expect_equal(
    as.character(Biostrings::subseq(out[["s.1.1"]], 801, 1300)),
    as.character(Biostrings::subseq(asmb[["s.1.1"]], 1, 500))
  )
  # no circular scaffolds: untouched
  expect_equal(
    Biostrings::width(.coverage_extend_circular(asmb, character(0))),
    c(800L, 800L)
  )
  # a fragment shorter than the flank appends only what it has
  short <- Biostrings::DNAStringSet(c("s.1.1" = paste(rep("A", 100), collapse = "")))
  expect_equal(Biostrings::width(.coverage_extend_circular(short, "s.1.1"))[[1]], 200)
})

# Helper: uniform depth over the mapping reference, so any depth above the
# baseline at position 1 can only have come from the folded junction construct.
mock_cov <- function(id, len, mapped_len) {
  data.frame(
    SeqId = id,
    Position = seq_len(mapped_len),
    Call = rep("A", mapped_len),
    Depth = 10,
    Correct = 10,
    ErrorRate = 0
  )
}

test_that(".coverage_reform_circular folds seam depth back per scaffold", {
  cov <- rbind(
    mock_cov("s.1.1", 1000, 1500), # circular, mapped to a 500 bp junction construct
    mock_cov("s.1.2", 1000, 1000)  # linear
  )
  lens <- c("s.1.1" = 1000, "s.1.2" = 1000)
  out <- .coverage_reform_circular(cov, lens, "s.1.1")

  circ <- out[out$SeqId == "s.1.1", ]
  lin <- out[out$SeqId == "s.1.2", ]

  # The circular scaffold's first 500 bases carry the seam reads as well
  expect_equal(max(circ$Position), 1000)
  expect_equal(circ$Depth[circ$Position == 1], 20)
  expect_equal(circ$Depth[circ$Position == 500], 20)
  expect_equal(circ$Depth[circ$Position == 501], 10)

  # The linear scaffold is untouched
  expect_equal(max(lin$Position), 1000)
  expect_true(all(lin$Depth == 10))
})

test_that(".coverage_reform_circular is a no-op with no circular scaffolds", {
  cov <- mock_cov("s.1.1", 1000, 1000)
  expect_equal(.coverage_reform_circular(cov, c("s.1.1" = 1000), character(0)), cov)
})

# ---- End to end through coverage(), with stand-in aligners -----------------
# bowtie2/bam-readcount are not available here, so they are replaced with stubs
# that report uniform depth 10 over the mapping reference with a 50 bp ramp-down
# at each end (what real mapping does). Depth near the seam is then the value
# that tells a circular scaffold from a linear one.

fake_aligner_bin <- function(dir) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  writeLines(c("#!/bin/bash", 'touch "${2}.1.bt2"', "exit 0"), file.path(dir, "bowtie2-build"))
  writeLines(c("#!/bin/bash", "exit 0"), file.path(dir, "bowtie2"))
  writeLines(c("#!/bin/bash", "cat > /dev/null", "exit 0"), file.path(dir, "samtools"))
  writeLines(c(
    "#!/bin/bash",
    'fa=""; prev=""',
    'for a in "$@"; do if [ "$prev" == "-f" ]; then fa="$a"; fi; prev="$a"; done',
    "awk '",
    "function emit(  i,b,d,cA,cC,cG,cT,L) {",
    "  L = length(seq)",
    "  for (i = 1; i <= L; i++) {",
    '    b = toupper(substr(seq, i, 1))',
    "    if (i <= 50) d = 1 + int((i-1)*9/50)",
    "    else if (i > L-50) d = 1 + int((L-i)*9/50)",
    "    else d = 10",
    '    cA = (b=="A") ? d : 0; cC = (b=="C") ? d : 0',
    '    cG = (b=="G") ? d : 0; cT = (b=="T") ? d : 0',
    '    printf "%s\\t%d\\t%s\\t%d\\t=:0:0\\tA:%d:0\\tC:%d:0\\tG:%d:0\\tT:%d:0\\tN:0:0\\n", id, i, b, d, cA, cC, cG, cT',
    "  }",
    "}",
    '/^>/ { if (id != "") emit(); id = substr($1,2); seq = ""; next }',
    "{ seq = seq $0 }",
    'END { if (id != "") emit() }',
    "' \"$fa\""
  ), file.path(dir, "conda"))
  Sys.chmod(list.files(dir, full.names = TRUE), "0755")
  dir
}

run_coverage <- function(headers, lens, dir) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  # bowtie2-build drops its index in the working directory
  withr::local_dir(dir)
  set.seed(1)
  seqs <- vapply(lens, function(n) {
    paste(sample(c("A", "C", "G", "T"), n, replace = TRUE), collapse = "")
  }, character(1))
  ss <- Biostrings::DNAStringSet(seqs)
  names(ss) <- headers
  fa <- file.path(dir, "s_assembly_1.fasta")
  Biostrings::writeXStringSet(ss, fa)
  reads <- file.path(dir, c("r1.fq", "r2.fq"))
  file.create(reads)
  suppressWarnings(coverage(fa, reads[1], reads[2], "NA", 1, dir))
  utils::read.csv(file.path(dir, "s_assembly_1_coverage.csv"))
}

test_that("coverage() computes seam depth per scaffold", {
  skip_on_os("windows")
  bin <- fake_aligner_bin(file.path(tempdir(), "fake_aligners"))
  old_path <- Sys.getenv("PATH")
  Sys.setenv(PATH = paste(bin, old_path, sep = .Platform$path.sep))
  on.exit(Sys.setenv(PATH = old_path), add = TRUE)

  root <- withr::local_tempdir()

  mixed <- run_coverage(
    c("s.1.1 circular", "s.1.2 linear"), c(3000, 2500), file.path(root, "mixed")
  )
  circ <- mixed[mixed$SeqId == "s.1.1", ]
  lin <- mixed[mixed$SeqId == "s.1.2", ]

  # The circular scaffold's seam is covered from both sides
  expect_equal(max(circ$Position), 3000)
  expect_gte(circ$Depth[circ$Position == 1], 10)
  expect_gte(circ$Depth[circ$Position == 3000], 10)
  # The linear scaffold still falls off at its ends
  expect_equal(max(lin$Position), 2500)
  expect_lte(lin$Depth[lin$Position == 1], 2)
  expect_lte(lin$Depth[lin$Position == 2500], 2)

  # Single-scaffold cases behave as they always have
  one_circ <- run_coverage("s.1.1 circular", 3000, file.path(root, "one_circ"))
  expect_equal(max(one_circ$Position), 3000)
  expect_gte(one_circ$Depth[one_circ$Position == 1], 10)

  one_lin <- run_coverage("s.1.1 linear", 3000, file.path(root, "one_lin"))
  expect_equal(max(one_lin$Position), 3000)
  expect_lte(one_lin$Depth[one_lin$Position == 1], 2)

  # A single-scaffold circular sample and the circular scaffold of the mixed
  # sample see the same seam depth
  expect_equal(circ$Depth[circ$Position == 1], one_circ$Depth[one_circ$Position == 1])
})

# ---- The topology stamping awk, read from the shipped module ---------------

nf_topology_awk <- function(id) {
  nf <- readLines(
    system.file("nextflow/modules/coverage_userAsmb.nf", package = "MitoPilot"),
    warn = FALSE
  )
  starts <- grep("^\\s*awk -v mapf=topology_map.txt '", nf)
  expect_length(starts, 2)
  ends <- grep("\\{print\\}' topology_map.txt !\\{assembly\\}", nf)
  expect_length(ends, 2)
  progs <- vapply(seq_along(starts), function(i) {
    block <- paste(nf[starts[i]:ends[i]], collapse = "\n")
    block <- sub("^\\s*awk -v mapf=topology_map.txt '", "", block)
    sub("' topology_map.txt.*$", "", block)
  }, character(1))
  expect_equal(progs[1], progs[2])
  gsub("!\\{id\\}", id, progs[1])
}

# Runs the shipped awk the way the process does: from the task directory, with
# the map named topology_map.txt, so FILENAME matches.
stamp <- function(map_lines, contigs, id = "SAMP") {
  d <- withr::local_tempdir()
  withr::local_dir(d)
  writeLines(map_lines, "topology_map.txt")
  writeLines(as.vector(rbind(paste0(">", contigs), "ACGT")), "in.fasta")
  out <- system2(
    "awk",
    c("-v", "mapf=topology_map.txt", shQuote(nf_topology_awk(id)),
      "topology_map.txt", "in.fasta"),
    stdout = TRUE
  )
  out
}

headers_of <- function(x) x[startsWith(x, ">")]

test_that("the coverage awk stamps each record with its own topology", {
  skip_on_os("windows")

  # mixed
  expect_equal(
    headers_of(stamp(c("ctgA circular", "ctgB linear"), c("ctgA", "ctgB"))),
    c(">SAMP.1.1 circular", ">SAMP.1.2 linear")
  )
  # all circular
  expect_equal(
    headers_of(stamp(c("ctgA circular", "ctgB circular"), c("ctgA", "ctgB"))),
    c(">SAMP.1.1 circular", ">SAMP.1.2 circular")
  )
  # all linear
  expect_equal(
    headers_of(stamp(c("ctgA linear", "ctgB linear"), c("ctgA", "ctgB"))),
    c(">SAMP.1.1 linear", ">SAMP.1.2 linear")
  )
  # single contig
  expect_equal(headers_of(stamp("ctgA circular", "ctgA")), ">SAMP.1.1 circular")

  # lookup uses the incoming contig name, not the new one, and ignores the
  # FASTA description
  expect_equal(
    headers_of(stamp(c("ctgA circular", "ctgB linear"), c("ctgA some description", "ctgB"))),
    c(">SAMP.1.1 circular", ">SAMP.1.2 linear")
  )

  # the skip branch's default map applies to every record
  expect_equal(
    headers_of(stamp("* circular", c("ctgA", "ctgB"))),
    c(">SAMP.1.1 circular", ">SAMP.1.2 circular")
  )
  expect_equal(
    headers_of(stamp("* linear", c("ctgA", "ctgB"))),
    c(">SAMP.1.1 linear", ">SAMP.1.2 linear")
  )

  # an unlisted contig with no default falls back to linear
  expect_equal(
    headers_of(stamp("ctgA circular", c("ctgA", "ctgZ"))),
    c(">SAMP.1.1 circular", ">SAMP.1.2 linear")
  )
})

test_that("an empty topology map keeps every record, headers intact", {
  skip_on_os("windows")

  # The NR==FNR idiom would treat the assembly as the map when the map has no
  # lines, swallowing the first header and dropping the record from the output.
  out <- stamp(character(0), c("ctgA", "ctgB", "ctgC"))
  expect_equal(
    out,
    c(">SAMP.1.1 linear", "ACGT", ">SAMP.1.2 linear", "ACGT", ">SAMP.1.3 linear", "ACGT")
  )

  # a map that is a single empty line is equally harmless
  expect_equal(
    headers_of(stamp("", c("ctgA", "ctgB"))),
    c(">SAMP.1.1 linear", ">SAMP.1.2 linear")
  )
})
