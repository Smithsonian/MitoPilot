# Mapper command builders and the bwa-mem path through the loop.

test_that("pass 1 appends relaxed seeding after the user's flags, later passes do not", {
  expect_equal(.mtr_pass_opts("bowtie2", "--very-sensitive-local", 1L),
               "--very-sensitive-local -N 1 -L 15 -i S,1,0.25 --mp 4,2 --score-min G,10,6")
  expect_equal(.mtr_pass_opts("bwa-mem", "-B 3", 1L), "-B 3 -k 15 -B 2 -T 20")
  expect_equal(.mtr_pass_opts("bwa-mem", "", 1L), "-k 15 -B 2 -T 20")
  expect_equal(.mtr_pass_opts("bowtie2", "--very-sensitive-local", 2L),
               "--very-sensitive-local")
  expect_equal(.mtr_pass_opts("bwa-mem", NA, 3L), "")
  expect_equal(.mtr_pass_opts("bwa-aln", "-l 1024 -n 0.01 -o 2", 1L),
               "-l 1024 -n 0.01 -o 2 -n 0.06 -o 2 -l 1024")
  expect_equal(.mtr_pass_opts("bwa-aln", "-l 1024 -n 0.01 -o 2", 2L), "-l 1024 -n 0.01 -o 2")
})

test_that("bwa-aln runs aln on each mate, then sampe, and shares bwa's index", {
  aln <- .mtr_map_cmd("bwa-aln", "-n 0.01", "idx", "r1.fq", "r2.fq", 4, "log", TRUE)
  expect_match(aln, paste0(
    "^bwa aln -t 4 -n 0.01 'idx' 'r1.fq' > 'idx_1.sai' 2>> 'log' && ",
    "bwa aln -t 4 -n 0.01 'idx' 'r2.fq' > 'idx_2.sai' 2>> 'log' && ",
    "bwa sampe 'idx' 'idx_1.sai' 'idx_2.sai' 'r1.fq' 'r2.fq' 2>> 'log' \\| samtools view -b -F 4 -$"))
  expect_false(grepl("samtools view",
                     .mtr_map_cmd("bwa-aln", "", "idx", "r1.fq", "r2.fq", 4, "log", FALSE)))
  expect_match(.mtr_index_cmd("bwa-aln", "ref.fa", "idx"), "^bwa index -p 'idx' 'ref.fa'$")
})

test_that("map commands mirror --no-unal for bwa and place threads", {
  bwa <- .mtr_map_cmd("bwa-mem", "-B 2", "idx", "r1.fq", "r2.fq", 4, "log", TRUE)
  expect_match(bwa, "^bwa mem -t 4 -B 2 'idx' 'r1.fq' 'r2.fq' 2>> 'log' \\| samtools view -b -F 4 -$")
  expect_false(grepl("samtools view",
                     .mtr_map_cmd("bwa-mem", "", "idx", "r1.fq", "r2.fq", 4, "log", FALSE)))

  bt <- .mtr_map_cmd("bowtie2", "--local", "idx", "r1.fq", "r2.fq", 2, "log", TRUE)
  expect_match(bt, "^bowtie2 --local --no-unal -x 'idx' -1 'r1.fq' -2 'r2.fq' --threads 2 2>> 'log'$")
  expect_false(grepl("--no-unal",
                     .mtr_map_cmd("bowtie2", "--local", "idx", "r1.fq", "r2.fq", 2, "log", FALSE)))

  expect_match(.mtr_index_cmd("bwa-mem", "ref.fa", "idx"), "^bwa index -p 'idx' 'ref.fa'$")
  expect_match(.mtr_index_cmd("bowtie2", "ref.fa", "idx"), "^bowtie2-build -q 'ref.fa' 'idx'$")
})

test_that("a missing mapper binary fails with a clear message", {
  skip_on_os("windows")
  withr::local_envvar(c(PATH = withr::local_tempdir()))
  expect_error(.mtr_check_tools("bwa-mem"), "needs bwa on PATH")
  expect_error(.mtr_check_tools("bwa-aln"), "needs bwa on PATH")
  expect_error(.mtr_check_tools("bowtie2"), "bowtie2")
})
