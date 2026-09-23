# Every "pkg=ver" pin in the conda YAMLs must appear with the same version in
# pixi.toml, and vice versa, so the two install routes never drift.
native_dir <- function() system.file("native", package = "MitoPilot")

yaml_pins <- function() {
  files <- list.files(file.path(native_dir(), "envs"), pattern = "[.]yml$", full.names = TRUE)
  out <- character()
  for (f in files) {
    dep <- grep("^\\s*-\\s*[a-z0-9_.-]+=", readLines(f), value = TRUE)
    dep <- sub("^\\s*-\\s*", "", dep)
    dep <- sub("\\s*#.*$", "", dep)
    out <- c(out, dep)
  }
  sort(unique(out))
}

pixi_pins <- function() {
  lines <- readLines(file.path(native_dir(), "pixi.toml"))
  dep <- grep('^[a-z0-9_.-]+\\s*=\\s*"=', lines, value = TRUE)
  name <- sub('\\s*=.*$', "", dep)
  ver <- sub('^.*"=([^"]+)".*$', "\\1", dep)
  sort(unique(paste0(name, "=", ver)))
}

test_that("conda YAMLs and pixi.toml carry identical exact pins", {
  skip_if(!nzchar(native_dir()))
  expect_setequal(yaml_pins(), pixi_pins())
})

test_that("every spec pin is present in the YAMLs", {
  skip_if(!nzchar(native_dir()))
  spec <- c("r-base=4.5.2", "fastp=0.23.4", "spades=4.1.0", "getorganelle=1.7.7.1",
            "bowtie2=2.5.4", "bwa=0.7.19", "samtools=1.24", "minimap2=2.28",
            "blast=2.16.0", "mitos=2.1.10", "trnascan-se=2.0.12", "aragorn=1.2.41",
            "bam-readcount=1.0.1", "openjdk=21", "nextflow=25.10.4", "python=2.7")
  expect_true(all(spec %in% yaml_pins()))
})
