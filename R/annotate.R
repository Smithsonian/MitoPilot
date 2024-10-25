annotate <- function(
  assembly = "/home/harpua/Jonah/MitoPilot-test/out/SRR22396740/assemble/default/SRR22396740_assembly_1.fasta",
  coverage = "/home/harpua/Jonah/MitoPilot-test/out/SRR22396740/assemble/default/SRR22396740_assembly_1_coverageStats.csv",
  cpus = NULL,
  memory = NULL,
  refDB = NULL,
  trnaScan = "-M vert",
  outDir = NULL
  ){

  seqs <- Biostrings::readDNAStringSet(assembly)

  # Coverage triming (optional)
  if(length(coverage)==1L && file.exists(coverage)){
    coverage <- read.csv(coverage) |>
      dplyr::arrange(SeqId, Position) |>
      dplyr::mutate(
        mask = stringr::str_detect(MeanDepth, "^#") |
          stringr::str_detect(ErrorRate, "^#"),
        MeanDepth = as.numeric(stringr::str_remove(MeanDepth, "^#")),
        ErrorRate = as.numeric(stringr::str_remove(ErrorRate, "^#"))
      )
    coverage_trimmed <- seqs |> purrr::imap(~{
      stats <- coverage[coverage$SeqId == stringr::str_extract(.y, "\\S+"), ]
      # Skip for circular assemblies
      if(stringr::str_detect("circular", .y)){
        return({
          list(
            seq = .x,
            stats = stats
          )
        })
      }
      # Run coverage trimming
      coverage_trim( seq = .x, stats = stats )
    })
    seqs <- purrr::map(coverage_trimmed, ~.x$seq) |> Biostrings::DNAStringSet()
    coverage <- purrr::map(coverage_trimmed, ~.x$stats) |> dplyr::bind_rows()
  }

  # tRNA annotation ----






}
