#' Map reads to assembly
#'
#' @param assembly_fn Path to the input assembly file (fasta)
#' @param paired_reads_1 path to the raw forward input reads (fastq)
#' @param paired_reads_2 path to the raw reverse input reads (fastq)
#' @param unpaired_reads path to the raw unpaired input reads (fastq)
#' @param cpus Number of CPUs to use
#' @param outDir Path to the output directory
#'
#' @export
#'
coverage <- function(
    assembly_fn = NULL,
    paired_reads_1 = NULL,
    paired_reads_2 = NULL,
    unpaired_reads = NULL,
    cpus = 4,
    outDir = NULL) {
  assembly <- Biostrings::readDNAStringSet(assembly_fn)
  assembly_len <- assembly@ranges@width
  seq_ids <- names(assembly)
  circular <- all(stringr::str_detect(seq_ids, "circular"))
  ids <- stringr::str_split(seq_ids, "\\.", simplify = T)[, 1]
  basename_prefix <- basename(assembly_fn) |> stringr::str_remove("\\.[^\\.]+$")

  # If the assembly is circular and only has one sequence, add a 500bp overlap for mapping
  if (circular && length(seq_ids) == 1) {
    assembly <- Biostrings::xscat(assembly, Biostrings::subseq(assembly, start = 1, end = 500)) |>
      setNames(seq_ids)
  }
  assembly_working <- assembly_fn |>
    stringr::str_remove("\\.[^\\.]+$") |>
    paste0("_working.fasta")
  Biostrings::writeXStringSet(assembly, assembly_working)

  # Create output directory
  if (!dir.exists(outDir)) {
    dir.create(outDir, showWarnings = FALSE, recursive = TRUE)
  }

  # Map Reads
  mapped_fn <- file.path(outDir, paste0(basename_prefix, ".bam"))
  stringr::str_glue(
    "bowtie2-build {assembly_working} index"
  ) |> system()
  stringr::str_glue(
    "bowtie2 --very-sensitive-local --no-unal -x index -1 {paired_reads_1} -2 {paired_reads_2} -U {unpaired_reads} --threads {cpus} ",
    "| samtools view -bS - | samtools sort - > {mapped_fn}"
  ) |> system()

  # Get coverage stats
  coverage_fn <- file.path(outDir, paste0(basename_prefix, "_coverage.csv"))
  stringr::str_glue(
    "conda run -n bam-readcount bam-readcount -w1 -f {assembly_working} {mapped_fn} > {coverage_fn}"
  ) |> system()

  # Load mapping results ----
  coverage <- readr::read_delim(
    coverage_fn,
    col_names = FALSE,
    guess_max = Inf, delim = "\t",
    col_select = 1:10,
    show_col_types = FALSE
  ) |>
    dplyr::transmute(
      SeqId = X1,
      Position = X2,
      Call = X3,
      Depth = X4,
      Correct = dplyr::case_when(
        Call == "A" ~ as.numeric(stringr::str_split(X6, ":", simplify = T)[, 2]),
        Call == "T" ~ as.numeric(stringr::str_split(X9, ":", simplify = T)[, 2]),
        Call == "C" ~ as.numeric(stringr::str_split(X7, ":", simplify = T)[, 2]),
        Call == "G" ~ as.numeric(stringr::str_split(X8, ":", simplify = T)[, 2])
      ),
      ErrorRate = (Depth - Correct) / Depth
    )

  # Reform circular assembly ---
  if (circular && length(seq_ids) == 1) {
    to_move <- coverage$Position > assembly_len
    coverage$Position[to_move] <- seq_len(sum(to_move))
    coverage <- coverage |>
      dplyr::summarise(
        Call = Call[1],
        Depth = sum(Depth),
        Correct = sum(Correct),
        .by = c(SeqId, Position)
      ) |>
      dplyr::mutate(
        ErrorRate = (Depth - Correct) / Depth
      )
    readr::write_csv(coverage, file = coverage_fn, quote = "none", na = "")
  }

  # Calculate rolling window stats ----
  stats <- coverage |>
    dplyr::mutate(
      MeanDepth = zoo::rollapply(
        Depth,
        width = 5,
        partial = T, align = "center",
        FUN = function(x) {
          mean(x) |> round(2)
        }
      ),
      MeanDepth_mask = abs(Depth - median(Depth)) > mad(Depth, constant = 8),
      ErrorRate = zoo::rollapply(
        ErrorRate,
        width = 5,
        partial = T, align = "center",
        FUN = function(x) {
          mean(x) |> round(2)
        }
      ),
      ErrorRate_mask = zoo::rollapply(
        ErrorRate,
        width = 5,
        partial = T, align = "center",
        FUN = function(x) {
          any(x > 0.05)
        }
      ),
      GC = zoo::rollapply(
        Call,
        width = 200,
        fill = NA, align = "center",
        FUN = function(x) {
          (sum(x %in% c("G", "C")) / length(x)) |> round(2)
        }
      ),
      .by = SeqId
    )

  stats_long <- stats |>
    tidyr::pivot_longer(
      dplyr::all_of(c("MeanDepth", "ErrorRate", "MeanDepth_mask", "ErrorRate_mask", "GC")),
      names_to = "Stat",
      values_to = "Val"
    )

  if (length(seq_ids) > 1) {
    stats_long <- stats_long |>
      dplyr::mutate(
        Scaffold = stringr::str_glue("Scaffold {stringr::str_extract(SeqId, '[0-9]$')} ({dplyr::n()} bp)"),
        .by = c(SeqId, Stat)
      )
  } else {
    stats_long <- stats_long |>
      dplyr::mutate(
        Scaffold = stringr::str_glue("{ifelse(circular==TRUE,'Circular','Linear')} ({dplyr::n()} bp)"),
        .by = c(SeqId, Stat)
      )
  }

  mask_dat <- stats_long |>
    dplyr::filter(stringr::str_detect(Stat, "_mask$")) |>
    dplyr::mutate(
      Stat = stringr::str_remove(Stat, "_mask$"),
      Stat = factor(Stat, levels = c("MeanDepth", "ErrorRate", "GC"))
    ) |>
    dplyr::filter(Val == 1)

  # Coverage plot ----
  plot <- stats_long |>
    dplyr::filter(!stringr::str_detect(Stat, "_mask$")) |>
    dplyr::mutate(
      Stat = factor(Stat, levels = c("MeanDepth", "ErrorRate", "GC"))
    ) |>
    tidyr::drop_na() |>
    ggplot2::ggplot(ggplot2::aes(x = Position, y = Val)) +
    ggplot2::geom_vline(
      data = mask_dat,
      ggplot2::aes(xintercept = Position),
      color = "#FF6670",
      size = 1.2
    ) +
    ggplot2::geom_line() +
    ggplot2::facet_grid(
      rows = ggplot2::vars(Stat), cols = ggplot2::vars(Scaffold), switch = "y",
      scales = "free", space = "free_x"
    ) +
    ggplot2::xlab("Base Position") +
    ggplot2::ylab("") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      strip.placement = "outside",
      axis.text.x = ggplot2::element_text(hjust = 1)
    )
  file.path(outDir, paste0(basename_prefix, "_coverage.pdf")) |>
    ggplot2::ggsave(plot, width = 12, height = 5, units = "in")

  # Save output ----
  stats_out <- stats |>
    dplyr::mutate(
      MeanDepth = dplyr::case_when(
        MeanDepth_mask == TRUE ~ paste0("#", MeanDepth),
        .default = as.character(MeanDepth)
      ),
      ErrorRate = dplyr::case_when(
        ErrorRate_mask == TRUE ~ paste0("#", ErrorRate),
        .default = as.character(ErrorRate)
      )
    ) |>
    dplyr::select(!dplyr::ends_with("_mask")) |>
    readr::write_csv(
      file = file.path(outDir, paste0(basename_prefix, "_coverageStats.csv")),
      quote = "none", na = ""
    )

  return(invisible(stats_out))
}
