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
    assembly_fn = "22030FL-06-02-140_assembly_1.fasta",
    paired_reads_1 = "extended_1_paired.fq",
    paired_reads_2 = "extended_2_paired.fq",
    unpaired_reads = "unpaired.fq",
    cpus = 4,
    outDir = NULL) {
  assembly <- Biostrings::readDNAStringSet(assembly_fn)
  assembly_len <- assembly@ranges@width
  seq_ids <- names(assembly)
  ids <- stringr::str_split(seq_ids, " ", simplify = T)[, 1]
  # Topology is per scaffold: a multi-scaffold assembly can mix circular and
  # linear records, so the junction construct is applied only to the circular
  # ones instead of once per sample.
  circular_ids <- .coverage_circular_ids(seq_ids, ids)
  names(assembly) <- ids
  names(assembly_len) <- ids
  basename_prefix <- basename(assembly_fn) |> stringr::str_remove("\\.[^\\.]+$")

  # Create output directory
  if (!dir.exists(outDir)) {
    dir.create(outDir, showWarnings = FALSE, recursive = TRUE)
  }
  coverage_fn <- file.path(outDir, paste0(basename_prefix, "_coverage.csv"))

  # No raw reads: build a per-base coverage table straight from the assembly so
  # the coverageStats.csv schema matches the read-based path. Depth/Correct/
  # ErrorRate are unknown (NA); GC is derived from Call in the shared tail below.
  no_reads <- identical(as.character(paired_reads_1), "NA")
  if (no_reads) {
    coverage <- purrr::map_dfr(seq_along(assembly), function(i) {
      calls <- stringr::str_split(as.character(assembly[[i]]), "")[[1]]
      data.frame(
        SeqId = names(assembly)[i], Position = seq_along(calls), Call = calls,
        Depth = NA_real_, Correct = NA_real_, ErrorRate = NA_real_
      )
    })
  } else {

  # Circular scaffolds get a 500bp overlap appended for mapping
  assembly <- .coverage_extend_circular(assembly, circular_ids)
  assembly_working <- assembly_fn |>
    stringr::str_remove("\\.[^\\.]+$") |>
    paste0("_working.fasta")
  Biostrings::writeXStringSet(assembly, assembly_working)

  # Map Reads
  mapped_fn <- file.path(outDir, paste0(basename_prefix, ".bam"))

  if(unpaired_reads == "NA"){
    stringr::str_glue(
      "bowtie2-build {assembly_working} index"
    ) |> system()
    stringr::str_glue(
      "bowtie2 --very-sensitive-local --no-unal -x index -1 {paired_reads_1} -2 {paired_reads_2} --threads {cpus} ",
      "| samtools view -bS - | samtools sort - > {mapped_fn}"
    ) |> system()
  } else {
    stringr::str_glue(
      "bowtie2-build {assembly_working} index"
    ) |> system()
    stringr::str_glue(
      "bowtie2 --very-sensitive-local --no-unal -x index -1 {paired_reads_1} -2 {paired_reads_2} -U {unpaired_reads} --threads {cpus} ",
      "| samtools view -bS - | samtools sort - > {mapped_fn}"
    ) |> system()
  }

  # Get coverage stats
  bam_readcount_cmd(assembly_working, mapped_fn, coverage_fn) |> system()

  # Load mapping results ----
  coverage <- readr::read_delim(
    coverage_fn,
    col_names = FALSE,
    guess_max = Inf,
    delim = "\t",
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
      ErrorRate = dplyr::if_else(Depth == 0, NA_real_, (Depth - Correct) / Depth)
    )

  # Reform circular assembly ---
  coverage <- .coverage_reform_circular(coverage, assembly_len, circular_ids)

  # bam-readcount skips uncovered positions (start, end, and internal runs such
  # as map-to-ref N gaps); downstream steps expect one row per base
  coverage <- .coverage_fill_positions(
    coverage, Biostrings::subseq(assembly, 1, assembly_len[names(assembly)])
  )
  }

  coverage <- coverage |>
    dplyr::arrange(SeqId, Position)

  readr::write_csv(coverage, file = coverage_fn, quote = "none", na = "")

  # Calculate rolling window stats ----
  stats <- .coverage_rolling_stats(coverage)

  stats_long <- stats |>
    tidyr::pivot_longer(
      dplyr::all_of(c("MeanDepth", "ErrorRate", "MeanDepth_mask", "ErrorRate_mask", "GC")),
      names_to = "Stat",
      values_to = "Val"
    )

  if (length(seq_ids) > 1) {
    stats_long <- stats_long |>
      dplyr::mutate(
        Scaffold = stringr::str_glue("Scaffold {stringr::str_extract(SeqId, '[0-9]+$')} ({dplyr::n()} bp)"),
        .by = c(SeqId, Stat)
      )
  } else {
    stats_long <- stats_long |>
      dplyr::mutate(
        Scaffold = stringr::str_glue("{ifelse(SeqId[1] %in% circular_ids,'Circular','Linear')} ({dplyr::n()} bp)"),
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

  # Coverage plot - one PDF per scaffold ----
  # Skipped with no raw reads: depth/error are empty, so only a GC track would
  # remain (not worth a PDF, and the empty depth facets break faceting).
  for (seq_id in if (no_reads) character(0) else unique(stats_long$SeqId)) {
    scaffold_num <- stringr::str_extract(seq_id, "[0-9]+$")

    scaf_stats <- stats_long |>
      dplyr::filter(SeqId == seq_id, !stringr::str_detect(Stat, "_mask$")) |>
      dplyr::mutate(Stat = factor(Stat, levels = c("MeanDepth", "ErrorRate", "GC"))) |>
      tidyr::drop_na()

    scaf_mask  <- mask_dat |> dplyr::filter(SeqId == seq_id)
    scaf_label <- unique(scaf_stats$Scaffold)[1]

    plot <- ggplot2::ggplot(scaf_stats, ggplot2::aes(x = Position, y = Val)) +
      ggplot2::geom_vline(
        data = scaf_mask,
        ggplot2::aes(xintercept = Position),
        color = "#FF6670",
        size = 1.2
      ) +
      ggplot2::geom_line() +
      ggplot2::facet_grid(
        rows = ggplot2::vars(Stat), switch = "y",
        scales = "free"
      ) +
      ggplot2::labs(title = scaf_label, x = "Base Position", y = "") +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        strip.placement = "outside",
        plot.title = ggplot2::element_text(hjust = 0.5),
        axis.text.x = ggplot2::element_text(hjust = 1)
      )

    file.path(outDir, paste0(basename_prefix, "_", scaffold_num, "_coverage.pdf")) |>
      ggplot2::ggsave(plot, width = 8, height = 5, units = "in")
  }

  # Save output ----
  stats_out <- .coverage_stats_to_output(stats)
  readr::write_csv(
    stats_out,
    file = file.path(outDir, paste0(basename_prefix, "_coverageStats.csv")),
    quote = "none", na = ""
  )

  return(invisible(stats_out))
}

#' Complete a per-base coverage table to one row per assembly position
#'
#' Missing positions get the assembly base, Depth 0, Correct 0 and ErrorRate NA.
#' Only scaffolds that already have rows are completed; a scaffold no read mapped
#' to stays absent so the no-coverage handling downstream still sees it.
#'
#' @param coverage data.frame with SeqId, Position, Call, Depth, Correct, ErrorRate
#' @param assembly named DNAStringSet (names = SeqId)
#' @noRd
.coverage_fill_positions <- function(coverage, assembly) {
  ids <- intersect(names(assembly), unique(coverage$SeqId))
  full <- purrr::map_dfr(ids, function(id) {
    calls <- strsplit(as.character(assembly[[id]]), "")[[1]]
    data.frame(SeqId = id, Position = seq_along(calls), Base = calls)
  })
  full |>
    dplyr::left_join(
      dplyr::select(coverage, "SeqId", "Position", "Call", "Depth", "Correct", "ErrorRate"),
      by = c("SeqId", "Position")
    ) |>
    dplyr::mutate(
      filled = is.na(Depth),
      Call = dplyr::if_else(filled, Base, Call),
      Depth = dplyr::if_else(filled, 0, as.numeric(Depth)),
      Correct = dplyr::if_else(filled, 0, as.numeric(Correct))
    ) |>
    dplyr::select("SeqId", "Position", "Call", "Depth", "Correct", "ErrorRate")
}

#' Read a coverageStats CSV, completing any scaffold that is missing positions
#'
#' Coverage written before [.coverage_fill_positions()] existed lacks rows for
#' internal zero-coverage runs. Those scaffolds are completed and their rolling
#' stats recomputed, exactly as [coverage()] now writes them; complete files are
#' returned as read.
#'
#' @param coverage_fn path to a `*_coverageStats.csv`
#' @param assembly DNAStringSet whose names start with the SeqId
#' @noRd
.coverage_read_complete <- function(coverage_fn, assembly) {
  coverage <- read.csv(coverage_fn)
  assembly <- stats::setNames(assembly, sub("\\s.*$", "", names(assembly)))
  gappy <- vapply(names(assembly), function(id) {
    p <- coverage$Position[coverage$SeqId == id]
    length(p) > 0 && !identical(sort(as.integer(p)), seq_len(Biostrings::width(assembly[id])))
  }, logical(1))
  if (!any(gappy)) {
    return(coverage)
  }
  fix <- names(assembly)[gappy]
  fixed <- coverage[coverage$SeqId %in% fix, ] |>
    dplyr::mutate(ErrorRate = dplyr::if_else(Depth == 0, NA_real_, (Depth - Correct) / Depth)) |>
    .coverage_fill_positions(assembly[fix]) |>
    .coverage_rolling_stats() |>
    .coverage_stats_to_output()
  dplyr::bind_rows(
    fixed,
    dplyr::mutate(coverage[!coverage$SeqId %in% fix, ],
                  dplyr::across(c("MeanDepth", "ErrorRate"), as.character))
  )
}

#' Scaffold ids whose FASTA description marks them circular
#'
#' Topology is stamped into the FASTA description, one value per record, so
#' circularity is read per scaffold rather than collapsed to one value per
#' sample.
#'
#' @param seq_ids full FASTA header lines (id plus description)
#' @param ids scaffold ids (first whitespace-delimited token of each header)
#' @return character vector of the ids marked circular
#' @noRd
.coverage_circular_ids <- function(seq_ids, ids = stringr::str_split(seq_ids, " ", simplify = T)[, 1]) {
  ids[stringr::str_detect(seq_ids, "circular")]
}

#' Append each circular scaffold's own first `flank` bases to itself
#'
#' The junction construct used for mapping. Applied per scaffold, so a linear
#' scaffold sharing an assembly with a circular one is left alone.
#' @noRd
.coverage_extend_circular <- function(assembly, circular_ids, flank = 500) {
  hit <- names(assembly) %in% circular_ids
  if (!any(hit)) {
    return(assembly)
  }
  ext <- assembly[hit]
  end <- pmin(flank, Biostrings::width(ext))
  assembly[hit] <- Biostrings::xscat(ext, Biostrings::subseq(ext, start = 1, end = end))
  assembly
}

#' Fold junction-construct positions back onto the scaffold they came from
#'
#' Positions past a circular scaffold's true length are the appended copy of its
#' start; moving them back and summing recombines the seam depth. Linear
#' scaffolds are passed through untouched.
#' @noRd
.coverage_reform_circular <- function(coverage, assembly_len, circular_ids) {
  is_circ <- coverage$SeqId %in% circular_ids
  if (!any(is_circ)) {
    return(coverage)
  }
  circ <- coverage[is_circ, ]
  lens <- assembly_len[circ$SeqId]
  to_move <- circ$Position > lens
  circ$Position[to_move] <- circ$Position[to_move] - lens[to_move]
  circ <- circ |>
    dplyr::summarise(
      Call = Call[1],
      Depth = sum(Depth),
      Correct = sum(Correct),
      .by = c(SeqId, Position)
    ) |>
    dplyr::mutate(
      ErrorRate = dplyr::if_else(Depth == 0, NA_real_, (Depth - Correct) / Depth)
    )
  dplyr::bind_rows(coverage[!is_circ, ], circ)
}

#' Rolling-window coverage stats (MeanDepth/ErrorRate/GC + outlier masks)
#'
#' Shared by [coverage()] and the multi-path consensus writer so both produce
#' an identical `*_coverageStats.csv` layout. Input `df` must contain columns
#' `SeqId, Position, Call, Depth, Correct, ErrorRate`.
#' @noRd
.coverage_rolling_stats <- function(df) {
  df |>
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
}

#' Apply outlier masks (`#` prefix) and drop mask columns for CSV output
#' @noRd
.coverage_stats_to_output <- function(stats) {
  stats |>
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
    dplyr::select(!dplyr::ends_with("_mask"))
}
