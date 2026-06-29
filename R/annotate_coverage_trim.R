#' Use coverage info to trim linear assemblies
#'
#' @param assembly a DNAString object
#' @param stats a data frame of coverage stats
#'
#' @export
#'
coverage_trim <- function(assembly, stats) {
  # Handle missing tail coverage (edge case)
  stats <- data.frame(Position = seq_len(length(assembly))) |>
    dplyr::left_join(
      stats,
      by = "Position"
    ) |>
    tidyr::replace_na(
      list(
        MeanDepth = 0,
        Depth = 0,
        Correct = 0,
        ErrorRate = 0,
        mask = TRUE
      )
    )

  # Set trailing trim point. Skip when no position clears the depth threshold
  # (min(which(...)) would be Inf); the trim heuristic doesn't apply there.
  good_tail <- which(rev(stats$MeanDepth > 10))
  if (length(good_tail) > 0L && min(good_tail) > 100) {
    good_tail15 <- which(rev(stats$MeanDepth > 15))
    if (length(good_tail15) > 0L) {
      stats$mask[(nrow(stats) - min(good_tail15) + 1):nrow(stats)] <- TRUE
    }
  }
  trailing_trim <- nrow(stats)
  if (sum(rev(stats$mask)[1:100]) > 10) {
    while (T) {
      if (any(stats$mask[(trailing_trim - 51):(trailing_trim - 1)])) {
        trailing_trim <- trailing_trim - 1
      } else {
        break
      }
    }
    assembly <- Biostrings::subseq(assembly, 1, trailing_trim - 1)
    stats <- stats[1:(trailing_trim - 1), ]
  }

  # Set leading trim point. Same guard as the trailing case.
  good_lead <- which(stats$MeanDepth > 10)
  if (length(good_lead) > 0L && min(good_lead) > 100) {
    good_lead15 <- which(stats$MeanDepth > 15)
    if (length(good_lead15) > 0L) {
      stats$mask[1:min(good_lead15)] <- TRUE
    }
  }
  if (sum(stats$mask[1:100]) > 10) {
    leading_trim <- 1
    while (T) {
      if (any(stats$mask[(leading_trim + 1):(leading_trim + 51)])) {
        leading_trim <- leading_trim + 1
      } else {
        break
      }
    }
    assembly <- Biostrings::subseq(assembly, leading_trim + 1, -1)
    stats <- stats[-1:-leading_trim, ] |>
      dplyr::mutate(
        Position = dplyr::row_number()
      )
  }

  return({
    list(
      assembly = assembly,
      stats = stats
    )
  })
}
