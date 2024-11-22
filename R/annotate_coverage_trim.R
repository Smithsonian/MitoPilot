#' Title
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

  # Set trailing trim point
  trailing_good_depth <- min(which(rev(stats$MeanDepth > 10)))
  if (trailing_good_depth > 100) {
    trailing_good_depth <- min(which(rev(stats$MeanDepth > 15)))
    stats$mask[(nrow(stats) - trailing_good_depth + 1):nrow(stats)] <- TRUE
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

  # Set leading trim point
  leading_good_depth <- min(which(stats$MeanDepth > 10))
  if (leading_good_depth > 100) {
    leading_good_depth <- min(which(stats$MeanDepth > 15))
    stats$mask[1:leading_good_depth] <- TRUE
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
