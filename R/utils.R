#' If x is `NULL`, return y, otherwise return x
#'
#' @param x,y Two elements to test, one potentially `NULL`
#'
#' @noRd
#'
"%||%" <- function(x, y) {
  if (length(x)==0L) {
    y
  } else {
    x
  }
}

#' If x is `NA`, return y, otherwise return x
#'
#' @param x,y Two elements to test, one potentially `NA`
#'
#' @noRd
#'
"%|NA|%" <- function(x, y) {
  if (is.na(x)) {
    y
  } else {
    x
  }
}

#' Inverted versions of in, is.null and is.na
#'
#' @noRd
#'
`%nin%` <- Negate(`%in%`)
not_null <- Negate(is.null)
not_na <- Negate(is.na)
