#' Feature not ready message
#' @noRd
coming_soon <- function(text = "This feature is not yet implemented.") {
  shinyWidgets::sendSweetAlert(
    title = "Coming soon...",
    text = text
  )
}

#' Set state of details element open/closed
#'
#' @param params
#'
#' @noRd
toggleDetails <- function(id, state, session = getDefaultReactiveDomain()) {
  session$sendCustomMessage("toggleDetails", list(id = id, state = state))
}
