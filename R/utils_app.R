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

#' Turn an R list into an HTML list
#'
#' @param list An R list
#' @param class a class for the list
#'
#' @return an HTML list
#' @noRd
#'
#' @examples
#' list_to_li(c("a", "b"))
#'
#' @importFrom shiny tags tagAppendAttributes tagList
list_to_li <- function(list, class = NULL) {
  if (is.null(class)) {
    tagList(
      lapply(
        list,
        tags$li
      )
    )
  } else {
    res <- lapply(
      list,
      tags$li
    )
    res <- lapply(
      res,
      function(x) {
        tagAppendAttributes(
          x,
          class = class
        )
      }
    )
    tagList(res)
  }
}
