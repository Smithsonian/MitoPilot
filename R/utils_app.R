#' Open a directory in an environment-aware way
#'
#' Local desktop opens the OS file browser; RStudio Server navigates the Files pane (and
#' notifies the user, since that is not obvious); headless sessions cannot open folders, so a
#' warning notification with the path is shown instead.
#'
#' @noRd
open_path <- function(pth) {
  if (isTRUE(getOption("MitoPilot.headless"))) {
    showNotification(
      paste0("Headless session: cannot open folders here. Use the Copy button. Path: ", pth),
      type = "warning", duration = 10
    )
    return(invisible(FALSE))
  }
  if (!dir.exists(pth)) {
    showNotification(
      paste0("Folder not found (it may have been cleaned or is on storage this host ",
             "cannot see): ", pth),
      type = "warning", duration = 10
    )
    return(invisible(FALSE))
  }
  if (tolower(Sys.getenv("RSTUDIO_PROGRAM_MODE")) == "server") {
    if (requireNamespace("rstudioapi", quietly = TRUE)) rstudioapi::filesPaneNavigate(pth)
    showNotification(
      "Opened in the RStudio Files pane (bottom-right panel).",
      type = "message", duration = 5
    )
  } else {
    utils::browseURL(pth)
  }
  invisible(TRUE)
}

#' Set state of details element open/closed
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
