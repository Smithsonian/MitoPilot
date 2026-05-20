#' Read a bundled tool help text file
#'
#' @param tool short name (matches `inst/tool_help/<tool>.txt`)
#' @return character scalar with the file contents, or a fallback message
#' @noRd
read_tool_help <- function(tool) {
  f <- system.file("tool_help", paste0(tool, ".txt"), package = "MitoPilot")
  if (!nzchar(f) || !file.exists(f)) {
    return(paste0(
      "No bundled help for '", tool, "' found.\n\n",
      "Run tools/capture_tool_help.sh against the MitoPilot Docker image to ",
      "generate inst/tool_help/", tool, ".txt"
    ))
  }
  paste(readLines(f, warn = FALSE), collapse = "\n")
}

#' Small inline "?" icon that opens a modal with the tool's bundled --help text
#'
#' Use inside any opts modal next to the textInput that takes that tool's args.
#' Each call creates a namespaced actionLink whose click is wired to its own
#' observer (so multiple icons coexist without colliding).
#'
#' @param tool short name (matches inst/tool_help/<tool>.txt)
#' @param label optional tooltip label (default: tool name + "options")
#' @param session current Shiny session (defaults to the calling reactive
#'   context's session). Required to namespace input IDs correctly.
#' @return a tagList with the icon link
#' @noRd
tool_help_icon <- function(tool,
                           label = paste(tool, "options"),
                           session = shiny::getDefaultReactiveDomain()) {
  ns <- session$ns
  input_id <- paste0("help_", tool)
  shiny::actionLink(
    ns(input_id),
    label = NULL,
    icon = shiny::icon("circle-question"),
    title = paste("Show", label, "documentation"),
    style = "color: #888; margin-left: 4px;"
  )
}

#' Register an observer that opens a help modal when the matching icon is clicked
#'
#' Call once per `tool_help_icon()` inside the same moduleServer.
#'
#' @param tool short name (must match the tool_help_icon() call)
#' @param input the moduleServer's `input` object
#' @noRd
register_tool_help <- function(tool, input, reopen = NULL,
                               session = shiny::getDefaultReactiveDomain()) {
  ns <- session$ns
  input_id <- paste0("help_", tool)
  close_id <- paste0("help_close_", tool)
  shiny::observeEvent(input[[input_id]], ignoreInit = TRUE, {
    shiny::showModal(shiny::modalDialog(
      title = paste("Documentation:", tool),
      size = "l",
      easyClose = FALSE,
      shiny::tags$pre(
        style = paste(
          "white-space: pre-wrap; font-size: 12px; max-height: 60vh;",
          "overflow-y: auto; background: #f7f7f7; padding: 10px;",
          "border-radius: 4px;"
        ),
        read_tool_help(tool)
      ),
      footer = shiny::tagList(shiny::actionButton(ns(close_id), "Close"))
    ))
  })
  shiny::observeEvent(input[[close_id]], ignoreInit = TRUE, {
    shiny::removeModal()
    if (is.function(reopen)) reopen()
  })
}
