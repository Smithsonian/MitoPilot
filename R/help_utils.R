#' Brief inline help text shown directly under an options-modal field
#'
#' Renders a small muted paragraph (the existing `text-muted` convention) to sit
#' immediately below an input. Optionally appends a "(learn more)" link to the
#' MitoPilot documentation or an external tool page.
#'
#' @param ... help text (character/﻿tags), one short sentence.
#' @param href optional URL to link out to.
#' @param link_text label for the trailing link (default "learn more").
#' @param id optional element id (namespace via `ns()`) so the help can be
#'   shown/hidden together with the field it describes.
#' @return a `tags$p` element.
#' @noRd
opts_help <- function(..., href = NULL, link_text = "learn more", id = NULL,
                      nested = FALSE) {
  inner <- list(...)
  if (!is.null(href)) {
    inner <- c(inner, list(
      " (",
      shiny::tags$a(href = href, target = "_blank", rel = "noopener", link_text),
      ")"
    ))
  }
  # The default negative top margin snugs the help under a sibling input. When
  # the help is appended INSIDE the input's container (nested = TRUE), that
  # negative margin overlaps the input box, so use a small positive margin.
  margin_top <- if (nested) "2px" else "-6px"
  shiny::tags$p(
    id = id,
    class = "text-muted",
    style = paste0("margin-top: ", margin_top, "; margin-bottom: 14px; font-size: 0.85em;"),
    inner
  )
}

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

#' Small inline "?" icon that toggles an in-place popover with the tool's
#' bundled --help text
#'
#' Use inside any opts modal next to the textInput that takes that tool's args.
#' The help text is rendered into a hidden popover that sits inside the same
#' modal and is shown/hidden client-side, so opening help never tears down the
#' options modal (and therefore never discards unsaved edits).
#'
#' @param tool short name (matches inst/tool_help/<tool>.txt)
#' @param label optional tooltip label (default: tool name + "options")
#' @param session current Shiny session (defaults to the calling reactive
#'   context's session). Required to namespace input IDs correctly.
#' @return a span with the icon link and its hidden help popover
#' @noRd
tool_help_icon <- function(tool,
                           label = paste(tool, "options"),
                           session = shiny::getDefaultReactiveDomain()) {
  ns <- session$ns
  input_id <- paste0("help_", tool)
  panel_id <- paste0("help_panel_", tool)
  shiny::tags$span(
    style = "position: relative; display: inline-block;",
    shiny::actionLink(
      ns(input_id),
      label = NULL,
      icon = shiny::icon("circle-question"),
      title = paste("Show", label, "documentation"),
      style = "color: #888; margin-left: 4px;"
    ),
    shiny::tags$div(
      id = ns(panel_id),
      class = "mp-tool-help-popover",
      style = paste(
        "display: none; position: absolute; z-index: 1080;",
        "left: 0; top: 1.6em; width: 720px; max-width: 90vw;",
        "max-height: 50vh; overflow-y: auto; text-align: left;",
        "font-weight: normal; background: #fff; border: 1px solid #ccc;",
        "border-radius: 4px; box-shadow: 0 4px 16px rgba(0,0,0,0.2);",
        "padding: 10px;"
      ),
      shiny::tags$pre(
        style = paste(
          "white-space: pre-wrap; font-size: 12px; margin: 0;",
          "background: transparent; border: none; padding: 0;"
        ),
        read_tool_help(tool)
      )
    )
  )
}

#' Register an observer that toggles the help popover when its icon is clicked
#'
#' Call once per `tool_help_icon()` inside the same moduleServer. `reopen` is
#' accepted for backward compatibility and ignored (the popover no longer
#' replaces the options modal).
#'
#' @param tool short name (must match the tool_help_icon() call)
#' @param input the moduleServer's `input` object
#' @noRd
register_tool_help <- function(tool, input, reopen = NULL,
                               session = shiny::getDefaultReactiveDomain()) {
  ns <- session$ns
  input_id <- paste0("help_", tool)
  panel_id <- paste0("help_panel_", tool)
  shiny::observeEvent(input[[input_id]], ignoreInit = TRUE, {
    shinyjs::toggle(id = panel_id)
  })
}
