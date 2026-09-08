#' Brief inline help text shown directly under an options-modal field
#'
#' Renders a small muted paragraph (the existing `text-muted` convention) to sit
#' immediately below an input. Optionally appends a "(learn more)" link to the
#' MitoPilot documentation or an external tool page.
#'
#' @param ... help text (character/tags), one short sentence.
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
    class = "text-muted mp-help-text",
    style = paste0("margin-top: ", margin_top, "; margin-bottom: 14px;"),
    inner
  )
}

#' Small "?" icon that opens a help popover on click or keyboard focus
#'
#' The one help affordance: a grey `circle-question` always means "click for
#' a popover". The popover itself is wired up once in `custom.js` for every
#' `[data-toggle="mp-popover"]` on the page, so this works inside modals and
#' inside dynamically rendered UI with no server code.
#'
#' @param text help text, one or two sentences (HTML allowed).
#' @param label optional name of the thing being explained, used in the
#'   accessible name.
#' @return a focusable button carrying the popover content.
#' @noRd
mp_help_tip <- function(text, label = NULL) {
  name <- if (is.null(label)) "Show help" else paste("Show help for", label)
  shiny::tags$button(
    type = "button",
    class = "mp-help-icon",
    `data-toggle` = "mp-popover",
    `data-content` = as.character(text),
    `aria-label` = name,
    shiny::tags$i(class = "fa-solid fa-circle-question", `aria-hidden` = "true")
  )
}

#' A field or column label with a help popover beside it
#'
#' @param label the visible label text.
#' @param tip help text for the popover.
#' @noRd
mp_help_label <- function(label, tip) {
  shiny::tagList(label, mp_help_tip(tip, label = label))
}

#' Read a bundled tool help text file
#'
#' @param tool short name (matches `inst/tool_help/<tool>.txt`)
#' @return character scalar with the file contents, or a fallback message
#' @noRd
read_tool_help <- function(tool) {
  f <- system.file("tool_help", paste0(tool, ".txt"), package = "MitoPilot")
  if (!nzchar(f) || !file.exists(f)) {
    return("Help for this tool is not bundled in this build.")
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
      class = "mp-help-icon"
    ),
    shiny::tags$div(
      id = ns(panel_id),
      class = "mp-tool-help-popover",
      style = paste(
        "display: none; position: absolute; z-index: 1080;",
        "left: 0; top: 1.6em; width: 720px; max-width: 90vw;",
        "max-height: 50vh; overflow-y: auto; text-align: left;",
        "font-weight: normal; background: var(--mp-surface);",
        "border: 1px solid var(--mp-border);",
        "border-radius: var(--mp-radius); box-shadow: 0 4px 16px rgba(0,0,0,0.2);",
        "padding: 10px;"
      ),
      shiny::tags$pre(
        style = paste(
          "white-space: pre-wrap; font-size: var(--mp-fs-meta); margin: 0;",
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
