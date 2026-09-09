# Shared UI building blocks. One toolbar button, one filter picker, one modal
# footer, one modal title, one CSV row, one pluraliser. See dev/ui_review
# themes T05, T10, T13, T16, T23.

#' Toolbar button with the shared class set and a tooltip.
#'
#' A disabled `<button>` fires no pointer events, so a `title` on it never
#' shows. When `needs_selection` is TRUE the button is wrapped in a span that
#' carries the requirement tooltip and the button keeps its own action tooltip.
#'
#' @noRd
mp_toolbar_button <- function(id, label, icon = NULL,
                              emphasis = c("default", "primary", "danger"),
                              title = NULL, needs_selection = FALSE) {
  emphasis <- match.arg(emphasis)
  btn <- actionButton(id, label, icon = icon, title = title)
  btn$attribs$class <- paste0("btn btn-sm action-button mp-toolbar-btn btn-", emphasis)
  if (!is.null(title) && (is.null(label) || (is.character(label) && !nzchar(label)))) {
    btn <- tagAppendAttributes(btn, `aria-label` = title)
  }
  if (!needs_selection) {
    return(btn)
  }
  tags$span(
    class = "mp-needs-selection-wrap",
    title = "Select one or more rows in the table first",
    tagAppendAttributes(btn, class = "mp-needs-selection")
  )
}

#' Multi-select filter picker for a table filter row.
#' @noRd
mp_filter_picker <- function(id, label, choices, selected = choices, width = "150px") {
  shinyWidgets::pickerInput(
    inputId = id, label = label, choices = choices, selected = selected,
    multiple = TRUE, width = width,
    options = list(
      `actions-box` = TRUE,
      `select-all-text` = "All",
      `deselect-all-text` = "None",
      `selected-text-format` = "count > 1",
      `count-selected-text` = "{0} of {1}"
    )
  )
}

#' Modal footer: extra, danger, dismiss, primary (primary right-most).
#'
#' @noRd
mp_footer <- function(primary = NULL, dismiss = "Cancel", danger = NULL, extra = NULL) {
  emph <- function(x, cls) if (is.null(x)) NULL else tagAppendAttributes(x, class = cls)
  parts <- list(
    extra,
    emph(danger, "btn-danger"),
    if (is.null(dismiss)) NULL else modalButton(dismiss),
    emph(primary, "btn-primary")
  )
  do.call(tagList, parts[!vapply(parts, is.null, logical(1))])
}

#' Modal title with a Bootstrap close X and an optional subtitle.
#'
#' `modalDialog()` already wraps whatever it is handed in its own
#' `h4.modal-title`, so this returns a plain div: one heading per dialog.
#'
#' @noRd
mp_modal_title <- function(text, subtitle = NULL, close = TRUE) {
  div(
    class = "mp-modal-title",
    if (close) {
      tags$button(
        type = "button", class = "close", `data-dismiss` = "modal",
        `aria-label` = "Close", tags$span(`aria-hidden` = "true", HTML("&times;"))
      )
    },
    text,
    if (is.null(subtitle)) NULL else tags$p(class = "text-muted mp-modal-subtitle", subtitle)
  )
}

#' The two table CSV download buttons.
#' @noRd
mp_csv_download_row <- function(ns) {
  div(
    class = "mp-csv-row",
    downloadButton(ns("export_selected"), "Download selected rows", class = "btn-sm"),
    downloadButton(ns("export_all"), "Download all rows", class = "btn-sm")
  )
}

#' Count plus noun, pluralised: "1 sample", "3 samples", "2 assemblies".
#'
#' Errors on `NA` rather than putting "NA" on screen.
#'
#' @noRd
mp_n <- function(n, noun, plural = NULL) {
  if (length(n) != 1 || is.na(n)) stop("mp_n() needs one non-missing count")
  if (n == 1) {
    return(paste(n, noun))
  }
  if (is.null(plural)) {
    plural <- if (grepl("[^aeiou]y$", noun)) sub("y$", "ies", noun) else paste0(noun, "s")
  }
  paste(n, plural)
}

#' Checkbox with a visible tick and an accessible name.
#'
#' `prettyCheckbox()` draws a filled square with no glyph and never binds its
#' label to the input; the check icon and the aria-label fix both (theme T18).
#'
#' @noRd
mp_checkbox <- function(inputId, label, value = FALSE, ...) {
  x <- shinyWidgets::prettyCheckbox(
    inputId, label = label, value = value, status = "primary", icon = icon("check"), ...
  )
  lab <- if (is.character(label)) label else paste(htmltools::doRenderTags(label), collapse = " ")
  htmltools::tagQuery(x)$find("input")$addAttrs(`aria-label` = lab)$allTags()
}

