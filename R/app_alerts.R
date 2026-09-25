# One alert system: a blocking alert, a yes/no confirm, and a toast. Every
# call states a type, and the session is passed here, not at the call site.
# See dev/ui_review themes T24 and T16.

#' Blocking alert. `type` is required so nothing renders typeless.
#' @noRd
mp_alert <- function(title, text = NULL, type, html = FALSE,
                     session = getDefaultReactiveDomain()) {
  type <- match.arg(type, c("success", "info", "warning", "error", "question"))
  shinyWidgets::sendSweetAlert(
    session = session, title = title, text = text, type = type, html = html,
    btn_labels = "OK", btn_colors = unname(MP_COLORS[["primary"]])
  )
}

#' Confirm dialog. Sets `input[[id]]` to TRUE when the action is taken.
#'
#' `confirmSweetAlert()` reads `btn_labels[1]` as the cancel button and
#' `btn_labels[2]` as the confirm button; `reverseButtons` makes sweetalert2
#' render that order left to right, so the verb sits right-most.
#'
#' `skippable = TRUE` adds a "Don't ask again this session" checkbox. Once it
#' is ticked the dialog is not shown again and this returns TRUE, so the caller
#' should act straight away; otherwise it returns FALSE.
#' @noRd
mp_confirm <- function(id, title, text, action_label, danger = FALSE, html = FALSE,
                       skippable = FALSE, session = getDefaultReactiveDomain()) {
  if (isTRUE(skippable)) {
    skip_id <- paste0(sub(session$ns(""), "", id, fixed = TRUE), "_skip")
    if (isTRUE(shiny::isolate(session$input[[skip_id]]))) {
      return(invisible(TRUE))
    }
    text <- shiny::tagList(
      if (html) text else shiny::tags$p(text),
      shiny::tags$label(
        style = "font-weight: normal; font-size: 0.9em;",
        shiny::tags$input(
          type = "checkbox",
          onchange = sprintf("Shiny.setInputValue('%s', this.checked)",
                             session$ns(skip_id))
        ),
        " Don't ask again this session"
      )
    )
    html <- TRUE
  }
  accent <- if (isTRUE(danger)) "danger" else "primary"
  shinyWidgets::confirmSweetAlert(
    session = session, inputId = id, title = title, text = text,
    type = if (isTRUE(danger)) "warning" else "question",
    btn_labels = c("Cancel", action_label),
    btn_colors = unname(MP_COLORS[c("grey", accent)]),
    html = html,
    reverseButtons = TRUE
  )
  invisible(FALSE)
}

#' Two-way choice: both buttons are answers, dismissing is neither.
#'
#' `labels[1]` is the cancel slot, so it sets `input[[id]]` to FALSE, and
#' `labels[2]` sets it to TRUE. `cancelOnDismiss = FALSE` keeps a closed
#' dialog from writing an answer the user did not pick.
#' @noRd
mp_choice <- function(id, title, text, labels,
                      session = getDefaultReactiveDomain()) {
  shinyWidgets::confirmSweetAlert(
    session = session, inputId = id, title = title, text = text,
    type = "question",
    btn_labels = labels,
    btn_colors = unname(MP_COLORS[c("primary", "primary")]),
    cancelOnDismiss = FALSE, showCloseButton = TRUE
  )
}

#' Non-blocking toast. One duration for the whole app.
#' @noRd
mp_toast <- function(text, type = c("message", "warning", "error", "success"),
                     duration = 6, session = getDefaultReactiveDomain()) {
  type <- match.arg(type)
  if (type == "success") type <- "message"
  showNotification(text, type = type, duration = duration, session = session)
}

#' Next value for a review-flag toggle over a mixed selection.
#'
#' If any selected unit is already `on`, the toggle clears; otherwise it sets.
#' Drives both the database write and the toolbar label (theme T02).
#'
#' @noRd
mp_flag_next <- function(x, on = "yes", off = "no") {
  if (any(x == on, na.rm = TRUE)) off else on
}
