#' App session lifecycle tracking
#'
#' Package-local state used to stop the Shiny app when the last browser
#' session closes, while surviving a refresh (which ends one session and
#' starts another within the grace period).
#'
#' @noRd
.mitopilot_app <- new.env(parent = emptyenv())
.mitopilot_app$active_sessions <- 0L

#' Track a session and stop the app once all sessions have closed
#'
#' Increments the active-session count and registers an `onSessionEnded`
#' handler that decrements it, then schedules a delayed check. The app only
#' stops if no session has reconnected within `grace` seconds, so an
#' accidental browser refresh does not kill the server.
#'
#' @param session Shiny session object.
#' @param grace numeric. Seconds to wait before stopping the app.
#'
#' @noRd
register_app_lifecycle <- function(session, grace = 5) {
  .mitopilot_app$active_sessions <- .mitopilot_app$active_sessions + 1L
  session$onSessionEnded(function() {
    .mitopilot_app$active_sessions <- .mitopilot_app$active_sessions - 1L
    later::later(function() {
      if (.mitopilot_app$active_sessions <= 0L) {
        message("No active sessions. Stopping app.")
        shiny::stopApp()
      }
    }, delay = grace)
  })
}
