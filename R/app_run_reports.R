#' Workflow whose runs a panel shows ("assemble", "annotate"), or NULL
#' @noRd
run_reports_workflow <- function(mode) {
  switch(mode %||% "", Assemble = "assemble", Annotate = "annotate", NULL)
}

#' Workflow to open for a new-report notice: the active panel's when it has
#' new reports, else the one with the newest new report
#' @noRd
run_reports_notice_workflow <- function(mode, bases) {
  wf <- sub("_.*$", "", bases)
  active <- run_reports_workflow(mode)
  if (!is.null(active) && active %in% wf) return(active)
  wf[order(sub("^[^_]+_", "", bases), decreasing = TRUE)][1]
}

#' Result cell: icon and label in a pill
#' @noRd
run_result_html <- function(result) {
  tone <- c(finished = "success", failures = "warning", failed = "danger",
            stopped = "neutral", unfinished = "info")
  ic <- c(finished = "circle-check", failures = "triangle-exclamation",
          failed = "circle-xmark", stopped = "circle-stop", unfinished = "hourglass-half")
  vapply(result, function(r) {
    if (is.na(r) || !r %in% names(tone)) return("")
    sprintf("<span class='mp-pill mp-pill-%s'>%s %s</span>",
            tone[[r]], as.character(mp_icon(ic[[r]])), RUN_RESULTS[[r]])
  }, "", USE.NAMES = FALSE)
}

#' Run reports toolbar button (UI)
#' @noRd
run_reports_ui <- function(id) {
  ns <- NS(id)
  mp_toolbar_button(
    ns("open"),
    label = "Run Reports",
    icon = mp_icon("file-lines"),
    title = "Read the reports of past pipeline runs for this panel"
  )
}

#' Run reports browser and new-report notice (server)
#'
#' Also sets `session$userData$run_reports_check(extra)`, which writes missing
#' reports and shows the new-report notice; `extra` names reports the caller
#' just wrote itself.
#' @noRd
run_reports_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    runs <- reactiveVal(NULL)
    opened <- reactiveVal(0)
    notice_wf <- NULL
    pending <- character(0)
    session$userData$run_reports_new <- character(0)

    sync <- function() {
      live <- session$userData$run_live_log
      new <- tryCatch(sync_run_reports(session$userData$dir, session$userData$con,
                                       skip = if (!is.null(live)) run_log_base(live)),
                      error = function(e) character(0))
      session$userData$run_reports_new <- union(session$userData$run_reports_new, new)
      new
    }

    show_reports <- function(workflow) {
      sync()
      panel <- if (is.null(workflow)) session$userData$mode %||% "" else
        c(assemble = "Assemble", annotate = "Annotate")[[workflow]]
      df <- if (is.null(workflow)) NULL else tryCatch(
        list_run_reports(session$userData$dir, workflow), error = function(e) NULL)
      runs(df)
      opened(opened() + 1)
      body <- if (is.null(workflow)) {
        p("Run reports are available on the Assemble and Annotate panels.")
      } else if (is.null(df) || nrow(df) == 0) {
        p("No MitoPilot run logs found for this panel.")
      } else {
        tagList(
          reactable::reactableOutput(ns("tbl")),
          uiOutput(ns("detail"))
        )
      }
      showModal(modalDialog(
        title = mp_modal_title(paste("Run reports:", panel)),
        size = "l",
        easyClose = TRUE,
        body,
        footer = modalButton("Close")
      ))
    }

    observeEvent(input$open, {
      show_reports(run_reports_workflow(session$userData$mode))
    })

    output$tbl <- reactable::renderReactable({
      opened()
      df <- runs()
      req(!is.null(df), nrow(df) > 0)
      disp <- data.frame(
        New = ifelse(df$base %in% session$userData$run_reports_new, "New", ""),
        Started = df$started,
        Duration = df$duration,
        Result = run_result_html(df$result),
        Samples = df$n_samples,
        `Failed samples` = df$n_failed_samples,
        `Failed tasks` = df$n_failed_tasks,
        Launched = ifelse(df$launch %in% "job", "Job", "App"),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      reactable::reactable(
        disp,
        selection = "single",
        onClick = "select",
        height = 300,
        pagination = FALSE,
        compact = TRUE,
        highlight = TRUE,
        wrap = FALSE,
        columns = list(
          New = reactable::colDef(
            name = "", maxWidth = 60, html = TRUE,
            cell = rt_pill(map = c(New = "info"), empty = "")
          ),
          Started = reactable::colDef(minWidth = 150),
          Result = reactable::colDef(html = TRUE, minWidth = 190)
        )
      )
    })

    output$detail <- renderUI({
      i <- reactable::getReactableState("tbl", "selected")
      df <- runs()
      req(length(i) == 1, !is.null(df), i <= nrow(df))
      rpt <- df$report[i]
      if (is.na(rpt) || !file.exists(rpt)) {
        log <- df$log[i]
        if (is.na(log) || !file.exists(log)) {
          return(p(style = "margin-top: 1em;", "No report yet: the run has not recorded a finish."))
        }
        invalidateLater(15000)
        return(div(
          style = "margin-top: 1em;",
          p("No report yet: the run has not recorded a finish. Progress from its Nextflow log, ",
            "updated every 15 seconds. A run with no log update for a long time may have stopped."),
          tags$pre(style = "max-height: 40vh; overflow: auto;", run_progress_text(log))
        ))
      }
      txt <- tryCatch(paste(readLines(rpt, warn = FALSE), collapse = "\n"),
                      error = function(e) "The report could not be read.")
      div(
        style = "margin-top: 1em;",
        tags$pre(id = ns("report_text"), style = "max-height: 40vh; overflow: auto;", txt),
        tags$button(
          type = "button", class = "btn btn-default btn-sm",
          onclick = sprintf(
            "navigator.clipboard.writeText(document.getElementById('%s').innerText)",
            ns("report_text")
          ),
          mp_icon("copy"), " Copy report"
        ),
        actionButton(ns("open_loc"), "Open report location",
                     icon = mp_icon("folder-open"), class = "btn-sm")
      )
    })

    observeEvent(input$open_loc, {
      i <- reactable::getReactableState("tbl", "selected")
      df <- runs()
      req(length(i) == 1, !is.null(df), i <= nrow(df), !is.na(df$report[i]))
      open_path(dirname(df$report[i]))
    })

    # New-report notice ----
    check <- function(extra = NULL) {
      extra <- setdiff(extra, session$userData$run_reports_new)
      new <- c(extra, sync())
      session$userData$run_reports_new <- union(session$userData$run_reports_new, new)
      pending <<- union(pending, new)
      if (!started || length(pending) == 0) return(invisible())
      notice_wf <<- run_reports_notice_workflow(session$userData$mode, pending)
      mp_confirm(
        "notice",
        title = sprintf("New run reports available (%d)", length(pending)),
        text = "Open Run Reports to see how the runs went.",
        action_label = "Open Run Reports",
        cancel_label = "Dismiss",
        session = session
      )
      pending <<- character(0)
    }
    session$userData$run_reports_check <- check

    # Startup check once the first page is sent; held until the next refresh
    # when a startup alert is up (one alert shows at a time)
    started <- FALSE
    session$onFlushed(function() {
      started <<- TRUE
      if (isTRUE(session$userData$startup_alert)) return(invisible())
      withReactiveDomain(session, isolate(check()))
    }, once = TRUE)

    observeEvent(input$notice, {
      if (isTRUE(input$notice)) show_reports(notice_wf)
    })

    on("refresh_assemble", check())
    on("refresh_annotate", check())
  })
}
