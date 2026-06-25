#' Sample work-directory browser (UI)
#'
#' Fixed bottom-right trigger button that opens a popup listing every Nextflow task
#' work directory for a chosen sample.
#'
#' @import shiny
#' @noRd
workdir_browser_ui <- function(id) {
  ns <- NS(id)
  div(
    style = "margin-left: auto;",
    shinyWidgets::actionBttn(
      ns("open_browser"),
      label = "Work Dirs",
      icon = icon("folder-tree"),
      style = "material-flat",
      size = "sm"
    )
  )
}

#' Sample work-directory browser (server)
#'
#' Lists the Nextflow task work directories for a chosen sample, parsed from the
#' project's `.logs/nextflow.log*` files. Captures both successful and failed task
#' attempts (each retry has its own work directory), so failed processes can be
#' inspected. Each row shows a success/failed status icon, a copy button, and an
#' environment-aware open button (see [open_path()]).
#'
#' @import shiny
#' @noRd
workdir_browser_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    rows <- reactiveVal(NULL)

    observeEvent(input$open_browser, {
      ids <- tryCatch(
        DBI::dbGetQuery(session$userData$con, "SELECT ID FROM samples ORDER BY ID")$ID,
        error = function(e) character(0)
      )
      # Pre-select the sample currently highlighted in the active mode's table
      # (first one if several are selected).
      mode <- session$userData$mode %||% "Assemble"
      presel <- session$userData$wd_selected[[mode]]
      presel <- if (length(presel) >= 1 && presel[1] %in% ids) presel[1] else NULL
      showModal(modalDialog(
        title = "Sample Work Directories",
        size = "l",
        easyClose = TRUE,
        tags$p(
          class = "text-muted",
          style = "font-size: 12px; margin-bottom: 0.75em;",
          "Each pipeline step creates working directories to hold intermediate files and logs. Use these to troubleshoot a failed process ",
          "or to inspect intermediate outputs. Failed attempts are listed too (see Status)."
        ),
        shinyWidgets::pickerInput(
          ns("sample"),
          label = "Sample",
          choices = ids,
          selected = presel,
          options = list(`live-search` = TRUE)
        ),
        div(style = "max-height: 60vh; overflow: auto;", uiOutput(ns("tbl"))),
        footer = modalButton("Close")
      ))
    })

    observeEvent(input$sample, {
      rows(find_workdirs(session$userData$dir, input$sample))
    })

    output$tbl <- renderUI({
      df <- rows()
      if (is.null(df) || nrow(df) == 0) {
        return(tags$em("No work directories found for this sample (has the pipeline run?)."))
      }
      body <- lapply(seq_len(nrow(df)), function(i) {
        tags$tr(
          tags$td(df$process[i]),
          tags$td(style = "text-align: center;", workdir_status_icon(df$status[i])),
          tags$td(df$param_set[i]),
          tags$td(style = "white-space: nowrap;", df$modified[i]),
          tags$td(
            tags$code(
              style = sprintf(
                "font-size: 11px; word-break: break-all;%s",
                if (!df$exists[i]) " color: #9e9e9e;" else ""
              ),
              df$workdir[i]
            ),
            if (!df$exists[i]) {
              tags$span(
                style = "color: #c62828; font-size: 11px; margin-left: 0.4em;",
                title = "Not reachable from this host (e.g. purged or node-local scratch)",
                "(missing)"
              )
            }
          ),
          tags$td(
            style = "white-space: nowrap;",
            rclipboard::rclipButton(
              ns(paste0("clip", i)),
              label = NULL,
              clipText = df$workdir[i],
              icon = icon("copy"),
              modal = TRUE,
              class = "btn-xs",
              title = "Copy path"
            ),
            if (df$exists[i]) {
              tags$button(
                type = "button",
                class = "btn btn-default btn-xs",
                title = "Open",
                onclick = sprintf(
                  "Shiny.setInputValue('%s', %d, {priority: 'event'})",
                  ns("open_row"), i
                ),
                icon("folder-open")
              )
            } else {
              tags$button(
                type = "button",
                class = "btn btn-default btn-xs",
                disabled = NA,
                title = "Not reachable from this host",
                icon("folder-open")
              )
            }
          )
        )
      })
      tags$table(
        class = "table table-striped table-condensed",
        style = "font-size: 13px;",
        tags$thead(tags$tr(
          tags$th("Process"), tags$th("Status"), tags$th("Param set"),
          tags$th("Modified"), tags$th("Work directory"), tags$th("")
        )),
        tags$tbody(body)
      )
    })

    observeEvent(input$open_row, {
      df <- rows()
      i <- as.integer(input$open_row)
      req(!is.null(df), i >= 1, i <= nrow(df))
      open_path(df$workdir[i])
    })
  })
}

#' Compact status icon for a work-directory row
#'
#' @noRd
workdir_status_icon <- function(status) {
  if (identical(status, "success")) {
    tags$span(
      style = "color: #2e7d32;", title = "Completed successfully",
      icon("circle-check")
    )
  } else {
    tags$span(
      style = "color: #c62828;", title = "Failed (non-zero exit)",
      icon("triangle-exclamation")
    )
  }
}

#' Enumerate Nextflow task work directories for a sample
#'
#' Parses the project's `.logs/nextflow.log*` files for completed-task records, which
#' Nextflow writes for every task attempt (success or failure), each with its own work
#' directory. This is the only source that captures failed processes, since failed tasks
#' are never published to the output tree.
#'
#' Status is taken from the task exit code (0 = success, otherwise failed). The param set
#' (opts_id) is recovered best-effort from the `<sample>/assemble/<opts_id>` directory the
#' process creates inside its work directory (NA / "-" for processes without one, or when
#' the work directory cannot be read on this host).
#'
#' Work directories are listed straight from the log and are NOT dropped when they don't
#' resolve on the current host: on a cluster the path may live on node-local or purged
#' scratch the app host can't see, yet the path is still worth copying. The `exists` column
#' flags whether the dir is reachable here; the UI disables Open for unreachable rows.
#'
#' Purely native bookkeeping tasks (e.g. `write_curated_result`, which writes the .sqlite
#' driver-side) are omitted: their work dirs hold nothing to inspect, and as native tasks
#' they have no OS exit code so the exit-based status check would mis-flag them as failed.
#'
#' @param project_dir Project root (the directory holding `.logs/`); `session$userData$dir`.
#' @param sample_id Sample ID to filter on.
#' @return data.frame with columns process, param_set, status, modified, workdir, exists
#'   (possibly 0 rows)
#' @noRd
find_workdirs <- function(project_dir, sample_id) {
  empty <- data.frame(
    process = character(0), param_set = character(0), status = character(0),
    modified = character(0), workdir = character(0), exists = logical(0),
    stringsAsFactors = FALSE
  )
  if (is.null(project_dir) || is.null(sample_id) || !nzchar(sample_id)) return(empty)
  logs <- list.files(
    file.path(project_dir, ".logs"),
    pattern = "^nextflow\\.log", full.names = TRUE
  )
  if (length(logs) == 0) return(empty)
  lines <- unlist(lapply(logs, function(f) {
    tryCatch(readLines(f, warn = FALSE), error = function(e) character(0))
  }))
  th <- grep("Task completed > TaskHandler\\[", lines, value = TRUE)
  if (length(th) == 0) return(empty)

  name    <- stringr::str_match(th, "name:\\s*(.*?);")[, 2]
  exit    <- stringr::str_match(th, "exit:\\s*(.*?);")[, 2]
  # Stop at first whitespace or "]": grid executors (e.g. SGE) append
  # " started: ...; exited: ...; " after the path before the closing "]".
  workdir <- stringr::str_match(th, "workDir:\\s*([^\\]\\s]+)")[, 2]
  # name is e.g. "WF1:ASSEMBLE:assemble (sample)"; tag may carry a ".<path_idx>" suffix
  nm      <- stringr::str_match(name, "([^:\\s]+)\\s*\\(([^)]+)\\)\\s*$")
  process <- nm[, 2]
  sample  <- sub("\\.[0-9]+$", "", nm[, 3])

  # Native bookkeeping tasks with no inspectable work dir (and no OS exit code).
  exclude_processes <- c("write_curated_result")
  keep <- !is.na(workdir) & !is.na(process) & sample == sample_id &
          !process %in% exclude_processes
  if (!any(keep)) return(empty)
  out <- data.frame(
    process = process[keep], sample = sample[keep], exit = exit[keep],
    workdir = trimws(workdir[keep]), stringsAsFactors = FALSE
  )
  out <- out[!duplicated(out$workdir), , drop = FALSE]
  out$status <- ifelse(out$exit == "0", "success", "failed")
  # Whether the work dir resolves on THIS host. Do not drop missing ones: on a cluster
  # the path may be on node-local / purged scratch the app host can't see, but the logged
  # path is still useful to copy. Missing rows are flagged and their Open is disabled.
  out$exists <- dir.exists(out$workdir)
  out$mtime <- file.info(out$workdir)$mtime  # NA when missing
  out$modified <- ifelse(is.na(out$mtime), "-", format(out$mtime, "%Y-%m-%d %H:%M"))
  # Param set (opts_id) only recoverable when the dir exists locally; "-" otherwise.
  out$param_set <- vapply(seq_len(nrow(out)), function(i) {
    if (!out$exists[i]) return("-")
    g <- tryCatch(
      Sys.glob(file.path(out$workdir[i], out$sample[i], "assemble", "*")),
      error = function(e) character(0)
    )
    g <- g[dir.exists(g)]
    if (length(g) >= 1) basename(g[1]) else "-"
  }, character(1))
  # Newest first; missing dirs (NA mtime) sort to the bottom.
  out <- out[order(out$mtime, decreasing = TRUE, na.last = TRUE), , drop = FALSE]
  rownames(out) <- NULL
  out[, c("process", "param_set", "status", "modified", "workdir", "exists")]
}
