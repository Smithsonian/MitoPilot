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
      showModal(modalDialog(
        title = "Sample Work Directories",
        size = "l",
        easyClose = TRUE,
        shinyWidgets::pickerInput(
          ns("sample"),
          label = "Sample",
          choices = ids,
          options = list(`live-search` = TRUE)
        ),
        uiOutput(ns("tbl")),
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
          tags$td(tags$code(df$workdir[i])),
          tags$td(
            style = "white-space: nowrap;",
            rclipboard::rclipButton(
              ns(paste0("clip", i)),
              label = "Copy",
              clipText = df$workdir[i],
              icon = icon("copy"),
              modal = TRUE
            ),
            tags$button(
              type = "button",
              class = "btn btn-default btn-sm",
              onclick = sprintf(
                "Shiny.setInputValue('%s', %d, {priority: 'event'})",
                ns("open_row"), i
              ),
              icon("folder-open"), " Open"
            )
          )
        )
      })
      tags$table(
        class = "table table-striped",
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
#' the work directory has been cleaned).
#'
#' @param project_dir Project root (the directory holding `.logs/`); `session$userData$dir`.
#' @param sample_id Sample ID to filter on.
#' @return data.frame with columns process, param_set, status, workdir (possibly 0 rows)
#' @noRd
find_workdirs <- function(project_dir, sample_id) {
  empty <- data.frame(
    process = character(0), param_set = character(0),
    status = character(0), workdir = character(0), stringsAsFactors = FALSE
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
  workdir <- stringr::str_match(th, "workDir:\\s*([^\\]]+)\\]")[, 2]
  # name is e.g. "WF1:ASSEMBLE:assemble (sample)"; tag may carry a ".<path_idx>" suffix
  nm      <- stringr::str_match(name, "([^:\\s]+)\\s*\\(([^)]+)\\)\\s*$")
  process <- nm[, 2]
  sample  <- sub("\\.[0-9]+$", "", nm[, 3])

  keep <- !is.na(workdir) & !is.na(process) & sample == sample_id
  if (!any(keep)) return(empty)
  out <- data.frame(
    process = process[keep], sample = sample[keep], exit = exit[keep],
    workdir = trimws(workdir[keep]), stringsAsFactors = FALSE
  )
  out <- out[!duplicated(out$workdir), , drop = FALSE]
  # Reruns spread the same process across log files, each with its own work dir. Keep only
  # dirs that still exist on disk (cleaned / overwritten reruns drop out), so the list
  # reflects what is actually inspectable.
  out <- out[dir.exists(out$workdir), , drop = FALSE]
  if (nrow(out) == 0) return(empty)
  out$status <- ifelse(out$exit == "0", "success", "failed")
  out$mtime <- file.info(out$workdir)$mtime
  out$modified <- format(out$mtime, "%Y-%m-%d %H:%M")
  out$param_set <- vapply(seq_len(nrow(out)), function(i) {
    g <- tryCatch(
      Sys.glob(file.path(out$workdir[i], out$sample[i], "assemble", "*")),
      error = function(e) character(0)
    )
    g <- g[dir.exists(g)]
    if (length(g) >= 1) basename(g[1]) else "-"
  }, character(1))
  # Newest first so the most recent run for each process is at the top.
  out <- out[order(out$mtime, decreasing = TRUE), , drop = FALSE]
  rownames(out) <- NULL
  out[, c("process", "param_set", "status", "modified", "workdir")]
}
