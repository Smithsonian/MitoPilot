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
        div(style = "max-height: 60vh; overflow: auto;", reactable::reactableOutput(ns("tbl"))),
        footer = modalButton("Close")
      ))
    })

    observeEvent(input$sample, {
      rows(find_workdirs(session$userData$dir, input$sample))
    })

    output$tbl <- reactable::renderReactable({
      df <- rows()
      if (is.null(df)) df <- df[0, , drop = FALSE]
      # Pre-render the per-row Actions cell in R, where `exists` is known: copy
      # button always, Open enabled only for reachable dirs, plus a "(missing)"
      # marker otherwise. The row index encoded in Open is the data-frame index,
      # so sorting/filtering the table cannot misroute the click.
      actions <- vapply(seq_len(nrow(df)), function(i) {
        copy_btn <- sprintf(
          paste0(
            "<button class='btn btn-default btn-xs' title='Copy path' ",
            "onclick=\"navigator.clipboard.writeText('%s')\">",
            "<i class='fa fa-copy'></i></button>"
          ),
          gsub("'", "\\\\'", df$workdir[i])
        )
        open_btn <- if (df$exists[i]) {
          sprintf(
            paste0(
              "<button class='btn btn-default btn-xs' title='Open' ",
              "onclick=\"Shiny.setInputValue('%s', %d, {priority: 'event'})\">",
              "<i class='fa fa-folder-open'></i></button>"
            ),
            ns("open_row"), i
          )
        } else {
          paste0(
            "<button class='btn btn-default btn-xs' disabled ",
            "title='Not reachable from this host'><i class='fa fa-folder-open'></i></button>",
            "<span style='color:#c62828; font-size:11px; margin-left:0.4em;' ",
            "title='Not reachable from this host (e.g. purged or node-local scratch)'>(missing)</span>"
          )
        }
        paste0("<span style='white-space:nowrap;'>", copy_btn, " ", open_btn, "</span>")
      }, character(1))

      disp <- data.frame(
        Process = df$process,
        Status = df$status,
        `Param set` = df$param_set,
        Modified = df$modified,
        `Work directory` = df$workdir,
        Actions = actions,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )

      reactable::reactable(
        disp,
        filterable = TRUE,
        sortable = TRUE,
        resizable = TRUE,
        defaultPageSize = 25,
        compact = TRUE,
        highlight = TRUE,
        wrap = FALSE,
        language = reactable::reactableLang(
          noData = "No work directories found for this sample (has the pipeline run?)."
        ),
        columns = list(
          Process = reactable::colDef(maxWidth = 160),
          `Param set` = reactable::colDef(maxWidth = 120),
          Status = reactable::colDef(
            cell = htmlwidgets::JS(
              "function(cellInfo) {
                if (cellInfo.value === 'success') {
                  return `<span style='color:#2e7d32;' title='Completed successfully'><i class='fa fa-circle-check'></i> success</span>`
                }
                return `<span style='color:#c62828;' title='Failed (non-zero exit)'><i class='fa fa-triangle-exclamation'></i> failed</span>`
              }"
            ),
            html = TRUE,
            maxWidth = 110
          ),
          `Work directory` = reactable::colDef(
            cell = htmlwidgets::JS(
              "function(cellInfo) {
                var v = cellInfo.value ? cellInfo.value : ''
                return `<code style='font-size:11px; word-break:break-all;'>${v}</code>`
              }"
            ),
            html = TRUE,
            minWidth = 220
          ),
          Actions = reactable::colDef(
            name = "",
            html = TRUE,
            sortable = FALSE,
            filterable = FALSE,
            resizable = FALSE,
            maxWidth = 110
          )
        )
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
