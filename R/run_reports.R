#' Run result labels, keyed by the `result` field of a run report
#' @noRd
RUN_RESULTS <- c(
  finished   = "Finished",
  failures   = "Finished with failures",
  failed     = "Failed",
  stopped    = "Stopped",
  unfinished = "Running / no finish recorded"
)

#' Fields of a run report's metadata (`.dcf`), in order
#' @noRd
RUN_META_FIELDS <- c("base", "workflow", "entry", "run_name", "started", "finished",
  "duration", "result", "exit_status", "n_samples", "n_failed_samples", "n_failed_tasks",
  "succeeded", "cached", "retried", "launch", "log", "mitopilot_version", "nextflow_version")

#' A project's run folder, or one of its subfolders
#'
#' @param sub "jobs", "logs", "nextflow", "reports", or NULL for `.runs/` itself.
#' @noRd
run_dir <- function(project, sub = NULL) {
  if (is.null(sub)) file.path(project, ".runs") else file.path(project, ".runs", sub)
}

#' Name shared by one run's script, logs, and report: `<workflow>_<YYYY-mm-dd_HH-MM-SS>`
#' @noRd
run_basename <- function(workflow, time = Sys.time()) {
  paste0(tolower(workflow[1]), "_", format(time, "%Y-%m-%d_%H-%M-%S"))
}

#' Run base name from its Nextflow log path
#' @noRd
run_log_base <- function(log) sub("\\.nextflow\\.log$", "", basename(log))

#' Start time encoded in a run base name
#' @noRd
run_base_time <- function(base) {
  as.POSIXct(sub("^[^_]+_", "", base), format = "%Y-%m-%d_%H-%M-%S")
}

#' Parse Nextflow log time stamps ("Sep-03 17:45:37.478"), which carry no year
#'
#' The year is taken from `ref` (the log's modification time), minus one when
#' that would put the stamp in the future.
#' @noRd
nf_log_time <- function(stamp, ref = Sys.time()) {
  if (length(stamp) == 0) return(as.POSIXct(character(0)))
  mon <- match(substr(stamp, 1, 3), month.abb)
  yr <- as.integer(format(ref, "%Y"))
  mk <- function(y) as.POSIXct(sprintf("%d-%02d-%s", y, mon, substr(stamp, 5, 15)),
    format = "%Y-%m-%d %H:%M:%S")
  t <- mk(yr)
  late <- !is.na(t) & t > ref + 86400
  t[late] <- mk(yr - 1L)[late]
  t
}

#' Likely cause of a task exit status
#' @noRd
nf_exit_cause <- function(code) {
  vapply(as.character(code), function(x) switch(x,
    "104" = , "247" = "likely out of memory",
    "125" = "the container could not start (image missing, or a folder could not be mounted)",
    "126" = "a command could not be run (permission denied)",
    "127" = "a command was not found (a tool is missing from the container or PATH)",
    "130" = "interrupted",
    "134" = "the tool aborted, often out of memory",
    "137" = "killed: out of memory, or stopped by the system",
    "139" = "the tool crashed (segmentation fault), often out of memory",
    "140" = "killed by the scheduler: usually over its time or memory limit",
    "143" = "terminated by the scheduler or the system, often over its time or memory limit",
    "-" = , "NA" = "no exit status: the task was stopped before it finished",
    "the tool stopped with an error"), "", USE.NAMES = FALSE)
}

#' Last non-empty lines of a task's stderr
#' @noRd
task_err_tail <- function(dir, n = 1L) {
  f <- file.path(dir, ".command.err")
  if (!file.exists(f)) return("")
  y <- tryCatch(readLines(f, warn = FALSE), error = function(e) character(0))
  y <- trimws(cli::ansi_strip(iconv(y, "UTF-8", "UTF-8", sub = "byte")))
  y <- y[nzchar(y)]
  if (length(y) == 0) "" else paste(utils::tail(y, n), collapse = " ")
}

#' Sample ID a task tag belongs to
#'
#' Tags are `<ID>`, `<ID>.<n>`, `<ID>.<path>.<scaffold>` or
#' `<ID>.<path>.<scaffold>.<accession>`. With `ids`, the longest known ID the
#' tag starts with wins; otherwise the numeric suffixes are stripped.
#' @noRd
tag_sample <- function(tag, ids = NULL) {
  out <- sub("(\\.\\d+){1,2}(\\.[A-Za-z].*)?$", "", tag)
  if (length(ids)) {
    ids <- ids[order(-nchar(ids))]
    hit <- vapply(tag, function(t) {
      m <- ids[t == ids | startsWith(t, paste0(ids, "."))]
      if (length(m)) m[1] else NA_character_
    }, "", USE.NAMES = FALSE)
    out <- ifelse(is.na(hit), out, hit)
  }
  out
}

#' What one Nextflow run did, read from its log
#'
#' @param log The run's Nextflow log.
#' @param ids Known sample IDs, to map task tags to samples.
#' @return list(run, version, entry, error, fatal, finished, started, ended,
#'   stats, tasks, samples, readable). `tasks` has one row per task whose last
#'   attempt failed: process, step, item (the task tag), sample, exit, tries,
#'   workdir, cause, stderr. `finished` is TRUE when the log records an end
#'   (Goodbye, or a fatal error).
#' @noRd
nf_log_facts <- function(log, ids = NULL) {
  x <- tryCatch(if (file.exists(log)) readLines(log, warn = FALSE) else character(0),
    error = function(e) character(0))
  x <- iconv(x, "UTF-8", "UTF-8", sub = "byte")
  x[is.na(x)] <- ""
  has <- function(k) x[grepl(k, x, fixed = TRUE)]
  grab <- function(rx, s = x) {
    m <- regmatches(s, regexec(rx, s, perl = TRUE))
    vapply(m, function(v) if (length(v) > 1) v[2] else NA_character_, "")
  }
  first <- function(v) if (any(!is.na(v))) v[!is.na(v)][1] else NA_character_
  last <- function(v) if (any(!is.na(v))) utils::tail(v[!is.na(v)], 1) else NA_character_

  aborted <- first(grab("Session aborted -- Cause: (.*)$", has("Session aborted")))
  errs <- stats::na.omit(grab("\\] ERROR +\\S+ - (.*)$", has("] ERROR")))
  errs <- errs[errs != "@unknown"]
  err <- if (!is.na(aborted)) aborted else if (length(errs)) errs[1] else NA_character_
  fatal <- !is.na(aborted) || any(grepl("\\] ERROR +nextflow\\.cli\\.Launcher - ", has("] ERROR")))

  st <- last(grab("WorkflowStats\\[(.*?)\\]", has("WorkflowStats[")))
  stats <- if (is.na(st)) integer(0) else {
    kv <- regmatches(st, gregexpr("(\\w+)Count=(\\d+)", st, perl = TRUE))[[1]]
    stats::setNames(as.integer(sub(".*=", "", kv)), sub("Count=.*", "", kv))
  }

  th <- grep("Task completed > \\w*TaskHandler\\[", has("Task completed > "), value = TRUE)
  att <- data.frame(name = grab("name: (.*?); status:", th), exit = trimws(grab("exit: (.*?);", th)),
    workdir = trimws(grab("workDir: ([^\\]\\s]+)", th)), stringsAsFactors = FALSE)
  note <- stats::na.omit(grab("NOTE: (.*) -- Error is ignored$", has("-- Error is ignored")))
  killed <- stats::na.omit(grab("Error executing process > '(.*)'$", has("Error executing process")))
  name <- c(grab("[Pp]rocess `([^`]+)`", note), killed)
  msg <- c(note, rep("stopped the run", length(killed)))
  keep <- !is.na(name) & !duplicated(name, fromLast = TRUE)
  name <- name[keep]; msg <- msg[keep]
  lst <- att[vapply(name, function(n) utils::tail(c(NA_integer_, which(att$name == n)), 1L),
    0L, USE.NAMES = FALSE), , drop = FALSE]
  process <- sub(" \\(.*$", "", name)
  item <- ifelse(grepl("\\(", name), sub("^.*\\((.*)\\)$", "\\1", name), "-")
  tasks <- data.frame(process = process, step = sub("^.*:", "", process), item = item,
    sample = ifelse(item == "-", "-", tag_sample(item, ids)),
    exit = ifelse(is.na(lst$exit), "-", lst$exit),
    tries = vapply(name, function(n) sum(att$name == n), 0L, USE.NAMES = FALSE),
    workdir = ifelse(is.na(lst$workdir), "-", lst$workdir),
    cause = ifelse(is.na(lst$exit) | lst$exit %in% c("0", "-"), msg, nf_exit_cause(lst$exit)),
    stringsAsFactors = FALSE)
  tasks$stderr <- vapply(tasks$workdir, task_err_tail, "", USE.NAMES = FALSE)

  names_all <- c(grab("(?:Submitted|Cached) process > (.*)$", has("process > ")), att$name)
  tags <- unique(stats::na.omit(grab("\\(([^()]+)\\)$", names_all)))
  tags <- tags[!grepl("^\\d+$", tags)]

  stamp <- substr(x, 1, 15)
  stamp <- stamp[grepl("^[A-Z][a-z]{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}$", stamp)]
  ref <- if (file.exists(log)) file.mtime(log) else Sys.time()
  tm <- nf_log_time(stamp, ref)
  tm <- tm[!is.na(tm)]

  list(run = first(grab("Run name: (\\S+)", has("Run name: "))),
    version = first(grab("N E X T F L O W\\s+~\\s+version (\\S+)", has("N E X T F L O W"))),
    entry = first(grab("\\$> nextflow .*-entry (\\S+)", has("$> nextflow"))),
    error = err, fatal = fatal,
    finished = fatal || any(grepl("Execution complete -- Goodbye", x, fixed = TRUE)),
    started = if (length(tm)) min(tm) else as.POSIXct(NA),
    ended = if (length(tm)) max(tm) else as.POSIXct(NA),
    stats = stats, tasks = tasks,
    samples = sort(unique(tag_sample(tags, ids))),
    readable = length(tm) > 0)
}

#' Exit status a submit script echoed into its scheduler log, or NA
#' @noRd
read_exit_trailer <- function(project, base) {
  logs <- list.files(run_dir(project, "logs"), pattern = paste0("^", base), full.names = TRUE)
  x <- unlist(lapply(logs, function(f) tryCatch(readLines(f, warn = FALSE),
    error = function(e) character(0))))
  s <- regmatches(x, regexec("\\[MitoPilot\\] nextflow exited with status (-?\\d+)", x))
  s <- stats::na.omit(unlist(lapply(s, `[`, 2)))
  if (length(s)) as.integer(utils::tail(s, 1)) else NA_integer_
}

#' Samples (annotate: units) a run left Failed, with their notes
#'
#' Matches rows whose `time_stamp` (epoch seconds of the run start, written by
#' the pipeline) falls in the run's window. Empty when the table has no
#' `time_stamp` column, or on any error.
#' @param since,until Epoch seconds (or POSIXct) bounding the run.
#' @return data.frame(ID, note)
#' @noRd
run_failed_samples <- function(con, workflow, since, until = NA) {
  empty <- data.frame(ID = character(0), note = character(0), stringsAsFactors = FALSE)
  if (is.null(con) || length(since) == 0 || is.na(since)) return(empty)
  tryCatch({
    tbl <- match.arg(workflow, c("assemble", "annotate"))
    cols <- DBI::dbListFields(con, tbl)
    sw <- paste0(tbl, "_switch")
    nt <- paste0(tbl, "_notes")
    if (!all(c("time_stamp", sw) %in% cols)) return(empty)
    key <- if (tbl == "annotate" && all(c("path", "scaffold") %in% cols))
      "ID || '.' || path || '.' || scaffold" else "ID"
    note <- if (nt %in% cols) sprintf("COALESCE(%s, '')", nt) else "''"
    until <- if (is.na(until)) 4e9 else as.numeric(until) + 1
    DBI::dbGetQuery(con, sprintf(
      "SELECT %s AS ID, %s AS note FROM %s WHERE %s = 3 AND time_stamp >= ? AND time_stamp <= ? ORDER BY ID",
      key, note, tbl, sw), params = list(floor(as.numeric(since)), until))
  }, error = function(e) empty)
}

#' Duration as "1h 2m 3s"
#' @noRd
fmt_duration <- function(secs) {
  if (length(secs) == 0 || is.na(secs)) return(NA_character_)
  s <- round(as.numeric(secs))
  h <- s %/% 3600; m <- (s %% 3600) %/% 60; s <- s %% 60
  paste(c(if (h) paste0(h, "h"), if (h || m) paste0(m, "m"), paste0(s, "s")), collapse = " ")
}

#' Report on one Nextflow run
#'
#' @param project Project directory.
#' @param log The run's Nextflow log, `.runs/nextflow/<base>.nextflow.log`.
#' @param exit_status Nextflow's exit status; NULL reads the scheduler log
#'   trailer, else infers it from the log (fatal error = 1, finish marker = 0).
#' @param how "stopped" when the user stopped the run, else NULL.
#' @param con Project DB connection, for failed samples; NULL opens the
#'   project `.sqlite` read-only when present.
#' @return list(meta, text): `meta` is a named list with RUN_META_FIELDS,
#'   `text` the plain-text report.
#' @noRd
run_report <- function(project, log, exit_status = NULL, how = NULL, con = NULL) {
  base <- run_log_base(log)
  workflow <- sub("_.*$", "", base)
  if (is.null(con)) {
    db <- file.path(project, ".sqlite")
    con <- if (file.exists(db)) tryCatch(
      DBI::dbConnect(RSQLite::SQLite(), db, flags = RSQLite::SQLITE_RO),
      error = function(e) NULL)
    if (!is.null(con)) on.exit(DBI::dbDisconnect(con), add = TRUE)
  }
  ids <- if (!is.null(con)) tryCatch(DBI::dbGetQuery(con, "SELECT DISTINCT ID FROM assemble")$ID,
    error = function(e) NULL)
  f <- tryCatch(nf_log_facts(log, ids), error = function(e) nf_log_facts(tempfile()))

  status <- exit_status %||% read_exit_trailer(project, base)
  if (is.na(status) && f$finished) status <- if (f$fatal) 1L else 0L
  status <- as.integer(status)
  started <- if (is.na(f$started)) run_base_time(base) else f$started
  bad <- run_failed_samples(con, workflow, started, f$ended)
  n_t <- nrow(f$tasks); n_s <- nrow(bad)
  cnt <- function(k) sum(f$stats[k], na.rm = TRUE)

  result <- if (identical(how, "stopped")) "stopped" else
    if ((!is.na(status) && status != 0L) || f$fatal) "failed" else
    if (n_t + n_s > 0) "failures" else "finished"
  fmt <- function(t) if (is.na(t)) NA_character_ else format(t, "%Y-%m-%d %H:%M:%S")
  mp_ver <- tryCatch(as.character(utils::packageVersion("MitoPilot")), error = function(e) NA_character_)

  meta <- list(
    base = base, workflow = workflow,
    entry = f$entry %|NA|% (if (workflow == "annotate") "WF2" else "WF1"),
    run_name = f$run, started = fmt(started), finished = fmt(f$ended),
    duration = fmt_duration(difftime(f$ended, started, units = "secs")),
    result = result, exit_status = status,
    n_samples = length(f$samples), n_failed_samples = n_s, n_failed_tasks = n_t,
    succeeded = cnt("succeeded"), cached = cnt("cached"), retried = cnt("retries"),
    launch = if (file.exists(file.path(run_dir(project, "jobs"), paste0(base, ".sh")))) "job" else "app",
    log = log, mitopilot_version = mp_ver, nextflow_version = f$version)

  tasks <- if (n_t == 0) character(0) else c("",
    "Failed tasks (sample | step | exit | likely cause | last error | work dir):",
    sprintf("%s | %s | %s | %s%s | %s | %s", f$tasks$sample, f$tasks$step, f$tasks$exit,
      f$tasks$cause, ifelse(f$tasks$tries > 1, sprintf("; tried %d times", f$tasks$tries), ""),
      ifelse(nzchar(f$tasks$stderr), f$tasks$stderr, "-"), f$tasks$workdir))
  samples <- if (n_s == 0) character(0) else c("",
    "Failed samples (sample | note):",
    sprintf("%s | %s", bad$ID, ifelse(nzchar(bad$note), bad$note, "-")))
  text <- c("MitoPilot run report",
    paste("Project:  ", project),
    sprintf("Workflow:  %s (%s), run %s", workflow, meta$entry, f$run %|NA|% "unnamed"),
    paste("Started:  ", meta$started %|NA|% "unknown"),
    sprintf("Finished:  %s%s", meta$finished %|NA|% "unknown",
      if (is.na(meta$duration)) "" else sprintf(" (%s)", meta$duration)),
    sprintf("Result:    %s, exit status %s", RUN_RESULTS[[result]], status %|NA|% "unknown"),
    paste("Launched: ", if (meta$launch == "job") "as a job (saved script)" else "from the app"),
    paste("MitoPilot:", mp_ver %|NA|% "unknown"),
    paste("Nextflow: ", f$version %|NA|% "unknown"),
    paste0("Log:       ", log, if (!f$readable) " (could not be read)"),
    if (length(f$stats)) sprintf(
      "Tasks:     %d succeeded, %d cached, %d failed and skipped, %d retried, %d aborted",
      cnt("succeeded"), cnt("cached"), cnt("ignored"), cnt("retries"), cnt("aborted")),
    paste("Samples:  ", length(f$samples)),
    if (!is.na(f$error)) paste("Error:    ", f$error),
    tasks, samples)
  list(meta = meta, text = paste(text, collapse = "\n"))
}

#' Paths of a run's report files
#' @noRd
run_report_paths <- function(project, base) {
  d <- run_dir(project, "reports")
  c(txt = file.path(d, paste0(base, "_report.txt")), dcf = file.path(d, paste0(base, "_report.dcf")))
}

#' Write a run report's text and metadata files
#' @return The report paths, invisibly.
#' @noRd
write_run_report <- function(project, rep) {
  p <- run_report_paths(project, rep$meta$base)
  dir.create(dirname(p[["txt"]]), recursive = TRUE, showWarnings = FALSE)
  writeLines(rep$text, p[["txt"]])
  meta <- lapply(rep$meta, function(v) if (length(v) == 0) NA else v)
  write.dcf(as.data.frame(meta, stringsAsFactors = FALSE), p[["dcf"]], width = 10000)
  invisible(p)
}

#' TRUE when a report metadata file exists and parses
#' @noRd
dcf_ok <- function(path) {
  file.exists(path) && isTRUE(tryCatch(nrow(read.dcf(path)) > 0, error = function(e) FALSE))
}

#' Last `bytes` of a file as lines
#' @noRd
read_tail <- function(path, bytes = 20000) {
  con <- file(path, "rb")
  on.exit(close(con))
  seek(con, max(0, file.size(path) - bytes))
  readLines(con, warn = FALSE)
}

#' Has a run recorded its end?
#'
#' Job scripts with the exit trailer count only once the trailer is logged, so
#' the real exit status is known. Otherwise the Nextflow log end is used.
#' @noRd
run_is_finished <- function(project, base, log) {
  if (!is.na(read_exit_trailer(project, base))) return(TRUE)
  job <- file.path(run_dir(project, "jobs"), paste0(base, ".sh"))
  if (file.exists(job) && any(grepl("nextflow exited with status", readLines(job, warn = FALSE), fixed = TRUE))) {
    return(FALSE)
  }
  x <- read_tail(log)
  any(grepl("Execution complete -- Goodbye|Session aborted -- Cause:|\\] ERROR +nextflow\\.cli\\.Launcher - ", x))
}

#' Write reports for finished runs that have none yet
#'
#' A run is finished when its scheduler log carries the exit trailer, or its
#' Nextflow log records an end.
#' @return Bases written (character(0) when none, or no `.runs/`).
#' @noRd
sync_run_reports <- function(project, con = NULL, skip = NULL) {
  logs <- list.files(run_dir(project, "nextflow"), pattern = "\\.nextflow\\.log$", full.names = TRUE)
  out <- character(0)
  for (log in logs) {
    base <- run_log_base(log)
    if (base %in% skip || dcf_ok(run_report_paths(project, base)[["dcf"]])) next
    ok <- tryCatch({
      if (!run_is_finished(project, base, log)) FALSE else {
        write_run_report(project, run_report(project, log, con = con))
        TRUE
      }
    }, error = function(e) FALSE)
    if (ok) out <- c(out, base)
  }
  out
}

#' A panel's runs, newest first
#'
#' From report metadata plus Nextflow logs that have no report yet (result
#' "unfinished").
#' @param workflow "assemble", "annotate", or NULL for both.
#' @return data.frame with RUN_META_FIELDS plus `report` (path of the text
#'   report, NA when unfinished). Counts and exit_status are integers.
#' @noRd
list_run_reports <- function(project, workflow = NULL) {
  dcf <- list.files(run_dir(project, "reports"), pattern = "_report\\.dcf$", full.names = TRUE)
  rows <- lapply(dcf, function(f) tryCatch({
    m <- read.dcf(f, fields = RUN_META_FIELDS)
    r <- as.data.frame(m, stringsAsFactors = FALSE)
    r$report <- sub("\\.dcf$", ".txt", f)
    r
  }, error = function(e) NULL))
  done <- vapply(rows, function(r) if (is.null(r)) "" else r$base, "")
  logs <- list.files(run_dir(project, "nextflow"), pattern = "\\.nextflow\\.log$", full.names = TRUE)
  logs <- logs[!run_log_base(logs) %in% done]
  open <- lapply(logs, function(log) {
    base <- run_log_base(log)
    r <- as.data.frame(stats::setNames(as.list(rep(NA_character_, length(RUN_META_FIELDS))),
      RUN_META_FIELDS), stringsAsFactors = FALSE)
    t <- run_base_time(base)
    r$base <- base; r$workflow <- sub("_.*$", "", base); r$result <- "unfinished"
    r$started <- if (is.na(t)) NA_character_ else format(t, "%Y-%m-%d %H:%M:%S")
    r$launch <- if (file.exists(file.path(run_dir(project, "jobs"), paste0(base, ".sh")))) "job" else "app"
    r$log <- log; r$report <- NA_character_
    r
  })
  out <- do.call(rbind, c(Filter(Negate(is.null), rows), open))
  if (is.null(out)) {
    out <- as.data.frame(stats::setNames(rep(list(character(0)), length(RUN_META_FIELDS) + 1),
      c(RUN_META_FIELDS, "report")), stringsAsFactors = FALSE)
  }
  num <- c("exit_status", "n_samples", "n_failed_samples", "n_failed_tasks", "succeeded", "cached", "retried")
  out[num] <- lapply(out[num], function(v) suppressWarnings(as.integer(v)))
  if (!is.null(workflow)) out <- out[out$workflow %in% tolower(workflow), , drop = FALSE]
  out <- out[order(sub("^[^_]+_", "", out$base), decreasing = TRUE), , drop = FALSE]
  rownames(out) <- NULL
  out
}
