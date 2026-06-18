#' Write curated assembly sequence to the assemblies table
#'
#' Reconstructs the curated assembly sequence (and per-position depth, GC and
#' error-rate strings) from a curate-stage `coverageStats` CSV and writes it to
#' the `assemblies` table in a single transaction. The UPDATE is verified: every
#' scaffold row must match exactly one row, otherwise the transaction is rolled
#' back and the call errors. Run as the final step of the curate process so the
#' sequence is committed (in the same frame as the annotation coordinates the
#' VALIDATE stage writes) before the sample can advance.
#'
#' Replaces the fire-and-forget nf-sqldb `.sqlInsert` write, whose deferred batch
#' commit could be lost on a failed run while annotations still committed,
#' leaving the two tables in different rotation frames.
#'
#' @param db_path Path to the project `.sqlite` database
#' @param coverage_fn Path to the curate-stage `coverageStats` CSV
#' @param time_stamp Timestamp string to record on the updated rows
#'
#' @export
#'
write_curated_assembly <- function(db_path, coverage_fn, time_stamp) {
  cov <- readr::read_csv(coverage_fn, show_col_types = FALSE)

  # One row per scaffold; preserves position order for sequence reconstruction.
  rows <- cov |>
    dplyr::group_by(SeqId) |>
    dplyr::summarise(
      sequence = paste0(Call, collapse = ""),
      depth    = paste0(MeanDepth, collapse = " "),
      gc       = paste0(GC, collapse = " "),
      errors   = paste0(ErrorRate, collapse = " "),
      .groups = "drop"
    ) |>
    dplyr::mutate(length = nchar(sequence))

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "PRAGMA busy_timeout = 30000")

  sql <- paste(
    "UPDATE assemblies SET sequence = ?, length = ?, depth = ?, gc = ?,",
    "errors = ?, time_stamp = ? WHERE ID = ? AND path = ? AND scaffold = ?"
  )

  DBI::dbBegin(con)
  ok <- TRUE
  for (i in seq_len(nrow(rows))) {
    # SeqId is "ID.path.scaffold"
    key <- strsplit(rows$SeqId[i], ".", fixed = TRUE)[[1]]
    n <- DBI::dbExecute(con, sql, params = list(
      rows$sequence[i], rows$length[i], rows$depth[i], rows$gc[i],
      rows$errors[i], time_stamp, key[1], key[2], key[3]
    ))
    if (n != 1L) {
      ok <- FALSE
      message(sprintf(
        "assemblies UPDATE matched %d rows (expected 1) for SeqId '%s'",
        n, rows$SeqId[i]
      ))
    }
  }

  if (!ok) {
    DBI::dbRollback(con)
    stop("Curated assembly write failed: a scaffold key did not match exactly ",
         "one assemblies row. Rolled back to avoid annotation/assembly desync.")
  }
  DBI::dbCommit(con)

  invisible(nrow(rows))
}
