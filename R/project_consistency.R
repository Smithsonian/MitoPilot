#' Published assembly output directory for a sample
#'
#' The ASSEMBLE workflow publishes to `out/<ID>/assemble/<assemble_opts>/`
#' (`inst/nextflow/modules/assemble.nf`), so the option-set name doubles as a
#' directory name. Every downstream stage rebuilds this path from the current
#' `assemble.assemble_opts` value.
#'
#' @param dir_out Project output directory (`session$userData$dir_out`).
#' @param ID Sample ID.
#' @param opts Option set name (`assemble.assemble_opts`).
#'
#' @noRd
assemble_out_dir <- function(dir_out, ID, opts) {
  file.path(dir_out, ID, "assemble", opts)
}

#' Option-set directories that actually exist on disk for a sample
#'
#' @param dir_out Project output directory.
#' @param ID Sample ID.
#'
#' @return Character vector of directory names, possibly empty.
#'
#' @noRd
assemble_dirs_on_disk <- function(dir_out, ID) {
  if (length(dir_out) != 1L || is.na(dir_out) || !nzchar(dir_out)) {
    return(character(0))
  }
  base <- file.path(dir_out, ID, "assemble")
  if (!dir.exists(base)) {
    return(character(0))
  }
  found <- basename(list.dirs(base, recursive = FALSE, full.names = TRUE))
  found[!startsWith(found, ".")]
}

#' Samples whose published assembly is not where the database says it is
#'
#' `assemble.assemble_opts` is a mutable pointer that is also the on-disk
#' directory name. Reassigning a sample to a different option set after it has
#' assembled leaves the published output under the old name, so every stage that
#' rebuilds the path (WF2 annotate / curate / ORF, the coverage viewer, the
#' output folder button) points somewhere that was never created.
#'
#' The test matches what WF2 itself dereferences
#' (`inst/nextflow/modules/annotate_workflow.nf`): the per-path assembly FASTA,
#' not merely the directory. Creating an empty directory therefore does not
#' silence this check.
#'
#' @param con An open connection to a project database.
#' @param dir_out Project output directory.
#' @param ids Optional sample IDs to restrict the check to. `NULL` checks the
#'   whole project.
#' @param pending_only Only report samples WF2 would actually try to process,
#'   i.e. locked, with annotation still queued. Set `FALSE` when checking
#'   samples that are about to be locked.
#'
#' @return A data frame with columns `ID`, `assemble_opts`, `expected` and
#'   `on_disk` (comma separated, `""` when nothing was published). Zero rows
#'   when the project is consistent or cannot be checked.
#'
#' @noRd
stale_assemble_dirs <- function(con, dir_out, ids = NULL, pending_only = TRUE) {
  empty <- data.frame(
    ID = character(0),
    assemble_opts = character(0),
    expected = character(0),
    on_disk = character(0),
    stringsAsFactors = FALSE
  )
  if (length(dir_out) != 1L || is.na(dir_out) || !nzchar(dir_out) ||
      !dir.exists(dir_out)) {
    return(empty)
  }
  if (!is.null(ids)) {
    ids <- unique(ids[!is.na(ids)])
    if (length(ids) == 0L) {
      return(empty)
    }
  }
  # Mirrors the WF2 gate: annotate_workflow.nf selects from assemblies joined to
  # assemble and annotate, requiring assemble_lock = 1, ignore = 0 and an
  # annotation that is switched on and unlocked.
  sql <- paste(
    "SELECT DISTINCT b.ID AS ID, b.assemble_opts AS assemble_opts, a.path AS path",
    "FROM assemble b JOIN assemblies a ON a.ID = b.ID",
    "WHERE COALESCE(a.ignore, 0) = 0"
  )
  if (isTRUE(pending_only)) {
    sql <- paste(
      sql,
      "AND b.assemble_lock = 1",
      "AND EXISTS (SELECT 1 FROM annotate an WHERE an.ID = b.ID",
      "AND an.annotate_switch = 1 AND an.annotate_lock = 0)"
    )
  }
  if (!is.null(ids)) {
    sql <- paste0(
      sql, " AND b.ID IN (",
      paste0("'", gsub("'", "''", ids), "'", collapse = ", "), ")"
    )
  }
  rows <- tryCatch(DBI::dbGetQuery(con, sql), error = function(e) NULL)
  if (is.null(rows) || nrow(rows) == 0L) {
    return(empty)
  }
  opts <- as.character(rows$assemble_opts)
  unset <- is.na(opts) | !nzchar(trimws(opts))
  dir <- assemble_out_dir(dir_out, rows$ID, opts)
  fasta <- file.path(dir, paste0(rows$ID, "_assembly_", rows$path, ".fasta"))
  missing <- unset | !file.exists(fasta)
  if (!any(missing)) {
    return(empty)
  }
  rows <- rows[missing, , drop = FALSE]
  opts <- ifelse(unset[missing], NA_character_, opts[missing])
  dir <- ifelse(unset[missing], NA_character_, dir[missing])
  keep <- !duplicated(paste(rows$ID, opts))
  data.frame(
    ID = rows$ID[keep],
    assemble_opts = opts[keep],
    expected = dir[keep],
    on_disk = vapply(
      rows$ID[keep],
      function(id) paste(assemble_dirs_on_disk(dir_out, id), collapse = ", "),
      character(1),
      USE.NAMES = FALSE
    ),
    stringsAsFactors = FALSE
  )
}

#' Bullet list describing stale assembly output, for a sweet alert
#'
#' @param stale Output of [stale_assemble_dirs()].
#' @param max_items Cap on enumerated samples.
#'
#' @noRd
stale_assemble_items <- function(stale, max_items = 10L) {
  shown <- stale[seq_len(min(nrow(stale), max_items)), , drop = FALSE]
  items <- lapply(seq_len(nrow(shown)), function(i) {
    shiny::tags$li(
      shiny::tags$b(shown$ID[i]),
      if (is.na(shown$assemble_opts[i])) {
        " has no assembly parameter set assigned"
      } else {
        list(
          " points at parameter set ",
          shiny::tags$code(shown$assemble_opts[i])
        )
      },
      ", but the assembly output on disk is ",
      if (nzchar(shown$on_disk[i])) {
        shiny::tags$code(shown$on_disk[i])
      } else {
        "missing"
      }
    )
  })
  if (nrow(stale) > nrow(shown)) {
    items <- c(items, list(shiny::tags$li(
      paste0("... and ", nrow(stale) - nrow(shown), " more")
    )))
  }
  items
}
