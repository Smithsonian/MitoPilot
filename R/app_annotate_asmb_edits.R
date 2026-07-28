#' Undoable assembly edits for the Annotate details modal
#'
#' Backs the two controls that rewrite a unit's assembly in place: "Linearize"
#' (rotate a circular molecule and open it) and "Trim unannotated ends" (cut a
#' linear molecule back to its outermost annotated feature). Both snapshot the
#' unit before their first edit so "Restore assembly" can put it back.
#'
#' Tracking follows the Linearize precedent: the `assemblies` row and the unit's
#' on-disk annotate artifacts are rewritten together and an "EDITED:" note goes
#' into `annotate.annotate_notes`, which VALIDATE strips when WF2 regenerates the
#' assembly. Export needs no changes - it reads `assemblies.sequence` through
#' [get_assembly()] and coordinates from `annotations`, so both follow.
#'
#' @noRd

#' DDL for the pre-edit snapshot table.
#' A row exists only while a unit carries an un-restored edit.
#' @noRd
ASSEMBLY_BACKUP_DDL <- "CREATE TABLE IF NOT EXISTS assembly_backup (
  ID TEXT NOT NULL,
  path INTEGER NOT NULL,
  scaffold INTEGER NOT NULL,
  sequence TEXT,
  length INTEGER,
  topology TEXT,
  depth TEXT,
  gc TEXT,
  errors TEXT,
  annotations TEXT,
  coverage TEXT,
  annotate_length INTEGER,
  annotate_topology TEXT,
  annotate_structure TEXT,
  ops TEXT,
  time_stamp INTEGER,
  PRIMARY KEY (ID, path, scaffold)
);"

#' Create the snapshot table if it is missing (idempotent).
#' @noRd
ensure_assembly_backup <- function(con) {
  DBI::dbExecute(con, ASSEMBLY_BACKUP_DDL)
  invisible()
}

#' The edits recorded against this unit, or character(0) if it is unedited.
#'
#' Read-only on purpose, so callers outside the app's write path never create a
#' table as a side effect of asking a question.
#' @noRd
assembly_backup_ops <- function(con, id, path, scaffold) {
  tryCatch({
    if (!DBI::dbExistsTable(con, "assembly_backup")) return(character(0))
    ops <- DBI::dbGetQuery(
      con,
      "SELECT ops FROM assembly_backup WHERE ID = ? AND path = ? AND scaffold = ?",
      params = list(as.character(id), as.integer(path), as.integer(scaffold))
    )$ops
    if (length(ops) == 0) return(character(0))
    strsplit(ops[1] %|NA|% "", "\\+")[[1]]
  }, error = function(e) character(0))
}

#' Does this unit have an edit that could be restored?
#' @noRd
has_assembly_backup <- function(con, id, path, scaffold) {
  length(assembly_backup_ops(con, id, path, scaffold)) > 0
}

#' Path to a unit's annotate-stage assembly FASTA / coverage CSV.
#' @noRd
unit_annotate_file <- function(dir_out, id, path, scaffold, what) {
  file.path(
    dir_out, id, "annotate",
    paste0(id, "_", what, "_", path, "_", scaffold,
           if (what == "assembly") ".fasta" else ".csv")
  )
}

#' Resolve a unit's coverage CSV the way the annotate modal's loader does
#'
#' Per-unit name first, then the legacy single-file fallback. Mirroring the
#' loader matters: if the two disagree, the plot shows one file's depths while an
#' edit rewrites another's.
#' @noRd
resolve_unit_coverage_file <- function(dir_out, id, path, scaffold) {
  f <- unit_annotate_file(dir_out, id, path, scaffold, "coverageStats")
  if (file.exists(f)) return(f)
  alt <- list.files(file.path(dir_out, id, "annotate"),
                    pattern = "coverageStats", full.names = TRUE)
  if (length(alt) > 0) alt[1] else f
}

#' Render one coverage-CSV column into an `assemblies` per-position string
#'
#' Byte-for-byte what VALIDATE writes (`rows.collect{...}.join(' ')`), including
#' the `#` outlier-mask prefixes and the empty slots the GC rolling window leaves
#' at both ends. Rebuilding from the (already trimmed) CSV keeps the two records
#' in step; slicing the stored string could not, because that join is lossy - a
#' run of empty GC cells collapses to one separator, so position is unrecoverable.
#' @noRd
cov_column_string <- function(v) {
  if (is.null(v) || length(v) == 0) return(NA_character_)
  v <- as.character(v)
  v[is.na(v)] <- ""
  paste(v, collapse = " ")
}

#' Slice one of the space-separated per-position strings in `assemblies`
#' (depth / gc / errors). Fallback for units with no coverage CSV on disk.
#'
#' A vector whose element count does not match the sequence has already lost its
#' position mapping, and keeping a full-length one against a shortened sequence
#' would silently mis-register the Assemble coverage view. Blank it instead:
#' absent reads as absent, stale reads as truth.
#' @noRd
slice_cov_string <- function(x, from, to, n) {
  if (is.null(x) || length(x) == 0 || is.na(x) || !nzchar(x)) return(NA_character_)
  v <- strsplit(trimws(x), "\\s+")[[1]]
  if (length(v) != n) return(NA_character_)
  paste(v[from:to], collapse = " ")
}

#' Write the unit's annotate-stage FASTA (the copy Linearize keeps in step).
#' @noRd
write_unit_assembly_fasta <- function(dir_out, id, path, scaffold, seq, topology) {
  fa <- unit_annotate_file(dir_out, id, path, scaffold, "assembly")
  if (!dir.exists(dirname(fa))) return(invisible(NULL))
  ss <- Biostrings::DNAStringSet(as.character(seq))
  names(ss) <- paste0(id, ".", path, ".", scaffold, " ", topology %|NA|% "linear")
  Biostrings::writeXStringSet(ss, fa)
  invisible(fa)
}

#' Snapshot a unit before its first destructive edit
#'
#' Records the sequence, the per-position depth/gc/error strings, every
#' annotation row, the coverage CSV and the denormalized `annotate` fields, so
#' [restore_assembly_unit()] can put all of them back. Only the FIRST edit writes
#' the snapshot; later edits just append their name to `ops`, so linearize then
#' trim then restore returns to the pipeline's own output.
#'
#' @param op Short name of the edit being applied ("linearize", "trim").
#' @noRd
snapshot_assembly_unit <- function(con, id, path, scaffold, dir_out, op) {
  id <- as.character(id); path <- as.integer(path); scaffold <- as.integer(scaffold)
  key <- list(id, path, scaffold)
  ensure_assembly_backup(con)

  ops <- assembly_backup_ops(con, id, path, scaffold)
  if (length(ops) > 0) {
    if (!(op %in% ops)) {
      DBI::dbExecute(
        con,
        "UPDATE assembly_backup SET ops = ? WHERE ID = ? AND path = ? AND scaffold = ?",
        params = list(paste(c(ops, op), collapse = "+"), id, path, scaffold)
      )
    }
    return(invisible(FALSE))
  }

  asm <- DBI::dbGetQuery(
    con,
    "SELECT sequence, length, topology, depth, gc, errors FROM assemblies
     WHERE ID = ? AND path = ? AND scaffold = ?",
    params = key
  )
  ann <- DBI::dbGetQuery(
    con, "SELECT * FROM annotations WHERE ID = ? AND path = ? AND scaffold = ?",
    params = key
  )
  meta <- DBI::dbGetQuery(
    con, "SELECT length, topology, structure FROM annotate
          WHERE ID = ? AND path = ? AND scaffold = ?",
    params = key
  )
  cov_fn <- resolve_unit_coverage_file(dir_out, id, path, scaffold)
  cov <- if (file.exists(cov_fn)) tryCatch(utils::read.csv(cov_fn), error = function(e) NULL) else NULL

  DBI::dbExecute(
    con,
    "INSERT INTO assembly_backup
       (ID, path, scaffold, sequence, length, topology, depth, gc, errors,
        annotations, coverage, annotate_length, annotate_topology,
        annotate_structure, ops, time_stamp)
     VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
    params = list(
      id, path, scaffold, asm$sequence[1], asm$length[1], asm$topology[1],
      asm$depth[1], asm$gc[1], asm$errors[1],
      as.character(jsonlite::toJSON(ann, dataframe = "rows", na = "null")),
      if (!is.null(cov)) as.character(jsonlite::toJSON(cov, dataframe = "rows", na = "null")) else NA_character_,
      if (nrow(meta)) meta$length[1] else NA_integer_,
      if (nrow(meta)) meta$topology[1] else NA_character_,
      if (nrow(meta)) meta$structure[1] else NA_character_,
      op, as.integer(Sys.time())
    )
  )
  invisible(TRUE)
}

#' How much unannotated flank a unit carries
#'
#' Always reports `topology` and `length` when the unit exists, so a caller can
#' gate on topology even for a unit with no feature model yet. `from`/`to` are
#' the outermost annotated positions and `lead`/`trail` the bp outside them; all
#' four are NA when the unit has no annotations.
#'
#' @return list(length, topology, from, to, lead, trail), or NULL if the unit has
#'   no assembly row.
#' @noRd
unannotated_ends <- function(con, id, path, scaffold) {
  key <- list(as.character(id), as.integer(path), as.integer(scaffold))
  asm <- DBI::dbGetQuery(
    con, "SELECT length, topology FROM assemblies WHERE ID = ? AND path = ? AND scaffold = ?",
    params = key
  )
  if (nrow(asm) == 0 || is.na(asm$length[1])) return(NULL)
  n <- as.integer(asm$length[1])
  out <- list(length = n, topology = asm$topology[1],
              from = NA_integer_, to = NA_integer_,
              lead = NA_integer_, trail = NA_integer_)
  sp <- DBI::dbGetQuery(
    con,
    "SELECT MIN(pos1) a, MAX(pos2) b FROM annotations
     WHERE ID = ? AND path = ? AND scaffold = ? AND pos1 > 0",
    params = key
  )
  if (nrow(sp) == 0 || is.na(sp$a[1])) return(out)
  out$from <- as.integer(sp$a[1])
  out$to <- min(as.integer(sp$b[1]), n)
  out$lead <- out$from - 1L
  out$trail <- n - out$to
  out
}

#' Cut a linear unit back to its outermost annotated feature
#'
#' Rewrites `assemblies` (sequence, length, per-position depth/gc/error strings),
#' shifts every live annotation by the same offset, rewrites the unit's
#' annotate-stage FASTA + coverage CSV, drops the now-stale cached reference
#' alignment, and updates `annotate.length`. No feature is ever dropped or cut,
#' so counts, spans and `annotate.structure` are unchanged by construction.
#'
#' @return list(length, removed_lead, removed_trail)
#' @noRd
trim_assembly_ends <- function(con, id, path, scaffold, dir_out) {
  id <- as.character(id); path <- as.integer(path); scaffold <- as.integer(scaffold)
  key <- list(id, path, scaffold)

  asm <- DBI::dbGetQuery(
    con,
    "SELECT sequence, topology, depth, gc, errors FROM assemblies
     WHERE ID = ? AND path = ? AND scaffold = ?",
    params = key
  )
  if (nrow(asm) == 0 || is.na(asm$sequence[1]) || !nzchar(asm$sequence[1])) {
    stop("No sequence on record for unit ", id, ".", path, ".", scaffold, call. = FALSE)
  }
  if (identical(asm$topology[1], "circular")) {
    stop("Circular assemblies cannot be trimmed. Linearize first.", call. = FALSE)
  }
  n <- nchar(asm$sequence[1])

  ends <- unannotated_ends(con, id, path, scaffold)
  if (is.null(ends) || is.na(ends$from)) {
    stop("This unit has no annotations to trim to.", call. = FALSE)
  }
  from <- ends$from
  to <- min(ends$to, n)
  if (from <= 1L && to >= n) {
    stop("The annotations already span the whole assembly; nothing to trim.", call. = FALSE)
  }

  cov_fn <- resolve_unit_coverage_file(dir_out, id, path, scaffold)
  cov <- if (file.exists(cov_fn)) tryCatch(utils::read.csv(cov_fn), error = function(e) NULL) else NULL
  if (!is.null(cov) && nrow(cov) > 0) {
    # A userAsmb unit's CSV concatenates every contig of the path with restarting
    # Positions, so row i is not base i of this scaffold.
    if ("SeqId" %in% names(cov) && dplyr::n_distinct(cov$SeqId) > 1) {
      stop(
        "This unit's coverage file covers more than one contig, so its depth ",
        "track cannot be trimmed with the sequence. Trimming is not supported ",
        "for user-supplied multi-contig assemblies.",
        call. = FALSE
      )
    }
    pos_ok <- "Position" %in% names(cov) &&
      identical(as.integer(cov$Position), seq_len(nrow(cov)))
    if (nrow(cov) != n || !pos_ok) {
      stop(
        "The coverage track (", nrow(cov), " positions) does not line up with ",
        "the assembly (", n, " bp), so trimming would desynchronize them. ",
        "Re-run the annotation workflow for this sample first.",
        call. = FALSE
      )
    }
  }

  snapshot_assembly_unit(con, id, path, scaffold, dir_out, "trim")

  new_seq <- substr(asm$sequence[1], from, to)
  new_len <- nchar(new_seq)

  # Cut the coverage track first: the per-position strings in `assemblies` are
  # rebuilt from it, exactly as VALIDATE builds them.
  if (!is.null(cov) && nrow(cov) > 0) {
    cov <- cov[from:to, , drop = FALSE]
    cov$Position <- seq_len(nrow(cov))
    # quote = "none", na = "" matches every other coverageStats writer; the Groovy
    # parsers in curate/validate split on bare commas and choke otherwise.
    readr::write_csv(cov, cov_fn, quote = "none", na = "")
    new_depth  <- cov_column_string(cov$MeanDepth)
    new_gc     <- cov_column_string(cov$GC)
    new_errors <- cov_column_string(cov$ErrorRate)
  } else {
    new_depth  <- slice_cov_string(asm$depth[1], from, to, n)
    new_gc     <- slice_cov_string(asm$gc[1], from, to, n)
    new_errors <- slice_cov_string(asm$errors[1], from, to, n)
  }

  DBI::dbExecute(
    con,
    "UPDATE assemblies SET sequence = ?, length = ?, depth = ?, gc = ?, errors = ?,
       edited = 1, time_stamp = ? WHERE ID = ? AND path = ? AND scaffold = ?",
    params = list(
      new_seq, new_len, new_depth, new_gc, new_errors,
      as.integer(Sys.time()), id, path, scaffold
    )
  )

  if (from > 1L) {
    ann <- DBI::dbGetQuery(
      con, "SELECT * FROM annotations WHERE ID = ? AND path = ? AND scaffold = ?",
      params = key
    )
    # Shift live features by a constant. Soft-deleted rows (pos1 = pos2 = 0) are
    # tombstones, not coordinates, and must stay at zero.
    idx <- !is.na(ann$pos1) & ann$pos1 > 0
    if (any(idx)) {
      ann$pos1[idx] <- ann$pos1[idx] - from + 1L
      ann$pos2[idx] <- ann$pos2[idx] - from + 1L
      # Delete + reinsert, never UPDATE: pos1 is part of the primary key, so an
      # in-place shift can transiently collide with a same-named sibling feature.
      DBI::dbExecute(
        con, "DELETE FROM annotations WHERE ID = ? AND path = ? AND scaffold = ?",
        params = key
      )
      DBI::dbAppendTable(con, "annotations", ann)
    }
  }

  drop_stale_ref_alignment(con, id, path, scaffold)
  write_unit_assembly_fasta(dir_out, id, path, scaffold, new_seq, asm$topology[1])

  DBI::dbExecute(
    con, "UPDATE annotate SET length = ? WHERE ID = ? AND path = ? AND scaffold = ?",
    params = list(new_len, id, path, scaffold)
  )

  list(length = new_len, removed_lead = from - 1L, removed_trail = n - to)
}

#' Drop a unit's cached reference alignment
#'
#' `blast_ref_alignment` holds a gapped whole-genome alignment computed against
#' the stored sequence; the synteny view projects sample coordinates through it.
#' Any edit to the sequence invalidates it, and nothing in the app rebuilds it,
#' so delete rather than let the panel silently mis-register. WF1's backfill is
#' NOT EXISTS-guarded, so the next pipeline run regenerates it.
#' @noRd
drop_stale_ref_alignment <- function(con, id, path, scaffold) {
  tryCatch(
    DBI::dbExecute(
      con,
      "DELETE FROM blast_ref_alignment WHERE ID = ? AND path = ? AND scaffold = ?",
      params = list(as.character(id), as.integer(path), as.integer(scaffold))
    ),
    error = function(e) NULL
  )
  invisible()
}

#' Undo every in-app edit to a unit's assembly
#'
#' Writes back the snapshot taken before the first edit: sequence, length,
#' topology, the per-position strings, all annotation rows, the coverage CSV, the
#' annotate-stage FASTA and the denormalized `annotate` fields. The snapshot is
#' dropped, so the unit is unedited again.
#'
#' @return (invisibly) list(length, topology, ops)
#' @noRd
restore_assembly_unit <- function(con, id, path, scaffold, dir_out) {
  id <- as.character(id); path <- as.integer(path); scaffold <- as.integer(scaffold)
  key <- list(id, path, scaffold)
  if (!tryCatch(DBI::dbExistsTable(con, "assembly_backup"), error = function(e) FALSE)) {
    stop("This unit has no recorded edit to undo.", call. = FALSE)
  }
  bak <- DBI::dbGetQuery(
    con, "SELECT * FROM assembly_backup WHERE ID = ? AND path = ? AND scaffold = ?",
    params = key
  )
  if (nrow(bak) == 0 || is.na(bak$sequence[1])) {
    stop("This unit has no recorded edit to undo.", call. = FALSE)
  }

  DBI::dbExecute(
    con,
    "UPDATE assemblies SET sequence = ?, length = ?, topology = ?, depth = ?, gc = ?,
       errors = ?, time_stamp = ? WHERE ID = ? AND path = ? AND scaffold = ?",
    params = list(
      bak$sequence[1], bak$length[1], bak$topology[1],
      bak$depth[1], bak$gc[1], bak$errors[1],
      as.integer(Sys.time()), id, path, scaffold
    )
  )

  DBI::dbExecute(
    con, "DELETE FROM annotations WHERE ID = ? AND path = ? AND scaffold = ?",
    params = key
  )
  if (!is.na(bak$annotations[1]) && nzchar(bak$annotations[1])) {
    ann <- jsonlite::fromJSON(bak$annotations[1])
    if (is.data.frame(ann) && nrow(ann) > 0) DBI::dbAppendTable(con, "annotations", ann)
  }

  cov_fn <- resolve_unit_coverage_file(dir_out, id, path, scaffold)
  if (!is.na(bak$coverage[1]) && nzchar(bak$coverage[1]) && dir.exists(dirname(cov_fn))) {
    cov <- jsonlite::fromJSON(bak$coverage[1])
    if (is.data.frame(cov) && nrow(cov) > 0) {
      readr::write_csv(cov, cov_fn, quote = "none", na = "")
    }
  }

  write_unit_assembly_fasta(dir_out, id, path, scaffold, bak$sequence[1], bak$topology[1])
  drop_stale_ref_alignment(con, id, path, scaffold)

  DBI::dbExecute(
    con,
    "UPDATE annotate SET length = ?, topology = ?, structure = ?
     WHERE ID = ? AND path = ? AND scaffold = ?",
    params = list(bak$annotate_length[1], bak$annotate_topology[1],
                  bak$annotate_structure[1], id, path, scaffold)
  )

  DBI::dbExecute(
    con, "DELETE FROM assembly_backup WHERE ID = ? AND path = ? AND scaffold = ?",
    params = key
  )
  invisible(list(
    length = bak$length[1],
    topology = bak$topology[1],
    annotate_topology = bak$annotate_topology[1],
    ops = strsplit(bak$ops[1] %|NA|% "", "\\+")[[1]]
  ))
}
