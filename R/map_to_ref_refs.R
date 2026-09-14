# Per-sample MapToRef references: classify, validate, ingest, and resolve.
# The same classifier runs on the user's machine at ingest and inside the
# container at run time, so it never consults the filesystem.

# NCBI nucleotide accession. Keep identical to isMaptorefAccession() in
# inst/nextflow/modules/assemble_workflow.nf.
.mtr_acc_re <- "^[A-Za-z]{1,2}_?[0-9]{5,9}(\\.[0-9]{1,3})?$"

# The value is interpolated into an Rscript -e call inside a bash double-quoted
# string (inst/nextflow/modules/assemble.nf, the MapToRef Rscript -e line): a
# quote ends the R expression, and $, a backtick, and a backslash are acted on
# by bash before R sees them.
.mtr_bad_chars_re <- "[\"'$`\\\\]"

#' @noRd
.mtr_ref_class <- function(x) {
  v <- trimws(.mtr_opts(x))
  if (!nzchar(v)) return("none")
  if (grepl("^(https?|ftp)://", v, ignore.case = TRUE)) return("url")
  if (grepl(.mtr_acc_re, v)) return("accession")
  "file"
}

# Pure parse of an esummary 200 body, so the one piece of new response-handling
# in this feature is testable without a network. Measured shapes: a hit carries
# result.<uid>.caption and .accessionversion; an "Empty id list" body has an
# esummaryresult key and no result, and is a definitive miss, not an unreadable
# response.
#' @noRd
.mtr_esummary_found <- function(txt) {
  j <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE),
                error = function(e) NULL)
  if (is.null(j) || !is.list(j)) return(NULL)
  if (!is.null(j$esummaryresult)) return(character(0))
  if (is.null(j$result)) return(NULL)
  hits <- as.character(unlist(lapply(
    j$result[names(j$result) %nin% "uids"],
    function(r) c(r$accessionversion, r$caption)
  )))
  unique(toupper(sub("\\.[0-9]+$", "", hits[nzchar(hits)])))
}

# Batched existence check. Deliberately not .blast_ref_efetch(): that helper
# sleeps 120s * attempt for five attempts, which is right inside the pipeline
# and wrong at an interactive validation boundary. esummary over efetch because
# efetch returns HTTP 200 with a garbled error body for a bad id.
# The top-level "error" field names only ONE bad id even when several are bad,
# so it is never parsed; the found set is diffed against the requested set.
#' @noRd
.mtr_ncbi_known <- function(accs, timeout = 30L) {
  found <- character(0)
  # A few hundred ids overflow what E-utilities accepts in a GET query string,
  # and the resulting non-200 would silently downgrade to a warning.
  for (chunk in split(accs, ceiling(seq_along(accs) / 200L))) {
    url <- paste0(
      "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi",
      "?db=nuccore&retmode=json&id=", paste(chunk, collapse = ","),
      .blast_ref_api_key_qs()
    )
    resp <- tryCatch(
      httr2::request(url) |>
        httr2::req_timeout(timeout) |>
        httr2::req_error(is_error = function(r) FALSE) |>
        httr2::req_perform(),
      error = function(e) e
    )
    if (inherits(resp, "error")) {
      return(list(ok = FALSE, reason = conditionMessage(resp)))
    }
    if (httr2::resp_status(resp) != 200L) {
      return(list(ok = FALSE, reason = paste0("HTTP ", httr2::resp_status(resp))))
    }
    hits <- .mtr_esummary_found(httr2::resp_body_string(resp))
    if (is.null(hits)) {
      return(list(ok = FALSE, reason = "unreadable esummary response"))
    }
    found <- c(found, hits)
  }
  list(ok = TRUE, found = found)
}

#' @noRd
.mtr_url_fetch <- function(url, timeout = 60L) {
  dest <- tempfile("mtrurl")
  # req_perform(path=) writes the body to dest even on a 404, so a plate of dead
  # URLs would otherwise leave one file per row behind in tempdir().
  ok <- FALSE
  on.exit(if (!ok) unlink(dest), add = TRUE)
  resp <- tryCatch(
    httr2::request(url) |>
      httr2::req_timeout(timeout) |>
      httr2::req_error(is_error = function(r) FALSE) |>
      httr2::req_perform(path = dest),
    error = function(e) e
  )
  if (inherits(resp, "error")) {
    return(list(ok = FALSE, reason = conditionMessage(resp)))
  }
  if (httr2::resp_status(resp) != 200L) {
    return(list(ok = FALSE, reason = paste0("HTTP ", httr2::resp_status(resp))))
  }
  ok <- TRUE
  list(ok = TRUE, file = dest)
}

# The real reader is the content check: it is the only thing that catches a
# multi-record database, an HTML error page, or a nuclear contig. The circular
# placeholder keeps the FASTA-topology rule from firing during validation.
#' @noRd
.mtr_content_problem <- function(file) {
  d <- tempfile("mtrchk")
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  tryCatch(
    {
      maptoref_prepare_ref(file, topology = "circular", out_dir = d)
      NA_character_
    },
    error = function(e) conditionMessage(e)
  )
}

# Returns the normalised value or a reason. Two channels, never one, so a
# failure message can never be mistaken for a value and written to the database.
#' @noRd
.mtr_check_ref_value <- function(v, cls) {
  if (identical(cls, "url")) {
    if (grepl("^ftp://", v, ignore.case = TRUE)) {
      return(list(ok = FALSE, msg = paste0(
        "ftp:// references cannot be checked; use the https:// form of the ",
        "same URL, or download the file and give its path"
      )))
    }
    got <- .mtr_url_fetch(v)
    if (!isTRUE(got$ok)) {
      return(list(ok = FALSE, msg = paste0("not reachable: ", got$reason)))
    }
    prob <- .mtr_content_problem(got$file)
    unlink(got$file)
    if (!is.na(prob)) {
      return(list(ok = FALSE, msg = paste0(
        "downloaded, but is not a usable reference: ", prob
      )))
    }
    return(list(ok = TRUE, value = v))
  }
  # file.exists() is TRUE for a directory on Linux, so test that separately.
  if (dir.exists(v)) return(list(ok = FALSE, msg = "is a directory, not a file"))
  if (!file.exists(v)) return(list(ok = FALSE, msg = "file not found"))
  if (file.access(v, 4L) != 0L) {
    return(list(ok = FALSE, msg = "file is not readable"))
  }
  p <- normalizePath(v, winslash = "/", mustWork = FALSE)
  prob <- .mtr_content_problem(p)
  if (!is.na(prob)) return(list(ok = FALSE, msg = prob))
  list(ok = TRUE, value = p)
}

#' @noRd
.mtr_validate_refs <- function(x, ids, context = "reference") {
  s <- trimws(as.character(x))
  s[is.na(s)] <- ""
  n <- length(s)
  ids <- as.character(ids)
  if (length(ids) != n) stop("ids must be the same length as x", call. = FALSE)
  out <- rep(NA_character_, n)
  # Indexed by row, not appended: the checks below run in class batches, so
  # appending would interleave the report of a 96-sample plate.
  bad <- rep(NA_character_, n)
  add <- function(i, msg) {
    bad[i] <<- sprintf("  %s [%s]: %s",
                       ids[i], gsub("\\s+", " ", s[i]), gsub("\\s+", " ", msg))
  }

  bad_chars <- which(grepl(.mtr_bad_chars_re, s))
  for (i in bad_chars) {
    add(i, paste("quote, dollar, backtick, and backslash characters are not",
                 "allowed in a reference value"))
  }

  cls <- vapply(s, .mtr_ref_class, character(1), USE.NAMES = FALSE)

  ai <- setdiff(which(cls == "accession"), bad_chars)
  if (length(ai) > 0L) {
    s[ai] <- toupper(s[ai])
    known <- .mtr_ncbi_known(unique(s[ai]))
    if (!isTRUE(known$ok)) {
      warning("Could not check ", length(unique(s[ai])), " accession(s) against ",
              "NCBI (", gsub("\\s+", " ", known$reason), "); they will be resolved ",
              "when the pipeline runs.", call. = FALSE)
      out[ai] <- s[ai]
    } else {
      miss <- sub("\\.[0-9]+$", "", s[ai]) %nin% known$found
      for (i in ai[miss]) add(i, "no such nucleotide record at NCBI")
      out[ai[!miss]] <- s[ai[!miss]]
    }
  }

  # Distinct values are checked once; the results map back to every row that
  # names them, so a plate with three references makes three checks.
  fi <- setdiff(which(cls %in% c("url", "file")), bad_chars)
  seen <- list()
  for (i in fi) {
    key <- s[i]
    if (is.null(seen[[key]])) seen[[key]] <- .mtr_check_ref_value(key, cls[i])
    res <- seen[[key]]
    if (isTRUE(res$ok)) out[i] <- res$value else add(i, res$msg)
  }

  bad <- bad[!is.na(bad)]
  if (length(bad) > 0L) {
    stop(sprintf("MapToRef reference problems (%d) in %s:\n%s",
                 length(bad), context, paste(bad, collapse = "\n")),
         call. = FALSE)
  }
  out
}

# Second pass over what .mtr_validate_refs() returned. Only a FASTA (a file or
# URL not named .gb/.gbk/.gbff) needs a topology; GenBank and accessions carry
# their own. Same report shape as .mtr_validate_refs().
#' @noRd
.mtr_validate_ref_topology <- function(vals, topology, ids, context = "reference") {
  n <- length(vals)
  topology <- tolower(trimws(as.character(topology)))
  topology[is.na(topology)] <- ""
  if (length(topology) != n) stop("topology must be the same length as vals", call. = FALSE)
  ids <- as.character(ids)
  out <- rep(NA_character_, n)
  bad <- character(0)
  for (i in seq_len(n)) {
    if (is.na(vals[i])) next
    needs <- .mtr_needs_topology(vals[i])
    if (nzchar(topology[i]) && topology[i] %nin% c("circular", "linear")) {
      bad <- c(bad, sprintf("  %s [%s]: topology must be circular or linear, not %s",
                            ids[i], vals[i], topology[i]))
    } else if (needs && !nzchar(topology[i])) {
      bad <- c(bad, sprintf("  %s [%s]: FASTA reference needs a topology (circular or linear)",
                            ids[i], vals[i]))
    } else if (needs) {
      out[i] <- topology[i]
    }
  }
  if (length(bad) > 0L) {
    stop(sprintf("MapToRef reference topology problems (%d) in %s:\n%s",
                 length(bad), context, paste(bad, collapse = "\n")),
         call. = FALSE)
  }
  out
}

# Strip the optional Reference and Reference_topology columns out of a mapping
# before the samples table is built from colnames(mapping). Precedent:
# R/init_db_userAsmb.R strips Assembly/Topology the same way. Values are
# returned raw; callers validate.
#' @noRd
.mtr_take_ref_col <- function(mapping, mapping_id = "ID") {
  has_ref <- "Reference" %in% colnames(mapping)
  has_topo <- "Reference_topology" %in% colnames(mapping)
  if (has_topo && !has_ref) {
    stop("The mapping file has a Reference_topology column but no Reference column",
         call. = FALSE)
  }
  if (!has_ref) {
    return(list(mapping = mapping, refs = NULL, topology = NULL))
  }
  ids <- as.character(mapping[[mapping_id]])
  refs <- stats::setNames(as.character(mapping[["Reference"]]), ids)
  topology <- if (has_topo) {
    stats::setNames(as.character(mapping[["Reference_topology"]]), ids)
  } else {
    stats::setNames(rep(NA_character_, length(ids)), ids)
  }
  keep <- setdiff(colnames(mapping), c("Reference", "Reference_topology"))
  list(mapping = mapping[, keep, drop = FALSE], refs = refs, topology = topology)
}

# The sample's own set, when it has one: named for the sample, used by nobody
# else, and assembling with MapToRef. set_maptoref_refs() edits that set instead
# of the column, so the reference keeps living in exactly one place.
#' @noRd
.mtr_dedicated_opts <- function(con, ids) {
  q <- DBI::dbGetQuery(con, paste(
    "SELECT a.ID, a.assemble_opts, o.assembler, o.maptoref_ref,",
    "o.maptoref_topology,",
    "(SELECT COUNT(*) FROM assemble x",
    "WHERE x.assemble_opts = a.assemble_opts) AS n_samples",
    "FROM assemble a JOIN assemble_opts o",
    "ON a.assemble_opts = o.assemble_opts"
  ))
  q <- q[match(ids, q$ID), , drop = FALSE]
  own <- !is.na(q$ID) &
    q$assemble_opts == .mtr_opts_name(q$ID) &
    q$n_samples == 1L &
    !is.na(q$assembler) & q$assembler == "MapToRef"
  own[is.na(own)] <- FALSE
  list(own = own, opts = q$assemble_opts, set_ref = q$maptoref_ref,
       topology = q$maptoref_topology)
}

#' A FASTA reference carries no topology, so its set must name one.
#' @noRd
.mtr_needs_topology <- function(x) {
  v <- trimws(.mtr_opts(x))
  nzchar(v) && !identical(.mtr_ref_class(v), "accession") &&
    !grepl("\\.(gb|gbk|gbff)$", v, ignore.case = TRUE)
}

#' Comparable form of a reference: a file by its normalised path, anything
#' else as typed.
#' @noRd
.mtr_ref_key <- function(x) {
  v <- trimws(.mtr_opts(x))
  if (identical(.mtr_ref_class(v), "file")) {
    v <- normalizePath(v, winslash = "/", mustWork = FALSE)
  }
  v
}

#' Write references to their one home, the sample's own parameter set
#'
#' A sample on its own set has the value written there; any other sample gets
#' a set cloned from the one it is on. The assemble column is cleared for every
#' sample touched, so nothing can shadow the set.
#'
#' @param con database connection
#' @param ids sample IDs
#' @param vals references, NA to clear
#' @noRd
.mtr_route_refs <- function(con, ids, vals) {
  if (length(ids) == 0L) return(invisible(character(0)))
  ded <- .mtr_dedicated_opts(con, ids)
  own <- ded$own
  # Every check runs before any write, so a refusal leaves the database as it
  # was (the migration relies on that to keep an unfoldable column value).
  orphan <- !is.na(vals) & is.na(ded$opts)
  if (any(orphan)) {
    stop("sample(s) ", paste(shQuote(ids[orphan]), collapse = ", "),
         " point at no existing assemble options set; fix that in the ",
         "Assemble module first", call. = FALSE)
  }
  needs <- !is.na(vals) & vapply(vals, .mtr_needs_topology, logical(1)) &
    (is.na(ded$topology) | !nzchar(trimws(ded$topology)))
  if (any(needs)) {
    warning("sample(s) ", paste(shQuote(ids[needs]), collapse = ", "),
            " get a FASTA reference on a parameter set with no topology; set ",
            "the reference topology (circular or linear) on ",
            paste(shQuote(unique(ifelse(own, ded$opts, .mtr_opts_name(ids))[needs])),
                  collapse = ", "),
            " in the Assemble options before running, or the sample will ",
            "fail at the Assemble step", call. = FALSE)
  }
  if (any(own)) {
    dplyr::tbl(con, "assemble_opts") |>
      dplyr::rows_update(
        data.frame(assemble_opts = ded$opts[own], maptoref_ref = vals[own]),
        unmatched = "ignore",
        in_place = TRUE,
        copy = TRUE,
        by = "assemble_opts"
      )
  }
  # A blank for a sample without a set of its own has nothing to clear.
  new <- which(!own & !is.na(vals))
  for (base in unique(ded$opts[new])) {
    i <- new[ded$opts[new] == base]
    .mtr_seed_per_sample_opts(con, stats::setNames(vals[i], ids[i]), base = base)
  }
  dplyr::tbl(con, "assemble") |>
    dplyr::rows_update(
      data.frame(ID = ids, maptoref_ref = NA_character_),
      unmatched = "ignore",
      in_place = TRUE,
      copy = TRUE,
      by = "ID"
    )
  invisible(ids)
}

#' Fold per-sample reference column values into per-sample sets (migration)
#'
#' A value that cannot be moved (its set name is taken by a set the sample
#' does not own) is left on the column, where the pipeline still reads it.
#'
#' @param con database connection
#' @return invisibly, the IDs folded
#' @noRd
.mtr_fold_override_column <- function(con) {
  if ("assembler" %nin% DBI::dbListFields(con, "assemble_opts") ||
      "maptoref_ref" %nin% DBI::dbListFields(con, "assemble")) {
    return(invisible(character(0)))
  }
  cur <- DBI::dbGetQuery(con, paste(
    "SELECT ID, maptoref_ref FROM assemble",
    "WHERE maptoref_ref IS NOT NULL AND TRIM(maptoref_ref) <> ''"
  ))
  done <- character(0)
  for (i in seq_len(nrow(cur))) {
    ok <- tryCatch({
      .mtr_route_refs(con, cur$ID[i], cur$maptoref_ref[i])
      TRUE
    }, error = function(e) {
      message("kept the MapToRef reference of ", cur$ID[i],
              " on the assemble table: ", conditionMessage(e))
      FALSE
    })
    if (ok) done <- c(done, cur$ID[i])
  }
  if (length(done) > 0L) {
    message("moved the MapToRef reference of ", length(done),
            " sample(s) onto their own parameter set")
  }
  invisible(done)
}

#' The reference the next run of a sample would use
#' @noRd
.mtr_ref_now <- function(con, id) {
  if ("maptoref_ref" %nin% DBI::dbListFields(con, "assemble")) {
    return(NA_character_)
  }
  v <- DBI::dbGetQuery(
    con, "SELECT NULLIF(TRIM(maptoref_ref), '') AS ref FROM assemble WHERE ID = ?",
    params = list(id)
  )$ref
  if (length(v) != 1L || is.na(v)) NA_character_ else v
}

# A reference stored under a set that does not assemble with MapToRef is not
# an error: the user may switch the set later. Say so once.
#' @noRd
.mtr_warn_refs_ignored <- function(con, ids) {
  if (length(ids) == 0L ||
      "assembler" %nin% DBI::dbListFields(con, "assemble_opts")) {
    return(invisible(character(0)))
  }
  q <- DBI::dbGetQuery(con, paste(
    "SELECT a.ID FROM assemble a JOIN assemble_opts o",
    "ON a.assemble_opts = o.assemble_opts",
    "WHERE o.assembler <> 'MapToRef'"
  ))$ID
  hit <- intersect(ids, q)
  if (length(hit) > 0L) {
    warning(length(hit), " sample(s) have a MapToRef reference but are on a ",
            "parameter set that does not assemble with MapToRef; the reference ",
            "is stored and ignored until the set is switched to MapToRef.",
            call. = FALSE)
  }
  invisible(hit)
}

# R8's warning, answered from the database rather than from the mapping file, so
# it sees both sources. The COALESCE is the same expression the pipeline uses in
# inst/nextflow/modules/assemble_workflow.nf.
#' @noRd
.mtr_warn_missing_refs <- function(con) {
  # userAsmb projects have a minimal assemble_opts with no assembler column.
  if ("assembler" %nin% DBI::dbListFields(con, "assemble_opts")) {
    return(invisible(character(0)))
  }
  # A project that has not been migrated yet has no a.maptoref_ref to select.
  # The loud failure belongs to the caller, not to a warning helper.
  if ("maptoref_ref" %nin% DBI::dbListFields(con, "assemble")) {
    return(invisible(character(0)))
  }
  ids <- DBI::dbGetQuery(con, paste(
    "SELECT a.ID FROM assemble a",
    "JOIN assemble_opts o ON a.assemble_opts = o.assemble_opts",
    "WHERE o.assembler = 'MapToRef'",
    "AND NULLIF(TRIM(a.maptoref_ref), '') IS NULL"
  ))$ID
  if (length(ids) > 0L) {
    warning("MapToRef has no reference for ", length(ids), " sample(s): ",
            paste(utils::head(ids, 10L), collapse = ", "),
            if (length(ids) > 10L) paste0(" and ", length(ids) - 10L, " more") else "",
            ". Those samples will fail at the Assemble step. Set a reference per ",
            "sample with MitoPilot::set_maptoref_refs() or by clicking the sample's ",
            "MapToRef ref cell in the Assemble table.", call. = FALSE)
  }
  invisible(ids)
}

#' Set per-sample MapToRef references
#'
#' Assigns a MapToRef reference mitogenome to individual samples in an existing
#' project. A reference has one home: the sample's own parameter set, named
#' \code{<ID>_maptoref}. A sample that already has one (used by no other sample,
#' assembling with MapToRef) has the reference written onto it; any other sample
#' gets one, cloned from the set it is on, with the assembler switched to
#' MapToRef. A blank value clears the reference on the sample's own set. The
#' Assemble options modal therefore always shows the reference the next run
#' will use.
#'
#' Samples whose reference actually changes are queued for (re-)assembly, the
#' same way changing a sample's parameter set does in the Assemble module.
#'
#' @param path Path to the project directory (default = current working
#'   directory)
#' @param refs A CSV path or a data frame. The first column holds sample IDs and
#'   the second holds references; column names are ignored, but a CSV must have
#'   a header row (its first line is not read as data). Any further columns are
#'   ignored. A reference is an absolute file path to a
#'   single-record GenBank or FASTA mitogenome, a URL, or an NCBI nucleotide
#'   accession (for example NC_002333). Blank clears the sample's reference.
#'
#' @return Invisibly, the IDs that still have no reference from either source.
#' @export
#'
set_maptoref_refs <- function(path = ".", refs = NULL) {
  if (!dir.exists(path)) {
    stop("Project directory does not exist")
  }
  path <- normalizePath(path)
  db <- file.path(path, ".sqlite")
  if (!file.exists(db)) {
    stop("No MitoPilot database found in ", path)
  }

  if (is.character(refs) && length(refs) == 1L) {
    # file.exists() is TRUE for a directory on Linux, and read.csv() on one
    # fails with a cryptic internal message.
    if (!file.exists(refs) || dir.exists(refs)) {
      stop("refs CSV not found: ", refs)
    }
    refs <- utils::read.csv(refs)
  }
  if (!is.data.frame(refs) || ncol(refs) < 2L || nrow(refs) == 0L) {
    stop("refs must be a CSV path or a data frame with at least two columns ",
         "(sample ID, reference) and at least one row")
  }

  ids <- trimws(as.character(refs[[1]]))
  vals <- as.character(refs[[2]])
  if (any(duplicated(ids))) {
    stop("Duplicate IDs in refs: ",
         paste(unique(ids[duplicated(ids)]), collapse = ", "))
  }

  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = db)
  on.exit(DBI::dbDisconnect(con))
  if ("maptoref_ref" %nin% DBI::dbListFields(con, "assemble")) {
    stop("This project predates the per-sample MapToRef reference column; run ",
         "MitoPilot::backwards_compatibility() first")
  }
  cur <- DBI::dbGetQuery(con, "SELECT ID, maptoref_ref, assemble_lock FROM assemble")

  unknown <- setdiff(ids, cur$ID)
  if (length(unknown) > 0L) {
    stop("sample(s) ", paste(shQuote(unknown), collapse = ", "),
         " absent in the existing database")
  }
  new_vals <- .mtr_validate_refs(vals, ids = ids, context = "the reference list")
  # The reference in force today: a value still on the assemble column (from
  # before the fold) wins in the pipeline, otherwise the set's value.
  ded <- .mtr_dedicated_opts(con, ids)
  old_vals <- cur$maptoref_ref[match(ids, cur$ID)]
  old_vals[is.na(old_vals)] <- ded$set_ref[is.na(old_vals)]
  same <- (is.na(new_vals) & is.na(old_vals)) |
    (!is.na(new_vals) & !is.na(old_vals) & new_vals == old_vals)
  changed <- which(!same)
  # Only rows that would be written: a locked row this call leaves alone is not
  # an edit, and writing without flipping assemble_switch would be a no-op.
  locked <- intersect(
    ids[changed],
    cur$ID[!is.na(cur$assemble_lock) & cur$assemble_lock == 1]
  )
  if (length(locked) > 0L) {
    stop("sample(s) ", paste(shQuote(locked), collapse = ", "),
         " are locked; unlock them in the Assemble module first")
  }
  if (length(changed) == 0L) {
    message("No changes: every sample already had that reference.")
    return(invisible(.mtr_warn_missing_refs(con)))
  }

  .mtr_route_refs(con, ids[changed], new_vals[changed])
  # Same requeue the app makes when a sample's parameter set changes
  # (R/app_assemble.R).
  dplyr::tbl(con, "assemble") |>
    dplyr::rows_update(
      data.frame(ID = ids[changed], assemble_switch = 1),
      unmatched = "ignore",
      in_place = TRUE,
      copy = TRUE,
      by = "ID"
    )
  message("Updated ", length(changed), " sample(s); ",
          length(ids) - length(changed), " already had that reference.")
  invisible(.mtr_warn_missing_refs(con))
}

#' @noRd
.mtr_log_if <- function(log_fn, ...) {
  if (!is.null(log_fn) && nzchar(.mtr_opts(log_fn))) {
    .mtr_log(log_fn, ...)
  }
  invisible(NULL)
}

# rettype=gb, not fasta: the LOCUS line carries the topology, so an NCBI-sourced
# reference needs no topology guess. Its own function so tests can mock the seam.
#' @noRd
.mtr_efetch_gb <- function(acc) {
  url <- paste0(
    "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi",
    "?db=nuccore&id=", utils::URLencode(acc, reserved = TRUE),
    "&rettype=gb&retmode=text", .blast_ref_api_key_qs()
  )
  httr2::resp_body_string(.blast_ref_efetch(url, 120L, paste0("gb/", acc)))
}

#' Resolve an NCBI accession to a MapToRef reference file
#'
#' Downloads the GenBank record for the accession from NCBI, so the reference
#' carries its own topology.
#'
#' @param accession An NCBI nucleotide accession, with or without its version
#'   (for example NC_002333 or NC_002333.1). Case insensitive.
#' @param out_dir Directory to write into; the file is placed in
#'   \code{<out_dir>/maptoref/}.
#' @param log_fn Optional path to an assembler log file to append to.
#'
#' @return A list with \code{file}, \code{source} ("ncbi"), and the uppercased
#'   \code{accession} that was resolved. Stops when NCBI has no record.
#' @export
#'
maptoref_fetch_accession <- function(accession, out_dir = ".", log_fn = NULL) {
  acc <- toupper(trimws(.mtr_opts(accession)))
  if (!nzchar(acc)) {
    stop("No accession given")
  }
  work <- file.path(out_dir, "maptoref")
  dir.create(work, recursive = TRUE, showWarnings = FALSE)

  .mtr_log_if(log_fn, "fetching accession ", acc, " from NCBI")
  txt <- tryCatch(.mtr_efetch_gb(acc), error = function(e) {
    .mtr_log_if(log_fn, "NCBI fetch failed for ", acc, ": ", conditionMessage(e))
    ""
  })
  if (!grepl("^LOCUS", trimws(txt))) {
    stop("could not resolve accession ", acc, ": ",
         "NCBI returned no GenBank record")
  }
  gb <- file.path(work, paste0("reference_", acc, ".gb"))
  writeLines(txt, gb)
  list(file = gb, source = "ncbi", accession = acc)
}
