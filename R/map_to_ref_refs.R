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

# Strip the optional Reference column out of a mapping before the samples table
# is built from colnames(mapping). Precedent: R/init_db_userAsmb.R strips
# Assembly/Topology the same way. Values are returned raw; callers validate.
#' @noRd
.mtr_take_ref_col <- function(mapping, mapping_id = "ID") {
  if ("Reference" %nin% colnames(mapping)) {
    return(list(mapping = mapping, refs = NULL))
  }
  refs <- as.character(mapping[["Reference"]])
  names(refs) <- as.character(mapping[[mapping_id]])
  keep <- setdiff(colnames(mapping), "Reference")
  list(mapping = mapping[, keep, drop = FALSE], refs = refs)
}

# Per-sample MapToRef parameter sets.
#
# A mapping-file Reference means "assemble this sample by mapping to that
# reference", so the sample gets a parameter set of its own, cloned from the
# base set with assembler = MapToRef and the reference on it. The reference is
# deliberately NOT also written to assemble.maptoref_ref: the pipeline
# COALESCEs the column over the set, so a value in both would make the Assemble
# options modal look editable while the column silently won.
#' @noRd
.mtr_opts_name <- function(id) paste0(id, "_maptoref")

# refs is a named character vector (names are sample IDs), as built by new_db()
# and add_samples(). Blank and NA references are left on the base set.
#' @noRd
.mtr_seed_per_sample_opts <- function(con, refs, base = "default") {
  if (is.null(refs) || length(refs) == 0L) return(invisible(character(0)))
  keep <- !is.na(refs) & nzchar(trimws(refs))
  if (!any(keep)) return(invisible(character(0)))
  refs <- refs[keep]
  ids <- names(refs)
  names <- .mtr_opts_name(ids)

  base_row <- DBI::dbGetQuery(
    con, "SELECT * FROM assemble_opts WHERE assemble_opts = ?",
    params = list(base)
  )
  if (nrow(base_row) != 1L) {
    stop("assemble options set ", shQuote(base), " not found", call. = FALSE)
  }
  # An upsert here would silently reconfigure a set the user built by hand.
  taken <- intersect(
    names,
    DBI::dbGetQuery(con, "SELECT assemble_opts FROM assemble_opts")$assemble_opts
  )
  if (length(taken) > 0L) {
    stop("assemble options set(s) ", paste(shQuote(taken), collapse = ", "),
         " already exist; rename them before adding these samples",
         call. = FALSE)
  }

  new_opts <- base_row[rep(1L, length(ids)), , drop = FALSE]
  rownames(new_opts) <- NULL
  new_opts$assemble_opts <- names
  new_opts$assembler <- "MapToRef"
  new_opts$maptoref_ref <- unname(refs)
  dplyr::tbl(con, "assemble_opts") |>
    dplyr::rows_insert(
      new_opts,
      in_place = TRUE,
      copy = TRUE,
      by = "assemble_opts",
      conflict = "ignore"
    )
  dplyr::tbl(con, "assemble") |>
    dplyr::rows_update(
      data.frame(ID = ids, assemble_opts = names),
      unmatched = "ignore",
      in_place = TRUE,
      copy = TRUE,
      by = "ID"
    )
  invisible(names)
}

# The sample's own set, when it has one: named for the sample, used by nobody
# else, and assembling with MapToRef. set_maptoref_refs() edits that set instead
# of the column, so the reference keeps living in exactly one place.
#' @noRd
.mtr_dedicated_opts <- function(con, ids) {
  q <- DBI::dbGetQuery(con, paste(
    "SELECT a.ID, a.assemble_opts, o.assembler, o.maptoref_ref,",
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
  list(own = own, opts = q$assemble_opts, set_ref = q$maptoref_ref)
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
    "AND COALESCE(NULLIF(TRIM(a.maptoref_ref), ''),",
    "NULLIF(TRIM(o.maptoref_ref), '')) IS NULL"
  ))$ID
  if (length(ids) > 0L) {
    warning("MapToRef has no reference for ", length(ids), " sample(s): ",
            paste(utils::head(ids, 10L), collapse = ", "),
            if (length(ids) > 10L) paste0(" and ", length(ids) - 10L, " more") else "",
            ". Those samples will fail at the Assemble step. Set a reference per ",
            "sample with MitoPilot::set_maptoref_refs(), or set one for the ",
            "parameter set in the Assemble options.", call. = FALSE)
  }
  invisible(ids)
}

#' Set per-sample MapToRef references
#'
#' Assigns a MapToRef reference mitogenome to individual samples in an existing
#' project. A sample that already has a parameter set of its own (one named for
#' the sample, used by no other sample, and assembling with MapToRef) has the
#' reference written onto that set. Any other sample gets a per-sample value
#' that overrides the reference on its shared parameter set; a blank value
#' clears the override so the parameter set applies again.
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
  # A sample on its own MapToRef set keeps its reference on that set, so that is
  # the value to compare against and the value to write.
  ded <- .mtr_dedicated_opts(con, ids)
  old_vals <- cur$maptoref_ref[match(ids, cur$ID)]
  old_vals[ded$own] <- ded$set_ref[ded$own]
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

  # Samples on their own set: the reference lives on the set, so write it there.
  on_set <- changed[ded$own[changed]]
  if (length(on_set) > 0L) {
    dplyr::tbl(con, "assemble_opts") |>
      dplyr::rows_update(
        data.frame(
          assemble_opts = ded$opts[on_set],
          maptoref_ref = new_vals[on_set]
        ),
        unmatched = "ignore",
        in_place = TRUE,
        copy = TRUE,
        by = "assemble_opts"
      )
  }
  # Everyone else keeps the per-sample column, which overrides a shared set.
  on_col <- changed[!ded$own[changed]]
  if (length(on_col) > 0L) {
    dplyr::tbl(con, "assemble") |>
      dplyr::rows_update(
        data.frame(ID = ids[on_col], maptoref_ref = new_vals[on_col]),
        unmatched = "ignore",
        in_place = TRUE,
        copy = TRUE,
        by = "ID"
      )
  }
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
