# Package-default FASTA header templates for the Export modal. Used to seed the
# export_opts DB row and as the fallback when no custom template is stored.
DEFAULT_FASTA_HEADER <- paste0(
  "{ID} [organism={Taxon}] [topology={topology}] [mgcode={genetic_code}] ",
  "[location=mitochondrion] {Taxon} mitochondrion, complete genome"
)
DEFAULT_FASTA_HEADER_GENE <- paste0(
  "{ID} [organism={Taxon}] [topology={topology}] [mgcode={genetic_code}] ",
  "[location=mitochondrion] {Taxon}"
)

#' List saved export header template names
#'
#' Returns the names (the `export_opts` PK column) of saved templates, with
#' "default" guaranteed first. Falls back to just "default" when the table is
#' missing (pre-migration DB).
#'
#' @param con database connection
#'
#' @return character vector of template names
#'
#' @noRd
list_export_templates <- function(con) {
  names <- tryCatch(
    dplyr::tbl(con, "export_opts") |>
      dplyr::pull("export_opts"),
    error = function(e) character(0)
  )
  unique(c("default", sort(names)))
}

#' Read a saved FASTA header template from the project DB
#'
#' Returns the named template's strings, falling back to the package defaults
#' when the table/row is missing (e.g. a project DB that predates the
#' export_opts migration, or an unknown name).
#'
#' @param con database connection
#' @param name template name (defaults to "default")
#'
#' @return list(fasta_header=, fasta_header_gene=)
#'
#' @noRd
get_export_opts <- function(con, name = "default") {
  defaults <- list(
    fasta_header = DEFAULT_FASTA_HEADER,
    fasta_header_gene = DEFAULT_FASTA_HEADER_GENE
  )
  row <- tryCatch(
    dplyr::tbl(con, "export_opts") |>
      dplyr::filter(export_opts == !!name) |>
      dplyr::collect(),
    error = function(e) NULL
  )
  if (is.null(row) || nrow(row) == 0) return(defaults)
  pick <- function(x, default) {
    if (length(x) == 0 || is.na(x[1]) || !nzchar(x[1])) default else x[1]
  }
  list(
    fasta_header = pick(row$fasta_header, defaults$fasta_header),
    fasta_header_gene = pick(row$fasta_header_gene, defaults$fasta_header_gene)
  )
}

#' Save (upsert) a named FASTA header template to the project DB
#'
#' No-op (with a warning) if the table is missing.
#'
#' @param con database connection
#' @param fasta_header,fasta_header_gene template strings
#' @param name template name (defaults to "default")
#'
#' @noRd
set_export_opts <- function(con, fasta_header, fasta_header_gene, name = "default") {
  tryCatch(
    dplyr::tbl(con, "export_opts") |>
      dplyr::rows_upsert(
        data.frame(
          export_opts = name,
          fasta_header = fasta_header,
          fasta_header_gene = fasta_header_gene
        ),
        in_place = TRUE,
        copy = TRUE,
        by = "export_opts"
      ),
    error = function(e) warning("Could not save export header templates: ", e$message)
  )
  invisible(NULL)
}

# A short "...near here..." context window around a brace position, for messages.
brace_snippet <- function(chars, pos) {
  n <- length(chars)
  lo <- max(1L, pos - 6L)
  hi <- min(n, pos + 10L)
  paste0(
    if (lo > 1L) "..." else "",
    paste(chars[lo:hi], collapse = ""),
    if (hi < n) "..." else ""
  )
}

#' Find the first unmatched brace in a template
#'
#' Treats every open and close brace literally (no glue escape special-casing)
#' and requires them to balance: each close brace matches the most recent open
#' brace. Reports the first brace that has no match: a close brace with no open
#' brace before it, or, if all close, the first open brace left at the end.
#'
#' @param template header template string
#'
#' @return NULL when braces balance, else
#'   list(pos = integer, type = "open"|"close", snippet = character)
#'
#' @noRd
find_unmatched_brace <- function(template) {
  if (length(template) != 1 || is.na(template)) return(NULL)
  chars <- strsplit(template, "", fixed = TRUE)[[1]]
  n <- length(chars)
  open_stack <- integer(0)  # positions of '{' awaiting a match
  for (i in seq_len(n)) {
    c <- chars[i]
    if (c == "{") {
      open_stack <- c(open_stack, i)
    } else if (c == "}") {
      if (length(open_stack) == 0L) {
        return(list(pos = i, type = "close", snippet = brace_snippet(chars, i)))
      }
      open_stack <- open_stack[-length(open_stack)]
    }
  }
  if (length(open_stack) > 0L) {
    pos <- open_stack[1]  # first '{' left unclosed
    return(list(pos = pos, type = "open", snippet = brace_snippet(chars, pos)))
  }
  NULL
}

#' Validate a glue-syntax FASTA header template
#'
#' Checks brace balance, then dry-runs the template through
#' [stringr::str_glue_data()] against a representative data row so malformed
#' braces and unknown `{column}` references are caught before they reach
#' [export_files()] (where they would crash the GUI). Returns a cleaned,
#' user-facing message rather than the raw glue error.
#'
#' @param template header template string
#' @param data data frame whose columns the template may reference (e.g. rv$data).
#'   When empty, the template is still parsed for brace balance.
#'
#' @return list(ok = logical, level = "ok"|"error", message = character).
#'   Blocking errors (unbalanced braces, unknown column, empty) return
#'   `ok = FALSE`.
#'
#' @noRd
validate_fasta_header <- function(template, data = NULL) {
  err <- function(msg) list(ok = FALSE, level = "error", message = msg)
  if (is.null(template) || !nzchar(trimws(template))) {
    return(err("Template is empty."))
  }
  # Braces must balance: every { needs a matching }. Point at the first
  # unmatched one so the user can find it.
  brace <- find_unmatched_brace(template)
  if (!is.null(brace)) {
    b <- if (brace$type == "open") "{" else "}"
    return(err(sprintf(
      "unmatched '%s' at position %d (near \"%s\")",
      b, brace$pos, brace$snippet
    )))
  }
  row <- if (!is.null(data) && nrow(data) > 0) data[1, , drop = FALSE] else data.frame()
  tryCatch({
    stringr::str_glue_data(row, template)
    list(ok = TRUE, level = "ok", message = "Valid template.")
  }, error = function(e) {
    raw <- conditionMessage(e)
    # Unknown column -> glue reports "object 'XXX' not found"
    col <- regmatches(raw, regexpr("object '[^']+' not found", raw))
    if (length(col) > 0) {
      name <- sub("object '([^']+)' not found", "\\1", col)
      return(err(sprintf('column "%s" not found in database', name)))
    }
    # Fallback: strip glue's multi-line wrapper to the last informative line
    err(sub("^.*!\\s*", "", gsub("\n", " ", raw)))
  })
}

#' Populate export table
#'
#' @param db database connection
#' @param session reactive session
#'
#' @noRd
fetch_export_data <- function(session = getDefaultReactiveDomain()) {
  db <- session$userData$con

  samples <- dplyr::tbl(db, "samples") |>
    dplyr::select(-dplyr::any_of("topology"))

  # ORF count per sample, blanked when ORF finding is disabled
  orf_counts <- dplyr::tbl(db, "annotations") |>
    dplyr::select(ID, type) |>
    dplyr::collect() |>
    dplyr::group_by(ID) |>
    dplyr::summarise(ORFCount = sum(type == "ORF"))
  orf_enabled <- dplyr::tbl(db, "annotate") |>
    dplyr::select(ID, orf_opts) |>
    dplyr::left_join(dplyr::tbl(db, "orf_opts"), by = "orf_opts") |>
    dplyr::select(ID, use_orffinder) |>
    dplyr::collect()

  dplyr::tbl(db, "assemble") |>
    dplyr::filter(assemble_lock == 1) |>
    dplyr::select(ID, blast_accession, blast_species, blast_lineage,
                  dplyr::any_of("poor_blast_ref")) |>
    dplyr::left_join(dplyr::tbl(db, "annotate"), by = "ID") |>
    dplyr::filter(annotate_lock == 1) |>
    dplyr::select(
      ID, blast_accession, blast_species, blast_lineage, curate_opts, topology,
      structure, PCGCount, tRNACount, rRNACount, missing, extra, warnings,
      dplyr::any_of("poor_blast_ref")
    ) |>
    dplyr::left_join(samples, by = "ID") |>
    dplyr::select(-R1, -R2) |>
    dplyr::relocate(Taxon, .after = ID) |>
    dplyr::collect() |>
    dplyr::left_join(orf_counts, by = "ID") |>
    dplyr::left_join(orf_enabled, by = "ID") |>
    dplyr::mutate(
      blast_ref_status = poor_blast_ref,
      structure = stringr::str_replace_all(structure, "trn[A-Z]", "\u2022"),
      export_group = as.character(export_group),
      ORFCount = dplyr::if_else(
        is.na(use_orffinder) | use_orffinder != 1L,
        NA_integer_,
        as.integer(ORFCount)
      )
    ) |>
    dplyr::select(-use_orffinder) |>
    dplyr::relocate(ORFCount, .after = rRNACount) |>
    dplyr::relocate(blast_ref_status, .after = blast_accession)
}
