# Package-default FASTA header templates for the Export modal. Used to seed the
# export_opts DB row and as the fallback when no custom template is stored.
# The leading token is {seqid}, not {ID}: a fragmented sample emits one record per
# scaffold and GenBank needs each SeqID unique within a submission. seqid is the
# plain ID for single-unit samples, so this is a no-op for unfragmented projects.
DEFAULT_FASTA_HEADER <- paste0(
  "{seqid} [organism={Taxon}] [topology={topology}] [mgcode={genetic_code}] ",
  "[location=mitochondrion] {Taxon} mitochondrion, {completeness}"
)
DEFAULT_FASTA_HEADER_GENE <- paste0(
  "{seqid} [organism={Taxon}] [topology={topology}] [mgcode={genetic_code}] ",
  "[location=mitochondrion] {Taxon}"
)
# The {ID}-era defaults, frozen for migration matching. Spelled out rather than
# derived from the current defaults: the default has changed twice ({completeness},
# then {seqid}), so deriving from the current value would stop matching the older
# ones. Only templates identical to a legacy default are migrated; custom templates
# are the user's own.
LEGACY_FASTA_HEADER_ID <- paste0(
  "{ID} [organism={Taxon}] [topology={topology}] [mgcode={genetic_code}] ",
  "[location=mitochondrion] {Taxon} mitochondrion, {completeness}"
)
LEGACY_FASTA_HEADER_GENE_ID <- paste0(
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
#' @param require_completeness when TRUE, an otherwise-valid template that does
#'   not end with `{completeness}` returns a non-blocking warning (ok = TRUE,
#'   level = "warn") so the user can still save/export.
#'
#' @return list(ok = logical, level = "ok"|"warn"|"error", message = character).
#'   Blocking errors (unbalanced braces, unknown column, empty) return
#'   `ok = FALSE`.
#'
#' @noRd
validate_fasta_header <- function(template, data = NULL, require_completeness = FALSE) {
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
    # A template without {seqid} gives every unit of a multi-assembly sample the
    # same defline, while the .tbl still carries the per-unit >Feature seqid. The
    # two must agree exactly or table2asn rejects the submission. seqid is the
    # plain ID for single-unit samples, so {seqid} is always the safe choice.
    if (!grepl("\\{seqid\\}", template)) {
      multi_unit <- !is.null(data) && "ID" %in% names(data) && any(duplicated(data$ID))
      msg <- paste(
        "Header does not use {seqid}. Samples with more than one assembly unit",
        "will produce duplicate FASTA deflines that do not match the .tbl",
        ">Feature line, and table2asn will reject the submission. Use {seqid}",
        "instead of {ID}: it is the plain ID for single-unit samples."
      )
      if (multi_unit) {
        return(err(msg))
      }
      return(list(ok = TRUE, level = "warn", message = msg))
    }
    if (require_completeness && !grepl("\\{completeness\\}\\s*$", template)) {
      return(list(
        ok = TRUE, level = "warn",
        message = paste(
          "Header does not end with {completeness}; GenBank submissions may not",
          "reflect partial vs complete genome status. You can export anyway."
        )
      ))
    }
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

#' GenBank SeqID for an assembly unit
#'
#' Plain `ID` when the sample contributes exactly one exported record, otherwise
#' `ID_p<path>_s<scaffold>`. `n_units` counts only exported (non-ignored) units, so
#' the suffix appears only where there is a sibling record to disambiguate from and
#' single-scaffold projects keep their existing names.
#'
#' @param ID,path,scaffold unit key (vectorised).
#' @param n_units number of exported units for that unit's sample.
#'
#' @noRd
export_seqid <- function(ID, path, scaffold, n_units) {
  # Recycle n_units explicitly: ifelse() returns a result the length of its test, so
  # a scalar n_units (e.g. dplyr::n() inside a grouped mutate) would otherwise
  # collapse every unit to the first one's SeqID.
  n <- rep_len(as.integer(n_units), length(ID))
  ifelse(
    n > 1L,
    paste0(ID, "_p", path, "_s", scaffold),
    as.character(ID)
  )
}

#' Populate export table
#'
#' @param db database connection
#' @param session reactive session
#'
#' @noRd
fetch_export_data <- function(con = NULL, session = getDefaultReactiveDomain()) {
  db <- con %||% session$userData$con

  samples <- dplyr::tbl(db, "samples") |>
    dplyr::select(-dplyr::any_of("topology"))

  # ORF count per annotate unit, blanked when ORF finding is disabled. annotate and
  # annotations are keyed (ID, path, scaffold), so both the grouping and the joins
  # below must carry the full key: keying on ID alone would sum ORFs across a
  # sample's scaffolds and fan each row out to one copy per scaffold.
  unit_key <- c("ID", "path", "scaffold")
  orf_counts <- dplyr::tbl(db, "annotations") |>
    dplyr::select(ID, path, scaffold, type) |>
    dplyr::collect() |>
    dplyr::group_by(ID, path, scaffold) |>
    dplyr::summarise(ORFCount = sum(type == "ORF", na.rm = TRUE), .groups = "drop")
  orf_enabled <- dplyr::tbl(db, "annotate") |>
    dplyr::select(ID, path, scaffold, orf_opts) |>
    dplyr::left_join(dplyr::tbl(db, "orf_opts"), by = "orf_opts") |>
    dplyr::select(ID, path, scaffold, use_orffinder) |>
    dplyr::collect()

  # Resolved per-unit reference (override, else this scaffold's own BLAST top hit),
  # the same source the Annotate table, synteny default and export note use. Each
  # unit is independent: a scaffold with no hit must not inherit a sibling's
  # accession, which is what reading the sample-level assemble.blast_* did.
  assemblies_unit <- unit_ref_facts(db) |>
    dplyr::select(ID, path, scaffold, blast_accession, blast_accession_auto,
                  blast_species, blast_lineage)

  # Topology is per scaffold. assemblies carries this contig's own value, while
  # annotate.topology can summarise a whole sample ("fragmented", or a legacy
  # joined string), and only a per-scaffold value belongs in the Export table or
  # the summary CSV. Matches what export_files() writes to the defline.
  unit_topology <- dplyr::tbl(db, "assemblies") |>
    dplyr::filter(ignore != 1) |>
    dplyr::select(ID, path, scaffold, scaffold_topology = topology) |>
    dplyr::collect()

  out <- dplyr::tbl(db, "assemble") |>
    dplyr::filter(assemble_lock == 1) |>
    dplyr::select(ID, dplyr::any_of("poor_blast_ref")) |>
    dplyr::left_join(dplyr::tbl(db, "annotate"), by = "ID") |>
    dplyr::filter(annotate_lock == 1) |>
    dplyr::select(
      ID, path, scaffold, curate_opts, topology,
      length, structure, PCGCount, tRNACount, rRNACount, missing, extra, warnings,
      dplyr::any_of(c("blast_accession_auto", "poor_blast_ref", "partial"))
    ) |>
    dplyr::left_join(
      dplyr::tbl(db, "curate_opts") |>
        dplyr::select(curate_opts, dplyr::any_of("linear_complete")),
      by = "curate_opts"
    ) |>
    dplyr::left_join(samples, by = "ID") |>
    # Export state is per unit; a unit with no row yet has never been grouped.
    dplyr::left_join(dplyr::tbl(db, "export"), by = unit_key) |>
    dplyr::select(-R1, -R2) |>
    dplyr::relocate(Taxon, .after = ID) |>
    dplyr::collect() |>
    # inner_join: gates rows to a non-ignored assembly, as assemblies_unit is
    # already filtered on ignore.
    dplyr::inner_join(assemblies_unit, by = unit_key) |>
    dplyr::relocate(blast_accession, blast_accession_auto, blast_species,
                    blast_lineage, .after = Taxon) |>
    dplyr::left_join(orf_counts, by = unit_key) |>
    dplyr::left_join(orf_enabled, by = unit_key) |>
    dplyr::left_join(unit_topology, by = unit_key)

  # these columns are absent on un-migrated DBs
  if (!"linear_complete" %in% names(out)) out$linear_complete <- NA_integer_
  if (!"partial" %in% names(out)) out$partial <- NA_character_

  out |>
    dplyr::mutate(
      # Per-scaffold topology wins, and anything not usable as a topology is
      # coerced to "linear". Same rule and same order as export_files(), so the
      # table can never disagree with the defline it is previewing.
      topology = dplyr::if_else(
        !is.na(scaffold_topology) & scaffold_topology != "",
        scaffold_topology, topology
      ),
      topology = dplyr::if_else(
        !is.na(topology) & topology %in% c("circular", "linear"),
        topology, "linear"
      ),
      # Ref-align status is only meaningful when this unit has a real BLAST hit
      # (mirrors the Annotate table).
      blast_ref_status = dplyr::if_else(
        is.na(blast_accession) | blast_accession == "NO HIT",
        NA_character_, poor_blast_ref
      ),
      # Auto-derive completeness from topology; per-sample "partial" forces
      # partial; project-level linear_complete forces linear -> complete.
      completeness = dplyr::case_when(
        !is.na(partial) & partial == "yes" ~ "partial genome",
        topology == "circular" ~ "complete genome",
        !is.na(linear_complete) & linear_complete == 1L ~ "complete genome",
        TRUE ~ "partial genome"
      ),
      structure = stringr::str_replace_all(structure, "trn[A-Z]", "\u2022"),
      export_group = as.character(export_group),
      ORFCount = dplyr::if_else(
        is.na(use_orffinder) | use_orffinder != 1L,
        NA_integer_,
        as.integer(ORFCount)
      )
    ) |>
    dplyr::select(-use_orffinder, -scaffold_topology,
                  -dplyr::any_of("linear_complete")) |>
    # The SeqID this unit will export under. Counted within (ID, export_group) to
    # mirror export_files(): a sample is only suffixed when it contributes more than
    # one record to the same submission. Also what {seqid} resolves to when the
    # Export modal previews a header template.
    dplyr::group_by(ID, export_group) |>
    dplyr::mutate(seqid = export_seqid(ID, path, scaffold, dplyr::n())) |>
    dplyr::ungroup() |>
    dplyr::relocate(path, scaffold, seqid, .after = ID) |>
    dplyr::relocate(ORFCount, .after = rRNACount) |>
    dplyr::relocate(blast_ref_status, .after = blast_accession)
}

#' Leading binomial of a BLAST reference defline
#'
#' `blast_species` carries the whole source description
#' ("Conger oceanicus voucher USNM:FISH:454713 mitochondrion, complete genome");
#' only the leading "Genus species" is useful in the UI.
#'
#' @noRd
species_binomial <- function(x) {
  vapply(x, function(s) {
    if (is.na(s) || !nzchar(trimws(s))) return(NA_character_)
    # Drop any leading annotation prefix ("UNVERIFIED: Conger oceanicus ...")
    s <- sub("^([[:alpha:]]+:[[:space:]]*)+", "", trimws(s))
    if (!nzchar(s)) return(NA_character_)
    toks <- strsplit(trimws(s), "[[:space:]]+")[[1]]
    paste(utils::head(toks, 2), collapse = " ")
  }, character(1), USE.NAMES = FALSE)
}

#' First whitespace token, when it looks like a genus name
#'
#' @noRd
genus_token <- function(x) {
  vapply(x, function(s) {
    if (is.na(s)) return(NA_character_)
    tok <- strsplit(trimws(s), "[[:space:]]+")[[1]][1]
    if (is.na(tok) || !grepl("^[A-Za-z]+$", tok) || nchar(tok) <= 2) {
      return(NA_character_)
    }
    tok
  }, character(1), USE.NAMES = FALSE)
}

#' Samples in an export group whose exported sequence contains gaps
#'
#' One row per sample with at least one run of `N` of `min_len` bp or more in a
#' non-ignored path-0 unit belonging to `group`, restricted to samples that have
#' `scaffold_junctions` rows (the only ones whose genus call is used by the
#' export). `genus_match` is the value stored in
#' `gap_evidence` when there is one, otherwise a suggestion from comparing the
#' first token of `Taxon` with the first token of `blast_species`.
#'
#' @param con database connection
#' @param group export group name
#' @param min_len minimum run of `N` to count as a gap
#'
#' @return data.frame(ID, Taxon, blast_accession, blast_species, n_gaps,
#'   gap_bp, genus_match)
#'
#' @noRd
gap_evidence_prompts <- function(con, group, min_len = 10L) {
  empty <- data.frame(
    ID = character(0), Taxon = character(0), blast_accession = character(0),
    blast_species = character(0), n_gaps = integer(0), gap_bp = integer(0),
    genus_match = character(0), stringsAsFactors = FALSE
  )
  if (is.null(group) || is.na(group) || !nzchar(group)) return(empty)
  # Only units the scaffold join actually built carry a genus call in export
  # (export.R only uses genus_match when the sample has scaffold_junctions rows
  # and the unit is path 0), so anything else must not be prompted for.
  joined_ids <- tryCatch(
    DBI::dbGetQuery(con, "SELECT DISTINCT ID FROM scaffold_junctions")$ID,
    error = function(e) character(0)
  )
  if (length(joined_ids) == 0) return(empty)
  units <- DBI::dbGetQuery(
    con,
    "SELECT e.ID AS ID, a.sequence AS sequence
       FROM export e
       JOIN assemblies a
         ON a.ID = e.ID AND a.path = e.path AND a.scaffold = e.scaffold
      WHERE e.export_group = ? AND a.ignore = 0 AND e.path = 0",
    params = list(as.character(group))
  )
  units <- units[units$ID %in% joined_ids, , drop = FALSE]
  if (nrow(units) == 0) return(empty)

  gaps <- lapply(units$sequence, find_sequence_gaps, min_len = min_len)
  units$n_gaps <- vapply(gaps, nrow, integer(1))
  units$gap_bp <- vapply(gaps, function(g) sum(g$length), numeric(1))
  units <- units[units$n_gaps > 0, , drop = FALSE]
  if (nrow(units) == 0) return(empty)

  out <- data.frame(
    ID = names(split(units$n_gaps, units$ID)),
    n_gaps = as.integer(vapply(split(units$n_gaps, units$ID), sum, numeric(1))),
    gap_bp = as.integer(vapply(split(units$gap_bp, units$ID), sum, numeric(1))),
    stringsAsFactors = FALSE
  )

  samples <- DBI::dbGetQuery(con, "SELECT ID, Taxon FROM samples")
  asmb <- DBI::dbGetQuery(
    con, "SELECT ID, blast_accession, blast_species FROM assemble"
  )
  stored <- tryCatch(
    DBI::dbGetQuery(con, "SELECT ID, genus_match FROM gap_evidence"),
    error = function(e) data.frame(ID = character(0), genus_match = character(0))
  )

  out$Taxon <- samples$Taxon[match(out$ID, samples$ID)]
  out$blast_accession <- asmb$blast_accession[match(out$ID, asmb$ID)]
  out$blast_species <- species_binomial(asmb$blast_species[match(out$ID, asmb$ID)])

  sample_genus <- genus_token(out$Taxon)
  ref_genus <- genus_token(out$blast_species)
  suggested <- ifelse(
    !is.na(sample_genus) & !is.na(ref_genus) &
      tolower(sample_genus) == tolower(ref_genus),
    "same", "different"
  )
  saved <- stored$genus_match[match(out$ID, stored$ID)]
  out$genus_match <- ifelse(is.na(saved), suggested, saved)

  out <- out[order(out$ID), c("ID", "Taxon", "blast_accession", "blast_species",
                              "n_gaps", "gap_bp", "genus_match"), drop = FALSE]
  rownames(out) <- NULL
  out
}
