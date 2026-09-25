# Pre-flight checks for new_project() / new_project_userAsmb().
#
# Every check appends to a shared collector instead of stopping, so the user
# sees the whole list at once. Nothing here writes into the project directory;
# scratch files go to tempfile().

#' @noRd
.issues <- function() {
  e <- new.env(parent = emptyenv())
  e$errors <- character(0)
  e$warnings <- character(0)
  e$err <- function(...) e$errors <- c(e$errors, paste0(...))
  e$warn <- function(...) e$warnings <- c(e$warnings, paste0(...))
  e
}

#' Compact "a, b, c, ... and N more" list for messages
#' @noRd
.lst <- function(x, max = 10L) {
  x <- as.character(x)
  if (length(x) <= max) return(paste(x, collapse = ", "))
  paste0(paste(x[seq_len(max)], collapse = ", "), ", ... and ", length(x) - max, " more")
}

.pl <- function(n, one, many) if (n == 1L) one else many

#' Print the collected issues and stop when any error was recorded
#' @noRd
.report_issues <- function(iss, context = "Project initialization") {
  ne <- length(iss$errors)
  nw <- length(iss$warnings)
  if (ne == 0L && nw == 0L) return(invisible(TRUE))
  txt <- paste(c(
    sprintf("%s checks: %d error(s), %d warning(s)", context, ne, nw),
    if (ne > 0L) paste0("  ERROR   ", iss$errors),
    if (nw > 0L) paste0("  WARNING ", iss$warnings)
  ), collapse = "\n")
  if (ne > 0L) {
    stop(txt, "\n", context, " aborted: fix the ", ne, " error(s) listed above and rerun.",
         call. = FALSE)
  }
  message(txt)
  invisible(TRUE)
}

.is_remote <- function(x) {
  length(x) == 1L && !is.na(x) && grepl("^(s3|gs|az|https?|ftp)://", x, ignore.case = TRUE)
}

.is_na_sentinel <- function(x) {
  is.null(x) || length(x) != 1L || is.na(x) || !nzchar(x) || identical(x, "NA")
}

#' Read the mapping file, recording problems instead of stopping
#' @return data.frame or NULL
#' @noRd
.read_mapping_checked <- function(mapping_fn, iss) {
  if (is.null(mapping_fn) || length(mapping_fn) != 1L || is.na(mapping_fn) ||
      !nzchar(mapping_fn)) {
    iss$err("mapping_fn: a mapping file is required")
    return(NULL)
  }
  if (dir.exists(mapping_fn)) {
    iss$err("mapping_fn: '", mapping_fn, "' is a directory, not a file")
    return(NULL)
  }
  if (!file.exists(mapping_fn)) {
    iss$err("mapping_fn: file not found: ", mapping_fn)
    return(NULL)
  }
  # Format checks before read.csv(), which silently shifts fields on a ragged
  # row and swallows the file after an unclosed quote.
  magic <- readBin(mapping_fn, "raw", 4L)
  if (length(magic) >= 2L && identical(magic[1:2], as.raw(c(0x50, 0x4b)))) {
    iss$err("mapping_fn: '", basename(mapping_fn), "' is an Excel workbook, not a ",
            "CSV; save the sheet as CSV")
    return(NULL)
  }
  if (any(magic == as.raw(0))) {
    iss$err("mapping_fn: '", basename(mapping_fn), "' is not a text file")
    return(NULL)
  }
  hdr <- sub("^\ufeff", "", readLines(mapping_fn, n = 1L, warn = FALSE))
  if (length(hdr) == 0L || !nzchar(trimws(hdr))) {
    iss$err("mapping_fn: the mapping file is empty")
    return(NULL)
  }
  if (!grepl(",", hdr)) {
    delim <- if (grepl("\t", hdr)) "tab" else if (grepl(";", hdr)) "semicolon" else NULL
    if (!is.null(delim)) {
      iss$err("mapping_fn: the header is ", delim, "-separated; the mapping file ",
              "must be comma-separated")
      return(NULL)
    }
  }
  nf <- tryCatch(utils::count.fields(mapping_fn, sep = ",", quote = "\"",
                                     blank.lines.skip = TRUE),
                 error = function(e) e)
  if (inherits(nf, "error")) {
    iss$err("mapping_fn: could not parse as CSV (unclosed quote?): ",
            conditionMessage(nf))
    return(NULL)
  }
  if (anyNA(nf)) {
    iss$err("mapping_fn: unclosed quote starting on line ", which(is.na(nf))[1])
    return(NULL)
  }
  if (length(nf) > 1L && any(nf[-1] != nf[1])) {
    bad <- which(nf[-1] != nf[1]) + 1L
    iss$err("mapping_fn: header has ", nf[1], " fields but ",
            .pl(length(bad), "line ", "lines "), .lst(bad), " ",
            .pl(length(bad), "does", "do"), " not (missing or extra comma, or an unclosed quote?)")
    return(NULL)
  }
  mapping <- tryCatch(utils::read.csv(mapping_fn, stringsAsFactors = FALSE),
                      error = function(e) e)
  if (inherits(mapping, "error")) {
    iss$err("mapping_fn: could not be read as CSV: ", conditionMessage(mapping))
    return(NULL)
  }
  if (nrow(mapping) == 0L) {
    iss$err("mapping_fn: the mapping file has no sample rows (is the first line ",
            "a header?)")
    return(NULL)
  }
  # read.csv() silently renames duplicate or non-syntactic headers.
  raw <- strsplit(sub("\r$", "", hdr), ",", fixed = TRUE)[[1]]
  raw <- gsub("^\"|\"$", "", trimws(raw))
  if (anyDuplicated(raw)) {
    iss$warn("mapping columns: duplicate column names will be renamed by R: ",
             .lst(unique(raw[duplicated(raw)])))
  }
  renamed <- setdiff(raw, colnames(mapping))
  renamed <- renamed[!duplicated(renamed) & nzchar(renamed)]
  if (length(renamed) > 0L) {
    iss$warn("mapping columns: names with spaces or symbols are renamed by R ",
             "(e.g. 'Voucher ID' becomes 'Voucher.ID'): ", .lst(renamed))
  }
  mapping
}


# NCBI table2asn rejects SeqIDs over 50 characters; export appends up to
# "_p<path>_s<scaffold>" to an ID, so 40 leaves room.
.max_id_chars <- 40L

#' Sample-ID rules shared by every mapping-file entry point
#' @return the collector; `iss$blank` marks blank IDs so callers can label rows
#' @noRd
check_sample_ids <- function(ids, iss = .issues()) {
  ids <- as.character(ids)
  blank <- is.na(ids) | !nzchar(ids)
  if (any(blank)) {
    iss$err("mapping IDs: empty ID in ", if (sum(blank) == 1L) "row " else "rows ",
            .lst(which(blank)))
  }
  ids[blank] <- ""
  if (anyDuplicated(ids[!blank])) {
    dup <- unique(ids[duplicated(ids) & !blank])
    iss$err("mapping IDs: duplicate ", .pl(length(dup), "ID", "IDs"), ": ", .lst(dup))
  }
  long <- !blank & nchar(ids) > .max_id_chars
  if (any(long)) {
    iss$err("mapping IDs: ", .pl(sum(long), "ID is", "IDs are"), " over ", .max_id_chars,
            " characters (NCBI SeqID limit): ", .lst(ids[long]))
  }
  badc <- !blank & !grepl("^[a-zA-Z0-9_:-]+$", ids)
  if (any(badc)) {
    iss$err("mapping IDs: ", .pl(sum(badc), "ID has", "IDs have"), " characters other ",
            "than letters, digits, dashes, underscores, and colons: ", .lst(ids[badc]))
  }
  iss$blank <- blank
  invisible(iss)
}

#' Mapping-level checks shared by new_db*() and the project pre-flight
#'
#' File-system checks (reads, assemblies) only run when `data_path` /
#' `assembly_path` are supplied; `new_db*()` pass NULL so a database can be
#' built where the data is not mounted.
#'
#' @param mapping data.frame from read.csv()
#' @param need_reads R1/R2 columns are required
#' @param user_asmb Assembly column is required
#' @param data_path NULL skips read-file checks
#' @param assembly_path NULL skips assembly-file checks; "NA" means the
#'   Assembly column holds full paths
#' @param check_assemblies run assembly-file checks at all (the pre-flight only)
#' @noRd
check_mapping <- function(mapping, mapping_id = "ID", mapping_taxon = "Taxon",
                          mapping_geome = "GEOME_BCID",
                          mapping_gbif = "GBIF_ID",
                          need_reads = TRUE, user_asmb = FALSE,
                          data_path = NULL, assembly_path = NULL,
                          check_assemblies = FALSE, find_mitogenome = FALSE,
                          iss = .issues()) {
  cols <- colnames(mapping)
  n <- nrow(mapping)

  # Required columns ----
  if (mapping_id %nin% cols) {
    iss$err("mapping columns: ID column '", mapping_id, "' not found (columns: ",
            .lst(cols), ")")
  }
  if (mapping_taxon %nin% cols) {
    iss$err("mapping columns: Taxon column '", mapping_taxon, "' not found")
  }
  if (need_reads) {
    for (col in c("R1", "R2")) {
      if (col %nin% cols) iss$err("mapping columns: '", col, "' column not found")
    }
  }
  if (user_asmb && "Assembly" %nin% cols) {
    iss$err("mapping columns: 'Assembly' column not found")
  }
  # Columns new_db() computes; a same-named metadata column would be overwritten.
  reserved <- c("genetic_code", if (user_asmb) c("topology", "assembly"))
  if (mapping_id != "ID") reserved <- c(reserved, "ID")
  if (mapping_taxon != "Taxon") reserved <- c(reserved, "Taxon")
  if (mapping_geome != "GEOME_BCID") {
    if (mapping_geome %nin% cols) {
      iss$err("mapping columns: GEOME BCID column '", mapping_geome, "' not found")
    }
    reserved <- c(reserved, "GEOME_BCID")
  }
  if (mapping_gbif != "GBIF_ID") {
    if (mapping_gbif %nin% cols) {
      iss$err("mapping columns: GBIF ID column '", mapping_gbif, "' not found")
    }
    reserved <- c(reserved, "GBIF_ID")
  }
  hit <- intersect(reserved, cols)
  if (length(hit) > 0L) {
    iss$err("mapping columns: reserved names that MitoPilot fills in itself: ",
            .lst(hit), ". Rename these columns.")
  }
  if (!user_asmb && "Topology" %in% cols) {
    iss$warn("mapping columns: 'Topology' only applies to user-assembly projects ",
             "and will be stored as plain metadata")
  }

  # IDs ----
  ids <- if (mapping_id %in% cols) as.character(mapping[[mapping_id]]) else rep("", n)
  blank <- check_sample_ids(ids, iss)$blank
  lab <- ifelse(blank, paste0("row ", seq_len(n)), ids)

  # Taxon ----
  if (mapping_taxon %in% cols) {
    tx <- trimws(as.character(mapping[[mapping_taxon]]))
    miss <- is.na(tx) | !nzchar(tx)
    if (any(miss)) {
      iss$warn("mapping Taxon: empty for ", .lst(lab[miss]))
    }
  }

  # GEOME BCIDs ----
  if (mapping_geome %in% cols) {
    raw <- trimws(as.character(mapping[[mapping_geome]]))
    raw[is.na(raw)] <- ""
    bad <- nzchar(raw) & is.na(geome_normalize_bcid(raw))
    if (any(bad)) {
      iss$warn("mapping GEOME BCID: not a GEOME ARK (ark:/NNNNN/...) for ",
               .lst(lab[bad]), "; these samples will show a failed GEOME fetch")
    }
  }

  # GBIF IDs ----
  if (mapping_gbif %in% cols) {
    raw <- trimws(.meta_chr(mapping[[mapping_gbif]]))
    raw[is.na(raw)] <- ""
    bad <- nzchar(raw) & is.na(gbif_normalize_id(raw))
    if (any(bad)) {
      iss$warn("mapping GBIF ID: not a GBIF occurrence ID (digits) or NMNH EZID for ",
               .lst(lab[bad]), "; these samples will show a failed GBIF fetch")
    }
  }

  # Empty metadata columns ----
  empty_cols <- cols[vapply(mapping, function(v) all(is.na(v) | !nzchar(trimws(v))),
                            logical(1))]
  if (length(empty_cols) > 0L) {
    iss$warn("mapping columns: entirely empty column(s): ", .lst(empty_cols))
  }

  # Reads ----
  if (need_reads && all(c("R1", "R2") %in% cols)) {
    r1 <- as.character(mapping$R1)
    r2 <- as.character(mapping$R2)
    miss <- is.na(r1) | !nzchar(r1) | is.na(r2) | !nzchar(r2)
    if (any(miss)) iss$err("mapping reads: R1/R2 empty for ", .lst(lab[miss]))
    same <- !miss & r1 == r2
    if (any(same)) iss$err("mapping reads: R1 and R2 name the same file for ", .lst(lab[same]))
    dup <- !miss & (duplicated(r1) | duplicated(r1, fromLast = TRUE))
    if (any(dup)) iss$warn("mapping reads: the same R1 file is used by ", .lst(lab[dup]))
    if (!is.null(data_path) && !.is_remote(data_path) && dir.exists(data_path)) {
      .check_fastq_files(c(r1[!miss], r2[!miss]), rep(lab[!miss], 2L), data_path, iss)
    }
  }

  # Assemblies ----
  if (user_asmb) {
    topo <- if ("Topology" %in% cols) as.character(mapping$Topology) else rep("", n)
    topo[is.na(topo)] <- ""
    badt <- nzchar(topo) & topo %nin% c("circular", "linear")
    if (any(badt)) {
      iss$err("mapping Topology: values must be lowercase 'circular' or 'linear' for ",
              .lst(paste0(lab[badt], " [", topo[badt], "]")))
    }
    if ("Assembly" %in% cols) {
      fa <- as.character(mapping$Assembly)
      miss <- is.na(fa) | !nzchar(fa)
      if (any(miss)) iss$err("mapping Assembly: empty for ", .lst(lab[miss]))
      dup <- !miss & (duplicated(fa) | duplicated(fa, fromLast = TRUE))
      if (any(dup)) iss$warn("mapping Assembly: the same file is used by ", .lst(lab[dup]))
      if (check_assemblies && (is.null(assembly_path) || !.is_remote(assembly_path))) {
        .check_assembly_files(fa[!miss], lab[!miss], topo[!miss], assembly_path,
                              find_mitogenome, iss)
      }
    }
  }
  iss
}

#' Every read file must exist and start like FASTQ (first record header)
#' @noRd
.check_fastq_files <- function(files, labels, data_path, iss) {
  paths <- file.path(data_path, files)
  ok <- file.exists(paths) & !dir.exists(paths)
  if (any(!ok)) {
    iss$err("reads: ", length(unique(files[!ok])), " file(s) not found in ", data_path,
            ": ", .lst(unique(files[!ok])))
  }
  # Only the first record is read, so this is cheap even for gzipped runs.
  for (i in which(ok)) {
    if (i > 1L && files[i] %in% files[seq_len(i - 1L)]) next
    first <- tryCatch({
      con <- gzfile(paths[i], "rt")
      l <- readLines(con, n = 4L, warn = FALSE)
      close(con)
      l
    }, error = function(e) character(0))
    if (length(first) < 4L || !startsWith(first[1], "@") || !startsWith(first[3], "+")) {
      iss$err("reads: ", files[i], " (", labels[i], ") is empty or not FASTQ")
    }
  }
}

#' Every assembly must exist and parse as FASTA; topology only fits one contig
#' @noRd
.check_assembly_files <- function(files, labels, topo, assembly_path,
                                  find_mitogenome, iss) {
  paths <- if (.is_na_sentinel(assembly_path)) files else file.path(assembly_path, files)
  ok <- file.exists(paths) & !dir.exists(paths)
  if (any(!ok)) {
    where <- if (.is_na_sentinel(assembly_path)) "" else paste0(" in ", assembly_path)
    iss$err("assemblies: ", sum(!ok), " file(s) not found", where, ": ",
            .lst(unique(files[!ok])))
  }
  for (i in which(ok)) {
    n <- tryCatch(length(Biostrings::fasta.seqlengths(paths[i])), error = function(e) {
      iss$err("assemblies: ", files[i], " (", labels[i], ") is not valid FASTA: ",
              sub(".*: ", "", conditionMessage(e)))
      NA_integer_
    })
    if (is.na(n)) next
    if (n == 0L) {
      iss$err("assemblies: ", files[i], " (", labels[i], ") holds no sequences")
    } else if (n > 1L && nzchar(topo[i])) {
      iss$warn("assemblies: ", labels[i], " declares Topology '", topo[i], "' but ",
               files[i], " holds ", n, " contigs; a topology only applies to a ",
               "single-sequence mitogenome and will be ignored (recorded as 'multi')")
    }
    if (n > 100L && !isTRUE(find_mitogenome)) {
      iss$warn("assemblies: ", files[i], " (", labels[i], ") holds ", n, " contigs; ",
               "for a whole-genome assembly use find_mitogenome = TRUE")
    }
  }
}

#' A database / reference argument: local path must exist, URL must answer
#' @param kind "fasta", "genbank", "dir", or "any"
#' @noRd
.check_resource <- function(x, label, kind = "any", iss, required = FALSE) {
  if (is.null(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    if (required) iss$err(label, ": required but not supplied")
    return(invisible(NULL))
  }
  if (.is_remote(x)) {
    if (!grepl("^https?://", x, ignore.case = TRUE)) {
      iss$warn(label, ": cannot check ", x, " before the pipeline runs")
      return(invisible(NULL))
    }
    ok <- tryCatch({
      httr2::request(x) |> httr2::req_method("HEAD") |> httr2::req_timeout(10) |>
        httr2::req_perform()
      TRUE
    }, error = function(e) FALSE)
    if (!ok) iss$warn(label, ": URL not reachable right now: ", x)
    return(invisible(NULL))
  }
  if (kind == "dir") {
    if (!dir.exists(x)) iss$err(label, ": directory not found: ", x)
    return(invisible(NULL))
  }
  if (dir.exists(x)) {
    iss$err(label, ": '", x, "' is a directory, not a file")
    return(invisible(NULL))
  }
  if (!file.exists(x)) {
    iss$err(label, ": file not found: ", x)
    return(invisible(NULL))
  }
  if (file.access(x, 4L) != 0L) {
    iss$err(label, ": file is not readable: ", x)
    return(invisible(NULL))
  }
  first <- tryCatch(readLines(x, n = 1L, warn = FALSE)[1], error = function(e) NA)
  if (is.na(first)) first <- ""
  if (kind == "fasta" && !startsWith(first, ">")) {
    iss$err(label, ": not a FASTA file (no '>' header): ", x)
  }
  if (kind == "genbank" && !grepl("^LOCUS ", first)) {
    iss$err(label, ": not a GenBank flat file (no LOCUS line): ", x)
  }
  invisible(NULL)
}

#' Processing parameters passed through `...` to new_db*()
#'
#' Type-checks every supplied value against the default's type, then the
#' handful of choice / resource arguments by name.
#' @noRd
.check_db_params <- function(db_fun, dots, iss) {
  fd <- formals(db_fun)
  unknown <- setdiff(names(dots), names(fd))
  if (length(unknown) > 0L) {
    iss$err("parameters: unknown argument(s): ", .lst(unknown))
  }
  get <- function(nm) {
    if (nm %in% names(dots)) return(dots[[nm]])
    if (nm %nin% names(fd)) return(NULL)
    tryCatch(eval(fd[[nm]], environment(db_fun)), error = function(e) NULL)
  }
  for (nm in intersect(names(dots), names(fd))) {
    v <- dots[[nm]]
    d <- tryCatch(eval(fd[[nm]], environment(db_fun)), error = function(e) NULL)
    if (is.null(d) || nm %in% c("curate_params", "mapping_fn")) next
    one <- length(v) == 1L && !is.na(v)
    if (is.numeric(d) && !(one && is.numeric(v) && is.finite(v) && v >= 0)) {
      iss$err("parameters: ", nm, " must be a single non-negative number")
    } else if (is.logical(d) && !(one && is.logical(v))) {
      iss$err("parameters: ", nm, " must be TRUE or FALSE")
    } else if (is.character(d) && !(one && is.character(v))) {
      iss$err("parameters: ", nm, " must be a single string")
    }
  }
  ok1 <- function(nm) { v <- get(nm); length(v) == 1L && !is.na(v) }

  if (ok1("assembler") && get("assembler") %nin% c("GetOrganelle", "MitoFinder", "MapToRef")) {
    iss$err("parameters: assembler must be GetOrganelle, MitoFinder, or MapToRef")
  }
  if (ok1("maptoref_mapper") && get("maptoref_mapper") %nin% .mtr_mappers) {
    iss$err("parameters: maptoref_mapper must be one of ", .lst(.mtr_mappers))
  }
  mtr <- paste0(get("maptoref") %||% "", get("maptoref_consensus") %||% "")
  if (grepl("['\"]", mtr)) {
    iss$err("parameters: maptoref and maptoref_consensus must not contain quote characters")
  }
  if (ok1("orf_max_overlap") && is.numeric(get("orf_max_overlap")) &&
      get("orf_max_overlap") > 1) {
    iss$err("parameters: orf_max_overlap is a fraction and must be <= 1")
  }
  ct <- get("curate_target")
  if (ok1("curate_target") &&
      (!exists(paste0("params_", ct), mode = "function") ||
       !exists(paste0("curate_", ct), mode = "function"))) {
    known <- sub("^params_", "", ls(environment(db_fun), pattern = "^params_"))
    iss$err("parameters: curate_target '", ct, "' has no curation ruleset. Known: ",
            .lst(sort(known), 40L))
  }
  if (ok1("annotate_ref_db") && get("annotate_ref_db") %nin% c("Metazoa_RefSeq89", "Chordata") &&
      .is_remote(get("annotate_ref_dir") %||% "")) {
    iss$warn("parameters: annotate_ref_db '", get("annotate_ref_db"),
             "' is not a bundled MITOS2 database (Metazoa_RefSeq89, Chordata)")
  }
  if (ok1("curate_ref_db") && !grepl("^(Metazoa_RefSeq(89|231|235)|Chordata)(_custom)?$",
                                     get("curate_ref_db"))) {
    iss$warn("parameters: curate_ref_db '", get("curate_ref_db"),
             "' is not a bundled curation database")
  }

  # Resources: only what the user supplied; the shipped defaults are trusted ----
  seeds <- get("seeds_db")
  labels <- get("labels_db")
  if ("seeds_db" %in% names(dots)) .check_resource(seeds, "seeds_db", "fasta", iss)
  if ("labels_db" %in% names(dots)) .check_resource(labels, "labels_db", "fasta", iss)
  if (length(seeds) == 1L && length(labels) == 1L && !is.na(seeds) && !is.na(labels) &&
      identical(basename(seeds), basename(labels))) {
    iss$err("parameters: seeds_db and labels_db must not share a file name")
  }
  if ("mitofinder_db" %in% names(dots)) {
    .check_resource(dots$mitofinder_db, "mitofinder_db", "genbank", iss)
  }
  ard <- get("annotate_ref_dir")
  if ("annotate_ref_dir" %in% names(dots) && !.is_remote(ard)) {
    .check_resource(ard, "annotate_ref_dir", "dir", iss)
  }
  if (length(ard) == 1L && !is.na(ard) && !.is_remote(ard) && dir.exists(ard) &&
      ok1("annotate_ref_db")) {
    tryCatch(check_mitos_ref_db(get("annotate_ref_db"), ard),
             error = function(e) iss$err("parameters: ", conditionMessage(e)))
  }
  invisible(iss)
}

#' Pre-flight for new_project() / new_project_userAsmb()
#'
#' Runs every check, prints the full list of errors and warnings, and stops when
#' any error was found. Called before the project directory or any file is
#' created.
#'
#' @param dots list of `...` destined for `db_fun`
#' @noRd
preflight_project <- function(path, mapping_fn, mapping_id, data_path, no_raw_data = FALSE,
                              user_asmb = FALSE, assembly_path = NULL,
                              find_mitogenome = FALSE, mitofinder_db = NULL,
                              executor = NULL, config = NULL, profile_dir = mitopilot_config_dir(),
                              container = NULL, min_depth = 2e6, genetic_code = NULL,
                              ncbi_api_key = NULL, custom_seeds_db = NULL,
                              custom_labels_db = NULL, force = FALSE,
                              db_fun = new_db, dots = list()) {
  iss <- .issues()

  # Project directory ----
  if (length(path) != 1L || is.na(path) || !nzchar(path)) {
    iss$err("path: a project directory is required")
  } else if (file.exists(path) && !dir.exists(path)) {
    iss$err("path: '", path, "' exists and is not a directory")
  } else {
    if (dir.exists(path) && file.exists(file.path(path, ".sqlite")) && !force) {
      iss$err("path: a project database already exists in ", path,
              ". Use force = TRUE to overwrite (old data will be lost)")
    }
    anc <- normalizePath(path, mustWork = FALSE)
    while (!dir.exists(anc) && dirname(anc) != anc) anc <- dirname(anc)
    if (file.access(anc, 2L) != 0L) iss$err("path: cannot write to ", anc)
  }

  # Data / assembly directories ----
  if (!no_raw_data) {
    if (.is_na_sentinel(data_path)) {
      iss$err("data_path: required (set no_raw_data = TRUE to run without reads)")
    } else if (.is_remote(data_path)) {
      iss$warn("data_path: remote location, read files cannot be checked before the ",
               "pipeline runs: ", data_path)
    } else if (!dir.exists(data_path)) {
      iss$err("data_path: directory not found: ", data_path)
    }
  }
  if (user_asmb && !.is_na_sentinel(assembly_path)) {
    if (.is_remote(assembly_path)) {
      iss$warn("assembly_path: remote location, assembly files cannot be checked ",
               "before the pipeline runs: ", assembly_path)
    } else if (!dir.exists(assembly_path)) {
      iss$err("assembly_path: directory not found: ", assembly_path)
    }
  }

  # Mapping file ----
  mapping <- .read_mapping_checked(mapping_fn, iss)
  if (!is.null(mapping)) {
    check_mapping(mapping, mapping_id = mapping_id,
                  mapping_taxon = dots$mapping_taxon %||% "Taxon",
                  mapping_geome = dots$mapping_geome %||% "GEOME_BCID",
                  mapping_gbif = dots$mapping_gbif %||% "GBIF_ID",
                  need_reads = !no_raw_data, user_asmb = user_asmb,
                  data_path = if (no_raw_data) NULL else data_path,
                  assembly_path = assembly_path, check_assemblies = user_asmb,
                  find_mitogenome = find_mitogenome, iss = iss)
    # MapToRef reference column: its validator already reports every row.
    if (!user_asmb && mapping_id %in% colnames(mapping)) {
      tryCatch({
        taken <- .mtr_take_ref_col(mapping, mapping_id = mapping_id)
        if (!is.null(taken$refs)) {
          refs <- .mtr_validate_refs(taken$refs, ids = names(taken$refs),
                                     context = "the mapping file 'Reference' column")
          .mtr_validate_ref_topology(refs, taken$topology, ids = names(taken$refs),
                                     context = "the mapping file 'Reference_topology' column")
        }
      }, error = function(e) iss$err("mapping Reference: ", conditionMessage(e)))
    }
  }

  geome_col <- dots$mapping_geome %||% "GEOME_BCID"
  if (!is.null(mapping) && geome_col %in% colnames(mapping) &&
      !isFALSE(dots$fetch_geome) &&
      any(!is.na(geome_normalize_bcid(mapping[[geome_col]])))) {
    .check_resource("https://api.geome-db.org/docs/geomeAPI.json", "GEOME", iss = iss)
  }

  gbif_col <- dots$mapping_gbif %||% "GBIF_ID"
  if (!is.null(mapping) && gbif_col %in% colnames(mapping) &&
      !isFALSE(dots$fetch_gbif) &&
      any(!is.na(gbif_normalize_id(mapping[[gbif_col]])))) {
    .check_resource("https://api.gbif.org/v1/enumeration/country", "GBIF", iss = iss)
  }

  # User-assembly extras ----
  if (user_asmb) {
    if (isTRUE(find_mitogenome) && .is_na_sentinel(mitofinder_db)) {
      iss$err("mitofinder_db: find_mitogenome = TRUE needs a MitoFinder GenBank ",
              "database; build one with custom_assembly_db(clade = <clade>, ",
              "db_type = \"mitofinder\")")
    } else {
      .check_resource(mitofinder_db, "mitofinder_db", "genbank", iss)
    }
  }

  # Executor / config ----
  if (!is.null(config)) {
    if (!file.exists(config)) iss$err("config: file not found: ", config)
  } else if (is.null(executor) || length(executor) != 1L || is.na(executor) ||
             !nzchar(executor)) {
    iss$err("executor: a value is required")
  } else {
    tryCatch(resolve_config(executor, profile_dir = profile_dir),
             error = function(e) iss$err("executor: ", conditionMessage(e)))
  }
  if (length(container) != 1L || is.na(container) || !nzchar(container)) {
    iss$err("container: must be a single non-empty string")
  }
  if (length(min_depth) != 1L || !is.numeric(min_depth) || is.na(min_depth) || min_depth < 0) {
    iss$err("min_depth: must be a single non-negative number")
  }
  if (!is.null(genetic_code) && (length(genetic_code) != 1L || is.na(genetic_code) ||
                                 !is.numeric(genetic_code) ||
                                 genetic_code != round(genetic_code) ||
                                 genetic_code < 1 || genetic_code > 33)) {
    iss$err("genetic_code: must be NULL or an NCBI translation table number (1-33)")
  }
  if (!is.null(ncbi_api_key) && (length(ncbi_api_key) != 1L || is.na(ncbi_api_key) ||
                                 !is.character(ncbi_api_key))) {
    iss$err("ncbi_api_key: must be NULL or a single string")
  } else if (!is.null(ncbi_api_key) && nzchar(ncbi_api_key) &&
             !grepl("^[A-Za-z0-9]{36}$", ncbi_api_key)) {
    iss$warn("ncbi_api_key: does not look like an NCBI API key (36 alphanumeric characters)")
  }

  # Database / pipeline parameters ----
  if (!is.null(custom_seeds_db)) dots$seeds_db <- custom_seeds_db
  if (!is.null(custom_labels_db)) dots$labels_db <- custom_labels_db
  .check_db_params(db_fun, dots, iss)

  .report_issues(iss)
}
