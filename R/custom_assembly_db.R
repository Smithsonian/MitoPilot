#' Build custom GetOrganelle and/or MitoFinder reference databases
#'
#' Downloads mitochondrial records from NCBI GenBank for a given clade and builds
#' reference databases for the MitoPilot Assemble module. Replaces the external
#' Entrez Direct / python / biopython workflow with a pure-R implementation, so no
#' external command-line tools are required.
#'
#' Two database types are supported:
#' \itemize{
#'   \item \strong{GetOrganelle} - a seed FASTA (whole/partial mitogenomes) and a
#'         label FASTA (individual gene sequences).
#'   \item \strong{MitoFinder} - a single GenBank (.gb) file containing only
#'         complete mitochondrial genomes.
#' }
#'
#' Databases are written to a dated, clade-named sub-directory of \code{db_path}
#' (e.g. \code{Percidae_refseq_2026-06-11}), recording when GenBank was accessed.
#' A \code{README.txt} and \code{manifest.json} capture the full query, taxid,
#' record counts, and access time.
#'
#' The historical manual review of unannotated ("nogene") sequences is automated:
#' such sequences are added to the seed database only if they are long enough
#' relative to the complete mitogenomes in the download (see \code{include_nogene} /
#' \code{nogene_min_frac}).
#'
#' @param clade Clade (taxon) name, validated against NCBI taxonomy.
#' @param db_path Directory in which to store the databases. Use a location outside
#'   your MitoPilot project directories so databases can be reused across projects.
#' @param db_type Which database(s) to build: "both" (default), "getorganelle", or "mitofinder".
#' @param refseq_only If TRUE, restrict to RefSeq mitogenomes. Default FALSE (all mitogenomes).
#' @param mito_only If TRUE (default), restrict to records flagged as mitochondrial in origin
#'   (NCBI `mitochondrion[filter]`). Excludes nuclear records that merely mention "mitochondrial"
#'   (e.g. nuclear-encoded mito genes, NUMTs). Set FALSE to broaden recall.
#' @param genomic_only If TRUE (default), restrict to genomic DNA (NCBI `biomol_genomic[PROP]`),
#'   excluding mRNA/rRNA/tRNA/cRNA records. Set FALSE to broaden recall.
#' @param search_terms Optional additional NCBI advanced-query term(s) combined with the
#'   clade via AND, e.g. \code{'"PRJNA720393"[BioProject]'}. Validated before download.
#' @param include_nogene If TRUE (default), add unannotated mitochondrial sequences to the
#'   GetOrganelle seed database when long enough (see \code{nogene_min_frac}).
#' @param nogene_min_frac Keep an unannotated sequence if its length is at least this fraction
#'   of the median complete-mitogenome length. Default 0.8.
#' @param nogene_min_length Absolute minimum length (bp) used when no complete mitogenomes
#'   are available to derive a threshold. Default 12000.
#' @param drop_no_product If TRUE (default), drop single-gene sequences lacking a GenBank
#'   product (often poorly annotated) from the label database.
#' @param retain_genbank If TRUE, keep the raw genbank.gb file for GetOrganelle-only builds.
#'   Ignored when a MitoFinder database is built (the .gb file is the MitoFinder database).
#' @param overwrite If TRUE, replace the dated output directory if it already exists and is
#'   non-empty. If FALSE (default), an existing non-empty output directory is an error.
#' @param resume If TRUE, continue an interrupted download instead of starting over: an
#'   existing output directory for this clade (any date) is reused and only the records not
#'   yet present in its \code{genbank.gb} are downloaded. Any partial trailing record is
#'   trimmed first. Best used soon after an interruption, while the NCBI result set is
#'   unchanged. Default FALSE. Ignored (treated as a fresh run) when no prior download exists.
#' @param api_key Optional NCBI API key (raises the rate limit). Defaults to the
#'   \code{NCBI_API_KEY} or \code{ENTREZ_KEY} environment variable if set.
#'
#' @export
#'
custom_assembly_db <- function(clade,
                               db_path,
                               db_type = c("both", "getorganelle", "mitofinder"),
                               refseq_only = FALSE,
                               mito_only = TRUE,
                               genomic_only = TRUE,
                               search_terms = NULL,
                               include_nogene = TRUE,
                               nogene_min_frac = 0.8,
                               nogene_min_length = 12000,
                               drop_no_product = TRUE,
                               retain_genbank = FALSE,
                               overwrite = FALSE,
                               resume = FALSE,
                               api_key = NULL) {
  db_type <- match.arg(db_type)

  if (missing(clade) || is.null(clade) || !nzchar(trimws(clade))) {
    stop("`clade` is required (a taxon name to query NCBI for).")
  }
  if (missing(db_path) || is.null(db_path) || !nzchar(trimws(db_path))) {
    stop("`db_path` is required (a directory to store the databases, ideally outside any MitoPilot project).")
  }
  clade <- trimws(clade)

  # resolve NCBI API key / polite rate limit
  if (is.null(api_key)) {
    api_key <- Sys.getenv("NCBI_API_KEY", unset = Sys.getenv("ENTREZ_KEY", unset = ""))
    if (!nzchar(api_key)) api_key <- NULL
  }
  delay <- if (is.null(api_key)) 0.34 else 0.11 # max 3 req/s (10 with key)

  build_getorg <- db_type %in% c("both", "getorganelle")
  build_mitofinder <- db_type %in% c("both", "mitofinder")

  # ---- 1. validate clade against NCBI taxonomy ----
  taxid <- .cadb_validate_clade(clade, api_key, delay)
  message("Validated clade '", clade, "' (NCBI taxid: ", taxid, ").")

  # ---- 2. build + validate the GenBank query ----
  query <- .cadb_build_query(clade, search_terms, refseq_only, mito_only, genomic_only)
  se <- .cadb_esearch(query, api_key, delay)

  if (length(se$warnings) > 0) {
    message("NCBI returned warnings for this query:")
    for (w in se$warnings) message("  - ", w)
    if (!is.null(search_terms)) {
      stop("One or more search terms appear to be invalid (see warnings above). ",
           "Fix `search_terms` and try again.")
    }
  }
  if (se$count == 0) {
    stop("No matching mitochondrial records found for this query:\n  ", query)
  }
  message("Found ", se$count, " matching records.")

  # ---- 3. create or locate the output directory ----
  date_str <- format(as.Date(Sys.time(), tz = "UTC"), "%Y-%m-%d")
  access_time <- strftime(as.POSIXlt(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")
  clade_san <- gsub("_+", "_", gsub("[^A-Za-z0-9]+", "_", clade))
  clade_san <- gsub("^_|_$", "", clade_san)
  src_tag <- if (refseq_only) "refseq" else "all"

  resume_from <- 0L
  resume_dir <- if (isTRUE(resume)) .cadb_find_resume_dir(db_path, clade_san, src_tag) else NULL
  if (!is.null(resume_dir)) {
    out_dir <- resume_dir
    gb_file <- file.path(out_dir, "genbank.gb")
    resume_from <- .cadb_trim_partial_gb(gb_file)
    if (resume_from >= se$count) {
      message("All ", se$count, " records already present in ", out_dir,
              "; skipping download.")
    } else {
      message("Resuming download in ", out_dir, ": ", resume_from, " of ", se$count,
              " records already present.")
    }
  } else {
    out_dir <- file.path(db_path, paste0(clade_san, "_", src_tag, "_", date_str))
    if (dir.exists(out_dir) && length(list.files(out_dir, all.files = TRUE, no.. = TRUE)) > 0) {
      if (!isTRUE(overwrite)) {
        stop("Output directory already exists and is not empty:\n  ", out_dir,
             "\nRemove it, set `overwrite = TRUE` to replace its contents, ",
             "or set `resume = TRUE` to continue an interrupted download.")
      }
      unlink(out_dir, recursive = TRUE)
    }
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    gb_file <- file.path(out_dir, "genbank.gb")
  }

  # ---- 4. download records ----
  message("Downloading ", se$count, " GenBank records (this may take a few minutes)...")
  .cadb_efetch(se, se$count, gb_file, rettype = "gb", retmode = "text", api_key, delay,
               what = "GenBank records", query = query, resume_from = resume_from)
  message("Download complete: ", gb_file)

  counts <- list(downloaded = se$count, multigene = 0L, singlegene = 0L,
                 nogene = 0L, nogene_kept = 0L, cds = 0L, mitofinder_complete = 0L)
  outputs <- character(0)

  if (build_getorg) {
    message("Parsing GenBank records, extracting CDS, and classifying sequences...")
    parsed <- .cadb_parse_gb(gb_file)
    recs <- parsed$records
    cds <- parsed$cds
    message("Retained ", nrow(recs), " mitochondrial records (", length(cds),
            " CDS extracted) after parsing.")

    seqs <- Biostrings::DNAStringSet(recs$seq)
    names(seqs) <- recs$accession

    is_multi <- recs$gene_count > 1
    is_nogene <- recs$gene_count == 0 & recs$cds_count == 0
    is_single <- !is_multi & !is_nogene

    counts$multigene <- sum(is_multi)
    counts$singlegene <- sum(is_single)
    counts$nogene <- sum(is_nogene)
    message("Classified records: ", counts$multigene, " multi-gene, ",
            counts$singlegene, " single-gene, ", counts$nogene, " un-annotated.")

    # --- seed database: whole mitogenomes + auto-selected unannotated sequences ---
    message("Building GetOrganelle seed database...")
    seed <- seqs[is_multi]
    if (include_nogene && any(is_nogene)) {
      nogene <- seqs[is_nogene]
      # threshold from complete mitogenome lengths (fall back to absolute min)
      is_complete <- recs$complete
      thresh <- if (any(is_complete)) {
        nogene_min_frac * stats::median(BiocGenerics::width(seqs[is_complete]))
      } else {
        nogene_min_length
      }
      keep <- BiocGenerics::width(nogene) >= thresh
      counts$nogene_kept <- sum(keep)
      basis <- if (any(is_complete)) {
        paste0(round(nogene_min_frac * 100), "% of median complete-mitogenome length")
      } else {
        "absolute minimum (no complete mitogenomes found)"
      }
      message("Auto-selected an additional ", sum(keep), " of ", length(nogene),
              " unannotated sequences for the seed database (length >= ",
              round(thresh), " bp; ", basis, ").")
      seed <- c(seed, nogene[keep])
    }
    seed <- seed[!duplicated(as.character(seed))]

    # --- label database: single-gene records (renamed by product) + extracted CDS ---
    single <- seqs[is_single]
    prod <- recs$product[is_single]
    has_product <- !is.na(prod) & nzchar(prod)
    names(single) <- ifelse(has_product,
                            paste0(gsub(" ", "_", prod), " ", names(single)),
                            paste0("no_product ", names(single)))
    if (drop_no_product) {
      single <- single[has_product]
    }

    counts$cds <- length(cds)

    label <- c(single, cds)
    label <- label[!duplicated(as.character(label))]

    message("Removing duplicate sequences and writing FASTA files...")
    seed_path <- file.path(out_dir, "getorganelle_seed.fasta")
    label_path <- file.path(out_dir, "getorganelle_label.fasta")
    Biostrings::writeXStringSet(seed, seed_path)
    Biostrings::writeXStringSet(label, label_path)
    outputs <- c(outputs, seed = seed_path, label = label_path)
    message("Seed database: ", length(seed), " sequences -> ", basename(seed_path))
    message("Label database: ", length(label), " sequences -> ", basename(label_path))
  }

  # ---- MitoFinder database: complete mitochondrial genomes only ----
  if (build_mitofinder) {
    message("Filtering MitoFinder database to complete mitochondrial genomes...")
    mf_path <- file.path(out_dir, paste0("mitofinder_", clade_san, "_", src_tag, "_", date_str, ".gb"))
    counts$mitofinder_complete <- .cadb_write_mitofinder_db(gb_file, mf_path)
    if (counts$mitofinder_complete == 0) {
      message("WARNING: no complete mitochondrial genomes found; MitoFinder database is empty.")
    }
    message("MitoFinder database: ", counts$mitofinder_complete,
            " complete mitochondrial genomes -> ", basename(mf_path))
    outputs <- c(outputs, mitofinder = mf_path)
  }

  # ---- raw GenBank file: keep only if requested ----
  if (retain_genbank) {
    outputs <- c(outputs, genbank = gb_file)
  } else if (file.exists(gb_file)) {
    file.remove(gb_file)
  }

  # ---- manifest + README ----
  .cadb_write_metadata(out_dir, clade, taxid, query, refseq_only, search_terms,
                       access_time, counts, db_type, outputs)

  # ---- usage instructions ----
  .cadb_print_instructions(out_dir, outputs, build_getorg, build_mitofinder)

  invisible(out_dir)
}


# ---- NCBI E-utilities helpers ----------------------------------------------

#' @noRd
.cadb_req <- function(endpoint, params, api_key = NULL) {
  if (!is.null(api_key)) params$api_key <- api_key
  req <- httr2::request(paste0("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/", endpoint))
  req <- do.call(httr2::req_url_query, c(list(req), params))
  req |>
    httr2::req_user_agent("MitoPilot custom_assembly_db (R package)") |>
    # force HTTP/1.1: NCBI's HTTP/2 endpoint intermittently drops long-running
    # streams (INTERNAL_ERROR), which aborts large batched downloads
    httr2::req_options(http_version = 2) |>
    httr2::req_retry(max_tries = 5, retry_on_failure = TRUE) |>
    httr2::req_perform()
}

#' Validate a clade name against NCBI taxonomy, returning its taxid
#' @noRd
.cadb_validate_clade <- function(clade, api_key, delay) {
  resp <- .cadb_req("esearch.fcgi",
                    list(db = "taxonomy", term = paste0(clade, "[Scientific Name]"), retmax = 1),
                    api_key)
  x <- httr2::resp_body_string(resp)
  Sys.sleep(delay)
  count <- as.integer(.cadb_grab("Count", x))
  if (is.na(count) || count == 0) {
    stop("'", clade, "' is not a valid NCBI taxonomy name. ",
         "Check spelling at https://www.ncbi.nlm.nih.gov/taxonomy")
  }
  id <- regmatches(x, regexpr("<Id>\\d+</Id>", x))
  gsub("</?Id>", "", id)
}

#' Build the GenBank nucleotide query string
#' @noRd
.cadb_build_query <- function(clade, search_terms, refseq_only,
                              mito_only = TRUE, genomic_only = TRUE) {
  q <- paste0('("mitochondrion"[All Fields] OR "mitochondrial"[All Fields])',
              ' AND "', clade, '"[Organism]')
  if (!is.null(search_terms) && nzchar(trimws(search_terms))) {
    q <- paste0(q, " AND ", trimws(search_terms))
  }
  # optional specificity filters (mirror the original efilter -location/-molecule)
  if (isTRUE(mito_only)) {
    q <- paste0(q, " AND mitochondrion[filter]")
  }
  if (isTRUE(genomic_only)) {
    q <- paste0(q, " AND biomol_genomic[PROP]")
  }
  if (isTRUE(refseq_only)) {
    q <- paste0(q, " AND refseq[filter]")
  }
  q
}

#' Run esearch with history; return count, WebEnv, query_key, and any warnings
#' @noRd
.cadb_esearch <- function(query, api_key, delay) {
  resp <- .cadb_req("esearch.fcgi",
                    list(db = "nuccore", term = query, retmax = 0, usehistory = "y"),
                    api_key)
  x <- httr2::resp_body_string(resp)
  Sys.sleep(delay)

  warn_block <- regmatches(x, regexpr("<WarningList>.*?</WarningList>", x))
  warnings <- character(0)
  if (length(warn_block) > 0) {
    warnings <- regmatches(warn_block,
                           gregexpr("<(QuotedPhraseNotFound|PhraseNotFound|FieldNotFound)>[^<]+</", warn_block))[[1]]
    warnings <- sub("</$", "", sub("^<[^>]+>", "", warnings))
  }

  list(
    count = as.integer(.cadb_grab("Count", x)),
    webenv = .cadb_grab("WebEnv", x),
    query_key = .cadb_grab("QueryKey", x),
    query_translation = .cadb_grab("QueryTranslation", x),
    warnings = warnings
  )
}

#' Fetch records from the esearch history in batches, writing to `out_file`
#'
#' NCBI's history server (WebEnv/query_key) can expire or transiently reject a
#' request mid-download (HTTP 400) on long, multi-minute batched fetches. When
#' `query` is supplied, a failed batch triggers a fresh esearch to renew the
#' history session, and the batch is retried before giving up.
#' @noRd
.cadb_efetch <- function(se, count, out_file, rettype, retmode, api_key, delay,
                         what = "records", query = NULL, max_refresh = 5L,
                         resume_from = 0L) {
  batch <- 200L
  report_every <- 1000L
  if (resume_from <= 0L) {
    if (file.exists(out_file)) file.remove(out_file)
    con <- file(out_file, open = "wb")
  } else {
    con <- file(out_file, open = "ab")  # append after the records already downloaded
  }
  on.exit(close(con))
  last_reported <- resume_from
  if (resume_from >= count) {
    cat("\n", file = stderr())
    return(invisible())
  }
  for (start in seq(resume_from, count - 1L, by = batch)) {
    attempt <- 0L
    repeat {
      resp <- tryCatch(
        .cadb_req("efetch.fcgi",
                  list(db = "nuccore", WebEnv = se$webenv, query_key = se$query_key,
                       rettype = rettype, retmode = retmode,
                       retstart = start, retmax = batch),
                  api_key),
        httr2_http = function(e) e
      )
      if (!inherits(resp, "condition")) break
      attempt <- attempt + 1L
      # without a query we cannot renew the history session, so re-raise
      if (is.null(query) || attempt > max_refresh) stop(resp)
      cat(sprintf("\n  ...NCBI request failed at record %d (%s); renewing history session and retrying (%d/%d)\n",
                  start, conditionMessage(resp), attempt, max_refresh), file = stderr())
      Sys.sleep(delay * 3)
      se <- .cadb_esearch(query, api_key, delay)
    }
    writeBin(httr2::resp_body_raw(resp), con)
    fetched <- min(start + batch, count)
    if (fetched - last_reported >= report_every || fetched == count) {
      # overwrite the same console line with \r (no newline until done)
      cat(sprintf("\r  ...downloaded %d of %d %s", fetched, count, what), file = stderr())
      utils::flush.console()
      last_reported <- fetched
    }
    Sys.sleep(delay)
  }
  cat("\n", file = stderr())
}

#' Find a prior output directory for this clade/source to resume into
#'
#' Matches the dated `clade_src_YYYY-MM-DD` naming, requires a `genbank.gb`,
#' and returns the most recent match (or NULL if none).
#' @noRd
.cadb_find_resume_dir <- function(db_path, clade_san, src_tag) {
  if (!dir.exists(db_path)) return(NULL)
  cand <- list.dirs(db_path, recursive = FALSE)
  pat <- paste0("^", clade_san, "_", src_tag, "_\\d{4}-\\d{2}-\\d{2}$")
  cand <- cand[grepl(pat, basename(cand))]
  cand <- cand[file.exists(file.path(cand, "genbank.gb"))]
  if (length(cand) == 0) return(NULL)
  cand[order(basename(cand), decreasing = TRUE)][1]
}

#' Count complete GenBank records in a partial flat file, trimming a partial tail
#'
#' Each GenBank record ends with a line `//`. Scans the file for the `\n//\n`
#' terminator, truncates any bytes after the last complete record (guards against
#' a crash mid-write), and returns the number of complete records. Streams in
#' chunks so multi-GB partial downloads are handled without loading the whole
#' file into memory.
#' @noRd
.cadb_trim_partial_gb <- function(path) {
  size <- file.info(path)$size
  if (is.na(size) || size == 0) return(0L)
  pat <- as.raw(c(0x0A, 0x2F, 0x2F, 0x0A))  # "\n//\n"
  plen <- length(pat)
  con <- file(path, open = "rb")
  count <- 0L
  truncate_at <- 0        # number of bytes through the last complete record
  base <- 0               # 0-based offset of the first byte of `dat`
  carry <- raw(0)
  chunksize <- 4194304L   # 4 MB
  repeat {
    buf <- readBin(con, what = "raw", n = chunksize)
    if (length(buf) == 0L) break
    dat <- c(carry, buf)
    m <- grepRaw(pat, dat, all = TRUE, fixed = TRUE)
    if (length(m) > 0L) {
      count <- count + length(m)
      truncate_at <- base + m[length(m)] - 1 + plen
    }
    keep <- min(length(dat), plen - 1L)  # retain tail so a boundary-spanning "\n//\n" is caught
    base <- base + (length(dat) - keep)
    carry <- utils::tail(dat, keep)
  }
  close(con)
  if (count == 0L) {
    file.remove(path)  # nothing usable; fall back to a fresh download
    return(0L)
  }
  if (truncate_at < size) {
    con2 <- file(path, open = "r+b")
    seek(con2, where = truncate_at, origin = "start")
    truncate(con2)
    close(con2)
  }
  count
}

#' Grab the text content of the first <tag>...</tag> in a string
#' @noRd
.cadb_grab <- function(tag, s) {
  m <- regmatches(s, regexpr(paste0("<", tag, ">[^<]*</", tag, ">"), s))
  if (length(m) == 0) return(NA_character_)
  sub(paste0("</", tag, ">$"), "", sub(paste0("^<", tag, ">"), "", m))
}


# ---- GenBank parsing -------------------------------------------------------

#' Parse a GenBank flat file
#'
#' Returns a list with two elements:
#' \itemize{
#'   \item \code{records} - data frame with columns accession, gene_count
#'     (gene+tRNA+rRNA features), cds_count, product (first product qualifier),
#'     complete (TRUE if DEFINITION says "complete genome"), seq (ORIGIN sequence).
#'   \item \code{cds} - a DNAStringSet of every CDS, extracted locally from the
#'     feature locations + ORIGIN sequence, named "<gene> <accession>".
#' }
#' Only records whose source feature is organelle="mitochondrion" are kept. CDS
#' are extracted from the GenBank file itself, so no separate download is needed.
#' @noRd
.cadb_parse_gb <- function(gb_file) {
  lines <- readLines(gb_file, warn = FALSE)
  ends <- which(lines == "//")
  if (length(ends) == 0) {
    stop("No GenBank records found in ", gb_file)
  }
  starts <- c(1L, utils::head(ends, -1L) + 1L)

  acc <- character(0); gc <- integer(0); cc <- integer(0)
  prod <- character(0); seqs <- character(0); comp <- logical(0)
  cds_seqs <- character(0); cds_names <- character(0)

  for (i in seq_along(starts)) {
    block <- lines[starts[i]:ends[i]]

    # mitochondrion records only
    if (!any(grepl('/organelle="mitochondrion"', block, fixed = TRUE))) next

    accession <- .cadb_grab_version(block)
    if (is.na(accession)) next

    complete <- grepl("complete genome", .cadb_grab_definition(block), ignore.case = TRUE)

    feat_keys <- sub("^ {5}(\\S+) +.*$", "\\1", grep("^ {5}\\S+ +", block, value = TRUE))
    gene_count <- sum(feat_keys %in% c("gene", "tRNA", "rRNA"))
    cds_count <- sum(feat_keys == "CDS")

    product <- NA_character_
    pl <- grep('/product="', block, fixed = TRUE)
    if (length(pl) > 0) {
      product <- sub('.*?/product="([^"]*)".*', "\\1", block[pl[1]])
    }

    origin <- which(block == "ORIGIN" | grepl("^ORIGIN", block))
    seq <- ""
    if (length(origin) > 0 && origin[1] < length(block)) {
      seq_lines <- block[(origin[1] + 1L):(length(block) - 1L)]
      seq <- toupper(gsub("[^A-Za-z]", "", paste(seq_lines, collapse = "")))
    }
    if (!nzchar(seq)) next

    acc <- c(acc, accession)
    gc <- c(gc, gene_count)
    cc <- c(cc, cds_count)
    prod <- c(prod, product)
    comp <- c(comp, complete)
    seqs <- c(seqs, seq)

    # extract CDS nucleotide sequences from this record's features
    if (cds_count > 0 && length(origin) > 0) {
      rec_cds <- .cadb_record_cds(block, origin[1], seq, accession)
      cds_seqs <- c(cds_seqs, rec_cds$seq)
      cds_names <- c(cds_names, rec_cds$name)
    }
  }

  cds <- Biostrings::DNAStringSet(cds_seqs)
  if (length(cds) > 0) names(cds) <- cds_names

  list(
    records = data.frame(accession = acc, gene_count = gc, cds_count = cc,
                         product = prod, complete = comp, seq = seqs,
                         stringsAsFactors = FALSE),
    cds = cds
  )
}

#' Extract CDS nucleotide sequences from one GenBank record block
#'
#' Walks the FEATURES table, and for each CDS feature parses its location
#' (handling join/complement/order and fuzzy <,> markers) and extracts the
#' subsequence from the record's ORIGIN sequence. Returns name + sequence vectors.
#' @noRd
.cadb_record_cds <- function(block, origin_idx, seq, accession) {
  feat_start <- grep("^FEATURES", block)
  if (length(feat_start) == 0) return(list(seq = character(0), name = character(0)))
  region <- (feat_start[1] + 1L):(origin_idx - 1L)
  feat_lines <- region[grepl("^ {5}\\S", block[region])]
  if (length(feat_lines) == 0) return(list(seq = character(0), name = character(0)))

  dna <- Biostrings::DNAString(seq)
  nbp <- length(dna)
  bounds <- c(feat_lines, origin_idx) # end marker for the last feature

  out_seq <- character(0); out_name <- character(0)
  for (k in seq_along(feat_lines)) {
    fi <- feat_lines[k]
    if (sub("^ {5}(\\S+).*$", "\\1", block[fi]) != "CDS") next
    fend <- bounds[k + 1L] - 1L

    # location: remainder of the key line + continuation lines (before qualifiers)
    loc <- sub("^ {5}\\S+ +", "", block[fi])
    j <- fi + 1L
    while (j <= fend && !startsWith(trimws(block[j]), "/")) {
      loc <- paste0(loc, trimws(block[j]))
      j <- j + 1L
    }
    quals <- if (j <= fend) trimws(block[j:fend]) else character(0)

    gene <- NA_character_
    gl <- grep('^/gene="', quals)
    if (length(gl) > 0) gene <- sub('^/gene="([^"]*)".*', "\\1", quals[gl[1]])
    if (is.na(gene)) {
      ql <- grep('^/product="', quals)
      if (length(ql) > 0) gene <- sub('^/product="([^"]*)".*', "\\1", quals[ql[1]])
    }
    if (is.na(gene) || !nzchar(gene)) gene <- "unknown_gene"

    parsed <- .cadb_parse_location(loc)
    if (length(parsed$ranges) == 0) next

    parts <- list()
    ok <- TRUE
    for (r in parsed$ranges) {
      s <- max(1L, r$start); e <- min(nbp, r$end)
      if (s > e) { ok <- FALSE; break }
      piece <- Biostrings::subseq(dna, start = s, end = e)
      if (isTRUE(r$comp)) piece <- Biostrings::reverseComplement(piece)
      parts[[length(parts) + 1L]] <- piece
    }
    if (!ok || length(parts) == 0) next

    cds_seq <- do.call(Biostrings::xscat, parts)
    if (isTRUE(parsed$global_comp)) cds_seq <- Biostrings::reverseComplement(cds_seq)

    out_seq <- c(out_seq, as.character(cds_seq))
    out_name <- c(out_name, paste0(gsub(" ", "_", gene), " ", accession))
  }
  list(seq = out_seq, name = out_name)
}

#' Parse a GenBank feature location string into ranges + strand
#'
#' Handles complement(), join()/order() and fuzzy <,> markers. Returns a list
#' with global_comp (logical) and ranges (each list of start, end, comp).
#' @noRd
.cadb_parse_location <- function(loc) {
  loc <- gsub("\\s", "", loc)
  global_comp <- FALSE
  if (grepl("^complement\\(.*\\)$", loc)) {
    global_comp <- TRUE
    loc <- sub("^complement\\((.*)\\)$", "\\1", loc)
  }
  loc <- sub("^join\\((.*)\\)$", "\\1", loc)
  loc <- sub("^order\\((.*)\\)$", "\\1", loc)

  ranges <- list()
  for (p in strsplit(loc, ",")[[1]]) {
    comp <- FALSE
    if (grepl("^complement\\(.*\\)$", p)) {
      comp <- TRUE
      p <- sub("^complement\\((.*)\\)$", "\\1", p)
    }
    p <- gsub("[<>]", "", p)
    m <- regmatches(p, regexec("^(\\d+)(?:\\.\\.(\\d+))?$", p))[[1]]
    if (length(m) >= 2 && nzchar(m[2])) {
      s <- as.integer(m[2])
      e <- if (length(m) >= 3 && nzchar(m[3])) as.integer(m[3]) else s
      ranges[[length(ranges) + 1L]] <- list(start = s, end = e, comp = comp)
    }
  }
  list(global_comp = global_comp, ranges = ranges)
}

#' Write a MitoFinder database containing only complete mitochondrial genomes
#'
#' Keeps GenBank record blocks whose source feature is organelle="mitochondrion"
#' and whose DEFINITION indicates a complete genome/sequence. Returns the number
#' of records kept.
#' @noRd
.cadb_write_mitofinder_db <- function(gb_file, out_file) {
  lines <- readLines(gb_file, warn = FALSE)
  ends <- which(lines == "//")
  if (length(ends) == 0) {
    stop("No GenBank records found in ", gb_file)
  }
  starts <- c(1L, utils::head(ends, -1L) + 1L)

  con <- file(out_file, open = "wt")
  on.exit(close(con))
  kept <- 0L
  for (i in seq_along(starts)) {
    block <- lines[starts[i]:ends[i]]
    if (!any(grepl('/organelle="mitochondrion"', block, fixed = TRUE))) next

    if (!grepl("complete genome", .cadb_grab_definition(block), ignore.case = TRUE)) next

    writeLines(block, con)
    kept <- kept + 1L
  }
  kept
}

#' Extract the (possibly multi-line) DEFINITION text from a GenBank record block
#' @noRd
.cadb_grab_definition <- function(block) {
  def_start <- grep("^DEFINITION", block)
  if (length(def_start) == 0) return("")
  def <- block[def_start[1]]
  j <- def_start[1] + 1L
  while (j <= length(block) && grepl("^ ", block[j])) {
    def <- paste(def, trimws(block[j]))
    j <- j + 1L
  }
  def
}

#' Extract the VERSION accession from a GenBank record block
#' @noRd
.cadb_grab_version <- function(block) {
  vl <- grep("^VERSION ", block, value = TRUE)
  if (length(vl) == 0) return(NA_character_)
  trimws(sub("^VERSION\\s+(\\S+).*$", "\\1", vl[1]))
}


# ---- metadata + instructions -----------------------------------------------

#' @noRd
.cadb_write_metadata <- function(out_dir, clade, taxid, query, refseq_only,
                                 search_terms, access_time, counts, db_type, outputs) {
  version <- tryCatch(as.character(utils::packageVersion("MitoPilot")),
                      error = function(e) "unknown")
  manifest <- list(
    clade = clade,
    ncbi_taxid = taxid,
    db_type = db_type,
    refseq_only = refseq_only,
    search_terms = search_terms %||% NA_character_,
    query = query,
    genbank_accessed = access_time,
    mitopilot_version = version,
    record_counts = counts,
    output_files = as.list(basename(outputs))
  )
  jsonlite::write_json(manifest, file.path(out_dir, "manifest.json"),
                       pretty = TRUE, auto_unbox = TRUE, null = "null")

  readme <- c(
    "MitoPilot custom assembly database",
    "==================================",
    paste0("Clade:             ", clade, " (NCBI taxid ", taxid, ")"),
    paste0("Database type:     ", db_type),
    paste0("RefSeq only:       ", refseq_only),
    paste0("Additional terms:  ", search_terms %||% "(none)"),
    paste0("GenBank accessed:  ", access_time),
    paste0("MitoPilot version: ", version),
    "",
    "GenBank query:",
    paste0("  ", query),
    "",
    "Record counts:",
    paste0("  downloaded:      ", counts$downloaded),
    paste0("  multi-gene:      ", counts$multigene),
    paste0("  single-gene:     ", counts$singlegene),
    paste0("  no-gene:         ", counts$nogene, " (", counts$nogene_kept, " added to seed db)"),
    paste0("  extracted CDS:   ", counts$cds),
    paste0("  complete genomes:", counts$mitofinder_complete, " (MitoFinder db)"),
    "",
    "Output files:",
    paste0("  ", basename(outputs))
  )
  writeLines(readme, file.path(out_dir, "README.txt"))
}

#' @noRd
.cadb_print_instructions <- function(out_dir, outputs, build_getorg, build_mitofinder) {
  message("\nFINISHED building custom assembly database(s) in:")
  message("  ", normalizePath(out_dir))
  message("\nTo use these databases in MitoPilot:")
  if (build_getorg) {
    message("\n  GetOrganelle - in MitoPilot::new_project(), set:")
    message("    custom_seeds_db  = \"", normalizePath(outputs[["seed"]]), "\"")
    message("    custom_labels_db = \"", normalizePath(outputs[["label"]]), "\"")
    message("  Or in the Shiny app assembly options, paste these paths into the")
    message("  getOrganelle 'Seeds' and 'Labels' database fields.")
  }
  if (build_mitofinder) {
    message("\n  MitoFinder - in MitoPilot::new_project(), set:")
    message("    mitofinder_db = \"", normalizePath(outputs[["mitofinder"]]), "\"")
    message("  Or paste this path into the 'MitoFinder database' field in the Shiny app.")
  }
}
