#' Fetch and parse NCBI GFF3 annotations and FASTA sequence for a BLAST top hit
#'
#' Downloads the GFF3 record and, optionally, the FASTA sequence for the given
#' accession from NCBI EFetch.  The GFF3-derived annotations are written to
#' \code{output_file} as a CSV suitable for ingestion into the
#' \code{blast_ref_annotations} SQLite table.  When \code{sequence_file} is
#' supplied the raw nucleotide sequence (no FASTA header, no line-breaks) is
#' written there for ingestion into the \code{blast_ref_sequences} table.
#'
#' @param accession NCBI accession number (e.g. "NC_012345.1")
#' @param output_file path to write the annotations CSV
#' @param sequence_file optional path to write the plain nucleotide sequence
#' @param genetic_code_file optional path to write the NCBI translation table number
#' @param json_file optional path to write a JSON bundle used by curation
#' @param blast_species optional BLAST hit species label to include in JSON
#' @param blast_evalue optional BLAST hit e-value to include in JSON
#'
#' @export
fetch_blast_ref <- function(accession, output_file, sequence_file = NULL,
                            genetic_code_file = NULL, json_file = NULL,
                            blast_species = NULL, blast_evalue = NULL) {
  message(sprintf("[fetch_blast_ref] START  accession=%s  time=%s", accession, format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
  message(sprintf("[fetch_blast_ref] host=%s  pid=%d", Sys.info()[["nodename"]], Sys.getpid()))
  mem <- tryCatch(gc(verbose = FALSE), error = function(e) NULL)
  if (!is.null(mem)) message(sprintf("[fetch_blast_ref] mem_used_MB=%.1f", sum(mem[, 2])))

  # Optional NCBI API key (set by nextflow process from params.ncbi_api_key).
  # When present, appended to eutils URLs to raise the per-IP rate limit from
  # 3 to 10 req/s. URL-encoded defensively even though valid keys are hex.
  ncbi_api_key <- Sys.getenv("NCBI_API_KEY", unset = "")
  api_key_qs <- if (nzchar(ncbi_api_key)) {
    paste0("&api_key=", utils::URLencode(ncbi_api_key, reserved = TRUE))
  } else ""

  empty <- data.frame(
    gene      = character(),
    type      = character(),
    pos1      = integer(),
    pos2      = integer(),
    direction = character(),
    ref_length = integer()
  )

  write_empty_ref <- function() {
    write.csv(empty, output_file, row.names = FALSE)
    if (!is.null(sequence_file)) writeLines("", sequence_file)
    if (!is.null(genetic_code_file)) writeLines("2", genetic_code_file)
    if (!is.null(json_file)) {
      jsonlite::write_json(list(), json_file, auto_unbox = TRUE)
    }
  }

  # Retry-aware NCBI EFetch helper. Returns the httr2 response on success;
  # logs a message before each retry wait; stops on final failure so the
  # outer tryCatch can re-throw transient errors to Nextflow.
  ncbi_efetch <- function(url, timeout = 120L, label = "") {
    transient <- c(429L, 500L, 503L)
    max_tries <- 5L
    for (attempt in seq_len(max_tries)) {
      safe_url <- sub("&api_key=[^&]*", "&api_key=REDACTED", url)
      message(sprintf("[fetch_blast_ref] HTTP attempt %d/%d  label=%s  url=%s",
                      attempt, max_tries, label, safe_url))
      t0 <- proc.time()[["elapsed"]]
      resp <- tryCatch(
        httr2::request(url) |>
          httr2::req_timeout(timeout) |>
          httr2::req_error(is_error = \(r) FALSE) |>
          httr2::req_perform(),
        error = function(e) e
      )
      elapsed <- proc.time()[["elapsed"]] - t0
      if (inherits(resp, "error")) {
        err_msg <- conditionMessage(resp)
        message(sprintf("[fetch_blast_ref] HTTP error after %.1fs: %s", elapsed, err_msg))
        if (attempt < max_tries) {
          message(sprintf("[fetch_blast_ref] %s attempt %d/%d failed: %s - retrying in %ds",
                          label, attempt, max_tries, err_msg, 120L * attempt))
          Sys.sleep(120L * attempt)
          next
        }
        message(sprintf("[fetch_blast_ref] %s all %d attempts failed: %s",
                        label, max_tries, err_msg))
        stop(err_msg, call. = FALSE)
      }
      status <- httr2::resp_status(resp)
      message(sprintf("[fetch_blast_ref] HTTP %d after %.1fs  label=%s", status, elapsed, label))
      if (status == 200L) return(resp)
      if (attempt < max_tries && status %in% transient) {
        message(sprintf("[fetch_blast_ref] %s attempt %d/%d returned HTTP %d - retrying in %ds",
                        label, attempt, max_tries, status, 120L * attempt))
        Sys.sleep(120L * attempt)
        next
      }
      message(sprintf("[fetch_blast_ref] %s all %d attempts failed: HTTP %d",
                      label, max_tries, status))
      stop(sprintf("HTTP %d", status), call. = FALSE)
    }
  }

  fasta_failed <- FALSE

  tryCatch({
    url <- paste0(
      "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi",
      "?db=nuccore&id=", accession,
      "&rettype=gff3&retmode=text",
      api_key_qs
    )
    gff3_text <- httr2::resp_body_string(
      ncbi_efetch(url, 120L, paste0("GFF3/", accession))
    )

    lines <- strsplit(gff3_text, "\n", fixed = TRUE)[[1]]

    # Sequence length from ##sequence-region pragma
    sr_line <- grep("^##sequence-region", lines, value = TRUE)[1]
    ref_length <- if (!is.na(sr_line)) {
      as.integer(strsplit(trimws(sr_line), "\\s+")[[1]][4])
    } else {
      NA_integer_
    }

    # Data lines only
    data_lines <- lines[!grepl("^#", lines) & nchar(trimws(lines)) > 0]
    if (length(data_lines) == 0) {
      stop("GFF3 response has no data lines for accession ", accession, call. = FALSE)
    }

    fields <- strsplit(data_lines, "\t", fixed = TRUE)
    fields <- fields[vapply(fields, length, integer(1)) >= 9]
    if (length(fields) == 0) {
      stop("GFF3 response has no tab-separated feature lines for accession ", accession, call. = FALSE)
    }

    df <- data.frame(
      feature    = vapply(fields, `[`, character(1), 3),
      pos1       = as.integer(vapply(fields, `[`, character(1), 4)),
      pos2       = as.integer(vapply(fields, `[`, character(1), 5)),
      direction  = vapply(fields, `[`, character(1), 7),
      attributes = vapply(fields, `[`, character(1), 9),
      stringsAsFactors = FALSE
    )

    # Extract a named attribute from GFF3 attributes string.
    # Uses sub() with a full-string pattern so the result is always the same
    # length as attrs (no elements dropped for non-matches).
    get_attr <- function(attrs, key) {
      has_key <- grepl(paste0("(?:^|;)", key, "="), attrs, perl = TRUE)
      ifelse(
        has_key,
        sub(paste0("^.*(?:^|;)", key, "=([^;]*).*$"), "\\1", attrs, perl = TRUE),
        NA_character_
      )
    }

    df$gbkey      <- get_attr(df$attributes, "gbkey")
    df$gene_nm    <- get_attr(df$attributes, "gene")
    df$product    <- get_attr(df$attributes, "product")
    df$reg_cls    <- get_attr(df$attributes, "regulatory_class")
    df$note       <- get_attr(df$attributes, "Note")
    df$transl_tbl <- get_attr(df$attributes, "transl_table")

    # GFF region organism attribute is best-effort fallback (often absent on RefSeq)
    region_attrs <- df$attributes[df$feature == "region"]
    gff_organism <- if (length(region_attrs) > 0L && !is.na(region_attrs[1])) {
      org <- get_attr(region_attrs[1], "organism")
      if (!is.na(org) && nzchar(org)) org else NULL
    } else NULL

    # Extract NCBI taxonomy ID from region Dbxref attribute (e.g. "taxon:9606")
    taxid <- tryCatch({
      dbxref <- if (length(region_attrs) > 0L && !is.na(region_attrs[1])) {
        get_attr(region_attrs[1], "Dbxref")
      } else NA_character_
      if (is.na(dbxref)) {
        NULL
      } else {
        m <- regmatches(dbxref, regexpr("(?<=taxon:)\\d+", dbxref, perl = TRUE))
        if (length(m) > 0L && nzchar(m)) m else NULL
      }
    }, error = function(e) NULL)

    # Fetch taxonomy XML once; derive both species name and condensed lineage.
    # NCBI occasionally returns HTTP 200 with an error payload inside <TaxaSet>
    # when its taxonomy backend times out. A valid record always carries a
    # <ScientificName>, so treat its absence as a transient failure and stop()
    # to let Nextflow's errorStrategy retry the task, rather than silently
    # writing null organism/lineage.
    tax_xml <- if (!is.null(taxid)) {
      tax_url <- paste0(
        "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi",
        "?db=taxonomy&id=", taxid, "&retmode=xml",
        api_key_qs
      )
      body <- httr2::resp_body_string(
        ncbi_efetch(tax_url, 60L, paste0("taxonomy/", taxid))
      )
      if (!grepl("<ScientificName>", body, fixed = TRUE)) {
        stop(sprintf(
          "taxonomy/%s returned no usable record (likely NCBI backend timeout) - retrying via Nextflow",
          taxid
        ), call. = FALSE)
      }
      body
    } else NULL

    # Clean species name = first <ScientificName> before <LineageEx>
    tax_species <- if (!is.null(tax_xml) && nzchar(tax_xml)) {
      prefix <- sub("(?s)<LineageEx>.*", "", tax_xml, perl = TRUE)
      m <- regmatches(prefix, regexpr("(?<=<ScientificName>)[^<]+", prefix, perl = TRUE))
      if (length(m) > 0L && nzchar(m)) m else NULL
    } else NULL

    organism <- tax_species %||% gff_organism

    # Phylum; order; family from <LineageEx>
    lineage <- if (!is.null(tax_xml) && nzchar(tax_xml)) {
      tryCatch({
        lex_m <- regmatches(tax_xml, regexpr("(?s)<LineageEx>.*?</LineageEx>", tax_xml, perl = TRUE))
        if (length(lex_m) == 0L) {
          NULL
        } else {
          taxon_blocks <- regmatches(lex_m, gregexpr("(?s)<Taxon>.*?</Taxon>", lex_m, perl = TRUE))[[1]]
          get_tag <- function(block, tag) {
            pat <- paste0("(?<=<", tag, ">)[^<]+")
            m <- regmatches(block, regexpr(pat, block, perl = TRUE))
            if (length(m) > 0L && nzchar(m)) m else NA_character_
          }
          ranks <- vapply(taxon_blocks, get_tag, character(1), tag = "Rank")
          names_vec <- vapply(taxon_blocks, get_tag, character(1), tag = "ScientificName")
          keep <- ranks %in% c("phylum", "order", "family") & !is.na(names_vec)
          if (any(keep)) paste(names_vec[keep], collapse = "; ") else NULL
        }
      }, error = function(e) NULL)
    } else NULL

    # Extract genetic code from CDS transl_table attribute; default 2
    cds_gc <- df$transl_tbl[df$feature == "CDS" & !is.na(df$transl_tbl)]
    genetic_code_num <- if (length(cds_gc) > 0) as.integer(cds_gc[1]) else 2L
    if (!is.null(genetic_code_file)) {
      writeLines(as.character(genetic_code_num), genetic_code_file)
    }

    # Classify feature type then drop anything that doesn't match
    df$type <- dplyr::case_when(
      df$feature == "CDS"  | (!is.na(df$gbkey) & df$gbkey == "CDS") ~ "PCG",
      df$feature == "tRNA"  ~ "tRNA",
      df$feature == "rRNA"  ~ "rRNA",
      df$feature == "D-loop" |
        (!is.na(df$gbkey)   & df$gbkey == "D_loop") |
        (!is.na(df$reg_cls) & grepl("control", df$reg_cls, ignore.case = TRUE)) |
        (!is.na(df$note)    & grepl("control region|d-loop|d loop|displacement loop", df$note, ignore.case = TRUE)) ~ "ctrl",
      .default = NA_character_
    )

    df <- df[!is.na(df$type), ]
    if (nrow(df) == 0) {
      stop("GFF3 has no recognized mitochondrial features for accession ", accession, call. = FALSE)
    }

    # Best available name for each row
    df$raw <- dplyr::coalesce(df$gene_nm, df$product)

    # Normalize gene names to MitoPilot convention
    df$gene <- mapply(
      normalize_mito_gene,
      df$raw, df$type, df$product,
      USE.NAMES = FALSE
    )

    df$direction <- dplyr::if_else(
      df$direction %in% c("+", "-"), df$direction, "+"
    )

    result <- df[!is.na(df$gene) & !is.na(df$type),
                 c("gene", "type", "pos1", "pos2", "direction")]
    result$ref_length <- ref_length

    write.csv(result, output_file, row.names = FALSE)

    # Optionally fetch and write the reference nucleotide sequence (with retries)
    seq_str <- ""
    if (!is.null(sequence_file)) {
      fasta_url <- paste0(
        "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi",
        "?db=nuccore&id=", accession,
        "&rettype=fasta&retmode=text",
        api_key_qs
      )
      fasta_text <- tryCatch(
        httr2::resp_body_string(
          ncbi_efetch(fasta_url, 120L, paste0("FASTA/", accession))
        ),
        error = function(e) ""
      )
      # Strip FASTA header line(s) and collapse sequence lines into one string
      seq_lines <- strsplit(fasta_text, "\n", fixed = TRUE)[[1]]
      seq_lines <- seq_lines[!grepl("^>", seq_lines) & nchar(trimws(seq_lines)) > 0]
      seq_str   <- paste(trimws(seq_lines), collapse = "")
      # Only write if it looks like a valid nucleotide sequence (not an API error)
      if (nzchar(seq_str) && grepl("^[ACGTNacgtnRYSWKMBDHVryswkmbdhv]+$", seq_str)) {
        writeLines(seq_str, sequence_file)
      } else {
        seq_str <- ""
        writeLines("", sequence_file)
        fasta_failed <- TRUE
      }
    }

    if (!is.null(json_file)) {
      pcg <- result[result$type == "PCG", c("gene", "pos1", "pos2", "direction")]
      ref_json <- list(
        accession = accession,
        blast_species = blast_species %||% accession,
        blast_evalue = blast_evalue,
        organism = organism,
        lineage = lineage,
        sequence = seq_str,
        genetic_code = genetic_code_num,
        pcg = pcg
      )
      jsonlite::write_json(ref_json, json_file, auto_unbox = TRUE, null = "null")
    }

  }, error = function(e) {
    msg <- conditionMessage(e)
    message("fetch_blast_ref error for ", accession, ": ", msg)
    stop(msg, call. = FALSE)
  })

  # Signal failure to Nextflow so the process can retry on transient FASTA fetch errors
  if (isTRUE(fasta_failed)) {
    stop("fetch_blast_ref: FASTA sequence fetch failed for ", accession,
         " after retries", call. = FALSE)
  }
  message(sprintf("[fetch_blast_ref] DONE  accession=%s  time=%s", accession, format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
  invisible(NULL)
}


#' Prepend remote BLAST top-hit translations to per-gene refHits in annotations
#'
#' Called in each \code{curate_*_mito()} function after the initial
#' \code{refHits} column is populated by \code{get_top_hits}.  For each PCG
#' row in \code{annotations}, all matching gene regions are extracted from
#' the remote BLAST reference sequence, translated using the genetic code
#' recorded in GenBank for that accession, and inserted as the first rows of
#' that gene's \code{refHits} JSON so they appear at the top of the alignment
#' table in the annotate details view.  When a gene has multiple copies in the
#' reference, each copy is given a distinct name
#' \code{<accession>:<pos1>-<pos2>} so they can be told apart.
#'
#' @param annotations The annotations data frame (with a \code{refHits} column)
#' @param blast_ref_file Path to a JSON file staged by Nextflow containing blast
#'   reference data (accession, blast_species, sequence, genetic_code, pcg list)
#'
#' @return The \code{annotations} data frame with updated \code{refHits} values
#'
#' @noRd
prepend_blast_hit_to_refhits <- function(annotations, blast_ref_file) {
  if (is.null(blast_ref_file) || !nzchar(blast_ref_file) || !file.exists(blast_ref_file)) {
    return(annotations)
  }

  ref_data <- tryCatch(
    jsonlite::fromJSON(blast_ref_file, simplifyDataFrame = TRUE),
    error = function(e) {
      message("prepend_blast_hit: cannot read blast_ref_file: ", e$message)
      NULL
    }
  )
  if (is.null(ref_data) || length(ref_data) == 0) return(annotations)

  accession    <- ref_data$accession
  organism     <- ref_data$organism
  taxon        <- if (!is.null(organism) && !is.na(organism) && nzchar(organism)) organism else (ref_data$blast_species %||% accession)
  blast_evalue <- ref_data$blast_evalue %||% 0
  if (is.null(accession) || is.na(accession) || accession %in% c("", "NO HIT")) {
    return(annotations)
  }

  ref_seq <- ref_data$sequence
  if (is.null(ref_seq) || !nzchar(ref_seq)) return(annotations)

  gc_num <- ref_data$genetic_code
  gc_num <- if (!is.null(gc_num) && !is.na(gc_num) && gc_num > 0) as.character(gc_num) else "2"
  ref_gc <- tryCatch(
    Biostrings::getGeneticCode(gc_num),
    error = function(e) Biostrings::getGeneticCode("2")
  )

  pcg <- ref_data$pcg
  if (is.null(pcg) || !is.data.frame(pcg) || nrow(pcg) == 0) return(annotations)

  ref_dna <- Biostrings::DNAString(ref_seq)

  # Translate one gene region; returns NULL on failure
  translate_ref_gene <- function(pos1, pos2, dir) {
    tryCatch({
      s <- Biostrings::subseq(ref_dna, start = pos1, end = pos2)
      if (identical(dir, "-")) s <- Biostrings::reverseComplement(s)
      len <- floor(length(s) / 3L) * 3L
      prot <- Biostrings::translate(Biostrings::subseq(s, 1L, len),
                                    genetic.code = ref_gc,
                                    if.fuzzy.codon = "solve")
      prot_str <- as.character(prot)
      if (endsWith(prot_str, "*")) prot_str <- substr(prot_str, 1L, nchar(prot_str) - 1L)
      prot_str
    }, error = function(e) NULL)
  }

  # Determine whether a gene has multiple copies in the reference
  gene_counts <- table(pcg$gene)

  metric_or_na <- function(expr) {
    tryCatch(expr, error = function(e) NA_real_)
  }

  # Update each PCG row's refHits
  annotations$refHits <- purrr::pmap_chr(
    list(annotations$type, annotations$gene, annotations$translation, annotations$refHits),
    function(type, gene, translation, refHits_json) {
      if (type != "PCG") return(refHits_json)

      pcg_rows <- pcg[pcg$gene == gene, , drop = FALSE]
      if (nrow(pcg_rows) == 0) return(refHits_json)

      multi_copy <- gene_counts[gene] > 1L

      hit_rows <- purrr::pmap_dfr(pcg_rows, function(gene, pos1, pos2, direction) {
        ref_aa <- translate_ref_gene(pos1, pos2, direction)
        if (is.null(ref_aa) || !nzchar(ref_aa) ||
            is.na(translation) || !nzchar(translation)) return(NULL)

        # Use coordinates in the name only when the gene appears more than once
        seq_name <- if (multi_copy) {
          paste0(accession, ":", pos1, "-", pos2)
        } else {
          accession
        }

        tryCatch(

          data.frame(
            acc          = seq_name,
            Taxon        = paste0(seq_name, "_", taxon),
            eval         = blast_evalue,
            target       = ref_aa,
            pctid        = metric_or_na(compare_aa(translation, ref_aa, "pctId")),
            similarity   = metric_or_na(compare_aa(translation, ref_aa, "similarity")),
            gap_leading  = metric_or_na(count_end_gaps(translation, ref_aa, "leading")),
            gap_trailing = metric_or_na(count_end_gaps(translation, ref_aa, "trailing")),
            stringsAsFactors = FALSE
          ),
          error = function(e) NULL
        )
      })

      if (nrow(hit_rows) == 0) return(refHits_json)

      existing <- json_parse(refHits_json[[1]] %|NA|% "{}", tibble = TRUE)
      has_existing <- is.data.frame(existing) && nrow(existing) > 0
      if (has_existing && "acc" %in% names(existing)) {
        existing <- existing[!existing$acc %in% hit_rows$acc, , drop = FALSE]
        has_existing <- nrow(existing) > 0
      }
      combined <- if (has_existing) dplyr::bind_rows(hit_rows, existing) else hit_rows
      json_string(combined) %||% refHits_json
    }
  )

  message("prepend_blast_hit: added ", accession, " to refHits")
  annotations
}


#' Append remote BLAST top-hit gene translations to local curation BLAST DB
#'
#' Translates each PCG region recorded in \code{blast_ref_file} from the remote
#' reference nucleotide sequence (using the genetic code recorded in GenBank for
#' that accession) and appends the resulting amino-acid sequences to the
#' per-gene FASTA files in the local curation BLAST database
#' (\code{<ref_dir>/featureProt/<gene>.fas}). After all sequences are appended,
#' \code{makeblastdb -dbtype prot} is run on each modified FASTA to rebuild the
#' BLAST index so subsequent \code{blastp} calls against the same gene FASTA
#' include the remote-derived sequence.
#'
#' Header format: \code{>{accession}[:pos1-pos2] {tag} {species}}. The
#' coordinate suffix is included only when the gene appears more than once in
#' the reference. The short \code{tag} marks the sequence as remote-derived to
#' distinguish it from the curated local DB entries in the BLAST hits table.
#'
#' Designed to run inside a Nextflow curation task where the reference DB has
#' been staged with \code{stageInMode 'copy'}; modifications are confined to
#' the task's private copy and do not bleed across samples.
#'
#' @param blast_ref_file path to remote BLAST hit JSON staged by Nextflow
#' @param ref_dir path to the curation BLAST DB root (containing a
#'   \code{featureProt/} subdirectory of per-gene FASTAs)
#' @param tag short tag (default \code{"[remote]"}) prepended to the species
#'   name in the FASTA header to indicate remote origin
#' @param path_to_makeblastdb optional explicit path to the \code{makeblastdb}
#'   executable; falls back to \code{Sys.which("makeblastdb")}
#'
#' @return Character vector of gene names whose FASTAs were modified
#'   (invisible).
#'
#' @noRd
inject_remote_hits_into_blast_db <- function(blast_ref_file, ref_dir,
                                              tag = "[remote]",
                                              path_to_makeblastdb = NULL) {
  if (is.null(blast_ref_file) || !nzchar(blast_ref_file) ||
      !file.exists(blast_ref_file)) {
    return(invisible(character()))
  }
  if (is.null(ref_dir) || !nzchar(ref_dir) || !dir.exists(ref_dir)) {
    message("inject_remote_hits: ref_dir not found: ", ref_dir)
    return(invisible(character()))
  }
  feature_prot_dir <- file.path(ref_dir, "featureProt")
  if (!dir.exists(feature_prot_dir)) {
    message("inject_remote_hits: featureProt/ not found in ref_dir")
    return(invisible(character()))
  }

  ref_data <- tryCatch(
    jsonlite::fromJSON(blast_ref_file, simplifyDataFrame = TRUE),
    error = function(e) NULL
  )
  if (is.null(ref_data) || length(ref_data) == 0) {
    return(invisible(character()))
  }

  accession <- ref_data$accession
  if (is.null(accession) || is.na(accession) ||
      accession %in% c("", "NO HIT")) {
    return(invisible(character()))
  }

  organism <- ref_data$organism
  species  <- if (!is.null(organism) && !is.na(organism) && nzchar(organism)) {
    organism
  } else {
    accession
  }

  ref_seq <- ref_data$sequence
  if (is.null(ref_seq) || !nzchar(ref_seq)) return(invisible(character()))

  pcg <- ref_data$pcg
  if (is.null(pcg) || !is.data.frame(pcg) || nrow(pcg) == 0) {
    return(invisible(character()))
  }

  gc_num <- ref_data$genetic_code
  gc_num <- if (!is.null(gc_num) && !is.na(gc_num) && gc_num > 0) {
    as.character(gc_num)
  } else "2"
  ref_gc <- tryCatch(
    Biostrings::getGeneticCode(gc_num),
    error = function(e) Biostrings::getGeneticCode("2")
  )

  ref_dna <- tryCatch(Biostrings::DNAString(ref_seq),
                      error = function(e) NULL)
  if (is.null(ref_dna)) return(invisible(character()))

  # Resolve makeblastdb. Missing executable is non-fatal - sequences are still
  # appended to the FASTA so a later DB rebuild (or BLAST recompute) can pick
  # them up. We just skip the index rebuild here.
  mkdb <- if (!is.null(path_to_makeblastdb) &&
              file.exists(path_to_makeblastdb)) {
    path_to_makeblastdb
  } else {
    p <- unname(Sys.which("makeblastdb"))
    if (nzchar(p)) p else NULL
  }
  if (is.null(mkdb)) {
    message("inject_remote_hits: makeblastdb not found in PATH; ",
            "DB index will not be rebuilt")
  }

  translate_region <- function(pos1, pos2, dir) {
    tryCatch({
      s <- Biostrings::subseq(ref_dna, start = pos1, end = pos2)
      if (identical(dir, "-")) s <- Biostrings::reverseComplement(s)
      len <- floor(length(s) / 3L) * 3L
      if (len < 3L) return(NULL)
      prot <- Biostrings::translate(Biostrings::subseq(s, 1L, len),
                                    genetic.code = ref_gc,
                                    if.fuzzy.codon = "solve")
      prot_str <- as.character(prot)
      if (endsWith(prot_str, "*")) {
        prot_str <- substr(prot_str, 1L, nchar(prot_str) - 1L)
      }
      if (!nzchar(prot_str)) NULL else prot_str
    }, error = function(e) NULL)
  }

  gene_counts <- table(pcg$gene)

  # Collect translations per gene so we can append + rebuild once per FASTA
  per_gene <- list()
  for (i in seq_len(nrow(pcg))) {
    gene <- pcg$gene[i]
    fas  <- file.path(feature_prot_dir, paste0(gene, ".fas"))
    if (!file.exists(fas)) next

    aa <- translate_region(pcg$pos1[i], pcg$pos2[i], pcg$direction[i])
    if (is.null(aa)) next

    seq_name <- if (gene_counts[gene] > 1L) {
      paste0(accession, ":", pcg$pos1[i], "-", pcg$pos2[i])
    } else {
      accession
    }
    header <- paste0(seq_name, " ", tag, " ", species)
    per_gene[[gene]]$headers <- c(per_gene[[gene]]$headers, header)
    per_gene[[gene]]$seqs    <- c(per_gene[[gene]]$seqs,    aa)
  }

  if (length(per_gene) == 0) return(invisible(character()))

  modified <- character()
  for (gene in names(per_gene)) {
    fas <- file.path(feature_prot_dir, paste0(gene, ".fas"))
    set <- Biostrings::AAStringSet(
      setNames(per_gene[[gene]]$seqs, per_gene[[gene]]$headers)
    )
    tryCatch({
      Biostrings::writeXStringSet(set, fas, append = TRUE)
      modified <- c(modified, gene)
    }, error = function(e) {
      message("inject_remote_hits: failed to append to ", fas, ": ",
              conditionMessage(e))
    })
  }

  if (!is.null(mkdb)) {
    for (gene in modified) {
      fas <- file.path(feature_prot_dir, paste0(gene, ".fas"))
      system2(mkdb, args = c("-dbtype", "prot", "-in", fas),
              stdout = NULL, stderr = NULL)
    }
  }

  message("inject_remote_hits: appended ", accession, " sequences to ",
          length(modified), " gene FASTA(s)")
  invisible(modified)
}


#' Normalize NCBI mitochondrial gene names to MitoPilot convention
#'
#' Accepts a raw gene/product name from an NCBI GFF3 record and returns the
#' canonical MitoPilot gene symbol (e.g. "nad1", "cox1", "rrnS", "trnL"). When
#' no confident normalization is possible, the original name is returned with
#' a "?" prefix so anomalies are visible in downstream UI (synteny plot label).
#'
#' Handles a wide range of NCBI naming variants encountered across vertebrate
#' and invertebrate mitogenomes, including:
#'   * MT-/mt-/m- HGNC-style prefixes
#'   * NADH aliases: nad/nd/ndh/nadh + 1..6, 4L
#'   * COX aliases: cox/co/coi-iii using Roman or Arabic numerals
#'   * ATP synthase aliases: atp6/8, ATPase 6/8, ATP synthase F0 subunit X
#'   * cytochrome b: cob/cytb/cyb/cytob
#'   * Product descriptions ("NADH dehydrogenase subunit 1", etc.)
#'   * rRNA: rrnS/L, s-rRNA/l-rRNA, 12S/16S, RNR1/2, mtSSU/mtLSU
#'   * tRNA: trnX, TRN-X, tRNA-Ala, tRNA-A, tRNAAla, "transfer RNA-Ala",
#'     anticodon suffixes (e.g. -UGC, (ugc)), codon-family parens
#'     (e.g. tRNA-Leu (CUN)), and isoacceptor numbers (trnL1 -> trnL)
#'
#' @param name raw gene/product name from NCBI GFF3
#' @param type one of "PCG", "tRNA", "rRNA", "ctrl"
#' @param product product name (used as fallback when name is missing or
#'   doesn't normalize)
#'
#' @noRd
normalize_mito_gene <- function(name, type, product = NA_character_) {
  if ((is.na(name) || nchar(trimws(name)) == 0) &&
      (is.na(product) || nchar(trimws(product)) == 0)) {
    return(NA_character_)
  }
  if (is.na(name) || nchar(trimws(name)) == 0) name <- product
  n_raw <- trimws(name)

  if (is.na(type)) return(flag_unknown_gene(n_raw))
  if (type == "ctrl") return("ctrl")

  # Strip mt-/MT-/mtDNA-/m- prefixes uniformly so downstream lookups work.
  n <- sub("^(?:mt[-_ ]?dna[-_ ]|mt[-_ ]|m[-_ ])", "", n_raw,
           ignore.case = TRUE, perl = TRUE)

  result <- switch(
    type,
    "PCG"  = normalize_pcg(n, product),
    "rRNA" = normalize_rrna(n, product),
    "tRNA" = normalize_trna(n, product),
    NA_character_
  )
  if (!is.na(result)) return(result)
  flag_unknown_gene(n_raw)
}

#' Flag a gene name as unrecognized.
#' @noRd
flag_unknown_gene <- function(x) {
  if (is.na(x) || nchar(trimws(x)) == 0) return(NA_character_)
  paste0("?", trimws(x))
}

#' Normalize a protein-coding gene name. Returns NA if no match.
#' @noRd
normalize_pcg <- function(n, product = NA_character_) {
  pcg_lookup <- c(
    # NADH dehydrogenase subunits
    "nad1"="nad1", "nd1"="nad1", "ndh1"="nad1", "nadh1"="nad1",
    "nad2"="nad2", "nd2"="nad2", "ndh2"="nad2", "nadh2"="nad2",
    "nad3"="nad3", "nd3"="nad3", "ndh3"="nad3", "nadh3"="nad3",
    "nad4"="nad4", "nd4"="nad4", "ndh4"="nad4", "nadh4"="nad4",
    "nad4l"="nad4l", "nd4l"="nad4l", "ndh4l"="nad4l", "nadh4l"="nad4l",
    "nad5"="nad5", "nd5"="nad5", "ndh5"="nad5", "nadh5"="nad5",
    "nad6"="nad6", "nd6"="nad6", "ndh6"="nad6", "nadh6"="nad6",
    # Cytochrome c oxidase
    "cox1"="cox1", "co1"="cox1", "coi"="cox1", "coxi"="cox1",
    "cox2"="cox2", "co2"="cox2", "coii"="cox2", "coxii"="cox2",
    "cox3"="cox3", "co3"="cox3", "coiii"="cox3", "coxiii"="cox3",
    # ATP synthase
    "atp6"="atp6", "atpase6"="atp6", "atp synthase 6"="atp6",
    "atp8"="atp8", "atpase8"="atp8", "atp synthase 8"="atp8",
    # Cytochrome b
    "cob"="cob", "cytb"="cob", "cyb"="cob", "cytob"="cob",
    "cytochromeb"="cob"
  )

  key <- tolower(trimws(n))
  result <- pcg_lookup[key]
  if (!is.na(result)) return(unname(result))

  # Try collapsing all separators: "nd-1" -> "nd1", "ATP 6" -> "atp6"
  key_collapsed <- gsub("[[:space:]_-]+", "", key)
  result <- pcg_lookup[key_collapsed]
  if (!is.na(result)) return(unname(result))

  # Try parsing the name as a free-text product description
  result <- pcg_from_product(n)
  if (!is.na(result)) return(result)

  # Fall back to product field if it differs from name
  if (!is.na(product) && nchar(trimws(product)) > 0 && product != n) {
    pkey <- tolower(trimws(product))
    result <- pcg_lookup[pkey]
    if (!is.na(result)) return(unname(result))
    result <- pcg_lookup[gsub("[[:space:]_-]+", "", pkey)]
    if (!is.na(result)) return(unname(result))
    result <- pcg_from_product(product)
    if (!is.na(result)) return(result)
  }
  NA_character_
}

#' Parse a free-text product description into a canonical PCG symbol.
#' @noRd
pcg_from_product <- function(product) {
  p <- tolower(trimws(product))
  if (nchar(p) == 0) return(NA_character_)

  # NADH dehydrogenase subunit X (X may be "4L" / "4l")
  m <- regmatches(p, regexpr("nadh[^a-z0-9]*(?:dehydrogenase)?[^a-z0-9]*(?:subunit)?[^a-z0-9]*([0-9]+l?)", p, perl = TRUE))
  if (length(m) > 0 && nchar(m) > 0) {
    sub_id <- regmatches(m, regexpr("[0-9]+l?$", m, perl = TRUE))
    return(paste0("nad", sub_id))
  }

  # Cytochrome c oxidase subunit X (Roman or Arabic)
  m <- regmatches(p, regexpr("cytochrome[^a-z0-9]*(?:c)?[^a-z0-9]*oxidase[^a-z0-9]*(?:subunit)?[^a-z0-9]*(i{1,3}|[0-9]+)", p, perl = TRUE))
  if (length(m) > 0 && nchar(m) > 0) {
    sub_id <- regmatches(m, regexpr("(i{1,3}|[0-9]+)$", m, perl = TRUE))
    arabic <- switch(sub_id, "i" = "1", "ii" = "2", "iii" = "3", sub_id)
    return(paste0("cox", arabic))
  }

  # Cytochrome b / apocytochrome b
  if (grepl("(?:apo)?cytochrome[^a-z0-9]*b\\b", p, perl = TRUE)) {
    return("cob")
  }

  # ATP synthase F0 subunit X / ATPase X
  m <- regmatches(p, regexpr("(?:atp[^a-z0-9]*synthase|atpase)[^0-9]*([0-9]+)", p, perl = TRUE))
  if (length(m) > 0 && nchar(m) > 0) {
    sub_id <- regmatches(m, regexpr("[0-9]+$", m, perl = TRUE))
    return(paste0("atp", sub_id))
  }

  NA_character_
}

#' Normalize a ribosomal RNA name. Returns NA if no match.
#' @noRd
normalize_rrna <- function(n, product = NA_character_) {
  rrna_lookup <- c(
    "s-rrna"="rrnS", "srrna"="rrnS", "rnr1"="rrnS", "rrns"="rrnS",
    "rrn12"="rrnS", "12s"="rrnS", "12s rrna"="rrnS",
    "12s ribosomal rna"="rrnS", "small subunit ribosomal rna"="rrnS",
    "ssu rrna"="rrnS", "ssu-rrna"="rrnS", "mtssu"="rrnS",
    "12s mitochondrial rrna"="rrnS",
    "l-rrna"="rrnL", "lrrna"="rrnL", "rnr2"="rrnL", "rrnl"="rrnL",
    "rrn16"="rrnL", "16s"="rrnL", "16s rrna"="rrnL",
    "16s ribosomal rna"="rrnL", "large subunit ribosomal rna"="rrnL",
    "lsu rrna"="rrnL", "lsu-rrna"="rrnL", "mtlsu"="rrnL",
    "16s mitochondrial rrna"="rrnL"
  )

  try_match <- function(x) {
    if (is.na(x) || nchar(trimws(x)) == 0) return(NA_character_)
    key <- tolower(trimws(x))
    key <- gsub("\\s+", " ", key)
    r <- rrna_lookup[key]
    if (!is.na(r)) return(unname(r))
    r <- rrna_lookup[gsub("[[:space:]_-]+", "", key)]
    if (!is.na(r)) return(unname(r))
    if (grepl("\\b12s\\b|small\\s*subunit|\\bssu\\b", key, perl = TRUE)) return("rrnS")
    if (grepl("\\b16s\\b|large\\s*subunit|\\blsu\\b", key, perl = TRUE)) return("rrnL")
    NA_character_
  }

  result <- try_match(n)
  if (!is.na(result)) return(result)
  if (!is.na(product) && product != n) {
    result <- try_match(product)
    if (!is.na(result)) return(result)
  }
  NA_character_
}

#' Normalize a tRNA name. Returns NA if no match.
#' @noRd
normalize_trna <- function(n, product = NA_character_) {
  aa3_to_letter <- c(
    "ala"="A", "cys"="C", "asp"="D", "glu"="E",
    "phe"="F", "gly"="G", "his"="H", "ile"="I",
    "lys"="K", "leu"="L", "met"="M", "asn"="N",
    "pro"="P", "gln"="Q", "arg"="R", "ser"="S",
    "thr"="T", "val"="V", "trp"="W", "tyr"="Y",
    # Common variants of methionine
    "fme"="M", "fmt"="M", "ime"="M"
  )
  valid_letters <- c("A","C","D","E","F","G","H","I","K","L",
                     "M","N","P","Q","R","S","T","V","W","Y")

  try_match <- function(x) {
    if (is.na(x) || nchar(trimws(x)) == 0) return(NA_character_)
    s <- trimws(x)

    # Expand "transfer RNA" -> "trna"
    s <- sub("(?i)\\btransfer\\s*rna\\b", "trna", s, perl = TRUE)
    # Strip codon-family parens like "(CUN)", "(UUR)", "(AGY)"
    s <- sub("\\s*\\([^)]*\\)\\s*$", "", s, perl = TRUE)
    # Strip all-uppercase 3-letter anticodon suffix "trnA-UGC"
    s <- sub("[-_][A-Z]{3}$", "", s, perl = TRUE)
    # Strip trailing isoacceptor number "trnL1" -> "trnL"
    s_stripped <- sub("[0-9]+$", "", s)

    # tRNA-XXX or trnXXX with explicit 3-letter amino acid code
    # (do this BEFORE single-letter match so "tRNA-Ala" doesn't become "trnA")
    m <- regmatches(
      s_stripped,
      regexpr("(?i)\\bt(?:rna?)?\\s*[-_]?\\s*([A-Za-z]{3})\\b\\s*$",
              s_stripped, perl = TRUE)
    )
    if (length(m) > 0 && nchar(m) > 0) {
      aa <- tolower(regmatches(m, regexpr("[A-Za-z]{3}$", m, perl = TRUE)))
      letter <- aa3_to_letter[aa]
      if (!is.na(letter)) return(paste0("trn", letter))
    }

    # tRNA-X / trnX / TRN-X (single letter, 1-letter aa code)
    m <- regmatches(
      s_stripped,
      regexpr("(?i)^t[-_ ]?rn[-_ ]?a?[-_ ]?([A-Za-z])$",
              s_stripped, perl = TRUE)
    )
    if (length(m) > 0 && nchar(m) > 0) {
      letter <- toupper(substr(m, nchar(m), nchar(m)))
      if (letter %in% valid_letters) return(paste0("trn", letter))
    }

    NA_character_
  }

  result <- try_match(n)
  if (!is.na(result)) return(result)
  if (!is.na(product) && product != n) {
    result <- try_match(product)
    if (!is.na(result)) return(result)
  }
  NA_character_
}


#' Pre-compute a whole-genome pairwise alignment of a sample against its BLAST
#' reference and write the result for ingestion into the
#' \code{blast_ref_alignment} SQLite table.
#'
#' Called from the \code{blast_ref_align} Nextflow process after annotation is
#' complete.  Both sequences are passed as plain nucleotide strings (no headers,
#' no line-breaks) so that this function can run entirely from file-based inputs
#' without a database connection.
#'
#' @param assembly_seq nucleotide sequence string for the sample assembly
#' @param ref_seq nucleotide sequence string for the BLAST reference genome
#' @param rotation integer offset (0-based) used to rotate the reference so
#'   that it starts at the same anchor gene as the sample assembly.  Equals
#'   \code{pos1 - 1} of the anchor gene in the reference's
#'   \code{blast_ref_annotations} record.
#' @param output_file path to write a one-row CSV with columns
#'   \code{aligned_sample}, \code{aligned_ref}, \code{rotation},
#'   \code{ref_length}
#'
#' @export
compute_blast_ref_alignment <- function(assembly_seq, ref_seq, rotation = 0L,
                                        output_file) {
  empty <- data.frame(
    aligned_sample = character(),
    aligned_ref    = character(),
    rotation       = integer(),
    ref_length     = integer()
  )

  # Quick guard: first character must be a valid IUPAC nucleotide.
  # Catches HTML error responses or other garbage from a failed NCBI fetch
  # without scanning the full sequence string in the master process.
  if (!grepl("^[ACGTNacgtnRYSWKMBDHVryswkmbdhv]", ref_seq)) {
    message("compute_blast_ref_alignment: ref_seq does not appear to be a valid nucleotide sequence, skipping")
    write.csv(empty, output_file, row.names = FALSE)
    return(invisible(NULL))
  }

  tryCatch({
    rotation <- as.integer(rotation)

    # Sanitise: keep only valid IUPAC nucleotide characters
    clean <- function(s) gsub("[^ACGTNacgtnRYSWKMBDHVryswkmbdhv]", "N", s)
    sample_dna <- Biostrings::DNAString(clean(assembly_seq))
    ref_dna    <- Biostrings::DNAString(clean(ref_seq))
    ref_len    <- length(ref_dna)

    # Rotate the reference so anchor gene aligns with sample start
    if (rotation > 0L && rotation < ref_len) {
      ref_dna <- Biostrings::xscat(
        Biostrings::subseq(ref_dna, rotation + 1L, ref_len),
        Biostrings::subseq(ref_dna, 1L, rotation)
      )
    }

    subMx <- pwalign::nucleotideSubstitutionMatrix(
      match = 1, mismatch = -1, baseOnly = TRUE
    )
    aln <- pwalign::pairwiseAlignment(
      pattern            = sample_dna,
      subject            = ref_dna,
      substitutionMatrix = subMx,
      gapOpening         = 10,
      gapExtension       = 4,
      type               = "global"
    )

    aligned_sample <- as.character(pwalign::alignedPattern(aln))
    aligned_ref    <- as.character(pwalign::alignedSubject(aln))

    result <- data.frame(
      aligned_sample = aligned_sample,
      aligned_ref    = aligned_ref,
      rotation       = rotation,
      ref_length     = ref_len,
      stringsAsFactors = FALSE
    )
    write.csv(result, output_file, row.names = FALSE)

  }, error = function(e) {
    message("compute_blast_ref_alignment error: ", e$message)
    write.csv(empty, output_file, row.names = FALSE)
  })
  invisible(NULL)
}
