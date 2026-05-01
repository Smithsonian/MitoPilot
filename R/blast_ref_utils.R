#' Fetch and parse NCBI GFF3 annotations for a BLAST top hit
#'
#' Downloads the GFF3 record for the given accession from NCBI EFetch and
#' writes a CSV of gene annotations suitable for ingestion into the
#' blast_ref_annotations SQLite table.
#'
#' @param accession NCBI accession number (e.g. "NC_012345.1")
#' @param output_file path to write the output CSV
#'
#' @export
fetch_blast_ref <- function(accession, output_file) {
  empty <- data.frame(
    gene      = character(),
    type      = character(),
    pos1      = integer(),
    pos2      = integer(),
    direction = character(),
    ref_length = integer()
  )

  tryCatch({
    url <- paste0(
      "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi",
      "?db=nuccore&id=", accession,
      "&rettype=gff3&retmode=text"
    )
    gff3_text <- RCurl::getURL(
      url,
      .opts = list(ssl.verifypeer = FALSE, connecttimeout = 30, timeout = 120)
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
      write.csv(empty, output_file, row.names = FALSE)
      return(invisible(NULL))
    }

    fields <- strsplit(data_lines, "\t", fixed = TRUE)
    fields <- fields[vapply(fields, length, integer(1)) >= 9]
    if (length(fields) == 0) {
      write.csv(empty, output_file, row.names = FALSE)
      return(invisible(NULL))
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

    df$gbkey   <- get_attr(df$attributes, "gbkey")
    df$gene_nm <- get_attr(df$attributes, "gene")
    df$product <- get_attr(df$attributes, "product")
    df$reg_cls <- get_attr(df$attributes, "regulatory_class")
    df$note    <- get_attr(df$attributes, "Note")

    # Classify feature type then drop anything that doesn't match
    df$type <- dplyr::case_when(
      df$feature == "CDS"  | (!is.na(df$gbkey) & df$gbkey == "CDS") ~ "PCG",
      df$feature == "tRNA"  ~ "tRNA",
      df$feature == "rRNA"  ~ "rRNA",
      df$feature == "D-loop" |
        (!is.na(df$gbkey)   & df$gbkey == "D_loop") |
        (!is.na(df$reg_cls) & grepl("control", df$reg_cls, ignore.case = TRUE)) |
        (!is.na(df$note)    & grepl("control region", df$note, ignore.case = TRUE)) ~ "ctrl",
      .default = NA_character_
    )

    df <- df[!is.na(df$type), ]
    if (nrow(df) == 0) {
      write.csv(empty, output_file, row.names = FALSE)
      return(invisible(NULL))
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

  }, error = function(e) {
    message("fetch_blast_ref error for ", accession, ": ", e$message)
    write.csv(empty, output_file, row.names = FALSE)
  })
  invisible(NULL)
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
