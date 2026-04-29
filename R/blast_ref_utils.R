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
#' @param name raw gene/product name from NCBI GFF3
#' @param type one of "PCG", "tRNA", "rRNA", "ctrl"
#' @param product product name (used as fallback for rRNA)
#'
#' @noRd
normalize_mito_gene <- function(name, type, product = NA_character_) {
  if (is.na(name) || name == "") {
    if (!is.na(product) && product != "") {
      name <- product
    } else {
      return(NA_character_)
    }
  }
  n <- trimws(name)

  if (type == "ctrl") return("ctrl")

  if (type == "PCG") {
    pcg_lookup <- c(
      "nad1"="nad1", "nd1"="nad1", "ndh1"="nad1",
      "nad2"="nad2", "nd2"="nad2", "ndh2"="nad2",
      "cox1"="cox1", "co1"="cox1", "coi"="cox1", "coxi"="cox1",
      "cox2"="cox2", "co2"="cox2", "coii"="cox2", "coxii"="cox2",
      "cox3"="cox3", "co3"="cox3", "coiii"="cox3", "coxiii"="cox3",
      "atp8"="atp8",
      "atp6"="atp6",
      "nad3"="nad3", "nd3"="nad3",
      "nad4l"="nad4l", "nd4l"="nad4l",
      "nad4"="nad4",  "nd4"="nad4",
      "nad5"="nad5",  "nd5"="nad5",
      "nad6"="nad6",  "nd6"="nad6",
      "cob"="cob", "cytb"="cob", "cyb"="cob", "mt-cyb"="cob"
    )
    result <- pcg_lookup[tolower(n)]
    return(unname(if (!is.na(result)) result else tolower(n)))
  }

  if (type == "rRNA") {
    rrna_lookup <- c(
      "s-rrna"="rrnS", "rnr1"="rrnS", "rrns"="rrnS",
      "12s ribosomal rna"="rrnS", "12s rrna"="rrnS",
      "small subunit ribosomal rna"="rrnS",
      "l-rrna"="rrnL", "rnr2"="rrnL", "rrnl"="rrnL",
      "16s ribosomal rna"="rrnL", "16s rrna"="rrnL",
      "large subunit ribosomal rna"="rrnL"
    )
    key <- tolower(n)
    result <- rrna_lookup[key]
    if (!is.na(result)) return(unname(result))
    # Fallback: check product
    if (!is.na(product)) {
      key_p <- tolower(product)
      result_p <- rrna_lookup[key_p]
      if (!is.na(result_p)) return(unname(result_p))
      if (grepl("12s|small subunit", key_p)) return("rrnS")
      if (grepl("16s|large subunit", key_p)) return("rrnL")
    }
    return(paste0("rrn", n))
  }

  if (type == "tRNA") {
    # Strip anticodon: trnA(ugc) or trnA-UGC -> trnA
    n <- sub("\\([^)]*\\)$", "", n)
    n <- sub("[-_][A-Za-z]{3}$", "", n)
    # Strip trailing number: TRNL1 -> TRNL
    n <- sub("[0-9]+$", "", n)

    # trnX or TRNX pattern
    m <- regmatches(n, regexpr("^[Tt][Rr][Nn]([A-Za-z])$", n))
    if (length(m) > 0 && nchar(m) > 0) {
      letter <- toupper(substr(m, nchar(m), nchar(m)))
      return(paste0("trn", letter))
    }

    # tRNA-AminoAcid pattern (e.g. tRNA-Ala, tRNA-Leu)
    m <- regmatches(n, regexpr("(?i)tRNA[-_]([A-Za-z]+)", n, perl = TRUE))
    if (length(m) > 0 && nchar(m) > 0) {
      aa <- tolower(sub("(?i)tRNA[-_]", "", m, perl = TRUE))
      aa_map <- c(
        "ala"="trnA", "cys"="trnC", "asp"="trnD", "glu"="trnE",
        "phe"="trnF", "gly"="trnG", "his"="trnH", "ile"="trnI",
        "lys"="trnK", "leu"="trnL", "met"="trnM", "asn"="trnN",
        "pro"="trnP", "gln"="trnQ", "arg"="trnR", "ser"="trnS",
        "thr"="trnT", "val"="trnV", "trp"="trnW", "tyr"="trnY",
        "a"="trnA", "c"="trnC", "d"="trnD", "e"="trnE", "f"="trnF",
        "g"="trnG", "h"="trnH", "i"="trnI", "k"="trnK", "l"="trnL",
        "m"="trnM", "n"="trnN", "p"="trnP", "q"="trnQ", "r"="trnR",
        "s"="trnS", "t"="trnT", "v"="trnV", "w"="trnW", "y"="trnY"
      )
      result <- aa_map[aa]
      if (!is.na(result)) return(unname(result))
    }

    return(tolower(n))
  }

  n
}
