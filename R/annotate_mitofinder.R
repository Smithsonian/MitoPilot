#' Annotate a mitogenome assembly with MitoFinder (WF2, optional)
#'
#' Runs MitoFinder in annotation-only mode (`-a`) on an existing assembly,
#' independent of which assembler produced the contigs. Parses MitoFinder's
#' feature-level GFF output into the common MitoPilot annotation data frame so
#' the calls can be deduplicated against the other annotation tools. MitoFinder
#' is treated as the lowest-priority tool by `annotate()` (it only fills gaps the
#' higher-priority tools left).
#'
#' Intron-containing genes annotated with `--allow-intron` are returned as
#' multiple rows sharing one gene name (one per exon), matching the model used by
#' `export.R` / `validate`. Only merged correctly downstream when the gene's
#' curate ruleset has `intron = TRUE`; otherwise the exon rows are not merged.
#'
#' @param assembly a DNAStringSet object
#' @param mitofinder_db path to a MitoFinder reference database (GenBank `.gb`),
#'   e.g. built with `custom_assembly_db(db_type = "mitofinder")`.
#' @param genetic_code NCBI genetic code number (default: 2)
#' @param new_genes logical; pass `--new-genes` to annotate non-standard genes
#'   (default: FALSE)
#' @param allow_introns logical; pass `--allow-intron` to search for genes with
#'   introns (default: FALSE). See note above re: ruleset `intron = TRUE`.
#' @param mitofinder_opts additional free-form command line options for MitoFinder
#' @param cpus number of processors for MitoFinder (`-p`)
#' @param condaenv conda environment containing MitoFinder, or NULL to run on
#'   PATH (default: NULL, mirroring the WF1 assembler call).
#' @param workdir working directory for the MitoFinder run (default: a tempdir).
#'
#' @return a data frame with the common annotation columns
#'   (`contig, type, gene, product, pos1, pos2, length, direction, tRNA_ID,
#'   anticodon, start_codon, stop_codon, translation`). The codon/translation
#'   fields are populated for PCGs (mirroring `annotate_mitos2()`). Empty
#'   (correctly typed) when MitoFinder is off or finds nothing.
#'
#' @export
#'
annotate_mitofinder <- function(
    assembly = NULL,
    mitofinder_db = NULL,
    genetic_code = "2",
    new_genes = FALSE,
    allow_introns = FALSE,
    mitofinder_opts = "",
    cpus = 4,
    condaenv = NULL,
    workdir = NULL) {

  empty <- data.frame(
    contig = character(), type = character(), gene = character(),
    product = character(), pos1 = integer(), pos2 = integer(),
    length = integer(), direction = character(),
    tRNA_ID = character(), anticodon = character(),
    start_codon = character(), stop_codon = character(),
    translation = character()
  )

  if (is.null(assembly) || length(assembly) == 0L) {
    return(empty)
  }
  if (is.null(mitofinder_db) || !nzchar(mitofinder_db) || !file.exists(mitofinder_db)) {
    warning("annotate_mitofinder: reference database not found; skipping MitoFinder")
    return(empty)
  }

  genetic_code <- as.character(genetic_code)

  # Resolve db to absolute path before any setwd so relative paths staged by
  # Nextflow (e.g. "NC_002333_Danio_rerio.gb" in the work dir) remain valid.
  ref <- normalizePath(mitofinder_db, mustWork = TRUE)

  workdir <- workdir %||% tempfile("mitofinder_")
  dir.create(workdir, showWarnings = FALSE, recursive = TRUE)

  # Run inside workdir; MitoFinder writes its output tree to the cwd.
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(workdir)

  # Truncate contig names to the first token so GFF seqids match the names used
  # by the other annotation tools (mirrors annotate_mitos2()).
  asmb <- assembly
  names(asmb) <- stringr::str_extract(names(asmb), "^\\S+")
  fasta <- file.path(workdir, "assembly.fasta")
  Biostrings::writeXStringSet(asmb, fasta)
  job <- "mitofinder"

  args <- paste(
    "-a", shQuote(fasta),
    "-r", shQuote(ref),
    "-o", genetic_code,
    "-j", job,
    "-p", cpus,
    "--ignore",
    if (isTRUE(new_genes)) "--new-genes" else "",
    if (isTRUE(allow_introns)) "--allow-intron" else "",
    mitofinder_opts
  ) |>
    stringr::str_squish()

  message("starting MitoFinder")
  message(paste("MitoFinder work dir:", workdir))

  if (!is.null(condaenv)) {
    reticulate::conda_run2(
      cmd = "mitofinder",
      args = args,
      envname = condaenv,
      echo = FALSE
    )
  } else {
    system2("mitofinder", args = args)
  }

  message("finished MitoFinder")

  annotations <- .parse_mitofinder_gff(workdir, asmb, genetic_code)

  # Clean up the (potentially large) MitoFinder output tree.
  unlink(workdir, recursive = TRUE, force = TRUE)

  if (nrow(annotations) == 0L) {
    return(empty)
  }
  annotations
}

#' Parse MitoFinder feature-level GFF output into the common annotation data frame
#'
#' Reads the GFF(3) file(s) under `<workdir>/.../<job>_Final_Results/`, keeps
#' feature-level rows (CDS / tRNA / rRNA), and emits one row per feature line so
#' intron genes (multiple CDS lines, same gene name) become one row per exon.
#'
#' @noRd
.parse_mitofinder_gff <- function(workdir, assembly, genetic_code) {

  empty <- data.frame(
    contig = character(), type = character(), gene = character(),
    product = character(), pos1 = integer(), pos2 = integer(),
    length = integer(), direction = character(),
    tRNA_ID = character(), anticodon = character(),
    start_codon = character(), stop_codon = character(),
    translation = character()
  )

  gff_files <- list.files(
    workdir,
    pattern = "\\.gff3?$",
    full.names = TRUE,
    recursive = TRUE
  )
  # Prefer files under a *_Final_Results directory when present.
  final <- gff_files[stringr::str_detect(gff_files, "_Final_Results")]
  if (length(final) > 0L) gff_files <- final
  if (length(gff_files) == 0L) {
    warning("annotate_mitofinder: no GFF output found")
    return(empty)
  }

  raw <- purrr::map_dfr(gff_files, ~ {
    lines <- readLines(.x, warn = FALSE)
    lines <- lines[!stringr::str_detect(lines, "^#") & nzchar(lines)]
    if (length(lines) == 0L) return(empty[0, 1:9])
    cols <- stringr::str_split(lines, "\t", simplify = TRUE)
    if (ncol(cols) < 9L) return(empty[0, 1:9])
    data.frame(
      contig    = cols[, 1],
      feature   = cols[, 3],
      start     = suppressWarnings(as.integer(cols[, 4])),
      end       = suppressWarnings(as.integer(cols[, 5])),
      strand    = cols[, 7],
      attrs     = cols[, 9],
      stringsAsFactors = FALSE
    )
  })

  if (nrow(raw) == 0L) return(empty)

  # Keep feature-level rows; map MitoFinder feature types to MitoPilot types.
  raw <- raw |>
    dplyr::filter(stringr::str_to_upper(feature) %in% c("CDS", "TRNA", "RRNA")) |>
    dplyr::mutate(
      type = dplyr::case_when(
        stringr::str_to_upper(feature) == "CDS"  ~ "PCG",
        stringr::str_to_upper(feature) == "TRNA" ~ "tRNA",
        stringr::str_to_upper(feature) == "RRNA" ~ "rRNA"
      )
    )

  if (nrow(raw) == 0L) return(empty)

  # Extract a raw gene label from the attributes column, trying common keys.
  attr_val <- function(attrs, key) {
    stringr::str_match(attrs, stringr::regex(paste0(key, "=([^;]+)"), ignore_case = TRUE))[, 2]
  }
  raw <- raw |>
    dplyr::mutate(
      raw_gene = dplyr::coalesce(
        attr_val(attrs, "Name"),
        attr_val(attrs, "gene"),
        attr_val(attrs, "product"),
        attr_val(attrs, "ID")
      ),
      anticodon = attr_val(attrs, "anticodon")
    ) |>
    dplyr::filter(!is.na(raw_gene) & !is.na(start) & !is.na(end))

  if (nrow(raw) == 0L) return(empty)

  # gene -> amino acid for tRNA products (covers trnL / trnS without 1/2 suffix)
  trn_aa <- c(
    trnF = "Phe", trnV = "Val", trnL = "Leu", trnI = "Ile", trnQ = "Gln",
    trnM = "Met", trnW = "Trp", trnA = "Ala", trnN = "Asn", trnC = "Cys",
    trnY = "Tyr", trnS = "Ser", trnD = "Asp", trnK = "Lys", trnG = "Gly",
    trnR = "Arg", trnH = "His", trnE = "Glu", trnT = "Thr", trnP = "Pro"
  )

  annotations <- raw |>
    dplyr::transmute(
      contig,
      type,
      # Map to a canonical name; when a CDS does not map to a known gene keep it
      # as a non-standard PCG with a sanitized fallback name so the user can
      # curate it. Unmapped tRNA/rRNA features are still dropped (no ruleset).
      gene = dplyr::coalesce(
        normalize_mitofinder_gene(raw_gene),
        dplyr::if_else(type == "PCG", mitofinder_fallback_gene(raw_gene), NA_character_)
      ),
      pos1 = pmin(start, end),
      pos2 = pmax(start, end),
      direction = ifelse(strand == "-", "-", "+"),
      anticodon = toupper(anticodon)
    ) |>
    dplyr::filter(!is.na(gene)) |>
    dplyr::mutate(
      length = as.integer(1L + abs(pos2 - pos1)),
      product = dplyr::case_when(
        gene == "rrnL" ~ "16S ribosomal RNA",
        gene == "rrnS" ~ "12S ribosomal RNA",
        type == "tRNA" ~ paste0("tRNA-", unname(trn_aa[gene])),
        type == "PCG"  ~ unname(CDS_key[gene]),
        .default = NA_character_
      ),
      tRNA_ID = dplyr::if_else(
        type == "tRNA", paste0(product, "-", dplyr::coalesce(anticodon, "NNN")), NA_character_
      )
    )

  # Compute start/stop codons and translation for PCGs, mirroring annotate_mitos2()
  # output so downstream curation has the same fields. MitoFinder coordinates are
  # always pos1 <= pos2 (no circular wrap), so only the forward/reverse cases apply.
  gc <- Biostrings::getGeneticCode(as.character(genetic_code))
  annotations <- annotations |>
    dplyr::rowwise() |>
    dplyr::mutate(
      # coding-strand sequence for this feature (reverse-complemented when on "-")
      .cds = if (type == "PCG") {
        suppressWarnings({
          s <- Biostrings::subseq(assembly[contig], pos1, pos2)
          if (direction == "-") s <- Biostrings::reverseComplement(s)
          unname(as.character(s))
        })
      } else NA_character_,
      start_codon = if (type == "PCG" && !is.na(.cds)) substr(.cds, 1L, 3L) else NA_character_,
      stop_codon = if (type == "PCG" && !is.na(.cds)) {
        n <- nchar(.cds)
        len <- if (n %% 3L == 0L) 3L else n %% 3L
        substr(.cds, n - len + 1L, n)
      } else NA_character_,
      translation = if (type == "PCG" && !is.na(.cds)) {
        suppressWarnings(
          Biostrings::DNAString(substr(.cds, 1L, nchar(.cds) - nchar(stop_codon))) |>
            Biostrings::translate(genetic.code = gc, if.fuzzy.codon = "solve") |>
            as.character()
        )
      } else NA_character_
    ) |>
    dplyr::ungroup() |>
    dplyr::select(
      contig, type, gene, product, pos1, pos2, length, direction, tRNA_ID,
      anticodon, start_codon, stop_codon, translation
    ) |>
    dplyr::arrange(contig, pos1)

  annotations
}

#' Normalize MitoFinder reference gene names to MitoPilot canonical names
#'
#' MitoFinder reports gene names taken from the reference `.gb` (often uppercase
#' NCBI names such as `COX1`, `ND4L`, `16S`, `tRNA-Phe`). Map them onto the
#' lowercase MitoPilot convention (`cox1`, `nad4l`, `rrnL`, `trnF`).
#'
#' @noRd
normalize_mitofinder_gene <- function(x) {
  # tRNAs: trnX, tRNA-Aaa, or three-letter amino acid codes
  aa3_to_trn <- c(
    phe = "trnF", val = "trnV", leu = "trnL", ile = "trnI", gln = "trnQ",
    met = "trnM", trp = "trnW", ala = "trnA", asn = "trnN", cys = "trnC",
    tyr = "trnY", ser = "trnS", asp = "trnD", lys = "trnK", gly = "trnG",
    arg = "trnR", his = "trnH", glu = "trnE", thr = "trnT", pro = "trnP"
  )
  one_to_trn <- c(
    f = "trnF", v = "trnV", l = "trnL", i = "trnI", q = "trnQ", m = "trnM",
    w = "trnW", a = "trnA", n = "trnN", c = "trnC", y = "trnY", s = "trnS",
    d = "trnD", k = "trnK", g = "trnG", r = "trnR", h = "trnH", e = "trnE",
    t = "trnT", p = "trnP"
  )

  pcg_map <- c(
    nad1 = "nad1", nad2 = "nad2", nad3 = "nad3", nad4 = "nad4",
    nad4l = "nad4l", nad5 = "nad5", nad6 = "nad6",
    nd1 = "nad1", nd2 = "nad2", nd3 = "nad3", nd4 = "nad4",
    nd4l = "nad4l", nd5 = "nad5", nd6 = "nad6",
    cox1 = "cox1", cox2 = "cox2", cox3 = "cox3",
    coi = "cox1", coii = "cox2", coiii = "cox3",
    co1 = "cox1", co2 = "cox2", co3 = "cox3",
    cytb = "cob", cob = "cob", cyb = "cob",
    atp6 = "atp6", atp8 = "atp8", atp9 = "atp9",
    atpase6 = "atp6", atpase8 = "atp8"
  )
  rrna_map <- c(
    rrnl = "rrnL", rrns = "rrnS",
    `16s` = "rrnL", `12s` = "rrnS",
    lrrna = "rrnL", srrna = "rrnS",
    rnl = "rrnL", rns = "rrnS",
    lsu = "rrnL", ssu = "rrnS"
  )

  # Map one normalized (lowercase, alphanumeric-only) key to a canonical name.
  map_key <- function(k) {
    if (is.na(k) || !nzchar(k)) return(NA_character_)
    # tRNA-Aaa / trnAaa
    aa <- stringr::str_match(k, "^(?:trna|trn)([a-z]{3})")[, 2]
    if (!is.na(aa) && aa %in% names(aa3_to_trn)) return(unname(aa3_to_trn[aa]))
    # trnX single-letter (optional numeric suffix, e.g. trnS2)
    one <- stringr::str_match(k, "^trn([a-z])[0-9]?$")[, 2]
    if (!is.na(one) && one %in% names(one_to_trn)) return(unname(one_to_trn[one]))
    if (k %in% names(pcg_map)) return(unname(pcg_map[k]))
    if (k %in% names(rrna_map)) return(unname(rrna_map[k]))
    NA_character_
  }
  norm_key <- function(s) {
    s |> stringr::str_to_lower() |> stringr::str_remove_all("[^a-z0-9]")
  }

  raw <- stringr::str_squish(x)
  vapply(raw, function(label) {
    if (is.na(label) || !nzchar(label)) return(NA_character_)
    # First try the whole label (handles "tRNA-Phe", "ND4L", "16S").
    hit <- map_key(norm_key(label))
    if (!is.na(hit)) return(hit)
    # MitoFinder derives the gene from the reference 'gene' qualifier split on
    # "_", and Name strings can carry trailing descriptors
    # (e.g. "CYTB CDS 3' Partial CDS"). Retry on the first space/underscore
    # delimited token.
    first_tok <- stringr::str_extract(label, "^[^\\s_]+")
    map_key(norm_key(first_tok))
  }, character(1), USE.NAMES = FALSE)
}

#' Derive a sanitized fallback gene name for a non-standard MitoFinder CDS
#'
#' MitoFinder can annotate CDS features that do not map to a canonical mito
#' gene. Keep them as non-standard PCGs under a safe, lowercase name (first
#' space/underscore token, restricted to the characters the gene-assignment UI
#' accepts). Empty labels fall back to "orf".
#'
#' @noRd
mitofinder_fallback_gene <- function(x) {
  raw <- stringr::str_squish(x)
  vapply(raw, function(label) {
    if (is.na(label) || !nzchar(label)) return("orf")
    first_tok <- stringr::str_extract(label, "^[^\\s_]+")
    nm <- first_tok |>
      stringr::str_to_lower() |>
      stringr::str_remove_all("[^a-z0-9._-]")
    if (is.na(nm) || !nzchar(nm)) "orf" else nm
  }, character(1), USE.NAMES = FALSE)
}
